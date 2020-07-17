{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Reopt
  ( readElf64
  , parseElf64
  , showPaddedHex
  , checkedReadFile
  , isCodeSection
  , printX86SectionDisassembly
    -- * SymAddrMap
  , SymAddrMap
  , SymAddrMapLookupError(..)
  , symAddrMapLookup
    -- * Architecture info
  , SomeArchitectureInfo(..)
    -- * Code discovery
  , discoverBinary
    -- * Function recovery
  , Reopt.AnnotatedTypes.AnnDeclarations
  , Reopt.AnnotatedTypes.emptyAnnDeclarations
  , Reopt.Header.parseHeader
  , RecoveredModule(..)
  , GetFnsLogEvent(..)
    -- * Redirections
  , addrRedirections
  , mergeAndWrite
    -- * LLVM
  , LLVMVersion
  , versionOfString
  , LLVMConfig
  , llvmAssembly
  , latestLLVMConfig
  , getLLVMConfig
  , compileLLVM
      -- * X86 specific
  , X86OS(..)
  , osPersonality
  , osLinkName
  , discoverX86Elf
    -- * Utility
  , copyrightNotice
    -- * Re-exports
  , Data.Macaw.Memory.ElfLoader.LoadOptions(..)
  ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Either
import           Data.ElfEdit
  ( Elf
  , SomeElf(..)
  )
import qualified Data.ElfEdit as Elf
import qualified Data.ElfEdit.ByteString as ElfBS
import           Data.Foldable
import           Data.IORef
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import qualified Data.Set as Set
import           Data.String
import qualified Data.Vector as V
import           Data.Word
import qualified Flexdis86 as F
import           Numeric
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import qualified Text.PrettyPrint.HughesPJ as HPJ
import           Text.Printf (printf)

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import qualified Data.Macaw.Dwarf as Dwarf
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.SyscallInfo
import           Data.Macaw.X86.X86Reg

import           Reopt.AnnotatedTypes
import           Reopt.ArgResolver
import           Reopt.CFG.FnRep
import           Reopt.CFG.FunctionCheck
import           Reopt.CFG.LLVM (LLVMArchSpecificOps, LLVMGenOptions, moduleForFunctions)
import           Reopt.CFG.Recovery
import qualified Reopt.ExternalTools as Ext
import           Reopt.Header
import           Reopt.Hints
import           Reopt.Relinker
import qualified Reopt.VCG.Annotations as Ann

#ifdef SUPPORT_ARM
import qualified Data.VEX.FFI
import           Data.Macaw.VEX.AArch32 (armArch32le)
import           Data.Macaw.VEX.AArch64 (armArch64le)
#endif

copyrightNotice :: String
copyrightNotice = "Copyright 2014-19 Galois, Inc."

showUsage :: Handle -> IO ()
showUsage h = hPutStrLn h "For help on using reopt, run \"reopt --help\"."

------------------------------------------------------------------------
-- Resolve which symbols to include

-- | Map from defined symbol names to the address of the symbol
newtype SymAddrMap w = SymAddrMap (Map BS.ByteString (Set.Set (MemSegmentOff w)))

symAddrMapEmpty :: SymAddrMap w
symAddrMapEmpty = SymAddrMap Map.empty

symAddrMapInsert :: BSC.ByteString -> MemSegmentOff w -> SymAddrMap w -> SymAddrMap w
symAddrMapInsert nm addr (SymAddrMap m) =
  seq nm $ seq addr $ SymAddrMap (Map.insertWith (\_ old -> Set.insert addr old) nm (Set.singleton addr) m)

symAddrMapFromList :: [MemSymbol w] -> SymAddrMap w
symAddrMapFromList l =
  foldl' (\m s -> seq s $ symAddrMapInsert (memSymbolName s) (memSymbolStart s) m) symAddrMapEmpty l

-- | Error code if @symAddrMapLookup@ fails.
data SymAddrMapLookupError
   = SymAddrMapNotFound
   | SymAddrMapAmbiguous

-- | Lookup entry in symbol to address map.
symAddrMapLookup :: SymAddrMap w -> BS.ByteString -> Either SymAddrMapLookupError (MemSegmentOff w)
symAddrMapLookup (SymAddrMap m) nm =
  let s = Map.findWithDefault Set.empty nm m
   in case Set.size s of
       0 -> Left SymAddrMapNotFound
       1 -> Right (Set.findMin s)
       _ -> Left SymAddrMapAmbiguous

-- | Attempt to find the address of a string identifying a symbol
-- name, and return either the string if it cannot be resolved or the
-- address.
resolveSymAddr :: Memory w
                  -- ^ Loaded memory object.
               -> RegionIndex
                  -- ^ Region index for resolving addresses.
               -> SymAddrMap w
                 -- ^ Map from symbol names in binary to associated addresses.
               -> String
                  -- ^ The name of a symbol as a string.
               -> Either String (MemSegmentOff w)
resolveSymAddr mem regIdx symMap nm0 = addrWidthClass (memAddrWidth mem) $
  case resolveSymName nm0 of
    AddrIdent w ->
      case resolveRegionOff mem regIdx (fromIntegral w) of
        Just off -> Right off
        Nothing -> Left nm0
    SymbolIdent nm ->
      case symAddrMapLookup symMap nm of
        Left _ -> Left nm0
        Right a -> Right a

resolveIncludeFn :: Memory w
                    -- ^ Memory for binary
                 -> RegionIndex
                    -- ^ Region index to use for resolving include/exclude values
                    -- given as numeric literals.
                 -> SymAddrMap w
                    -- ^ Map from symbol names to addresses with name.
                    --
                    -- Note. This includes local and weak symbols.
                    -- Currently there is no way to disambiguate which
                    -- symbol is referred to, but that is likely ok.
                 -> [String] -- ^ Addresses to include
                 -> [String] -- ^ Addresses to exclude.
                 -> IO ([MemSegmentOff w], (MemSegmentOff w -> Bool))
resolveIncludeFn mem ridx symMap [] excludeNames = do
  let (bad, excludeAddrs) = partitionEithers $ resolveSymAddr mem ridx symMap <$> excludeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList excludeAddrs
  pure ([], (`Set.notMember` s))
resolveIncludeFn mem ridx symMap includeNames [] = do
  let (bad, includeAddrs) = partitionEithers $ resolveSymAddr mem ridx  symMap  <$> includeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList includeAddrs
  pure $ (includeAddrs, (`Set.member` s))
resolveIncludeFn _ _ _ _ _ = do
  fail "Cannot both include and exclude specific addresses."

------------------------------------------------------------------------
-- Architecture info


type ProcessPLTEntries w
   = Elf.ElfData
   -> BS.ByteString -- ^ Symbol table data
   -> BS.ByteString -- ^ String table data for symbol table.
   -> Map BS.ByteString [Elf.ElfSection (Elf.ElfWordType w)]
      -- ^ Map from section names to section.
   -> Memory w
      -- ^ Memory
   -> IORef (Maybe (MemSegmentOff w, MemWord w))
      -- ^ Records PLT bounds
   -> IORef [(MemSegmentOff w, BS.ByteString)]
      -- ^ PLT bounds
   -> IORef (SymbolMaps w)
      -- ^ Symbol maps to update
   -> IO ()

data SomeArchitectureInfo w where
  SomeArch :: !(ArchitectureInfo arch)
           -> !(ProcessPLTEntries (ArchAddrWidth arch))
           -> SomeArchitectureInfo (ArchAddrWidth arch)


------------------------------------------------------------------------
-- checkedReadFile

-- | This reads a file as a strict bytestring.
--
-- This will exit the program if failures occur, and also write to stderr.
checkedReadFile :: FilePath -> IO BS.ByteString
checkedReadFile path = do
  when (null path) $ do
    hPutStrLn stderr "Please specify a path."
    showUsage stderr
    exitFailure
  let h e | isDoesNotExistError e = do
            hPutStrLn stderr $ path ++ " does not exist."
            showUsage stderr
            exitFailure
          | isUserError e = do
            hPutStrLn stderr (ioeGetErrorString e)
            exitFailure
          | otherwise = do
            hPutStrLn stderr (show e)
            hPutStrLn stderr (show (ioeGetErrorType e))
            exitFailure
  BS.readFile path `catch` h

------------------------------------------------------------------------
-- Read an elf file

-- | Print errors that occured when reading to @stderr@.
showElfParseErrors :: [Elf.ElfParseError]
                   -> IO ()
showElfParseErrors l = do
  when (not (null l)) $ do
    hPutStrLn stderr $ "Recoverable errors occurred in reading elf file:"
    forM_ l $ \emsg -> do
      hPutStrLn stderr (show emsg)

parseElf64 :: String
              -- ^ Name of output for error messages
           -> BS.ByteString
              -- ^ Data to read
           -> IO (Elf 64)
parseElf64 nm bs =
  case Elf.parseElfHeaderInfo bs of
    Left (_, msg) -> do
      hPutStrLn stderr $ "Could not parse Elf file " ++ nm ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Right (Elf32 _) -> do
      hPutStrLn stderr "32-bit elf files are not yet supported."
      exitFailure
    Right (Elf64 hdr) -> do
      let (l, e) = Elf.getElf hdr
      showElfParseErrors l
      return e

-- | Read a 64-bit elf file.
readElf64 :: FilePath
             -- ^ Filepath to rad.
          -> IO (Elf 64)
readElf64 path = checkedReadFile path >>= parseElf64 path

-- | Read an elf file from the path and write errors to standard error.
--
-- This will print to stderr and exit if path cannot be read.
readSomeElf :: FilePath -> IO (Some Elf.ElfHeaderInfo)
readSomeElf path = do
  bs <- checkedReadFile path
  case Elf.parseElfHeaderInfo bs of
    Left (_, msg) -> do
      hPutStrLn stderr $ "Error reading " ++ path ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Right (Elf32 hdr) ->
      pure $! Some hdr
    Right (Elf64 hdr) ->
      pure $! Some hdr

------------------------------------------------------------------------
-- Get binary information

getElfArchInfo :: Elf.ElfClass w -> Elf.ElfMachine -> Elf.ElfOSABI -> IO (SomeArchitectureInfo w)
getElfArchInfo cl arch abi =
  case (cl, arch, abi) of
    (Elf.ELFCLASS64, Elf.EM_X86_64, Elf.ELFOSABI_LINUX)   ->
      pure $! SomeArch x86_64_linux_info   processX86PLTEntries
    (Elf.ELFCLASS64, Elf.EM_X86_64, Elf.ELFOSABI_SYSV)    ->
      pure $! SomeArch x86_64_linux_info   processX86PLTEntries
    (Elf.ELFCLASS64, Elf.EM_X86_64, Elf.ELFOSABI_FREEBSD) ->
      pure $! SomeArch x86_64_freeBSD_info processX86PLTEntries
#ifdef SUPPORT_ARM
    (Elf.ELFCLASS32, Elf.EM_ARM, ELFOSABI_SYSV) -> do
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure $! SomeArch armArch32le ignorePLTEntries
    (Elf.ELFCLASS64, Elf.EM_AARCH64, ELFOSABI_SYSV) -> do
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure $! SomeArch armArch64le ignorePLTEntries
#endif
    _ -> do
     let archName = case Map.lookup arch Elf.elfMachineNameMap of
                      Just nm -> nm
                      Nothing -> "unknown-abi(" ++ showHex (Elf.fromElfMachine arch) ")"
     hPutStrLn stderr
        $ "Do not support " ++ show (Elf.elfClassBitWidth cl) ++ "-bit "
        ++ archName ++ " " ++ show abi ++ " binaries."
     exitFailure

------------------------------------------------------------------------
-- Explore a control flow graph.

elfInstances :: Elf.ElfHeaderInfo w
             -> ((MemWidth w, Integral (Elf.ElfWordType w), Show (Elf.ElfWordType w)) => a)
             -> a
elfInstances hdr x =
  case Elf.headerClass (Elf.header hdr) of
    Elf.ELFCLASS32 -> x
    Elf.ELFCLASS64 -> x

-- | Symbol table maps needed for function discovery/post processing.
type SymbolMaps w = (SymAddrMap w, AddrSymMap w)

-- | Write a warning to stderr.
showWarning :: MonadIO m => String -> m ()
showWarning msg = liftIO $ hPutStrLn stderr $ "Warning: " ++ msg
{-# INLINE showWarning #-}

insertSymbolMaps :: IORef (SymbolMaps w) -> BS.ByteString -> MemSegmentOff w -> IO ()
insertSymbolMaps r nm addr = do
  modifyIORef' r $ \(symAddrMap, addrSymMap) ->
    let symAddrMap' = symAddrMapInsert nm addr symAddrMap
        addrSymMap' = Map.insert addr nm addrSymMap
     in seq symAddrMap' $ seq addrSymMap' $ (symAddrMap', addrSymMap')

-- | Insert a symbol table entry into map.
insSymbol :: (MemWidth w, Integral (Elf.ElfWordType w))
          => SectionIndexMap w
          -> IORef (SymbolMaps w)
          -> (Int, Elf.ElfSymbolTableEntry (Elf.ElfWordType w))
          -> IO ()
insSymbol secMap symMapRef (idx, symEntry)
  -- Skip non-function symbols
  | Elf.steType symEntry /= Elf.STT_FUNC = do
      pure ()
  | Elf.steIndex symEntry == Elf.SHN_UNDEF = do
      pure ()
  | BS.null (Elf.steName symEntry) = do
      showWarning (show (EmptySymbolName idx (Elf.steType symEntry)))
  | Elf.steIndex symEntry == Elf.SHN_ABS = do
      showWarning "SHN_ABS symbols not supported in dynamic binaries."
  | otherwise = do
      let nm = Elf.steName symEntry
      let val = Elf.steValue symEntry
      case Map.lookup (Elf.steIndex symEntry) secMap of
        Just (base,sec)
          | Elf.elfSectionAddr sec <= val && val < Elf.elfSectionAddr sec + Elf.elfSectionSize sec
          , off <- toInteger val - toInteger (Elf.elfSectionAddr sec) -> do
              let addr = fromMaybe (error "Invalid address") (incSegmentOff base off)
              insertSymbolMaps symMapRef nm addr
        _ -> do
          showWarning (show (CouldNotResolveAddr nm))

-- | Run a computation with the given section, and print a warning if
-- the section is not found or multiply defined.
withSection :: Map BS.ByteString [Elf.ElfSection tp]
            -> String -- ^ Name of section for warning messages.
            -> BS.ByteString -- ^ Name of section in Elf file.
            -> (Elf.ElfSection tp -> IO ())
            -> IO ()
withSection sectionNameMap warnName secName f =
  case Map.findWithDefault [] secName sectionNameMap of
    []    -> showWarning $ "Could not find " ++ warnName ++ " sections."
    _:_:_ -> showWarning $ "Found multiple " ++ warnName ++ " sections."
    [s] -> f s

withSymtab :: V.Vector (Elf.ElfSection tp)
              -- ^ Section vector
           -> Map BS.ByteString [Elf.ElfSection tp]
              -- ^ Map from names to sections
           -> String
           -> BS.ByteString
           -> (BS.ByteString -> BS.ByteString -> IO ())
              -- ^ Continuation to run to get symtab and string table (respectively).
           -> IO ()
withSymtab sections sectionNameMap warnName secName f = do
  withSection sectionNameMap warnName secName $ \symtab -> do
    let strtabIdx = Elf.elfSectionLink symtab
    if strtabIdx == 0 || fromIntegral strtabIdx >= V.length sections then do
      showWarning $ "Invalid symbol table index in" ++ warnName ++ "."
     else do
      let strtab = sections V.! fromIntegral strtabIdx
      f (Elf.elfSectionData symtab) (Elf.elfSectionData strtab)


addDefinedSymbolTableFuns
  :: (MemWidth w, Integral (Elf.ElfWordType w))
  => Elf.ElfClass w
  -> Elf.ElfData
  -> SectionIndexMap w
  -> BS.ByteString -- ^ Symbol table for parsing.
  -> BS.ByteString -- ^ String table
  -> IORef (SymbolMaps w)
  -> IO ()
addDefinedSymbolTableFuns cl dta secMap symtabData strtab symMapRef = do
  let symEntrySize :: Int
      symEntrySize = Elf.symbolTableEntrySize cl
  let cnt :: Word32
      cnt = fromIntegral (BS.length symtabData `quot` symEntrySize)
  let symtabDataL = BSL.fromChunks [symtabData]
  let go idx
        | idx >= cnt = pure ()
        | otherwise =
            case Elf.parseSymbolTableEntry cl dta strtab symtabDataL idx of
              Left e -> do
                showWarning $ "Failed to parse symbol table entry " ++ show e
                go (idx+1)
              Right symEntry -> do
                insSymbol secMap symMapRef (fromIntegral idx, symEntry)
                go (idx+1)
  go 1

-- | Arguments for identifying a PLT stub.
data MatchPLTStubArgs =
  MatchPLTStubArgs { -- | Address of PLT in Elf file
                     pltElfAddr :: !Word64
                     -- | Address in memory of PLT section.
                     --
                     -- This is a function of the PLT Elf addr and the load options.
                   , pltMemAddr :: !(MemSegmentOff 64)
                     -- | Code in PLT
                   , pltPLTData :: !BS.ByteString
                     -- | Contents of `.plt.rela` for relocations
                   , pltPLTRelaData :: !BS.ByteString
                     -- | Symbol table data
                   , pltSymtab  :: !BS.ByteString
                     -- | String table for symbol table.
                   , pltStrtab  :: !BS.ByteString
                   }

-- | This match a X86_64 PLT stub.
--
-- The format is
--   jmpq off(%rip)  -- This is "ff 25" followed by (got address) - rip.
--   pushq idx      -- This is "68" followed by a 4 byte index into GOT table.
--   jmpq plt entry -- This is "e9" followed by a 4-byte signed int
matchPLTStub :: Elf.ElfData -- ^ Endianess of Elf file
             -> MatchPLTStubArgs
             -> IORef [(MemSegmentOff 64, BS.ByteString)]
             -- ^ PLT functions
             -> IORef (SymbolMaps 64)
             -> Int -- ^ Index of PLT entry (first function is at zero)
             -> ExceptT () IO ()
matchPLTStub dta args pltFnsRef symMapRef idx = do
  -- Offset of PLT stub in data
  let off :: Int
      off = 0x10 * (idx+1)
  -- PLT code
  let plt = pltPLTData args
  -- Get relocation entry for this PLT stub.
  let rela :: Elf.RelaEntry Elf.X86_64_RelocationType
      rela = Elf.relaEntry dta (pltPLTRelaData args) idx
  let pltEntryAddr :: Word64
      pltEntryAddr = pltElfAddr args + fromIntegral off
  unless (Elf.relaType rela == Elf.R_X86_64_JUMP_SLOT) $ do
    showWarning $ "PLT stub relocation type is not a jump slot."
  unless (Elf.relaAddend rela == 0) $ do
    showWarning $ "PLT stub relocation addened is non-zero."
  unless (plt `BS.index` off == 0xff && plt `BS.index` (off+ 1) == 0x25) $ do
    showWarning $ "PLT stub first instruction is not RIP relative jump."
  -- Address of the pushq instruction.
  let pushqAddr :: Word64
      pushqAddr = pltEntryAddr + 6
  -- Get offset of PLT Got entry as encoded in plt code.
  let pltGotAddr = pushqAddr + fromIntegral (ElfBS.bsWord32le (BS.take 4 (BS.drop (off+2) plt)))
  unless (pltGotAddr == Elf.relaAddr rela) $ do
    showWarning $ "PLT jump address does not point to expected relocation entry in GOT table."
  unless (plt `BS.index` (off+ 6) == 0x68) $ do
    showWarning $ "PLT stub second instruction is not push."

  -- Index stored in pushq instruction
  let pushqIdx = ElfBS.bsWord32le (BS.take 4 (BS.drop (off+7) plt))
  unless (pushqIdx == fromIntegral idx) $ do
    showWarning $ "PLT stub push index does not match expected value."

  unless (plt `BS.index` (off+11) == 0xe9) $ do
    showWarning $ "PLT stub third instruction is not direct jump."
  let pltDelta :: Int32
      pltDelta = fromIntegral $ ElfBS.bsWord32le $ BS.take 4 $ BS.drop (off+12) plt
  unless (fromIntegral pltDelta == -0x10 * (idx+2)) $ do
    showWarning "PLT stub third instruction is not direct jump."
  -- Get symbol index for relation
  let symIndex :: Word32
      symIndex = Elf.relaSym rela
  let symtab = BSL.fromChunks [pltSymtab args]
  sym <-
    case Elf.parseSymbolTableEntry Elf.ELFCLASS64 dta (pltStrtab args) symtab symIndex of
      Left err -> do
        showWarning (show err)
        throwError ()
      Right entry ->
        pure entry
  let symName = Elf.steName sym
  unless (Elf.steType sym == Elf.STT_FUNC) $
    showWarning "PLT symbol must be a function."
  unless (Elf.steBind sym == Elf.STB_GLOBAL) $
    showWarning "PLT symbol must have global vinding."
  unless (Elf.steIndex sym == Elf.SHN_UNDEF) $
    showWarning "PLT symbol is must be undefined."
  unless (Elf.steValue sym == 0 && Elf.steSize sym == 0) $ do
    showWarning "PLT symbol must be unassigned."
  a <-
    case incSegmentOff (pltMemAddr args) (toInteger off) of
      Nothing -> do
        showWarning "PLT does not appear to have a valid address."
        throwError ()
      Just a -> pure a
  liftIO $ do
    modifyIORef' pltFnsRef (\l -> (a,symName):l)
    insertSymbolMaps symMapRef symName a

#ifdef SUPPORT_ARM
ignorePLTEntries :: ProcessPLTEntries w
ignorePLTEntries _ _ _ _ _ _ _ _ = pure ()
#endif

processX86PLTEntries :: Elf.ElfData
                     -> BS.ByteString -- ^ Symbol table data
                     -> BS.ByteString -- ^ String table data for symbol table.
                     -> Map BS.ByteString [Elf.ElfSection (Elf.ElfWordType 64)]
                        -- ^ Map from section names to section.
                     -> Memory 64
                     -> IORef (Maybe (MemSegmentOff 64, MemWord 64))
                        -- ^ Records PLT bounds
                     -> IORef [(MemSegmentOff 64, BS.ByteString)]
                        -- ^ PLT functions
                     -> IORef (SymbolMaps 64)
                     -> IO ()
processX86PLTEntries dta symtab strtab sectionNameMap mem pltBoundsRef pltFnsRef symMapRef = do
  withSection sectionNameMap "PLT" ".plt" $ \pltSec -> do
    case Map.lookup (Elf.elfSectionIndex pltSec) (memSectionIndexMap mem) of
      Nothing -> do
        showWarning "PLT section is not loaded in memory"
      Just pltAddr -> do
        writeIORef pltBoundsRef $! Just (pltAddr, fromIntegral (Elf.elfSectionSize pltSec))
        withSection sectionNameMap "PLT relocation" ".rela.plt" $ \relaPLT -> do
          let relaData = Elf.elfSectionData relaPLT
          let (relaCount, r) = BS.length relaData `quotRem` Elf.relaEntSize Elf.ELFCLASS64
          when (r /= 0) $ do
            showWarning ".rela.plt data is not a multiple of relocation entry size."

          -- Find base address of PLT stubs
          let pltStubArgs = MatchPLTStubArgs { pltElfAddr = Elf.elfSectionAddr pltSec
                                             , pltMemAddr = pltAddr
                                             , pltPLTData = Elf.elfSectionData pltSec
                                             , pltPLTRelaData = relaData
                                             , pltSymtab = symtab
                                             , pltStrtab = strtab
                                             }
          forM_ [0..relaCount-1] $ \i -> do
            void $ runExceptT (matchPLTStub dta pltStubArgs pltFnsRef symMapRef i)


-- | Information returned by `initDiscovery` below.
data InitDiscovery arch = InitDiscovery { initDiscElf :: !(Elf (ArchAddrWidth arch))
                                          -- ^ Elf file read in.
                                        , initDiscState :: !(DiscoveryState arch)
                                          -- ^ Initial state for discovery
                                        , initDiscAddrSymMap :: !(AddrSymMap (ArchAddrWidth arch))
                                          -- ^ Partial Map from addresses to associated external symbol name
                                        , initDiscSymAddrMap :: !(SymAddrMap (ArchAddrWidth arch))
                                          -- ^ Map from symbols to addresses addresses
                                        , initDiscPLTFuns :: [(MemSegmentOff (ArchAddrWidth arch), BS.ByteString)]
                                          -- ^ A list of address and name pairs for PLT functions.
                                        , initDiscPred :: !(ArchSegmentOff arch -> Bool)
                                          -- ^ Predicate that returns true if we should explore a function at that
                                          -- address.
                                        , initDiscBaseCodeAddr :: !(MemAddr (ArchAddrWidth arch))
                                          -- ^ Address to use as base address for program counters in Dwarf
                                          -- debug information.
                                        }

-- | Discover functions in an elf file.
--
--  Note. This prints warnings to stderr
initDiscovery :: forall arch
                     . LoadOptions
                     -- ^ Option to load the binary at the given address
                     -> Elf.ElfHeaderInfo (ArchAddrWidth arch)
                     -> ArchitectureInfo arch
                     -> ProcessPLTEntries (ArchAddrWidth arch)
                     -> [String] -- ^ Included addresses
                     -> [String] -- ^ Excluded addresses
                     -> IO (InitDiscovery arch)
initDiscovery loadOpts hdrInfo ainfo pltFn includeAddr excludeAddr = elfInstances hdrInfo $ do
  let hdr = Elf.header hdrInfo
  case Elf.headerType hdr of
    -- This is for object files.
    Elf.ET_REL -> do
      case loadOffset loadOpts of
        Nothing -> pure ()
        Just _ -> showWarning $ "Ignoring load offset for object file as there is no global base address."
      let (l, elfFile) = Elf.getElf hdrInfo
      showElfParseErrors l
      (mem, secMap, warnings) <-
        case memoryForElfSections elfFile of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings
      let isRelevant ste = Elf.steType ste == Elf.STT_FUNC

      let (symErrs, symbols) = resolveElfFuncSymbols mem secMap isRelevant elfFile

      mapM_ (hPutStrLn stderr . show) symErrs

      let addrSymMap = Map.fromList [ (memSymbolStart msym, memSymbolName msym) | msym <- symbols ]
      let symAddrMap = symAddrMapFromList symbols

      -- Get index of text section section.
      textSectionIndex <-
        case filter (\s -> Elf.elfSectionName s == ".text") (elfFile^..Elf.elfSections) of
          [] -> do
            hPutStrLn stderr "Could not find .text section."
            exitFailure
          (textSec:r) -> do
            when (not (null r)) $ do
              hPutStrLn stderr "Could not find .text section."
              exitFailure
            pure $! Elf.elfSectionIndex textSec
      -- Get offset for text section.
      textBaseAddr <-
        case Map.lookup (Elf.ElfSectionIndex textSectionIndex) secMap of
          Nothing -> do
            hPutStrLn stderr "Text section is not loaded."
            exitFailure
          Just (a, _textSec) ->
            pure a
      -- Get region of text segment
      let regIdx :: RegionIndex
          regIdx = segmentBase (segoffSegment textBaseAddr)
      -- Get initial entries and predicate for exploring
      (allEntries, fnPred) <- resolveIncludeFn mem regIdx symAddrMap includeAddr excludeAddr
      let readyState
            = emptyDiscoveryState mem addrSymMap ainfo
            & markAddrsAsFunction InitAddr allEntries
      pure $! InitDiscovery { initDiscElf = elfFile
                            , initDiscState = readyState
                            , initDiscAddrSymMap = addrSymMap
                            , initDiscSymAddrMap = symAddrMap
                            , initDiscPLTFuns = []
                            , initDiscPred = fnPred
                            , initDiscBaseCodeAddr = MemAddr regIdx 0
                            }
    -- Static executable
    Elf.ET_EXEC -> do
      case loadOffset loadOpts of
        Nothing -> pure ()
        Just _ -> showWarning $ "Ignoring load offset for unrelocatable executable."
      let (l, elfFile) = Elf.getElf hdrInfo
      showElfParseErrors l
      (mem, secMap, warnings) <-
        case memoryForElfSegments defaultLoadOptions elfFile of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings
      let isRelevant ste = Elf.steType ste == Elf.STT_FUNC
      let (symErrs, symbols) = resolveElfFuncSymbols mem secMap isRelevant elfFile
      mapM_ (hPutStrLn stderr . show) symErrs
      let addrSymMap = Map.fromList [ (memSymbolStart msym, memSymbolName msym) | msym <- symbols ]
      let symAddrMap = symAddrMapFromList symbols
      -- Get initial entries and predicate for exploring
      (entries, fnPred) <- resolveIncludeFn mem 0 symAddrMap includeAddr excludeAddr
      let initState
            = emptyDiscoveryState mem addrSymMap ainfo
            & markAddrsAsFunction InitAddr entries

      let entry = Elf.headerEntry hdr
      readyState <-
        case resolveRegionOff mem 0 (fromIntegral entry) of
          Nothing -> do
            hPutStrLn stderr $ "Could not resolve entry point 0x" ++ showHex entry ""
            pure $! initState
          Just v -> do
            pure $! initState & markAddrAsFunction InitAddr v
      pure $! InitDiscovery { initDiscElf = elfFile
                            , initDiscState = readyState
                            , initDiscAddrSymMap = addrSymMap
                            , initDiscSymAddrMap = symAddrMap
                            , initDiscPLTFuns = []
                            , initDiscPred = fnPred
                            , initDiscBaseCodeAddr = MemAddr 0 0
                            }
    -- This is a shared library or position-independent executable.
    Elf.ET_DYN -> do
      let (l, elfFile) = Elf.getElf hdrInfo
      showElfParseErrors l
      -- Create memory image for elf file.
      (mem, secMap, warnings) <-
        case memoryForElfSegments loadOpts elfFile of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings
      -- Get symbol table.
      let cl = Elf.headerClass hdr
      let dta = Elf.headerData hdr

      let sections = Elf.headerSections hdrInfo

      let sectionNameMap :: Map BS.ByteString [Elf.ElfSection (Elf.ElfWordType (ArchAddrWidth arch))]
          sectionNameMap =
            Map.fromListWith (++)
              [ (Elf.elfSectionName s, [s]) | (_rng,s) <- V.toList sections ]

      symMapRef <- newIORef $ (symAddrMapEmpty, Map.empty)
      withSymtab (snd <$> sections) sectionNameMap "static symbol table" ".symtab" $ \symtab strtab -> do
        addDefinedSymbolTableFuns cl dta secMap symtab strtab symMapRef

      pltBoundsRef <- newIORef Nothing
      pltFnsRef <- newIORef []
      withSymtab (snd <$> sections) sectionNameMap "dynamic symbol table" ".dynsym" $ \dynSymtab dynStrtab -> do
        addDefinedSymbolTableFuns cl dta secMap dynSymtab dynStrtab symMapRef
        pltFn dta dynSymtab dynStrtab sectionNameMap mem pltBoundsRef pltFnsRef symMapRef

      mbounds <- readIORef pltBoundsRef
      let exploreFn =
            case mbounds of
              Nothing -> \_ -> True
              Just (pltStartSegOff, pltSize) ->
                let pltStartAddr = segoffAddr pltStartSegOff
                    pltBase   = addrBase pltStartAddr
                    pltStartOff = addrOffset pltStartAddr
                    inPLT a = pltBase == addrBase a
                              && pltStartOff <= addrOffset a
                              && addrOffset a - pltStartOff < pltSize
                 in \a -> not (inPLT (segoffAddr a))
      pltFns <- readIORef pltFnsRef
      (symAddrMap, addrSymMap) <- readIORef symMapRef

      -- Get initial entries and predicate for exploring
      let regIdx :: RegionIndex
          regIdx = if isJust (loadOffset loadOpts) then 0 else 1

      (entries, fnPred) <- resolveIncludeFn mem regIdx symAddrMap includeAddr excludeAddr
      let postEntryState = emptyDiscoveryState mem addrSymMap ainfo
                         & exploreFunctionAddr .~ exploreFn
                         & markAddrsAsFunction InitAddr entries
      -- Get initial entry address
      let entry = Elf.headerEntry hdr
      -- Adjust entry by load offset.
      let adjEntry =
            case loadOffset loadOpts of
              Nothing -> toInteger entry
              Just o -> toInteger o + toInteger entry
      -- Mark entry as address.
      readyState <-
        case resolveRegionOff mem regIdx (fromInteger adjEntry) of
          Nothing -> do
            hPutStrLn stderr $ "Could not resolve entry point: " ++ showHex entry ""
            pure $! postEntryState
          Just v -> do
            pure $! postEntryState & markAddrAsFunction InitAddr v
      -- Get base address for Dwarf
      let dwarfBaseAddr =
            case loadOffset loadOpts of
              Just o -> MemAddr 0 (fromIntegral o)
              Nothing -> MemAddr 1 0
      -- Return discovery
      pure $! InitDiscovery { initDiscElf = elfFile
                            , initDiscState = readyState
                            , initDiscAddrSymMap = addrSymMap
                            , initDiscSymAddrMap = symAddrMap
                            , initDiscPLTFuns = pltFns
                            , initDiscPred = fnPred
                            , initDiscBaseCodeAddr = dwarfBaseAddr
                            }
    Elf.ET_CORE -> do
      hPutStrLn stderr "Reopt does not support loading core files."
      exitFailure
    tp -> do
      hPutStrLn stderr $ "Reopt does not support loading elf files with type " ++ show tp ++ "."
      exitFailure

-- | Discover code in the binary identified by the given path.
discoverBinary :: FilePath
               -> LoadOptions
                  -- ^ Option to load the binary at the given address
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> [String] -- ^ Included addresses
               -> [String] -- ^ Excluded addresses
               -> IO (Some DiscoveryState)
discoverBinary path loadOpts disOpt includeAddr excludeAddr = do
  Some hdrInfo <- readSomeElf path
  let hdr = Elf.header hdrInfo
  -- Get architecture information for elf
  SomeArch ainfo pltFn <- getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr)
  initState <- initDiscovery loadOpts hdrInfo ainfo pltFn includeAddr excludeAddr
  Some <$> completeDiscoveryState (initDiscState initState) disOpt (initDiscPred initState)


------------------------------------------------------------------------
-- Print disassembly

-- | Return number of digits required to show a given unsigned number in hex.
hexDigitsReq :: Bits a => a -> Int
hexDigitsReq b = go 1 (b `shiftR` 4)
  where go r v | popCount v == 0 = r
               | otherwise = go (r+1) (v `shiftR` 4)

trimForWord64Buffer :: Word64 -> Int -> String -> String
trimForWord64Buffer base n s = drop d s
  where m = hexDigitsReq (max base (base + fromIntegral n))
        d = 16 - m

-- | Show a given hexideimal number with a fixed width, adding
-- zeros as needed.
showPaddedHex :: (FiniteBits a, Integral a, Show a) => a -> String
showPaddedHex v = assert (l >= n) $ replicate (l-n) '0' ++ s
  where l = finiteBitSize v `shiftR` 2
        s | v >= 0 = showHex v ""
          | otherwise = error "showPaddedHex given negtive number"
        n = length s

-- | Convert ByteString to a string of hex digits.
showBytes :: BS.ByteString -> String
showBytes b = unwords (showPaddedHex <$> BS.unpack b)

-- | Slice part of bytestring.
slice :: Int -> Int -> BS.ByteString -> BS.ByteString
slice i n b = BS.take n (BS.drop i b)

-- | @stringToFixedBuffer n s@ returns a string with length @n@ containing
-- @s@ or a prefix of @s@.  If @n@ exceeds the length of @s@, then additional
-- whitespace is appended to @s@.
stringToFixedBuffer :: Int -> String -> String
stringToFixedBuffer g s | g == n = s
                        | g < n = take g s
                        | otherwise = s ++ replicate (g-n) ' '
  where n = length s

-- | Print out disasembly for a specific line.
printX86DisassemblyLine :: Handle  -- ^ Handle to write to
                        -> Word64 -- ^ Base address for section or segment.
                        -> BS.ByteString -- ^ Data region for code.
                        -> F.DisassembledAddr -- ^ Output from flexdis
                        -> IO ()
printX86DisassemblyLine h base buffer (F.DAddr i n mi) = do
  let o = base + fromIntegral i
  let ppAddr x = trimForWord64Buffer base (BS.length buffer) (showPaddedHex x)
  let b = showBytes $ slice i n buffer
  let r = case mi of
            Nothing  -> take 20 b
            Just ins -> stringToFixedBuffer 21 b ++ "\t" ++ show (F.ppInstruction ins)
  hPutStrLn h $ "  " ++ ppAddr o ++ ":\t" ++ r
  when (n > 7) $ do
    printX86DisassemblyLine h base buffer $ F.DAddr (i+7) (n-7) Nothing

-- | Print all the disassembly for a buffer to stdout.
printX86SectionDisassembly :: Handle
                           -> BSC.ByteString
                           -> Word64
                           -> BS.ByteString
                           -> IO ()
printX86SectionDisassembly h nm addr buffer = do
  hPutStrLn h $ "Disassembly of section " ++ BSC.unpack nm ++ ":"
  hPutStrLn h ""
  hPutStrLn h $ showPaddedHex addr ++ " <" ++ BSC.unpack nm ++ ">:"
  let dta = F.disassembleBuffer buffer
  mapM_ (printX86DisassemblyLine h addr buffer) dta
  hPutStrLn h ""

isCodeSection :: (Bits w, Num w) => Elf.ElfSection w -> Bool
isCodeSection s = Elf.elfSectionFlags s .&. Elf.shf_execinstr == Elf.shf_execinstr

------------------------------------------------------------------------
-- X86-specific functions

-- | Returns information about the registers needed and modified by a
-- x86 terminal statement.
summarizeX86TermStmt :: SyscallPersonality
                     -> ComputeArchTermStmtEffects X86_64 ids
summarizeX86TermStmt _ Hlt _ =
  ArchTermStmtRegEffects { termRegDemands = []
                         , termRegTransfers = []
                         }
summarizeX86TermStmt _ UD2 _ =
  ArchTermStmtRegEffects { termRegDemands = []
                         , termRegTransfers = []
                         }
summarizeX86TermStmt sysp X86Syscall proc_state = do
  -- Compute the arguments needed by the function
  let argRegs
        | BVValue _ call_no <- proc_state^.boundValue syscall_num_reg
        , Just (_,_,argtypes) <- Map.lookup (fromIntegral call_no) (spTypeInfo sysp) =
            take (length argtypes) syscallArgumentRegs
        | otherwise =
            syscallArgumentRegs
  let callRegs = [Some sp_reg] ++ Set.toList x86CalleeSavedRegs
  ArchTermStmtRegEffects { termRegDemands = Some <$> argRegs
                         , termRegTransfers = callRegs
                         }

x86DemandInfo :: SyscallPersonality
              -> ArchDemandInfo X86_64
x86DemandInfo sysp =
  ArchDemandInfo { functionArgRegs = [Some RAX]
                                     ++ (Some <$> x86ArgumentRegs)
                                     ++ (Some <$> x86FloatArgumentRegs)
                 , functionRetRegs = ((Some <$> x86ResultRegs) ++ (Some <$> x86FloatResultRegs))
                 , calleeSavedRegs = x86CalleeSavedRegs
                 , computeArchTermStmtEffects = summarizeX86TermStmt sysp
                 , demandInfoCtx = x86DemandContext
                 }

------------------------------------------------------------------------
-- LLVMVersion

newtype LLVMVersion = LLVMVersion [Integer]
  deriving (Eq, Ord)

-- | Parse a string as a version
versionOfString :: String -> Maybe LLVMVersion
versionOfString s = do
  let (f,r) = span (/= '.') s
  case (reads f, r) of
    ([(v,"")], [])
      | v >= 0 ->
        if v == 0 then
          Just (LLVMVersion [])
        else
          Just (LLVMVersion [v])
    ([(v,"")], '.':rest)
      | v >= 0 -> do
        LLVMVersion l <- versionOfString rest
        if null l && v == 0 then
          pure (LLVMVersion [])
        else
          pure (LLVMVersion (v : l))
    _ -> Nothing


instance IsString LLVMVersion where
  fromString s =
    case versionOfString s of
      Just v -> v
      Nothing -> error $ "Could not interpret " ++ show s ++ " as a version."

type LLVMConfig = LPP.Config

-- | Configuration for LLVM 3.5 - 3.6
llvm35Config :: LLVMConfig
llvm35Config =
  LPP.Config { LPP.cfgLoadImplicitType = True
             , LPP.cfgGEPImplicitType  = True
             , LPP.cfgUseDILocation    = False
             }

-- | Configuration for LLVM 3.7 & 3.8
latestLLVMConfig :: LLVMConfig
latestLLVMConfig =
  LPP.Config { LPP.cfgLoadImplicitType = False
             , LPP.cfgGEPImplicitType  = False
             , LPP.cfgUseDILocation    = True
             }

llvmVersionMap :: Map LLVMVersion LLVMConfig
llvmVersionMap = Map.fromList
  [ (,) "3.5.0" llvm35Config
  , (,) "3.7.0" latestLLVMConfig
  ]

--  | Get the LLVM LLVM config associated with a version
getLLVMConfig :: LLVMVersion -> Maybe LLVMConfig
getLLVMConfig v = snd <$> Map.lookupLE v llvmVersionMap

-- | Pretty print an LLVM module using the format expected by the given LLVM version.
ppLLVM :: LPP.Config -> L.Module -> HPJ.Doc
ppLLVM c m = LPP.withConfig c $ LPP.ppModule m

------------------------------------------------------------------------
-- Execution

data X86OS = Linux | FreeBSD

instance Show X86OS where
  show Linux = "Linux"
  show FreeBSD = "FreeBSD"

osPersonality :: X86OS -> SyscallPersonality
osPersonality Linux = linux_syscallPersonality
osPersonality FreeBSD = freeBSD_syscallPersonality

osArchitectureInfo :: X86OS -> ArchitectureInfo X86_64
osArchitectureInfo Linux = x86_64_linux_info
osArchitectureInfo FreeBSD = x86_64_freeBSD_info

-- | Return the name to pass the linker for this architecture.
osLinkName :: X86OS -> String
osLinkName Linux = "x86_64-unknown-linux-gnu"
osLinkName FreeBSD = "x86_64-unknown-freebsd-elf"

getX86ElfArchInfo :: Elf.ElfMachine -> Elf.ElfOSABI -> IO X86OS
getX86ElfArchInfo Elf.EM_X86_64 Elf.ELFOSABI_LINUX   = pure Linux
getX86ElfArchInfo Elf.EM_X86_64 Elf.ELFOSABI_SYSV    = pure Linux
getX86ElfArchInfo Elf.EM_X86_64 Elf.ELFOSABI_FREEBSD = pure FreeBSD
getX86ElfArchInfo Elf.EM_X86_64 Elf.ELFOSABI_NETBSD  = pure FreeBSD
getX86ElfArchInfo m abi = do
  hPutStrLn stderr $ printf "Do not support %s %s binaries." (show m) (show abi)
  exitFailure

-- | Map memory addresses to the associated type for that address.
type FunctionTypeMap arch = Map BS.ByteString (FunctionType arch)

-- | This updates the map by adding the types for this function along
-- with the types for all directly referenced functions.
--
-- It calls `error` if any of the functions have inconsistent typing,
-- as this error should have been discovered during function recovery.
getReferencedFunctions :: forall arch
                       .  ( Eq (FunctionType arch)
                          , Show (FunctionType arch)
                          , MemWidth (ArchAddrWidth arch)
                          , FoldableFC (ArchFn arch)
                          , FoldableF (FnArchStmt arch)
                          )
                       => Set.Set BS.ByteString
                       -- ^ Functions with definitions that we do not
                       -- need declarations for.
                       -> FunctionTypeMap arch
                       -> Function arch
                       -> FunctionTypeMap arch
getReferencedFunctions excluded m0 f =
    foldl' (foldFnValue findReferencedFunctions) m0 (fnBlocks f)
  where findReferencedFunctions :: FunctionTypeMap arch
                                -> FnValue arch tp
                                -> FunctionTypeMap arch
        findReferencedFunctions m (FnFunctionEntryValue ft nm) =
          insertAddr nm ft m
        findReferencedFunctions m _ = m

        insertAddr :: BS.ByteString
                   -> FunctionType arch
                   -> FunctionTypeMap arch
                   -> FunctionTypeMap arch
        insertAddr nm ft m
          | Set.member nm excluded = m
          | otherwise =
            case Map.lookup nm m of
              Just ft' | ft /= ft' ->
                         error $ BSC.unpack nm ++ " has incompatible types:\n"
                              ++ show ft  ++ "\n"
                              ++ show ft' ++ "\n"
                       | otherwise -> m
              _ -> Map.insert nm ft m

-- | Attempt to resolve what register to associate with an argument.
resolveArgType :: String  -- ^ Argument name.
               -> AnnType -- ^ Type to resolve
               -> ArgResolver ()
resolveArgType nm tp0 =
  case tp0 of
    VoidAnnType ->
      throwError $ UnsupportedArgType nm (ppAnnType tp0)
    CharAnnType ->
      addGPReg64 nm
    ShortAnnType ->
      addGPReg64 nm
    IntAnnType ->
      addGPReg64 nm
    LongAnnType ->
      addGPReg64 nm
    LongLongAnnType ->
      addGPReg64 nm
    FloatAnnType ->
      throwError $ UnsupportedArgType nm (ppAnnType tp0)
    DoubleAnnType ->
      throwError $ UnsupportedArgType nm (ppAnnType tp0)
    BoolAnnType ->
      addGPReg64 nm
    PtrAnnType _ ->
      addGPReg64 nm
    TypedefAnnType _ tp ->
      resolveArgType nm tp

-- | This parses the types extracted from header function argumnts to
-- the machine code registers that the function will expect.
argsToRegisters :: Int -- ^ Number of arguments processed so far.
                -> V.Vector AnnFunArg
                   -- ^ Remaining arguments to parse
                -> ArgResolver ()
argsToRegisters cnt args
  | cnt >= V.length args = pure ()
  | otherwise = do
      let arg = args V.! cnt
      let nm = fromMaybe ("arg" ++ show cnt) (funArgName arg)
      resolveArgType nm (funArgType arg)
      argsToRegisters (cnt+1) args

parseReturnType :: AnnType -> Either ArgResolverError [Some X86RetInfo]
parseReturnType tp0 =
  case tp0 of
    VoidAnnType      -> Right []
    CharAnnType      -> Right $ [Some (RetBV64 F.RAX)]
    ShortAnnType     -> Right $ [Some (RetBV64 F.RAX)]
    IntAnnType       -> Right $ [Some (RetBV64 F.RAX)]
    LongAnnType      -> Right $ [Some (RetBV64 F.RAX)]
    LongLongAnnType  -> Right $ [Some (RetBV64 F.RAX)]
    FloatAnnType     -> Left  $ UnsupportedReturnType (ppAnnType tp0)
    DoubleAnnType    -> Left  $ UnsupportedReturnType (ppAnnType tp0)
    BoolAnnType      -> Right $ [Some (RetBV64 F.RAX)]
    PtrAnnType _     -> Right $ [Some (RetBV64 F.RAX)]
    TypedefAnnType _ tp -> parseReturnType tp

resolveHdrFunType :: (GetFnsLogEvent -> IO ())
                  -- ^ Logging function for errors
                  -> BSC.ByteString
                  -> AnnFunType
                  -> ExceptT ArgResolverError IO X86FunTypeInfo
resolveHdrFunType _logger _funName funType =
   if funVarArg funType then do
     throwError VarArgsUnsupported
    else do
     args <- runArgResolver (argsToRegisters 0 (funArgs funType))
     ret <- case parseReturnType (funRet funType) of
              Left e -> throwError e
              Right r -> pure r
     pure $! X86NonvarargFunType args ret

-- | This checks whether any of the symbols in the map start with the given string as a prefix.
isUsedPrefix :: BSC.ByteString -> AddrSymMap w -> Bool
isUsedPrefix prefix m = any (\nm -> prefix `BSC.isPrefixOf` nm) (Map.elems m)

-- | Name of recovered function when no function exists.
nosymFunctionName :: BSC.ByteString -> MemSegmentOff w -> BSC.ByteString
nosymFunctionName prefix segOff =
  let addr = segoffAddr segOff
   in prefix <> "_" <> BSC.pack (show (addrBase addr)) <> "_" <> BSC.pack (show (addrOffset addr))

-- | Returns name of recovered function.
recoveredFunctionName :: MemWidth w
                      => AddrSymMap w
                      -- ^ Maps addresses of symbols to the associated symbol name.
                      -> BSC.ByteString
                      -- ^ Prefix to use for automatically generated symbols.
                      -- To be able to distinguish symbols, this should not be
                      -- a prefix for any of the symbols in the map.
                      -> MemSegmentOff w
                      -> BSC.ByteString
recoveredFunctionName m prefix segOff =
  case Map.lookup segOff m of
    Just sym -> sym
    Nothing -> nosymFunctionName prefix segOff

-- | Map x86 function type to known functon abi.
--
-- This is used for global function argument analysis which doesn't yet support vararg
-- functions such as printf.
toKnownFunABI :: X86FunTypeInfo -> [KnownFunABI X86Reg]
toKnownFunABI (X86NonvarargFunType args rets) =
  [KnownFnABI { kfArguments = argReg <$> args
              , kfReturn = viewSome retReg <$> rets
              }
  ]
toKnownFunABI (X86PrintfFunType _) = []
toKnownFunABI X86UnsupportedFunType = []

-- | Resolve annotations on funbction types from C header, and return
-- warnings and the list of functions.
resolveHeaderFuns :: (GetFnsLogEvent -> IO ())
                  -- ^ Logging function for errors
                  -> AnnDeclarations
                  -> IO (Map BSC.ByteString X86FunTypeInfo)
resolveHeaderFuns logger hdr =
  let resolveTypeRegs :: Map BSC.ByteString X86FunTypeInfo
                      -> (BSC.ByteString, AnnFunType)
                      -> IO (Map BSC.ByteString X86FunTypeInfo)
      resolveTypeRegs prev (funName, tp) = do
        mfti <- runExceptT $ resolveHdrFunType logger funName tp
        fti <-
          case mfti of
            Left e -> X86UnsupportedFunType <$ logger (ArgResolverError (BSC.unpack funName) e)
            Right fti -> pure fti
        pure $! Map.insert funName fti prev
   in foldlM resolveTypeRegs Map.empty (Map.toList (funDecls hdr))

-- | Construct function type from demands.
inferFunctionTypeFromDemands :: Map (MemSegmentOff 64) (DemandSet X86Reg)
                             -> Map (MemSegmentOff 64) X86FunTypeInfo
inferFunctionTypeFromDemands dm =
  let go :: DemandSet X86Reg
         -> Map (MemSegmentOff 64) (RegisterSet X86Reg)
         -> Map (MemSegmentOff 64) (RegisterSet X86Reg)
      go ds m = Map.unionWith Set.union (functionResultDemands ds) m

      retDemands :: Map (MemSegmentOff 64) (RegisterSet X86Reg)
      retDemands = foldr go Map.empty dm

      -- drop the suffix which isn't a member of the arg set.  This
      -- allows e.g. arg0, arg2 to go to arg0, arg1, arg2.
      dropArgSuffix :: (a -> X86Reg tp)
                    -> [a]
                    -> RegisterSet X86Reg
                    -> [a]
      dropArgSuffix f regs rs =
        reverse $ dropWhile (not . (`Set.member` rs) . Some . f) $ reverse regs

      -- Turns a set of arguments into a prefix of x86 argument registers and friends
      orderPadArgs :: (RegisterSet X86Reg, RegisterSet X86Reg) -> X86FunTypeInfo
      orderPadArgs (argSet, retSet) =
        let args = fmap ArgBV64   (dropArgSuffix X86_GP     x86GPPArgumentRegs argSet)
                ++ fmap ArgMM512D (dropArgSuffix X86_ZMMReg [0..7] argSet)
            rets = fmap (Some . RetBV64)   (dropArgSuffix X86_GP     [F.RAX, F.RDX] retSet)
                ++ fmap (Some . RetMM512D) (dropArgSuffix X86_ZMMReg [0,1]          retSet)
         in X86NonvarargFunType args rets
  in fmap orderPadArgs
     $ Map.mergeWithKey (\_ ds rets -> Just (registerDemands ds, rets))
                        (fmap (\ds ->  (registerDemands ds, mempty)))
                        (fmap (\s -> (mempty,s)))
                        dm
                        retDemands

-- | Event passed to logger when discovering functions
data GetFnsLogEvent
   = ArgResolverError !String !ArgResolverError
   | DuplicateSections !BSC.ByteString
   | DebugError !String
   | StartFunRecovery !BSC.ByteString
     -- ^ Notify we are starting analysis of given function.
   | RecoveryFailed !BSC.ByteString !String
     -- ^ @RecoveryFailed fnm msg@ notes we failed to recover function due to given reason.
   | RecoveryPLTSkipped !BSC.ByteString
   | RecoveryWarning !BSC.ByteString !String
   | GetFnsError !String
     -- ^ A general error message

instance Show GetFnsLogEvent where
  show (ArgResolverError fnm e)  = printf "Type error on %s: %s" fnm (showArgResolverError e)
  show (DuplicateSections nm)  = "Multiple sections named " ++ BSC.unpack nm
  show (DebugError msg)        = msg
  show (StartFunRecovery fnm)  = "Recovering function " ++ BSC.unpack fnm
  show (RecoveryFailed _ msg)  = "  Failed:  " ++ msg
  show (RecoveryPLTSkipped _)  = "  Skipped PLT stub"
  show (RecoveryWarning _ msg) = "  Warning: " ++ msg
  show (GetFnsError msg) = msg

defaultX86Type :: X86FunTypeInfo
defaultX86Type = X86NonvarargFunType args rets
  where args = fmap ArgBV64 x86GPPArgumentRegs ++ fmap ArgMM512D [0..7]
        rets = [ Some (RetBV64 F.RAX), Some (RetBV64 F.RDX) ]


$(pure [])

-- | Function type information parsed from annotations or debug information.
data FunTypeMaps =
  FunTypeMaps { macawMemory :: !(Memory 64)
                -- ^ Memory
              , dwarfBaseCodeAddr :: !(MemAddr 64)
                -- ^ Address to add to all code offsets in dwarf file.
              , nameToAddrMap :: !(SymAddrMap 64)
                -- ^ Map from symbol names to the address.
              , nameTypeMap :: !(Map BS.ByteString X86FunTypeInfo)
                -- ^ Map from external undefined symbol names to type.
              , addrTypeMap :: !(Map (MemSegmentOff 64) X86FunTypeInfo)
                -- ^ Map from code addresses that are start of function
                -- to type.
              , noreturnMap :: !(Map (MemSegmentOff 64) NoReturnFunStatus)
              }

$(pure [])

-- | Add a
addCheckExisting :: Ord k
                 => (GetFnsLogEvent -> IO ())
                 -> String
                 -> k
                 -> X86FunTypeInfo
                 -> Map k X86FunTypeInfo
                 -> IO (Map k X86FunTypeInfo)
addCheckExisting logger nm k v m =
  case Map.lookup k m of
    Nothing ->
      pure $! Map.insert k v m
    Just pv -> do
      when (pv /= v) $ do
        logger $ GetFnsError $ printf "%s assigned incompatible types.\nPrev:\n%s\nNew:\n%s" nm (show pv) (show v)
      pure m

-- | Add a new function type to a function
funTypeIsDefined :: FunTypeMaps -- ^ Current type map information
                 -> Maybe BS.ByteString -- ^ External name of function if not defined
                 -> Maybe (MemSegmentOff 64)
                 -> Bool
funTypeIsDefined funTypeMaps msym maddr = do
  let symDef = case msym of
                 Nothing -> True
                 Just sym -> Map.member sym (nameTypeMap funTypeMaps)
      addrDef = case maddr of
                  Nothing -> True
                  Just addr -> Map.member addr (addrTypeMap funTypeMaps)
   in symDef && addrDef

-- | Add a new function type to a function
addNamedFunType :: (GetFnsLogEvent -> IO ()) -- ^ Logging function for recording errors.
                -> FunTypeMaps -- ^ Current type map information
                -> String -- ^ Name of entry for logging purposes.
                -> Maybe BS.ByteString -- ^ External name of function if not defined
                -> Maybe (MemSegmentOff 64)
                -> X86FunTypeInfo
                -> IO FunTypeMaps
addNamedFunType logger funTypeMaps loggingName msym maddr annType = do
  funTypeMaps' <-
    case msym of
      Nothing ->
        pure $! funTypeMaps
      Just sym -> do
        ntm <- addCheckExisting logger loggingName sym annType (nameTypeMap funTypeMaps)
        pure $! funTypeMaps { nameTypeMap = ntm }
  case maddr of
    Nothing -> do
      pure $ funTypeMaps'
    Just addr -> do
      let nm = loggingName ++ " address"
      atm <- addCheckExisting logger nm addr annType (addrTypeMap funTypeMaps)
      pure $! funTypeMaps' { addrTypeMap = atm }

$(pure [])

throwDwarfTypeError :: MonadError ArgResolverError m => Dwarf.TypeRef -> String -> m a
throwDwarfTypeError ref msg =
  throwError $ DebugResolveError $ printf "Bad type ref 0x%x: %s" (Dwarf.typeRefFileOffset ref) msg

resolveDwarfTypeRef :: (MonadIO m, MonadError ArgResolverError m)
                    => (GetFnsLogEvent -> IO ())
                    -> Map Dwarf.TypeRef Dwarf.AbsType
                       -- ^ Logging function for errors
                    -> Dwarf.TypeRef
                    -> m Dwarf.TypeApp
resolveDwarfTypeRef logger typeMap ref = do
  case Map.lookup ref typeMap of
    Nothing -> do
      let o = Dwarf.typeRefFileOffset ref
      throwDwarfTypeError ref $ printf "Could not find type %x." o
    Just (Left msg, warnings) -> do
      liftIO $ mapM_ (logger . DebugError) warnings
      throwDwarfTypeError ref $ printf "Dwarf parsing error: %s" msg
    Just (Right tp, warnings) -> do
      liftIO $ mapM_ (logger . DebugError) warnings
      pure tp

-- | Resolve Dwarf arg types
resolveDwarfArgType :: (GetFnsLogEvent -> IO ())
                     -- ^ Logging function for errors
                     -> Map Dwarf.TypeRef Dwarf.AbsType
                     -> String -- ^ Name of argument
                     -> Dwarf.TypeRef
                     -> ArgResolver ()
resolveDwarfArgType logger typeMap nm ref = do
  tp <- resolveDwarfTypeRef logger typeMap ref
  case tp of
    Dwarf.BoolType -> do
      addGPReg64 nm
    Dwarf.UnsignedIntType byteCount -> do
      when (byteCount > 8) $ do
        throwDwarfTypeError ref $ printf "Byte count %s too large." (show byteCount)
      addGPReg64 nm
    Dwarf.SignedIntType byteCount -> do
      when (byteCount > 8) $ do
        throwDwarfTypeError ref $ printf "Byte count %s too large." (show byteCount)
      addGPReg64 nm
    Dwarf.FloatType -> do
      throwDwarfTypeError ref "Float is not supported."
    Dwarf.DoubleType -> do
      throwDwarfTypeError ref "Double is not supported."
    Dwarf.LongDoubleType -> do
      throwDwarfTypeError ref "Long double is not supported."
    Dwarf.UnsignedCharType -> do
      addGPReg64 nm
    Dwarf.SignedCharType -> do
      addGPReg64 nm
    Dwarf.ArrayType _ _ -> do
      throwDwarfTypeError ref "Array arguments are not supported."
    Dwarf.PointerType _ _ -> do
      addGPReg64 nm
    Dwarf.StructType _ -> do
      throwDwarfTypeError ref "Struct arguments are not supported."
    Dwarf.UnionType _ -> do
      throwDwarfTypeError ref "Union arguments are not supported."
    Dwarf.EnumType d ->
      case Dwarf.enumDeclType d of
        Just r -> resolveDwarfArgType logger typeMap nm r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for enum at " ++ show ref
    Dwarf.SubroutinePtrType _ -> do
      addGPReg64 nm
    Dwarf.TypedefType d -> do
      resolveDwarfArgType logger typeMap nm (Dwarf.typedefType d)
    Dwarf.TypeQualType ann -> do
      case Dwarf.tqaType ann of
        Just r -> resolveDwarfArgType logger typeMap nm r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for qualifier at " ++ show ref
    Dwarf.SubroutineTypeF _ -> do
      throwDwarfTypeError ref "Subroutines may not be passed as arguments."
        --        _ -> Error $ printf "%s do not yet support arg.\n%s" nm (show tp)

-- | Resolve Dwarf arg types
resolveDwarfArgTypes :: (GetFnsLogEvent -> IO ())
                     -- ^ Logging function for errors
                     -> Maybe Dwarf.Subprogram
                        -- ^ Origin subprogram if defined
                     -> Map Dwarf.TypeRef Dwarf.AbsType
                     -> Int -- ^ Number of arguments passed so far.
                     -> [Dwarf.Variable]
                     -> ArgResolver ()
resolveDwarfArgTypes _ _morigin _typeMap _cnt [] = pure ()
resolveDwarfArgTypes logger morigin typeMap cnt (a:r) = seq cnt $ do
  let nm | Dwarf.varName a == "" = "arg" ++ show cnt
         | otherwise = BSC.unpack (Dwarf.nameVal (Dwarf.varName a))
  case Dwarf.varType a of
    Nothing ->
      case (morigin, Dwarf.varOrigin a) of
        (Nothing, Nothing) -> throwError $ MissingArgType nm
        (Nothing, Just _) -> throwError $ DebugResolveError $ "Missing subroutine abstract origin."
        (Just _, Nothing) -> throwError $ DebugResolveError $ "Missing argument abstract origin."
        (Just subOrigin, Just varOrigRef) -> do
          varOrig <-
            case Map.lookup varOrigRef (Dwarf.subParamMap subOrigin) of
              Nothing -> throwError $ DebugResolveError $ printf "Could not find variable origin %s for %s." (show (pretty varOrigRef)) nm
              Just o -> pure o

          -- Get origin ref
          case Dwarf.varType varOrig of
            Nothing -> throwError $ MissingArgType nm
            Just ref -> resolveDwarfArgType logger (Dwarf.subTypeMap subOrigin) nm ref
    Just ref -> resolveDwarfArgType logger typeMap nm ref
  resolveDwarfArgTypes logger morigin typeMap (cnt+1) r

retGPReg64 :: ExceptT ArgResolverError IO [Some X86RetInfo]
retGPReg64 = pure [Some (RetBV64 F.RAX)]

resolveDwarfRetType :: (GetFnsLogEvent -> IO ())
                       -- ^ Logging function for errors
                    -> Map Dwarf.TypeRef Dwarf.AbsType
                    -> Dwarf.TypeRef
                    -> ExceptT ArgResolverError IO [Some X86RetInfo]
resolveDwarfRetType logger typeMap ref = do
  tp <- resolveDwarfTypeRef logger typeMap ref
  case tp of
    Dwarf.BoolType -> do
      retGPReg64
    Dwarf.UnsignedIntType byteCount -> do
      when (byteCount > 8) $ do
        throwDwarfTypeError ref $ printf "Byte count %s too large." (show byteCount)
      retGPReg64
    Dwarf.SignedIntType byteCount -> do
      when (byteCount > 8) $ do
        throwDwarfTypeError ref $ printf "Byte count %s too large." (show byteCount)
      retGPReg64
    Dwarf.FloatType -> do
      throwDwarfTypeError ref "Float return type is not supported."
    Dwarf.DoubleType -> do
      throwDwarfTypeError ref "Double return type is not supported."
    Dwarf.LongDoubleType -> do
      throwDwarfTypeError ref "Long double return type is not supported."
    Dwarf.UnsignedCharType -> do
      retGPReg64
    Dwarf.SignedCharType -> do
      retGPReg64
    Dwarf.ArrayType _ _ -> do
      throwDwarfTypeError ref "Array arguments are not supported."
    Dwarf.PointerType _ _ -> do
      retGPReg64
    Dwarf.StructType _ -> do
      throwDwarfTypeError ref "Struct arguments are not supported."
    Dwarf.UnionType _ -> do
      throwDwarfTypeError ref "Union arguments are not supported."
    Dwarf.EnumType d ->
      case Dwarf.enumDeclType d of
        Just r -> resolveDwarfRetType logger typeMap r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for enum at " ++ show ref
    Dwarf.SubroutinePtrType _ -> do
      retGPReg64
    Dwarf.TypedefType d -> do
      resolveDwarfRetType logger typeMap (Dwarf.typedefType d)
    Dwarf.TypeQualType ann -> do
      case Dwarf.tqaType ann of
        Just r -> resolveDwarfRetType logger typeMap r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for qualifier at " ++ show ref
    Dwarf.SubroutineTypeF _ -> do
      throwDwarfTypeError ref "Subroutines may not be passed as return values."

-- | @resolveDwarfSubprogramDebugName nm isExt o@ resolve the name to
-- use for subprogram with the given name and offset.
--
-- This returns nothing if name is empty and ext is true or
-- ext if false and the address is empty.
resolveDwarfSubprogramDebugName :: Dwarf.Subprogram -- ^ Subprogram
                                -> Maybe Word64 -- ^ Offset of subprogram.
                                -> Maybe String
resolveDwarfSubprogramDebugName sub moff
  | Dwarf.subExternal sub =
    if Dwarf.subName sub == "" then
      Nothing
     else
      Just $! BSC.unpack (Dwarf.nameVal (Dwarf.subName sub))
  | otherwise =
    case moff of
      Nothing -> Nothing
      Just o ->
        let nmVal :: String
            nmVal | Dwarf.subName sub == "" = "Unnamed function"
                  | otherwise = BSC.unpack (Dwarf.nameVal (Dwarf.subName sub))
         in Just $! printf "%s (0x%x)" nmVal (toInteger o)

-- | Resolve the type of a Dwarf subprogram
resolveDwarfSubprogramFunType :: (GetFnsLogEvent -> IO ())
                              -- ^ Logging function for errors
                              -> Dwarf.Subprogram
                              -> Maybe Dwarf.Subprogram -- ^ Origin if subprogram is generated from another.
                              -> ExceptT ArgResolverError IO X86FunTypeInfo
resolveDwarfSubprogramFunType logger sub morigin = do
  when (Dwarf.subUnspecifiedParams sub) $
    throwError $ VarArgsUnsupported
  argTypes <- runArgResolver $ resolveDwarfArgTypes logger morigin (Dwarf.subTypeMap sub) 0 (Map.elems (Dwarf.subParamMap sub))
  retTypes <-
    case Dwarf.subRetType sub of
      Nothing ->
        case morigin of
          Nothing -> pure []
          Just origin ->
            case Dwarf.subRetType origin of
              Nothing -> pure []
              Just ref -> resolveDwarfRetType logger (Dwarf.subTypeMap origin) ref
      Just ref -> resolveDwarfRetType logger (Dwarf.subTypeMap sub) ref

  pure $! X86NonvarargFunType argTypes retTypes


-- | Get name as an external symbol
dwarfExternalName :: Dwarf.Subprogram -> Maybe BS.ByteString
dwarfExternalName sub
  | Dwarf.subExternal sub, Dwarf.subName sub /= "" = Just $ Dwarf.nameVal $ Dwarf.subName sub
  | otherwise = Nothing

dwarfSubEntry :: Dwarf.Subprogram -> Maybe Word64
dwarfSubEntry sub =
  case Dwarf.subEntryPC sub of
    Just e -> Just e
    Nothing -> Dwarf.subLowPC =<< Dwarf.subDef sub

-- | Resolve type information from subroutine.
resolveSubprogramType :: (GetFnsLogEvent -> IO ())
                      -- ^ Logging function for errors
                      -> Dwarf.CompileUnit
                      -- ^ Compile unit for this sub program
                      -> FunTypeMaps
                      -- ^ Annotations from source file
                      -> Dwarf.Subprogram
                      -- ^ Dwarf function information
                      -> Maybe (MemSegmentOff 64)
                         -- Address
                      -> IO FunTypeMaps
resolveSubprogramType logger cu annMap sub entryAddr
  -- Non-defining subprograms are skipped.
  | Dwarf.subIsDeclaration sub = do
      pure annMap
    -- Var args functions have a special usage.
  | Dwarf.subUnspecifiedParams sub = do
      -- Get name as an external symbol
      let externalName :: Maybe BS.ByteString
          externalName = dwarfExternalName sub
      -- Get entry address in terms of memory.
      case resolveDwarfSubprogramDebugName sub (dwarfSubEntry sub) of
        Nothing -> pure annMap
        Just debugName -> do
          when (not (funTypeIsDefined annMap externalName entryAddr)) $ do
            logger $ ArgResolverError debugName VarArgsUnsupported
          pure annMap
  | otherwise = do
      -- Get name as an external symbol
      let externalName :: Maybe BS.ByteString
          externalName = dwarfExternalName sub
      -- Get origin if this is an inlined or specialized instance of a source subprogram.
      let emorigin =
            case Dwarf.subOrigin sub of
              Nothing -> Right Nothing
              Just originRef ->
                case Map.lookup originRef (Dwarf.cuSubprogramMap cu) of
                  Nothing -> Left $ "Could not find origin " ++ show (pretty originRef)
                  Just r -> Right (Just r)
      case emorigin of
        Left err -> do
          logger $ DebugError err
          pure annMap
        Right morigin ->
          case resolveDwarfSubprogramDebugName sub (dwarfSubEntry sub) of
            Nothing -> pure annMap
            Just debugName -> do
              mfunType <- runExceptT $ resolveDwarfSubprogramFunType logger sub morigin
              funType <-
                case mfunType of
                  Left e -> do
                    logger $ ArgResolverError debugName e
                    pure $! X86UnsupportedFunType
                  Right funType ->
                    pure funType
              addNamedFunType logger annMap debugName externalName entryAddr funType

-- | Resolve type information from subroutine.
resolveSubprogram :: (GetFnsLogEvent -> IO ())
                  -- ^ Logging function for errors
                  -> Dwarf.CompileUnit
                  -- ^ Compile unit for this sub program
                  -> FunTypeMaps
                  -- ^ Annotations from source file
                  -> Dwarf.Subprogram
                  -- ^ Elf file for header information
                  -> IO FunTypeMaps
resolveSubprogram logger cu annMap sub = do
  -- Get entry address in terms of memory.
  let mem = macawMemory annMap
  entryAddr <-
    case dwarfSubEntry sub of
      Nothing -> pure Nothing
      Just entry -> do
        let adjEntry = incAddr (toInteger entry) (dwarfBaseCodeAddr annMap)
        case asSegmentOff mem adjEntry of
          Nothing -> do
            let dwarfName = Dwarf.subName sub
            let debugName | dwarfName == "" = "Unnamed symbol"
                          | otherwise = BSC.unpack (Dwarf.nameVal dwarfName)
            logger $ DebugError $ printf "%s invalid debug address %s." debugName (show adjEntry)
            pure Nothing
          Just a ->
            pure (Just a)
  annMap' <- resolveSubprogramType logger cu annMap sub entryAddr
  case entryAddr of
    Nothing -> pure annMap'
    Just entry -> do
      let val | Dwarf.subNoreturn sub = NoReturnFun
              | otherwise = MayReturnFun
      let fn NoReturnFun _ = NoReturnFun
          fn _ NoReturnFun = NoReturnFun
          fn _ _ = MayReturnFun
      pure $ annMap' { noreturnMap = Map.insertWith fn entry val (noreturnMap annMap') }

-- | Add all compile units in plugin
resolveCompileUnits :: (GetFnsLogEvent -> IO ())
                    -- ^ Logging function for errors
                    -> FunTypeMaps
                    -- ^ Map from function names to type info.
                    -> Maybe (Either String Dwarf.CUContext)
                    -- ^ Elf file for header information
                    -> IO FunTypeMaps
resolveCompileUnits _logge annMap Nothing = do
  pure annMap
resolveCompileUnits logger annMap (Just (Left e)) = do
  logger (DebugError e)
  pure annMap
resolveCompileUnits logger annMap (Just (Right ctx)) = do
  let (mcr, warnings) = Dwarf.getCompileUnit ctx
  mapM_ (logger . DebugError) (reverse warnings)
  case mcr of
    Left msg -> do
      logger (DebugError msg)
      resolveCompileUnits logger annMap (Dwarf.nextCUContext ctx)
    Right cu -> do
      annMap' <- foldlM (resolveSubprogram logger cu) annMap (Dwarf.cuSubprograms cu)
      resolveCompileUnits logger annMap' (Dwarf.nextCUContext ctx)

-- | Extend function types with header information.
resolveDebugFunTypes :: (GetFnsLogEvent -> IO ())
                     -- ^ Logging function for errors
                     -> FunTypeMaps
                     -- ^ Annotations from source file
                     -> Elf.ElfHeaderInfo 64
                     -- ^ Elf file for header information
                     -> IO FunTypeMaps
resolveDebugFunTypes logger annMap elfInfo = do
  let hdr = Elf.header elfInfo
  let secDataMap :: Map BS.ByteString [(Elf.Range (Elf.ElfWordType 64), Elf.ElfSection (Elf.ElfWordType 64))]
      secDataMap = Map.fromListWith (++)
        [ (Elf.elfSectionName sec, [(r,sec)])
        | (r,sec) <- V.toList (Elf.headerSections elfInfo)
        ]
  case Map.findWithDefault [] ".debug_info" secDataMap of
    [] -> do
      -- No debug information
      pure annMap
    _:_ -> do
      let end =
            case Elf.headerData hdr of
              Elf.ELFDATA2LSB -> Dwarf.LittleEndian
              Elf.ELFDATA2MSB -> Dwarf.BigEndian
      sections <- Dwarf.mkSections $ \nm ->
        case Map.findWithDefault [] nm secDataMap of
          [] -> pure BS.empty
          (_, s):r -> do
            when (not (null r)) $ logger (DuplicateSections nm)
            pure $! Elf.elfSectionData s
      resolveCompileUnits logger annMap (Dwarf.firstCUContext end sections)

-- | Analyze an elf binary to extract information.
--
--  Note. This prints warnings to stderr and may exit if the Elf file cannot be parsed.
discoverX86Elf :: (GetFnsLogEvent -> IO ())
               -- ^ Logging function for errors
               -> FilePath
               -- ^ Path to binary for exploring CFG
               -> LoadOptions
               -- ^ Option to load the binary at the given address
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> [String] -- ^ Included addresses (if empty then all addresses included)
               -> [String] -- ^ Excluded addresses
               -> AnnDeclarations
               -- ^ Header with hints for assisting typing.
               -> BSC.ByteString -- ^ Prefix to use if we need to generate new function endpoints later.
               -> IO ( Elf 64
                     , X86OS
                     , DiscoveryState X86_64
                     , RecoveredModule X86_64
                     )
discoverX86Elf logger path loadOpts disOpt includeAddr excludeAddr hdrAnn unnamedFunPrefix = do
  bs <- checkedReadFile path
  hdrInfo <-
    case Elf.parseElfHeaderInfo bs of
      Left (_, msg) -> do
        hPutStrLn stderr $ "Could not parse Elf file " ++ path ++ ":"
        hPutStrLn stderr $ "  " ++ msg
        exitFailure
      Right (Elf32 _) -> do
        hPutStrLn stderr "32-bit elf files are not yet supported."
        exitFailure
      Right (Elf64 hdrInfo) -> do
        pure hdrInfo
  let hdr = Elf.header hdrInfo
  os <- getX86ElfArchInfo (Elf.headerMachine hdr) (Elf.headerOSABI hdr)

  -- Resolve function type annotations from header, and report rwarnings.
  hdrTypeMap <- resolveHeaderFuns logger hdrAnn

  -- Generate type information from annotations
  let nameAnnTypeMap
        = hdrTypeMap
        <> Map.singleton "printf"   (X86PrintfFunType 0)
        <> Map.singleton "fprintf"  (X86PrintfFunType 1)
        <> Map.singleton "sprintf"  (X86PrintfFunType 1)
        <> Map.singleton "snprintf" (X86PrintfFunType 2)

  -- Create initial state
  let ainfo = osArchitectureInfo os
  let procPLT = processX86PLTEntries
  initState <-
    initDiscovery loadOpts hdrInfo ainfo procPLT includeAddr excludeAddr
  let e = initDiscElf initState
      readyState = initDiscState initState
      addrSymMap = initDiscAddrSymMap initState
      symAddrMap = initDiscSymAddrMap initState
      pltFuns = initDiscPLTFuns initState
      fnPred = initDiscPred initState

  -- Generate map from address names to known type.
  --
  -- This is used when we see a function jumps to a defined address.
  addrAnnTypeMap <- do
    let insSymType :: Map (MemSegmentOff 64) X86FunTypeInfo
                   -> (BS.ByteString, X86FunTypeInfo)
                   -> IO (Map (MemSegmentOff 64) X86FunTypeInfo)
        insSymType m (sym,annTp) = do
          case symAddrMapLookup symAddrMap sym of
            Left SymAddrMapNotFound -> do
              -- Silently drop symbols without addresses as they may be undefined.
              pure m
            Left SymAddrMapAmbiguous -> do
              logger $ GetFnsError $ "Ambiguous symbol " ++ BSC.unpack sym ++ "."
              pure m
            Right addr -> do
              pure $! Map.insert addr annTp m
    foldlM insSymType Map.empty (Map.toList nameAnnTypeMap)

  let mem = memory readyState

  let annTypeMap = FunTypeMaps
        { macawMemory       = mem
        , dwarfBaseCodeAddr = initDiscBaseCodeAddr initState
        , nameToAddrMap     = symAddrMap
        , nameTypeMap = nameAnnTypeMap
        , addrTypeMap = addrAnnTypeMap
        , noreturnMap = readyState^.trustedFunctionEntryPoints
        }

  -- Resolve debug information.
  debugTypeMap <- resolveDebugFunTypes logger annTypeMap hdrInfo

  when (isUsedPrefix unnamedFunPrefix addrSymMap) $ do
    hPutStrLn stderr $
      printf "No symbol in the binary may start with the prefix %d."
             (BSC.unpack unnamedFunPrefix)
    exitFailure

  -- Run discovery
  let postDebugState = readyState & trustedFunctionEntryPoints .~ noreturnMap debugTypeMap
  discState <- completeDiscoveryState postDebugState disOpt fnPred

  let sysp = osPersonality os

  -- Compute the function demands
  let fDems =
        -- Generate map from symbol names to known type.
        let symFunABIMap :: Map BS.ByteString (KnownFunABI X86Reg)
            symFunABIMap = Map.fromList $
              [ (nm, tp)
              | (nm, annTp) <- Map.toList (nameTypeMap debugTypeMap)
              , tp <- toKnownFunABI annTp
              ]
            -- Generate map from address names to known type.
            --
            -- This is used when we see a function jumps to a defined address.
            addrFunABIMap = Map.fromList $
              [ (addr,tp)
              | (addr, annTp) <- Map.toList (addrTypeMap debugTypeMap)
              , tp <- toKnownFunABI annTp
              ]
            -- Compute only those functions whose types are not known.
            notKnown (Some f) = not (Map.member (discoveredFunAddr f) addrFunABIMap)
         in inferFunctionTypeFromDemands $
              functionDemands (x86DemandInfo sysp) addrFunABIMap symFunABIMap mem $
                filter notKnown $ exploredFunctions discState

  let funNameMap ::  Map (MemSegmentOff 64) BS.ByteString
      funNameMap = addrSymMap
                <> Map.fromList [ (addr, nosymFunctionName unnamedFunPrefix addr)
                                | Some finfo <- exploredFunctions discState
                                , let addr = discoveredFunAddr finfo
                                , Map.notMember addr addrSymMap
                                ]
  let funTypeMap ::  Map BS.ByteString X86FunTypeInfo
      funTypeMap = nameTypeMap debugTypeMap
                <> Map.fromList [ (recoveredFunctionName addrSymMap unnamedFunPrefix addr, tp)
                                | (addr,tp) <- Map.toList (addrTypeMap debugTypeMap)
                                ]
                <> Map.fromList [ (recoveredFunctionName addrSymMap unnamedFunPrefix addr, tp)
                                | (addr,tp) <- Map.toList fDems
                                ]
                <> Map.fromList [ (nm, defaultX86Type) | (_addr,nm) <- pltFuns ]
  fnDefs <- fmap catMaybes $
    forM (exploredFunctions discState) $ \(Some finfo) -> do
      let fnm = discoveredFunName finfo
      logger $ StartFunRecovery fnm
      case checkFunction finfo of
        FunctionOK -> do
          case recoverFunction sysp funNameMap funTypeMap mem finfo of
            Left msg -> do
              logger $ RecoveryFailed fnm msg
              pure Nothing
            Right (warnings, fn) -> do
              mapM_ (logger . RecoveryWarning fnm) warnings
              pure (Just fn)
        FunctionHasPLT -> do
          logger $ RecoveryPLTSkipped fnm
          -- Skip PLT functions with no error message.
          pure Nothing
        FunctionIncomplete -> do
          logger $ RecoveryFailed fnm $ "Incomplete discovery."
          pure Nothing
  -- Get list of addresses included in this set.
  let excludedSet = Set.fromList $ recoveredFunctionName addrSymMap unnamedFunPrefix . fnAddr <$> fnDefs

  -- Get all functions that are referenced, but not defined in the module.
  let declFunTypeMap :: FunctionTypeMap X86_64
      declFunTypeMap = foldl (getReferencedFunctions excludedSet) Map.empty fnDefs

  let fnDecls =  [ FunctionDecl { funDeclName = nm
                                , funDeclType = tp
                                }
                 | (nm, tp) <- Map.toList declFunTypeMap
                 ]
  let recMod = RecoveredModule { recoveredDecls = fnDecls
                               , recoveredDefs  = fnDefs
                               }
  seq recMod $ pure (e, os, discState, recMod)

-- | Produce a LLVM textual rendering of the module for the LLVM version.
llvmAssembly :: LLVMArchSpecificOps X86_64
                -- ^ architecture specific functions
             -> LLVMGenOptions
                -- ^ Options for generating LLVM
             -> RecoveredModule X86_64
                -- ^ Module to generate
             -> LPP.Config
             -> (Builder.Builder, [Either String Ann.FunctionAnn])
llvmAssembly archOps genOpts recMod cfg =
      -- Generate LLVM module
   let (m,ann) = moduleForFunctions archOps genOpts recMod
       -- Render into LLVM
       out = HPJ.fullRender HPJ.PageMode 10000 1 pp mempty (ppLLVM cfg m)
    in (out, ann)
  where pp :: HPJ.TextDetails -> Builder.Builder -> Builder.Builder
        pp (HPJ.Chr c)  b = Builder.charUtf8 c <> b
        pp (HPJ.Str s)  b = Builder.stringUtf8 s <> b
        pp (HPJ.PStr s) b = Builder.stringUtf8 s <> b

--------------------------------------------------------------------------------
-- Compile the LLVM

-- | Compile a bytestring containing LLVM assembly or bitcode into an object.
--
-- This writes to standard out and throws an error.
compileLLVM :: Int -- ^ Optimization level
            -> FilePath -- ^ Path to LLVM `opt` command
            -> FilePath -- ^ Path to llc
            -> FilePath -- ^ Path to llvm-mc
            -> String   -- ^ Architure triple to pass to LLC
            -> Builder.Builder -- ^ Representiation of LLVM to serialize
            -> IO BS.ByteString
compileLLVM optLevel optPath llcPath llvmMcPath arch llvm = do
  -- Run llvm on resulting binary
  mres <- runExceptT $ do
    -- Skip optimization if optLevel == 0
    llvm_opt <-
      if optLevel /= 0 then do
        Ext.run_opt optPath optLevel $ \inHandle -> do
          Builder.hPutBuilder inHandle llvm
       else
        pure $ BSL.toStrict $ Builder.toLazyByteString llvm
    let llc_opts = Ext.LLCOptions { Ext.llc_triple    = Just arch
                                  , Ext.llc_opt_level = optLevel
                                  }
    asm <- Ext.run_llc llcPath llc_opts llvm_opt
    Ext.runLlvmMc llvmMcPath asm arch

  case mres of
    Left f -> do
      hPutStrLn stderr (show f)
      exitFailure
    Right b -> return b

--------------------------------------------------------------------------------
-- ControlFlowTargetMap

-- | A map from all control flow targets in the program to the start address of
-- functions that may target them.
--
-- This is used to compute when it is safe to insert a redirection.  We want to
-- ensure that adding a redirection will not break unknown functions..
newtype ControlFlowTargetSet w = CFTS { cfTargets :: Map (MemSegmentOff w) [MemSegmentOff w]
                                      }

-- | Return how many bytes of space there are to write after address without
-- ovewriting another control flow target.
lookupControlFlowTargetSpace :: forall w
                             .  MemWidth w
                             => MemSegmentOff w
                             -> ControlFlowTargetSet w
                             -> MemWord w
lookupControlFlowTargetSpace addr0 = go 0 addr0 addr0
  where seg = segoffSegment addr0
        go :: MemWord w -> MemSegmentOff w -> MemSegmentOff w -> ControlFlowTargetSet w -> MemWord w
        go inc base addr s =
          case Map.lookupGT addr (cfTargets s) of
            Just (next,fns)
              | segoffSegment addr == segoffSegment next ->
                let d = segoffOffset next - segoffOffset addr
                 in if null (filter (/= base) fns) then
                      go (inc+d) base next s
                     else
                      inc+d
            _ ->
              if segmentSize seg >= segoffOffset addr then
                segmentSize seg - segoffOffset addr
               else
                0

addControlFlowTarget :: ControlFlowTargetSet w
                     -> MemSegmentOff w
                     -> MemSegmentOff w -- ^ Function entry point
                     -> ControlFlowTargetSet w
addControlFlowTarget m a f = m { cfTargets = Map.insertWith (++) a [f] (cfTargets m) }

addFunctionEntryPoint :: ControlFlowTargetSet w
                      -> MemSegmentOff w
                      -> ControlFlowTargetSet w
addFunctionEntryPoint s a = addControlFlowTarget s a a

addFunDiscoveryControlFlowTargets :: ControlFlowTargetSet (ArchAddrWidth arch)
                                  -> Some (DiscoveryFunInfo arch)
                                  -> ControlFlowTargetSet (ArchAddrWidth arch)
addFunDiscoveryControlFlowTargets m0 (Some f) =
  foldl' (\m b -> addControlFlowTarget m b (discoveredFunAddr f)) m0 (Map.keys (f^.parsedBlocks))

discoveryControlFlowTargets :: DiscoveryState arch -> ControlFlowTargetSet (ArchAddrWidth arch)
discoveryControlFlowTargets info =
  let m0 = CFTS { cfTargets = Map.empty }
      m = foldl' addFunDiscoveryControlFlowTargets m0 (exploredFunctions info)
   in foldl' addFunctionEntryPoint m (Map.keys (symbolNames info))

-- | This creates a code redirection or returns the address as failing.
addrRedirection :: ControlFlowTargetSet 64
                -> Function X86_64
                -> CodeRedirection Word64
addrRedirection tgts f = do
  let a = fnAddr f
  let off :: Word64
      off = fromIntegral (segmentOffset (segoffSegment a)) + fromIntegral (segoffOffset a)
   in CodeRedirection { redirSourceVAddr = off
                      , redirSourceSize = fromIntegral (lookupControlFlowTargetSpace (fnAddr f) tgts)
                      , redirTarget = fnName f
                      }

-- | This generates redirections from original functions to a list of new functions.
addrRedirections :: DiscoveryState X86_64
                 -> [Function X86_64]
                 -> [CodeRedirection Word64]
addrRedirections s fns =
  let tgts = discoveryControlFlowTargets s
   in addrRedirection tgts <$> fns

-- | Merge a binary and new object
mergeAndWrite' :: FilePath
               -> Elf 64 -- ^ Original binary
              -> Elf 64 -- ^ New object
              -> [CodeRedirection Word64]
              -- ^ List of redirections from old binary to new.
              -> IO ()
mergeAndWrite' output_path orig_binary new_obj redirs = do

  let mres = mergeObject orig_binary new_obj redirs x86_64_immediateJump
  case mres of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right new_binary -> do
      BSL.writeFile output_path $ Elf.renderElf new_binary
      -- Update the file mode
      do fs <- getFileStatus output_path
         let fm = ownerExecuteMode
               .|. groupExecuteMode
               .|. otherExecuteMode
         setFileMode output_path (fileMode fs `unionFileModes` fm)

-- | Merge a binary and new object
mergeAndWrite :: FilePath
              -> Elf 64 -- ^ Original binary
              -> Elf 64 -- ^ New object
              -> [CodeRedirection Word64]
              -- ^ List of redirections from old binary to new.
              -> IO ()
mergeAndWrite output_path orig_binary new_obj redirs = do
  let mres = mergeObject orig_binary new_obj redirs x86_64_immediateJump
  case mres of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right new_binary -> do
      BSL.writeFile output_path $ Elf.renderElf new_binary
      -- Update the file mode
      do fs <- getFileStatus output_path
         let fm = ownerExecuteMode
               .|. groupExecuteMode
               .|. otherExecuteMode
         setFileMode output_path (fileMode fs `unionFileModes` fm)
