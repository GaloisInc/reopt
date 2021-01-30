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
  , parseElfHeaderInfo64
  , showPaddedHex
  , checkedReadFile
  , isCodeSection
  , printX86SectionDisassembly
    -- * Architecture info
  , SomeArchitectureInfo(..)
    -- * Code discovery
  , ReoptOptions(..)
  , discoverBinary
    -- * Function recovery
  , Reopt.AnnotatedTypes.AnnDeclarations
  , Reopt.AnnotatedTypes.emptyAnnDeclarations
  , Reopt.Header.parseHeader
  , RecoveredModule(..)
  , GetFnsLogEvent(..)
    -- * Object merging
  , Reopt.Relinker.MergeRelations
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
  , recoverX86Elf
    -- * Utility
  , copyrightNotice
    -- * Re-exports
  , Data.Macaw.Memory.ElfLoader.LoadOptions(..)
  ) where

import           Control.Monad.State
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.ElfEdit (Elf)
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
import           Prettyprinter (pretty)
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import qualified Text.PrettyPrint.HughesPJ as HPJ
import           Text.Printf (printf)

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import qualified Data.Macaw.Dwarf as Dwarf
import           Data.Macaw.Memory.ElfLoader
import           Data.Macaw.X86 (X86_64, X86TermStmt(..))
import qualified Data.Macaw.X86 as X86
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
copyrightNotice = "Copyright 2014-21 Galois, Inc."

showUsage :: Handle -> IO ()
showUsage h = hPutStrLn h "For help on using reopt, run \"reopt --help\"."

------------------------------------------------------------------------
-- Resolve which symbols to include

-- | Name of a symbol along with its visibility
data QualifiedSymbolName
   = QualifiedSymbolName
   { qsnBytes :: !BSC.ByteString
     -- ^ Bytestring
   , qsnGlobal :: !Bool
     -- ^ Flag indicating if this is a global symbol
     --
     -- Global symbols should be unique for a binary while non-global
     -- symbols are only unique for a compilation unit.
   }

mkQualifiedSymbolName :: Elf.SymtabEntry BS.ByteString w
                      -> QualifiedSymbolName
mkQualifiedSymbolName ste =
  QualifiedSymbolName { qsnBytes  = Elf.steName ste
                      , qsnGlobal = Elf.steBind ste == Elf.STB_GLOBAL
                      }


-- | @mergeName new old@ picks the symbol to use when two symbols @new@ and @old@
-- have the same address, and we need a sensible default.
mergeName :: QualifiedSymbolName -> QualifiedSymbolName -> QualifiedSymbolName
mergeName new old =
  case (qsnGlobal new, qsnGlobal old) of
    -- Replace local symbols with global symbols.
    (True, False) -> new
    -- Otherwise use old symbol.
    _ -> old

--------------------------------------------------------------------------------
-- SymAddrMap

-- | Maintain symbol/address name mappings.
data SymAddrMap w =
  SymAddrMap { samNameMap :: !(Map BS.ByteString (Set.Set (MemSegmentOff w)))
               -- ^ Map from global symbol names to their address.
             , samAddrMap :: !(Map (MemSegmentOff w) QualifiedSymbolName)
               -- ^ Map from address to the symbol to use for that address.
               --
               -- In the case where multiple symbols have the same address, we
               -- use the first symbol with that address in the symbol table but
               -- prioritze global symbols over local symbols.
             }

getAddrSymMap :: SymAddrMap w -> Map (MemSegmentOff w) BS.ByteString
getAddrSymMap sam = fmap qsnBytes (samAddrMap sam)

-- | Empty symbol address map
symAddrMapEmpty :: SymAddrMap w
symAddrMapEmpty = SymAddrMap { samNameMap = Map.empty
                             , samAddrMap = Map.empty
                             }

-- | Symbol address map insertyion
symAddrMapInsert :: Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w)
                 -> MemSegmentOff w
                 -> SymAddrMap w
                 -> SymAddrMap w
symAddrMapInsert sym addr sam = seq addr $
  let qnm = mkQualifiedSymbolName sym
      nmMap' = Map.insertWith (\_new -> Set.insert addr)
                              (qsnBytes qnm)
                              (Set.singleton addr)
                              (samNameMap sam)
      addrMap' = Map.insertWith mergeName addr qnm (samAddrMap sam)
   in seq qnm $ SymAddrMap { samNameMap = nmMap'
                           , samAddrMap = addrMap'
                           }

-- | Error code if @symAddrMapLookup@ fails.
data SymAddrMapLookupError
   = SymAddrMapNotFound
   | SymAddrMapAmbiguous

-- | Lookup entry in symbol to address map.
symAddrMapLookup :: SymAddrMap w -> BS.ByteString -> Either SymAddrMapLookupError (MemSegmentOff w)
symAddrMapLookup sam nm =
  let s = Map.findWithDefault Set.empty nm (samNameMap sam)
   in case Set.size s of
       0 -> Left SymAddrMapNotFound
       1 -> Right (Set.findMin s)
       _ -> Left SymAddrMapAmbiguous

----------------------------------------------------------------------------------
-- Resolution functions

-- | Attempt to find the address of a string identifying a symbol
-- name, and return either the string if it cannot be resolved or the
-- address.
resolveSymAddr :: Memory w
                  -- ^ Loaded memory object.
               -> Either String RegionIndex
                  -- ^ Region index for resolving addresses or error message tro print
               -> SymAddrMap w
                 -- ^ Map from symbol names in binary to associated addresses.
               -> String
                  -- ^ The name of a symbol as a string.
               -> IO (MemSegmentOff w)
resolveSymAddr mem mregIdx symMap nm0 = addrWidthClass (memAddrWidth mem) $
  case resolveSymName nm0 of
    AddrIdent w -> do
      regIdx <-
        case mregIdx of
          Left e -> do
            hPutStrLn stderr e
            exitFailure
          Right regIdx ->
            pure regIdx
      case resolveRegionOff mem regIdx (fromIntegral w) of
        Just off ->
          pure off
        Nothing -> do
          hPutStrLn stderr $ "Could not resolve address: " ++ nm0
          exitFailure
    SymbolIdent nm ->
      case symAddrMapLookup symMap nm of
        Left _ -> do
          hPutStrLn stderr $ "Could not resolve symbol: " ++ nm0
          exitFailure
        Right a ->
          pure a

-- | Information from user to control which addresses to include and
-- exclude.
data ReoptOptions =
  ReoptOptions { roIncluded :: [String] -- ^ Symbols/addresses user wanted included
               , roExcluded :: [String] -- ^ Symbols/addresses user wanted exluded.
               }

addKnownFn :: SymAddrMap w
           -> BS.ByteString
           -> NoReturnFunStatus
           -> Map (MemSegmentOff w) NoReturnFunStatus
           -> Map (MemSegmentOff w) NoReturnFunStatus
addKnownFn sam nm noRet m0 =
  let s = Map.findWithDefault Set.empty nm (samNameMap sam)
   in foldl (\m a -> Map.insert a noRet m) m0 s

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
   -> IORef (SymAddrMap w)
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

parseElfHeaderInfo64 :: String
                     -- ^ Name of output for error messages
                     -> BS.ByteString
                     -- ^ Data to read
                     -> IO (Elf.ElfHeaderInfo 64)
parseElfHeaderInfo64 path bs = do
  case Elf.decodeElfHeaderInfo bs of
    Left (_, msg) -> do
      hPutStrLn stderr $ "Could not parse Elf file " ++ path ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Right (Elf.SomeElf hdrInfo) -> do
      case Elf.headerClass (Elf.header hdrInfo) of
        Elf.ELFCLASS32 -> do
          hPutStrLn stderr "32-bit elf files are not yet supported."
          exitFailure
        Elf.ELFCLASS64 -> do
          pure hdrInfo

parseElf64 :: String
              -- ^ Name of output for error messages
           -> BS.ByteString
              -- ^ Data to read
           -> IO (Elf 64)
parseElf64 nm bs = do
  hdr <- parseElfHeaderInfo64 nm bs
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
  case Elf.decodeElfHeaderInfo bs of
    Left (_, msg) -> do
      hPutStrLn stderr $ "Error reading " ++ path ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Right (Elf.SomeElf hdr) ->
      pure $! Some hdr

------------------------------------------------------------------------
-- Get binary information

getElfArchInfo :: Elf.ElfClass w -> Elf.ElfMachine -> Elf.ElfOSABI -> IO (SomeArchitectureInfo w)
getElfArchInfo cl arch abi =
  case (cl, arch, abi) of
    (Elf.ELFCLASS64, Elf.EM_X86_64, Elf.ELFOSABI_LINUX)   ->
      pure $! SomeArch X86.x86_64_linux_info   processX86PLTEntries
    (Elf.ELFCLASS64, Elf.EM_X86_64, Elf.ELFOSABI_SYSV)    ->
      pure $! SomeArch X86.x86_64_linux_info   processX86PLTEntries
    (Elf.ELFCLASS64, Elf.EM_X86_64, Elf.ELFOSABI_FREEBSD) ->
      pure $! SomeArch X86.x86_64_freeBSD_info processX86PLTEntries
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
     hPutStrLn stderr $
       printf "Do not support %d-bit %s %s binaries."
              (Elf.elfClassBitWidth cl) archName (show abi)
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

-- | Write a warning to stderr.
showWarning :: MonadIO m => String -> m ()
showWarning msg = liftIO $ hPutStrLn stderr $ "Warning: " ++ msg
{-# INLINE showWarning #-}

-- | Insert a symbol table entry into map.
--
-- This is used in dynamic binaries.
insSymbol :: forall w
          . (MemWidth w, Integral (Elf.ElfWordType w))
          => Memory w
          -- ^ Loaded memory
          -> MemAddr w
          -- ^ Base address that binary is loaded at
          -> IORef (SymAddrMap w)
          -- ^ Map to add symbol to
          -> (Int, Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w))
          -> IO ()
insSymbol mem baseAddr symMapRef (idx, symEntry)
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
      -- Get memory address of symbol
      let symAddr :: MemAddr w
          symAddr = incAddr (toInteger val) baseAddr
      -- Resolve address as segment offset.
      case asSegmentOff mem symAddr of
        Just addr ->
          modifyIORef' symMapRef $ symAddrMapInsert symEntry addr
        Nothing ->
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
  => Elf.ElfHeaderInfo w
  -> Memory w  -- ^ Memory created from binary.
  -> MemAddr w -- ^ Address binary is loaded at
  -> BS.ByteString -- ^ Symbol table for parsing.
  -> BS.ByteString -- ^ String table
  -> IORef (SymAddrMap w)
  -> IO ()
addDefinedSymbolTableFuns hdrInfo mem baseAddr symtabData strtab symMapRef = do
  let hdr = Elf.header hdrInfo
  let cl = Elf.headerClass hdr
  let dta = Elf.headerData hdr
  let symEntrySize :: Int
      symEntrySize = Elf.symtabEntrySize cl
  let cnt :: Word32
      cnt = fromIntegral (BS.length symtabData `quot` symEntrySize)
  let go idx
        | idx >= cnt = pure ()
        | otherwise =
            case Elf.decodeSymtabEntry cl dta strtab symtabData idx of
              Left e -> do
                showWarning $ "Failed to parse symbol table entry " ++ show e
                go (idx+1)
              Right symEntry -> do
                insSymbol mem baseAddr symMapRef (fromIntegral idx, symEntry)
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
             -> IORef (SymAddrMap 64)
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
      rela = Elf.decodeRelaEntry dta (pltPLTRelaData args) idx
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
  let symtab = pltSymtab args
  sym <-
    case Elf.decodeSymtabEntry Elf.ELFCLASS64 dta (pltStrtab args) symtab symIndex of
      Left err -> do
        showWarning (show err)
        throwError ()
      Right entry ->
        pure entry
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
    modifyIORef' pltFnsRef (\l -> (a, Elf.steName sym):l)
    modifyIORef' symMapRef $ symAddrMapInsert sym a


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
                     -> IORef (SymAddrMap 64)
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

$(pure [])

--------------------------------------------------------------------------------
-- InitDiscovery

-- | Information returned by `initDiscovery` below.
data InitDiscovery arch
  = InitDiscovery
    { initDiscMemory :: !(Memory (ArchAddrWidth arch))
      -- ^ Memory to use when exploring
    , initDiscEntryPoint :: !(Maybe (ArchSegmentOff arch))
      -- ^ Entry point for binary (defined if an executable or shared library)
    , initDiscRegionIndex :: !RegionIndex
      -- ^ Region index to use for resolving include/exclude values
      -- given as numeric literals or error message to print
    , initDiscSymAddrMap :: !(SymAddrMap (ArchAddrWidth arch))
      -- ^ Map from symbols to addresses.
    , initDiscExploreFn :: !(ArchSegmentOff arch -> Bool)
      -- ^ A function for filtering addresses to explore.
    , initDiscPLTFuns :: [(MemSegmentOff (ArchAddrWidth arch), BS.ByteString)]
      -- ^ A list of address and name pairs for PLT functions.
    , initDiscBaseCodeAddr :: !(MemAddr (ArchAddrWidth arch))
      -- ^ Address to use as base address for program counters in
      -- Dwarf debug information.
    }

$(pure [])

-- | Resolve a symbol table entry in an object file.
resolveObjSymbol :: Elf.ElfHeaderInfo w
                 -> Memory w
                 -> Map Word16 (MemSegmentOff w)
                    -- ^ Map from section index to offset in memory of section.
                 -> SymAddrMap w
                    -- ^ Symbol addr map
                 -> (Int, Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w))
                    -- ^ Index of symbol in symbol table and entry.
                 -> IO (SymAddrMap w)
resolveObjSymbol hdrInfo mem secMap sam (idx, ste) = elfInstances hdrInfo $ do
  let secIdx = Elf.steIndex ste
  if Elf.steType ste /= Elf.STT_FUNC then do
    pure sam
   else if secIdx == Elf.SHN_UNDEF then
    pure sam
   else if Elf.steName ste == "" then do
    hPutStrLn stderr $ show $ EmptySymbolName idx (Elf.steType ste)
    pure sam
   else if secIdx == Elf.SHN_ABS then do
    let val = Elf.steValue ste
    case resolveAbsoluteAddr mem (fromIntegral val) of
      Just addr -> do
        pure $! symAddrMapInsert ste addr sam
      Nothing -> do
        hPutStrLn stderr $ show $ CouldNotResolveAddr (Elf.steName ste)
        pure sam
   else if Elf.fromElfSectionIndex secIdx >= Elf.shdrCount hdrInfo then do
    hPutStrLn stderr $ show $ CouldNotResolveAddr (Elf.steName ste)
    pure sam
   else do
    let shdr = Elf.shdrByIndex hdrInfo (Elf.fromElfSectionIndex secIdx)
    let val = Elf.steValue ste
    case Map.lookup (Elf.fromElfSectionIndex secIdx) secMap of
      Just base
        | Elf.shdrAddr shdr <= val && (val - Elf.shdrAddr shdr) < Elf.shdrSize shdr
        , off <- toInteger (val - Elf.shdrAddr shdr)
        , Just addr <- incSegmentOff base off -> do
            pure $! symAddrMapInsert ste addr sam
      _ -> do
        hPutStrLn stderr $ show $ CouldNotResolveAddr (Elf.steName ste)
        pure sam

$(pure [])

-- | Discover functions in an elf file.
--
--  Note. This prints warnings to stderr
initDiscovery :: forall arch
              . LoadOptions
              -- ^ Option to load the binary at the given address
              -> Elf.ElfHeaderInfo (ArchAddrWidth arch)
              -> ProcessPLTEntries (ArchAddrWidth arch)
              -> IO (InitDiscovery arch)
initDiscovery loadOpts hdrInfo pltFn = elfInstances hdrInfo $ do
  let hdr = Elf.header hdrInfo
  case Elf.headerType hdr of
    -- This is for object files.
    Elf.ET_REL -> do
      -- Note. This code requires code is in a single text section.  It does
      -- not support objects with -ffunction-sections

      case loadOffset loadOpts of
        Nothing -> pure ()
        Just _ -> showWarning $ "Ignoring load offset for object file as there is no global base address."
      -- Load elf sections
      (mem, secMap, warnings) <- do
        -- Do loading
        case memoryForElfSections hdrInfo of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings

      -- Get static symbol table
      symAddrMap <-
        case Elf.decodeHeaderSymtab hdrInfo of
          Nothing -> pure symAddrMapEmpty
          Just (Left e) -> do
            hPutStrLn stderr (show e)
            pure symAddrMapEmpty
          Just (Right tbl) -> do
            let staticEntries = zip [0..] (V.toList (Elf.symtabEntries tbl))
            foldlM (resolveObjSymbol hdrInfo mem secMap) symAddrMapEmpty staticEntries

      -- Get index of text section section.
      textSectionIndex <-
        case Elf.headerNamedShdrs hdrInfo of
          Left (secIdx, _) -> do
            hPutStrLn stderr $ printf "Could not resolve name of section %s." (show secIdx)
            exitFailure
          Right shdrs -> do
            let isText s = Elf.shdrName s == ".text"
            case V.findIndex isText shdrs of
              Nothing -> do
                hPutStrLn stderr "Could not find .text section."
                exitFailure
              Just secIdx -> do
                when (isJust (V.findIndex isText (V.drop (secIdx+1) shdrs))) $ do
                  hPutStrLn stderr "Duplicate .text sections found."
                  exitFailure
                pure $ (fromIntegral secIdx :: Word16)
      -- Get offset for text section.
      textBaseAddr <-
        case Map.lookup textSectionIndex secMap of
          Nothing -> do
            hPutStrLn stderr "Text section is not loaded."
            exitFailure
          Just a ->
            pure a
       -- Get region of text segment
      let regIdx :: RegionIndex
          regIdx = segmentBase (segoffSegment textBaseAddr)
      -- Get initial entries and predicate for exploring
      pure $! InitDiscovery { initDiscMemory = mem
                            , initDiscEntryPoint = Nothing
                            , initDiscRegionIndex = regIdx
                            , initDiscSymAddrMap = symAddrMap
                            , initDiscExploreFn = \_ -> True
                            , initDiscPLTFuns = []
                            , initDiscBaseCodeAddr = MemAddr regIdx 0
                            }
    -- Static executable
    Elf.ET_EXEC -> do
      case loadOffset loadOpts of
        Nothing -> pure ()
        Just _ -> showWarning $ "Ignoring load offset for unrelocatable executable."
      (mem, _secMap, warnings) <-
        case memoryForElfSegments defaultLoadOptions hdrInfo of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings

      -- Get static symbol table
      staticEntries <-
        case Elf.decodeHeaderSymtab hdrInfo of
          Nothing -> pure []
          Just (Left e) -> do
            hPutStrLn stderr (show e)
            pure []
          Just (Right symtab) -> do
            pure $ V.toList $ Elf.symtabEntries symtab

      let insStatSymbol :: SymAddrMap (ArchAddrWidth arch)
                        -> Elf.SymtabEntry BS.ByteString (Elf.ElfWordType (ArchAddrWidth arch))
                        -> IO (SymAddrMap (ArchAddrWidth arch))
          insStatSymbol sam sym = do
            if Elf.steType sym /= Elf.STT_FUNC then
              pure sam
            -- Check symbol is defined
             else if Elf.steIndex sym == Elf.SHN_UNDEF then
              pure sam
            -- Check symbol name is non-empty
             else if Elf.steName sym == "" then
              pure sam
            -- Lookup symbol as absolute
             else do
              let val = Elf.steValue sym
              case resolveAbsoluteAddr mem (fromIntegral val) of
                Just addr ->
                  pure $! symAddrMapInsert sym addr sam
                Nothing -> do
                  hPutStrLn stderr $ show $ CouldNotResolveAddr (Elf.steName sym)
                  pure sam

      symAddrMap <- foldlM insStatSymbol symAddrMapEmpty staticEntries
      -- Get entry addr
      let entry = Elf.headerEntry hdr
      let entryAddr = resolveRegionOff mem 0 (fromIntegral entry)
      when (isNothing entryAddr) $ do
        hPutStrLn stderr $ "Could not resolve entry point 0x" ++ showHex entry ""
      -- Return discovery
      pure $! InitDiscovery { initDiscMemory = mem
                            , initDiscEntryPoint = entryAddr
                            , initDiscRegionIndex = 0
                            , initDiscSymAddrMap = symAddrMap
                            , initDiscExploreFn = \_ -> True
                            , initDiscPLTFuns = []
                            , initDiscBaseCodeAddr = MemAddr 0 0
                            }
    -- This is a shared library or position-independent executable.
    Elf.ET_DYN -> do
      -- Create memory image for elf file.
      (mem, _secMap, warnings) <-
        case memoryForElfSegments loadOpts hdrInfo of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings

      -- Get base address to use for computing section offsets.
      let baseAddr :: MemAddr (ArchAddrWidth arch)
          baseAddr =
            case loadOffset loadOpts of
              Just o  -> MemAddr { addrBase = 0, addrOffset = fromIntegral o }
              Nothing -> MemAddr { addrBase = 1, addrOffset = 0 }

      -- Get symbol table.
      let dta = Elf.headerData hdr

      let sections = Elf.headerSections hdrInfo

      let sectionNameMap :: Map BS.ByteString [Elf.ElfSection (Elf.ElfWordType (ArchAddrWidth arch))]
          sectionNameMap =
            Map.fromListWith (++)
              [ (Elf.elfSectionName s, [s]) | (_rng,s) <- V.toList sections ]

      symMapRef <- newIORef symAddrMapEmpty
      withSymtab (snd <$> sections) sectionNameMap "static symbol table" ".symtab" $ \symtab strtab -> do
        addDefinedSymbolTableFuns hdrInfo mem baseAddr symtab strtab symMapRef

      pltBoundsRef <- newIORef Nothing
      pltFnsRef <- newIORef []
      withSymtab (snd <$> sections) sectionNameMap "dynamic symbol table" ".dynsym" $ \dynSymtab dynStrtab -> do
        addDefinedSymbolTableFuns hdrInfo mem baseAddr dynSymtab dynStrtab symMapRef
        pltFn dta dynSymtab dynStrtab sectionNameMap mem pltBoundsRef pltFnsRef symMapRef

      mbounds <- readIORef pltBoundsRef
      -- Exclude PLT bounds
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
      symAddrMap <- readIORef symMapRef

      -- Get initial entries and predicate for exploring
      let regIdx :: RegionIndex
          regIdx = if isJust (loadOffset loadOpts) then 0 else 1

      -- Get initial entry address
      let entry = Elf.headerEntry hdr
      -- Adjust entry by load offset.
      let adjEntry =
            case loadOffset loadOpts of
              Nothing -> toInteger entry
              Just o -> toInteger o + toInteger entry
      -- Mark entry as address as function
      let entryAddr = resolveRegionOff mem regIdx (fromInteger adjEntry)
      when (isNothing entryAddr) $ do
        hPutStrLn stderr $ "Could not resolve entry point: " ++ showHex entry ""
      -- Get base address for Dwarf
      let dwarfBaseAddr =
            case loadOffset loadOpts of
              Just o -> MemAddr 0 (fromIntegral o)
              Nothing -> MemAddr 1 0
      -- Return discovery
      pure $! InitDiscovery { initDiscMemory = mem
                            , initDiscEntryPoint = entryAddr
                            , initDiscRegionIndex = regIdx
                            , initDiscSymAddrMap = symAddrMap
                            , initDiscExploreFn = exploreFn
                            , initDiscPLTFuns = pltFns
                            , initDiscBaseCodeAddr = dwarfBaseAddr
                            }
    Elf.ET_CORE -> do
      hPutStrLn stderr "Reopt does not support loading core files."
      exitFailure
    tp -> do
      hPutStrLn stderr $ "Reopt does not support loading elf files with type " ++ show tp ++ "."
      exitFailure

$(pure [])

initDiscState :: InitDiscovery arch
              -> ArchitectureInfo arch
              -> ReoptOptions
              -> IO (DiscoveryState arch)
initDiscState disc ainfo reoptOpts = do
  let mem = initDiscMemory disc
  let regIdx = initDiscRegionIndex disc
  let symAddrMap = initDiscSymAddrMap disc
  let explorePred = initDiscExploreFn disc
  let resolveEntry qsn | ".cold" `BS.isSuffixOf` qsnBytes qsn = Nothing
                       | otherwise = Just MayReturnFun
  let entryPoints = Map.mapMaybe resolveEntry (samAddrMap symAddrMap)
                  & addKnownFn symAddrMap "abort"            NoReturnFun
                  & addKnownFn symAddrMap "exit"             NoReturnFun
                  & addKnownFn symAddrMap "_Unwind_Resume"   NoReturnFun
                  & addKnownFn symAddrMap "__cxa_rethrow"    NoReturnFun
                  & addKnownFn symAddrMap "__cxa_throw"      NoReturnFun
                  & addKnownFn symAddrMap "__malloc_assert"  NoReturnFun
                  & addKnownFn symAddrMap "__stack_chk_fail" NoReturnFun
                  & addKnownFn symAddrMap "_ZSt9terminatev"  NoReturnFun
  s <-
    case (roIncluded reoptOpts, roExcluded reoptOpts) of
      ([], excludeNames) -> do
        excludeAddrs <- mapM (resolveSymAddr mem (Right regIdx) symAddrMap) excludeNames
        let s = Set.fromList excludeAddrs
        let initState = emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
                      & trustedFunctionEntryPoints .~ entryPoints
                      & exploreFnPred .~ (\a -> Set.notMember a s && explorePred a)
                      & markAddrsAsFunction InitAddr (Map.keys entryPoints)
        pure $! initState
      (includeNames, []) -> do
        includeAddrs <- mapM (resolveSymAddr mem (Right regIdx) symAddrMap) includeNames
        let s = Set.fromList includeAddrs
        let initState = emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
                      & trustedFunctionEntryPoints .~ entryPoints
                      & exploreFnPred .~ (\a -> Set.member a s)
                      & markAddrsAsFunction InitAddr s
        pure $! initState
      _ -> do
        fail "Cannot both include and exclude specific addresses."
  case initDiscEntryPoint disc of
    Nothing -> pure $! s
    Just entry -> pure $! s & markAddrAsFunction InitAddr entry

$(pure [])

---------------------------------------------------------------------------------
-- GetFnsLogEvent

-- | Event passed to logger when discovering functions
data GetFnsLogEvent
   = ArgResolverError !String !ArgResolverError
   | DuplicateSections !BSC.ByteString
   | DebugError !String
   | StartFunRecovery !(Maybe BSC.ByteString) !(MemSegmentOff 64)
     -- ^ Notify we are starting analysis of given function.
   | RecoveryFailed !(Maybe BSC.ByteString)  !(MemSegmentOff 64) !String
     -- ^ @RecoveryFailed dnm addr msg@ notes we failed to recover function due to given reason.
   | RecoveryPLTSkipped  !(Maybe BSC.ByteString)  !(MemSegmentOff 64)
   | RecoveryWarning !(Maybe BSC.ByteString) !(MemSegmentOff 64) !String
   | GetFnsError !String
     -- ^ A general error message

instance Show GetFnsLogEvent where
  show (ArgResolverError fnm e)  = printf "Type error on %s: %s" fnm (showArgResolverError e)
  show (DuplicateSections nm)  = "Multiple sections named " ++ BSC.unpack nm
  show (DebugError msg)        = msg
  show (StartFunRecovery dnm faddr)  =
     let fnm = case dnm of
                 Just nm -> BSC.unpack nm <> "(0x" <> showHex (addrOffset (segoffAddr faddr)) ")"
                 Nothing -> "0x" <> showHex (addrOffset (segoffAddr faddr)) ""
      in "Recovering function " <> fnm
  show (RecoveryFailed _ _ msg)  = "  " <> msg
  show (RecoveryPLTSkipped _ _)  = "  Skipped PLT stub"
  show (RecoveryWarning _ _ msg) = "  Warning: " ++ msg
  show (GetFnsError msg) = msg

$(pure [])

---------------------------------------------------------------------------------
-- ReoptFunType

-- | This describes the arguments to a function using the type system
-- internally maintained by Reopt.
data ReoptFunType
   = ReoptNonvarargFunType !AnnFunType
   | ReoptPrintfFunType !Int
     -- ^ A function that is like printf where the last non-vararg
     -- argument is a string and subsequent arguments are inferred
     -- from it.
     --
     -- The int denotes the number of 64-bit bitvectors previously.
   | ReoptUnsupportedFunType
  deriving (Eq, Show)

$(pure [])

---------------------------------------------------------------------------------
-- FunTypeMaps

-- | Function type information parsed from annotations and debug information.
data FunTypeMaps w =
  FunTypeMaps { dwarfAddrResolve :: !(BS.ByteString -> Word64 -> Maybe (MemSegmentOff w))
                -- ^ This resolve the address of a function given its name and object.
                --
                -- This general type is used for eventual support of object files
                -- with function sections, where the Dwarf information does not
                -- contain address information, and so we use symbol addresses.
                --
                -- It returns nothing if an address cannot be resolved.
              , dwarfBaseCodeAddr :: !(MemAddr w)
                -- ^ Address to add to all code offsets in dwarf file.
              , nameToAddrMap :: !(SymAddrMap w)
                -- ^ Map from symbol names to the address.
              , nameTypeMap :: !(Map BS.ByteString ReoptFunType)
                -- ^ Map from external undefined symbol names to type.
              , addrTypeMap :: !(Map (MemSegmentOff w) ReoptFunType)
                -- ^ Map from code addresses that are start of function
                -- to type.
              , noreturnMap :: !(Map (MemSegmentOff w) NoReturnFunStatus)
              }

$(pure [])

-- | Add a new function type to a function
funTypeIsDefined :: FunTypeMaps w -- ^ Current type map information
                 -> Maybe BS.ByteString -- ^ External name of function if not defined
                 -> Maybe (MemSegmentOff w)
                 -> Bool
funTypeIsDefined funTypeMaps msym maddr = do
  let symDef = case msym of
                 Nothing -> True
                 Just sym -> Map.member sym (nameTypeMap funTypeMaps)
      addrDef = case maddr of
                  Nothing -> True
                  Just addr -> Map.member addr (addrTypeMap funTypeMaps)
   in symDef && addrDef

$(pure [])

-- | Add a type to a map
addCheckExisting :: (Ord k, Eq tp, Show tp)
                 => (GetFnsLogEvent -> IO ())
                 -> String
                 -> k
                 -> tp
                 -> Map k tp
                 -> IO (Map k tp)
addCheckExisting logger nm k v m =
  case Map.lookup k m of
    Nothing ->
      pure $! Map.insert k v m
    Just pv -> do
      when (pv /= v) $ do
        logger $ GetFnsError $ printf "%s assigned incompatible types.\nPrev:\n%s\nNew:\n%s" nm (show pv) (show v)
      pure m

$(pure [])

-- | Add a new function type to a function
addNamedFunType :: (GetFnsLogEvent -> IO ()) -- ^ Logging function for recording errors.
                -> FunTypeMaps w -- ^ Current type map information
                -> String -- ^ Name of entry for logging purposes.
                -> Maybe BS.ByteString -- ^ External name of function if not defined
                -> Maybe (MemSegmentOff w)
                -> ReoptFunType
                -> IO (FunTypeMaps w)
addNamedFunType logger funTypeMaps loggingName msym maddr reoptFunType = do
  ntm <-
    case msym of
      Nothing ->
        pure $! nameTypeMap funTypeMaps
      Just sym -> do
        addCheckExisting logger loggingName sym reoptFunType (nameTypeMap funTypeMaps)
  atm <-
    case maddr of
      Nothing -> do
        pure $! addrTypeMap funTypeMaps
      Just addr -> do
        let nm = loggingName ++ " address"
        addCheckExisting logger nm addr reoptFunType (addrTypeMap funTypeMaps)
  pure $! funTypeMaps { nameTypeMap = ntm, addrTypeMap = atm }

$(pure [])

---------------------------------------------------------------------------------
-- Debug resolution

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

$(pure [])

throwDwarfTypeError :: MonadError ArgResolverError m => Dwarf.TypeRef -> String -> m a
throwDwarfTypeError ref msg =
  throwError $ DebugResolveError $ printf "Bad type ref 0x%x: %s" (Dwarf.typeRefFileOffset ref) msg

$(pure [])

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

$(pure [])

resolveDwarfType :: (GetFnsLogEvent -> IO ())
                    -- ^ Logging function for errors
                 -> Map Dwarf.TypeRef Dwarf.AbsType
                 -> Dwarf.TypeRef
                 -> ExceptT ArgResolverError IO AnnType
resolveDwarfType logger typeMap ref = do
  tp <- resolveDwarfTypeRef logger typeMap ref
  case tp of
    Dwarf.BoolType -> do
      pure $! IAnnType 1
    Dwarf.UnsignedIntType byteCount -> do
      pure $! IAnnType (8*byteCount)
    Dwarf.SignedIntType byteCount -> do
      pure $! IAnnType (8*byteCount)
    Dwarf.FloatType -> do
      pure FloatAnnType
    Dwarf.DoubleType -> do
      pure DoubleAnnType
    Dwarf.LongDoubleType -> do
      throwDwarfTypeError ref "Long double return type is not supported."
    Dwarf.UnsignedCharType -> do
      pure $! IAnnType 8
    Dwarf.SignedCharType -> do
      pure $! IAnnType 8
    Dwarf.ArrayType _ _ -> do
      throwDwarfTypeError ref "Array arguments are not supported."
    Dwarf.PointerType _ _ -> do
      -- We just use void pointers for now.
      pure $ PtrAnnType VoidAnnType
    Dwarf.StructType _ -> do
      throwDwarfTypeError ref "Struct arguments are not supported."
    Dwarf.UnionType _ -> do
      throwDwarfTypeError ref "Union arguments are not supported."
    Dwarf.EnumType d ->
      case Dwarf.enumDeclType d of
        Just r -> resolveDwarfType logger typeMap r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for enum at " ++ show ref
    Dwarf.SubroutinePtrType _ -> do
      -- We just use void pointers for now.
      pure $ PtrAnnType VoidAnnType
    Dwarf.TypedefType d -> do
      resolveDwarfType  logger typeMap (Dwarf.typedefType d)
    Dwarf.TypeQualType ann -> do
      case Dwarf.tqaType ann of
        Just r -> resolveDwarfType logger typeMap r
        Nothing -> throwError $ DebugResolveError $ printf "Could not find type for qualifier at " ++ show ref
    Dwarf.SubroutineTypeF _ -> do
      throwDwarfTypeError ref "Subroutines may not be passed as return values."

$(pure [])

-- | Resolve Dwarf arg types
resolveDwarfArgTypes :: (GetFnsLogEvent -> IO ())
                      -- ^ Logging function for errors
                      -> Maybe Dwarf.Subprogram
                        -- ^ Origin subprogram if defined
                      -> Map Dwarf.TypeRef Dwarf.AbsType
                      -> [AnnFunArg] -- ^ Arguments processed so far in reverse order.
                      -> Int -- ^ Number of arguments passed so far.
                      -> [Dwarf.Variable]
                      -> ExceptT ArgResolverError IO (V.Vector AnnFunArg)
resolveDwarfArgTypes _ _morigin _typeMap prev _cnt [] =
  pure $! V.fromList (reverse prev)
resolveDwarfArgTypes logger morigin typeMap prev cnt (a:r) = seq cnt $ do
  let nm | Dwarf.varName a == "" = "arg" ++ show cnt
         | otherwise = BSC.unpack (Dwarf.nameVal (Dwarf.varName a))
  let mnm | Dwarf.varName a == "" = Nothing
          | otherwise = Just (BSC.unpack (Dwarf.nameVal (Dwarf.varName a)))
  tp <-
    case Dwarf.varType a of
      Just ref -> do
        resolveDwarfType logger typeMap ref
      Nothing -> do
        subOrigin <-
          case morigin of
            Nothing -> throwError $ MissingArgType nm
            Just subOrigin -> pure subOrigin
        varOrig <-
          case Dwarf.varOrigin a of
            Nothing -> throwError $ DebugResolveError $ "Missing argument abstract origin."
            Just varOrigRef ->
              case Map.lookup varOrigRef (Dwarf.subParamMap subOrigin) of
                Nothing ->
                  throwError $ DebugResolveError $
                    printf "Could not find variable origin %s for %s." (show (pretty varOrigRef)) nm
                Just o -> pure o
        -- Get origin ref
        ref <-
          case Dwarf.varType varOrig of
            Nothing -> throwError $ MissingArgType nm
            Just ref -> pure ref
        resolveDwarfType logger (Dwarf.subTypeMap subOrigin) ref
  let a' = AnnFunArg { funArgName = mnm, funArgType = tp }
  resolveDwarfArgTypes logger morigin typeMap (a':prev) (cnt+1) r

-- | Resolve the type of a Dwarf subprogram
resolveDwarfSubprogramFunType :: (GetFnsLogEvent -> IO ())
                                 -- ^ Logging function for errors
                              -> Dwarf.Subprogram
                              -> Maybe Dwarf.Subprogram -- ^ Origin if subprogram is generated from another.
                              -> ExceptT ArgResolverError IO AnnFunType
resolveDwarfSubprogramFunType logger sub morigin = do
  when (Dwarf.subUnspecifiedParams sub) $
    throwError $ VarArgsUnsupported
  argTypes <- resolveDwarfArgTypes logger morigin (Dwarf.subTypeMap sub) [] 0 (Map.elems (Dwarf.subParamMap sub))
  retType <-
    case Dwarf.subRetType sub of
      Nothing ->
        case morigin of
          Nothing -> pure VoidAnnType
          Just origin ->
            case Dwarf.subRetType origin of
              Nothing -> pure VoidAnnType
              Just ref -> resolveDwarfType logger (Dwarf.subTypeMap origin) ref
      Just ref -> resolveDwarfType logger (Dwarf.subTypeMap sub) ref

  pure $! AnnFunType
    { funRet    = retType
    , funArgs  = argTypes
    , funVarArg = False
    }

$(pure [])

-- | Resolve type information from subroutine.
resolveSubprogramType :: (GetFnsLogEvent -> IO ())
                      -- ^ Logging function for errors
                      -> Dwarf.CompileUnit
                      -- ^ Compile unit for this sub program
                      -> FunTypeMaps w
                      -- ^ Annotations from source file
                      -> Dwarf.Subprogram
                      -- ^ Dwarf function information
                      -> Maybe (MemSegmentOff w)
                         -- Address
                      -> IO (FunTypeMaps w)
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
                    pure $! ReoptUnsupportedFunType
                  Right funType -> do
                    pure (ReoptNonvarargFunType funType)
              addNamedFunType logger annMap debugName externalName entryAddr funType

-- | Resolve type information from subroutine.
resolveSubprogram :: (GetFnsLogEvent -> IO ())
                  -- ^ Logging function for errors
                  -> Dwarf.CompileUnit
                  -- ^ Compile unit for this sub program
                  -> FunTypeMaps w
                  -- ^ Annotations from source file
                  -> Dwarf.Subprogram
                  -- ^ Elf file for header information
                  -> IO (FunTypeMaps w)
resolveSubprogram logger cu annMap sub = do
  -- Get entry address in terms of memory.
  entryAddr <-
    case dwarfSubEntry sub of
      Nothing -> pure Nothing
      Just entry -> do
        let dwarfName = Dwarf.subName sub
        let r = dwarfAddrResolve annMap (Dwarf.nameVal dwarfName) entry
        when (isNothing r) $ do
          let debugName | dwarfName == "" = "Unnamed symbol"
                        | otherwise = BSC.unpack (Dwarf.nameVal dwarfName)
          logger $ DebugError $ printf "%s invalid debug address %s." debugName (show entry)
        pure r
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
                    -> FunTypeMaps w
                    -- ^ Map from function names to type info.
                    -> Maybe (Either String Dwarf.CUContext)
                    -- ^ Elf file for header information
                    -> IO (FunTypeMaps w)
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
resolveDebugFunTypes :: forall w
                     . (GetFnsLogEvent -> IO ())
                     -- ^ Logging function for errors
                     -> FunTypeMaps w
                     -- ^ Annotations from source file
                     -> Elf.ElfHeaderInfo w
                     -- ^ Elf file for header information
                     -> IO (FunTypeMaps w)
resolveDebugFunTypes logger annMap elfInfo = do
  let hdr = Elf.header elfInfo
  let secDataMap :: Map BS.ByteString [(Elf.FileRange (Elf.ElfWordType w), Elf.ElfSection (Elf.ElfWordType w))]
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

$(pure [])

---------------------------------------------------------------------------------
-- Complete discovery

doDiscovery :: forall arch
            .  (GetFnsLogEvent -> IO ())
               -- ^ Logging function for errors
            -> AnnDeclarations
               -- ^ Header with hints for assisting typing.
            -> Elf.ElfHeaderInfo (ArchAddrWidth arch)
            -> ArchitectureInfo arch
            -> InitDiscovery arch
            -> DiscoveryOptions -- ^ Options controlling discovery
            -> ReoptOptions
            -> IO ( FunTypeMaps (ArchAddrWidth arch)
                  , DiscoveryState arch
                  )
doDiscovery logger hdrAnn hdrInfo ainfo initState disOpt reoptOpts = withArchConstraints ainfo $ do
  s <- initDiscState initState ainfo reoptOpts
  let mem = memory s
  let symAddrMap = initDiscSymAddrMap initState

  -- Generate type information from annotations
  let nameAnnTypeMap
        = fmap ReoptNonvarargFunType (funDecls hdrAnn)
        <> Map.singleton "printf"   (ReoptPrintfFunType 0)
        <> Map.singleton "fprintf"  (ReoptPrintfFunType 1)
        <> Map.singleton "sprintf"  (ReoptPrintfFunType 1)
        <> Map.singleton "snprintf" (ReoptPrintfFunType 2)

  -- Generate map from address names to known type.
  --
  -- This is used when we see a function jumps to a defined address.
  addrAnnTypeMap <- do
    let insSymType :: Map (ArchSegmentOff arch) ReoptFunType
                   -> (BS.ByteString, ReoptFunType)
                   -> IO (Map (ArchSegmentOff arch) ReoptFunType)
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

  let annTypeMap :: FunTypeMaps (ArchAddrWidth arch)
      annTypeMap = FunTypeMaps
        { dwarfBaseCodeAddr = initDiscBaseCodeAddr initState
        , dwarfAddrResolve = \_symName off -> do
            let addr = incAddr (toInteger off) (initDiscBaseCodeAddr initState)
             in asSegmentOff mem addr
        , nameToAddrMap = initDiscSymAddrMap initState
        , nameTypeMap = nameAnnTypeMap
        , addrTypeMap = addrAnnTypeMap
        , noreturnMap = s^.trustedFunctionEntryPoints
        }

  -- Resolve debug information.
  debugTypeMap <- resolveDebugFunTypes logger annTypeMap hdrInfo
  let postDebugState = s & trustedFunctionEntryPoints .~ noreturnMap debugTypeMap

  discState <- completeDiscoveryState postDebugState disOpt
  pure (debugTypeMap, discState)

-- | Discover code in the binary identified by the given path.
discoverBinary :: (GetFnsLogEvent -> IO ())
                  -- ^ Logging function for errors
               -> FilePath
               -> LoadOptions
                  -- ^ Option to load the binary at the given address
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> ReoptOptions
               -> AnnDeclarations
                  -- ^ Header with hints for assisting typing.
               -> IO (Some DiscoveryState)
discoverBinary logger path loadOpts disOpt reoptOpts hdrAnn = do
  Some hdrInfo <- readSomeElf path
  let hdr = Elf.header hdrInfo
  -- Get architecture information for elf
  SomeArch ainfo pltFn <- getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr)
  -- Parse elf file to get relevant state.
  initState <- initDiscovery loadOpts hdrInfo pltFn
  (_,r) <- doDiscovery logger hdrAnn hdrInfo ainfo initState disOpt reoptOpts
  pure (Some r)

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
                 , demandInfoCtx = X86.x86DemandContext
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
osPersonality Linux   = X86.linux_syscallPersonality
osPersonality FreeBSD = X86.freeBSD_syscallPersonality

osArchitectureInfo :: X86OS -> ArchitectureInfo X86_64
osArchitectureInfo Linux   = X86.x86_64_linux_info
osArchitectureInfo FreeBSD = X86.x86_64_freeBSD_info

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

-- | Map function names to the associated type for the function with that name.
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
                       -- ^ Name of functions with definitions.
                       -> FunctionTypeMap arch
                       -> Function arch
                       -> FunctionTypeMap arch
getReferencedFunctions excluded m0 f =
    foldl' (foldFnValue findReferencedFunctions) m0 (fnBlocks f)
  where findReferencedFunctions :: FunctionTypeMap arch
                                -> FnValue arch tp
                                -> FunctionTypeMap arch
        findReferencedFunctions m (FnFunctionEntryValue ft nm) =
          insertName nm ft m
        findReferencedFunctions m _ = m

        insertName :: BS.ByteString
                   -> FunctionType arch
                   -> FunctionTypeMap arch
                   -> FunctionTypeMap arch
        insertName nm ft m
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
resolveArgType :: Monad m
               => String  -- ^ Argument name.
               -> AnnType -- ^ Type to resolve
               -> ArgResolver m ()
resolveArgType nm tp0 =
  case tp0 of
    VoidAnnType ->
      throwError $ UnsupportedArgType nm (ppAnnType tp0)
    IAnnType w -> do
      when (w > 64) $ throwError $ UnsupportedArgType nm (ppAnnType tp0)
      addGPReg64 nm
    FloatAnnType ->
      throwError $ UnsupportedArgType nm (ppAnnType tp0)
    DoubleAnnType ->
      throwError $ UnsupportedArgType nm (ppAnnType tp0)
    PtrAnnType _ ->
      addGPReg64 nm
    TypedefAnnType _ tp ->
      resolveArgType nm tp

-- | This parses the types extracted from header function argumnts to
-- the machine code registers that the function will expect.
argsToRegisters :: Monad m
                => Int -- ^ Number of arguments processed so far.
                -> V.Vector AnnFunArg
                   -- ^ Remaining arguments to parse
                -> ArgResolver m ()
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
    IAnnType w | w > 64 -> Left  $ UnsupportedReturnType (ppAnnType tp0)
               | otherwise -> Right $ [Some (RetBV64 F.RAX)]
    FloatAnnType     -> Left  $ UnsupportedReturnType (ppAnnType tp0)
    DoubleAnnType    -> Left  $ UnsupportedReturnType (ppAnnType tp0)
    PtrAnnType _     -> Right $ [Some (RetBV64 F.RAX)]
    TypedefAnnType _ tp -> parseReturnType tp


resolveAnnFunType :: Monad m
                  => AnnFunType
                  -> ExceptT ArgResolverError m X86FunTypeInfo
resolveAnnFunType funType =
   if funVarArg funType then do
     throwError VarArgsUnsupported
    else do
     args <- runArgResolver (argsToRegisters 0 (funArgs funType))
     ret <- case parseReturnType (funRet funType) of
              Left e -> throwError e
              Right r -> pure r
     pure $! X86NonvarargFunType args ret

-- | This checks whether any of the symbols in the map start with the given string as a prefix.
isUsedPrefix :: BSC.ByteString -> SymAddrMap  w -> Bool
isUsedPrefix prefix sam = any (\s -> prefix `BSC.isPrefixOf` qsnBytes s) (Map.elems (samAddrMap sam))

-- | Name of recovered function from a qualified symbol name.
localFunctionName :: BSC.ByteString -> QualifiedSymbolName -> MemSegmentOff w -> BSC.ByteString
localFunctionName prefix qnm segOff =
  if qsnGlobal qnm then
    qsnBytes qnm
   else
    let addr = segoffAddr segOff
     in prefix <> "_" <> qsnBytes qnm
               <> "_" <> BSC.pack (show (addrBase addr))
               <> "_" <> BSC.pack (show (addrOffset addr))

-- | Name of recovered function when no function exists.
nosymFunctionName :: BSC.ByteString -> MemSegmentOff w -> BSC.ByteString
nosymFunctionName prefix segOff =
  let addr = segoffAddr segOff
   in prefix <> "_" <> BSC.pack (show (addrBase addr)) <> "_" <> BSC.pack (show (addrOffset addr))

-- | Returns name of recovered function.
recoveredFunctionName :: MemWidth w
                      => SymAddrMap w
                      -- ^ Maps addresses of symbols to the associated symbol name.
                      -> BSC.ByteString
                      -- ^ Prefix to use for automatically generated symbols.
                      -- To be able to distinguish symbols, this should not be
                      -- a prefix for any of the symbols in the map.
                      -> MemSegmentOff w
                      -> BSC.ByteString
recoveredFunctionName m prefix segOff =
  case Map.lookup segOff (samAddrMap m) of
    Just qname -> localFunctionName prefix qname segOff
    Nothing -> nosymFunctionName prefix segOff

$(pure [])

-- | Map x86 function type to known functon abi.
--
-- This is used for global function argument analysis which doesn't yet support vararg
-- functions such as printf.
toKnownFunABI :: ReoptFunType -> Maybe (KnownFunABI X86Reg)
toKnownFunABI (ReoptNonvarargFunType atp) =
  case runExcept (resolveAnnFunType atp) of
    Right (X86NonvarargFunType args rets) ->
      Just $ KnownFnABI { kfArguments = argReg <$> args
                        , kfReturn = viewSome retReg <$> rets
                        }
    _ -> Nothing
toKnownFunABI (ReoptPrintfFunType _) = Nothing
toKnownFunABI ReoptUnsupportedFunType = Nothing

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

defaultX86Type :: X86FunTypeInfo
defaultX86Type = X86NonvarargFunType args rets
  where args = fmap ArgBV64 x86GPPArgumentRegs ++ fmap ArgMM512D [0..7]
        rets = [ Some (RetBV64 F.RAX), Some (RetBV64 F.RDX) ]


$(pure [])

resolveReoptFunType :: Monad m
                    => ReoptFunType
                    -> ExceptT ArgResolverError m X86FunTypeInfo
resolveReoptFunType (ReoptNonvarargFunType ftp) =
  resolveAnnFunType ftp
resolveReoptFunType (ReoptPrintfFunType i) =
  pure $! X86PrintfFunType i
resolveReoptFunType ReoptUnsupportedFunType =
  pure $! X86UnsupportedFunType

$(pure [])

--------------------------------------------------------------------------------
-- UseMap

-- | Type synonym to identify segment offsets that should correspond
-- to the start of a function.
type FunAddress w = MemSegmentOff w

type BlockSize = Word64

-- | Maps offsets within a region to the size and starting addresses
-- of functions
type FunUseOffsetMap w = Map BlockOff (BlockOff, [FunAddress w])

type BlockOff = Word64

-- | Create map with a single region
initFunUseOffsetMap :: FunAddress w -> BlockOff -> BlockOff -> FunUseOffsetMap w
initFunUseOffsetMap f off endOff = Map.singleton off (endOff, [f])

-- | Replace a region at a given block
replaceRegion :: BlockOff
              -> FunAddress w -- ^ New function
              -> BlockOff -- ^ End offset of new function block
              -> [FunAddress w] -- ^ Old functions
              -> BlockOff -- ^ Size of old function blocks
              -> FunUseOffsetMap w
              -> FunUseOffsetMap w
replaceRegion off f newEnd oldFuns oldEnd m =
  case compare oldEnd newEnd of
    LT -> Map.insert off (oldEnd, (f:oldFuns)) $
          Map.insert oldEnd (newEnd, [f]) m
    EQ -> Map.insert off (oldEnd, (f:oldFuns)) m
    GT -> Map.insert off (newEnd, (f:oldFuns)) $
          Map.insert newEnd (oldEnd, [f]) m

-- | Update funUseOffsetMap with new address
updateFunUseOffsetMap :: FunAddress w
                      -> BlockOff
                      -> BlockOff -- ^ end offset
                      -> FunUseOffsetMap w
                      -> FunUseOffsetMap w
updateFunUseOffsetMap f off newEnd old = do
  case Map.lookupLT newEnd old of
    Nothing -> Map.insert off (newEnd, [f]) old
    Just (oldOff, (oldEnd, oldFuns))
      -- If old region ends before this one, then just add new region.
      | oldEnd <= off -> Map.insert off (newEnd, [f]) old
      | otherwise ->
          case compare oldOff off of
            LT ->
              replaceRegion off f newEnd oldFuns oldEnd $
                Map.insert oldOff (off, oldFuns) old
            EQ -> replaceRegion off f newEnd oldFuns oldEnd old
            GT ->
              updateFunUseOffsetMap f off oldOff $
                replaceRegion oldOff f newEnd oldFuns oldEnd old

-- | Maps regions indices for code to offsets witin region to the size of the segment and a list
-- of functions that occupy that region.
--
-- This maintains the invariant that the list of functions are
-- non-empty, and no regions overlap.
newtype FunUseMap w = FunUseMap (Map RegionIndex (FunUseOffsetMap w))

-- | Empty use map
emptyFunUseMap :: FunUseMap w
emptyFunUseMap = FunUseMap Map.empty

-- | Mark a region as belonging to a particular function
recordRegionUse :: FunAddress w -> MemSegmentOff w -> BlockSize -> State (FunUseMap w) ()
recordRegionUse f so sz = do
  let a = segoffAddr so
      -- Offset of block
      off :: BlockOff
      off = memWordValue (addrOffset a)
      initMap = initFunUseOffsetMap f off (off+sz)
      updMap _ old = updateFunUseOffsetMap f off (off + sz) old
  modify $ \(FunUseMap regionMap) ->
     FunUseMap (Map.insertWith updMap (addrBase a) initMap regionMap)

-- | Record memory used by block to function address
recordBlockUse :: FunAddress (ArchAddrWidth arch)
               -> ParsedBlock arch ids
               -> State (FunUseMap (ArchAddrWidth arch)) ()
recordBlockUse f b = do
  recordRegionUse f (pblockAddr b) (fromIntegral (blockSize b))
  -- Record jumptable backing as well.
  case pblockTermStmt b of
    ParsedLookupTable layout _ _ _ -> do
      let a = jtlBackingAddr layout
          sz = jtlBackingSize layout
      recordRegionUse f a sz
    _ -> pure ()

-- | Record memory used by block to function address
recordFunUse :: DiscoveryFunInfo arch ids
             -> State (FunUseMap (ArchAddrWidth arch)) ()
recordFunUse f =
  mapM_ (recordBlockUse (discoveredFunAddr f)) (f^.parsedBlocks)

mkFunUseMap :: DiscoveryState arch -> FunUseMap (ArchAddrWidth arch)
mkFunUseMap s = flip execState emptyFunUseMap $ do
  mapM_ (\(Some f) -> recordFunUse f) (s^.funInfo)

-- | Return the number of bytes only allocated to the function.
endOwnedByFun :: FunAddress w -> BlockOff -> FunUseOffsetMap w -> BlockOff -> BlockOff
endOwnedByFun f o m regionSize =
  case Map.lookupGE o m of
    Nothing -> regionSize
    Just (s, (e, fns))
      | fns == [f] -> endOwnedByFun f e m regionSize
      | otherwise -> s

-- | Return the number of bytes starting from function entry
-- point in the segment that are exclusive allocated to function.
lookupFunSize :: MemWidth w => MemSegmentOff w -> FunUseMap w -> Word64
lookupFunSize f (FunUseMap m) =
  let seg = segoffSegment f
      a = segoffAddr f
      o = memWordValue (addrOffset a)
      err = error "internal error: Function unregistered"
      offMap = Map.findWithDefault err (addrBase a) m
      maxValue = memWordValue (segmentSize seg) - memWordValue (segoffOffset f)
      endOff = endOwnedByFun f o offMap maxValue
   in endOff - o

$(pure [])

--------------------------------------------------------------------------------
-- recoverX86Elf

-- | Analyze an elf binary to extract information.
--
--  Note. This prints warnings to stderr and may exit if the Elf file cannot be parsed.
recoverX86Elf :: (GetFnsLogEvent -> IO ())
               -- ^ Logging function for errors
               -> FilePath
               -- ^ Path to binary for exploring CFG
               -> LoadOptions
               -- ^ Option to load the binary at the given address
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> ReoptOptions -- ^ User
               -> AnnDeclarations
               -- ^ Header with hints for assisting typing.
               -> BSC.ByteString -- ^ Prefix to use if we need to generate new function endpoints later.
               -> IO ( Elf.ElfHeaderInfo 64
                     , X86OS
                     , DiscoveryState X86_64
                     , RecoveredModule X86_64
                     , MergeRelations
                     )
recoverX86Elf logger path loadOpts disOpt reoptOpts hdrAnn unnamedFunPrefix = do
  bs <- checkedReadFile path
  hdrInfo <- parseElfHeaderInfo64 path bs
  let hdr = Elf.header hdrInfo

  os <- getX86ElfArchInfo (Elf.headerMachine hdr) (Elf.headerOSABI hdr)

  let ainfo = osArchitectureInfo os
  let pltFn = processX86PLTEntries
  initState <- initDiscovery loadOpts hdrInfo pltFn

  (debugTypeMap, discState) <-
    doDiscovery logger hdrAnn hdrInfo ainfo initState disOpt reoptOpts
  let mem = memory discState

  let symAddrMap = initDiscSymAddrMap initState
  when (isUsedPrefix unnamedFunPrefix symAddrMap) $ do
    hPutStrLn stderr $
      printf "No symbol in the binary may start with the prefix %d."
             (BSC.unpack unnamedFunPrefix)
    exitFailure

  let sysp = osPersonality os

  -- Compute the function demands
  let fDems =
        -- Generate map from symbol names to known type.
        let symFunABIMap :: Map BS.ByteString (KnownFunABI X86Reg)
            symFunABIMap = Map.mapMaybe toKnownFunABI (nameTypeMap debugTypeMap)
            -- Generate map from address names to known type.
            --
            -- This is used when we see a function jumps to a defined address.
            addrFunABIMap = Map.mapMaybe toKnownFunABI (addrTypeMap debugTypeMap)
            -- Compute only those functions whose types are not known.
            notKnown (Some f) = not (Map.member (discoveredFunAddr f) addrFunABIMap)
         in inferFunctionTypeFromDemands $
              functionDemands (x86DemandInfo sysp) addrFunABIMap symFunABIMap mem $
                filter notKnown $ exploredFunctions discState

  let resolveX86Type :: ReoptFunType -> Maybe X86FunTypeInfo
      resolveX86Type rtp =
        case runExcept (resolveReoptFunType rtp) of
          Left _ -> Nothing
          Right r -> Just r

  -- Maps address to name of function to use.
  let funNameMap ::  Map (MemSegmentOff 64) BS.ByteString
      funNameMap =
        Map.mapWithKey (\addr qnm -> localFunctionName unnamedFunPrefix qnm addr)
                       (samAddrMap symAddrMap)
        <> Map.fromList [ (addr, nosymFunctionName unnamedFunPrefix addr)
                        | Some finfo <- exploredFunctions discState
                        , let addr = discoveredFunAddr finfo
                        , Map.notMember addr (samAddrMap symAddrMap)
                        ]
  let funTypeMap ::  Map BS.ByteString X86FunTypeInfo
      funTypeMap = Map.mapMaybe resolveX86Type (nameTypeMap debugTypeMap)
                <> Map.fromList [ (recoveredFunctionName symAddrMap unnamedFunPrefix addr, xtp)
                                | (addr,rtp) <- Map.toList (addrTypeMap debugTypeMap)
                                , Right xtp <- [runExcept (resolveReoptFunType rtp)]
                                ]
                <> Map.fromList [ (recoveredFunctionName symAddrMap unnamedFunPrefix addr, tp)
                                | (addr,tp) <- Map.toList fDems
                                ]
                <> Map.fromList [ (nm, defaultX86Type) | (_addr,nm) <- initDiscPLTFuns initState ]
  fnDefs <- fmap catMaybes $
    forM (exploredFunctions discState) $ \(Some finfo) -> do
      let faddr = discoveredFunAddr finfo
      let dnm = discoveredFunSymbol finfo
      logger $ StartFunRecovery dnm faddr
      case checkFunction finfo of
        FunctionOK -> do
          case recoverFunction sysp funNameMap funTypeMap mem finfo of
            Left msg -> do
              logger $ RecoveryFailed dnm faddr msg
              pure Nothing
            Right (warnings, fn) -> do
              mapM_ (logger . RecoveryWarning dnm faddr) warnings
              pure (Just fn)
        FunctionHasPLT -> do
          logger $ RecoveryPLTSkipped dnm faddr
          -- Skip PLT functions with no error message.
          pure Nothing
        FunctionIncomplete -> do
          logger $ RecoveryFailed dnm faddr "Incomplete discovery."
          pure Nothing
  -- Get list of names of functions defined
  let definedNames :: Set.Set BSC.ByteString
      definedNames = Set.fromList $
        recoveredFunctionName symAddrMap unnamedFunPrefix . fnAddr <$> fnDefs

  -- Get all functions that are referenced, but not defined in the module.
  let declFunTypeMap :: FunctionTypeMap X86_64
      declFunTypeMap = foldl (getReferencedFunctions definedNames) Map.empty fnDefs

  let fnDecls =  [ FunctionDecl { funDeclName = nm
                                , funDeclType = tp
                                }
                 | (nm, tp) <- Map.toList declFunTypeMap
                 ]
  let recMod = RecoveredModule { recoveredDecls = fnDecls
                               , recoveredDefs  = fnDefs
                               }


  -- Maps ranges of addresses used in binary to the starting address
  -- of functions found.
  --
  -- Used to compute sizes of functions for overwriting purposes.
  let addrUseMap :: FunUseMap 64
      addrUseMap = mkFunUseMap discState

  let mkObjFunDef :: Function X86_64 -> ObjFunDef
      mkObjFunDef f =
        ObjFunDef { ofdObjName = fnName f
                  , ofdBinAddr = memWordValue (addrOffset (segoffAddr (fnAddr f)))
                  , ofdBinSize = lookupFunSize (fnAddr f) addrUseMap
                  }


    -- Map name of declared functions to the address in binary (if any)
  let undefinedFuns :: V.Vector ObjFunRef
      undefinedFuns = V.fromList
        [ ObjFunRef { ofrObjName = name, ofrBinAddr = memWordValue addr }
        | (segOff, name) <- Map.toList funNameMap
        , not (Set.member name definedNames)
        , let addr = addrOffset (segoffAddr segOff)
        ]


  let mergeRel =
        MergeRelations
        { mrObjectFuns = mkObjFunDef <$> V.fromList fnDefs
        , mrUndefinedFuns = undefinedFuns
        }
  seq recMod $ pure (hdrInfo, os, discState, recMod, mergeRel)

$(pure [])

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
    let llc_opts = Ext.LLCOptions { Ext.llcTriple    = Just arch
                                  , Ext.llcOptLevel  = optLevel
                                  , Ext.llcFunctionSections = True
                                  }
    asm <- Ext.runLLC llcPath llc_opts llvm_opt
    Ext.runLlvmMc llvmMcPath asm arch

  case mres of
    Left f -> do
      hPutStrLn stderr (show f)
      exitFailure
    Right b -> return b

-- | Merge a binary and new object
mergeAndWrite :: FilePath -- ^ Path to write output to.
              -> Elf.ElfHeaderInfo 64 -- ^ Original binary
              -> FilePath
              -- ^ Path or name to use to refer to new object code in
              -- error messages.
              -> BS.ByteString -- ^ New object contents
              -> MergeRelations
              -> IO ()
mergeAndWrite outputPath origBinary objName objContents mergeRel = do
  newObjHdrInfo <- parseElfHeaderInfo64 objName objContents
  newBinary <- mergeObject origBinary newObjHdrInfo mergeRel
  BSL.writeFile outputPath newBinary
  -- Update the file mode
  do fs <- getFileStatus outputPath
     let fm = ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode
     setFileMode outputPath (fileMode fs `unionFileModes` fm)
