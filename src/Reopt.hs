{-# LANGUAGE LambdaCase #-}
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
  , parseElfHeaderInfo64
  , showPaddedHex
  , checkedReadFile
  , isCodeSection
  , printX86SectionDisassembly
    -- * Architecture info
  , SomeArchitectureInfo(..)
    -- * Code discovery
  , ReoptOptions(..)
  , defaultReoptOptions
  , discoverBinary
  , InitDiscoveryComp
    -- ** Initialization
  , InitDiscovery
  , initDiscSymAddrMap
  , initDiscovery
  , SymAddrMap
    -- * Function recovery
  , Reopt.TypeInference.HeaderTypes.AnnDeclarations
  , Reopt.TypeInference.HeaderTypes.emptyAnnDeclarations
  , Reopt.TypeInference.Header.parseHeader
  , RecoveredModule(..)
  , Reopt.Events.ReoptLogEvent(..)
  , Reopt.Events.ReoptStep(..)
  , Reopt.Events.ReoptEventSeverity(..)
  , Reopt.Events.isErrorEvent
  , recoverFunctions
  , resolveHeader
    -- * Object merging
  , Reopt.Relinker.MergeRelations
  , mergeAndWrite
    -- * LLVM
  , LLVMVersion
  , versionOfString
  , LLVMConfig
  , latestLLVMConfig
  , getLLVMConfig
  , compileLLVM
  , renderLLVMBitcode
  , defaultLLVMGenOptions
  , Reopt.CFG.LLVM.LLVMGenOptions(..)
  , LLVM.x86LLVMArchOps
  , llvmAssembly
    -- * X86 specific
  , X86OS(..)
  , osPersonality
  , osLinkName
  , recoverX86Elf
  , runReoptInIO
  , recoverX86Elf'
  , x86OSForABI
  , osArchitectureInfo
  , processX86PLTEntries
  , Reopt.CFG.FnRep.X86.X86FnStmt
    -- * Utility
  , copyrightNotice
    -- * Reporting
  , Reopt.Events.ReoptStepTag(..)
  , Reopt.Events.ReoptErrorTag(..)
  , Reopt.Events.ReoptStats(..)
  , Reopt.Events.initReoptStats
  , Reopt.Events.statsHeader
  , Reopt.Events.statsRows
  , Reopt.Events.statsStepErrorCount
  , Reopt.Events.mergeFnFailures
  , Reopt.Events.renderAllFailures
  , Reopt.Events.stepErrorCount
  , joinLogEvents
  , printLogEvent
  , recoverLogEvent
  , warnABIUntested
    -- * Re-exports
  , Data.Macaw.CFG.MemWidth
  , Data.Macaw.CFG.IsArchStmt
  , Reopt.CFG.FnRep.FnArchStmt
  , Data.Macaw.Memory.ElfLoader.LoadOptions(..)
  , Data.Macaw.Memory.ElfLoader.defaultLoadOptions
  , Data.Macaw.CFG.MemSegmentOff
  , Data.Macaw.CFG.segoffAddr
  , Data.Macaw.CFG.addrBase
  , Data.Macaw.CFG.addrOffset
  , Data.Macaw.CFG.memWordValue
  , Data.Macaw.X86.X86_64
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
import           Data.Foldable
import           Data.IORef
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
import           Numeric.Natural (Natural)
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import qualified Text.PrettyPrint.HughesPJ as HPJ
import           Text.Printf (printf)

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Analysis.RegisterUse (callArgValues)
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import           Data.Macaw.Memory.ElfLoader
import           Data.Macaw.Utils.IncComp
import           Data.Macaw.X86 (X86_64, X86TermStmt(..))
import qualified Data.Macaw.X86 as X86
import           Data.Macaw.X86.SyscallInfo
import           Data.Macaw.X86.X86Reg

import           Reopt.TypeInference.HeaderTypes
import           Reopt.ArgResolver
import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86
import           Reopt.CFG.FunctionCheck
import           Reopt.CFG.LLVM (LLVMArchSpecificOps, LLVMGenOptions(..), moduleForFunctions)
import qualified Reopt.CFG.LLVM.X86 as LLVM
import           Reopt.CFG.Recovery
import           Reopt.Events
import qualified Reopt.ExternalTools as Ext
import           Reopt.Hints
import           Reopt.PltParser
import           Reopt.Relinker
import           Reopt.TypeInference.DebugTypes
import           Reopt.TypeInference.FunTypeMaps
import           Reopt.TypeInference.Header
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

----------------------------------------------------------------------------------
-- Resolution functions

-- | Region index to use for resolving include/exclude values or `Nothing`
-- if this is a object file compiled with function sections and there is no
-- default region.
data RegionInfo
   = HasDefaultRegion !RegionIndex
   | ObjectFunctionSections

-- | Attempt to find the address of a string identifying a symbol
-- name, and return either the string if it cannot be resolved or the
-- address.
resolveSymAddr :: Memory w
                  -- ^ Loaded memory object.
               -> RegionInfo
                  -- ^ Region index for resolving addresses or why it is missing.
               -> SymAddrMap w
                 -- ^ Map from symbol names in binary to associated addresses.
               -> String
                  -- ^ The name of a symbol as a string.
               -> Except String (MemSegmentOff w)
resolveSymAddr mem regInfo symMap nm0 = addrWidthClass (memAddrWidth mem) $
  case resolveSymName nm0 of
    AddrIdent w -> do
      regIdx <-
        case regInfo of
          ObjectFunctionSections -> do
            throwError "Addresses unsupported in object files compiled with function sections."
          HasDefaultRegion regIdx ->
            pure regIdx
      case resolveRegionOff mem regIdx (fromIntegral w) of
        Just off ->
          pure off
        Nothing -> do
          throwError $ "Could not resolve address: " ++ nm0
    SymbolIdent nm ->
      case symAddrMapLookup symMap nm of
        Left _ -> do
          throwError $ "Could not resolve symbol: " ++ nm0
        Right a ->
          pure a

-- | Information from user to control which addresses to include and
-- exclude.
data ReoptOptions =
  ReoptOptions { roIncluded :: [String] -- ^ Symbols/addresses user wanted included
               , roExcluded :: [String] -- ^ Symbols/addresses user wanted exluded.
               }

-- | Reopt options with no additional functions to explore or not explore.
defaultReoptOptions :: ReoptOptions
defaultReoptOptions = ReoptOptions { roIncluded = [], roExcluded = [] }

addKnownFn :: SymAddrMap w
           -> BS.ByteString
           -> NoReturnFunStatus
           -> Map (MemSegmentOff w) NoReturnFunStatus
           -> Map (MemSegmentOff w) NoReturnFunStatus
addKnownFn sam nm noRet m0 =
  let s = Map.findWithDefault Set.empty nm (samNameMap sam)
   in foldl (\m a -> Map.insert a noRet m) m0 s

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

$(pure [])

--------------------------------------------------------------------------------
-- InitDiscovery

-- | Information returned by `initDiscovery` below.
data InitDiscovery arch
  = InitDiscovery
    { initDiscSymAddrMap :: !(SymAddrMap (ArchAddrWidth arch))
      -- ^ Map from symbols to addresses.
    , initDiscBaseCodeAddr :: !(MemAddr (ArchAddrWidth arch))
      -- ^ Address to use as base address for program counters in
      -- Dwarf debug information.
    , initDiscoveryState :: !(DiscoveryState arch)
    }

$(pure [])

------------------------------------------------------------------------
-- InitDiscoveryComp

-- | Computation that logs string messages.
type InitDiscM r = IncCompM String r

-- | Warning in the initialization code.
initWarning :: String -> InitDiscM r ()
initWarning = incCompLog

-- | Report a fatal error in an incremental computation that may fail.
fatalError :: String -> IncCompM l (Either String r) a
fatalError = incCompDone . Left

-- | Run the reopt operations in IO.
runReoptInIO :: (l -> IO ())
             -> IncCompM l (Either String a) a
             -> IO a
runReoptInIO logger m = do
  res <- processIncCompLogs logger $ runIncCompM (fmap Right m)
  case res of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right r -> do
      pure r

type InitDiscoveryComp r  = IncComp String (Either String r)

$(pure [])

------------------------------------------------------------------------
-- Utilities

-- | Run a computation with the given section, and print a warning if
-- the section is not found or multiply defined.
withShdr :: Map BS.ByteString [s]
               -- ^ Map from section names to sections with that name.
            -> String -- ^ Name of section for warning messages.
            -> BS.ByteString -- ^ Name of section in Elf file.
            -> Bool -- ^ Flag that should be true if expected
            -> a -- ^ Value if nothing defined.
            -> (s -> InitDiscM r a)
            -> InitDiscM r a
withShdr sectionNameMap warnName secName expected d f =
  case Map.findWithDefault [] secName sectionNameMap of
    []    -> do
      when expected $ do
        initWarning $ "Could not find " ++ warnName ++ " sections."
      pure d
    _:_:_ -> do
      initWarning $ "Found multiple " ++ warnName ++ " sections."
      pure d
    [s] -> f s

------------------------------------------------------------------------
-- PLT functions

-- | Add Plt symbols
addPltSyms
  :: forall w r
  .  (Num (MemWord w), Integral (Elf.ElfWordType w))
  => Elf.ElfHeader w
  -> BS.ByteString -- ^ String table contents
  -> BS.ByteString -- ^ Symbol table contents
  -> Memory w
  -> RegionIndex
  -> SymAddrMap w
  -> PltInfo w
  -> InitDiscM r (SymAddrMap w)
addPltSyms hdr strtab symtab mem regIdx m0 p = do
  let cl = Elf.headerClass hdr
  let dta = Elf.headerData hdr
  let ins :: Elf.ElfWordType w
          -> (ResolvedPltEntry, Elf.ElfWordType w)
          -> (SymAddrMap w -> InitDiscM r (SymAddrMap w))
          -> (SymAddrMap w -> InitDiscM r (SymAddrMap w))
      ins o (f, _sz) cont m = do
        case resolveRegionOff mem regIdx (fromIntegral o) of
          Nothing -> do
            initWarning $ printf "Unexpected symbol offset %s." (showHex (toInteger o) "")
            cont m
          Just a -> do
            case f of
              PltStub idx ->
                case Elf.decodeSymtabEntry cl dta strtab symtab idx of
                  Left e -> do
                    initWarning (show e)
                    cont m
                  Right sym -> do
                    cont $! symAddrMapInsert sym a m
              PltNotCallable ->
                cont m
  Map.foldrWithKey' ins pure (pltMap p) m0

{-
-- | This match a X86_64 PLT stub.
--
-- The format is
--   jmpq off(%rip)  -- This is "ff 25" followed by (got address) - rip.
--   pushq idx      -- This is "68" followed by a 4 byte index into GOT table.
--   jmpq plt entry -- This is "e9" followed by a 4-byte signed int
matchPLTStub :: Elf.ElfData -- ^ Endianess of Elf file
             -> MatchPLTStubArgs
             -> Int -- ^ Index of PLT entry (first function is at zero)
             -> ExceptT () (StateT (PltMap 64) (InitDiscM r)) ()
matchPLTStub dta args idx = do
  -- Offset of PLT stub in data
  let off :: Int
      off = 0x10 * (idx+1)
  -- Get relocation entry for this PLT stub.
  let rela :: Elf.RelaEntry Elf.X86_64_RelocationType
      rela = Elf.decodeRelaEntry dta (pltPLTRelaData args) idx
  lift $ lift $ do
    unless (Elf.relaType rela == Elf.R_X86_64_JUMP_SLOT) $ do
      initWarning $ "PLT stub relocation type is not a jump slot."
    unless (Elf.relaAddend rela == 0) $ do
      initWarning $ "PLT stub relocation addened is non-zero."

  -- Get symbol index for relation
  let symIndex :: Word32
      symIndex = Elf.relaSym rela
  let symtab = pltSymtab args
  sym <-
    case Elf.decodeSymtabEntry Elf.ELFCLASS64 dta (pltStrtab args) symtab symIndex of
      Left err -> do
        lift $ lift $ initWarning (show err)
        throwError ()
      Right entry ->
        pure entry
  lift $ lift $ do
    unless (Elf.steType sym == Elf.STT_FUNC) $
      initWarning "PLT symbol must be a function."
    unless (Elf.steBind sym == Elf.STB_GLOBAL) $
      initWarning "PLT symbol must have global vinding."
    unless (Elf.steIndex sym == Elf.SHN_UNDEF) $
      initWarning "PLT symbol is must be undefined."
    unless (Elf.steValue sym == 0 && Elf.steSize sym == 0) $ do
      initWarning "PLT symbol must be unassigned."
  a <-
    case incSegmentOff (pltMemAddr args) (toInteger off) of
      Nothing -> do
        lift $ lift $ initWarning "PLT does not appear to have a valid address."
        throwError ()
      Just a -> pure a
  modify $ \pltRes ->
    pltRes { pltSymAddrMap = symAddrMapInsert sym a (pltSymAddrMap pltRes)
           }
-}

shdrContents :: Integral (Elf.ElfWordType w)
             => Elf.ElfHeaderInfo w -> Elf.Shdr nm (Elf.ElfWordType w) -> BS.ByteString
shdrContents hdrInfo shdr =
  let fileOff = Elf.shdrOff shdr
      size    = Elf.shdrSize shdr
   in BS.take (fromIntegral size)
    $ BS.drop (fromIntegral fileOff)
    $ Elf.headerFileContents hdrInfo

processX86PLTEntries :: Elf.ElfHeaderInfo 64
                     -> InitDiscM r (Maybe (PltInfo 64))
processX86PLTEntries hdrInfo  = do
  let shdrs =
        case Elf.headerNamedShdrs hdrInfo of
          Left _ -> V.empty
          Right r -> r
  case extractPLTEntries hdrInfo shdrs of
    Left err -> do
      incCompLog err
      pure Nothing
    Right m ->
      pure m
{-
processX86PLTEntries hdrInfo symtab strtab shdrNameMap mem pltRes = do
  let dta = Elf.headerData (Elf.header hdrInfo)
  withShdr shdrNameMap "PLT" ".plt" True pltRes $ \pltIdx -> do
    let pltShdr = Elf.shdrByIndex hdrInfo pltIdx
    let pltSize = Elf.shdrSize pltShdr
--    let pltData = shdrContents hdrInfo pltShdr
    case Map.lookup pltIdx (memSectionIndexMap mem) of
      Nothing -> do
        initWarning "PLT section is not loaded in memory"
        pure pltRes
      Just pltAddr -> do
        let pltRes2 = pltRes { pltBounds = Just (pltAddr, fromIntegral pltSize) }
        withShdr shdrNameMap "PLT relocation" ".rela.plt" True pltRes2 $ \relaIdx -> do
          let relaShdr = Elf.shdrByIndex hdrInfo relaIdx
          let relaData = shdrContents hdrInfo relaShdr
          let (relaCount, r) = BS.length relaData `quotRem` Elf.relaEntSize Elf.ELFCLASS64
          when (r /= 0) $ do
            initWarning ".rela.plt data is not a multiple of relocation entry size."
          -- Find base address of PLT stubs
          let pltStubArgs = MatchPLTStubArgs { pltMemAddr = pltAddr
                                             , pltPLTRelaData = relaData
                                             , pltSymtab = symtab
                                             , pltStrtab = strtab
                                             }
          flip execStateT pltRes2 $ forM_ [0..relaCount-1] $ \i -> do
            void $ runExceptT (matchPLTStub dta pltStubArgs i)
-}

$(pure [])

------------------------------------------------------------------------
--X86 calling convention

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

-- | Warning message when encountering untested abi.
warnABIUntested :: Elf.ElfOSABI -> String
warnABIUntested abi = printf "Warning: Support for %s binaries is untested." (show abi)

-- | Maps x86 ABIs to the x86 OS value we use for it.
x86ABIMap :: Map Elf.ElfOSABI X86OS
x86ABIMap = Map.fromList
  [ (Elf.ELFOSABI_LINUX,    Linux)
  , (Elf.ELFOSABI_SYSV,     Linux)
  , (Elf.ELFOSABI_FREEBSD, FreeBSD)
  , (Elf.ELFOSABI_NETBSD,  FreeBSD)
  ]

-- | Return x86 OS value for Elf file with given ABI
x86OSForABI :: Elf.ElfOSABI -> Maybe X86OS
x86OSForABI abi = Map.lookup abi x86ABIMap

------------------------------------------------------------------------
-- Get binary information

type ProcessPLTEntries w
   = forall r
  .   Elf.ElfHeaderInfo w
   -> InitDiscM r (Maybe (PltInfo w))

data SomeArchitectureInfo w where
  SomeArch :: !(ArchitectureInfo arch)
           -> !(ProcessPLTEntries (ArchAddrWidth arch))
           -> SomeArchitectureInfo (ArchAddrWidth arch)

getElfArchInfo :: Elf.ElfClass w -> Elf.ElfMachine -> Elf.ElfOSABI -> IO (SomeArchitectureInfo w)
getElfArchInfo cl arch abi =
  case (cl, arch) of
    (Elf.ELFCLASS64, Elf.EM_X86_64) -> do
      -- Test if ABI is known.
      when (isNothing (x86OSForABI abi)) $ hPutStrLn stderr $ warnABIUntested abi
      pure $! SomeArch X86.x86_64_linux_info processX86PLTEntries
#ifdef SUPPORT_ARM
    (Elf.ELFCLASS32, Elf.EM_ARM) -> do
      when (abi /= ELFOSABI_SYSV) $ hPutStrLn stderr $ warnABIUntested abi
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure $! SomeArch armArch32le ignorePLTEntries
    (Elf.ELFCLASS64, Elf.EM_AARCH64) -> do
      when (abi /= ELFOSABI_SYSV) $ hPutStrLn stderr $ warnABIUntested abi
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

$(pure [])

------------------------------------------------------------------------
-- Explore a control flow graph.

elfInstances :: Elf.ElfHeaderInfo w
             -> ((MemWidth w, Integral (Elf.ElfWordType w), Show (Elf.ElfWordType w)) => a)
             -> a
elfInstances hdr x =
  case Elf.headerClass (Elf.header hdr) of
    Elf.ELFCLASS32 -> x
    Elf.ELFCLASS64 -> x

-- | Insert a symbol table entry into map.
--
-- This is used in dynamic binaries.
insSymbol :: forall r w
          . (MemWidth w, Integral (Elf.ElfWordType w))
          => Memory w
          -- ^ Loaded memory
          -> MemAddr w
          -- ^ Base address that binary is loaded at
          -> (Int, Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w))
          -> StateT (SymAddrMap w) (InitDiscM r) ()
insSymbol mem baseAddr (idx, symEntry)
  -- Skip non-function symbols
  | Elf.steType symEntry /= Elf.STT_FUNC = do
      pure ()
  | Elf.steIndex symEntry == Elf.SHN_UNDEF = do
      pure ()
  | BS.null (Elf.steName symEntry) = do
      lift $ initWarning (show (EmptySymbolName idx (Elf.steType symEntry)))
  | Elf.steIndex symEntry == Elf.SHN_ABS = do
    if addrBase baseAddr == 0 then do
      let val = Elf.steValue symEntry
      case resolveAbsoluteAddr mem (fromIntegral val) of
        Just addr ->
          modify $ symAddrMapInsert symEntry addr
        Nothing -> do
          lift $ initWarning $ show $ CouldNotResolveAddr (Elf.steName symEntry)
          pure ()
     else
      lift $ initWarning "SHN_ABS symbols not supported in dynamic binaries."
  | otherwise = do
      let nm = Elf.steName symEntry
      let val = Elf.steValue symEntry
      -- Get memory address of symbol
      let symAddr :: MemAddr w
          symAddr = incAddr (toInteger val) baseAddr
      -- Resolve address as segment offset.
      case asSegmentOff mem symAddr of
        Just addr ->
          modify $ symAddrMapInsert symEntry addr
        Nothing -> do
          lift $ initWarning (show (CouldNotResolveAddr nm))

withSymtab
  :: Integral (Elf.ElfWordType w)
  => Elf.ElfHeaderInfo w
    -- ^ Header information
  -> Map BS.ByteString [Word16]
    -- ^ Map from names to section indices with name.
  -> String
  -> BS.ByteString
  -> a -- ^ Value if nothing defined.
  -> (BS.ByteString -> BS.ByteString -> InitDiscM r a)
    -- ^ Continuation to run to get symtab and string table (respectively).
  -> InitDiscM r a
withSymtab hdrInfo shdrNameMap warnName secName d f = do
  withShdr shdrNameMap warnName secName False d $ \symtabIdx -> do
    let symtabShdr = Elf.shdrByIndex hdrInfo symtabIdx
    let symtabData = shdrContents hdrInfo symtabShdr
    let strtabIdx = Elf.shdrLink symtabShdr
    if strtabIdx == 0 || strtabIdx >= fromIntegral (Elf.shdrCount hdrInfo) then do
      initWarning $ printf "Invalid string table index %s in %s." (show strtabIdx) (BSC.unpack secName)
      pure d
     else do
      let strtabShdr = Elf.shdrByIndex hdrInfo (fromIntegral strtabIdx)
      let strtabData = shdrContents hdrInfo strtabShdr
      f symtabData strtabData

addDefinedSymbolTableFuns
  :: (MemWidth w, Integral (Elf.ElfWordType w))
  => Elf.ElfHeaderInfo w
  -> Memory w  -- ^ Memory created from binary.
  -> MemAddr w -- ^ Address binary is loaded at
  -> BS.ByteString -- ^ Symbol table for parsing.
  -> BS.ByteString -- ^ String table
  -> StateT (SymAddrMap w) (InitDiscM r)  ()
addDefinedSymbolTableFuns hdrInfo mem baseAddr symtabData strtab = do
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
                lift $ initWarning $ "Failed to parse symbol table entry " ++ show e
                go (idx+1)
              Right symEntry -> do
                insSymbol mem baseAddr (fromIntegral idx, symEntry)
                go (idx+1)
  go 1

#ifdef SUPPORT_ARM
ignorePLTEntries :: ProcessPLTEntries w
ignorePLTEntries _ _ _ _ _ _ _ = pure ()
#endif

$(pure [])

reportSymbolResError :: SymbolResolutionError -> State [SymbolResolutionError] ()
reportSymbolResError e = seq e $ modify $ \s -> (e:s)

-- | Resolve a symbol table entry in an object file.
resolveObjSymbol :: Elf.ElfHeaderInfo w
                 -> Memory w
                 -> Map Word16 (MemSegmentOff w)
                    -- ^ Map from section index to offset in memory of section.
                 -> (SymAddrMap w)
                    -- ^ Symbol addr map
                 -> (Int, Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w))
                    -- ^ Index of symbol in symbol table and entry.
                 -> State [SymbolResolutionError] (SymAddrMap w)
resolveObjSymbol hdrInfo mem secMap sam (idx, ste) = elfInstances hdrInfo $ do
  let secIdx = Elf.steIndex ste
  if Elf.steType ste /= Elf.STT_FUNC then do
    pure sam
   else if secIdx == Elf.SHN_UNDEF then
    pure sam
   else if Elf.steName ste == "" then do
    reportSymbolResError $ EmptySymbolName idx (Elf.steType ste)
    pure sam
   else if secIdx == Elf.SHN_ABS then do
    let val = Elf.steValue ste
    case resolveAbsoluteAddr mem (fromIntegral val) of
      Just addr -> do
        pure $! symAddrMapInsert ste addr sam
      Nothing -> do
        reportSymbolResError $ CouldNotResolveAddr (Elf.steName ste)
        pure sam
   else if Elf.fromElfSectionIndex secIdx >= Elf.shdrCount hdrInfo then do
    reportSymbolResError $ CouldNotResolveAddr (Elf.steName ste)
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
        reportSymbolResError $ CouldNotResolveAddr (Elf.steName ste)
        pure sam

$(pure [])

initDiscState :: Memory (ArchAddrWidth arch) -- ^ Initial memory
              -> Maybe (ArchSegmentOff arch) -- ^ Entry point
              -> RegionInfo -- ^ Region information
              -> SymAddrMap (ArchAddrWidth arch) -- ^ Symbol addr map
              -> (ArchSegmentOff arch -> Bool) -- ^ Explore predicate
              -> ArchitectureInfo arch
              -> ReoptOptions
              -> Except String (DiscoveryState arch)
initDiscState mem entryPoint regInfo symAddrMap explorePred ainfo reoptOpts = do
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
        excludeAddrs <- mapM (resolveSymAddr mem regInfo symAddrMap) excludeNames
        let s = Set.fromList excludeAddrs
        let initState = emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
                      & trustedFunctionEntryPoints .~ entryPoints
                      & exploreFnPred .~ (\a -> Set.notMember a s && explorePred a)
                      & markAddrsAsFunction InitAddr (Map.keys entryPoints)
        pure $! initState
      (includeNames, []) -> do
        includeAddrs <- mapM (resolveSymAddr mem regInfo symAddrMap) includeNames
        let s = Set.fromList includeAddrs
        let initState = emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
                      & trustedFunctionEntryPoints .~ entryPoints
                      & exploreFnPred .~ (\a -> Set.member a s)
                      & markAddrsAsFunction InitAddr s
        pure $! initState
      _ -> do
        throwError "Cannot both include and exclude specific addresses."
  case entryPoint of
    Nothing -> pure $! s
    Just entry -> pure $! s & markAddrAsFunction InitAddr entry

$(pure [])

-- | Identiy symbol names
discoverSymbolNames :: (MemWidth w, Integral (Elf.ElfWordType w))
                    => Elf.ElfHeaderInfo w -- ^ Binary information
                    -> Memory w  -- ^ Initial memory for binary
                    -> MemAddr w -- ^ Base address to add to symbol offsets.
                    -> InitDiscM r (SymAddrMap w)
discoverSymbolNames hdrInfo mem baseAddr = do
  let shdrs =
        case Elf.headerNamedShdrs hdrInfo of
          Left _ -> V.empty
          Right r -> r
  let shdrNameMap :: Map BS.ByteString [Word16]
      shdrNameMap =
        Map.fromListWith (++)
          [ (Elf.shdrName s, [fromIntegral (idx-1)])
          | idx <- [1..V.length shdrs]
          , let s = shdrs V.! (idx-1)
          ]
  symAddrMap0 <- do
    let nm = "static symbol table"
    withSymtab hdrInfo shdrNameMap nm ".symtab" symAddrMapEmpty $ \symtab strtab -> do
      flip execStateT symAddrMapEmpty $
        addDefinedSymbolTableFuns hdrInfo mem baseAddr symtab strtab
  let nm = "dynamic symbol table"
  withSymtab hdrInfo shdrNameMap nm ".dynsym" symAddrMap0 $ \dynSymtab dynStrtab -> do
    flip execStateT symAddrMap0 $
      addDefinedSymbolTableFuns hdrInfo mem baseAddr dynSymtab dynStrtab

-- | Creates InitDiscovery state containing all information needed
-- to perform function discovery.
initExecDiscovery :: forall arch r
                  .  MemAddr (ArchAddrWidth arch) -- ^ Base address for loading
                  -> Elf.ElfHeaderInfo (ArchAddrWidth arch)
                  -> ArchitectureInfo arch
                  -> ProcessPLTEntries (ArchAddrWidth arch)
                  -> ReoptOptions
                  -> InitDiscM (Either String r) (InitDiscovery arch)
initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts = elfInstances hdrInfo $ do
  -- Create memory image for elf file.
  (mem, _secMap, warnings) <-
    case memoryForElfSegments' (addrBase baseAddr) (toInteger (addrOffset baseAddr)) hdrInfo of
      Left errMsg -> fatalError errMsg
      Right r -> pure r
  mapM_ (initWarning . show) warnings

  symAddrMap0 <- discoverSymbolNames hdrInfo mem baseAddr

  mpltRes <- pltFn hdrInfo

  -- Create symbol address map that includes plt information if available.
  symAddrMap <-
    case mpltRes of
      Just pltRes -> do
        let vmap = pltVirtMap pltRes
        let hdr = Elf.header hdrInfo
        let regIdx = addrBase baseAddr
        strtab <-
          case Elf.lookupVirtAddrContents (pltStrtabOff pltRes) vmap of
            Nothing -> fatalError "Dynamic string table invalid."
            Just s -> do
              when (BS.length s < fromIntegral (pltStrtabSize pltRes)) $ do
                fatalError "Dynamic string table range invalid."
              pure $ BS.take (fromIntegral (pltStrtabSize pltRes)) s
        symtab <-
          case Elf.lookupVirtAddrContents (pltSymtabOff pltRes) vmap of
            Nothing -> fatalError "Dynamic symbol table invalid."
            Just s -> pure s
        addPltSyms hdr strtab symtab mem regIdx symAddrMap0 pltRes
      Nothing -> do
        pure symAddrMap0

  -- Exclude PLT bounds
  let explorePred a =
        case mpltRes of
          Nothing -> True
          Just pltRes -> do
            let off = fromIntegral $ addrOffset (segoffAddr a)
            case Map.lookupLE off (pltMap pltRes) of
              Just (entryOff, (_pltFn, entrySize))
                | off - entryOff < entrySize ->
                  False
              _ -> True

  -- Get initial entry address
  let hdr = Elf.header hdrInfo
  let entry = Elf.headerEntry hdr
  -- Mark entry as address as function
  let entryAddr = asSegmentOff mem (incAddr (toInteger entry) baseAddr)

  when (isNothing entryAddr) $ do
    initWarning $ "Could not resolve entry point: " ++ showHex entry ""
  let regInfo :: RegionInfo
      regInfo = HasDefaultRegion (addrBase baseAddr)
  s <-
    case runExcept (initDiscState mem entryAddr regInfo symAddrMap explorePred ainfo reoptOpts) of
      Left e -> fatalError e
      Right r -> pure r
  -- Return discovery
  pure $! InitDiscovery { initDiscSymAddrMap = symAddrMap
                        , initDiscBaseCodeAddr = baseAddr
                        , initDiscoveryState   = s
                        }

-- | Creates InitDiscovery state containing all information needed
-- to perform function discovery.
initDiscovery :: forall arch
              . LoadOptions
              -- ^ Option to load the binary at the given address
              -> Elf.ElfHeaderInfo (ArchAddrWidth arch)
              -> ArchitectureInfo arch
              -> ProcessPLTEntries (ArchAddrWidth arch)
              -> ReoptOptions
              -> InitDiscoveryComp (InitDiscovery arch)
initDiscovery loadOpts hdrInfo ainfo pltFn reoptOpts = runIncCompM $ fmap Right $ elfInstances hdrInfo $ do
  let hdr = Elf.header hdrInfo
  case Elf.headerType hdr of
    -- This is for object files.
    Elf.ET_REL -> do
      -- Note. This code requires code is in a single text section.  It does
      -- not support objects with -ffunction-sections
      case loadOffset loadOpts of
        Nothing -> pure ()
        Just _ -> initWarning $ "Ignoring load offset for object file as there is no global base address."
      -- Load elf sections
      (mem, secMap, warnings) <- do
        -- Do loading
        case memoryForElfSections hdrInfo of
          Left errMsg -> fatalError errMsg
          Right r -> pure r
      mapM_ (initWarning . show) warnings

      -- Get static symbol table
      symAddrMap <-
        case Elf.decodeHeaderSymtab hdrInfo of
          Nothing -> pure symAddrMapEmpty
          Just (Left e) -> do
            initWarning (show e)
            pure symAddrMapEmpty
          Just (Right tbl) -> do
            let staticEntries = zip [0..] (V.toList (Elf.symtabEntries tbl))
            let (sm, errs) = runState (foldlM (resolveObjSymbol hdrInfo mem secMap) symAddrMapEmpty staticEntries) []
            forM_ errs $ \e -> do
              initWarning (show e)
            pure sm

      -- Get index of text section section.
      textSectionIndex <-
        case Elf.headerNamedShdrs hdrInfo of
          Left (secIdx, _) -> do
            fatalError $ printf "Could not resolve name of section %s." (show secIdx)
          Right shdrs -> do
            let isText s = Elf.shdrName s == ".text"
            case V.findIndex isText shdrs of
              Nothing -> do
                fatalError "Could not find .text section."
              Just secIdx -> do
                when (isJust (V.findIndex isText (V.drop (secIdx+1) shdrs))) $ do
                  fatalError "Duplicate .text sections found."
                pure $ (fromIntegral secIdx :: Word16)
      -- Get offset for text section.
      textBaseAddr <-
        case Map.lookup textSectionIndex secMap of
          Nothing -> do
            fatalError "Text section is not loaded."
          Just a ->
            pure a
      let entryAddr = Nothing
       -- Get region of text segment
      let regIdx = segmentBase (segoffSegment textBaseAddr)
      let regInfo :: RegionInfo
          regInfo = HasDefaultRegion regIdx
      let explorePred = \_ -> True
      s <- case runExcept (initDiscState mem entryAddr regInfo symAddrMap explorePred ainfo reoptOpts) of
            Left e -> fatalError e
            Right r -> pure r
      -- Get initial entries and predicate for exploring
      pure $! InitDiscovery { initDiscSymAddrMap = symAddrMap
                            , initDiscBaseCodeAddr = MemAddr regIdx 0
                            , initDiscoveryState   = s
                            }
    -- Executable
    Elf.ET_EXEC -> do
      -- Get base address to use for computing section offsets.
      let baseAddr :: MemAddr (ArchAddrWidth arch)
          baseAddr = MemAddr { addrBase = 0, addrOffset = fromInteger (loadRegionBaseOffset loadOpts) }
      initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts
    -- Shared library or position-independent executable.
    Elf.ET_DYN -> do
      -- Get base address to use for computing section offsets.
      let baseAddr :: MemAddr (ArchAddrWidth arch)
          baseAddr =
            case loadOffset loadOpts of
              Just o  -> MemAddr { addrBase = 0, addrOffset = fromIntegral o }
              Nothing -> MemAddr { addrBase = 1, addrOffset = 0 }
      initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts
    Elf.ET_CORE -> do
      fatalError "Core files unsupported."
    tp -> do
      fatalError $ "Elf files with type " ++ show tp ++ " unsupported."

$(pure [])

---------------------------------------------------------------------------------
-- ReoptLogEvent

discEventToGetFnsEvent :: AddrSymMap (ArchAddrWidth arch)
                       -> DiscoveryEvent (ArchAddrWidth arch)
                       -> ReoptLogEvent arch
discEventToGetFnsEvent symMap e =
  case e of
    ReportAnalyzeFunction a -> do
      ReoptStepStarted (Discovery (segoffWord64 a) (Map.lookup a symMap))
    ReportAnalyzeFunctionDone a -> do
      ReoptStepFinished (Discovery (segoffWord64 a) (Map.lookup a symMap)) ()
    ReportIdentifyFunction a tgt rsn -> do
      ReoptLogEvent (Discovery (segoffWord64 a) (Map.lookup a symMap)) ReoptInfo
      $ ReoptLogMessage
      $ printf "Candidate function %s %s." (ppFnEntry (Map.lookup tgt symMap) tgt) (ppFunReason rsn)
    ReportAnalyzeBlock fa b ->
      ReoptLogEvent (Discovery (segoffWord64 fa) (Map.lookup fa symMap)) ReoptInfo
      $ ReoptLogMessage
      $ printf "Discovered block %s." (ppSegOff b)

$(pure [])

---------------------------------------------------------------------------------
-- Logging


stepStarted :: ReoptStep arch a -> IncCompM (ReoptLogEvent arch) r ()
stepStarted s = incCompLog (ReoptStepStarted s)

logInitEntryPointCount :: Int -> IncCompM (ReoptLogEvent arch) r ()
logInitEntryPointCount cnt =
  let n = if cnt >= 0 then fromIntegral cnt else 0
  in incCompLog (ReoptLogEvent DiscoveryInitialization ReoptInfo (ReoptLogInitEntryPointCount n))

logEvent :: ReoptStep arch a -> ReoptEventSeverity -> String -> IncCompM (ReoptLogEvent arch) r ()
logEvent s t m = incCompLog (ReoptLogEvent s t (ReoptLogMessage m))

stepFinished :: ReoptStep arch a -> a -> IncCompM (ReoptLogEvent arch) r ()
stepFinished s x = incCompLog (ReoptStepFinished s x)

stepFailed :: ReoptStep arch a -> ReoptErrorTag -> String -> IncCompM (ReoptLogEvent arch) r ()
stepFailed s tag m = incCompLog (ReoptStepFailed s tag m)

---------------------------------------------------------------------------------
-- Initial type inference


headerTypeMap :: forall arch r
              .  MemWidth (ArchAddrWidth arch)
              => AnnDeclarations
                -- ^ Header with hints for assisting typing.
              -> Memory (ArchAddrWidth arch)
              -> MemAddr (ArchAddrWidth arch)
              -> SymAddrMap (ArchAddrWidth arch)
              -> Map (ArchSegmentOff arch) NoReturnFunStatus
              -> IncCompM (ReoptLogEvent arch) r (FunTypeMaps (ArchAddrWidth arch))
headerTypeMap hdrAnn mem baseAddr symAddrMap noretMap = do
  stepStarted HeaderTypeInference
  let voidPtrType = PtrAnnType VoidAnnType
  let charPtrType = PtrAnnType (IAnnType 8)
  let sizetType = IAnnType 64
  let offType = sizetType
  let intType = IAnnType 32
  let ftype res args = ReoptNonvarargFunType (AnnFunType { funRet = res, funArgs = V.fromList args })
  let declFn = Map.singleton
  let nmArg nm tp = AnnFunArg (Just nm) tp
  let nonmArg tp = AnnFunArg Nothing tp
  -- Generate type information from annotations
  let nameAnnTypeMap
        = fmap ReoptNonvarargFunType (funDecls hdrAnn)
        <> declFn "__errno_location" (ftype (PtrAnnType (IAnnType 32)) [])
        <> declFn "__fstat" (ftype intType (fmap nonmArg [intType, voidPtrType]))
        <> declFn "__printf_chk"     (ftype intType [nmArg "flag" intType, AnnFunArg (Just "format") charPtrType])
        <> declFn "__stack_chk_fail" (ftype VoidAnnType [])
        <> declFn "exit"     (ftype VoidAnnType [AnnFunArg (Just "status") (IAnnType 64)])
        <> declFn "fprintf"  (ReoptPrintfFunType 1)
        <> declFn "memcmp"   (ftype intType [nmArg "s1" voidPtrType, nmArg "s1" voidPtrType, nmArg "n" sizetType])
        <> declFn "mmap"     (ftype voidPtrType (fmap nonmArg [voidPtrType, sizetType, intType, intType, intType, offType ]))
        <> declFn "open"     ReoptOpenFunType
        <> declFn "printf"   (ReoptPrintfFunType 0)
        <> declFn "putchar"  (ftype intType [nmArg "c" intType])
        <> declFn "puts"     (ftype intType [nonmArg charPtrType])
        <> declFn "sprintf"  (ReoptPrintfFunType 1)
        <> declFn "snprintf" (ReoptPrintfFunType 2)

  -- Generate map from address names to known type.
  --
  -- This is used when we see a function jumps to a defined address.
  addrAnnTypeMap <- do
    let insSymType :: Map (ArchSegmentOff arch) ReoptFunType
                   -> (BS.ByteString, ReoptFunType)
                   -> IncCompM (ReoptLogEvent w) r (Map (ArchSegmentOff arch) ReoptFunType)
        insSymType m (sym,annTp) = do
          case symAddrMapLookup symAddrMap sym of
            Left SymAddrMapNotFound -> do
              -- Silently drop symbols without addresses as they may be undefined.
              pure m
            Left SymAddrMapAmbiguous -> do
              logEvent HeaderTypeInference ReoptWarning $
                 "Ambiguous symbol " ++ BSC.unpack sym ++ "."
              pure m
            Right addr -> do
              pure $! Map.insert addr annTp m
    foldlM insSymType Map.empty (Map.toList nameAnnTypeMap)

  let annTypeMap :: FunTypeMaps (ArchAddrWidth arch)
      annTypeMap = FunTypeMaps
        { dwarfBaseCodeAddr = baseAddr
        , dwarfAddrResolve = \_symName off -> do
            let addr = incAddr (toInteger off) baseAddr
             in asSegmentOff mem addr
        , nameToAddrMap = symAddrMap
        , nameTypeMap = nameAnnTypeMap
        , addrTypeMap = addrAnnTypeMap
        , noreturnMap = noretMap
        }
  stepFinished HeaderTypeInference ()

  pure annTypeMap

---------------------------------------------------------------------------------
-- Complete discovery

doDiscovery :: forall arch r
            .  AnnDeclarations
               -- ^ Header with hints for assisting typing.
            -> Elf.ElfHeaderInfo (ArchAddrWidth arch)
            -> ArchitectureInfo arch
            -> InitDiscovery arch
            -> DiscoveryOptions -- ^ Options controlling discovery
            -> IncCompM (ReoptLogEvent arch) r
                  ( FunTypeMaps (ArchAddrWidth arch)
                  , DiscoveryState arch
                  )
doDiscovery hdrAnn hdrInfo ainfo initState disOpt = withArchConstraints ainfo $ do
  let s = initDiscoveryState initState
  logInitEntryPointCount $ Map.size $ view unexploredFunctions s
  let mem = memory s
  let symAddrMap = initDiscSymAddrMap initState

  annTypeMap <- headerTypeMap hdrAnn mem (initDiscBaseCodeAddr initState) symAddrMap (s^.trustedFunctionEntryPoints)

  -- Resolve debug information.
  debugTypeMap <- resolveDebugFunTypes annTypeMap hdrInfo
  let postDebugState = s & trustedFunctionEntryPoints .~ noreturnMap debugTypeMap

  let symMap = getAddrSymMap symAddrMap
  discState <- liftIncComp (discEventToGetFnsEvent symMap) $ runIncCompM $
    incCompleteDiscovery postDebugState disOpt
  pure (debugTypeMap, discState)

-- | Discover code in the binary identified by the given path.
--
-- Writes to stderr.
discoverBinary :: (forall w . ReoptLogEvent w -> IO ())
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
  initState <-
    runReoptInIO logger $ do
      mr <- liftIncComp ((ReoptLogEvent DiscoveryInitialization ReoptWarning) . ReoptLogMessage) $ do
        initDiscovery loadOpts hdrInfo ainfo pltFn reoptOpts
      case mr of
        Left msg -> do
          fatalError msg
        Right r ->  do
          pure r
  runReoptInIO logger $ do
    Some . snd <$> doDiscovery hdrAnn hdrInfo ainfo initState disOpt

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
                                     ++ (Some . X86_GP <$> x86GPPArgumentRegs)
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
      addDoubleArg nm
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
               | otherwise -> Right [Some (RetBV64 F.RAX)]
    FloatAnnType     -> Left  $ UnsupportedReturnType (ppAnnType tp0)
    DoubleAnnType    -> Right [Some (RetZMM ZMMDouble 0)]
    PtrAnnType _     -> Right [Some (RetBV64 F.RAX)]
    TypedefAnnType _ tp -> parseReturnType tp


resolveAnnFunType :: Monad m
                  => AnnFunType
                  -> ExceptT ArgResolverError m X86FunTypeInfo
resolveAnnFunType funType = do
  args <- runArgResolver (argsToRegisters 0 (funArgs funType))
  ret <-
    case parseReturnType (funRet funType) of
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
        let args = fmap ArgBV64            (dropArgSuffix X86_GP     x86GPPArgumentRegs argSet)
                ++ fmap (ArgZMM ZMM512D)   (dropArgSuffix X86_ZMMReg [0..7] argSet)
            rets = fmap (Some . RetBV64)   (dropArgSuffix X86_GP     [F.RAX, F.RDX] retSet)
                ++ fmap (Some . RetZMM ZMM512D) (dropArgSuffix X86_ZMMReg [0,1]          retSet)
         in X86NonvarargFunType args rets
  in orderPadArgs <$>
       Map.mergeWithKey (\_ ds rets -> Just (registerDemands ds, rets))
                        (fmap (\ds ->  (registerDemands ds, mempty)))
                        (fmap (\s -> (mempty,s)))
                        dm
                        retDemands

$(pure [])

resolveReoptFunType :: Monad m
                    => ReoptFunType
                    -> ExceptT ArgResolverError m X86FunTypeInfo
resolveReoptFunType (ReoptNonvarargFunType ftp) =
  resolveAnnFunType ftp
resolveReoptFunType (ReoptPrintfFunType i) =
  pure $! X86PrintfFunType i
resolveReoptFunType ReoptOpenFunType =
  pure $! X86OpenFunType
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
          Map.insert newEnd (oldEnd, oldFuns) m

-- | Update funUseOffsetMap with new address
updateFunUseOffsetMap :: FunAddress w
                      -> BlockOff
                      -> BlockOff -- ^ end offset
                      -> FunUseOffsetMap w
                      -> FunUseOffsetMap w
updateFunUseOffsetMap f off newEnd old = do
  case Map.lookupLT newEnd old of
    Just (oldOff, (oldEnd, oldFuns)) | oldEnd > off ->
      case compare oldOff off of
        LT -> do
          let m = Map.insert oldOff (off, oldFuns) old
          replaceRegion off f newEnd oldFuns oldEnd m
        EQ -> replaceRegion off f newEnd oldFuns oldEnd old
        GT -> do
          let m  = replaceRegion oldOff f newEnd oldFuns oldEnd old
          updateFunUseOffsetMap f off oldOff m
    _ -> Map.insert off (newEnd, [f]) old

-- | Maps regions indices for code to offsets witin region to the size
-- of the segment and a list of functions that occupy that region.
--
-- This maintains the invariant that the list of functions are
-- non-empty, and no regions overlap.
newtype FunUseMap w = FunUseMap (Map RegionIndex (FunUseOffsetMap w))

-- | Empty use map
emptyFunUseMap :: FunUseMap w
emptyFunUseMap = FunUseMap Map.empty

-- | @recordRegionUse f o sz@ Mark a region as belonging to a particular function
recordRegionUse :: FunAddress w
                -> MemSegmentOff w
                -> BlockSize
                -> FunUseMap w
                -> FunUseMap w
recordRegionUse f so sz (FunUseMap regionMap) = do
  let a = segoffAddr so
      -- Offset of block
      off :: BlockOff
      off = memWordValue (addrOffset a)
      initMap = initFunUseOffsetMap f off (off+sz)
      updMap _ old = updateFunUseOffsetMap f off (off + sz) old
   in FunUseMap (Map.insertWith updMap (addrBase a) initMap regionMap)

-- | Record memory used by block to function address
recordBlockUse :: FunAddress (ArchAddrWidth arch)
               -> ParsedBlock arch ids
               -> State (FunUseMap (ArchAddrWidth arch)) ()
recordBlockUse f b = do
  modify $ recordRegionUse f (pblockAddr b) (fromIntegral (blockSize b))
  -- Record jumptable backing as well.
  case pblockTermStmt b of
    ParsedLookupTable layout _ _ _ -> do
      let a = jtlBackingAddr layout
          sz = jtlBackingSize layout
      modify $ recordRegionUse f a sz
    _ -> pure ()

-- | Record memory used by block to function address
recordFunUse :: DiscoveryFunInfo arch ids
             -> State (FunUseMap (ArchAddrWidth arch)) ()
recordFunUse f =
  mapM_ (recordBlockUse (discoveredFunAddr f)) (f^.parsedBlocks)

-- | Create a function use map from all functions in discover state.
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
      err = error "internal error: Function register index"
      offMap = Map.findWithDefault err (addrBase a) m
      maxValue = memWordValue (segmentSize seg) - memWordValue (segoffOffset f)
   in case Map.lookup o offMap of
        Just _ -> endOwnedByFun f o offMap maxValue - o
        Nothing -> error $ "Unknown function " ++ showHex o ""

$(pure [])

--------------------------------------------------------------------------------
-- recoverX86Elf


matchPLT :: DiscoveryFunInfo arch ids -> Maybe VersionedSymbol
matchPLT finfo
  | [b] <- Map.elems (finfo^.parsedBlocks)
  , PLTStub _ _ sym <- pblockTermStmt b =
    Just sym
matchPLT _ = Nothing

-- | Infer arguments for functions that we do not already know.
x86ArgumentAnalysis :: SyscallPersonality
                    -> Map (MemSegmentOff 64) BSC.ByteString
                       -- ^ Map from addresses to function name.
                    -> Map BSC.ByteString X86FunTypeInfo
                       -- ^ Map from address to the name at that address along with type
                    -> DiscoveryState X86_64
                    -> IncCompM (ReoptLogEvent X86_64) r (Map (MemSegmentOff 64) X86FunTypeInfo)
x86ArgumentAnalysis sysp funNameMap knownFunTypeMap discState = do
  -- Generate map from symbol names to known type.
  let mem = memory discState
      -- Compute only those functions whose types are not known.
  let known :: DiscoveryFunInfo X86_64 ids -> Bool
      known f =
        case Map.lookup (discoveredFunAddr f) funNameMap of
          Just nm -> Map.member nm knownFunTypeMap
          Nothing -> False
  let shouldPropagate (Some f) = not (known f) && isNothing (matchPLT f)

  stepStarted $ FunctionArgInference

  let (dems, summaryFails) = do
        let resolveFn :: MemSegmentOff 64
                      -> RegState X86Reg (Value X86_64 ids)
                      -> Either String [Some (Value X86_64 ids)]
            resolveFn callSite callRegs = do
              callArgValues <$> x86CallRegs mem funNameMap knownFunTypeMap callSite callRegs
        functionDemands (x86DemandInfo sysp) mem resolveFn $
          filter shouldPropagate $ exploredFunctions discState

  forM_ (Map.toList summaryFails) $ \(faddr, rsn) -> do
    case rsn of
      PLTStubNotSupported -> do
        let dnm = Map.lookup faddr funNameMap
        logEvent FunctionArgInference ReoptWarning $
          printf "%s: Unexpected PLT stub." (ppFnEntry dnm faddr)
      CallAnalysisError callSite msg -> do
        let dnm =  Map.lookup faddr funNameMap
        logEvent FunctionArgInference ReoptWarning $
          printf "%s: Could not determine signature at callsite %s:\n    %s" (ppFnEntry dnm faddr) (ppSegOff callSite) msg
  stepFinished FunctionArgInference ()
  pure $ inferFunctionTypeFromDemands dems

-- | Analyze an elf binary to extract information.
--
--  Note. This prints warnings to stderr and may exit if the Elf file cannot be parsed.
doRecoverX86 :: BSC.ByteString -- ^ Prefix to use if we need to generate new function endpoints later.
             -> SyscallPersonality
             -> SymAddrMap 64
             -> FunTypeMaps 64
             -> DiscoveryState X86_64
             -> IncCompM (ReoptLogEvent X86_64) r
                  ( RecoveredModule X86_64
                  , MergeRelations
                  )
doRecoverX86 unnamedFunPrefix sysp symAddrMap debugTypeMap discState = do
  let mem = memory discState

  -- Maps address to name of function to use.
  let funNameMap ::  Map (MemSegmentOff 64) BS.ByteString
      funNameMap =
        Map.mapWithKey (\addr qnm -> localFunctionName unnamedFunPrefix qnm addr)
                       (samAddrMap symAddrMap)
        <> Map.fromList [ (addr, nm)
                        | Some finfo <- exploredFunctions discState
                        , let addr = discoveredFunAddr finfo
                        , Map.notMember addr (samAddrMap symAddrMap)
                        , let nm = case matchPLT finfo of
                                     Just sym -> versymName sym
                                     Nothing -> nosymFunctionName unnamedFunPrefix addr
                        ]

  let resolveX86Type :: ReoptFunType -> Maybe X86FunTypeInfo
      resolveX86Type rtp =
        case runExcept (resolveReoptFunType rtp) of
          Left _ -> Nothing
          Right r -> Just r

  -- Map names to known function types when we have explcit information.
  let knownFunTypeMap ::  Map BS.ByteString X86FunTypeInfo
      knownFunTypeMap
        = Map.mapMaybe resolveX86Type (nameTypeMap debugTypeMap)
        <> Map.fromList [ (recoveredFunctionName symAddrMap unnamedFunPrefix addr, xtp)
                        | (addr,rtp) <- Map.toList (addrTypeMap debugTypeMap)
                        , Right xtp <- [runExcept (resolveReoptFunType rtp)]
                        ]

  -- Infer registers each function demands.
  fDems <- x86ArgumentAnalysis sysp funNameMap knownFunTypeMap discState

  let funTypeMap ::  Map BS.ByteString X86FunTypeInfo
      funTypeMap = knownFunTypeMap
                <> Map.fromList [ (recoveredFunctionName symAddrMap unnamedFunPrefix addr, tp)
                                | (addr,tp) <- Map.toList fDems
                                ]

  fnDefs <- fmap catMaybes $
    forM (exploredFunctions discState) $ \(Some finfo) -> do
      let faddr = discoveredFunAddr finfo
      let dnm = discoveredFunSymbol finfo
      let fnInvEvt = InvariantInference (memWordValue (addrOffset (segoffAddr faddr))) dnm
      stepStarted fnInvEvt
      case checkFunction finfo of
        FunctionOK -> do
          case x86BlockInvariants sysp mem funNameMap funTypeMap finfo of
            Left (failTag, msg) -> do
              stepFailed fnInvEvt failTag msg
              pure Nothing
            Right invMap -> do
              stepFinished fnInvEvt invMap
              -- Do function recovery
              let fnRecEvt = Recovery (memWordValue (addrOffset (segoffAddr faddr))) dnm
              stepStarted fnRecEvt
              case recoverFunction sysp funNameMap funTypeMap mem finfo invMap of
                Left (errTag, errMsg) -> do
                  stepFailed fnRecEvt errTag errMsg
                  pure Nothing
                Right (warnings, fn) -> do
                  mapM_ (logEvent fnRecEvt ReoptWarning) warnings
                  stepFinished fnRecEvt ()
                  pure (Just fn)
        FunctionIncomplete errTag -> do
          stepFailed fnInvEvt errTag "Incomplete discovery."
          pure Nothing
        -- We should have filtered out PLT entries from the explored functions,
        -- so this is considered an error.
        FunctionHasPLT -> do
          stepFailed fnInvEvt ReoptCannotRecoverFnWithPLTStubsTag "Encountered unexpected PLT stub."
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
  seq recMod $ pure (recMod, mergeRel)

type RecoverIncCompM arch r a
   = IncCompM (ReoptLogEvent arch) (Either String r) a

-- | Analyze an elf binary to extract information.
--
--  Note. This prints warnings to stderr and may exit if the Elf file cannot be parsed.
recoverX86Elf' :: LoadOptions
               -- ^ Option to load the binary at the given address
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> ReoptOptions -- ^ User
               -> AnnDeclarations
               -- ^ Header with hints for assisting typing.
               -> BSC.ByteString -- ^ Prefix to use if we need to generate new function endpoints later.
               -> Elf.ElfHeaderInfo 64
               -> RecoverIncCompM X86_64 r
                    ( X86OS
                    , DiscoveryState X86_64
                    , RecoveredModule X86_64
                    , MergeRelations
                    )
recoverX86Elf' loadOpts disOpt reoptOpts hdrAnn unnamedFunPrefix hdrInfo = do
  let hdr = Elf.header hdrInfo
  case Elf.headerMachine hdr of
    Elf.EM_X86_64 ->
      pure ()
    m ->
      fatalError $ printf "Recovery does not support %s binaries." (show m)

  let pltFn = processX86PLTEntries

  stepStarted DiscoveryInitialization
  os <-
    case x86OSForABI (Elf.headerOSABI hdr) of
      Just os -> pure os
      Nothing -> do
        logEvent DiscoveryInitialization ReoptWarning (warnABIUntested (Elf.headerOSABI hdr))
        pure Linux
  let ainfo = osArchitectureInfo os
  mr <- liftIncComp ((ReoptLogEvent DiscoveryInitialization ReoptWarning) . ReoptLogMessage) $ do
    initDiscovery loadOpts hdrInfo ainfo pltFn reoptOpts

  initState <-
    case mr of
    Left msg -> do
      fatalError msg
    Right r ->  do
      pure r

  -- Check function names
  let symAddrMap = initDiscSymAddrMap initState
  when (isUsedPrefix unnamedFunPrefix symAddrMap) $ do
    fatalError $
        printf "No symbol in the binary may start with the prefix %d."
            (BSC.unpack unnamedFunPrefix)
  stepFinished DiscoveryInitialization ()

  (debugTypeMap, discState) <-
    doDiscovery hdrAnn hdrInfo ainfo initState disOpt

  let sysp = osPersonality os
  (recMod, mergeRel) <-
    doRecoverX86 unnamedFunPrefix sysp symAddrMap debugTypeMap discState

  pure (os, discState, recMod, mergeRel)

-- | Analyze an elf binary to extract information.
--
--  Note. This prints warnings to stderr and may exit if the Elf file cannot be parsed.
recoverX86Elf :: (ReoptLogEvent X86_64 -> IO ())
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
  runReoptInIO logger $ do
    (os, discState, recMod, mergeRel) <-
      recoverX86Elf' loadOpts disOpt reoptOpts hdrAnn unnamedFunPrefix hdrInfo
    pure (hdrInfo, os, discState, recMod, mergeRel)

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
            -> Either Builder.Builder BS.ByteString -- ^ Representiation of LLVM to serialize
            -> IO BS.ByteString
compileLLVM optLevel optPath llcPath llvmMcPath arch llvm = do
  -- Run llvm on resulting binary
  mres <- runExceptT $ do
    -- Skip optimization if optLevel == 0
    llvm_opt <-
      case llvm of
        Left llvmBldr ->
          if optLevel /= 0 then do
            Ext.run_opt optPath optLevel $ \inHandle -> do
              Builder.hPutBuilder inHandle llvmBldr
          else
            pure $ BSL.toStrict $ Builder.toLazyByteString llvmBldr
        Right llvmBS -> pure llvmBS
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
  let (warnings, mergeRes) = mergeObject origBinary newObjHdrInfo mergeRel
  forM_ warnings $ \w -> do
    hPutStrLn stderr $ "Relinker warning: " ++ w
  newBinary <-
    case mergeRes of
      Left msg -> do
        hPutStrLn stderr msg
        exitFailure
      Right r -> do
        pure r
  BSL.writeFile outputPath newBinary
  -- Update the file mode
  do fs <- getFileStatus outputPath
     let fm = ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode
     setFileMode outputPath (fileMode fs `unionFileModes` fm)

--------------------------------------------------------------------------------
-- Function Recovery and Statistics

joinLogEvents :: Monad m => (a -> m ()) -> (a -> m ()) -> a -> m ()
joinLogEvents x y e = x e >> y e

-- | This parses function argument information from a user-provided header file.
resolveHeader :: Maybe FilePath -- ^ Filepath for C header with program info (if any).
              -> FilePath -- ^ Path to clang.
              -> IO AnnDeclarations
resolveHeader mHdrPath clangPath =
  case mHdrPath of
    Nothing -> pure emptyAnnDeclarations
    Just p -> parseHeader clangPath p

-- | Function for recovering log information.
--
-- This has a side effect where it increments an IORef so
-- that the number of errors can be recorded.
printLogEvent
  :: ReoptLogEvent arch  -- ^ Message to log
  -> IO ()
printLogEvent event = do
  -- Print log info of important events to stderr.
  case event of
    _ -> do
      hPutStrLn stderr (show event)

-- | Function for recovering log information.
--
-- This has a side effect where it increments an IORef so
-- that the number of errors can be recorded.
recoverLogEvent
  :: IORef ReoptStats
  -> ReoptLogEvent arch  -- ^ Message to log
  -> IO ()
recoverLogEvent statsRef event = do
  -- Update error count when applicable
  when (isErrorEvent event) $ do
    modifyIORef' statsRef $ \s -> s {statsErrorCount = 1 + (statsErrorCount s)}
  -- Record more detailed info when appropriate.
  case event of
    ReoptLogEvent _step _severity content ->
      case content of
        ReoptLogInitEntryPointCount cnt -> do
          let update s = s { statsInitEntryPointCount = cnt }
          modifyIORef' statsRef update
        ReoptLogMessage _ -> pure ()
    ReoptStepStarted step ->
      case step of
        Discovery _ _ -> do
          let update s = s { statsFnDiscoveredCount = 1 + (statsFnDiscoveredCount s) }
          modifyIORef' statsRef update
        _ -> pure ()
    ReoptStepFinished step _ ->
      case step of
        Discovery addr mNm -> do
          let update s = s { statsFnResults = Map.insert (mNm, addr) FnDiscovered (statsFnResults s)
                           }
          modifyIORef' statsRef update
        Recovery addr mNm -> do
          let update s = s { statsFnResults = Map.insert (mNm, addr) FnRecovered (statsFnResults s)
                           , statsFnRecoveredCount = 1 + (statsFnRecoveredCount s) }
          modifyIORef' statsRef update
        _ -> pure ()
    ReoptStepFailed step errTag _errMsg -> do
      case stepFailResult step of
          Nothing -> pure ()
          Just (result, addr, mNm) -> do
            let update s = s { statsFnResults = incFnResult mNm addr result $ statsFnResults s }
            modifyIORef' statsRef update
      let update s = s { statsStepErrors = incStepError (reoptStepTag step) errTag $ statsStepErrors s }
      modifyIORef' statsRef update

getFileSize :: String -> IO Natural
getFileSize path = do
    stat <- getFileStatus path
    return $ fromIntegral (fileSize stat)

-- | Parse arguments to get information needed for function representation.
recoverFunctions
  :: FilePath -- ^ Path to program
  -> FilePath -- ^ Path to clang
  -> LoadOptions
  -> DiscoveryOptions
  -> ReoptOptions
  -> Maybe FilePath -- ^ Filepath for C header with program info (if any).
  -> BS.ByteString -- ^ Prefix for unnamed functions identified in code discovery.
  -> IO (X86OS, RecoveredModule X86_64, ReoptStats)
recoverFunctions progPath clangPath lOpts dOpts rOpts mCHdr unnamedFnPrefix = do
  hdrAnn <- resolveHeader mCHdr clangPath
  progSize <- getFileSize progPath
  statsRef <- newIORef $ initReoptStats progPath progSize
  (_, os, _, recMod, _) <-
    recoverX86Elf (joinLogEvents printLogEvent (recoverLogEvent statsRef))
                  progPath
                  lOpts
                  dOpts
                  rOpts
                  hdrAnn
                  unnamedFnPrefix
  stats <- readIORef statsRef
  pure (os, recMod, stats)


defaultLLVMGenOptions :: LLVMGenOptions
defaultLLVMGenOptions = LLVMGenOptions { mcExceptionIsUB = False }

-- | Rendered a recovered X86_64 module as LLVM bitcode
renderLLVMBitcode
  :: LLVMGenOptions
  -> LLVMConfig
  -> X86OS -- ^ Operating system
  -> RecoveredModule X86_64 -- ^ Recovered module
  -> (Builder.Builder, [Either String Ann.FunctionAnn])
renderLLVMBitcode llvmGenOpt llvmCfg os recMod =
  let archOps = LLVM.x86LLVMArchOps (show os)
   in llvmAssembly archOps llvmGenOpt recMod llvmCfg
