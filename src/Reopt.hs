{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Reopt
  ( -- * ReoptM Monad
    ReoptM,
    runReoptM,
    reoptRunInit,
    reoptWrite,
    reoptWriteBuilder,
    reoptWriteByteString,
    reoptWriteStrictByteString,
    reoptInTempDirectory,
    reoptIO,
    reoptEndNow,
    reoptFatalError,
    globalStepStarted,
    globalStepWarning,
    globalStepFinished,
    Reopt.Events.ReoptFatalError(..),
    Reopt.Events.ReoptFileType(..),
    -- Initiization
    Reopt.Relinker.parseElfHeaderInfo64,
    isCodeSection,
    printX86SectionDisassembly,
    getElfArchInfo,
    doInit,
    reoptX86Init,
    checkSymbolUnused,
    SomeArchitectureInfo (..),
    -- * Code discovery
    ReoptOptions (..),
    defaultReoptOptions,
    InitDiscovery,
    initDiscSymAddrMap,
    doDiscovery,
    -- * Debug info discovery
    debugInfoDir,
    debugInfoFileCache,
    discoverFunDebugInfo,
    -- * Function recovery
    Reopt.TypeInference.HeaderTypes.AnnDeclarations,
    Reopt.TypeInference.HeaderTypes.emptyAnnDeclarations,
    RecoveredModule,
    recoveredDefs,
    resolveHeader,
    -- * LLVM
    LLVMVersion,
    versionOfString,
    renderLLVMBitcode,
    Reopt.CFG.LLVM.LLVMGenOptions (..),
    defaultLLVMGenOptions,
    LLVMConfig,
    latestLLVMConfig,
    getLLVMConfig,
    optimizeLLVM,
    compileLLVM,
    -- Reopt.Relinker.MergeRelations,
    Reopt.Relinker.mergeObject,
    -- * X86 specific
    X86OS (..),
    osPersonality,
    osLinkName,
    doRecoverX86,
    recoverX86Elf,
    x86OSForABI,
    osArchitectureInfo,
    processX86PLTEntries,

    -- * Utility
    copyrightNotice,
    writeExecutable,
    warnABIUntested,

    -- * Re-exports
    Data.Macaw.Memory.ElfLoader.LoadOptions (..),
    Data.Macaw.Memory.ElfLoader.defaultLoadOptions,
    Data.Macaw.CFG.MemSegmentOff,
    Data.Macaw.CFG.segoffAddr,
    Data.Macaw.CFG.addrBase,
    Data.Macaw.CFG.addrOffset,
    Data.Macaw.CFG.memWordValue,
    {-
        Data.Macaw.CFG.MemWidth,
        Data.Macaw.CFG.IsArchStmt,
        Reopt.CFG.FnRep.FnArchStmt,
        -}
    Data.Macaw.X86.X86_64,
  )
where

import Control.Exception (assert, bracket, try)
import Control.Lens ( (.~), (&), (^.) )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Dwarf as Dwarf
import qualified Data.ElfEdit as Elf
import Data.Foldable (foldl', foldlM)
import Data.Macaw.Analysis.FunctionArgs
import Data.Macaw.Analysis.RegisterUse (callArgValues, ppRegisterUseErrorReason)
import Data.Macaw.Architecture.Info (ArchitectureInfo (..))
import Data.Macaw.CFG
import Data.Macaw.Discovery
import Data.Macaw.Memory.ElfLoader
import Data.Macaw.Utils.IncComp
import Data.Macaw.X86 (X86TermStmt (..), X86_64)
import qualified Data.Macaw.X86 as X86
import Data.Macaw.X86.SyscallInfo
import Data.Macaw.X86.X86Reg
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Parameterized.Some
import Data.Parameterized.TraversableF
import qualified Data.Set as Set
import Data.String
import qualified Data.Vector as V
import Data.Word
import qualified Flexdis86 as F
import Numeric
import Reopt.ArgResolver
import Reopt.CFG.FnRep
import Reopt.CFG.FunctionCheck
import Reopt.CFG.LLVM (LLVMGenOptions (..), moduleForFunctions)
import Reopt.CFG.LLVM.X86 as LLVM ( x86LLVMArchOps )
import Reopt.CFG.Recovery
import Reopt.Events
import qualified Reopt.ExternalTools as Ext
import Reopt.FunUseMap
import Reopt.Hints
import Reopt.PltParser
import Reopt.Relinker
  ( MergeRelations (..),
    ObjFunDef (..),
    ObjFunRef (..),
    mergeObject,
    parseElfHeaderInfo64,
  )
import Reopt.TypeInference.DebugTypes
import Reopt.TypeInference.FunTypeMaps
import Reopt.TypeInference.Header
import Reopt.TypeInference.HeaderTypes
import qualified Reopt.VCG.Annotations as Ann
import System.Directory (withCurrentDirectory, getXdgDirectory, XdgDirectory(XdgData), doesFileExist)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, IOMode (..), hPutStrLn, withBinaryFile, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix as Posix
    ( getEnv,
      groupExecuteMode,
      groupReadMode,
      groupWriteMode,
      otherExecuteMode,
      otherReadMode,
      otherWriteMode,
      ownerExecuteMode,
      ownerReadMode,
      ownerWriteMode,
      createFile,
      closeFd,
      fdToHandle )
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import qualified Text.PrettyPrint.HughesPJ as HPJ
import Text.Printf (printf)

#ifdef SUPPORT_ARM
import qualified Data.VEX.FFI
import           Data.Macaw.VEX.AArch32 (armArch32le)
import           Data.Macaw.VEX.AArch64 (armArch64le)
#endif

copyrightNotice :: String
copyrightNotice = "Copyright 2014-21 Galois, Inc."

--usageMessage :: String
--usageMessage = "For help on using reopt, run \"reopt --help\"."

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
resolveSymAddr ::
  -- | Loaded memory object.
  Memory w ->
  -- | Region index for resolving addresses or why it is missing.
  RegionInfo ->
  -- | Map from symbol names in binary to associated addresses.
  SymAddrMap w ->
  -- | The name of a symbol as a string.
  String ->
  Except String (MemSegmentOff w)
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
data ReoptOptions = ReoptOptions
  { -- | Symbols/addresses user wanted included
    roIncluded :: [String],
    -- | Symbols/addresses user wanted exluded.
    roExcluded :: [String]
  }

-- | Reopt options with no additional functions to explore or not explore.
defaultReoptOptions :: ReoptOptions
defaultReoptOptions = ReoptOptions {roIncluded = [], roExcluded = []}

addKnownFn ::
  SymAddrMap w ->
  BS.ByteString ->
  NoReturnFunStatus ->
  Map (MemSegmentOff w) NoReturnFunStatus ->
  Map (MemSegmentOff w) NoReturnFunStatus
addKnownFn sam nm noRet m0 =
  let s = Map.findWithDefault Set.empty nm (samNameMap sam)
   in foldl (\m a -> Map.insert a noRet m) m0 s

--------------------------------------------------------------------------------
-- InitDiscovery

-- | Information returned by `initDiscovery` below.
data InitDiscovery arch = InitDiscovery
  { -- | Map from symbols to addresses.
    initDiscSymAddrMap :: !(SymAddrMap (ArchAddrWidth arch)),
    -- | Address to use as base address for program counters in
    -- Dwarf debug information.
    initDiscBaseCodeAddr :: !(MemAddr (ArchAddrWidth arch)),
    initDiscoveryState :: !(DiscoveryState arch)
  }

$(pure [])

------------------------------------------------------------------------
-- InitDiscoveryComp

-- | Computation that logs string messages.
type InitDiscM = IncCompM String

-- | Warning in the initialization code.
initWarning :: String -> InitDiscM r ()
initWarning = incCompLog

-- | Report a fatal error in an incremental computation that may fail.
initError :: String -> InitDiscM (Either String r) a
initError msg = incCompDone $ Left msg

newtype ReoptM arch r a
  = ReoptM (ReaderT (ReoptLogEvent arch -> IO ()) (ContT (Either ReoptFatalError r) IO) a)
  deriving (Functor, Applicative, Monad)

runReoptM ::
  (ReoptLogEvent arch -> IO ()) ->
  ReoptM arch r r ->
  IO (Either ReoptFatalError r)
runReoptM logger (ReoptM m) = runContT (runReaderT m logger) (pure . Right)

-- | Run an IO comput
reoptIO :: IO a -> ReoptM arch r a
reoptIO = ReoptM . liftIO

-- | Run the computation inside a system temporary directory.
reoptInTempDirectory :: ReoptM arch a a -> ReoptM arch r a
reoptInTempDirectory (ReoptM m) =
  ReoptM $ ReaderT $ \logger -> ContT $ \c -> do
    -- Run computation inside directory
    ma <-
      withSystemTempDirectory "reopt" $ \tempDir -> do
        withCurrentDirectory tempDir $ do
          runContT (runReaderT m logger) (pure . Right)
    case ma of
      Left e -> pure $! Left e
      Right a -> c a

-- | End the reopt computation with the given return value.
reoptEndNow :: r -> ReoptM arch r a
--reoptEndNow r = ReoptM $ ReaderT $ \_ -> ContT $ \_ -> pure (Right r)
reoptEndNow = ReoptM . ReaderT . \r _ -> ContT (\_ -> pure (Right r))

reoptFatalError :: ReoptFatalError -> ReoptM arch r a
reoptFatalError e = ReoptM $ ReaderT $ \_ -> ContT $ \_ -> pure (Left e)

reoptLog :: ReoptLogEvent arch -> ReoptM arch r ()
reoptLog e = ReoptM $ ReaderT $ \logger -> lift (logger e)

--reoptEnd :: ReoptM arch r ()
--reoptEnd = reopt

reoptWrite :: ReoptFileType -> FilePath -> (Handle -> IO ()) -> ReoptM arch r ()
reoptWrite tp path f =
  ReoptM $ ReaderT $ \_ -> ContT $ \c -> do
    mr <- try $ withBinaryFile path WriteMode f
    case mr of
      Left e ->  pure $ Left $ ReoptWriteError tp path e
      Right () -> c ()

reoptWriteBuilder :: ReoptFileType -> FilePath -> Builder.Builder -> ReoptM arch r ()
reoptWriteBuilder tp path buffer = reoptWrite tp path $ \h -> Builder.hPutBuilder h buffer

reoptWriteByteString :: ReoptFileType -> FilePath -> BSL.ByteString -> ReoptM arch r ()
reoptWriteByteString tp path buffer = do
  reoptWrite tp path $ \h -> BSL.hPut h buffer

reoptWriteStrictByteString :: ReoptFileType -> FilePath -> BS.ByteString -> ReoptM arch r ()
reoptWriteStrictByteString tp path buffer = do
  reoptWrite tp path $ \h -> BS.hPut h buffer

reoptIncComp ::
  IncCompM (ReoptLogEvent arch) a a ->
  ReoptM arch r a
reoptIncComp m =
  ReoptM $
    ReaderT $ \logger ->
      liftIO $ processIncCompLogs logger $ runIncCompM m

reoptRunInit ::
  InitDiscM (Either String a) a ->
  ReoptM arch r a
reoptRunInit m =
  ReoptM $
    ReaderT $ \logger ->
      ContT $ \c -> do
        mr <-
          processIncCompLogs
            (logger . ReoptGlobalStepWarning DiscoveryInitialization)
            (runIncCompM (Right <$> m))
        case mr of
          Left e -> pure (Left (ReoptInitError e))
          Right v -> c v

checkBlockError :: ParsedBlock arch ids -> Maybe DiscoveryError
checkBlockError b = do
  let a = memWordValue $ addrOffset $ segoffAddr $ pblockAddr b
  case pblockTermStmt b of
    PLTStub{} ->
      Just $! DiscoveryError {
          discErrorTag = DiscoveryPLTErrorTag,
          discErrorBlockAddr = a,
          discErrorBlockInsnIndex = length (pblockStmts b),
          discErrorMessage = "Unexpected PLT stub outside PLT"
        }
    ParsedTranslateError msg ->
      Just $! DiscoveryError {
          discErrorTag = DiscoveryTransErrorTag,
          discErrorBlockAddr = a,
          discErrorBlockInsnIndex = length (pblockStmts b),
          discErrorMessage = msg
        }
    ClassifyFailure _ _ ->
      Just $! DiscoveryError {
          discErrorTag = DiscoveryClassErrorTag,
          discErrorBlockAddr = a,
          discErrorBlockInsnIndex = length (pblockStmts b),
          discErrorMessage = "Unclassified control flow transfer."
        }
    _ -> Nothing

-- | Prepend discovery event to list of reopt log evnts.
logDiscEventAsReoptEvents ::
  (ReoptLogEvent arch -> IO ()) ->
  AddrSymMap (ArchAddrWidth arch) ->
  DiscoveryEvent arch ->
  IO ()
logDiscEventAsReoptEvents logger symMap evt = do
  let mkFunId a = funId a (Map.lookup a symMap)
  case evt of
    ReportAnalyzeFunction a ->
      logger $ ReoptFunStepStarted Discovery (mkFunId a)
    ReportAnalyzeFunctionDone f -> do
      let a = discoveredFunAddr f
      let fId = mkFunId a
      case mapMaybe checkBlockError (Map.elems (f ^. parsedBlocks)) of
        [] ->
          logger $ ReoptFunStepFinished Discovery fId ()
        errs ->
          logger $ ReoptFunStepFailed Discovery fId errs
    ReportIdentifyFunction a tgt rsn -> do
      let msg =
            printf
              "Candidate function %s %s."
              (ppFnEntry (Map.lookup tgt symMap) tgt)
              (ppFunReason rsn)
      logger $ ReoptFunStepLog Discovery (mkFunId a) msg
    ReportAnalyzeBlock fa b -> do
      let msg = printf "Discovered block %s." (ppSegOff b)
      logger $ ReoptFunStepLog Discovery (mkFunId fa) msg

reoptRunDiscovery ::
  AddrSymMap (ArchAddrWidth arch) ->
  IncCompM (DiscoveryEvent arch) a a ->
  ReoptM arch r a
reoptRunDiscovery symMap m = ReoptM $
  ReaderT $ \logger -> ContT $ \c -> do
    processIncCompLogs (logDiscEventAsReoptEvents logger symMap) (runIncCompM m) >>= c

$(pure [])

------------------------------------------------------------------------
-- Utilities

-- | Run a computation with the given section, and print a warning if
-- the section is not found or multiply defined.
withShdr ::
  -- | Map from section names to sections with that name.
  Map BS.ByteString [s] ->
  -- | Name of section for warning messages.
  String ->
  -- | Name of section in Elf file.
  BS.ByteString ->
  -- | Flag that should be true if expected
  Bool ->
  -- | Value if nothing defined.
  a ->
  (s -> InitDiscM r a) ->
  InitDiscM r a
withShdr sectionNameMap warnName secName expected d f =
  case Map.findWithDefault [] secName sectionNameMap of
    [] -> do
      when expected $ do
        initWarning $ "Could not find " ++ warnName ++ " sections."
      pure d
    _ : _ : _ -> do
      initWarning $ "Found multiple " ++ warnName ++ " sections."
      pure d
    [s] -> f s

------------------------------------------------------------------------
-- PLT functions

-- | Add Plt symbols
addPltSyms ::
  forall w r.
  (Num (MemWord w), Integral (Elf.ElfWordType w)) =>
  Elf.ElfHeader w ->
  -- | String table contents
  BS.ByteString ->
  -- | Symbol table contents
  BS.ByteString ->
  Memory w ->
  RegionIndex ->
  SymAddrMap w ->
  PltInfo w ->
  InitDiscM r (SymAddrMap w)
addPltSyms hdr strtab symtab mem regIdx m0 p = do
  let cl = Elf.headerClass hdr
  let dta = Elf.headerData hdr
  let ins ::
        Elf.ElfWordType w ->
        (ResolvedPltEntry, Elf.ElfWordType w) ->
        (SymAddrMap w -> InitDiscM r (SymAddrMap w)) ->
        (SymAddrMap w -> InitDiscM r (SymAddrMap w))
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

shdrContents ::
  Integral (Elf.ElfWordType w) =>
  Elf.ElfHeaderInfo w ->
  Elf.Shdr nm (Elf.ElfWordType w) ->
  BS.ByteString
shdrContents hdrInfo shdr =
  let fileOff = Elf.shdrOff shdr
      size = Elf.shdrSize shdr
   in BS.take (fromIntegral size) $
        BS.drop (fromIntegral fileOff) $
          Elf.headerFileContents hdrInfo

processX86PLTEntries ::
  Elf.ElfHeaderInfo 64 ->
  V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType 64)) ->
  InitDiscM r (Maybe (PltInfo 64))
processX86PLTEntries hdrInfo shdrs = do
  case extractPLTEntries hdrInfo shdrs of
    Left err -> do
      incCompLog err
      pure Nothing
    Right m ->
      pure m

$(pure [])

------------------------------------------------------------------------
--X86 calling convention

data X86OS = Linux | FreeBSD

instance Show X86OS where
  show Linux = "Linux"
  show FreeBSD = "FreeBSD"

osPersonality :: X86OS -> SyscallPersonality
osPersonality Linux = X86.linux_syscallPersonality
osPersonality FreeBSD = X86.freeBSD_syscallPersonality

osArchitectureInfo :: X86OS -> ArchitectureInfo X86_64
osArchitectureInfo Linux = X86.x86_64_linux_info
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
x86ABIMap =
  Map.fromList
    [ (Elf.ELFOSABI_LINUX, Linux),
      (Elf.ELFOSABI_SYSV, Linux),
      (Elf.ELFOSABI_FREEBSD, FreeBSD),
      (Elf.ELFOSABI_NETBSD, FreeBSD)
    ]

-- | Return x86 OS value for Elf file with given ABI
x86OSForABI :: Elf.ElfOSABI -> Maybe X86OS
x86OSForABI abi = Map.lookup abi x86ABIMap

------------------------------------------------------------------------
-- Get binary information

type ProcessPLTEntries w =
  forall r.
  Elf.ElfHeaderInfo w ->
  V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w)) ->
  InitDiscM r (Maybe (PltInfo w))

data SomeArchitectureInfo w where
  SomeArch ::
    !(ArchitectureInfo arch) ->
    !(ProcessPLTEntries (ArchAddrWidth arch)) ->
    SomeArchitectureInfo (ArchAddrWidth arch)

getElfArchInfo ::
  Elf.ElfClass w ->
  Elf.ElfMachine ->
  Elf.ElfOSABI ->
  IO (Either String ([String], SomeArchitectureInfo w))
getElfArchInfo cl arch abi =
  case (cl, arch) of
    (Elf.ELFCLASS64, Elf.EM_X86_64) -> do
      -- Test if ABI is known.
      let warnings
            | isNothing (x86OSForABI abi) = [warnABIUntested abi]
            | otherwise = []
      pure $ Right $ (warnings, SomeArch X86.x86_64_linux_info processX86PLTEntries)
#ifdef SUPPORT_ARM
    (Elf.ELFCLASS32, Elf.EM_ARM) -> do
      let warnings
            | abi /= ELFOSABI_SYSV = [warnABIUntested abi]
            | otherwise = []
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure $ Right $ (warnings, SomeArch armArch32le ignorePLTEntries)
    (Elf.ELFCLASS64, Elf.EM_AARCH64) -> do
      let warnings
            | abi /= ELFOSABI_SYSV = [warnABIUntested abi]
            | otherwise = []
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure $ Right $ (warnings, SomeArch armArch64le ignorePLTEntries)
#endif
    _ -> do
      let archName = case Map.lookup arch Elf.elfMachineNameMap of
            Just nm -> nm
            Nothing -> "unknown-abi(" ++ showHex (Elf.fromElfMachine arch) ")"
      pure $
        Left $
          printf
            "Do not support %d-bit %s %s binaries."
            (Elf.elfClassBitWidth cl)
            archName
            (show abi)

$(pure [])


------------------------------------------------------------------------
-- Explore a control flow graph.

elfInstances ::
  Elf.ElfHeaderInfo w ->
  ((MemWidth w, Integral (Elf.ElfWordType w), Show (Elf.ElfWordType w)) => a) ->
  a
elfInstances hdr x =
  case Elf.headerClass (Elf.header hdr) of
    Elf.ELFCLASS32 -> x
    Elf.ELFCLASS64 -> x

-- | Insert a symbol table entry into map.
--
-- This is used in dynamic binaries.
insSymbol ::
  forall r w.
  (MemWidth w, Integral (Elf.ElfWordType w)) =>
  -- | Loaded memory
  Memory w ->
  -- | Base address that binary is loaded at
  MemAddr w ->
  (Int, Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w)) ->
  StateT (SymAddrMap w) (InitDiscM r) ()
insSymbol mem baseAddr (idx, symEntry)
  -- Skip non-function symbols
  | Elf.steType symEntry /= Elf.STT_FUNC = do
    pure ()
  | Elf.steIndex symEntry == Elf.SHN_UNDEF = do
    pure ()
  | BS.null (Elf.steName symEntry) = do
    lift $ initWarning (show (EmptySymbolName idx (Elf.steType symEntry)))
  | Elf.steIndex symEntry == Elf.SHN_ABS = do
    if addrBase baseAddr == 0
      then do
        let val = Elf.steValue symEntry
        case resolveAbsoluteAddr mem (fromIntegral val) of
          Just addr ->
            modify $ symAddrMapInsert symEntry addr
          Nothing -> do
            lift $ initWarning $ show $ CouldNotResolveAddr (Elf.steName symEntry)
            pure ()
      else lift $ initWarning "SHN_ABS symbols not supported in dynamic binaries."
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

withSymtab ::
  Integral (Elf.ElfWordType w) =>
  -- | Header information
  Elf.ElfHeaderInfo w ->
  -- | Map from names to section indices with name.
  Map BS.ByteString [Word16] ->
  String ->
  BS.ByteString ->
  -- | Value if nothing defined.
  a ->
  -- | Continuation to run to get symtab and string table (respectively).
  (BS.ByteString -> BS.ByteString -> InitDiscM r a) ->
  InitDiscM r a
withSymtab hdrInfo shdrNameMap warnName secName d f = do
  withShdr shdrNameMap warnName secName False d $ \symtabIdx -> do
    let symtabShdr = Elf.shdrByIndex hdrInfo symtabIdx
    let symtabData = shdrContents hdrInfo symtabShdr
    let strtabIdx = Elf.shdrLink symtabShdr
    if strtabIdx == 0 || strtabIdx >= fromIntegral (Elf.shdrCount hdrInfo)
      then do
        initWarning $ printf "Invalid string table index %s in %s." (show strtabIdx) (BSC.unpack secName)
        pure d
      else do
        let strtabShdr = Elf.shdrByIndex hdrInfo (fromIntegral strtabIdx)
        let strtabData = shdrContents hdrInfo strtabShdr
        f symtabData strtabData

addDefinedSymbolTableFuns ::
  (MemWidth w, Integral (Elf.ElfWordType w)) =>
  Elf.ElfHeaderInfo w ->
  -- | Memory created from binary.
  Memory w ->
  -- | Address binary is loaded at
  MemAddr w ->
  -- | Symbol table for parsing.
  BS.ByteString ->
  -- | String table
  BS.ByteString ->
  StateT (SymAddrMap w) (InitDiscM r) ()
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
              go (idx + 1)
            Right symEntry -> do
              insSymbol mem baseAddr (fromIntegral idx, symEntry)
              go (idx + 1)
  go 1

#ifdef SUPPORT_ARM
ignorePLTEntries :: ProcessPLTEntries w
ignorePLTEntries _ _ _ _ _ _ _ = pure ()
#endif

$(pure [])

reportSymbolResError :: SymbolResolutionError -> State [SymbolResolutionError] ()
reportSymbolResError e = seq e $ modify $ \s -> (e : s)

-- | Resolve a symbol table entry in an object file.
resolveObjSymbol ::
  Elf.ElfHeaderInfo w ->
  Memory w ->
  -- | Map from section index to offset in memory of section.
  Map Word16 (MemSegmentOff w) ->
  -- | Symbol addr map
  (SymAddrMap w) ->
  -- | Index of symbol in symbol table and entry.
  (Int, Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w)) ->
  State [SymbolResolutionError] (SymAddrMap w)
resolveObjSymbol hdrInfo mem secMap sam (idx, ste) = elfInstances hdrInfo $ do
  let secIdx = Elf.steIndex ste
  if Elf.steType ste /= Elf.STT_FUNC
    then do
      pure sam
    else
      if secIdx == Elf.SHN_UNDEF
        then pure sam
        else
          if Elf.steName ste == ""
            then do
              reportSymbolResError $ EmptySymbolName idx (Elf.steType ste)
              pure sam
            else
              if secIdx == Elf.SHN_ABS
                then do
                  let val = Elf.steValue ste
                  case resolveAbsoluteAddr mem (fromIntegral val) of
                    Just addr -> do
                      pure $! symAddrMapInsert ste addr sam
                    Nothing -> do
                      reportSymbolResError $ CouldNotResolveAddr (Elf.steName ste)
                      pure sam
                else
                  if Elf.fromElfSectionIndex secIdx >= Elf.shdrCount hdrInfo
                    then do
                      reportSymbolResError $ CouldNotResolveAddr (Elf.steName ste)
                      pure sam
                    else do
                      let shdr = Elf.shdrByIndex hdrInfo (Elf.fromElfSectionIndex secIdx)
                      let val = Elf.steValue ste
                      case Map.lookup (Elf.fromElfSectionIndex secIdx) secMap of
                        Just base
                          | Elf.shdrAddr shdr <= val && (val - Elf.shdrAddr shdr) < Elf.shdrSize shdr,
                            off <- toInteger (val - Elf.shdrAddr shdr),
                            Just addr <- incSegmentOff base off -> do
                            pure $! symAddrMapInsert ste addr sam
                        _ -> do
                          reportSymbolResError $ CouldNotResolveAddr (Elf.steName ste)
                          pure sam

$(pure [])

initDiscState ::
  -- | Initial memory
  Memory (ArchAddrWidth arch) ->
  -- | Initial entry points
  [MemSegmentOff (ArchAddrWidth arch)] ->
  -- | Region information
  RegionInfo ->
  -- | Symbol addr map
  SymAddrMap (ArchAddrWidth arch) ->
  -- | Explore predicate
  (ArchSegmentOff arch -> Bool) ->
  ArchitectureInfo arch ->
  ReoptOptions ->
  Except String (DiscoveryState arch)
initDiscState mem initPoints regInfo symAddrMap explorePred ainfo reoptOpts = do
  let resolveEntry qsn
        | ".cold" `BS.isSuffixOf` qsnBytes qsn = Nothing
        | otherwise = Just MayReturnFun
  let entryPoints0 =
        Map.mapMaybe resolveEntry (samAddrMap symAddrMap)
          & addKnownFn symAddrMap "abort" NoReturnFun
          & addKnownFn symAddrMap "exit" NoReturnFun
          & addKnownFn symAddrMap "_Unwind_Resume" NoReturnFun
          & addKnownFn symAddrMap "__cxa_rethrow" NoReturnFun
          & addKnownFn symAddrMap "__cxa_throw" NoReturnFun
          & addKnownFn symAddrMap "__malloc_assert" NoReturnFun
          & addKnownFn symAddrMap "__stack_chk_fail" NoReturnFun
          & addKnownFn symAddrMap "_ZSt9terminatev" NoReturnFun
  let entryPoints = foldl (\m a -> Map.insert a MayReturnFun m) entryPoints0 initPoints
  case (roIncluded reoptOpts, roExcluded reoptOpts) of
    ([], excludeNames) -> do
      excludeAddrs <- mapM (resolveSymAddr mem regInfo symAddrMap) excludeNames
      let s = Set.fromList excludeAddrs
      let initState =
            emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
              & trustedFunctionEntryPoints .~ entryPoints
              & exploreFnPred .~ (\a -> Set.notMember a s && explorePred a)
              & markAddrsAsFunction InitAddr (Map.keys entryPoints)
      pure $! initState
    (includeNames, []) -> do
      includeAddrs <- mapM (resolveSymAddr mem regInfo symAddrMap) includeNames
      let s = Set.fromList includeAddrs
      let initState =
            emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
              & trustedFunctionEntryPoints .~ entryPoints
              & exploreFnPred .~ (\a -> Set.member a s)
              & markAddrsAsFunction InitAddr s
      pure $! initState
    _ -> do
      throwError "Cannot both include and exclude specific addresses."

$(pure [])

-- | Identify symbol names
discoverSymbolNames ::
  (MemWidth w, Integral (Elf.ElfWordType w)) =>
  -- | Binary information
  Elf.ElfHeaderInfo w ->
  -- | Initial memory for binary
  Memory w ->
  -- | Base address to add to symbol offsets.
  MemAddr w ->
  InitDiscM r (SymAddrMap w)
discoverSymbolNames hdrInfo mem baseAddr = do
  let shdrs =
        case Elf.headerNamedShdrs hdrInfo of
          Left _ -> V.empty
          Right r -> r
  let shdrNameMap :: Map BS.ByteString [Word16]
      shdrNameMap =
        Map.fromListWith
          (++)
          [ (Elf.shdrName s, [fromIntegral (idx -1)])
            | idx <- [1 .. V.length shdrs],
              let s = shdrs V.! (idx -1)
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

-- | Information about .eh_frame/.debug_frame
data Frame w = Frame
  { frameMem :: !(Memory w),
    frameRegion :: !RegionIndex,
    -- | Flag to indicate if this is .eh_frame or .debug_frame.
    frameCtx :: !Dwarf.FrameContext,
    -- | Endianess
    frameEnd :: !Dwarf.Endianess,
    -- | Address frame is loaded at.
    frameAddr :: !Word64,
    -- | Bytes in frame
    frameData :: !BS.ByteString
  }

-- | Print out all the FDEs for the given CIE in Dwarf dump format.
fdeEntryPoints ::
  Frame w ->
  [MemSegmentOff w] ->
  -- | CIE for this FDE
  Dwarf.DW_CIE ->
  -- | Offset within eh frame.
  Word64 ->
  InitDiscM r (Dwarf.FDEParseError, [MemSegmentOff w])
fdeEntryPoints f entries cie off = do
  let mem = frameMem f
  case Dwarf.getFDEAt (frameCtx f) (frameData f) cie off of
    Left e -> do
      pure (e, entries)
    Right (fde, off') -> addrWidthClass (memAddrWidth mem) $ do
      let faddr = frameAddr f + Dwarf.fdeStartAddress fde
      case resolveRegionOff mem (frameRegion f) (fromIntegral faddr) of
        Nothing -> do
          incCompLog "Could not resolve FDE address."
          fdeEntryPoints f entries cie off'
        Just a -> do
          fdeEntryPoints f (a : entries) cie off'

-- | Pretty print CIEs in file with
cieEntryPoints ::
  Frame w ->
  Word64 ->
  [MemSegmentOff w] ->
  InitDiscM r [MemSegmentOff w]
cieEntryPoints f off entries
  | BS.length (frameData f) <= fromIntegral off =
    pure entries
cieEntryPoints f off entries =
  case Dwarf.getCIE (frameCtx f) (frameEnd f) Dwarf.TargetSize64 (frameData f) off of
    Left (_, msg) -> do
      incCompLog $ "CIE " <> showHex off " parse failure: " <> msg
      pure entries
    Right (_, Nothing) -> do
      pure entries
    Right (off', Just cie) -> do
      (fdeErr, entries') <- fdeEntryPoints f entries cie off'
      case fdeErr of
        Dwarf.FDEReachedEnd ->
          pure entries
        Dwarf.FDEParseError fdeOff msg -> do
          incCompLog $ "FDE error " ++ showHex fdeOff ": " ++ msg
          pure entries
        Dwarf.FDECIE nextCIEOff -> do
          cieEntryPoints f nextCIEOff entries'
        Dwarf.FDEEnd _ -> do
          pure entries'

-- | Pretty print CIEs in file with
ehframeEntryPoints ::
  -- | Elf file
  Elf.ElfHeaderInfo w ->
  -- | Section header map
  Map BS.ByteString [Elf.Shdr nm (Elf.ElfWordType w)] ->
  -- | Initial memory
  Memory w ->
  -- | Index for resolving CIE addresses
  RegionIndex ->
  [MemSegmentOff w] ->
  InitDiscM r [MemSegmentOff w]
ehframeEntryPoints elfFile shdrMap mem regIdx entries = do
  let nm = ".eh_frame"
  case Map.findWithDefault [] nm shdrMap of
    [] -> do
      pure entries
    frameSection : rest -> do
      when (not (null rest)) $ do
        incCompLog $ "Multiple " <> BSC.unpack nm <> " sections."
      if not (null (Map.findWithDefault [] (".rela" <> nm) shdrMap))
        then do
          incCompLog $ "Do not support relocations in " <> BSC.unpack nm <> "."
          pure entries
        else do
          let f =
                Frame
                  { frameMem = mem,
                    frameRegion = regIdx,
                    frameCtx = Dwarf.EhFrame,
                    frameEnd =
                      case Elf.headerData (Elf.header elfFile) of
                        Elf.ELFDATA2LSB -> Dwarf.LittleEndian
                        Elf.ELFDATA2MSB -> Dwarf.BigEndian,
                    frameAddr =
                      Elf.elfClassInstances (Elf.headerClass (Elf.header elfFile)) $
                        fromIntegral (Elf.shdrAddr frameSection),
                    frameData = Elf.shdrData elfFile frameSection
                  }
          seq f $ cieEntryPoints f 0 entries

-- | Creates InitDiscovery state containing all information needed
-- to perform function discovery.
initExecDiscovery ::
  forall arch r.
  -- | Base address for loading
  MemAddr (ArchAddrWidth arch) ->
  Elf.ElfHeaderInfo (ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  ProcessPLTEntries (ArchAddrWidth arch) ->
  ReoptOptions ->
  InitDiscM (Either String r) (InitDiscovery arch)
initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts = elfInstances hdrInfo $ do
  -- Create memory image for elf file.
  (mem, _secMap, warnings) <-
    case memoryForElfSegments' (addrBase baseAddr) (toInteger (addrOffset baseAddr)) hdrInfo of
      Left errMsg -> initError errMsg
      Right r -> pure r
  mapM_ (initWarning . show) warnings

  symAddrMap0 <-
    discoverSymbolNames hdrInfo mem baseAddr

  -- Resolve section headers
  shdrs <-
    case Elf.headerNamedShdrs hdrInfo of
      Left (secIdx, _) -> do
        initError $ printf "Could not resolve name of section %s." (show secIdx)
      Right shdrs -> do
        pure shdrs

  -- Resolve PLT entries
  mpltRes <- pltFn hdrInfo shdrs

  -- Create symbol address map that includes plt information if available.
  symAddrMap <-
    case mpltRes of
      Just pltRes -> do
        let vmap = pltVirtMap pltRes
        let hdr = Elf.header hdrInfo
        let regIdx = addrBase baseAddr
        strtab <-
          case Elf.lookupVirtAddrContents (pltStrtabOff pltRes) vmap of
            Nothing -> initError "Dynamic string table invalid."
            Just s -> do
              when (BS.length s < fromIntegral (pltStrtabSize pltRes)) $ do
                initError "Dynamic string table range invalid."
              pure $ BS.take (fromIntegral (pltStrtabSize pltRes)) s
        symtab <-
          case Elf.lookupVirtAddrContents (pltSymtabOff pltRes) vmap of
            Nothing -> initError "Dynamic symbol table invalid."
            Just s -> pure s
        addPltSyms hdr strtab symtab mem regIdx symAddrMap0 pltRes
      Nothing -> do
        pure symAddrMap0

  -- Exclude PLT bounds
  let addrIsNotInPlt :: ArchSegmentOff arch -> Bool
      addrIsNotInPlt =
        case mpltRes of
          Nothing -> const True
          Just pltRes -> \a -> do
            let off = fromIntegral $ addrOffset (segoffAddr a)
            case Map.lookupLE off (pltMap pltRes) of
              Just (entryOff, (_pltFn, entrySize))
                | off - entryOff < entrySize ->
                  False
              _ -> True

  -- Maps section header names to the section headers for it.
  let shdrMap :: Map BS.ByteString [Elf.Shdr BS.ByteString (Elf.ElfWordType (ArchAddrWidth arch))]
      shdrMap =
        Map.fromListWith (++) [(Elf.shdrName s, [s]) | s <- V.toList shdrs]

  let addrInRodata =
        case Map.findWithDefault [] ".rodata" shdrMap of
          [shdr] ->
            let sOff :: Word64
                sOff = fromIntegral $ Elf.shdrAddr shdr
                sSize :: Word64
                sSize = fromIntegral (Elf.shdrSize shdr)
             in \a ->
                  let aOff = memWordValue $ addrOffset (segoffAddr a)
                   in sOff <= aOff && (aOff - sOff) < sSize
          _ -> \_ -> False

  let explorePred a = addrIsNotInPlt a && not (addrInRodata a)
  -- Get initial entry address
  let hdr = Elf.header hdrInfo
  let entry = Elf.headerEntry hdr
  let entryAddr = asSegmentOff mem (incAddr (toInteger entry) baseAddr)
  when (isNothing entryAddr) $ do
    initWarning $ "Could not resolve entry point: " ++ showHex entry ""


  -- Get eh_frame entry points
  ehFrameAddrs <-
    ehframeEntryPoints hdrInfo shdrMap mem (addrBase baseAddr) (maybeToList entryAddr)

  -- Create initial discovery state.
  let regInfo :: RegionInfo
      regInfo = HasDefaultRegion (addrBase baseAddr)
  s <-
    case runExcept (initDiscState mem ehFrameAddrs regInfo symAddrMap explorePred ainfo reoptOpts) of
      Left e -> initError e
      Right r -> pure r
  -- Return discovery
  pure
    $! InitDiscovery
      { initDiscSymAddrMap = symAddrMap,
        initDiscBaseCodeAddr = baseAddr,
        initDiscoveryState = s
      }

-- | Creates InitDiscovery state containing all information needed
-- to perform function discovery.
doInit ::
  forall arch r.
  -- | Option to load the binary at the given address
  LoadOptions ->
  Elf.ElfHeaderInfo (ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  ProcessPLTEntries (ArchAddrWidth arch) ->
  ReoptOptions ->
  InitDiscM (Either String r) (InitDiscovery arch)
doInit loadOpts hdrInfo ainfo pltFn reoptOpts = elfInstances hdrInfo $ do
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
          Left errMsg -> initError errMsg
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
            let staticEntries = zip [0 ..] (V.toList (Elf.symtabEntries tbl))
            let (sm, errs) = runState (foldlM (resolveObjSymbol hdrInfo mem secMap) symAddrMapEmpty staticEntries) []
            forM_ errs $ \e -> do
              initWarning (show e)
            pure sm

      shdrs <-
        case Elf.headerNamedShdrs hdrInfo of
          Left (secIdx, _) -> do
            initError $ printf "Could not resolve name of section %s." (show secIdx)
          Right shdrs -> do
            pure shdrs

      -- Get index of text section section.
      textSectionIndex <- do
        let isText s = Elf.shdrName s == ".text"
        case V.findIndex isText shdrs of
          Nothing -> do
            initError "Could not find .text section."
          Just secIdx -> do
            when (isJust (V.findIndex isText (V.drop (secIdx + 1) shdrs))) $ do
              initError "Duplicate .text sections found."
            pure $ (fromIntegral secIdx :: Word16)
      -- Get offset for text section.
      textBaseAddr <-
        case Map.lookup textSectionIndex secMap of
          Nothing -> do
            initError "Text section is not loaded."
          Just a ->
            pure a
      let entryAddr = Nothing
      -- Get region of text segment
      let regIdx = segmentBase (segoffSegment textBaseAddr)
      let regInfo :: RegionInfo
          regInfo = HasDefaultRegion regIdx
      let explorePred = \_ -> True
      s <- case runExcept (initDiscState mem (maybeToList entryAddr) regInfo symAddrMap explorePred ainfo reoptOpts) of
        Left e -> initError e
        Right r -> pure r
      -- Get initial entries and predicate for exploring
      pure
        $! InitDiscovery
          { initDiscSymAddrMap = symAddrMap,
            initDiscBaseCodeAddr = MemAddr regIdx 0,
            initDiscoveryState = s
          }
    -- Executable
    Elf.ET_EXEC -> do
      -- Get base address to use for computing section offsets.
      let baseAddr :: MemAddr (ArchAddrWidth arch)
          baseAddr = MemAddr {addrBase = 0, addrOffset = fromInteger (loadRegionBaseOffset loadOpts)}
      initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts
    -- Shared library or position-independent executable.
    Elf.ET_DYN -> do
      -- Get base address to use for computing section offsets.
      let baseAddr :: MemAddr (ArchAddrWidth arch)
          baseAddr =
            case loadOffset loadOpts of
              Just o -> MemAddr {addrBase = 0, addrOffset = fromIntegral o}
              Nothing -> MemAddr {addrBase = 1, addrOffset = 0}
      initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts
    Elf.ET_CORE -> do
      initError "Core files unsupported."
    tp -> do
      initError $ "Elf files with type " ++ show tp ++ " unsupported."

$(pure [])

------------------------------------------------------------------------
-- Dynamic dependencies

-- | Get values of DT_NEEDED entries.
parseDynamicNeeded ::
  Elf.ElfHeaderInfo w ->
  Either String [BS.ByteString]
parseDynamicNeeded elf = elfInstances elf $
  case filter (\p -> Elf.phdrSegmentType p == Elf.PT_DYNAMIC) (Elf.headerPhdrs elf) of
    [dynPhdr] ->
      let dynContents = Elf.slice (Elf.phdrFileRange dynPhdr) (Elf.headerFileContents elf)
      in case Elf.dynamicEntries (Elf.headerData ehdr) (Elf.headerClass ehdr) dynContents of
           Left dynError ->
             let errMsg = "Could not parse dynamic section of Elf file: " ++ show dynError
             in Left errMsg
           Right dynSection -> do
             case Elf.virtAddrMap (Elf.headerFileContents elf) (Elf.headerPhdrs elf) of
               Nothing -> do
                 Left $ "Could not construct virtual address map from bytestring and list of program headers in."
               Just phdrs ->
                 case Elf.dynNeeded dynSection phdrs of
                   Left errMsg -> Left $ "Could not parse phdrs from Elf file: "++errMsg
                   Right deps -> Right deps
    [] -> Left "No PT_DYNAMIC section."
    _ -> Left "Multiple PT_DYNAMIC sections."
  where ehdr = Elf.header elf


$(pure [])

---------------------------------------------------------------------------------
-- Logging

globalStepStarted :: ReoptGlobalStep arch a -> ReoptM arch r ()
globalStepStarted s = reoptLog (ReoptGlobalStepStarted s)

globalStepFinished :: ReoptGlobalStep arch a -> a -> ReoptM arch r ()
globalStepFinished s a = reoptLog (ReoptGlobalStepFinished s a)

globalStepWarning :: ReoptGlobalStep arch a -> String -> ReoptM arch r ()
globalStepWarning s m = reoptLog (ReoptGlobalStepWarning s m)

funStepStarted :: ReoptFunStep arch r e a -> FunId -> ReoptM arch z ()
funStepStarted s f = reoptLog (ReoptFunStepStarted s f)

funStepFailed :: ReoptFunStep arch r e a -> FunId -> e -> ReoptM arch z ()
funStepFailed s f e = reoptLog (ReoptFunStepFailed s f e)

funStepFinished :: ReoptFunStep arch r e a -> FunId -> r -> ReoptM arch z ()
funStepFinished s f r = reoptLog (ReoptFunStepFinished s f r)

funStepAllFinished :: ReoptFunStep arch r e a -> a -> ReoptM arch z ()
funStepAllFinished f a = reoptLog (ReoptFunStepAllFinished f a)

---------------------------------------------------------------------------------
-- Initial type inference

headerTypeMap ::
  forall arch r.
  MemWidth (ArchAddrWidth arch) =>
  -- | Header with hints for assisting typing.
  AnnDeclarations ->
  -- | Function information gleaned from dynamic dependencies.
  Map BS.ByteString ReoptFunType ->
  SymAddrMap (ArchAddrWidth arch) ->
  Map (ArchSegmentOff arch) NoReturnFunStatus ->
  ReoptM arch r (FunTypeMaps (ArchAddrWidth arch))
headerTypeMap hdrAnn dynDepsTypeMap symAddrMap noretMap = do
  globalStepStarted HeaderTypeInference

  let voidPtrType = PtrAnnType VoidAnnType
  let charPtrType = PtrAnnType (IAnnType 8)
  let sizetType = IAnnType 64
  let offType = sizetType
  let intType = IAnnType 32
  let ftype res args = ReoptNonvarargFunType (AnnFunType {funRet = res, funArgs = V.fromList args})
  let declFn = Map.singleton
  let nmArg nm tp = AnnFunArg (Just nm) tp
  let nonmArg tp = AnnFunArg Nothing tp
  -- Generate type information from annotations
  let nameAnnTypeMap =
        fmap ReoptNonvarargFunType (funDecls hdrAnn)
          <> dynDepsTypeMap
          <> declFn "__errno_location" (ftype (PtrAnnType (IAnnType 32)) [])
          <> declFn "__fstat" (ftype intType (fmap nonmArg [intType, voidPtrType]))
          <> declFn "__printf_chk" (ftype intType [nmArg "flag" intType, AnnFunArg (Just "format") charPtrType])
          <> declFn "__stack_chk_fail" (ftype VoidAnnType [])
          <> declFn "exit" (ftype VoidAnnType [AnnFunArg (Just "status") (IAnnType 64)])
          <> declFn "fprintf" (ReoptPrintfFunType 1)
          <> declFn "memcmp" (ftype intType [nmArg "s1" voidPtrType, nmArg "s1" voidPtrType, nmArg "n" sizetType])
          <> declFn "mmap" (ftype voidPtrType (fmap nonmArg [voidPtrType, sizetType, intType, intType, intType, offType]))
          <> declFn "open" ReoptOpenFunType
          <> declFn "printf" (ReoptPrintfFunType 0)
          <> declFn "putchar" (ftype intType [nmArg "c" intType])
          <> declFn "puts" (ftype intType [nonmArg charPtrType])
          <> declFn "sprintf" (ReoptPrintfFunType 1)
          <> declFn "snprintf" (ReoptPrintfFunType 2)

  -- Generate map from address names to known type.
  --
  -- This is used when we see a function jumps to a defined address.
  addrAnnTypeMap <- do
    let insSymType ::
          Map (ArchSegmentOff arch) ReoptFunType ->
          (BS.ByteString, ReoptFunType) ->
          ReoptM arch r (Map (ArchSegmentOff arch) ReoptFunType)
        insSymType m (sym, annTp) = do
          case symAddrMapLookup symAddrMap sym of
            Left SymAddrMapNotFound -> do
              -- Silently drop symbols without addresses as they may be undefined.
              pure m
            Left SymAddrMapAmbiguous -> do
              globalStepWarning HeaderTypeInference $
                "Ambiguous symbol " ++ BSC.unpack sym ++ "."
              pure m
            Right addr -> do
              pure $! Map.insert addr annTp m
    foldlM insSymType Map.empty (Map.toList nameAnnTypeMap)

  let annTypeMap :: FunTypeMaps (ArchAddrWidth arch)
      annTypeMap =
        FunTypeMaps
          { nameToAddrMap = symAddrMap,
            nameTypeMap = nameAnnTypeMap,
            addrTypeMap = addrAnnTypeMap,
            noreturnMap = noretMap
          }
  globalStepFinished HeaderTypeInference ()
  pure annTypeMap


---------------------------------------------------------------------------------
-- Dynamic Dependency Helpers

-- | Return the path to the debug info directory -- N.B., depends
-- on environment variable REOPTHOME.
debugInfoDir :: IO FilePath
debugInfoDir = do
  mStr <- getEnv "REOPTHOME"
  case mStr of
    Nothing -> getXdgDirectory XdgData ".reopt"
    Just "" -> getXdgDirectory XdgData ".reopt"
    Just path -> pure path

-- | Given a binary's name, return the path to its debug function information
--   cache (whether or not the file exists).
debugInfoFileCache :: String -> IO FilePath
debugInfoFileCache binaryName = do
  dir <- debugInfoDir
  pure $ dir </> binaryName <.> "debug_info"

-- | Finds the cached debug info file for a dependency (if one exists).
findDepDebugInfo :: String -> IO (Maybe FilePath)
findDepDebugInfo depName = do
  cFile <- debugInfoFileCache depName
  cFileExists <- doesFileExist cFile
  if cFileExists
  then pure $ Just cFile
  else do
    cFile' <- debugInfoFileCache $ depName ++ ".debug"
    cFileExists' <- doesFileExist cFile'
    if cFileExists'
    then pure $ Just cFile'
    else do
      hPutStrLn stderr $ "Could not find debug info associated with " ++ depName
      pure $ Nothing


-- | Add the cached debug information from dynamic
--   dependencies (if available).
addDynDepDebugInfo ::
  Map BS.ByteString ReoptFunType ->
  BS.ByteString ->
  ReoptM arch r (Map BS.ByteString ReoptFunType)
addDynDepDebugInfo m dep = reoptIO $ do
  mCacheFile <- findDepDebugInfo $ BSC.unpack dep
  case mCacheFile of
    Nothing -> pure m
    Just cacheFile -> do
      contents <- readFile cacheFile
      case (reads contents) :: [(Map BS.ByteString ReoptFunType, String)] of
        [] -> do
          hPutStrLn stderr $ "Internal warning: " ++ cacheFile ++ " did not contain valid data."
          pure m
        ((m',_):_) -> do
          pure $ m <> m'

---------------------------------------------------------------------------------
-- Complete discovery

doDiscovery ::
  forall arch r.
  -- | Header with hints for assisting typing.
  AnnDeclarations ->
  Elf.ElfHeaderInfo (ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  InitDiscovery arch ->
  -- | Options controlling discovery
  DiscoveryOptions ->
  ReoptM
    arch
    r
    ( FunTypeMaps (ArchAddrWidth arch),
      DiscoveryState arch
    )
doDiscovery hdrAnn hdrInfo ainfo initState disOpt = withArchConstraints ainfo $ do
  let s = initDiscoveryState initState
  let mem = memory s
  let symAddrMap = initDiscSymAddrMap initState

  -- Mark initialization as finished.
  globalStepFinished DiscoveryInitialization s

  dynDepsMap <- do
    dynDeps <- case parseDynamicNeeded hdrInfo of
                 Left errMsg -> do
                   globalStepWarning DebugTypeInference errMsg
                   pure []
                 Right deps -> pure deps
    foldlM addDynDepDebugInfo Map.empty dynDeps

  let baseAddr = initDiscBaseCodeAddr initState
  annTypeMap <- headerTypeMap hdrAnn dynDepsMap symAddrMap (s ^. trustedFunctionEntryPoints)

  -- Resolve debug information.
  let resolveFn _symName off =
       let addr = incAddr (toInteger off) baseAddr
        in asSegmentOff mem addr


  debugTypeMap <-
    reoptIncComp $
      resolveDebugFunTypes resolveFn annTypeMap hdrInfo
  let postDebugState = s & trustedFunctionEntryPoints .~ noreturnMap debugTypeMap

  let symMap = getAddrSymMap symAddrMap
  discState <-
    reoptRunDiscovery symMap $
      incCompleteDiscovery postDebugState disOpt
  funStepAllFinished Discovery discState
  pure (debugTypeMap, discState)

------------------------------------------------------------------------
-- Print disassembly

-- | Return number of digits required to show a given unsigned number in hex.
hexDigitsReq :: Bits a => a -> Int
hexDigitsReq b = go 1 (b `shiftR` 4)
  where
    go r v
      | popCount v == 0 = r
      | otherwise = go (r + 1) (v `shiftR` 4)

trimForWord64Buffer :: Word64 -> Int -> String -> String
trimForWord64Buffer base n s = drop d s
  where
    m = hexDigitsReq (max base (base + fromIntegral n))
    d = 16 - m

-- | Show a given hexideimal number with a fixed width, adding
-- zeros as needed.
showPaddedHex :: (FiniteBits a, Integral a, Show a) => a -> String
showPaddedHex v = assert (l >= n) $ replicate (l - n) '0' ++ s
  where
    l = finiteBitSize v `shiftR` 2
    s
      | v >= 0 = showHex v ""
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
stringToFixedBuffer g s
  | g == n = s
  | g < n = take g s
  | otherwise = s ++ replicate (g - n) ' '
  where
    n = length s

-- | Print out disasembly for a specific line.
printX86DisassemblyLine ::
  -- | Handle to write to
  Handle ->
  -- | Base address for section or segment.
  Word64 ->
  -- | Data region for code.
  BS.ByteString ->
  -- | Output from flexdis
  F.DisassembledAddr ->
  IO ()
printX86DisassemblyLine h base buffer (F.DAddr i n mi) = do
  let o = base + fromIntegral i
  let ppAddr x = trimForWord64Buffer base (BS.length buffer) (showPaddedHex x)
  let b = showBytes $ slice i n buffer
  let r = case mi of
        Nothing -> take 20 b
        Just ins -> stringToFixedBuffer 21 b ++ "\t" ++ show (F.ppInstruction ins)
  hPutStrLn h $ "  " ++ ppAddr o ++ ":\t" ++ r
  when (n > 7) $ do
    printX86DisassemblyLine h base buffer $ F.DAddr (i + 7) (n -7) Nothing

-- | Print all the disassembly for a buffer to stdout.
printX86SectionDisassembly ::
  Handle ->
  BSC.ByteString ->
  Word64 ->
  BS.ByteString ->
  IO ()
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
summarizeX86TermStmt ::
  SyscallPersonality ->
  ComputeArchTermStmtEffects X86_64 ids
summarizeX86TermStmt _ Hlt _ =
  ArchTermStmtRegEffects
    { termRegDemands = [],
      termRegTransfers = []
    }
summarizeX86TermStmt _ UD2 _ =
  ArchTermStmtRegEffects
    { termRegDemands = [],
      termRegTransfers = []
    }
summarizeX86TermStmt sysp X86Syscall proc_state = do
  -- Compute the arguments needed by the function
  let argRegs
        | BVValue _ call_no <- proc_state ^. boundValue syscall_num_reg,
          Just (_, _, argtypes) <- Map.lookup (fromIntegral call_no) (spTypeInfo sysp) =
          take (length argtypes) syscallArgumentRegs
        | otherwise =
          syscallArgumentRegs
  let callRegs = [Some sp_reg] ++ Set.toList x86CalleeSavedRegs
  ArchTermStmtRegEffects
    { termRegDemands = Some <$> argRegs,
      termRegTransfers = callRegs
    }

x86DemandInfo ::
  SyscallPersonality ->
  ArchDemandInfo X86_64
x86DemandInfo sysp =
  ArchDemandInfo
    { functionArgRegs =
        [Some RAX]
          ++ (Some . X86_GP <$> x86GPPArgumentRegs)
          ++ (Some <$> x86FloatArgumentRegs),
      functionRetRegs = ((Some <$> x86ResultRegs) ++ (Some <$> x86FloatResultRegs)),
      calleeSavedRegs = x86CalleeSavedRegs,
      computeArchTermStmtEffects = summarizeX86TermStmt sysp,
      demandInfoCtx = X86.x86DemandContext
    }

------------------------------------------------------------------------
-- LLVMVersion

newtype LLVMVersion = LLVMVersion [Integer]
  deriving (Eq, Ord)

-- | Parse a string as a version
versionOfString :: String -> Maybe LLVMVersion
versionOfString s = do
  let (f, r) = span (/= '.') s
  case (reads f, r) of
    ([(v, "")], [])
      | v >= 0 ->
        if v == 0
          then Just (LLVMVersion [])
          else Just (LLVMVersion [v])
    ([(v, "")], '.' : rest)
      | v >= 0 -> do
        LLVMVersion l <- versionOfString rest
        if null l && v == 0
          then pure (LLVMVersion [])
          else pure (LLVMVersion (v : l))
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
  LPP.Config
    { LPP.cfgLoadImplicitType = True,
      LPP.cfgGEPImplicitType = True,
      LPP.cfgUseDILocation = False
    }

-- | Configuration for LLVM 3.7 & 3.8
latestLLVMConfig :: LLVMConfig
latestLLVMConfig =
  LPP.Config
    { LPP.cfgLoadImplicitType = False,
      LPP.cfgGEPImplicitType = False,
      LPP.cfgUseDILocation = True
    }

llvmVersionMap :: Map LLVMVersion LLVMConfig
llvmVersionMap =
  Map.fromList
    [ (,) "3.5.0" llvm35Config,
      (,) "3.7.0" latestLLVMConfig
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
getReferencedFunctions ::
  forall arch.
  ( Eq (FunctionType arch),
    Show (FunctionType arch),
    MemWidth (ArchAddrWidth arch),
    FoldableFC (ArchFn arch),
    FoldableF (FnArchStmt arch)
  ) =>
  -- | Name of functions with definitions.
  Set.Set BS.ByteString ->
  FunctionTypeMap arch ->
  Function arch ->
  FunctionTypeMap arch
getReferencedFunctions excluded m0 f =
  foldl' (foldFnValue findReferencedFunctions) m0 (fnBlocks f)
  where
    findReferencedFunctions ::
      FunctionTypeMap arch ->
      FnValue arch tp ->
      FunctionTypeMap arch
    findReferencedFunctions m (FnFunctionEntryValue ft nm) =
      insertName nm ft m
    findReferencedFunctions m _ = m

    insertName ::
      BS.ByteString ->
      FunctionType arch ->
      FunctionTypeMap arch ->
      FunctionTypeMap arch
    insertName nm ft m
      | Set.member nm excluded = m
      | otherwise =
        case Map.lookup nm m of
          Just ft'
            | ft /= ft' ->
              error $
                BSC.unpack nm ++ " has incompatible types:\n"
                  ++ show ft
                  ++ "\n"
                  ++ show ft'
                  ++ "\n"
            | otherwise -> m
          _ -> Map.insert nm ft m

-- | Attempt to resolve what register to associate with an argument.
resolveArgType ::
  Monad m =>
  -- | Argument name.
  String ->
  -- | Type to resolve
  AnnType ->
  ArgResolver m ()
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
argsToRegisters ::
  Monad m =>
  -- | Number of arguments processed so far.
  Int ->
  -- | Remaining arguments to parse
  V.Vector AnnFunArg ->
  ArgResolver m ()
argsToRegisters cnt args
  | cnt >= V.length args = pure ()
  | otherwise = do
    let arg = args V.! cnt
    let nm = fromMaybe ("arg" ++ show cnt) (funArgName arg)
    resolveArgType nm (funArgType arg)
    argsToRegisters (cnt + 1) args

parseReturnType :: AnnType -> Either ArgResolverError [Some X86RetInfo]
parseReturnType tp0 =
  case tp0 of
    VoidAnnType -> Right []
    IAnnType w
      | w > 64 -> Left $ UnsupportedReturnType (ppAnnType tp0)
      | otherwise -> Right [Some (RetBV64 F.RAX)]
    FloatAnnType -> Left $ UnsupportedReturnType (ppAnnType tp0)
    DoubleAnnType -> Right [Some (RetZMM ZMMDouble 0)]
    PtrAnnType _ -> Right [Some (RetBV64 F.RAX)]
    TypedefAnnType _ tp -> parseReturnType tp

resolveAnnFunType ::
  Monad m =>
  AnnFunType ->
  ExceptT ArgResolverError m X86FunTypeInfo
resolveAnnFunType funType = do
  args <- runArgResolver (argsToRegisters 0 (funArgs funType))
  ret <-
    case parseReturnType (funRet funType) of
      Left e -> throwError e
      Right r -> pure r
  pure $! X86NonvarargFunType args ret

-- | This checks whether any of the symbols in the map start with the given string as a prefix.
isUsedPrefix :: BSC.ByteString -> SymAddrMap w -> Bool
isUsedPrefix prefix sam = any (\s -> prefix `BSC.isPrefixOf` qsnBytes s) (Map.elems (samAddrMap sam))

-- | Name of recovered function from a qualified symbol name.
localFunctionName :: BSC.ByteString -> QualifiedSymbolName -> MemSegmentOff w -> BSC.ByteString
localFunctionName prefix qnm segOff =
  if qsnGlobal qnm
    then qsnBytes qnm
    else
      let addr = segoffAddr segOff
       in prefix <> "_" <> qsnBytes qnm
            <> "_"
            <> BSC.pack (show (addrBase addr))
            <> "_"
            <> BSC.pack (show (addrOffset addr))

-- | Name of recovered function when no function exists.
nosymFunctionName :: BSC.ByteString -> MemSegmentOff w -> BSC.ByteString
nosymFunctionName prefix segOff =
  let addr = segoffAddr segOff
   in prefix <> "_" <> BSC.pack (show (addrBase addr)) <> "_" <> BSC.pack (show (addrOffset addr))

-- | Returns name of recovered function.
recoveredFunctionName ::
  MemWidth w =>
  -- | Maps addresses of symbols to the associated symbol name.
  SymAddrMap w ->
  -- | Prefix to use for automatically generated symbols.
  -- To be able to distinguish symbols, this should not be
  -- a prefix for any of the symbols in the map.
  BSC.ByteString ->
  MemSegmentOff w ->
  BSC.ByteString
recoveredFunctionName m prefix segOff =
  case Map.lookup segOff (samAddrMap m) of
    Just qname -> localFunctionName prefix qname segOff
    Nothing -> nosymFunctionName prefix segOff

$(pure [])

-- | Construct function type from demands.
inferFunctionTypeFromDemands ::
  Map (MemSegmentOff 64) (DemandSet X86Reg) ->
  Map (MemSegmentOff 64) X86FunTypeInfo
inferFunctionTypeFromDemands dm =
  let go ::
        DemandSet X86Reg ->
        Map (MemSegmentOff 64) (RegisterSet X86Reg) ->
        Map (MemSegmentOff 64) (RegisterSet X86Reg)
      go ds m = Map.unionWith Set.union (functionResultDemands ds) m

      retDemands :: Map (MemSegmentOff 64) (RegisterSet X86Reg)
      retDemands = foldr go Map.empty dm

      -- drop the suffix which isn't a member of the arg set.  This
      -- allows e.g. arg0, arg2 to go to arg0, arg1, arg2.
      dropArgSuffix ::
        (a -> X86Reg tp) ->
        [a] ->
        RegisterSet X86Reg ->
        [a]
      dropArgSuffix f regs rs =
        reverse $ dropWhile (not . (`Set.member` rs) . Some . f) $ reverse regs

      -- Turns a set of arguments into a prefix of x86 argument registers and friends
      orderPadArgs :: (RegisterSet X86Reg, RegisterSet X86Reg) -> X86FunTypeInfo
      orderPadArgs (argSet, retSet) =
        let args =
              fmap ArgBV64 (dropArgSuffix X86_GP x86GPPArgumentRegs argSet)
                ++ fmap (ArgZMM ZMM512D) (dropArgSuffix X86_ZMMReg [0 .. 7] argSet)
            rets =
              fmap (Some . RetBV64) (dropArgSuffix X86_GP [F.RAX, F.RDX] retSet)
                ++ fmap (Some . RetZMM ZMM512D) (dropArgSuffix X86_ZMMReg [0, 1] retSet)
         in X86NonvarargFunType args rets
   in orderPadArgs
        <$> Map.mergeWithKey
          (\_ ds rets -> Just (registerDemands ds, rets))
          (fmap (\ds -> (registerDemands ds, mempty)))
          (fmap (\s -> (mempty, s)))
          dm
          retDemands

$(pure [])

resolveReoptFunType ::
  Monad m =>
  ReoptFunType ->
  ExceptT ArgResolverError m X86FunTypeInfo
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
-- recoverX86Elf

matchPLT :: DiscoveryFunInfo arch ids -> Maybe VersionedSymbol
matchPLT finfo
  | [b] <- Map.elems (finfo ^. parsedBlocks),
    PLTStub _ _ sym <- pblockTermStmt b =
    Just sym
matchPLT _ = Nothing

-- | Infer arguments for functions that we do not already know.
x86ArgumentAnalysis ::
  SyscallPersonality ->
  -- | Map from addresses to function name.
  Map (MemSegmentOff 64) BSC.ByteString ->
  -- | Map from address to the name at that address along with type
  Map BSC.ByteString X86FunTypeInfo ->
  DiscoveryState X86_64 ->
  ReoptM X86_64 r (Map (MemSegmentOff 64) X86FunTypeInfo)
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

  globalStepStarted FunctionArgInference

  let (dems, summaryFails) = do
        let resolveFn ::
              MemSegmentOff 64 ->
              RegState X86Reg (Value X86_64 ids) ->
              Either String [Some (Value X86_64 ids)]
            resolveFn callSite callRegs = do
              case x86CallRegs mem funNameMap knownFunTypeMap callSite callRegs of
                Left rsn -> Left (ppRegisterUseErrorReason rsn)
                Right r -> Right (callArgValues r)
        functionDemands (x86DemandInfo sysp) mem resolveFn $
          filter shouldPropagate $ exploredFunctions discState

  forM_ (Map.toList summaryFails) $ \(faddr, rsn) -> do
    case rsn of
      PLTStubNotSupported -> do
        let dnm = Map.lookup faddr funNameMap
        globalStepWarning FunctionArgInference $
          printf "%s: Unexpected PLT stub." (ppFnEntry dnm faddr)
      CallAnalysisError callSite msg -> do
        let dnm = Map.lookup faddr funNameMap
        globalStepWarning FunctionArgInference $
          printf "%s: Could not determine signature at callsite %s:\n    %s" (ppFnEntry dnm faddr) (ppSegOff callSite) msg
  globalStepFinished FunctionArgInference ()
  pure $ inferFunctionTypeFromDemands dems

-- | Analyze an elf binary to extract information.
doRecoverX86 ::
  -- | Prefix to use if we need to generate new function endpoints later.
  BSC.ByteString ->
  SyscallPersonality ->
  SymAddrMap 64 ->
  FunTypeMaps 64 ->
  DiscoveryState X86_64 ->
  ReoptM
    X86_64
    r
    ( RecoveredModule X86_64,
      MergeRelations
    )
doRecoverX86 unnamedFunPrefix sysp symAddrMap debugTypeMap discState = do
  -- Map names to known function types when we have explicit information.
  let knownFunTypeMap :: Map BS.ByteString X86FunTypeInfo
      knownFunTypeMap =
        Map.fromList
          [ (recoveredFunctionName symAddrMap unnamedFunPrefix addr, xtp)
            | (addr, rtp) <- Map.toList (addrTypeMap debugTypeMap),
              Right xtp <- [runExcept (resolveReoptFunType rtp)]
          ]
          <> Map.mapMaybe resolveX86Type (nameTypeMap debugTypeMap)

  -- Used to compute sizes of functions for overwriting purposes.
  let addrUseMap = mkFunUseMap discState

  let mem = memory discState

  -- Maps address to name of function to use.
  let funNameMap :: Map (MemSegmentOff 64) BS.ByteString
      funNameMap =
        Map.mapWithKey
          (\addr qnm -> localFunctionName unnamedFunPrefix qnm addr)
          (samAddrMap symAddrMap)
          <> Map.fromList
            [ (addr, nm)
              | Some finfo <- exploredFunctions discState,
                let addr = discoveredFunAddr finfo,
                Map.notMember addr (samAddrMap symAddrMap),
                let nm = case matchPLT finfo of
                      Just sym -> versymName sym
                      Nothing -> nosymFunctionName unnamedFunPrefix addr
            ]
  -- Infer registers each function demands.
  fDems <- x86ArgumentAnalysis sysp funNameMap knownFunTypeMap discState

  let funTypeMap :: Map BS.ByteString X86FunTypeInfo
      funTypeMap =
        knownFunTypeMap
          <> Map.fromList
            [ (recoveredFunctionName symAddrMap unnamedFunPrefix addr, tp)
              | (addr, tp) <- Map.toList fDems
            ]

  fnDefs <- fmap catMaybes $
    forM (exploredFunctions discState) $ \(Some finfo) -> do
      let faddr = discoveredFunAddr finfo
      let dnm = discoveredFunSymbol finfo
      let fnId = funId faddr dnm
      let nm = Map.findWithDefault (error "Address undefined in funNameMap") faddr funNameMap
      case Map.lookup nm funTypeMap of
        Nothing -> do
          -- TODO: Check an error has already been reported on this.
          pure Nothing
        Just X86UnsupportedFunType -> do
          -- TODO: Check an error has already been reported on this.
          pure Nothing
        Just (X86PrintfFunType _) -> do
{-
          funStepImmediateFailed
            InvariantInference
            fnId
            (ReoptVarArgFnTag, BSC.unpack nm ++ " is a printf-style vararg function and not supported.")
            -}
          pure Nothing
        Just X86OpenFunType -> do
{-
          funStepImmediateFailed
            InvariantInference
            fnId
            (ReoptVarArgFnTag, BSC.unpack nm ++ " is a open-style varrarg function and not supported.")
            -}
          pure Nothing
        Just (X86NonvarargFunType argRegs retRegs) -> do
          case checkFunction finfo of
            FunctionIncomplete _errTag -> do
{-
              funStepImmediateFailed
                InvariantInference
                fnId
                (errTag, "Incomplete discovery.")
                -}
              pure Nothing
            -- We should have filtered out PLT entries from the explored functions,
            -- so this is considered an error.
            FunctionHasPLT -> do
{-
              funStepImmediateFailed
                InvariantInference
                fnId
                (ReoptCannotRecoverFnWithPLTStubsTag, "Encountered unexpected PLT stub.")
                -}
              pure Nothing
            FunctionOK -> do
              funStepStarted InvariantInference fnId
              case x86BlockInvariants sysp mem funNameMap funTypeMap finfo retRegs of
                Left e -> do
                  funStepFailed InvariantInference fnId e
                  pure Nothing
                Right invMap -> do
                  funStepFinished InvariantInference fnId invMap
                  -- Do function recovery
                  funStepStarted Recovery fnId
                  case recoverFunction sysp mem finfo invMap nm argRegs retRegs of
                    Left e -> do
                      funStepFailed Recovery fnId e
                      pure Nothing
                    Right fn -> do
                      funStepFinished Recovery fnId ()
                      pure (Just fn)
  -- Get list of names of functions defined
  let definedNames :: Set.Set BSC.ByteString
      definedNames =
        Set.fromList $
          recoveredFunctionName symAddrMap unnamedFunPrefix . fnAddr <$> fnDefs

  -- Get all functions that are referenced, but not defined in the module.
  let declFunTypeMap :: FunctionTypeMap X86_64
      declFunTypeMap = foldl (getReferencedFunctions definedNames) Map.empty fnDefs

  let fnDecls =
        [ FunctionDecl
            { funDeclName = nm,
              funDeclType = tp
            }
          | (nm, tp) <- Map.toList declFunTypeMap
        ]
  let recMod =
        RecoveredModule
          { recoveredDecls = fnDecls,
            recoveredDefs = fnDefs
          }

  let mkObjFunDef :: Function X86_64 -> ObjFunDef
      mkObjFunDef f =
        ObjFunDef
          { ofdObjName = fnName f,
            ofdBinAddr = memWordValue (addrOffset (segoffAddr (fnAddr f))),
            ofdBinSize = lookupFunSize (fnAddr f) addrUseMap
          }

  -- Map name of declared functions to the address in binary (if any)
  let undefinedFuns :: V.Vector ObjFunRef
      undefinedFuns =
        V.fromList
          [ ObjFunRef {ofrObjName = name, ofrBinAddr = memWordValue addr}
            | (segOff, name) <- Map.toList funNameMap,
              not (Set.member name definedNames),
              let addr = addrOffset (segoffAddr segOff)
          ]

  let mergeRel =
        MergeRelations
          { mrObjectFuns = mkObjFunDef <$> V.fromList fnDefs,
            mrUndefinedFuns = undefinedFuns
          }
  seq recMod $ pure (recMod, mergeRel)

resolveX86Type :: ReoptFunType -> Maybe X86FunTypeInfo
resolveX86Type rtp =
  case runExcept (resolveReoptFunType rtp) of
    Left _ -> Nothing
    Right r -> Just r

{-
-- | Initialization checks needed if running function recovery.
--
-- These run at initialization time so we can report failures prior
-- to running discovery.  They also compute the OS
recoveryInitChecks :: ReoptM X86_64 r X86OS
recoveryInitChecks = do
  let hdr = Elf.header hdrInfo
  case Elf.headerMachine hdr of
    Elf.EM_X86_64 ->
      pure ()
    m ->
      initError $ printf "Recovery does not support %s binaries." (show m)
  os <-
    case x86OSForABI (Elf.headerOSABI hdr) of
      Just os -> pure os
      Nothing -> do
        globalStepWarning DiscoveryInitialization (warnABIUntested (Elf.headerOSABI hdr))
        pure Linux
-}

-- | Initialize function discovery for x86.

reoptX86Init ::
  -- | Option to load the binary at the given address
  LoadOptions ->
  ReoptOptions ->
  Elf.ElfHeaderInfo 64 ->
  ReoptM X86_64 r ( X86OS, InitDiscovery X86_64 )
reoptX86Init loadOpts reoptOpts hdrInfo = do
  globalStepStarted DiscoveryInitialization

  let hdr = Elf.header hdrInfo
  case Elf.headerMachine hdr of
    Elf.EM_X86_64 -> pure ()
    m ->
      reoptFatalError $ ReoptInitError $
        printf "Recovery does not support %s binaries." (show m)
  os <-
    case x86OSForABI (Elf.headerOSABI hdr) of
      Just os -> pure os
      Nothing -> do
        globalStepWarning DiscoveryInitialization $
          warnABIUntested (Elf.headerOSABI hdr)
        pure Linux
  initState <-
    reoptRunInit $ do
      let ainfo = osArchitectureInfo os
      let pltFn = processX86PLTEntries
      doInit loadOpts hdrInfo ainfo pltFn reoptOpts
  pure (os, initState)

checkSymbolUnused ::
  -- | Prefix to use if we need to generate new function endpoints later.
  BSC.ByteString ->
  -- | Symbol map constructor for binary.
  SymAddrMap (ArchAddrWidth arch) ->
  ReoptM arch r ()
checkSymbolUnused unnamedFunPrefix symAddrMap = do
  when (isUsedPrefix unnamedFunPrefix symAddrMap) $ do
    reoptFatalError $ ReoptInitError $
      printf
        "No symbol in the binary may start with the prefix %d."
        (BSC.unpack unnamedFunPrefix)

-- | Analyze an elf binary to extract information.
recoverX86Elf ::
  -- | Option to load the binary at the given address
  LoadOptions ->
  -- | Options controlling discovery
  DiscoveryOptions ->
  ReoptOptions ->
  -- | Header with hints for assisting typing.
  AnnDeclarations ->
  -- | Prefix to use if we need to generate new function endpoints later.
  BSC.ByteString ->
  Elf.ElfHeaderInfo 64 ->
  ReoptM
    X86_64
    r
    ( X86OS,
      DiscoveryState X86_64,
      RecoveredModule X86_64,
      MergeRelations
    )
recoverX86Elf loadOpts disOpt reoptOpts hdrAnn unnamedFunPrefix hdrInfo = do
  (os, initState) <- reoptX86Init loadOpts reoptOpts hdrInfo
  let symAddrMap = initDiscSymAddrMap initState
  checkSymbolUnused unnamedFunPrefix symAddrMap

  let ainfo = osArchitectureInfo os
  (debugTypeMap, discState) <- doDiscovery hdrAnn hdrInfo ainfo initState disOpt

  let sysp = osPersonality os
  (recMod, mergeRel) <- doRecoverX86 unnamedFunPrefix sysp symAddrMap debugTypeMap discState

  pure (os, discState, recMod, mergeRel)

$(pure [])

--------------------------------------------------------------------------------
-- Compile the LLVM

optimizeLLVM ::
  -- | Optimization level
  Int ->
  -- | Path to LLVM `opt` command
  FilePath ->
  -- | Contents
  Builder.Builder ->
  ExceptT Ext.Failure IO BS.ByteString
optimizeLLVM 0 _ llvmBldr = do
  pure $ BSL.toStrict $ Builder.toLazyByteString llvmBldr
optimizeLLVM optLevel optPath llvmBldr = do
  Ext.run_opt optPath optLevel $ \inHandle -> do
    Builder.hPutBuilder inHandle llvmBldr

-- | Compile a bytestring containing LLVM assembly or bitcode into an object.
--
-- This writes to standard out and throws an error.
compileLLVM ::
  -- | Optimization level
  Int ->
  -- | Path to llc
  FilePath ->
  -- | Path to llvm-mc
  FilePath ->
  -- | Architure triple to pass to LLC
  String ->
  -- | Representiation of LLVM to serialize
  BS.ByteString ->
  ExceptT Ext.Failure IO BS.ByteString
compileLLVM optLevel llcPath llvmMcPath arch llvm = do
  -- Run llvm on resulting binary
  let llcOpts =
        Ext.LLCOptions
          { Ext.llcTriple = Just arch,
            Ext.llcOptLevel = optLevel,
            Ext.llcFunctionSections = True
          }
  asm <- Ext.runLLC llcPath llcOpts llvm
  Ext.runLlvmMc llvmMcPath asm arch

writeExecutable ::
  FilePath ->
  BSL.ByteString ->
  IO ()
writeExecutable path contents = do
  let mode =
        ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode
          .|. groupReadMode
          .|. groupWriteMode
          .|. groupExecuteMode
          .|. otherReadMode
          .|. otherWriteMode
          .|. otherExecuteMode
  bracket (Posix.createFile path mode) closeFd $ \fd -> do
    h <- Posix.fdToHandle fd
    BSL.hPut h contents

--------------------------------------------------------------------------------
-- Function Recovery and Statistics

-- | This parses function argument information from a user-provided header file.
resolveHeader ::
  -- | Filepath for C header with program info (if any).
  Maybe FilePath ->
  -- | Path to clang.
  FilePath ->
  ReoptM arch r AnnDeclarations
resolveHeader mHdrPath clangPath =
  case mHdrPath of
    Nothing ->
      pure emptyAnnDeclarations
    Just p -> do
      mr <- reoptIO $ runExceptT $ Ext.runClangPreprocessor clangPath p
      bytes <-
        case mr of
          Right r -> pure r
          Left f -> do
            reoptFatalError $! ReoptUserAnnPreprocessorError f
      case parseHeader p bytes of
        Left e ->
          reoptFatalError $ ReoptTextParseError UserHeaderFileType p e
        Right r ->
          pure r

defaultLLVMGenOptions :: LLVMGenOptions
defaultLLVMGenOptions = LLVMGenOptions {mcExceptionIsUB = False}

-- | Rendered a recovered X86_64 module as LLVM bitcode
renderLLVMBitcode ::
  LLVMGenOptions ->
  LLVMConfig ->
  -- | Operating system
  X86OS ->
  -- | Recovered module
  RecoveredModule X86_64 ->
  (Builder.Builder, [Either String Ann.FunctionAnn])
renderLLVMBitcode llvmGenOpt cfg os recMod =
  -- Generate LLVM module
  let archOps = LLVM.x86LLVMArchOps (show os)
      (m, ann) = moduleForFunctions archOps llvmGenOpt recMod
      -- Render into LLVM
      out = HPJ.fullRender HPJ.PageMode 10000 1 pp mempty (ppLLVM cfg m)
   in (out, ann)
  where
    pp :: HPJ.TextDetails -> Builder.Builder -> Builder.Builder
    pp (HPJ.Chr c) b = Builder.charUtf8 c <> b
    pp (HPJ.Str s) b = Builder.stringUtf8 s <> b
    pp (HPJ.PStr s) b = Builder.stringUtf8 s <> b


---------------------------------------------------------------------------------
-- Debug info discovery



-- | Return the debug information regarding functions in the given elf file.
discoverFunDebugInfo ::
  forall arch r.
  Elf.ElfHeaderInfo (ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  ReoptM
    arch
    r
    (FunTypeMaps (RegAddrWidth (ArchReg arch)))
discoverFunDebugInfo hdrInfo ainfo = withArchConstraints ainfo $ do
  let resolveFn _symName _off = Nothing
  debugTypeMap <-
    reoptIncComp $
      resolveDebugFunTypes resolveFn funTypeMapsEmpty hdrInfo
  pure $ debugTypeMap
