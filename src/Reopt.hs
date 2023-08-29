{-# LANGUAGE OverloadedStrings #-}

module Reopt (
  -- * ReoptM Monad
  ReoptM,
  runReoptM,
  reoptRunInit,
  reoptWrite,
  reoptWriteTextual,
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
  funStepStarted,
  funStepFinished,
  funStepFailed,
  funStepAllFinished,
  Events.ReoptFatalError (..),
  Events.ReoptFileType (..),
  -- Initiization
  Reopt.Relinker.parseElfHeaderInfo64,
  isCodeSection,
  printX86SectionDisassembly,
  doInit,
  reoptX86Init,
  checkSymbolUnused,
  SomeArchitectureInfo (..),

  -- * Code discovery
  ReoptOptions (..),
  reoptDefaultDiscoveryOptions,
  defaultReoptOptions,
  InitDiscovery,
  initDiscSymAddrMap,
  doDiscovery,

  -- * Debug info discovery
  reoptHomeDir,
  reoptHomeDirEnvVar,
  debugInfoCacheFilePath,
  discoverFunDebugInfo,
  getGdbDebugInfoDirs,

  -- * Function recovery
  Reopt.TypeInference.HeaderTypes.AnnDeclarations,
  Reopt.TypeInference.HeaderTypes.emptyAnnDeclarations,
  RecoveredModule,
  recoveredDefs,
  resolveHeader,
  updateRecoveredModule,

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
  LLVMLogEvent (..),
  llvmLogEventHeader,
  llvmLogEventToStrings,
  -- Reopt.Relinker.MergeRelations,
  Reopt.Relinker.mergeObject,

  -- * X86 specific
  RecoverX86Output (..),
  doRecoverX86,
  recoverX86Elf,

  -- * Utility
  copyrightNotice,
  showPaddedHex,
  writeExecutable,

  -- * Re-exports
  Data.Macaw.Memory.ElfLoader.LoadOptions (..),
  Data.Macaw.Memory.ElfLoader.defaultLoadOptions,
  Data.Macaw.CFG.MemSegmentOff,
  Data.Macaw.CFG.segoffAddr,
  Data.Macaw.CFG.addrBase,
  Data.Macaw.CFG.addrOffset,
  Data.Macaw.CFG.memWordValue,
  Data.Macaw.X86.X86_64,
)
where

import Control.Exception (
  SomeException,
  assert,
  bracket,
  catch,
  try,
 )
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (
  Except,
  ExceptT,
  MonadError (throwError),
  MonadIO (liftIO),
  MonadTrans (lift),
  forM,
  forM_,
  runExcept,
  runExceptT,
  unless,
  when,
 )
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (
  State,
  StateT,
  execStateT,
  modify,
  runState,
 )
import Data.Bits (
  Bits (popCount, shiftR, (.&.), (.|.)),
  FiniteBits (finiteBitSize),
 )
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Dwarf qualified as Dwarf
import Data.ElfEdit qualified as Elf
import Data.Foldable (foldl', foldlM)
import Data.Macaw.Analysis.FunctionArgs (
  ArchDemandInfo (..),
  ArchTermStmtRegEffects (
    ArchTermStmtRegEffects,
    termRegDemands,
    termRegTransfers
  ),
  ComputeArchTermStmtEffects,
  DemandSet (functionResultDemands, registerDemands),
  FunctionArgAnalysisFailure (..),
  RegisterSet,
  functionDemands,
 )
import Data.Macaw.Analysis.RegisterUse (
  callArgValues,
  ppRegisterUseErrorReason,
 )
import Data.Macaw.CFG (
  ArchConstraints,
  ArchFn,
  ArchReg,
  ArchSegmentOff,
  FoldableFC,
  MemAddr (..),
  MemSegment (segmentBase),
  MemSegmentOff (segoffSegment),
  MemWidth,
  MemWord (..),
  Memory (memAddrWidth),
  RegAddrWidth,
  RegState,
  RegionIndex,
  Value,
  VersionedSymbol (versymName),
  addrWidthClass,
  asSegmentOff,
  incAddr,
  incSegmentOff,
  resolveAbsoluteAddr,
  resolveRegionOff,
  segoffAddr,
 )
import Data.Macaw.Discovery qualified as Macaw
import Data.Macaw.Memory.ElfLoader (
  LoadOptions (..),
  SymbolResolutionError (CouldNotResolveAddr, EmptySymbolName),
  defaultLoadOptions,
  loadRegionBaseOffset,
  memoryForElfSections,
  memoryForElfSegments',
 )
import Data.Macaw.Utils.IncComp (
  ContT (..),
  IncCompM,
  incCompDone,
  incCompLog,
  processIncCompLogs,
  runIncCompM,
 )
import Data.Macaw.X86 (ArchitectureInfo, X86TermStmt (..), X86_64)
import Data.Macaw.X86 qualified as X86
import Data.Macaw.X86.SyscallInfo (SyscallPersonality)
import Data.Macaw.X86.X86Reg (
  X86Reg (X86_GP, X86_ZMMReg),
  x86CalleeSavedRegs,
  x86FloatArgumentRegs,
  x86FloatResultRegs,
  x86GPPArgumentRegs,
  x86ResultRegs,
  pattern RAX,
 )
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (
  catMaybes,
  fromMaybe,
  isJust,
  isNothing,
  mapMaybe,
  maybeToList,
 )
import Data.Parameterized.Some (Some (..))
import Data.Parameterized.TraversableF (FoldableF)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64)
import Flexdis86 qualified as F
import Numeric (showHex)
import Prettyprinter qualified as PP
import Reopt.ArgResolver (
  ArgResolver,
  ArgResolverError (UnsupportedArgType, UnsupportedReturnType),
  addDoubleArg,
  addGPReg64,
  runArgResolver,
 )
import Reopt.CFG.FnRep (
  FnArchStmt,
  FnValue (FnFunctionEntryValue),
  FoldFnValue (foldFnValue),
  Function (fnAddr, fnName),
  FunctionDecl (
    FunctionDecl,
    funDeclAddr,
    funDeclName,
    funDeclNoReturn,
    funDeclType
  ),
  FunctionType,
  RecoveredModule (..),
  fnBlocks,
 )
import Reopt.CFG.FunctionCheck (
  CheckFunctionResult (
    FunctionHasPLT,
    FunctionIncomplete,
    FunctionOK
  ),
  checkFunction,
 )
import Reopt.CFG.LLVM (
  LLVMGenOptions (..),
  moduleForFunctions,
 )
import Reopt.CFG.LLVM.X86 as LLVM (x86LLVMArchOps)
import Reopt.CFG.Recovery (
  LLVMLogEvent (..),
  RecoveredFunction (llvmLogEvents, recoveredFunction),
  X86ArgInfo (ArgBV64, ArgZMM),
  X86FunTypeInfo (..),
  X86RetInfo (..),
  ZMMType (ZMM512D, ZMMDouble),
  llvmLogEventHeader,
  llvmLogEventToStrings,
  mkX86FunctionType,
  recoverFunction,
  x86BlockInvariants,
  x86CallRegs,
 )
import Reopt.Events qualified as Events
import Reopt.ExternalTools qualified as Ext
import Reopt.FunUseMap (lookupFunSize, mkFunUseMap)
import Reopt.Hints (
  FunIdent (AddrIdent, SymbolIdent),
  resolveSymName,
 )
import Reopt.PLTParser as Reopt (
  PLTInfo (..),
  ResolvedPLTEntry (..),
 )
import Reopt.Relinker (
  MergeRelations (..),
  ObjFunDef (..),
  ObjFunRef (..),
  mergeObject,
  parseElfHeaderInfo64,
 )
import Reopt.TypeInference.DebugTypes (resolveDebugFunTypes)
import Reopt.TypeInference.FunTypeMaps (
  FunTypeMaps (..),
  QualifiedSymbolName (qsnBytes, qsnGlobal),
  ReoptFunType (..),
  SymAddrMap (samAddrMap, samNameMap),
  SymAddrMapLookupError (SymAddrMapAmbiguous, SymAddrMapNotFound),
  funTypeMapsEmpty,
  getAddrSymMap,
  symAddrMapEmpty,
  symAddrMapInsert,
  symAddrMapLookup,
 )
import Reopt.TypeInference.Header (parseHeader)
import Reopt.TypeInference.HeaderTypes (
  AnnDeclarations (funDecls),
  AnnFunArg (..),
  AnnFunType (..),
  AnnType (..),
  emptyAnnDeclarations,
  ppAnnType,
 )
import Reopt.VCG.Annotations qualified as Ann
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getXdgDirectory,
  withCurrentDirectory,
 )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((<.>), (</>))
import System.IO (
  Handle,
  IOMode (..),
  hPutStrLn,
  stderr,
  withBinaryFile,
  withFile,
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Posix as Posix (
  closeFd,
  createFile,
  fdToHandle,
  fileExist,
  getEnv,
  groupExecuteMode,
  groupReadMode,
  groupWriteMode,
  otherExecuteMode,
  otherReadMode,
  otherWriteMode,
  ownerExecuteMode,
  ownerReadMode,
  ownerWriteMode,
 )
import System.Process (
  readCreateProcessWithExitCode,
  shell,
 )
import Text.LLVM qualified as L
import Text.LLVM.PP qualified as LPP
import Text.PrettyPrint.HughesPJ qualified as HPJ
import Text.Printf (printf)

import Reopt.ELFArchInfo (
  InitDiscM,
  ProcessPLTEntries,
  SomeArchitectureInfo (SomeArch),
  getElfArchInfo,
  processX86PLTEntries,
  warnABIUntested,
 )
import Reopt.TypeInference.ConstraintGen (
  ModuleConstraints,
  genModuleConstraints,
 )
import Reopt.X86 (
  X86OS (..),
  osArchitectureInfo,
  osPersonality,
  x86OSForABI,
 )

copyrightNotice :: String
copyrightNotice = "Copyright 2014-21 Galois, Inc."

-- usageMessage :: String
-- usageMessage = "For help on using reopt, run \"reopt --help\"."

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

reoptDefaultDiscoveryOptions :: Macaw.DiscoveryOptions
reoptDefaultDiscoveryOptions =
  Macaw.DiscoveryOptions
    { Macaw.exploreFunctionSymbols = False
    , Macaw.exploreCodeAddrInMem = False
    , Macaw.logAtAnalyzeFunction = True
    , Macaw.logAtAnalyzeBlock = False
    }

-- | Information from user to control which addresses to include and
-- exclude.
data ReoptOptions = ReoptOptions
  { roExtraEntryPoints :: [String]
  -- ^ Additional entry points we want Reopt to consider
  , roIncluded :: [String]
  -- ^ Symbols/addresses user wanted included
  , roExcluded :: [String]
  -- ^ Symbols/addresses user wanted exluded.
  , roVerboseMode :: !Bool
  -- ^ Should reopt be verbose in reporting?
  , roDiscoveryOptions :: !Macaw.DiscoveryOptions
  -- ^ Discovery options for Macaw.
  , roDynDepPaths :: ![FilePath]
  -- ^ Additional paths to search for dynamic dependencies.
  , roDynDepDebugPaths :: ![FilePath]
  -- ^ Additional paths to search for debug versions of dynamic dependencies.
  , roTraceUnification :: !Bool
  -- ^ Trace unification in the solver
  , roTraceConstraintOrigins :: !Bool
  -- ^ Trace the origin of constraints
  }

-- | Reopt options with no additional functions to explore or not explore.
defaultReoptOptions :: ReoptOptions
defaultReoptOptions =
  ReoptOptions
    { roExtraEntryPoints = []
    , roIncluded = []
    , roExcluded = []
    , roVerboseMode = False
    , roDiscoveryOptions = reoptDefaultDiscoveryOptions
    , roDynDepPaths = []
    , roDynDepDebugPaths = []
    , roTraceUnification = False
    , roTraceConstraintOrigins = False
    }

addKnownFn ::
  SymAddrMap w ->
  BS.ByteString ->
  Macaw.NoReturnFunStatus ->
  Map (MemSegmentOff w) Macaw.NoReturnFunStatus ->
  Map (MemSegmentOff w) Macaw.NoReturnFunStatus
addKnownFn sam nm noRet m0 =
  let s = Map.findWithDefault Set.empty nm (samNameMap sam)
   in foldl (\m a -> Map.insert a noRet m) m0 s

--------------------------------------------------------------------------------
-- InitDiscovery

-- | Information returned by `initDiscovery` below.
data InitDiscovery arch = InitDiscovery
  { initDiscSymAddrMap :: !(SymAddrMap (Macaw.ArchAddrWidth arch))
  -- ^ Map from symbols to addresses.
  , initDiscBaseCodeAddr :: !(MemAddr (Macaw.ArchAddrWidth arch))
  -- ^ Address to use as base address for program counters in
  -- Dwarf debug information.
  , initDiscoveryState :: !(Macaw.DiscoveryState arch)
  }

------------------------------------------------------------------------
-- InitDiscoveryComp

-- | Warning in the initialization code.
initWarning :: String -> InitDiscM r ()
initWarning = incCompLog

-- | Report a fatal error in an incremental computation that may fail.
initError :: String -> InitDiscM (Either String r) a
initError msg = incCompDone $ Left msg

newtype ReoptM arch r a
  = ReoptM (ReaderT (Events.ReoptLogEvent arch -> IO ()) (ContT (Either Events.ReoptFatalError r) IO) a)
  deriving (Functor, Applicative, Monad)

runReoptM ::
  (Events.ReoptLogEvent arch -> IO ()) ->
  ReoptM arch r r ->
  IO (Either Events.ReoptFatalError r)
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
      Left e -> pure $ Left e
      Right a -> c a

-- | End the reopt computation with the given return value.
reoptEndNow :: r -> ReoptM arch r a
reoptEndNow = ReoptM . ReaderT . \r _ -> ContT (\_ -> pure (Right r))

reoptFatalError :: Events.ReoptFatalError -> ReoptM arch r a
reoptFatalError e = ReoptM $ ReaderT $ \_ -> ContT $ \_ -> pure (Left e)

reoptLog :: Events.ReoptLogEvent arch -> ReoptM arch r ()
reoptLog e = ReoptM $ ReaderT $ \logger -> lift (logger e)

-- reoptEnd :: ReoptM arch r ()
-- reoptEnd = reopt

reoptWriteTextual :: Events.ReoptFileType -> FilePath -> (Handle -> IO ()) -> ReoptM arch r ()
reoptWriteTextual tp path f =
  ReoptM $ ReaderT $ \_ -> ContT $ \c -> do
    mr <- try $ withFile path WriteMode f
    case mr of
      Left e -> pure $ Left $ Events.ReoptWriteError tp path e
      Right () -> c ()

reoptWrite :: Events.ReoptFileType -> FilePath -> (Handle -> IO ()) -> ReoptM arch r ()
reoptWrite tp path f =
  ReoptM $ ReaderT $ \_ -> ContT $ \c -> do
    mr <- try $ withBinaryFile path WriteMode f
    case mr of
      Left e -> pure $ Left $ Events.ReoptWriteError tp path e
      Right () -> c ()

reoptWriteBuilder :: Events.ReoptFileType -> FilePath -> Builder.Builder -> ReoptM arch r ()
reoptWriteBuilder tp path buffer = reoptWrite tp path $ \h -> Builder.hPutBuilder h buffer

reoptWriteByteString :: Events.ReoptFileType -> FilePath -> BSL.ByteString -> ReoptM arch r ()
reoptWriteByteString tp path buffer =
  reoptWrite tp path $ \h -> BSL.hPut h buffer

reoptWriteStrictByteString :: Events.ReoptFileType -> FilePath -> BS.ByteString -> ReoptM arch r ()
reoptWriteStrictByteString tp path buffer =
  reoptWrite tp path $ \h -> BS.hPut h buffer

reoptIncComp ::
  IncCompM (Events.ReoptLogEvent arch) a a ->
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
            (logger . Events.ReoptGlobalStepWarning Events.DiscoveryInitialization)
            (runIncCompM (Right <$> m))
        case mr of
          Left e -> pure (Left (Events.ReoptInitError e))
          Right v -> c v

checkBlockError ::
  ArchConstraints arch =>
  Macaw.ParsedBlock arch ids ->
  Maybe Events.DiscoveryError
checkBlockError b = do
  let a = memWordValue $ addrOffset $ segoffAddr $ Macaw.pblockAddr b
  case Macaw.pblockTermStmt b of
    Macaw.PLTStub{} ->
      Just $!
        Events.DiscoveryError
          { Events.discErrorTag = Events.DiscoveryPLTErrorTag
          , Events.discErrorBlockAddr = a
          , Events.discErrorBlockSize = Macaw.blockSize b
          , Events.discErrorBlockInsnIndex = length (Macaw.pblockStmts b)
          , Events.discErrorMessage = "Unexpected PLT stub outside PLT"
          }
    Macaw.ParsedTranslateError msg ->
      Just $!
        Events.DiscoveryError
          { Events.discErrorTag = Events.DiscoveryTransErrorTag
          , Events.discErrorBlockAddr = a
          , Events.discErrorBlockSize = Macaw.blockSize b
          , Events.discErrorBlockInsnIndex = length (Macaw.pblockStmts b)
          , Events.discErrorMessage = PP.pretty msg
          }
    Macaw.ClassifyFailure _ reasons ->
      Just $!
        Events.DiscoveryError
          { Events.discErrorTag = Events.DiscoveryClassErrorTag
          , Events.discErrorBlockAddr = a
          , Events.discErrorBlockSize = Macaw.blockSize b
          , Events.discErrorBlockInsnIndex = length (Macaw.pblockStmts b)
          , Events.discErrorMessage =
              PP.vcat
                [ "Unclassified control flow transfer"
                , PP.indent 2 $
                    PP.vcat
                      [ "Block statements:"
                      , PP.indent 2 $ PP.vcat $ map PP.viaShow (Macaw.pblockStmts b)
                      , "Classifier failures:"
                      , PP.indent 2 $ PP.vcat $ map PP.viaShow reasons
                      ]
                ]
          }
    _ -> Nothing

-- | Prepend discovery event to list of reopt log evnts.
logDiscEventAsReoptEvents ::
  ArchConstraints arch =>
  MemWidth (Macaw.ArchAddrWidth arch) =>
  (Events.ReoptLogEvent arch -> IO ()) ->
  Macaw.AddrSymMap (Macaw.ArchAddrWidth arch) ->
  Macaw.DiscoveryEvent arch ->
  IO ()
logDiscEventAsReoptEvents logger symMap evt = do
  let mkFunId a = Events.funId a (Map.lookup a symMap)
  case evt of
    Macaw.ReportAnalyzeFunction a ->
      logger $ Events.ReoptFunStepStarted Events.Discovery (mkFunId a)
    Macaw.ReportAnalyzeFunctionDone f -> do
      let a = Macaw.discoveredFunAddr f
      let fId = mkFunId a
      case mapMaybe checkBlockError (Map.elems (f ^. Macaw.parsedBlocks)) of
        [] ->
          logger $ Events.ReoptFunStepFinished Events.Discovery fId ()
        errs ->
          logger $ Events.ReoptFunStepFailed Events.Discovery fId errs
    Macaw.ReportIdentifyFunction a tgt rsn -> do
      let msg =
            printf
              "Candidate function %s %s."
              (Events.ppFnEntry (Map.lookup tgt symMap) tgt)
              (Macaw.ppFunReason rsn)
      logger $ Events.ReoptFunStepLog Events.Discovery (mkFunId a) msg
    Macaw.ReportAnalyzeBlock fa b mReason -> do
      let msg = case mReason of
            Nothing -> printf "Examining block %s." (Events.ppSegOff b)
            Just (Macaw.SplitAt o _prior) ->
              show $
                "Re-examining block"
                  PP.<+> PP.pretty (Events.ppSegOff b)
                  PP.<+> "after it was split at offset"
                  PP.<+> PP.pretty o PP.<> "."
            Just reason -> printf "Examining block %s (%s)." (Events.ppSegOff b) (show (PP.pretty reason))
      logger $ Events.ReoptFunStepLog Events.Discovery (mkFunId fa) msg

reoptRunDiscovery ::
  ArchConstraints arch =>
  MemWidth (Macaw.ArchAddrWidth arch) =>
  Macaw.AddrSymMap (Macaw.ArchAddrWidth arch) ->
  IncCompM (Macaw.DiscoveryEvent arch) a a ->
  ReoptM arch r a
reoptRunDiscovery symMap m = ReoptM $
  ReaderT $ \logger -> ContT $ \c -> do
    processIncCompLogs (logDiscEventAsReoptEvents logger symMap) (runIncCompM m) >>= c

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

-- | Add PLT symbols
addPLTSyms ::
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
  PLTInfo w ->
  InitDiscM r (SymAddrMap w)
addPLTSyms hdr strtab symtab mem regIdx m0 p = do
  let cl = Elf.headerClass hdr
  let dta = Elf.headerData hdr
  let
    ins ::
      Elf.ElfWordType w ->
      (ResolvedPLTEntry, Elf.ElfWordType w) ->
      (SymAddrMap w -> InitDiscM r (SymAddrMap w)) ->
      (SymAddrMap w -> InitDiscM r (SymAddrMap w))
    ins o (f, _sz) cont m = do
      case resolveRegionOff mem regIdx (fromIntegral o) of
        Nothing -> do
          initWarning $ printf "Unexpected symbol offset %s." (showHex (toInteger o) "")
          cont m
        Just a -> do
          case f of
            Reopt.PLTStub idx ->
              case Elf.decodeSymtabEntry cl dta strtab symtab idx of
                Left e -> do
                  initWarning (show e)
                  cont m
                Right sym -> do
                  cont $! symAddrMapInsert sym a m
            PLTNotCallable ->
              cont m
  Map.foldrWithKey' ins pure (pltMap p) m0

shdrContents ::
  Integral (Elf.ElfWordType w) =>
  Elf.ElfHeaderInfo w ->
  Elf.Shdr nm (Elf.ElfWordType w) ->
  BS.ByteString
shdrContents hdrInfo shdr =
  let
    fileOff = Elf.shdrOff shdr
    size = Elf.shdrSize shdr
   in
    BS.take (fromIntegral size) $
      BS.drop (fromIntegral fileOff) $
        Elf.headerFileContents hdrInfo

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
      let
        symAddr :: MemAddr w
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
  let
    symEntrySize :: Int
    symEntrySize = Elf.symtabEntrySize cl
  let
    cnt :: Word32
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

reportSymbolResError :: SymbolResolutionError -> State [SymbolResolutionError] ()
reportSymbolResError e = seq e $ modify (e :)

-- | Resolve a symbol table entry in an object file.
resolveObjSymbol ::
  Elf.ElfHeaderInfo w ->
  Memory w ->
  -- | Map from section index to offset in memory of section.
  Map Word16 (MemSegmentOff w) ->
  -- | Symbol addr map
  SymAddrMap w ->
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
                          | Elf.shdrAddr shdr <= val && (val - Elf.shdrAddr shdr) < Elf.shdrSize shdr
                          , off <- toInteger (val - Elf.shdrAddr shdr)
                          , Just addr <- incSegmentOff base off -> do
                              pure $! symAddrMapInsert ste addr sam
                        _ -> do
                          reportSymbolResError $ CouldNotResolveAddr (Elf.steName ste)
                          pure sam

initDiscState ::
  -- | Initial memory
  Memory (Macaw.ArchAddrWidth arch) ->
  -- | Initial entry points
  [MemSegmentOff (Macaw.ArchAddrWidth arch)] ->
  -- | Region information
  RegionInfo ->
  -- | Symbol addr map
  SymAddrMap (Macaw.ArchAddrWidth arch) ->
  -- | Explore predicate
  (ArchSegmentOff arch -> Bool) ->
  ArchitectureInfo arch ->
  ReoptOptions ->
  Except String (Macaw.DiscoveryState arch)
initDiscState mem initPoints regInfo symAddrMap explorePred ainfo reoptOpts = do
  let resolveEntry qsn
        | ".cold" `BS.isSuffixOf` qsnBytes qsn = Nothing
        | otherwise = Just Macaw.MayReturnFun
  let noReturnEntryPoints =
        Map.mapMaybe resolveEntry (samAddrMap symAddrMap)
          & addKnownFn symAddrMap "abort" Macaw.NoReturnFun
          & addKnownFn symAddrMap "exit" Macaw.NoReturnFun
          & addKnownFn symAddrMap "_Unwind_Resume" Macaw.NoReturnFun
          & addKnownFn symAddrMap "__cxa_rethrow" Macaw.NoReturnFun
          & addKnownFn symAddrMap "__cxa_throw" Macaw.NoReturnFun
          & addKnownFn symAddrMap "__malloc_assert" Macaw.NoReturnFun
          & addKnownFn symAddrMap "__stack_chk_fail" Macaw.NoReturnFun
          & addKnownFn symAddrMap "_ZSt9terminatev" Macaw.NoReturnFun
  extraEntryPoints <- mapM (resolveSymAddr mem regInfo symAddrMap) (roExtraEntryPoints reoptOpts)
  let mayReturnEntryPoints = initPoints ++ extraEntryPoints
  let entryPoints = foldl (\m a -> Map.insert a Macaw.MayReturnFun m) noReturnEntryPoints mayReturnEntryPoints
  case (roIncluded reoptOpts, roExcluded reoptOpts) of
    ([], excludeNames) -> do
      excludeAddrs <- mapM (resolveSymAddr mem regInfo symAddrMap) excludeNames
      let s = Set.fromList excludeAddrs
      let initState =
            Macaw.emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
              & Macaw.trustedFunctionEntryPoints .~ entryPoints
              & Macaw.exploreFnPred .~ (\a -> Set.notMember a s && explorePred a)
              & Macaw.markAddrsAsFunction Macaw.InitAddr (Map.keys entryPoints)
      pure $! initState
    (includeNames, []) -> do
      includeAddrs <- mapM (resolveSymAddr mem regInfo symAddrMap) includeNames
      let s = Set.fromList includeAddrs
      let initState =
            Macaw.emptyDiscoveryState mem (getAddrSymMap symAddrMap) ainfo
              & Macaw.trustedFunctionEntryPoints .~ entryPoints
              -- NOTE (val) It looks a bit weird that we're not also checking
              -- `explorePred a` here.  Not sure that's intended, and it's
              -- definitely not documented.
              & Macaw.exploreFnPred .~ (`Set.member` s)
              & Macaw.markAddrsAsFunction Macaw.InitAddr s
      pure $! initState
    _ -> do
      throwError "Cannot both include and exclude specific addresses."

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
  let
    shdrNameMap :: Map BS.ByteString [Word16]
    shdrNameMap =
      Map.fromListWith
        (++)
        [ (Elf.shdrName s, [fromIntegral (idx - 1)])
        | idx <- [1 .. V.length shdrs]
        , let s = shdrs V.! (idx - 1)
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
  { frameMem :: !(Memory w)
  , frameRegion :: !RegionIndex
  , frameCtx :: !Dwarf.FrameContext
  -- ^ Flag to indicate if this is .eh_frame or .debug_frame.
  , frameEnd :: !Dwarf.Endianess
  -- ^ Endianess
  , frameAddr :: !Word64
  -- ^ Address frame is loaded at.
  , frameData :: !BS.ByteString
  -- ^ Bytes in frame
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
      unless (null rest) $ do
        incCompLog $ "Multiple " <> BSC.unpack nm <> " sections."
      if not (null (Map.findWithDefault [] (".rela" <> nm) shdrMap))
        then do
          incCompLog $ "Do not support relocations in " <> BSC.unpack nm <> "."
          pure entries
        else do
          let f =
                Frame
                  { frameMem = mem
                  , frameRegion = regIdx
                  , frameCtx = Dwarf.EhFrame
                  , frameEnd =
                      case Elf.headerData (Elf.header elfFile) of
                        Elf.ELFDATA2LSB -> Dwarf.LittleEndian
                        Elf.ELFDATA2MSB -> Dwarf.BigEndian
                  , frameAddr =
                      Elf.elfClassInstances (Elf.headerClass (Elf.header elfFile)) $
                        fromIntegral (Elf.shdrAddr frameSection)
                  , frameData = Elf.shdrData elfFile frameSection
                  }
          seq f $ cieEntryPoints f 0 entries

-- | Creates InitDiscovery state containing all information needed
-- to perform function discovery.
initExecDiscovery ::
  forall arch r.
  -- | Base address for loading
  MemAddr (Macaw.ArchAddrWidth arch) ->
  Elf.ElfHeaderInfo (Macaw.ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  ProcessPLTEntries (Macaw.ArchAddrWidth arch) ->
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
  mPLTRes <- pltFn hdrInfo shdrs

  -- Create symbol address map that includes PLT information if available.
  symAddrMap <-
    case mPLTRes of
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
        addPLTSyms hdr strtab symtab mem regIdx symAddrMap0 pltRes
      Nothing -> do
        pure symAddrMap0

  -- Exclude PLT bounds
  let
    addrIsNotInPLT :: ArchSegmentOff arch -> Bool
    addrIsNotInPLT =
      case mPLTRes of
        Nothing -> const True
        Just pltRes -> \a -> do
          let off = fromIntegral $ addrOffset (segoffAddr a)
          case Map.lookupLE off (pltMap pltRes) of
            Just (entryOff, (_pltFn, entrySize))
              | off - entryOff < entrySize ->
                  False
            _ -> True

  -- Maps section header names to the section headers for it.
  let
    shdrMap :: Map BS.ByteString [Elf.Shdr BS.ByteString (Elf.ElfWordType (Macaw.ArchAddrWidth arch))]
    shdrMap =
      Map.fromListWith (++) [(Elf.shdrName s, [s]) | s <- V.toList shdrs]

  let addrInRodata =
        case Map.findWithDefault [] ".rodata" shdrMap of
          [shdr] ->
            let
              sOff :: Word64
              sOff = fromIntegral $ Elf.shdrAddr shdr
              sSize :: Word64
              sSize = fromIntegral (Elf.shdrSize shdr)
             in
              \a ->
                let aOff = memWordValue $ addrOffset (segoffAddr a)
                 in sOff <= aOff && (aOff - sOff) < sSize
          _ -> const False

  let explorePred a = addrIsNotInPLT a && not (addrInRodata a)
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
  let
    regInfo :: RegionInfo
    regInfo = HasDefaultRegion (addrBase baseAddr)
  s <-
    case runExcept (initDiscState mem ehFrameAddrs regInfo symAddrMap explorePred ainfo reoptOpts) of
      Left e -> initError e
      Right r -> pure r
  -- Return discovery
  pure $!
    InitDiscovery
      { initDiscSymAddrMap = symAddrMap
      , initDiscBaseCodeAddr = baseAddr
      , initDiscoveryState = s
      }

-- | Creates InitDiscovery state containing all information needed
-- to perform function discovery.
doInit ::
  forall arch r.
  -- | Option to load the binary at the given address
  LoadOptions ->
  Elf.ElfHeaderInfo (Macaw.ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  ProcessPLTEntries (Macaw.ArchAddrWidth arch) ->
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
        Just _ -> initWarning "Ignoring load offset for object file as there is no global base address."
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
            pure (fromIntegral secIdx :: Word16)
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
      let
        regInfo :: RegionInfo
        regInfo = HasDefaultRegion regIdx
      let explorePred = const True
      s <- case runExcept (initDiscState mem (maybeToList entryAddr) regInfo symAddrMap explorePred ainfo reoptOpts) of
        Left e -> initError e
        Right r -> pure r
      -- Get initial entries and predicate for exploring
      pure $!
        InitDiscovery
          { initDiscSymAddrMap = symAddrMap
          , initDiscBaseCodeAddr = MemAddr regIdx 0
          , initDiscoveryState = s
          }
    -- Executable
    Elf.ET_EXEC -> do
      -- Get base address to use for computing section offsets.
      let
        baseAddr :: MemAddr (Macaw.ArchAddrWidth arch)
        baseAddr = MemAddr{addrBase = 0, addrOffset = fromInteger (loadRegionBaseOffset loadOpts)}
      initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts
    -- Shared library or position-independent executable.
    Elf.ET_DYN -> do
      -- Get base address to use for computing section offsets.
      let
        baseAddr :: MemAddr (Macaw.ArchAddrWidth arch)
        baseAddr =
          case loadOffset loadOpts of
            Just o -> MemAddr{addrBase = 0, addrOffset = fromIntegral o}
            Nothing -> MemAddr{addrBase = 1, addrOffset = 0}
      initExecDiscovery baseAddr hdrInfo ainfo pltFn reoptOpts
    Elf.ET_CORE -> do
      initError "Core files unsupported."
    tp -> do
      initError $ "Elf files with type " ++ show tp ++ " unsupported."

---------------------------------------------------------------------------------
-- Dynamic Dependency Handling

reoptHomeDirEnvVar :: FilePath
reoptHomeDirEnvVar = "REOPTHOME"

-- | Return the path to the debug info directory -- N.B., depends
-- on environment variable REOPTHOME.
reoptHomeDir :: IO FilePath
reoptHomeDir = do
  mStr <- getEnv "REOPTHOME"
  case mStr of
    Nothing -> getXdgDirectory XdgData ".reopt"
    Just "" -> getXdgDirectory XdgData ".reopt"
    Just path -> pure path

-- | Given a binary's name, return the path to its debug function information
--   cache (whether or not the file exists).
debugInfoCacheFilePath :: String -> IO FilePath
debugInfoCacheFilePath binaryName = do
  dir <- reoptHomeDir
  pure $ dir </> binaryName <.> "debug_info"

-- | Finds the cached debug info for a dependency, returning its file path and
-- contents if found.
findCachedDebugInfo :: String -> IO (Maybe (FilePath, String))
findCachedDebugInfo depName = do
  cFile <- debugInfoCacheFilePath depName
  doesFileExist cFile >>= \case
    True -> do
      contents <- readFile cFile
      pure $ Just (cFile, contents)
    False -> pure Nothing

-- | Return the debug information regarding functions in the given elf file.
discoverFunDebugInfo ::
  forall arch r.
  Elf.ElfHeaderInfo (Macaw.ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  ReoptM
    arch
    r
    (FunTypeMaps (RegAddrWidth (ArchReg arch)))
discoverFunDebugInfo hdrInfo ainfo = X86.withArchConstraints ainfo $ do
  let resolveFn _symName _off = Nothing
  reoptIncComp $
    resolveDebugFunTypes resolveFn funTypeMapsEmpty hdrInfo

-- | Default directories to search for dynamic dependencies in on unix platforms.
unixLibDirs :: [FilePath]
unixLibDirs =
  [ "/usr/local/lib"
  , "/usr/local/lib64"
  , "/usr/lib"
  , "/usr/lib64"
  , "/lib"
  , "/lib64"
  ]

-- | Ask gdb where it looks for debug information.
getGdbDebugInfoDirs :: Bool -> IO [FilePath]
getGdbDebugInfoDirs verbose = do
  let cmd = shell "gdb --batch --eval-command=\"show debug-file-directory\""
  let stdIn = ""
  readCreateProcessWithExitCode cmd stdIn >>= \case
    (ExitSuccess, sOut, _sErr) ->
      case dropWhile (/= '"') sOut of
        ('"' : rst) -> do
          let debugDir = takeWhile (/= '"') rst
          doesDirectoryExist debugDir >>= \case
            True -> pure [debugDir]
            False -> couldNotFindDir $ "debug directory reported by gdb does not exist: " ++ debugDir
        _ -> couldNotFindDir $ "response on stdout did not match expected pattern: " ++ sOut
    (_, _, sErr) -> couldNotFindDir sErr
 where
  couldNotFindDir msg = do
    when verbose $ do
      hPutStrLn stderr "Could not determine gdb's debug-file-directory:"
      hPutStrLn stderr $ "  " ++ msg
    pure []

-- | Populate function type information using debug information.
findGnuDebugLinkSection ::
  forall w.
  -- | Elf file for header information
  Elf.ElfHeaderInfo w ->
  Maybe String
findGnuDebugLinkSection elfInfo = do
  let
    secDataMap ::
      Map
        BSC.ByteString
        [ ( Elf.FileRange (Elf.ElfWordType w)
          , Elf.ElfSection (Elf.ElfWordType w)
          )
        ]
    secDataMap =
      Map.fromListWith
        (++)
        [ (Elf.elfSectionName sec, [(r, sec)])
        | (r, sec) <- V.toList (Elf.headerSections elfInfo)
        ]
  case Map.findWithDefault [] ".gnu_debuglink" secDataMap of
    [] -> Nothing
    (_, s) : _ ->
      -- The `.gnu_debuglink` section begins with "A filename, with any leading
      -- directory components removed, followed by a zero byte", which is the
      -- part we're interested in (we'll skip checking the remaining padding
      -- and CRC checksum...).
      Just $ BSC.unpack $ BS.takeWhile (/= 0) $ Elf.elfSectionData s

-- | Finds the debug version of a dynamic dependency and return @Just@ the path
-- and contents if possible, otherwise returns @Nothing@.
findDebugDynDep ::
  ReoptOptions ->
  String ->
  IO (Maybe (FilePath, BS.ByteString))
findDebugDynDep opts depName = do
  -- Try to find the dependency on disk.
  findDepLoc >>= \case
    Just (depLoc, depContents) -> do
      -- We found the dependency! Now:
      -- 1. Verify it is an Elf file
      -- 2. Check for a .gnu_debuglink entry
      --    and... if none exists, try `depName.debug`
      case Elf.decodeElfHeaderInfo depContents of
        Left (_, msg) -> failWithMessage ("Error reading " ++ depLoc ++ ":") msg
        Right (Elf.SomeElf hdrInfo) -> do
          let hdr = Elf.header hdrInfo
          getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr) >>= \case
            Left msg -> failWithMessage ("Error reading ELF header in " ++ depLoc ++ ":") msg
            Right (warnings, SomeArch ainfo _pltFn) -> X86.withArchConstraints ainfo $ do
              unless (null warnings) $ do
                hPutStrLn stderr $ "Warnings reading ELF header in " ++ depLoc ++ ":"
                mapM_ (hPutStrLn stderr) warnings
              case findGnuDebugLinkSection hdrInfo of
                -- If we can't find a `.gnu_debuglink` entry, assume the name is
                -- the same as the dependency with `.debug` appended.
                Nothing -> do
                  hPutStrLn stderr $ "No .gnu_debuglink section for " ++ depName
                  findDebugLoc (depName <.> "debug")
                -- If we found the name, use that (making sure it exists
                -- somewhere and returning that file path).
                Just debugName -> do
                  findDebugLoc debugName
    Nothing -> findDebugLoc (depName <.> "debug")
 where
  failWithMessage :: String -> String -> IO (Maybe (FilePath, BS.ByteString))
  failWithMessage header details = do
    hPutStrLn stderr header
    hPutStrLn stderr $ "  " ++ details
    pure Nothing
  findDepLoc :: IO (Maybe (FilePath, BS.ByteString))
  findDepLoc = go libDirs
   where
    go :: [FilePath] -> IO (Maybe (FilePath, BS.ByteString))
    go [] = pure Nothing
    go (dir : rst) = do
      let fPath = dir <.> depName
      fileExist fPath >>= \case
        True -> readFileAsByteString fPath
        False -> go rst
    libDirs :: [FilePath]
    libDirs = roDynDepPaths opts ++ unixLibDirs
  findDebugLoc :: FilePath -> IO (Maybe (FilePath, BS.ByteString))
  findDebugLoc debugName = do
    go debugDirs
   where
    go :: [FilePath] -> IO (Maybe (FilePath, BS.ByteString))
    go [] = pure Nothing
    go (debugDir : rst) = do
      let fPath = debugDir </> debugName
      fileExist fPath >>= \case
        True -> readFileAsByteString fPath
        False -> go rst
    debugDirs :: [FilePath]
    debugDirs = roDynDepDebugPaths opts
  readFileAsByteString :: FilePath -> IO (Maybe (FilePath, BS.ByteString))
  readFileAsByteString fPath = do
    catch
      ( do
          bs <- BS.readFile fPath
          pure $ Just (fPath, bs)
      )
      ( \(e :: SomeException) ->
          failWithMessage
            ("Unable to read debug file " ++ fPath ++ ":")
            (show e)
      )

-- | Add the debug information from dynamic dependencies (if available either in
--   cached format or on disk for analysis).
addDynDepDebugInfo ::
  ReoptOptions ->
  -- | Map to extend with debug info.
  Map BS.ByteString ReoptFunType ->
  -- | Dependency name as it appears in a DT_NEEDED entry in an elf file.
  BS.ByteString ->
  IO (Map BS.ByteString ReoptFunType)
addDynDepDebugInfo rDisOpt m rawDepName = do
  let depName = BSC.unpack rawDepName
  when (roVerboseMode rDisOpt) $
    hPutStrLn stderr $
      "Searching for dynamic dependency " ++ depName ++ "'s debug info..."
  findCachedDebugInfo depName >>= \case
    Just (cacheFile, contents) -> do
      case reads contents :: [(Map BS.ByteString ReoptFunType, String)] of
        [] -> do
          hPutStrLn stderr $ "Internal warning: " ++ cacheFile ++ " did not contain valid data."
          pure m
        ((m', _) : _) -> do
          pure $ m <> m'
    Nothing -> do
      findDebugDynDep rDisOpt depName >>= \case
        Nothing -> do
          when (roVerboseMode rDisOpt) $
            hPutStrLn stderr $
              "No debug info for " ++ depName ++ " found."
          pure m
        Just (fPath, fContent) ->
          case Elf.decodeElfHeaderInfo fContent of
            Left (_, msg) -> do
              hPutStrLn stderr $ "Error decoding elf header info in " ++ fPath ++ ":"
              hPutStrLn stderr $ "  " ++ msg
              pure m
            Right (Elf.SomeElf hdrInfo) -> do
              let hdr = Elf.header hdrInfo
              -- Get architecture specific information (Either String ([String], SomeArchitectureInfo w))
              getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr) >>= \case
                Left errMsg -> do
                  hPutStrLn stderr $ "Error decoding elf header info in " ++ fPath ++ ":"
                  hPutStrLn stderr $ "  " ++ errMsg
                  pure m
                Right (warnings, SomeArch ainfo _pltFn) -> do
                  unless (null warnings) $ do
                    hPutStrLn stderr $ "Warnings while computing architecture specific info for " ++ fPath ++ ":"
                    mapM_ (hPutStrLn stderr) warnings -- IO (Either Events.ReoptFatalError r) r = mFnMap
                  runReoptM Events.printLogEvent (discoverFunDebugInfo hdrInfo ainfo) >>= \case
                    Left err -> do
                      hPutStrLn stderr $ "Error decoding elf header info in " ++ fPath ++ ":"
                      hPutStrLn stderr $ "  " ++ show err
                      pure m
                    Right fnMaps -> do
                      let addrTypeMapSz = Map.size $ addrTypeMap fnMaps
                      let noreturnMapSz = Map.size $ noreturnMap fnMaps
                      let fnMap = nameTypeMap fnMaps
                      unless (addrTypeMapSz == 0) $ do
                        hPutStrLn stderr $ "WARNING: " ++ show addrTypeMapSz ++ " functions in debug info ignored (addrTypeMap) in " ++ fPath ++ "."
                      unless (noreturnMapSz == 0) $ do
                        hPutStrLn stderr $ "WARNING: " ++ show noreturnMapSz ++ " functions in debug info ignored (noreturnMap) in " ++ fPath ++ "."
                      cPath <- debugInfoCacheFilePath depName
                      writeFile cPath (show fnMap)
                      pure $ fnMap <> m

-- | Get values of DT_NEEDED entries in an ELF file.
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
                  Left "Could not construct virtual address map from bytestring and list of program headers in."
                Just phdrs ->
                  case Elf.dynNeeded dynSection phdrs of
                    Left errMsg -> Left $ "Could not parse phdrs from Elf file: " ++ errMsg
                    Right deps -> Right deps
    [] -> Left "No PT_DYNAMIC section."
    _ -> Left "Multiple PT_DYNAMIC sections."
 where
  ehdr = Elf.header elf

-- | Identifies the ELF file's dynamic dependencies and searches
-- for their debug versions to glean function type annotations.
findDynamicDependencyDebugInfo ::
  Elf.ElfHeaderInfo w ->
  ReoptOptions ->
  IO (Map BS.ByteString ReoptFunType)
findDynamicDependencyDebugInfo hdrInfo rDisOpt = do
  infoDir <- reoptHomeDir
  createDirectoryIfMissing True infoDir
  case parseDynamicNeeded hdrInfo of
    Left errMsg -> do
      hPutStrLn stderr $ "Error retrieving dynamic dependencies: " ++ errMsg
      pure Map.empty
    Right dynDeps ->
      foldlM (addDynDepDebugInfo rDisOpt) Map.empty dynDeps

---------------------------------------------------------------------------------
-- Logging

globalStepStarted :: Events.ReoptGlobalStep arch a -> ReoptM arch r ()
globalStepStarted s = reoptLog (Events.ReoptGlobalStepStarted s)

globalStepFinished :: Events.ReoptGlobalStep arch a -> a -> ReoptM arch r ()
globalStepFinished s a = reoptLog (Events.ReoptGlobalStepFinished s a)

globalStepWarning :: Events.ReoptGlobalStep arch a -> String -> ReoptM arch r ()
globalStepWarning s m = reoptLog (Events.ReoptGlobalStepWarning s m)

funStepStarted :: Events.ReoptFunStep arch r e a -> Events.FunId -> ReoptM arch z ()
funStepStarted s f = reoptLog (Events.ReoptFunStepStarted s f)

funStepFailed :: Events.ReoptFunStep arch r e a -> Events.FunId -> e -> ReoptM arch z ()
funStepFailed s f e = reoptLog (Events.ReoptFunStepFailed s f e)

funStepFinished :: Events.ReoptFunStep arch r e a -> Events.FunId -> r -> ReoptM arch z ()
funStepFinished s f r = reoptLog (Events.ReoptFunStepFinished s f r)

funStepAllFinished :: Events.ReoptFunStep arch r e a -> a -> ReoptM arch z ()
funStepAllFinished f a = reoptLog (Events.ReoptFunStepAllFinished f a)

---------------------------------------------------------------------------------
-- Initial type inference

headerTypeMap ::
  forall arch r.
  MemWidth (Macaw.ArchAddrWidth arch) =>
  -- | Header with hints for assisting typing.
  AnnDeclarations ->
  -- | Function information gleaned from dynamic dependencies.
  Map BS.ByteString ReoptFunType ->
  SymAddrMap (Macaw.ArchAddrWidth arch) ->
  Map (ArchSegmentOff arch) Macaw.NoReturnFunStatus ->
  ReoptM arch r (FunTypeMaps (Macaw.ArchAddrWidth arch))
headerTypeMap hdrAnn dynDepsTypeMap symAddrMap noretMap = do
  globalStepStarted Events.HeaderTypeInference

  let voidPtrType = PtrAnnType VoidAnnType
  let charPtrType = PtrAnnType (IAnnType 8)
  let sizetType = IAnnType 64
  let offType = sizetType
  let intType = IAnnType 32
  let ftype res args = ReoptNonvarargFunType (AnnFunType{funRet = res, funArgs = V.fromList args})
  let declFn = Map.singleton
  let nmArg nm = AnnFunArg (Just nm)
  let nonmArg = AnnFunArg Nothing
  -- Generate type information from annotations
  let nameAnnTypeMap =
        fmap ReoptNonvarargFunType (funDecls hdrAnn)
          <> dynDepsTypeMap
          <> declFn "__errno_location" (ftype (PtrAnnType (IAnnType 32)) [])
          <> declFn "__fstat" (ftype intType (fmap nonmArg [intType, voidPtrType]))
          <> declFn "__isoc99_sscanf" (ReoptPrintfFunType 1)
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
          <> declFn "syslog" (ReoptPrintfFunType 1)

  -- Generate map from address names to known type.
  --
  -- This is used when we see a function jumps to a defined address.
  addrAnnTypeMap <- do
    let
      insSymType ::
        Map (ArchSegmentOff arch) ReoptFunType ->
        (BS.ByteString, ReoptFunType) ->
        ReoptM arch r (Map (ArchSegmentOff arch) ReoptFunType)
      insSymType m (sym, annTp) = do
        case symAddrMapLookup symAddrMap sym of
          Left SymAddrMapNotFound -> do
            -- Silently drop symbols without addresses as they may be undefined.
            pure m
          Left SymAddrMapAmbiguous -> do
            globalStepWarning Events.HeaderTypeInference $
              "Ambiguous symbol " ++ BSC.unpack sym ++ "."
            pure m
          Right addr -> do
            pure $! Map.insert addr annTp m
    foldlM insSymType Map.empty (Map.toList nameAnnTypeMap)

  let
    annTypeMap :: FunTypeMaps (Macaw.ArchAddrWidth arch)
    annTypeMap =
      FunTypeMaps
        { nameToAddrMap = symAddrMap
        , nameTypeMap = nameAnnTypeMap
        , addrTypeMap = addrAnnTypeMap
        , noreturnMap = noretMap
        }
  globalStepFinished Events.HeaderTypeInference ()
  pure annTypeMap

---------------------------------------------------------------------------------
-- Complete discovery

doDiscovery ::
  forall arch r.
  ArchConstraints arch =>
  -- | Header with hints for assisting typing.
  AnnDeclarations ->
  Elf.ElfHeaderInfo (Macaw.ArchAddrWidth arch) ->
  ArchitectureInfo arch ->
  InitDiscovery arch ->
  ReoptOptions ->
  ReoptM
    arch
    r
    ( FunTypeMaps (Macaw.ArchAddrWidth arch)
    , Macaw.DiscoveryState arch
    )
doDiscovery hdrAnn hdrInfo ainfo initState rDisOpt = X86.withArchConstraints ainfo $ do
  let s = initDiscoveryState initState
  let mem = Macaw.memory s
  let symAddrMap = initDiscSymAddrMap initState

  -- Mark initialization as finished.
  globalStepFinished Events.DiscoveryInitialization s

  dynDepsMap <-
    reoptIO $
      findDynamicDependencyDebugInfo hdrInfo rDisOpt

  let baseAddr = initDiscBaseCodeAddr initState
  annTypeMap <- headerTypeMap hdrAnn dynDepsMap symAddrMap (s ^. Macaw.trustedFunctionEntryPoints)

  -- Resolve debug information.
  let resolveFn _symName off =
        let addr = incAddr (toInteger off) baseAddr
         in asSegmentOff mem addr

  debugTypeMap <-
    reoptIncComp $
      resolveDebugFunTypes resolveFn annTypeMap hdrInfo
  let postDebugState = s & Macaw.trustedFunctionEntryPoints .~ noreturnMap debugTypeMap

  let symMap = getAddrSymMap symAddrMap
  discState <-
    reoptRunDiscovery symMap $
      Macaw.incCompleteDiscovery postDebugState (roDiscoveryOptions rDisOpt)
  funStepAllFinished Events.Discovery discState
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
trimForWord64Buffer base n = drop d
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
    printX86DisassemblyLine h base buffer $ F.DAddr (i + 7) (n - 7) Nothing

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
    { termRegDemands = []
    , termRegTransfers = []
    }
summarizeX86TermStmt _ UD2 _ =
  ArchTermStmtRegEffects
    { termRegDemands = []
    , termRegTransfers = []
    }

x86DemandInfo ::
  SyscallPersonality ->
  ArchDemandInfo X86_64
x86DemandInfo sysp =
  ArchDemandInfo
    { functionArgRegs =
        [Some RAX]
          ++ (Some . X86_GP <$> x86GPPArgumentRegs)
          ++ (Some <$> x86FloatArgumentRegs)
    , functionRetRegs = (Some <$> x86ResultRegs) ++ (Some <$> x86FloatResultRegs)
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
    { LPP.cfgLoadImplicitType = True
    , LPP.cfgGEPImplicitType = True
    , LPP.cfgUseDILocation = False
    }

-- | Configuration for LLVM 3.7 & 3.8
latestLLVMConfig :: LLVMConfig
latestLLVMConfig =
  LPP.Config
    { LPP.cfgLoadImplicitType = False
    , LPP.cfgGEPImplicitType = False
    , LPP.cfgUseDILocation = True
    }

llvmVersionMap :: Map LLVMVersion LLVMConfig
llvmVersionMap =
  Map.fromList
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
getReferencedFunctions ::
  forall arch.
  ( Eq (FunctionType arch)
  , Show (FunctionType arch)
  , MemWidth (Macaw.ArchAddrWidth arch)
  , FoldableFC (ArchFn arch)
  , FoldableF (FnArchStmt arch)
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
                  BSC.unpack nm
                    ++ " has incompatible types:\n"
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

-- | Computes a name for a recovered function from its qualified symbol name.
-- When the name is global, we just use it.  When the name is only unique in a
-- compilation unit, we add the prefix, segment index, and segment address, to
-- make the name unique.
localFunctionName :: BSC.ByteString -> MemSegmentOff w -> QualifiedSymbolName -> BSC.ByteString
localFunctionName prefix segOff qnm =
  if qsnGlobal qnm
    then qsnBytes qnm
    else
      let addr = segoffAddr segOff
       in prefix
            <> "_"
            <> qsnBytes qnm
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
  -- | Prefix to use for automatically generated symbols.  To be able to
  -- distinguish symbols, this should not be a prefix for any of the symbols in
  -- the map.
  BSC.ByteString ->
  -- | Address of the function for which we're looking up a name
  MemSegmentOff w ->
  BSC.ByteString
recoveredFunctionName m prefix segOff =
  case Map.lookup segOff (samAddrMap m) of
    Just qname -> localFunctionName prefix segOff qname
    Nothing -> nosymFunctionName prefix segOff

-- | Construct function type from demands.
inferFunctionTypeFromDemands ::
  Map (MemSegmentOff 64) (DemandSet X86Reg) ->
  Map (MemSegmentOff 64) X86FunTypeInfo
inferFunctionTypeFromDemands dm =
  let
    go ::
      DemandSet X86Reg ->
      Map (MemSegmentOff 64) (RegisterSet X86Reg) ->
      Map (MemSegmentOff 64) (RegisterSet X86Reg)
    go ds = Map.unionWith Set.union (functionResultDemands ds)

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
      let
        args =
          fmap ArgBV64 (dropArgSuffix X86_GP x86GPPArgumentRegs argSet)
            ++ fmap (ArgZMM ZMM512D) (dropArgSuffix X86_ZMMReg [0 .. 7] argSet)
        rets =
          fmap (Some . RetBV64) (dropArgSuffix X86_GP [F.RAX, F.RDX] retSet)
            ++ fmap (Some . RetZMM ZMM512D) (dropArgSuffix X86_ZMMReg [0, 1] retSet)
       in
        X86NonvarargFunType args rets
   in
    orderPadArgs
      <$> Map.mergeWithKey
        (\_ ds rets -> Just (registerDemands ds, rets))
        (fmap (\ds -> (registerDemands ds, mempty)))
        (fmap (mempty,))
        dm
        retDemands

resolveReoptFunType ::
  Monad m =>
  ReoptFunType ->
  ExceptT ArgResolverError m X86FunTypeInfo
resolveReoptFunType (ReoptNonvarargFunType ftp) =
  resolveAnnFunType ftp
resolveReoptFunType (ReoptPrintfFunType i) =
  pure $! X86PrintfFunType i
resolveReoptFunType ReoptOpenFunType =
  pure X86OpenFunType

--------------------------------------------------------------------------------
-- recoverX86Elf

matchPLT :: Macaw.DiscoveryFunInfo arch ids -> Maybe VersionedSymbol
matchPLT finfo
  | [b] <- Map.elems (finfo ^. Macaw.parsedBlocks)
  , Macaw.PLTStub _ _ sym <- Macaw.pblockTermStmt b =
      Just sym
matchPLT _ = Nothing

-- | Infer arguments for functions that we do not already know.  Returns a pair
-- of the successful arguments on the left, and the analysis failures on the
-- right.
x86ArgumentAnalysis ::
  SyscallPersonality ->
  -- | Map from addresses to function name.
  (MemSegmentOff 64 -> Maybe BSC.ByteString) ->
  -- | Map from address to the name at that address along with type
  (BSC.ByteString -> Maybe X86FunTypeInfo) ->
  Macaw.DiscoveryState X86_64 ->
  ReoptM
    X86_64
    r
    ( Map (MemSegmentOff 64) X86FunTypeInfo
    , Map (MemSegmentOff 64) (FunctionArgAnalysisFailure 64)
    )
x86ArgumentAnalysis sysp resolveFunName resolveFunType discState = do
  -- Generate map from symbol names to known type.
  let mem = Macaw.memory discState
  -- Compute only those functions whose types are not known.
  let
    known :: Macaw.DiscoveryFunInfo X86_64 ids -> Bool
    known f =
      case resolveFunName (Macaw.discoveredFunAddr f) of
        Just nm -> isJust (resolveFunType nm)
        Nothing -> False
  let shouldPropagate (Some f) = not (known f) && isNothing (matchPLT f)

  globalStepStarted Events.FunctionArgInference

  let (dems, summaryFails) = do
        let
          resolveFn ::
            MemSegmentOff 64 ->
            RegState X86Reg (Value X86_64 ids) ->
            Either String [Some (Value X86_64 ids)]
          resolveFn callSite callRegs = do
            case x86CallRegs mem resolveFunName resolveFunType callSite callRegs of
              Left rsn -> Left (ppRegisterUseErrorReason rsn)
              Right r -> Right (callArgValues r)
        functionDemands (x86DemandInfo sysp) mem resolveFn $
          filter shouldPropagate $
            Macaw.exploredFunctions discState

  forM_ (Map.toList summaryFails) $ \(faddr, rsn) -> do
    case rsn of
      PLTStubNotSupported -> do
        let dnm = resolveFunName faddr
        globalStepWarning Events.FunctionArgInference $
          printf "%s: Unexpected PLT stub." (Events.ppFnEntry dnm faddr)
      CallAnalysisError callSite msg -> do
        let dnm = resolveFunName faddr
        globalStepWarning Events.FunctionArgInference $
          printf "%s: Could not determine signature at callsite %s:\n    %s" (Events.ppFnEntry dnm faddr) (Events.ppSegOff callSite) msg
  globalStepFinished Events.FunctionArgInference ()

  pure (inferFunctionTypeFromDemands dems, summaryFails)

data RecoverX86Output = RecoverX86Output
  { recoveredModule :: RecoveredModule X86_64
  , mergeRelations :: MergeRelations
  , logEvents :: [LLVMLogEvent]
  , summaryFailures :: Map (MemSegmentOff 64) (FunctionArgAnalysisFailure 64)
  }

-- | Analyze an elf binary to extract information.
doRecoverX86 ::
  -- | Prefix to use if we need to generate new function endpoints later.
  BSC.ByteString ->
  SyscallPersonality ->
  SymAddrMap 64 ->
  FunTypeMaps 64 ->
  Macaw.DiscoveryState X86_64 ->
  ReoptM X86_64 r RecoverX86Output
doRecoverX86 unnamedFunPrefix sysp symAddrMap debugTypeMap discState = do
  -- Map names to known function types when we have explicit information.
  let
    knownFunTypeMap :: Map BS.ByteString (MemSegmentOff 64, X86FunTypeInfo)
    knownFunTypeMap =
      Map.fromList
        [ (recoveredFunctionName symAddrMap unnamedFunPrefix addr, (addr, xtp))
        | (addr, rtp) <- Map.toList (addrTypeMap debugTypeMap)
        , Right xtp <- [runExcept (resolveReoptFunType rtp)]
        ]
        <> Map.mapMaybeWithKey (resolveX86Type symAddrMap) (nameTypeMap debugTypeMap)

  -- Used to compute sizes of functions for overwriting purposes.
  let addrUseMap = mkFunUseMap discState

  let mem = Macaw.memory discState

  -- Maps address to name of function to use.
  let
    funNameMap :: Map (MemSegmentOff 64) BS.ByteString
    funNameMap =
      Map.mapWithKey
        (localFunctionName unnamedFunPrefix)
        (samAddrMap symAddrMap)
        <> Map.fromList
          [ (addr, nm)
          | Some finfo <- Macaw.exploredFunctions discState
          , let addr = Macaw.discoveredFunAddr finfo
          , Map.notMember addr (samAddrMap symAddrMap)
          , let nm = case matchPLT finfo of
                  Just sym -> versymName sym
                  Nothing -> nosymFunctionName unnamedFunPrefix addr
          ]

  -- Infer registers each function demands.
  (fDems, summaryFailures) <- do
    let resolveFunName a = Map.lookup a funNameMap
    let resolveFunType fnm = snd <$> Map.lookup fnm knownFunTypeMap
    x86ArgumentAnalysis sysp resolveFunName resolveFunType discState

  let
    funTypeMap :: Map BS.ByteString (MemSegmentOff 64, X86FunTypeInfo)
    funTypeMap =
      knownFunTypeMap
        <> Map.fromList
          [ (nm, (faddr, tp))
          | Some finfo <- Macaw.exploredFunctions discState
          , let faddr = Macaw.discoveredFunAddr finfo
          , let nm = Map.findWithDefault (error "Address undefined in funNameMap") faddr funNameMap
          , tp <- maybeToList $ Map.lookup faddr fDems
          ]

  fnDefsAndLogEvents <- fmap catMaybes $
    forM (Macaw.exploredFunctions discState) $ \(Some finfo) -> do
      let faddr = Macaw.discoveredFunAddr finfo
      let dnm = Macaw.discoveredFunSymbol finfo
      let fnId = Events.funId faddr dnm
      let nm = Map.findWithDefault (error "Address undefined in funNameMap") faddr funNameMap
      case snd <$> Map.lookup nm funTypeMap of
        Nothing -> do
          -- TODO: Check an error has already been reported on this.
          pure Nothing
        Just (X86PrintfFunType _) -> do
          -- Var-args cannot be recovered.
          pure Nothing
        Just X86OpenFunType -> do
          -- open cannot be recovered.
          pure Nothing
        Just (X86NonvarargFunType argRegs retRegs) -> do
          case checkFunction finfo of
            FunctionIncomplete _errTag -> do
              pure Nothing
            FunctionHasPLT -> do
              pure Nothing
            FunctionOK -> do
              funStepStarted Events.InvariantInference fnId
              let resolveFunName a = Map.lookup a funNameMap
              let resolveFunType fnm = snd <$> Map.lookup fnm funTypeMap
              case x86BlockInvariants sysp mem resolveFunName resolveFunType finfo retRegs of
                Left e -> do
                  funStepFailed Events.InvariantInference fnId e
                  pure Nothing
                Right invMap -> do
                  funStepFinished Events.InvariantInference fnId invMap
                  -- Do function recovery
                  funStepStarted Events.Recovery fnId
                  case recoverFunction sysp mem finfo invMap nm argRegs retRegs of
                    Left e -> do
                      funStepFailed Events.Recovery fnId e
                      pure Nothing
                    Right fn -> do
                      funStepFinished Events.Recovery fnId ()
                      pure (Just fn)
  let
    fnDefs = map recoveredFunction fnDefsAndLogEvents
    logEvents = concatMap llvmLogEvents fnDefsAndLogEvents
  -- Get list of names of functions defined
  let
    definedNames :: Set.Set BSC.ByteString
    definedNames =
      Set.fromList $
        recoveredFunctionName symAddrMap unnamedFunPrefix . fnAddr <$> fnDefs

  -- Get all functions that are referenced, but not defined in the module.
  let
    declFunTypeMap :: FunctionTypeMap X86_64
    declFunTypeMap = foldl (getReferencedFunctions definedNames) Map.empty fnDefs

  let fnDecls =
        [ FunctionDecl
          { funDeclAddr = addr
          , funDeclName = nm
          , funDeclType = mkX86FunctionType tp
          , funDeclNoReturn =
              case noRet of
                Macaw.MayReturnFun -> False
                Macaw.NoReturnFun -> True
          }
        | (nm, _) <- Map.toList declFunTypeMap
        , (addr, tp) <- maybeToList $ Map.lookup nm funTypeMap
        , let noRet = Map.findWithDefault Macaw.MayReturnFun addr (discState ^. Macaw.trustedFunctionEntryPoints)
        ]

  let recoveredModule =
        RecoveredModule
          { recoveredDecls = fnDecls
          , recoveredDefs = fnDefs
          }

  let
    mkObjFunDef :: Function X86_64 -> ObjFunDef
    mkObjFunDef f =
      ObjFunDef
        { ofdObjName = fnName f
        , ofdBinAddr = memWordValue (addrOffset (segoffAddr (fnAddr f)))
        , ofdBinSize = lookupFunSize (fnAddr f) addrUseMap
        }

  -- Map name of declared functions to the address in binary (if any)
  let
    undefinedFuns :: V.Vector ObjFunRef
    undefinedFuns =
      V.fromList
        [ ObjFunRef{ofrObjName = name, ofrBinAddr = memWordValue addr}
        | (segOff, name) <- Map.toList funNameMap
        , not (Set.member name definedNames)
        , let addr = addrOffset (segoffAddr segOff)
        ]

  let mergeRelations =
        MergeRelations
          { mrObjectFuns = mkObjFunDef <$> V.fromList fnDefs
          , mrUndefinedFuns = undefinedFuns
          }

  seq recoveredModule $
    pure $
      RecoverX86Output
        { recoveredModule
        , mergeRelations
        , logEvents
        , summaryFailures
        }

resolveX86Type ::
  SymAddrMap 64 ->
  BS.ByteString ->
  ReoptFunType ->
  Maybe (MemSegmentOff 64, X86FunTypeInfo)
resolveX86Type m nm rtp
  | Right r <- runExcept (resolveReoptFunType rtp)
  , [addr] <- Set.toList (Map.findWithDefault Set.empty nm (samNameMap m)) =
      Just (addr, r)
  | otherwise =
      Nothing

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
  ReoptM X86_64 r (X86OS, InitDiscovery X86_64)
reoptX86Init loadOpts reoptOpts hdrInfo = do
  globalStepStarted Events.DiscoveryInitialization

  let hdr = Elf.header hdrInfo
  case Elf.headerMachine hdr of
    Elf.EM_X86_64 -> pure ()
    m ->
      reoptFatalError $
        Events.ReoptInitError $
          printf "Recovery does not support %s binaries." (show m)
  os <-
    case x86OSForABI (Elf.headerOSABI hdr) of
      Just os -> pure os
      Nothing -> do
        globalStepWarning Events.DiscoveryInitialization $
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
  SymAddrMap (Macaw.ArchAddrWidth arch) ->
  ReoptM arch r ()
checkSymbolUnused unnamedFunPrefix symAddrMap = do
  when (isUsedPrefix unnamedFunPrefix symAddrMap) $ do
    reoptFatalError $
      Events.ReoptInitError $
        printf
          "No symbol in the binary may start with the prefix %d."
          (BSC.unpack unnamedFunPrefix)

-- | Analyze an elf binary to extract information.
recoverX86Elf ::
  -- | Option to load the binary at the given address
  LoadOptions ->
  ReoptOptions ->
  -- | Header with hints for assisting typing.
  AnnDeclarations ->
  -- | Prefix to use if we need to generate new function endpoints later.
  BSC.ByteString ->
  Elf.ElfHeaderInfo 64 ->
  ReoptM
    X86_64
    r
    ( X86OS
    , Macaw.DiscoveryState X86_64
    , RecoverX86Output
    , ModuleConstraints X86_64
    )
recoverX86Elf loadOpts reoptOpts hdrAnn unnamedFunPrefix hdrInfo = do
  (os, initState) <- reoptX86Init loadOpts reoptOpts hdrInfo
  let symAddrMap = initDiscSymAddrMap initState
  checkSymbolUnused unnamedFunPrefix symAddrMap

  let ainfo = osArchitectureInfo os
  (debugTypeMap, discState) <-
    doDiscovery hdrAnn hdrInfo ainfo initState reoptOpts

  let sysp = osPersonality os
  recoverX86Output <-
    doRecoverX86 unnamedFunPrefix sysp symAddrMap debugTypeMap discState

  let recMod = recoveredModule recoverX86Output
  let constraints =
        genModuleConstraints
          recMod
          (Macaw.memory discState)
          (roTraceUnification reoptOpts)
          (roTraceConstraintOrigins reoptOpts)

  pure (os, discState, recoverX86Output, constraints)

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
  Ext.runOpt optPath optLevel $ \inHandle -> do
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
          { Ext.llcTriple = Just arch
          , Ext.llcOptLevel = optLevel
          , Ext.llcFunctionSections = True
          }
  asm <- Ext.runLLC llcPath llcOpts llvm
  Ext.runLlvmMc llvmMcPath asm arch

writeExecutable ::
  FilePath ->
  BSL.ByteString ->
  IO ()
writeExecutable path contents = do
  let mode =
        ownerReadMode
          .|. ownerWriteMode
          .|. ownerExecuteMode
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
            reoptFatalError $! Events.ReoptUserAnnPreprocessorError f
      case parseHeader p bytes of
        Left e ->
          reoptFatalError $ Events.ReoptTextParseError Events.UserHeaderFileType p e
        Right r ->
          pure r

defaultLLVMGenOptions :: LLVMGenOptions
defaultLLVMGenOptions = LLVMGenOptions{mcExceptionIsUB = False}

-- | Rendered a recovered X86_64 module as LLVM bitcode
renderLLVMBitcode ::
  LLVMGenOptions ->
  LLVMConfig ->
  -- | Operating system
  X86OS ->
  -- | Recovered module
  RecoveredModule X86_64 ->
  ModuleConstraints X86_64 ->
  ( Builder.Builder
  , [(Events.FunId, Either String Ann.FunctionAnn)]
  , [Ann.ExternalFunctionAnn]
  , [LLVMLogEvent]
  )
renderLLVMBitcode llvmGenOpt cfg os recMod constraints =
  -- Generate LLVM module
  let
    archOps = LLVM.x86LLVMArchOps (show os)
    (m, ann, ext, logEvents) = moduleForFunctions archOps llvmGenOpt recMod constraints
    -- Render into LLVM
    out = HPJ.fullRender HPJ.PageMode 10000 1 pp mempty (ppLLVM cfg m)
   in
    (out, ann, ext, logEvents)
 where
  pp :: HPJ.TextDetails -> Builder.Builder -> Builder.Builder
  pp (HPJ.Chr c) b = Builder.charUtf8 c <> b
  pp (HPJ.Str s) b = Builder.stringUtf8 s <> b
  pp (HPJ.PStr s) b = Builder.stringUtf8 s <> b

updateRecoveredModule ::
  forall arch. ModuleConstraints arch -> RecoveredModule arch -> RecoveredModule arch
updateRecoveredModule _ rm = rm

--   { recoveredDecls = updateFunctionDecl <$> recoveredDecls rm
--   , recoveredDefs = updateFunction <$> recoveredDefs rm
--   }
-- where

--   updateFunctionDecl :: FunctionDecl arch -> FunctionDecl arch
--   updateFunctionDecl fd =
--     let funType = mcFunTypes constraints Map.! funDeclAddr fd in
--     trace "Updating a FunctionDecl" $
--     fd { funDeclType = updateFunctionType funType (funDeclType fd)
--        }

--   updateFunctionType :: FunType arch -> FunctionType arch -> FunctionType arch
--   updateFunctionType fty f =
--     f { fnArgTypes = updateFnArgType <$> zip (fnArgTypes f) (funTypeArgs fty)
--       }

--   updateFnArgType :: (Some TypeRepr, TyVar) -> Some TypeRepr
--   updateFnArgType (ty, tyv) =
--     let found = mcTypeMap constraints Map.! tyv in
--     trace (show tyv) $
--     trace (show found) $
--       case found of
--         TopTy -> ty
--         PtrTy _ _ -> ty
--         _ -> ty

--   updateFunction :: Function arch -> Function arch
--   updateFunction f =
--     let funType = mcFunTypes constraints Map.! fnAddr f in
--     f { fnType = updateFunctionType funType (fnType f)
--       }
