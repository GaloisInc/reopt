-- | Datatype for recording events in Reopt.
module Reopt.Events (
  ReoptLogEvent (..),
  ReoptErrorTag (..),
  ReoptGlobalStep (..),
  ReoptFunStep (..),
  ReoptStepTag (..),
  mkReoptErrorTag,
  DiscoveryError (..),
  DiscoveryErrorTag (..),
  RecoverError (..),
  FunId (..),
  funId,
  printLogEvent,

  -- * Fatal errors
  ReoptFatalError (..),
  ReoptFileType (..),

  -- * Summary
  ReoptSummary (..),
  FnRecoveryResult (..),
  initReoptSummary,

  -- * Statistics
  ReoptStats (..),
  renderAllFailures,
  incStepError,
  ppStats,
  summaryHeader,
  summaryRows,
  ppFnEntry,
  segoffWord64,
  ppSegOff,
  recoverLogEvent,

  -- * Exported errors (for VSCode plugin)
  ReoptExportedError (..),
  ReoptExportedErrorLocation (..),

  -- * Utilities,
  Data.Void.Void,
  groupDigits,
  outputRow,
  ppIndent,
  joinLogEvents,
)
where

import Control.Exception (IOException)
import Control.Lens ((^.))
import Data.ByteString.Char8 qualified as BS
import Data.IORef (IORef, modifyIORef')
import Data.List (foldl', intercalate, unfoldr)
import Data.Macaw.Analysis.RegisterUse (BlockInvariantMap, RegisterUseError (..), RegisterUseErrorReason (..), RegisterUseErrorTag (..), ppRegisterUseErrorReason)
import Data.Macaw.Discovery (ArchAddrWidth, DiscoveryState, memory, unexploredFunctions)
import Data.Macaw.Memory (
  MemAddr (addrOffset),
  MemSegmentOff,
  MemWord (memWordValue),
  addrWidthClass,
  memAddrWidth,
  memSegments,
  segmentSize,
  segoffAddr,
 )
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Parameterized.Some (Some (..))
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)
import Numeric (showHex)
import Prettyprinter qualified as PP
import Prettyprinter.Render.String (renderString)
import Reopt.ExternalTools qualified as Ext
import Reopt.FunUseMap (mkFunUseMap, totalFunUseSize)
import Reopt.Utils.Folds (sumBy)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- | Function identifier and name.
data FunId = FunId
  { funIdAddr :: !Word64
  , funIdName :: !BS.ByteString
  }
  deriving (Eq, Ord)

segoffWord64 :: MemSegmentOff w -> Word64
segoffWord64 = memWordValue . addrOffset . segoffAddr

funId :: MemSegmentOff w -> Maybe BS.ByteString -> FunId
funId a mnm =
  FunId
    { funIdAddr = segoffWord64 a
    , funIdName = fromMaybe BS.empty mnm
    }

ppFunId :: FunId -> String
ppFunId (FunId a nm)
  | BS.null nm = "0x" <> showHex a ""
  | otherwise = BS.unpack nm <> "(0x" <> showHex a ")"

joinLogEvents :: Monad m => (a -> m ()) -> (a -> m ()) -> a -> m ()
joinLogEvents x y e = x e >> y e

-------------------------------------------------------------------------------
-- Errors exported to VSCode

-- | Location for error
data ReoptExportedErrorLocation
  = -- | File offset in the input binary.
    ReoptErrorBinaryOffset !Int
  | -- | Describes a location in the control flow graph with function id,
    -- an optional block identifier and an optional index in the instruction.
    ReoptErrorControlFlowGraph !FunId !(Maybe Word64) !(Maybe Int)
  | -- | Describes a location in the function graph with function id,
    -- an optional block identifier and an optional index in the instruction.
    ReoptFunctionGraph !FunId !(Maybe Word64) !(Maybe Int)

data ReoptExportedError = ReoptExportedError
  { errorLocation :: !ReoptExportedErrorLocation
  , errorMessage :: !String
  }

-------------------------------------------------------------------------------
-- DiscoveryError

-- | Errors for discovery process.
data DiscoveryErrorTag
  = -- | Discovery uncovered an unexpected PLT stub
    DiscoveryPLTErrorTag
  | -- | Discovery had a translation error in block
    DiscoveryTransErrorTag
  | -- | Discovery had a PLT error in block.
    DiscoveryClassErrorTag
  deriving (Eq, Ord, Show)

instance Semigroup DiscoveryErrorTag where
  DiscoveryPLTErrorTag <> _ = DiscoveryPLTErrorTag
  _ <> DiscoveryPLTErrorTag = DiscoveryPLTErrorTag
  DiscoveryTransErrorTag <> _ = DiscoveryTransErrorTag
  DiscoveryClassErrorTag <> r = r

data DiscoveryError = DiscoveryError
  { discErrorTag :: !DiscoveryErrorTag
  , discErrorBlockAddr :: !Word64
  , discErrorBlockSize :: !Int
  , discErrorBlockInsnIndex :: !Int
  -- ^ Instruction index.
  , discErrorMessage :: !(PP.Doc ())
  }

-------------------------------------------------------------------------------
-- RecoveryError

data RecoverError w = RecoverErrorAt
  { recoverErrorTag :: !ReoptErrorTag
  , recoverErrorBlock :: !(MemSegmentOff w)
  , recoverErrorBlockSize :: !Int
  , recoverErrorInsnIndex :: !Int
  , recoverErrorMessage :: !Text
  }

-------------------------------------------------------------------------------
--  Other

-- | Identifies a step in Reopt's recompilation pipeline that is over all
-- functions.
--
-- The `r` parameter is used to represent information returned if the step
-- completes successfully.
data ReoptGlobalStep arch r where
  -- | Initial argument checking and setup for discovery.
  DiscoveryInitialization :: ReoptGlobalStep arch (DiscoveryState arch)
  -- | Parse information from header file to infer function types.
  HeaderTypeInference :: ReoptGlobalStep arch ()
  -- | Parse debug information to infer function types.
  DebugTypeInference :: ReoptGlobalStep arch ()
  -- | Global function argument inference
  FunctionArgInference :: ReoptGlobalStep arch ()
  -- | Run relinker
  Relinking :: ReoptGlobalStep arch ()

type GlobalStepId = Int

-- | Unique identifier for global steps so we can store them in map.
globalStepId :: ReoptGlobalStep arch r -> GlobalStepId
globalStepId DiscoveryInitialization = 0
globalStepId HeaderTypeInference = 1
globalStepId DebugTypeInference = 2
globalStepId FunctionArgInference = 3
globalStepId Relinking = 4

ppGlobalStep :: ReoptGlobalStep arch r -> String
ppGlobalStep DiscoveryInitialization = "Initialization"
ppGlobalStep HeaderTypeInference = "Header Processing"
ppGlobalStep DebugTypeInference = "Debug Processing"
ppGlobalStep FunctionArgInference = "Argument inference"
ppGlobalStep Relinking = "Relinker"

-- | Identifies steps in Reopt's recompilation pipeline that involves
-- a single function.
--
-- Parameters indicate result when a function step completes, error type when a
-- function step fails, and result returned when all functions are analyzed.
data ReoptFunStep arch r e ar where
  -- | Function discovery at given address and name.
  --
  -- The error should be a non-empty list of errors.
  Discovery :: ReoptFunStep arch () [DiscoveryError] (DiscoveryState arch)
  -- | Function invariant inference.
  InvariantInference :: ReoptFunStep arch (BlockInvariantMap arch ids) (RegisterUseError arch) ()
  -- | Function recovery at given address and name.
  Recovery :: ReoptFunStep arch () (RecoverError (ArchAddrWidth arch)) ()
  -- | Annotation generation for a function
  AnnotationGeneration :: ReoptFunStep arch () String ()

ppFunStep :: ReoptFunStep arch r e a -> String
ppFunStep Discovery = "Discovering"
ppFunStep InvariantInference = "Analyzing"
ppFunStep Recovery = "Recovering"
ppFunStep AnnotationGeneration = "Generating Annotations"

-- | A specific reason a ReoptStep failed for reporting purposes/statistics.
data ReoptErrorTag
  = MacawDiscoveryError !DiscoveryErrorTag
  | MacawRegisterUseErrorTag !(Some RegisterUseErrorTag)
  | MacawParsedTranslateFailureTag
  | MacawClassifyFailureTag
  | MacawCallAnalysisErrorTag
  | ReoptVarArgFnTag
  | ReoptUnsupportedTypeTag
  | ReoptBlockPreconditionUnresolvedTag
  | ReoptUnsupportedMemOpTag
  | ReoptTypeMismatchTag
  | ReoptUnsupportedFnValueTag
  | ReoptUninitializedAssignmentTag
  | ReoptUnimplementedFeatureTag
  | ReoptUnsupportedInFnRecoveryTag
  | ReoptUnimplementedLLVMBackendFeatureTag
  | ReoptStackOffsetEscapeTag
  | ReoptRegisterEscapeTag
  | ReoptStackReadOverlappingOffsetTag
  | ReoptUninitializedPhiVarTag
  | ReoptUnresolvedReturnValTag
  | ReoptCannotRecoverFnWithPLTStubsTag
  | ReoptInvariantInferenceFailureTag
  | ReoptMissingVariableValue
  | ReoptWarningTag
  | ReoptAnnotationGeneration
  deriving (Eq, Ord)

mkReoptErrorTag :: ReoptFunStep arch r e a -> e -> ReoptErrorTag
mkReoptErrorTag s e =
  case s of
    Discovery ->
      case e of
        [] -> error "Discovery failure reasons must be non-empty"
        de0 : rest ->
          MacawDiscoveryError (foldr (\de t -> discErrorTag de <> t) (discErrorTag de0) rest)
    InvariantInference ->
      case ruReason e of
        Reason tag _ -> MacawRegisterUseErrorTag (Some tag)
    Recovery ->
      recoverErrorTag e
    AnnotationGeneration -> ReoptAnnotationGeneration

ppReoptErrorTag :: ReoptErrorTag -> String
ppReoptErrorTag =
  \case
    MacawDiscoveryError e ->
      case e of
        DiscoveryPLTErrorTag -> "Unexpected PLT stub"
        DiscoveryTransErrorTag -> "Unhandled instruction"
        DiscoveryClassErrorTag -> "Unidentified control flow"
    MacawParsedTranslateFailureTag -> "Block translation error"
    MacawClassifyFailureTag -> "Block classification error"
    MacawRegisterUseErrorTag (Some tag) -> show tag
    MacawCallAnalysisErrorTag -> "Call analysis error"
    ReoptVarArgFnTag -> "Unsupported variadic function"
    ReoptUnsupportedTypeTag -> "Unsupported type tag"
    ReoptBlockPreconditionUnresolvedTag -> "Block precondition unresolved"
    ReoptUnsupportedMemOpTag -> "Unsupported memory operation"
    ReoptTypeMismatchTag -> "Type mismatch"
    ReoptUnsupportedFnValueTag -> "Unsupported function value"
    ReoptUninitializedAssignmentTag -> "Uninitialized assignment"
    ReoptUnimplementedFeatureTag -> "Unimplemented feature"
    ReoptUnsupportedInFnRecoveryTag -> "Unsupported action in function recovery"
    ReoptUnimplementedLLVMBackendFeatureTag -> "Unimplemented LLVM backend feature"
    ReoptStackOffsetEscapeTag -> "Stack offset escape"
    ReoptRegisterEscapeTag -> "Register escape"
    ReoptStackReadOverlappingOffsetTag -> "Stack read overlapping offset"
    ReoptUninitializedPhiVarTag -> "Uninitialized phi variable"
    ReoptUnresolvedReturnValTag -> "Unresolved return value"
    ReoptCannotRecoverFnWithPLTStubsTag -> "Unexpected PLT stub"
    ReoptInvariantInferenceFailureTag -> "Invariant inference failure"
    ReoptMissingVariableValue -> "Missing variable value"
    ReoptWarningTag -> "Warning"
    ReoptAnnotationGeneration -> "Annotations Failed"

-- | Event passed to logger when discovering functions
data ReoptLogEvent arch where
  -- | Indicates we started a global analysis step.
  ReoptGlobalStepStarted :: !(ReoptGlobalStep arch a) -> ReoptLogEvent arch
  -- | Indicate a step completed successfully.
  ReoptGlobalStepFinished :: !(ReoptGlobalStep arch a) -> !a -> ReoptLogEvent arch
  -- | Any information message.
  ReoptGlobalStepInfo :: !(ReoptGlobalStep arch a) -> !String -> ReoptLogEvent arch
  -- | Log a warning.
  ReoptGlobalStepWarning :: !(ReoptGlobalStep arch a) -> !String -> ReoptLogEvent arch
  -- | Any information message.
  ReoptFunStepInfo :: !(ReoptFunStep arch r e a) -> !String -> ReoptLogEvent arch
  -- | Indicates we started as step
  ReoptFunStepStarted :: !(ReoptFunStep arch r e a) -> !FunId -> ReoptLogEvent arch
  -- | Indicate a step completed successfully.
  ReoptFunStepFinished :: !(ReoptFunStep arch r e a) -> !FunId -> !r -> ReoptLogEvent arch
  -- | Indicate a step failed due to the given error.
  ReoptFunStepFailed :: !(ReoptFunStep arch r e a) -> !FunId -> !e -> ReoptLogEvent arch
  -- | Log an event specific to a function.
  ReoptFunStepLog :: !(ReoptFunStep arch r e a) -> !FunId -> !String -> ReoptLogEvent arch
  -- | All function steps of the given type are complete.
  ReoptFunStepAllFinished :: !(ReoptFunStep arch r e a) -> a -> ReoptLogEvent arch

ppSegOff :: MemSegmentOff w -> String
ppSegOff addr = "0x" <> showHex (segoffWord64 addr) ""

-- | Human-readable name of discovered function.
ppFnEntry :: Maybe BS.ByteString -> MemSegmentOff w -> String
ppFnEntry (Just nm) addr = BS.unpack nm <> "(" <> ppSegOff addr <> ")"
ppFnEntry Nothing addr = ppSegOff addr

-- | How much character space to spend on the herald, delimiters and spaces included
heraldLength :: Int
heraldLength = 11

-- | Displays output with the wanted herald.  Currently would look something like:
--
--   [HERALD] THING
withHerald :: String -> String -> IO ()
withHerald herald = hPutStrLn stderr . (printf formatString ("[" <> heraldText <> "] ") <>)
 where
  -- NOTE: 3 accounts for '[' and '] '
  heraldText = take (heraldLength - 3) herald
  formatString = "%-" <> show heraldLength <> "s"

logBeginOf :: String -> IO ()
logBeginOf = withHerald "BEGIN"

logEndOf :: String -> IO ()
logEndOf = withHerald "END"

logError :: String -> IO ()
logError = withHerald "ERROR"

logInfo :: String -> IO ()
logInfo = withHerald "INFO"

logStep :: String -> IO ()
logStep = withHerald "STEP"

logWarning :: String -> IO ()
logWarning = withHerald "WARNING"

-- | Function for recovering log information.
--
-- This has a side effect where it increments an IORef so
-- that the number of errors can be recorded.
printLogEvent ::
  ReoptLogEvent arch ->
  IO ()
printLogEvent event = do
  case event of
    ReoptGlobalStepStarted s -> logBeginOf $ ppGlobalStep s
    ReoptGlobalStepFinished s _ -> logEndOf $ ppGlobalStep s
    ReoptGlobalStepInfo _st msg -> logInfo msg
    ReoptGlobalStepWarning _st msg -> logWarning msg
    ReoptFunStepInfo _st msg -> logInfo msg
    ReoptFunStepStarted s f ->
      logBeginOf $ printf "%s function %s" (ppFunStep s) (ppFunId f)
    ReoptFunStepFinished s f _ ->
      logEndOf $ printf "%s function %s" (ppFunStep s) (ppFunId f)
    ReoptFunStepFailed s _ e ->
      logError $
        case s of
          Discovery ->
            unlines $
              [ printf "Discovery failed (0x%x): %s" (discErrorBlockAddr de) (show (discErrorMessage de))
              | de <- e
              ]
                ++ ["Incomplete."]
          InvariantInference ->
            printf
              "Invariant inference failed (0x%x): %s"
              (segoffWord64 (ruBlock e))
              (ppRegisterUseErrorReason (ruReason e))
          Recovery ->
            printf
              "Recovery failed (0x%x:%d, %s): %s"
              (segoffWord64 (recoverErrorBlock e))
              (recoverErrorInsnIndex e)
              (ppReoptErrorTag (recoverErrorTag e))
              (recoverErrorMessage e)
          AnnotationGeneration ->
            printf "%s" e
    ReoptFunStepLog _st _f msg -> logStep msg
    ReoptFunStepAllFinished _ _ -> pure ()

-------------------------------------------------------------------------------
-- ReoptFatalErrors

-- | Enumeration of different file types Reopt may read or write.
data ReoptFileType
  = -- | Annotations of function signatures provided by user.
    UserHeaderFileType
  | -- CFG file type generated by reopt
    CfgFileType
  | -- Functions recovered for binary
    FunsFileType
  | -- | Annotations file generated for ReoptVCG.
    AnnotationsFileType
  | -- | JSON file with information needed to run relinker.
    RelinkerInfoFileType
  | -- | @.ll@ file produced by generating LLVM file
    LlvmFileType
  | -- | @.o@ file produced by compiling LLVM file
    ObjectFileType
  | -- | File reopt reads to configure Occam.
    OccamConfigFileType
  | -- | Manifest file passed to Occam slash
    --
    -- This file is generated by Reopt
    OccamManifestFileType

ppReoptFileType :: ReoptFileType -> String
ppReoptFileType tp =
  case tp of
    UserHeaderFileType -> "header file"
    CfgFileType -> "Discovered functions"
    FunsFileType -> "Recovered functions"
    AnnotationsFileType -> "annotations"
    RelinkerInfoFileType -> "relinker information"
    LlvmFileType -> "LLVM bitcode"
    ObjectFileType -> "Object file"
    OccamConfigFileType -> "Occam configuration file"
    OccamManifestFileType -> "Generated OCCAM manifest file."

-- | Errors that terminate reopt immediately when they occur.
data ReoptFatalError
  = -- | Error occuring when writing to a file.
    ReoptWriteError !ReoptFileType !FilePath !IOException
  | -- | Error occurring in initialization
    ReoptInitError !String
  | -- | Error in running C preprocessor to parse user annotations.
    ReoptUserAnnPreprocessorError !Ext.Failure
  | -- | Error in content of a text file.
    ReoptTextParseError !ReoptFileType !FilePath !String
  | -- | Fatal error during relinking
    ReoptRelinkerFatalError !String

instance Show ReoptFatalError where
  show (ReoptWriteError tp path e) =
    printf "Error writing %s:\n  Path: %s\n  Message: %s" (ppReoptFileType tp) path (show e)
  show (ReoptInitError msg) = msg
  show (ReoptUserAnnPreprocessorError f) =
    printf "Error running preprocessor to parse annotations:\n  %s" (show f)
  show (ReoptTextParseError tp path msg) =
    unlines
      [ printf "Error reading %s:" (ppReoptFileType tp)
      , printf "  Path: %s" path
      , printf "  %s" msg
      ]
  show (ReoptRelinkerFatalError e) = e

-------------------------------------------------------------------------------

-- | Tags for reporting errors during various reopt steps.
newtype ReoptStepTag = ReoptStepTag BS.ByteString
  deriving (Eq, Ord)

instance Show ReoptStepTag where
  show (ReoptStepTag s) = BS.unpack s

ppReoptStepTag :: ReoptStepTag -> String
ppReoptStepTag (ReoptStepTag s) = BS.unpack s

-- | Returns the step tag corresponding to the given ReoptStep.
-- N.B., we combine several inference steps and just refer to
-- them as part of discovery for error reporting purposes.
reoptStepTag :: ReoptFunStep arch r e a -> ReoptStepTag
reoptStepTag s = ReoptStepTag (BS.pack (ppFunStep s))

type StepErrorMap a = Map ReoptStepTag (Map ReoptErrorTag a)

incStepError ::
  Num a =>
  ReoptStepTag ->
  ReoptErrorTag ->
  StepErrorMap a ->
  StepErrorMap a
incStepError stepTag failureTag = Map.alter logFail stepTag
 where
  incErr Nothing = Just 1 -- if there is not an entry for the particular error, start at 1
  incErr (Just cnt) = Just $ cnt + 1 -- otherwise just increment the count by 1
  logFail Nothing = Just $ Map.fromList [(failureTag, 1)] -- if there is no map for this step, start one
  logFail (Just m) = Just $ Map.alter incErr failureTag m -- otherwise just increment the particular failure

-- | Render the registered failures in an indented list-style Doc.
renderAllFailures' :: forall a. (Num a, Show a) => StepErrorMap a -> PP.Doc ()
renderAllFailures' = PP.vsep . map renderStepFailures . Map.toList
 where
  renderStepFailures :: (ReoptStepTag, Map ReoptErrorTag a) -> PP.Doc ()
  renderStepFailures (tag, failures) =
    let hdr =
          PP.hsep
            [ PP.viaShow $ stepCount failures
            , PP.pretty "failures during"
            , PP.pretty (ppReoptStepTag tag) <> PP.pretty " step:"
            ]
     in PP.hang 2 $ PP.vsep $ hdr : map renderFailure (Map.toList failures)
  renderFailure :: (ReoptErrorTag, a) -> PP.Doc ()
  renderFailure (tag, cnt) = PP.hsep [PP.pretty $ show cnt, PP.pretty $ ppReoptErrorTag tag]
  stepCount :: Map ReoptErrorTag a -> a
  stepCount = foldl' (+) 0 . Map.elems

renderAllFailures :: (Num a, Show a) => StepErrorMap a -> String
renderAllFailures failures =
  renderString $
    PP.layoutPretty PP.defaultLayoutOptions $
      PP.indent 2 $
        renderAllFailures' failures

-----------------------------------------------------------------------
-- FunStepStats

data FunStepStats = FunStepStats
  { fsSuccessCount :: !Int
  , fsFailMap :: !(Map ReoptErrorTag Int)
  }

instance Semigroup FunStepStats where
  x <> y =
    FunStepStats
      { fsSuccessCount = fsSuccessCount x + fsSuccessCount y
      , fsFailMap = Map.unionWith (+) (fsFailMap x) (fsFailMap y)
      }

instance Monoid FunStepStats where
  mempty =
    FunStepStats
      { fsSuccessCount = 0
      , fsFailMap = Map.empty
      }

recordFunStepStatsSuccess :: FunStepStats -> FunStepStats
recordFunStepStatsSuccess fs = fs{fsSuccessCount = fsSuccessCount fs + 1}

recordFunStepStatsFail :: ReoptErrorTag -> FunStepStats -> FunStepStats
recordFunStepStatsFail tag fs = fs{fsFailMap = Map.insertWith (+) tag 1 (fsFailMap fs)}

fsFailCount :: FunStepStats -> Int
fsFailCount fs = sum (fsFailMap fs)

fsTotalCount :: FunStepStats -> Int
fsTotalCount fs = fsSuccessCount fs + fsFailCount fs

-- | Render digits grouped by commas every 3 digits
groupDigits :: Show a => a -> String
groupDigits = addCommas . show
 where
  addCommas = reverse . intercalate "," . unfoldr chunkBy3 . reverse
  chunkBy3 l = case splitAt 3 l of
    ([], _) -> Nothing
    p -> Just p

-- | Pretty print a value along with a fraction relative to a denominator.
ppFrac :: String -> Int -> Int -> String
ppFrac nm s t = printf "%s: %s (%s%%)" nm (groupDigits s) (show (round (100.0 * fromIntegral s / fromIntegral t :: Double) :: Int))

ppIndent :: [String] -> [String]
ppIndent = fmap ("  " ++)

ppFunStepStats :: FunStepStats -> [String]
ppFunStepStats s =
  let t = fsTotalCount s
   in [ ppFrac "Succeeded" (fsSuccessCount s) t
      , ppFrac "Failed" (fsFailCount s) t
      ]
        ++ ppIndent [ppFrac (ppReoptErrorTag tag) c t | (tag, c) <- Map.toList (fsFailMap s)]

-----------------------------------------------------------------------
-- ReoptStats

-- | Statistics summarizing Reopt
data ReoptStats = ReoptStats
  { statsCodeSegmentSize :: !Word64
  -- ^ Number of bytes in code segment.
  , statsInitEntryPointCount :: !Int
  -- ^ Number of initial entry points in the binary
  , statsDiscoveredCodeSize :: !Word64
  -- ^ Number of bytes in discovered functions
  , statsGlobalStepWarnings :: !(Map GlobalStepId Int)
  -- ^ Global warnings
  , statsFunStepStats :: !(Map ReoptStepTag FunStepStats)
  -- ^ Statistics about different per function stages.
  }

instance Semigroup ReoptStats where
  x <> y =
    ReoptStats
      { statsCodeSegmentSize = statsCodeSegmentSize x + statsCodeSegmentSize y
      , statsInitEntryPointCount = statsInitEntryPointCount x + statsInitEntryPointCount y
      , statsDiscoveredCodeSize = statsDiscoveredCodeSize x + statsDiscoveredCodeSize y
      , statsGlobalStepWarnings = Map.unionWith (+) (statsGlobalStepWarnings x) (statsGlobalStepWarnings y)
      , statsFunStepStats = Map.unionWith (<>) (statsFunStepStats x) (statsFunStepStats y)
      }

instance Monoid ReoptStats where
  mempty =
    ReoptStats
      { statsCodeSegmentSize = 0
      , statsInitEntryPointCount = 0
      , statsDiscoveredCodeSize = 0
      , statsGlobalStepWarnings = Map.empty
      , statsFunStepStats = Map.empty
      }

globalStepWarningCount :: ReoptGlobalStep arch r -> ReoptStats -> Int
globalStepWarningCount step stats = Map.findWithDefault 0 (globalStepId step) (statsGlobalStepWarnings stats)

recordGlobalStepWarning ::
  ReoptGlobalStep arch r ->
  ReoptStats ->
  ReoptStats
recordGlobalStepWarning step stats =
  stats
    { statsGlobalStepWarnings =
        Map.insertWith (+) (globalStepId step) 1 (statsGlobalStepWarnings stats)
    }

-- | Return function step stats for given step.
lookupFunStepStats :: ReoptFunStep arch r e a -> ReoptStats -> FunStepStats
lookupFunStepStats step stats =
  Map.findWithDefault mempty (reoptStepTag step) (statsFunStepStats stats)

modifyFunStepStats ::
  ReoptFunStep arch r e a ->
  (FunStepStats -> FunStepStats) ->
  ReoptStats ->
  ReoptStats
modifyFunStepStats step f stats =
  let upd _new = f
   in stats{statsFunStepStats = Map.insertWith upd (reoptStepTag step) (f mempty) (statsFunStepStats stats)}

modifyIORefFunStepStats ::
  IORef ReoptStats ->
  ReoptFunStep arch r e a ->
  (FunStepStats -> FunStepStats) ->
  IO ()
modifyIORefFunStepStats r step f =
  modifyIORef' r (modifyFunStepStats step f)

ppSection :: String -> [String] -> [String]
ppSection nm rows = (nm ++ ":") : ppIndent rows

outputRow :: String -> String -> String
outputRow hdr val = hdr ++ ": " ++ val

ppInitializationStats :: ReoptStats -> [String]
ppInitializationStats stats =
  ppSection
    "Initialization"
    [ outputRow "Code segment" (groupDigits (statsCodeSegmentSize stats) ++ " bytes")
    , outputRow "Initial entry points" (show (statsInitEntryPointCount stats))
    , outputRow "Warnings" (show (globalStepWarningCount DiscoveryInitialization stats))
    ]

ppDiscoveryStats :: ReoptStats -> [String]
ppDiscoveryStats stats = do
  let discCodeSize = fromIntegral (statsDiscoveredCodeSize stats)
  let totalCodeSize = fromIntegral (statsCodeSegmentSize stats)
  ppSection "Discovery" $
    ppFrac "Bytes discovered" discCodeSize totalCodeSize
      : ppFunStepStats (lookupFunStepStats Discovery stats)

ppArgumentAnalysisStats :: ReoptStats -> [String]
ppArgumentAnalysisStats stats = do
  let discTotal = fsSuccessCount $ lookupFunStepStats Discovery stats
  let inferTotal = fsTotalCount $ lookupFunStepStats InvariantInference stats
  ppSection
    "Argument Analysis"
    [ ppFrac "Succeeded" inferTotal discTotal
    , ppFrac "Failed" (discTotal - inferTotal) discTotal
    , outputRow "Header Warnings" (groupDigits (globalStepWarningCount HeaderTypeInference stats))
    , outputRow "DWARF Warnings" (groupDigits (globalStepWarningCount DebugTypeInference stats))
    , outputRow "Code Warnings" (groupDigits (globalStepWarningCount FunctionArgInference stats))
    ]

ppInferenceStats :: ReoptStats -> [String]
ppInferenceStats stats =
  ppSection "Invariant Inference" $
    ppFunStepStats (lookupFunStepStats InvariantInference stats)

ppRecoveryStats :: ReoptStats -> [String]
ppRecoveryStats stats =
  ppSection "Recovery" $
    ppFunStepStats (lookupFunStepStats Recovery stats)

ppStats :: ReoptStats -> [String]
ppStats stats =
  ppInitializationStats stats
    ++ ppDiscoveryStats stats
    ++ ppArgumentAnalysisStats stats
    ++ ppInferenceStats stats
    ++ ppRecoveryStats stats

-----------------------------------------------------------------------
-- ReoptSummary

-- | Describes the result of a function recovery attempt.
data FnRecoveryResult
  = forall arch r e ar. FnFailedAt !(ReoptFunStep arch r e ar)

instance Show FnRecoveryResult where
  show (FnFailedAt f) =
    case f of
      Discovery -> "DiscoveryFailed"
      InvariantInference -> "InvariantFailed"
      Recovery -> "RecoveryFailed"
      AnnotationGeneration -> "AnnotationsFailed"

-- | Collected information on recovery results.
--
-- Used for generating statistics.
data ReoptSummary = ReoptSummary
  { summaryBinaryPath :: !FilePath
  -- ^ Which binary are these statistics for?
  , summaryFnResults :: !(Map FunId FnRecoveryResult)
  -- ^ Mapping of functions to the result of recovery
  }

initReoptSummary :: FilePath -> ReoptSummary
initReoptSummary binPath =
  ReoptSummary
    { summaryBinaryPath = binPath
    , summaryFnResults = Map.empty
    }

setSummaryFnStatus ::
  FunId ->
  FnRecoveryResult ->
  ReoptSummary ->
  ReoptSummary
setSummaryFnStatus f r s =
  s{summaryFnResults = Map.insert f r (summaryFnResults s)}

-- | Header row for data produced by @statsRows@
summaryHeader :: [String]
summaryHeader = ["binary", "fn name", "address", "recovery result"]

-- | Rows for table summary of recovery statistics; see also @statsHeader@.
summaryRows ::
  -- | Stats to convert to rows.
  ReoptSummary ->
  [[String]]
summaryRows stats = map toCsvRow $ Map.toList $ summaryFnResults stats
 where
  toCsvRow :: (FunId, FnRecoveryResult) -> [String]
  toCsvRow (FunId faddr nm, res) =
    let
      name = BS.unpack nm
      hexAddr = "0x" ++ showHex faddr ""
     in
      [summaryBinaryPath stats, name, hexAddr, show res]

-- | Function for updating statistics using events capturing during run.
recoverLogEvent ::
  IORef ReoptSummary ->
  IORef ReoptStats ->
  -- | Message to log
  ReoptLogEvent arch ->
  IO ()
recoverLogEvent summaryRef statsRef event = do
  -- Record more detailed info when appropriate.
  case event of
    ReoptGlobalStepWarning step _ -> do
      modifyIORef' statsRef $ recordGlobalStepWarning step
    ReoptGlobalStepFinished DiscoveryInitialization ds -> do
      let mem = memory ds
      let codeSize =
            addrWidthClass (memAddrWidth mem) $
              sumBy (memWordValue . segmentSize) (memSegments mem)
      let cnt = Map.size $ ds ^. unexploredFunctions
      modifyIORef' statsRef $ \s ->
        s
          { statsCodeSegmentSize = codeSize
          , statsInitEntryPointCount = cnt
          }
    ReoptFunStepFinished step _ _ -> do
      modifyIORefFunStepStats statsRef step recordFunStepStatsSuccess
    ReoptFunStepFailed step f e -> do
      modifyIORef' summaryRef $ setSummaryFnStatus f (FnFailedAt step)
      -- Record fun step failure
      let errTag = mkReoptErrorTag step e
      modifyIORefFunStepStats statsRef step (recordFunStepStatsFail errTag)
    ReoptFunStepAllFinished Discovery discState -> do
      let m = mkFunUseMap discState
      modifyIORef' statsRef $ \s ->
        s{statsDiscoveredCodeSize = totalFunUseSize m}

    -- Ignore other events
    _ -> do
      pure ()
