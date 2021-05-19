{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Events
  ( ReoptLogEvent(..)
  , ReoptErrorTag(..)
  , ReoptStep(..)
  , ReoptStepTag(..)
  , ReoptEventSeverity(..)
  , ReoptLogData(..)
  , isErrorEvent
    -- * Statistics
  , ReoptStats(..)
  , FnRecoveryResult(..)
  , initReoptStats
  , statsStepErrorCount
  , renderAllFailures
  , stepErrorCount
  , incFnResult
  , incStepError
  , reoptStepTag
  , stepFailResult
  , mergeFnFailures
  , statsHeader
  , statsRows
  , ppFnEntry
  , segoffWord64
  , ppSegOff
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Word
import           Numeric ( showHex )
import           Numeric.Natural ( Natural )
import           Text.Printf (printf)

import           Data.Macaw.Analysis.RegisterUse (BlockInvariantMap)
import Data.Macaw.CFG
    ( MemSegmentOff,
      MemAddr(addrOffset),
      segoffAddr,
      MemWord(memWordValue) )
import           Prettyprinter
  (Doc, pretty, vsep, hsep, indent, hang, layoutPretty, defaultLayoutOptions)
import           Prettyprinter.Render.String (renderString)

-- | Identifies steps in Reopt's recompilation pipeline that may
-- generate events.
--
-- The parameter is used to represent information returned if the
-- event completes successfully.
data ReoptStep arch a where
  -- | Initial argument checking and setup for discovery.
  DiscoveryInitialization :: ReoptStep arch ()
  -- | Parse information from header file to infer function types.
  HeaderTypeInference :: ReoptStep arch ()
  -- | Parse debug information to infer function types.
  DebugTypeInference :: ReoptStep arch ()
  -- | Function discovery at given address and name.
  Discovery :: !Word64
            -> !(Maybe BS.ByteString)
            -> ReoptStep arch ()
  -- | Global function argument inference
  FunctionArgInference :: Maybe (Word64 , Maybe BS.ByteString) -> ReoptStep arch ()
  -- | Function invariant inference.
  InvariantInference :: !Word64 -> !(Maybe BS.ByteString) -> ReoptStep arch (BlockInvariantMap arch ids)
  -- | Function recovery at given address and name.
  Recovery :: !Word64 -> !(Maybe BS.ByteString) -> ReoptStep arch ()

ppFn :: Word64 -> Maybe BS.ByteString -> String
ppFn a (Just nm) = BS.unpack nm <> "(0x" <> showHex a ")"
ppFn a Nothing   = "0x" <> showHex a ""


ppStep :: ReoptStep arch a -> String
ppStep DiscoveryInitialization = "Initialization"
ppStep HeaderTypeInference = "Header Processing"
ppStep DebugTypeInference = "Debug Processing"
ppStep (Discovery a mnm) = "Discovering " <> ppFn a mnm
ppStep (FunctionArgInference Nothing) = "Argument inference"
ppStep (FunctionArgInference (Just (a, mnm))) = "Argument inference " <> ppFn a mnm
ppStep (Recovery a mnm) = "Recovering " <> ppFn a mnm
ppStep (InvariantInference a mnm) = "Analyzing " <> ppFn a mnm

data ReoptEventSeverity
   = ReoptInfo
     -- ^ Informational event used to report progress.
   | ReoptWarning
     -- ^ Warning that something was amiss that likely will affect results.

-- | Tags for reporting errors during various reopt steps.
data ReoptStepTag
  = DiscoveryStepTag
  | RecoveryStepTag
  deriving (Eq, Ord, Show)

ppReoptStepTag :: ReoptStepTag -> String
ppReoptStepTag =
  \case
    DiscoveryStepTag -> "discovery"
    RecoveryStepTag -> "recovery"

-- | Returns the step tag corresponding to the given ReoptStep.
-- N.B., we combine several inference steps and just refer to
-- them as part of discovery for error reporting purposes.
reoptStepTag :: ReoptStep arch a -> ReoptStepTag
reoptStepTag =
  \case
    DiscoveryInitialization{} -> DiscoveryStepTag
    HeaderTypeInference{} -> DiscoveryStepTag
    DebugTypeInference{} -> DiscoveryStepTag
    Discovery{} -> DiscoveryStepTag
    FunctionArgInference{} -> DiscoveryStepTag
    InvariantInference{} -> DiscoveryStepTag
    Recovery{} -> RecoveryStepTag


-- | A specific reason a ReoptStep failed for reporting purposes/statistics.
data ReoptErrorTag
  = MacawParsedTranslateFailureTag
  | MacawClassifyFailureTag
  | MacawRegisterUseErrorTag
  | MacawCallAnalysisErrorTag
  | ReoptVarArgFnTag
  | ReoptMissingNameTag
  | ReoptMissingTypeTag
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
  deriving (Eq, Ord, Show)

ppReoptErrorTag :: ReoptErrorTag -> String
ppReoptErrorTag =
  \case
    MacawParsedTranslateFailureTag -> "block translation error"
    MacawClassifyFailureTag -> "block classification error"
    MacawRegisterUseErrorTag -> "register use error"
    MacawCallAnalysisErrorTag -> "call analysis error"
    ReoptVarArgFnTag -> "unsupported variadic function"
    ReoptMissingNameTag -> "missing name"
    ReoptMissingTypeTag -> "missing type"
    ReoptUnsupportedTypeTag -> "unsupported type tag"
    ReoptBlockPreconditionUnresolvedTag -> "block precondition unresolved"
    ReoptUnsupportedMemOpTag -> "unsupported memory operation"
    ReoptTypeMismatchTag -> "type mismatch"
    ReoptUnsupportedFnValueTag -> "unsupported function value"
    ReoptUninitializedAssignmentTag -> "uninitialized assignment"
    ReoptUnimplementedFeatureTag -> "unimplemented feature"
    ReoptUnsupportedInFnRecoveryTag -> "unsupported action in function recovery"
    ReoptUnimplementedLLVMBackendFeatureTag -> "unimplemented LLVM backend feature"
    ReoptStackOffsetEscapeTag -> "stack offset escape"
    ReoptRegisterEscapeTag -> "register escape"
    ReoptStackReadOverlappingOffsetTag -> "stack read overlapping offset"
    ReoptUninitializedPhiVarTag -> "uninitialized phi variable"
    ReoptUnresolvedReturnValTag -> "unresolved return value"
    ReoptCannotRecoverFnWithPLTStubsTag -> "unexpected PLT stub"
    ReoptInvariantInferenceFailureTag -> "invariant inference failure"
    ReoptMissingVariableValue -> "missing variable value"
    ReoptWarningTag -> "warning"

data ReoptLogData
  = ReoptLogMessage !String
  | ReoptLogInitEntryPointCount !Natural

-- | Event passed to logger when discovering functions
data ReoptLogEvent arch
     -- | Indicates we started as step.
   = forall a. ReoptStepStarted !(ReoptStep arch a)
     -- | Log an event.
   | forall a. ReoptLogEvent !(ReoptStep arch a) !ReoptEventSeverity !ReoptLogData
     -- | Indicate a step failed due to the given error.
   | forall a. ReoptStepFailed !(ReoptStep arch a) !ReoptErrorTag !String
     -- | Indicate a step completed successfully.
   | forall a. ReoptStepFinished !(ReoptStep arch a) !a

segoffWord64 :: MemSegmentOff w -> Word64
segoffWord64 = memWordValue . addrOffset . segoffAddr


ppSegOff :: MemSegmentOff w -> String
ppSegOff addr = "0x" <> showHex (segoffWord64 addr) ""

-- | Human-readable name of discovered function.
ppFnEntry :: Maybe BS.ByteString -> MemSegmentOff w -> String
ppFnEntry (Just nm) addr = BS.unpack nm <> "(" <> ppSegOff addr <> ")"
ppFnEntry Nothing addr   = ppSegOff addr


--ppSeverity :: ReoptEventSeverity -> String
--ppSeverity ReoptInfo = "Info"
--ppSeverity ReoptWarning = "Warn"


instance Show (ReoptLogEvent arch) where
  show (ReoptStepStarted st) = ppStep st
  show (ReoptStepFinished _ _) = printf "  Complete."
  show (ReoptLogEvent _st _sev (ReoptLogMessage msg)) = printf "  %s" msg
  show (ReoptLogEvent _st _sev (ReoptLogInitEntryPointCount cnt)) =
    printf "  initial entry points: %d" cnt
  show (ReoptStepFailed _st tag msg) =
    printf "  Failed (%s): %s" (ppReoptErrorTag tag) msg

-- | Should this event increase the error count?
isErrorEvent ::  ReoptLogEvent arch -> Bool
isErrorEvent =
  \case
    ReoptStepStarted{} -> False
    ReoptLogEvent _ ReoptInfo _ -> False
    ReoptLogEvent _ _ _         -> True
    ReoptStepFailed{} -> True
    ReoptStepFinished{} -> False

-------------------------------------------------------------------------------

-- | Describes the result of a function recovery attempt.
data FnRecoveryResult
  = FnDiscovered
  | FnRecovered
  | FnFailedDiscovery
  | FnFailedRecovery
  deriving (Show, Eq)

-- | Convert the failure of a step to the appropriate FnRecoveryResult if
-- possible along with the address and function name (if present), else return
-- Nothing.
stepFailResult :: ReoptStep arch a -> Maybe (FnRecoveryResult, Word64, Maybe BS.ByteString)
stepFailResult =
  \case
    DiscoveryInitialization{} -> Nothing
    HeaderTypeInference{} -> Nothing
    DebugTypeInference{} -> Nothing
    Discovery a mnm -> Just (FnFailedDiscovery, a, mnm)
    FunctionArgInference (Just (a, mnm)) -> Just (FnFailedDiscovery, a, mnm)
    FunctionArgInference Nothing -> Nothing
    InvariantInference a mnm-> Just (FnFailedRecovery, a, mnm)
    Recovery a mnm -> Just (FnFailedRecovery, a, mnm)

-- | Statistics summarizing our recovery efforts.
data ReoptStats =
  ReoptStats
  { statsBinaryPath :: !FilePath
  -- ^ Which binary are these statistics for?
  , statsBinarySize :: !Natural
  -- ^ How many bytes is the binary?
  , statsInitEntryPointCount :: !Natural
  -- ^ Number of initial entry points in the binary
  , statsFnResults :: !(Map (Maybe BS.ByteString, Word64) FnRecoveryResult)
  -- ^ Mapping of functions to the result of recovery
  , statsFnDiscoveredCount :: !Natural
  -- ^ Number of discovered functions (i.e., may or may not end up being successfully recovered).
  , statsFnRecoveredCount :: !Natural
  -- ^ Number of successfully recovered functions.
  , statsStepErrors :: !(Map ReoptStepTag (Map ReoptErrorTag Natural))
  -- ^ Errors and warnings encountered, organized by reopt step.
  , statsErrorCount :: !Natural
  -- ^ Overall error count.
  }

initReoptStats :: FilePath -> Natural -> ReoptStats
initReoptStats binPath binSize =
  ReoptStats
  { statsBinaryPath = binPath
  , statsBinarySize = binSize
  , statsInitEntryPointCount = 0
  , statsFnResults = Map.empty
  , statsFnDiscoveredCount = 0
  , statsFnRecoveredCount = 0
  , statsStepErrors = Map.empty
  , statsErrorCount = 0
  }

statsStepErrorCount :: ReoptStats -> Natural
statsStepErrorCount stats = foldl' (+) 0 totals
  where totals = concatMap Map.elems $ Map.elems $ statsStepErrors stats


-- | Number of errors during a particular step.
stepErrorCount :: ReoptStepTag -> ReoptStats -> Natural
stepErrorCount step stats = foldl' (+) 0 errors
  where errors = Map.findWithDefault Map.empty step
                 $ statsStepErrors stats



incFnResult ::
  Maybe BS.ByteString ->
  Word64 ->
  FnRecoveryResult ->
  Map (Maybe BS.ByteString, Word64) FnRecoveryResult ->
  Map (Maybe BS.ByteString, Word64) FnRecoveryResult
incFnResult mFnName fnAddress fnResult results = Map.insert (mFnName, fnAddress) fnResult results

incStepError ::
  ReoptStepTag ->
  ReoptErrorTag ->
  Map ReoptStepTag (Map ReoptErrorTag Natural) ->
  Map ReoptStepTag (Map ReoptErrorTag Natural)
incStepError stepTag failureTag = Map.alter logFail stepTag
  where incErr Nothing    = Just 1 -- if there is not an entry for the particular error, start at 1
        incErr (Just cnt) = Just $ cnt+1 -- otherwise just increment the count by 1
        logFail Nothing = Just $ Map.fromList [(failureTag, 1)] -- if there is no map for this step, start one
        logFail (Just m) = Just $ Map.alter incErr failureTag m -- otherwise just increment the particular failure

-- | Combine two maps of reopt failures, i.e., combining their respective counts.
mergeFnFailures ::
  Map ReoptStepTag (Map ReoptErrorTag Natural) ->
  Map ReoptStepTag (Map ReoptErrorTag Natural) ->
  Map ReoptStepTag (Map ReoptErrorTag Natural)
mergeFnFailures = Map.unionWith mergeStepMap
  where mergeStepMap :: Map ReoptErrorTag Natural -> Map ReoptErrorTag Natural -> Map ReoptErrorTag Natural
        mergeStepMap = Map.unionWith (+)

-- | Render the registered failures in an indented list-style Doc.
renderAllFailures' :: Map ReoptStepTag (Map ReoptErrorTag Natural) -> Doc ()
renderAllFailures' = vsep . (map renderStepFailures) . Map.toList
  where
    renderStepFailures :: (ReoptStepTag, Map ReoptErrorTag Natural) -> Doc ()
    renderStepFailures (tag, failures) =
      let hdr = hsep [pretty $ stepCount failures
                     , pretty "failures during"
                     , (pretty $ ppReoptStepTag tag) <> (pretty " step:")]
      in hang 2 $ vsep $ [hdr] ++ (map renderFailure $ Map.toList failures)
    renderFailure :: (ReoptErrorTag, Natural) -> Doc ()
    renderFailure (tag, cnt) = hsep [pretty $ show cnt, pretty $ ppReoptErrorTag tag]
    stepCount :: Map ReoptErrorTag Natural -> Natural
    stepCount = foldl' (+) 0 . Map.elems


renderAllFailures :: Map ReoptStepTag (Map ReoptErrorTag Natural) -> String
renderAllFailures  failures =
  renderString
  $ layoutPretty defaultLayoutOptions
  $ indent 2
  $ renderAllFailures' failures


-- | Header row for data produced by @statsRows@
statsHeader :: [String]
statsHeader = ["binary", "fn name", "address", "recovery result"]

-- | Rows for table summary of recovery statistics; see also @statsHeader@.
statsRows :: ReoptStats -- ^ Stats to convert to rows.
          -> [[String]]
statsRows stats = map toCsvRow $ Map.toList $ statsFnResults stats
  where toCsvRow :: ((Maybe BS.ByteString, Word64), FnRecoveryResult) -> [String]
        toCsvRow ((mNm, faddr), res) =
          let name = case mNm of Nothing -> ""; Just nm -> BS.unpack nm
              hexAddr = "0x" ++ showHex faddr ""
          in [statsBinaryPath stats, name, hexAddr, show res]
