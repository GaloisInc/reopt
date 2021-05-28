{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Datatype for recording events in Reopt.
module Reopt.Events
  ( ReoptLogEvent (..),
    ReoptErrorTag (..),
    ReoptGlobalStep (..),
    ReoptFunStep (..),
    ReoptStepTag (..),
    mkReoptErrorTag,
    DiscoveryErrorType (..),
    FunId (..),
    funId,
    printLogEvent,

    -- * Summary
    ReoptSummary (..),
    FnRecoveryResult (..),
    initReoptSummary,
    setSummaryFnStatus,

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
  )
where

import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.List (foldl', intersperse, unfoldr)
import Data.Macaw.Analysis.RegisterUse (BlockInvariantMap)
import Data.Macaw.Discovery (DiscoveryState, memory, unexploredFunctions)
import Data.Macaw.Memory
  ( MemAddr (addrOffset),
    MemSegmentOff,
    MemWord (memWordValue),
    addrWidthClass,
    memAddrWidth,
    memSegments,
    segmentSize,
    segoffAddr,
  )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Word (Word64)
import Numeric (showHex)
import Prettyprinter
  ( Doc,
    defaultLayoutOptions,
    hang,
    hsep,
    indent,
    layoutPretty,
    pretty,
    viaShow,
    vsep,
  )
import Prettyprinter.Render.String (renderString)
import Reopt.FunUseMap
import Reopt.Utils.Folds
import System.IO
import Text.Printf (printf)

-- | Function identifier and name.
data FunId = FunId !Word64 !BS.ByteString
  deriving (Eq, Ord)

funId :: MemSegmentOff w -> Maybe BS.ByteString -> FunId
funId a mnm = FunId (memWordValue (addrOffset (segoffAddr a))) (fromMaybe BS.empty mnm)

ppFunId :: FunId -> String
ppFunId (FunId a nm)
  | BS.null nm = "0x" <> showHex a ""
  | otherwise = BS.unpack nm <> "(0x" <> showHex a ")"

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
  { errorLocation :: !ReoptExportedErrorLocation,
    errorMessage :: !String
  }

-------------------------------------------------------------------------------
--  Other

-- | Errors for discovery process.
data DiscoveryErrorType
  = DiscoveryTransError
  | DiscoveryClassError
  deriving (Eq, Ord, Show)

instance Semigroup DiscoveryErrorType where
  DiscoveryTransError <> _ = DiscoveryTransError
  DiscoveryClassError <> r = r

-- | Identifies a step in Reopt's recompilation pipeline that
-- is over all functions.
--
-- The parameter is used to represent information returned if the
-- step completes successfully.
data ReoptGlobalStep arch r where
  -- | Initial argument checking and setup for discovery.
  DiscoveryInitialization :: ReoptGlobalStep arch (DiscoveryState arch)
  -- | Parse information from header file to infer function types.
  HeaderTypeInference :: ReoptGlobalStep arch ()
  -- | Parse debug information to infer function types.
  DebugTypeInference :: ReoptGlobalStep arch ()
  -- | Global function argument inference
  FunctionArgInference :: ReoptGlobalStep arch ()

type GlobalStepId = Int

globalStepId :: ReoptGlobalStep arch r -> GlobalStepId
globalStepId DiscoveryInitialization = 0
globalStepId HeaderTypeInference = 1
globalStepId DebugTypeInference = 2
globalStepId FunctionArgInference = 3

ppGlobalStep :: ReoptGlobalStep arch r -> String
ppGlobalStep DiscoveryInitialization = "Initialization"
ppGlobalStep HeaderTypeInference = "Header Processing"
ppGlobalStep DebugTypeInference = "Debug Processing"
ppGlobalStep FunctionArgInference = "Argument inference"

-- | Identifies steps in Reopt's recompilation pipeline that involves
-- a single function.
--
-- Parameters indicate result when a function step completes, error type when a
-- function step fails, and result returned when all functions are analyzed.
data ReoptFunStep arch r e ar where
  -- | Function discovery at given address and name.
  Discovery :: ReoptFunStep arch () DiscoveryErrorType (DiscoveryState arch)
  -- | Function invariant inference.
  InvariantInference :: ReoptFunStep arch (BlockInvariantMap arch ids) (ReoptErrorTag, String) ()
  -- | Function recovery at given address and name.
  Recovery :: ReoptFunStep arch () (ReoptErrorTag, String) ()

ppFunStep :: ReoptFunStep arch r e a -> String
ppFunStep Discovery = "Discovering"
ppFunStep InvariantInference = "Analyzing"
ppFunStep Recovery = "Recovering"

-- | A specific reason a ReoptStep failed for reporting purposes/statistics.
data ReoptErrorTag
  = MacawDiscoveryError !DiscoveryErrorType
  | MacawParsedTranslateFailureTag
  | MacawClassifyFailureTag
  | MacawRegisterUseErrorTag
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
  deriving (Eq, Ord)

mkReoptErrorTag :: ReoptFunStep arch r e a -> e -> ReoptErrorTag
mkReoptErrorTag s e =
  case s of
    Discovery -> MacawDiscoveryError e
    InvariantInference -> fst e
    Recovery -> fst e

ppReoptErrorTag :: ReoptErrorTag -> String
ppReoptErrorTag =
  \case
    MacawDiscoveryError e ->
      case e of
        DiscoveryTransError -> "Unhandled instruction"
        DiscoveryClassError -> "Unidentified control flow"
    MacawParsedTranslateFailureTag -> "Block translation error"
    MacawClassifyFailureTag -> "Block classification error"
    MacawRegisterUseErrorTag -> "Register use error"
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

-- | Event passed to logger when discovering functions
data ReoptLogEvent arch where
  -- | Indicates we started a global analysis step.
  ReoptGlobalStepStarted :: !(ReoptGlobalStep arch a) -> ReoptLogEvent arch
  -- | Indicate a step completed successfully.
  ReoptGlobalStepFinished :: !(ReoptGlobalStep arch a) -> !a -> ReoptLogEvent arch
  -- | Log a warning.
  ReoptGlobalStepWarning :: !(ReoptGlobalStep arch a) -> !String -> ReoptLogEvent arch
  -- | Log an event.
  ReoptGlobalStepLog :: !(ReoptGlobalStep arch a) -> !String -> ReoptLogEvent arch
  -- | Indicates we started as step
  ReoptFunStepStarted :: !(ReoptFunStep arch r e a) -> !FunId -> ReoptLogEvent arch
  -- | Indicate a step completed successfully.
  ReoptFunStepFinished :: !(ReoptFunStep arch r e a) -> !FunId -> !r -> ReoptLogEvent arch
  -- | Indicate a step failed due to the given error.
  ReoptFunStepFailed :: !(ReoptFunStep arch r e a) -> !FunId -> !e -> ReoptLogEvent arch
  -- | Log an event.
  ReoptFunStepLog :: !(ReoptFunStep arch r e a) -> !FunId -> !String -> ReoptLogEvent arch
  -- | Log an event.
  ReoptFunStepWarning :: !(ReoptFunStep arch r e a) -> !FunId -> !String -> ReoptLogEvent arch
  -- | All function steps of the given type are complete.
  ReoptFunStepAllFinished :: !(ReoptFunStep arch r e a) -> a -> ReoptLogEvent arch

segoffWord64 :: MemSegmentOff w -> Word64
segoffWord64 = memWordValue . addrOffset . segoffAddr

ppSegOff :: MemSegmentOff w -> String
ppSegOff addr = "0x" <> showHex (segoffWord64 addr) ""

-- | Human-readable name of discovered function.
ppFnEntry :: Maybe BS.ByteString -> MemSegmentOff w -> String
ppFnEntry (Just nm) addr = BS.unpack nm <> "(" <> ppSegOff addr <> ")"
ppFnEntry Nothing addr = ppSegOff addr

ppReoptError :: (ReoptErrorTag, String) -> String
ppReoptError (tag, msg) = printf "  Failed (%s): %s" (ppReoptErrorTag tag) msg

-- | Function for recovering log information.
--
-- This has a side effect where it increments an IORef so
-- that the number of errors can be recorded.
printLogEvent ::
  ReoptLogEvent arch ->
  IO ()
printLogEvent event = do
  case event of
    ReoptGlobalStepStarted s ->
      hPutStrLn stderr $ ppGlobalStep s
    ReoptGlobalStepFinished _ _ ->
      hPutStrLn stderr $ printf "  Complete."
    ReoptGlobalStepWarning _st msg ->
      hPutStrLn stderr $ printf "  %s" msg
    ReoptGlobalStepLog _st msg ->
      hPutStrLn stderr $ printf "  %s" msg
    ReoptFunStepStarted s f ->
      hPutStrLn stderr $ ppFunStep s ++ " " ++ ppFunId f
    ReoptFunStepFinished _ _ _ ->
      hPutStrLn stderr $ printf "  Complete."
    ReoptFunStepFailed s _ e ->
      hPutStrLn stderr $
        ("  " ++) $
          case s of
            Discovery -> "Incomplete."
            InvariantInference -> ppReoptError e
            Recovery -> ppReoptError e
    ReoptFunStepLog _st _f msg ->
      hPutStrLn stderr $ printf "  %s" msg
    ReoptFunStepWarning _st _f msg ->
      hPutStrLn stderr $ printf "  %s" msg
    ReoptFunStepAllFinished _ _ ->
      pure ()

-------------------------------------------------------------------------------

-- | Describes the result of a function recovery attempt.
data FnRecoveryResult
  = FnDiscovered
  | FnRecovered
  | FnFailedDiscovery
  | FnFailedRecovery
  deriving (Show, Eq)

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
renderAllFailures' :: forall a. (Num a, Show a) => StepErrorMap a -> Doc ()
renderAllFailures' = vsep . (map renderStepFailures) . Map.toList
  where
    renderStepFailures :: (ReoptStepTag, Map ReoptErrorTag a) -> Doc ()
    renderStepFailures (tag, failures) =
      let hdr =
            hsep
              [ viaShow $ stepCount failures,
                pretty "failures during",
                (pretty $ ppReoptStepTag tag) <> (pretty " step:")
              ]
       in hang 2 $ vsep $ [hdr] ++ (map renderFailure $ Map.toList failures)
    renderFailure :: (ReoptErrorTag, a) -> Doc ()
    renderFailure (tag, cnt) = hsep [pretty $ show cnt, pretty $ ppReoptErrorTag tag]
    stepCount :: Map ReoptErrorTag a -> a
    stepCount = foldl' (+) 0 . Map.elems

renderAllFailures :: (Num a, Show a) => StepErrorMap a -> String
renderAllFailures failures =
  renderString $
    layoutPretty defaultLayoutOptions $
      indent 2 $
        renderAllFailures' failures

-----------------------------------------------------------------------
-- FunStepStats

data FunStepStats = FunStepStats
  { fsSuccessCount :: !Int,
    fsFailMap :: !(Map ReoptErrorTag Int)
  }

instance Semigroup FunStepStats where
  x <> y =
    FunStepStats
      { fsSuccessCount = fsSuccessCount x + fsSuccessCount y,
        fsFailMap = Map.unionWith (+) (fsFailMap x) (fsFailMap y)
      }

instance Monoid FunStepStats where
  mempty =
    FunStepStats
      { fsSuccessCount = 0,
        fsFailMap = Map.empty
      }

recordFunStepStatsSuccess :: FunStepStats -> FunStepStats
recordFunStepStatsSuccess fs = fs {fsSuccessCount = fsSuccessCount fs + 1}

recordFunStepStatsFail :: ReoptErrorTag -> FunStepStats -> FunStepStats
recordFunStepStatsFail tag fs = fs {fsFailMap = Map.insertWith (+) tag 1 (fsFailMap fs)}

fsFailCount :: FunStepStats -> Int
fsFailCount fs = sum (fsFailMap fs)

fsTotalCount :: FunStepStats -> Int
fsTotalCount fs = fsSuccessCount fs + fsFailCount fs

-- | Render digits grouped by commas every 3 digits
groupDigits :: Show a => a -> String
groupDigits = addCommas . show
  where
    addCommas = reverse . concat . intersperse "," . unfoldr chunkBy3 . reverse
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
   in [ ppFrac "Succeeded" (fsSuccessCount s) t,
        ppFrac "Failed" (fsFailCount s) t
      ]
        ++ ppIndent [ppFrac (ppReoptErrorTag tag) c t | (tag, c) <- Map.toList (fsFailMap s)]

-----------------------------------------------------------------------
-- ReoptStats

-- | Statistics summarizing Reopt
data ReoptStats = ReoptStats
  { -- | Number of bytes in code segment.
    statsCodeSegmentSize :: !Word64,
    -- | Number of initial entry points in the binary
    statsInitEntryPointCount :: !Int,
    -- | Number of bytes in discovered functions
    statsDiscoveredCodeSize :: !Word64,
    -- | Global warnings
    statsGlobalStepWarnings :: !(Map GlobalStepId Int),
    -- | Statistics about different per function stages.
    statsFunStepStats :: !(Map ReoptStepTag FunStepStats)
  }

instance Semigroup ReoptStats where
  x <> y =
    ReoptStats
      { statsCodeSegmentSize = statsCodeSegmentSize x + statsCodeSegmentSize y,
        statsInitEntryPointCount = statsInitEntryPointCount x + statsInitEntryPointCount y,
        statsDiscoveredCodeSize = statsDiscoveredCodeSize x + statsDiscoveredCodeSize y,
        statsGlobalStepWarnings = Map.unionWith (+) (statsGlobalStepWarnings x) (statsGlobalStepWarnings y),
        statsFunStepStats = Map.unionWith (<>) (statsFunStepStats x) (statsFunStepStats y)
      }

instance Monoid ReoptStats where
  mempty =
    ReoptStats
      { statsCodeSegmentSize = 0,
        statsInitEntryPointCount = 0,
        statsDiscoveredCodeSize = 0,
        statsGlobalStepWarnings = Map.empty,
        statsFunStepStats = Map.empty
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
  let upd _new old = f old
   in stats {statsFunStepStats = Map.insertWith upd (reoptStepTag step) (f mempty) (statsFunStepStats stats)}

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
    [ outputRow "Code segment" (groupDigits (statsCodeSegmentSize stats) ++ " bytes"),
      outputRow "Initial entry points" (show (statsInitEntryPointCount stats)),
      outputRow "Warnings" (show (globalStepWarningCount DiscoveryInitialization stats))
    ]

ppDiscoveryStats :: ReoptStats -> [String]
ppDiscoveryStats stats = do
  let discCodeSize = fromIntegral (statsDiscoveredCodeSize stats)
  let totalCodeSize = fromIntegral (statsCodeSegmentSize stats)
  ppSection "Discovery" $
    [ppFrac "Bytes discovered" discCodeSize totalCodeSize]
      ++ ppFunStepStats (lookupFunStepStats Discovery stats)

ppArgumentAnalysisStats :: ReoptStats -> [String]
ppArgumentAnalysisStats stats = do
  let discTotal = fsSuccessCount $ lookupFunStepStats Discovery stats
  let inferTotal = fsTotalCount $ lookupFunStepStats InvariantInference stats
  ppSection "Argument Analysis" $
    [ ppFrac "Succeeded" inferTotal discTotal,
      ppFrac "Failed" (discTotal - inferTotal) discTotal,
      outputRow "Header Warnings" (groupDigits (globalStepWarningCount HeaderTypeInference stats)),
      outputRow "DWARF Warnings" (groupDigits (globalStepWarningCount DebugTypeInference stats)),
      outputRow "Code Warnings" (groupDigits (globalStepWarningCount FunctionArgInference stats))
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

-- | Collected information on recovery results.
--
-- Used for generating statistics.
data ReoptSummary = ReoptSummary
  { -- | Which binary are these statistics for?
    summaryBinaryPath :: !FilePath,
    -- | Mapping of functions to the result of recovery
    summaryFnResults :: !(Map FunId FnRecoveryResult)
  }

initReoptSummary :: FilePath -> ReoptSummary
initReoptSummary binPath =
  ReoptSummary
    { summaryBinaryPath = binPath,
      summaryFnResults = Map.empty
    }

setSummaryFnStatus ::
  FunId ->
  FnRecoveryResult ->
  ReoptSummary ->
  ReoptSummary
setSummaryFnStatus f r s =
  s {summaryFnResults = Map.insert f r (summaryFnResults s)}

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
      let name = BS.unpack nm
          hexAddr = "0x" ++ showHex faddr ""
       in [summaryBinaryPath stats, name, hexAddr, show res]

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
          { statsCodeSegmentSize = codeSize,
            statsInitEntryPointCount = cnt
          }
    ReoptFunStepFinished step _ _ -> do
      modifyIORefFunStepStats statsRef step recordFunStepStatsSuccess
    {-
          case step of
            Discovery -> do
              modifyIORef' summaryRef $ setSummaryFnStatus f FnDiscovered
            InvariantInference -> do
              pure ()
            Recovery -> do
              modifyIORef' summaryRef $ setSummaryFnStatus f FnRecovered
    -}

    ReoptFunStepFailed step f e -> do
      let result =
            case step of
              Discovery -> FnFailedDiscovery
              InvariantInference -> FnFailedRecovery
              Recovery -> FnFailedRecovery
      modifyIORef' summaryRef $ setSummaryFnStatus f result
      -- Record fun step failure
      let errTag = mkReoptErrorTag step e
      modifyIORefFunStepStats statsRef step (recordFunStepStatsFail errTag)
    ReoptFunStepAllFinished Discovery discState -> do
      let m = mkFunUseMap discState
      modifyIORef' statsRef $ \s ->
        s {statsDiscoveredCodeSize = totalFunUseSize m}

    -- Ignore other events
    _ -> do
      pure ()