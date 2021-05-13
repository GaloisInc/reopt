{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Events
  ( ReoptLogEvent(..)
  , ReoptStep(..)
  , ReoptEventSeverity(..)
  , isErrorEvent
    -- * Statistics
  , ReoptStats(..)
  , FnRecoveryResult(..)
  , initReoptStats
  , reportStats
  , renderFnStats
  , statsHeader
  , statsRows
  , ppFnEntry
  , segoffWord64
  , ppSegOff
  ) where

import           Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Word
import           Numeric ( showHex )
import           Numeric.Natural ( Natural )
import           System.IO
import           Text.Printf (printf)

import           Data.Macaw.Analysis.RegisterUse (BlockInvariantMap)
import           Data.Macaw.CFG

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
  FunctionArgInference :: ReoptStep arch ()
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
ppStep FunctionArgInference = "Argument inference"
ppStep (Recovery a mnm) = "Recovering " <> ppFn a mnm
ppStep (InvariantInference a mnm) = "Analyzing " <> ppFn a mnm

data ReoptEventSeverity
   = ReoptInfo
     -- ^ Informational event used to report progress.
   | ReoptWarning
     -- ^ Warning that something was amiss that likely will affect results.


-- | Event passed to logger when discovering functions
data ReoptLogEvent arch
     -- | Indicates we started as step.
   = forall a. ReoptStepStarted !(ReoptStep arch a)
     -- | Log an event.
   | forall a. ReoptLogEvent !(ReoptStep arch a) !ReoptEventSeverity !String
     -- | Indicate a step failed due to the given error.
   | forall a. ReoptStepFailed !(ReoptStep arch a) !String
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
  show (ReoptLogEvent _st _sev msg) = printf "  %s" msg
  show (ReoptStepFailed _st msg) = printf "  Failed: %s" msg

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
  | FnFailedRecovery
  deriving (Show, Eq)

-- | Statistics summarizing our recovery efforts.
data ReoptStats =
  ReoptStats
  { statsBinary :: !FilePath
  -- ^ Which binary are these statistics for?
  , statsFnResults :: Map (Maybe BS.ByteString, Word64) FnRecoveryResult
  -- ^ Mapping of functions to the result of recovery
  , statsFnDiscoveredCount :: Natural
  -- ^ Number of discovered functions (i.e., may or may not end up being successfully recovered).
  , statsFnRecoveredCount :: Natural
  -- ^ Number of successfully recovered functions.
  , statsFnFailedCount :: Natural
  -- ^ Number of functions which failed during recovery.
  , statsErrorCount :: Natural
  -- ^ Overall error count.
  }

initReoptStats :: FilePath -> ReoptStats
initReoptStats binPath =
  ReoptStats
  { statsBinary = binPath
  , statsFnResults = Map.empty
  , statsFnDiscoveredCount = 0
  , statsFnRecoveredCount = 0
  , statsFnFailedCount = 0
  , statsErrorCount = 0
  }

renderFnStats :: ReoptStats -> String
renderFnStats s =
  if statsFnDiscoveredCount s == 0 then
    "reopt discovered no functions."
   else do
    let passed :: Double = (fromIntegral $ statsFnRecoveredCount s) / (fromIntegral $  statsFnDiscoveredCount s)
        passedStr = printf " (%.2f%%)" (passed * 100.0)
        failed :: Double = (fromIntegral $ statsFnFailedCount s) / (fromIntegral $  statsFnDiscoveredCount s)
        failedStr = printf " (%.2f%%)" (failed * 100.0)
    "reopt discovered " ++ (show (statsFnDiscoveredCount s)) ++ " functions in the binary "++(statsBinary s)++":\n"
      ++ "  recovery succeeded: " ++ (show (statsFnRecoveredCount s)) ++ passedStr ++ "\n"
      ++ "  recovery failed: " ++ (show (statsFnFailedCount s)) ++ failedStr ++ "\n"

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
          in [statsBinary stats, name, hexAddr, show res]

exportFnStats :: FilePath -- ^ Path to write statistics to.
              -> ReoptStats -- ^ Stats to export.
              -> IO ()
exportFnStats outPath stats = do
  let hdrStr = intercalate "," statsHeader
      rowsStr = map (intercalate ",") $ statsRows stats
  writeFile outPath $ unlines $ hdrStr:rowsStr

-- | Print and/or export statistics (if relevant flags are set) and the error count.
reportStats
  :: Bool -- ^ Whether to print statistics to stderr.
  -> Maybe FilePath -- ^ Where to export stats to.
  -> ReoptStats
  -> IO ()
reportStats printStats mStatsPath stats = do
  when printStats $ do
    hPutStrLn stderr $ renderFnStats stats
  case mStatsPath of
    Nothing -> pure ()
    Just statsPath -> exportFnStats statsPath stats
  when ((statsErrorCount stats) > 0) $ do
    hPutStrLn stderr $
      if (statsErrorCount stats) == 1 then
        "1 error occured."
       else
        show (statsErrorCount stats) ++ " errors occured."
