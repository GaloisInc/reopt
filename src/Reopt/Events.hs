{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Events
  ( ReoptLogEvent(..)
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
  ) where

import           Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Numeric ( showHex )
import           Numeric.Natural ( Natural )
import           System.IO
import           Text.Printf (printf)

import           Data.Macaw.Analysis.RegisterUse (BlockInvariantMap)
import           Data.Macaw.CFG
import           Data.Macaw.Discovery

import           Reopt.ArgResolver ( showArgResolverError, ArgResolverError )

-- | Event passed to logger when discovering functions
data ReoptLogEvent arch
   = InitializationWarning !String
   | ArgResolverError !String !ArgResolverError
   | DuplicateSections !BS.ByteString
   | DebugError !String
   | StartFunDiscovery !(Maybe BS.ByteString) !(ArchSegmentOff arch) !(FunctionExploreReason (ArchAddrWidth arch))
   | StartBlockDiscovery !(ArchSegmentOff arch)
     -- | Failed to infer arguments to function do to unresolvable call sitee
   | FnArgInferenceFailed !(Maybe BS.ByteString) !(ArchSegmentOff arch) !(ArchSegmentOff arch) String
   | FnInvariantsFailed !(Maybe BS.ByteString) !(ArchSegmentOff arch) String
     -- ^ Notify register use computation failed.
   | forall ids . StartFunRecovery !(Maybe BS.ByteString) !(ArchSegmentOff arch) !(BlockInvariantMap arch ids)
     -- ^ Notify we are starting analysis of given function.
   | EndFunRecovery  !(Maybe BS.ByteString) !(ArchSegmentOff arch)
     -- ^ Notify we successfully completed analysis of given function.
   | RecoveryFailed !(Maybe BS.ByteString)  !(ArchSegmentOff arch) String
     -- ^ @RecoveryFailed dnm addr msg@ notes we failed to recover function due to given reason.
   | RecoveryPLTSkipped  !(Maybe BS.ByteString) !(ArchSegmentOff arch)
   | RecoveryWarning !(Maybe BS.ByteString) !(ArchSegmentOff arch) String
   | RecoveryError !String
     -- ^ A general error message

ppSegOff :: MemSegmentOff w -> String
ppSegOff addr = "0x" <> showHex (memWordValue (addrOffset (segoffAddr addr))) ""

-- | Human-readable name of discovered function.
ppFnEntry :: Maybe BS.ByteString -> MemSegmentOff w -> String
ppFnEntry (Just nm) addr = BS.unpack nm <> "(" <> ppSegOff addr <> ")"
ppFnEntry Nothing addr   = ppSegOff addr



instance Show (ReoptLogEvent arch) where
  show (InitializationWarning msg) = "Warning: " ++ msg
  show (ArgResolverError fnm e)  = printf "Type error on %s: %s" fnm (showArgResolverError e)
  show (DuplicateSections nm)  = "Multiple sections named " ++ BS.unpack nm
  show (DebugError msg)        = msg
  show (StartFunDiscovery dnm faddr rsn) =
    "Discovering function: " <> ppFnEntry dnm faddr ++  ppFunReason rsn
  show (StartBlockDiscovery addr) =
    "  Analyzing block: " <> ppSegOff addr
  show (FnArgInferenceFailed dnm faddr callSite msg) =
    printf "Call argument analysis at %s in %s failed:\n  %s" (ppSegOff callSite) (ppFnEntry dnm faddr) msg
  show (FnInvariantsFailed dnm faddr msg) =
    "Invariant synthesis failed " <> ppFnEntry dnm faddr <> "\n  " <> msg
  show (StartFunRecovery dnm faddr _)  =
    "Recovering function " <> ppFnEntry dnm faddr
  show (EndFunRecovery dnm faddr)  =
     let fnm = ppFnEntry dnm faddr
      in "Completed recovering function " <> fnm
  show (RecoveryFailed _ _ msg)  = "  " <> msg
  show (RecoveryPLTSkipped _ _)  = "  Skipped PLT stub"
  show (RecoveryWarning _ _ msg) = "  Warning: " ++ msg
  show (RecoveryError msg) = msg

-- | Should this event increase the error count?
isErrorEvent ::  ReoptLogEvent arch -> Bool
isErrorEvent =
  \case
    InitializationWarning{} -> True
    ArgResolverError{}    -> True
    DuplicateSections{}   -> True
    DebugError{}          -> True
    StartFunDiscovery{}   -> False
    StartBlockDiscovery{} -> False
    FnArgInferenceFailed{} -> True
    FnInvariantsFailed{}   -> True
    StartFunRecovery{}    -> False
    EndFunRecovery{}      -> False
    RecoveryFailed{}      -> True
    RecoveryPLTSkipped{}  -> True
    RecoveryWarning{}     -> True
    RecoveryError{}       -> True

-------------------------------------------------------------------------------

-- | Describes the result of a function recovery attempt.
data FnRecoveryResult
  = FnDiscovered
  | FnRecovered
  | FnFailedRecovery
  | FnPLTSkipped
  deriving (Show, Eq)

-- | Statistics summarizing our recovery efforts.
data ReoptStats w =
  ReoptStats
  { statsBinary :: !FilePath
  -- ^ Which binary are these statistics for?
  , statsFnResults :: Map (Maybe BS.ByteString, MemSegmentOff w) FnRecoveryResult
  -- ^ Mapping of functions to the result of recovery
  , statsFnDiscoveredCount :: Natural
  -- ^ Number of discovered functions (i.e., may or may not end up being successfully recovered).
  , statsFnRecoveredCount :: Natural
  -- ^ Number of successfully recovered functions.
  , statsFnPLTSkippedCount :: Natural
  -- ^ Number of skipped PLT stubs.
  , statsFnFailedCount :: Natural
  -- ^ Number of functions which failed during recovery.
  , statsErrorCount :: Natural
  -- ^ Overall error count.
  }

initReoptStats :: FilePath -> ReoptStats w
initReoptStats binPath =
  ReoptStats
  { statsBinary = binPath
  , statsFnResults = Map.empty
  , statsFnDiscoveredCount = 0
  , statsFnRecoveredCount = 0
  , statsFnPLTSkippedCount = 0
  , statsFnFailedCount = 0
  , statsErrorCount = 0
  }

renderFnStats :: ReoptStats w -> String
renderFnStats s =
  if statsFnDiscoveredCount s == 0 then
    "reopt discovered no functions."
   else do
    let passed :: Double = (fromIntegral $ statsFnRecoveredCount s) / (fromIntegral $  statsFnDiscoveredCount s)
        passedStr = printf " (%.2f%%)" (passed * 100.0)
        failed :: Double = (fromIntegral $ statsFnFailedCount s) / (fromIntegral $  statsFnDiscoveredCount s)
        failedStr = printf " (%.2f%%)" (failed * 100.0)
        skipped :: Double = (fromIntegral $ statsFnPLTSkippedCount s) / (fromIntegral $  statsFnDiscoveredCount s)
        skippedStr = printf " (%.2f%%)" (skipped * 100.0)
    "reopt discovered " ++ (show (statsFnDiscoveredCount s)) ++ " functions in the binary "++(statsBinary s)++":\n"
      ++ "  recovery succeeded: " ++ (show (statsFnRecoveredCount s)) ++ passedStr ++ "\n"
      ++ "  recovery failed: " ++ (show (statsFnFailedCount s)) ++ failedStr ++ "\n"
      ++ "  skipped PLT stub: " ++ (show (statsFnPLTSkippedCount s)) ++ skippedStr

-- | Header row for data produced by @statsRows@
statsHeader :: [String]
statsHeader = ["binary", "fn name", "address", "recovery result"]

-- | Rows for table summary of recovery statistics; see also @statsHeader@.
statsRows :: forall w
          .  MemWidth w
          => ReoptStats w -- ^ Stats to convert to rows.
          -> [[String]]
statsRows stats = map toCsvRow $ Map.toList $ statsFnResults stats
  where toCsvRow :: (((Maybe BS.ByteString), (MemSegmentOff w)), FnRecoveryResult) -> [String]
        toCsvRow ((mNm, faddr), res) =
          let name = case mNm of Nothing -> ""; Just nm -> BS.unpack nm
              hexAddr = "0x" ++ (showHex (addrOffset (segoffAddr faddr)) "")
          in [statsBinary stats, name, hexAddr, show res]

exportFnStats :: MemWidth w
              => FilePath -- ^ Path to write statistics to.
              -> ReoptStats w -- ^ Stats to export.
              -> IO ()
exportFnStats outPath stats = do
  let hdrStr = intercalate "," statsHeader
      rowsStr = map (intercalate ",") $ statsRows stats
  writeFile outPath $ unlines $ hdrStr:rowsStr

-- | Print and/or export statistics (if relevant flags are set) and the error count.
reportStats
  :: MemWidth w
  => Bool -- ^ Whether to print statistics to stderr.
  -> Maybe FilePath -- ^ Where to export stats to.
  -> ReoptStats w
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