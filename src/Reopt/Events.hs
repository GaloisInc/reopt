{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Events
  ( ReoptLogEvent(..)
  , isErrorEvent
    -- * Statistics
  , ReoptStats(..)
  , initReoptStats
  , reportStats
  , recoverLogEvent
  , renderFnStats
  , statsHeader
  , statsRows
  ) where

import           Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import           Data.List (intercalate)
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Numeric ( showHex )
import           Numeric.Natural ( Natural )
import           System.IO
import           Text.Printf (printf)

import           Data.Macaw.CFG
import           Data.Macaw.Discovery

import Reopt.ArgResolver ( showArgResolverError, ArgResolverError )

-- | Event passed to logger when discovering functions
data ReoptLogEvent w
   = InitializationWarning !String
   | ArgResolverError !String !ArgResolverError
   | DuplicateSections !BS.ByteString
   | DebugError !String
   | StartFunDiscovery !(Maybe BS.ByteString) !(MemSegmentOff w) !(FunctionExploreReason w)
   | StartBlockDiscovery !(MemSegmentOff w)
   | StartFunRecovery !(Maybe BS.ByteString) !(MemSegmentOff w)
     -- ^ Notify we are starting analysis of given function.
   | EndFunRecovery  !(Maybe BS.ByteString) !(MemSegmentOff w)
     -- ^ Notify we successfully completed analysis of given function.
   | RecoveryFailed !(Maybe BS.ByteString)  !(MemSegmentOff w) !String
     -- ^ @RecoveryFailed dnm addr msg@ notes we failed to recover function due to given reason.
   | RecoveryPLTSkipped  !(Maybe BS.ByteString)  !(MemSegmentOff w)
   | RecoveryWarning !(Maybe BS.ByteString) !(MemSegmentOff w) !String
   | RecoveryError !String
     -- ^ A general error message

-- | Human-readable name of discovered function.
ppFnEntry :: Maybe BS.ByteString -> MemSegmentOff w -> String
ppFnEntry (Just nm) addr = BS.unpack nm <> "(0x" <> showHex (memWordValue (addrOffset (segoffAddr addr))) ")"
ppFnEntry Nothing addr   = "0x" <> showHex (memWordValue (addrOffset (segoffAddr addr))) ""

instance Show (ReoptLogEvent w) where
  show (InitializationWarning msg) = "Warning: " ++ msg
  show (ArgResolverError fnm e)  = printf "Type error on %s: %s" fnm (showArgResolverError e)
  show (DuplicateSections nm)  = "Multiple sections named " ++ BS.unpack nm
  show (DebugError msg)        = msg
  show (StartFunDiscovery dnm faddr rsn) =
    "Discovering function: " <> ppFnEntry dnm faddr ++  ppFunReason rsn
  show (StartBlockDiscovery addr) =
    "  Analyzing block: " <> "0x" <> showHex (memWordValue (addrOffset (segoffAddr addr))) ""
  show (StartFunRecovery dnm faddr)  =
    "Recovering function " <> ppFnEntry dnm faddr
  show (EndFunRecovery dnm faddr)  =
     let fnm = ppFnEntry dnm faddr
      in "Completed recovering function " <> fnm
  show (RecoveryFailed _ _ msg)  = "  " <> msg
  show (RecoveryPLTSkipped _ _)  = "  Skipped PLT stub"
  show (RecoveryWarning _ _ msg) = "  Warning: " ++ msg
  show (RecoveryError msg) = msg

-- | Should this event increase the error count?
isErrorEvent ::  ReoptLogEvent w -> Bool
isErrorEvent =
  \case
    InitializationWarning{} -> True
    ArgResolverError{}   -> True
    DuplicateSections{}  -> True
    DebugError{}         -> True
    StartFunDiscovery{}  -> False
    StartBlockDiscovery{} -> False
    StartFunRecovery{}   -> False
    EndFunRecovery{}     -> False
    RecoveryFailed{}     -> True
    RecoveryPLTSkipped{} -> True
    RecoveryWarning{}    -> True
    RecoveryError{}      -> True

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


-- | Function for recovering log information.
--
-- This has a side effect where it increments an IORef so
-- that the number of errors can be recorded.
recoverLogEvent
  :: IORef (ReoptStats w)
  -> ReoptLogEvent w  -- ^ Message to log
  -> IO ()
recoverLogEvent statsRef event = do
  -- Update error count when applicable
  when (isErrorEvent event) $ do
    modifyIORef' statsRef $ \s -> s {statsErrorCount = 1 + (statsErrorCount s)}
  -- Record more detailed info when appropriate.
  case event of
    StartFunRecovery mNm addr -> do
      let update s = s { statsFnResults = Map.insert (mNm, addr) FnDiscovered (statsFnResults s)
                       , statsFnDiscoveredCount = 1 + (statsFnDiscoveredCount s)}
      modifyIORef' statsRef update
    EndFunRecovery mNm addr -> do
      let update s = s { statsFnResults = Map.insert (mNm, addr) FnRecovered (statsFnResults s)
                       , statsFnRecoveredCount = 1 + (statsFnRecoveredCount s) }
      prevEntry <- Map.lookup (mNm, addr) <$> statsFnResults <$> readIORef statsRef
      case prevEntry of
        Nothing -> hPutStrLn stderr (show event)
        Just FnDiscovered -> pure ()
        Just res -> hPutStrLn stderr $ "function " ++ ppFnEntry mNm addr
                                       ++ " had an unexpected previous state at the end of discovery: "
                                       ++ show res
      modifyIORef' statsRef update
    RecoveryFailed mNm addr _errMsg -> do
      let update s = s { statsFnResults = Map.insert (mNm, addr) FnFailedRecovery (statsFnResults s)
                       , statsFnFailedCount = 1 + (statsFnFailedCount s) }
      modifyIORef' statsRef update
    RecoveryPLTSkipped mNm addr -> do
      let update s = s { statsFnResults = Map.insert (mNm, addr) FnPLTSkipped (statsFnResults s)
                       , statsFnPLTSkippedCount = 1 + (statsFnPLTSkippedCount s) }
      modifyIORef' statsRef update
    _ -> pure ()
  -- Print log info to stderr.
  hPutStrLn stderr (show event)