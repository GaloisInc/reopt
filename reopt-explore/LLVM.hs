module LLVM (runLLVM) where

import Control.Exception (
  SomeException,
  catch,
  handle,
 )
import Control.Monad (when)
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (newIORef, readIORef)
import Data.List (intercalate)
import Data.Macaw.X86 (X86_64)
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)
import System.IO (
  hPrint,
  hPutStr,
  hPutStrLn,
  stderr,
 )
import System.Timeout (timeout)
import Text.Printf (printf)

import Reopt (
  LLVMLogEvent,
  LoadOptions (LoadOptions, loadOffset),
  RecoverX86Output (logEvents, recoveredModule),
  RecoveredModule,
  ReoptOptions (..),
  defaultLLVMGenOptions,
  emptyAnnDeclarations,
  latestLLVMConfig,
  llvmLogEventHeader,
  llvmLogEventToStrings,
  parseElfHeaderInfo64,
  recoverX86Elf,
  renderLLVMBitcode,
  reoptWriteBuilder,
  runReoptM,
 )
import Reopt.Events
import Reopt.TypeInference.ConstraintGen (ModuleConstraints)
import Reopt.Utils.Exit

import CommandLine
import Common
import Reopt.X86 (X86OS)

data LLVMGenResult
  = -- | Error message
    LLVMGenFail String
  | -- | How many bytes of LLVM bitcode were generated and any logging.
    LLVMGenPass Natural ![LLVMLogEvent]

llvmGenSuccess :: LLVMGenResult -> Bool
llvmGenSuccess LLVMGenPass{} = True
llvmGenSuccess LLVMGenFail{} = False

llvmGenLogEvents :: LLVMGenResult -> [LLVMLogEvent]
llvmGenLogEvents (LLVMGenFail _) = []
llvmGenLogEvents (LLVMGenPass _ events) = events

data ExplorationResult
  = ExplorationStats ReoptSummary ReoptStats LLVMGenResult ![LLVMLogEvent]
  | ExplorationFailure FilePath String

renderExplorationResult :: ExplorationResult -> String
renderExplorationResult (ExplorationStats summary stats lgen _logEvents) = do
  let llvmGen = case lgen of
        LLVMGenPass _ _ -> "Succeeded."
        LLVMGenFail errMsg -> "Failed: " ++ errMsg
  summaryBinaryPath summary
    ++ "\n"
    ++ unlines (ppIndent (ppStats stats ++ ["LLVM generation status: " ++ llvmGen]))
renderExplorationResult (ExplorationFailure path msg) =
  "Exploration of " ++ path ++ " failed: " ++ msg

renderLogEvents :: ExplorationResult -> Maybe [String]
renderLogEvents (ExplorationStats summary _stats lgen events) =
  Just $ map renderRow $ events ++ llvmGenLogEvents lgen
 where
  binPath = summaryBinaryPath summary
  renderRow event = intercalate "," $ binPath : llvmLogEventToStrings event
renderLogEvents (ExplorationFailure _ _) = Nothing

data SummaryStats = SummaryStats
  { totalBinaryCount :: !Natural
  -- ^ How many binaries were analyzed?
  , totalStats :: !ReoptStats
  -- ^ Summary of stats from individual runs
  , totalLLVMGenerated :: !Natural
  -- ^ Number of binaries which we successfully produced LLVM bitcode for.
  , totalFailureCount :: !Natural
  -- ^ Total number of complete failures.
  }

totalSuccessCount :: SummaryStats -> Natural
totalSuccessCount stats = totalBinaryCount stats - totalFailureCount stats

initSummaryStats :: SummaryStats
initSummaryStats =
  SummaryStats
    { totalBinaryCount = 0
    , totalStats = mempty
    , totalLLVMGenerated = 0
    , totalFailureCount = 0
    }

renderSummaryStats :: [ExplorationResult] -> String
renderSummaryStats results = formatSummary $ foldr processResult initSummaryStats results
 where
  processResult :: ExplorationResult -> SummaryStats -> SummaryStats
  processResult (ExplorationStats _summary stats llvmGenRes _logEvents) acc =
    acc
      { totalBinaryCount = 1 + totalBinaryCount acc
      , totalLLVMGenerated = totalLLVMGenerated acc + if llvmGenSuccess llvmGenRes then 1 else 0
      , totalStats = totalStats acc <> stats
      }
  processResult (ExplorationFailure _fPath _msg) acc =
    acc
      { totalBinaryCount = 1 + totalBinaryCount acc
      , totalFailureCount = 1 + totalFailureCount acc
      }
  formatSummary :: SummaryStats -> String
  formatSummary s =
    unlines $
      [ ""
      , printf "reopt successfully analyzed %d out of %d binaries:" (totalSuccessCount s) (totalBinaryCount s)
      , printf
          "Generated LLVM bitcode for %s out of %s binaries."
          (show $ totalLLVMGenerated s)
          (show $ totalBinaryCount s)
      ]
        ++ ppStats (totalStats s)

exploreAllElfInDirs ::
  LLVMOptions ->
  ReoptOptions ->
  [FilePath] ->
  IO [ExplorationResult]
exploreAllElfInDirs args opts paths = do
  elfFiles <- findAllElfFilesInDirs paths
  mapM (exploreBinary args opts (length elfFiles)) elfFiles

exploreBinary ::
  LLVMOptions ->
  ReoptOptions ->
  Int ->
  (Int, FilePath) ->
  IO ExplorationResult
exploreBinary args opts totalCount (index, fPath) = do
  handle handleSomeException $
    case loBinTimeoutInSec args of
      Nothing -> performRecovery
      Just sec ->
        -- timeout takes microseconds
        timeout (sec * 1000000) performRecovery >>= \case
          Nothing -> pure $ ExplorationFailure fPath "timeout"
          Just res -> pure res
 where
  lOpts = LoadOptions{loadOffset = Nothing}
  unnamedFunPrefix = BSC.pack "reopt"
  performRecovery :: IO ExplorationResult
  performRecovery = do
    hPutStrLn stderr $
      "["
        ++ show index
        ++ " of "
        ++ show totalCount
        ++ "] Analyzing "
        ++ fPath
        ++ " ..."
    bs <- checkedReadFile fPath
    summaryRef <- newIORef $ initReoptSummary fPath
    statsRef <- newIORef mempty
    let logger
          | roVerboseMode opts =
              joinLogEvents printLogEvent (recoverLogEvent summaryRef statsRef)
          | otherwise =
              recoverLogEvent summaryRef statsRef
    let annDecl = emptyAnnDeclarations
    hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs
    (os, _, recovOut, constraints) <-
      -- (os, _, recMod, constraints, _, logEvents) <-
      handleEitherWithExit
        =<< runReoptM logger (recoverX86Elf lOpts opts annDecl unnamedFunPrefix hdrInfo)
    res <-
      catch
        (generateLLVM os (recoveredModule recovOut) constraints)
        (handleFailure $ \_ errMsg -> LLVMGenFail errMsg)
    summary <- readIORef summaryRef
    stats <- readIORef statsRef
    pure $ ExplorationStats summary stats res (logEvents recovOut)
  handleSomeException :: SomeException -> IO ExplorationResult
  handleSomeException exn = pure $ ExplorationFailure fPath (show exn)

  generateLLVM ::
    X86OS ->
    RecoveredModule X86_64 ->
    ModuleConstraints X86_64 ->
    IO LLVMGenResult
  generateLLVM os recMod constraints = do
    let (objLLVM, _, _, events) =
          renderLLVMBitcode
            defaultLLVMGenOptions
            latestLLVMConfig
            os
            recMod
            constraints
    let sz = BSL.length $ BS.toLazyByteString objLLVM
    llvmRes <- seq sz $ do
      if roVerboseMode opts
        then hPutStrLn stderr $ "Completed " ++ fPath ++ "."
        else hPutStrLn stderr "  Done."
      let res = if sz < 0 then 0 else fromIntegral sz
      pure $ LLVMGenPass res events
    when (loEmitLLVM args) $ do
      let llvmPath = fPath ++ ".ll"
      mr <- runReoptM printLogEvent $ do
        reoptWriteBuilder LlvmFileType llvmPath objLLVM
      case mr of
        Left f -> hPrint stderr f
        Right () -> hPutStrLn stderr $ "LLVM written to " ++ llvmPath ++ "."
    pure llvmRes
  handleFailure :: (FilePath -> String -> a) -> SomeException -> IO a
  handleFailure mkResult e = do
    hPutStrLn stderr "Error raised during exploration"
    hPrint stderr e
    pure $ mkResult fPath (show e)

runLLVM :: Options -> LLVMOptions -> ReoptOptions -> IO ()
runLLVM _opts lopts ropts = do
  results <- exploreAllElfInDirs lopts ropts (loPaths lopts)
  mapM_ (\s -> hPutStr stderr ("\n" ++ renderExplorationResult s)) results
  hPutStrLn stderr $ renderSummaryStats results
  case loExportFnResultsPath lopts of
    Nothing -> pure ()
    Just exportPath -> do
      let hdrStr = intercalate "," summaryHeader
          rowsStr = map (intercalate ",") $ concatMap toRows results
      writeFile exportPath $ unlines $ hdrStr : rowsStr
      hPutStrLn stderr $ "CSV-formatted function result statistics written to " ++ exportPath ++ "."
  case loExportSummaryPath lopts of
    Nothing -> pure ()
    Just summaryPath -> do
      let individualSummaries = concatMap (\s -> "\n" ++ renderExplorationResult s) results
          overallSummary = renderSummaryStats results
      writeFile summaryPath $ individualSummaries ++ "\n" ++ overallSummary
      hPutStrLn stderr $ "Summary statistics written to " ++ summaryPath ++ "."
  case loExportLogCSVPath lopts of
    Nothing -> pure ()
    Just logEventsPath -> do
      let logEventsHeader = intercalate "," $ "File" : llvmLogEventHeader
          logEventsRows = concat $ mapMaybe renderLogEvents results
      writeFile logEventsPath $ unlines $ logEventsHeader : logEventsRows
      hPutStrLn stderr $ "LLVM logging events written to " ++ logEventsPath ++ "."
 where
  toRows :: ExplorationResult -> [[String]]
  toRows (ExplorationStats summary _stats _ _logEvents) = summaryRows summary
  toRows ExplorationFailure{} = []
