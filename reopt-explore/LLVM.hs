{-# LANGUAGE LambdaCase #-}

module LLVM (runLLVM) where

import           Control.Exception                 (SomeException, catch,
                                                    handle)
import           Control.Monad                     (when)
import qualified Data.ByteString.Builder           as BS
import qualified Data.ByteString.Char8             as BSC
import qualified Data.ByteString.Lazy              as BSL
import           Data.IORef                        (newIORef, readIORef)
import           Data.List                         (intercalate)
import           Data.Macaw.X86                    (X86_64)
import           Data.Maybe                        (mapMaybe)
import           Numeric.Natural                   (Natural)
import           System.IO                         (hPrint, hPutStr, hPutStrLn,
                                                    stderr)
import           System.Timeout                    (timeout)
import           Text.Printf                       (printf)

import           Reopt                             (LLVMLogEvent,
                                                    LoadOptions (LoadOptions, loadOffset),
                                                    RecoveredModule,
                                                    ReoptOptions (..), X86OS,
                                                    defaultLLVMGenOptions,
                                                    emptyAnnDeclarations,
                                                    latestLLVMConfig,
                                                    llvmLogEventHeader,
                                                    llvmLogEventToStrings,
                                                    parseElfHeaderInfo64,
                                                    recoverX86Elf,
                                                    renderLLVMBitcode,
                                                    reoptWriteBuilder,
                                                    runReoptM)
import           Reopt.Events
import           Reopt.TypeInference.ConstraintGen (ModuleConstraints)
import           Reopt.Utils.Exit

import           CommandLine
import           Common

data LLVMGenResult
  = -- | Error message
    LLVMGenFail String
  | -- | How many bytes of LLVM bitcode were generated and any logging.
    LLVMGenPass Natural ![LLVMLogEvent]

llvmGenSuccess :: LLVMGenResult -> Bool
llvmGenSuccess LLVMGenPass {} = True
llvmGenSuccess LLVMGenFail {} = False

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
  summaryBinaryPath summary ++ "\n"
    ++ unlines (ppIndent (ppStats stats ++ ["LLVM generation status: " ++ llvmGen]))
renderExplorationResult (ExplorationFailure path msg) =
  "Exploration of " ++ path ++ " failed: " ++ msg

renderLogEvents :: ExplorationResult -> Maybe [String]
renderLogEvents (ExplorationStats summary _stats lgen logEvents) =
    Just $ map renderRow $ logEvents ++ llvmGenLogEvents lgen
  where binPath = summaryBinaryPath summary
        renderRow event = intercalate "," $ binPath:(llvmLogEventToStrings event)
renderLogEvents (ExplorationFailure _ _) = Nothing

data SummaryStats = SummaryStats
  { -- | How many binaries were analyzed?
    totalBinaryCount :: !Natural,
    -- | Summary of stats from individual runs
    totalStats :: !ReoptStats,
    -- | Number of binaries which we successfully produced LLVM bitcode for.
    totalLLVMGenerated :: !Natural,
    -- | Total number of complete failures.
    totalFailureCount :: !Natural
  }

totalSuccessCount :: SummaryStats -> Natural
totalSuccessCount stats = (totalBinaryCount stats) - (totalFailureCount stats)

initSummaryStats :: SummaryStats
initSummaryStats =
  SummaryStats
    { totalBinaryCount = 0,
      totalStats = mempty,
      totalLLVMGenerated = 0,
      totalFailureCount = 0
    }

renderSummaryStats :: [ExplorationResult] -> String
renderSummaryStats results = formatSummary $ foldr processResult initSummaryStats results
  where
    processResult :: ExplorationResult -> SummaryStats -> SummaryStats
    processResult (ExplorationStats _summary stats llvmGenRes _logEvents) acc =
      acc
        { totalBinaryCount = 1 + totalBinaryCount acc,
          totalLLVMGenerated = totalLLVMGenerated acc + if llvmGenSuccess llvmGenRes then 1 else 0,
          totalStats = totalStats acc <> stats
        }
    processResult (ExplorationFailure _fPath _msg) acc =
      acc
        { totalBinaryCount  = 1 + totalBinaryCount acc,
          totalFailureCount = 1 + totalFailureCount acc
        }
    formatSummary :: SummaryStats -> String
    formatSummary s =
      unlines $
        [ "",
          printf "reopt successfully analyzed %d out of %d binaries:" (totalSuccessCount s) (totalBinaryCount s),
          printf
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
    lOpts = LoadOptions {loadOffset = Nothing}
    unnamedFunPrefix = BSC.pack "reopt"
    performRecovery :: IO ExplorationResult
    performRecovery = do
      hPutStrLn stderr $ "[" ++ (show index) ++ " of " ++ (show totalCount)
                         ++ "] Analyzing " ++ fPath ++ " ..."
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
      (os, _, recMod, constraints, _, logEvents) <-
        handleEitherWithExit =<<
          runReoptM logger (recoverX86Elf lOpts opts annDecl unnamedFunPrefix hdrInfo)
      res <-
        catch
          (generateLLVM os recMod constraints)
          (handleFailure $ \_ errMsg -> LLVMGenFail errMsg)
      summary <- readIORef summaryRef
      stats <- readIORef statsRef
      pure $ ExplorationStats summary stats res logEvents
    handleSomeException :: SomeException -> IO ExplorationResult
    handleSomeException exn = pure $ ExplorationFailure fPath (show exn)

    generateLLVM ::
      X86OS ->
      RecoveredModule X86_64 ->
      ModuleConstraints X86_64 ->
      IO LLVMGenResult
    generateLLVM os recMod constraints = do
      let (objLLVM, _, _, logEvents) =
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
          else hPutStrLn stderr $ "  Done."
        let res = if sz < 0 then 0 else fromIntegral sz
        pure $ (LLVMGenPass res logEvents)
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
      hPutStrLn stderr $ show e
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
      let logEventsHeader = intercalate "," $ "File":llvmLogEventHeader
          logEventsRows   =  concat $ mapMaybe renderLogEvents results
      writeFile logEventsPath $ unlines $ logEventsHeader:logEventsRows
      hPutStrLn stderr $ "LLVM logging events written to " ++ logEventsPath ++ "."
  where
    toRows :: ExplorationResult -> [[String]]
    toRows (ExplorationStats summary _stats _ _logEvents) = summaryRows summary
    toRows ExplorationFailure{} = []
