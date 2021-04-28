{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad (when, foldM)
import Control.Exception (catch, SomeException)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Macaw.Discovery ( DiscoveryOptions(..) )
import Data.Macaw.X86 ( X86_64 )
import Data.List ( intercalate )
import Data.Maybe ( isNothing )
import Data.Version ( Version(versionBranch) )
import Numeric.Natural ( Natural )
import Paths_reopt (version)
import Reopt
    ( LoadOptions(LoadOptions, loadOffset),
      ReoptOptions(ReoptOptions, roIncluded, roExcluded),
      copyrightNotice,
      ReoptStats(..),
      statsHeader,
      recoverFunctions,
      renderLLVMBitcode,
      defaultLLVMGenOptions,
      latestLLVMConfig,
      statsRows,
      RecoveredModule,
      X86OS
    )
import Reopt.Utils.Dir
import System.Console.CmdArgs.Explicit
    ( process, flagNone, flagReq, mode, Arg(..), Flag, Mode )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

reoptVersion :: String
reoptVersion = "Reopt binary explorer (reopt-explore) "  ++ versionString ++ "."
  where [h,l,r] = versionBranch version
        versionString = show h ++ "." ++ show l ++ "." ++ show r


-- | Command line arguments.
data Args
   = Args { programPaths  :: ![FilePath]
            -- ^ Path to input program to optimize/export
          , clangPath :: !FilePath
            -- ^ Path to `clang` command.
            --
            -- This is only used as a C preprocessor for parsing
            -- header files.
          , printStats :: !Bool
            -- ^ Should we print discovery/recovery statistics to stdout?
          , exportStatsPath :: !(Maybe FilePath)
            -- ^ Should we export discovery/recovery statistics?
          }

defaultArgs :: Args
defaultArgs =
  Args
  { programPaths  = []
  , clangPath = "clang"
  , printStats = False
  , exportStatsPath = Nothing
  }

-- | Flag to set clang path.
clangPathFlag :: Flag Args
clangPathFlag =
  let upd s old = Right $ old {clangPath = s}
      help = printf "Path to clang (default "++(clangPath defaultArgs)++")"
  in flagReq [ "clang" ] upd "PATH" help

statsPrintFlag :: Flag Args
statsPrintFlag = flagNone [ "print-stats" ] upd help
  where upd old = old { printStats = True}
        help = "Print discovery/recovery statistics."

statsExportFlag :: Flag Args
statsExportFlag = flagReq [ "export-stats" ] upd "PATH" help
  where upd path old = Right $ old { exportStatsPath = Just path }
        help = "Path at which to write discovery/recovery statistics."

-- | Flag to set the path to the binary to analyze.
filenameArg :: Arg Args
filenameArg = Arg { argValue = addFilename
                  , argType = "FILE"
                  , argRequire = False
                  }
  where addFilename :: String -> Args -> Either String Args
        addFilename nm a = Right (a { programPaths = nm:(programPaths a) })


arguments :: Mode Args
arguments = mode "reopt-explore" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ clangPathFlag
                , statsPrintFlag
                , statsExportFlag
                ]

getCommandLineArgs :: IO Args
getCommandLineArgs = do
  argStrings <- getArgs
  case process arguments argStrings of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right v -> return v

data LLVMGenResult
  = LLVMGenFail String -- ^ Error message
  | LLVMGenPass Natural -- ^ How many bytes of LLVM bitcode were generated.

llvmGenSuccess :: LLVMGenResult -> Bool
llvmGenSuccess LLVMGenPass{} = True
llvmGenSuccess LLVMGenFail{} = False

data ExplorationResult
  = ExplorationStats ReoptStats LLVMGenResult
  | ExplorationError FilePath String

binStats :: ReoptStats -> String -> String
binStats stats llvmGen =
  unlines
    [ statsBinary stats
    , "  Discovered:  " ++ show (statsFnDiscoveredCount stats)
    , "  Recovered:   " ++ show (statsFnRecoveredCount stats)
    , "  PLT entries: " ++ show (statsFnPLTSkippedCount stats)
    , "  Failures:    " ++ show (statsFnFailedCount stats)
    , "  Errors:      " ++ show (statsErrorCount stats)
    , "  LLVM:        " ++ llvmGen
    ]

renderExplorationResult :: ExplorationResult -> String
renderExplorationResult =
  \case
    ExplorationStats stats (LLVMGenPass _n)  -> do
      binStats stats "Success"
    ExplorationStats stats (LLVMGenFail errMsg)  ->
      binStats stats ("Failed: " ++ errMsg)
    ExplorationError fpath errMsg ->
      unlines $
        [ fpath
        , "  Fatal error:"]
        ++ ((\m -> "    " ++ m) <$> lines errMsg)

exploreBinary ::
  Args ->
  [ExplorationResult] ->
  FilePath ->
  IO [ExplorationResult]
exploreBinary args results fPath = do
  hPutStrLn stderr $ "Analyzing binary " ++ fPath ++ " ..."
  result <- catch performRecovery
                  (handleFailure ExplorationError)
  pure $ result:results
  where
    lOpts = LoadOptions { loadOffset = Nothing }
    dOpts = DiscoveryOptions
            { exploreFunctionSymbols = False
            , exploreCodeAddrInMem   = False
            , logAtAnalyzeFunction   = True
            , logAtAnalyzeBlock      = False
            }
    rOpts = ReoptOptions { roIncluded = []
                          , roExcluded = []
                          }
    hdrPath = Nothing
    unnamedFunPrefix = BSC.pack "reopt"
    performRecovery :: IO ExplorationResult
    performRecovery = do
        (os, recMod, stats) <- recoverFunctions fPath
                                                (clangPath args)
                                                lOpts
                                                dOpts
                                                rOpts
                                                hdrPath
                                                unnamedFunPrefix
        hPutStrLn stderr $ "Completed analyzing binary " ++ fPath ++ "."
        catch (do sz <- generateLLVM os recMod; pure $ ExplorationStats stats $ LLVMGenPass sz)
              (handleFailure $ \_ errMsg -> ExplorationStats stats $ LLVMGenFail errMsg)
    -- | Generate LLVM bitcode and return the number of bytes generated.
    generateLLVM :: X86OS -> RecoveredModule X86_64 -> IO Natural
    generateLLVM os recMod = do
        hPutStrLn stderr $ "Generating LLVM bitcode..."
        let (llvm, _) = renderLLVMBitcode defaultLLVMGenOptions
                                          latestLLVMConfig
                                          os
                                          recMod
        let sz = BSL.length $ BS.toLazyByteString llvm
        hPutStrLn stderr $ (show sz) ++ " bytes of LLVM bitcode generated."
        pure $ if sz < 0 then 0 else fromIntegral sz
    handleFailure :: (FilePath -> String -> ExplorationResult) -> SomeException -> IO ExplorationResult
    handleFailure mkResult e = do
        hPutStrLn stderr "Error raised during exploration"
        hPutStrLn stderr $ show e
        pure $ mkResult fPath (show e)


data SummaryStats =
  SummaryStats
  { totalBinaryCount :: Natural
  -- ^ Which binary are these statistics for?
  , totalFnDiscoveredCount :: Natural
  -- ^ Number of discovered functions.
  , totalFnRecoveredCount :: Natural
  -- ^ Number of successfully recovered functions.
  , totalFnPLTSkippedCount :: Natural
  -- ^ Number of skipped PLT stubs.
  , totalFnFailedCount :: Natural
  -- ^ Number of functions which failed during recovery.
  , totalErrorCount :: Natural
  -- ^ Overall number of errors encountered while exploring binaries.
  , totalFailedBinaries :: Natural
  -- ^ Number of binaries which failed to complete discovery.
  , totalLLVMGenerated :: Natural
  -- ^ Number of binaries which we successfully produced LLVM bitcode for.
  }

initSummaryStats :: SummaryStats
initSummaryStats = SummaryStats 0 0 0 0 0 0 0 0

renderSummaryStats :: [ExplorationResult] -> String
renderSummaryStats results = formatSummary $ foldr processResult initSummaryStats results
  where
    processResult :: ExplorationResult -> SummaryStats -> SummaryStats
    processResult (ExplorationStats s llvmGenRes) acc =
      acc { totalBinaryCount = 1 + (totalBinaryCount acc)
          , totalFnDiscoveredCount = (statsFnDiscoveredCount s) + (totalFnDiscoveredCount acc)
          , totalFnRecoveredCount = (statsFnRecoveredCount s) + (totalFnRecoveredCount acc)
          , totalFnPLTSkippedCount = (statsFnPLTSkippedCount s) + (totalFnPLTSkippedCount acc)
          , totalFnFailedCount = (statsFnFailedCount s) + (totalFnFailedCount acc)
          , totalErrorCount = (statsErrorCount s) + (totalErrorCount acc)
          , totalLLVMGenerated = (totalLLVMGenerated acc) + (if llvmGenSuccess llvmGenRes then 1 else 0)
          }
    processResult (ExplorationError _ _) acc =
      acc { totalBinaryCount = 1 + (totalBinaryCount acc)
          , totalFailedBinaries = 1 + (totalFailedBinaries acc)
          }
    formatSummary :: SummaryStats -> String
    formatSummary s =
      if (totalFnDiscoveredCount s) == 0
      then "\nreopt discovered no functions after exploring "++(show $ totalBinaryCount s)++" binaries."
      else
        let passed :: Double = (fromIntegral $ totalFnRecoveredCount s) / (fromIntegral $  totalFnDiscoveredCount s)
            passedStr = printf " (%.2f%%)" (passed * 100.0)
            failed :: Double = (fromIntegral $ totalFnFailedCount s) / (fromIntegral $  totalFnDiscoveredCount s)
            failedStr = printf " (%.2f%%)" (failed * 100.0)
            skipped :: Double = (fromIntegral $ totalFnPLTSkippedCount s) / (fromIntegral $  totalFnDiscoveredCount s)
            skippedStr = printf " (%.2f%%)" (skipped * 100.0)
        in "\nrepot generated LLVM bitcode for "++(show $ totalLLVMGenerated s)++" out of "++(show $ totalBinaryCount s)++" binaries."++
           "\nreopt discovered " ++ (show (totalFnDiscoveredCount s)) ++ " functions while exploring "++(show $ totalBinaryCount s)++" binaries:" ++
           "\n  recovery succeeded: " ++ (show (totalFnDiscoveredCount s)) ++ passedStr ++
           "\n     recovery failed: " ++ (show (totalFnFailedCount s)) ++ failedStr ++
           "\n    skipped PLT stub: " ++ (show (totalFnPLTSkippedCount s)) ++ skippedStr ++
           "\n"++(show $ totalErrorCount s)++" errors occurred during exploration."





main :: IO ()
main = do
  args <- getCommandLineArgs
  case programPaths args of
    [] -> do
      hPutStrLn stderr "Must provide at least one input program or directory to explore."
      exitFailure
    paths -> do
      when ((not $ printStats args) && isNothing (exportStatsPath args)) $ do
        hPutStrLn stderr "There is nothing to be done."
        hPutStrLn stderr "Please provide either the --print-stats and/or --export-stats=PATH flag(s)."
        exitFailure
      results <- foldM (withElfFilesInDir (exploreBinary args)) [] paths
      when (printStats args) $ do
        mapM_ (\s -> hPutStrLn stderr ("\n" ++ renderExplorationResult s)) results
        hPutStrLn stderr $ renderSummaryStats results
      case exportStatsPath args of
        Nothing -> pure ()
        Just exportPath -> do
          let hdrStr = intercalate "," statsHeader
              rowsStr = map (intercalate ",") $ concatMap toRows results
          writeFile exportPath $ unlines $ hdrStr:rowsStr
          hPutStrLn stderr $ "CSV-formatted statistics written to " ++ exportPath
  where
    toRows :: ExplorationResult -> [[String]]
    toRows (ExplorationStats stats _) = statsRows stats
    toRows (ExplorationError _ _) = [[]]



