{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (foldM)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (newIORef, readIORef)
import Data.List (intercalate)
import Data.Macaw.Discovery (DiscoveryOptions (..))
import Data.Macaw.X86 (X86_64)
import Data.Version (Version (versionBranch))
import Numeric.Natural (Natural)
import Paths_reopt (version)
import Reopt
  ( runReoptM,
    LoadOptions (LoadOptions, loadOffset),
    RecoveredModule,
    ReoptOptions (ReoptOptions, roExcluded, roIncluded),
    X86OS,
    copyrightNotice,
    defaultLLVMGenOptions,
    emptyAnnDeclarations,
    latestLLVMConfig,
    parseElfHeaderInfo64,
    recoverX86Elf,
    renderLLVMBitcode,
  )
import Reopt.Events
import Reopt.Utils.Dir
import Reopt.Utils.Exit
import System.Console.CmdArgs.Explicit
  ( Arg (..),
    Flag,
    HelpFormat (..),
    Mode,
    flagHelpSimple,
    flagNone,
    flagReq,
    helpText,
    mode,
    process,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)
import Text.Printf (printf)

reoptVersion :: String
reoptVersion = "Reopt binary explorer (reopt-explore) " ++ versionString ++ "."
  where
    [h, l, r] = versionBranch version
    versionString = show h ++ "." ++ show l ++ "." ++ show r

-- | Command line arguments.
data Args = Args
  { -- | Path to input program to optimize/export
    programPaths :: ![FilePath],
    -- | Path to `clang` command.
    --
    -- This is only used as a C preprocessor for parsing
    -- header files.
    clangPath :: !FilePath,
    -- | Should we export function discovery/recovery results?
    exportFnResultsPath :: !(Maybe FilePath),
    -- | Should we export summary information?
    exportSummaryPath :: !(Maybe FilePath),
    -- | Show help to user?
    showHelp :: !Bool,
    -- | Report output of individual binaries.
    verbose :: !Bool
  }

defaultArgs :: Args
defaultArgs =
  Args
    { programPaths = [],
      clangPath = "clang",
      exportFnResultsPath = Nothing,
      exportSummaryPath = Nothing,
      showHelp = False,
      verbose = False
    }

-- | Flag to set clang path.
clangPathFlag :: Flag Args
clangPathFlag =
  let upd s old = Right $ old {clangPath = s}
      help = printf "Path to clang (default " ++ (clangPath defaultArgs) ++ ")"
   in flagReq ["clang"] upd "PATH" help

exportFnResultsFlag :: Flag Args
exportFnResultsFlag = flagReq ["export-fn-results"] upd "PATH" help
  where
    upd path old = Right $ old {exportFnResultsPath = Just path}
    help = "Path at which to write function discovery/recovery results."

exportSummaryFlag :: Flag Args
exportSummaryFlag = flagReq ["export-summary"] upd "PATH" help
  where
    upd path old = Right $ old {exportSummaryPath = Just path}
    help = "Path at which to write discovery/recovery summary statistics."

showHelpFlag :: Flag Args
showHelpFlag = flagHelpSimple upd
  where
    upd old = old {showHelp = True}

verboseFlag :: Flag Args
verboseFlag = flagNone ["verbose", "v"] upd help
  where
    upd old = old {verbose = True}
    help = "Show output of individual binaries."

-- | Flag to set the path to the binary to analyze.
filenameArg :: Arg Args
filenameArg =
  Arg
    { argValue = addFilename,
      argType = "PATH ...",
      argRequire = False
    }
  where
    addFilename :: String -> Args -> Either String Args
    addFilename nm a = Right (a {programPaths = nm : (programPaths a)})

arguments :: Mode Args
arguments = mode "reopt-explore" defaultArgs help filenameArg flags
  where
    help = reoptVersion ++ "\n" ++ copyrightNotice
    flags =
      [ showHelpFlag,
        clangPathFlag,
        exportFnResultsFlag,
        exportSummaryFlag,
        verboseFlag
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
  = -- | Error message
    LLVMGenFail String
  | -- | How many bytes of LLVM bitcode were generated.
    LLVMGenPass Natural

llvmGenSuccess :: LLVMGenResult -> Bool
llvmGenSuccess LLVMGenPass {} = True
llvmGenSuccess LLVMGenFail {} = False

data ExplorationResult
  = ExplorationStats ReoptSummary ReoptStats LLVMGenResult

renderExplorationResult :: ExplorationResult -> String
renderExplorationResult (ExplorationStats summary stats lgen) = do
  let llvmGen = case lgen of
        LLVMGenPass _ -> "Succeeded."
        LLVMGenFail errMsg -> "Failed: " ++ errMsg
  summaryBinaryPath summary ++ "\n"
    ++ unlines (ppIndent (ppStats stats ++ ["LLVM generation status: " ++ llvmGen]))

exploreBinary ::
  Args ->
  [ExplorationResult] ->
  FilePath ->
  IO [ExplorationResult]
exploreBinary args results fPath = do
  result <- performRecovery
  pure $ result : results
  where
    lOpts = LoadOptions {loadOffset = Nothing}
    dOpts =
      DiscoveryOptions
        { exploreFunctionSymbols = False,
          exploreCodeAddrInMem = False,
          logAtAnalyzeFunction = True,
          logAtAnalyzeBlock = False
        }
    rOpts =
      ReoptOptions
        { roIncluded = [],
          roExcluded = []
        }
    unnamedFunPrefix = BSC.pack "reopt"
    performRecovery :: IO ExplorationResult
    performRecovery = do
      hPutStrLn stderr $ "Analyzing " ++ fPath ++ " ..."
      bs <- checkedReadFile fPath
      summaryRef <- newIORef $ initReoptSummary fPath
      statsRef <- newIORef mempty
      let logger
            | verbose args =
              joinLogEvents printLogEvent (recoverLogEvent summaryRef statsRef)
            | otherwise =
              recoverLogEvent summaryRef statsRef
      let annDecl = emptyAnnDeclarations
      hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs
      mr <-
        runReoptM logger $
          recoverX86Elf lOpts dOpts rOpts annDecl unnamedFunPrefix hdrInfo
      (os, _, recMod, _) <- handleEitherWithExit mr
      res <-
        catch
          (generateLLVM os recMod)
          (handleFailure $ \_ errMsg -> LLVMGenFail errMsg)
      summary <- readIORef summaryRef
      stats <- readIORef statsRef
      pure $ ExplorationStats summary stats res

    generateLLVM :: X86OS -> RecoveredModule X86_64 -> IO LLVMGenResult
    generateLLVM os recMod = do
      let (llvm, _) =
            renderLLVMBitcode
              defaultLLVMGenOptions
              latestLLVMConfig
              os
              recMod
      let sz = BSL.length $ BS.toLazyByteString llvm
      seq sz $ do
        if verbose args
          then hPutStrLn stderr $ "Completed " ++ fPath ++ "."
          else hPutStrLn stderr $ "  Done."
        pure $ LLVMGenPass $ if sz < 0 then 0 else fromIntegral sz
    handleFailure :: (FilePath -> String -> a) -> SomeException -> IO a
    handleFailure mkResult e = do
      hPutStrLn stderr "Error raised during exploration"
      hPutStrLn stderr $ show e
      pure $ mkResult fPath (show e)

data SummaryStats = SummaryStats
  { -- | How many binaries were analyzed?
    totalBinaryCount :: !Natural,
    -- | Summary of stats from individual runs
    totalStats :: !ReoptStats,
    -- | Number of binaries which we successfully produced LLVM bitcode for.
    totalLLVMGenerated :: !Natural
  }

initSummaryStats :: SummaryStats
initSummaryStats =
  SummaryStats
    { totalBinaryCount = 0,
      totalStats = mempty,
      totalLLVMGenerated = 0
    }

renderSummaryStats :: [ExplorationResult] -> String
renderSummaryStats results = formatSummary $ foldr processResult initSummaryStats results
  where
    processResult :: ExplorationResult -> SummaryStats -> SummaryStats
    processResult (ExplorationStats _summary stats llvmGenRes) acc =
      acc
        { totalBinaryCount = 1 + totalBinaryCount acc,
          totalLLVMGenerated = totalLLVMGenerated acc + if llvmGenSuccess llvmGenRes then 1 else 0,
          totalStats = totalStats acc <> stats
        }
    formatSummary :: SummaryStats -> String
    formatSummary s =
      unlines $
        [ "",
          printf "reopt analyzed %d binaries:" (totalBinaryCount s),
          printf
            "Generated LLVM bitcode for %s out of %s binaries."
            (show $ totalLLVMGenerated s)
            (show $ totalBinaryCount s)
        ]
          ++ ppStats (totalStats s)

main :: IO ()
main = do
  args <- getCommandLineArgs
  case (showHelp args, programPaths args) of
    (True, _) -> do
      print $ helpText [] HelpFormatAll arguments
    (False, []) -> do
      hPutStrLn stderr "Must provide at least one input program or directory to explore."
      hPutStrLn stderr "Use --help to see additional options."
      exitFailure
    (False, paths) -> do
      results <- foldM (withElfFilesInDir (exploreBinary args)) [] paths
      mapM_ (\s -> hPutStr stderr ("\n" ++ renderExplorationResult s)) results
      hPutStrLn stderr $ renderSummaryStats results
      case exportFnResultsPath args of
        Nothing -> pure ()
        Just exportPath -> do
          let hdrStr = intercalate "," summaryHeader
              rowsStr = map (intercalate ",") $ concatMap toRows results
          writeFile exportPath $ unlines $ hdrStr : rowsStr
          hPutStrLn stderr $ "CSV-formatted function result statistics written to " ++ exportPath ++ "."
      case exportSummaryPath args of
        Nothing -> pure ()
        Just exportPath -> do
          let individualSummaries = concatMap (\s -> "\n" ++ renderExplorationResult s) results
              overallSummary = renderSummaryStats results
          writeFile exportPath $ individualSummaries ++ "\n" ++ overallSummary
          hPutStrLn stderr $ "Summary statistics written to " ++ exportPath ++ "."
  where
    toRows :: ExplorationResult -> [[String]]
    toRows (ExplorationStats summary _stats _) = summaryRows summary