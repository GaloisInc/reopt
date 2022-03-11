{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Main (main) where

import Control.Exception (SomeException, catch, handle)
import Control.Monad (foldM, when)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ElfEdit as Elf
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (intercalate)
import Data.Macaw.X86 (X86_64)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Map as Map
import Data.Parameterized.Some
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Version (Version (versionBranch))
import Numeric.Natural (Natural)
import Paths_reopt (version)
import Reopt
  ( runReoptM,
    LoadOptions (LoadOptions, loadOffset),
    RecoveredModule,
    ReoptOptions (..),
    X86OS,
    LLVMLogEvent,
    llvmLogEventHeader,
    llvmLogEventToStrings,
    copyrightNotice,
    defaultLLVMGenOptions,
    emptyAnnDeclarations,
    latestLLVMConfig,
    parseElfHeaderInfo64,
    recoverX86Elf,
    renderLLVMBitcode,
    SomeArchitectureInfo(..),
    getElfArchInfo,
    discoverFunDebugInfo,
    debugInfoCacheFilePath,
    reoptHomeDir,
    getGdbDebugInfoDirs,
    defaultReoptOptions,
    reoptWriteBuilder
  )
import Reopt.Events
import Reopt.TypeInference.FunTypeMaps
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
import System.FilePath (splitFileName)
import System.IO (hPutStr, hPutStrLn, stderr, IOMode(..), withFile, hPrint)
import System.Directory
  (createDirectoryIfMissing, getSymbolicLinkTarget, canonicalizePath, createFileLink,
   withCurrentDirectory, doesFileExist, removeFile)
import System.Timeout (timeout)
import Text.Printf (printf)
import Reopt.TypeInference.ConstraintGen (ModuleConstraints)

reoptVersion :: String
reoptVersion = "Reopt binary explorer (reopt-explore) " ++ versionString ++ "."
  where
    [h, l, r] = versionBranch version
    versionString = show h ++ "." ++ show l ++ "." ++ show r

data ExploreMode =
  -- | Attempt to perform a full reopt run on each binary for statistics.
  ReoptExploreMode
  -- | Extract debug information for functions only, storing the result
  -- for later use by reopt.
  | DebugExploreMode

-- | Command line arguments.
data Args = Args
  { -- | What to do with each encountered binary.
    exploreMode :: ExploreMode,
    -- | Path to input program to optimize/export
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
    -- | Should we export log events?
    exportLogCSVPath :: !(Maybe FilePath),
    -- | Show help to user?
    showHelp :: !Bool,
    -- | Report output of individual binaries.
    verbose :: !Bool,
    -- | Emit generated LLVM next to binary with `.ll` suffix.
    emitLLVM :: !Bool,
    -- | Additional locations to search for dynamic dependencies.
    dynDepPath :: ![FilePath],
    -- | Additional locations to search for dynamic dependencies' debug info.
    dynDepDebugPath :: ![FilePath],
    -- | Timeout in seconds for analyzing a single binary.
    binTimeoutInSec :: !(Maybe Int)
  }

defaultArgs :: Args
defaultArgs =
  Args
    { exploreMode = ReoptExploreMode,
      programPaths = [],
      clangPath = "clang",
      exportFnResultsPath = Nothing,
      exportSummaryPath = Nothing,
      exportLogCSVPath = Nothing,
      showHelp = False,
      verbose = False,
      emitLLVM = True,
      dynDepPath = [],
      dynDepDebugPath = [],
      binTimeoutInSec = Nothing
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

exportLogFlag :: Flag Args
exportLogFlag = flagReq ["export-log"] upd "PATH" help
  where
    upd path old = Right $ old {exportLogCSVPath = Just path}
    help = "Path at which to write recovery and LLVM generation log events (as a CSV)."

showHelpFlag :: Flag Args
showHelpFlag = flagHelpSimple upd
  where
    upd old = old {showHelp = True}

verboseFlag :: Flag Args
verboseFlag = flagNone ["verbose", "v"] upd help
  where
    upd old = old {verbose = True}
    help = "Show output of individual binaries."

omitLLVMFlag :: Flag Args
omitLLVMFlag = flagNone ["omit-llvm"] upd help
  where
    upd old = old {emitLLVM = False}
    help = "Do not output generated LLVM."

debugInfoFlag :: Flag Args
debugInfoFlag = flagNone ["debug-info", "d"] upd help
  where
    upd old = old {exploreMode = DebugExploreMode}
    help = "Explore and export debug information for functions only."

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

-- | Used to add a path at which to search for dynamic dependencies.
dynDepPathFlag :: Flag Args
dynDepPathFlag = flagReq ["lib-dir"] upd "PATH" help
  where
    upd path args = Right $ args {dynDepPath = path:(dynDepPath args)}
    help = "Additional location to search for dynamic dependencies."


-- | Used to add a path at which to search for dynamic dependencies.
dynDepDebugPathFlag :: Flag Args
dynDepDebugPathFlag = flagReq ["debug-dir"] upd "PATH" help
  where
    upd path args = Right $ args {dynDepDebugPath = path:(dynDepDebugPath args)}
    help = "Additional location to search for dynamic dependencies' debug info."

binTimeoutInSecFlag :: Flag Args
binTimeoutInSecFlag = flagReq ["timeout"] upd "SEC" help
  where
    upd sec old = case reads sec of
                  ((n,_):_) -> Right $ old {binTimeoutInSec = Just n}
                  _    -> Left "Invalid timeout; please provide an integer."
    help = "Timeout for analyzing individual binaries (in seconds)."

arguments :: Mode Args
arguments = mode "reopt-explore" defaultArgs help filenameArg flags
  where
    help = reoptVersion ++ "\n" ++ copyrightNotice
    flags =
      [ showHelpFlag,
        clangPathFlag,
        exportFnResultsFlag,
        exportSummaryFlag,
        exportLogFlag,
        verboseFlag,
        debugInfoFlag,
        omitLLVMFlag,
        dynDepPathFlag,
        dynDepDebugPathFlag,
        binTimeoutInSecFlag
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

exploreBinary ::
  Args ->
  ReoptOptions ->
  Int ->
  (Int, FilePath) ->
  IO ExplorationResult
exploreBinary args opts totalCount (index, fPath) = do
  handle handleSomeException $
    case binTimeoutInSec args of
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
          (runReoptM logger $
            recoverX86Elf lOpts opts annDecl unnamedFunPrefix hdrInfo)
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
      when (emitLLVM args) $ do
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

-- | Summary of results from parsing the debug info of an elf file.
data ExploreDebugResult =
  ExploreDebugResult
  { -- | Absolute path to file debug info was gathered for.
    debugFileAbsPath :: !FilePath,
    -- | File debug info was cached in.
    debugFileCachePath :: !FilePath,
    -- | Number of functions debug info was gathered for.
    debugFnCount :: !Int,
    -- | Whether there was additional info gathered that was not used.
    debugSkippedInfo :: !Bool
  }


-- | Parse the debug section of an elf file, emit the gathered information
-- into a file in the REOPTHOME directory, and record some basic metrics
-- regarding the data collected.
exploreDebugInfo ::
  [ExploreDebugResult] ->
  FilePath ->
  IO [ExploreDebugResult]
exploreDebugInfo results fPath = do
  Some hdrInfo <- do
    bs <- checkedReadFile fPath
    case Elf.decodeElfHeaderInfo bs of
      Left (_, msg) -> do
        hPutStrLn stderr $ "Error reading " ++ fPath ++ ":"
        hPutStrLn stderr $ "  " ++ msg
        exitFailure
      Right (Elf.SomeElf hdr) ->
        pure $! Some hdr
  let hdr = Elf.header hdrInfo
  -- Get architecture specific information
  marchInfo <- getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr)
  (warnings, SomeArch ainfo _pltFn) <- handleEitherStringWithExit marchInfo
  mapM_ (hPutStrLn stderr) warnings
  mFnMap <- runReoptM printLogEvent $
              discoverFunDebugInfo hdrInfo ainfo
  fnMap <- handleEitherWithExit mFnMap
  cPath <- debugInfoCacheFilePath $ snd (splitFileName fPath)
  withFile cPath WriteMode $ \h -> hPutStrLn h (show $ nameTypeMap fnMap)

  absPath <- canonicalizePath fPath
  let addrTypeMapSz = Map.size $ addrTypeMap fnMap
  let noreturnMapSz = Map.size $ noreturnMap fnMap
  let result = ExploreDebugResult
                { debugFileAbsPath = absPath,
                  debugFileCachePath = cPath,
                  debugFnCount = Map.size $ nameTypeMap fnMap,
                  debugSkippedInfo = addrTypeMapSz > 0 || noreturnMapSz > 0
                }
  when (not $ 0 == addrTypeMapSz) $ do
    hPutStrLn stderr $ "WARNING: " ++ show addrTypeMapSz ++ " functions in debug info ignored (addrTypeMap) in " ++ fPath ++ "."
  when (not $ 0 == noreturnMapSz) $ do
    hPutStrLn stderr $ "WARNING: " ++ show noreturnMapSz ++ " functions in debug info ignored (noreturnMap) in "  ++ fPath ++ "."
  pure $ result : results

-- | Examine a symbolic link to see if it refers to a previously cached debug
-- library's debug info. If the link does correspond to such a file, create a
-- symbolic link in the debug cache to the other cached file. This is necessary
-- because many binaries list libraries they depend on which are actually
-- symbolic links to a library with a slightly different name, so by mimicking
-- these links in our debug cache we can find the cached debug info.
exploreLink ::
  Set FilePath ->
  () ->
  FilePath ->
  IO ()
exploreLink targets () linkPath = do
  tgtPath <- getSymbolicLinkTarget linkPath
  let (linkDir, linkName) = splitFileName linkPath
  absTgtPath <- withCurrentDirectory linkDir $ canonicalizePath tgtPath
  if not $ Set.member absTgtPath targets then pure ()
  else do
    newLinkDestPath <- debugInfoCacheFilePath $ snd $ splitFileName absTgtPath
    newLinkPath     <- debugInfoCacheFilePath $ linkName
    alreadyExists <- doesFileExist newLinkPath
    when alreadyExists $ removeFile newLinkPath
    createFileLink newLinkDestPath newLinkPath

renderDebugResult :: ExploreDebugResult -> String
renderDebugResult res =
  (debugFileAbsPath res)++":\n  "++(show $ debugFnCount res)++" functions' type info discovered in debug section."

renderDebugSummary :: FilePath -> [ExploreDebugResult] -> String
renderDebugSummary debugDir results =
  "\n\nDebug Exploration Totals"
  ++"\n  "++(show totalCnt)++" functions discovered in the debug sections of "++(show (length results))++" elf files."
  ++ maybeWarnMsg
  ++ "\n  Information cached at " ++ debugDir ++ "."
  where totalCnt = foldl (+) 0 $ map debugFnCount results
        warnCnt  = foldl (+) (0 :: Int) $ map (\res -> if debugSkippedInfo res then 1 else 0) results
        maybeWarnMsg = if warnCnt == 0
                       then ""
                       else "\n  "++(show warnCnt)++" elf files had debug type information which was not incorporated."

findAllElfFilesInDirs ::
  [FilePath] ->
  IO [(Int, FilePath)]
findAllElfFilesInDirs paths = do
  counter <- newIORef 1
  files <- foldM (withElfExeFilesInDir (recordFile counter)) [] paths
  pure $ reverse files
  where recordFile :: IORef Int -> [(Int, FilePath)] -> FilePath -> IO [(Int, FilePath)]
        recordFile counter ps p = do
          index <- readIORef counter
          modifyIORef' counter (+ 1)
          pure $ (index, p):ps

exploreAllElfInDirs ::
  Args ->
  ReoptOptions ->
  [FilePath] ->
  IO [ExplorationResult]
exploreAllElfInDirs args opts paths = do
  elfFiles <- findAllElfFilesInDirs paths
  mapM (exploreBinary args opts (length elfFiles)) elfFiles

main :: IO ()
main = do
  args <- getCommandLineArgs
  gdbDebugDirs <- getGdbDebugInfoDirs (verbose args)
  let opts = defaultReoptOptions { roVerboseMode = verbose args,
                                   roDynDepPaths = dynDepPath args,
                                   roDynDepDebugPaths = (dynDepDebugPath args) ++ gdbDebugDirs
                                 }
  case (showHelp args, programPaths args, exploreMode args) of
    (True, _, _) -> do
      print $ helpText [] HelpFormatAll arguments
    (False, [], _) -> do
      hPutStrLn stderr "Must provide at least one input program or directory to explore."
      hPutStrLn stderr "Use --help to see additional options."
      exitFailure
    (False, paths, ReoptExploreMode) -> do
      results <- exploreAllElfInDirs args opts paths
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
        Just summaryPath -> do
          let individualSummaries = concatMap (\s -> "\n" ++ renderExplorationResult s) results
              overallSummary = renderSummaryStats results
          writeFile summaryPath $ individualSummaries ++ "\n" ++ overallSummary
          hPutStrLn stderr $ "Summary statistics written to " ++ summaryPath ++ "."
      case exportLogCSVPath args of
        Nothing -> pure ()
        Just logEventsPath -> do
          let logEventsHeader = intercalate "," $ "File":llvmLogEventHeader
              logEventsRows   =  concat $ mapMaybe renderLogEvents results
          writeFile logEventsPath $ unlines $ logEventsHeader:logEventsRows
          hPutStrLn stderr $ "LLVM logging events written to " ++ logEventsPath ++ "."
    (False, paths, DebugExploreMode) -> do
      when (isJust $ exportFnResultsPath args) $ do
        hPutStrLn stderr "The --export-fn-results flag not compatible with the --debug-info flag."
        exitFailure
      when (isJust $ exportFnResultsPath args) $ do
        hPutStrLn stderr "The --export-summary flag not compatible with the --debug-info flag."
        exitFailure
      when (isJust $ exportLogCSVPath args) $ do
        hPutStrLn stderr "The --export-log flag not compatible with the --debug-info flag."
        exitFailure
      infoDir <- reoptHomeDir
      createDirectoryIfMissing True infoDir
      results <- foldM (withElfFilesInDir exploreDebugInfo) [] paths
      let tgts = Set.fromList $ map debugFileAbsPath results
      foldM (withSymLinksInDir (exploreLink tgts)) () paths
      mapM_ (\s -> hPutStr stderr ("\n" ++ renderDebugResult s)) results
      hPutStrLn stderr $ renderDebugSummary infoDir results


  where
    toRows :: ExplorationResult -> [[String]]
    toRows (ExplorationStats summary _stats _ _logEvents) = summaryRows summary
    toRows ExplorationFailure{} = []
