{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Reopt
  ( ReoptOptions (..),
    getGdbDebugInfoDirs,
    defaultReoptOptions
  )

import CommandLine
import Residual
import LLVM

-- -- | Summary of results from parsing the debug info of an elf file.
-- data ExploreDebugResult =
--   ExploreDebugResult
--   { -- | Absolute path to file debug info was gathered for.
--     debugFileAbsPath :: !FilePath,
--     -- | File debug info was cached in.
--     debugFileCachePath :: !FilePath,
--     -- | Number of functions debug info was gathered for.
--     debugFnCount :: !Int,
--     -- | Whether there was additional info gathered that was not used.
--     debugSkippedInfo :: !Bool
--   }


-- -- | Parse the debug section of an elf file, emit the gathered information
-- -- into a file in the REOPTHOME directory, and record some basic metrics
-- -- regarding the data collected.
-- exploreDebugInfo ::
--   [ExploreDebugResult] ->
--   FilePath ->
--   IO [ExploreDebugResult]
-- exploreDebugInfo results fPath = do
--   Some hdrInfo <- do
--     bs <- checkedReadFile fPath
--     case Elf.decodeElfHeaderInfo bs of
--       Left (_, msg) -> do
--         hPutStrLn stderr $ "Error reading " ++ fPath ++ ":"
--         hPutStrLn stderr $ "  " ++ msg
--         exitFailure
--       Right (Elf.SomeElf hdr) ->
--         pure $! Some hdr
--   let hdr = Elf.header hdrInfo
--   -- Get architecture specific information
--   marchInfo <- getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr)
--   (warnings, SomeArch ainfo _pltFn) <- handleEitherStringWithExit marchInfo
--   mapM_ (hPutStrLn stderr) warnings
--   mFnMap <- runReoptM printLogEvent $
--               discoverFunDebugInfo hdrInfo ainfo
--   fnMap <- handleEitherWithExit mFnMap
--   cPath <- debugInfoCacheFilePath $ snd (splitFileName fPath)
--   withFile cPath WriteMode $ \h -> hPutStrLn h (show $ nameTypeMap fnMap)

--   absPath <- canonicalizePath fPath
--   let addrTypeMapSz = Map.size $ addrTypeMap fnMap
--   let noreturnMapSz = Map.size $ noreturnMap fnMap
--   let result = ExploreDebugResult
--                 { debugFileAbsPath = absPath,
--                   debugFileCachePath = cPath,
--                   debugFnCount = Map.size $ nameTypeMap fnMap,
--                   debugSkippedInfo = addrTypeMapSz > 0 || noreturnMapSz > 0
--                 }
--   when (not $ 0 == addrTypeMapSz) $ do
--     hPutStrLn stderr $ "WARNING: " ++ show addrTypeMapSz ++ " functions in debug info ignored (addrTypeMap) in " ++ fPath ++ "."
--   when (not $ 0 == noreturnMapSz) $ do
--     hPutStrLn stderr $ "WARNING: " ++ show noreturnMapSz ++ " functions in debug info ignored (noreturnMap) in "  ++ fPath ++ "."
--   pure $ result : results

-- -- | Examine a symbolic link to see if it refers to a previously cached debug
-- -- library's debug info. If the link does correspond to such a file, create a
-- -- symbolic link in the debug cache to the other cached file. This is necessary
-- -- because many binaries list libraries they depend on which are actually
-- -- symbolic links to a library with a slightly different name, so by mimicking
-- -- these links in our debug cache we can find the cached debug info.
-- exploreLink ::
--   Set FilePath ->
--   () ->
--   FilePath ->
--   IO ()
-- exploreLink targets () linkPath = do
--   tgtPath <- getSymbolicLinkTarget linkPath
--   let (linkDir, linkName) = splitFileName linkPath
--   absTgtPath <- withCurrentDirectory linkDir $ canonicalizePath tgtPath
--   if not $ Set.member absTgtPath targets then pure ()
--   else do
--     newLinkDestPath <- debugInfoCacheFilePath $ snd $ splitFileName absTgtPath
--     newLinkPath     <- debugInfoCacheFilePath $ linkName
--     alreadyExists <- doesFileExist newLinkPath
--     when alreadyExists $ removeFile newLinkPath
--     createFileLink newLinkDestPath newLinkPath

-- renderDebugResult :: ExploreDebugResult -> String
-- renderDebugResult res =
--   (debugFileAbsPath res)++":\n  "++(show $ debugFnCount res)++" functions' type info discovered in debug section."

-- renderDebugSummary :: FilePath -> [ExploreDebugResult] -> String
-- renderDebugSummary debugDir results =
--   "\n\nDebug Exploration Totals"
--   ++"\n  "++(show totalCnt)++" functions discovered in the debug sections of "++(show (length results))++" elf files."
--   ++ maybeWarnMsg
--   ++ "\n  Information cached at " ++ debugDir ++ "."
--   where totalCnt = foldl (+) 0 $ map debugFnCount results
--         warnCnt  = foldl (+) (0 :: Int) $ map (\res -> if debugSkippedInfo res then 1 else 0) results
--         maybeWarnMsg = if warnCnt == 0
--                        then ""
--                        else "\n  "++(show warnCnt)++" elf files had debug type information which was not incorporated."

main :: IO ()
main = do
  opts <- getOptions
  gdbDebugDirs <- getGdbDebugInfoDirs (optVerbose opts)
  let ropts = defaultReoptOptions { roVerboseMode = optVerbose opts,
                                    roDynDepPaths = optDynDepPath opts,
                                    roDynDepDebugPaths = optDynDepDebugPath opts ++ gdbDebugDirs
                                  }
  case optCommand opts of
    RunResidual gopts -> runResidual opts gopts ropts
    RunLLVM     lopts -> runLLVM opts lopts ropts

{-  
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

-}
