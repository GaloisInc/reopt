{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad (foldM)
import Control.Exception (catch, SomeException)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List ( intercalate, foldl', unfoldr, intersperse)
import Data.Macaw.Discovery ( DiscoveryOptions(..) )
import Data.Macaw.X86 ( X86_64 )
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Version ( Version(versionBranch) )
import Numeric.Natural ( Natural )
import Paths_reopt (version)
import Reopt
    ( LoadOptions(LoadOptions, loadOffset),
      ReoptOptions(ReoptOptions, roIncluded, roExcluded),
      ReoptStepTag(..),
      ReoptErrorTag(..),
      copyrightNotice,
      ReoptStats(..),
      statsHeader,
      recoverFunctions,
      renderLLVMBitcode,
      defaultLLVMGenOptions,
      latestLLVMConfig,
      renderAllFailures,
      stepErrorCount,
      mergeFnFailures,
      statsRows,
      RecoveredModule,
      X86OS
    )
import Reopt.Utils.Dir
import System.Console.CmdArgs.Explicit
    ( process, flagReq, mode, Arg(..), Flag, Mode )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)
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
          , exportStatsPath :: !(Maybe FilePath)
            -- ^ Should we export discovery/recovery statistics?
          }

defaultArgs :: Args
defaultArgs =
  Args
  { programPaths  = []
  , clangPath = "clang"
  , exportStatsPath = Nothing
  }

-- | Flag to set clang path.
clangPathFlag :: Flag Args
clangPathFlag =
  let upd s old = Right $ old {clangPath = s}
      help = printf "Path to clang (default "++(clangPath defaultArgs)++")"
  in flagReq [ "clang" ] upd "PATH" help

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

formatNatural :: Natural -> String
formatNatural = addCommas . show
    where addCommas = reverse . concat . intersperse "," . unfoldr chunkBy3 . reverse
          chunkBy3 l = case splitAt 3 l of
                        ([], _) -> Nothing
                        p -> Just p

-- FIXME use a package for this textual alignment...?
binStats :: ReoptStats -> String -> String
binStats stats llvmGen =
  let sizeHdr       = "          Binary size (bytes): "
      entriesHdr    = "         Initial entry points: "
      discoveredHdr = "         Functions discovered: "
      recoveredHdr  = "          Functions recovered: "
      totalErrsHdr  = "    Total error/warning count: "
      llvmGenHdr    = "       LLVM generation status: "
      discErrHdr    = "             Discovery errors: "
      recErrHdr     = "              Recovery errors: "
      discErrCount = stepErrorCount DiscoveryStepTag stats
      recErrCount = stepErrorCount RecoveryStepTag stats
      maybeRow cnt hdr = if cnt == 0 then [] else [hdr ++ (show cnt)]
  in unlines $
       [ statsBinaryPath stats
       , sizeHdr ++ (formatNatural $ statsBinarySize stats)
       , entriesHdr ++ show (statsInitEntryPointCount stats)
       , discoveredHdr ++ show (statsFnDiscoveredCount stats)
       , recoveredHdr ++ show (statsFnRecoveredCount stats)
       ]
       ++ (maybeRow discErrCount discErrHdr)
       ++ (maybeRow recErrCount recErrHdr)
       ++ [ totalErrsHdr ++ show (statsErrorCount stats)
          , llvmGenHdr ++ llvmGen
          ]

renderExplorationResult :: ExplorationResult -> String
renderExplorationResult =
  \case
    ExplorationStats stats (LLVMGenPass sz)  -> do
      binStats stats $ printf "Succeeded (%s bytes generated)" (formatNatural sz)
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
        hPutStrLn stderr $ (show sz) ++ " bytes of LLVM textual bitcode generated."
        pure $ if sz < 0 then 0 else fromIntegral sz
    handleFailure :: (FilePath -> String -> ExplorationResult) -> SomeException -> IO ExplorationResult
    handleFailure mkResult e = do
        hPutStrLn stderr "Error raised during exploration"
        hPutStrLn stderr $ show e
        pure $ mkResult fPath (show e)


data SummaryStats =
  SummaryStats
  { totalBinaryCount :: !Natural
  -- ^ How many binaries were analyzed?
  , totalBinaryBytes :: !Natural
  -- ^ How many binaries were analyzed?
  , totalInitEntryPointCount :: !Natural
  -- ^ How many initial entry points were encountered?
  , totalFnDiscoveredCount :: !Natural
  -- ^ Number of discovered functions.
  , totalFnRecoveredCount :: !Natural
  -- ^ Number of successfully recovered functions.
  , totalFnFailures :: !(Map ReoptStepTag (Map ReoptErrorTag Natural))
  -- ^ Overall collection of failures by tag.
  , totalFailedBinaries :: !Natural
  -- ^ Number of binaries which failed to complete discovery.
  , totalLLVMGenerated :: !Natural
  -- ^ Number of binaries which we successfully produced LLVM bitcode for.
  , totalLLVMBytes :: !Natural
  -- ^ Number of bytes of LLVM generated.
  , totalErrorCount :: !Natural
  -- ^ Overall number of errors encountered while exploring binaries.
  }

initSummaryStats :: SummaryStats
initSummaryStats = SummaryStats 0 0 0 0 0 Map.empty 0 0 0 0

totalFailureCount :: SummaryStats -> Natural
totalFailureCount stats = foldl' (+) 0 totals
  where totals = concatMap Map.elems $ Map.elems $ totalFnFailures stats

renderSummaryStats :: [ExplorationResult] -> String
renderSummaryStats results = formatSummary $ foldr processResult initSummaryStats results
  where
    processResult :: ExplorationResult -> SummaryStats -> SummaryStats
    processResult (ExplorationStats s llvmGenRes) acc =
      acc { totalBinaryCount = 1 + (totalBinaryCount acc)
          , totalBinaryBytes = (statsBinarySize s) + (totalBinaryBytes acc)
          , totalInitEntryPointCount = (statsInitEntryPointCount s) + (totalInitEntryPointCount acc)
          , totalFnDiscoveredCount = (statsFnDiscoveredCount s) + (totalFnDiscoveredCount acc)
          , totalFnRecoveredCount = (statsFnRecoveredCount s) + (totalFnRecoveredCount acc)
          , totalFnFailures = mergeFnFailures (statsStepErrors s) (totalFnFailures acc)
          , totalErrorCount = (statsErrorCount s) + (totalErrorCount acc)
          , totalLLVMGenerated = (totalLLVMGenerated acc) + (if llvmGenSuccess llvmGenRes then 1 else 0)
          , totalLLVMBytes = (totalLLVMBytes acc) + (case llvmGenRes of LLVMGenPass sz -> sz; _ -> 0)
          }
    processResult (ExplorationError _ _) acc =
      acc { totalBinaryCount = 1 + (totalBinaryCount acc)
          , totalFailedBinaries = 1 + (totalFailedBinaries acc)
          , totalErrorCount = 1 + (totalErrorCount acc)
          }
    formatSummary :: SummaryStats -> String
    formatSummary s =
      if (totalFnDiscoveredCount s) == 0
      then "\nreopt discovered no functions after exploring "++(show $ totalBinaryCount s)++" binaries."
      else
        let passedPercent :: Double = (fromIntegral $ totalFnRecoveredCount s) / (fromIntegral $  totalFnDiscoveredCount s)
        in "\nreopt-explore discovered the following:" ++
           "\n  " ++ (printf "%d binaries (%s bytes total)" (totalBinaryCount s) (formatNatural $ totalBinaryBytes s)) ++
           "\n  " ++ (printf "%d initial entry points" (totalInitEntryPointCount s)) ++
           "\n  " ++ (printf "%d functions" (totalFnDiscoveredCount s)) ++
           "\n"++(printf "%d (%.2f%%) discovered functions were successfully recovered." (totalFnRecoveredCount s) (passedPercent * 100.0)) ++
           "\nreopt generated LLVM bitcode for "++(show $ totalLLVMGenerated s)++" out of "++(show $ totalBinaryCount s)++" binaries."++
           "\n"++(printf "%s bytes of textual LLVM bitcode were generated." (formatNatural $ totalLLVMBytes s))++
           "\n"++(show $ totalFailureCount s)++" errors/warnings during exploration." ++
           "\nError metrics:" ++
           "\n"++(renderAllFailures $ totalFnFailures s)





main :: IO ()
main = do
  args <- getCommandLineArgs
  case programPaths args of
    [] -> do
      hPutStrLn stderr "Must provide at least one input program or directory to explore."
      exitFailure
    paths -> do
      results <- foldM (withElfFilesInDir (exploreBinary args)) [] paths
      mapM_ (\s -> hPutStr stderr ("\n" ++ renderExplorationResult s)) results
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



