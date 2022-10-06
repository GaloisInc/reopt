
module CommandLine (getOptions, Options(..), Command(..), ResidualOptions(..), LLVMOptions(..)) where

import Options.Applicative

data Options = Options
  { optVerbose :: Bool
  , optDynDepPath :: ![FilePath]
  , optDynDepDebugPath :: ![FilePath]
  , optCommand :: Command
  }

data Command = RunLLVM LLVMOptions | RunResidual ResidualOptions

------------------------------------------------------------------------------------------
-- Residual

data ResidualOptions = ResidualOptions
  {
  roPaths :: [FilePath]
  }

residualP :: Parser Command
residualP = RunResidual <$> (ResidualOptions <$> some argsP)

--------------------------------------------------------------------------------
-- LLVM

-- | Command line arguments.
data LLVMOptions = LLVMOptions
  { 
    -- | Path to `clang` command.
    --
    -- This is only used as a C preprocessor for parsing
    -- header files.
    loClangPath :: !FilePath
    -- | Should we export function discovery/recovery results?
    , loExportFnResultsPath :: !(Maybe FilePath)
    -- | Should we export summary information?
    , loExportSummaryPath :: !(Maybe FilePath)
    -- | Should we export log events?
    , loExportLogCSVPath :: !(Maybe FilePath)
    -- | Emit generated LLVM next to binary with `.ll` suffix.
    , loEmitLLVM :: !Bool
    -- | Timeout in seconds for analyzing a single binary.
    , loBinTimeoutInSec :: !(Maybe Int)
    -- | Path to input program to optimize/export
    , loPaths :: ![FilePath]
  }

-- | Flag to set clang path.
clangPathP :: Parser FilePath
clangPathP =
  strOption (long "clang"
              <> metavar "PATH"
              <> value "clang"
              <> showDefaultWith (\s -> "`" ++ s ++ "'")
              <> help "Path to clang."
            )

exportFnResultsP :: Parser FilePath
exportFnResultsP =
  strOption (long "export-fn-results"
             <> metavar "PATH"
             <> help "Path at which to write function discovery/recovery results."
            )
  
exportSummaryP :: Parser FilePath
exportSummaryP =
  strOption (long "export-summary"
             <> metavar "PATH"
             <> help "Path at which to write discovery/recovery summary statistics."
            )

exportLogCSVP :: Parser FilePath
exportLogCSVP =
  strOption (long "export-log"
             <> metavar "PATH"
             <> help "Path at which to write recovery and LLVM generation log events (as a CSV)."
            )

omitLLVMP :: Parser Bool
omitLLVMP =
  flag True False (long "omit-llvm"
                   <> help "Do not output generated LLVM."
                  )

-- debugInfoFlag :: Flag Args
-- debugInfoFlag = flagNone ["debug-info", "d"] upd help
--   where
--     upd old = old {exploreMode = DebugExploreMode}
--     help = "Explore and export debug information for functions only."

binTimeoutInSecP :: Parser Int
binTimeoutInSecP = option auto (long "timeout"
                                <> metavar "SECS"
                                <> help "Timeout for analyzing individual binaries (in seconds)."
                               )
                   
llvmOptionsP :: Parser LLVMOptions
llvmOptionsP =
  LLVMOptions <$> clangPathP
              <*> optional exportFnResultsP
              <*> optional exportSummaryP
              <*> optional exportLogCSVP
              <*> omitLLVMP
              <*> optional binTimeoutInSecP
              <*> some argsP
   
llvmP :: Parser Command
llvmP = RunLLVM <$> llvmOptionsP
  
--------------------------------------------------------------------------------
-- Top-level

argsP :: Parser String
argsP = argument str (metavar "ELFFILE...")

commandP :: Parser Command
commandP = hsubparser (command "residuals" (info residualP (progDesc "Print out the residuals in the given binary"))
                      <> command "llvm" (info llvmP (progDesc "Generate a binary via LLVM")))

-- | Used to add a path at which to search for dynamic dependencies.
dynDepPathP :: Parser [FilePath]
dynDepPathP =
  many (strOption (long "lib-dir"
                   <> metavar "PATH"
                   <> help "Additional location to search for dynamic dependencies."
                  ))

-- | Used to add a path at which to search for dynamic dependencies.
dynDepDebugPathP :: Parser [FilePath]
dynDepDebugPathP =
  many (strOption (long "debug-dir"
                   <> metavar "PATH"
                   <> help "Additional location to search for dynamic dependencies' debug info."
                  ))

optionsP :: Parser Options
optionsP = Options <$> switch (long "verbose"
                               <> short 'v'
                               <> help "Verbose output")
                   <*> dynDepPathP
                   <*> dynDepDebugPathP
                   <*> commandP

opts :: ParserInfo Options
opts =info (optionsP <**> helper)
           (fullDesc <> progDesc "Analyze a collection of ELF binaries")

getOptions :: IO Options
getOptions = execParser opts

{-

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
-}
