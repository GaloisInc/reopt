module CommandLine (getOptions, Options (..), Command (..), ResidualOptions (..), LLVMOptions (..), GroundTruthOptions (..)) where

import Options.Applicative

data Options = Options
  { optVerbose :: Bool
  , optDynDepPath :: ![FilePath]
  , optDynDepDebugPath :: ![FilePath]
  , optCommand :: Command
  }

data Command = RunLLVM LLVMOptions | RunResidual ResidualOptions | RunGroundTruth GroundTruthOptions

------------------------------------------------------------------------------------------
-- Ground Truth

data GroundTruthOptions = GroundTruthOptions 
  {
    gtPaths :: ![FilePath]
  , gtExportDir :: !(Maybe FilePath)
  }

groundTruthP :: Parser Command
groundTruthP = 
    fmap RunGroundTruth $
        GroundTruthOptions
        <$> some argsP
        <*> optional exportProtoP

exportProtoP :: Parser FilePath
exportProtoP =
  strOption $
    long "export-fn-results"
      <> metavar "PATH"
      <> help "Directory at which to write the ground truth protobufs."

------------------------------------------------------------------------------------------
-- Residual

data ResidualOptions = ResidualOptions
  { roClangPath :: !FilePath
  -- ^ TODO: document me
  , roHeader :: !(Maybe FilePath)
  -- ^ TODO: document me
  , roOutputForSpreadsheet :: !Bool
  -- ^ TODO: document me
  , roPaths :: ![FilePath]
  -- ^ TODO: document me
  }

residualP :: Parser Command
residualP =
  fmap RunResidual $
    ResidualOptions
      <$> clangPathP
      <*> optional headerP
      <*> outputForSpreadsheetP
      <*> some argsP

--------------------------------------------------------------------------------
-- LLVM

-- | Command line arguments.
data LLVMOptions = LLVMOptions
  { loClangPath :: !FilePath
  -- ^ Path to `clang` command.
  --
  -- This is only used as a C preprocessor for parsing
  -- header files.
  , loExportFnResultsPath :: !(Maybe FilePath)
  -- ^ Should we export function discovery/recovery results?
  , loExportSummaryPath :: !(Maybe FilePath)
  -- ^ Should we export summary information?
  , loExportLogCSVPath :: !(Maybe FilePath)
  -- ^ Should we export log events?
  , loEmitLLVM :: !Bool
  -- ^ Emit generated LLVM next to binary with `.ll` suffix.
  , loBinTimeoutInSec :: !(Maybe Int)
  -- ^ Timeout in seconds for analyzing a single binary.
  , loPaths :: ![FilePath]
  -- ^ Path to input program to optimize/export
  }

-- | Flag to set clang path.
clangPathP :: Parser FilePath
clangPathP =
  strOption $
    long "clang"
      <> metavar "PATH"
      <> value "clang"
      <> showDefaultWith (\s -> "`" ++ s ++ "'")
      <> help "Path to clang."

outputForSpreadsheetP :: Parser Bool
outputForSpreadsheetP =
  switch $
    long "output-for-spreadsheet"
      <> help "Set to format the input for spreadsheet insertion."

exportFnResultsP :: Parser FilePath
exportFnResultsP =
  strOption $
    long "export-fn-results"
      <> metavar "PATH"
      <> help "Path at which to write function discovery/recovery results."

exportSummaryP :: Parser FilePath
exportSummaryP =
  strOption $
    long "export-summary"
      <> metavar "PATH"
      <> help "Path at which to write discovery/recovery summary statistics."

exportLogCSVP :: Parser FilePath
exportLogCSVP =
  strOption $
    long "export-log"
      <> metavar "PATH"
      <> help "Path at which to write recovery and LLVM generation log events (as a CSV)."

omitLLVMP :: Parser Bool
omitLLVMP =
  switch $
    long "omit-llvm"
      <> help "Do not output generated LLVM."

headerP :: Parser FilePath
headerP =
  strOption $
    long "header"
      <> metavar "PATH"
      <> help "Optional header with function declarations (as in reopt)"

-- debugInfoFlag :: Flag Args
-- debugInfoFlag = flagNone ["debug-info", "d"] upd help
--   where
--     upd old = old {exploreMode = DebugExploreMode}
--     help = "Explore and export debug information for functions only."

binTimeoutInSecP :: Parser Int
binTimeoutInSecP =
  option auto $
    long "timeout"
      <> metavar "SECS"
      <> help "Timeout for analyzing individual binaries (in seconds)."

llvmOptionsP :: Parser LLVMOptions
llvmOptionsP =
  LLVMOptions
    <$> clangPathP
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
commandP =
  hsubparser $
    command "residuals" (info residualP (progDesc "Print out the residuals in the given binary"))
      <> command "llvm" (info llvmP (progDesc "Generate a binary via LLVM"))
      <> command "ground-truth" (info groundTruthP (progDesc "Dump discovery information as protobuf to compare with ground truth"))


-- | Used to add a path at which to search for dynamic dependencies.
dynDepPathP :: Parser FilePath
dynDepPathP =
  strOption $
    long "lib-dir"
      <> metavar "PATH"
      <> help "Additional location to search for dynamic dependencies."

-- | Used to add a path at which to search for dynamic dependencies.
dynDepDebugPathP :: Parser FilePath
dynDepDebugPathP =
  strOption $
    long "debug-dir"
      <> metavar "PATH"
      <> help "Additional location to search for dynamic dependencies' debug info."

verboseP :: Parser Bool
verboseP =
  switch $
    long "verbose"
      <> short 'v'
      <> help "Verbose output"

optionsP :: Parser Options
optionsP =
  Options
    <$> verboseP
    <*> many dynDepPathP
    <*> many dynDepDebugPathP
    <*> commandP

opts :: ParserInfo Options
opts =
  info
    (optionsP <**> helper)
    (fullDesc <> progDesc "Analyze a collection of ELF binaries")

getOptions :: IO Options
getOptions = execParser opts
