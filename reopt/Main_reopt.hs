{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (catch)
import Control.Lens (Lens', (^.), (^..))
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BSC
import Data.Either (rights)
import Data.ElfEdit (
  ElfSection (elfSectionAddr, elfSectionData, elfSectionName),
  elfSections,
 )
import Data.ElfEdit qualified as Elf
import Data.Generics.Labels ()
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import Data.Version (Version (versionBranch))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric (readHex)
import Options.Applicative
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Exit (exitFailure)
import System.FilePath (splitFileName)
import System.IO qualified as IO
import System.IO.Error (
  ioeGetErrorString,
  ioeGetErrorType,
  isUserError,
 )
import Text.Printf (printf)

import Data.Macaw.Analysis.RegisterUse qualified as Macaw
import Data.Macaw.DebugLogging qualified as Macaw
import Data.Macaw.Discovery qualified as Macaw
import Data.Parameterized.Some (Some (Some))

import Reopt
import Reopt.ELFArchInfo (getElfArchInfo)
import Reopt.EncodeInvariants (
  encodeInvariantFailedMsg,
  encodeInvariantMsg,
 )
import Reopt.Events (
  FunId (FunId),
  ReoptFunStep (AnnotationGeneration, InvariantInference),
  ReoptGlobalStep (DiscoveryInitialization, Relinking),
  ReoptLogEvent (ReoptFunStepFailed, ReoptFunStepFinished),
  joinLogEvents,
  printLogEvent,
 )
import Reopt.Events.Export qualified as Export
import Reopt.ExternalTools (runSlash)
import Reopt.Occam (
  ReoptOccamConfig (occamLDFlags, occamSlashOptions),
  toOccamManifest,
 )
import Reopt.Server (runServer)
import Reopt.TypeInference.ConstraintGen (ModuleConstraints (..), genModuleConstraints)
import Reopt.TypeInference.Pretty (ppFunction)
import Reopt.Utils.Exit (
  checkedReadFile,
  handleEitherStringWithExit,
  handleEitherWithExit,
  handleExceptTWithExit,
 )
import Reopt.VCG.Annotations as Ann (ModuleAnnotations (..))
import Reopt.X86 (
  X86OS,
  osArchitectureInfo,
  osLinkName,
  osPersonality,
 )

import Paths_reopt (version)

reoptVersion :: String
reoptVersion = printf "Reopt binary reoptimizer (reopt) %s" v
 where
  v = List.intercalate "." $ map (printf "%d") $ versionBranch version

-- | Write a builder object to a file if defined or standard out if not.
writeOutput :: Maybe FilePath -> (IO.Handle -> IO a) -> IO a
writeOutput Nothing f = f IO.stdout
writeOutput (Just nm) f = IO.withBinaryFile nm IO.WriteMode f

------------------------------------------------------------------------
-- Utilities

unintercalate :: String -> String -> [String]
unintercalate punct = reverse . go [] ""
 where
  go acc "" [] = acc
  go acc thisAcc [] = reverse thisAcc : acc
  go acc thisAcc str'@(x : xs)
    | Just sfx <- List.stripPrefix punct str' = go (reverse thisAcc : acc) "" sfx
    | otherwise = go acc (x : thisAcc) xs

------------------------------------------------------------------------
-- Action

-- | Action to perform when running
data Action
  = -- | Print out disassembler output only.
    DumpDisassembly FilePath
  | -- | Print out control-flow microcode.
    ShowCFG FilePath
  | -- | Print out generated constraints
    ShowConstraints FilePath
  | -- | Start reopt in server mode
    ServerMode
  | -- | Print out help message
    ShowHomeDirectory
  | -- | Print out version
    ShowVersion
  | -- | Perform a full reoptimization
    Reopt FilePath
  deriving (Show)

------------------------------------------------------------------------
-- Args

-- | Command line arguments.
data Args = Args
  { reoptAction :: !Action
  , debugKeys :: [Macaw.DebugClass]
  -- ^ Debug information TODO: See if we can omit this.
  , outputPath :: !(Maybe FilePath)
  -- ^ Path to output
  , headerPath :: !(Maybe FilePath)
  -- ^ Filepath for C header file that helps provide
  -- information about program.
  , clangPath :: !FilePath
  -- ^ Path to `clang` command.
  --
  -- This is only used as a C preprocessor for parsing
  -- header files.
  , llvmVersion :: !LLVMConfig
  -- ^ LLVM version to generate LLVM for.
  --
  -- Only used when generating LLVM.
  , llcPath :: !FilePath
  -- ^ Path to LLVM `llc` command
  --
  -- Only used when generating assembly file.
  , optPath :: !FilePath
  -- ^ Path to LLVM opt command.
  --
  -- Only used when generating LLVM to be optimized.
  , optLevel :: !Int
  -- ^ Optimization level to pass to opt and llc
  --
  -- This defaults to 2
  , slashPath :: !FilePath
  -- ^ Path to OCCCAM slash command.
  , llvmMcPath :: !FilePath
  -- ^ Path to llvm-mc
  --
  -- Only used when generating object file from assembly generated by llc.
  , addEntryPoints :: ![String]
  -- ^ List of extra entry points to start discovery from.  This does not
  -- preclude other entry points from being added through discovery.
  , includeAddrs :: ![String]
  -- ^ List of allowed function entry points for discovery.  This prevents
  -- exploration of entry points beyond the ones explicitly listed.
  , excludeAddrs :: ![String]
  -- ^ List of function entry points that we exclude for translation.
  , loadBaseAddress :: !(Maybe Word64)
  -- ^ Address to load binary at if relocatable.
  , discOpts :: !Macaw.DiscoveryOptions
  -- ^ Options affecting discovery
  , unnamedFunPrefix :: !BS.ByteString
  -- ^ Prefix for unnamed functions identified in code discovery.
  , llvmGenOptions :: !LLVMGenOptions
  -- ^ Generation options for LLVM
  , eventsExportPath :: !(Maybe FilePath)
  -- ^ Path to export events if any
  , annotationsExportPath :: !(Maybe FilePath)
  -- ^ Path to write reopt-vcg annotations to if any
  , invariantsExportPath :: !(Maybe FilePath)
  -- ^ Path to export invariants
  , cfgExportPath :: !(Maybe FilePath)
  -- ^ Path to export CFG
  , fnsExportPath :: !(Maybe FilePath)
  -- ^ Path to export functions file
  , typedFnsExportPath :: !(Maybe FilePath)
  -- ^ Path to export functions file
  , llvmExportPath :: !(Maybe FilePath)
  -- ^ Path to export LLVM file
  , objectExportPath :: !(Maybe FilePath)
  -- ^ Path to export object file
  , relinkerInfoExportPath :: !(Maybe FilePath)
  -- ^ Path to write relationships needed for relinker
  , occamConfigPath :: !(Maybe FilePath)
  -- ^ OCCAM configuration (if applicable)
  , dynDepPath :: ![FilePath]
  -- ^ Additional locations to search for dynamic dependencies.
  , dynDepDebugPath :: ![FilePath]
  -- ^ Additional locations to search for dynamic dependencies' debug info.
  , performRecovery :: Bool
  -- ^ Perform recovery regardless of other options.
  , traceTypeUnification :: Bool
  -- ^ Trace unification of the type inference solver
  , traceConstraintOrigins :: Bool
  -- ^ Trace the origin of constraints in the type inference solver
  }
  deriving (Generic)

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs =
  Args
    { reoptAction = Reopt ""
    , debugKeys = []
    , outputPath = Nothing
    , headerPath = Nothing
    , llvmVersion = latestLLVMConfig
    , clangPath = "clang"
    , llcPath = "llc"
    , optPath = "opt"
    , optLevel = 2
    , slashPath = "slash"
    , llvmMcPath = "llvm-mc"
    , addEntryPoints = []
    , includeAddrs = []
    , excludeAddrs = []
    , loadBaseAddress = Nothing
    , discOpts = reoptDefaultDiscoveryOptions
    , invariantsExportPath = Nothing
    , unnamedFunPrefix = "reopt"
    , eventsExportPath = Nothing
    , annotationsExportPath = Nothing
    , llvmGenOptions = defaultLLVMGenOptions
    , cfgExportPath = Nothing
    , fnsExportPath = Nothing
    , typedFnsExportPath = Nothing
    , llvmExportPath = Nothing
    , objectExportPath = Nothing
    , relinkerInfoExportPath = Nothing
    , occamConfigPath = Nothing
    , dynDepPath = []
    , dynDepDebugPath = []
    , performRecovery = False
    , traceTypeUnification = False
    , traceConstraintOrigins = False
    }

------------------------------------------------------------------------
-- Loading flags

resolveHex :: String -> Maybe Word64
resolveHex ('0' : x : wval)
  | x `elem` ['x', 'X']
  , [(w, "")] <- readHex wval
  , fromInteger w <= toInteger (maxBound :: Word64) =
      Just $! fromInteger w
resolveHex _ = Nothing

-- | Define a flag that tells reopt to load the binary at a particular base
-- address.
--
-- Primarily used for loading shared libraries at a fixed address.
loadBaseAddressP :: Parser Word64
loadBaseAddressP =
  option (eitherReader validate) $
    long "load-at-addr"
      <> metavar "OFFSET"
      <> help "Load a relocatable file at the given base address"
 where
  validate :: String -> Either String Word64
  validate s =
    maybe
      (Left $ printf "Expected a hexadecimal address of form '0x???', passed %s" s)
      return
      $ resolveHex s

------------------------------------------------------------------------
-- Other Flags

performRecoveryP :: Parser Bool
performRecoveryP =
  switch $
    long "recover"
      <> help "Perform recovery regardless of other options"

traceTypeUnificationP :: Parser Bool
traceTypeUnificationP =
  switch $
    long "trace-unification"
      <> help "Trace unification in the type inference engine"

traceConstraintOriginsP :: Parser Bool
traceConstraintOriginsP =
  switch $
    long "trace-constraint-origins"
      <> help "Trace the origins of constraints in the type inference engine"

outputPathP :: Parser String
outputPathP =
  strOption $
    short 'o'
      <> long "output"
      <> metavar "PATH"
      <> help "Path to write output executable"

headerPathP :: Parser String
headerPathP =
  strOption $
    long "header"
      <> metavar "PATH"
      <> help "Optional header with function declarations"

llvmVersionP :: Parser LLVMConfig
llvmVersionP =
  option (eitherReader validate) $
    long "llvm-version"
      <> value latestLLVMConfig
      <> metavar "VERSION"
      <> help "LLVM version (e.g. 3.5.2)"
 where
  validate :: String -> Either String LLVMConfig
  validate s = do
    v <- case versionOfString s of
      Just v -> Right v
      Nothing -> Left "Could not interpret LLVM version"
    case getLLVMConfig v of
      Just c -> pure c
      Nothing -> Left $ printf "Unsupported LLVM version %s" s

parseDebugFlags :: [Macaw.DebugClass] -> String -> Either String [Macaw.DebugClass]
parseDebugFlags oldKeys cl =
  case cl of
    '-' : cl' -> do
      ks <- getKeys cl'
      return (oldKeys List.\\ ks)
    cl' -> do
      ks <- getKeys cl'
      return (List.nub $ oldKeys ++ ks)
 where
  getKeys "all" = Right Macaw.allDebugKeys
  getKeys s = case Macaw.parseDebugKey s of
    Nothing -> Left $ "Unknown debug key `" ++ s ++ "'"
    Just k -> Right [k]

debugKeysP :: Parser [Macaw.DebugClass]
debugKeysP =
  option (eitherReader validate) $
    long "debug"
      <> short 'D'
      <> metavar "FLAGS"
      <> value []
      <> showDefault
      <> help
        ( "Debug keys to enable.  This flag may be used multiple times, "
            ++ "with comma-separated keys.  Keys may be preceded by a '-' which "
            ++ "means disable that key.\n"
            ++ "Supported keys: all, "
            ++ List.intercalate ", " (map Macaw.debugKeyName Macaw.allDebugKeys)
        )
 where
  validate s = do
    let ks = unintercalate "," s
    foldM parseDebugFlags [] ks

-- | Defines a flag to update a file path to an executable with a default.
pathP ::
  -- | Command name and flag name
  String ->
  -- | Lens to the field in `Args`
  Lens' Args String ->
  Parser String
pathP name l =
  strOption $
    long name
      <> value (defaultArgs ^. l)
      <> showDefault
      <> metavar "PATH"
      <> help ("Path to " <> name)

-- | Parser to set clang path.
clangPathP :: Parser String
clangPathP = pathP "clang" #clangPath

-- | Parser to set llc path.
llcPathP :: Parser String
llcPathP = pathP "llc" #llcPath

-- | Parser to set path to opt.
optPathP :: Parser String
optPathP = pathP "opt" #optPath

-- | Parser to set llc optimization level.
optLevelP :: Parser Int
optLevelP =
  option (eitherReader validate) $
    short 'O'
      <> long "opt-level"
      <> metavar "PATH"
      <> value (defaultArgs ^. #optLevel)
      <> showDefault
      <> help "Optimization level"
 where
  validate s =
    case reads s of
      [(lvl, "")] | 0 <= lvl && lvl <= 3 -> Right lvl
      _ -> Left "Expected optimization level to be a number between 0 and 3"

-- | Parser to set path to OCCAM slash
slashPathP :: Parser String
slashPathP = pathP "slash" #slashPath

-- | Parser to set path to llvm-mc
llvmMcPathP :: Parser String
llvmMcPathP = pathP "llvm-mc" #llvmMcPath

-- | Used to add extra entry points to seed discovery.
addEntryPointP :: Parser String
addEntryPointP =
  strOption
    ( long "add-entry-point"
        <> metavar "ADDR"
        <> help "Extra address to seed discovery frontier with (may be repeated)"
    )

-- | Used to specify the only addresses we care to explore.
includeAddrP :: Parser String
includeAddrP =
  strOption
    ( long "include"
        <> metavar "ADDR"
        <> help "Address of function to include in analysis (may be repeated)"
    )

-- | Used to specify addresses we wish **not** to explore.
excludeAddrP :: Parser String
excludeAddrP =
  strOption
    ( long "exclude"
        <> metavar "ADDR"
        <> help
          "Address of function to exclude in analysis (may be repeated)"
    )

-- | Used to add a path at which to search for dynamic dependencies.
dynDepPathP :: Parser String
dynDepPathP =
  strOption $
    long "lib-dir"
      <> metavar "PATH"
      <> help
        "Additional location to search for dynamic dependencies"

-- | Used to add a path at which to search for dynamic dependencies.
dynDepDebugPathP :: Parser String
dynDepDebugPathP =
  strOption $
    long "debug-dir"
      <> metavar "PATH"
      <> help
        "Additional location to search for dynamic dependencies' debug info"

-- | Print out a trace message when we analyze a function
logAtAnalyzeFunctionP :: Parser Bool
logAtAnalyzeFunctionP =
  switch $
    long "trace-function-discovery"
      <> help
        "Report when starting analysis of each function"

-- | Print out a trace message when we analyze a function
logAtAnalyzeBlockP :: Parser Bool
logAtAnalyzeBlockP =
  switch $
    long "trace-block-discovery"
      <> help
        "Report when starting analysis of each basic block with a function"

exploreCodeAddrInMemP :: Parser Bool
exploreCodeAddrInMemP =
  switch $
    long "include-mem"
      <> help
        "Include memory code addresses in discovery"

-- | If set, allows the LLVM generator to trigger some undefined behavior in
-- cases like instructions throwing exceptions or errors.
allowLLVMUBP :: Parser Bool
allowLLVMUBP =
  switch $
    long "allow-undef-llvm"
      <> help
        "Generate LLVM instead of inline assembly even when LLVM may result in undefined behavior"

-- | Generate an export flag
exportP ::
  -- | Parser name
  String ->
  -- | Name for help
  String ->
  Parser String
exportP flagName helpName =
  strOption $
    long flagName
      <> metavar "PATH"
      <> help
        (printf "Where to export %s" helpName)

-- | Path to export invariants to
invariantsExportP :: Parser String
invariantsExportP = exportP "invariants" "Reopt invariants"

-- | Path to log events to
eventsExportP :: Parser String
eventsExportP = exportP "export-events" "Reopt events"

-- | Path to write LLVM annotations to
annotationsExportP :: Parser String
annotationsExportP = exportP "annotations" "reopt-vcg annotations"

-- | Path to export CFG
cfgExportP :: Parser String
cfgExportP = exportP "export-cfg" "CFG file"

-- | Path to export functions to.
fnsExportP :: Parser String
fnsExportP = exportP "export-fns" "functions file"

-- | Path to export typed functions to.
typedFnsExportP :: Parser String
typedFnsExportP = exportP "export-typed-fns" "functions file"

-- | Path to export LLVM to
llvmExportP :: Parser String
llvmExportP = exportP "export-llvm" "LLVM"

-- | Path to export object file to.
objectExportP :: Parser String
objectExportP = exportP "export-object" "object file"

-- | Path to write LLVM annotations to.
relinkerInfoExportP :: Parser String
relinkerInfoExportP = exportP "relinker-info" "relinker relationships"

-- | Parser to enable OCCAM (with default config if not accompanied by the `occam-config` flag).
occamConfigPathP :: Parser String
occamConfigPathP =
  strOption $
    long "occam"
      <> metavar "PATH"
      <> help "Enables OCCAM as reopt's optimizer using the manifest at PATH"

command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  command label (info (parser <**> helper) (progDesc description))

commands :: [Mod CommandFields Action]
commands =
  [ command' "disassemble" "Show raw disassembler output" (DumpDisassembly <$> programPathP)
  , command' "cfg" "Show recovered control-flow-graph" (ShowCFG <$> programPathP)
  , command'
      "constraints"
      "Show recovered control-flow type constraints"
      (ShowConstraints <$> programPathP)
  , command'
      "home-dir"
      ( "Display the directory reopt uses to cache info"
          <> " (customizable via the environment variable "
          <> reoptHomeDirEnvVar
          <> ")."
      )
      (pure ShowHomeDirectory)
  , command' "reopt" "Run reopt" (Reopt <$> programPathP)
  , command' "server" "Run in server mode" (pure ServerMode)
  , command' "version" "Show version" (pure ShowVersion)
  ]

arguments :: Parser Args
arguments =
  Args
    <$> (subparser (mconcat commands) <|> (Reopt <$> programPathP))
    <*> debugKeysP
    <*> optional outputPathP
    <*> optional headerPathP
    <*> clangPathP
    <*> llvmVersionP
    <*> llcPathP
    <*> optPathP
    <*> optLevelP
    <*> slashPathP
    <*> llvmMcPathP
    <*> many addEntryPointP
    <*> many includeAddrP
    <*> many excludeAddrP
    <*> optional loadBaseAddressP
    <*> ( Macaw.DiscoveryOptions
            -- This was never exposed to the CLI
            (Macaw.exploreFunctionSymbols Macaw.defaultDiscoveryOptions)
            <$> exploreCodeAddrInMemP
            <*> logAtAnalyzeFunctionP
            <*> logAtAnalyzeBlockP
        )
    -- This was never exposed to the CLI
    <*> pure (defaultArgs ^. #unnamedFunPrefix)
    <*> ( LLVMGenOptions
            <$> allowLLVMUBP
        )
    <*> optional eventsExportP
    <*> optional annotationsExportP
    <*> optional invariantsExportP
    <*> optional cfgExportP
    <*> optional fnsExportP
    <*> optional typedFnsExportP
    <*> optional llvmExportP
    <*> optional objectExportP
    <*> optional relinkerInfoExportP
    <*> optional occamConfigPathP
    <*> many dynDepPathP
    <*> many dynDepDebugPathP
    <*> performRecoveryP
    <*> traceTypeUnificationP
    <*> traceConstraintOriginsP

-- | Parser to set the path to the binary to analyze.
programPathP :: Parser String
programPathP = strArgument $ metavar "PATH"

displayError :: String -> IO ()
displayError = IO.hPutStrLn IO.stderr

getCommandLineArgs :: IO Args
getCommandLineArgs = execParser (info (arguments <**> helper) fullDesc)

-- | Print out the disassembly of all executable sections.
--
-- Note.  This does not apply relocations.
dumpDisassembly :: Args -> FilePath -> IO ()
dumpDisassembly args elfPath = do
  bs <- checkedReadFile elfPath
  hdr <- handleEitherStringWithExit $ parseElfHeaderInfo64 elfPath bs
  let (l, e) = Elf.getElf hdr
  unless (null l) $ do
    displayError "Recoverable errors occurred in reading elf file:"
    mapM_ (IO.hPrint IO.stderr) l
  let sections = filter isCodeSection $ e ^.. elfSections
  when (null sections) $ do
    displayError "Binary contains no executable sections"
    exitFailure
  writeOutput (outputPath args) $ \h -> do
    forM_ sections $ \s -> do
      printX86SectionDisassembly h (elfSectionName s) (elfSectionAddr s) (elfSectionData s)

loadOptions :: Args -> LoadOptions
loadOptions args = LoadOptions{loadOffset = loadBaseAddress args}

argsReoptOptions :: Args -> IO ReoptOptions
argsReoptOptions args = do
  gdbDebugDirs <- getGdbDebugInfoDirs True
  pure $
    ReoptOptions
      { roExtraEntryPoints = addEntryPoints args
      , roIncluded = includeAddrs args
      , roExcluded = excludeAddrs args
      , roVerboseMode = True
      , roDiscoveryOptions = args ^. #discOpts
      , roDynDepPaths = dynDepPath args
      , roDynDepDebugPaths = dynDepDebugPath args ++ gdbDebugDirs
      , roTraceUnification = traceTypeUnification args
      , roTraceConstraintOrigins = traceConstraintOrigins args
      }

-- | Discovery symbols in program and show function CFGs.
showCFG :: Args -> FilePath -> IO String
showCFG args elfPath = do
  reoptOpts <- argsReoptOptions args
  Some hdrInfo <- do
    bs <- checkedReadFile elfPath
    case Elf.decodeElfHeaderInfo bs of
      Left (_, msg) -> do
        displayError $ "Error reading " ++ elfPath ++ ":"
        displayError $ "  " ++ msg
        exitFailure
      Right (Elf.SomeElf hdr) ->
        pure $! Some hdr
  let hdr = Elf.header hdrInfo
  -- Get architecture specific information
  marchInfo <- getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr)
  (w, SomeArch ainfo pltFn) <- handleEitherStringWithExit marchInfo
  mapM_ displayError w
  mr <-
    runReoptM printLogEvent $ do
      -- Resolve header annotations
      hdrAnn <- resolveHeader (headerPath args) (clangPath args)
      -- Perform Discovery
      globalStepStarted DiscoveryInitialization
      initState <- reoptRunInit $ doInit (loadOptions args) hdrInfo ainfo pltFn reoptOpts
      (_, discState) <- doDiscovery hdrAnn hdrInfo ainfo initState reoptOpts
      -- Print discovery
      pure $ show $ Macaw.ppDiscoveryStateBlocks discState
  handleEitherWithExit mr

-- | Show the constraints generated by the type inference step.
showConstraints :: Args -> FilePath -> IO ()
showConstraints args elfPath = do
  -- FIXME: copied from main performReopt command
  origElf <- do
    bytes <- checkedReadFile elfPath
    handleEitherStringWithExit $ parseElfHeaderInfo64 elfPath bytes

  rOpts <- argsReoptOptions args

  mr <- runReoptM printLogEvent $ do
    hdrAnn <- resolveHeader (headerPath args) (clangPath args)

    let
      funPrefix :: BSC.ByteString
      funPrefix = unnamedFunPrefix args

    (os, initState) <- reoptX86Init (loadOptions args) rOpts origElf
    let symAddrMap = initDiscSymAddrMap initState

    checkSymbolUnused funPrefix symAddrMap

    let ainfo = osArchitectureInfo os
    (debugTypeMap, discState) <- doDiscovery hdrAnn origElf ainfo initState rOpts

    let sysp = osPersonality os
    recoverX86Output <-
      doRecoverX86 funPrefix sysp symAddrMap debugTypeMap discState

    let recMod = recoveredModule recoverX86Output
    pure $ genModuleConstraints recMod (Macaw.memory discState) (traceTypeUnification args) (traceConstraintOrigins args)

  mc <- handleEitherWithExit mr

  displayConstraintsInformation mc

------------------------------------------------------------------------
-- Reopt action

-- | Return true if Reopt can should compile the LLVM
shouldCompileLLVM :: Args -> Bool
shouldCompileLLVM args =
  isJust (objectExportPath args)
    || isJust (outputPath args)
    || isJust (occamConfigPath args)

-- | Retun true if Reopt should generate LLVM and/or notations.
shouldGenerateLLVM :: Args -> Bool
shouldGenerateLLVM args =
  isJust (llvmExportPath args)
    || isJust (annotationsExportPath args)
    || shouldCompileLLVM args

-- | Return true when Reopt should perform function recovery.
shouldRecover :: Args -> Bool
shouldRecover args =
  isJust (fnsExportPath args)
    || isJust (typedFnsExportPath args)
    || isJust (invariantsExportPath args)
    || isJust (relinkerInfoExportPath args)
    || shouldGenerateLLVM args
    || performRecovery args

collectInvariants ::
  IORef [Aeson.Encoding] ->
  ReoptLogEvent X86_64 ->
  IO ()
collectInvariants ref evt = do
  case evt of
    ReoptFunStepFinished InvariantInference (FunId addr _mnm) invMap -> do
      let enc = encodeInvariantMsg addr invMap
      seq enc $ modifyIORef' ref (enc :)
    ReoptFunStepFailed InvariantInference (FunId addr _mnm) e -> do
      let enc = encodeInvariantFailedMsg addr (Macaw.ppRegisterUseErrorReason (Macaw.ruReason e))
      seq enc $ modifyIORef' ref (enc :)
    _ -> do
      pure ()

-- | This command is called when reopt is called with no specific
-- action.
performReopt :: Args -> FilePath -> IO ()
performReopt args elfPath = do
  let somethingToDo = isJust (cfgExportPath args) || shouldRecover args
  unless somethingToDo $ do
    displayError $
      unlines
        [ "Nothing to do"
        , "  Specify output via --output or other info to export"
        ]
    exitFailure

  when (isJust (annotationsExportPath args) && isNothing (llvmExportPath args)) $ do
    displayError "Using --annotations requires --export-llvm"
    exitFailure

  origElf <- do
    bytes <- checkedReadFile elfPath
    handleEitherStringWithExit $ parseElfHeaderInfo64 elfPath bytes

  invariantsRef <- newIORef []

  let logger0 = printLogEvent
  let logger1 =
        case invariantsExportPath args of
          Just _ -> joinLogEvents logger0 (collectInvariants invariantsRef)
          Nothing -> logger0

  eventExportHandle <-
    case eventsExportPath args of
      Nothing -> pure Nothing
      Just path -> Just <$> IO.openFile path IO.WriteMode

  let logger2 =
        case eventExportHandle of
          Nothing -> logger1
          Just h -> joinLogEvents logger1 (Export.exportEvent h)

  rOpts <- argsReoptOptions args

  mr <- runReoptM logger2 $ do
    hdrAnn <- resolveHeader (headerPath args) (clangPath args)

    let
      funPrefix :: BSC.ByteString
      funPrefix = unnamedFunPrefix args

    (os, symAddrMap, debugTypeMap, discState) <-
      reoptPrepareForRecovery (loadOptions args) rOpts hdrAnn funPrefix origElf

    when (shouldRecover args) $
      checkSymbolUnused funPrefix symAddrMap

    case cfgExportPath args of
      Nothing -> pure ()
      Just path -> do
        reoptWrite CfgFileType path $ \h -> do
          PP.hPutDoc h (Macaw.ppDiscoveryStateBlocks discState)

    unless (shouldRecover args) $ reoptEndNow ()

    let sysp = osPersonality os
    (_, recoverX86Output, recMod, moduleConstraints) <-
      reoptRecoveryLoop symAddrMap rOpts funPrefix sysp debugTypeMap discState

    -- forM_ (recoveredDefs recMod) $ \ f -> do
    --   trace "FUNCTION" (pure ())
    --   trace (show (PP.pretty f)) (pure ())

    let relinkerInfo = mergeRelations recoverX86Output
    case relinkerInfoExportPath args of
      Nothing -> pure ()
      Just path -> do
        reoptWriteByteString RelinkerInfoFileType path (Aeson.encode relinkerInfo)

    case fnsExportPath args of
      Nothing -> pure ()
      Just path -> do
        reoptWrite FunsFileType path $ \h ->
          PP.hPutDoc h $ PP.vcat $ PP.punctuate PP.line $ map PP.pretty $ recoveredDefs recMod

    -- Write invariants
    case invariantsExportPath args of
      Nothing -> pure ()
      Just path -> do
        invariants <- reoptIO $ reverse <$> readIORef invariantsRef
        let buffer = AE.encodingToLazyByteString (AE.list id invariants)
        reoptWriteByteString AnnotationsFileType path buffer

    -- FIXME: move
    let
      prettyDefs =
        [ PP.pretty n PP.<+> "=" PP.<+> PP.pretty ty
        | (n, ty) <- mcNamedTypes moduleConstraints
        ]
      prettyWarnings =
        ["# Warning: " <> PP.viaShow w | w <- mcWarnings moduleConstraints]

    case typedFnsExportPath args of
      Nothing -> pure ()
      Just path -> do
        -- FIXME: should we add another file type?
        reoptWriteTextual FunsFileType path $ \h ->
          PP.hPutDoc h $
            PP.vsep $
              prettyWarnings
                ++ prettyDefs
                ++ map (ppFunction moduleConstraints) (recoveredDefs recMod)

    -- set to True to dump constraints while generating LLVM, for debugging
    when False $ reoptIO $ displayConstraintsInformation moduleConstraints

    unless (shouldGenerateLLVM args) $ reoptEndNow ()

    -- Generate LLVM
    let (objLLVM, ann, ext, _logEvents) =
          renderLLVMIR (llvmGenOptions args) (llvmVersion args) os recMod moduleConstraints

    -- Write LLVM if requested.
    case llvmExportPath args of
      Nothing -> pure ()
      Just llvmPath -> do
        reoptWriteBuilder LlvmFileType llvmPath objLLVM
        case annotationsExportPath args of
          Nothing -> pure ()
          Just annPath -> do
            forM_ ann $ \(fid, fann) -> do
              funStepStarted AnnotationGeneration fid
              case fann of
                Left msg -> do
                  funStepFailed AnnotationGeneration fid msg
                Right _ -> do
                  funStepFinished AnnotationGeneration fid ()

            let
              vcgAnn :: Ann.ModuleAnnotations
              vcgAnn =
                Ann.ModuleAnnotations
                  { Ann.llvmFilePath = llvmPath
                  , Ann.binFilePath = elfPath
                  , Ann.pageSize = 4096
                  , Ann.stackGuardPageCount = 1
                  , Ann.functions = rights (snd <$> ann)
                  , Ann.extFunctions = ext
                  }
            reoptWriteByteString AnnotationsFileType annPath (Aeson.encode vcgAnn)
            funStepAllFinished AnnotationGeneration ()

    unless (shouldCompileLLVM args) $ reoptEndNow ()

    -- Generate object file
    objContents <- generateObjectFile args elfPath os objLLVM
    case objectExportPath args of
      Nothing -> pure ()
      Just path -> reoptWriteStrictByteString ObjectFileType path objContents
    case outputPath args of
      Nothing ->
        reoptEndNow ()
      Just outPath -> do
        globalStepStarted Relinking
        let objName = fromMaybe "new object" (objectExportPath args)
        -- Parse new object file
        let (warnings, mergeRes) = mergeObject origElf objName objContents relinkerInfo
        mapM_ (globalStepWarning Relinking) warnings
        case mergeRes of
          Left e ->
            reoptFatalError $ ReoptRelinkerFatalError e
          Right newBinary -> do
            reoptIO $ writeExecutable outPath newBinary
            globalStepFinished Relinking ()

  case mr of
    Left f -> do
      case eventExportHandle of
        Nothing -> pure ()
        Just h -> Export.exportFatalError h f
      IO.hPrint IO.stderr f
      exitFailure
    Right () ->
      pure ()
  forM_ eventExportHandle IO.hClose

-- | Use OCCAM's slash on a bitcode file to optimize and recompile it.
reoptRunSlash ::
  -- | Arguments to run Slash
  Args ->
  FilePath ->
  -- | Configuration information for OCCAM.
  FilePath ->
  -- | Bitcode to optimize and compile with OCCAM's slash
  Builder.Builder ->
  ReoptM arch r BS.ByteString
reoptRunSlash args elfPath cfgPath llContents = do
  res <- reoptIO $ BS.readFile cfgPath
  cfg <-
    case Aeson.eitherDecodeStrict res of
      Left e ->
        reoptFatalError $ ReoptTextParseError OccamConfigFileType cfgPath e
      Right c ->
        pure c

  reoptInTempDirectory $ do
    let (_, progName) = splitFileName elfPath
    -- Write input LLVM
    let inputPath = "input.ll"
    reoptWriteBuilder LlvmFileType inputPath llContents
    -- Path for slash to store outputt in.
    let occamOutputPath = "occam.bc"
    -- Manifest path for slash
    let manifestPath = "manifest.occam"
    -- Emit manifest
    reoptWriteByteString OccamManifestFileType manifestPath $
      Aeson.encode $
        toOccamManifest inputPath progName occamOutputPath (occamLDFlags cfg)

    reoptIO $
      handleExceptTWithExit $
        runSlash (slashPath args) manifestPath (T.unpack <$> occamSlashOptions cfg)
    reoptIO $ BS.readFile occamOutputPath

generateObjectFile :: Args -> FilePath -> X86OS -> Builder.Builder -> ReoptM arch r BS.ByteString
generateObjectFile args elfPath os objLLVM = do
  optLLVM <-
    case occamConfigPath args of
      Nothing -> reoptIO $ handleExceptTWithExit $ do
        optimizeLLVM (optLevel args) (optPath args) objLLVM
      Just cfgPath -> do
        reoptRunSlash args elfPath cfgPath objLLVM
  reoptIO $
    handleExceptTWithExit $
      compileLLVM (optLevel args) (llcPath args) (llvmMcPath args) (osLinkName os) optLLVM

displayConstraintsInformation :: ModuleConstraints arch -> IO ()
displayConstraintsInformation moduleConstraints = do
  putStrLn "Warnings"
  putStrLn (unlines (map ((++) "\t" . show) (mcWarnings moduleConstraints)))

-- putStrLn "Constraints (generated)"
-- putStrLn (unlines (map (show . PP.indent 4 . PP.pretty) (mcConstraints moduleConstraints)))
-- putStrLn "Constraints (solving)"
-- putStrLn (unlines (map (show . PP.indent 4 . PP.pretty) (mcTyConstraints moduleConstraints)))
-- putStrLn "Inferred types"
-- putStrLn (showInferredTypes moduleConstraints)

main' :: IO ()
main' = do
  args <- getCommandLineArgs
  Macaw.setDebugKeys (args ^. #debugKeys)
  case args ^. #reoptAction of
    DumpDisassembly file -> dumpDisassembly args file
    ShowCFG file ->
      writeOutput (outputPath args) $ \h -> do
        IO.hPutStrLn h =<< showCFG args file
    ShowConstraints file -> showConstraints args file
    ServerMode -> runServer
    ShowHomeDirectory -> do
      homeDir <- reoptHomeDir
      print $ "Reopt's current home directory: " ++ homeDir
    ShowVersion -> putStrLn reoptVersion
    Reopt file -> performReopt args file

main :: IO ()
main = main' `catch` h
 where
  h e
    | isUserError e = do
        displayError "User error"
        displayError $ ioeGetErrorString e
    | otherwise = do
        displayError "Other error"
        IO.hPrint IO.stderr e
        IO.hPrint IO.stderr $ ioeGetErrorType e
