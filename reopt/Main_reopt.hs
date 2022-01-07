{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Exception (catch)
import Control.Lens (Lens', lens, (%~), (&), (.~), (^.), (^..))
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import Data.Either (rights)
import Data.ElfEdit
  ( ElfSection (elfSectionAddr, elfSectionData, elfSectionName),
    elfSections,
  )
import qualified Data.ElfEdit as Elf
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate, nub, stripPrefix, (\\))
import Data.Macaw.Analysis.RegisterUse (ruReason, ppRegisterUseErrorReason)
import Data.Macaw.DebugLogging
import Data.Macaw.Discovery
  ( DiscoveryOptions (..),
    ppDiscoveryStateBlocks,
    memory
  )
import Data.Maybe
import Data.Parameterized.Some
import qualified Data.Text as T
import Data.Version
import Data.Word
import Numeric
import Paths_reopt (version)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import Reopt
import Reopt.CFG.FnRep.X86 ()
import Reopt.EncodeInvariants
import Reopt.Events
import qualified Reopt.Events.Export as Export
import Reopt.ExternalTools (runSlash)
import Reopt.Occam
import Reopt.Server
import Reopt.Utils.Exit
import Reopt.VCG.Annotations as Ann
import System.Console.CmdArgs.Explicit
import System.Environment (getArgs)
import System.Exit
import System.FilePath (splitFileName)
import System.IO
import System.IO.Error
import Text.Printf (printf)

import Reopt.TypeInference.ConstraintGen

reoptVersion :: String
reoptVersion = printf "Reopt binary reoptimizer (reopt) %d.%d.%d." h l r
  where
    [h, l, r] = versionBranch version

-- | Write a builder object to a file if defined or standard out if not.
writeOutput :: Maybe FilePath -> (Handle -> IO a) -> IO a
writeOutput Nothing f = f stdout
writeOutput (Just nm) f = withBinaryFile nm WriteMode f

------------------------------------------------------------------------
-- Utilities

unintercalate :: String -> String -> [String]
unintercalate punct str = reverse $ go [] "" str
  where
    go acc "" [] = acc
    go acc thisAcc [] = reverse thisAcc : acc
    go acc thisAcc str'@(x : xs)
      | Just sfx <- stripPrefix punct str' = go (reverse thisAcc : acc) "" sfx
      | otherwise = go acc (x : thisAcc) xs

------------------------------------------------------------------------
-- Action

-- | Action to perform when running
data Action
  = -- | Print out disassembler output only.
    DumpDisassembly
  | -- | Print out control-flow microcode.
    ShowCFG
  | -- | Print out generated constraints
    ShowConstraints
  | -- | Start reopt in server mode
    ServerMode
  | -- | Print out help message
    ShowHelp
  | -- | Print out help message
    ShowHomeDirectory
  | -- | Print out version
    ShowVersion
  | -- | Perform a full reoptimization
    Reopt
  deriving (Show)

------------------------------------------------------------------------
-- Args

-- | Command line arguments.
data Args = Args
  { _reoptAction :: !Action,
    -- | Path to input program to optimize/export
    programPath :: !FilePath,
    _debugKeys :: [DebugClass],
    -- Debug information ^ TODO: See if we can omit this.

    -- | Path to output
    outputPath :: !(Maybe FilePath),
    -- | Filepath for C header file that helps provide
    -- information about program.
    headerPath :: !(Maybe FilePath),
    -- | Path to `clang` command.
    --
    -- This is only used as a C preprocessor for parsing
    -- header files.
    clangPath :: !FilePath,
    -- | LLVM version to generate LLVM for.
    --
    -- Only used when generating LLVM.
    llvmVersion :: !LLVMConfig,
    -- | Path to LLVM `llc` command
    --
    -- Only used when generating assembly file.
    llcPath :: !FilePath,
    -- | Path to LLVM opt command.
    --
    -- Only used when generating LLVM to be optimized.
    optPath :: !FilePath,
    -- | Optimization level to pass to opt and llc
    --
    -- This defaults to 2
    optLevel :: !Int,
    -- | Path to OCCCAM slash command.
    slashPath :: !FilePath,
    -- | Path to llvm-mc
    --
    -- Only used when generating object file from assembly generated by llc.
    llvmMcPath :: !FilePath,
    -- | List of entry points for translation
    includeAddrs :: ![String],
    -- | List of function entry points that we exclude for translation.
    excludeAddrs :: ![String],
    -- | Address to load binary at if relocatable.
    loadBaseAddress :: !(Maybe Word64),
    -- | Options affecting discovery
    _discOpts :: !DiscoveryOptions,
    -- | Prefix for unnamed functions identified in code discovery.
    unnamedFunPrefix :: !BS.ByteString,
    -- | Generation options for LLVM
    llvmGenOptions :: !LLVMGenOptions,
    -- | Path to export events if any
    eventsExportPath :: !(Maybe FilePath),
    -- | Path to write reopt-vcg annotations to if any
    annotationsExportPath :: !(Maybe FilePath),
    -- | Path to export invariants
    invariantsExportPath :: !(Maybe FilePath),
    -- | Path to export CFG
    cfgExportPath :: !(Maybe FilePath),
    -- | Path to export functions file
    fnsExportPath :: !(Maybe FilePath),
    -- | Path to export LLVM file
    llvmExportPath :: !(Maybe FilePath),
    -- | Path to export object file
    objectExportPath :: !(Maybe FilePath),
    -- | Path to write relationships needed for relinker
    relinkerInfoExportPath :: !(Maybe FilePath),
    -- | OCCAM configuration (if applicable)
    occamConfigPath :: !(Maybe FilePath),
    -- | Additional locations to search for dynamic dependencies.
    dynDepPath :: ![FilePath],
    -- | Additional locations to search for dynamic dependencies' debug info.
    dynDepDebugPath :: ![FilePath],
    -- | Perform recovery regardless of other options.
    performRecovery :: Bool
  }

-- | Action to perform when running
reoptAction :: Lens' Args Action
reoptAction = lens _reoptAction (\s v -> s {_reoptAction = v})

-- | Which debug keys (if any) to output
debugKeys :: Lens' Args [DebugClass]
debugKeys = lens _debugKeys (\s v -> s {_debugKeys = v})

-- | Options for controlling discovery
discOpts :: Lens' Args DiscoveryOptions
discOpts = lens _discOpts (\s v -> s {_discOpts = v})

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs =
  Args
    { _reoptAction = Reopt,
      programPath = "",
      _debugKeys = [],
      outputPath = Nothing,
      headerPath = Nothing,
      llvmVersion = latestLLVMConfig,
      clangPath = "clang",
      llcPath = "llc",
      optPath = "opt",
      optLevel = 2,
      slashPath = "slash",
      llvmMcPath = "llvm-mc",
      includeAddrs = [],
      excludeAddrs = [],
      loadBaseAddress = Nothing,
      _discOpts = reoptDefaultDiscoveryOptions,
      invariantsExportPath = Nothing,
      unnamedFunPrefix = "reopt",
      eventsExportPath = Nothing,
      annotationsExportPath = Nothing,
      llvmGenOptions = defaultLLVMGenOptions,
      cfgExportPath = Nothing,
      fnsExportPath = Nothing,
      llvmExportPath = Nothing,
      objectExportPath = Nothing,
      relinkerInfoExportPath = Nothing,
      occamConfigPath = Nothing,
      dynDepPath = [],
      dynDepDebugPath = [],
      performRecovery = False
    }

------------------------------------------------------------------------
-- Loading flags

resolveHex :: String -> Maybe Word64
resolveHex ('0' : x : wval)
  | x `elem` ['x', 'X'],
    [(w, "")] <- readHex wval,
    fromInteger w <= toInteger (maxBound :: Word64) =
    Just $! fromInteger w
resolveHex _ = Nothing

-- | Define a flag that tells reopt to load the binary at a particular
-- base address.
--
-- Primarily used for loading shared libraries at a fixed address.
loadBaseAddressFlag :: Flag Args
loadBaseAddressFlag = flagReq ["load-at-addr"] upd "OFFSET" help
  where
    help = "Load a relocatable file at the given base address."
    upd :: String -> Args -> Either String Args
    upd val args =
      case resolveHex val of
        Just off ->
          Right $ args {loadBaseAddress = Just off}
        Nothing ->
          Left $ printf "Expected a hexadecimal address of form '0x???', passed %s." val

------------------------------------------------------------------------
-- Other Flags

disassembleFlag :: Flag Args
disassembleFlag = flagNone ["disassemble", "d"] upd help
  where
    upd = reoptAction .~ DumpDisassembly
    help = "Show raw disassembler output."

cfgFlag :: Flag Args
cfgFlag = flagNone ["c", "cfg"] upd help
  where
    upd = reoptAction .~ ShowCFG
    help = "Show recovered control-flow graphs."

constraintsFlag :: Flag Args
constraintsFlag = flagNone ["C", "constraints"] upd help
  where
    upd = reoptAction .~ ShowConstraints
    help = "Show recovered control-flow type constraints."

performRecoveryFlag :: Flag Args
performRecoveryFlag = flagNone ["recover"] upd help
  where
    upd s = s {performRecovery = True}
    help = "Perform recovery regardless of other options."

showHomeDirFlag :: Flag Args
showHomeDirFlag = flagNone ["home-dir"] upd help
  where
    upd = reoptAction .~ ShowHomeDirectory
    help = "Display the directory reopt uses to cache info"
           ++ " (customizable via the environment variable "
           ++ reoptHomeDirEnvVar ++ ")."

serverModeFlag :: Flag Args
serverModeFlag = flagNone ["server"] upd help
  where
    upd = reoptAction .~ ServerMode
    help = "Run reopt in server mode."

outputFlag :: Flag Args
outputFlag = flagReq ["o", "output"] upd "PATH" help
  where
    upd s old = Right $ old {outputPath = Just s}
    help = "Path to write output."

headerFlag :: Flag Args
headerFlag = flagReq ["header"] upd "PATH" help
  where
    upd s old = Right $ old {headerPath = Just s}
    help = "Optional header with function declarations."

llvmVersionFlag :: Flag Args
llvmVersionFlag = flagReq ["llvm-version"] upd "VERSION" help
  where
    upd :: String -> Args -> Either String Args
    upd s old = do
      v <- case versionOfString s of
        Just v -> Right v
        Nothing -> Left "Could not interpret LLVM version."
      cfg <- case getLLVMConfig v of
        Just c -> pure c
        Nothing -> Left $ printf "Unsupported LLVM version %s." s
      pure $ old {llvmVersion = cfg}

    help = "LLVM version (e.g. 3.5.2)"

parseDebugFlags :: [DebugClass] -> String -> Either String [DebugClass]
parseDebugFlags oldKeys cl =
  case cl of
    '-' : cl' -> do
      ks <- getKeys cl'
      return (oldKeys \\ ks)
    cl' -> do
      ks <- getKeys cl'
      return (nub $ oldKeys ++ ks)
  where
    getKeys "all" = Right allDebugKeys
    getKeys str = case parseDebugKey str of
      Nothing -> Left $ "Unknown debug key `" ++ str ++ "'"
      Just k -> Right [k]

debugFlag :: Flag Args
debugFlag = flagOpt "all" ["debug", "D"] upd "FLAGS" help
  where
    upd s old = do
      let ks = unintercalate "," s
      new <- foldM parseDebugFlags (old ^. debugKeys) ks
      Right $ (debugKeys .~ new) old
    help =
      "Debug keys to enable.  This flag may be used multiple times, "
        ++ "with comma-separated keys.  Keys may be preceded by a '-' which "
        ++ "means disable that key.\n"
        ++ "Supported keys: all, "
        ++ intercalate ", " (map debugKeyName allDebugKeys)

-- | Defines a flag to update a file path to an executable with a
-- default.
pathFlag ::
  -- | Command name and flag name
  String ->
  -- | Lens for reading and writng
  Lens' Args FilePath ->
  Flag Args
pathFlag nm l = flagReq [nm] upd "PATH" help
  where
    upd s old = Right $ old & l .~ s
    help = printf "Path to %s (default %s)" (show nm) (show (defaultArgs ^. l))

-- | Flag to set clang path.
clangPathFlag :: Flag Args
clangPathFlag = pathFlag "clang" (lens clangPath (\s v -> s {clangPath = v}))

-- | Flag to set llc path.
llcPathFlag :: Flag Args
llcPathFlag = pathFlag "llc" (lens llcPath (\s v -> s {llcPath = v}))

-- | Flag to set path to opt.
optPathFlag :: Flag Args
optPathFlag = pathFlag "opt" (lens optPath (\s v -> s {optPath = v}))

-- | Flag to set llc optimization level.
optLevelFlag :: Flag Args
optLevelFlag = flagReq ["O", "opt-level"] upd "PATH" help
  where
    upd s old =
      case reads s of
        [(lvl, "")] | 0 <= lvl && lvl <= 3 -> Right $ old {optLevel = lvl}
        _ -> Left "Expected optimization level to be a number between 0 and 3."
    help = "Optimization level."

-- | Flag to set path to OCCAM slash
slashPathFlag :: Flag Args
slashPathFlag = pathFlag "slash" (lens slashPath (\s v -> s { slashPath = v}))

-- | Flag to set path to llvm-mc
llvmMcPathFlag :: Flag Args
llvmMcPathFlag = pathFlag "llvm-mc" (lens llvmMcPath (\s v -> s {llvmMcPath = v}))

-- | Used to add a new function to ignore translation of.
includeAddrFlag :: Flag Args
includeAddrFlag = flagReq ["include"] upd "ADDR" help
  where
    upd s old = Right $ old { includeAddrs = words s ++ includeAddrs old }
    help = "Address of function to include in analysis (may be repeated)."

-- | Used to add a new function to ignore translation of.
excludeAddrFlag :: Flag Args
excludeAddrFlag = flagReq ["exclude"] upd "ADDR" help
  where
    upd s old = Right $ old { excludeAddrs = words s ++ excludeAddrs old }
    help = "Address of function to exclude in analysis (may be repeated)."

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


-- | Print out a trace message when we analyze a function
logAtAnalyzeFunctionFlag :: Flag Args
logAtAnalyzeFunctionFlag = flagBool ["trace-function-discovery"] upd help
  where
    upd b = discOpts %~ \o -> o {logAtAnalyzeFunction = b}
    help = "Report when starting analysis of each function."

-- | Print out a trace message when we analyze a function
logAtAnalyzeBlockFlag :: Flag Args
logAtAnalyzeBlockFlag = flagBool ["trace-block-discovery"] upd help
  where
    upd b = discOpts %~ \o -> o {logAtAnalyzeBlock = b}
    help = "Report when starting analysis of each basic block with a function."

exploreCodeAddrInMemFlag :: Flag Args
exploreCodeAddrInMemFlag = flagBool ["include-mem"] upd help
  where
    upd b = discOpts %~ \o -> o {exploreCodeAddrInMem = b}
    help = "Include memory code addresses in discovery."

-- | This flag if set allows the LLVM generator to treat
-- trigger a undefined-behavior in cases like instructions
-- throwing exceptions or errors.
allowLLVMUB :: Flag Args
allowLLVMUB = flagBool ["allow-undef-llvm"] upd help
  where
    upd b s = s {llvmGenOptions = LLVMGenOptions {mcExceptionIsUB = b}}
    help = "Generate LLVM instead of inline assembly even when LLVM may result in undefined behavior."

-- | Generate an export flag
exportFlag ::
  -- | Flag name
  String ->
  -- | Name for help
  String ->
  -- | Setter
  (Args -> Maybe FilePath -> Args) ->
  Flag Args
exportFlag flagName helpName setter =
  flagReq [flagName] upd "PATH" help
  where
    upd s old = Right $ setter old (Just s)
    help = printf "Where to export %s." helpName

-- | Path to export invariants to
invariantsExportFlag :: Flag Args
invariantsExportFlag = exportFlag "invariants" "Reopt invariants" upd
  where
    upd s v = s {invariantsExportPath = v}

-- | Path to log events to.
eventsExportFlag :: Flag Args
eventsExportFlag = exportFlag "export-events" "Reopt events" upd
  where
    upd s v = s {eventsExportPath = v}

-- | Path to write LLVM annotations to.
annotationsExportFlag :: Flag Args
annotationsExportFlag = exportFlag "annotations" "reopt-vcg annotations" upd
  where
    upd s v = s {annotationsExportPath = v}

-- | Path to export CFG
cfgExportFlag :: Flag Args
cfgExportFlag = exportFlag "export-cfg" "CFG file" upd
  where
    upd s v = s {cfgExportPath = v}

-- | Path to export functions to.
fnsExportFlag :: Flag Args
fnsExportFlag = exportFlag "export-fns" "functions file" upd
  where
    upd s v = s {fnsExportPath = v}

-- | Path to export LLVM to
llvmExportFlag :: Flag Args
llvmExportFlag = exportFlag "export-llvm" "LLVM" upd
  where
    upd s v = s {llvmExportPath = v}

-- | Path to export object file to.
objectExportFlag :: Flag Args
objectExportFlag = exportFlag "export-object" "object file" upd
  where
    upd s v = s {objectExportPath = v}

-- | Path to write LLVM annotations to.
relinkerInfoExportFlag :: Flag Args
relinkerInfoExportFlag = exportFlag "relinker-info" "relinker relationships" upd
  where
    upd s v = s {relinkerInfoExportPath = v}

-- | Flag to enable OCCAM (with default config if not accompanied by the `occam-config` flag).
occamFlag :: Flag Args
occamFlag =
  flagReq ["occam"] upd "PATH" help
  where
    upd path cfg = Right $ cfg {occamConfigPath = Just path}
    help = printf "Enables OCCAM as reopt's optimizer using the manifest at PATH."

arguments :: Mode Args
arguments = mode "reopt" defaultArgs help filenameArg flags
  where
    help = reoptVersion ++ "\n" ++ copyrightNotice
    flags =
      [ -- General purpose options
        flagHelpSimple (reoptAction .~ ShowHelp),
        flagVersion (reoptAction .~ ShowVersion),
        debugFlag,
        performRecoveryFlag,
        -- Output for final binary
        outputFlag,
        -- Output events
        eventsExportFlag,
        -- Explicit Modes
        disassembleFlag,
        cfgFlag,
        constraintsFlag,
        serverModeFlag,
        showHomeDirFlag,
        -- Discovery options
        logAtAnalyzeFunctionFlag,
        logAtAnalyzeBlockFlag,
        exploreCodeAddrInMemFlag,
        includeAddrFlag,
        excludeAddrFlag,
        -- Dependency options
        dynDepPathFlag,
        dynDepDebugPathFlag,
        -- Function options
        headerFlag,
        -- Loading options
        loadBaseAddressFlag,
        -- Recovery options
        invariantsExportFlag,
        -- LLVM options
        llvmVersionFlag,
        annotationsExportFlag,
        allowLLVMUB,
        -- Command line programs
        clangPathFlag,
        llcPathFlag,
        optPathFlag,
        slashPathFlag,
        llvmMcPathFlag,
        -- Optimization
        optLevelFlag,
        occamFlag,
        -- Export
        cfgExportFlag,
        fnsExportFlag,
        llvmExportFlag,
        objectExportFlag,
        relinkerInfoExportFlag
      ]

-- | Flag to set the path to the binary to analyze.
filenameArg :: Arg Args
filenameArg =
  Arg
    { argValue = setFilename,
      argType = "FILE",
      argRequire = False
    }
  where
    setFilename :: String -> Args -> Either String Args
    setFilename nm a = Right (a {programPath = nm})

getCommandLineArgs :: IO Args
getCommandLineArgs = do
  argStrings <- getArgs
  case process arguments argStrings of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right v -> return v

-- | Print out the disassembly of all executable sections.
--
-- Note.  This does not apply relocations.
dumpDisassembly :: Args -> IO ()
dumpDisassembly args = do
  when (null (programPath args)) $ do
    hPutStrLn stderr "Missing program to disassemble"
    exitFailure
  bs <- checkedReadFile (programPath args)
  hdr <- handleEitherStringWithExit $ parseElfHeaderInfo64 (programPath args) bs
  let (l, e) = Elf.getElf hdr
  unless (null l) $ do
    hPutStrLn stderr "Recoverable errors occurred in reading elf file:"
    mapM_ (hPrint stderr) l
  let sections = filter isCodeSection $ e ^.. elfSections
  when (null sections) $ do
    hPutStrLn stderr "Binary contains no executable sections."
    exitFailure
  writeOutput (outputPath args) $ \h -> do
    forM_ sections $ \s -> do
      printX86SectionDisassembly h (elfSectionName s) (elfSectionAddr s) (elfSectionData s)

loadOptions :: Args -> LoadOptions
loadOptions args = LoadOptions {loadOffset = loadBaseAddress args}

argsReoptOptions :: Args -> IO ReoptOptions
argsReoptOptions args = do
    gdbDebugDirs <- getGdbDebugInfoDirs True
    pure $ ReoptOptions
            { roIncluded = includeAddrs args,
              roExcluded = excludeAddrs args,
              roVerboseMode = True,
              roDiscoveryOptions = args ^. discOpts,
              roDynDepPaths = dynDepPath args,
              roDynDepDebugPaths = dynDepDebugPath args ++ gdbDebugDirs
            }

-- | Discovery symbols in program and show function CFGs.
showCFG :: Args -> IO String
showCFG args = do
  when (null (programPath args)) $ do
    hPutStrLn stderr "Missing program to analyze."
    exitFailure

  let path = programPath args
  reoptOpts <- argsReoptOptions args
  Some hdrInfo <- do
    bs <- checkedReadFile path
    case Elf.decodeElfHeaderInfo bs of
      Left (_, msg) -> do
        hPutStrLn stderr $ "Error reading " ++ path ++ ":"
        hPutStrLn stderr $ "  " ++ msg
        exitFailure
      Right (Elf.SomeElf hdr) ->
        pure $! Some hdr
  let hdr = Elf.header hdrInfo
  -- Get architecture specific information
  marchInfo <- getElfArchInfo (Elf.headerClass hdr) (Elf.headerMachine hdr) (Elf.headerOSABI hdr)
  (w, SomeArch ainfo pltFn) <- handleEitherStringWithExit marchInfo
  mapM_ (hPutStrLn stderr) w
  mr <-
    runReoptM printLogEvent $ do
      -- Resolve header annotations
      hdrAnn <- resolveHeader (headerPath args) (clangPath args)
      -- Perform Discovery
      globalStepStarted DiscoveryInitialization
      initState <- reoptRunInit $ doInit (loadOptions args) hdrInfo ainfo pltFn reoptOpts
      (_, discState) <- doDiscovery hdrAnn hdrInfo ainfo initState reoptOpts
      -- Print discovery
      pure $ show $ ppDiscoveryStateBlocks discState
  handleEitherWithExit mr

-- | Show the constraints generated by the type inference step.
showConstraints :: Args -> IO ()
showConstraints args = do -- FIXME: copied from main performReopt command
  let elfPath = programPath args
  when (null (programPath args)) $ do
    hPutStrLn stderr "Missing program to analyze."
    exitFailure
  origElf <- do
    bytes <- checkedReadFile elfPath
    handleEitherStringWithExit $ parseElfHeaderInfo64 elfPath bytes

  rOpts <- argsReoptOptions args

  mr <- runReoptM printLogEvent $ do
    hdrAnn <- resolveHeader (headerPath args) (clangPath args)

    let funPrefix :: BSC.ByteString
        funPrefix = unnamedFunPrefix args

    (os, initState) <- reoptX86Init (loadOptions args) rOpts origElf
    let symAddrMap = initDiscSymAddrMap initState

    checkSymbolUnused funPrefix symAddrMap

    let ainfo = osArchitectureInfo os
    (debugTypeMap, discState) <- doDiscovery hdrAnn origElf ainfo initState rOpts

    let sysp = osPersonality os
    (recMod, _relinkerInfo, _logEvents) <-
      doRecoverX86 funPrefix sysp symAddrMap debugTypeMap discState

    pure (genModule recMod (memory discState))

  mc <-   handleEitherWithExit mr

  putStrLn "Warnings"
  putStrLn (unlines (map ((++) "\t" . show) (mcWarnings mc)))
  putStrLn "Constraints"
  putStrLn (unlines (map ((++) "\t" . show) (mcConstraints mc)))
  putStrLn "Inferred types"
  putStrLn (showInferredTypes mc)

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
      let enc = encodeInvariantFailedMsg addr (ppRegisterUseErrorReason (ruReason e))
      seq enc $ modifyIORef' ref (enc :)
    _ -> do
      pure ()

-- | This command is called when reopt is called with no specific
-- action.
performReopt :: Args -> IO ()
performReopt args = do
  let somethingToDo = isJust (cfgExportPath args) || shouldRecover args
  unless somethingToDo $ do
    hPutStrLn stderr $
      unlines
        [ "Nothing to do",
          "  Specify output via --output or other info to export."
        ]
    exitFailure

  when (isJust (annotationsExportPath args) && isNothing (llvmExportPath args)) $ do
    hPutStrLn stderr $
      "Using --annotations requires --export-llvm."
    exitFailure

  let elfPath = programPath args
  when (null (programPath args)) $ do
    hPutStrLn stderr "Missing program to analyze."
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
      Just path -> Just <$> openFile path WriteMode

  let logger2 =
        case eventExportHandle of
          Nothing -> logger1
          Just h -> joinLogEvents logger1 (Export.exportEvent h)

  rOpts <- argsReoptOptions args

  mr <- runReoptM logger2 $ do
    hdrAnn <- resolveHeader (headerPath args) (clangPath args)

    let funPrefix :: BSC.ByteString
        funPrefix = unnamedFunPrefix args

    (os, initState) <- reoptX86Init (loadOptions args) rOpts origElf
    let symAddrMap = initDiscSymAddrMap initState

    when (shouldRecover args) $
      checkSymbolUnused funPrefix symAddrMap

    let ainfo = osArchitectureInfo os
    (debugTypeMap, discState) <- doDiscovery hdrAnn origElf ainfo initState rOpts

    case cfgExportPath args of
      Nothing -> pure ()
      Just path -> do
        reoptWrite CfgFileType path $ \h -> do
          PP.hPutDoc h (ppDiscoveryStateBlocks discState)

    unless (shouldRecover args) $ reoptEndNow ()

    let sysp = osPersonality os
    (recMod, relinkerInfo, _logEvents) <-
      doRecoverX86 funPrefix sysp symAddrMap debugTypeMap discState

    case fnsExportPath args of
      Nothing -> pure ()
      Just path -> do
        reoptWrite FunsFileType path $ \h -> do
          mapM_ (PP.hPutDoc h . PP.pretty) (recoveredDefs recMod)

    -- Write invariants
    case invariantsExportPath args of
      Nothing -> pure ()
      Just path -> do
        invariants <- reoptIO $ reverse <$> readIORef invariantsRef
        let buffer = AE.encodingToLazyByteString (AE.list id invariants)
        reoptWriteByteString AnnotationsFileType path buffer

    case relinkerInfoExportPath args of
      Nothing -> pure ()
      Just path -> do
        reoptWriteByteString RelinkerInfoFileType path (Aeson.encode relinkerInfo)

    unless (shouldGenerateLLVM args) $ reoptEndNow ()

    -- Generate constraints
    let _moduleConstraints = genModule recMod (memory discState)

    -- Generate LLVM
    let (objLLVM, ann, ext, _logEvents) =
          renderLLVMBitcode (llvmGenOptions args) (llvmVersion args) os recMod
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

            let vcgAnn :: Ann.ModuleAnnotations
                vcgAnn = Ann.ModuleAnnotations
                  { Ann.llvmFilePath = llvmPath
                  , Ann.binFilePath = programPath args
                  , Ann.pageSize = 4096
                  , Ann.stackGuardPageCount = 1
                  , Ann.functions = rights (snd <$> ann)
                  , Ann.extFunctions = ext
                  }
            reoptWriteByteString AnnotationsFileType annPath (Aeson.encode vcgAnn)
            funStepAllFinished AnnotationGeneration ()

    unless (shouldCompileLLVM args) $ reoptEndNow ()

    -- Generate object file
    objContents <- generateObjectFile args os objLLVM
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
      hPrint stderr f
      exitFailure
    Right () ->
      pure ()
  case eventExportHandle of
    Nothing -> pure ()
    Just h -> hClose h

-- | Use OCCAM's slash on a bitcode file to optimize and recompile it.
reoptRunSlash ::
  -- | Arguments to run Slash
  Args ->
  -- | Configuration information for OCCAM.
  FilePath ->
  -- | Bitcode to optimize and compile with OCCAM's slash
  Builder.Builder ->
  ReoptM arch r BS.ByteString
reoptRunSlash args cfgPath llContents = do
  res <- reoptIO $ BS.readFile cfgPath
  cfg <-
    case Aeson.eitherDecodeStrict res of
      Left e  ->
        reoptFatalError $ ReoptTextParseError OccamConfigFileType cfgPath e
      Right c ->
        pure c

  reoptInTempDirectory $ do
    let (_, progName) = splitFileName $ programPath args
    -- Write input LLVM
    let inputPath = "input.ll"
    reoptWriteBuilder LlvmFileType inputPath llContents
    -- Path for slash to store outputt in.
    let occamOutputPath = "occam.bc"
    -- Manifest path for slash
    let manifestPath = "manifest.occam"
    -- Emit manifest
    reoptWriteByteString OccamManifestFileType manifestPath $ Aeson.encode $
      toOccamManifest inputPath progName occamOutputPath (occamLDFlags cfg)

    reoptIO $ handleExceptTWithExit $
      runSlash (slashPath args) manifestPath (T.unpack <$> occamSlashOptions cfg)
    reoptIO $ BS.readFile occamOutputPath

generateObjectFile :: Args -> X86OS -> Builder.Builder -> ReoptM arch r BS.ByteString
generateObjectFile args os objLLVM = do
  optLLVM <-
    case occamConfigPath args of
      Nothing -> reoptIO $ handleExceptTWithExit $ do
        optimizeLLVM (optLevel args) (optPath args) objLLVM
      Just cfgPath -> do
        reoptRunSlash args cfgPath objLLVM
  reoptIO $ handleExceptTWithExit $
    compileLLVM (optLevel args) (llcPath args) (llvmMcPath args) (osLinkName os) optLLVM

main' :: IO ()
main' = do
  args <- getCommandLineArgs
  setDebugKeys (args ^. debugKeys)
  case args ^. reoptAction of
    DumpDisassembly -> do
      dumpDisassembly args
    ShowCFG ->
      writeOutput (outputPath args) $ \h -> do
        hPutStrLn h =<< showCFG args
    ShowConstraints -> showConstraints args
    ServerMode -> do
      runServer
    ShowHelp -> do
      print $ helpText [] HelpFormatAll arguments
    ShowHomeDirectory -> do
      homeDir <- reoptHomeDir
      print $ "Reopt's current home directory: " ++ homeDir
    ShowVersion ->
      putStrLn (modeHelp arguments)
    Reopt -> do
      performReopt args

main :: IO ()
main = main' `catch` h
  where
    h e
      | isUserError e = do
        hPutStrLn stderr "User error"
        hPutStrLn stderr $ ioeGetErrorString e
      | otherwise = do
        hPutStrLn stderr "Other error"
        hPutStrLn stderr $ show e
        hPutStrLn stderr $ show (ioeGetErrorType e)
