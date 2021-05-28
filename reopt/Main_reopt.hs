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

import           Control.Exception
import Control.Lens
    ( (&), (^..), (^.), lens, (%~), (.~), Lens' )
import           Control.Monad
import           Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Either
import Data.ElfEdit
    ( elfSections,
      ElfSection(elfSectionName, elfSectionAddr, elfSectionData) )
import Data.IORef ( readIORef, modifyIORef', newIORef, IORef )
import           Data.List ((\\), nub, stripPrefix, intercalate)
import           Data.Maybe
import           Data.Parameterized.Some
import           Data.Version
import           Data.Word
import           Numeric
import           Prettyprinter (pretty)
import           System.Console.CmdArgs.Explicit
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           System.IO.Error
import           Text.Printf

import           Data.Macaw.DebugLogging
import Data.Macaw.Discovery
    ( ppDiscoveryStateBlocks, DiscoveryOptions(..) )
import           Data.Macaw.X86

import           Reopt
import           Reopt.Events
import           Reopt.EncodeInvariants
import           Reopt.Server
import           Reopt.CFG.FnRep.X86 ()
import           Reopt.ExternalTools (run_slash)
import           Reopt.CFG.FnRep.X86 ()
import           Reopt.Utils.Builder (builderWriteFile)
import qualified Reopt.VCG.Annotations as Ann
import           System.FilePath (splitFileName)

import           Paths_reopt (version)

reoptVersion :: String
reoptVersion = "Reopt binary reoptimizer (reopt) "  ++ versionString ++ "."
  where [h,l,r] = versionBranch version
        versionString = show h ++ "." ++ show l ++ "." ++ show r


-- | Write a builder object to a file if defined or standard out if not.
writeOutput :: Maybe FilePath -> (Handle -> IO a) -> IO a
writeOutput Nothing f = f stdout
writeOutput (Just nm) f =
  bracket (openBinaryFile nm WriteMode) hClose f

------------------------------------------------------------------------
-- Utilities

unintercalate :: String -> String -> [String]
unintercalate punct str = reverse $ go [] "" str
  where
    go acc "" [] = acc
    go acc thisAcc [] = (reverse thisAcc) : acc
    go acc thisAcc str'@(x : xs)
      | Just sfx <- stripPrefix punct str' = go ((reverse thisAcc) : acc) "" sfx
      | otherwise = go acc (x : thisAcc) xs

------------------------------------------------------------------------
-- Action

-- | Action to perform when running
data Action
   = DumpDisassembly -- ^ Print out disassembler output only.
   | ShowCFG         -- ^ Print out control-flow microcode.
   | ShowFunctions   -- ^ Print out generated functions
   | ShowLLVM        -- ^ Print out LLVM in textual format
   | ServerMode      -- ^ Start reopt in server mode
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version
   | Reopt           -- ^ Perform a full reoptimization
  deriving (Show)

------------------------------------------------------------------------
-- Args

-- | Command line arguments.
data Args
   = Args { _reoptAction  :: !Action
          , programPath  :: !FilePath
            -- ^ Path to input program to optimize/export
          , _debugKeys    :: [DebugClass]
            -- Debug information ^ TODO: See if we can omit this.
          , outputPath   :: !(Maybe FilePath)
            -- ^ Path to output
          , headerPath :: !(Maybe FilePath)
            -- ^ Filepath for C header file that helps provide
            -- information about program.
          , clangPath :: !FilePath
            -- ^ Path to `clang` command.
            --
            -- This is only used as a C preprocessor for parsing
            -- header files.
          , llvmVersion  :: !LLVMConfig
            -- ^ LLVM version to generate LLVM for.
            --
            -- Only used when generating LLVM.
          , llcPath :: !FilePath
            -- ^ Path to LLVM `llc` command
            --
            -- Only used when generating assembly file.
          , optPath      :: !FilePath
            -- ^ Path to LLVM opt command.
            --
            -- Only used when generating LLVM to be optimized.
          , optLevel     :: !Int
            -- ^ Optimization level to pass to opt and llc
            --
            -- This defaults to 2
          , llvmMcPath      :: !FilePath
            -- ^ Path to llvm-mc
            --
            -- Only used when generating object file from assembly generated by llc.
          , _includeAddrs   :: ![String]
            -- ^ List of entry points for translation
          , _excludeAddrs :: ![String]
            -- ^ List of function entry points that we exclude for translation.
          , loadBaseAddress :: !(Maybe Word64)
            -- ^ Address to load binary at if relocatable.
          , _discOpts :: !DiscoveryOptions
            -- ^ Options affecting discovery
          , unnamedFunPrefix :: !BS.ByteString
            -- ^ Prefix for unnamed functions identified in code discovery.
          , annotationsExportPath :: !(Maybe FilePath)
            -- ^ Path to write reopt-vcg annotations to.
            --
            -- If `Nothing` then annotations are not generated.
          , llvmGenOptions :: !LLVMGenOptions
            -- ^ Generation options for LLVM
          , invariantsExportPath :: !(Maybe FilePath)
            -- ^ Export invariants to file.
          , cfgExportPath :: !(Maybe FilePath)
            -- ^ Path to export CFG
          , fnsExportPath :: !(Maybe FilePath)
            -- ^ Path to export functions file.
          , llvmExportPath :: !(Maybe FilePath)
            -- ^ Path to export LLVM file.
          , objectExportPath :: !(Maybe FilePath)
            -- ^ Path to export object file.
          , relinkerInfoExportPath :: !(Maybe FilePath)
            -- ^ Path to write relationships needed for relinker
          , occamConfigPath   :: !(Maybe FilePath)
            -- ^ OCCAM configuration (if applicable)
          }

-- | Action to perform when running
reoptAction :: Lens' Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Which debug keys (if any) to output
debugKeys :: Lens' Args [DebugClass]
debugKeys = lens _debugKeys (\s v -> s { _debugKeys = v })

-- | Function entry points to translate (overrides notrans if non-empty)
includeAddrs :: Lens' Args [String]
includeAddrs = lens _includeAddrs (\s v -> s { _includeAddrs = v })

-- | Function entry points that we exclude for translation.
excludeAddrs :: Lens' Args [String]
excludeAddrs = lens _excludeAddrs (\s v -> s { _excludeAddrs = v })

-- | Options for controlling discovery
discOpts :: Lens' Args DiscoveryOptions
discOpts = lens _discOpts (\s v -> s { _discOpts = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Reopt
                   , programPath = ""
                   , _debugKeys = []
                   , outputPath = Nothing
                   , headerPath = Nothing
                   , llvmVersion = latestLLVMConfig
                   , clangPath = "clang"
                   , llcPath = "llc"
                   , optPath = "opt"
                   , optLevel  = 2
                   , llvmMcPath = "llvm-mc"
                   , _includeAddrs = []
                   , _excludeAddrs  = []
                   , loadBaseAddress = Nothing
                   , _discOpts     =
                       DiscoveryOptions { exploreFunctionSymbols = False
                                        , exploreCodeAddrInMem   = False
                                        , logAtAnalyzeFunction   = True
                                        , logAtAnalyzeBlock      = False
                                        }
                   , invariantsExportPath = Nothing
                   , unnamedFunPrefix = "reopt"
                   , annotationsExportPath  = Nothing
                   , llvmGenOptions = defaultLLVMGenOptions
                   , cfgExportPath = Nothing
                   , fnsExportPath = Nothing
                   , llvmExportPath = Nothing
                   , objectExportPath = Nothing
                   , relinkerInfoExportPath = Nothing
                   , occamConfigPath        = Nothing
                   }

------------------------------------------------------------------------
-- Loading flags

resolveHex :: String -> Maybe Word64
resolveHex ('0':x:wval)
  | x `elem` ['x', 'X']
  , [(w,"")] <- readHex wval
  , fromInteger w <= toInteger (maxBound :: Word64)  =
    Just $! fromInteger w
resolveHex _ = Nothing

-- | Define a flag that tells reopt to load the binary at a particular
-- base address.
--
-- Primarily used for loading shared libraries at a fixed address.
loadBaseAddressFlag :: Flag Args
loadBaseAddressFlag = flagReq [ "load-at-addr" ] upd "OFFSET" help
  where help = "Load a relocatable file at the given base address."
        upd :: String -> Args -> Either String Args
        upd val args =
          case resolveHex val of
            Just off -> Right $
               args { loadBaseAddress = Just off }
            Nothing -> Left $
              "Expected a hexadecimal address of form '0x???', passsed "
              ++ show val

------------------------------------------------------------------------
-- Other Flags

disassembleFlag :: Flag Args
disassembleFlag = flagNone [ "disassemble", "d" ] upd help
  where upd  = reoptAction .~ DumpDisassembly
        help = "Show raw disassembler output."

cfgFlag :: Flag Args
cfgFlag = flagNone [ "c", "cfg" ] upd help
  where upd  = reoptAction .~ ShowCFG
        help = "Show recovered control-flow graphs."

funFlag :: Flag Args
funFlag = flagNone [ "fns", "functions" ] upd help
  where upd  = reoptAction .~ ShowFunctions
        help = "Show recovered functions."

llvmFlag :: Flag Args
llvmFlag = flagNone [ "llvm" ] upd help
  where upd  = reoptAction .~ ShowLLVM
        help = "Show generated LLVM."

serverModeFlag :: Flag Args
serverModeFlag = flagNone [ "server" ] upd help
  where upd = reoptAction .~ ServerMode
        help = "Run reopt in server mode."

outputFlag :: Flag Args
outputFlag = flagReq [ "o", "output" ] upd "PATH" help
  where upd s old = Right $ old { outputPath = Just s }
        help = "Path to write output."

headerFlag :: Flag Args
headerFlag = flagReq [ "header" ] upd "PATH" help
  where upd s old = Right $ old { headerPath = Just s }
        help = "Optional header with function declarations."

llvmVersionFlag :: Flag Args
llvmVersionFlag = flagReq [ "llvm-version" ] upd "VERSION" help
  where upd :: String -> Args -> Either String Args
        upd s old = do
          v <- case versionOfString s of
                 Just v -> Right v
                 Nothing -> Left $ "Could not interpret LLVM version."
          cfg <- case getLLVMConfig v of
                   Just c -> pure c
                   Nothing -> Left $ "Unsupported LLVM version " ++ show s ++ "."
          pure $ old { llvmVersion = cfg }

        help = "LLVM version (e.g. 3.5.2)"

parseDebugFlags ::  [DebugClass] -> String -> Either String [DebugClass]
parseDebugFlags oldKeys cl =
  case cl of
    '-' : cl' -> do ks <- getKeys cl'
                    return (oldKeys \\ ks)
    cl'       -> do ks <- getKeys cl'
                    return (nub $ oldKeys ++ ks)
  where
    getKeys "all" = Right allDebugKeys
    getKeys str = case parseDebugKey str of
                    Nothing -> Left $ "Unknown debug key `" ++ str ++ "'"
                    Just k  -> Right [k]

debugFlag :: Flag Args
debugFlag = flagOpt "all" [ "debug", "D" ] upd "FLAGS" help
  where upd s old = do let ks = unintercalate "," s
                       new <- foldM parseDebugFlags (old ^. debugKeys) ks
                       Right $ (debugKeys .~ new) old
        help = "Debug keys to enable.  This flag may be used multiple times, "
            ++ "with comma-separated keys.  Keys may be preceded by a '-' which "
            ++ "means disable that key.\n"
            ++ "Supported keys: all, " ++ intercalate ", " (map debugKeyName allDebugKeys)

-- | Defines a flag to update a file path to an executable with a
-- default.
pathFlag :: String
         -- ^ Command name and flag name
         -> Lens' Args FilePath
         -- ^ Lens for reading and writng
         -> Flag Args
pathFlag nm l = flagReq [ nm ] upd "PATH" help
  where upd s old = Right $ old & l .~ s
        help = printf "Path to %s (default %s)" (show nm) (show (defaultArgs^.l))

-- | Flag to set clang path.
clangPathFlag :: Flag Args
clangPathFlag = pathFlag "clang" (lens clangPath (\s v -> s { clangPath = v }))

-- | Flag to set llc path.
llcPathFlag :: Flag Args
llcPathFlag = pathFlag "llc" (lens llcPath (\s v -> s { llcPath = v }))

-- | Flag to set path to opt.
optPathFlag :: Flag Args
optPathFlag = pathFlag "opt" (lens optPath (\s v -> s { optPath = v }))

-- | Flag to set path to llvm-mc
llvmMcPathFlag :: Flag Args
llvmMcPathFlag = pathFlag "llvm-mc" (lens llvmMcPath (\s v -> s { llvmMcPath = v }))

-- | Flag to set llc optimization level.
optLevelFlag :: Flag Args
optLevelFlag = flagReq [ "O", "opt-level" ] upd "PATH" help
  where upd s old =
          case reads s of
            [(lvl, "")] | 0 <= lvl && lvl <= 3 -> Right $ old { optLevel = lvl }
            _ -> Left "Expected optimization level to be a number between 0 and 3."
        help = "Optimization level."

-- | Used to add a new function to ignore translation of.
includeAddrFlag :: Flag Args
includeAddrFlag = flagReq [ "include" ] upd "ADDR" help
  where upd s old = Right $ old & includeAddrs %~ (words s ++)
        help = "Address of function to include in analysis (may be repeated)."

-- | Used to add a new function to ignore translation of.
excludeAddrFlag :: Flag Args
excludeAddrFlag = flagReq [ "exclude" ] upd "ADDR" help
  where upd s old = Right $ old & excludeAddrs %~ (s:)
        help = "Address of function to exclude in analysis (may be repeated)."

-- | Print out a trace message when we analyze a function
logAtAnalyzeFunctionFlag :: Flag Args
logAtAnalyzeFunctionFlag = flagBool [ "trace-function-discovery" ] upd help
  where upd b = discOpts %~ \o -> o { logAtAnalyzeFunction = b }
        help = "Report when starting analysis of each function."

-- | Print out a trace message when we analyze a function
logAtAnalyzeBlockFlag :: Flag Args
logAtAnalyzeBlockFlag = flagBool [ "trace-block-discovery" ] upd help
  where upd b = discOpts %~ \o -> o { logAtAnalyzeBlock = b }
        help = "Report when starting analysis of each basic block with a function."

exploreCodeAddrInMemFlag :: Flag Args
exploreCodeAddrInMemFlag = flagBool [ "include-mem" ] upd help
  where upd b = discOpts %~ \o -> o { exploreCodeAddrInMem = b }
        help = "Include memory code addresses in discovery."

-- | This flag if set allows the LLVM generator to treat
-- trigger a undefined-behavior in cases like instructions
-- throwing exceptions or errors.
allowLLVMUB :: Flag Args
allowLLVMUB = flagBool [ "allow-undef-llvm" ] upd help
  where upd b s = s { llvmGenOptions =
                        LLVMGenOptions { mcExceptionIsUB = b } }
        help = "Generate LLVM instead of inline assembly even when LLVM may result in undefined behavior."

-- | Generate an export flag
exportFlag :: String -- ^ Flag name
           -> String -- ^ Name for help
           -> (Args -> Maybe FilePath -> Args) -- ^ Setter
           -> Flag Args
exportFlag flagName helpName setter =
    flagReq [ flagName ] upd "PATH" help
  where upd s old = Right $ setter old (Just s)
        help = printf "Where to export %s." helpName

-- | Path to export invariants to
invariantsExportFlag :: Flag Args
invariantsExportFlag = exportFlag "invariants" "Reopt invariants" upd
  where upd s v = s { invariantsExportPath = v }

-- | Path to write LLVM annotations to.
annotationsExportFlag :: Flag Args
annotationsExportFlag = exportFlag "annotations" "reopt-vcg annotations" upd
  where upd s v = s { annotationsExportPath = v }

-- | Path to export CFG
cfgExportFlag :: Flag Args
cfgExportFlag = exportFlag "export-cfg" "CFG file" upd
  where upd s v = s { cfgExportPath = v }

-- | Path to export functions to.
fnsExportFlag :: Flag Args
fnsExportFlag = exportFlag "export-fns" "functions file" upd
  where upd s v = s { fnsExportPath = v }

-- | Path to export LLVM to
llvmExportFlag :: Flag Args
llvmExportFlag = exportFlag "export-llvm" "LLVM" upd
  where upd s v = s { llvmExportPath = v }

-- | Path to export object file to.
objectExportFlag :: Flag Args
objectExportFlag = exportFlag "export-object" "object file" upd
  where upd s v = s { objectExportPath = v }

-- | Path to write LLVM annotations to.
relinkerInfoExportFlag :: Flag Args
relinkerInfoExportFlag = exportFlag "relinker-info" "relinker relationships" upd
  where upd s v = s { relinkerInfoExportPath = v }

-- | Flag to enable OCCAM (with default config if not accompanied by the `occam-config` flag).
occamFlag :: Flag Args
occamFlag =
  flagReq [ "occam" ] upd "PATH" help
  where upd path cfg = Right $ cfg {occamConfigPath = Just path}
        help = printf "Enables OCCAM as reopt's optimizer using the manifest at PATH."

arguments :: Mode Args
arguments = mode "reopt" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ -- General purpose options
                  flagHelpSimple (reoptAction .~ ShowHelp)
                , flagVersion (reoptAction .~ ShowVersion)
                , debugFlag
                  -- Redirect output to file.
                , outputFlag
                  -- Explicit Modes
                , disassembleFlag
                , cfgFlag
                , funFlag
                , llvmFlag
                , serverModeFlag
                , occamFlag
                  -- Discovery options
                , logAtAnalyzeFunctionFlag
                , logAtAnalyzeBlockFlag
                , exploreCodeAddrInMemFlag
                , includeAddrFlag
                , excludeAddrFlag
                  -- Function options
                , headerFlag
                  -- Loading options
                , loadBaseAddressFlag
                  -- Recovery options
                , invariantsExportFlag
                  -- LLVM options
                , llvmVersionFlag
                , annotationsExportFlag
                , allowLLVMUB
                  -- Command line programs
                , clangPathFlag
                , llcPathFlag
                , optPathFlag
                , llvmMcPathFlag
                  -- Optimization
                , optLevelFlag
                  -- Export
                , cfgExportFlag
                , fnsExportFlag
                , llvmExportFlag
                , objectExportFlag
                , relinkerInfoExportFlag
                ]

-- | Flag to set the path to the binary to analyze.
filenameArg :: Arg Args
filenameArg = Arg { argValue = setFilename
                  , argType = "FILE"
                  , argRequire = False
                  }
  where setFilename :: String -> Args -> Either String Args
        setFilename nm a = Right (a { programPath = nm })

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
  e <- readElf64 (programPath args)
  let sections = filter isCodeSection $ e^..elfSections
  when (null sections) $ do
    hPutStrLn stderr "Binary contains no executable sections."
    exitFailure
  writeOutput (outputPath args) $ \h -> do
    forM_ sections $ \s -> do
      printX86SectionDisassembly h (elfSectionName s) (elfSectionAddr s) (elfSectionData s)

loadOptions :: Args -> LoadOptions
loadOptions args = LoadOptions { loadOffset = loadBaseAddress args }

argsReoptOptions :: Args -> ReoptOptions
argsReoptOptions args = ReoptOptions { roIncluded = args^.includeAddrs
                                     , roExcluded = args^.excludeAddrs
                                     }

-- | Discovery symbols in program and show function CFGs.
showCFG :: Args -> IO String
showCFG args = do
  hdrAnn <- resolveHeader (headerPath args) (clangPath args)
  Some discState <-
    discoverBinary printLogEvent
                   (programPath args) (loadOptions args) (args^.discOpts) (argsReoptOptions args) hdrAnn
  pure $ show $ ppDiscoveryStateBlocks discState

------------------------------------------------------------------------
-- Reopt action

-- | Return true if Reopt should compile LLVM
shouldCompileLLVM :: Args -> Bool
shouldCompileLLVM args
  =  isJust (objectExportPath args)
  || isJust (outputPath args)
  || isJust (occamConfigPath args)


-- | Retun true if we should generate LLVM
shouldGenerateLLVM :: Args -> Bool
shouldGenerateLLVM args
  =  isJust (llvmExportPath args)
  || shouldCompileLLVM  args

collectInvariants :: IORef [Aeson.Encoding]
                  -> ReoptLogEvent X86_64
                  -> IO ()
collectInvariants ref evt = do
  case evt of
    ReoptFunStepFinished InvariantInference (FunId addr _mnm) invMap -> do
      let enc = encodeInvariantMsg addr invMap
      seq enc $ modifyIORef' ref $ (enc:)
    ReoptFunStepFailed InvariantInference (FunId addr _mnm) (ReoptInvariantInferenceFailureTag, msg) -> do
      let enc = encodeInvariantFailedMsg addr msg
      seq enc $ modifyIORef' ref $ (enc:)
    _ -> do
      pure ()

-- | This command is called when reopt is called with no specific
-- action.
performReopt :: Args -> IO ()
performReopt args = do
  let somethingToDo = isJust (cfgExportPath args)
                   || isJust (fnsExportPath args)
                   || isJust (invariantsExportPath args)
                   || isJust (relinkerInfoExportPath args)
                   || shouldGenerateLLVM args
  when (not somethingToDo) $ do
    hPutStrLn stderr $ "Nothing to do\n"
      ++ "  Specify an output via --output or other info to export."
    exitFailure

  hdrAnn <- resolveHeader (headerPath args) (clangPath args)
  let funPrefix :: BSC.ByteString
      funPrefix = unnamedFunPrefix args

  let elfPath = programPath args
  origElf <- parseElfHeaderInfo64 elfPath =<< checkedReadFile elfPath

  invariantsRef <- newIORef []

  let logger =
        case invariantsExportPath args of
          Just _ -> joinLogEvents printLogEvent (collectInvariants invariantsRef)
          Nothing -> printLogEvent

  (os, _, recMod, relinkerInfo) <-
    runReoptInIO logger $ do
      recoverX86Elf' (loadOptions args) (args^.discOpts) (argsReoptOptions args) hdrAnn funPrefix origElf

  -- Write invariants
  case invariantsExportPath args of
    Nothing -> pure ()
    Just path -> do
      invariants <- reverse <$> readIORef invariantsRef
      let onErr :: IOException -> IO ()
          onErr e = do
            hPutStrLn stderr "Error writing annotations:"
            hPutStrLn stderr $ "  " <> show e
            exitFailure
      let buffer = Builder.toLazyByteString (AE.fromEncoding (AE.list id invariants))
      BSL.writeFile path buffer `catch` onErr

  case relinkerInfoExportPath args of
    Nothing -> pure ()
    Just path -> do
      let onErr :: IOException -> IO ()
          onErr e = do
            hPutStrLn stderr "Error writing annotations:"
            hPutStrLn stderr $ "  " <> show e
            exitFailure
      Aeson.encodeFile path relinkerInfo  `catch` onErr

--  stats <- readIORef statsRef
--  reportStats (printStats args) (exportStatsPath args) stats
  when (not (shouldGenerateLLVM args)) $ do
    exitFailure
  -- Generate LLVM
  let (objLLVM,_) = renderLLVMBitcode (llvmGenOptions args) (llvmVersion args) os recMod
  -- Write LLVM if requested.
  case llvmExportPath args of
    Nothing -> pure ()
    Just path -> do
      let onErr :: IOException -> IO ()
          onErr e = do
            hPutStrLn stderr "Error writing LLVM file:"
            hPutStrLn stderr $ "  " <> show e
            exitFailure
      builderWriteFile path objLLVM `catch` onErr

  unless (shouldCompileLLVM args) $ do
    exitSuccess
  -- Compile LLVM
  objContents <- do
    contents <- compileLLVM (optLevel args) (optPath args) (llcPath args) (llvmMcPath args)
                            (osLinkName os) (Left objLLVM)
    case objectExportPath args of
      Nothing -> pure ()
      Just path -> do
        let onErr :: IOException -> IO ()
            onErr e = do
              hPutStrLn stderr "Error writing object file:"
              hPutStrLn stderr $ "  " <> show e
              exitFailure
        BS.writeFile path contents `catch` onErr
    case occamConfigPath args of
      Nothing -> pure contents
      Just cfgPath -> do
        let (_, fileName) = splitFileName $ programPath args
        maybeRes <- runExceptT $ run_slash "slash" cfgPath fileName objLLVM
        case maybeRes of
          Left e -> do
            hPutStrLn stderr (show e)
            exitFailure
          Right slashedBC -> compileLLVM (optLevel args) (optPath args) (llcPath args) (llvmMcPath args)
                                        (osLinkName os) (Right slashedBC)
  -- Merge and write out
  outPath <-
    case outputPath args of
      Nothing -> do
        hPutStrLn stderr "Skipping final relinking: Use --output <filename> if final binary expected."
        exitSuccess
      Just outPath ->
        pure outPath
  hPutStrLn stderr "Performing final relinking."
  mergeAndWrite outPath origElf "new object" objContents relinkerInfo

getFunctions :: Args -> IO (X86OS, RecoveredModule X86_64, ReoptSummary, ReoptStats)
getFunctions args =
  recoverFunctions (programPath args)
                   (clangPath args)
                   (loadOptions args)
                   (args^.discOpts)
                   (argsReoptOptions args)
                   (headerPath args)
                   (unnamedFunPrefix args)

main' :: IO ()
main' = do
  args <- getCommandLineArgs
  setDebugKeys (args ^. debugKeys)
  case args^.reoptAction of
    DumpDisassembly -> do
      dumpDisassembly args
    ShowCFG ->
      writeOutput (outputPath args) $ \h -> do
        hPutStrLn h =<< showCFG args
    -- Write function discovered
    ShowFunctions -> do
      (_,recMod, _summary, _stats) <- getFunctions args
      writeOutput (outputPath args) $ \h -> do
        mapM_ (hPutStrLn h . show . pretty) (recoveredDefs recMod)
    ShowLLVM -> do
      when (isJust (annotationsExportPath args) && isNothing (outputPath args)) $ do
        hPutStrLn stderr "Must specify --output for LLVM when generating annotations."
        exitFailure
      (os, recMod, _summary, _stats) <- getFunctions args
      let (llvmMod, mFunAnn) = renderLLVMBitcode (llvmGenOptions args)
                                                 (llvmVersion args)
                                                 os
                                                 recMod
      case annotationsExportPath args of
        Nothing -> pure ()
        Just annPath -> do
          let (annErrs, funAnn) = partitionEithers mFunAnn
          -- Print annotation errors
          forM_ annErrs $ \e -> do
            hPutStrLn stderr $ "Annotation error: " ++ e
          -- Write out annotation.
          let Just llvmPath = outputPath args
          let vcgAnn :: Ann.ModuleAnnotations
              vcgAnn = Ann.ModuleAnnotations
                { Ann.llvmFilePath = llvmPath
                , Ann.binFilePath = programPath args
                , Ann.pageSize = 4096
                , Ann.stackGuardPageCount = 1
                , Ann.functions = funAnn
                }
          BSL.writeFile annPath (Aeson.encode vcgAnn)
      writeOutput (outputPath args) $ \h -> do
        Builder.hPutBuilder h llvmMod
    ServerMode -> do
      runServer
    ShowHelp -> do
      print $ helpText [] HelpFormatAll arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
    Reopt -> do
      performReopt args

main :: IO ()
main = main' `catch` h
  where h e
          | isUserError e = do
            hPutStrLn stderr "User error"
            hPutStrLn stderr $ ioeGetErrorString e
          | otherwise = do
            hPutStrLn stderr "Other error"
            hPutStrLn stderr $ show e
            hPutStrLn stderr $ show (ioeGetErrorType e)
