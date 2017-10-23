{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Either
import           Data.ElfEdit
import           Data.Foldable
import           Data.List ((\\), nub, stripPrefix, intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList, catMaybes)
import           Data.Monoid
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Tuple (swap)
import           Data.Type.Equality
import qualified Data.Vector as V
import           Data.Version
import           Data.Word
import qualified Data.Yaml as Yaml
import           GHC.IO (ioToST, stToIO)
import           Numeric (readHex)
import           System.Console.CmdArgs.Explicit
import           System.Directory (doesFileExist)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.IO.Temp
import           System.Posix.Files
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import qualified Text.PrettyPrint.HughesPJ as HPJ
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>), (<>))

import           Paths_reopt (getLibDir, version)

#ifdef SUPPORT_ARM
import qualified Data.VEX.FFI
import           Data.Macaw.ARM
#endif

import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Discovery
import           Data.Macaw.Memory
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.X86Reg (X86Reg)

import           Reopt
import           Reopt.CFG.FnRep (Function(..))
import           Reopt.CFG.FunctionArgs (DemandSet, functionDemands, inferFunctionTypeFromDemands)
import           Reopt.CFG.FunctionCheck
import qualified Reopt.CFG.LLVM as LLVM
import           Reopt.CFG.Recovery
import qualified Reopt.ExternalTools as Ext
import           Reopt.Relinker

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
-- LLVMVersion

-- | Version of LLVM to generate
data LLVMVersion
   = LLVM35
   | LLVM36
   | LLVM37
   | LLVM38

-- | Convert a string to the LLVM version identifier.
asLLVMVersion :: String -> Maybe LLVMVersion
asLLVMVersion s =
  case s of
    "llvm35" -> Just LLVM35
    "llvm36" -> Just LLVM36
    "llvm37" -> Just LLVM37
    "llvm38" -> Just LLVM38
    _ -> Nothing


-- | Pretty print an LLVM module using the format expected by the given LLVM version.
ppLLVM :: LLVMVersion -> L.Module -> HPJ.Doc
ppLLVM LLVM35 m = LPP.ppLLVM35 $ LPP.ppModule m
ppLLVM LLVM36 m = LPP.ppLLVM36 $ LPP.ppModule m
ppLLVM LLVM37 m = LPP.ppLLVM37 $ LPP.ppModule m
ppLLVM LLVM38 m = LPP.ppLLVM38 $ LPP.ppModule m

------------------------------------------------------------------------
-- Discovery options

-- | Options controlling discovery
data DiscoveryOptions
   = DiscoveryOptions { logAtAnalyzeFunction  :: !Bool
                        -- ^ Print a message each time we apply discovery analysis to
                        -- a new function.
                      , logAtAnalyzeBlock     :: !Bool
                        -- ^ Print a message each time we analyze a block within a
                        -- function.
                      , exploreFunctionSymbols :: !Bool
                        -- ^ Flag indicates we should explore function symbols
                      , exploreCodeAddrInMem :: !Bool
                        -- ^ Flag indicating we should explore all potential code
                        -- addresses in memory
                      }

defaultDiscoveryOptions :: DiscoveryOptions
defaultDiscoveryOptions =
  DiscoveryOptions { logAtAnalyzeFunction = True
                   , logAtAnalyzeBlock      = False
                   , exploreFunctionSymbols = True
                   , exploreCodeAddrInMem = False
                   }

------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = DumpDisassembly -- ^ Print out disassembler output only.
   | ShowCFG         -- ^ Print out control-flow microcode.
   | ShowFn String   -- ^ Print a specific function
   | ShowCFGAI       -- ^ Print out control-flow microcode + abs domain
   | ShowLLVM String -- ^ Write out the LLVM into the argument path
   | ShowFunctions   -- ^ Print out generated functions
   | ShowGaps        -- ^ Print out gaps in discovered blocks
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version
   | Relink          -- ^ Link an existing binary and new code together.
   | Reopt           -- ^ Perform a full reoptimization
  deriving (Show)

-- | Command line arguments.
data Args = Args { _reoptAction  :: !Action
                 , _programPath  :: !FilePath
                 , _argsLoadStyle    :: !LoadStyle
                 , _debugKeys    :: [DebugClass]
                 , _newobjPath   :: !FilePath
                 , _redirPath    :: !FilePath
                 , _outputPath   :: !FilePath
                 , _gasPath      :: !FilePath
                 , _llvmVersion  :: !LLVMVersion
                   -- ^ Version to use when printing LLVM.
                 , _llcPath      :: !FilePath
                 , _optPath      :: !FilePath
                 , _optLevel     :: !Int
                 , _llvmLinkPath :: !FilePath
                 , _libreoptPath :: !(Maybe FilePath)
                 , _notransAddrs :: !(Set String)
                   -- ^ Set of function entry points that we ignore for translation.
                 , _discOpts :: !DiscoveryOptions
                   -- ^ Options affecting discovery
                 }

-- | Action to perform when running
reoptAction :: Simple Lens Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Path for main executable
programPath :: Simple Lens Args FilePath
programPath = lens _programPath (\s v -> s { _programPath = v })

-- | Whether to load file by segment or sections
argsLoadStyle :: Simple Lens Args LoadStyle
argsLoadStyle = lens _argsLoadStyle (\s v -> s { _argsLoadStyle = v })

-- | Which debug keys (if any) to output
debugKeys :: Simple Lens Args [DebugClass]
debugKeys = lens _debugKeys (\s v -> s { _debugKeys = v })

-- | Path to new object code for relinker
newobjPath :: Simple Lens Args FilePath
newobjPath = lens _newobjPath (\s v -> s { _newobjPath = v })

-- | Path to JSON file describing the redirections
redirPath :: Simple Lens Args FilePath
redirPath = lens _redirPath (\s v -> s { _redirPath = v })

-- | Path to JSON file describing the output
outputPath :: Simple Lens Args FilePath
outputPath = lens _outputPath (\s v -> s { _outputPath = v })

-- | Path to GNU assembler
gasPath :: Simple Lens Args FilePath
gasPath = lens _gasPath (\s v -> s { _gasPath = v })

-- | Version to use when printing LLVM.
llvmVersion :: Simple Lens Args LLVMVersion
llvmVersion = lens _llvmVersion (\s v -> s { _llvmVersion = v })

-- | Path to llc
llcPath :: Simple Lens Args FilePath
llcPath = lens _llcPath (\s v -> s { _llcPath = v })

-- | Path to opt
optPath :: Simple Lens Args FilePath
optPath = lens _optPath (\s v -> s { _optPath = v })

-- | Optimization level to pass to llc and opt
optLevel :: Simple Lens Args Int
optLevel = lens _optLevel (\s v -> s { _optLevel = v })

-- | Path to llvm-link
llvmLinkPath :: Simple Lens Args FilePath
llvmLinkPath = lens _llvmLinkPath (\s v -> s { _llvmLinkPath = v })

-- | Path to libreopt
libreoptPath :: Simple Lens Args (Maybe FilePath)
libreoptPath = lens _libreoptPath (\s v -> s { _libreoptPath = v })

-- | Set of function entry points that we ignore for translation.
notransAddrs :: Simple Lens Args (Set String)
notransAddrs = lens _notransAddrs (\s v -> s { _notransAddrs = v })

-- | Set of function entry points that we ignore for translation.
discOpts :: Simple Lens Args DiscoveryOptions
discOpts = lens _discOpts (\s v -> s { _discOpts = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Reopt
                   , _programPath = ""
                   , _argsLoadStyle = LoadBySegment
                   , _debugKeys = []
                   , _newobjPath = ""
                   , _redirPath  = ""
                   , _outputPath = "a.out"
                   , _gasPath = "gas"
                   , _llvmVersion = LLVM38
                   , _llcPath = "llc"
                   , _optPath = "opt"
                   , _optLevel  = 2
                   , _llvmLinkPath = "llvm-link"
                   , _libreoptPath = Nothing
                   , _notransAddrs = Set.empty
                   , _discOpts     = defaultDiscoveryOptions
                   }

loadOpt :: Args -> LoadOptions
loadOpt args =  LoadOptions { loadStyle = args^.argsLoadStyle
                            , includeBSS = False
                            }

------------------------------------------------------------------------
-- Flags

disassembleFlag :: Flag Args
disassembleFlag = flagNone [ "disassemble", "d" ] upd help
  where upd  = reoptAction .~ DumpDisassembly
        help = "Disassemble code segment of binary, and print it in an objdump style."

cfgFlag :: Flag Args
cfgFlag = flagNone [ "cfg", "c" ] upd help
  where upd  = reoptAction .~ ShowCFG
        help = "Print out the functions recovered from an executable."

showFnFlag :: Flag Args
showFnFlag = flagReq [ "show-fn" ] upd "FUNCTION" help
  where upd s old = Right $ old & reoptAction .~ ShowFn s
        help = "Print out a specific recovered function."

cfgAIFlag :: Flag Args
cfgAIFlag = flagNone [ "ai", "a" ] upd help
  where upd  = reoptAction .~ ShowCFGAI
        help = "Print out recovered control flow graph + AI of executable."

llvmFlag :: Flag Args
llvmFlag = flagReq [ "llvm", "l" ] upd "DIR" help
  where upd s old = Right $ old & reoptAction .~ ShowLLVM s
        help = "Write out generated LLVM."

llvmVersionFlag :: Flag Args
llvmVersionFlag = flagReq [ "llvm-version" ] upd "VERSION" help
  where upd :: String -> Args -> Either String Args
        upd s old =
          case asLLVMVersion s of
            Just v -> Right $ old & llvmVersion .~ v
            Nothing -> Left $
              unlines [ "Could not interpret llvm version " ++  s
                      , "  Expects one of: llvm35, llvm36, llvm37, llvm38"
                      ]
        help = unlines
          [ "Specify LLVM version."
          , "  Expects one of: llvm35, llvm36, llvm37, llvm38"
          ]

funFlag :: Flag Args
funFlag = flagNone [ "functions", "f" ] upd help
  where upd  = reoptAction .~ ShowFunctions
        help = "Print out functions after stack and argument recovery."

gapFlag :: Flag Args
gapFlag = flagNone [ "gap", "g" ] upd help
  where upd  = reoptAction .~ ShowGaps
        help = "Print out gaps in the recovered control flow graph of executable."

relinkFlag :: Flag Args
relinkFlag = flagNone [ "relink" ] upd help
  where upd  = reoptAction .~ Relink
        help = "Link a binary with new object code."

segmentFlag :: Flag Args
segmentFlag = flagNone [ "load-segments" ] upd help
  where upd  = argsLoadStyle .~ LoadBySegment
        help = "Load the Elf file using segment information (default)."

sectionFlag :: Flag Args
sectionFlag = flagNone [ "load-sections" ] upd help
  where upd  = argsLoadStyle .~ LoadBySection
        help = "Load the Elf file using section information."

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

newobjFlag :: Flag Args
newobjFlag = flagReq [ "new" ] upd "PATH" help
  where upd s old = Right $ old & newobjPath .~ s
        help = "Path to new object code to link into existing binary."

redirFlag :: Flag Args
redirFlag = flagReq [ "r", "redirections" ] upd "PATH" help
  where upd s old = Right $ old & redirPath .~ s
        help = "Path to redirections JSON file that specifies where to patch existing code."

outputFlag :: Flag Args
outputFlag = flagReq [ "o", "output" ] upd "PATH" help
  where upd s old = Right $ old & outputPath .~ s
        help = "Path to write new binary."

gasFlag :: Flag Args
gasFlag = flagReq [ "gas" ] upd "PATH" help
  where upd s old = Right $ old & gasPath .~ s
        help = "Path to GNU assembler."

-- | Flag to set llc path.
llcPathFlag :: Flag Args
llcPathFlag = flagReq [ "llc" ] upd "PATH" help
  where upd s old = Right $ old & llcPath .~ s
        help = "Path to llc."

-- | Flag to set path to opt.
optFlag :: Flag Args
optFlag = flagReq [ "opt" ] upd "PATH" help
  where upd s old = Right $ old & optPath .~ s
        help = "Path to opt."

-- | Flag to set llc optimization level.
optLevelFlag :: Flag Args
optLevelFlag = flagReq [ "opt-level" ] upd "PATH" help
  where upd s old =
          case reads s of
            [(lvl, "")] | 0 <= lvl && lvl <= 3 -> Right $ old & optLevel .~ lvl
            _ -> Left "Expected optimization level to be a number between 0 and 3."
        help = "Optimization level."

llvmLinkFlag :: Flag Args
llvmLinkFlag = flagReq [ "llvm-link" ] upd "PATH" help
  where upd s old = Right $ old & llvmLinkPath .~ s
        help = "Path to llvm-link."

libreoptFlag :: Flag Args
libreoptFlag = flagReq [ "lib" ] upd "PATH" help
  where upd s old = Right $ old & libreoptPath .~ Just s
        help = "Path to libreopt.bc."

-- | Used to add a new no-translate add
notransFlag :: Flag Args
notransFlag = flagReq [ "notrans" ] upd "ADDR" help
  where upd s old = Right $ old & notransAddrs %~ Set.insert s
        help = "Address of function to omit from translation (may be repeated)."

-- | Print out a trace message when we analyze a function
logAtAnalyzeFunctionFlag :: Flag Args
logAtAnalyzeFunctionFlag = flagBool [ "trace-function-discovery" ] upd help
  where upd b = discOpts %~ \o -> o { logAtAnalyzeFunction = b }
        help = "Log when starting analysis of a discovered function."

-- | Print out a trace message when we analyze a function
logAtAnalyzeBlockFlag :: Flag Args
logAtAnalyzeBlockFlag = flagBool [ "trace-block-discovery" ] upd help
  where upd b = discOpts %~ \o -> o { logAtAnalyzeBlock = b }
        help = "Log when starting analysis of a discovered block within a function."

exploreFunctionSymbolsFlag :: Flag Args
exploreFunctionSymbolsFlag = flagBool [ "include-syms" ] upd help
  where upd b = discOpts %~ \o -> o { exploreFunctionSymbols = b }
        help = "Include function symbols in discovery."

exploreCodeAddrInMemFlag :: Flag Args
exploreCodeAddrInMemFlag = flagBool [ "include-mem" ] upd help
  where upd b = discOpts %~ \o -> o { exploreCodeAddrInMem = b }
        help = "Include memory code addresses in discovery."

arguments :: Mode Args
arguments = mode "reopt" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ disassembleFlag
                , cfgFlag
                , showFnFlag
                , cfgAIFlag
                , llvmFlag
                , llvmVersionFlag
                , funFlag
                , gapFlag
                , segmentFlag
                , sectionFlag
                , debugFlag
                , relinkFlag
                , newobjFlag
                , redirFlag
                , outputFlag
                , gasFlag
                , llcPathFlag
                , optFlag
                , optLevelFlag
                , llvmLinkFlag
                , libreoptFlag
                , notransFlag
                , flagHelpSimple (reoptAction .~ ShowHelp)
                , flagVersion (reoptAction .~ ShowVersion)
                , logAtAnalyzeFunctionFlag
                , logAtAnalyzeBlockFlag
                , exploreFunctionSymbolsFlag
                , exploreCodeAddrInMemFlag
                ]

reoptVersion :: String
reoptVersion = "Reopt binary reoptimizer (reopt) "
             ++ versionString ++ ", June 2014."
  where [h,l,r] = versionBranch version
        versionString = show h ++ "." ++ show l ++ "." ++ show r

copyrightNotice :: String
copyrightNotice = "Copyright 2014 Galois, Inc. All rights reserved."

filenameArg :: Arg Args
filenameArg = Arg { argValue = setFilename
                  , argType = "FILE"
                  , argRequire = False
                  }
  where setFilename :: String -> Args -> Either String Args
        setFilename nm a = Right (a & programPath .~ nm)

getCommandLineArgs :: IO Args
getCommandLineArgs = do
  argStrings <- getArgs
  case process arguments argStrings of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right v -> return v

------------------------------------------------------------------------
-- Execution

dumpDisassembly :: FilePath -> IO ()
dumpDisassembly path = do
  e <- readElf64 path
  let sections = filter isCodeSection $ e^..elfSections
  when (null sections) $ do
    putStrLn "Binary contains no executable sections."
  mapM_ printSectionDisassembly sections


ppSymbol :: MemWidth w => MemSegmentOff w -> SymbolAddrMap w -> String
ppSymbol addr sym_map =
  case symbolAtAddr addr sym_map of
    Just fnName -> show addr ++ " (" ++ UTF8.toString fnName ++ ")"
    Nothing  -> show addr

resolveFuns :: MemWidth (RegAddrWidth (ArchReg arch))
            => DiscoveryOptions -- ^ Options controlling discovery
            -> SymbolAddrMap (ArchAddrWidth arch)
            -> DiscoveryState arch
            -> IO (DiscoveryState arch)
resolveFuns disOpt sym_map info = seq info $
  case Map.lookupMin (info^.unexploredFunctions) of
    Nothing -> pure info
    Just (addr, rsn) -> do
      when (logAtAnalyzeFunction disOpt) $ do
        hPutStrLn stderr $ "Analyzing function: " ++ ppSymbol addr sym_map
      info' <- stToIO $ analyzeFunction (blockLogFn disOpt) addr rsn info
      resolveFuns disOpt sym_map (fst info')

-- | Return the segment offset of the elf file entry point or fail if undefined.
getElfEntry :: Monad m => Memory w -> Elf w -> m (MemSegmentOff w)
getElfEntry mem e =  addrWidthClass (memAddrWidth mem) $ do
  elfClassInstances (elfClass e) $ do
  case resolveAbsoluteAddr mem (fromIntegral (elfEntry e)) of
    Nothing -> fail "Could not resolve entry"
    Just v  -> pure v

getSymbolMap :: Monad m => Memory w -> Elf w -> m (SymbolAddrMap w)
getSymbolMap mem e = do
  entries <-
    case elfSymtab e of
      [] -> pure $ []
      [tbl] -> pure $ V.toList (elfSymbolTableEntries tbl)
      _ -> fail "Elf contains multiple symbol tables."

  let resolved = resolvedSegmentedElfFuncSymbols mem entries
  -- Check for unresolved symbols
  case symbolAddrMap (head <$> resolved) of
    Left msg -> fail msg
    Right m -> pure m

-- | Create a discovery state and symbol-address map
mkFinalCFGWithSyms :: forall arch
                   .  ArchitectureInfo arch
                   -> Memory (ArchAddrWidth arch) -- ^ Layout in memory of file
                   -> Elf (ArchAddrWidth arch) -- ^ Elf file to create CFG for.
                   -> DiscoveryOptions -- ^ Options controlling discovery
                   -> IO (DiscoveryState arch, SymbolAddrMap (ArchAddrWidth arch))
mkFinalCFGWithSyms ainfo mem e disOpt = withArchConstraints ainfo $ do
  -- Get meap from addresses to symbol names
  sym_map <- getSymbolMap mem e
  -- Create initial discovery state
  entry <- getElfEntry mem e
  let ds0 = emptyDiscoveryState mem sym_map ainfo
          & markAddrsAsFunction InitAddr [entry]
  -- Add symbol table entries to discovery state if requested
  let ds1 | exploreFunctionSymbols disOpt =
              ds0 & markAddrsAsFunction InitAddr (symbolAddrs sym_map)
          | otherwise = ds0
  -- Perform discovery
  ds2 <- resolveFuns disOpt sym_map ds1
  -- Discover functions from arbitrary mem pointers
  ds3 <-
    if exploreCodeAddrInMem disOpt then do
      let mem_contents = withArchConstraints ainfo $ memAsAddrPairs mem LittleEndian
      resolveFuns disOpt sym_map $ ds2 & exploreMemPointers mem_contents
     else
      return ds2
  -- Return
  pure (ds3, sym_map)

data SomeArchitectureInfo w =
  forall arch
    . ( w ~ RegAddrWidth (ArchReg arch)
      )
    => SomeArch (ArchitectureInfo arch)

getElfArchInfo :: Elf w -> IO (SomeArchitectureInfo w)
getElfArchInfo e =
  case (elfClass e, elfMachine e, elfOSABI e) of
    (ELFCLASS64, EM_X86_64, ELFOSABI_LINUX)   -> pure (SomeArch x86_64_linux_info)
    (ELFCLASS64, EM_X86_64, ELFOSABI_SYSV)    -> pure (SomeArch x86_64_linux_info)
    (ELFCLASS64, EM_X86_64, ELFOSABI_FREEBSD) -> pure (SomeArch x86_64_freeBSD_info)
#ifdef SUPPORT_ARM
    (ELFCLASS32, EM_ARM, ELFOSABI_SYSV) -> do
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure (SomeArch armArchle)
#endif
    (cl, arch, abi) -> do
     fail $ "Do not support " ++ show (elfClassBitWidth cl) ++ "-bit "
            ++ show arch ++ "-" ++ show abi ++ "binaries."

getX86ElfArchInfo :: Elf 64 -> IO (ArchitectureInfo X86_64, SyscallPersonality X86_64, String)
getX86ElfArchInfo e =
  case elfOSABI e of
    ELFOSABI_LINUX   -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_SYSV    -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_FREEBSD -> pure (x86_64_freeBSD_info, freeBSD_syscallPersonality, "FreeBSD")
    abi              ->
     fail $ "Do not support " ++ show EM_X86_64 ++ "-" ++ show abi ++ "binaries."

showNonfatalErrors :: (Eq (ElfWordType w), Num (ElfWordType w), Show (ElfWordType w))
                   => [ElfParseError w] -> IO ()
showNonfatalErrors l = do
  when (not (null l)) $ do
    hPutStrLn stderr $ "Recoverable errors occurred in reading elf file:"
    forM_ l $ \emsg -> do
      hPutStrLn stderr (show emsg)

readSomeElf :: FilePath -> IO (Some Elf)
readSomeElf path = do
  when (null path) $ do
    hPutStrLn stderr "Please specify a path."
    hPutStrLn stderr "For help on using reopt, run \"reopt --help\"."
    exitFailure
  let h e | isDoesNotExistError e = do
            hPutStrLn stderr $ path ++ " does not exist."
            hPutStrLn stderr "For help on using reopt, run \"reopt --help\"."
            exitFailure
          | isUserError e = do
            hPutStrLn stderr (ioeGetErrorString e)
            exitFailure
          | otherwise = do
            hPutStrLn stderr (show e)
            hPutStrLn stderr (show (ioeGetErrorType e))
            exitFailure
  bs <- BS.readFile path `catch` h
  case parseElf bs of
    ElfHeaderError _ msg -> do
      hPutStrLn stderr $ "Error reading " ++ path ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Elf32Res l e -> do
      showNonfatalErrors l
      return (Some e)
    Elf64Res l e -> do
      showNonfatalErrors l
      return (Some e)

showCFG :: Args -> IO ()
showCFG args = do
  Some e <- readSomeElf (args^.programPath)
  -- Get architecture information for elf
  SomeArch ainfo <- getElfArchInfo e
  (_,mem) <- either fail return $ memoryForElf (loadOpt args) e
  (disc_info, _) <- mkFinalCFGWithSyms ainfo mem e (args^.discOpts)
  print $ ppDiscoveryStateBlocks disc_info

-- | Attempt to find the address of a string identifying a symbol name, and
-- return either the string if it cannot be resolved or the address.
resolveSymAddr :: Memory w
               -> Map BS.ByteString (MemSegmentOff w)
                 -- ^ Map from symbol names in binary to their address.
              -> String
                 -- ^ The name of a symbol as a string.
              -> Either String (MemSegmentOff w)
resolveSymAddr mem symMap nm0 = addrWidthClass (memAddrWidth mem) $
  case resolveSymName nm0 of
    Left w ->
      case resolveAbsoluteAddr mem (fromIntegral w) of
        Just off -> Right off
        Nothing -> Left nm0
    Right nm -> do
      case Map.lookup (fromString nm) symMap of
         Just addr -> Right addr
         Nothing -> Left nm

blockLogFn :: MemWidth w => DiscoveryOptions -> MemSegmentOff w -> ST RealWorld ()
blockLogFn disOpt
  | logAtAnalyzeBlock disOpt = \addr ->
     ioToST $ hPutStrLn stderr $ "  Analyzing block: " ++ show addr
  | otherwise = \_ -> pure ()

showFn :: Args -> String -> IO ()
showFn args functionName = do
  Some e <- readSomeElf (args^.programPath)
  -- Get architecture information for elf
  SomeArch ainfo <- getElfArchInfo e
  withArchConstraints ainfo $ do
  (secMap,mem) <- either fail return $ memoryForElf (loadOpt args) e
  addr <-
    case resolveSymAddr mem (elfSymAddrMap secMap e) functionName of
      Left nm -> fail $ "Could not resolve " ++ nm
      Right a -> pure a

  -- Get meap from addresses to symbol names
  sym_map <- getSymbolMap mem e


  when (logAtAnalyzeFunction (args^.discOpts)) $ do
    hPutStrLn stderr $ "Analyzing function: " ++ ppSymbol addr sym_map

  let ds0 = emptyDiscoveryState mem sym_map ainfo
  (_, Some fnInfo) <- stToIO $ analyzeFunction (blockLogFn (args^.discOpts)) addr InitAddr ds0
  print $ pretty fnInfo

-- | Try to recover function information from the information
-- recovered during code discovery.
getFns :: SyscallPersonality X86_64
       -> Map BS.ByteString (MemSegmentOff 64)
          -- ^ Maps symbol names to addresses
       -> Set String
          -- ^ Name of symbols/addresses to exclude
       -> DiscoveryState X86_64
          -- ^ Information about original binary recovered from static analysis.
       -> IO [Function]
getFns sysp symMap excludedNames info = do

  let mem = memory info
  -- Compute which functions to compute by looking at the binary
  let nms = Set.toList excludedNames
  let (bad, excludedAddrs) = partitionEithers $ resolveSymAddr mem symMap  <$> nms
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad

  let excludeSet = Set.fromList excludedAddrs
  -- Check that the address of the function is not one that we are excluding.
  let include :: Some (DiscoveryFunInfo X86_64) -> Bool
      include (Some f) = Set.notMember (discoveredFunAddr f) excludeSet

  let entries = filter include $ exploredFunctions info

  let fDems :: Map (MemSegmentOff 64) (DemandSet X86Reg)
      fDems = functionDemands sysp info
  let fArgs :: AddrToFunctionTypeMap
      fArgs = Map.mapKeys relativeSegmentAddr $ inferFunctionTypeFromDemands fDems
  seq fArgs $ do
  fmap catMaybes $ forM entries $ \(Some finfo) -> do
    let entry = discoveredFunAddr finfo
    case () of
      _ | checkFunction finfo -> do
            case recoverFunction sysp fArgs mem finfo of
              Left msg -> do
                hPutStrLn stderr $ "Could not recover function " ++ show entry ++ ":\n  " ++ msg
                pure Nothing
              Right fn -> do
                pure (Just fn)
        | otherwise -> do
            hPutStrLn stderr $ "Invalid function at " ++ show entry
            pure Nothing

showFunctions :: Args -> IO ()
showFunctions args = do
  e <- readElf64 (args^.programPath)
  -- Create memory for elf
  (ainfo, sysp,_) <- getX86ElfArchInfo e
  (secMap, mem) <- either fail return $ memoryForElf (loadOpt args) e
  (s,_) <- mkFinalCFGWithSyms ainfo mem e (args^.discOpts)
  fns <- getFns sysp (elfSymAddrMap secMap e) (args^.notransAddrs) s
  hPutStr stderr "Got fns"
  mapM_ (print . pretty) fns

------------------------------------------------------------------------
-- Pattern match on stack pointer possibilities.

-- | Extract list containing symbol names and addresses.
elfAddrSymEntries :: SectionIndexMap w
                     -- ^ Map from elf section indices to base address for section
                     -- and section in Elf file
                  -> Elf w
                  -> [(BS.ByteString, MemSegmentOff w)]
elfAddrSymEntries m binary =
  elfClassInstances (elfClass binary) $
  addrWidthClass (elfAddrWidth (elfClass binary)) $
  [ (steName entry, val)
  | tbl <- elfSymtab binary
  , entry <- V.toList $ elfSymbolTableEntries tbl
    -- Check this is a function or NOTYPE
  , steType entry `elem` [ STT_FUNC, STT_NOTYPE ]
    -- Compute address of symbol from section
  , let idx = steIndex entry
  , idx /= SHN_UNDEF && idx <= SHN_LORESERVE
    -- Get base index of section
  , (base, sec) <- maybeToList $ Map.lookup idx m
  , let diff = toInteger (steValue entry) - toInteger (elfSectionAddr sec)
  , val <- maybeToList $ incSegmentOff base diff
  ]

-- | Create map from symbol names to address.
elfSymAddrMap  :: SectionIndexMap w
                  -- ^ Map from elf section indices to base address for section
                  -- and section in Elf file
               -> Elf w
               -> Map BS.ByteString (MemSegmentOff w)
elfSymAddrMap m binary = Map.fromList $ elfAddrSymEntries m binary

-- | Create map from addresses to symbol name.
--
-- Used for naming functions.
elfAddrSymMap :: SectionIndexMap w
              -> Elf w
              -> LLVM.AddrSymMap w
elfAddrSymMap m binary = Map.fromList $ swap <$> elfAddrSymEntries m binary

-- | Merge a binary and new object
mergeAndWrite :: FilePath
              -> Elf 64 -- ^ Original binary
              -> Elf 64 -- ^ New object
              -> SymbolNameToAddrMap Word64 -- ^ Extra rdictions
              -> [CodeRedirection Word64] -- ^ List of redirections from old binary to new.
              -> IO ()
mergeAndWrite output_path orig_binary new_obj extra_syms redirs = do
  putStrLn $ "Performing final relinking."
  let (mres, warnings) = mergeObject orig_binary new_obj extra_syms redirs
  when (hasRelinkWarnings warnings) $ do
    hPrint stderr (pretty warnings)
  case mres of
    Left e -> fail e
    Right new_binary -> do
      BSL.writeFile output_path $ renderElf new_binary
      -- Update the file mode
      do fs <- getFileStatus output_path
         let fm = ownerExecuteMode
               .|. groupExecuteMode
               .|. otherExecuteMode
         setFileMode output_path (fileMode fs `unionFileModes` fm)

performRedir :: Args -> IO ()
performRedir args = do
  -- Get original binary
  orig_binary <- readElf64 (args^.programPath)

  let output_path = args^.outputPath
  case args^.newobjPath of
    -- When no new object is provided, we just copy the input
    -- file to test out Elf decoder/encoder.
    "" -> do
      putStrLn $ "Copying binary to: " ++ output_path
      BSL.writeFile output_path $ renderElf orig_binary
    -- When a non-empty new obj is provided we test
    new_obj_path -> do
      putStrLn $ "new_obj_path: " ++ new_obj_path
      new_obj <- readElf64 new_obj_path
      redirs <-
        case args^.redirPath of
          "" -> return []
          redir_path -> do
            mredirs <- Yaml.decodeFileEither redir_path
            case mredirs of
              Left e -> fail $ show e
              Right r -> return r
      mergeAndWrite output_path orig_binary new_obj Map.empty redirs


llvmAssembly :: LLVMVersion -> L.Module -> Builder.Builder
llvmAssembly v m = HPJ.fullRender HPJ.PageMode 10000 1 pp mempty (ppLLVM v m)
  where pp :: HPJ.TextDetails -> Builder.Builder -> Builder.Builder
        pp (HPJ.Chr c)  b = Builder.charUtf8 c <> b
        pp (HPJ.Str s)  b = Builder.stringUtf8 s <> b
        pp (HPJ.PStr s) b = Builder.stringUtf8 s <> b

-- | Maps virtual addresses to the phdr at them.
type ElfSegmentMap w = Map (ElfWordType w) (Phdr w)

-- | Create an elf segment map from a layout.
elfSegmentMap :: forall w . ElfLayout w -> ElfSegmentMap w
elfSegmentMap l = elfClassInstances (elfLayoutClass l) $ foldl' insertElfSegment Map.empty (allPhdrs l)
  where insertElfSegment ::  Ord (ElfWordType w) => ElfSegmentMap w -> Phdr w -> ElfSegmentMap w
        insertElfSegment m p
          | elfSegmentType seg == PT_LOAD = Map.insert a p m
          | otherwise = m
          where seg = phdrSegment p
                a = elfSegmentVirtAddr (phdrSegment p)

-- | Lookup an address in the segment map, returning the index of the phdr
-- and the offset.
lookupElfOffset :: ElfSegmentMap 64 -> Word64 -> Maybe (Word16, Word64)
lookupElfOffset m a =
  case Map.lookupLE a m of
    Just (base, phdr) | a < base + phdrFileSize phdr ->
        Just (elfSegmentIndex seg, a - base)
      where seg = phdrSegment phdr
    _ -> Nothing

--------------------------------------------------------------------------------
-- ControlFlowTargetMap

-- | A map from all control flow targets in the program to the start address of
-- functions that may target them.
--
-- This is used to compute when it is safe to insert a redirection.  We want to
-- ensure that adding a redirection will not break unknow
newtype ControlFlowTargetSet w = CFTS { cfTargets :: Map (MemSegmentOff w) [MemSegmentOff w]
                                      }

-- | Return how many bytes of space there are to write after address without
-- ovewriting another control flow target.
lookupControlFlowTargetSpace :: forall w
                             .  MemWidth w
                             => MemSegmentOff w
                             -> ControlFlowTargetSet w
                             -> MemWord w
lookupControlFlowTargetSpace addr0 = go 0 addr0 addr0
  where seg = msegSegment addr0
        go :: MemWord w -> MemSegmentOff w -> MemSegmentOff w -> ControlFlowTargetSet w -> MemWord w
        go inc base addr s =
          case Map.lookupGT addr (cfTargets s) of
            Just (next,fns)
              | msegSegment addr == msegSegment next ->
                let d = msegOffset next - msegOffset addr
                 in if null (filter (/= base) fns) then
                      go (inc+d) base next s
                     else
                      inc+d
            _ ->
              if segmentSize seg >= msegOffset addr then
                segmentSize seg - msegOffset addr
               else
                0

addControlFlowTarget :: ControlFlowTargetSet w
                     -> MemSegmentOff w
                     -> MemSegmentOff w -- ^ Function entry point
                     -> ControlFlowTargetSet w
addControlFlowTarget m a f = m { cfTargets = Map.insertWith (++) a [f] (cfTargets m) }

addFunctionEntryPoint :: ControlFlowTargetSet w
                      -> MemSegmentOff w
                      -> ControlFlowTargetSet w
addFunctionEntryPoint s a = addControlFlowTarget s a a


addFunDiscoveryControlFlowTargets :: ControlFlowTargetSet (ArchAddrWidth arch)
                                  -> Some (DiscoveryFunInfo arch)
                                  -> ControlFlowTargetSet (ArchAddrWidth arch)
addFunDiscoveryControlFlowTargets m0 (Some f) =
  foldl' (\m b -> addControlFlowTarget m b (discoveredFunAddr f)) m0 (Map.keys (f^.parsedBlocks))

discoveryControlFlowTargets :: DiscoveryState arch -> ControlFlowTargetSet (ArchAddrWidth arch)
discoveryControlFlowTargets info =
  let m0 = CFTS { cfTargets = Map.empty }
      m = foldl' addFunDiscoveryControlFlowTargets m0 (exploredFunctions info)
   in foldl' addFunctionEntryPoint m (symbolAddrs (symbolNames info))

--------------------------------------------------------------------------------
-- Redirections

-- | This creates a code redirection or returns the address as failing.
addrRedirection :: ControlFlowTargetSet 64
                -> LLVM.AddrSymMap 64
                -> ElfSegmentMap 64
                -> Function
                -> Either (MemSegmentOff 64) (CodeRedirection Word64)
addrRedirection tgts addrSymMap m f = do
  let a = fnAddr f
  let w = case msegAddr a of
            Just absAddr -> fromIntegral absAddr
            Nothing -> error "Redirection does not yet support relocatable binaries."
  case lookupElfOffset m w of
    Nothing -> Left (fnAddr f)
    Just (idx,off) -> Right redir
      where L.Symbol sym_name = LLVM.functionName addrSymMap (fnAddr f)
            redir = CodeRedirection { redirSourcePhdr   = idx
                                    , redirSourceOffset = off
                                    , redirSourceSize   = fromIntegral (lookupControlFlowTargetSpace (fnAddr f) tgts)
                                    , redirTarget       = UTF8.fromString sym_name
                                    }


targetArch :: ElfOSABI -> IO String
targetArch abi =
  case abi of
    ELFOSABI_SYSV    -> return "x86_64-unknown-linux-elf"
    ELFOSABI_NETBSD  -> return "x86_64-unknown-freebsd-elf"
    ELFOSABI_FREEBSD -> return "x86_64-unknown-linux-elf"
    _ -> fail $ "Do not support architecture " ++ show abi ++ "."

-- | Compile a bytestring containing LLVM assembly or bitcode into an object.
--
-- This takses the
compile_llvm_to_obj :: Args -> String -> BS.ByteString -> FilePath -> IO ()
compile_llvm_to_obj args arch llvm obj_path = do
  -- Run llvm on resulting binary
  putStrLn "Compiling new code"
  mres <- runExceptT $ do
    -- Skip optimization if optLevel == 0
    llvm_opt <-
      if args^.optLevel /= 0 then do
        Ext.run_opt (args^.optPath) (args^.optLevel) llvm
       else
        pure llvm
    let llc_opts = Ext.LLCOptions { Ext.llc_triple    = Just arch
                                  , Ext.llc_opt_level = args^.optLevel
                                  }
    asm <- Ext.run_llc (args^.llcPath) llc_opts llvm_opt
    Ext.run_gas (args^.gasPath) asm obj_path

  case mres of
    Left f -> fail $ show f
    Right () -> return ()

-- | Link the object and libreopt path together and return new object.
link_with_libreopt :: FilePath -- ^ Path to directory to write temport files to.
                   -> Args -- ^ Arguments to function
                   -> String -- ^ Name of architecture
                   -> Builder.Builder -- ^ Object file.
                   -> IO BS.ByteString
link_with_libreopt obj_dir args arch obj_llvm = do
  libreopt_path <-
    case args^.libreoptPath of
      Just s -> return s
      Nothing -> (</> arch </> "libreopt.bc") <$> getLibDir

  do exists <- doesFileExist libreopt_path
     when (not exists) $ do
       fail $ "Could not find path to libreopt.bc needed to link object; tried " ++ libreopt_path

  let obj_llvm_path = obj_dir </> "obj.ll"
  writeFileBuilder obj_llvm_path obj_llvm

  mllvm <- runExceptT $
    Ext.run_llvm_link (args^.llvmLinkPath) [ obj_llvm_path, libreopt_path ]
  either (fail . show) return mllvm


resolveSymName :: String -> Either Word64 String
resolveSymName ('0':'x': nm) | [(w,"")] <- readHex nm = Left w
resolveSymName nm = Right nm

writeFileBuilder :: FilePath -> Builder.Builder -> IO ()
writeFileBuilder nm b = bracket (openBinaryFile nm WriteMode) hClose (\h -> Builder.hPutBuilder h b)

elfIs64Bit :: ElfClass w -> Maybe (w :~: 64)
elfIs64Bit ELFCLASS32 = Nothing
elfIs64Bit ELFCLASS64 = Just Refl


performReopt :: Args -> IO ()
performReopt args =
  withSystemTempDirectory "reopt." $ \obj_dir -> do
    -- Get original binary
    Some orig_binary <- readSomeElf (args^.programPath)

    (secMap, mem) <- either fail return $ memoryForElf (loadOpt args) orig_binary
    let addrSymMap = elfAddrSymMap secMap orig_binary
    let output_path = args^.outputPath
    let llvmVer = args^.llvmVersion

    -- Construct CFG from binary
--    (ainfo, sysp, syscallPostfix) <- getX86ElfArchInfo orig_binary


    case takeExtension output_path of
      ".bc" -> do
        hPutStrLn stderr $
          "Generating '.bc' (LLVM ASCII assembly) is not supported!\n" ++
          "Use '.ll' extension to get assembled LLVM bitcode, and then " ++
          "use 'llvm-as out.ll' to generate an 'out.bc' file."
        exitFailure
      ".blocks" -> do
        SomeArch ainfo <- getElfArchInfo orig_binary
        elfClassInstances (elfClass orig_binary) $ do
        withArchConstraints ainfo $ do
        (disc_info,_) <- mkFinalCFGWithSyms ainfo mem orig_binary (args^.discOpts)
        writeFile output_path $ show $ ppDiscoveryStateBlocks disc_info
      ".fns" -> do
        Just Refl <- pure $ elfIs64Bit $ elfClass orig_binary
        (ainfo, sysp, _) <- getX86ElfArchInfo orig_binary
        (disc_info,_) <- mkFinalCFGWithSyms ainfo mem orig_binary (args^.discOpts)
        fns <- getFns sysp (elfSymAddrMap secMap orig_binary) (args^.notransAddrs) disc_info
        writeFile output_path $ show (vcat (pretty <$> fns))
      ".ll" -> do
        hPutStrLn stderr "Generating LLVM"
        Just Refl <- pure $ elfIs64Bit $ elfClass orig_binary
        (ainfo, sysp, syscallPostfix) <- getX86ElfArchInfo orig_binary
        (disc_info,_) <- mkFinalCFGWithSyms ainfo mem orig_binary (args^.discOpts)
        fns <- getFns sysp (elfSymAddrMap secMap orig_binary) (args^.notransAddrs) disc_info
        let obj_llvm = llvmAssembly llvmVer $ LLVM.moduleForFunctions syscallPostfix addrSymMap fns
        writeFileBuilder output_path obj_llvm
      ".o" -> do
        Just Refl <- pure $ elfIs64Bit $ elfClass orig_binary
        (ainfo, sysp, syscallPostfix) <- getX86ElfArchInfo orig_binary
        (disc_info,_) <- mkFinalCFGWithSyms ainfo mem orig_binary (args^.discOpts)
        fns <- getFns sysp (elfSymAddrMap secMap orig_binary)  (args^.notransAddrs) disc_info
        let obj_llvm = llvmAssembly llvmVer $ LLVM.moduleForFunctions syscallPostfix addrSymMap fns
        arch <- targetArch (elfOSABI orig_binary)
        llvm <- link_with_libreopt obj_dir args arch obj_llvm
        compile_llvm_to_obj args arch llvm output_path
      ".s" -> do
        hPutStrLn stderr $
          "Generating '.s' (LLVM ASCII assembly) is not supported!\n" ++
          "Use '.ll' extension to get assembled LLVM bitcode, and then " ++
          "compile to generate a .s file."
        exitFailure
      _ -> do
        Just Refl <- pure $ elfIs64Bit $ elfClass orig_binary
        (ainfo, sysp, syscallPostfix) <- getX86ElfArchInfo orig_binary
        (disc_info,_) <- mkFinalCFGWithSyms ainfo mem orig_binary (args^.discOpts)
        let notrans = args^.notransAddrs
        let symAddrMap = elfSymAddrMap secMap orig_binary
        fns <- getFns sysp symAddrMap notrans disc_info

        let obj_llvm = llvmAssembly llvmVer $ LLVM.moduleForFunctions  syscallPostfix addrSymMap fns
        arch <- targetArch (elfOSABI orig_binary)
        llvm <- link_with_libreopt obj_dir args arch obj_llvm
        let obj_path = obj_dir </> "obj.o"
        compile_llvm_to_obj args arch llvm obj_path

        new_obj <- parseElf64 "new object" =<< BS.readFile obj_path
        putStrLn $ "obj_path: " ++ obj_path

        hPutStrLn stderr "Start merge and write"
        -- Convert binary to LLVM
        let tgts = discoveryControlFlowTargets disc_info
            (bad_addrs, redirs) = partitionEithers $ mkRedir <$> fns
              where m = elfSegmentMap (elfLayout orig_binary)
                    mkRedir f = addrRedirection tgts addrSymMap m f
        unless (null bad_addrs) $ do
          error $ "Found functions outside program headers:\n  "
            ++ unwords (show <$> bad_addrs)
        -- Merge and write out
        let extra_addrs :: SymbolNameToAddrMap Word64
            extra_addrs = Map.fromList
              [ (fromString "reopt_gen_" `BS.append` nm, w)
              | Right binary_nm <- resolveSymName <$> Set.toList notrans
              , Just addr <- [Map.lookup (fromString binary_nm) symAddrMap]
              , let w :: Word64
                    w = case msegAddr addr of
                          Just b -> fromIntegral b
                          Nothing -> error $ "Merging does not yet support virtual addresses."
                -- Get symbol name used in object.
              , Just nm <- [Map.lookup addr addrSymMap]
              ]
        mergeAndWrite (args^.outputPath) orig_binary new_obj extra_addrs redirs

main :: IO ()
main = main' `catch` h
  where h e
          | isUserError e =
            hPutStrLn stderr (ioeGetErrorString e)
          | otherwise = do
            hPutStrLn stderr (show e)
            hPutStrLn stderr (show (ioeGetErrorType e))

main' :: IO ()
main' = do
  args <- getCommandLineArgs
  setDebugKeys (args ^. debugKeys)
  case args^.reoptAction of
    DumpDisassembly -> do
      dumpDisassembly (args^.programPath)
    ShowCFG -> showCFG args
    ShowFn addr -> showFn args addr

    ShowCFGAI -> do
      error $ "ShowCFGAI not supported"
--      e <- readElf64 (args^.programPath)
--      showCFGAndAI (args^.loadStyle) e
    ShowLLVM _path -> do
--      showLLVM args path
      error $ "ShowLLVM not supported"
    ShowFunctions -> do
      showFunctions args
    ShowGaps -> do
--      e <- readElf64 (args^.programPath)
--      showGaps (args^.loadStyle) e
      error $ "ShowGaps not supported"
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
    Relink -> do
      performRedir args
    Reopt -> do
      performReopt args
