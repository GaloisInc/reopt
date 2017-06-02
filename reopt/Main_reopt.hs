{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
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
import qualified Data.Vector as V
import           Data.Version
import           Data.Word
import qualified Data.Yaml as Yaml
import           Numeric (readHex, showHex)
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

--import           Data.Macaw.AbsDomain.AbsState
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Discovery
import           Data.Macaw.Discovery.Info
                 ( DiscoveryInfo
                 , DiscoveryFunInfo
                 , discoveredFunName
                 , ppDiscoveryInfoBlocks
                 , functionEntries
                 , funInfo
                 , memory
                 , symbolNames
                 , parsedBlocks
                 )
import           Data.Macaw.Memory
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.X86Reg (X86Reg)

import           Reopt
import           Reopt.CFG.FnRep (Function(..), FunctionType)
import           Reopt.CFG.FunctionArgs (DemandSet, functionDemands, inferFunctionTypeFromDemands)
import           Reopt.CFG.FunctionCheck
import qualified Reopt.CFG.LLVM as LLVM
import           Reopt.CFG.Recovery (recoverFunction)
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

{-
-- | This returns how much space there is before start of next function,
-- or the end of the memory segment if code address is undefined.
--
-- Note: Calls error if code addr is not in a valid memory location.
functionSize :: DiscoveryInfo X86_64 ids -> SegmentedAddr 64 -> MemWord 64
functionSize s a = do
  let seg = addrSegment a
  assert (segmentFlags seg `Perm.hasPerm` Perm.execute) $ do
    case Set.lookupGT a (s^.functionEntries) of
      Just next | segmentIndex (addrSegment next) == segmentIndex seg ->
           next^.addrOffset - a^.addrOffset
      _ -> segmentSize seg - a^.addrOffset
-}

------------------------------------------------------------------------
-- LoadStyle

-- | How to load Elf file.
data LoadStyle
   = LoadBySection
     -- ^ Load loadable sections in Elf file.
   | LoadBySegment
     -- ^ Load segments in Elf file.

-- | Create a memory from a load style.
mkElfMem :: (Monad m, Integral (ElfWordType w), Bits (ElfWordType w), MemWidth w)
         => LoadStyle
         -> AddrWidthRepr w
         -> Elf (ElfWordType w)
         -> m (SectionIndexMap (ElfWordType w) w, Memory w)
mkElfMem sty w e = do
  case sty of
    LoadBySection -> either fail return $ memoryForElfSections w e
    LoadBySegment -> either fail return $ memoryForElfSegments w e

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
-- Args

-- | Action to perform when running
data Action
   = DumpDisassembly -- ^ Print out disassembler output only.
   | ShowCFG         -- ^ Print out control-flow microcode.
   | ShowCFGAI       -- ^ Print out control-flow microcode + abs domain
   | ShowLLVM String -- ^ Write out the LLVM into the argument path
   | ShowFunctions   -- ^ Print out generated functions
   | ShowGaps        -- ^ Print out gaps in discovered blocks
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version
   | Relink          -- ^ Link an existing binary and new code together.
   | Reopt           -- ^ Perform a full reoptimization


-- | Command line arguments.
data Args = Args { _reoptAction  :: !Action
                 , _programPath  :: !FilePath
                 , _loadStyle    :: !LoadStyle
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
                 }

-- | Action to perform when running
reoptAction :: Simple Lens Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Path for main executable
programPath :: Simple Lens Args FilePath
programPath = lens _programPath (\s v -> s { _programPath = v })

-- | Whether to load file by segment or sections
loadStyle :: Simple Lens Args LoadStyle
loadStyle = lens _loadStyle (\s v -> s { _loadStyle = v })

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

-- | Version to use when printing LLVM.
llvmVersion :: Simple Lens Args LLVMVersion
llvmVersion = lens _llvmVersion (\s v -> s { _llvmVersion = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Reopt
                   , _programPath = ""
                   , _loadStyle = LoadBySegment
                   , _debugKeys = []
                   , _newobjPath = ""
                   , _redirPath  = ""
                   , _outputPath = "a.out"
                   , _gasPath = "gas"
                   , _llvmVersion = LLVM35
                   , _llcPath = "llc"
                   , _optPath = "opt"
                   , _optLevel  = 2
                   , _llvmLinkPath = "llvm-link"
                   , _libreoptPath = Nothing
                   , _notransAddrs = Set.empty
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
        help = "Print out recovered control flow graph of executable."

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
        help = "Print out generated functions."

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
  where upd  = loadStyle .~ LoadBySegment
        help = "Load the Elf file using segment information (default)."

sectionFlag :: Flag Args
sectionFlag = flagNone [ "load-sections" ] upd help
  where upd  = loadStyle .~ LoadBySection
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

arguments :: Mode Args
arguments = mode "reopt" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ disassembleFlag
                , cfgFlag
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

{-
mkCFG :: Integral (ArchAddr arch)
      => Map (ArchSegmentedAddr arch) (BlockRegion arch ids)
      -> CFG arch ids
mkCFG m = Map.foldlWithKey' go emptyCFG m
  where go g addr br = insertBlocksForCode addr (brSize br) l g
          where l = Map.elems (brBlocks br)
-}

-- | The takes the elf symbol table map and attempts to identify segmented addresses for each one.
--
-- It returns a two maps, the first contains entries that could not be resolved; the second
-- contains those that could.
resolvedSegmentedElfFuncSymbols :: forall w v
                                .  (Integral v, MemWidth w)
                                => Memory w
                                -> [ElfSymbolTableEntry v]
                                -> (Map (MemWord w)  [BS.ByteString], Map (SegmentedAddr w) [BS.ByteString])
resolvedSegmentedElfFuncSymbols mem entries = (Map.fromList u, Map.fromList r)
  where -- Filter out just function entries
        isCodeFuncSymbol ste = steType ste == STT_FUNC
                            && isCodeAddr mem (fromIntegral (steValue ste))
        func_entries = filter isCodeFuncSymbol entries
        -- Build absolute address map
        absAddrMap :: Map (MemWord w) [BS.ByteString]
        absAddrMap = Map.fromListWith (++) $ [ (fromIntegral (steValue ste), [steName ste]) | ste <- func_entries ]
        -- Resolve addresses
        resolve (v,nms) =
          case absoluteAddrSegment mem v of
            Nothing -> Left  (v,  nms)
            Just sv -> Right (sv, nms)
        (u,r) = partitionEithers $ resolve <$> Map.toList absAddrMap

ppElfUnresolvedSymbols :: forall w
                       .  MemWidth w
                       => Map (MemWord w) [BS.ByteString]
                       -> Doc
ppElfUnresolvedSymbols m =
    text "Could not resolve addresses of ELF symbols" <$$>
    indent 2 (vcat $ pp <$> Map.toList m)
  where pp :: (MemWord w, [BS.ByteString]) -> Doc
        pp (w, nms) = text (showHex w ":") <+> hsep (text . BSC.unpack <$> nms)

-- | Create a final CFG
mkFinalCFGWithSyms :: Integral v
                   => ArchitectureInfo arch
                   -> Memory (ArchAddrWidth arch) -- ^ Layout in memory of file
                   -> Elf v -- ^ Elf file to create CFG for.
                   -> IO (DiscoveryInfo arch, Map (SegmentedAddr (ArchAddrWidth arch)) BS.ByteString)
mkFinalCFGWithSyms archInfo mem e = withArchConstraints archInfo $ do
  entries <-
    case elfSymtab e of
      [] -> pure $ []
      [tbl] -> pure $ V.toList (elfSymbolTableEntries tbl)
      _ -> fail "Elf contains multiple symbol tables."

  let (unresolved, resolved) = resolvedSegmentedElfFuncSymbols mem entries
  -- Check for unresolved symbols
  when (not (Map.null unresolved)) $ do
    fail $ show $ ppElfUnresolvedSymbols unresolved
  let sym_map = fmap head resolved
  entry <- case absoluteAddrSegment mem (fromIntegral (elfEntry e)) of
             Nothing -> fail "Could not resolve entry"
             Just v  -> pure v
  let sym_addrs = entry : Map.keys resolved

  pure ( cfgFromAddrs archInfo mem sym_map sym_addrs (memAsAddrPairs mem LittleEndian)
       , sym_map
       )

data SomeArchitectureInfo v =
  forall arch
  . ( FnHasRefs (ArchFn arch)
    , StmtHasRefs (ArchStmt arch)
    , v ~ ElfWordType (ArchAddrWidth arch)
    , Bits v
    , Integral v
    )
   => SomeArch (ArchitectureInfo arch)

getElfArchInfo :: Elf v -> IO (SomeArchitectureInfo v)
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
    (_,_,abi)              -> fail $ "Unknown OSABI: " ++ show abi

getX86ElfArchInfo :: Elf Word64 -> IO (ArchitectureInfo X86_64, SyscallPersonality X86_64, String)
getX86ElfArchInfo e =
  case elfOSABI e of
    ELFOSABI_LINUX   -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_SYSV    -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_FREEBSD -> pure (x86_64_freeBSD_info, freeBSD_syscallPersonality, "FreeBSD")
    abi              -> fail $ "Unknown OSABI: " ++ show abi

showNonfatalErrors :: [ElfParseError w] -> IO ()
showNonfatalErrors l = do
  when (not (null l)) $ do
    hPutStrLn stderr $ "Recoverable errors occurred in reading elf file:"
    forM_ l $ \emsg -> do
      hPutStrLn stderr (show emsg)

showCFG :: LoadStyle -> String -> IO ()
showCFG loadSty path = do
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

  Some e <-
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
  -- Get architecture information for elf
  SomeArch archInfo <- getElfArchInfo e
  withArchConstraints archInfo $ do
  (_,mem) <- mkElfMem loadSty (archAddrWidth archInfo) e
  (disc_info, _) <- mkFinalCFGWithSyms archInfo mem e
  print $ ppDiscoveryInfoBlocks disc_info

-- | Try to recover function information from the information recovered during
-- code discovery.
getFns :: SyscallPersonality X86_64
       -> Map BS.ByteString (SegmentedAddr 64)
          -- ^ Maps symbol names to addresses
       -> Set String
          -- ^ Name of symbols/addresses to exclude
       -> DiscoveryInfo X86_64
          -- ^ Information about original binary recovered from static analysis.
       -> IO [Function]
getFns sysp symMap excludedNames info = do

  -- Compute which functions to compute by looking at the binary
  let nms = Set.toList excludedNames
  let (bad, excludedAddrs) = partitionEithers $
        resolveSymAddr symMap . resolveSymName <$> nms

  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad

  let excludeSet = Set.fromList excludedAddrs
  let include :: SegmentedAddr 64 -> Bool
      include addr = do
        case segmentBase (addrSegment addr) of
          Just base -> Set.notMember word excludeSet
            where word = fromIntegral (base + addr^.addrOffset)
          _ -> True
  let doCheck :: SegmentedAddr 64 -> Bool
      doCheck a =
          case Map.lookup a (info^.funInfo) of
            Nothing -> error "doCheck failed"
            Just (Some finfo) -> checkFunction finfo a

  let entries = filter include $ Set.toList $ info^.functionEntries

  let mem = memory info
  let fDems :: Map (SegmentedAddr 64) (DemandSet X86Reg)
      fDems = functionDemands sysp info (filter doCheck (Set.toList $ info^.functionEntries))
  let fArgs :: Map (SegmentedAddr 64) FunctionType
      fArgs = inferFunctionTypeFromDemands fDems
  seq fArgs $ do
  fmap catMaybes $ forM entries $ \entry -> do
    case Map.lookup entry (info^.funInfo) of
      Nothing -> do
        hPutStrLn stderr $ "Could not find function info for " ++ show entry
        pure Nothing
      Just (Some finfo)
        | checkFunction finfo entry -> do
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
  (archInfo, sysp,_) <- getX86ElfArchInfo e
  (secMap, mem) <- mkElfMem (args^.loadStyle) Addr64 e
  (s,_) <- mkFinalCFGWithSyms archInfo mem e
  fns <- getFns sysp (elfSymAddrMap secMap e) (args^.notransAddrs) s
  hPutStr stderr "Got fns"
  mapM_ (print . pretty) fns

------------------------------------------------------------------------
-- Pattern match on stack pointer possibilities.

{-
ppStmtAndAbs :: MapF (AssignId ids) (AbsValue 64) -> Stmt X86_64 ids -> Doc
ppStmtAndAbs m stmt =
  case stmt of
    AssignStmt a ->
      case ppAbsValue =<< MapF.lookup (assignId a) m of
        Just d -> vcat [ text "#" <+> ppAssignId (assignId a) <+> text ":=" <+> d
                       , pretty a
                       ]
        Nothing -> pretty a
    _ -> pretty stmt


ppBlockAndAbs :: MapF (AssignId ids) (AbsValue 64) -> Block X86_64 ids -> Doc
ppBlockAndAbs m b =
  pretty (blockLabel b) <> text ":" <$$>
  indent 2 (vcat (ppStmtAndAbs m <$> blockStmts b) <$$>
            pretty (blockTerm b))

showCFGAndAI :: LoadStyle -> Elf Word64 -> IO ()
showCFGAndAI loadSty e = do
  -- Create memory for elf
  (archInfo,_, _) <- getX86ElfArchInfo e
  (_,mem) <- mkElfMem loadSty Addr64 e
  (Some disc_info,_) <- mkFinalCFGWithSyms archInfo mem e
  forM_ (Map.elems (disc_info^.funInfo)) $ \finfo -> do
    let abst = foundAbstractState <$> finfo^.foundAddrs
    let g  = eliminateDeadRegisters $ mkCFG (disc_info^.blocks)
    let amap = assignmentAbsValues archInfo mem g abst
        ppOne b =
          vcat [case (blockLabel b, Map.lookup (labelAddr (blockLabel b)) abst) of
                  (GeneratedBlock _ 0, Just ab) -> pretty ab
                  (GeneratedBlock _ 0, Nothing) -> text "Stored in memory"
                  (_,_) -> text ""

               , ppBlockAndAbs amap b
               ]
    print $ vcat (map ppOne $ Map.elems (g^.cfgBlocks))
--    forM_ (Map.elems (g^.cfgBlocks)) $ \b -> do
--      case blockLabel b of
--        GeneratedBlock _ 0 -> do
--          checkReturnsIdentified g b
--        _ -> return ()
--    -- Check that the CFG correctly identifies call statements.
--    checkCallsIdentified mem g
-}

-- | Extract list containing symbol names and addresses.
elfAddrSymEntries :: SectionIndexMap Word64 64
                     -- ^ Map from elf section indices to base address for section
                     -- and section in Elf file
                  -> Elf Word64
                  -> [(BS.ByteString, SegmentedAddr 64)]
elfAddrSymEntries m binary =
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
  , let val = base & addrOffset +~ fromIntegral (steValue entry - elfSectionAddr sec)
  ]

-- | Create map from symbol names to address.
elfSymAddrMap  :: SectionIndexMap Word64 64
                  -- ^ Map from elf section indices to base address for section
                  -- and section in Elf file
               -> Elf Word64
               -> Map BS.ByteString (SegmentedAddr 64)
elfSymAddrMap m binary = Map.fromList $ elfAddrSymEntries m binary

-- | Create map from addresses to symbol name.
--
-- Used for naming functions.
elfAddrSymMap :: SectionIndexMap Word64 64
              -> Elf Word64
              -> LLVM.AddrSymMap 64
elfAddrSymMap m binary = Map.fromList $ swap <$> elfAddrSymEntries m binary

{-
showLLVM :: Args -> String -> IO ()
showLLVM args dir = do
  e <- readElf64 (args^.programPath)

  let loadSty = args^.loadStyle

  -- Create memory for elf
  (archInfo, sysp, syscallPostfix) <- getX86ElfArchInfo e
  (secMap, mem) <- mkElfMem loadSty Addr64 e
  (Some cfg, symMap) <- mkFinalCFGWithSyms archInfo mem e

  let mkName f = dir </> (name ++ "_" ++ addr_str ++ ".ll")
        where
          name = case Map.lookup addr symMap of
                   Nothing -> "unknown"
                   Just s  -> BSC.unpack s
          addr = fnAddr f
          addr_str = case segmentBase (addrSegment addr) of
                       Just b -> show (b + addr^.addrOffset)
                       Nothing -> show (segmentIndex (addrSegment addr))
                         ++ "_" ++ show (addr^.addrOffset)
  let addrSymMap = elfAddrSymMap secMap e
  let writeF :: Function -> IO ()
      writeF f  = do
        let (_,m) = L.runLLVM $ do
              LLVM.declareIntrinsics
              let refs = Map.delete (fnAddr f) (LLVM.getReferencedFunctions f)
              itraverse_ (LLVM.declareFunction addrSymMap) refs
              LLVM.defineFunction syscallPostfix addrSymMap f
        writeFile (mkName f) $ ppLLVM (args^.llvmVersion) m

  createDirectoryIfMissing True dir
  fns <- getFns sysp (elfSymAddrMap secMap e) (args^.notransAddrs) cfg
  mapM_ writeF fns

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a RegState to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
stateEndsWithRet :: RegState X86Reg (Value X86_64 ids) -> Bool
stateEndsWithRet s = do
  let next_ip = s^.boundValue ip_reg
      next_sp = s^.boundValue sp_reg
  case () of
    _ | AssignedValue (Assignment _ (ReadMem a _)) <- next_ip
      , (ip_base, ip_off) <- asBaseOffset a
      , (sp_base, sp_off) <- asBaseOffset next_sp ->
        ip_base == sp_base && ip_off + 8 == sp_off
    _ -> False

-- | @isWriteTo stmt add tpr@ returns true if @stmt@ writes to @addr@
-- with a write having the given type.
isWriteTo :: Stmt X86_64 ids -> BVValue X86_64 ids 64 -> TypeRepr tp -> Maybe (Value X86_64 ids tp)
isWriteTo (WriteMem a val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (typeRepr val) tp =
    Just val
isWriteTo _ _ _ = Nothing

-- | @isCodeAddrWriteTo mem stmt addr@ returns true if @stmt@ writes to @addr@ and
-- @addr@ is a code pointer.
isCodeAddrWriteTo :: Memory 64 -> Stmt X86_64 ids -> BVValue X86_64 ids 64 -> Maybe Word64
isCodeAddrWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodeAddr mem (fromInteger val)
  = Just (fromInteger val)
isCodeAddrWriteTo _ _ _ = Nothing

-- | Returns true if it looks like block ends with a call.
blockContainsCall :: Memory 64
                  -> Block X86_64 ids
                  -> RegState X86Reg (Value X86_64 ids)
                  -> Bool
blockContainsCall mem b s =
  let next_sp = s^.boundValue sp_reg
      go [] = False
      go (stmt:_) | Just _ <- isCodeAddrWriteTo mem stmt next_sp = True
      go (ExecArchStmt WriteLoc{}:_) = False
      go (_:r) = go r
   in go (reverse (blockStmts b))

-- | Return next states for block.
blockNextStates :: CFG X86_64 ids
                -> Block X86_64 ids
                -> [RegState X86Reg (Value X86_64 ids)]
blockNextStates g b =
  case blockTerm b of
    FetchAndExecute s -> [s]
    Branch _ x_lbl y_lbl -> blockNextStates g x ++ blockNextStates g y
      where Just x = findBlock g x_lbl
            Just y = findBlock g y_lbl
    Syscall{} -> []
    TranslateError{} -> []

checkReturnsIdentified :: CFG X86_64 ids -> Block X86_64 ids -> IO ()
checkReturnsIdentified g b =
  case blockNextStates g b of
    [s] -> do
      let lbl = blockLabel b
      case (stateEndsWithRet s, hasRetComment b) of
        (True, True) -> return ()
        (True, False) -> do
          hPutStrLn stderr $ "UNEXPECTED return Block " ++ show (labelAddr lbl)
        (False, True) -> do
          hPutStrLn stderr $ "MISSING return Block " ++ show (labelAddr lbl)
        _ -> return ()
    _ -> return ()

-- | Returns true if block has a call comment.
hasCallComment :: Block X86_64 ids -> Bool
hasCallComment b = any isCallComment (blockStmts b)
  where isCallComment (Comment s) = "call" `Text.isInfixOf` s
        isCallComment _ = False

-- | Returns true if block has a ret comment.
hasRetComment :: Block X86_64 ids -> Bool
hasRetComment b = any isRetComment (blockStmts b)
  where isRetComment (Comment s) = "ret" `Text.isSuffixOf` s
        isRetComment _ = False

-- | This prints a report summarizing where calls are found that do not have
-- call instructions
checkCallsIdentified :: Memory 64 -> CFG X86_64 ids -> IO ()
checkCallsIdentified mem g = do
  let g_blocks = Map.elems (g^.cfgBlocks)
  -- Check to see if block contains a call
  let checkBlockForCall b =
        case blockNextStates g b of
          [s] -> blockContainsCall mem b s
          _ -> False
  let blocksWithCallDetected = Set.fromList $ fmap blockLabel $ filter checkBlockForCall g_blocks

  let blocksWithCallComment  = Set.fromList $ fmap blockLabel $ filter hasCallComment g_blocks

  let blocksWithSpuriousCall = blocksWithCallDetected `Set.difference` blocksWithCallComment
  let blocksWithMissedCall   = blocksWithCallComment  `Set.difference` blocksWithCallDetected

  when (not (Set.null blocksWithSpuriousCall)) $ do
    hPutStrLn stderr $
         "WARNING: Some blocks were interpreted as having a call, but no call instruction\n"
      ++ "was found.  The address for the start of each block is listed below:"
    mapM_ (\a -> hPutStrLn stderr $ "  " ++ show a) (Set.toList blocksWithSpuriousCall)

  when (not (Set.null blocksWithMissedCall)) $ do
    hPutStrLn stderr $
         "WARNING: Some blocks contained a call comment, but were not interpreted as\n"
      ++ "having a call.  The address for the start of each block is listed below:"
    mapM_ (\a -> hPutStrLn stderr $ "  " ++ show a) (Set.toList blocksWithMissedCall)
-}

-- | Merge a binary and new object
mergeAndWrite :: FilePath
              -> Elf Word64 -- ^ Original binary
              -> Elf Word64 -- ^ New object
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
type ElfSegmentMap w = Map w (Phdr w)

-- | Create an elf segment map from a layout.
elfSegmentMap :: forall w . Integral w => ElfLayout w -> ElfSegmentMap w
elfSegmentMap l = foldl' insertElfSegment Map.empty (allPhdrs l)
  where insertElfSegment ::  ElfSegmentMap w -> Phdr w -> ElfSegmentMap w
        insertElfSegment m p
          | elfSegmentType seg == PT_LOAD = Map.insert a p m
          | otherwise = m
          where seg = phdrSegment p
                a = elfSegmentVirtAddr (phdrSegment p)

-- | Lookup an address in the segment map, returning the index of the phdr
-- and the offset.
lookupElfOffset :: ElfSegmentMap Word64 -> Word64 -> Maybe (Word16, Word64)
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
newtype ControlFlowTargetSet w = CFTS { cfTargets :: Map (SegmentedAddr w) [SegmentedAddr w]
                                      }

-- | Return how many bytes of space there are to write after address without
-- ovewriting another control flow target.
lookupControlFlowTargetSpace :: MemWidth w
                             => SegmentedAddr w
                             -> ControlFlowTargetSet w
                             -> MemWord w
lookupControlFlowTargetSpace addr0 = go 0 addr0 addr0
  where seg = addrSegment addr0
        go inc base addr s =
          case Map.lookupGT addr (cfTargets s) of
            Just (next,fns)
              | addrSegment addr == addrSegment next ->
                let d = next^.addrOffset - addr^.addrOffset
                 in if null (filter (/= base) fns) then
                      go (inc+d) base next s
                     else
                      inc+d
            _ ->
              if segmentSize seg >= addr^.addrOffset then
                segmentSize seg - addr^.addrOffset
               else
                0

addControlFlowTarget :: ControlFlowTargetSet w
                     -> SegmentedAddr w
                     -> SegmentedAddr w -- ^ Function entry point
                     -> ControlFlowTargetSet w
addControlFlowTarget m a f = m { cfTargets = Map.insertWith (++) a [f] (cfTargets m) }

addFunctionEntryPoint :: ControlFlowTargetSet w
                      -> SegmentedAddr w
                      -> ControlFlowTargetSet w
addFunctionEntryPoint s a = addControlFlowTarget s a a


addFunDiscoveryControlFlowTargets :: ControlFlowTargetSet (ArchAddrWidth arch)
                                  -> (ArchSegmentedAddr arch, Some (DiscoveryFunInfo arch))
                                  -> ControlFlowTargetSet (ArchAddrWidth arch)
addFunDiscoveryControlFlowTargets m0 (base, Some f) =
  foldl' (\m b -> addControlFlowTarget m b base) m0 (Map.keys (f^.parsedBlocks))

discoveryControlFlowTargets :: DiscoveryInfo arch -> ControlFlowTargetSet (ArchAddrWidth arch)
discoveryControlFlowTargets info =
  let m0 = CFTS { cfTargets = Map.empty }
      m = foldl' addFunDiscoveryControlFlowTargets m0 (Map.toList (info^.funInfo))
   in foldl' addFunctionEntryPoint m (Map.keys (symbolNames info))

--------------------------------------------------------------------------------
-- Redirections

-- | This creates a code redirection or returns the address as failing.
addrRedirection :: ControlFlowTargetSet 64
                -> LLVM.AddrSymMap 64
                -> ElfSegmentMap Word64
                -> Function
                -> Either (SegmentedAddr 64) (CodeRedirection Word64)
addrRedirection tgts addrSymMap m f = do
  let a = fnAddr f
  let w = case segmentBase (addrSegment a) of
            Just b -> fromIntegral (b + a^.addrOffset)
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
       fail "Could not find path to libreopt.bc needed to link object."

  let obj_llvm_path = obj_dir </> "obj.ll"
  writeFileBuilder obj_llvm_path obj_llvm

  mllvm <- runExceptT $
    Ext.run_llvm_link (args^.llvmLinkPath) [ obj_llvm_path, libreopt_path ]
  either (fail . show) return mllvm


resolveSymName :: String -> Either Word64 String
resolveSymName ('0':'x': nm) | [(w,"")] <- readHex nm = Left w
resolveSymName nm = Right nm

-- | Attempt to find the address of a string identifying a symbol name, and
-- return either the string if it cannot be resolved or the address.
resolveSymAddr :: Map BS.ByteString (SegmentedAddr 64)
                  -- ^ Map from symbol names in binary to their address.
               -> Either Word64 String
                  -- ^ The name of a symbol as a string.
               -> Either String Word64
resolveSymAddr _ (Left w) = Right w
resolveSymAddr symMap (Right nm) =
  case Map.lookup (fromString nm) symMap of
    Just addr ->
      case segmentBase (addrSegment addr) of
        Just b -> Right (fromIntegral (b + addr^.addrOffset))
        Nothing -> error "Relocation does not yet support relocatable executables."
    Nothing -> Left nm

writeFileBuilder :: FilePath -> Builder.Builder -> IO ()
writeFileBuilder nm b = bracket (openBinaryFile nm WriteMode) hClose (\h -> Builder.hPutBuilder h b)

performReopt :: Args -> IO ()
performReopt args =
  withSystemTempDirectory "reopt." $ \obj_dir -> do
    -- Get original binary
    orig_binary <- readElf64 (args^.programPath)
    -- Construct CFG from binary
    (archInfo, sysp, syscallPostfix) <- getX86ElfArchInfo orig_binary
    (secMap, mem) <- mkElfMem (args^.loadStyle) Addr64 orig_binary
    (disc_info,_) <- mkFinalCFGWithSyms archInfo mem orig_binary
    let addrSymMap = elfAddrSymMap secMap orig_binary
    let output_path = args^.outputPath
    let llvmVer = args^.llvmVersion

    case takeExtension output_path of
      ".bc" -> do
        hPutStrLn stderr $
          "Generating '.bc' (LLVM ASCII assembly) is not supported!\n" ++
          "Use '.ll' extension to get assembled LLVM bitcode, and then " ++
          "use 'llvm-as out.ll' to generate an 'out.bc' file."
        exitFailure
      ".blocks" -> do
        writeFile output_path $ show $ ppDiscoveryInfoBlocks disc_info
        let tgts = discoveryControlFlowTargets disc_info
        forM_ (Map.toList (disc_info^.funInfo)) $ \(a, Some f) -> do
          putStrLn $ BSC.unpack (discoveredFunName f) ++ ": "
            ++ show (lookupControlFlowTargetSpace a tgts)

      ".fns" -> do
        fns <- getFns sysp (elfSymAddrMap secMap orig_binary) (args^.notransAddrs) disc_info
        writeFile output_path $ show (vcat (pretty <$> fns))
      ".ll" -> do
        hPutStrLn stderr "Generating LLVM"
        fns <- getFns sysp (elfSymAddrMap secMap orig_binary) (args^.notransAddrs) disc_info
        let obj_llvm = llvmAssembly llvmVer $ LLVM.moduleForFunctions syscallPostfix addrSymMap fns
        writeFileBuilder output_path obj_llvm
      ".o" -> do
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
        let notrans = args^.notransAddrs
        let symAddrMap = elfSymAddrMap secMap orig_binary
        fns <- getFns sysp symAddrMap notrans disc_info

        let obj_llvm = llvmAssembly llvmVer $ LLVM.moduleForFunctions  syscallPostfix addrSymMap fns
        arch <- targetArch (elfOSABI orig_binary)
        llvm <- link_with_libreopt obj_dir args arch obj_llvm
        let obj_path = obj_dir </> "obj.o"
        compile_llvm_to_obj args arch llvm obj_path

        new_obj <- parseElf64 "new object" =<< BS.readFile obj_path

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
                    w = case segmentBase (addrSegment addr) of
                          Just b -> fromIntegral (b + addr^.addrOffset)
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
    ShowCFG -> do
      showCFG (args^.loadStyle) (args^.programPath)

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
