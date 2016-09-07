{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Werror #-}
module Main (main) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Either
import           Data.ElfEdit
import           Data.Foldable
import           Data.List ((\\), nub, stripPrefix, intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Tuple (swap)
import           Data.Type.Equality as Equality
import qualified Data.Vector as V
import           Data.Version
import           Data.Word
import qualified Data.Yaml as Yaml
import           Flexdis86 (InstructionInstanceF(..))
import           Numeric (readHex, showHex)
import           System.Console.CmdArgs.Explicit
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.IO.Temp
import           System.Posix.Files
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import           Paths_reopt (getLibDir, version)

import           Data.Macaw.CFG
import           Data.Macaw.Types

import           Reopt
import           Data.Macaw.AbsDomain.AbsState
import           Data.Macaw.Discovery
import           Data.Macaw.Discovery.Info
                 ( DiscoveryInfo
                 , absState
                 , blocks
                 , functionEntries
                 )
import           Reopt.CFG.FnRep (Function(..))
import           Reopt.CFG.FunctionArgs (functionArgs)
import           Reopt.CFG.Implementation
import qualified Reopt.CFG.LLVM as LLVM
import           Reopt.CFG.Recovery (recoverFunction)
import qualified Reopt.ExternalTools as Ext
import           Reopt.Machine.SysDeps
import           Reopt.Machine.X86State
import           Data.Macaw.Memory.ElfLoader
import           Data.Macaw.Memory
import           Reopt.Relinker
import           Reopt.Semantics.DeadRegisterElimination
import           Data.Macaw.DebugLogging

import           Debug.Trace

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
-- LoadStyle

-- | How to load Elf file.
data LoadStyle
   = LoadBySection
     -- ^ Load loadable sections in Elf file.
   | LoadBySegment
     -- ^ Load segments in Elf file.

-- | Create a memory from a load style.
mkElfMem :: (Bits w, Integral w,  Monad m)
         => LoadStyle
         -> Elf w
         -> m (Memory w)
mkElfMem sty e = do
  case elfInterpreter e of
    Nothing ->
      fail "Could not parse elf interpreter."
    Just Nothing ->
      return ()
    Just (Just{}) ->
      fail "reopt does not yet support generating CFGs from dynamically linked executables."
  case sty of
    LoadBySection -> either fail return $ memoryForElfSections e
    LoadBySegment -> either fail return $ memoryForElfSegments e

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

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Reopt
                   , _programPath = ""
                   , _loadStyle = LoadBySection
                   , _debugKeys = []
                   , _newobjPath = ""
                   , _redirPath  = ""
                   , _outputPath = "a.out"
                   , _gasPath = "gas"
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
        help = "Load the Elf file using segment information."

sectionFlag :: Flag Args
sectionFlag = flagNone [ "load-sections" ] upd help
  where upd  = loadStyle .~ LoadBySection
        help = "Load the Elf file using section information (default)."


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

-- | Flag to set llc optimization level.
optLevelFlag :: Flag Args
optLevelFlag = flagReq [ "opt" ] upd "PATH" help
  where upd s old =
          case reads s of
            [(lvl, "")] | 0 <= lvl && lvl <= 3 -> Right $ old & optLevel .~ lvl
            _ -> Left "Expected optimization level to be a number between 0 and 3."
        help = "Path to llc."

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
  -- print $ Set.size $ instructionNames sections
  --print $ Set.toList $ instructionNames sections

isInterestingCode :: Memory Word64 -> (CodeAddr, Maybe CodeAddr) -> Bool
isInterestingCode mem (start, Just end) = go start end
  where
    isNop ii = (iiOp ii == "nop")
               || (iiOp ii == "xchg" &&
                   case iiArgs ii of
                    [x, y] -> x == y
                    _      -> False)

    go b e | b < e = case readInstruction mem b of
                      Left _           -> False -- FIXME: ignore illegal sequences?
                      Right (ii, next_i) -> not (isNop ii) || go next_i e
           | otherwise = False
isInterestingCode _ _ = True -- Last bit

-- | This prints out code that is stored in the elf file, but is not part of any
-- block generated by the CFG.
showGaps :: LoadStyle -> Elf Word64 -> IO ()
showGaps loadSty binary = do
    -- Create memory for elf
    mem <- mkElfMem loadSty binary
    Some disc_info <- return $ cfgFromMemAndBinary mem binary
    let cfg = mkCFG (disc_info^.blocks)
    let ends = cfgBlockEnds cfg
    let cfg_blocks = [ addr | GeneratedBlock addr 0 <- Map.keys (cfg ^. cfgBlocks) ]
    let gaps = filter (isInterestingCode mem)
             $ out_gap cfg_blocks (Set.elems ends)
    mapM_ (print . pretty . ppOne) gaps
  where
    ppOne (start, m_end) = text ("[" ++ showHex start "..") <>
                           case m_end of
                            Nothing -> text "END)"
                            Just e  -> text (showHex e ")")

    in_gap start bs@(b:_) ess = (start, Just b) : out_gap bs (dropWhile (<= b) ess)
    in_gap start [] _ = [(start, Nothing)]

    out_gap (b:bs') (e:es')
      | b < e          = out_gap bs' (e:es')
      | b == e         = out_gap bs' es'
    out_gap bs (e:es') = in_gap e bs es'
    out_gap _ _        = []

showCFG :: LoadStyle -> Elf Word64 -> IO ()
showCFG loadSty e = do
  -- Create memory for elf
  mem <- mkElfMem loadSty e
  Some disc_info <- return $ cfgFromMemAndBinary mem e
  let fg = mkCFG (disc_info^.blocks)
  let g = eliminateDeadRegisters fg
  print (pretty g)

{-
getEntries :: Map BS.ByteString CodeAddr -- ^ Maps symbol names to addresses
           -> Set String  -- ^ Name of symbols/addresses to exclude
-}

-- | Try to recover function information from the information recovered during
-- code discovery.
getFns :: Map BS.ByteString CodeAddr -- ^ Maps symbol names to addresses
       -> Set String    -- ^ Name of symbols/addresses to exclude
       -> DiscoveryInfo X86_64 ids -- ^ Information about original binary recovered from static analysis.
       -> IO [Function]
getFns symMap excludedNames s = do

  -- Compute which functions to compute by looking at the binary
  let nms = Set.toList excludedNames
  let (bad, excludedAddrs) = partitionEithers $
        resolveSymAddr symMap . resolveSymName <$> nms

  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad

  let excludeSet = Set.fromList excludedAddrs
  let include addr = Set.notMember addr excludeSet
  let entries = filter include $ Set.toList (s^.functionEntries)

  let fArgs = functionArgs s
  case traverse (recoverFunction fArgs s) entries of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right fns -> return fns

showFunctions :: Args -> IO ()
showFunctions args = do
  e <- readElf64 (args^.programPath)
  -- Create memory for elf
  mem <- mkElfMem (args^.loadStyle) e
  Some s <- return $ cfgFromMemAndBinary mem e
  fns <- getFns (elfSymAddrMap e) (args^.notransAddrs) s
  mapM_ (print . pretty) fns

------------------------------------------------------------------------
-- Pattern match on stack pointer possibilities.

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

-- | Create a final CFG
mkFinalCFGWithSyms :: Memory Word64 -- ^ Layout in memory of file
                   -> Elf Word64 -- ^ Elf file to create CFG for.
                   -> (Some (DiscoveryInfo X86_64), Map CodeAddr BS.ByteString)
mkFinalCFGWithSyms mem e =
    ( cfgFromAddrs x86ArchitectureInfo mem sym_map sysp (elfEntry e:sym_addrs)
    , sym_map
    )
        -- Get list of code locations to explore starting from entry points (i.e., eltEntry)
  where entries =
          case elfSymtab e of
            [] -> []
            [tbl] -> V.toList (elfSymbolTableEntries tbl)
            _ -> error "Elf contains multiple symbol tables."
        sym_assocs = [ (steValue ste, steName ste)
                     | ste <- entries
                     , steType ste == STT_FUNC
                     , isCodeAddr mem (steValue ste)
                     ]
        sym_addrs = map fst sym_assocs
        sym_map   = Map.fromList sym_assocs

        -- FIXME: just everything.
        Just sysp =
          case elfOSABI e of
            ELFOSABI_LINUX   -> Map.lookup "Linux" sysDeps
            ELFOSABI_SYSV    -> Map.lookup "Linux" sysDeps
            ELFOSABI_FREEBSD -> Map.lookup "FreeBSD" sysDeps
            abi                -> error $ "Unknown OSABI: " ++ show abi

cfgFromMemAndBinary :: Memory Word64 -> Elf Word64 -> Some (DiscoveryInfo X86_64)
cfgFromMemAndBinary mem e = fst (mkFinalCFGWithSyms mem e)

showCFGAndAI :: LoadStyle -> Elf Word64 -> IO ()
showCFGAndAI loadSty e = do
  -- Create memory for elf
  mem <- mkElfMem loadSty e
  Some s <- return $ cfgFromMemAndBinary mem e
  let fg = mkCFG (s^.blocks)
  let abst = s^.absState
      amap = assignmentAbsValues x86ArchitectureInfo mem fg abst
  let g  = eliminateDeadRegisters fg
      ppOne b =
         vcat [case (blockLabel b, Map.lookup (labelAddr (blockLabel b)) abst) of
                  (GeneratedBlock _ 0, Just ab) -> pretty ab
                  (GeneratedBlock _ 0, Nothing) -> text "Stored in memory"
                  (_,_) -> text ""

              , ppBlockAndAbs amap b
              ]
  print $ vcat (map ppOne $ Map.elems (g^.cfgBlocks))
  forM_ (Map.elems (g^.cfgBlocks)) $ \b -> do
    case blockLabel b of
      GeneratedBlock _ 0 -> do
        checkReturnsIdentified g b
      _ -> return ()
  forM_ (Map.elems (g^.cfgBlocks)) $ \b -> do
    case blockLabel b of
      GeneratedBlock _ 0 -> do
        checkCallsIdentified mem g b
      _ -> return ()


-- | Extract list containing symbol names and addresses.
elfAddrSymEntries :: Elf Word64 -> [(BS.ByteString, Word64)]
elfAddrSymEntries binary =
  case elfSymtab binary of
     -- Assume that no section means no relocations
    []  -> []
    [tbl] ->
      let entries = V.toList $ elfSymbolTableEntries tbl
          f s =  (\v -> (steName s, v)) <$> symAddr s
       in mapMaybe f entries
    _   -> []

-- | Create map from symbol names to address.
elfSymAddrMap  :: Elf Word64 -> Map BS.ByteString CodeAddr
elfSymAddrMap binary = Map.fromList $ elfAddrSymEntries binary

-- | Create map from addresses to symbol name.
--
-- Used for naming functions.
elfAddrSymMap :: Elf Word64 -> LLVM.AddrSymMap
elfAddrSymMap binary = Map.fromList $ swap <$> elfAddrSymEntries binary

-- | Returns binary symbol addr
symAddr :: Num w
        => ElfSymbolTableEntry w
        -> Maybe w
symAddr entry
  | steType entry `elem` [ STT_FUNC, STT_NOTYPE ]  =
    Just (steValue entry)
  | otherwise = Nothing

showLLVM :: Args -> String -> IO ()
showLLVM args dir = do
  e <- readElf64 (args^.programPath)

  let loadSty = args^.loadStyle

  -- Create memory for elf
  mem <- mkElfMem loadSty e
  (Some cfg, symMap) <- return $ mkFinalCFGWithSyms mem e

  let mkName f = dir </> (name ++ "_" ++ showHex addr ".ll")
        where
          name = case Map.lookup addr symMap of
                   Nothing -> "unknown"
                   Just s  -> BSC.unpack s
          addr = fnAddr f
  let addrSymMap = elfAddrSymMap e
  let writeF :: Function -> IO ()
      writeF f  = do
        let (_,m) = L.runLLVM $ do
              LLVM.declareIntrinsics
              let refs = Map.delete (fnAddr f) (LLVM.getReferencedFunctions f)
              itraverse_ (LLVM.declareFunction addrSymMap) refs
              LLVM.defineFunction addrSymMap f
        writeFile (mkName f) (show $ L.ppModule m)

  createDirectoryIfMissing True dir
  fns <- getFns (elfSymAddrMap e) (args^.notransAddrs) cfg
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
isCodeAddrWriteTo :: Memory Word64 -> Stmt X86_64 ids -> BVValue X86_64 ids 64 -> Maybe Word64
isCodeAddrWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodeAddr mem (fromInteger val)
  = Just (fromInteger val)
isCodeAddrWriteTo _ _ _ = Nothing

-- | Returns true if it looks like block ends with a call.
blockContainsCall :: Memory Word64
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
    Syscall _ -> []

checkReturnsIdentified :: CFG X86_64 ids -> Block X86_64 ids -> IO ()
checkReturnsIdentified g b =
  case blockNextStates g b of
    [s] -> do
      let lbl = blockLabel b
      case (stateEndsWithRet s, hasRetComment b) of
        (True, True) -> return ()
        (True, False) -> do
          hPutStrLn stderr $ "UNEXPECTED return Block " ++ showHex (labelAddr lbl) ""
        (False, True) -> do
          hPutStrLn stderr $ "MISSING return Block " ++ showHex (labelAddr lbl) ""
        _ -> return ()
    _ -> return ()

checkCallsIdentified :: Memory Word64 -> CFG X86_64 ids -> Block X86_64 ids -> IO ()
checkCallsIdentified mem g b = do
  let lbl = blockLabel b
  case blockNextStates g b of
    [s] -> do
      case (blockContainsCall mem b s, hasCallComment b) of
        (True, False) -> do
          hPutStrLn stderr $ "UNEXPECTED call Block " ++ showHex (labelAddr lbl) ""
        (False, True) -> do
          hPutStrLn stderr $ "MISSING call Block " ++ showHex (labelAddr lbl) ""
        _ -> return ()
    _ -> return ()

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


llvmAssembly :: L.Module -> BS.ByteString
llvmAssembly m = UTF8.fromString (show (L.ppModule m))

-- | Maps virtual addresses to the phdr at them.
type ElfSegmentMap w = Map w (Phdr w)

-- | Create an elf segment map from a layout.
elfSegmentMap :: forall w . (Integral w, Show w) => ElfLayout w -> ElfSegmentMap w
elfSegmentMap l = foldl' insertElfSegment Map.empty (allPhdrs l)
  where insertElfSegment ::  ElfSegmentMap w -> Phdr w -> ElfSegmentMap w
        insertElfSegment m p
          | elfSegmentType seg == PT_LOAD =
            trace ("Insert segment " ++ showHex a ".") $ Map.insert a p m
          | otherwise = m
          where seg = phdrSegment p
                a = elfSegmentVirtAddr (phdrSegment p)

-- | Lookup an address in the segment map, returning the index of the phdr
-- and the offset.
lookupElfOffset :: (Num w, Ord w) => ElfSegmentMap w -> w -> Maybe (Word16, w)
lookupElfOffset m a =
  case Map.lookupLE a m of
    Just (base, phdr) | a < base + phdrFileSize phdr ->
        Just (elfSegmentIndex seg, a - base)
      where seg = phdrSegment phdr
    _ -> Nothing

-- | This creates a code redirection or returns the address as failing.
addrRedirection :: LLVM.AddrSymMap
                -> ElfSegmentMap Word64
                -> Function
                -> Either Word64 (CodeRedirection Word64)
addrRedirection addrSymMap m f = do
  case lookupElfOffset m (fnAddr f) of
    Nothing -> Left (fnAddr f)
    Just (idx,off) -> Right redir
      where L.Symbol sym_name = LLVM.functionName addrSymMap (fnAddr f)
            redir = CodeRedirection { redirSourcePhdr   = idx
                                    , redirSourceOffset = off
                                    , redirSourceSize   = fromIntegral (fnSize f)
                                    , redirTarget       = UTF8.fromString sym_name
                                    }


targetArch :: ElfOSABI -> IO String
targetArch abi =
  case abi of
    ELFOSABI_SYSV   -> return "x86_64-unknown-linux-elf"
    ELFOSABI_NETBSD -> return "x86_64-unknown-freebsd-elf"
    _ -> fail $ "Do not support architecture " ++ show abi ++ "."

-- | Compile a bytestring containing LLVM assembly or bitcode into an object.
--
-- This takses the
compile_llvm_to_obj :: Args -> String -> BS.ByteString -> FilePath -> IO ()
compile_llvm_to_obj args arch llvm obj_path = do
  -- Run llvm on resulting binary
  putStrLn "Compiling new code"
  mres <- runExceptT $ do
    llvm_opt <- Ext.run_opt (args^.optPath) (args^.optLevel) llvm
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
                   -> BS.ByteString -- ^ Object file.
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
  BS.writeFile obj_llvm_path obj_llvm

  mllvm <- runExceptT $
    Ext.run_llvm_link (args^.llvmLinkPath) [ obj_llvm_path, libreopt_path ]
  either (fail . show) return mllvm


resolveSymName :: String -> Either CodeAddr String
resolveSymName ('0':'x': nm) | [(w,"")] <- readHex nm = Left w
resolveSymName nm = Right nm

-- | Attempt to find the address of a string identifying a symbol name, and
-- return either the string if it cannot be resolved or the address.
resolveSymAddr :: Map BS.ByteString CodeAddr
                  -- ^ Map from symbol names in binary to their address.
               -> Either CodeAddr String
                  -- ^ The name of a symbol as a string.
               -> Either String CodeAddr
resolveSymAddr _ (Left w) = Right w
resolveSymAddr symMap (Right nm) =
  case Map.lookup (fromString nm) symMap of
    Just w -> Right w
    Nothing -> Left nm

performReopt :: Args -> IO ()
performReopt args =
  withSystemTempDirectory "reopt." $ \obj_dir -> do
    -- Get original binary
    orig_binary <- readElf64 (args^.programPath)
    -- Construct CFG from binary
    mem <- mkElfMem (args^.loadStyle) orig_binary
    Some cfg <- return $ cfgFromMemAndBinary mem orig_binary
    let addrSymMap = elfAddrSymMap orig_binary
    let output_path = args^.outputPath

    case takeExtension output_path of
      ".bc" -> error $
          "Generating '.bc' (LLVM ASCII assembly) is not supported!\n" ++
          "Use '.ll' extension to get assembled LLVM bitcode, and then " ++
          "use 'llvm-as out.ll' to generate an 'out.bc' file."
      ".blocks" -> do
        let g = eliminateDeadRegisters (mkCFG $ cfg^.blocks)
        writeFile output_path $ show (pretty g)
      ".fns" -> do
        fns <- getFns (elfSymAddrMap orig_binary) (args^.notransAddrs) cfg
        writeFile output_path $ show (vcat (pretty <$> fns))
      ".ll" -> do
        fns <- getFns (elfSymAddrMap orig_binary) (args^.notransAddrs) cfg
        let obj_llvm = llvmAssembly $ LLVM.moduleForFunctions addrSymMap fns
        BS.writeFile output_path obj_llvm
      ".o" -> do
        fns <- getFns (elfSymAddrMap orig_binary)  (args^.notransAddrs) cfg
        let obj_llvm = llvmAssembly $ LLVM.moduleForFunctions addrSymMap fns
        arch <- targetArch (elfOSABI orig_binary)
        llvm <- link_with_libreopt obj_dir args arch obj_llvm
        compile_llvm_to_obj args arch llvm output_path
      ".s" -> do
        error "Reopt does not support generating asm files for relinker directly."
      _ -> do
        let notrans = args^.notransAddrs
        let symAddrMap = elfSymAddrMap orig_binary
        fns <- getFns symAddrMap notrans cfg

        let obj_llvm = llvmAssembly $ LLVM.moduleForFunctions addrSymMap fns
        arch <- targetArch (elfOSABI orig_binary)
        llvm <- link_with_libreopt obj_dir args arch obj_llvm
        let obj_path = obj_dir </> "obj.o"
        compile_llvm_to_obj args arch llvm obj_path

        new_obj <- parseElf64 "new object" =<< BS.readFile obj_path

        putStrLn "Start merge and write"
        -- Convert binary to LLVM
        let (bad_addrs, redirs) = partitionEithers $ mkRedir <$> fns
              where m = elfSegmentMap (elfLayout orig_binary)
                    mkRedir f = addrRedirection addrSymMap m f
        unless (null bad_addrs) $ do
          error $ "Found functions outside program headers:\n  "
            ++ unwords ((`showHex` "") <$> bad_addrs)
        -- Merge and write out
        let extra_addrs = Map.fromList
              [ (fromString "reopt_gen_" `BS.append` nm, w)
              | Right binary_nm <- resolveSymName <$> Set.toList notrans
              , Just w <- [Map.lookup (fromString binary_nm) symAddrMap]
                -- Get symbol name used in object.
              , Just nm <- [Map.lookup w addrSymMap]
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
      e <- readElf64 (args^.programPath)
      showCFG (args^.loadStyle) e
    ShowCFGAI -> do
      e <- readElf64 (args^.programPath)
      showCFGAndAI (args^.loadStyle) e
    ShowLLVM path -> do
      showLLVM args path
    ShowFunctions -> do
      showFunctions args
    ShowGaps -> do
      e <- readElf64 (args^.programPath)
      showGaps (args^.loadStyle) e
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
    Relink -> do
      performRedir args
    Reopt -> do
      performReopt args
