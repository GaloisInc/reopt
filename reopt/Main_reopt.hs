{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Werror #-}
module Main (main) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Elf
import           Data.List ((\\), nub, stripPrefix, intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import qualified Data.Set as Set
import           Data.Type.Equality as Equality
import           Data.Version
import           Data.Word
import qualified Data.Yaml as Yaml
import           Numeric (showHex)
import           System.Console.CmdArgs.Explicit
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import           Paths_reopt (version)

import           Flexdis86 (InstructionInstance(..))

import           Reopt
import           Reopt.Analysis.AbsState
import           Reopt.CFG.CFGDiscovery
import           Reopt.CFG.FnRep (Function(..))
import           Reopt.CFG.LLVM
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.SysDeps
import           Reopt.Machine.Types
import           Reopt.Object.Loader
import           Reopt.Object.Memory
import           Reopt.Relinker
import           Reopt.Semantics.DeadRegisterElimination
import           Reopt.Utils.Debug

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

-- | Command line arguments.
data Args = Args { _reoptAction :: !Action
                 , _programPath :: !FilePath
                 , _loadStyle   :: !LoadStyle
                 , _debugKeys   :: [DebugClass]
                 , _newobjPath  :: !FilePath
                 , _redirPath   :: !FilePath
                 , _outputPath  :: !FilePath
                 }

-- | How to load Elf file.
data LoadStyle
   = LoadBySection
     -- ^ Load loadable sections in Elf file.
   | LoadBySegment
     -- ^ Load segments in Elf file.

-- | Action to perform when running.
reoptAction :: Simple Lens Args Action
reoptAction = lens _reoptAction (\s v -> s { _reoptAction = v })

-- | Path for main executable.
programPath :: Simple Lens Args FilePath
programPath = lens _programPath (\s v -> s { _programPath = v })

-- | Whether to load file by segment or sections.
loadStyle :: Simple Lens Args LoadStyle
loadStyle = lens _loadStyle (\s v -> s { _loadStyle = v })

-- | Which debug keys (if any) to output
debugKeys :: Simple Lens Args [DebugClass]
debugKeys = lens _debugKeys (\s v -> s { _debugKeys = v })

-- | Path to new object code for relinker.
newobjPath :: Simple Lens Args FilePath
newobjPath = lens _newobjPath (\s v -> s { _newobjPath = v })

-- | Path to JSON file describing the redirections.
redirPath :: Simple Lens Args FilePath
redirPath = lens _redirPath (\s v -> s { _redirPath = v })

-- | Path to JSON file describing the outputections.
outputPath :: Simple Lens Args FilePath
outputPath = lens _outputPath (\s v -> s { _outputPath = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = ShowCFG
                   , _programPath = ""
                   , _loadStyle = LoadBySection
                   , _debugKeys = []
                   , _newobjPath = ""
                   , _redirPath  = ""
                   , _outputPath = "a.out"
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

unintercalate :: String -> String -> [String]
unintercalate punct str = reverse $ go [] "" str
  where
    go acc "" [] = acc
    go acc thisAcc [] = (reverse thisAcc) : acc
    go acc thisAcc str'@(x : xs)
      | Just sfx <- stripPrefix punct str' = go ((reverse thisAcc) : acc) "" sfx
      | otherwise = go acc (x : thisAcc) xs

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
      putStrLn msg
      exitFailure
    Right v -> return v

------------------------------------------------------------------------
-- Execution

showUsage :: IO ()
showUsage = do
  putStrLn "For help on using reopt, run \"reopt --help\"."

readElf64 :: (BS.ByteString -> Either (ByteOffset, String) (SomeElf f))
             -- ^ Function for reading value.
          -> FilePath
             -- ^ Filepath to rad.
          -> IO (f Word64)
readElf64 parseFn path = do
  when (null path) $ do
    putStrLn "Please specify a binary."
    showUsage
    exitFailure
  bs <- BS.readFile path
  case parseFn bs of
    Left (_,msg) -> do
      putStrLn $ "Error reading " ++ path ++ ":"
      putStrLn $ "  " ++ msg
      exitFailure
    Right (Elf32 _) -> do
      putStrLn "32-bit executables are not yet supported."
      exitFailure
    Right (Elf64 e) ->
      return e

dumpDisassembly :: FilePath -> IO ()
dumpDisassembly path = do
  e <- readElf64 parseElf path
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
showGaps :: LoadStyle -> ElfHeaderInfo Word64 -> IO ()
showGaps loadSty hdr = do
    -- Create memory for elf
    mem <- mkElfMem loadSty (getElf hdr)
    let cfg = finalCFG (mkFinalCFG mem hdr)
    let ends = cfgBlockEnds cfg
    let blocks = [ addr | GeneratedBlock addr 0 <- Map.keys (cfg ^. cfgBlocks) ]
    let gaps = filter (isInterestingCode mem)
             $ out_gap blocks (Set.elems ends)
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

showCFG :: LoadStyle -> ElfHeaderInfo Word64 -> IO ()
showCFG loadSty hdr = do
  -- Create memory for elf
  mem <- mkElfMem loadSty (getElf hdr)
  let fg = mkFinalCFG mem hdr
  let g = eliminateDeadRegisters (finalCFG fg)
  print (pretty g)

showFunctions :: LoadStyle -> ElfHeaderInfo Word64 -> IO ()
showFunctions loadSty hdr = do
  -- Create memory for elf
  mem <- mkElfMem loadSty (getElf hdr)
  print mem
  let fg = mkFinalCFG mem hdr
  -- let g = eliminateDeadRegisters (finalCFG fg)
  mapM_ (print . pretty) (finalFunctions fg)

------------------------------------------------------------------------
-- Pattern match on stack pointer possibilities.

ppStmtAndAbs :: MapF Assignment AbsValue -> Stmt -> Doc
ppStmtAndAbs m stmt =
  case stmt of
    AssignStmt a ->
      case ppAbsValue =<< MapF.lookup a m of
        Just d -> vcat [ text "#" <+> ppAssignId (assignId a) <+> text ":=" <+> d
                       , pretty a
                       ]
        Nothing -> pretty a
    _ -> pretty stmt


ppBlockAndAbs :: MapF Assignment AbsValue -> Block -> Doc
ppBlockAndAbs m b =
  pretty (blockLabel b) <> text ":" <$$>
  indent 2 (vcat (ppStmtAndAbs m <$> blockStmts b) <$$>
            pretty (blockTerm b))

mkFinalCFGWithSyms :: Memory Word64
                   -> ElfHeaderInfo Word64
                   -> (FinalCFG, Map CodeAddr BS.ByteString)
mkFinalCFGWithSyms mem hdr = (cfgFromAddrs mem sym_map sysp (elfEntry e:sym_addrs), sym_map)
        -- Get list of code locations to explore starting from entry points (i.e., eltEntry)
  where e = getElf hdr
        sym_assocs = [ (steValue ste, steName ste)
                     | ste <- concat (parseSymbolTables hdr)
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

mkFinalCFG :: Memory Word64 -> ElfHeaderInfo Word64 -> FinalCFG
mkFinalCFG mem hdr = fst (mkFinalCFGWithSyms mem hdr)

showCFGAndAI :: LoadStyle -> ElfHeaderInfo Word64 -> IO ()
showCFGAndAI loadSty hdr = do
  -- Create memory for elf
  mem <- mkElfMem loadSty (getElf hdr)
  let fg = mkFinalCFG mem hdr
  let abst = finalAbsState fg
      amap = assignmentAbsValues mem fg
  let g  = eliminateDeadRegisters (finalCFG fg)
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

showLLVM :: LoadStyle -> ElfHeaderInfo Word64 -> String -> IO ()
showLLVM loadSty e dir = do
  -- Create memory for elf
  mem <- mkElfMem loadSty (getElf e)
  let (cfg, symMap) = mkFinalCFGWithSyms mem e

  let mkName f = dir </> (name ++ "_" ++ showHex addr ".ll")
        where
          name = case Map.lookup addr symMap of
                   Nothing -> "unknown"
                   Just s  -> BSC.unpack s
          addr = fnAddr f

  let mkF f = (,) (mkName f) . snd $ L.runLLVM (functionToLLVM f)
  let writeF (n, m) = writeFile n (show $ L.ppModule m)

  createDirectoryIfMissing True dir
  mapM_ (writeF . mkF) (finalFunctions cfg)

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
stateEndsWithRet :: X86State Value -> Bool
stateEndsWithRet s = do
  let next_ip = s^.register N.rip
      next_sp = s^.register N.rsp
  case () of
    _ | AssignedValue (Assignment _ (Read (MemLoc a _))) <- next_ip
      , (ip_base, ip_off) <- asBaseOffset a
      , (sp_base, sp_off) <- asBaseOffset next_sp ->
        ip_base == sp_base && ip_off + 8 == sp_off
    _ -> False

-- | @isrWriteTo stmt add tpr@ returns true if @stmt@ writes to @addr@
-- with a write having the given type.
isWriteTo :: Stmt -> Value (BVType 64) -> TypeRepr tp -> Maybe (Value tp)
isWriteTo (Write (MemLoc a _) val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (valueType val) tp =
    Just val
isWriteTo _ _ _ = Nothing

-- | @isCodeAddrWriteTo mem stmt addr@ returns true if @stmt@ writes to @addr@ and
-- @addr@ is a code pointer.
isCodeAddrWriteTo :: Memory Word64 -> Stmt -> Value (BVType 64) -> Maybe Word64
isCodeAddrWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodeAddr mem (fromInteger val)
  = Just (fromInteger val)
isCodeAddrWriteTo _ _ _ = Nothing

-- | Returns true if it looks like block ends with a call.
blockContainsCall :: Memory Word64 -> Block -> X86State Value -> Bool
blockContainsCall mem b s =
  let next_sp = s^.register N.rsp
      go [] = False
      go (stmt:_) | Just _ <- isCodeAddrWriteTo mem stmt next_sp = True
      go (Write _ _:_) = False
      go (_:r) = go r
   in go (reverse (blockStmts b))

-- | Return next states for block.
blockNextStates :: CFG -> Block -> [X86State Value]
blockNextStates g b =
  case blockTerm b of
    FetchAndExecute s -> [s]
    Branch _ x_lbl y_lbl -> blockNextStates g x ++ blockNextStates g y
      where Just x = findBlock g x_lbl
            Just y = findBlock g y_lbl
    Syscall _ -> []

checkReturnsIdentified :: CFG -> Block -> IO ()
checkReturnsIdentified g b = do
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

checkCallsIdentified :: Memory Word64 -> CFG -> Block -> IO ()
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

mkElfMem :: (ElfWidth w, Functor m, Monad m) => LoadStyle -> Elf w -> m (Memory w)
mkElfMem sty e = do
  mi <- elfInterpreter e
  case mi of
    Nothing ->
      return ()
    Just{} ->
      fail "reopt does not yet support generating CFGs from dynamically linked executables."
  case sty of
    LoadBySection -> memoryForElfSections e
    LoadBySegment -> memoryForElfSegments e

performRedir :: Args -> IO ()
performRedir args = do
  -- Get original binary
  orig_binary_header <- readElf64 parseElfHeaderInfo (args^.programPath)

  let output_path = args^.outputPath
  case args^.newobjPath of
    -- When no new object is provided, we just copy the input
    -- file to test out Elf decoder/encoder.
    "" -> do
      putStrLn $ "Copying binary to: " ++ output_path
      BSL.writeFile output_path $ renderElf (getElf orig_binary_header)
    -- When a non-empty new obj is provided we test
    new_obj_path -> do
      new_obj <- readElf64 parseElfHeaderInfo new_obj_path
      redirs <-
        case args^.redirPath of
          "" -> return []
          redir_path -> do
            mredirs <- Yaml.decodeFileEither redir_path
            case mredirs of
              Left e -> fail $ show e
              Right r -> return r
      case mergeObject orig_binary_header new_obj redirs of
        Left msg -> fail msg
        Right new_binary -> do
          putStrLn $ "Writing new binary: " ++ output_path
          print $ elfType new_binary
          BSL.writeFile output_path $ renderElf new_binary

main :: IO ()
main = do
  args <- getCommandLineArgs
  setDebugKeys (args ^. debugKeys)
  case args^.reoptAction of
    DumpDisassembly -> do
      dumpDisassembly (args^.programPath)
    ShowCFG -> do
      e <- readElf64 parseElfHeaderInfo (args^.programPath)
      showCFG (args^.loadStyle) e
    ShowCFGAI -> do
      e <- readElf64 parseElfHeaderInfo (args^.programPath)
      showCFGAndAI (args^.loadStyle) e
    ShowLLVM path -> do
      e <- readElf64 parseElfHeaderInfo (args^.programPath)
      showLLVM (args^.loadStyle) e path
    ShowFunctions -> do
      e <- readElf64 parseElfHeaderInfo (args^.programPath)
      showFunctions (args^.loadStyle) e
    ShowGaps -> do
      e <- readElf64 parseElfHeaderInfo (args^.programPath)
      showGaps (args^.loadStyle) e
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
    Relink -> do
      performRedir args
