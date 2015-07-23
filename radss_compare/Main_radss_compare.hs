{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.BitVector as BV
import           Data.Elf
import           Data.Foldable (traverse_)
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Word
import           Data.Version
import           GHC.TypeLits
import           Numeric (showHex, readHex)
import           Reopt.Analysis.AbsState
import           System.Console.CmdArgs.Explicit
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some

import           Paths_reopt (version)
import           Data.Type.Equality as Equality

import           Flexdis86 (InstructionInstance(..))
import           Reopt
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Machine.X86State
import           Reopt.Object.Loader
import           Reopt.Object.Memory
import           Reopt.Semantics.DeadRegisterElimination
import           Reopt.Semantics.Monad (Type(..))
import           Reopt.BasicBlock.Extract
import           Reopt.BasicBlock.FunctionCFG
import qualified Reopt.Concrete.Semantics as CS
import           Reopt.Concrete.BitVector
import           Reopt.Concrete.MachineState as MS

------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = FindBlocks      -- ^ Display a cfg for a function
   | CompareBlocks   -- ^ Compare basic blocks in two programs
   | ShowSingleBlock -- ^ Print out concrete semantics of a basic block
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version

-- | Command line arguments.
data Args = Args { _reoptAction  :: !Action
                 , _programPaths :: [FilePath]
                 , _addr1        :: !Word64
                 , _addr2        :: !Word64
                 , _loadStyle    :: !LoadStyle
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

-- | Path to load
programPaths :: Simple Lens Args [FilePath]
programPaths = lens _programPaths (\s v -> s { _programPaths = v })

-- | Path to load
addr1 :: Simple Lens Args Word64
addr1 = lens _addr1 (\s v -> s { _addr1 = v })

-- | Path to load
addr2 :: Simple Lens Args Word64
addr2 = lens _addr2 (\s v -> s { _addr2 = v })

-- | Whether to load file by segment or sections.
loadStyle :: Simple Lens Args LoadStyle
loadStyle = lens _loadStyle (\s v -> s { _loadStyle = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = ShowSingleBlock
                   , _programPaths = []
                   , _addr1 = 0
                   , _addr2 = 0
                   , _loadStyle = LoadBySection
                   }

------------------------------------------------------------------------
-- Argument processing

singleBlockFlag :: Flag Args
singleBlockFlag = flagNone [ "concrete-blocks", "b" ] upd help
  where upd  = reoptAction .~ ShowSingleBlock
        help = "Print out concrete semantics for basic blocks of executable."

functionCFGFlag :: Flag Args
functionCFGFlag = flagNone [ "function-cfg", "f" ] upd help
  where upd  = reoptAction .~ FindBlocks
        help = "Print out control flow graph of a function."

compareBlocksFlag :: Flag Args
compareBlocksFlag = flagNone [ "compare-blocks", "c" ] upd help
  where upd  = reoptAction .~ CompareBlocks
        help = "Print out concrete semantics for basic blocks of executable."

segmentFlag :: Flag Args
segmentFlag = flagNone [ "load-segments" ] upd help
  where upd  = loadStyle .~ LoadBySegment
        help = "Load the Elf file using segment information."

sectionFlag :: Flag Args
sectionFlag = flagNone [ "load-sections" ] upd help
  where upd  = loadStyle .~ LoadBySection
        help = "Load the Elf file using section information (default)."

addr1Flag :: Flag Args
addr1Flag = flagReq [ "addr1" ] upd "Hex String" help
  where upd s old  = case readHex s of
          (addr, _) : _ -> Right $ (addr1 .~ addr) old
          _ -> Left "Could not parse addr1 - should be a bare hex value, like deadbeef"
        help = "Address to start from in the first executable."

addr2Flag :: Flag Args
addr2Flag = flagReq [ "addr2" ] upd "Hex String" help
  where upd s old  = case readHex s of
          (addr, _) : _ -> Right $ (addr2 .~ addr) old
          _ -> Left "Could not parse addr2 - should be a bare hex value, like deadbeef"
        help = "Address to start from in the first executable."

arguments :: Mode Args
arguments = mode "radss_compare" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ segmentFlag
                , sectionFlag
                , singleBlockFlag
                , compareBlocksFlag
                , functionCFGFlag
                , addr1Flag
                , addr2Flag
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
        setFilename nm a = Right (a & programPaths %~ (++ [nm]))

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

readElf64 :: FilePath -> IO (Elf Word64)
readElf64 path = do
  when (null path) $ do
    putStrLn "Please specify a binary."
    showUsage
    exitFailure
  bs <- B.readFile path
  case parseElf bs of
    Left (_,msg) -> do
      putStrLn $ "Error reading " ++ path ++ ":"
      putStrLn $ "  " ++ msg
      exitFailure
    Right (Elf32 _) -> do
      putStrLn "32-bit executables are not yet supported."
      exitFailure
    Right (Elf64 e) ->
      return e

readStaticElf :: FilePath -> IO (Elf Word64)
readStaticElf path = do
  e <- readElf64 path
  mi <- elfInterpreter e
  case mi of
    Nothing ->
      return ()
    Just{} ->
      fail "reopt does not yet support generating CFGs from dynamically linked executables."
  return e

showSingleBlock :: Memory Word64 -> Word64 -> IO ()
showSingleBlock mem start = 
  case runMemoryByteReader pf_x mem start $ extractBlock start S.empty of
    Left err -> putStrLn $ show err
    Right (Left err, _) -> putStrLn  $ "Could not disassemble instruction at 0x"
                                      ++ showHex err ""
    Right (Right (nexts, ret, stmts), _) -> do
      putStrLn $ show nexts
      putStrLn $ show ret
      putStrLn $ show $ CS.ppStmts stmts


compareBlocks :: Memory Word64 -> Word64 -> Memory Word64 -> Word64 -> IO ()
compareBlocks mem1 start1 mem2 start2 = do
  (nexts1, ret1, stmts1) <- getBlock mem1 start1
  (nexts2, ret2, stmts2) <- getBlock mem2 start2
  let (_, (cmem1, regs1)) = runNullMachineState $ runConcreteState (evalStateT (mapM_ CS.evalStmt stmts1) MapF.empty) M.empty emptyX86State 
  let (_, (cmem2, regs2)) = runNullMachineState $ runConcreteState (evalStateT (mapM_ CS.evalStmt stmts2) MapF.empty) M.empty emptyX86State
  let regCmp = compareRegs regs1 regs2
  let memCmp1 = M.foldrWithKey (\k v l -> case M.lookup k cmem2 of
                              Just v' -> if v == v' then l else ("different values at address " ++ show k ++ 
                                                                 ":   1: " ++ show v ++ "   2:" ++ show v') : l
                              Nothing -> ("1 had a value at address " ++ show k ++ " but 2 did not") : l)
                  [] cmem1
  let memCmp2 = M.foldrWithKey (\k v l -> case M.lookup k cmem1 of
                              Just v' -> if v == v' then l else ("different values at address " ++ show k ++ 
                                                                 ":   1: " ++ show v' ++ "   2:" ++ show v) : l
                              Nothing -> ("2 had a value at address " ++ show k ++ " but 1 did not") : l)
                  [] cmem1
  let allCmp = regCmp ++ memCmp1 ++ memCmp2
  case allCmp of [] -> putStrLn "all matched!"
                 _ -> mapM_ putStrLn allCmp
  
  where
  getBlock mem start = case runMemoryByteReader pf_x mem start $ extractBlock start S.empty of
    Left err -> fail $ show err
    Right (Left err, _) -> fail $ "Could not disassemble instruction at 0x" ++
                                    showHex err ""
    Right (Right (nexts, ret, stmts), _) -> return (nexts, ret, stmts)
  emptyX86State = mkX86State (\reg -> 
    Literal $ bitVector (N.registerWidth reg) 
      (BV.bitVec (fromIntegral $ natValue $ N.registerWidth reg) (0 :: Integer)))

compareRegs :: X86State MS.Value -> X86State MS.Value -> [String]
compareRegs rs1 rs2 =
  catMaybes $ map (viewSome (\reg ->
    let lens = register reg
        r1Val = rs1^.lens
        r2Val = rs2^.lens
    in if r1Val `equalOrUndef` r2Val
        then Nothing
        else Just $ show reg ++ " did not match.  1: " ++ show r1Val ++ "   2:" ++ show r2Val)) 
    x86StateRegisters

extractCFG :: Memory Word64 -> Word64 -> IO ()
extractCFG mem entry =
  case findBlocks mem entry of 
    Left err -> putStrLn err
    Right cfg -> M.foldlWithKey 
      (\ m addr (Block _ t) -> do m; putStrLn $ "0x" ++ showHex addr ": " ++ show t) 
      (return ()) 
      cfg

mkElfMem :: (ElfWidth w, Functor m, Monad m) => LoadStyle -> Elf w -> m (Memory w)
mkElfMem LoadBySection e = memoryForElfSections e
mkElfMem LoadBySegment e = memoryForElfSegments e


main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    ShowSingleBlock -> do
      e <- case args^.programPaths of
        [] -> fail "A file name is required\n"
        _ : _ : _ -> fail "Exactly one file name for show single block"
        [path] -> readStaticElf path
      mem <- mkElfMem (args^.loadStyle) e
      showSingleBlock mem (elfEntry e)
    FindBlocks -> do
      e <- case args^.programPaths of
        [] -> fail "A file name is required\n"
        _ : _ : _ -> fail "Exactly one file name for function cfg"
        [path] -> readStaticElf path
      mem <- mkElfMem (args^.loadStyle) e
      let a = case args^.addr1 of 0 -> elfEntry e
                                  a' -> a'
      extractCFG mem a
    CompareBlocks -> do
      (e1, e2) <- case args^.programPaths of
        [] -> fail "Two file names are required\n"
        [_] -> fail "Two file names are required\n"
        [f1,f2] -> do
          e1' <- readStaticElf f1
          e2' <- readStaticElf f2
          return (e1', e2')
        _ : _ : _ : _ -> fail "Exactly two file names for comparison"
      mem1 <- mkElfMem (args^.loadStyle) e1
      mem2 <- mkElfMem (args^.loadStyle) e2
      let a1 = case args^.addr1 of 0 -> elfEntry e1
                                   a -> a
      let a2 = case args^.addr2 of 0 -> elfEntry e2
                                   a -> a
      compareBlocks mem1 a1 mem2 a2
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
