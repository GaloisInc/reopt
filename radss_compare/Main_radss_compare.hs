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

import Debug.Trace

import           Flexdis86 (InstructionInstance(..))
import           Lang.Crucible.Utils.MonadST
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
import           Reopt.Symbolic.Semantics


------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = FindBlocks      -- ^ Display a cfg for a function
   | CompareBlocks   -- ^ Compare basic blocks in two programs
   | MatchBlocks     -- ^ Match basic blocks in two programs
   | ShowSingleBlock -- ^ Print out concrete semantics of a basic block
   | SymExec         -- ^ Symbolically execute a block
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
singleBlockFlag = flagNone [ "show-block", "b" ] upd help
  where upd  = reoptAction .~ ShowSingleBlock
        help = "Dump one basic block of an executable."

functionCFGFlag :: Flag Args
functionCFGFlag = flagNone [ "function-cfg", "f" ] upd help
  where upd  = reoptAction .~ FindBlocks
        help = "Print out control flow graph of a function."

compareBlocksFlag :: Flag Args
compareBlocksFlag = flagNone [ "compare-blocks", "c" ] upd help
  where upd  = reoptAction .~ CompareBlocks
        help = "Print out concrete semantics for basic blocks of executable."

symExecFlag :: Flag Args
symExecFlag = flagNone [ "sym-exec", "s" ] upd help
  where upd  = reoptAction .~ SymExec
        help = "Symbolically execute a block."
matchBlocksFlag :: Flag Args
matchBlocksFlag = flagNone [ "match-blocks", "m" ] upd help
  where upd  = reoptAction .~ MatchBlocks
        help = "Match basic blocks of a function in two different binaries."

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
                , symExecFlag
                , matchBlocksFlag
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

symExec :: Memory Word64 -> Word64 -> IO ()
symExec mem start =
  -- case findBlocks mem entry of
  --   Left err -> putStrLn err
  --   Right cfg -> do
  --     let block0 = M.lookup entry cfg
  --     block0 <- case M.lookup entry cfg of
  --                 Nothing -> putStrLn "impossible" >> exitFailure
  --                 Just b -> return b
  case runMemoryByteReader pf_x mem start $ extractBlock start S.empty of
    Left err -> putStrLn $ show err
    Right (Left err, _) -> putStrLn  $ "Could not disassemble instruction at 0x"
                                      ++ showHex err ""
    Right (Right (nexts, ret, stmts), _) -> do
      let block0 = Block stmts Ret
      halloc <- liftST newHandleAllocator
      crucibleCFG <- liftST $ translateBlock halloc block0
      return ()


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

matchCFGs :: Memory Word64 -> Word64 -> Memory Word64 -> Word64 -> IO ()
matchCFGs mem1 entry1 mem2 entry2 
  | Right cfg1 <- findBlocks mem1 entry1
  , Right cfg2 <- findBlocks mem2 entry2 = do
      let Just initBlock1 = M.lookup entry1 cfg1
      let Just initBlock2 = M.lookup entry2 cfg2
      let blockMap = globalMatch cfg1 cfg2 M.empty allSels
      let matched = matchCFGs' cfg1 cfg2 (M.insert entry1 entry2 blockMap) allSels
      M.foldlWithKey (\m k v -> m >> (putStrLn $ "0x" ++ showHex k " <--> 0x" ++ showHex v "" )) (return ())  matched
      S.foldl (\m k -> m >> (putStrLn $ "Ox" ++ showHex k "")) (return ()) (S.difference (M.keysSet cfg1) (M.keysSet matched))
      S.foldl (\m k -> m >> (putStrLn $ "              Ox" ++ showHex k "")) (return ()) (S.difference (M.keysSet cfg2) (M.foldr S.insert S.empty matched))
     
     -- TODO: more fixed-points
      -- TODO: backwards chain as well
      -- TODO: unique parent of unique child chain shrinking
matchCFGs' cfg1 cfg2 mapping (sel : sels) =
  let mapping' = M.foldlWithKey (matchChildSets sel cfg1 cfg2) mapping mapping
      mapping'' = M.foldlWithKey (matchParentSets sel cfg1 cfg2 (getParentMap cfg1) (getParentMap cfg2)) mapping' mapping'
  in if mapping'' == mapping then matchCFGs' cfg1 cfg2 mapping sels else matchCFGs' cfg1 cfg2 mapping'' allSels
matchCFGs' _ _ mapping [] = mapping

globalMatch cfg1 cfg2 mapping (sel : sels) =
  let s1 = M.keysSet cfg1
      s2 = M.keysSet cfg2
      (matched1, unmatched1) = S.partition (flip M.member mapping) s1
      unmatched2 = S.difference s2 $ S.foldl (\s v -> case M.lookup v mapping of
                                              Just v' -> S.insert v' s
                                              Nothing -> s) S.empty matched1
      newMap = snd $ S.foldl (\ (others, newMap) p1 -> case sel cfg1 cfg2 p1 others of 
                                        Just (p2, newOthers) -> (newOthers, M.insert p1 p2 newMap)
                                        Nothing -> (others, newMap)) (unmatched2, mapping) unmatched1
  in if newMap == mapping then globalMatch cfg1 cfg2 mapping sels
                          else globalMatch cfg1 cfg2 newMap allSels
globalMatch _ _  mapping [] = mapping

-- do we need an old mapping and a mapping being iterated over?
matchChildSets :: (CFG -> CFG -> Word64 -> Set Word64 -> Maybe (Word64, Set Word64))
               -> CFG -> CFG -> Map Word64 Word64 -> Word64 -> Word64 -> Map Word64 Word64
matchChildSets sel cfg1 cfg2 mapping k1 k2
  | Just (Block _ (Direct c1)) <- M.lookup k1 cfg1
  , Just (Block _ (Direct c2)) <- M.lookup k2 cfg2 = M.insert c1 c2 mapping

  | Just (Block _ (Direct c1)) <- M.lookup k1 cfg1
  , Just (Block _ (Fallthrough c2)) <- M.lookup k2 cfg2 = M.insert c1 c2 mapping
  
  | Just (Block _ (Fallthrough c1)) <- M.lookup k1 cfg1
  , Just (Block _ (Direct c2)) <- M.lookup k2 cfg2 = M.insert c1 c2 mapping
  
  | Just (Block _ (Fallthrough c1)) <- M.lookup k1 cfg1
  , Just (Block _ (Fallthrough c2)) <- M.lookup k2 cfg2 = M.insert c1 c2 mapping
  | Just (Block _ (Call callee1 ret1 )) <- M.lookup k1 cfg1
  , Just (Block _ (Call callee2 ret2)) <- M.lookup k2 cfg2 = M.insert ret1 ret2 mapping
  | Just (Block _ (Cond c1 c1')) <- M.lookup k1 cfg1
  , Just (Block _ (Cond c2 c2')) <- M.lookup k2 cfg2 = 
      case sel cfg1 cfg2 c1 (S.fromList [c2, c2']) of
        Just (m, s) -> M.insert c1' ( trace "matchChildSets" $ S.findMin s) (M.insert c1 m mapping)
        Nothing -> case sel cfg1 cfg2 c1' (S.fromList [c2, c2']) of
          Just (m, s) -> M.insert c1 (trace "matchChildSets" $ S.findMin s) (M.insert c1' m mapping)
          Nothing -> mapping
  | otherwise = mapping
matchChildSets _ _ _ mapping _ _ = mapping

allSels = [singletonSelector, termFuzzyMatchSelector]

singletonSelector :: CFG -> CFG -> Word64 -> Set Word64 -> Maybe (Word64, Set Word64)
singletonSelector _ _ a as
  | S.size as == 1 = Just (trace "singletonSelector" $ S.findMin as, S.empty)
  | otherwise = Nothing

termFuzzyMatchSelector :: CFG -> CFG -> Word64 -> Set Word64 -> Maybe (Word64, Set Word64)
termFuzzyMatchSelector cfg1 cfg2 a as = do 
  b <- M.lookup a cfg1
  let bs = S.map (\(Just b', a') -> (b', a')) $ S.filter (isJust . fst) $ S.map (\a' -> (M.lookup a' cfg2, a')) as
  let matches = S.map snd $ S.filter (blockTermFuzzyMatches b . fst) bs
  (match, _) <- if S.size matches <= 1 then S.minView matches else Nothing
  Just (match, S.delete match as)

getParentMap :: CFG -> Map Word64 (Set Word64)
getParentMap = 
  M.foldlWithKey (\m p (Block _ t) -> 
                      foldl (\m' c -> M.insertWith (S.union) c (S.singleton p) m')
                            m (termChildren t)) M.empty

matchParentSets :: (CFG -> CFG -> Word64 -> Set Word64 -> Maybe (Word64, Set Word64))
                -> CFG 
                -> CFG 
                -> Map Word64  (Set Word64) 
                -> Map Word64  (Set Word64) 
                -> Map Word64 Word64 
                -> Word64 
                -> Word64 
                -> Map Word64 Word64
matchParentSets selector cfg1 cfg2 parentMap1 parentMap2 mapping k1 k2
  | Just p1s <- M.lookup k1 parentMap1
  , Just p2s <- M.lookup k2 parentMap2
  , (matched1, unmatched1) <- S.partition (flip M.member mapping) p1s
  , unmatched2 <- S.difference p2s $ S.foldl (\s v -> case M.lookup v mapping of
                                              Just v' -> S.insert v' s
                                              Nothing -> s) S.empty matched1 =
      snd $ S.foldl (\ (others, newMap) p1 -> case selector cfg1 cfg2 p1 others of 
                                        Just (p2, newOthers) -> (newOthers, M.insert p1 p2 newMap)
                                        Nothing -> (others, newMap)) (unmatched2, mapping) unmatched1
  | otherwise = mapping


-- returns true if the two blocks have superficially similar control structures
-- at the end - helps differentiate two children of a matched conditional
blockTermFuzzyMatches :: Block -> Block -> Bool --  add some context here?
blockTermFuzzyMatches (Block _ (Direct _)) (Block _ (Direct _)) = True
blockTermFuzzyMatches (Block _ (Direct _)) (Block _ (Fallthrough _)) = True
blockTermFuzzyMatches (Block _ (Fallthrough _)) (Block _ (Direct _)) = True
blockTermFuzzyMatches (Block _ (Fallthrough _)) (Block _ (Fallthrough _)) = True
blockTermFuzzyMatches (Block _ (Call _ _)) (Block _ (Call _ _)) = True
blockTermFuzzyMatches (Block _ Ret) (Block _ Ret) = True
blockTermFuzzyMatches (Block _ (Cond _ _)) (Block _ (Cond _ _)) = True
blockTermFuzzyMatches (Block _ (Indirect _)) (Block _ (Indirect _)) = True
blockTermFuzzyMatches _ _ = False

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
    SymExec -> do
      e <- case args^.programPaths of
        [path] -> readStaticElf path
        _      -> fail "usage: filename"
      mem <- mkElfMem (args^.loadStyle) e
      let a = case args^.addr1 of
                0 -> elfEntry e
                a' -> a'
      symExec mem a
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
    MatchBlocks -> do
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
      matchCFGs mem1 a1 mem2 a2
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
