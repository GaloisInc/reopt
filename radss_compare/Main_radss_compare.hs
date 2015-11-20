{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
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

import           Data.Parameterized.Context ((%>))
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some

import           Paths_reopt (version)
import           Data.Type.Equality as Equality

import           Debug.Trace

import           Flexdis86 (InstructionInstance(..))

import           Lang.Crucible.Config (initialConfig)
import qualified Lang.Crucible.Core as C
import           Lang.Crucible.ExtractSubgraph
import           Lang.Crucible.MATLAB.UtilityFunctions (newMatlabUtilityFunctions)
import           Lang.Crucible.ProgramLoc
import           Lang.Crucible.Simulator.CallFns
import           Lang.Crucible.Simulator.ExecutionTree
import           Lang.Crucible.Simulator.MSSim as MSS
import           Lang.Crucible.Simulator.RegMap
import           Lang.Crucible.Simulator.SimError
import           Lang.Crucible.Solver.Adapter
import qualified Lang.Crucible.Solver.Interface as I
import           Lang.Crucible.Solver.PartExpr
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.Solver.SimpleBackend.CVC4
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
                 , _ripFile      :: !FilePath
                 , _bcFile       :: !FilePath
                 , _gprsFile     :: !FilePath
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

-- | Start address in first file
addr1 :: Simple Lens Args Word64
addr1 = lens _addr1 (\s v -> s { _addr1 = v })

-- | Start address in second file
addr2 :: Simple Lens Args Word64
addr2 = lens _addr2 (\s v -> s { _addr2 = v })

-- | file to load rip mapping from
ripFile :: Simple Lens Args FilePath
ripFile = lens _ripFile (\s v -> s { _ripFile = v })

-- | file to load breadcrumb mapping from
bcFile :: Simple Lens Args FilePath
bcFile = lens _bcFile (\s v -> s { _bcFile = v })

-- | file to load gpr mapping from
gprsFile :: Simple Lens Args FilePath
gprsFile = lens _gprsFile (\s v -> s { _gprsFile = v })

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
                   , _ripFile = ""
                   , _bcFile = ""
                   , _gprsFile = ""
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
        help = "Address to start from in the second executable."

ripFlag :: Flag Args
ripFlag = flagReq [ "rip" ] upd "File Path" help
  where upd s old  = Right $ (ripFile .~ s) old
        -- The @--help@ pretty printer reformats this, which might be
        -- helpful, except it doesn't preserve the empty lines :P
        help = unlines [ "File containing a mapping between control points in the two variants."
                       , ""
                       , "Example content of file argument:"
                       , ""
                       , "    'fromList [(4198515,[4198520]),(4198454,[4198468]),(4198427,[4198437])]'"
                       ]

bcFlag :: Flag Args
bcFlag = flagReq [ "bc" ] upd "File Path" help
  where upd s old  = Right $ (bcFile .~ s) old
        help = "File containing a mapping of breadcrumb addresses across two variants"

gprsFlag :: Flag Args
gprsFlag = flagReq [ "gprs" ] upd "File Path" help
  where upd s old  = Right $ (gprsFile .~ s) old
        help = unlines [ "File containing a mapping between related registers between registers."
                       , ""
                       , "The same mapping is used at all related control points (as identified"
                       , "by the '--rip RIP_FILE' option), although in some cases this is not"
                       , "general enough."
                       , ""
                       , "Example content of file argument:"
                       , ""
                       , "    'fromList [(0,0),(8,9),(6,6),(7,7),(2,8),(1,2),(4,4),(5,5)]'"
                       ]

arguments :: Mode Args
arguments = mode "radss_compare" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ segmentFlag
                , sectionFlag
                , singleBlockFlag
                , symExecFlag
                , matchBlocksFlag
                , functionCFGFlag
                , addr1Flag
                , addr2Flag
                , ripFlag
                , bcFlag
                , gprsFlag
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

extractFirstBlock :: Memory Word64 -> Word64 -> IO Block
extractFirstBlock mem start =
  case runMemoryByteReader pf_x mem start $ extractBlock start S.empty of
    Left err -> error $ show err
    Right (Left err, _) -> error $ "Could not disassemble instruction at 0x"
                                ++ showHex err ""
    Right (Right (nexts, stmts), _) -> return (Block stmts Ret)

simulateCFGs :: Word64
            -> Map Word64 Block
            -> Word64
            -> Map Word64 Block
            -> Map Word64 [Word64]
            -> Map Integer Integer
            -> IO ()
simulateCFGs entry1 reifcfg1 entry2 reifcfg2 ripRel gprRel = do
  halloc <- liftST newHandleAllocator
  cfg1 <- liftST $ translateFunction halloc reifcfg1 entry1
--  case cfg1 of C.SomeCFG cfg1' -> putStrLn $ show $ C.ppCFG True cfg1'
  cfg2 <- liftST $ translateFunction halloc reifcfg2 entry2
--  case cfg2 of C.SomeCFG cfg2' -> putStrLn $ show $ C.ppCFG True cfg2'
  M.foldlWithKey (\ res k v -> do
      res
      putStrLn $ "carving first subcfg starting at 0x" ++ showHex k ""
      Just subCFG1 <- case cfg1 of
        C.SomeCFG cfg1' -> do
          let cuts = findCuts (M.keysSet ripRel) cfg1'
          putStrLn $ show cuts
          liftST $ extractSubgraph cfg1' cuts (findBlock k cfg1') halloc
      putStrLn "first subcfg found"
      Just subCFG2 <- case v of
        [v'] -> case cfg2 of
          C.SomeCFG cfg2' -> do
            let cuts = findCuts (S.fromList $ concat $ M.elems ripRel) cfg2'
            putStrLn $ show cuts
            liftST $ extractSubgraph cfg2' cuts (findBlock v' cfg2') halloc
      putStrLn "second subcfg found"
      simulate (showHex k "") subCFG1 subCFG2 halloc ripRel gprRel) (return ()) ripRel

-- Finds the set of matched basic blocks that we will use as endpoints of
-- subgraphs
findCuts :: Set Word64 -> C.CFG blocks init MachineState -> Set (C.BlockID blocks MachineStateCtx)
findCuts tags C.CFG{C.cfgBlockMap = blockMap} =
  S.fromList $ catMaybes $ Ctx.toList (\block ->
    case testEquality machineStateCtx $ C.blockInputs block of
      Just Refl -> case getBlockStartPos block of
        Just tag -> if S.member tag tags then Just $ C.blockID block else Nothing
        _ -> Nothing
      Nothing -> Nothing) blockMap

-- find a particular block in the subgraph - if there are multiple copies, pick
-- the first one (the multiple copies case is related to Lang.Crucible.Generator
-- state handling)
findBlock :: Word64 -> C.CFG blocks init MachineState -> C.BlockID blocks MachineStateCtx
findBlock tag C.CFG{C.cfgBlockMap = blockMap} =
  case catMaybes $ Ctx.toList (\block ->
    case testEquality machineStateCtx $ C.blockInputs block of
      Just Refl -> case getBlockStartPos block of
        Just tag' -> if tag == tag' then Just $ C.blockID block else Nothing
        _ -> Nothing
      Nothing -> Nothing) blockMap of
    (blockID : _) -> blockID
    [] -> error $ "no block found for address 0x" ++ showHex tag ""

getBlockStartPos :: C.Block blocks init ret -> Maybe Word64
getBlockStartPos block =
  case plSourceLoc (case block of
    C.Block{C.blockDiffStmts = C.ConsStmt loc _ _} -> loc
    C.Block{C.blockDiffStmts = C.TermStmt loc _} -> loc) of
      BinaryPos _ w -> Just w
      _ -> Nothing

simulateBlocks :: Block -> Block -> Map Word64 [Word64] -> Map Integer Integer -> IO ()
simulateBlocks block1 block2 ripRel gprRel = do
  halloc <- liftST newHandleAllocator
  cfg1 <- liftST $ translateSingleBlock halloc block1
  cfg2 <- liftST $ translateSingleBlock halloc block2
  simulate "0" cfg1 cfg2 halloc ripRel gprRel

-- Given two Crucible CFGs, a relation of control points, and a relation of
-- registers, construct an SMT term asserting that there is some set of
-- assignments of registers that is related such that the resulting assignment
-- and control flow points are unrelated.
-- TODO: generatlize the gprRel to differ at different control points - an
-- initial relation, plus a mapping from pairs of related control points to the
-- relation on states at those control points.
simulate :: String
         -> C.SomeCFG MachineStateCtx MachineState
         -> C.SomeCFG MachineStateCtx MachineState
         -> HandleAllocator RealWorld
         -> Map Word64 [Word64]
         -> Map Integer Integer
         -> IO ()
simulate name (C.SomeCFG cfg1) (C.SomeCFG cfg2) halloc ripRel gprRel = do
  out <- openFile ("/tmp/sat_trans_" ++ name) WriteMode
  withSimpleBackend' halloc $ \ctx -> do
    let sym = ctx^.ctxSymInterface
    (initGprs1, initGprs2) <- mkInitialGPRs gprRel sym n4 n64 [0..15]
    flags  <- I.emptyWordMap sym n5 (C.BaseBVRepr n1)
    flags' <- foldM (initWordMap sym n5 n1) flags [0..31]
    pc     <- I.freshConstant sym (C.BaseBVRepr n64)
    heap   <- I.freshConstant sym (C.BaseArrayRepr C.indexTypeRepr C.baseTypeRepr)
    let struct1 = Ctx.empty %> (RV heap) %> (RV initGprs1) %> (RV flags') %> (RV pc)
        struct2 = Ctx.empty %> (RV heap) %> (RV initGprs2) %> (RV flags') %> (RV pc)
        initialRegMap1 = assignReg C.typeRepr struct1 emptyRegMap
        initialRegMap2 = assignReg C.typeRepr struct2 emptyRegMap
    -- Run cfg1
    rr1 <- MSS.run ctx emptyGlobals defaultErrorHandler machineState $ do
      callCFG cfg1 initialRegMap1
    -- Run cfg2
    rr2 <-trace "Second:" $ MSS.run ctx emptyGlobals defaultErrorHandler machineState $ do
      callCFG cfg2 initialRegMap2
    -- Compare
    case (rr1,rr2) of
      (FinishedExecution _ (TotalRes (view gpValue -> xs1)),
       FinishedExecution _ (TotalRes (view gpValue -> xs2))) -> do
        let heap1 = unRV $ xs1^._1
            heap2 = unRV $ xs2^._1
            gprs1 = unRV $ xs1^._2
            gprs2 = unRV $ xs2^._2
            flags1 = xs1^._3
            flags2 = xs2^._3
            flagPair = (unRV flags1, unRV flags2)
            pc1 = xs1^._4
            pc2 = xs2^._4
            -- flagIdxs = [0,2,4,6,7,8,9,10,11]
            true = I.truePred sym
        rels <- sequence [ I.arrayEq sym heap1 heap2
                         , mkGPRsCheck gprRel sym n4 n64 gprs1 gprs2
                         , foldM (compareWordMapIdx sym n5 n1 flagPair) true []
                         , mkRipRelation ripRel sym pc1 pc2 ]
        pred <- I.notPred sym =<< foldM (I.andPred sym) true rels
        -- heapUnEq <- I.notPred sym =<< 
        -- gprsUnRel  <- I.notPred sym =<< 
        -- flagsUnEq <- I.notPred sym =<<  -- [0,2,4,6,7]
        -- pcsUnRel <- I.notPred sym =<< 
        -- pred    <- I.orPred sym gprsUnRel =<< I.orPred sym flagsUnEq pcsUnRel
        -- let pred = flagsEq
        -- satisfied if there is some assignment of registers such that
        -- relations at the end don't hold.  We don't have forall on
        -- bitvectors, so we express it as an implicit exists with
        -- negation...
        solver_adapter_write_smt2 cvc4Adapter out pred
        putStrLn $ "Wrote to file " ++ show out
        return ()
      _ -> fail "Execution not finished"

  where
    initWordMap sym nrKey nrVal acc i = do
      idx <- I.bvLit sym nrKey i
      val <- I.freshConstant sym (C.BaseBVRepr nrVal)
      I.insertWordMap sym nrKey (C.BaseBVRepr nrVal) idx val acc

    compareWordMapIdx sym nrKey nrVal (wm1,wm2) acc i = do
      idx <- I.bvLit sym nrKey i
      val1Part <- I.lookupWordMap sym nrKey (C.BaseBVRepr nrVal) idx wm1
      val2Part <- I.lookupWordMap sym nrKey (C.BaseBVRepr nrVal) idx wm2
      val1 <- readPartExpr sym val1Part $ GenericSimError $ unwords ["Comparison at index", show i, "failed"]
      val2 <- readPartExpr sym val2Part $ GenericSimError $ unwords ["Comparison at index", show i, "failed"]
      I.andPred sym acc =<< I.bvEq sym val1 val2


    withSimpleBackend' :: HandleAllocator RealWorld -> (SimContext SimpleBackend -> IO a) -> IO a
    withSimpleBackend' halloc action = do
      sym <- newSimpleBackend MapF.empty
      cfg <- initialConfig 0 []
      matlabFns <- liftST $ newMatlabUtilityFunctions halloc
      let ctx = initSimContext sym cfg halloc stdout emptyHandleMap matlabFns M.empty []
      action ctx

    defaultErrorHandler = MSS.EH $ \simErr mssState -> error (show simErr)

-- given a relation of control points in two variants, expressed as a Map,and
-- the final values of %rip in the two variants, vreate an SMT term asserting
-- that the two final values are related
mkRipRelation :: I.IsExprBuilder sym
              => Map Word64 [Word64]
              -> sym
              -> RegValue' sym (C.BVType 64)
              -> RegValue' sym (C.BVType 64)
              -> IO (I.Pred sym)
mkRipRelation map sym reg1 reg2 = do
  in1 <- foldM mkInCase (I.falsePred sym) (M.keys map')
  match <- M.foldlWithKey mkMatchCase (return $ I.truePred sym) map'
  I.andPred sym in1 match
  where
  map' = M.insert 0 [0] map
  mkInCase rest entry = do
    lit <- I.bvLit sym (knownNat :: NatRepr 64) (fromIntegral entry)
    myEq <- I.bvEq sym lit $ unRV reg1
    I.orPred sym rest myEq
  mkMatchCase rest key vals = do
    restPred <- rest
    keyLit <- I.bvLit sym (knownNat :: NatRepr 64) (fromIntegral key)
    keyEq <- I.bvEq sym keyLit $ unRV reg1
    foldM (\restPred' val -> do
      valLit <- I.bvLit sym (knownNat :: NatRepr 64) (fromIntegral val)
      valEq <- I.bvEq sym valLit $ unRV reg2
      keyValImp <- I.impliesPred sym keyEq valEq
      I.andPred sym restPred' keyValImp) restPred vals

-- Constructs a pair of initial register states such that, if reg a in the left
-- variant is related to reg b in the right variant, reg a in the left WordMap
-- has the same fresh value as reg b in the right WordMap.
mkInitialGPRs :: (I.IsExprBuilder sym, I.IsSymInterface sym, 1 <= nKey, 1 <= nVal)
              => Map Integer Integer
              -> sym
              -> NatRepr nKey
              -> NatRepr nVal
              -> [Integer]
              -> IO (I.WordMap sym nKey (C.BaseBVType nVal), I.WordMap sym nKey (C.BaseBVType nVal))
mkInitialGPRs map sym nrKey nrVal range = do
  regs1Empty <- I.emptyWordMap sym nrKey (C.BaseBVRepr nrVal)
  (regs1, invMap) <- foldM (\(regs, invMap) entry -> do
    idx <- I.bvLit sym nrKey entry
    val <- I.freshConstant sym (C.BaseBVRepr nrVal)
    regs' <- I.insertWordMap sym nrKey (C.BaseBVRepr nrVal) idx val regs
    return (regs', case M.lookup entry map of
                    Just inv -> M.insert inv val invMap
                    Nothing -> invMap)) (regs1Empty, M.empty) range
  regs2Empty <- I.emptyWordMap sym nrKey (C.BaseBVRepr nrVal)
  regs2 <- foldM (\regs entry -> do
    idx <- I.bvLit sym nrKey entry
    val <- case M.lookup entry invMap of
            Just val -> return val
            Nothing -> I.freshConstant sym (C.BaseBVRepr nrVal)
    I.insertWordMap sym nrKey (C.BaseBVRepr nrVal) idx val regs) regs2Empty range
  return (regs1, regs2)

-- Given the final values of registers in after two executions, asserts that
-- they are equivalent according to the relation in map.
-- TODO: generalize this and integrate it with mkRipRel to handle cases where
-- the relation varies at different control points within a function.
mkGPRsCheck :: (I.IsExprBuilder sym, I.IsBoolSolver sym (I.SymExpr sym C.BaseBoolType), 1 <= nKey, 1 <= nVal)
            => Map Integer Integer
            -> sym
            -> NatRepr nKey
            -> NatRepr nVal
            -> I.WordMap sym nKey (C.BaseBVType nVal)
            -> I.WordMap sym nKey (C.BaseBVType nVal)
            -> IO (I.Pred sym)
mkGPRsCheck map sym nrKey nrVal regs1 regs2 =
  M.foldlWithKey (\pred idx1 idx2 -> do
    pred' <- pred
    idx1' <- I.bvLit sym nrKey idx1
    idx2' <- I.bvLit sym nrKey idx2
    val1Part <- I.lookupWordMap sym nrKey (C.BaseBVRepr nrVal) idx1' regs1
    val2Part <- I.lookupWordMap sym nrKey (C.BaseBVRepr nrVal) idx2' regs2
    val1 <- readPartExpr sym val1Part $ GenericSimError $ unwords ["Comparison at indexes", show idx1, "and", show idx2,"failed"]
    val2 <- readPartExpr sym val2Part $ GenericSimError $ unwords ["Comparison at indexes", show idx1, "and", show idx2,"failed"]
    I.andPred sym pred' =<< I.bvEq sym val1 val2
    ) (return $ I.truePred sym) map

-- testing code - finds and prints a single basic block
showSingleBlock :: Memory Word64 -> Word64 -> IO ()
showSingleBlock mem start =
  case runMemoryByteReader pf_x mem start $ extractBlock start S.empty of
    Left err -> putStrLn $ show err
    Right (Left err, _) -> putStrLn  $ "Could not disassemble instruction at 0x"
                                      ++ showHex err ""
    Right (Right (nexts, stmts), _) -> do
      putStrLn $ show nexts
      putStrLn $ show $ CS.ppStmts stmts

-- testing code - prints a CFG from mem, starting at entry
extractCFG :: Memory Word64 -> Word64 -> IO ()
extractCFG mem entry =
  case findBlocks mem entry of
    Left err -> putStrLn err
    Right cfg -> M.foldlWithKey
      (\ m addr (Block _ t) -> do m; putStrLn $ "0x" ++ showHex addr ": " ++ show t)
      (return ())
      cfg

------------------------------------------------------------------------------
-- An early cut at control flow graphing - made several assumptions that don't
-- seem to be correct (DWARF info seems to be more reliable than we expected,
-- but blocks may not be unique given some of the changes UCI has mentioned...)
-- We might just get rid of this.
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
      putStrLn "--- in rip_map format ---"
      print $ M.map (: []) matched

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
blockTermFuzzyMatches (Block _ Indirect) (Block _ Indirect) = True
blockTermFuzzyMatches _ _ = False
-- end of the early block-matching attempt
----------------------------------------------------------------------------

mkElfMem :: (ElfWidth w, Functor m, Monad m) => LoadStyle -> Elf w -> m (Memory w)
mkElfMem LoadBySection e = memoryForElfSections e
mkElfMem LoadBySegment e = memoryForElfSegments e

getMemAndEntry args path addrL = do
  e <- readStaticElf path
  mem <- mkElfMem (args^.loadStyle) e
  let a = case args^.addrL of
             0 -> elfEntry e
             a' -> a'
  return (mem, a)



-- Generates a RIP mapping of blocks in one CFG to another using breadcrumbs.
--
-- A breadcrumb is a pair of instruction addresses - one in each
-- variant - that each correspond to the same source location. Here we
-- assume that corresponding breadcrumb locations mean their
-- containing blocks also correspond. This function determines the
-- start of the blocks containing each breadcrumb, and uses those
-- (starts of) blocks to generate a RIP mapping between variants.
--
-- This won't always be correct, but it's a start while we decide our
-- general approch for modeling rip mappings and determine what we can
-- guarantee vs. what requires heuristics.
matchByBCs :: CFG -> CFG -> Map Word64 Word64 -> Map Word64 [Word64]
matchByBCs cfg1 cfg2 = M.foldWithKey insertParents M.empty
  where blockStart cfg addr = last $ takeWhile (<= addr) (sort $ M.keys cfg)
        insertParents v1 v2 = M.insert (blockStart cfg1 v1) [blockStart cfg2 v2]

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
    SymExec -> case args^.programPaths of
      [path1, path2] -> do
--        block1 <- uncurry extractFirstBlock =<< getMemAndEntry args path1 addr1
--        block2 <- uncurry extractFirstBlock =<< getMemAndEntry args path2 addr2
        (mem1, entry1) <- getMemAndEntry args path1 addr1
        (mem2, entry2) <- getMemAndEntry args path2 addr2
        let cfg1 = case findBlocks mem1 entry1 of
                     Left err -> error err
                     Right res -> res
        let cfg2 = case findBlocks mem2 entry2 of
                     Left err -> error err
                     Right res -> res

--        block1 <- uncurry extractFirstBlock =<< getMemAndEntry args path1 addr1
--        block2 <- uncurry extractFirstBlock =<< getMemAndEntry args path2 addr2
        ripMap <- case (args^.ripFile, args^.bcFile) of
                   ("", "") -> fail "You must specify either a breadcrumb or rip map file"
                   (rf, "") -> read <$> readFile rf
                   ("", bc) -> matchByBCs cfg1 cfg2 . read <$> readFile bc
                   (_, _) -> fail "You must specify EITHER a breadcrumb or rip map file, but not both"
        putStrLn $ "using ripMap = " ++ show ripMap
        gprMap <- fmap read $ readFile $ args^.gprsFile
        simulateCFGs entry1 cfg1 entry2 cfg2 ripMap gprMap
      _      -> fail "usage: filename1 filename2"
    FindBlocks -> do
      e <- case args^.programPaths of
        [] -> fail "A file name is required\n"
        _ : _ : _ -> fail "Exactly one file name for function cfg"
        [path] -> readStaticElf path
      mem <- mkElfMem (args^.loadStyle) e
      let a = case args^.addr1 of 0 -> elfEntry e
                                  a' -> a'
      extractCFG mem a
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
