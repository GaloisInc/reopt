{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString as B
import           Data.Elf
import           Data.Foldable (traverse_)
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Version
import           Data.Word
import           GHC.TypeLits
import           Numeric (showHex, readHex)
import           Reopt.Analysis.AbsState
import           System.Console.CmdArgs.Explicit
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Text.Read (readMaybe)

import           Data.Parameterized.Context ((%>))
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some

import           Paths_reopt (version)
import           Data.Type.Equality as Equality

import           Debug.Trace

import           Flexdis86 (InstructionInstance(..))

import           Lang.Crucible.Config (initialConfig)
import qualified Lang.Crucible.Core as C
import           Lang.Crucible.ExtractSubgraph
-- import           Lang.Crucible.MATLAB.Intrinsics (newMatlabUtilityFunctions)
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
import           Lang.Crucible.Solver.SimpleBuilder
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
data Action = SymExec -- ^ Symbolically execute all blocks in the file.
              | ShowHelp
              | ShowVersion
                
-- | Command line arguments.
data Args = Args { _reoptAction  :: !Action
                 , _programPath  :: !FilePath
                 , _entryPoint   :: !(Maybe Word64)
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
programPath :: Simple Lens Args FilePath
programPath = lens _programPath (\s v -> s { _programPath = v })

-- | Optional entry point for the binary (the actual entry point is
-- used if none is given)
entryPoint :: Simple Lens Args (Maybe Word64)
entryPoint = lens _entryPoint (\s v -> s { _entryPoint = v })

-- | Whether to load file by segment or sections.
loadStyle :: Simple Lens Args LoadStyle
loadStyle = lens _loadStyle (\s v -> s { _loadStyle = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = SymExec
                   , _programPath = ""
                   , _entryPoint  = Nothing
                   , _loadStyle = LoadBySection
                   }

------------------------------------------------------------------------
-- Argument processing

symExecFlag :: Flag Args
symExecFlag = flagNone [ "sym-exec", "s" ] upd help
  where upd  = reoptAction .~ SymExec
        help = "Symbolically execute a block."

segmentFlag :: Flag Args
segmentFlag = flagNone [ "load-segments" ] upd help
  where upd  = loadStyle .~ LoadBySegment
        help = "Load the Elf file using segment information."

sectionFlag :: Flag Args
sectionFlag = flagNone [ "load-sections" ] upd help
  where upd  = loadStyle .~ LoadBySection
        help = "Load the Elf file using section information (default)."

entryPointFlag :: Flag Args
entryPointFlag = flagReq [ "entry", "e" ] upd "Entry point" help
  where
    upd :: String -> Args -> Either String Args 
    upd s old = case readMaybe s of
          Just addr -> Right $ (entryPoint .~ Just addr) old
          _         -> Left "Could not parse entry point"
    help = "Address to start from (default is entry point)."

arguments :: Mode Args
arguments = mode "bin_to_smt" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ segmentFlag
                , sectionFlag
                , symExecFlag
                , entryPointFlag
                , flagHelpSimple (reoptAction .~ ShowHelp)
                , flagVersion (reoptAction .~ ShowVersion)
                ]

reoptVersion :: String
reoptVersion = "Binary to SMT (bin_to_smt) " ++ versionString ++ ", Dec 2015."
  where [h,l,r] = versionBranch version
        versionString = show h ++ "." ++ show l ++ "." ++ show r

copyrightNotice :: String
copyrightNotice = "Copyright 2015 Galois, Inc. All rights reserved."

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

simulateCFG :: Word64 -> Map Word64 Block -> IO ()
simulateCFG entry reifcfg = do
  halloc <- liftST newHandleAllocator
  let simOne (k, b) = do
        cfg <- liftST $ translateSingleBlock halloc b
        simulate (showHex k "") cfg halloc
            
  mapM_ simOne (M.assocs reifcfg)

simulate :: String
         -> C.SomeCFG MachineStateCtx MachineState
         -> HandleAllocator RealWorld
         -> IO ()
simulate name (C.SomeCFG cfg) halloc = do
  out <- openFile ("/tmp/sat_trans_" ++ name) WriteMode
  withSimpleBackend' halloc $ \ctx -> do
    let sym = ctx^.ctxSymInterface
    
    initGprs0  <- I.emptyWordMap sym n4 (C.BaseBVRepr n64)
    initGprs   <- foldM (initWordMap sym n4 n64) initGprs0 [0..15]
    dummyGprs  <- foldM (initWordMap sym n4 n64) initGprs0 [0..15]
    
    flags      <- I.emptyWordMap sym n5 (C.BaseBVRepr n1)
    flags'     <- foldM (initWordMap sym n5 n1) flags [0..31]
    dummyFlags <- foldM (initWordMap sym n5 n1) flags [0..31]
    
    pc     <- I.freshConstant sym "pc" (C.BaseBVRepr n64)
    dummyPC <- I.freshConstant sym "" (C.BaseBVRepr n64)
    
    heap      <- I.freshConstant sym "heap" (C.BaseArrayRepr C.indexTypeRepr C.baseTypeRepr)
    dummyHeap <- I.freshConstant sym "" (C.BaseArrayRepr C.indexTypeRepr C.baseTypeRepr)
    
    let struct        = Ctx.empty %> (RV heap) %> (RV initGprs) %> (RV flags') %> (RV pc)
        initialRegMap = assignReg C.typeRepr struct emptyRegMap
    -- Run cfg1
    rr <- MSS.run ctx emptyGlobals defaultErrorHandler machineState $ callCFG cfg initialRegMap
    case rr of
      FinishedExecution _ (TotalRes (view gpValue -> xs)) -> do
        let heap   = unRV $ xs ^. _1
            gprs   = unRV $ xs ^. _2
            flags  = unRV $ xs ^. _3
            pc     = unRV $ xs ^. _4
            true = I.truePred sym

        -- Force all the output elements to be used so we don't simplify them away.
        
        rels <- sequence [ I.arrayEq sym heap dummyHeap
                         , foldM (compareWordMapIdx sym n4 n64 (gprs, dummyGprs)) true [0..15]
                         , foldM (compareWordMapIdx sym n5 n1 (flags, dummyFlags)) true [0..31]
                         , I.bvEq sym pc dummyPC
                         ]
        pred <- foldM (I.andPred sym) true rels
        -- satisfied if there is some assignment of registers such that
        -- relations at the end don't hold.  We don't have forall on
        -- bitvectors, so we express it as an implicit exists with
        -- negation...
        solver_adapter_write_smt2 cvc4Adapter sym out emptySymbolVarBimap pred
        putStrLn $ "Wrote to file " ++ show out
        return ()
  where
    initWordMap sym nrKey nrVal acc i = do
      idx <- I.bvLit sym nrKey i
      val <- I.freshConstant sym "" (C.BaseBVRepr nrVal)
      I.insertWordMap sym nrKey (C.BaseBVRepr nrVal) idx val acc
      
    compareWordMapIdx sym nrKey nrVal (wm1,wm2) acc i = do
      idx <- I.bvLit sym nrKey i
      val1Part <- I.lookupWordMap sym nrKey (C.BaseBVRepr nrVal) idx wm1
      val2Part <- I.lookupWordMap sym nrKey (C.BaseBVRepr nrVal) idx wm2
      val1 <- readPartExpr sym val1Part $ GenericSimError $ unwords ["Comparison at index", show i, "failed"]
      val2 <- readPartExpr sym val2Part $ GenericSimError $ unwords ["Comparison at index", show i, "failed"]
      I.andPred sym acc =<< I.bvEq sym val1 val2

    withSimpleBackend' :: HandleAllocator RealWorld
                       -> (forall t . SimContext (SimpleBackend t) -> IO a)
                       -> IO a
    withSimpleBackend' halloc action = do
      withIONonceGenerator $ \gen -> do
        sym <- newSimpleBackend gen MapF.empty
        cfg <- initialConfig 0 []
        -- matlabFns <- liftST $ newMatlabUtilityFunctions halloc
        let ctx = initSimContext sym cfg halloc stdout emptyHandleMap M.empty []
        action ctx

    defaultErrorHandler = MSS.EH $ \simErr mssState -> error (show simErr)

-- Given the final values of registers in after two executions, asserts that
-- they are equivalent according to the relation in map.
-- TODO: generalize this and integrate it with mkRipRel to handle cases where
-- the relation varies at different control points within a function.
mkGPRsCheck :: (I.IsExprBuilder sym, I.IsBoolSolver sym, 1 <= nKey, 1 <= nVal)
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

----------------------------------------------------------------------------

mkElfMem :: (ElfWidth w, Functor m, Monad m) => LoadStyle -> Elf w -> m (Memory w)
mkElfMem LoadBySection e = memoryForElfSections e
mkElfMem LoadBySegment e = memoryForElfSegments e

getMemAndEntry args = do
  e <- readStaticElf (args ^. programPath)
  mem <- mkElfMem (args^.loadStyle) e
  let a = case (args ^. entryPoint) of
             Nothing -> elfEntry e
             Just v  -> v
  return (mem, a)

main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    SymExec -> do 
        (mem, entry) <- getMemAndEntry args
        let cfg = case findBlocks mem entry of
                     Left err -> error err
                     Right res -> res
        simulateCFG entry cfg
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
