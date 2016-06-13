{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is a Crucible symbolic evaluation study, meant to be a minimal example.
--
-- It was a stepping stone on the way to @radss_compare@, and can be
-- deleted when it's no longer considered useful. However, there may
-- not be any other minimal Crucible examples, so it might be best to
-- incorporate it into Crucible as documentation.
module Main (main) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ElfEdit
import           Data.Foldable (traverse_)
import           Data.Int
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Data.Version
import           GHC.TypeLits
import           Numeric (showHex)
import           System.Console.CmdArgs.Explicit as CmdArgs
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Debug.Trace

import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some

-- import Reopt.Semantics.BitVector
import           Numeric (readHex)

import           Paths_reopt (version)
import           Data.Type.Equality as Equality

import           Flexdis86 (InstructionInstance(..), ppInstruction, ByteReader(..), disassembleInstruction)
import           Lang.Crucible.Config (initialConfig)
import qualified Lang.Crucible.Core as C
-- import           Lang.Crucible.MATLAB.UtilityFunctions (newMatlabUtilityFunctions)
import           Lang.Crucible.Simulator.CallFns
import           Lang.Crucible.Simulator.ExecutionTree
import           Lang.Crucible.Simulator.MSSim as MSS
import           Lang.Crucible.Simulator.RegMap
import           Lang.Crucible.Solver.Adapter
import qualified Lang.Crucible.Solver.Interface as I
import           Lang.Crucible.Solver.SatResult
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.Solver.SimpleBackend.CVC4
import           Lang.Crucible.Solver.SimpleBuilder
import           Lang.Crucible.Solver.Symbol
import           Lang.Crucible.Utils.MonadST
import           Reopt
import           Reopt.Analysis.AbsState
import           Reopt.CFG.CFGDiscovery
import           Data.Macaw.CFG
import qualified Reopt.Machine.StateNames as N
import           Data.Macaw.Types
import           Reopt.Machine.X86State
--import           Reopt.Semantics.ConcreteState
import           Reopt.Object.Memory
import           Reopt.Object.Loader
import           Reopt.Semantics.DeadRegisterElimination
import           Reopt.Semantics.FlexdisMatcher
import           Reopt.Semantics.Monad (Type(..), bvLit)
import qualified Reopt.Semantics.Monad as SM

import           Reopt.Symbolic.Semantics

------------------------------------------------------------------------
-- Args

-- | Action to perform when running
data Action
   = Test            -- * Execute and simulate in parallel, printing errors
   | Instr
   | ShowHelp        -- ^ Print out help message
   | ShowVersion     -- ^ Print out version

-- | Command line arguments.
data Args = Args { _reoptAction :: !Action
                 , _programPath :: !FilePath
                 , _loadStyle   :: !LoadStyle
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

-- | Whether to load file by segment or sections.
loadStyle :: Simple Lens Args LoadStyle
loadStyle = lens _loadStyle (\s v -> s { _loadStyle = v })

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _reoptAction = Test
                   , _programPath = ""
                   , _loadStyle = LoadBySection
                   }

------------------------------------------------------------------------
-- Argument processing

arguments :: Mode Args
arguments = mode "reopt_test" defaultArgs help filenameArg flags
  where help = reoptVersion ++ "\n" ++ copyrightNotice
        flags = [ instrFlag
                , testFlag
                , flagHelpSimple (reoptAction .~ ShowHelp)
                , flagVersion (reoptAction .~ ShowVersion)
                ]

testFlag :: CmdArgs.Flag Args
testFlag = flagNone [ "test", "t"] upd help
  where upd = reoptAction .~ Test
        help = "Test concrete semantics by executing in parallel with a binary"

instrFlag :: CmdArgs.Flag Args
instrFlag = flagNone [ "instructions", "i"] upd help
  where upd = reoptAction .~ Instr
        help = "Print disassembly of executed instructions in a binary"

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

mkElfMem :: (ElfWidth w, Functor m, Monad m) => LoadStyle -> Elf w -> m (Memory w)
mkElfMem LoadBySection e = memoryForElfSections e
mkElfMem LoadBySegment e = memoryForElfSegments e


test :: Args -> IO ()
test args = do
  out <- openFile "/tmp/sat" WriteMode
  halloc <- liftST newHandleAllocator
  C.AnyCFG cfg1 <- liftST $ gimmeCFG halloc gen1
  C.AnyCFG cfg2 <- liftST $ gimmeCFG halloc gen2

  case ( testEquality (C.cfgArgTypes cfg1) argTypes
       , testEquality (C.cfgReturnType cfg1) retType
       , testEquality (C.cfgArgTypes cfg2) argTypes
       , testEquality (C.cfgReturnType cfg2) retType
       ) of
    (Just Refl, Just Refl, Just Refl, Just Refl) ->
      withSimpleBackend' halloc $ \ctx -> do
        let sym = ctx^.ctxSymInterface
        arg <- I.freshConstant sym emptySymbol (C.BaseBVRepr knownNat)
        -- Run cfg1
        rr1 <- MSS.run ctx emptyGlobals defaultErrorHandler retType $ do
          let rMap = assignReg C.knownRepr arg emptyRegMap
          regValue <$> callCFG cfg1 rMap
        -- Run cfg2
        rr2 <- MSS.run ctx emptyGlobals defaultErrorHandler retType $ do
          let rMap = assignReg C.knownRepr arg emptyRegMap
          regValue <$> callCFG cfg2 rMap
        -- Compare
        case (rr1,rr2) of
          (FinishedExecution _ (TotalRes (view gpValue -> RegEntry _ xs1)),
           FinishedExecution _ (TotalRes (view gpValue -> RegEntry _ xs2))) -> do
            putStrLn $ unwords ["Execution finished!:", show (xs1,xs2)]
            pred <- I.notPred sym =<< I.bvEq sym xs1 xs2
            solver_adapter_write_smt2 cvc4Adapter sym out pred
            putStrLn $ "Wrote to file " ++ show out
          _ -> fail "Execution not finished"


withSimpleBackend' :: HandleAllocator RealWorld
                   -> (forall t . SimContext (SimpleBackend t) -> IO a)
                   -> IO a
withSimpleBackend' halloc action = do
  withIONonceGenerator $ \gen -> do
    sym <- newSimpleBackend gen
    cfg <- initialConfig 0 []
    -- matlabFns <- liftST $ newMatlabUtilityFunctions halloc
    let ctx = initSimContext sym MapF.empty cfg halloc stdout emptyHandleMap Map.empty []
    action ctx

defaultErrorHandler = MSS.EH $ \_ _ -> error "Error which I don't know how to handle!"


main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    Test -> test args
    Instr -> putStrLn "Instr unsupported" >> exitFailure
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
