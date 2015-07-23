{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Elf
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
import System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Debug.Trace

import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF

-- import Reopt.Semantics.BitVector
import           Numeric (readHex)

import           Paths_reopt (version)
import           Data.Type.Equality as Equality

import           Flexdis86 (InstructionInstance(..), ppInstruction, ByteReader(..), defaultX64Disassembler, disassembleInstruction)
import           Lang.Crucible.Config (initialConfig)
import qualified Lang.Crucible.Core as C
import           Lang.Crucible.MATLAB.UtilityFunctions (newMatlabUtilityFunctions)
import           Lang.Crucible.Simulator.CallFns
import           Lang.Crucible.Simulator.ExecutionTree
import           Lang.Crucible.Simulator.MSSim as MSS
import           Lang.Crucible.Simulator.RegMap
import           Lang.Crucible.Solver.Adapter
import qualified Lang.Crucible.Solver.Interface as I
import           Lang.Crucible.Solver.SimpleBackend
import           Lang.Crucible.Solver.SimpleBackend.CVC4
import           Lang.Crucible.Utils.MonadST
import           Reopt
import           Reopt.Analysis.AbsState
import           Reopt.CFG.CFGDiscovery
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
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
  halloc <- liftST newHandleAllocator
  (C.AnyCFG cfg, retType) <- liftST $ gimmeCFG halloc
  case testEquality (C.cfgArgTypes cfg) argTypes of
    Nothing -> error "main should take no arguments"
    Just Refl -> case C.cfgReturnType cfg of
      retType@(C.BVRepr w) | Just LeqProof <- isPosNat w -> do
        withSimpleBackend' halloc $ \ctx -> do
          let sym = ctx^.ctxSymInterface
          rr <- MSS.run ctx defaultErrorHandler retType $ do
            arg1 <- liftIO $ I.freshConstant sym (I.BVVarType (knownNat))
            -- arg1 <- liftIO $ I.bvLit sym knownNat 7
            let rMap = assignReg C.typeRepr arg1 emptyRegMap
            callCFG cfg rMap
          liftIO $ case rr of
            FinishedExecution _ (TotalRes xs) -> do
              putStrLn $ "Execution finished! " ++ show xs
              pred <- I.bvIsNonzero sym xs
              solver_adapter_write_smt2 cvc4Adapter stdout pred
              -- do inps <- readIORef (SAW.saw_inputs sym)
              --    xs' <- traverse (traverse (SAW.closeOverValue sym inps)) xs
              --    SAW.printSAWValues xs'
            _ -> putStrLn $ "Execution not finished? "
      _ -> fail "main should return an integer value"

withSimpleBackend' :: HandleAllocator RealWorld -> (SimContext SimpleBackend -> IO a) -> IO a
withSimpleBackend' halloc action =
  withSimpleBackend $ \sym -> do
    cfg <- initialConfig 0 []
    matlabFns <- liftST $ newMatlabUtilityFunctions halloc
    let ctx = initSimContext sym cfg halloc stdout emptyHandleMap matlabFns Map.empty []
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
