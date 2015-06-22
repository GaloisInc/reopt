{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
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

import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF

import           Paths_reopt (version)
import           Data.Type.Equality as Equality

import           Flexdis86 (InstructionInstance(..), ppInstruction)
import           Reopt
import           Reopt.Analysis.AbsState
import           Reopt.CFG.CFGDiscovery
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Memory
import           Reopt.Object.Loader
import           Reopt.Semantics.DeadRegisterElimination
import           Reopt.Semantics.Monad (Type(..))
import System.Posix.Waitpid as W
import System.Posix.Types
import System.Posix.Process
import System.Posix.Signals
import System.Linux.Ptrace
import System.Linux.Ptrace.Syscall
import System.Linux.Ptrace.Types
import System.Linux.Ptrace.X86_64Regs

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
  e <- readStaticElf (args^.programPath)
  let Identity mem = mkElfMem (args^.loadStyle) e
  child <- traceFile $ args^.programPath
  runStateT (testInner (printRegsAndInstr mem) child) ()
  return ()

printExecutedInstructions :: Args -> IO ()
printExecutedInstructions args = do
  e <- readStaticElf (args^.programPath)
  let Identity mem = mkElfMem (args^.loadStyle) e
  child <- traceFile $ args^.programPath
  runStateT (testInner (printInstr mem) child) ()
  return ()

traceFile :: FilePath -> IO CPid
traceFile path = do
  child <- forkProcess $ traceChild path
  waitpid child []
  return child

traceChild :: FilePath -> IO ()
traceChild file = do
  ptrace_traceme
  executeFile file False [] Nothing
  trace "EXEC FAILED" $ fail "EXEC FAILED"

testInner :: (CPid -> StateT s IO ()) -> CPid -> StateT s IO ()
testInner act pid = do
  act pid
  lift $ ptrace_singlestep pid Nothing
  (spid, status) <- lift $ waitForRes pid
  if spid == pid
    then case status of W.Stopped _ -> testInner act pid
                        Signaled _ -> testInner act pid
                        Continued -> testInner act pid
                        W.Exited _ -> return ()
    else fail "Wrong pid from waitpid!"

printInstr :: Memory Word64 -> CPid -> StateT s IO ()
printInstr mem pid = do
  regs <- lift $ ptrace_getregs pid
  case regs 
    of X86 _ -> fail "X86Regs! only 64 bit is handled"
       X86_64 regs64 -> do
         let rip_val = rip regs64
         case readInstruction mem rip_val
           of Left err -> lift $ putStrLn $ "Couldn't disassemble instruction " ++ show err
              Right (ii, nextAddr) -> lift $ putStrLn $ show $ ppInstruction nextAddr ii

printRegsAndInstr :: Memory Word64 -> CPid -> StateT s IO ()
printRegsAndInstr mem pid = do
  regs <- lift $ ptrace_getregs pid
  case regs 
    of X86 _ -> fail "X86Regs! only 64 bit is handled"
       X86_64 regs64 -> do
         lift $ putStrLn $ show regs64
         let rip_val = rip regs64
         case readInstruction mem rip_val
           of Left err -> lift $ putStrLn "Couldn't find instruction at address "
              Right (ii, nextAddr) -> lift $ putStrLn $ show $ ppInstruction nextAddr ii

waitForRes :: CPid -> IO (CPid, Status)
waitForRes pid = do
  res <- waitpid pid []
  case res of Just x -> return x
              Nothing -> waitForRes pid
  
  

main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.reoptAction of
    Test -> test args
    Instr -> printExecutedInstructions args
    ShowHelp ->
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion ->
      putStrLn (modeHelp arguments)
