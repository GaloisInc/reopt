module Reopt.CFG.Generator where

import Control.Lens
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

import Reopt.CFG (CFG)
import qualified Reopt.CFG as CFG
import Reopt.Memory

-- | An abstract representation of the system state for CFG
-- construction purposes.
data SystemState = SS { _curPC :: Expr Word64 }

initialSystemState :: Word64 -> SystemState
initialSystemState w = SS { _curPC = fromIntegral w }

curPC :: Simple Lens SystemState (Expr Word64)
curPC = lens _curPC (\s v -> s {_curPC = v})

incPC :: Word64 -> SystemState -> SystemState
incPC w s = s & curPC %~ (+ fromIntegral w)

data ExprF f tp where
  CWord64 :: Word64 -> Expr Word64

data Expr tp = E { unE :: ExprF Reg tp }

instance Num (Expr Word64) where


data CFGState = CFGState { initialMemory :: !(Memory Word64)
                         , _pastBlocks :: ![Block Word64]
                         , _curStmts :: ![MicroOp addr]
                         , _system :: !SystemState
                         , _knownEntryPoints :: !(Set Word64)
                         , _nextEntryPoints :: !([Word64])
                         }

-- | Blocks generated so far during CFG construction.
pastBlocks :: Simple Lens CFGState [Block Word64]
pastBlocks = lens _pastBlocks (\s v -> s { _pastBlocks = v })

-- | Blocks generated so far during CFG construction.
curStmts :: Simple Lens CFGState [Block Word64]
curStmts = lens _pastBlocks (\s v -> s { _pastBlocks = v })

-- | Current system state for generating SCFG
system :: Simple Lens CFGState SystemState
system = lens _system (\s v -> s { _system = v })

-- | Set of address identified to be potential basic block locations.
knownEntryPoints :: Simple Lens CFGState (Set Word64)
knownEntryPoints = lens _knownEntryPoints (\s v -> s { _knownEntryPoints = v })

-- | List of addresses to (re)-translate.
nextEntryPoints :: Simple Lens CFGState [Word64]
nextEntryPoints = lens _knownEntryPoints (\s v -> s { _knownEntryPoints = v })

finalCFG :: CFGState -> CFG
finalCFG = undefined


type Generator = StateT CFGState IO

-- | Record the given address as an entry point.
recordEntryPoint :: Word64 -> Generator ()
recordEntryPoint a = do
  s <- use knownEntryPoints
  when (not (Set.member a s)) $ do
    knownEntryPoints .= Set.insert a s
    nextEntryPoints %= (a:)

initial_state :: Memory Word64 -> [Word64] -> CFGState
initial_state m l =
  let s = Set.fromList l
  CFGState { initialMemory = m
           , _system = initialSystemState 0
           , _knownEntryPoints = s
           , _nextEntryPoints = Set.toList s
           }

explore_all_entry_points :: Generator ()
explore_all_entry_points = do
  l <- use nextEntryPoints
  case l of
    [] -> return ()
    h:r -> do
      nextEntryPoints .= r
      explore_entry_point h
      explore_all_entry_points

-- | Explore a single entry point.
explore_entry_point :: Word64 -> Generator ()
explore_entry_point base = do
  mem <- gets initialMemory
  case runMemoryByteReader pf_x mem base disassembleInstruction of
    Left e -> fail $ "Error when decoding instruction at " ++ base ++ "\n"
                     ++ show e
    Right (i,w) -> do
      -- Update the PC to point next instruction.
      system . curPC .= fromIntegral w
      -- Run instruction
      exec_instruction i

-- | 
exploreBlock :: Word64 -> Generator ()

exec_instruction :: InstructionInstance -> Generator ()
exec_instruction i = do
  

  disassembleInstruction 

undefined base

-- | Generate a control-flow graph from a list of entry points.
generate :: Memory Word64 -- ^ Memory
         -> [Word64] -- ^ List of entry points.
         -> IO (CFG Word64)
generate m entry_points = do
  let s0 = initial_state m entry_points
  finalCFG <$> execStateT explore_all_entry_points s0


data BlockState 

