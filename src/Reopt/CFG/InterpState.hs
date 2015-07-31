{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.CFG.InterpState
  ( AbsStateMap
  , lookupAbsBlock
  , FrontierReason(..)
  , BlockRegion(..)
  , lookupBlock
  , GlobalDataInfo(..)
    -- * The interpreter state
  , InterpState(..)
  , emptyInterpState
  , genState
  , blocks
  , functionEntries
  , reverseEdges
  , globalDataMap
  , frontier
  , function_frontier
  , absState
    -- ** InterpState utilities
  , getFunctionEntryPoint
  , inSameFunction
  , returnCount
  )  where

import Control.Lens
import Control.Monad (join)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Parameterized.Some
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word
import GHC.TypeLits
import Numeric (showHex)

import Reopt.Analysis.AbsState
import Reopt.CFG.Implementation (GlobalGenState, emptyGlobalGenState)
import Reopt.CFG.Representation
import Reopt.Object.Memory

------------------------------------------------------------------------
-- AbsStateMap

-- | Maps each code address to a set of abstract states
type AbsStateMap = Map CodeAddr AbsBlockState

lookupAbsBlock :: CodeAddr -> AbsStateMap -> AbsBlockState
lookupAbsBlock addr s = fromMaybe (error msg) (Map.lookup addr s)
  where msg = "Could not find block " ++ showHex addr "."

------------------------------------------------------------------------
-- BlockRegion

-- | The blocks contained in a single contiguous region of instructions.
data BlockRegion = BlockRegion { brEnd :: !CodeAddr
                                 -- | Map from labelIndex to associated block.
                               , brBlocks :: !(Map Word64 Block)
                               }

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock :: Map CodeAddr (Maybe BlockRegion) -> BlockLabel -> Maybe Block
lookupBlock m lbl = do
  br <- join $ Map.lookup (labelAddr lbl) m
  Map.lookup (labelIndex lbl) (brBlocks br)

------------------------------------------------------------------------
-- FrontierReason

-- | Data describing why an address was added to the frontier.
data FrontierReason
     -- | Exploring because a pointer to this address was found stored in
     -- memory.
   = InInitialData
     -- | Exploring because the given block writes it to memory.
   | InWrite       !BlockLabel
     -- | Exploring because the given block stores address as a
     -- return address.
   | ReturnAddress !BlockLabel
     -- | Exploring because the given block jumps here.
   | NextIP !BlockLabel
     -- | Added as the initial start state.
   | StartAddr
     -- | Added because a previous block split this block.
   | BlockSplit
  deriving (Show)

------------------------------------------------------------------------
-- GlobalDataInfo

data GlobalDataInfo
     -- | A jump table that appears to end just before the given address.
   = JumpTable !(Maybe CodeAddr)
     -- | Some value that appears in the program text.
   | ReferencedValue

instance Show GlobalDataInfo where
  show (JumpTable Nothing) = "unbound jump table"
  show (JumpTable (Just w)) = "jump table end " ++ showHex w ""
  show ReferencedValue = "global addr"

------------------------------------------------------------------------
-- ParsedTermStmt

-- | This term statement is used to describe higher level expressions
-- of how block ending with a a FetchAndExecute statement should be
-- interpreted.
data ParsedTermStmt
   = ParsedCall !(BVValue 64)
                -- ^ Function to call
                !([BVValue 64])
                -- ^ Integer arguments to function
                !([BVValue 128])
                -- ^ Floating point values passed via XMM registers
                !([Some Value])
                -- ^ Values passed in the stack in order.
                -- i.e.,The first argument is at the top of the stack
                -- When calling these should be pushed to the stack in
                -- reverse order.
                !Word64 -- ^ Return location.
     -- | A jump within a block
   | ParsedJump !Word64
     -- | A lookup table that branches to the given locations.
   | forall n . (1 <= n) =>
       ParsedLookupTable (BVValue n) (V.Vector Word64)
     -- | A tail cthat branches to the given locations.
   | ParsedTailCall !(BVValue 64)
                    -- ^ Function to call
                    !([BVValue 64])
                    -- ^ Integer arguments to function
                    !([BVValue 128])
                    -- ^ Floating point values passed via XMM registers
                    !([Some Value])
                    -- ^ Values passed in the stack in order.
                    -- i.e.,The first argument is at the top of the stack
                    -- When calling these should be pushed to the stack in
                    -- reverse order.

   | ParsedReturn !(BVValue 64)
                  -- ^ The integer return value (in RAX)
                  !(BVValue 128)





------------------------------------------------------------------------
-- InterpState

-- | The state of the interpreter
data InterpState
   = InterpState { -- | The initial memory when disassembly started.
                   memory   :: !(Memory Word64)
                   -- | State used for generating blocks.
                 , _genState :: !GlobalGenState
                   -- | Intervals maps code addresses to blocks at address
                   -- or nothing if disassembly failed.
                 , _blocks   :: !(Map CodeAddr (Maybe BlockRegion))
                   -- | Maps addresses that are marked as the start of a function
                 , _functionEntries :: !(Set CodeAddr)
                   -- | Maps each code address to the list of predecessors that
                   -- affected its abstract state.
                 , _reverseEdges :: !(Map CodeAddr (Set CodeAddr))
                   -- | Maps each address that appears to be global data to information
                   -- inferred about it.
                 , _globalDataMap :: !(Map CodeAddr GlobalDataInfo)
                   -- | Information about the next state for blocks that end with
                   -- @FetchAndExecute@ statements.
                 , _parsedTermStmts :: !(Map CodeAddr ParsedTermStmt)
                   -- | Set of addresses to explore next.
                   -- This is a map so that we can associate a reason why a code address
                   -- was added to the frontier.
                 , _frontier :: !(Map CodeAddr FrontierReason)
                   -- | Set of functions to explore next.
                 , _function_frontier :: !(Set CodeAddr)
                   -- | Map from code addresses to the abstract state at the start of
                   -- the block.
                 , _absState :: !AbsStateMap
                 , _returnCount :: !Int
                 }

-- | Empty interpreter state.
emptyInterpState :: Memory Word64 -> InterpState
emptyInterpState mem = InterpState
      { memory        = mem
      , _genState     = emptyGlobalGenState
      , _blocks       = Map.empty
      , _functionEntries = Set.empty
      , _reverseEdges    = Map.empty
      , _globalDataMap   = Map.empty
      , _parsedTermStmts = Map.empty
      , _frontier        = Map.empty
      , _function_frontier = Set.empty
      , _absState        = Map.empty
      , _returnCount     = 0
      }

genState :: Simple Lens InterpState GlobalGenState
genState = lens _genState (\s v -> s { _genState = v })

blocks :: Simple Lens InterpState (Map CodeAddr (Maybe BlockRegion))
blocks = lens _blocks (\s v -> s { _blocks = v })

-- | Addresses that start each function.
functionEntries :: Simple Lens InterpState (Set CodeAddr)
functionEntries = lens _functionEntries (\s v -> s { _functionEntries = v })

reverseEdges :: Simple Lens InterpState (Map CodeAddr (Set CodeAddr))
reverseEdges = lens _reverseEdges (\s v -> s { _reverseEdges = v })

-- | Map each jump table start to the address just after the end.
globalDataMap :: Simple Lens InterpState (Map CodeAddr GlobalDataInfo)
globalDataMap = lens _globalDataMap (\s v -> s { _globalDataMap = v })

-- | Information about the next state for blocks that end with
-- @FetchAndExecute@ statements.
parsedTermStmts :: Simple Lens InterpState (Map CodeAddr ParsedTermStmt)
parsedTermStmts = lens _parsedTermStmts (\s v -> s { _parsedTermStmts = v })

frontier :: Simple Lens InterpState (Map CodeAddr FrontierReason)
frontier = lens _frontier (\s v -> s { _frontier = v })

function_frontier :: Simple Lens InterpState (Set CodeAddr)
function_frontier = lens _function_frontier (\s v -> s { _function_frontier = v })

absState :: Simple Lens InterpState AbsStateMap
absState = lens _absState (\s v -> s { _absState = v })

returnCount :: Simple Lens InterpState Int
returnCount = lens _returnCount (\s v -> s { _returnCount = v })

------------------------------------------------------------------------
-- InterpState utilities

-- | Returns the guess on the entry point of the given function.
getFunctionEntryPoint :: CodeAddr -> InterpState -> CodeAddr
getFunctionEntryPoint addr s = do
  case Set.lookupLE addr (s^.functionEntries) of
    Just a -> a
    Nothing -> error $ "Could not find address of " ++ showHex addr "."

inSameFunction :: CodeAddr -> CodeAddr -> InterpState -> Bool
inSameFunction x y s =
  getFunctionEntryPoint x s == getFunctionEntryPoint y s