module Reopt.CFG.InterpState
  ( AbsState
  , lookupAbsBlock
  , FrontierReason(..)
  , BlockRegion(..)
  , lookupBlock
  , GlobalDataInfo(..)
    -- * The interpreter state
  , InterpState(..)
  , emptyInterpState
  , genState
  , blockStartAddrs
  , blocks
  , failedAddrs
  , functionEntries
  , reverseEdges
  , globalDataMap
  , frontier
  , absState
    -- ** InterpState utilities
  , getFunctionEntryPoint
  , inSameFunction
  )  where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Numeric (showHex)

import Reopt.Analysis.AbsState
import Reopt.CFG.Implementation (GlobalGenState, emptyGlobalGenState)
import Reopt.CFG.Representation
import Reopt.Object.Memory

------------------------------------------------------------------------
-- AbsState

-- | Maps each code address to a set of abstract states
type AbsState = Map CodeAddr AbsBlockState

lookupAbsBlock :: CodeAddr -> AbsState -> AbsBlockState
lookupAbsBlock addr s = fromMaybe (error msg) (Map.lookup addr s)
  where msg = "Could not find block " ++ show addr

------------------------------------------------------------------------
-- BlockRegion

-- | The blocks contained in a single contiguous region of instructions.
data BlockRegion = BlockRegion { brEnd :: !CodeAddr
                                 -- | Map from labelIndex to associated block.
                               , brBlocks :: !(Map Word64 Block)
                               }

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock :: Map CodeAddr BlockRegion -> BlockLabel -> Maybe Block
lookupBlock m lbl = do
  br <-  Map.lookup (labelAddr lbl) m
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
-- InterpState

-- | The state of the interpreter
data InterpState
   = InterpState { -- | The initial memory when disassembly started.
                   memory   :: !(Memory Word64)
                 , _genState :: !GlobalGenState
                   -- | Addresses that are known to start block locations.
                   -- Should be a superset of function entries and fialedAddrs
                 , _blockStartAddrs :: !(Set CodeAddr)
                   -- | Intervals maps code addresses to blocks at address.
                 , _blocks   :: !(Map CodeAddr BlockRegion)
                   -- | Set of code adddresses that could not be interpreted.
                 , _failedAddrs  :: !(Set CodeAddr)
                   -- | Maps addresses that are marked as the start of a function
                 , _functionEntries :: !(Set CodeAddr)
                   -- | Maps each code address to the list of predecessors that
                   -- affected its abstract state.
                 , _reverseEdges :: !(Map CodeAddr (Set CodeAddr))
                   -- | Maps each address that appears to be global data to information
                   -- inferred about it.
                 , _globalDataMap :: !(Map CodeAddr GlobalDataInfo)
                   -- | Set of addresses to explore next.
                   -- This is a map so that we can associate a reason why a code address
                   -- was added to the frontier.
                 , _frontier :: !(Map CodeAddr FrontierReason)
                   -- | Map from code addresses to the abstract state at the start of
                   -- the block.
                 , _absState :: !AbsState
                 }

-- | Empty interpreter state.
emptyInterpState :: Memory Word64 -> InterpState
emptyInterpState mem = InterpState
      { memory        = mem
      , _genState     = emptyGlobalGenState
      , _blockStartAddrs    = Set.empty
      , _blocks       = Map.empty
      , _failedAddrs  = Set.empty
      , _functionEntries = Set.empty
      , _reverseEdges = Map.empty
      , _globalDataMap  = Map.empty
      , _frontier     = Map.empty
      , _absState     = Map.empty
      }

genState :: Simple Lens InterpState GlobalGenState
genState = lens _genState (\s v -> s { _genState = v })

blockStartAddrs :: Simple Lens InterpState (Set CodeAddr)
blockStartAddrs = lens _blockStartAddrs (\s v -> s { _blockStartAddrs = v })

blocks :: Simple Lens InterpState (Map CodeAddr BlockRegion)
blocks = lens _blocks (\s v -> s { _blocks = v })

failedAddrs :: Simple Lens InterpState (Set CodeAddr)
failedAddrs = lens _failedAddrs (\s v -> s { _failedAddrs = v })

-- | Addresses that start each function.
functionEntries :: Simple Lens InterpState (Set CodeAddr)
functionEntries = lens _functionEntries (\s v -> s { _functionEntries = v })

reverseEdges :: Simple Lens InterpState (Map CodeAddr (Set CodeAddr))
reverseEdges = lens _reverseEdges (\s v -> s { _reverseEdges = v })

-- | Map each jump table start to the address just after the end.
globalDataMap :: Simple Lens InterpState (Map CodeAddr GlobalDataInfo)
globalDataMap = lens _globalDataMap (\s v -> s { _globalDataMap = v })

frontier :: Simple Lens InterpState (Map CodeAddr FrontierReason)
frontier = lens _frontier (\s v -> s { _frontier = v })

absState :: Simple Lens InterpState AbsState
absState = lens _absState (\s v -> s { _absState = v })

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