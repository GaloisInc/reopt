{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
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
  , ParsedTermStmt(..)
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
  , identifyCall
  , identifyReturn
  , classifyBlock
  , getClassifyBlock
  )  where

import           Control.Lens
import           Control.Monad (join)
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Type.Equality
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen (pretty)
                 
import           Data.Parameterized.Some

import           Reopt.Analysis.AbsState
import           Reopt.CFG.Implementation (GlobalGenState, emptyGlobalGenState)
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.SysDeps.Types
import           Reopt.Machine.Types
import           Reopt.Object.Memory

import           Reopt.Utils.Debug

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
   = ParsedCall !(X86State Value)
                !(Seq Stmt)
                -- ^ Statements less the pushed return value, if any
                !(Either Word64 (BVValue 64))
                -- ^ Function to call.  If it is statically known,
                -- then we get Left, otherwise Right
                !(Maybe Word64) -- ^ Return location, Nothing if a tail call.
     -- | A jump within a block
   | ParsedJump !(X86State Value) !Word64
     -- | A lookup table that branches to the given locations.
   | -- forall n . (1 <= n) =>
       ParsedLookupTable !(X86State Value) (BVValue 64) (V.Vector Word64)
     -- | A tail cthat branches to the given locations.
   | ParsedReturn !(X86State Value) !(Seq Stmt)
     -- | A branch (i.e., BlockTerm is Branch)
   | ParsedBranch !(Value BoolType) !(BlockLabel) !(BlockLabel)
   | ParsedSyscall !(X86State Value) !Word64 !Word64 !String !String ![N.RegisterName 'N.GP] ![Some N.RegisterName]

------------------------------------------------------------------------
-- InterpState

-- | The state of the interpreter
data InterpState
   = InterpState { -- | The initial memory when disassembly started.
                   memory   :: !(Memory Word64)
                   -- | The set of symbol names (not necessarily complete)
                 , symbolNames :: Map CodeAddr BS.ByteString
                   -- | Syscall personailty, mainly used by getClassifyBlock etc.
                 , syscallPersonality :: SyscallPersonality
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
                 -- , _parsedTermStmts :: !(Map CodeAddr ParsedTermStmt)
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
emptyInterpState :: Memory Word64
                 -> Map CodeAddr BS.ByteString
                 -> SyscallPersonality
                 -> InterpState
emptyInterpState mem symbols sysp = InterpState
      { memory        = mem
      , symbolNames   = symbols
      , syscallPersonality = sysp
      , _genState     = emptyGlobalGenState
      , _blocks       = Map.empty
      , _functionEntries = Set.empty
      , _reverseEdges    = Map.empty
      , _globalDataMap   = Map.empty
      -- , _parsedTermStmts = Map.empty
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
-- parsedTermStmts :: Simple Lens InterpState (Map CodeAddr ParsedTermStmt)
-- parsedTermStmts = lens _parsedTermStmts (\s v -> s { _parsedTermStmts = v })

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

getFunctionEntryPoint' :: CodeAddr -> InterpState -> Maybe CodeAddr
getFunctionEntryPoint' addr s = Set.lookupLE addr (s^.functionEntries) 

inSameFunction :: CodeAddr -> CodeAddr -> InterpState -> Bool
inSameFunction x y s =
  getFunctionEntryPoint x s == getFunctionEntryPoint y s

-- | @isWriteTo stmt add tpr@ returns true if @stmt@ writes to @addr@
-- with a write having the given type.
isWriteTo :: Stmt -> Value (BVType 64) -> TypeRepr tp -> Maybe (Value tp)
isWriteTo (Write (MemLoc a _) val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (valueType val) tp =
    Just val
isWriteTo _ _ _ = Nothing

-- | @isCodeAddrWriteTo mem stmt addr@ returns true if @stmt@ writes
-- a single address to a marked executable in @mem@ to @addr@.
isCodeAddrWriteTo :: Memory Word64 -> Stmt -> Value (BVType 64) -> Maybe Word64
isCodeAddrWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodeAddr mem (fromInteger val)
  = Just (fromInteger val)
isCodeAddrWriteTo _ _ _ = Nothing

-- | Attempt to identify the write to a stack return address, returning
-- instructions prior to that write and return  values.
identifyCall :: Memory Word64
             -> [Stmt]
             -> X86State Value
             -> Maybe (Seq Stmt, Word64)
identifyCall mem stmts0 s = go (Seq.fromList stmts0)
  where next_sp = s^.register N.rsp
        go stmts =
          case Seq.viewr stmts of
            Seq.EmptyR -> Nothing
            prev Seq.:> stmt
              | Just ret <- isCodeAddrWriteTo mem stmt next_sp ->
                Just (prev, ret)
              | Write{} <- stmt -> Nothing
              | otherwise -> go prev

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
identifyReturn :: X86State Value -> Maybe (Assignment (BVType 64))
identifyReturn s = do
  let next_ip = s^.register N.rip
      next_sp = s^.register N.rsp
  case next_ip of
    AssignedValue asgn@(Assignment _ (Read (MemLoc ip_addr _)))
      | let (ip_base, ip_off) = asBaseOffset ip_addr
      , let (sp_base, sp_off) = asBaseOffset next_sp
      , (ip_base, ip_off + 8) == (sp_base, sp_off) -> Just asgn
    _ -> Nothing

identifyJumpTable :: InterpState
                  -> BlockLabel
                      -- | Memory address that IP is read from.
                  -> Value (BVType 64)
                  -- Returns the (symbolic) index and concrete next blocks
                  -> Maybe (Value (BVType 64), V.Vector CodeAddr) 
identifyJumpTable s lbl (AssignedValue (Assignment _ (Read (MemLoc ptr _))))
    -- Turn the read address into base + offset.
   | Just (BVAdd _ offset (BVValue _ base)) <- valueAsApp ptr
    -- Turn the offset into a multiple by an index.
   , Just (BVMul _ (BVValue _ 8) idx) <- valueAsApp offset
   , isReadonlyAddr mem (fromInteger base) =
       Just (idx, V.unfoldr nextWord (fromInteger base))
  where
    enclosingFun    = getFunctionEntryPoint (labelAddr lbl) s
    nextWord tblPtr
      | Right codePtr <- memLookupWord64 mem pf_r tblPtr
      , isReadonlyAddr mem tblPtr
      , getFunctionEntryPoint' codePtr s == Just enclosingFun = Just (codePtr, tblPtr + 8)
      | otherwise = Nothing
    mem = memory s
identifyJumpTable _ _ _ = Nothing

classifyBlock :: Block -> InterpState -> Maybe ParsedTermStmt
classifyBlock b interp_state = 
  case blockTerm b of
    Branch c x y -> Just (ParsedBranch c x y)
    FetchAndExecute proc_state
        -- The last statement was a call.
      | Just (prev_stmts, ret_addr) <- identifyCall mem (blockStmts b) proc_state ->
          let fptr = case proc_state ^. register N.rip of
                       BVValue _ v -> Left (fromInteger v)
                       ip          -> Right ip
          in Just (ParsedCall proc_state prev_stmts fptr (Just ret_addr))

      -- Jump to concrete offset.
      | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.register N.rip
      , inSameFunction (labelAddr (blockLabel b)) tgt_addr interp_state ->
           Just (ParsedJump proc_state tgt_addr)

      -- Return
      | Just asgn <- identifyReturn proc_state ->
        let isRetLoad s =
              case s of
                AssignStmt asgn' | Just Refl <- testEquality asgn asgn' -> True
                _ -> False
            nonret_stmts = Seq.fromList $ filter (not . isRetLoad) (blockStmts b)

        in Just (ParsedReturn proc_state nonret_stmts)

      -- Tail calls to a concrete address (or, nop pads after a non-returning call)
      | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.register N.rip ->
        Just (ParsedCall proc_state (Seq.fromList $ blockStmts b) (Left tgt_addr) Nothing)

      | Just (idx, nexts) <- identifyJumpTable interp_state (blockLabel b)
                                               (proc_state ^. register N.rip) ->
          Just (ParsedLookupTable proc_state idx nexts)

      -- Finally, we just assume that this is a tail call through a pointer
      -- FIXME: probably unsound.
      | otherwise -> Just (ParsedCall proc_state
                                      (Seq.fromList $ blockStmts b)
                                      (Right $ proc_state^.register N.rip) Nothing)

    -- rax is concrete in the first case, so we don't need to propagate it etc.
    Syscall proc_state
      | BVValue _ (fromInteger -> next_addr) <- proc_state^.register N.rip
      , BVValue _ (fromInteger -> call_no) <- proc_state^.register N.rax
      , Just (name, _rettype, argtypes) <- Map.lookup call_no (spTypeInfo sysp) ->
         let result = Just (ParsedSyscall proc_state next_addr call_no (spName sysp) name
                            (take (length argtypes) x86SyscallArgumentRegisters)
                            (spResultRegisters sysp)
                           )
         in case () of
              _ | any ((/=) WordArgType) argtypes -> error "Got a non-word arg type"
              _ | length argtypes > length x86SyscallArgumentRegisters -> 
                  debug DUrgent ("Got more than register args calling " ++ name
                                 ++ " in block " ++ show (blockLabel b))
                                result 
              _ -> result
              
        -- FIXME: Should subsume the above ...
        -- FIXME: only works if rax is an initial register
      | BVValue _ (fromInteger -> (next_addr :: Word64)) <- proc_state^.register N.rip
      , Initial r <- proc_state ^. register N.rax 
      , Just absSt <- Map.lookup (labelAddr $ blockLabel b) (interp_state ^. absState)
      , Just (fromInteger -> call_no) <- asConcreteSingleton (absSt ^. absX86State ^. register r)
      , Just (name, _rettype, argtypes) <- Map.lookup call_no (spTypeInfo sysp) ->
         let result = Just (ParsedSyscall proc_state next_addr call_no (spName sysp) name
                            (take (length argtypes) x86SyscallArgumentRegisters)
                            (spResultRegisters sysp)
                           )
         in case () of
              _ | any ((/=) WordArgType) argtypes -> error "Got a non-word arg type"
              _ | length argtypes > length x86SyscallArgumentRegisters -> 
                  debug DUrgent ("Got more than register args calling " ++ name
                                 ++ " in block " ++ show (blockLabel b))
                                result 
              _ -> result

          
      | BVValue _ (fromInteger -> next_addr) <- proc_state^.register N.rip ->
          debug DUrgent ("Unknown syscall in block " ++ show (blockLabel b)
                         ++ " rax is " ++ show (pretty $ proc_state^.register N.rax)
                        )
          Just (ParsedSyscall proc_state next_addr 0 (spName sysp) "unknown"
                             x86SyscallArgumentRegisters
                             (spResultRegisters sysp)
               )
      | otherwise -> error "shouldn't get here"          
  where
    mem = memory interp_state
    sysp = syscallPersonality interp_state

getClassifyBlock :: BlockLabel -> InterpState ->  Maybe (Block, Maybe ParsedTermStmt)
getClassifyBlock lbl interp_state = do
  b <- lookupBlock (interp_state ^. blocks) lbl
  return (b, classifyBlock b interp_state)
  
