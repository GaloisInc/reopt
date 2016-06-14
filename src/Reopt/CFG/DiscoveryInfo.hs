{-|
Module     : Reopt.CFG.DiscoveryInfo
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

This defines the information learned during the code discovery phase of Reopt.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reopt.CFG.DiscoveryInfo
  ( BlockRegion(..)
  , lookupBlock
  , GlobalDataInfo(..)
  , ParsedTermStmt(..)
    -- * The interpreter state
  , DiscoveryInfo
  , emptyDiscoveryInfo
  , memory
  , symbolNames
  , genState
  , blocks
  , functionEntries
  , reverseEdges
  , globalDataMap
    -- * Frontier
  , FrontierReason(..)
  , frontier
  , function_frontier
    -- ** Abstract state information
  , absState
  , AbsStateMap
  , lookupAbsBlock
    -- ** DiscoveryInfo utilities
  , getFunctionEntryPoint
  , inSameFunction
  , returnCount
  , identifyCall
  , identifyReturn
  , classifyBlock
  , getClassifyBlock
  , setAbsIP
  )  where

import           Control.Lens
import           Control.Monad (join)
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Type.Equality
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Data.Macaw.CFG
import           Data.Macaw.Types

import           Reopt.Analysis.AbsState
import qualified Reopt.Analysis.Domains.StridedInterval as SI
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.SysDeps.Types
import           Reopt.Machine.X86State
import           Reopt.Object.Memory

import           Reopt.Utils.Debug

------------------------------------------------------------------------
-- X86 Specific Abstract methods.

instance AbsRegState X86_64 where
  mkRegStateM = mkX86StateM
  mkRegState  = mkX86State

instance SupportAbsEvaluation X86_64 X86PrimFn where
  transferAbsValue = transferX86PrimFn

transferX86PrimFn :: AbsProcessorState X86_64 -> X86PrimFn tp -> AbsValue 64 tp
transferX86PrimFn r f =
  case f of
    ReadLoc _ -> TopV
    ReadFSBase -> TopV
    ReadGSBase -> TopV
      -- We know only that it will return up to (and including(?)) cnt
    MemCmp _sz cnt _src _dest _rev
      | Just upper <- hasMaximum knownType (transferValue r cnt) ->
          stridedInterval $ SI.mkStridedInterval knownNat False 0 upper 1
      | otherwise -> TopV
    FindElement _sz _findEq cnt _buf _val _rev
      | Just upper <- hasMaximum knownType (transferValue r cnt) ->
          stridedInterval $ SI.mkStridedInterval knownNat False 0 upper 1
      | otherwise -> TopV

-- | Update the block state to point to a specific IP address.
setAbsIP :: (Word64 -> Bool)
            -- ^ Predicate to check that given IP is a code pointer.
         -> Word64
            -- ^ The width of a code pointer.
         -> AbsBlockState X86_64
         -> Maybe (AbsBlockState X86_64)
setAbsIP is_code a b
  | is_code a == False =
    Nothing
    -- Check to avoid reassigning next IP if it is not needed.
  | CodePointers s <- b^.absRegState^.curIP
  , Set.size s == 1
  , Set.member a s =
    Just b
  | otherwise =
    Just $ b & absRegState . curIP .~ CodePointers (Set.singleton a)

------------------------------------------------------------------------
-- AbsStateMap

-- | Maps each code address to a set of abstract states
type AbsStateMap = Map CodeAddr (AbsBlockState X86_64)

lookupAbsBlock :: CodeAddr -> AbsStateMap -> AbsBlockState X86_64
lookupAbsBlock addr s = fromMaybe (error msg) (Map.lookup addr s)
  where msg = "Could not find block " ++ showHex addr "."

------------------------------------------------------------------------
-- BlockRegion

-- | The blocks contained in a single contiguous region of instructions.
data BlockRegion = BlockRegion { brEnd :: !CodeAddr
                                 -- | Map from labelIndex to associated block.
                               , brBlocks :: !(Map Word64 (Block X86_64))
                               }

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock :: Map CodeAddr (Maybe BlockRegion)
            -> BlockLabel Word64
            -> Maybe (Block X86_64)
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
   | InWrite       !(BlockLabel Word64)
     -- | Exploring because the given block stores address as a
     -- return address.
   | ReturnAddress !(BlockLabel Word64)
     -- | Exploring because the given block jumps here.
   | NextIP !(BlockLabel Word64)
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
   = ParsedCall !(X86State (Value X86_64))
                !(Seq (Stmt X86_64))
                -- ^ Statements less the pushed return value, if any
                !(Either Word64 (BVValue X86_64 64))
                -- ^ Function to call.  If it is statically known,
                -- then we get Left, otherwise Right
                !(Maybe Word64) -- ^ Return location, Nothing if a tail call.
     -- | A jump within a block
   | ParsedJump !(X86State (Value X86_64)) !Word64
     -- | A lookup table that branches to the given locations.
   | -- forall n . (1 <= n) =>
       ParsedLookupTable !(X86State (Value X86_64)) (BVValue X86_64 64) (V.Vector Word64)
     -- | A tail cthat branches to the given locations.
   | ParsedReturn !(X86State (Value X86_64)) !(Seq (Stmt X86_64))
     -- | A branch (i.e., BlockTerm is Branch)
   | ParsedBranch !(Value X86_64 BoolType) !(BlockLabel Word64) !(BlockLabel Word64)
   | ParsedSyscall !(X86State (Value X86_64))
                   !Word64
                   !Word64
                   !String
                   !String
                   ![X86Reg (BVType 64)]
                   ![Some N.RegisterName]
  deriving (Show)

------------------------------------------------------------------------
-- DiscoveryInfo

-- | The state of the interpreter
data DiscoveryInfo
   = DiscoveryInfo { memory   :: !(Memory Word64)
                     -- ^ The initial memory when disassembly started.
                   , symbolNames :: Map CodeAddr BS.ByteString
                     -- ^ The set of symbol names (not necessarily complete)
                   , syscallPersonality :: SyscallPersonality
                   -- ^ Syscall personailty, mainly used by getClassifyBlock etc.
                   , _genState :: !AssignId
                   -- ^ Next index to use for generating an assignment.
                   -- | Intervals maps code addresses to blocks at address
                   -- or nothing if disassembly failed.
                   , _blocks   :: !(Map CodeAddr (Maybe BlockRegion))
                   , _functionEntries :: !(Set CodeAddr)
                      -- ^ Maps addresses that are marked as the start of a function
                   , _reverseEdges :: !(Map CodeAddr (Set CodeAddr))
                     -- ^ Maps each code address to the list of predecessors that
                     -- affected its abstract state.
                   , _globalDataMap :: !(Map CodeAddr GlobalDataInfo)
                     -- ^ Maps each address that appears to be global data to information
                     -- inferred about it.
                   , _frontier :: !(Map CodeAddr FrontierReason)
                     -- ^ Set of addresses to explore next.
                     --
                     -- This is a map so that we can associate a reason why a code address
                     -- was added to the frontier.
                   , _function_frontier :: !(Set CodeAddr)
                     -- ^ Set of functions to explore next.
                   , _absState :: !AbsStateMap
                     -- ^ Map from code addresses to the abstract state at the start of
                     -- the block.
                   , _returnCount :: !Int
                   }

-- | Empty interpreter state.
emptyDiscoveryInfo :: Memory Word64
                 -> Map CodeAddr BS.ByteString
                 -> SyscallPersonality
                 -> DiscoveryInfo
emptyDiscoveryInfo mem symbols sysp = DiscoveryInfo
      { memory        = mem
      , symbolNames   = symbols
      , syscallPersonality = sysp
      , _genState     = 0
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

-- | Next id to use for generating assignments
genState :: Simple Lens DiscoveryInfo AssignId
genState = lens _genState (\s v -> s { _genState = v })

blocks :: Simple Lens DiscoveryInfo (Map CodeAddr (Maybe BlockRegion))
blocks = lens _blocks (\s v -> s { _blocks = v })

-- | Addresses that start each function.
functionEntries :: Simple Lens DiscoveryInfo (Set CodeAddr)
functionEntries = lens _functionEntries (\s v -> s { _functionEntries = v })

reverseEdges :: Simple Lens DiscoveryInfo (Map CodeAddr (Set CodeAddr))
reverseEdges = lens _reverseEdges (\s v -> s { _reverseEdges = v })

-- | Map each jump table start to the address just after the end.
globalDataMap :: Simple Lens DiscoveryInfo (Map CodeAddr GlobalDataInfo)
globalDataMap = lens _globalDataMap (\s v -> s { _globalDataMap = v })

-- | Information about the next state for blocks that end with
-- @FetchAndExecute@ statements.
-- parsedTermStmts :: Simple Lens DiscoveryInfo (Map CodeAddr ParsedTermStmt)
-- parsedTermStmts = lens _parsedTermStmts (\s v -> s { _parsedTermStmts = v })

frontier :: Simple Lens DiscoveryInfo (Map CodeAddr FrontierReason)
frontier = lens _frontier (\s v -> s { _frontier = v })

function_frontier :: Simple Lens DiscoveryInfo (Set CodeAddr)
function_frontier = lens _function_frontier (\s v -> s { _function_frontier = v })

absState :: Simple Lens DiscoveryInfo AbsStateMap
absState = lens _absState (\s v -> s { _absState = v })

returnCount :: Simple Lens DiscoveryInfo Int
returnCount = lens _returnCount (\s v -> s { _returnCount = v })

------------------------------------------------------------------------
-- DiscoveryInfo utilities

-- | Returns the guess on the entry point of the given function.
getFunctionEntryPoint :: CodeAddr -> DiscoveryInfo -> CodeAddr
getFunctionEntryPoint addr s = do
  case Set.lookupLE addr (s^.functionEntries) of
    Just a -> a
    Nothing -> error $ "Could not find address of " ++ showHex addr "."

getFunctionEntryPoint' :: CodeAddr -> DiscoveryInfo -> Maybe CodeAddr
getFunctionEntryPoint' addr s = Set.lookupLE addr (s^.functionEntries)

inSameFunction :: CodeAddr -> CodeAddr -> DiscoveryInfo -> Bool
inSameFunction x y s =
  getFunctionEntryPoint x s == getFunctionEntryPoint y s

-- | @isWriteTo stmt add tpr@ returns true if @stmt@ writes to @addr@
-- with a write having the given type.
isWriteTo :: Stmt X86_64
          -> BVValue X86_64 64
          -> TypeRepr tp
          -> Maybe (Value X86_64 tp)
isWriteTo (WriteMem a val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (typeRepr val) tp =
    Just val
isWriteTo _ _ _ = Nothing

-- | @isCodeAddrWriteTo mem stmt addr@ returns true if @stmt@ writes
-- a single address to a marked executable in @mem@ to @addr@.
isCodeAddrWriteTo :: Memory Word64 -> Stmt X86_64 -> BVValue X86_64 64 -> Maybe Word64
isCodeAddrWriteTo mem s sp
  | Just (BVValue _ val) <- isWriteTo s sp (knownType :: TypeRepr (BVType 64))
  , isCodeAddr mem (fromInteger val)
  = Just (fromInteger val)
isCodeAddrWriteTo _ _ _ = Nothing

-- | Attempt to identify the write to a stack return address, returning
-- instructions prior to that write and return  values.
identifyCall :: Memory Word64
             -> [Stmt X86_64]
             -> X86State (Value X86_64)
             -> Maybe (Seq (Stmt X86_64), Word64)
identifyCall mem stmts0 s = go (Seq.fromList stmts0)
  where next_sp = s^.boundValue sp_reg
        go stmts =
          case Seq.viewr stmts of
            Seq.EmptyR -> Nothing
            prev Seq.:> stmt
              | Just ret <- isCodeAddrWriteTo mem stmt next_sp ->
                Just (prev, ret)
              | ExecArchStmt WriteLoc{} <- stmt -> Nothing
              | otherwise -> go prev

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
identifyReturn :: X86State (Value X86_64) -> Maybe (Assignment X86_64 (BVType 64))
identifyReturn s = do
  let next_ip = s^.boundValue ip_reg
      next_sp = s^.boundValue sp_reg
  case next_ip of
    AssignedValue asgn@(Assignment _ (ReadMem ip_addr _))
      | let (ip_base, ip_off) = asBaseOffset ip_addr
      , let (sp_base, sp_off) = asBaseOffset next_sp
      , (ip_base, ip_off + 8) == (sp_base, sp_off) -> Just asgn
    _ -> Nothing

identifyJumpTable :: DiscoveryInfo
                  -> BlockLabel Word64
                      -- | Memory address that IP is read from.
                  -> BVValue X86_64 64
                  -- Returns the (symbolic) index and concrete next blocks
                  -> Maybe (BVValue X86_64 64, V.Vector Word64)
identifyJumpTable s lbl (AssignedValue (Assignment _ (ReadMem ptr _)))
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

-- | Classifies the terminal statement in a block using discovered information.
classifyBlock :: Block X86_64 -> DiscoveryInfo -> Maybe ParsedTermStmt
classifyBlock b interp_state =
  case blockTerm b of
    Branch c x y -> Just (ParsedBranch c x y)
    FetchAndExecute proc_state
        -- The last statement was a call.
      | Just (prev_stmts, ret_addr) <- identifyCall mem (blockStmts b) proc_state ->
          let fptr = case proc_state^.boundValue ip_reg of
                       BVValue _ v -> Left (fromInteger v)
                       ip          -> Right ip
          in Just (ParsedCall proc_state prev_stmts fptr (Just ret_addr))

      -- Jump to concrete offset.
      | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.boundValue ip_reg
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
      | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.boundValue ip_reg ->
        Just (ParsedCall proc_state (Seq.fromList $ blockStmts b) (Left tgt_addr) Nothing)

      | Just (idx, nexts) <- identifyJumpTable interp_state (blockLabel b)
                                               (proc_state^.boundValue ip_reg) ->
          Just (ParsedLookupTable proc_state idx nexts)

      -- Finally, we just assume that this is a tail call through a pointer
      -- FIXME: probably unsound.
      | otherwise -> Just (ParsedCall proc_state
                                      (Seq.fromList $ blockStmts b)
                                      (Right $ proc_state^.boundValue ip_reg) Nothing)

    -- rax is concrete in the first case, so we don't need to propagate it etc.
    Syscall proc_state
      | BVValue _ (fromInteger -> next_addr) <- proc_state^.boundValue ip_reg
      , BVValue _ (fromInteger -> call_no)   <- proc_state^.boundValue rax_reg
      , Just (name, _rettype, argtypes) <- Map.lookup call_no (spTypeInfo sysp) ->
         let result = Just (ParsedSyscall proc_state next_addr call_no (spName sysp) name
                            (take (length argtypes) x86SyscallArgumentRegs)
                            (spResultRegisters sysp)
                           )
         in case () of
              _ | any ((/=) WordArgType) argtypes -> error "Got a non-word arg type"
              _ | length argtypes > length x86SyscallArgumentRegs ->
                  debug DUrgent ("Got more than register args calling " ++ name
                                 ++ " in block " ++ show (blockLabel b))
                                result
              _ -> result

        -- FIXME: Should subsume the above ...
        -- FIXME: only works if rax is an initial register
      | BVValue _ (fromInteger -> (next_addr :: Word64)) <- proc_state^.boundValue ip_reg
      , Initial r <- proc_state^.boundValue rax_reg
      , Just absSt <- Map.lookup (labelAddr $ blockLabel b) (interp_state ^. absState)
      , Just (fromInteger -> call_no) <-
          asConcreteSingleton (absSt ^. absRegState ^. boundValue r)
      , Just (name, _rettype, argtypes) <- Map.lookup call_no (spTypeInfo sysp) ->
         let result = Just $
               ParsedSyscall proc_state next_addr call_no (spName sysp) name
                             (take (length argtypes) x86SyscallArgumentRegs)
                             (spResultRegisters sysp)
         in case () of
              _ | any ((/=) WordArgType) argtypes -> error "Got a non-word arg type"
              _ | length argtypes > length x86SyscallArgumentRegs ->
                  debug DUrgent ("Got more than register args calling " ++ name
                                 ++ " in block " ++ show (blockLabel b))
                                result
              _ -> result


      | BVValue _ (fromInteger -> next_addr) <- proc_state^.boundValue ip_reg ->
          debug DUrgent ("Unknown syscall in block " ++ show (blockLabel b)
                         ++ " rax is " ++ show (pretty $ proc_state^.boundValue rax_reg)
                        )
          Just $ ParsedSyscall proc_state next_addr 0 (spName sysp) "unknown"
                               x86SyscallArgumentRegs
                               (spResultRegisters sysp)
      | otherwise -> error "shouldn't get here"
  where
    mem = memory interp_state
    sysp = syscallPersonality interp_state

getClassifyBlock :: BlockLabel Word64
                 -> DiscoveryInfo
                 ->  Maybe (Block X86_64, Maybe ParsedTermStmt)
getClassifyBlock lbl interp_state = do
  b <- lookupBlock (interp_state ^. blocks) lbl
  return (b, classifyBlock b interp_state)
