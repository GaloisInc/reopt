{-|
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

This defines the main data structure for storing information learned from code
discovery.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Macaw.Discovery.Info
  ( BlockRegion(..)
  , lookupBlock
  , GlobalDataInfo(..)
  , ParsedTermStmt(..)
    -- * The interpreter state
  , DiscoveryInfo
  , emptyDiscoveryInfo
  , nonceGen
  , archInfo
  , memory
  , symbolNames
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
  , ArchConstraint
  , identifyCall
  , identifyReturn
  , asLiteralAddr
  , getClassifyBlock
  )  where

import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.ST
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Classes
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..))

import           Data.Macaw.AbsDomain.AbsState
import           Data.Macaw.Architecture.Info
import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm
import           Data.Macaw.Types

------------------------------------------------------------------------
-- AbsStateMap

-- | Maps each code address to a set of abstract states
type AbsStateMap arch = Map (ArchSegmentedAddr arch) (AbsBlockState (ArchReg arch))

lookupAbsBlock :: ( Integral addr
                  , Ord addr
                  , Show addr
                  )
                  => addr
                  -> Map addr (AbsBlockState r)
                  -> AbsBlockState r
lookupAbsBlock addr s = fromMaybe (error msg) (Map.lookup addr s)
  where msg = "Could not find block " ++ showHex addr "."

------------------------------------------------------------------------
-- BlockRegion

-- | The blocks contained in a single contiguous region of instructions.
data BlockRegion arch ids
   = BlockRegion { brSize :: !(ArchAddr arch)
                 , brBlocks :: !(Map Word64 (Block arch ids))
                   -- ^ Map from labelIndex to associated block.
                 }

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock :: Ord (ArchAddr arch)
            => Map (ArchSegmentedAddr arch) (Maybe (BlockRegion arch ids))
            -> ArchLabel arch
            -> Maybe (Block arch ids)
lookupBlock m lbl = do
  br <- join $ Map.lookup (labelAddr lbl) m
  Map.lookup (labelIndex lbl) (brBlocks br)

------------------------------------------------------------------------
-- FrontierReason

-- | Data describing why an address was added to the frontier.
data FrontierReason w
   = InInitialData
     -- ^ Exploring because a pointer to this address was found stored in
     -- memory.
   | InWrite       !(BlockLabel w)
     -- ^ Exploring because the given block writes it to memory.
   | ReturnAddress !(BlockLabel w)
     -- ^ Exploring because the given block stores address as a
     -- return address.
   | NextIP !(BlockLabel w)
     -- ^ Exploring because the given block jumps here.
   | StartAddr
     -- ^ Added as the initial start state.
   | BlockSplit
     -- ^ Added because a previous block split this block.
  deriving (Show)

------------------------------------------------------------------------
-- GlobalDataInfo

data GlobalDataInfo w
     -- | A jump table that appears to end just before the given address.
   = JumpTable !(Maybe w)
     -- | A value that appears in the program text.
   | ReferencedValue

instance (Integral w, Show w) => Show (GlobalDataInfo w) where
  show (JumpTable Nothing) = "unbound jump table"
  show (JumpTable (Just w)) = "jump table end " ++ showHex w ""
  show ReferencedValue = "global addr"

------------------------------------------------------------------------
-- ParsedTermStmt

-- | This term statement is used to describe higher level expressions
-- of how block ending with a a FetchAndExecute statement should be
-- interpreted.
data ParsedTermStmt arch ids
   = ParsedCall !(RegState (ArchReg arch) (Value arch ids))
                !(Seq (Stmt arch ids))
                -- /\ Statements less the pushed return value, if any
                !(Either (ArchSegmentedAddr arch) (BVValue arch ids (ArchAddrWidth arch)))
                -- /\ Function to call.  If it is statically known,
                -- then we get Left, otherwise Right
                !(Maybe (ArchSegmentedAddr arch))
                -- ^ Return location, Nothing if a tail call.
     -- | A jump within a block
   | ParsedJump !(RegState (ArchReg arch) (Value arch ids)) !(ArchSegmentedAddr arch)
     -- | A lookup table that branches to the given locations.
   | ParsedLookupTable !(RegState (ArchReg arch) (Value arch ids))
                       !(BVValue arch ids (ArchAddrWidth arch))
                       !(V.Vector (ArchSegmentedAddr arch))
     -- | A tail cthat branches to the given locations.
   | ParsedReturn !(RegState (ArchReg arch) (Value arch ids)) !(Seq (Stmt arch ids))
     -- | A branch (i.e., BlockTerm is Branch)
   | ParsedBranch !(Value arch ids BoolType) !(ArchLabel arch) !(ArchLabel arch)
   | ParsedSyscall !(RegState (ArchReg arch) (Value arch ids))
                   !(ArchSegmentedAddr arch)
                   !(ArchAddr arch)
                   !String
                   !String
                   ![ArchReg arch (BVType (ArchAddrWidth arch))]
                   ![Some (ArchReg arch)]

deriving instance
  ( PrettyCFGConstraints arch
  , Show (ArchReg arch (BVType (ArchAddrWidth arch)))
  )
  => Show (ParsedTermStmt arch ids)

------------------------------------------------------------------------
-- DiscoveryInfo

-- | The state of the interpreter
data DiscoveryInfo arch ids
   = DiscoveryInfo { nonceGen :: !(NonceGenerator (ST ids) ids)
                     -- ^ Generator for creating fresh ids.
                   , memory   :: !(Memory (ArchAddrWidth arch))
                     -- ^ The initial memory when disassembly started.
                   , symbolNames :: !(Map (ArchSegmentedAddr arch) BS.ByteString)
                     -- ^ The set of symbol names (not necessarily complete)
                   , syscallPersonality :: !(SyscallPersonality arch)
                     -- ^ Syscall personailty, mainly used by getClassifyBlock etc.
                   , archInfo :: !(ArchitectureInfo arch)
                     -- ^ Architecture-specific information needed for discovery.
                   -- | Intervals maps code addresses to blocks at address
                   -- or nothing if disassembly failed.
                   , _blocks   :: !(Map (ArchSegmentedAddr arch)
                                        (Maybe (BlockRegion arch ids)))
                   , _functionEntries :: !(Set (ArchSegmentedAddr arch))
                      -- ^ Maps addresses that are marked as the start of a function
                   , _reverseEdges :: !(Map (ArchSegmentedAddr arch)
                                            (Set (ArchSegmentedAddr arch)))
                     -- ^ Maps each code address to the list of predecessors that
                     -- affected its abstract state.
                   , _globalDataMap :: !(Map (ArchSegmentedAddr arch)
                                             (GlobalDataInfo (ArchSegmentedAddr arch)))
                     -- ^ Maps each address that appears to be global data to information
                     -- inferred about it.
                   , _frontier :: !(Map (ArchSegmentedAddr arch)
                                        (FrontierReason (ArchAddrWidth arch)))
                     -- ^ Set of addresses to explore next.
                     --
                     -- This is a map so that we can associate a reason why a code address
                     -- was added to the frontier.
                   , _function_frontier :: !(Set (ArchSegmentedAddr arch))
                     -- ^ Set of functions to explore next.
                   , _absState :: !(AbsStateMap arch)
                     -- ^ Map from code addresses to the abstract state at the start of
                     -- the block.
                   }

-- | Empty interpreter state.
emptyDiscoveryInfo :: NonceGenerator (ST ids) ids
                   -> Memory (ArchAddrWidth arch)
                   -> Map (ArchSegmentedAddr arch) BS.ByteString
                   -> SyscallPersonality arch
                   -> ArchitectureInfo arch
                      -- ^ Stack delta
                   -> DiscoveryInfo arch ids
emptyDiscoveryInfo ng mem symbols sysp info = DiscoveryInfo
      { nonceGen           = ng
      , memory             = mem
      , symbolNames        = symbols
      , syscallPersonality = sysp
      , archInfo           = info
      , _blocks            = Map.empty
      , _functionEntries   = Set.empty
      , _reverseEdges      = Map.empty
      , _globalDataMap     = Map.empty
      , _frontier          = Map.empty
      , _function_frontier = Set.empty
      , _absState          = Map.empty
      }

blocks :: Simple Lens (DiscoveryInfo arch ids)
                      (Map (ArchSegmentedAddr arch) (Maybe (BlockRegion arch ids)))
blocks = lens _blocks (\s v -> s { _blocks = v })

-- | Addresses that start each function.
functionEntries :: Simple Lens (DiscoveryInfo arch ids) (Set (ArchSegmentedAddr arch))
functionEntries = lens _functionEntries (\s v -> s { _functionEntries = v })

reverseEdges :: Simple Lens (DiscoveryInfo arch ids)
                            (Map (ArchSegmentedAddr arch) (Set (ArchSegmentedAddr arch)))
reverseEdges = lens _reverseEdges (\s v -> s { _reverseEdges = v })

-- | Map each jump table start to the address just after the end.
globalDataMap :: Simple Lens (DiscoveryInfo arch ids)
                             (Map (ArchSegmentedAddr arch)
                                  (GlobalDataInfo (ArchSegmentedAddr arch)))
globalDataMap = lens _globalDataMap (\s v -> s { _globalDataMap = v })

-- | Set of addresses to explore next.
--
-- This is a map so that we can associate a reason why a code address
-- was added to the frontier.
frontier :: Simple Lens (DiscoveryInfo arch ids)
                        (Map (ArchSegmentedAddr arch) (FrontierReason (ArchAddrWidth arch)))
frontier = lens _frontier (\s v -> s { _frontier = v })

-- | Set of functions to explore next.
function_frontier :: Simple Lens (DiscoveryInfo arch ids)
                                 (Set (ArchSegmentedAddr arch))
function_frontier = lens _function_frontier (\s v -> s { _function_frontier = v })

absState :: Simple Lens (DiscoveryInfo arch ids) (AbsStateMap arch)
absState = lens _absState (\s v -> s { _absState = v })

------------------------------------------------------------------------
-- DiscoveryInfo utilities

{-
-- | Constraint on architecture addresses needed by code exploration.
type AddrConstraint a = (Ord a, Integral a, Show a, Num a)
-}

-- | Returns the guess on the entry point of the given function.
--
-- Note. This code assumes that a block address is associated with at most one function.
getFunctionEntryPoint :: ArchSegmentedAddr a
                      -> DiscoveryInfo a ids
                      -> ArchSegmentedAddr a
getFunctionEntryPoint addr s = do
  case Set.lookupLE addr (s^.functionEntries) of
    Just a -> a
    Nothing -> error $ "Could not find address of " ++ show addr ++ "."

-- | Returns the guess on the entry point of the given function.
--
-- Note. This code assumes that a block address is associated with at most one function.
getFunctionEntryPoint' :: Ord (ArchAddr a)
                       => ArchSegmentedAddr a
                       -> DiscoveryInfo a ids
                       -> Maybe (ArchSegmentedAddr a)
getFunctionEntryPoint' addr s = Set.lookupLE addr (s^.functionEntries)

-- | Return true if the two addresses look like they are in the same
inSameFunction :: ArchSegmentedAddr a
               -> ArchSegmentedAddr a
               -> DiscoveryInfo a ids
               -> Bool
inSameFunction x y s = xf == yf
  where Just xf = Set.lookupLE x (s^.functionEntries)
        Just yf = Set.lookupLE y (s^.functionEntries)

-- | Constraint on architecture register values needed by code exploration.
type RegConstraint r = (OrdF r, HasRepr r TypeRepr, RegisterInfo r, ShowF r)

-- | Constraint on architecture so that we can do code exploration.
type ArchConstraint a ids = ( RegConstraint (ArchReg a)
--                            , HasRepr (ArchFn a ids) TypeRepr
                            )

{-
-- | @isWriteTo stmt add tpr@ returns 'Just v' if @stmt@ writes 'v'
-- to @addr@ with a write having the given type 'tpr',  and 'Nothing' otherwise.
isWriteTo :: ArchConstraint a ids
          => Stmt a ids
          -> BVValue a ids (ArchAddrWidth a)
          -> TypeRepr tp
          -> Maybe (Value a ids tp)
isWriteTo (WriteMem a val) expected tp
  | Just _ <- testEquality a expected
  , Just Refl <- testEquality (typeRepr val) tp =
    Just val
isWriteTo _ _ _ = Nothing
-}

-- | This returns a segmented address if the value can be interpreted as a literal memory
-- address, and returns nothing otherwise.
asLiteralAddr :: MemWidth (ArchAddrWidth arch)
              => Memory (ArchAddrWidth arch)
              -> BVValue arch ids (ArchAddrWidth arch)
              -> Maybe (ArchSegmentedAddr arch)
asLiteralAddr mem (BVValue _ val) =
  absoluteAddrSegment mem (fromInteger val)
asLiteralAddr _   (RelocatableValue _ a) = Just a
asLiteralAddr _ _ = Nothing

-- | Attempt to identify the write to a stack return address, returning
-- instructions prior to that write and return  values.
--
-- This can also return Nothing if the call is not supported.
identifyCall :: ( ArchConstraint a ids
                , RegisterInfo (ArchReg a)
                , MemWidth (ArchAddrWidth a)
                )
             => Memory (ArchAddrWidth a)
             -> [Stmt a ids]
             -> RegState (ArchReg a) (Value a ids)
             -> Maybe (Seq (Stmt a ids), ArchSegmentedAddr a)
identifyCall mem stmts0 s = go (Seq.fromList stmts0)
  where -- Get value of stack pointer
        next_sp = s^.boundValue sp_reg
        -- Recurse on statements.
        go stmts =
          case Seq.viewr stmts of
            Seq.EmptyR -> Nothing
            prev Seq.:> stmt
              -- Check for a call statement by determining if the last statement
              -- writes an executable address to the stack pointer.
              | WriteMem a val <- stmt
              , Just _ <- testEquality a next_sp
                -- Check this is the right length.
              , Just Refl <- testEquality (typeRepr next_sp) (typeRepr val)
                -- Check if value is a valid literal address
              , Just val_a <- asLiteralAddr mem val
                -- Check if segment of address is marked as executable.
              , Perm.isExecutable (segmentFlags (addrSegment val_a)) ->

                Just (prev, val_a)
                -- Stop if we hit any architecture specific instructions prior to
                -- identifying return address since they may have side effects.
              | ExecArchStmt _ <- stmt -> Nothing
                -- Otherwise skip over this instruction.
              | otherwise -> go prev

-- | This is designed to detect returns from the register state representation.
--
-- It pattern matches on a 'RegState' to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
--
-- Note that this assumes the stack decrements as values are pushed, so we will
-- need to fix this on other architectures.
identifyReturn :: ArchConstraint arch ids
               => RegState (ArchReg arch) (Value arch ids)
               -> Integer
                  -- ^ How stack pointer moves when a call is made
               -> Maybe (Assignment arch ids (BVType (ArchAddrWidth arch)))
identifyReturn s stack_adj = do
  let next_ip = s^.boundValue ip_reg
      next_sp = s^.boundValue sp_reg
  case next_ip of
    AssignedValue asgn@(Assignment _ (ReadMem ip_addr _))
      | let (ip_base, ip_off) = asBaseOffset ip_addr
      , let (sp_base, sp_off) = asBaseOffset next_sp
      , (ip_base, ip_off) == (sp_base, sp_off + stack_adj) -> Just asgn
    _ -> Nothing

-- | This identifies a jump table
--
-- A jump table consists of a contiguous sequence of jump targets laid out in
-- memory.  Each potential jump target is in the same function as the calling
-- function.
identifyJumpTable :: forall arch ids
                  .  MemWidth (ArchAddrWidth arch)
                  => DiscoveryInfo arch ids
                  -> ArchSegmentedAddr arch
                      -- ^ Address of enclosing function.
                  -> BVValue arch ids (ArchAddrWidth arch)
                     -- ^ The location we are jumping to
                     --
                     -- This is parsed to be of the form:
                     --   (mult * idx) + base
                     -- base is expected to be an integer.
                  -> Maybe ( BVValue arch ids (ArchAddrWidth arch)
                           , V.Vector (ArchSegmentedAddr arch)
                           )
identifyJumpTable s enclosingFun (AssignedValue (Assignment _ (ReadMem ptr _)))
    -- Turn the read address into base + offset.
   | Just (BVAdd _ offset base_val) <- valueAsApp ptr
   , Just base <- asLiteralAddr mem base_val
    -- Turn the offset into a multiple by an index.
   , Just (BVMul _ (BVValue _ mult) idx) <- valueAsApp offset
   , mult == toInteger (jumpTableEntrySize info)
    -- Find segment associated with base(if any)
    -- Check if it read only
    --
    -- The convention seems to be to store jump tables in read only memory.
  , Perm.isReadonly (segmentFlags (addrSegment base)) =
       Just (idx, V.unfoldr nextWord base)
  where
    info = archInfo s
    mem  = memory   s

    nextWord :: ArchSegmentedAddr arch
             -> Maybe (ArchSegmentedAddr arch, ArchSegmentedAddr arch)
    nextWord base
      | Right codePtr <- readAddr mem LittleEndian base
      , getFunctionEntryPoint' codePtr s == Just enclosingFun =
        Just (codePtr, base & addrOffset +~ jumpTableEntrySize info)
      | otherwise = Nothing
identifyJumpTable _ _ _ = Nothing

-- | Classifies the terminal statement in a block using discovered information.
classifyBlock :: forall arch ids
              .  (ArchConstraint arch ids, MemWidth (ArchAddrWidth arch))
              => Block arch ids
              -> DiscoveryInfo arch ids
              -> Maybe (ParsedTermStmt arch ids)
classifyBlock b interp_state =
  case blockTerm b of
    Branch c x y -> Just (ParsedBranch c x y)
    FetchAndExecute proc_state
        -- The last statement was a call.
      | Just (prev_stmts, ret_addr) <- identifyCall mem (blockStmts b) proc_state -> do
          let ip = proc_state^.boundValue ip_reg
          let fptr = case asLiteralAddr mem ip of
                       Just ip_addr -> Left ip_addr
                       Nothing      -> Right ip
          Just (ParsedCall proc_state prev_stmts fptr (Just ret_addr))

      -- Jump to concrete offset.
      | Just tgt_addr <- asLiteralAddr mem (proc_state^.boundValue ip_reg)
      , inSameFunction (labelAddr (blockLabel b)) tgt_addr interp_state ->
           Just (ParsedJump proc_state tgt_addr)

      -- Return
      | Just asgn <- identifyReturn proc_state (callStackDelta (archInfo interp_state)) ->
        let isRetLoad s =
              case s of
                AssignStmt asgn'
                  | Just Refl <- testEquality (assignId asgn) (assignId  asgn') -> True
                _ -> False
            nonret_stmts = Seq.fromList $ filter (not . isRetLoad) (blockStmts b)

        in Just (ParsedReturn proc_state nonret_stmts)

      -- Tail calls to a concrete address (or, nop pads after a non-returning call)
      | Just tgt_addr <- asLiteralAddr mem (proc_state^.boundValue ip_reg) ->
        Just (ParsedCall proc_state (Seq.fromList $ blockStmts b) (Left tgt_addr) Nothing)

      | Just (idx, nexts) <- identifyJumpTable interp_state
                                               (getFunctionEntryPoint
                                                (labelAddr (blockLabel b))
                                                interp_state)
                                               (proc_state^.boundValue ip_reg) ->
          Just (ParsedLookupTable proc_state idx nexts)

      -- Finally, we just assume that this is a tail call through a pointer
      -- FIXME: probably unsound.
      | otherwise -> Just (ParsedCall proc_state
                                      (Seq.fromList $ blockStmts b)
                                      (Right $ proc_state^.boundValue ip_reg) Nothing)

    -- rax is concrete in the first case, so we don't need to propagate it etc.
    Syscall proc_state
      | Just next_addr <- asLiteralAddr mem (proc_state^.boundValue ip_reg)
      , BVValue _ call_no   <- proc_state^.boundValue syscall_num_reg
      , Just (name, _rettype, argtypes) <-
          Map.lookup (fromInteger call_no) (spTypeInfo sysp) -> do
         let syscallRegs :: [ArchReg arch (BVType (ArchAddrWidth arch))]
             syscallRegs = syscallArgumentRegs
         let result = Just $
               ParsedSyscall
                 proc_state
                 next_addr
                 (fromInteger call_no)
                 (spName sysp)
                 name
                 (take (length argtypes) syscallRegs)
                 (spResultRegisters sysp)
         case () of
           _ | any ((/=) WordArgType) argtypes -> error "Got a non-word arg type"
           _ | length argtypes > length syscallRegs ->
                  debug DUrgent ("Got more than register args calling " ++ name
                                 ++ " in block " ++ show (blockLabel b))
                                result
           _ -> result

        -- FIXME: Should subsume the above ...
        -- FIXME: only works if rax is an initial register
      | Just next_addr <- asLiteralAddr mem (proc_state^.boundValue ip_reg)
      , Initial r <- proc_state^.boundValue syscall_num_reg
      , Just absSt <- Map.lookup (labelAddr $ blockLabel b) (interp_state ^. absState)
      , Just call_no <-
          asConcreteSingleton (absSt ^. absRegState ^. boundValue r)
      , Just (name, _rettype, argtypes) <-
          Map.lookup (fromInteger call_no) (spTypeInfo sysp) -> do
         let syscallRegs :: [ArchReg arch (BVType (ArchAddrWidth arch))]
             syscallRegs = syscallArgumentRegs
         let result = Just $
               ParsedSyscall
                 proc_state
                 next_addr
                 (fromInteger call_no)
                 (spName sysp)
                 name
                 (take (length argtypes) syscallRegs)
                 (spResultRegisters sysp)
         case () of
           _ | any ((/=) WordArgType) argtypes -> error "Got a non-word arg type"
           _ | length argtypes > length syscallRegs ->
                 debug DUrgent ("Got more than register args calling " ++ name
                                ++ " in block " ++ show (blockLabel b))
                       result
           _ -> result


      | Just next_addr <- asLiteralAddr mem (proc_state^.boundValue ip_reg) ->
          debug DUrgent ("Unknown syscall in block " ++ show (blockLabel b)
                         ++ " syscall number is "
                         ++ show (pretty $ proc_state^.boundValue syscall_num_reg)
                        )
          Just $ ParsedSyscall proc_state next_addr 0 (spName sysp) "unknown"
                               syscallArgumentRegs
                               (spResultRegisters sysp)
      | otherwise -> error "shouldn't get here"
  where
    mem = memory interp_state
    sysp = syscallPersonality interp_state

getClassifyBlock :: (ArchConstraint arch ids, MemWidth (ArchAddrWidth arch))
                 => ArchLabel arch
                 -> DiscoveryInfo arch ids
                 -> Maybe (Block arch ids, Maybe (ParsedTermStmt arch ids))
getClassifyBlock lbl interp_state = do
  b <- lookupBlock (interp_state ^. blocks) lbl
  return (b, classifyBlock b interp_state)
