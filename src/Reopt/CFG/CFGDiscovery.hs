------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.CFGDiscovery
-- Description      : Control Flow Graph discovery support
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Simon Winwood <sjw@galois.com>
-- Stability        : provisional
--
-- This contains an implementation of a CFG discovery algorithm based
-- upon an interleaved abstract interpretation (currently unsound)
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Reopt.CFG.CFGDiscovery
       ( FinalCFG(..)
       , cfgFromAddrs
       , assignmentAbsValues
       ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import           Data.Int
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import           Debug.Trace
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Reopt.Analysis.AbsState
import qualified Reopt.Analysis.Domains.StridedInterval as SI
import           Reopt.CFG.Implementation
import           Reopt.CFG.InterpState
import           Reopt.CFG.Recovery
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Memory

------------------------------------------------------------------------
-- Utilities

-- | Run a computation over a part of the current state.
subMonad :: (MonadState s m)
         => Simple Lens s t
         -> State t r
         -> m r
subMonad l m = l %%= runState m

liftEither :: StateT s (Either e) a -> State s (Either e a)
liftEither m = state go
  where
    go s = case runStateT m s of
             Left e       -> (Left e,  s)
             Right (r, t) -> (Right r, t)

doMaybe :: Monad m => m (Maybe a) -> b -> (a -> m b) -> m b
doMaybe m n j = do
  ma <- m
  case ma of
    Nothing -> return n
    Just a -> j a

-- | Get code pointers out of a abstrcct value.
concretizeAbsCodePointers :: Memory Word64 -> AbsValue (BVType 64) -> [CodeAddr]
concretizeAbsCodePointers mem (FinSet s) =
  filter (isCodeAddr mem) $ fromInteger <$> Set.toList s
concretizeAbsCodePointers mem (CodePointers s) =
  filter (isCodeAddr mem) $ Set.toList s
concretizeAbsCodePointers mem (StridedInterval s) =
  filter (isCodeAddr mem) $ fromInteger <$> SI.toList s
concretizeAbsCodePointers mem _ = []

------------------------------------------------------------------------
-- Block discovery

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock' :: MonadState InterpState m => BlockLabel -> m (Maybe Block)
lookupBlock' lbl = uses blocks (`lookupBlock` lbl)

getAbsBlockState :: CodeAddr -> State InterpState AbsBlockState
getAbsBlockState a = uses absState $ lookupAbsBlock a

blockOverlaps :: CodeAddr -> Maybe BlockRegion -> Bool
blockOverlaps a Nothing = True
blockOverlaps a (Just br) = a < brEnd br

-- | Mark this as the start of a block.
markBlockStart :: CodeAddr -> AbsBlockState -> InterpState -> InterpState
markBlockStart addr ab s =
  -- Lookup block just before this address
  case Map.lookupLT addr (s^.blocks) of
    -- If that block overlaps with the address
    Just (l,br) | addr `blockOverlaps` br ->
      s   -- Get block for addr
        & tryDisassembleAddr addr ab
          -- Get block for old block
        & tryDisassembleAddr l    (lookupAbsBlock l    (s^.absState))
          -- Add old block to frontier
        & frontier %~ Map.insert l BlockSplit
    _ ->
      s & tryDisassembleAddr addr ab

-- | This is the worker for getBlock, in the case that we have not already
-- read the block.
tryDisassembleAddr :: CodeAddr
                      -> AbsBlockState
                      -> InterpState
                      -> InterpState
tryDisassembleAddr addr ab s0 = do
  -- Get FPU top
  let Just t = getAbsX87Top ab
  -- Create explore loc
  let loc = ExploreLoc { loc_ip = addr
                       , loc_x87_top = t
                       }
  -- Attempt to disassemble block.
  -- Get memory so that we can decode from it.
  let block_addrs = s0^.blocks
  -- Returns true if we are not at the start of a block.
  -- This is used to stop the disassembler when we reach code
  -- that is part of a new block.
  let not_at_block = (`Map.notMember` block_addrs)
  let mem = memory s0
  let gs0 = s0^.genState
  case runStateT (disassembleBlock mem not_at_block loc) gs0 of
    Left _e ->
      s0 & blocks %~ Map.insert addr Nothing
    Right ((bs, next_ip), gs) -> assert (next_ip > addr) $ do
      let block_map = Map.fromList [ (labelIndex (blockLabel b), b) | b <- bs ]
      -- Add block region to blocks.
      let br = BlockRegion { brEnd = next_ip
                           , brBlocks = block_map
                           }
      s0 & genState .~ gs
         & blocks   %~ Map.insert addr (Just $! br)

-- | This is the worker for getBlock, in the case that we have not already
-- read the block.
reallyGetBlockList :: AbsState
                   -> InterpState
                   -> InterpState
reallyGetBlockList m s0 = Map.foldrWithKey' tryDisassembleAddr s0 m

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all it's children are
-- present in the cfg (assuming successful disassembly)
getBlock :: CodeAddr -> State InterpState (Maybe Block)
getBlock addr = do
  m_b <- use blocks
  case Map.lookup addr m_b of
    Just mbr ->
      return $! Map.lookup 0 . brBlocks =<< mbr
    -- We haven't tried to get this block.
    Nothing -> error $ "getBlock called on block " ++ showHex addr " we have not seen."


------------------------------------------------------------------------
-- Transfer stmts

transferStmt :: Monad m
             => Stmt
             -> StateT AbsProcessorState m ()
transferStmt stmt =
  case stmt of
    AssignStmt a -> do
      modify $ addAssignment a
    Write (MemLoc addr _) v -> do
      modify $ addMemWrite addr v
    _ -> return ()

fnRegCodePointers :: X86State AbsValue -> [CodeAddr]
fnRegCodePointers s = Set.toList (foldX86StateValue codePointerSet s)

stackCodePointers :: AbsBlockStack -> [CodeAddr]
stackCodePointers stk =
  [ ptr
  | (offset, StackEntry _ v) <- Map.toList stk
  , offset /= 0
  , ptr <- Set.toList (codePointerSet v)
  ]

defBlockState :: Memory Word64 -> CodeAddr -> AbsBlockState
defBlockState mem addr =
  top & setAbsIP mem addr
      & absX86State . register N.rsp .~ concreteStackOffset 0
      & absX86State . x87TopReg .~ abstractSingleton mem knownNat 7

newtype HexWord = HexWord Word64

instance Show HexWord where
  showsPrec _ (HexWord w) = showHex w

showHexList :: [Word64] -> String
showHexList l = show (fmap HexWord l)

-- | Insert keys into map with given value, keeping old value if they are alread there.
insertKeysIntoMap :: (Ord k, Foldable t) => t k -> a -> Map k a -> Map k a
insertKeysIntoMap kl v m0 = Fold.foldl' (\m k -> Map.insertWith (const id) k v m) m0 kl

-- | Mark a escaped code pointer as a function entry.
markAddrAsFunction :: Word64 -> InterpState -> InterpState
markAddrAsFunction addr s
  | addr == 0 = s
  | Set.member addr (s^.functionEntries) = s
  | otherwise = trace ("Found function entry " ++ showHex addr ".") $
     let mem = memory s
         ab = defBlockState mem addr
         -- TODO: Figure out what we want to do with entries that used to think this
         -- was a function.
         --prev = fromMaybe Set.empty $ Map.lookup addr (s^.reverseEdges)
         s' = s & markBlockStart addr ab
                & absState        %~ Map.insert addr ab
                & functionEntries %~ Set.insert addr
                & function_frontier %~ Set.insert addr
      in s'

recordFunctionAddrs :: Memory Word64 -> AbsValue (BVType 64) -> State InterpState ()
recordFunctionAddrs mem av = do
  let addrs = concretizeAbsCodePointers mem av
  modify $ \s0 -> foldl' (flip markAddrAsFunction) s0 addrs

recordWriteStmt :: BlockLabel -> AbsProcessorState -> Stmt -> State InterpState ()
recordWriteStmt lbl regs (Write (MemLoc addr _) v)
  | Just Refl <- testEquality (valueType v) (knownType :: TypeRepr (BVType 64))
  , av <- transferValue regs v = do
    mem <- gets memory
    trace ("Found escaped code pointers via write of " ++ show addr ++ " to memory.") $ do
    recordFunctionAddrs mem av
recordWriteStmt _ _ _ = return ()

transferStmts :: Monad m => AbsProcessorState -> [Stmt] -> m AbsProcessorState
transferStmts r stmts = execStateT (mapM_ transferStmt stmts) r

finalBlockState :: Memory Word64 -> CodeAddr -> FinalCFG -> AbsBlockState
finalBlockState mem a g
  | Set.member a (finalCodePointersInMem g) = defBlockState mem a
  | otherwise = lookupAbsBlock a (finalAbsState g)

-- | Generate map that maps each assignment in the CFG to the abstract value
-- associated with it.
assignmentAbsValues :: Memory Word64 -> FinalCFG -> MapF Assignment AbsValue
assignmentAbsValues mem fg = foldl' go MapF.empty (Map.elems (g^.cfgBlocks))
  where g = finalCFG fg

        go :: MapF Assignment AbsValue
           -> Block
           -> MapF Assignment AbsValue
        go m0 b =
          case blockLabel b of
            GeneratedBlock a 0 -> insBlock b (initAbsProcessorState mem (finalBlockState mem a fg)) m0
            _ -> m0

        insBlock :: Block
                 -> AbsProcessorState
                 -> MapF Assignment AbsValue
                 -> MapF Assignment AbsValue
        insBlock b r0 m0 =
            case blockTerm b of
              Branch _ lb rb -> do
                let Just l = findBlock g lb
                let Just r = findBlock g rb
                insBlock l final $
                  insBlock r final $
                  m
              FetchAndExecute _ -> m
              Syscall _ -> m

          where final = runIdentity $ transferStmts r0 (blockStmts b)
                m = MapF.union (final^.absAssignments) m0

------------------------------------------------------------------------
-- Transfer functions

-- | Joins in the new abstract state and returns the locations for
-- which the new state is changed.
mergeIntraJump  :: BlockLabel
                  -- ^ Source label that we are jumping from.
                -> AbsBlockState
                   -- ^ Block state after executing instructions.
                -> CodeAddr
                   -- ^ Address we are trying to reach.
                -> State InterpState ()
mergeIntraJump src ab tgt = do
  -- Associate a new abstract state with the code region.
  let upd new s = do
        -- Add reverse edge
        s & reverseEdges %~ Map.insertWith Set.union tgt (Set.singleton (labelAddr src))
          & absState %~ Map.insert tgt new
          & frontier %~ Map.insert tgt (NextIP src)
  s0 <- get
  case Map.lookup tgt (s0^.absState) of
    -- We have seen this block before, so need to join and see if
    -- the results is changed.
    Just ab_old ->
      case joinD ab_old ab of
        Nothing  -> return ()
        Just new -> modify $ upd new
    -- We haven't seen this block before
    Nothing  ->
      put $ s0 & upd ab
               & markBlockStart tgt ab

-- | This updates the state of a function when returning from a function.
mergeFreeBSDSyscall :: BlockLabel
                       -- ^ Label for callee block that is making this call.
                    -> AbsBlockState
                       -- ^ Block state just before this call.
                    -> CodeAddr
                       -- ^ Address that system call should return to.
                       -- We think this belongs to the same function.
                    -> State InterpState ()
mergeFreeBSDSyscall src_lbl ab0 addr = do
  let regFn :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
      regFn r
          -- Set IPReg
        | N.IPReg <- r =
          CodePointers (Set.singleton addr)
          -- Two pointers that are modified by syscalls
        | Just Refl <- testEquality r N.rcx =
          TopV
        | Just Refl <- testEquality r N.r11 =
          TopV
          -- Carry flag and rax is used by BSD for return values.
        | Just Refl <- testEquality r N.cf =
          TopV
        | Just Refl <- testEquality r N.rax =
          TopV
          -- Keep other registers the same.
        | otherwise =
          ab0^.absX86State^.register r
  let ab = mkAbsBlockState regFn (ab0^.startAbsStack)

  -- Merge the new abstract
  mergeIntraJump src_lbl ab addr

-- | This updates the abstract information based on the assumption that
-- a called method will return to the return address, and will follow
-- x86_64 ABI.
mergeCallerReturn :: BlockLabel
                     -- ^ Label of block maing call.
                  -> AbsBlockState
                     -- ^ Block state just before call.
                  -> CodeAddr
                     -- ^ Address we will return to.
                  -> State InterpState ()
mergeCallerReturn lbl ab0 addr = do
  s <- get
  let regFn :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
      regFn r
          -- We set IPReg
        | Just Refl <- testEquality r N.IPReg =
          CodePointers (Set.singleton addr)
          -- We don't know anything about rax as it is the return value.
        | Just Refl <- testEquality r N.rax =
          TopV
          -- TODO: Transmit no value to first floating point register.
        | N.XMMReg 0 <- r =
          ab0^.absX86State^.register r
          -- TODO: Fix this (can we prove detect whether a floating point value was read)?
        | N.X87TopReg <- r =
          ab0^.absX86State^.register r
          -- Copy callee saved registers
        | Set.member (Some r) x86CalleeSavedRegisters =
          ab0^.absX86State^.register r
          -- We know nothing about other registers.
        | otherwise =
          TopV
      -- Get values below return address.
      -- TODO: Fix this; the called function may modify the stack.
  let stk = Map.filterWithKey (\k _ -> k >= 8) (ab0^.startAbsStack)
  let ab = shiftSpecificOffset regFn stk 8

  mergeIntraJump lbl ab addr

_showAbsDiff :: AbsBlockState -> AbsBlockState -> Doc
_showAbsDiff x y = vcat (pp <$> absBlockDiff x y)
  where pp (Some n) = pretty (show n) <+> pretty (x^.absX86State^.register n)
                                      <+> pretty (x^.absX86State^.register n)

-- Check floating point top.
getAbsX87Top :: Monad m => AbsBlockState -> m Int
getAbsX87Top abst =
  case asConcreteSingleton (abst^.absX86State^. x87TopReg) of
    Just v -> return (fromInteger v)
    _ -> fail "x87top is not concrete"

-- | @isrWriteTo stmt add tpr@ returns true if @stmt@ writes to @addr@
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

-- -----------------------------------------------------------------------------
-- Refining an abstract state based upon a condition

-- FIXME: if val \notin av then we should return bottom
-- @refineProcState v av s@ returns a processor state after we have
-- asserted that @v@ is contained in the set @AbsValue@.
refineProcState :: Value tp -- ^ Value in processor state
                -> AbsValue tp -- ^ Abstract value to assign value.
                -> AbsProcessorState
                -> AbsProcessorState
refineProcState (BVValue _n _val) _av regs = regs
refineProcState (Initial r) av regs =
  regs & (absInitialRegs . register r) %~ flip meet av
refineProcState (AssignedValue ass@(Assignment _ rhs)) av regs
  -- av is not a subset.
  | Nothing <- joinAbsValue av av_old = regs
  | otherwise = do
    -- Get joined abstract value.
    let av'    = meet av_old av
    -- Get registers after updating assignment value to av'
    let  regs'  = regs & (absAssignments . assignLens ass) .~ av'
    case rhs of
      -- av adds new information, we need to refine any parents
      EvalApp app -> refineApp app av' regs'
      -- no parents, but update ass
      _ -> regs'
  where
    av_old = regs ^. absAssignments ^. assignLens ass

refineApp :: App Value tp
             -> AbsValue tp
             -> AbsProcessorState
             -> AbsProcessorState
refineApp app av regs =
  case app of
   -- We specialise these to booleans for the moment
   -- BVComplement sz v
   --   | Just Refl <- testEquality sz n1
   --   , Just b    <- asConcreteSingleton av ->
   --     refineProcState v (abstractSingleton n1 (1 - b)) regs
   -- BVAnd sz l r
   --   | Just Refl <- testEquality sz n1
   --   , Just b    <- asConcreteSingleton av ->
   --     let l_regs = refineProcState l av regs
   --         r_regs = refineProcState r av regs
   --     in if b == 1 then  -- both are true, so we do a meet
   --          glb l_regs r_regs
   --        else -- one is false, so we do a join
   --          lub l_regs r_regs

   -- If we know something about the result of a trunc, we can
   -- propagate back a subvalue.
   Trunc x sz -> refineTrunc x sz av regs

   -- Assertion "r <= x"
   BVUnsignedLe r x
     | Just b    <- asConcreteSingleton av ->
       refineLeq r x b regs
   BVUnsignedLt r x
     | Just b    <- asConcreteSingleton av ->
       refineLt  r x b regs

   BVUnsignedLt l r
     | Just b    <- asConcreteSingleton av -> refineLt l r b regs

   BVUnsignedLe l r
     | Just b    <- asConcreteSingleton av -> refineLeq l r b regs

   -- FIXME: HACK
   -- This detects r - x < 0 || r - x == 0, i.e. r <= x
   BVOr _ (getAssignApp -> Just (UsbbOverflows _ r xv@(BVValue sz x) (BVValue _ 0)))
          (getAssignApp -> Just (BVEq (getAssignApp -> Just (BVAdd _ r' y)) (BVValue _ 0)))
     | Just Refl <- testEquality r r'
     , Just Refl <- testEquality y (mkLit sz (negate x))
     , Just b    <- asConcreteSingleton av ->
       refineLeq r xv b regs

   -- FIXME: HACK
   -- This detects not (r - x < 0) && not (r - x == 0), i.e. x < r
   BVAnd _ (getAssignApp -> Just (BVComplement _
                                  (getAssignApp -> Just (UsbbOverflows _ r xv@(BVValue sz x) (BVValue _ 0)))))
           (getAssignApp -> Just (BVComplement _
                                  (getAssignApp -> Just (BVEq (getAssignApp -> Just (BVAdd _ r' y)) (BVValue _ 0)))))
     | Just Refl <- testEquality r r'
     , Just Refl <- testEquality y (mkLit sz (negate x))
     , Just b    <- asConcreteSingleton av ->
       refineLt xv r b regs

  -- Mux can let us infer the condition?
   _ -> regs
  where
    getAssignApp (AssignedValue (Assignment _ (EvalApp a))) = Just a
    getAssignApp _ = Nothing

refineTrunc :: ((n + 1) <= n') =>
               Value (BVType n') -> NatRepr n -> AbsValue (BVType n)
               -> AbsProcessorState -> AbsProcessorState
refineTrunc v sz av regs = refineProcState v (subValue sz av) regs

refineULeqTrue :: Value tp
               -> Value tp
               -> AbsProcessorState
               -> AbsProcessorState
refineULeqTrue x y regs = refineProcState x x_leq (refineProcState y y_leq regs)
  where
    (x_leq, y_leq) = abstractULeq (valueType x) (transferValue regs x) (transferValue regs y)
    -- check r@(a, b)
    --   | isBottom a = flip trace r $ "Bottom in refineLeq: "
    --                  ++ show (pretty regs)
    --   | isBottom b = flip trace r $ "Bottom in refineLeq: "
    --                  ++ show (pretty regs)
    --   | otherwise  = r

refineULtTrue :: Value tp
              -> Value tp
              -> AbsProcessorState
              -> AbsProcessorState
refineULtTrue x y regs = refineProcState x x_lt (refineProcState y y_lt regs)
  where
    (x_lt, y_lt) = abstractULt (valueType x) (transferValue regs x) (transferValue regs y)

refineLeq :: Value tp
          -> Value tp
          -> Integer
          -> AbsProcessorState
          -> AbsProcessorState
refineLeq x y b regs
     -- y < x
    | b == 0     = refineULtTrue y x regs
    -- x <= y
    | otherwise  = refineULeqTrue x y regs

refineLt :: Value tp -> Value tp -> Integer -> AbsProcessorState -> AbsProcessorState
refineLt x y b regs
  -- y <= x
  | b == 0     = refineULeqTrue y x regs
  -- x < y case
  | otherwise  = refineULtTrue  x y regs

-- -- FIXME: bottom
-- refineLVal :: Simple Lens AbsProcessorState (AbsValue tp)
--               -> Value tp
--               -> AbsProcessorState
--               -> AbsProcessorState
-- refineLVal l (BVValue n val) regs =
--   -- FIXME: if val \notin absinit l then we should return bottom
--   regs & l .~ abstractSingleton n val
-- refineLVal l (Initial r) regs =
--   regs & l .~ new_v & (absInitialRegs . register r) .~ new_v
--   where
--     abs_l  = regs ^. l
--     abs_r  = regs ^. absInitialRegs ^. register r
--     new_v  = meet abs_l abs_r
-- refineLVal l (AssignedValue ass@(Assignment _ rhs)) regs =
--   regs & l .~ new_v & (absInitialRegs .  assignLens ass) .~ new_v
--   where
--     abs_l  = regs ^. l
--     abs_r  = regs ^. absInitialRegs ^. assignLens ass
--     new_v  = meet abs_l abs_r


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

-- | List of registers that a callee must save.
x86CalleeSavedRegisters :: Set (Some N.RegisterName)
x86CalleeSavedRegisters = Set.fromList $
  [ Some N.rsp
  , Some N.rbp
  , Some N.rbx
  , Some N.r12
  , Some N.r13
  , Some N.r14
  , Some N.r15
  ]

intervalForSegment :: MemSegment Word64 -> SI.StridedInterval (BVType 64)
intervalForSegment roseg =
  let base = fromIntegral $ memBase roseg
      sz   = fromIntegral $ BS.length (memBytes roseg)
   in SI.mkStridedInterval (BVTypeRepr n64) False base (base + sz - 1) 1

-- | Return true if abstract value only reads from read only memory.
intervalInReadonlyMem :: Memory Word64 -> SI.StridedInterval (BVType 64) -> Bool
intervalInReadonlyMem mem si_new =
  any (\seg  -> si_new `SI.isSubsetOf` intervalForSegment seg)
      (readonlySegments mem)


rangeInReadonlySegment :: Word64 -- ^ Start of range
                       -> Word64 -- ^ One past last index in range.
                       -> Memory Word64 -> Bool
rangeInReadonlySegment base end mem =
  case findSegment base mem of
    Nothing -> False
    Just seg -> isReadonly (memFlags seg)
             && end <= memBase seg + segmentSize seg

-- See if expression matches form expected by jump tables
matchJumpTable :: Memory Word64
               -> Value (BVType 64) -- ^ Memory address that IP is read from.
               -> Maybe (Word64, Value (BVType 64))
matchJumpTable mem read_addr
    -- Turn the read address into base + offset.
  | Just (BVAdd _ offset (BVValue _ base)) <- valueAsApp read_addr
    -- Turn the offset into a multiple by an index.
  , Just (BVMul _ (BVValue _ 8) index) <- valueAsApp offset
  , isReadonlyAddr mem (fromInteger base) = do
    Just (fromInteger base, index)
matchJumpTable _ _ =
    Nothing

-- Returns the index bounds for
getJumpTableBounds :: Memory Word64 -- ^ State of memory
                   -> AbsProcessorState       -- ^ Current processor registers.
                   -> Word64
                   -> Value (BVType 64) -- ^ Index in jump table
                   -> Maybe Word64 -- ^ One past last index in jump table.
getJumpTableBounds mem regs base index
    -- Get range for the index.
  | let abs_value = transferValue regs index
  , StridedInterval index_interval  <- abs_value
    -- Check that relevant interval is completely contained within a read-only
    -- read only range in the memory.
  , SI.StridedInterval _ index_base index_range index_stride <-
        trace "getJumpTable3" $ index_interval
  , index_end <- index_base + (index_range + 1) * index_stride
  , read_end <- toInteger base + 8 * index_end
  , rangeInReadonlySegment base (fromInteger read_end) mem =

    -- Get the addresses associated.
    trace ("Fixed table " ++ showHex base (" [" ++ shows index "]")) $
      Just $! fromInteger index_end
getJumpTableBounds _ _ _ _ = Nothing

  -- -- basically, (8 * x) + addr
  -- | AssignedValue (Assignment _ (Read (MemLoc ptr _))) <- conc^.curIP
  -- , AssignedValue (Assignment _ (EvalApp (BVAdd _ lhs (BVValue _ base)))) <- ptr
  -- , AssignedValue (Assignment _ (EvalApp (BVMul _ (BVValue _ 8) xv))) <- lhs
  -- , AssignedValue x <- xv
  -- , Just vs <- concretize (regs^.absAssignments^.assignLens x) =
  --   let addrs = Set.map (\off -> fromIntegral $ base + off * 8) vs
  --       bptrs = Set.map (\addr -> fromIntegral $ fromJust
  --                                 $ Map.lookup addr wordMap) addrs
  --   in
  --   if all (isRODataPointer mem) $ Set.toList addrs
  --      then trace ("getJumpTable: " ++ show (pretty x)
  --                  ++ " " ++ show (Set.map (flip showHex "") bptrs)) $
  --           Just bptrs
  --      else Nothing
  -- | otherwise = Nothing

-- | This is designed to detect returns from the X86 representation.
-- It pattern matches on a X86State to detect if it read its instruction
-- pointer from an address that is 8 below the stack pointer.
recoverIsReturnStmt :: X86State Value -> Bool
recoverIsReturnStmt s = do
  let next_ip = s^.register N.rip
      next_sp = s^.register N.rsp
  case next_ip of
    AssignedValue (Assignment _ (Read (MemLoc ip_stored_here _))) ->
      let (ip_base, ip_off) = asBaseOffset ip_stored_here
          (sp_base, sp_off) = asBaseOffset next_sp
       in ip_base == sp_base
          && ip_off + 8 == sp_off
    _ -> False

transferBlock :: Block   -- ^ Block to start from.
              -> AbsProcessorState -- ^ Registers at this block.
              -> State InterpState ()
transferBlock b regs = do
  let lbl = blockLabel b
  trace ("transferBlock " ++ show lbl) $ do
  mem <- gets memory
  regs' <- transferStmts regs (blockStmts b)
  -- FIXME: we should propagate c back to the initial block, not just b
  case blockTerm b of
    Branch c lb rb -> do
      mapM_ (recordWriteStmt lbl regs') (blockStmts b)
      Just l <- lookupBlock' lb
      let  l_regs = refineProcState c (abstractSingleton mem n1 1) regs'
      Just r <- lookupBlock' rb
      let r_regs = refineProcState c (abstractSingleton mem n1 0) regs'
      -- We re-transfer the stmts to propagate any changes from
      -- the above refineProcState.  This could be more efficient by
      -- tracking what (if anything) changed.  We also might
      -- need to keep going back and forth until we reach a
      -- fixpoint
      transferBlock l =<< transferStmts l_regs (blockStmts b)
      transferBlock r =<< transferStmts r_regs (blockStmts b)

    Syscall s' -> do
      mapM_ (recordWriteStmt lbl regs') (blockStmts b)
      let abst = finalAbsBlockState regs' s'
      let ips = concretizeAbsCodePointers mem (abst^.absX86State^.curIP)
      -- Merge system call result with possible next IPs.
      Fold.forM_ ips $ \addr -> do
        mergeFreeBSDSyscall lbl abst addr

    FetchAndExecute s' -> do
      -- See if next statement appears to end with a call.
      -- We define calls as statements that end with a write that
      -- stores the pc to an address.
      case () of
          -- The last statement was a call.
        _ | Just (prev_stmts, ret) <- identifyCall mem (blockStmts b) s' -> do
            trace ("Call statement") $ do
            Fold.mapM_ (recordWriteStmt lbl regs') prev_stmts
            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- Merge caller return information
            mergeCallerReturn lbl abst ret
            -- Look for new ips.
            recordFunctionAddrs mem (abst^.absX86State^.curIP)

          -- This block ends with a return.
          | recoverIsReturnStmt s' -> do
            trace ("Return statement") $ do
            mapM_ (recordWriteStmt lbl regs') (blockStmts b)
-- Note: In the code below, we used to try to determine the return target and
-- merge information into the return address.  We no longer do that as it did not
-- appear to help discover new blocks, and we would like to minimize inter-procedural
-- analysis.
--            let abst = finalAbsBlockState regs' s'
--            let ips = concretizeAbsCodePointers mem (abst^.absX86State^.curIP)
--            -- Look for new ips.
--            Fold.forM_ ips $ \addr -> do
--              mergeCalleeReturn lbl mem abst addr

          -- Jump to concrete offset.
          | BVValue _ tgt_addr_as_integer <- s'^.register N.rip ->
            trace ("Concrete jump") $ do

            let tgt_addr = fromInteger tgt_addr_as_integer
            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- Try to check for a tail call.
            this_fn <- gets $ getFunctionEntryPoint (labelAddr lbl)
            tgt_fn  <- gets $ getFunctionEntryPoint tgt_addr
            -- When the jump appears to go to another function, assume the
            -- jump if a tail call.
            if (this_fn /= tgt_fn) then do
              -- Check that the current stack height is correct so that a
              -- tail call when go to the right place.
              -- TODO: Add check to ensure stack height is correct.
              trace ("Found jump to concrete address after function " ++ showHex tgt_fn ".") $ do
              modify $ markAddrAsFunction tgt_addr
            else do
              -- Merge block state.
              mergeIntraJump lbl (abst & setAbsIP mem tgt_addr) tgt_addr


          -- Block ends with what looks like a jump table.
          | AssignedValue (Assignment _ (Read (MemLoc ptr _)))
                <- trace "try jump table" $ s'^.curIP
            -- Attempt to compute interval of addresses interval is over.
          , Just (base, index) <- matchJumpTable mem ptr -> do
            trace ("Found jump table at " ++ show lbl) $ do

            mapM_ (recordWriteStmt lbl regs') (blockStmts b)


            -- Try to compute jump table bounds
            let mread_end = getJumpTableBounds mem regs' base index

            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- This function resolves jump table entries.
            -- It is a recursive function that has an index into the jump table.
            -- If the current index can be interpreted as a intra-procedural jump,
            -- then it will add that to the current procedure.
            -- This returns the last address read.
            let resolveJump :: [Word64] -- ^ Addresses in jump table in reverse order
                            -> Word64 -- ^ Current index
                            -> State InterpState [Word64]
                resolveJump prev idx | Just idx == mread_end = do
                  -- Stop jump table when we have reached computed bounds.
                  return (reverse prev)
                resolveJump prev idx = do
                  let read_addr = base + 8 * idx
                  interpState <- get
                  case memLookupWord64 mem pf_r read_addr of
                    Right tgt_addr
                      | isCodeAddr mem tgt_addr
                      , inSameFunction (labelAddr lbl) tgt_addr interpState -> do

                        trace ("Trying IP " ++ showHex tgt_addr ".") $ do
                        mergeIntraJump lbl (abst & setAbsIP mem tgt_addr) tgt_addr
                        resolveJump (tgt_addr:prev) (idx+1)
                    _ -> do
                      trace ("Stop jump table: " ++ show idx ++ " " ++ show mread_end) $ do
                      return (reverse prev)
            read_addrs <- resolveJump [] 0
            let last_index = fromIntegral (length read_addrs)
            globalDataMap %= Map.insert base (JumpTable $! (Just $! base + 8 * last_index))

          -- We have a jump that we do not understand.
          -- This could be a tail call.
          | otherwise -> trace "Uninterpretable jump" $ do
            Just br <- Map.lookup (labelAddr lbl) <$> use blocks
            mapM_ (recordWriteStmt lbl regs') (blockStmts b)
            let abst = finalAbsBlockState regs' s'
            recordFunctionAddrs mem (abst^.absX86State^.curIP)

transfer :: CodeAddr -> State InterpState ()
transfer addr = trace ("transfer " ++ showHex addr ".") $ do
  mem <- gets memory

  doMaybe (getBlock addr) () $ \root -> do
    ab <- getAbsBlockState addr
    fn_addr <- gets $ getFunctionEntryPoint addr
--    trace ("In function at " ++ showHex fn_addr ".") $ do
    transferBlock root (initAbsProcessorState mem ab)

------------------------------------------------------------------------
-- Main loop

data FinalCFG = FinalCFG { finalCFG :: !CFG
                         , finalAbsState :: !AbsState
                         , finalCodePointersInMem :: !(Set CodeAddr)
                         , finalFailedAddrs :: !(Set CodeAddr)
                         , finalFunctions :: ![Function]
                         }

mkCFG :: Map CodeAddr (Maybe BlockRegion) -> CFG
mkCFG m = Map.foldlWithKey' go emptyCFG m
  where go g addr (Just br) = insertBlocksForCode addr (brEnd br) l g
          where l = Map.elems (brBlocks br)
        go g addr Nothing = g

ppFunctionEntries :: [CodeAddr] -> String
ppFunctionEntries l = unlines (pp <$> l)
  where pp a = "discovered function entry " ++ showHex a ""

ppGlobalData :: [(CodeAddr, GlobalDataInfo)] -> String
ppGlobalData l = unlines (pp <$> l)
  where pp (a,d) = "global " ++ showHex a (" " ++ show d)

mkFinalCFG :: InterpState -> FinalCFG
mkFinalCFG s =
  case traverse (recoverFunction s) (Set.toList (s^.functionEntries)) of
    Left msg -> error msg
    Right fns ->
      trace (ppFunctionEntries (Set.toList (s^.functionEntries))) $
      trace (ppGlobalData (Map.toList (s^.globalDataMap))) $
      FinalCFG { finalCFG = mkCFG (s^.blocks)
               , finalAbsState = s^.absState
               , finalCodePointersInMem = s^.functionEntries
               , finalFailedAddrs = Set.empty
               , finalFunctions = fns
               }

explore_frontier :: InterpState -> InterpState
explore_frontier st =
  case Map.minViewWithKey (st^.frontier) of
    Nothing ->
      case Set.minView (st^.function_frontier) of
        Nothing -> st
        Just (addr, next_roots) ->
          let st_pre = st & function_frontier .~ next_roots
              st_post = flip execState st_pre $ transfer addr
           in explore_frontier st_post
    Just ((addr,rsn), next_roots) ->
      let st_pre = st & frontier .~ next_roots
          st_post = flip execState st_pre $ transfer addr
       in explore_frontier st_post

-- | Attempt to determine if this address is in a jump table.
addrInJumpTable :: InterpState -> Word64 -> Bool
addrInJumpTable s a =
  case Map.lookupLE a (s^.globalDataMap) of
    Just (_,JumpTable _) -> True
    _ -> False

emptyAbsState :: Memory Word64 -> CodeAddr -> AbsState
emptyAbsState mem start = Map.singleton start (defBlockState mem start)


cfgFromAddrs :: Memory Word64
                -- ^ Memory to use when decoding instructions.
             -> [CodeAddr]
                -- ^ Location to start disassembler form.
             -> FinalCFG
cfgFromAddrs mem init_addrs = g
  where
--    fn = recoverFunction s3 0x422b10
--    0x422030

--    functionAddrs = Set.toList (s3^.functionEntries)
--    stack_heights = recoverFunction s3 <$> functionAddrs


--    ppAddr a = showHex a "\n"
--    code_pointers = filter (isCodeAddr mem) (memAsWord64le mem)
    global_data = Map.fromList
      [ (v, ReferencedValue)
      | (_,v) <- memAsWord64le_withAddr mem
        -- Check this is readable, non-executable data
      , addrPermissions v mem .&. (pf_r .|. pf_x) == pf_r
      ]
    init_abs_state = Map.fromList
                     [ (a, defBlockState mem a)
                     | a <- init_addrs
                     ]


    s0 = emptyInterpState mem
       & functionEntries .~ Set.fromList init_addrs
       & absState .~ init_abs_state
       & function_frontier .~ Set.fromList init_addrs
       & globalDataMap .~ global_data
       & reallyGetBlockList init_abs_state

    s1 = explore_frontier s0

    -- Add in code pointers from memory.
    go s (a,v)
        -- Skip values not in memory.
      | not (isCodeAddr mem v) = s
        -- Skip this if it is already a known function.
      | Set.member v (s^.functionEntries) = s
        -- Ignore entries found in read only segments.
        -- They have a high liklyhood of being elements of jump tables
      | isReadonlyAddr mem a = s
        -- Check if we already found this
      | Map.member v (s^.blocks) =
        trace ("Identified function entry "
                ++ showHex v (" due to global store at " ++ showHex a ".")) $
        markAddrAsFunction v s
      | otherwise =
        trace ("Found function entry from memory" ++ showHex v " at " ++ showHex a ".") $
        markAddrAsFunction v s

    -- Explore data values
    s2  = foldl' go s1 (memAsWord64le_withAddr mem)
    s3 = explore_frontier s2
    g = mkFinalCFG s3

------------------------------------------------------------------------
-- Function recovery

asFixedStackAddrOffset :: Value (BVType 64) -> Maybe Int64
asFixedStackAddrOffset addr = do
  asInt64Constant =<< asStackAddrOffset addr
