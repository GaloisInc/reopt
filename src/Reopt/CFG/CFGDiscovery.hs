{-
Module           : Reopt.Semantics.CFGDiscovery
Copyright        : (c) Galois, Inc 2015
Maintainer       : Joe Hendrix <jhendrix@galois.com>, Simon Winwood <sjw@galois.com>

This contains an implementation of a CFG discovery algorithm based upon an
interleaved abstract interpretation.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.CFGDiscovery
       ( mkCFG
       , cfgFromAddrs
       , assignmentAbsValues
       ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Reopt.Analysis.AbsState
import qualified Reopt.Analysis.Domains.StridedInterval as SI
import           Reopt.CFG.DiscoveryInfo
import           Reopt.CFG.Implementation
import           Reopt.CFG.Representation
import           Reopt.Machine.SysDeps.Types
import           Reopt.Machine.Types
import           Reopt.Machine.X86State
import           Reopt.Object.Memory
import           Reopt.Utils.Debug
import           Reopt.Utils.Hex

------------------------------------------------------------------------
-- Utilities

doMaybe :: Monad m => m (Maybe a) -> b -> (a -> m b) -> m b
doMaybe m n j = do
  ma <- m
  case ma of
    Nothing -> return n
    Just a -> j a

-- | Get code pointers out of a abstract value.
concretizeAbsCodePointers :: Memory Word64 -> AbsValue (BVType 64) -> [CodeAddr]
concretizeAbsCodePointers mem (FinSet s) =
  filter (isCodeAddr mem) $ fromInteger <$> Set.toList s
concretizeAbsCodePointers mem (CodePointers s) =
  filter (isCodeAddr mem) $ Set.toList s
  -- FIXME: this is dangerous !!
concretizeAbsCodePointers _mem StridedInterval{} = [] -- FIXME: this case doesn't make sense
  -- debug DCFG ("I think these are code pointers!: " ++ show s) $ []
  -- filter (isCodeAddr mem) $ fromInteger <$> SI.toList s
concretizeAbsCodePointers _mem _ = []

{-
-- | Insert keys into map with given value, keeping old value if they are alread there.
insertKeysIntoMap :: (Ord k, Foldable t) => t k -> a -> Map k a -> Map k a
insertKeysIntoMap kl v m0 = Fold.foldl' (\m k -> Map.insertWith (const id) k v m) m0 kl
-}

-- | @deleteMapRange l h m@ deletes all entries with keys greater than @l@ and
-- less than @h@.
deleteMapRange :: Ord k => Maybe k -> Maybe k -> Map k v -> Map k v
deleteMapRange (Just l) (Just h) m =
  case Map.splitLookup l m of
    (lm, Nothing, hm) -> Map.union lm (deleteMapLessThan h hm)
    (lm, Just v,  hm) -> Map.union (Map.insert l v lm) (deleteMapLessThan h hm)
deleteMapRange (Just l) Nothing  m = deleteMapGreaterThan l m
deleteMapRange Nothing  (Just h) m = deleteMapLessThan h m
deleteMapRange Nothing  Nothing  m = m

-- | @deleteMapGreaterThan k m@ returns a map with all keys greater than @k@ in @m@ deleted.
deleteMapGreaterThan :: Ord k => k -> Map k v -> Map k v
deleteMapGreaterThan k m =
  case Map.splitLookup k m of
    (lm, Nothing, _) -> lm
    (lm, Just v, _)  -> Map.insert k v lm

-- | @deleteMapLessThan k m@ returns a map with all keys less than @k@ in @m@ deleted.
deleteMapLessThan :: Ord k => k -> Map k v -> Map k v
deleteMapLessThan k m =
  case Map.splitLookup k m of
    (_, Nothing, hm) -> hm
    (_, Just v, hm) -> Map.insert k v hm

maybeSetInsert :: Ord a => Maybe a -> Set a -> Set a
maybeSetInsert (Just k) s = Set.insert k s
maybeSetInsert Nothing  s = s

{-
deleteSetRange :: Ord a => Maybe a -> Maybe a -> Set a -> Set a
deleteSetRange (Just l) (Just h) s =
  case Set.splitMember l s of
    (ls, False, hs) -> Set.union ls                (deleteSetLessThan h hs)
    (ls, True,  hs) -> Set.union (Set.insert l ls) (deleteSetLessThan h hs)
deleteSetRange (Just l) Nothing  s = deleteSetGreaterThan l s
deleteSetRange Nothing  (Just h) s = deleteSetLessThan    h s
deleteSetRange Nothing  Nothing  s = s

-- | @deleteSetGreaterThan k m@ returns a set with all keys greater than @k@ in @m@ deleted.
deleteSetGreaterThan :: Ord k => k -> Set k -> Set k
deleteSetGreaterThan k m =
  case Set.splitMember k m of
    (lm, False, _) -> lm
    (lm, True,  _) -> Set.insert k lm

-- | @deleteSetLessThan k m@ returns a map with all keys less than @k@ in @m@ deleted.
deleteSetLessThan :: Ord k => k -> Set k -> Set k
deleteSetLessThan k m =
  case Set.splitMember k m of
    (_, False, hm) -> hm
    (_, True,  hm) -> Set.insert k hm
-}

------------------------------------------------------------------------
-- Block discovery

-- | Does a simple lookup in the cfg at a given DecompiledBlock address.
lookupBlock' :: MonadState DiscoveryInfo m => BlockLabel Word64 -> m (Maybe (Block X86_64))
lookupBlock' lbl = uses blocks (`lookupBlock` lbl)

getAbsBlockState :: CodeAddr -> State DiscoveryInfo AbsBlockState
getAbsBlockState a = uses absState $ lookupAbsBlock a

blockOverlaps :: CodeAddr -> Maybe BlockRegion -> Bool
blockOverlaps _ Nothing = True
blockOverlaps a (Just br) = a < brEnd br

-- | Mark this as the start of a block.
markBlockStart :: CodeAddr -> AbsBlockState -> DiscoveryInfo -> DiscoveryInfo
markBlockStart addr ab s = do
  -- Lookup block just before this address
  case Map.lookupLT addr (s^.blocks) of
    -- If that block overlaps with the address
    Just (l,br) | addr `blockOverlaps` br -> do
      let l_start = getFunctionEntryPoint l s
          a_start = getFunctionEntryPoint addr s
--          l_high  = Set.lookupGT addr (s^.functionEntries)
--          a_high  = Set.lookupGT addr (s^.functionEntries)
          -- Get block for addr
      s & tryDisassembleAddr addr ab
          -- Get block for old block
        & tryDisassembleAddr l    (lookupAbsBlock l (s^.absState))
          -- Add function starts to split to frontier
          -- This will result in us re-exploring l_start and a_start
          -- once the current function is done.
        & function_frontier %~ Set.insert l_start . Set.insert a_start
    _ ->
      s & tryDisassembleAddr addr ab

-- | This is the worker for getBlock, in the case that we have not already
-- read the block.
tryDisassembleAddr :: CodeAddr
                      -> AbsBlockState
                      -> DiscoveryInfo
                      -> DiscoveryInfo
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
  let global_state = s0^.genState
  -- Build state for exploring this.
  case disassembleBlock global_state mem not_at_block loc of
    Left e ->
      debug DCFG ("Failed to disassemble block at " ++ showHex addr " " ++ show e) $
      s0 & blocks %~ Map.insert addr Nothing
    Right (bs, next_ip, gs) -> assert (next_ip > addr) $ do
      debug DCFG (show $ vcat $ map pretty bs) $ do
      let block_map = Map.fromList [ (labelIndex (blockLabel b), b) | b <- bs ]
      -- Add block region to blocks.
      let br = BlockRegion { brEnd = next_ip
                           , brBlocks = block_map
                           }
      s0 & genState .~ gs
         & blocks   %~ Map.insert addr (Just $! br)

-- | This is the worker for getBlock, in the case that we have not already
-- read the block.
reallyGetBlockList :: AbsStateMap
                   -> DiscoveryInfo
                   -> DiscoveryInfo
reallyGetBlockList m s0 = Map.foldrWithKey' tryDisassembleAddr s0 m

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all it's children are
-- present in the cfg (assuming successful disassembly)
getBlock :: CodeAddr -> State DiscoveryInfo (Maybe (Block X86_64))
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
             => Stmt X86_64
             -> StateT AbsProcessorState m ()
transferStmt stmt =
  case stmt of
    AssignStmt a -> do
      modify $ addAssignment a
    WriteMem addr v -> do
      modify $ addMemWrite addr v
    _ -> return ()

{-
fnRegCodePointers :: X86State AbsValue -> [CodeAddr]
fnRegCodePointers s = Set.toList (foldX86StateValue codePointerSet s)

stackCodePointers :: AbsBlockStack -> [CodeAddr]
stackCodePointers stk =
  [ ptr
  | (offset, StackEntry _ v) <- Map.toList stk
  , offset /= 0
  , ptr <- Set.toList (codePointerSet v)
  ]
-}

-- | The abstract state for a function begining at a given address.
fnBlockState :: Memory Word64 -> CodeAddr -> AbsBlockState
fnBlockState mem addr = do
  let Just abst' = top & setAbsIP (isCodeAddrOrNull mem) addr
   in abst'
      & absX86State . boundValue sp_reg .~ concreteStackOffset addr 0
        -- x87 top registe points to top of stack.
      & absX86State . x87TopReg         .~ FinSet (Set.singleton 7)
      & absX86State . boundValue df_reg .~ FinSet (Set.singleton 0)
      & startAbsStack .~ Map.singleton 0 (StackEntry (BVTypeRepr n64) ReturnAddr)

newtype HexWord = HexWord Word64

instance Show HexWord where
  showsPrec _ (HexWord w) = showHex w

{-
showHexList :: [Word64] -> String
showHexList l = show (fmap HexWord l)
-}

-- | Mark a escaped code pointer as a function entry.
markAddrAsFunction :: Word64 -> DiscoveryInfo -> DiscoveryInfo
markAddrAsFunction addr s
  | addr == 0 = s
  | Set.member addr (s^.functionEntries) = s
  | otherwise = debug DCFG ("Found function entry " ++ showHex addr ".") $ do
     let mem = memory s
     let low = Set.lookupLT addr (s^.functionEntries)
     let _high = Set.lookupGT addr (s^.functionEntries)
     let s' = s & markBlockStart addr (fnBlockState mem addr)
                & absState %~ Map.insert addr (fnBlockState mem addr)
                & functionEntries %~ Set.insert addr
                & function_frontier %~ maybeSetInsert low . Set.insert addr
     s'

recordFunctionAddrs :: BlockLabel Word64
                    -> Memory Word64
                    -> AbsValue (BVType 64)
                    -> State DiscoveryInfo ()
recordFunctionAddrs lbl mem av = do
  let addrs = concretizeAbsCodePointers mem av
  debugM DCFG (show lbl ++ ": Adding function entries " ++ intercalate ", " (map (flip showHex "") addrs))
  modify $ \s0 -> foldl' (flip markAddrAsFunction) s0 addrs

recordWriteStmt :: BlockLabel Word64
                -> AbsProcessorState
                -> Stmt X86_64
                -> State DiscoveryInfo ()
recordWriteStmt lbl regs (WriteMem _addr v)
  | Just Refl <- testEquality (typeRepr v) (knownType :: TypeRepr (BVType 64))
  , av <- transferValue regs v = do
    mem <- gets memory
    recordFunctionAddrs lbl mem av
recordWriteStmt _ _ _ = return ()

transferStmts :: Monad m
              => AbsProcessorState
              -> [Stmt X86_64]
              -> m AbsProcessorState
transferStmts r stmts = execStateT (mapM_ transferStmt stmts) r

-- | Generate map that maps each assignment in the CFG to the abstract value
-- associated with it.
assignmentAbsValues :: Memory Word64
                    -> CFG X86_64
                    -> AbsStateMap
                    -> MapF (Assignment X86_64) AbsValue
assignmentAbsValues mem g absm = foldl' go MapF.empty (Map.elems (g^.cfgBlocks))
  where go :: MapF (Assignment X86_64) AbsValue
           -> Block X86_64
           -> MapF (Assignment X86_64) AbsValue
        go m0 b =
          case blockLabel b of
            GeneratedBlock a 0 -> do
              let abs_state =
                    initAbsProcessorState (isCodeAddrOrNull mem) (lookupAbsBlock a absm)
              insBlock b abs_state m0
            _ -> m0

        insBlock :: Block X86_64
                 -> AbsProcessorState
                 -> MapF (Assignment X86_64) AbsValue
                 -> MapF (Assignment X86_64) AbsValue
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
mergeIntraJump  :: BlockLabel Word64
                  -- ^ Source label that we are jumping from.
                -> AbsBlockState
                   -- ^ Block state after executing instructions.
                -> CodeAddr
                   -- ^ Address we are trying to reach.
                -> State DiscoveryInfo ()
mergeIntraJump src ab tgt = modify $ mergeIntraJump' src ab tgt

-- | Joins in the new abstract state and returns the locations for
-- which the new state is changed.
mergeIntraJump'  :: BlockLabel Word64
                    -- ^ Source label that we are jumping from.
                 -> AbsBlockState
                    -- ^ Block state after executing instructions.
                 -> CodeAddr
                    -- ^ Address we are trying to reach.
                 -> DiscoveryInfo
                 -> DiscoveryInfo
mergeIntraJump' src ab _tgt _s0
  | not (absStackHasReturnAddr ab)
  , debug DCFG ("WARNING: Missing return value in jump from " ++ show src ++ " to\n" ++ show ab) False = error "Unexpected mergeIntraJump'"
mergeIntraJump' src ab tgt s0 = do
  -- Associate a new abstract state with the code region.
  let upd new s = do
        -- Add reverse edge
        s & reverseEdges %~ Map.insertWith Set.union tgt (Set.singleton (labelAddr src))
          & absState %~ Map.insert tgt new
          & frontier %~ Map.insert tgt (NextIP src)
  case Map.lookup tgt (s0^.absState) of
    -- We have seen this block before, so need to join and see if
    -- the results is changed.
    Just ab_old ->
      case joinD ab_old ab of
        Nothing  -> s0
        Just new -> upd new s0
    -- We haven't seen this block before
    Nothing -> s0 & upd ab
                  & markBlockStart tgt ab

-- | This updates the state of a function when returning from a function.
mergeFreeBSDSyscall :: BlockLabel Word64
                       -- ^ Label for callee block that is making this call.
                    -> AbsBlockState
                       -- ^ Block state just before this call.
                    -> CodeAddr
                       -- ^ Address that system call should return to.
                       -- We think this belongs to the same function.
                    -> State DiscoveryInfo ()
mergeFreeBSDSyscall src_lbl ab0 addr = do
  let regFn :: X86Reg tp -> AbsValue tp
      regFn r
          -- Set IPReg
        | Just Refl <- testEquality r ip_reg =
          CodePointers (Set.singleton addr)
          -- Two pointers that are modified by syscalls
        | Just Refl <- testEquality r rcx_reg =
          TopV
        | Just Refl <- testEquality r r11_reg =
          TopV
          -- Carry flag and rax is used by BSD for return values.
        | Just Refl <- testEquality r cf_reg =
          TopV
        | Just Refl <- testEquality r rax_reg =
          TopV
          -- Keep other registers the same.
        | otherwise =
          ab0^.absX86State^.boundValue r
  let ab = mkAbsBlockState regFn (ab0^.startAbsStack)

  -- Merge the new abstract
  mergeIntraJump src_lbl ab addr

-- | This updates the abstract information based on the assumption that
-- a called method will return to the return address, and will follow
-- x86_64 ABI.
mergeCallerReturn :: BlockLabel Word64
                     -- ^ Label of block maing call.
                  -> AbsBlockState
                     -- ^ Block state just before call.
                  -> CodeAddr
                     -- ^ Address we will return to.
                  -> State DiscoveryInfo ()
mergeCallerReturn lbl ab0 addr = do
  s <- get
  let regFn :: X86Reg tp -> AbsValue tp
      regFn r
          -- We set IPReg
        | Just Refl <- testEquality r ip_reg =
          CodePointers (Set.singleton addr)
        | Just Refl <- testEquality r sp_reg = do
            bvadd n64 (ab0^.absX86State^.boundValue r) (FinSet (Set.singleton 8))
          -- TODO: Transmit no value to first floating point register.
        | X86_XMMReg 0 <- r =
          ab0^.absX86State^.boundValue r
          -- TODO: Fix this (can we prove detect whether a floating point value was read)?
        | X87_TopReg <- r =
          ab0^.absX86State^.boundValue r
          -- Copy callee saved registers
        | Set.member (Some r) x86CalleeSavedRegs =
          ab0^.absX86State^.boundValue r
          -- We don't know anything about rax as it is the return value.
        | Just Refl <- testEquality r rax_reg =
          TopV
          -- We know nothing about other registers.
        | otherwise =
          TopV
  --TODO: Compute how far  stack to clear.

      -- Get values below return address.
      -- TODO: Fix this; the called function may modify the stack.
--  let stk = Map.filterWithKey (\k _ -> k >= 8) (ab0^.startAbsStack)
--  let ab = shiftSpecificOffset regFn stk 8

  let ab = mkAbsBlockState regFn (ab0^.startAbsStack)

  put $ mergeIntraJump' lbl ab addr s

_showAbsDiff :: AbsBlockState -> AbsBlockState -> Doc
_showAbsDiff x y = vcat (pp <$> absBlockDiff x y)
  where pp (Some n) = pretty (show n) <+> pretty (x^.absX86State^.boundValue n)
                                      <+> pretty (x^.absX86State^.boundValue n)

-- Check floating point top.
getAbsX87Top :: Monad m => AbsBlockState -> m Int
getAbsX87Top abst =
  case asConcreteSingleton (abst^.absX86State^. x87TopReg) of
    Just v -> return (fromInteger v)
    _ -> fail "x87top is not concrete"

-- -----------------------------------------------------------------------------
-- Refining an abstract state based upon a condition

-- FIXME: if val \notin av then we should return bottom
-- @refineProcState v av s@ returns a processor state after we have
-- asserted that @v@ is contained in the set @AbsValue@.
refineProcState :: Value X86_64 tp -- ^ Value in processor state
                -> AbsValue tp -- ^ Abstract value to assign value.
                -> AbsProcessorState
                -> AbsProcessorState
refineProcState (BVValue _n _val) _av regs = regs
refineProcState (Initial r) av regs =
  regs & (absInitialRegs . boundValue r) %~ flip meet av
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

refineApp :: App (Value X86_64) tp
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

refineTrunc :: ((n + 1) <= n')
            => BVValue X86_64 n'
            -> NatRepr n
            -> AbsValue (BVType n)
            -> AbsProcessorState
            -> AbsProcessorState
refineTrunc v sz av regs = refineProcState v (subValue sz av) regs

refineULeqTrue :: Value X86_64 tp
               -> Value X86_64 tp
               -> AbsProcessorState
               -> AbsProcessorState
refineULeqTrue x y regs = refineProcState x x_leq (refineProcState y y_leq regs)
  where
    (x_leq, y_leq) = abstractULeq (typeRepr x) (transferValue regs x) (transferValue regs y)
    -- check r@(a, b)
    --   | isBottom a = flip trace r $ "Bottom in refineLeq: "
    --                  ++ show (pretty regs)
    --   | isBottom b = flip trace r $ "Bottom in refineLeq: "
    --                  ++ show (pretty regs)
    --   | otherwise  = r

refineULtTrue :: Value X86_64 tp
              -> Value X86_64 tp
              -> AbsProcessorState
              -> AbsProcessorState
refineULtTrue x y regs = refineProcState x x_lt (refineProcState y y_lt regs)
  where
    (x_lt, y_lt) = abstractULt (typeRepr x) (transferValue regs x) (transferValue regs y)

refineLeq :: Value X86_64 tp
          -> Value X86_64 tp
          -> Integer
          -> AbsProcessorState
          -> AbsProcessorState
refineLeq x y b regs
     -- y < x
    | b == 0     = refineULtTrue y x regs
    -- x <= y
    | otherwise  = refineULeqTrue x y regs

refineLt :: Value X86_64 tp
         -> Value X86_64 tp
         -> Integer
         -> AbsProcessorState
         -> AbsProcessorState
refineLt x y b regs
  -- y <= x
  | b == 0     = refineULeqTrue y x regs
  -- x < y case
  | otherwise  = refineULtTrue  x y regs

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
               -> BVValue X86_64 64 -- ^ Memory address that IP is read from.
               -> Maybe (Word64, BVValue X86_64 64)
matchJumpTable mem read_addr
    -- Turn the read address into base + offset.
  | Just (BVAdd _ offset (BVValue _ base)) <- valueAsApp read_addr
    -- Turn the offset into a multiple by an index.
  , Just (BVMul _ (BVValue _ 8) jump_index) <- valueAsApp offset
  , isReadonlyAddr mem (fromInteger base) = do
    Just (fromInteger base, jump_index)
matchJumpTable _ _ =
    Nothing

-- Returns the index bounds for a jump table of 'Nothing' if this is not a block
-- table.
getJumpTableBounds :: Memory Word64 -- ^ State of memory
                   -> AbsProcessorState       -- ^ Current processor registers.
                   -> Word64 -- ^ Base
                   -> BVValue X86_64 64 -- ^ Index in jump table
                   -> Maybe Word64
                   -- ^ One past last index in jump table or nothing
getJumpTableBounds mem regs base jump_index
    -- Get range for the index.
  | let abs_value = transferValue regs jump_index
  , StridedInterval index_interval  <- abs_value
    -- Check that relevant interval is completely contained within a read-only
    -- read only range in the memory.
  , SI.StridedInterval _ index_base index_range index_stride <-
        debug DCFG "getJumpTable3" $ index_interval
  , index_end <- index_base + (index_range + 1) * index_stride
  , read_end <- toInteger base + 8 * index_end
  , rangeInReadonlySegment base (fromInteger read_end) mem =

    -- Get the addresses associated.
    debug DCFG ("Fixed table " ++ showHex base (" [" ++ shows jump_index "]")) $
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

transferBlock :: Block X86_64      -- ^ Block to start from
              -> AbsProcessorState -- ^ Registers at this block
              -> State DiscoveryInfo ()
transferBlock b regs = do
  let lbl = blockLabel b
  debugM DCFG ("transferBlock " ++ show lbl)
  mem <- gets memory
  regs' <- transferStmts regs (blockStmts b)
  -- FIXME: we should propagate c back to the initial block, not just b
  case blockTerm b of
    Branch c lb rb -> do
      mapM_ (recordWriteStmt lbl regs') (blockStmts b)
      Just l <- lookupBlock' lb
      let  l_regs = refineProcState c absTrue regs'
      Just r <- lookupBlock' rb
      let r_regs = refineProcState c absFalse regs'
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
          -- Note that in some cases the call is known not to return, and thus
          -- this code will never jump to the return value.
        _ | Just (prev_stmts, ret) <- identifyCall mem (blockStmts b) s' -> do
            Fold.mapM_ (recordWriteStmt lbl regs') prev_stmts
            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- Merge caller return information
            mergeCallerReturn lbl abst ret
            -- Look for new ips.
            recordFunctionAddrs lbl mem (abst^.absX86State^.curIP)

          -- This block ends with a return.
          | Just _ <- identifyReturn s' -> do
            mapM_ (recordWriteStmt lbl regs') (blockStmts b)

            let ip_val = s'^.boundValue ip_reg
            case transferValue regs' ip_val of
              ReturnAddr -> return ()
              -- The return_val is bad.
              -- This could indicate an imprecision in analysis or that the
              -- function will never return, and hence never was provided
              -- with an address to return to.
              rv ->
                debug DCFG ("return_val is bad at " ++ show lbl ++ ": " ++ show rv) $
                  return ()

          -- Jump to concrete offset.
          | BVValue _ (fromInteger -> tgt_addr) <- s'^.boundValue ip_reg -> do
            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- Try to check for a tail call.
            this_fn <- gets $ getFunctionEntryPoint (labelAddr lbl)
            tgt_fn  <- gets $ getFunctionEntryPoint tgt_addr
            -- When the jump appears to go to another function, this could be a tail
            -- call or it could be dead code.
            if (this_fn /= tgt_fn) then do
              -- Check that the current stack height is correct so that a
              -- tail call when go to the right place.
              -- TODO: Add check to ensure stack height is correct.
              debug DCFG ("Found jump to concrete address after function " ++ showHex tgt_fn ".") $ do
              modify $ markAddrAsFunction tgt_addr
              -- Check top of stack points to return value.
              let sp_val = s'^.boundValue sp_reg
              let ret_val = transferRHS regs' (ReadMem sp_val (BVTypeRepr n64))
              case ret_val of
                ReturnAddr ->
                  debug DCFG ("tail_ret_val is correct " ++ show lbl) $
                    returnCount += 1
                TopV ->
                  debug DCFG ("tail_ret_val is top at " ++ show lbl) $
                    returnCount += 1
                rv ->
                  -- The return_val is bad.
                  -- This could indicate that the caller knows that the function does
                  -- not return, and hence will not provide a reutrn value.
                  debug DCFG ("tail_ret_val is bad at " ++ show lbl ++ ": " ++ show rv) $
                    returnCount += 1

            else do
              -- Merge block state.
              let Just abst' = abst & setAbsIP (isCodeAddrOrNull mem) tgt_addr
              modify $ mergeIntraJump' lbl abst' tgt_addr


          -- Block ends with what looks like a jump table.
          | AssignedValue (Assignment _ (ReadMem ptr _))
                <- debug DCFG "try jump table" $ s'^.curIP
            -- Attempt to compute interval of addresses interval is over.
          , Just (base, jump_idx) <- matchJumpTable mem ptr -> do
            debug DCFG ("Found jump table at " ++ show lbl) $ do

            mapM_ (recordWriteStmt lbl regs') (blockStmts b)

            -- Try to compute jump table bounds
            let mread_end = getJumpTableBounds mem regs' base jump_idx

            let abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- This function resolves jump table entries.
            -- It is a recursive function that has an index into the jump table.
            -- If the current index can be interpreted as a intra-procedural jump,
            -- then it will add that to the current procedure.
            -- This returns the last address read.
            let resolveJump :: [Word64] -- ^ Addresses in jump table in reverse order
                            -> Word64 -- ^ Current index
                            -> State DiscoveryInfo [Word64]
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
                        let Just abst' = abst & setAbsIP (isCodeAddrOrNull mem) tgt_addr
                        mergeIntraJump lbl abst' tgt_addr
                        resolveJump (tgt_addr:prev) (idx+1)
                    _ -> do
                      debug DCFG ("Stop jump table: " ++ show idx ++ " " ++ show mread_end) $ do
                      return (reverse prev)
            read_addrs <- resolveJump [] 0
            let last_index = fromIntegral (length read_addrs)
            globalDataMap %= Map.insert base (JumpTable $! (Just $! base + 8 * last_index))

          -- We have a jump that we do not understand.
          -- This could be a tail call.
          | otherwise -> debug DCFG "Uninterpretable jump" $ do
            mapM_ (recordWriteStmt lbl regs') (blockStmts b)
            let abst = finalAbsBlockState regs' s'
            recordFunctionAddrs lbl mem (abst^.absX86State^.curIP)

transfer :: CodeAddr -> State DiscoveryInfo ()
transfer addr = do
  mem <- gets memory

  doMaybe (getBlock addr) () $ \root -> do
    ab <- getAbsBlockState addr
    transferBlock root (initAbsProcessorState (isCodeAddrOrNull mem) ab)

------------------------------------------------------------------------
-- Main loop

mkCFG :: Map CodeAddr (Maybe BlockRegion) -> CFG X86_64
mkCFG m = Map.foldlWithKey' go emptyCFG m
  where go g addr (Just br) = insertBlocksForCode addr (brEnd br) l g
          where l = Map.elems (brBlocks br)
        go g _ Nothing = g

explore_frontier :: DiscoveryInfo -> DiscoveryInfo
explore_frontier st =
  case Map.minViewWithKey (st^.frontier) of
    Nothing ->
      case Set.minView (st^.function_frontier) of
        Nothing -> st
        Just (addr, next_roots) -> do
          let high = Set.lookupGT addr (st^.functionEntries)
              st' = st & function_frontier .~ next_roots
                       & frontier .~ Map.singleton addr StartAddr
                         -- Delete any entries we previously discovered for function.
                       & reverseEdges    %~ deleteMapRange (Just addr) high
                         -- Delete any entries we previously discovered for function.
                       & absState        %~ deleteMapRange (Just addr) high
           in explore_frontier st'
    Just ((addr,_rsn), next_roots) ->
      let st_pre = st & frontier .~ next_roots
          st_post = flip execState st_pre $ transfer addr
       in explore_frontier st_post

cfgFromAddrs :: Memory Word64
                -- ^ Memory to use when decoding instructions.
             -> Map CodeAddr BS.ByteString
                -- ^ Names for (some) function entry points
             -> SyscallPersonality
                -- ^ Syscall personality
             -> [CodeAddr]
                -- ^ Location to start disassembler form.
             -> DiscoveryInfo
cfgFromAddrs mem symbols sysp init_addrs =
  debug DCFG ("Starting addrs " ++ show (Hex <$> init_addrs)) $ s3
  where
    global_data = Map.fromList
      [ (v, ReferencedValue)
      | (_,v) <- memAsWord64le_withAddr mem
        -- Check this is readable, non-executable data
      , addrPermissions v mem .&. (pf_r .|. pf_x) == pf_r
      ]
    init_abs_state = Map.fromList
                     [ (a, fnBlockState mem a)
                     | a <- init_addrs
                     ]


    s0 = emptyDiscoveryInfo mem symbols sysp
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
        debug DCFG ("Identified function entry "
                ++ showHex v (" due to global store at " ++ showHex a ".")) $
        markAddrAsFunction v s
      | otherwise =
        debug DCFG ("Found function entry from memory" ++ showHex v " at " ++ showHex a ".") $
        markAddrAsFunction v s

    -- Explore data values
    s2  = foldl' go s1 (memAsWord64le_withAddr mem)
    s3 = explore_frontier s2
