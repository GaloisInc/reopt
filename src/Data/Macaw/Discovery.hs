{- |
Module           : Reopt.Semantics.CFGDiscovery
Copyright        : (c) Galois, Inc 2015-2016
Maintainer       : Joe Hendrix <jhendrix@galois.com>, Simon Winwood <sjw@galois.com>

This contains an implementation of a CFG discovery algorithm based upon an
interleaved abstract interpretation.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Macaw.Discovery
       ( mkCFG
       , cfgFromAddrs
       , assignmentAbsValues
       ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.AbsDomain.AbsState
import           Data.Macaw.AbsDomain.Refine
import qualified Data.Macaw.AbsDomain.StridedInterval as SI
import           Data.Macaw.Architecture.Info
import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Discovery.Info
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm
import           Data.Macaw.Types

transferRHS :: forall a ids tp
            .  ( OrdF (ArchReg a)
               , ShowF (ArchReg a)
               , MemWidth (ArchAddrWidth a)
               )
            => ArchitectureInfo a
            -> AbsProcessorState (ArchReg a) ids
            -> AssignRhs a ids tp
            -> ArchAbsValue a tp
transferRHS info r rhs =
  case rhs of
    EvalApp app    -> transferApp r app
    SetUndefined _ -> TopV
    ReadMem a tp
      | StackOffset _ s <- transferValue r a
      , [o] <- Set.toList s
      , Just (StackEntry v_tp v) <- Map.lookup o (r^.curAbsStack)
      , Just Refl <- testEquality tp v_tp ->
         v
      | otherwise -> TopV
    EvalArchFn f _ -> absEvalArchFn info r f

-- | Merge in the value of the assignment.
--
-- If we have already seen a value, this will combine with meet.
addAssignment :: ( OrdF  (ArchReg a)
                 , ShowF (ArchReg a)
                 , MemWidth (ArchAddrWidth a)
                 )
              => ArchitectureInfo a
              -> Assignment a ids tp
              -> AbsProcessorState (ArchReg a) ids
              -> AbsProcessorState (ArchReg a) ids
addAssignment info a c =
  c & (absAssignments . assignLens (assignId a))
    %~ flip meet (transferRHS info c (assignRhs a))

------------------------------------------------------------------------
-- Utilities

-- | Get code pointers out of a abstract value.
concretizeAbsCodePointers :: MemWidth w
                          => Memory w
                          -> AbsValue w (BVType w)
                          -> [SegmentedAddr w]
concretizeAbsCodePointers mem (FinSet s) =
  [ sa
  | a <- Set.toList s
  , Just sa <- [absoluteAddrSegment mem (fromInteger a)]
  , Perm.isExecutable (segmentFlags (addrSegment sa))
  ]
concretizeAbsCodePointers _ (CodePointers s _) =
  [ sa
  | sa <- Set.toList s
  , Perm.isExecutable (segmentFlags (addrSegment sa))
  ]
  -- FIXME: this is dangerous !!
concretizeAbsCodePointers _mem StridedInterval{} = [] -- FIXME: this case doesn't make sense
  -- debug DCFG ("I think these are code pointers!: " ++ show s) $ []
  -- filter (isCodeAddr mem) $ fromInteger <$> SI.toList s
concretizeAbsCodePointers _mem _ = []

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

------------------------------------------------------------------------
-- Memory utilities

-- | Return true if range is entirely contained within a single read only segment.Q
rangeInReadonlySegment :: MemWidth w
                       => SegmentedAddr w -- ^ Start of range
                       -> MemWord w -- ^ The size of the range
                       -> Bool
rangeInReadonlySegment base size
    = base^.addrOffset + size <= segmentSize seg
    && Perm.isReadonly (segmentFlags seg)
  where seg = addrSegment base


------------------------------------------------------------------------
-- Block discovery

-- | The CFG-building monad: includes a state component with a 'DiscoveryInfo'
-- and a 'NonceGenerator', layered on top of the 'ST' monad
newtype CFGM arch ids a =
    CFGM { unCFGM :: StateT (DiscoveryInfo arch ids) (ST ids) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (DiscoveryInfo arch ids)
           )

liftST :: ST ids a -> CFGM arch ids a
liftST = CFGM . lift

-- | Run a CFGM at the top level
runCFGM :: ArchitectureInfo arch
           -- ^ Architecture-specific information needed for doing control-flow exploration.
        -> Memory (ArchAddrWidth arch)
           -- ^ Memory to use when decoding instructions.
        -> Map (ArchSegmentedAddr arch) BS.ByteString
           -- ^ Names for (some) function entry points
        -> SyscallPersonality arch
           -- ^ Syscall personality
        -> (forall ids . CFGM arch ids ())
           -- ^ Computation to run.
        -> Some (DiscoveryInfo arch)
runCFGM arch_info mem symbols sysp m = do
  withGlobalSTNonceGenerator $ \nonce_gen -> do
    let init_info = emptyDiscoveryInfo nonce_gen mem symbols sysp arch_info
    Some <$> execStateT (unCFGM m) init_info

{-
blockOverlaps :: ArchSegmentedAddr arch
              -> ArchSegmentedAddr arch
              -> Maybe (BlockRegion arch ids)
              -> Bool
blockOverlaps _ start Nothing = segmentIndex (addrSegment a) == segmentIndex (addrSegment a)
blockOverlaps a (Just br) = a < brEnd br
-}

-- | This is the worker for getBlock, in the case that we have not already
-- read the block.
tryDisassembleAddr :: PrettyCFGConstraints arch
                   => ArchSegmentedAddr arch
                   -> AbsBlockState (ArchReg arch)
                   -> CFGM arch ids ()
tryDisassembleAddr addr ab = do
  s0 <- get
  -- Attempt to disassemble block.
  -- Get memory so that we can decode from it.
  let block_addrs = s0^.blocks
  -- Returns true if we are not at the start of a block.
  -- This is used to stop the disassembler when we reach code
  -- that is part of a new block.
  let not_at_block = (`Map.notMember` block_addrs)
  let mem = memory s0
  nonce_gen <- nonceGen <$> get
  res <- liftST $ disassembleFn (archInfo s0) nonce_gen mem not_at_block addr ab
  -- Build state for exploring this.
  case res of
    Left e ->
      debugM DCFG ("Failed to disassemble block at " ++ show addr ++ " " ++ show e) >>
      put $ (s0 & blocks %~ Map.insert addr Nothing)
    Right (bs, next_ip) -> do
      assert (segmentIndex (addrSegment next_ip) == segmentIndex (addrSegment addr)) $ do
      assert (next_ip^.addrOffset > addr^.addrOffset) $ do
      debugM DCFG (show $ vcat $ map pretty bs)
      let block_map = Map.fromList [ (labelIndex (blockLabel b), b) | b <- bs ]
      -- Add block region to blocks.
      let br = BlockRegion { brSize = next_ip^.addrOffset - addr^.addrOffset
                           , brBlocks = block_map
                           }
      put $ (s0 & blocks %~ Map.insert addr (Just $! br))

-- | Mark address as the start of a block.
markBlockStart :: PrettyCFGConstraints arch
               => ArchSegmentedAddr arch
               -> AbsBlockState (ArchReg arch)
               -> CFGM arch ids ()
markBlockStart addr ab = do
  s <- get
  -- Lookup block just before this address
  case Map.lookupLT addr (s^.blocks) of
    -- If that block overlaps with the address
    Just (l,mbr)
      | segmentIndex (addrSegment addr) == segmentIndex (addrSegment l)
      , case mbr of
          Nothing -> True
          Just br -> addr^.addrOffset < l^.addrOffset + brSize br -> do
      let l_start = getFunctionEntryPoint l s
          a_start = getFunctionEntryPoint addr s
      -- Get block for addr
      tryDisassembleAddr addr ab
      -- Get block for old block
      let Just old_ab = Map.lookup l (s^.absState)
      tryDisassembleAddr l old_ab
      -- Add function starts to split to frontier
      -- This will result in us re-exploring l_start and a_start
      -- once the current function is done.
      modify $ \s0 -> s0 & function_frontier %~ Set.insert l_start . Set.insert a_start
    _ ->
      tryDisassembleAddr addr ab

-- | Returns a block at the given location, if at all possible.  This
-- will disassemble the binary if the block hasn't been seen before.
-- In particular, this ensures that a block and all its children are
-- present in the cfg (assuming successful disassembly)
getBlock :: ArchSegmentedAddr arch
         -> CFGM arch ids (Maybe (Block arch ids))
getBlock addr = do
  m_b <- use blocks
  case Map.lookup addr m_b of
    Just mbr ->
      return $! Map.lookup 0 . brBlocks =<< mbr
    -- We haven't tried to get this block.
    Nothing -> error $ "getBlock called on block " ++ show addr ++ " we have not seen."

------------------------------------------------------------------------
-- Transfer stmts

transferStmt :: ( RegisterInfo (ArchReg arch)
                , HasRepr (ArchReg arch) TypeRepr
                , MemWidth (ArchAddrWidth arch)
                )
             => ArchitectureInfo arch
             -> Stmt arch ids
             -> State (AbsProcessorState (ArchReg arch) ids) ()
transferStmt info stmt =
  case stmt of
    AssignStmt a -> do
      modify $ addAssignment info a
    WriteMem addr v -> do
      modify $ \r -> addMemWrite (r^.absInitialRegs^.curIP) addr v r
    _ -> return ()


newtype HexWord = HexWord Word64

instance Show HexWord where
  showsPrec _ (HexWord w) = showHex w

-- | Mark a escaped code pointer as a function entry.
markAddrAsFunction :: PrettyCFGConstraints arch
                   => ArchSegmentedAddr arch
                   -> CFGM arch ids ()
markAddrAsFunction addr = do
  s <- get
  when (not (Set.member addr (s^.functionEntries))) $ do
    debugM DCFG ("Found function entry " ++ show addr ++ ".")
    let mem = memory s
    let low = Set.lookupLT addr (s^.functionEntries)
    let _high = Set.lookupGT addr (s^.functionEntries)
    -- Get abstract state associated with function begining at address
    let abstState = fnBlockStateFn (archInfo s) mem addr
    markBlockStart addr abstState
    modify $ \s0 -> s0 & absState          %~ Map.insert addr abstState
                       & functionEntries   %~ Set.insert addr
                       & function_frontier %~ maybeSetInsert low . Set.insert addr

recordFunctionAddrs :: ( PrettyCFGConstraints arch
                       , MemWidth (ArchAddrWidth arch)
                       )
                    => BlockLabel (ArchAddrWidth arch)
                    -> Memory (ArchAddrWidth arch)
                    -> ArchAbsValue arch (BVType (ArchAddrWidth arch))
                    -> CFGM arch ids ()
recordFunctionAddrs lbl mem av = do
  let addrs = concretizeAbsCodePointers mem av
  debugM DCFG (show lbl ++ ": Adding function entries "
               ++ intercalate ", " (map show addrs))
  mapM_ markAddrAsFunction addrs

recordWriteStmt :: ( PrettyCFGConstraints arch
                   , HasRepr (ArchReg arch) TypeRepr
                   , MemWidth (ArchAddrWidth arch)
                   )
                => BlockLabel (ArchAddrWidth arch)
                -> AbsProcessorState (ArchReg arch) ids
                -> Stmt arch ids
                -> CFGM arch ids ()
recordWriteStmt lbl regs stmt = do
  addrWidth <- gets $ addrWidthNatRepr . archAddrWidth . archInfo
  case stmt of
    WriteMem _addr v
      | Just Refl <- testEquality (typeRepr v) (BVTypeRepr addrWidth) -> do
          mem <- gets memory
          recordFunctionAddrs lbl mem (transferValue regs v)
    _ ->
      return ()

transferStmts :: ( HasRepr      (ArchReg arch) TypeRepr
                 , RegisterInfo (ArchReg arch)
                 , MemWidth (ArchAddrWidth arch)
                 )
              => ArchitectureInfo arch
              -> AbsProcessorState (ArchReg arch) ids
              -> [Stmt arch ids]
              -> AbsProcessorState (ArchReg arch) ids
transferStmts info r stmts = execState (mapM_ (transferStmt info) stmts) r

-- | Generate map that maps each assignment in the CFG to the abstract value
-- associated with it.
assignmentAbsValues :: forall arch ids
                    .  ( HasRepr      (ArchReg arch) TypeRepr
                       , RegisterInfo (ArchReg arch)
                       , MemWidth (ArchAddrWidth arch)
                       )
                    => ArchitectureInfo arch
                    -> Memory (ArchAddrWidth arch)
                    -> CFG arch ids
                    -> AbsStateMap arch
                    -> MapF (AssignId ids) (ArchAbsValue arch)
assignmentAbsValues info mem g absm =
     foldl' go MapF.empty (Map.elems (g^.cfgBlocks))
  where go :: MapF (AssignId ids) (ArchAbsValue arch)
           -> Block arch ids
           -> MapF (AssignId ids) (ArchAbsValue arch)
        go m0 b =
          case blockLabel b of
            GeneratedBlock a 0 -> do
              let w = addrWidthNatRepr (archAddrWidth info)
              let Just ab = Map.lookup a absm
              let abs_state = initAbsProcessorState w mem ab
              insBlock b abs_state m0
            _ -> m0

        insBlock :: Block arch ids
                 -> AbsProcessorState (ArchReg arch) ids
                 -> MapF (AssignId ids) (ArchAbsValue arch)
                 -> MapF (AssignId ids) (ArchAbsValue arch)
        insBlock b r0 m0 =
          let final = transferStmts info r0 (blockStmts b)
              m = MapF.union (final^.absAssignments) m0 in
          case blockTerm b of
            Branch _ lb rb -> do
              let Just l = findBlock g lb
              let Just r = findBlock g rb
              insBlock l final $
                insBlock r final $
                m
            FetchAndExecute _ -> m
            Syscall _ -> m

------------------------------------------------------------------------
-- Transfer functions

-- | Joins in the new abstract state and returns the locations for
-- which the new state is changed.
mergeIntraJump  :: ( PrettyCFGConstraints arch
                   , RegisterInfo (ArchReg arch)
                   )
                => BlockLabel (ArchAddrWidth arch)
                  -- ^ Source label that we are jumping from.
                -> AbsBlockState (ArchReg arch)
                   -- ^ Block state after executing instructions.
                -> ArchSegmentedAddr arch
                   -- ^ Address we are trying to reach.
                -> CFGM arch ids ()
mergeIntraJump src ab _tgt
  | not (absStackHasReturnAddr ab)
  , debug DCFG ("WARNING: Missing return value in jump from " ++ show src ++ " to\n" ++ show ab) False
  = error "Unexpected mergeIntraJump"
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
    Nothing -> do modify $ upd ab
                  markBlockStart tgt ab

-- -----------------------------------------------------------------------------
-- Refining an abstract state based upon a condition


-- See if expression matches form expected by jump tables
-- TODO: Fixme, this uses a fixed multiple of 8 for the jump table
matchJumpTable :: MemWidth (ArchAddrWidth arch)
               => Memory (ArchAddrWidth arch)
               -> BVValue arch ids (ArchAddrWidth arch) -- ^ Memory address that IP is read from.
               -> Maybe (ArchSegmentedAddr arch, BVValue arch ids (ArchAddrWidth arch))
matchJumpTable mem read_addr
    -- Turn the read address into base + offset.
  | Just (BVAdd _ offset base_val) <- valueAsApp read_addr
  , Just base <- asLiteralAddr mem base_val
    -- Turn the offset into a multiple by an index.
  , Just (BVMul _ (BVValue _ 8) jump_index) <- valueAsApp offset
  , Perm.isReadonly (segmentFlags (addrSegment base)) = do
    Just (base, jump_index)
matchJumpTable _ _ =
    Nothing

-- Returns the index bounds for a jump table of 'Nothing' if this is not a block
-- table.
getJumpTableBounds :: ( OrdF (ArchReg a)
                      , ShowF (ArchReg a)
                      , MemWidth (ArchAddrWidth a)
                      )
                   => AbsProcessorState (ArchReg a) ids -- ^ Current processor registers.
                   -> ArchSegmentedAddr a -- ^ Base
                   -> BVValue a ids (ArchAddrWidth a) -- ^ Index in jump table
                   -> Maybe (ArchAddr a)
                   -- ^ One past last index in jump table or nothing
getJumpTableBounds regs base jump_index
    -- Get range for the index.
  | let abs_value = transferValue regs jump_index
  , StridedInterval index_interval  <- abs_value
    -- Check that relevant interval is completely contained within a read-only
    -- read only range in the memory.
  , SI.StridedInterval _ index_base index_range index_stride <-
        debug DCFG "getJumpTable3" $ index_interval
  , index_end <- index_base + (index_range + 1) * index_stride
  , rangeInReadonlySegment base (8 * fromInteger index_end) =
    -- Get the addresses associated.
    debug DCFG ("Fixed table " ++ show base ++ " [" ++ shows jump_index "]") $
      Just $! fromInteger index_end
getJumpTableBounds _ _ _ = Nothing

-- | This
fetchAndExecute :: forall arch ids
                .  ( RegisterInfo (ArchReg arch)
                   , ArchConstraint arch ids
                   , PrettyCFGConstraints arch
                   , MemWidth (ArchAddrWidth arch)
                   )
                => Block arch ids
                -> AbsProcessorState (ArchReg arch) ids
                   -- ^ Registers at this block after statements executed
                -> RegState (ArchReg arch) (Value arch ids)
                -> CFGM arch ids ()
fetchAndExecute b regs' s' = do
  let lbl = blockLabel b
  mem <- gets memory :: CFGM arch ids (Memory (ArchAddrWidth arch))
  arch_info <- gets archInfo
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
        mergeIntraJump lbl (postCallAbsStateFn arch_info abst ret) ret
        -- Look for new ips.
        recordFunctionAddrs lbl mem (abst^.absRegState^.curIP)
    -- This block ends with a return.
      | Just _ <- identifyReturn s' (callStackDelta arch_info) -> do
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
      | Just tgt_addr <- asLiteralAddr mem (s'^.boundValue ip_reg) -> do
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
            debug DCFG ("Found jump to concrete address after function " ++ show tgt_fn ++ ".") $ do
            markAddrAsFunction tgt_addr
            -- Check top of stack points to return value.
            let sp_val = s'^.boundValue sp_reg
            let ptrType = BVTypeRepr (addrWidthNatRepr (archAddrWidth arch_info))
            let ret_val = transferRHS arch_info regs' (ReadMem sp_val ptrType)
            case ret_val of
              ReturnAddr ->
                debug DCFG ("tail_ret_val is correct " ++ show lbl) $
                  return ()
              TopV ->
                debug DCFG ("tail_ret_val is top at " ++ show lbl) $
                  return ()
              rv ->
                -- The return_val is bad.
                -- This could indicate that the caller knows that the function does
                -- not return, and hence will not provide a reutrn value.
                debug DCFG ("tail_ret_val is bad at " ++ show lbl ++ ": " ++ show rv) $
                  return ()
           else do
              assert (segmentFlags (addrSegment tgt_addr) `Perm.hasPerm` Perm.execute) $ do
              -- Merge block state.
              let abst' = abst & setAbsIP tgt_addr
              mergeIntraJump lbl abst' tgt_addr

      -- Block ends with what looks like a jump table.
      | AssignedValue (Assignment _ (ReadMem ptr _))
          <- debug DCFG "try jump table" $ s'^.curIP
        -- Attempt to compute interval of addresses interval is over.
      , Just (base, jump_idx) <- matchJumpTable mem ptr -> do
            debug DCFG ("Found jump table at " ++ show lbl) $ do

            mapM_ (recordWriteStmt lbl regs') (blockStmts b)

            -- Try to compute jump table bounds
            let mread_end = getJumpTableBounds regs' base jump_idx

            let abst :: AbsBlockState (ArchReg arch)
                abst = finalAbsBlockState regs' s'
            seq abst $ do
            -- This function resolves jump table entries.
            -- It is a recursive function that has an index into the jump table.
            -- If the current index can be interpreted as a intra-procedural jump,
            -- then it will add that to the current procedure.
            -- This returns the last address read.
            let resolveJump :: [ArchSegmentedAddr arch]
                               -- /\ Addresses in jump table in reverse order
                            -> ArchAddr arch
                               -- /\ Current index
                            -> CFGM arch ids [ArchSegmentedAddr arch]
                resolveJump prev idx | Just idx == mread_end = do
                  -- Stop jump table when we have reached computed bounds.
                  return (reverse prev)
                resolveJump prev idx = do
                  let read_addr = base & addrOffset +~ 8 * idx
                  interpState <- get
                  case readAddr mem LittleEndian read_addr of
                    Right tgt_addr
                      | Perm.isReadonly (segmentFlags (addrSegment read_addr))
                      , inSameFunction (labelAddr lbl) tgt_addr interpState -> do
                        let flags = segmentFlags (addrSegment tgt_addr)
                        assert (flags `Perm.hasPerm` Perm.execute) $ do
                        let abst' = abst & setAbsIP tgt_addr
                        mergeIntraJump lbl abst' tgt_addr
                        resolveJump (tgt_addr:prev) (idx+1)
                    _ -> do
                      debug DCFG ("Stop jump table: " ++ show idx ++ " " ++ show mread_end) $ do
                      return (reverse prev)
            read_addrs <- resolveJump [] 0
            let last_index = fromIntegral (length read_addrs)
            let last_addr = Just $! base & addrOffset +~ 8 * last_index
            globalDataMap %= Map.insert base (JumpTable $! last_addr)

          -- We have a jump that we do not understand.
          -- This could be a tail call.
      | otherwise -> debug DCFG "Uninterpretable jump" $ do
        mapM_ (recordWriteStmt lbl regs') (blockStmts b)
        let abst = finalAbsBlockState regs' s'
        recordFunctionAddrs lbl mem (abst^.absRegState^.curIP)

type TransferConstraints arch
   = ( PrettyCFGConstraints arch
     , RegisterInfo (ArchReg arch)
     , HasRepr (ArchReg arch)  TypeRepr
     , MemWidth (ArchAddrWidth arch)
     )

transferBlock :: TransferConstraints arch
              => Block arch ids -- ^ Block to start from
              -> AbsProcessorState (ArchReg arch) ids -- ^ Registers at this block
              -> CFGM arch ids ()
transferBlock b regs = do
  let lbl = blockLabel b
  debugM DCFG ("transferBlock " ++ show lbl)
  mem <- gets memory
  arch_info <- gets archInfo
  let regs' = transferStmts arch_info regs (blockStmts b)
  -- FIXME: we should propagate c back to the initial block, not just b
  case blockTerm b of
    Branch c lb rb -> do
      mapM_ (recordWriteStmt lbl regs') (blockStmts b)
      Just l <- uses blocks (`lookupBlock` lb)
      let  l_regs = refineProcState c absTrue regs'
      Just r <- uses blocks (`lookupBlock` rb)
      let r_regs = refineProcState c absFalse regs'
      -- We re-transfer the stmts to propagate any changes from
      -- the above refineProcState.  This could be more efficient by
      -- tracking what (if anything) changed.  We also might
      -- need to keep going back and forth until we reach a
      -- fixpoint
      transferBlock l (transferStmts arch_info l_regs (blockStmts b))
      transferBlock r (transferStmts arch_info r_regs (blockStmts b))

    Syscall s' -> do
      mapM_ (recordWriteStmt lbl regs') (blockStmts b)
      let abst = finalAbsBlockState regs' s'
      let ips = concretizeAbsCodePointers mem (abst^.absRegState^.curIP)
      -- Merge system call result with possible next IPs.
      Fold.forM_ ips $ \addr -> do
        mergeIntraJump lbl (postSyscallFn arch_info abst addr)  addr

    FetchAndExecute s' -> do
      fetchAndExecute b regs' s'

transfer :: TransferConstraints arch
         => ArchSegmentedAddr arch
         -> CFGM arch ids ()
transfer addr = do
  mem <- gets memory
  mroot <- getBlock addr
  case mroot of
    Nothing -> return ()
    Just root -> do
      addrWidth <- gets $ addrWidthNatRepr . archAddrWidth . archInfo
      mab <- uses absState $ Map.lookup addr
      case mab of
        Nothing -> error $ "Could not find block " ++ show addr ++ "."
        Just ab -> do
          transferBlock root $
            initAbsProcessorState addrWidth mem ab

------------------------------------------------------------------------
-- Main loop

mkCFG :: Integral (ArchAddr arch)
      => Map (ArchSegmentedAddr arch) (Maybe (BlockRegion arch ids))
      -> CFG arch ids
mkCFG m = Map.foldlWithKey' go emptyCFG m
  where go g addr (Just br) = insertBlocksForCode addr (brSize br) l g
          where l = Map.elems (brBlocks br)
        go g _ Nothing = g

explore_frontier :: TransferConstraints arch
                 => CFGM arch ids ()
explore_frontier = do
  st <- get
  case Map.minViewWithKey (st^.frontier) of
    Nothing ->
      case Set.minView (st^.function_frontier) of
        Nothing -> return ()
        Just (addr, next_roots) -> do
          let high = Set.lookupGT addr (st^.functionEntries)
              st' = st & function_frontier .~ next_roots
                       & frontier .~ Map.singleton addr StartAddr
                         -- Delete any entries we previously discovered for function.
                       & reverseEdges    %~ deleteMapRange (Just addr) high
                         -- Delete any entries we previously discovered for function.
                       & absState        %~ deleteMapRange (Just addr) high
          put st'
          explore_frontier
    Just ((addr,_rsn), next_roots) -> do
      put $ st & frontier .~ next_roots
      transfer addr
      explore_frontier

-- | This returns true if the address is writable and value, and points to code.
memIsDataCodePointer :: Memory w -> SegmentedAddr w -> SegmentedAddr w -> Bool
memIsDataCodePointer _ a v
  =  segmentFlags (addrSegment v) `Perm.hasPerm` Perm.execute
  && segmentFlags (addrSegment a) `Perm.hasPerm` Perm.write

markFunctionAddrs :: TransferConstraints arch
                  => [ArchSegmentedAddr arch]
                  -> CFGM arch ids ()
markFunctionAddrs addrs = do
  mapM_ markAddrAsFunction addrs
  explore_frontier

-- | Construct a discovery info by starting with exploring from a given set of
-- function entry points.
cfgFromAddrs :: forall arch
             .  TransferConstraints arch
             => ArchitectureInfo arch
                -- ^ Architecture-specific information needed for doing control-flow exploration.
             -> Memory (ArchAddrWidth arch)
                -- ^ Memory to use when decoding instructions.
             -> Map (ArchSegmentedAddr arch) BS.ByteString
                -- ^ Names for (some) function entry points
             -> SyscallPersonality arch
                -- ^ Syscall personality
             -> [ArchSegmentedAddr arch]
                -- ^ Initial function entry points.
             -> [(ArchSegmentedAddr arch, ArchSegmentedAddr arch)]
                -- ^ Function entry points in memory to be explored
                -- after exploring function entry points.
                --
                -- Each entry contains an address and the value stored in it.
             -> Some (DiscoveryInfo arch)
cfgFromAddrs arch_info mem symbols sysp init_addrs mem_words =
  runCFGM arch_info mem symbols sysp $ do
    -- Set abstract state for initial functions
    markFunctionAddrs init_addrs
    -- Add in code pointers from memory.
    let notAlreadyFunction s a v
            | Set.member v (s^.functionEntries) = False
            | otherwise = debug DCFG msg True
          where msg | Map.member v (s^.blocks) =
                        "Identified function entry "
                        ++ show v ++ " due to global store at " ++ show a ++ "."
                    | otherwise =
                        "Found function entry from memory" ++ show v ++ " at " ++ show a ++ "."
    s <- get
    let mem_addrs =
          fmap snd $
          filter (uncurry (notAlreadyFunction s)) $
          filter (uncurry (memIsDataCodePointer mem)) $
          mem_words
    markFunctionAddrs mem_addrs
