{-|
Copyright   : (c) Galois Inc, 2015-2016
Maintainer  : jhendrix@galois.com

This module provides methods for constructing functions from the basic
blocks discovered by 'Data.Macaw.Discovery'.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.Recovery
  ( Function(..)
  , FnBlock(..)
  , FnStmt(..)
  , FnTermStmt(..)
  , FnValue(..)
  , FnAssignment(..)
  , FnAssignRhs(..)
  , recoverFunction
  ) where

import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (toList, traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Type.Equality
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Discovery.Info
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm
import           Data.Macaw.Types

import           Reopt.CFG.FnRep
import           Reopt.CFG.RegisterUse
import           Reopt.CFG.StackDepth
import           Reopt.Machine.X86State

------------------------------------------------------------------------
-- RecoverState

type FnRegValueMap = MapF X86Reg FnRegValue

data RecoverState ids = RS { _rsInterp :: !(DiscoveryInfo X86_64 ids)
                           , _rsNextAssignId :: !FnAssignId
                           , _rsAssignMap :: !(MapF (AssignId ids) FnAssignment)

                             -- Local state
                           , _rsCurLabel  :: !(BlockLabel 64)
                           , _rsCurStmts  :: !(Seq FnStmt)

                           , _rsCurRegs   :: !(FnRegValueMap)
                             -- ^ This maps registers to the associated value
                             -- at the start of the block after any stack allocations have
                             -- been performed.
                           , rsSyscallPersonality  :: !(SyscallPersonality X86_64)
                             -- ^ System call personality
                           , rsAssignmentsUsed     :: !(Set (Some (AssignId ids)))
                           , rsCurrentFunctionType :: !FunctionType
                           , rsFunctionArgs        :: !FunctionArgs
                           }

rsInterp :: Simple Lens (RecoverState ids) (DiscoveryInfo X86_64 ids)
rsInterp = lens _rsInterp (\s v -> s { _rsInterp = v })

rsNextAssignId :: Simple Lens (RecoverState ids) FnAssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

rsCurLabel :: Simple Lens (RecoverState ids) (BlockLabel 64)
rsCurLabel = lens _rsCurLabel (\s v -> s { _rsCurLabel = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens (RecoverState ids) (Seq FnStmt)
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens (RecoverState ids) (MapF (AssignId ids) FnAssignment)
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

-- | This maps registers to the associated value at the start of the block after
-- any stack allocations have been performed.
rsCurRegs :: Simple Lens (RecoverState ids) FnRegValueMap
rsCurRegs = lens _rsCurRegs (\s v -> s { _rsCurRegs = v })

------------------------------------------------------------------------
-- Recover

data RecoverResult ids a
  = RecoverError !String
  | RecoverSuccess !a !(RecoverState ids)

newtype Recover ids a = Recover { runRecover :: RecoverState ids -> RecoverResult ids a
                                }

evalRecover :: RecoverState ids -> Recover ids a -> Either String a
evalRecover s0 m =
  case runRecover m s0 of
    RecoverError msg -> Left msg
    RecoverSuccess r _ -> Right r

instance Functor (Recover ids) where
  fmap f m = Recover $ \s ->
    case runRecover m s of
      RecoverError msg -> RecoverError msg
      RecoverSuccess v t -> RecoverSuccess (f v) t

instance Applicative (Recover ids) where
  pure v = Recover (\s -> RecoverSuccess v s)
  mf <*> mv = Recover $ \s0 ->
    case runRecover mf s0 of
      RecoverError msg -> RecoverError msg
      RecoverSuccess f s1 -> runRecover (f <$> mv) s1

instance Monad (Recover ids) where
  return = pure
  m >>= h = Recover $ \s0 ->
    case runRecover m s0 of
      RecoverError msg -> RecoverError msg
      RecoverSuccess v s1 -> runRecover (h v) s1

instance MonadError String (Recover ids) where
  throwError msg = Recover $ \_ -> RecoverError msg
  catchError m h = Recover $ \s ->
    case runRecover m s of
      RecoverError msg -> runRecover (h msg) s
      RecoverSuccess v t -> RecoverSuccess v t

instance MonadState (RecoverState ids) (Recover ids) where
  get  = Recover $ \s -> RecoverSuccess s s
  put t = Recover $ \_ -> RecoverSuccess () t

{-

type Recover ids a = StateT (RecoverState ids) (ExceptT String Identity) a

runRecover :: RecoverState ids -> Recover ids a -> Either String a
runRecover s m = runIdentity $ runExceptT $ evalStateT m s
-}

mkId :: (FnAssignId -> a) -> Recover ids a
mkId f = do
  FnAssignId next_id <- use rsNextAssignId
  rsNextAssignId .= FnAssignId (next_id + 1)
  return $! f (FnAssignId next_id)

mkFnAssign :: FnAssignRhs tp -> Recover ids (FnAssignment tp)
mkFnAssign rhs = mkId (\next_id -> FnAssignment next_id rhs)

mkReturnVar :: TypeRepr tp -> Recover ids (FnReturnVar tp)
mkReturnVar tp = mkId (\next_id -> FnReturnVar next_id tp)

addFnStmt :: FnStmt -> Recover ids ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

------------------------------------------------------------------------
-- recoverFunction

-- | This returns how much space there is before start of next function,
-- or the end of the memory segment if code address is undefined.
--
-- Note: Calls error if code addr is not in a valid memory location.
functionSize :: DiscoveryInfo X86_64 ids -> SegmentedAddr 64 -> MemWord 64
functionSize s a = do
  let seg = addrSegment a
  assert (segmentFlags seg `Perm.hasPerm` Perm.execute) $ do
    case Set.lookupGT a (s^.functionEntries) of
      Just next | segmentIndex (addrSegment next) == segmentIndex seg ->
           next^.addrOffset - a^.addrOffset
      _ -> segmentSize seg - a^.addrOffset


-- | Recover the function at a given address.
recoverFunction :: SyscallPersonality X86_64
                -> FunctionArgs
                -> DiscoveryInfo X86_64 ids
                -> SegmentedAddr 64
                -> Either String Function
recoverFunction sysp fArgs s a = do
  let (usedAssigns, blockRegs, blockRegProvides, blockPreds)
        = registerUse sysp fArgs s a

  let lbl = GeneratedBlock a 0

  let cft = fromMaybe
              (debug DFunRecover ("Missing type for label " ++ show lbl) ftMinimumFunctionType) $
              Map.lookup a fArgs

  let insIntReg i r = MapF.insert r (FnRegValue (FnIntArg i))
  let insFloatReg i r = MapF.insert r (FnRegValue (FnFloatArg i))
  let insCalleeSaved (Some r) = MapF.insert r (CalleeSaved r)

  let initRegs = MapF.empty
               & flip (ifoldr insIntReg)     (ftIntArgRegs cft)
               & flip (ifoldr insFloatReg)   (ftFloatArgRegs cft)
               & flip (foldr insCalleeSaved) x86CalleeSavedRegs

                 -- Set df to 0 at function start.
                 -- FIXME: We may want to check this at function calls and returns to ensure
                 -- ABI is correctly respected.
               & MapF.insert df_reg (FnRegValue (FnConstantValue n1 0))

  let rs = RS { _rsInterp = s
              , _rsNextAssignId = FnAssignId 0
              , _rsCurLabel  = lbl
              , _rsCurStmts  = Seq.empty
              , _rsAssignMap = MapF.empty
              , _rsCurRegs   = initRegs
              , rsSyscallPersonality = sysp
              , rsAssignmentsUsed = usedAssigns
              , rsCurrentFunctionType = cft
              , rsFunctionArgs    = fArgs
              }

--  let mkRegIds
  let recoverInnerBlock :: RecoveredBlockInfo
                        -> BlockLabel 64
                        -> Recover ids RecoveredBlockInfo
      recoverInnerBlock blockInfo lbl' = do
        let regs0 = case Map.lookup lbl' blockRegs of
                      Nothing -> debug DFunRecover ("WARNING: No regs for " ++ show (pretty lbl')) []
                      Just x  -> Set.toList x
        let mkIdFromReg :: MapF X86Reg FnRegValue
                        -> Some X86Reg
                        -> Recover ids (MapF X86Reg FnRegValue)
            mkIdFromReg m (Some r) = mkId addReg
              where addReg :: FnAssignId -> MapF X86Reg FnRegValue
                    addReg next_id =
                      let phi_var = FnPhiVar next_id (typeRepr r)
                       in MapF.insert r (FnRegValue $ FnPhiValue phi_var) m

        regs <- foldM mkIdFromReg MapF.empty regs0
        rsCurRegs    .= regs
        let phis = makePhis blockPreds lbl' regs
        recoverBlock blockRegProvides phis lbl' blockInfo


  evalRecover rs $ do
    -- The first block is special as it needs to allocate space for
    -- the block stack area.  It should also not be in blockPreds (we
    -- assume no recursion/looping through the initial block)

    -- Make the alloca and init rsp.  This is the only reason we
    -- need rsCurStmts

    case maximumStackDepth s a of
      Right depths ->  allocateStackFrame depths
      Left msg -> throwError $ "maximumStackDepth: " ++ msg
    r0 <- recoverBlock blockRegProvides MapF.empty lbl emptyRecoveredBlockInfo
    rf <- foldM recoverInnerBlock r0 (Map.keys blockPreds)

    pure Function { fnAddr = a
                  , fnSize = functionSize s a
                  , fnType = cft
                  , fnBlocks = Map.elems (recoveredBlocks rf)
                  , fnLabelRegMap = recoveredBlockEndRegs rf
                  }
makePhis :: Map (BlockLabel 64) [BlockLabel 64]
         -> BlockLabel 64
         -> MapF X86Reg FnRegValue
         -> MapF FnPhiVar FnPhiNodeInfo
makePhis blockPreds lbl regs = do

  let Just preds = Map.lookup lbl blockPreds

  let collate :: forall tp . X86Reg tp -> FnPhiNodeInfo tp
      collate r =
        let doOne lbl' = (lbl', r)
         in FnPhiNodeInfo (doOne <$> preds)

  let go :: forall tp
         .  X86Reg tp
         -> FnRegValue tp
         -> MapF FnPhiVar FnPhiNodeInfo
         -> MapF FnPhiVar FnPhiNodeInfo
      go r (FnRegValue (FnPhiValue phi_var)) m = MapF.insert phi_var (collate r) m
      go _ _ _ = error "impossible"

  MapF.foldrWithKey go MapF.empty regs

mkAddAssign :: FnAssignRhs tp -> Recover ids (FnValue tp)
mkAddAssign rhs = do
  fnAssign <- mkFnAssign rhs
  addFnStmt $ FnAssignStmt fnAssign
  return $ FnAssignedValue fnAssign

allocateStackFrame :: forall ids . Set (StackDepthValue X86_64 ids) -> Recover ids ()
allocateStackFrame sd
  | Set.null sd          =
    debug DFunRecover "WARNING: no stack use detected" $ return ()
  | [s] <- Set.toList sd = do
      let sz0 = FnConstantValue n64 (toInteger (negate $ staticPart s))
      let doOneDelta :: StackDepthOffset X86_64 ids
                     -> Recover ids (FnValue (BVType 64))
                     -> Recover ids (FnValue (BVType 64))
          doOneDelta (Pos _) _   = error "Saw positive stack delta"
          doOneDelta (Neg x) m_v = do
            v0 <- m_v
            v  <- recoverValue "stackDelta" x
            mkAddAssign $ FnEvalApp $ BVAdd knownNat v v0
      szv <- foldr doOneDelta (return sz0) (dynamicPart s)
      alloc <- mkAddAssign $ FnAlloca szv
      spTop <- mkAddAssign $ FnEvalApp $ BVAdd knownNat alloc szv
      rsCurRegs %= MapF.insert sp_reg (FnRegValue spTop)

  | otherwise            = debug DFunRecover "WARNING: non-singleton stack depth" $ return ()

-- regValuePair :: N.RegisterName cl
--              -> FnValue (BVType (N.RegisterClassBits cl))
--              -> Recover (Maybe (MapF.Pair N.RegisterName FnRegValue))
-- regValuePair nm v = return $ Just $ MapF.Pair nm (FnRegValue v)

-- FIXME: clag from RegisterUse.hs
lookupFunctionArgs :: SegmentedAddr 64
                   -> Recover ids FunctionType
lookupFunctionArgs faddr = do
  fArgs <- gets (Map.lookup faddr . rsFunctionArgs)
  case fArgs of
    Nothing -> do debugM DUrgent ("Warning: no args for function " ++ show faddr)
                  return ftMaximumFunctionType
    Just v  -> return v

      -- Figure out what is preserved across the function call.
getPostCallValue :: BlockLabel 64
                 -> RegState X86Reg (Value X86_64 ids)
                     -- ^ Value of registers before syscall
                 -> [FnReturnVar (BVType 64)] -- ^ Integer values returned by function.
                 -> [FnReturnVar XMMType]     -- ^ Floating point values returned by function.
                 -> X86Reg tp
                 -> Recover ids (FnValue tp)
getPostCallValue lbl proc_state intrs floatrs r = do
  case r of
    _ | Just Refl <- testEquality (typeRepr r) (BVTypeRepr n64)
      , Just rv <- lookup r ([rax_reg, rdx_reg] `zip` intrs) ->
        return $ FnReturn rv
      | Just Refl <- testEquality r sp_reg -> do
        spv <- recoverRegister proc_state sp_reg
        mkAddAssign $ FnEvalApp $ BVAdd knownNat spv (FnConstantValue n64 8)

    _ | Just Refl <- testEquality (typeRepr r) (BVTypeRepr n128)
      , Just rv <- lookup r (zip x86FloatResultRegs floatrs) ->
        return $ FnReturn rv

   -- df is 0 after a function call.
    _ | Just Refl <- testEquality r df_reg -> return $ FnConstantValue knownNat 0

    _ | Some r `Set.member` x86CalleeSavedRegs -> recoverRegister proc_state r
    _ -> debug DFunRecover ("WARNING: Nothing known about register "
                                         ++ show r ++ " at " ++ show lbl ++ "\n"
                                        ) $ do
      let nm = "post-call register " ++ show r
      return $! FnValueUnsupported nm (typeRepr r)


-- | Get value for register after a system call.
--
-- This is subtly different to that for function calls.
getPostSyscallValue :: BlockLabel 64
                       -- ^ Label of block where we syscall occurred.
                    -> RegState X86Reg (Value X86_64 ids)
                       -- ^ Value of registers before syscall
                    -> X86Reg tp
                    -> Recover ids (FnValue tp)
getPostSyscallValue lbl proc_state r =
  case r of
    _ | Just Refl <- testEquality r sp_reg -> do
          recoverRegister proc_state sp_reg
      | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister proc_state r

    _ | Just Refl <- testEquality r df_reg -> return $ FnConstantValue knownNat 0

    _ -> debug DFunRecover ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show lbl) $
      return (FnValueUnsupported ("post-syscall register " ++ show r) (typeRepr r))

{-
data FnRegNamer a = FnRegNamer a

instance Functor FnRegNamer where
  fmap f m = m >>= \v -> pure (f v)

instance Applicative FnRegNamer where

instance Monad FnRegNamer where

recoverRegisterName :: RegState X86Reg (Value X86_64 ids) -> X86Reg tp -> FnRegNamer (FnValue tp)
recoverRegisterName = undefined

nameReturnVar :: TypeRepr tp -> FnRegNamer (FnReturnVar tp)
nameReturnVar = undefined


-- Figure out what is preserved across the function call.
namePostCallValue :: BlockLabel 64
                  -> RegState X86Reg (Value X86_64 ids)
                     -- ^ Value of registers before syscall
                  -> [FnReturnVar (BVType 64)] -- ^ Integer values returned by function.
                  -> [FnReturnVar XMMType]     -- ^ Floating point values returned by function.
                  -> X86Reg tp
                  -> FnRegNamer (FnValue tp)
namePostCallValue lbl proc_state intrs floatrs r = do
  case r of
    _ | Just Refl <- testEquality (typeRepr r) (BVTypeRepr n64)
      , Just rv <- lookup r ([rax_reg, rdx_reg] `zip` intrs) ->
        return $ FnReturn rv
      | Just Refl <- testEquality r sp_reg -> do
        spv <- recoverRegisterName proc_state sp_reg
        mkAddAssign $ FnEvalApp $ BVAdd knownNat spv (FnConstantValue n64 8)

    _ | Just Refl <- testEquality (typeRepr r) (BVTypeRepr n128)
      , Just rv <- lookup r (zip x86FloatResultRegs floatrs) ->
        return $ FnReturn rv

   -- df is 0 after a function call.
    _ | Just Refl <- testEquality r df_reg -> return $ FnConstantValue knownNat 0

    _ | Some r `Set.member` x86CalleeSavedRegs -> recoverRegisterName proc_state r
    _ -> debug DFunRecover ("WARNING: Nothing known about register "
                                         ++ show r ++ " at " ++ show lbl ++ "\n"
                                        ) $ do
      let nm = "post-call register " ++ show r
      return $! FnValueUnsupported nm (typeRepr r)

-- | Get value for register after a system call.
--
-- This is subtly different to that for function calls.
namePostSyscallValue :: BlockLabel 64
                       -- ^ Label of block where we syscall occurred.
                    -> RegState X86Reg (Value X86_64 ids)
                       -- ^ Value of registers before syscall
                    -> X86Reg tp
                    -> FnRegNamer (FnValue tp)
namePostSyscallValue = undefined

blockRegValue :: forall ids
              .  DiscoveryInfo X86_64 ids
              -> SyscallPersonality X86_64
              -> FunctionArgs
              -> Map (BlockLabel 64) (Set (Some X86Reg))
              -> BlockLabel 64
              -> FnRegNamer (Map (BlockLabel 64) (MapF X86Reg FnRegValue))
blockRegValue interp_state sysp fargs blockRegProvides lbl = do
  Just b <- pure $ lookupParsedBlock interp_state lbl

  case pblockTerm b of
    ParsedTranslateError _ -> do
      error "Cannot recover function in blocks where translation error occurs."
    ClassifyFailure _ ->
      error $ "Classification failed in Recovery"
    ParsedBranch _c x y -> do
      xm <- blockRegValue interp_state sysp fargs blockRegProvides (lbl { labelIndex = x })
      ym <- blockRegValue interp_state sysp fargs blockRegProvides (lbl { labelIndex = y })
      pure $ mappend xm ym

    ParsedCall proc_state _m_ret_addr -> do
      let ft =
            case asLiteralAddr (memory interp_state) (proc_state^.boundValue ip_reg) of
              Nothing   -> ftMaximumFunctionType
              Just addr ->
                fromMaybe ftMaximumFunctionType $ Map.lookup addr fargs

      -- May not be used (only if called function returns at these types)
      intrs   <- replicateM (fnNIntRets ft) $ nameReturnVar (knownType :: TypeRepr (BVType 64))
      floatrs <- replicateM (fnNFloatRets ft ) $ nameReturnVar (knownType :: TypeRepr XMMType)

      -- Figure out what is preserved across the function call.
      let go :: MapF X86Reg FnRegValue
             -> Some X86Reg
             -> FnRegNamer (MapF X86Reg FnRegValue)
          go m (Some r) = do
            v <- namePostCallValue lbl proc_state intrs floatrs r
            return $ MapF.insert r (FnRegValue v) m

      let provides = blockRegProvides ^. ix lbl -- maybe mempty
      regs' <- foldM go MapF.empty provides

      return $ Map.singleton lbl regs'

    ParsedJump proc_state _tgt_addr -> do
        let go :: MapF X86Reg FnRegValue
               -> Some X86Reg
               -> FnRegNamer (MapF X86Reg FnRegValue)
            go m (Some r) = do
               v <- recoverRegisterName proc_state r
               return $ MapF.insert r (FnRegValue v) m

        let provides = blockRegProvides ^. ix lbl
        regs' <- foldM go MapF.empty provides
        pure $ Map.singleton lbl regs'

    ParsedReturn _ -> do
        pure Map.empty

    ParsedSyscall proc_state _next_addr -> do
      let rregs = spResultRegisters sysp


      let mkRet :: MapF X86Reg FnRegValue
                -> Some X86Reg
                -> FnRegNamer (MapF X86Reg FnRegValue)
          mkRet m (Some r) = do
            rv <- nameReturnVar (typeRepr r)
            return $ MapF.insert r (FnRegValue $ FnReturn rv) m

      initMap <- foldM mkRet MapF.empty rregs

      -- Fold operation to update register map with values persisted across system calls.
      let go ::  MapF X86Reg FnRegValue
             -> Some X86Reg
             -> FnRegNamer (MapF X86Reg FnRegValue)
          go m (Some r) = do
             v <- namePostSyscallValue lbl proc_state r
             return $ MapF.insert r (FnRegValue v) m

      let provides   = case Map.lookup lbl blockRegProvides of
                         Just p -> p
                         Nothing -> debug DFunRecover ("No blockRegProvides at " ++ show lbl) mempty

      regs' <- foldM go initMap (provides `Set.difference` Set.fromList rregs)

      return $ Map.singleton lbl regs'

    ParsedLookupTable proc_state _idx _vec -> do

      let go :: MapF X86Reg FnRegValue -> Some X86Reg -> FnRegNamer (MapF X86Reg FnRegValue)
          go m (Some r) = do
            v <- recoverRegisterName proc_state r
            return $ MapF.insert r (FnRegValue v) m
      regs' <- foldM go MapF.empty (blockRegProvides ^. ix lbl)
      pure $ Map.singleton lbl regs'

recoverFnBlock :: forall ids
             .  Map (BlockLabel 64) (Set (Some X86Reg))
             -> MapF FnPhiVar FnPhiNodeInfo
             -> BlockLabel 64
             -> Recover ids (Map (BlockLabel 64) FnBlock)
recoverFnBlock blockRegProvides phis lbl = do
  -- Clear stack offsets
  rsCurLabel  .= lbl

  let mkBlock tm = do
        stmts <- rsCurStmts <<.= Seq.empty
        return $! Map.singleton lbl $ FnBlock { fbLabel = lbl
                                              , fbPhiNodes = phis
                                              , fbStmts = Fold.toList stmts
                                              , fbTerm = tm
                                              }

  interp_state <- use rsInterp
  Just b <- pure $ lookupParsedBlock interp_state lbl
  -- Recover statements
  Fold.traverse_ recoverStmt (pblockStmts b)
  case pblockTerm b of
    ParsedTranslateError _ -> do
      error "Cannot recover function in blocks where translation error occurs."
    ClassifyFailure _ ->
      error $ "Classification failed: " ++ show (labelAddr lbl)
    ParsedBranch c x y -> do
      cv <- recoverValue "branch_cond" c
      fb <- mkBlock (FnBranch cv (lbl { labelIndex = x }) (lbl { labelIndex = y }))

      xb <- recoverFnBlock blockRegProvides MapF.empty (lbl { labelIndex = x })
      yb <- recoverFnBlock blockRegProvides MapF.empty (lbl { labelIndex = y })
      return $! mconcat [fb, xb, yb]

    ParsedCall proc_state m_ret_addr -> do
      ft <-
        case asLiteralAddr (memory interp_state) (proc_state^.boundValue ip_reg) of
          Nothing   -> return ftMaximumFunctionType
          Just addr -> lookupFunctionArgs addr

      -- May not be used (only if called function returns at these types)
      intrs   <- replicateM (fnNIntRets ft) $ mkReturnVar (knownType :: TypeRepr (BVType 64))
      floatrs <- replicateM (fnNFloatRets ft ) $ mkReturnVar (knownType :: TypeRepr XMMType)

      let ret_lbl = mkRootBlockLabel <$> m_ret_addr
      call_tgt <- recoverRegister proc_state ip_reg

      gargs' <- mapM (recoverRegister proc_state) (ftIntArgRegs ft)
      fargs' <- mapM (recoverRegister proc_state) (ftFloatArgRegs ft)
      -- args <- (++ stackArgs stk) <$> stateArgs proc_state

      mkBlock (FnCall call_tgt (gargs', fargs') (intrs, floatrs) ret_lbl)

    ParsedJump _ tgt_addr -> do
      let tgt_lbl = mkRootBlockLabel tgt_addr
      mkBlock (FnJump tgt_lbl)

    ParsedReturn proc_state -> do
        ft <- gets rsCurrentFunctionType
        grets' <- mapM (recoverRegister proc_state) (ftIntRetRegs ft)
        frets' <- mapM (recoverRegister proc_state) (ftFloatRetRegs ft)
        mkBlock (FnRet (grets', frets'))

    ParsedSyscall proc_state next_addr -> do
      sysp <- gets rsSyscallPersonality

      let syscallRegs :: [ArchReg X86_64 (BVType 64)]
          syscallRegs = syscallArgumentRegs

      let (name, call_no, args)
            | Just this_call_no <- tryGetStaticSyscallNo interp_state (labelAddr lbl) proc_state
            , Just (call_name,_,argtypes) <- Map.lookup (fromIntegral call_no) (spTypeInfo sysp) =
              (call_name, this_call_no, take (length argtypes) syscallRegs)
            | otherwise =
              ("unknown", 0, syscallRegs)

      let rregs = spResultRegisters sysp


      let mkRet :: MapF X86Reg FnRegValue
                -> Some X86Reg
                -> Recover ids (MapF X86Reg FnRegValue)
          mkRet m (Some r) = do
            rv <- mkReturnVar (typeRepr r)
            return $ MapF.insert r (FnRegValue $ FnReturn rv) m

      initMap <- foldM mkRet MapF.empty rregs

      -- pull the return variables out of initMap (in order of rregs)
      let getVar :: Maybe (FnRegValue tp) -> FnReturnVar tp
          getVar (Just (FnRegValue (FnReturn rv))) = rv
          getVar _ = error "impossible"

      let rets :: [Some FnReturnVar]
          rets = map f rregs
            where f (Some r) = Some $ getVar $ MapF.lookup r initMap

      args'  <- mapM (recoverRegister proc_state) args

      mkBlock (FnSystemCall (fromInteger call_no) name args' rets (mkRootBlockLabel next_addr))

    ParsedLookupTable _ idx vec -> do
      idx'   <- recoverValue "jump_index" idx
      mkBlock (FnLookupTable idx' vec)
-}

-- | Builds a mapping from labels to recovered function blocks and a mapping from the block label to the
-- register map resulting from block.
data RecoveredBlockInfo
   = RecoveredBlockInfo { recoveredBlocks :: !(Map (BlockLabel 64) FnBlock)
                        , recoveredBlockEndRegs :: !(Map (BlockLabel 64) (MapF X86Reg FnRegValue))
                          -- ^ End register end regs.
                        }

emptyRecoveredBlockInfo :: RecoveredBlockInfo
emptyRecoveredBlockInfo =
  RecoveredBlockInfo { recoveredBlocks = Map.empty
                     , recoveredBlockEndRegs = Map.empty
                     }

addFnBlock :: FnBlock -> RecoveredBlockInfo -> RecoveredBlockInfo
addFnBlock b info = seq b $ info { recoveredBlocks = Map.insert (fbLabel b) b (recoveredBlocks info) }

addFnBlockAndRegs :: FnBlock -> MapF X86Reg FnRegValue -> RecoveredBlockInfo -> RecoveredBlockInfo
addFnBlockAndRegs b m info = seq b $ seq m $
  RecoveredBlockInfo { recoveredBlocks       = Map.insert (fbLabel b) b (recoveredBlocks info)
                     , recoveredBlockEndRegs = Map.insert (fbLabel b) m (recoveredBlockEndRegs info)
                     }

recoverBlock :: forall ids
             .  Map (BlockLabel 64) (Set (Some X86Reg))
             -> MapF FnPhiVar FnPhiNodeInfo
             -> BlockLabel 64
             -> RecoveredBlockInfo
             -> Recover ids RecoveredBlockInfo
recoverBlock blockRegProvides phis lbl blockInfo = seq blockInfo $ do
  -- Clear stack offsets
  rsCurLabel  .= lbl

  let mkBlock tm = do
        stmts <- use rsCurStmts
        rsCurStmts .= Seq.empty
        return $! FnBlock { fbLabel = lbl
                          , fbPhiNodes = phis
                          , fbStmts = Fold.toList stmts
                          , fbTerm = tm
                          }
  interp_state <- use rsInterp
  Just b <- pure $ lookupParsedBlock interp_state lbl
  -- Recover statements
  Fold.traverse_ recoverStmt (pblockStmts b)
  case pblockTerm b of
    ParsedTranslateError _ -> do
      error "Cannot recover function in blocks where translation error occurs."
    ClassifyFailure _ ->
      error $ "Classification failed in Recovery"
    ParsedBranch c x y -> do
      cv <- recoverValue "branch_cond" c
      fb <- mkBlock (FnBranch cv (lbl { labelIndex = x }) (lbl { labelIndex = y }))

      xr <- recoverBlock blockRegProvides MapF.empty (lbl { labelIndex = x }) (blockInfo & addFnBlock fb)
      recoverBlock blockRegProvides MapF.empty (lbl { labelIndex = y }) xr

    ParsedCall proc_state m_ret_addr -> do
      ft <-
        case asLiteralAddr (memory interp_state) (proc_state^.boundValue ip_reg) of
          Nothing   -> return ftMaximumFunctionType
          Just addr -> lookupFunctionArgs addr

      -- May not be used (only if called function returns at these types)
      intrs   <- replicateM (fnNIntRets ft) $ mkReturnVar (knownType :: TypeRepr (BVType 64))
      floatrs <- replicateM (fnNFloatRets ft ) $ mkReturnVar (knownType :: TypeRepr XMMType)

      -- Figure out what is preserved across the function call.
      let go :: MapF X86Reg FnRegValue
             -> Some X86Reg
             -> Recover ids (MapF X86Reg FnRegValue)
          go m (Some r) = do
            v <- getPostCallValue lbl proc_state intrs floatrs r
            return $ MapF.insert r (FnRegValue v) m

      let provides = blockRegProvides ^. ix lbl -- maybe mempty
      regs' <- foldM go MapF.empty provides

      let ret_lbl = mkRootBlockLabel <$> m_ret_addr
      call_tgt <- recoverRegister proc_state ip_reg

      gargs' <- mapM (recoverRegister proc_state) (ftIntArgRegs ft)
      fargs' <- mapM (recoverRegister proc_state) (ftFloatArgRegs ft)
      -- args <- (++ stackArgs stk) <$> stateArgs proc_state

      fb <- mkBlock (FnCall call_tgt (gargs', fargs') (intrs, floatrs) ret_lbl)
      return $! blockInfo & addFnBlockAndRegs fb regs'

    ParsedJump proc_state tgt_addr -> do
      let go :: MapF X86Reg FnRegValue -> Some X86Reg -> Recover ids (MapF X86Reg FnRegValue)
          go m (Some r) = do
            v <- recoverRegister proc_state r
            return $ MapF.insert r (FnRegValue v) m

      let provides = blockRegProvides ^. ix lbl
      regs' <- foldM go MapF.empty provides

      let tgt_lbl = mkRootBlockLabel tgt_addr
      bl <- mkBlock (FnJump tgt_lbl)
      pure $! blockInfo & addFnBlockAndRegs bl regs'

    ParsedReturn proc_state -> do
      ft <- gets rsCurrentFunctionType
      grets' <- mapM (recoverRegister proc_state) (ftIntRetRegs ft)
      frets' <- mapM (recoverRegister proc_state) (ftFloatRetRegs ft)
      bl <- mkBlock (FnRet (grets', frets'))
      pure $! blockInfo & addFnBlock bl

    ParsedSyscall proc_state next_addr -> do
      sysp <- gets rsSyscallPersonality

      let syscallRegs :: [ArchReg X86_64 (BVType 64)]
          syscallRegs = syscallArgumentRegs

      let (name, call_no, args)
            | Just this_call_no <- tryGetStaticSyscallNo interp_state (labelAddr lbl) proc_state
            , Just (call_name,_,argtypes) <- Map.lookup (fromIntegral this_call_no) (spTypeInfo sysp) =
              (call_name, this_call_no, take (length argtypes) syscallRegs)
            | otherwise =
              ("unknown", 0, syscallRegs)

      let rregs = spResultRegisters sysp


      let mkRet :: MapF X86Reg FnRegValue
                -> Some X86Reg
                -> Recover ids (MapF X86Reg FnRegValue)
          mkRet m (Some r) = do
            rv <- mkReturnVar (typeRepr r)
            return $ MapF.insert r (FnRegValue $ FnReturn rv) m

      initMap <- foldM mkRet MapF.empty rregs

      -- pull the return variables out of initMap (in order of rregs)
      let getVar :: Maybe (FnRegValue tp) -> FnReturnVar tp
          getVar (Just (FnRegValue (FnReturn rv))) = rv
          getVar _ = error "impossible"

      let rets :: [Some FnReturnVar]
          rets = map f rregs
            where f (Some r) = Some $ getVar $ MapF.lookup r initMap

      -- Fold operation to update register map with values persisted across system calls.
      let go ::  MapF X86Reg FnRegValue
                 -> Some X86Reg
                 -> Recover ids (MapF X86Reg FnRegValue)
          go m (Some r) = do
             v <- getPostSyscallValue lbl proc_state r
             return $ MapF.insert r (FnRegValue v) m

      let m_provides = Map.lookup lbl blockRegProvides
          provides   = case m_provides of
                         Just p -> p
                         Nothing -> debug DFunRecover ("No blockRegProvides at " ++ show lbl) mempty

      regs' <- foldM go initMap (provides `Set.difference` Set.fromList rregs)

      args'  <- mapM (recoverRegister proc_state) args
      -- args <- (++ stackArgs stk) <$> stateArgs proc_state

      fb <- mkBlock (FnSystemCall (fromInteger call_no) name args' rets (mkRootBlockLabel next_addr))
      pure $! blockInfo & addFnBlockAndRegs fb regs'

    ParsedLookupTable proc_state idx vec -> do
      let go :: MapF X86Reg FnRegValue
             -> Some X86Reg
             -> Recover ids (MapF X86Reg FnRegValue)
          go m (Some r) = do
            v <- recoverRegister proc_state r
            return $ MapF.insert r (FnRegValue v) m

      let provides = blockRegProvides ^. ix lbl
      regs' <- foldM go MapF.empty provides

      idx'   <- recoverValue "jump_index" idx

      bl <-  mkBlock (FnLookupTable idx' vec)
      pure $! blockInfo & addFnBlockAndRegs bl regs'

recoverWrite :: BVValue X86_64 ids 64 -> Value X86_64 ids tp -> Recover ids ()
recoverWrite addr val = do
  r_addr <- recoverAddr addr
  r_val  <- recoverValue "write_val" val
  addFnStmt $ FnWriteMem r_addr r_val

emitAssign :: AssignId ids tp -> FnAssignRhs tp -> Recover ids (FnAssignment tp)
emitAssign asgn rhs = do
  fnAssign <- mkFnAssign rhs
  rsAssignMap %= MapF.insert asgn fnAssign
  addFnStmt $ FnAssignStmt fnAssign
  return fnAssign

-- | This should add code as needed to support the statement.
recoverStmt :: Stmt X86_64 ids -> Recover ids ()
recoverStmt s = do
  case s of
    AssignStmt asgn -> do
      usedAssigns <- gets rsAssignmentsUsed
      -- Only add assignment if it is used.
      when (Some (assignId asgn) `Set.member` usedAssigns) $ do
        void $ recoverAssign asgn
    WriteMem addr val
      | Initial reg <- val -> do
        reg_v <- uses rsCurRegs (MapF.lookup reg)
        case reg_v of
          Just (CalleeSaved _saved_reg) -> do
            case asStackAddrOffset addr of
              Just _int_addr_off -> do
                -- modifyCurStack $ recordCalleeSavedWrite int_addr_off saved_reg
                return ()
              Nothing -> debug DFunRecover "Could not interpret callee saved offset" $ do
                recoverWrite addr val
          _ -> do
            recoverWrite addr val
      | otherwise -> do
        recoverWrite addr val
    Comment msg -> do
      addFnStmt $ FnComment msg
    ExecArchStmt (MemCopy bytesPerCopy nValues src dest direction) -> do
      stmt <- FnMemCopy bytesPerCopy <$> recoverValue "memcopy" nValues
                                     <*> recoverValue "memcopy" src
                                     <*> recoverValue "memcopy" dest
                                     <*> recoverValue "memcopy" direction
      addFnStmt stmt
    ExecArchStmt (MemSet count v ptr df) -> do
      stmt <- FnMemSet <$> recoverValue "memset" count
                       <*> recoverValue "memset" v
                       <*> recoverValue "memset" ptr
                       <*> recoverValue "memset" df
      addFnStmt stmt
    _ -> debug DFunRecover ("recoverStmt undefined for " ++ show (pretty s)) $ do
      addFnStmt $ FnComment (fromString $ "UNIMPLEMENTED: " ++ show (pretty s))
      return ()

recoverAddr :: BVValue X86_64 ids 64 -> Recover ids (FnValue (BVType 64))
recoverAddr v = recoverValue "addr" v

recoverAssign :: Assignment X86_64 ids tp -> Recover ids (FnValue tp)
recoverAssign asgn = do
  m_seen <- uses rsAssignMap (MapF.lookup (assignId asgn))
  case m_seen of
    Just fnAssign -> return $! FnAssignedValue fnAssign
    Nothing -> do
      case assignRhs asgn of
        EvalApp app -> do
          app' <- traverseApp (recoverValue "recoverAssign") app
          fnAssign <- emitAssign (assignId asgn) (FnEvalApp app')
          return $! FnAssignedValue fnAssign
        SetUndefined w ->
          FnAssignedValue <$> emitAssign (assignId asgn) (FnSetUndefined w)
        ReadMem addr tp -> do
          fn_addr <- recoverAddr addr
          FnAssignedValue <$> emitAssign (assignId asgn) (FnReadMem fn_addr tp)
        _ -> do
          debug DFunRecover ("recoverAssign does not yet support assignment " ++ show (pretty asgn)) $
            return $ FnValueUnsupported ("assignment " ++ show (pretty asgn))
                                        (typeRepr (assignRhs asgn))

recoverRegister :: RegState X86Reg (Value X86_64 ids)
                -> X86Reg tp
                -> Recover ids (FnValue tp)
recoverRegister proc_state r = recoverValue ("register " ++ show r) (proc_state ^. boundValue r)

hasWidth :: HasRepr f TypeRepr => f tp -> NatRepr w -> Maybe (tp :~: BVType w)
hasWidth f w =
  case typeRepr f of
    BVTypeRepr n -> do
      Refl <- testEquality n w
      pure Refl


recoverValue :: String -> Value X86_64 ids tp -> Recover ids (FnValue tp)
recoverValue nm v = do
  interpState <- use rsInterp
  mem <- uses rsInterp memory
  case v of
    _ | Just Refl <- hasWidth v (memWidth mem)
      , Just addr <- asLiteralAddr mem v -> do
        let seg = addrSegment addr
        case () of
          _ | segmentFlags seg `Perm.hasPerm` Perm.execute
            , Set.member addr (interpState^.functionEntries) -> do
              ft <- lookupFunctionArgs addr
              return $! FnFunctionEntryValue ft addr

            | segmentFlags seg `Perm.hasPerm` Perm.execute
            , Map.member addr (interpState^.blocks) -> do
              return $! FnBlockValue addr

            | segmentFlags seg `Perm.hasPerm` Perm.write -> do
              return $! FnGlobalDataAddr addr

            -- FIXME: do something more intelligent with rodata?
            | segmentFlags seg `Perm.hasPerm` Perm.read -> do
              return $! FnGlobalDataAddr addr

            | otherwise -> do
              debug DFunRecover ("WARNING: recoverValue " ++ nm ++ " given segment pointer: " ++ show addr) $ do
              return $! FnValueUnsupported ("segment pointer " ++ show addr) (typeRepr v)
    RelocatableValue{} -> error "Expected relocatable value to be covered by previous case."
    BVValue w i ->
      return $ FnConstantValue w i

    AssignedValue assign' -> recoverAssign assign'

    Initial reg -> do
      reg_v <- uses rsCurRegs (MapF.lookup reg)
      case reg_v of
        Nothing ->
          return (FnValueUnsupported ("Initial register " ++ show reg) (typeRepr reg))
        Just (CalleeSaved _) -> do
          -- trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          return $! FnValueUnsupported ("Initial (callee) register " ++ show reg)
                                       (typeRepr reg)
        Just (FnRegValue v') -> return v'
