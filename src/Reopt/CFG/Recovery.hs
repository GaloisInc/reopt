{-|
Module      : Reopt.CFG.Recovery
Copyright   : (c) Galois Inc, 2015
Maintainer  : jhendrix@galois.com

This module provides methods for constructing functions from the basic
blocks discovered by 'Reopt.CFG.CFGDiscovery'.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
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
  , recoverFunctions
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
import           Data.Type.Equality
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.String (fromString)
import           Reopt.CFG.DiscoveryInfo
import           Reopt.CFG.FnRep
import           Reopt.CFG.FunctionArgs ( functionArgs )
import           Reopt.CFG.RegisterUse
import           Reopt.CFG.Representation
import           Reopt.CFG.StackDepth
import           Reopt.Machine.Types
import           Reopt.Machine.X86State
import           Reopt.Object.Memory
import           Reopt.Utils.Debug

import           Debug.Trace (trace)

------------------------------------------------------------------------
-- RecoverState

type FnRegValueMap = MapF X86Reg FnRegValue

data RecoverState = RS { _rsInterp :: !DiscoveryInfo
                       , _rsNextAssignId :: !AssignId
                       , _rsAssignMap :: !(MapF (Assignment X86_64) FnAssignment)

                         -- Local state
                       , _rsCurLabel  :: !(BlockLabel Word64)
                       , _rsCurStmts  :: !(Seq FnStmt)

                       , _rsCurRegs   :: !(FnRegValueMap)
                         -- ^ This maps registers to the associated value
                         -- at the start of the block after any stack allocations have
                         -- been performed.

                       , rsAssignmentsUsed :: !(Set AssignId)
                       , rsCurrentFunctionType :: !FunctionType
                       , rsFunctionArgs    :: !FunctionArgs
                       }

rsInterp :: Simple Lens RecoverState DiscoveryInfo
rsInterp = lens _rsInterp (\s v -> s { _rsInterp = v })

rsNextAssignId :: Simple Lens RecoverState AssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

rsCurLabel :: Simple Lens RecoverState (BlockLabel Word64)
rsCurLabel = lens _rsCurLabel (\s v -> s { _rsCurLabel = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens RecoverState (Seq FnStmt)
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens RecoverState (MapF (Assignment X86_64) FnAssignment)
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

-- | This maps registers to the associated value at the start of the block after
-- any stack allocations have been performed.
rsCurRegs :: Simple Lens RecoverState FnRegValueMap
rsCurRegs = lens _rsCurRegs (\s v -> s { _rsCurRegs = v })

------------------------------------------------------------------------
-- Recover

type Recover a = StateT RecoverState (ExceptT String Identity) a

runRecover :: RecoverState -> Recover a -> Either String a
runRecover s m = runIdentity $ runExceptT $ evalStateT m s

-- getCurStack :: Recover FnStack
-- getCurStack = do
--   lbl  <- use rsCurLabel
--   m_stk <- uses rsStackMap (Map.lookup lbl)
--   case m_stk of
--     Nothing -> error "Current stack undefined."
--     Just stk -> return stk

-- modifyCurStack :: (FnStack -> FnStack) -> Recover ()
-- modifyCurStack f = do
--   lbl  <- use rsCurLabel
--   m_stk <- uses rsStackMap (Map.lookup lbl)
--   case m_stk of
--     Nothing -> error "Current stack undefined."
--     Just stk -> rsStackMap . at lbl .= Just (f stk)

-- | Return value bound to register (if any)
-- getCurRegs :: Recover (MapF X86Reg FnRegValue)
-- getCurRegs = do
--   lbl <- use rsCurLabel
--   maybe_map <- uses rsRegMap (Map.lookup lbl)
--   case maybe_map of
--     Nothing -> do
--       error $ "Did not define register map for " ++ show lbl ++ "."
--     Just reg_map -> do
--       return reg_map

mkId :: (AssignId -> a) -> Recover a
mkId f = do
  next_id <- use rsNextAssignId
  rsNextAssignId .= next_id + 1
  return $! f next_id

mkFnAssign :: FnAssignRhs tp -> Recover (FnAssignment tp)
mkFnAssign rhs = mkId (\next_id -> FnAssignment next_id rhs)

_mkPhiVar :: TypeRepr tp -> Recover (FnPhiVar tp)
_mkPhiVar tp = mkId (flip FnPhiVar tp)

mkReturnVar :: TypeRepr tp -> Recover (FnReturnVar tp)
mkReturnVar tp = mkId (\next_id -> FnReturnVar next_id tp)

addFnStmt :: FnStmt -> Recover ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

-- | Add a sub-block to the frontier.
-- addSubBlockFrontier :: BlockLabel
--                        -- ^ Label for block
--                        -> X86State FnRegValue
--                        -- ^ Map from register names to value (at src)
--                        -> FnStack
--                        -- ^ State of stack when frontier occurs.
--                        -> MapF Assignment FnAssignment
--                        -- ^ Register rename map.
--                        -> Recover ()
-- addSubBlockFrontier lbl regs stk assigns = do
--   when (isRootBlockLabel lbl) $ fail "Expecting a subblock label"
--   mr <- uses rsBlocks (Map.lookup lbl)
--   case mr of
--     Nothing -> debug DFunRecover ("Adding block to frontier: " ++ show (pretty lbl)) $ do
--       rsFrontier %= Set.insert lbl
--       rsStackMap %= Map.insert lbl stk
--       rsSubBlockState %= Map.insert lbl (regs, assigns)

--     Just b -> debug DFunRecover ("WARNING: Saw a sub-block again (" ++ show lbl ++ ")") $
--               return ()

-- addFrontier :: BlockLabel
--                -- ^ Incoming edge
--                -> BlockLabel
--                -- ^ Label for block
--                -> X86State FnValue
--                -- ^ Map from register names to value (at src)
--                -> FnStack
--                -- ^ State of stack when frontier occurs.
--                -> Recover ()
-- addFrontier src lbl regs stk = do
--   unless (isRootBlockLabel lbl) $ fail "Expecting a root label"
--   mr <- uses rsBlocks (Map.lookup lbl)
--   case mr of
--     Nothing -> debug DFunRecover ("Adding block to frontier: " ++ show (pretty lbl)) $ do
--       rsFrontier %= Set.insert lbl
--       rsPredRegs %= Map.insertWith (Map.union) lbl (Map.singleton src regs)
--       rsStackMap %= Map.insert lbl stk
--     Just b -> do
--       -- Add this block as a predecessor of lbl
--       let b' = b { fbPhiNodes = Map.insert src regs (fbPhiNodes b) }
--       rsBlocks %= Map.insert lbl b'

-- | Return value bound to register (if any)

-- lookupInitialReg :: N.RegisterName cl -> Recover (Maybe (FnRegValue cl))
-- lookupInitialReg reg = MapF.lookup reg <$> getCurRegs

{-
regMapFromState :: X86State f -> MapF X86Reg f
regMapFromState s =
  MapF.fromList [ MapF.Pair nm (s^.register nm)
                | Some nm <- x86StateRegisters
                ]
-}

------------------------------------------------------------------------
-- recoverFunction

type FunctionArgs = Map CodeAddr FunctionType

-- | This returns how much space there is before start of next function,
-- or the end of the memory segment if code address is undefined.
--
-- Note: Calls error if code addr is not in a valid memory location.
functionEnd :: DiscoveryInfo -> CodeAddr -> CodeAddr
functionEnd s a =
  case findSegment a (memory s) of
    Just seg -> assert (memFlags seg `hasPermissions` pf_x) $
      let end = memBase seg + segmentSize seg
       in case Set.lookupGT a (s^.functionEntries) of
            Just next | next < end -> next
            _ -> end
    Nothing -> error $ "Could not find memory segment for " ++ showHex a "."


-- | Recover the function at a given address.
recoverFunction :: FunctionArgs -> DiscoveryInfo -> CodeAddr -> Either String Function
recoverFunction fArgs s a = do
  let (usedAssigns, blockRegs, blockRegProvides, blockPreds)
        = registerUse fArgs s a

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
              , _rsNextAssignId = 0
              , _rsCurLabel  = lbl
              , _rsCurStmts  = Seq.empty
              , _rsAssignMap = MapF.empty
              , _rsCurRegs   = initRegs
              , rsAssignmentsUsed = usedAssigns
              , rsCurrentFunctionType = cft
              , rsFunctionArgs    = fArgs
              }

  let recoverInnerBlock blockRegMap lbl' = do
        (phis, regs) <- makePhis blockRegs blockPreds blockRegMap lbl'
        rsCurRegs    .= regs
        recoverBlock blockRegProvides phis lbl'

  let go ~(_, blockRegMap) = do
         -- Make the alloca and init rsp.  This is the only reason we
         -- need rsCurStmts
         allocateStackFrame (maximumStackDepth s a)
         r0 <- recoverBlock blockRegProvides MapF.empty lbl
         rets <- mapM (recoverInnerBlock blockRegMap) (Map.keys blockPreds)
         -- disjoint maps here, so mconcat is OK
         return (mconcat (r0 : rets))

  runRecover rs $ do
    -- The first block is special as it needs to allocate space for
    -- the block stack area.  It should also not be in blockPreds (we
    -- assume no recursion/looping through the initial block)
    (block_map, _) <- mfix go

    return $! Function { fnAddr = a
                       , fnSize = functionEnd s a - a
                       , fnType = cft
                       , fnBlocks = Map.elems block_map
                       }

makePhis :: Map (BlockLabel Word64) (Set (Some X86Reg))
         -> Map (BlockLabel Word64) [BlockLabel Word64]
         -> Map (BlockLabel Word64) (MapF X86Reg FnRegValue)
         -> BlockLabel Word64
         -> Recover (MapF FnPhiVar FnPhiNodeInfo, FnRegValueMap)
makePhis blockRegs blockPreds blockRegMap lbl = do
  let mkIdFromReg :: MapF X86Reg FnRegValue
                  -> Some X86Reg
                  -> Recover (MapF X86Reg FnRegValue)
      mkIdFromReg m (Some r) = mkId (addReg m r)
  regs <- foldM mkIdFromReg MapF.empty regs0
  let nodes = MapF.foldrWithKey go MapF.empty regs
  return (nodes, regs)
  where
    addReg :: MapF X86Reg FnRegValue -> X86Reg tp -> AssignId -> MapF X86Reg FnRegValue
    addReg m r next_id =
      let phi_var = FnPhiVar next_id (typeRepr r)
       in MapF.insert r (FnRegValue $ FnPhiValue phi_var) m
    -- FIXME
    go :: forall tp
       .  X86Reg tp
       -> FnRegValue tp
       -> MapF FnPhiVar FnPhiNodeInfo
       -> MapF FnPhiVar FnPhiNodeInfo
    go r (FnRegValue (FnPhiValue phi_var)) m =
      MapF.insert phi_var (collate r) m
    go _ _ _ = error "impossible"

    collate :: forall tp . X86Reg tp -> FnPhiNodeInfo tp
    collate r =
      let undef lbl' =
            ( lbl'
            , FnValueUnsupported
                ("makePhis " ++ show r ++ " at " ++ show (pretty lbl'))
                (typeRepr r)
            )
          doOne lbl' =
            fromMaybe (debug DFunRecover ("WARNING: missing blockRegMap/register for " ++ show (pretty lbl')) (undef lbl')) $
            do rm <- Map.lookup lbl' blockRegMap
               FnRegValue rv <- MapF.lookup r rm
               return (lbl', rv)
      in FnPhiNodeInfo (map doOne preds)

    Just preds = Map.lookup lbl blockPreds
    regs0 = case Set.toList <$> Map.lookup lbl blockRegs of
              Nothing -> debug DFunRecover ("WARNING: No regs for " ++ show (pretty lbl)) []
              Just x  -> x

mkAddAssign :: FnAssignRhs tp -> Recover (FnValue tp)
mkAddAssign rhs = do
  fnAssign <- mkFnAssign rhs
  addFnStmt $ FnAssignStmt fnAssign
  return $ FnAssignedValue fnAssign

allocateStackFrame :: Set StackDepthValue -> Recover ()
allocateStackFrame sd
  | Set.null sd          = debug DFunRecover "WARNING: no stack use detected" $ return ()
  | [s] <- Set.toList sd = do
      let sz0 = FnConstantValue n64 (toInteger (negate $ staticPart s))
      szv <- foldr doOneDelta (return sz0) (dynamicPart s)
      alloc <- mkAddAssign $ FnAlloca szv
      spTop <- mkAddAssign $ FnEvalApp $ BVAdd knownNat alloc szv
      rsCurRegs %= MapF.insert sp_reg (FnRegValue spTop)

  | otherwise            = debug DFunRecover "WARNING: non-singleton stack depth" $ return ()
  where
    doOneDelta :: StackDepthOffset
                  -> Recover (FnValue (BVType 64))
                  -> Recover (FnValue (BVType 64))
    doOneDelta (Pos _) _   = error "Saw positive stack delta"
    doOneDelta (Neg x) m_v =
      do v0 <- m_v
         v  <- recoverValue "stackDelta" x
         mkAddAssign $ FnEvalApp $ BVAdd knownNat v v0

-- regValuePair :: N.RegisterName cl
--              -> FnValue (BVType (N.RegisterClassBits cl))
--              -> Recover (Maybe (MapF.Pair N.RegisterName FnRegValue))
-- regValuePair nm v = return $ Just $ MapF.Pair nm (FnRegValue v)

-- FIXME: clag from RegisterUse.hs
lookupFunctionArgs :: Either CodeAddr (BVValue X86_64 64)
                   -> Recover FunctionType
lookupFunctionArgs fn =
  case fn of
    Right _dynaddr -> return ftMaximumFunctionType
    Left faddr -> do
      fArgs <- gets (Map.lookup faddr . rsFunctionArgs)
      case fArgs of
        Nothing -> do debugM DUrgent ("Warning: no args for function " ++ show faddr)
                      return ftMaximumFunctionType
        Just v  -> return v

      -- Figure out what is preserved across the function call.
getPostCallValue :: BlockLabel Word64
                 -> X86State (Value X86_64)
                     -- ^ Value of registers before syscall
                 -> [FnReturnVar (BVType 64)] -- ^ Integer values returned by function.
                 -> [FnReturnVar XMMType]     -- ^ Floating point values returned by function.
                 -> X86Reg tp
                 -> Recover (FnValue tp)
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
getPostSyscallValue :: BlockLabel Word64
                       -- ^ Label of block where we syscall occurred.
                    -> X86State (Value X86_64)
                       -- ^ Value of registers before syscall
                    -> X86Reg tp
                    -> Recover (FnValue tp)
getPostSyscallValue lbl proc_state r =
  case r of
    _ | Just Refl <- testEquality r sp_reg -> do
          recoverRegister proc_state sp_reg
      | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister proc_state r

    _ | Just Refl <- testEquality r df_reg -> return $ FnConstantValue knownNat 0

    _ -> debug DFunRecover ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show lbl) $
      return (FnValueUnsupported ("post-syscall register " ++ show r) (typeRepr r))

recoverBlock :: Map (BlockLabel Word64) (Set (Some X86Reg))
             -> MapF FnPhiVar FnPhiNodeInfo
             -> BlockLabel Word64
             -> Recover ( Map (BlockLabel Word64) FnBlock
                        , Map (BlockLabel Word64) (MapF X86Reg FnRegValue)
                        )
recoverBlock blockRegProvides phis lbl = do
  -- Clear stack offsets
  rsCurLabel  .= lbl

  let mkBlock tm = do
      stmts <- rsCurStmts <<.= Seq.empty
      return $! Map.singleton lbl $ FnBlock { fbLabel = lbl
                                            , fbPhiNodes = phis
                                            , fbStmts = Fold.toList stmts
                                            , fbTerm = tm
                                            }

  Just (b, m_pterm) <- uses rsInterp (getClassifyBlock lbl)

  case m_pterm of
    Just (ParsedBranch c x y) -> do
      Fold.traverse_ recoverStmt (blockStmts b)
      cv <- recoverValue "branch_cond" c
      fb <- mkBlock (FnBranch cv x y)

      xr <- recoverBlock blockRegProvides MapF.empty x
      yr <- recoverBlock blockRegProvides MapF.empty y
      return $! mconcat [(fb, Map.empty), xr, yr]

    Just (ParsedCall proc_state prev_stmts fn m_ret_addr) -> do
      Fold.traverse_ recoverStmt prev_stmts

      ft <- lookupFunctionArgs fn

      -- May not be used (only if called function returns at these types)
      intrs   <- replicateM (fnNIntRets ft) $ mkReturnVar (knownType :: TypeRepr (BVType 64))
      floatrs <- replicateM (fnNFloatRets ft ) $ mkReturnVar (knownType :: TypeRepr XMMType)

      -- Figure out what is preserved across the function call.
      let go ::  MapF X86Reg FnRegValue
             -> Some X86Reg
             -> Recover (MapF X86Reg FnRegValue)
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
      return $! (fb, Map.singleton lbl regs')

    Just (ParsedJump proc_state tgt_addr) -> do

        -- Recover statements
        Fold.traverse_ recoverStmt (blockStmts b)

        let go :: MapF X86Reg FnRegValue
               -> Some X86Reg
               -> Recover (MapF X86Reg FnRegValue)
            go m (Some r) = do
               v <- recoverRegister proc_state r
               return $ MapF.insert r (FnRegValue v) m

        let provides = blockRegProvides ^. ix lbl
        regs' <- foldM go MapF.empty provides

        let tgt_lbl = mkRootBlockLabel tgt_addr
        flip (,) (Map.singleton lbl regs') <$> mkBlock (FnJump tgt_lbl)

    Just (ParsedReturn proc_state nonret_stmts) -> do
        -- Recover statements
        Fold.traverse_ recoverStmt nonret_stmts

        ft <- gets rsCurrentFunctionType

        grets' <- mapM (recoverRegister proc_state) (ftIntRetRegs ft)
        frets' <- mapM (recoverRegister proc_state) (ftFloatRetRegs ft)

        flip (,) Map.empty <$> mkBlock (FnRet (grets', frets'))

    Just (ParsedSyscall proc_state next_addr call_no pname name args rreg_names) -> do
      Fold.traverse_ recoverStmt (blockStmts b)

      let Just rregs = traverse (\(Some nm) -> Some <$> x86Reg nm) rreg_names

      let mkRet :: MapF X86Reg FnRegValue
                -> Some X86Reg
                -> Recover (MapF X86Reg FnRegValue)
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
                 -> Recover (MapF X86Reg FnRegValue)
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

      fb <- mkBlock (FnSystemCall call_no pname name args' rets (mkRootBlockLabel next_addr))
      return $! (fb, Map.singleton lbl regs')

    Just (ParsedLookupTable proc_state idx vec) -> do
        -- Recover statements
        Fold.traverse_ recoverStmt (blockStmts b)

        let go :: MapF X86Reg FnRegValue
               -> Some X86Reg
               -> Recover (MapF X86Reg FnRegValue)
            go m (Some r) = do
              v <- recoverRegister proc_state r
              return $ MapF.insert r (FnRegValue v) m

        let provides = blockRegProvides ^. ix lbl
        regs' <- foldM go MapF.empty provides

        idx'   <- recoverValue "jump_index" idx

        flip (,) (Map.singleton lbl regs') <$> mkBlock (FnLookupTable idx' vec)

    Nothing -> do
      debug DFunRecover ("WARNING: recoverTermStmt undefined for " ++ show (pretty (blockTerm b))) $ do

      Fold.traverse_ recoverStmt (blockStmts b)
      flip (,) Map.empty <$> mkBlock FnTermStmtUndefined

recoverWrite :: BVValue X86_64 64 -> Value X86_64 tp -> Recover ()
recoverWrite addr val = do
  r_addr <- recoverAddr addr
  r_val  <- recoverValue "write_val" val
  addFnStmt $ FnWriteMem r_addr r_val

emitAssign :: Assignment X86_64 tp -> FnAssignRhs tp -> Recover (FnAssignment tp)
emitAssign asgn rhs = do
  fnAssign <- mkFnAssign rhs
  rsAssignMap %= MapF.insert asgn fnAssign
  addFnStmt $ FnAssignStmt fnAssign
  return fnAssign

-- | This should add code as needed to support the statement.
recoverStmt :: Stmt X86_64 -> Recover ()
recoverStmt s = do
  case s of
    AssignStmt asgn -> do
      usedAssigns <- gets rsAssignmentsUsed
      do lbl <- use rsCurLabel
         when (labelAddr lbl == 0x400128) $ do
           trace ("At block " ++ showHex (labelAddr lbl) ".") $ do
           trace ("Used " ++ show usedAssigns) $ do
             return ()
      -- Only add assignment if it is used.
      when (assignId asgn `Set.member` usedAssigns) $ do
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

recoverAddr :: BVValue X86_64 64 -> Recover (FnValue (BVType 64))
recoverAddr v = recoverValue "addr" v

recoverAssign :: Assignment X86_64 tp -> Recover (FnValue tp)
recoverAssign asgn = do
  m_seen <- uses rsAssignMap (MapF.lookup asgn)
  case m_seen of
    Just fnAssign -> return $! FnAssignedValue fnAssign
    Nothing -> do
      case assignRhs asgn of
        EvalApp app -> do
          app' <- traverseApp (recoverValue "recoverAssign") app
          fnAssign <- emitAssign asgn (FnEvalApp app')
          return $! FnAssignedValue fnAssign
        SetUndefined w -> FnAssignedValue <$> emitAssign asgn (FnSetUndefined w)
        ReadMem addr tp -> do
          fn_addr <- recoverAddr addr
          FnAssignedValue <$> emitAssign asgn (FnReadMem fn_addr tp)
        _ -> do
          debug DFunRecover ("recoverAssign does not yet support assignment " ++ show (pretty asgn)) $
            return $ FnValueUnsupported ("assignment " ++ show (pretty asgn)) (typeRepr asgn)

recoverRegister :: X86State (Value X86_64)
                -> X86Reg tp
                -> Recover (FnValue tp)
recoverRegister proc_state r = recoverValue ("register " ++ show r) (proc_state ^. boundValue r)

recoverValue :: String -> Value X86_64 tp -> Recover (FnValue tp)
recoverValue nm v = do
  interpState <- use rsInterp
  mem <- uses rsInterp memory
  case v of
    BVValue w i
      | Just Refl <- testEquality w n64
      , let addr = fromInteger i
      , Just seg <- findSegment addr mem -> do
        case () of
          _ | memFlags seg `hasPermissions` pf_x
            , Set.member addr (interpState^.functionEntries) -> do
              ft <- lookupFunctionArgs (Left addr)
              return $! FnFunctionEntryValue ft addr

            | memFlags seg `hasPermissions` pf_x
            , Map.member addr (interpState^.blocks) -> do
              cur_addr <- uses rsCurLabel labelAddr
              when (not (inSameFunction cur_addr addr interpState)) $ do
                debug DFunRecover ("Cross function jump " ++ showHex cur_addr " to " ++ showHex addr ".") $
                  return ()
              return $! FnBlockValue addr

            | memFlags seg `hasPermissions` pf_w -> do
              return $! FnGlobalDataAddr addr

            -- FIXME: do something more intelligent with rodata?
            | memFlags seg `hasPermissions` pf_r -> do
              return $! FnGlobalDataAddr addr

            | otherwise -> do
              debug DFunRecover ("WARNING: recoverValue " ++ nm ++ " given segment pointer: " ++ showHex i "") $ do
              return $! FnValueUnsupported ("segment pointer " ++ showHex i "") (typeRepr v)
      | otherwise -> do
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

recoverFunctions :: DiscoveryInfo -> Either String [Function]
recoverFunctions s =
  let fArgs = functionArgs s
   in traverse (recoverFunction fArgs s) (Set.toList (s^.functionEntries))
