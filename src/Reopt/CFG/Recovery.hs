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
  ) where

import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (toList, traverse_)
import           Data.List (elemIndex, elem, intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Type.Equality
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Reopt.CFG.FnRep
import           Reopt.CFG.InterpState
import           Reopt.CFG.RegisterUse
import           Reopt.CFG.Representation
import           Reopt.CFG.StackDepth
import           Reopt.CFG.StackHeight
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Memory
import           Reopt.Utils.Debug


------------------------------------------------------------------------
-- StackDelta

-- | This code is reponsible for parsing the statement to determine
-- what code needs to be added to support an alloca at the beginning
-- of the block.
-- recoverStmtStackDelta :: Stmt -> State StackDelta ()
-- recoverStmtStackDelta s = do
--   case s of
--     Write (MemLoc addr _) val ->
--       modify $ recordPotentialStackOffset addr 0 False

--     _ -> do
--       return ()

-- recoverTermStmtStackDelta :: TermStmt -> State StackDelta ()
-- recoverTermStmtStackDelta (FetchAndExecute s) =
--   modify $ recordPotentialStackOffset (s ^. register N.rsp) 0 True
-- recoverTermStmtStackDelta _ = do
--   return ()

-- computeAllocSize :: State StackDelta () -> Recover ()
-- computeAllocSize action = do
--   stk <- getCurStack
--   let dt0 = initStackDelta stk
--       dt = execState action dt0
--   case stackDeltaAllocSize dt of
--     Just sz -> do
--       fnAssign <- mkFnAssign (FnAlloca sz)
--       addFnStmt $ FnAssignStmt fnAssign
--       -- Update the stack to reflect the allocation.
--       modifyCurStack $ recordStackAlloca sz
--     Nothing -> do
--       return ()


------------------------------------------------------------------------
-- RecoverState

type InitRegsMap = MapF N.RegisterName FnRegValue

data RecoverState = RS { _rsInterp :: !InterpState
                       , _rsNextAssignId :: !AssignId
                       , _rsAssignMap :: !(MapF Assignment FnAssignment)

                         -- Local state
                       , _rsCurLabel  :: !BlockLabel
                       , _rsCurStmts  :: !(Seq FnStmt)
                         -- more or less read only (set once for each
                         -- block, and for rsp alloc.)
                       , _rsCurRegs   :: !(InitRegsMap)
                         -- read only
                       , rsAssignmentsUsed :: !(Set (Some Assignment))
                       , rsFunctionArgs    :: !FunctionArgs
                       }

rsInterp :: Simple Lens RecoverState InterpState
rsInterp = lens _rsInterp (\s v -> s { _rsInterp = v })

-- rsBlocks :: Simple Lens RecoverState (Map BlockLabel FnBlock)
-- rsBlocks = lens _rsBlocks (\s v -> s { _rsBlocks = v })

-- rsFrontier :: Simple Lens RecoverState (Set BlockLabel)
-- rsFrontier = lens _rsFrontier (\s v -> s { _rsFrontier = v })

rsNextAssignId :: Simple Lens RecoverState AssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

-- rsRegMap :: Simple Lens RecoverState (Map BlockLabel (MapF N.RegisterName FnRegValue))
-- rsRegMap = lens _rsRegMap (\s v -> s { _rsRegMap = v })

-- rsPredRegs :: Simple Lens RecoverState (Map BlockLabel (Map BlockLabel (X86State FnValue)))
-- rsPredRegs = lens _rsPredRegs (\s v -> s { _rsPredRegs = v })

-- rsStackMap :: Simple Lens RecoverState (Map BlockLabel FnStack)
-- rsStackMap = lens _rsStackMap (\s v -> s { _rsStackMap = v })

-- rsSubBlockState :: Simple Lens RecoverState (Map BlockLabel
--                                              (X86State FnRegValue
--                                              , MapF Assignment FnAssignment))
-- rsSubBlockState = lens _rsSubBlockState (\s v -> s { _rsSubBlockState = v })

rsCurLabel :: Simple Lens RecoverState BlockLabel
rsCurLabel = lens _rsCurLabel (\s v -> s { _rsCurLabel = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens RecoverState (Seq FnStmt)
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens RecoverState (MapF Assignment FnAssignment)
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

rsCurRegs :: Simple Lens RecoverState InitRegsMap
rsCurRegs = lens _rsCurRegs (\s v -> s { _rsCurRegs = v })

------------------------------------------------------------------------
-- Recover

type Recover a = StateT RecoverState (ErrorT String Identity) a

runRecover :: RecoverState -> Recover a -> Either String a
runRecover s m = runIdentity $ runErrorT $ evalStateT m s

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
-- getCurRegs :: Recover (MapF N.RegisterName FnRegValue)
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

mkPhiVar :: TypeRepr tp -> Recover (FnPhiVar tp)
mkPhiVar tp = mkId (flip FnPhiVar tp)

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
regMapFromState :: X86State f -> MapF N.RegisterName f
regMapFromState s =
  MapF.fromList [ MapF.Pair nm (s^.register nm)
                | Some nm <- x86StateRegisters
                ]
-}

------------------------------------------------------------------------
-- recoverFunction

type FunctionArgs = Map CodeAddr (Set (Some N.RegisterName), Set (Some N.RegisterName))

-- | Recover the function at a given address.
recoverFunction :: FunctionArgs -> InterpState -> CodeAddr -> Either String Function
recoverFunction fArgs s a = do
  let (usedAssigns, blockRegs, blockRegProvides, blockPreds)
        = registerUse fArgs s a
  
  let initRegs = MapF.empty
                 & flip (ifoldr (\i r     -> MapF.insert r (FnRegValue (FnIntArg i)))) x86ArgumentRegisters
                 & flip (ifoldr (\i r     -> MapF.insert r (FnRegValue (FnFloatArg i)))) x86FloatArgumentRegisters
                 & flip (foldr (\(Some r) -> MapF.insert r (CalleeSaved r))) x86CalleeSavedRegisters

  let lbl = GeneratedBlock a 0
  let rs = RS { _rsInterp = s
              , _rsNextAssignId = 0
              , _rsCurLabel  = lbl
              , _rsCurStmts  = Seq.empty
              , _rsAssignMap = MapF.empty
              , _rsCurRegs   = initRegs
              , rsAssignmentsUsed = usedAssigns
              , rsFunctionArgs    = fArgs
              }

  let recoverInnerBlock blockRegMap lbl = do
        (phis, regs) <- makePhis blockRegs blockPreds blockRegMap lbl
        rsCurRegs    .= regs
        recoverBlock blockRegProvides phis lbl

  let go ~(_, blockRegMap) = do
         -- Make the alloca and init rsp.  This is the only reason we
         -- need rsCurStmts
         allocateStackFrame (maximumStackDepth s a)
         r0 <- recoverBlock blockRegProvides MapF.empty lbl
         rs <- mapM (recoverInnerBlock blockRegMap) (Map.keys blockPreds)
         -- disjoint maps here, so mconcat is OK         
         return (mconcat (r0 : rs)) 

  runRecover rs $ do
    -- The first block is special as it needs to allocate space for
    -- the block stack area.  It should also not be in blockPreds (we
    -- assume no recursion/looping through the initial block)
    (block_map, _) <- mfix go

    return $! Function { fnAddr = a
                       , fnIntArgTypes   = map (Some . N.registerType) x86ArgumentRegisters   -- FIXME
                       , fnFloatArgTypes = map (Some . N.registerType) x86FloatArgumentRegisters -- FIXME
                       , fnBlocks = Map.elems block_map
                       }

makePhis :: Map BlockLabel (Set (Some N.RegisterName))
            -> Map BlockLabel [BlockLabel]
            -> Map BlockLabel (MapF N.RegisterName FnRegValue)
            -> BlockLabel
            -> Recover (MapF FnPhiVar FnPhiNodeInfo, InitRegsMap)
makePhis blockRegs blockPreds blockRegMap lbl = do
  regs <- foldM (\m (Some r) -> mkId (addReg m r)) MapF.empty regs0
  let nodes = MapF.foldrWithKey go MapF.empty regs
  return (nodes, regs)
  where
    addReg m r next_id =
      let phi_var = FnPhiVar next_id (N.registerType r)
      in MapF.insert r (FnRegValue $ FnPhiValue phi_var) m
    -- FIXME
    go :: forall s. N.RegisterName s -> FnRegValue s
          -> MapF FnPhiVar FnPhiNodeInfo
          -> MapF FnPhiVar FnPhiNodeInfo
    go r (FnRegValue (FnPhiValue phi_var)) m =
      MapF.insert phi_var (collate r) m
    go _ _ _ = error "impossible"

    collate :: forall cl. N.RegisterName cl -> FnPhiNodeInfo (N.RegisterType cl)
    collate r =
      let undef lbl = (lbl, FnValueUnsupported ("makePhis " ++ show r ++ " at " ++ show (pretty lbl)) (N.registerType r))
          doOne lbl =
            fromMaybe (debug DFunRecover ("WARNING: missing blockRegMap/register for " ++ show (pretty lbl)) (undef lbl)) $
            do rm <- Map.lookup lbl blockRegMap
               FnRegValue rv <- MapF.lookup r rm
               return (lbl, rv)
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
      rsCurRegs %= MapF.insert N.rsp (FnRegValue spTop)

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

-- | Extract function arguments from state
stateArgs :: X86State Value -> Recover [Some FnValue]
stateArgs _ = debug DFunRecover "startArgs not yet implemented" $ return []

-- FIXME: clag from RegisterUse.hs
lookupFunctionArgs :: Either CodeAddr (Value (BVType 64))
                   -> Recover (Set (Some N.RegisterName), Set (Some N.RegisterName))
lookupFunctionArgs fn = 
  case fn of
    Right _dynaddr -> return nothingKnown
    Left faddr -> do 
      fArgs <- gets (Map.lookup faddr . rsFunctionArgs)
      case fArgs of
        Nothing -> do debugM DUrgent ("Warning: no args for function " ++ show faddr)
                      return nothingKnown
        Just v  -> return v
  where
    nothingKnown = (Set.fromList (( Some <$> x86ArgumentRegisters) ++
                                  ( Some <$> x86FloatArgumentRegisters ))
                   , Set.fromList x86ResultRegisters)

recoverBlock :: Map BlockLabel (Set (Some N.RegisterName))
             -> MapF FnPhiVar FnPhiNodeInfo
             -> BlockLabel
             -> Recover (Map BlockLabel FnBlock
                        , Map BlockLabel (MapF N.RegisterName FnRegValue))
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

  interp_state <- use rsInterp
  let mem = memory interp_state
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

      (args, rets) <- lookupFunctionArgs fn

      -- May not be used (only if called function returns at these types)
      intr   <- mkReturnVar (knownType :: TypeRepr (BVType 64))
      intr2  <- mkReturnVar (knownType :: TypeRepr (BVType 64))      
      floatr <- mkReturnVar (knownType :: TypeRepr XMMType)

      -- Figure out what is preserved across the function call.
      -- FIXME: use rets
      let go ::  MapF N.RegisterName FnRegValue -> Some N.RegisterName
                -> Recover (MapF N.RegisterName FnRegValue)
          go m (Some r) = do
             v <- case r of
               N.GPReg {}
                 | Just Refl <- testEquality r N.rax ->
                     return (FnReturn intr)
                 | Just Refl <- testEquality r N.rdx ->
                     return (FnReturn intr2)                     
                 | Just _ <- testEquality r N.rsp -> do
                     spv <- recoverValue "callee_saved" (proc_state ^. register N.rsp)
                     mkAddAssign $ FnEvalApp $ BVAdd knownNat spv (FnConstantValue n64 8)
                 | Some r `Set.member` x86CalleeSavedRegisters ->
                     recoverValue "callee_saved" (proc_state ^. register r)
               N.XMMReg 0 -> return (FnReturn floatr)
               _ -> debug DFunRecover ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show lbl) $
                    return (FnValueUnsupported ("post-call register " ++ show r) (N.registerType r))
             return $ MapF.insert r (FnRegValue v) m

      let provides = blockRegProvides ^. ix lbl -- maybe mempty
      regs' <- foldM go MapF.empty provides

      let ret_lbl = mkRootBlockLabel <$> m_ret_addr
      call_tgt <- recoverValue "ip" (proc_state ^. register N.rip)

      let argVs = [ Some (proc_state ^. register r) | Some r <- Set.toList args ]
      args'  <- mapM (viewSome (fmap Some . recoverValue "arguments")) argVs
      -- args <- (++ stackArgs stk) <$> stateArgs proc_state

      fb <- mkBlock (FnCall call_tgt args' intr floatr ret_lbl)
      return $! (fb, Map.singleton lbl regs')

    Just (ParsedJump proc_state tgt_addr) -> do

        -- Recover statements
        Fold.traverse_ recoverStmt (blockStmts b)

        let go ::  MapF N.RegisterName FnRegValue -> Some N.RegisterName
                  -> Recover (MapF N.RegisterName FnRegValue)
            go m (Some r) = do
               v <- recoverValue "phi" (proc_state ^. register r)
               return $ MapF.insert r (FnRegValue v) m

        let provides = blockRegProvides ^. ix lbl
        regs' <- foldM go MapF.empty provides

        let tgt_lbl = mkRootBlockLabel tgt_addr
        flip (,) (Map.singleton lbl regs') <$> mkBlock (FnJump tgt_lbl)

    Just (ParsedReturn proc_state nonret_stmts) -> do
        -- Recover statements
        Fold.traverse_ recoverStmt nonret_stmts
        intr   <- recoverValue "int_result"   (proc_state ^. register N.rax)
        floatr <- recoverValue "float_result" (proc_state ^. register (N.XMMReg 0))
        flip (,) Map.empty <$> mkBlock (FnRet intr floatr)

    Just (ParsedSyscall proc_state next_addr _name argRegs) -> do
      Fold.traverse_ recoverStmt (blockStmts b)
      
      -- Figure out what is preserved across the function call.
      let go ::  MapF N.RegisterName FnRegValue -> Some N.RegisterName
                -> Recover (MapF N.RegisterName FnRegValue)
          go m (Some r) = do 
             v <- case r of
               N.GPReg {}
                 -- | Just Refl <- testEquality r N.rax ->
                 --     return (FnReturn intr)
                 | Just _ <- testEquality r N.rsp -> do
                     recoverValue "callee_saved" (proc_state ^. register N.rsp)
                 | Some r `Set.member` x86CalleeSavedRegisters ->
                     recoverValue "callee_saved" (proc_state ^. register r)
               -- N.XMMReg 0 -> return (FnReturn floatr)
               _ -> debug DFunRecover ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show lbl) $
                    return (FnValueUnsupported ("post-syscall register " ++ show r) (N.registerType r))
             return $ MapF.insert r (FnRegValue v) m

      let Just provides = Map.lookup lbl blockRegProvides
      regs' <- foldM go MapF.empty provides 
      
      let args = [ Some (proc_state ^. register r) | r <- argRegs ]

      _args'  <- mapM (viewSome (fmap Some . recoverValue "arguments")) args
      -- args <- (++ stackArgs stk) <$> stateArgs proc_state
      
      fb <- mkBlock FnTermStmtUndefined
      return $! (fb, Map.singleton lbl regs')

    Just (ParsedLookupTable _proc_state _idx _vec) -> error "LookupTable"

    Nothing -> do
      debug DFunRecover ("WARNING: recoverTermStmt undefined for " ++ show (pretty (blockTerm b))) $ do

      Fold.traverse_ recoverStmt (blockStmts b)
      flip (,) Map.empty <$> mkBlock FnTermStmtUndefined

recoverWrite :: Value (BVType 64) -> Value tp -> Recover ()
recoverWrite addr val = do
  r_addr <- recoverAddr addr
  r_val  <- recoverValue "write_val" val
  addFnStmt $ FnWriteMem r_addr r_val

emitAssign :: Assignment tp -> FnAssignRhs tp -> Recover (FnAssignment tp)
emitAssign assign rhs = do
  fnAssign <- mkFnAssign rhs
  rsAssignMap %= MapF.insert assign fnAssign
  addFnStmt $ FnAssignStmt fnAssign
  return fnAssign

-- | This should add code as needed to support the statement.
recoverStmt :: Stmt -> Recover ()
recoverStmt s = do
  usedAssigns <- gets rsAssignmentsUsed
  seenAssigns <- use rsAssignMap
  case s of
    AssignStmt assign
      | not (Some assign `Set.member` usedAssigns) -> return ()
      | Just _ <- MapF.lookup assign seenAssigns   -> return ()
      | otherwise -> void $ recoverAssign assign
    Write (MemLoc addr _) val
      | Initial reg <- val -> do
        reg_v <- uses rsCurRegs (MapF.lookup reg)
        case reg_v of
          Just (CalleeSaved saved_reg) -> do
            case asStackAddrOffset addr of
              Just int_addr_off -> do
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
    _ -> debug DFunRecover ("recoverStmt undefined for " ++ show (pretty s)) $ do
      return ()

recoverAddr :: Value (BVType 64) -> Recover (FnValue (BVType 64))
recoverAddr v = recoverValue "addr" v

recoverAssign :: Assignment tp -> Recover (FnValue tp)
recoverAssign assign = do
  m_seen <- uses rsAssignMap (MapF.lookup assign)
  case m_seen of
    Just fnAssign -> return $! FnAssignedValue fnAssign
    Nothing -> do
      case assignRhs assign of
        EvalApp app -> do
          app' <- traverseApp (recoverValue "recoverAssign") app
          fnAssign <- emitAssign assign (FnEvalApp app')
          return $! FnAssignedValue fnAssign
        SetUndefined w -> FnAssignedValue <$> emitAssign assign (FnSetUndefined w)
        Read (MemLoc addr tp) -> do
          fn_addr <- recoverAddr addr
          FnAssignedValue <$> emitAssign assign (FnReadMem fn_addr tp)
        _ -> do
          debug DFunRecover ("recoverAssign does not yet support assignment " ++ show (pretty assign)) $
            return $ FnValueUnsupported ("assignment " ++ show (pretty assign)) (assignmentType assign)

recoverValue :: String -> Value tp -> Recover (FnValue tp)
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
              return $! FnFunctionEntryValue addr

            | memFlags seg `hasPermissions` pf_x
            , Map.member addr (interpState^.blocks) -> do
              cur_addr <- uses rsCurLabel labelAddr
              when (not (inSameFunction cur_addr addr interpState)) $ do
                debug DFunRecover ("Cross function jump " ++ showHex cur_addr " to " ++ showHex addr ".") $
                  return ()
              return $! FnBlockValue addr

            | memFlags seg `hasPermissions` pf_w -> do
              return $! FnGlobalDataAddr addr

            | otherwise -> do
              debug DFunRecover ("WARNING: recoverValue " ++ nm ++ " given segment pointer: " ++ showHex i "") $ do
              return $! FnValueUnsupported ("segment pointer " ++ showHex i "") (valueType v)
      | otherwise -> do
        return $ FnConstantValue w i

    AssignedValue assign -> recoverAssign assign

    Initial reg -> do
      reg_v <- uses rsCurRegs (MapF.lookup reg)
      case reg_v of
        Nothing -> return (FnValueUnsupported ("Initial register " ++ show reg) (N.registerType reg))
        Just (CalleeSaved _) -> do
          -- trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          return (FnValueUnsupported ("Initial (callee) register " ++ show reg) (N.registerType reg))
        Just (FnRegValue v) -> return v

{-
edgeRelation :: InterpState
                -> CodeAddr
                -> Either String (Map BlockLabel (Set BlockLabel))
edgeRelation s addr =

inBounds :: Ord a => a -> (a,a) -> Bool
inBounds v (l,h) = l <= v && v < h

resolveTermStmtEdges :: (Word64, Word64) -> TermStmt -> Maybe [BlockLabel]
resolveTermStmtEdges bounds s =
  case s of
    Branch _ x y -> Just [x,y]
    Syscall s ->
      case s^.register N.rsp of
        BVValue _ a | inBounds a bounds -> Just [GeneratedBlock a 0]
        _ -> Nothing


resolveEdges :: (Word64, Word64) -> [BlockLabel] -> State (Map BlockLabel (Set BlockLabel))
resolveEdges _ [] = return ()
resolveEdges bounds (lbl:rest) = do
  m0 <- get
  case resolveTermStmtEdges bounds undefined of
    Just next ->
      put $ Map.insert lbl next
    Nothing -> do
-}
