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
  , identifyCall
  , identifyReturn
  ) where

import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (toList, traverse_)
import           Data.Int (Int64)
import           Data.List (elemIndex, elem)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
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
import           Reopt.CFG.Representation
import           Reopt.CFG.StackHeight
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Object.Memory

import           Debug.Trace


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
             -> Maybe (Seq Stmt, Word64, [Some Value])
identifyCall mem stmts0 s = go (Seq.fromList stmts0)
  where next_sp = s^.register N.rsp
        defaultArgs = map (\r -> Some $ s ^. register r) x86ArgumentRegisters
                      ++ map (\r -> Some $ s ^. register r) x86FloatArgumentRegisters
        go stmts =
          case Seq.viewr stmts of
            Seq.EmptyR -> Nothing
            prev Seq.:> stmt
              | Just ret <- isCodeAddrWriteTo mem stmt next_sp ->
                Just (prev, ret, defaultArgs)
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
    AssignedValue assign@(Assignment _ (Read (MemLoc ip_addr _)))
      | let (ip_base, ip_off) = asBaseOffset ip_addr
      , let (sp_base, sp_off) = asBaseOffset next_sp
      , (ip_base, ip_off + 8) == (sp_base, sp_off) -> Just assign
    _ -> Nothing

------------------------------------------------------------------------
-- StackDelta

-- | This code is reponsible for parsing the statement to determine
-- what code needs to be added to support an alloca at the beginning
-- of the block.
recoverStmtStackDelta :: Stmt -> State StackDelta ()
recoverStmtStackDelta s = do
  case s of
    Write (MemLoc addr _) val ->
      modify $ mergeStackDelta addr 0 False

    _ -> do
      return ()

recoverTermStmtStackDelta :: TermStmt -> State StackDelta ()
recoverTermStmtStackDelta (FetchAndExecute s) =
  modify $ mergeStackDelta (s ^. register N.rsp) 0 True
recoverTermStmtStackDelta _ = do
  return ()

computeAllocSize :: State StackDelta () -> Recover ()
computeAllocSize action = do
  stk <- getCurStack
  let dt0 = initStackDelta stk
      dt = execState action dt0
  case stackDeltaAllocSize dt of
    Just sz -> do
      fnAssign <- mkFnAssign (FnAlloca sz)
      addFnStmt $ FnAssignStmt fnAssign
      -- Update the stack to reflect the allocation.
      modifyCurStack $ recordStackAlloca sz
    Nothing -> do
      return ()


------------------------------------------------------------------------
-- RecoverState

type InitRegsMap = X86State FnRegValue

data RecoverState = RS { _rsInterp :: !InterpState
                       , _rsBlocks :: !(Map BlockLabel FnBlock)
                         -- | Labels to explore next.
                       , _rsFrontier :: !(Set BlockLabel)
                       , _rsNextAssignId :: !AssignId
                         -- | This is to support phi nodes -- this
                         -- accumulates the edges into a block until
                         -- the block is visited, after which the
                         -- block itself is updated.
                         --
                         -- Only defined for root blocks
                       , _rsPredRegs  :: !(Map BlockLabel (Map BlockLabel (X86State FnValue)))
                       --   -- | Map code addresses and registers to function value at start of
                       --   -- execution.
                       -- , _rsRegMap    :: !(Map BlockLabel (X86State FnRegValue))
                       , _rsStackMap  :: !(Map BlockLabel FnStack)
                         -- Information to propagate to sub-blocks
                       , _rsSubBlockState :: !(Map BlockLabel
                                               (X86State FnRegValue
                                               , MapF Assignment FnAssignment))
                         -- Local state
                       , _rsCurLabel  :: !BlockLabel
                       , _rsCurStmts  :: !(Seq FnStmt)
                       , _rsAssignMap :: !(MapF Assignment FnAssignment)
                       , _rsCurRegs   :: X86State FnRegValue
                       }

rsInterp :: Simple Lens RecoverState InterpState
rsInterp = lens _rsInterp (\s v -> s { _rsInterp = v })

rsBlocks :: Simple Lens RecoverState (Map BlockLabel FnBlock)
rsBlocks = lens _rsBlocks (\s v -> s { _rsBlocks = v })

rsFrontier :: Simple Lens RecoverState (Set BlockLabel)
rsFrontier = lens _rsFrontier (\s v -> s { _rsFrontier = v })

rsNextAssignId :: Simple Lens RecoverState AssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

-- rsRegMap :: Simple Lens RecoverState (Map BlockLabel (MapF N.RegisterName FnRegValue))
-- rsRegMap = lens _rsRegMap (\s v -> s { _rsRegMap = v })

rsPredRegs :: Simple Lens RecoverState (Map BlockLabel (Map BlockLabel (X86State FnValue)))
rsPredRegs = lens _rsPredRegs (\s v -> s { _rsPredRegs = v })

rsStackMap :: Simple Lens RecoverState (Map BlockLabel FnStack)
rsStackMap = lens _rsStackMap (\s v -> s { _rsStackMap = v })

rsSubBlockState :: Simple Lens RecoverState (Map BlockLabel
                                             (X86State FnRegValue
                                             , MapF Assignment FnAssignment))
rsSubBlockState = lens _rsSubBlockState (\s v -> s { _rsSubBlockState = v })

rsCurLabel :: Simple Lens RecoverState BlockLabel
rsCurLabel = lens _rsCurLabel (\s v -> s { _rsCurLabel = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens RecoverState (Seq FnStmt)
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens RecoverState (MapF Assignment FnAssignment)
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

rsCurRegs :: Simple Lens RecoverState (X86State FnRegValue)
rsCurRegs = lens _rsCurRegs (\s v -> s { _rsCurRegs = v })

------------------------------------------------------------------------
-- Recover

type Recover a = StateT RecoverState (ErrorT String Identity) a

runRecover :: RecoverState -> Recover a -> Either String a
runRecover s m = runIdentity $ runErrorT $ evalStateT m s

getCurStack :: Recover FnStack
getCurStack = do
  lbl  <- use rsCurLabel
  m_stk <- uses rsStackMap (Map.lookup lbl)
  case m_stk of
    Nothing -> error "Current stack undefined."
    Just stk -> return stk

modifyCurStack :: (FnStack -> FnStack) -> Recover ()
modifyCurStack f = do
  lbl  <- use rsCurLabel
  m_stk <- uses rsStackMap (Map.lookup lbl)
  case m_stk of
    Nothing -> error "Current stack undefined."
    Just stk -> rsStackMap . at lbl .= Just (f stk)

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
addSubBlockFrontier :: BlockLabel
                       -- ^ Label for block
                       -> X86State FnRegValue
                       -- ^ Map from register names to value (at src)
                       -> FnStack
                       -- ^ State of stack when frontier occurs.
                       -> MapF Assignment FnAssignment
                       -- ^ Register rename map.
                       -> Recover ()
addSubBlockFrontier lbl regs stk assigns = do
  when (isRootBlockLabel lbl) $ fail "Expecting a subblock label"  
  mr <- uses rsBlocks (Map.lookup lbl)
  case mr of
    Nothing -> trace ("Adding block to frontier: " ++ show (pretty lbl)) $ do      
      rsFrontier %= Set.insert lbl
      rsStackMap %= Map.insert lbl stk
      rsSubBlockState %= Map.insert lbl (regs, assigns)

    Just b -> trace ("WARNING: Saw a sub-block again (" ++ show lbl ++ ")") $
              return ()

addFrontier :: BlockLabel
               -- ^ Incoming edge
               -> BlockLabel
               -- ^ Label for block
               -> X86State FnValue
               -- ^ Map from register names to value (at src)
               -> FnStack
               -- ^ State of stack when frontier occurs.
               -> Recover ()
addFrontier src lbl regs stk = do
  unless (isRootBlockLabel lbl) $ fail "Expecting a root label"
  mr <- uses rsBlocks (Map.lookup lbl)
  case mr of
    Nothing -> trace ("Adding block to frontier: " ++ show (pretty lbl)) $ do      
      rsFrontier %= Set.insert lbl
      rsPredRegs %= Map.insertWith (Map.union) lbl (Map.singleton src regs)
      rsStackMap %= Map.insert lbl stk
    Just b -> do
      -- Add this block as a predecessor of lbl
      let b' = b { fbPhiNodes = Map.insert src regs (fbPhiNodes b) }
      rsBlocks %= Map.insert lbl b'

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
                      
-- | Recover the function at a given address.
recoverFunction :: InterpState -> CodeAddr -> Either String Function
recoverFunction s a = do
  let initRegs = mkX86State $ \r ->
        case r of
          N.GPReg {}
            | Just i <- elemIndex r x86ArgumentRegisters -> 
              FnRegValue (FnIntArg i)
            | Some r `Set.member` x86CalleeSavedRegisters -> CalleeSaved r
          -- FIXME: actually just N.XMMReg i | i < 8 -> ..., but this
          -- is more consistent
          N.XMMReg {} | Just i <- elemIndex r x86FloatArgumentRegisters -> 
              FnRegValue (FnFloatArg i)                           
          _ -> FnRegUninitialized
  let lbl = GeneratedBlock a 0
  let rs = RS { _rsInterp = s
              , _rsBlocks   = Map.empty
              , _rsFrontier = Set.singleton lbl
              , _rsNextAssignId = 0
              , _rsPredRegs  = Map.empty
              , _rsStackMap  = Map.singleton lbl initFnStack
              , _rsSubBlockState = Map.empty
              , _rsCurLabel  = lbl
              , _rsCurStmts  = Seq.empty
              , _rsAssignMap = MapF.empty
              , _rsCurRegs   = initRegs
              }
  runRecover rs $ do
    -- The first block is special as it has no predecessors, and hence no
    -- phi nodes
    recoverIter Map.empty initRegs MapF.empty lbl
    block_map <- use rsBlocks
    return $! Function { fnAddr = a
                       , fnIntArgTypes   = map (Some . N.registerType) x86ArgumentRegisters   -- FIXME
                       , fnFloatArgTypes = map (Some . N.registerType) x86FloatArgumentRegisters -- FIXME
                       , fnBlocks = Map.elems (filterUsedPhis block_map)
                       }

-- | Explore states until we have reached end of frontier.
recoverIter :: Map AssignId (Some N.RegisterName)
            -> X86State FnRegValue
            -> MapF Assignment FnAssignment
            -> BlockLabel
            -> Recover ()
recoverIter phivs regs assigns lbl = do
  b <- recoverBlock phivs regs assigns lbl
  rsFrontier %= Set.delete lbl
  rsBlocks   %= Map.insert lbl b
  f <- use rsFrontier
  case Set.maxView f of
    Nothing -> return ()
    Just (lbl, _) -> do
      (phis, regs', assigns') <-
         if isRootBlockLabel lbl then do          
            (phis', regs') <- makePhis
            return (phis', regs', MapF.empty)
         else do
            Just (regs', assigns') <- uses rsSubBlockState (Map.lookup lbl)
            return (Map.empty, regs', assigns')
      recoverIter phis regs' assigns' lbl

makePhis :: Recover ( Map AssignId (Some N.RegisterName)
                    , X86State FnRegValue)
makePhis = do
  phis <- mkX86StateM (mkPhiVar . N.registerType)
  let varMap = Map.fromList [ (viewSome (\r' -> unFnPhiVar $ phis ^. register r') r, r)
                            | r <- x86StateRegisters ]
  return (varMap, mapX86State (FnRegValue . FnPhiValue) phis)

-- regValuePair :: N.RegisterName cl
--              -> FnValue (BVType (N.RegisterClassBits cl))
--              -> Recover (Maybe (MapF.Pair N.RegisterName FnRegValue))
-- regValuePair nm v = return $ Just $ MapF.Pair nm (FnRegValue v)

-- | Extract function arguments from state
stateArgs :: X86State Value -> Recover [Some FnValue]
stateArgs _ = trace "startArgs not yet implemented" $ return []

recoverBlock :: Map AssignId (Some N.RegisterName)
             -> X86State FnRegValue
             -> MapF Assignment FnAssignment
             -> BlockLabel
             -> Recover FnBlock
recoverBlock phivs regs assigns lbl = do
  -- Clear stack offsets
  rsCurLabel  .= lbl
  rsCurStmts  .= Seq.empty
  rsCurRegs   .= regs
  rsAssignMap .= assigns

  phis <- uses rsPredRegs (Map.findWithDefault Map.empty lbl)    
  let mkBlock stmts tm = FnBlock { fbLabel = lbl
                                 , fbPhiVars = phivs
                                 , fbPhiNodes = phis
                                 , fbStmts = stmts
                                 , fbTerm = tm
                                 }
  
  -- Get original block for address.
  Just b <- uses (rsInterp . blocks) (`lookupBlock` lbl)

  -- Compute stack height
  -- computeAllocSize $ do
  --   Fold.traverse_ recoverStmtStackDelta (blockStmts b)
  --   recoverTermStmtStackDelta (blockTerm b)

  interp_state <- use rsInterp
  let mem = memory interp_state
  case blockTerm b of
    Branch c x y -> do
      computeAllocSize $ do
        Fold.traverse_ recoverStmtStackDelta (blockStmts b)
        recoverTermStmtStackDelta (blockTerm b)
      Fold.traverse_ recoverStmt (blockStmts b)
      cv <- recoverValue "branch_cond" c
      stk  <- getCurStack
      assigns <- use rsAssignMap

      addSubBlockFrontier x regs stk assigns
      addSubBlockFrontier y regs stk assigns
      stmts <- uses rsCurStmts Fold.toList
      return $! mkBlock stmts (FnBranch cv x y)
    FetchAndExecute proc_state
        -- The last statement was a call.
      | Just (prev_stmts, ret_addr, args) <- identifyCall mem (blockStmts b) proc_state -> do
        -- Compute any allocations that need to occur.
        computeAllocSize $ do
          Fold.traverse_ recoverStmtStackDelta prev_stmts
          modify $ mergeStackDelta (proc_state ^. register N.rsp) 8 True

        Fold.traverse_ recoverStmt prev_stmts
        stk <- getCurStack
        intr   <- mkReturnVar (knownType :: TypeRepr (BVType 64))
        floatr <- mkReturnVar (knownType :: TypeRepr XMMType)

        -- Figure out what is preserved across the function call.
        let go :: forall cl. N.RegisterName cl -> Value (N.RegisterType cl)
                  -> Recover (FnValue (N.RegisterType cl))
            go r v =
              case r of
                N.IPReg -> return $ FnConstantValue knownNat (toInteger ret_addr)
                N.GPReg {}
                  | Just _ <- testEquality r N.rax -> return (FnReturn intr)
                  | Some r `Set.member` x86CalleeSavedRegisters -> recoverValue "callee_saved" v
                N.XMMReg 0 -> return (FnReturn floatr)
                _ -> return $ FnUndefined (N.registerType r)
        regs' <- mapX86StateWithM go proc_state

        let ret_lbl = GeneratedBlock ret_addr 0
        addFrontier lbl ret_lbl regs' stk
        fn_stmts <- uses rsCurStmts Fold.toList
        call_tgt <- recoverValue "ip" (proc_state^.register N.rip)
        args'  <- mapM (viewSome (fmap Some . recoverValue "arguments")) args
        -- args <- (++ stackArgs stk) <$> stateArgs proc_state
        return $! mkBlock fn_stmts (FnCall call_tgt args' intr floatr ret_lbl)

      -- Jump to concrete offset.
      | BVValue _ (fromInteger -> tgt_addr) <- proc_state^.register N.rip
        -- Check that we are in the same function
      , let this_fn = getFunctionEntryPoint (labelAddr lbl) interp_state
      , let tgt_fn  = getFunctionEntryPoint tgt_addr interp_state
      , this_fn == tgt_fn -> do

        -- Compute amount to allocate.
        computeAllocSize $ do
          Fold.traverse_ recoverStmtStackDelta (blockStmts b)
          recoverTermStmtStackDelta (blockTerm b)

        -- Recover statements
        Fold.traverse_ recoverStmt (blockStmts b)
        regs' <- mapX86StateM (recoverValue "proc_state") proc_state

        stmts <- uses rsCurStmts Fold.toList
        let tgt_lbl = GeneratedBlock tgt_addr 0
        stk  <- getCurStack
        addFrontier lbl tgt_lbl regs' stk
        return $! mkBlock stmts (FnJump tgt_lbl)

       -- Return
      | Just assign <- identifyReturn proc_state -> do
        -- Compute amount to allocate.
        computeAllocSize $ do
          Fold.traverse_ recoverStmtStackDelta (blockStmts b)
          recoverTermStmtStackDelta (blockTerm b)
        -- Throw away pc load from stack.
        let isRetLoad s =
              case s of
                AssignStmt assign' | Just Refl <- testEquality assign assign' -> True
                _ -> False
            nonret_stmts = filter (not . isRetLoad) (blockStmts b)
        
        -- Recover statements
        Fold.traverse_ recoverStmt nonret_stmts
        intr   <- recoverValue "int_result"   (proc_state ^. register N.rax)
        floatr <- recoverValue "float_result" (proc_state ^. register (N.XMMReg 0))
        stmts <- uses rsCurStmts Fold.toList
        return $! mkBlock stmts (FnRet intr floatr)
    _ -> do
      trace ("WARNING: recoverTermStmt undefined for " ++ show (pretty (blockTerm b))) $ do
      computeAllocSize $ do
        Fold.traverse_ recoverStmtStackDelta (blockStmts b)
        recoverTermStmtStackDelta (blockTerm b)
      Fold.traverse_ recoverStmt (blockStmts b)
      stmts <- uses rsCurStmts Fold.toList
      return $! mkBlock stmts FnTermStmtUndefined

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
recoverStmt s =
  case s of
    AssignStmt assign -> do
      let lhs = assignId assign
      case assignRhs assign of
        -- Apps are handled in recoverValue (lazily).
        EvalApp _ -> return () 
        SetUndefined w -> void $ emitAssign assign (FnSetUndefined w)
        Read (MemLoc addr tp) -> do
          fn_addr <- recoverAddr addr
          void $ emitAssign assign (FnReadMem fn_addr tp)
        _ -> trace ("recoverStmt undefined for " ++ show (pretty s)) $ do
          return ()
    Write (MemLoc addr _) val
      | Initial reg <- val -> do
        regs <- use rsCurRegs
        case regs ^. register reg of
          CalleeSaved saved_reg -> do
            case asStackAddrOffset addr of
              Just int_addr_off -> do
                modifyCurStack $ recordCalleeSavedWrite int_addr_off saved_reg
              Nothing -> trace "Could not interpret callee saved offset" $ do
                recoverWrite addr val
          _ -> do
            recoverWrite addr val
      | otherwise -> do
        recoverWrite addr val
    Comment msg -> do
      addFnStmt $ FnComment msg
    _ -> trace ("recoverStmt undefined for " ++ show (pretty s)) $ do
      return ()

recoverAddr :: Value (BVType 64) -> Recover (FnValue (BVType 64))
recoverAddr v = recoverValue "addr" v

recoverValue :: String -> Value tp -> Recover (FnValue tp)
recoverValue nm v = do
  interpState <- use rsInterp
  mem <- uses rsInterp memory
  case v of
    _ | Just int_addr_off <- asStackAddrOffset v -> do
      trace ("recoverValue encountered stack offset: " ++ show int_addr_off) $ do
      return $ FnValueUnsupported (valueType v)

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
                trace ("Cross function jump " ++ showHex cur_addr " to " ++ showHex addr ".") $
                  return ()
              return $! FnBlockValue addr

            | memFlags seg `hasPermissions` pf_w -> do
              return $! FnGlobalDataAddr addr

            | otherwise -> do
              trace ("WARNING: recoverValue " ++ nm ++ " given segment pointer: " ++ showHex i "") $ do
              return $! FnValueUnsupported (valueType v)
      | otherwise -> do
        return $ FnConstantValue w i

    AssignedValue assign -> do
      m_seen <- uses rsAssignMap (MapF.lookup assign)
      case m_seen of
        Just fnAssign ->
          return $! FnAssignedValue fnAssign
        Nothing -> do
          case assignRhs assign of
            EvalApp app -> do
              app' <- traverseApp (recoverValue ('r':nm)) app
              fnAssign <- emitAssign assign (FnEvalApp app')
              return $! FnAssignedValue fnAssign
            _ -> do
              trace ("recoverValue does not yet support assignment " ++ show (pretty assign)) $
                return $ FnValueUnsupported (assignmentType assign)
    Initial reg -> do
      regs <- use rsCurRegs
      case regs ^. register reg of
        CalleeSaved _ -> do
          -- trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          return (FnUndefined (N.registerType reg))
        FnRegValue v -> return v
        FnRegUninitialized -> return (FnUndefined (N.registerType reg))

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


--------------------------------------------------------------------------------
-- Phi node uses
--------------------------------------------------------------------------------

data PhiState = PhiState { phiWorkList  :: !(Set BlockLabel)
                         , phiBlockData :: Map BlockLabel (Set AssignId) }

filterUsedPhis :: Map BlockLabel FnBlock -> Map BlockLabel FnBlock
filterUsedPhis blocks = Map.intersectionWith filterVars blocks used 
  where
    used = phiUses blocks
    filterVars b phis =
      let phisAsMap = Map.fromSet (const ()) phis
      in  b { fbPhiVars = Map.intersection (fbPhiVars b) phisAsMap }

phiUses :: Map BlockLabel FnBlock -> Map BlockLabel (Set AssignId)
phiUses blocks = phiBlockData $ execState (iteratePhi blocks) s0
  where
    phis0 = fmap phis blocks
    s0 = PhiState { phiWorkList = Map.keysSet phis0
                  , phiBlockData = phis0 }
    phis b = Set.unions (map stmtPhis (fbStmts b))
             `Set.union` termStmtPhis (fbTerm b)
    
iteratePhi :: Map BlockLabel FnBlock -> State PhiState ()
iteratePhi bs = go
  where
    go = do
      wl <- gets (Set.maxView . phiWorkList) 
      case wl of
        Nothing -> return ()
        Just (lbl, rest) -> do
          modify (\s -> s {phiWorkList = rest})
          m_phis <- gets (Map.lookup lbl . phiBlockData)
          case (m_phis, Map.lookup lbl bs) of
            (Just phis, Just b) -> phisPhis phis b
            _ -> error "IMPOSSIBLE"
          go

-- FIXME: this could be a bit more efficient I think
mergePhis :: Map BlockLabel (Set AssignId) -> State PhiState ()
mergePhis newPhis = do
  prePBD <- gets phiBlockData
  let diff new old
        | new `Set.isProperSubsetOf` old = Nothing
        | otherwise = Just new
      frontier = Map.keysSet $ Map.differenceWith diff newPhis prePBD
  modify (\s -> s { phiWorkList = phiWorkList s `Set.union` frontier
                  , phiBlockData = Map.unionWith Set.union newPhis prePBD })

-- Calculate the backward phis for a block, given a set of used phi vars
phisPhis :: Set AssignId -> FnBlock -> State PhiState ()
phisPhis used b = mergePhis predPhis
  where
    -- FIXME: we really just want Map restriction here.
    regs = [ r | (aid, r) <- Map.assocs (fbPhiVars b)
               , aid `Set.member` used ]
    predPhis :: Map BlockLabel (Set AssignId)
    predPhis   = fmap go (fbPhiNodes b)
    go phiNode = Set.unions
                 $ map (\(Some r) -> valuePhis $ phiNode ^. register r) regs
    
stmtPhis :: FnStmt -> Set AssignId
stmtPhis stmt =
  case stmt of
    FnWriteMem addr v -> valuePhis addr `Set.union` valuePhis v
    FnComment _ -> Set.empty
    FnAssignStmt (FnAssignment lhs rhs) -> assignRhsPhis rhs

termStmtPhis :: FnTermStmt -> Set AssignId
termStmtPhis term =
  case term of
    FnJump _ -> Set.empty
    FnRet iret fret -> valuePhis iret `Set.union` valuePhis fret
    FnBranch cond _ _ -> valuePhis cond
    FnCall fn args _ _ _ ->
      valuePhis fn `Set.union`
      (Set.unions $ map (viewSome valuePhis) args)
    FnTermStmtUndefined -> Set.empty

assignRhsPhis :: FnAssignRhs to -> Set AssignId
assignRhsPhis rhs =
  case rhs of
    FnSetUndefined _ -> Set.empty
    FnReadMem addr _ -> valuePhis addr
    FnEvalApp app    -> foldApp valuePhis app
    FnAlloca bytes   -> valuePhis bytes

valuePhis :: FnValue tp -> Set AssignId
valuePhis v =
  case v of
    FnPhiValue phi -> Set.singleton (unFnPhiVar phi)
    _              -> Set.empty
