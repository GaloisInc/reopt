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
  ) where

import Control.Lens
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.Int (Int64)
import Data.Foldable as Fold (toList, traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality
import Data.Word
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Reopt.CFG.FnRep
import Reopt.CFG.InterpState
import Reopt.CFG.Representation
import Reopt.CFG.StackHeight
import qualified Reopt.Machine.StateNames as N
import Reopt.Machine.Types
import Reopt.Object.Memory

import Debug.Trace


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

data FnRegValue cl where
  -- This is a callee saved register.
  CalleeSaved :: N.RegisterName 'N.GP -> FnRegValue 'N.GP
  -- A value assigned to a register
  FnRegValue :: !(FnValue (BVType (N.RegisterClassBits cl))) -> FnRegValue cl

data RecoverState = RS { _rsInterp :: !InterpState
                       , _rsBlocks :: !(Map BlockLabel FnBlock)
                         -- | Labels to explore next.
                       , _rsFrontier :: !(Set BlockLabel)
                       , _rsNextAssignId :: !AssignId
                         -- | Map code addresses and registers to function value at start of
                         -- execution.
                       , _rsRegMap    :: !(Map BlockLabel (MapF N.RegisterName FnRegValue))
                       , _rsStackMap  :: !(Map BlockLabel FnStack)

                       , _rsCurLabel  :: !BlockLabel
                       , _rsCurStmts  :: !(Seq FnStmt)
                       , _rsAssignMap :: !(MapF Assignment FnAssignment)
                       }

rsInterp :: Simple Lens RecoverState InterpState
rsInterp = lens _rsInterp (\s v -> s { _rsInterp = v })

rsBlocks :: Simple Lens RecoverState (Map BlockLabel FnBlock)
rsBlocks = lens _rsBlocks (\s v -> s { _rsBlocks = v })

rsFrontier :: Simple Lens RecoverState (Set BlockLabel)
rsFrontier = lens _rsFrontier (\s v -> s { _rsFrontier = v })

rsNextAssignId :: Simple Lens RecoverState AssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

rsRegMap :: Simple Lens RecoverState (Map BlockLabel (MapF N.RegisterName FnRegValue))
rsRegMap = lens _rsRegMap (\s v -> s { _rsRegMap = v })

rsStackMap :: Simple Lens RecoverState (Map BlockLabel FnStack)
rsStackMap = lens _rsStackMap (\s v -> s { _rsStackMap = v })

rsCurLabel :: Simple Lens RecoverState BlockLabel
rsCurLabel = lens _rsCurLabel (\s v -> s { _rsCurLabel = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens RecoverState (Seq FnStmt)
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens RecoverState (MapF Assignment FnAssignment)
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

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
getCurRegs :: Recover (MapF N.RegisterName FnRegValue)
getCurRegs = do
  lbl <- use rsCurLabel
  maybe_map <- uses rsRegMap (Map.lookup lbl)
  case maybe_map of
    Nothing -> do
      error $ "Did not define register map for " ++ show lbl ++ "."
    Just reg_map -> do
      return reg_map

mkFnAssign :: FnAssignRhs tp -> Recover (FnAssignment tp)
mkFnAssign rhs = do
  next_id <- use rsNextAssignId
  rsNextAssignId .= next_id + 1
  return $! FnAssignment next_id rhs

addFnStmt :: FnStmt -> Recover ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

addFrontier :: BlockLabel
               -- ^ Label for block
               -> MapF N.RegisterName FnRegValue
               -- ^ Map from register names to value
               -> FnStack
               -- ^ State of stack when frontier occurs.
               -> Recover ()
addFrontier lbl regs stk = do
  mr <- uses rsBlocks (Map.lookup lbl)
  case mr of
    Nothing -> do
      rsFrontier %= Set.insert lbl
      rsRegMap   %= Map.insert lbl regs
      rsStackMap %= Map.insert lbl stk
    Just{} -> do
      return ()

-- | Return value bound to register (if any)
lookupInitialReg :: N.RegisterName cl -> Recover (Maybe (FnRegValue cl))
lookupInitialReg reg = MapF.lookup reg <$> getCurRegs

{-
regMapFromState :: X86State f -> MapF N.RegisterName f
regMapFromState s =
  MapF.fromList [ MapF.Pair nm (s^.register nm)
                | Some nm <- x86StateRegisters
                ]
-}

------------------------------------------------------------------------
-- recoverFunction

insertXMMRegs :: MapF N.RegisterName FnRegValue -> MapF N.RegisterName FnRegValue
insertXMMRegs m = foldr (\i -> MapF.insert (N.XMMReg i) (FnRegValue (FnFloatArg i))) m [0..7]

-- | Recover the function at a given address.
recoverFunction :: InterpState -> CodeAddr -> Either String Function
recoverFunction s a = do
  let initRegs = MapF.empty
               & MapF.insert N.rdi (FnRegValue (FnIntArg 0))
               & MapF.insert N.rsi (FnRegValue (FnIntArg 1))
               & MapF.insert N.rdx (FnRegValue (FnIntArg 2))
               & MapF.insert N.rcx (FnRegValue (FnIntArg 3))
               & MapF.insert N.r8  (FnRegValue (FnIntArg 4))
               & MapF.insert N.r9  (FnRegValue (FnIntArg 5))
               & insertXMMRegs
               & MapF.insert N.rbx (CalleeSaved N.rbx)
               & MapF.insert N.r12 (CalleeSaved N.r12)
               & MapF.insert N.r13 (CalleeSaved N.r13)
               & MapF.insert N.r14 (CalleeSaved N.r14)
               & MapF.insert N.r15 (CalleeSaved N.r15)
               & MapF.insert N.rbp (CalleeSaved N.rbp)
  let rs = RS { _rsInterp = s
              , _rsBlocks   = Map.empty
              , _rsFrontier = Set.empty
              , _rsNextAssignId = 0
              , _rsRegMap    = Map.empty
              , _rsStackMap  = Map.empty
              , _rsCurLabel  = GeneratedBlock 0 0
              , _rsCurStmts  = Seq.empty
              , _rsAssignMap = MapF.empty
              }
  runRecover rs $ do
    addFrontier (GeneratedBlock a 0) initRegs initFnStack
    recoverIter
    block_map <- use rsBlocks
    return $! Function { fnAddr = a
                       , fnBlocks = Map.elems block_map
                       }

-- | Explore states until we have reached end of frontier.
recoverIter :: Recover ()
recoverIter = do
  f <- use rsFrontier
  case Set.maxView f of
    Nothing -> return ()
    Just (lbl,f') -> do
      trace ("Exploring " ++ show lbl) $ do
      b <- recoverBlock lbl
      rsFrontier %= Set.delete lbl
      rsBlocks   %= Map.insert lbl b

regValuePair :: N.RegisterName cl
             -> FnValue (BVType (N.RegisterClassBits cl))
             -> Recover (Maybe (MapF.Pair N.RegisterName FnRegValue))
regValuePair nm v = return $ Just $ MapF.Pair nm (FnRegValue v)

-- | Extract function arguments from state
stateArgs :: X86State Value -> Recover [Some FnValue]
stateArgs _ = trace "startArgs not yet implemented" $ return []

recoverBlock :: BlockLabel
             -> Recover FnBlock
recoverBlock lbl = do
  -- Clear stack offsets
  rsCurLabel  .= lbl
  rsCurStmts  .= Seq.empty
  rsAssignMap .= MapF.empty

  -- Get original block for address.
  Just b <- uses (rsInterp . blocks) (`lookupBlock` lbl)

  -- Compute stack height
  computeAllocSize $ do
    Fold.traverse_ recoverStmtStackDelta (blockStmts b)
    recoverTermStmtStackDelta (blockTerm b)

  interp_state <- use rsInterp
  let mem = memory interp_state
  case blockTerm b of
    Branch c x y -> do
      computeAllocSize $ do
        Fold.traverse_ recoverStmtStackDelta (blockStmts b)
        recoverTermStmtStackDelta (blockTerm b)
      Fold.traverse_ recoverStmt (blockStmts b)
      cv <- recoverValue "branch_cond" c
      regs <- getCurRegs
      stk  <- getCurStack
      addFrontier x regs stk
      addFrontier y regs stk
      stmts <- uses rsCurStmts Fold.toList
      return $! FnBlock { fbLabel = lbl
                        , fbStmts = stmts
                        , fbTerm = FnBranch cv x y
                        }
    FetchAndExecute proc_state
        -- The last statement was a call.
      | Just (prev_stmts, ret_addr) <- identifyCall mem (blockStmts b) proc_state -> do
        -- Compute any allocations that need to occur.
        computeAllocSize $ do
          Fold.traverse_ recoverStmtStackDelta prev_stmts
          modify $ mergeStackDelta (proc_state ^. register N.rsp) 8 True

        Fold.traverse_ recoverStmt prev_stmts
        stk <- getCurStack
        reg_pairs <- forM x86StateRegisters $ \(Some reg_name) -> do
          case () of
            _ | Just Refl <- testEquality reg_name N.IPReg -> do
              regValuePair reg_name $ FnConstantValue knownNat (toInteger ret_addr)
            _ | otherwise -> do
              return $ Nothing
        addFrontier (GeneratedBlock ret_addr 0) (MapF.fromList (catMaybes reg_pairs)) stk
        fn_stmts <- uses rsCurStmts Fold.toList
        call_tgt <- recoverValue "ip" (proc_state^.register N.rip)
        args <- (++ stackArgs stk) <$> stateArgs proc_state
        let ret_lbl = GeneratedBlock ret_addr 0
        return $! FnBlock { fbLabel = lbl
                          , fbStmts = fn_stmts
                          , fbTerm = FnCall call_tgt args ret_lbl
                          }
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
        stmts <- uses rsCurStmts Fold.toList
        let tgt_lbl = GeneratedBlock tgt_addr 0
        regs <- getCurRegs
        stk  <- getCurStack
        addFrontier tgt_lbl regs stk
        return $! FnBlock { fbLabel = lbl
                          , fbStmts = stmts
                          , fbTerm = FnJump tgt_lbl
                          }
    _ -> do
      trace ("WARNING: recoverTermStmt undefined for " ++ show (pretty (blockTerm b))) $ do
      computeAllocSize $ do
        Fold.traverse_ recoverStmtStackDelta (blockStmts b)
        recoverTermStmtStackDelta (blockTerm b)
      Fold.traverse_ recoverStmt (blockStmts b)
      stmts <- uses rsCurStmts Fold.toList
      return $! FnBlock { fbLabel = lbl
                        , fbStmts = stmts
                        , fbTerm = FnTermStmtUndefined
                        }

recoverWrite :: Value (BVType 64) -> Value tp -> Recover ()
recoverWrite addr val = do
  r_addr <- recoverAddr addr
  r_val  <- recoverValue "write_val" val
  addFnStmt $ FnWriteMem r_addr r_val

-- | This should add code as needed to support the statement.
recoverStmt :: Stmt -> Recover ()
recoverStmt s =
  case s of
    AssignStmt assign -> do
      let lhs = assignId assign
      case assignRhs assign of
        EvalApp _ -> return ()
        SetUndefined w -> do
          fnAssign <- mkFnAssign (FnSetUndefined w)
          rsAssignMap %= MapF.insert assign fnAssign
          addFnStmt $ FnAssignStmt fnAssign
        Read (MemLoc addr tp) -> do
          fn_addr <- recoverAddr addr
          fnAssign <- mkFnAssign (FnReadMem fn_addr tp)
          rsAssignMap %= MapF.insert assign fnAssign
          addFnStmt $ FnAssignStmt fnAssign
        _ -> trace ("recoverStmt undefined for " ++ show (pretty s)) $ do
          return ()
    Write (MemLoc addr _) val
      | Initial reg <- val -> do
        m_reg_val <- lookupInitialReg reg
        case m_reg_val of
          Just (CalleeSaved saved_reg) -> do
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
      return $ FnValueUnsupported

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
              return $! FnValueUnsupported
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
              fnAssign <- mkFnAssign (FnEvalApp app')
              rsAssignMap %= MapF.insert assign fnAssign
              return $! FnAssignedValue fnAssign
            _ -> do
              trace ("recoverValue does not yet support assignment " ++ show (pretty assign)) $
                return $ FnValueUnsupported
    Initial reg -> do
      m_reg_val <- lookupInitialReg reg
      case m_reg_val of
        Just (CalleeSaved _) -> do
          trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          return $ FnValueUnsupported
        Just (FnRegValue v) -> do
          return v
        _ ->
          trace ("recoverValue " ++ nm ++ " does not yet support initial value " ++ show reg) $
            return $ FnValueUnsupported

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
