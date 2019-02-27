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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.Recovery
  ( recoverFunction
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Foldable as Fold (toList, traverse_, foldlM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified GHC.Err.Located as Loc
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.CFG
import           Data.Macaw.CFG.BlockLabel
import           Data.Macaw.Discovery.State
import qualified Data.Macaw.Memory.Permissions as Perm
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.SyscallInfo
import           Data.Macaw.X86.X86Reg

import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86
import           Reopt.CFG.RegisterUse
import           Reopt.CFG.StackDepth

------------------------------------------------------------------------
-- RecoverState

data RecoverState ids = RS { rsMemory        :: !(Memory 64)
                           , rsInterp        :: !(DiscoveryFunInfo X86_64 ids)
                           , _rsNextAssignId :: !FnAssignId
                           , _rsAssignMap    :: !(MapF (AssignId ids) (FnValue X86_64))

                             -- Local state
                           , _rsCurLabel  :: !(BlockLabel 64)
                           , _rsCurStmts  :: !(Seq (FnStmt X86_64))

                           , _rsCurRegs   :: !(MapF X86Reg (FnRegValue X86_64))
                             -- ^ This maps registers to the associated value
                             -- at the start of the block after any stack allocations have
                             -- been performed.
                           , rsSyscallPersonality  :: !SyscallPersonality
                             -- ^ System call personality
                           , rsAssignmentsUsed     :: !(Set (Some (AssignId ids)))
                           , rsCurrentFunctionType :: !X86FunctionType
                             -- ^ The type of the function being recovered.
                           , rsFunctionArgs        :: !(Map (MemSegmentOff 64) X86FunctionType)
                           , rsWarnings :: ![String]
                             -- ^ List of warnings added so far.
                           }

rsNextAssignId :: Simple Lens (RecoverState ids) FnAssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

rsCurLabel :: Simple Lens (RecoverState ids) (BlockLabel 64)
rsCurLabel = lens _rsCurLabel (\s v -> s { _rsCurLabel = v })

-- | List of statements accumulated so far.
rsCurStmts :: Simple Lens (RecoverState ids) (Seq (FnStmt X86_64))
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments in original block to assignment in
rsAssignMap :: Simple Lens (RecoverState ids) (MapF (AssignId ids) (FnValue X86_64))
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

-- | This maps registers to the associated value at the start of the block after
-- any stack allocations have been performed.
rsCurRegs :: Simple Lens (RecoverState ids) (MapF X86Reg (FnRegValue X86_64))
rsCurRegs = lens _rsCurRegs (\s v -> s { _rsCurRegs = v })

------------------------------------------------------------------------
-- Recover

data RecoverResult ids a
  = RecoverError !String
  | RecoverSuccess !a !(RecoverState ids)

newtype Recover ids a = Recover { runRecover :: RecoverState ids -> RecoverResult ids a
                                }

evalRecover :: RecoverState ids -> Recover ids a -> Either String ([String], a)
evalRecover s0 m =
  case runRecover m s0 of
    RecoverError msg -> Left msg
    RecoverSuccess r s -> Right (reverse (rsWarnings s), r)

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

--  | Report a warning
addWarning :: String -> Recover ids ()
addWarning msg = do
  s <- get
  put $! s { rsWarnings = msg : rsWarnings s }

-- | Create a fresh assign id.
freshId :: Recover ids FnAssignId
freshId = do
  FnAssignId next_id <- use rsNextAssignId
  rsNextAssignId .= FnAssignId (next_id + 1)
  return $! FnAssignId next_id

mkReturnVar :: TypeRepr tp -> Recover ids (FnReturnVar tp)
mkReturnVar tp = (\next_id -> FnReturnVar next_id tp) <$> freshId

-- | Add a statement to the list of statements in current function block.
addFnStmt :: FnStmt X86_64 -> Recover ids ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

-- | This evaluates the right-hand side of an assignment and returns
-- the value.
evalAssignRhs :: FnAssignRhs X86_64 (FnValue X86_64) tp -> Recover ids (FnValue X86_64 tp)
evalAssignRhs rhs = do
  fnAssign <- (\next_id -> FnAssignment next_id rhs) <$> freshId
  seq fnAssign $ do
    addFnStmt $ FnAssignStmt fnAssign
    return $ FnAssignedValue fnAssign

------------------------------------------------------------------------
-- valueDependencies

-- | Compute the complete set of functional values needed to evaluate the given values.
--
-- e.g. 'valueDependencies l' will return the complete set of assignments needed to compute the values in 'l'
-- or return an error if the values in 'l' depend on architecture-specific functions or memory.
valueDependencies :: forall ids . [Some (Value X86_64 ids)] -> Either String (Set (Some (AssignId ids)))
valueDependencies = go Set.empty
  where go :: Set (Some (AssignId ids))
           -> [Some (Value X86_64 ids)]
           -> Either String (Set (Some (AssignId ids)))
        go s [] = Right s
        go s (Some v:r) =
          case v of
            BoolValue{} -> go s r
            BVValue{} -> go s r
            RelocatableValue{} -> go s r
            SymbolValue{} -> go s r
            Initial{} -> go s r
            AssignedValue a
              | Set.member (Some (assignId a)) s -> go s r
              | otherwise ->
                case assignRhs a of
                  EvalApp app -> go (Set.insert (Some (assignId a)) s) (foldlFC (\prev l -> Some l:prev) r app)
                  SetUndefined{} -> go s r
                  ReadMem{} -> Left $ "Depends on read " ++ show (pretty a)
                  CondReadMem{} -> Left $ "Depends on read " ++ show (pretty a)
                  EvalArchFn{} -> Left $ "Depends on archfn " ++ show (pretty a)

------------------------------------------------------------------------
-- recoverValue

-- | Recover a stack value
recoverValue' :: Loc.HasCallStack
              => RecoverState ids
              -> Value X86_64 ids tp
              -> Either String (FnValue X86_64 tp)
recoverValue' s v = do
  let interpState = rsInterp s
  let mem = rsMemory s
  let assignMap   = s^.rsAssignMap
  case v of
    RelocatableValue w addr ->
      case asSegmentOff mem addr of
        Nothing ->
          case asAbsoluteAddr addr of
            Just absAddr -> Right $ FnConstantValue (addrWidthNatRepr w) (toInteger absAddr)
            Nothing -> Right $ FnValueUnsupported ("Relative addr " ++ show addr) (addrWidthTypeRepr w)

        Just addrRef -> do
          let seg = segoffSegment addrRef
          case () of
            _ | segmentFlags seg `Perm.hasPerm` Perm.execute
              , Just ft <- Map.lookup addrRef (rsFunctionArgs s) -> do
                  Right $! FnFunctionEntryValue (resolveX86FunctionType ft) addrRef

            _ | Map.member addrRef (interpState^.parsedBlocks) ->
                  let msg = "Do not support unctions that reference block addresses."
                   in Right $! FnValueUnsupported msg (addrWidthTypeRepr w)

              | segmentFlags seg `Perm.hasPerm` Perm.write -> do
                  Right $! FnGlobalDataAddr addrRef

              -- FIXME: do something more intelligent with rodata?
              | segmentFlags seg `Perm.hasPerm` Perm.read -> do
                  Right $! FnGlobalDataAddr addrRef

              | otherwise -> do
                  Right $! FnValueUnsupported ("segment pointer " ++ show addr) (typeRepr v)

    SymbolValue w sym ->
      Right $ FnValueUnsupported ("Symbol references " ++ show sym) (addrWidthTypeRepr w)
    BoolValue b ->
      Right $ FnConstantBool b

    BVValue w i ->
      Right $ FnConstantValue w i

    AssignedValue asgn -> do
      case MapF.lookup (assignId asgn) assignMap of
        Just rval -> Right rval
        Nothing -> Left $ "Encountered uninitialized assignment: " ++ show (assignId asgn) ++ "\n"
          ++ show (MapF.keys assignMap)
    Initial reg -> do
      case MapF.lookup reg (s^.rsCurRegs) of
        Nothing ->
          Right $! FnValueUnsupported ("Initial register " ++ show reg) (typeRepr reg)
        Just (CalleeSaved _) ->
          -- trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          Right $! FnValueUnsupported ("Initial (callee) register " ++ show reg) (typeRepr reg)
        Just (FnRegValue v') ->
          Right v'

recoverValue :: Loc.HasCallStack
             => Value X86_64 ids tp
             -> Recover ids (FnValue X86_64 tp)
recoverValue v = do
  s <- get
  case recoverValue' s v of
    Left msg -> Loc.error $ "Error at " ++ show (s^.rsCurLabel) ++ "\n" ++ msg
    Right fnVal -> pure fnVal

------------------------------------------------------------------------
-- recoverRegister

recoverRegister :: Loc.HasCallStack
                => RegState X86Reg (Value X86_64 ids)
                -> X86Reg tp
                -> Recover ids (FnValue X86_64 tp)
recoverRegister proc_state r = do
  s <- get
  case recoverValue' s (proc_state ^. boundValue r) of
    Left msg -> do
      usedAssigns <- gets rsAssignmentsUsed
      Loc.error $ "Unbound register " ++ show r ++ "\n"
             ++ msg ++ "\n"
             ++ "Used: " ++ show usedAssigns
    Right fnVal -> pure fnVal

recoverCallTarget :: Loc.HasCallStack
                => RegState X86Reg (Value X86_64 ids)
                -> Recover ids (FnValue X86_64 (BVType 64), X86FunctionType)
recoverCallTarget regState = do
  s <- get
  let mem = rsMemory s
  let v = regState ^. boundValue ip_reg
  case v of
    RelocatableValue _ addr
      | Just addrRef <- asSegmentOff mem addr
      , Just ft <- Map.lookup addrRef (rsFunctionArgs s) -> do
          pure (FnFunctionEntryValue (resolveX86FunctionType ft) addrRef, ft)
    _ ->
      case recoverValue' s v of
        Left msg -> do
          usedAssigns <- gets rsAssignmentsUsed
          Loc.error $ "IP unresolved\n"
            ++ msg ++ "\n"
            ++ "Used: " ++ show usedAssigns
        Right call_tgt ->
          pure (call_tgt, ftMaximumFunctionType)

------------------------------------------------------------------------
-- recoverStmt

-- | Add statements for the assignment
recoverAssign :: Loc.HasCallStack => Assignment X86_64 ids tp -> Recover ids ()
recoverAssign asgn = do
  m_seen <- uses rsAssignMap (MapF.lookup (assignId asgn))
  case m_seen of
    Just{} -> return ()
    Nothing -> do
      rhs <-
        case assignRhs asgn of
          EvalApp app ->
            FnEvalApp <$> traverseFC recoverValue app
          SetUndefined tp ->
            pure (FnSetUndefined tp)
          ReadMem addr tp ->
            (`FnReadMem` (typeRepr tp)) <$> recoverValue addr
          CondReadMem tp cond addr def ->
            FnCondReadMem tp
            <$> recoverValue cond
            <*> recoverValue addr
            <*> recoverValue def
          EvalArchFn f _ ->
            FnEvalArchFn <$> traverseFC recoverValue f
      rval <- evalAssignRhs rhs
      -- Associate assign id with the reuslt
      rsAssignMap %= MapF.insert (assignId asgn) rval

recoverWrite :: BVValue X86_64 ids 64 -> Value X86_64 ids tp -> Recover ids ()
recoverWrite addr val = do
  r_addr <- recoverValue addr
  r_val  <- recoverValue val
  addFnStmt $ FnWriteMem r_addr r_val

-- | This should add code as needed to support the statement.
recoverStmt :: Stmt X86_64 ids -> Recover ids ()
recoverStmt s = do
  case s of
    AssignStmt asgn -> do
      usedAssigns <- gets rsAssignmentsUsed
      -- Only add assignment if it is used.
      when (Some (assignId asgn) `Set.member` usedAssigns) $ do
        recoverAssign asgn
    WriteMem addr _ val
      | Initial reg <- val -> do
        reg_v <- uses rsCurRegs (MapF.lookup reg)
        case reg_v of
          Just (CalleeSaved _saved_reg) -> do
            case asStackAddrOffset addr of
              Just _int_addr_off -> do
                -- modifyCurStack $ recordCalleeSavedWrite int_addr_off saved_reg
                return ()
              Nothing -> do
                addWarning "Could not interpret callee saved offset"
                recoverWrite addr val
          _ -> do
            recoverWrite addr val
      | otherwise -> do
        recoverWrite addr val
    Comment msg -> do
      addFnStmt $ FnComment msg
    ExecArchStmt stmt0 -> do
      stmt <- traverseF recoverValue stmt0
      addFnStmt (FnArchStmt (X86FnStmt stmt))
    InstructionStart _ _ -> do
      pure ()
    ArchState _ _ -> do
      pure ()

------------------------------------------------------------------------
-- RecoveredBlockInfo

-- | Builds a mapping from labels to recovered function blocks and a
-- mapping from the block label to the register map resulting from
-- block.
type RecoveredBlockInfo = Map (BlockLabel 64) (FnBlock X86_64)

emptyRecoveredBlockInfo :: RecoveredBlockInfo
emptyRecoveredBlockInfo = Map.empty

addFnBlock :: FnBlock X86_64 -> RecoveredBlockInfo -> RecoveredBlockInfo
addFnBlock b info = Map.insert (fbLabel b) b info

------------------------------------------------------------------------
-- recoverBlock

bitcast :: FnValue X86_64 i -> WidthEqProof i o -> Recover ids (FnValue X86_64 o)
bitcast x p = evalAssignRhs $ FnEvalApp (Bitcast x p)

-- Figure out what is preserved across the function call.
getPostCallValue :: BlockLabel 64
                 -> RegState X86Reg (Value X86_64 ids)
                     -- ^ Value of registers before syscall
                 -> [FnReturnVar (BVType 64)]
                 -> [FnReturnVar (VecType 8 (FloatType DoubleFloat))]
                 -> X86Reg tp
                 -> Recover ids (FnValue X86_64 tp)
getPostCallValue lbl proc_state intrs floatrs r = do
  case r of
    _ | Just Refl <- testEquality (typeRepr r) (BVTypeRepr n64)
      , Just rv <- lookup r (x86ResultRegs `zip` intrs) ->
        return $ FnReturn rv

      | Just Refl <- testEquality r sp_reg -> do
        spv <- recoverRegister proc_state sp_reg
        evalAssignRhs $ FnEvalApp $ BVAdd knownNat spv (FnConstantValue n64 8)

    X86_ZMMReg rIdx
      | length floatrs > fromIntegral rIdx -> do
        let x = FnReturn (floatrs !! fromIntegral rIdx)
        v <- bitcast x (VecEqCongruence n8 (FromFloat DoubleFloatRepr))
        bitcast v (PackBits n8 n64)

   -- df is 0 after a function call.
    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False

    _ | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister proc_state r
    _ -> do
      addWarning ("WARNING: Nothing known about register "
                   ++ show r ++ " at " ++ show lbl ++ "\n"
                 )
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
                    -> Recover ids (FnValue X86_64 tp)
getPostSyscallValue lbl proc_state r =
  case r of
    _ | Just Refl <- testEquality r sp_reg -> do
          recoverRegister proc_state sp_reg
      | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister proc_state r

    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False

    _ -> do
      addWarning ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show lbl)
      return (FnValueUnsupported ("post-syscall register " ++ show r) (typeRepr r))

mkBlock :: BlockLabel 64
        -> [Some (PhiBinding X86Reg)]
        -> FnTermStmt X86_64
        -> MapF X86Reg (FnRegValue X86_64)
        -> Recover ids (FnBlock X86_64)
mkBlock lbl phis tm m = do
  curStmts <- use rsCurStmts
  rsCurStmts .= Seq.empty
  return $! FnBlock { fbLabel = lbl
                    , fbPhiNodes = phis
                    , fbStmts  = Fold.toList curStmts
                    , fbTerm   = tm
                    , fbRegMap = m
                    }

recoverX86TermStmt :: forall ids
                   .  DemandedUseMap
                   -- ^ Map from address to registers that address will read.
                   -> [Some (PhiBinding X86Reg)]
                   -- ^ Phi bindings from input block
                   -> BlockLabel 64
                   -> X86TermStmt ids
                   -> RegState (ArchReg X86_64) (Value X86_64 ids)
                   -> Maybe (MemSegmentOff 64)
                   -> Recover ids (FnBlock X86_64)
recoverX86TermStmt registerUseMap phis lbl tstmt proc_state mnext_addr =
  case tstmt of
    Hlt ->
      throwError "hlt is not supported in function recovery."
    UD2 -> do
      throwError "ud2 is not supported in function recovery."
    X86Syscall -> do
      sysp <- gets rsSyscallPersonality

      let syscallRegs :: [ArchReg X86_64 (BVType 64)]
          syscallRegs = syscallArgumentRegs

      let args
            | BVValue _ this_call_no <- proc_state^.boundValue syscall_num_reg
            , Just (_,_,argtypes) <- Map.lookup (fromInteger this_call_no) (spTypeInfo sysp) =
              take (length argtypes) syscallRegs
            | otherwise =
              syscallRegs

      let rregs = spResultRegisters sysp


      let mkRet :: MapF X86Reg (FnRegValue X86_64)
                -> Some X86Reg
                -> Recover ids (MapF X86Reg (FnRegValue X86_64))
          mkRet m (Some r) = do
            rv <- mkReturnVar (typeRepr r)
            return $ MapF.insert r (FnRegValue $ FnReturn rv) m

      initMap <- foldM mkRet MapF.empty rregs

      -- pull the return variables out of initMap (in order of rregs)
      let getVar :: Maybe (FnRegValue X86_64 tp) -> FnReturnVar tp
          getVar (Just (FnRegValue (FnReturn rv))) = rv
          getVar _ = Loc.error "impossible"

      let rets :: [Some FnReturnVar]
          rets = map f rregs
            where f (Some r) = Some $ getVar $ MapF.lookup r initMap

      -- Fold operation to update register map with values persisted across system calls.
      let go ::  MapF X86Reg (FnRegValue X86_64)
            -> Some X86Reg
            -> Recover ids (MapF X86Reg (FnRegValue X86_64))
          go m (Some r) = do
            v <- getPostSyscallValue lbl proc_state r
            return $ MapF.insert r (FnRegValue v) m

      call_num <- recoverRegister proc_state syscall_num_reg
      args'  <- mapM (recoverRegister proc_state) args
      addFnStmt (FnArchStmt (X86FnSystemCall call_num args' rets))
      case mnext_addr of
        Nothing -> do
          -- TODO: Fix this by adding a
          error "Recovery: Could not find system call return label"
        Just next_addr -> do
          let provides = Map.findWithDefault Set.empty next_addr registerUseMap
          regs' <- foldM go initMap (provides `Set.difference` Set.fromList rregs)
          mkBlock lbl phis (FnJump (mkRootBlockLabel next_addr)) regs'

-- | Given a register state this interprets the function arguments.
evalFunctionArgs :: RegState X86Reg (Value X86_64 ids)
                 -> X86FunctionType
                 -> Recover ids [Some (FnValue X86_64)]
evalFunctionArgs proc_state ft = do
  intArgs <-
    forM (take (fnNIntArgs ft) x86ArgumentRegs) $ \r ->
      Some <$> recoverRegister proc_state r
  floatArgs <-
    forM [0 .. (fnNFloatArgs ft - 1)] $ \i -> do
      v512 <- recoverRegister proc_state (X86_ZMMReg (fromIntegral i))
      vv <- bitcast v512 (UnpackBits n8 n64)
      fmap Some $ bitcast vv (VecEqCongruence n8 (ToFloat DoubleFloatRepr))
      -- evalAssignRhs $ FnEvalApp $ Bitcast v512 (VecTypeRepr n8 (FloatTypeRepr DoubleFloatRepr))
  pure $! intArgs ++ floatArgs

-- | This removes the instruction that pushes the return address to the stack.
stripPushReturn :: MemSegmentOff 64 -- ^ Expected return address
                -> BVValue X86_64 ids 64 -- ^ Value in stack pointer next.
                -> [Stmt X86_64 ids] -- ^ Statements in reverse order
                -> Either String [Stmt X86_64 ids]
stripPushReturn _addr _next_sp [] =
  Left "No instructions with push of return address."
stripPushReturn _addr _next_sp (AssignStmt{}:_) =
  Left "Assignment before push of return address."
stripPushReturn retAddr next_sp (WriteMem addr valRepr val:l)
  -- Check for a call statement by determining if the last statement
  -- writes an executable address to the stack pointer.
  | Just _ <- testEquality addr next_sp
    -- Check this is the right length.
  , Just Refl <- testEquality (BVMemRepr n8 LittleEndian) valRepr
    -- Check if value is a valid literal address
  , Just val_a <- valueAsMemAddr val
    -- Check if segment of address is marked as executable.
  , segoffAddr retAddr == val_a =
    Right l
  | otherwise =
    Left "Unexpected right before push of return address."
stripPushReturn _addr _ (InstructionStart{}:_) =
  Left "Instruction start before return address."
stripPushReturn addr next_sp (s@Comment{}:l)= do
  (s:) <$> stripPushReturn addr next_sp l
stripPushReturn _addr _ (ExecArchStmt{}:_) =
  Left "Arch statement before push of return address."
stripPushReturn addr next_sp (s@ArchState{}:l) =
  (s:) <$> stripPushReturn addr next_sp l

mkIntReturnVars :: X86FunctionType -> Recover ids [FnReturnVar (BVType 64)]
mkIntReturnVars ft = replicateM (fnNIntRets ft) $ mkReturnVar knownRepr

type ZMMFloatType = VecType 8 (FloatType DoubleFloat)

zmmFloatRepr :: TypeRepr ZMMFloatType
zmmFloatRepr = VecTypeRepr n8 (FloatTypeRepr DoubleFloatRepr)


mkFloatReturnVars :: X86FunctionType
                  -> Recover ids [FnReturnVar ZMMFloatType]
mkFloatReturnVars ft =
  replicateM (fnNFloatRets ft) $ mkReturnVar zmmFloatRepr

recoverBlock :: forall ids
             .  DemandedUseMap
                -- ^ Map from address to registers that address will read.
             -> [Some (PhiBinding X86Reg)]
                -- ^ Phi bindings from input block
             -> ParsedBlock X86_64 ids
                -- ^ Map for entire block
             -> StatementList X86_64 ids
                -- ^ Current list of statements to recover
             -> RecoveredBlockInfo
             -> Recover ids RecoveredBlockInfo
recoverBlock registerUseMap phis b stmts blockInfo = seq blockInfo $ do
  let addr = pblockAddr b
  let lbl = GeneratedBlock addr (stmtsIdent stmts)
  when (stmtsIdent stmts /= 0 &&  not (null phis)) $ do
    error $ "recoverBlock asked to create a subblock " ++ show lbl ++ " with phi nodes."
  -- Clear stack offsets
  rsCurLabel  .= lbl

  -- Recover term statement
  case stmtsTerm stmts of
    ParsedCall proc_state m_ret_addr -> do
      case m_ret_addr of
        -- Add a call, then jump
        Just ret_addr -> do
          -- Recover statements
          let next_sp = proc_state^.boundValue sp_reg
          case stripPushReturn ret_addr next_sp (reverse (stmtsNonterm stmts)) of
            Left e -> error e
            Right l -> Fold.traverse_ recoverStmt (reverse l)
          -- Get call target
          (call_tgt, ft) <- recoverCallTarget proc_state
          -- Evaluate call arguments
          args <- evalFunctionArgs proc_state ft
          -- Case split on whether this is a tail call or not so we can decide which
          -- type of statement to emit.
          let ftp = resolveX86FunctionType ft
          -- May not be used (only if called function returns at these types)
          intrs   <- mkIntReturnVars ft
          floatrs <- mkFloatReturnVars ft
          let ri = fmap Some intrs ++ fmap Some floatrs
          -- Add call statement
          addFnStmt (FnCall call_tgt ftp args ri)
          -- Get registers that return address needs.
          let nextRegsNeeded = Map.findWithDefault Set.empty ret_addr registerUseMap
          -- Get registers for next block.
          regs' <- do
            let go :: MapF X86Reg (FnRegValue X86_64)
                   -> Some X86Reg
                   -> Recover ids (MapF X86Reg (FnRegValue X86_64))
                go m (Some r) = do
                  v <- getPostCallValue lbl proc_state intrs floatrs r
                  return $! MapF.insert r (FnRegValue v) m
            foldM go MapF.empty nextRegsNeeded
          -- Create block that ends with jump to return address.
          fb <- mkBlock lbl phis (FnJump (mkRootBlockLabel ret_addr)) regs'
          return $! blockInfo & addFnBlock fb
        Nothing -> do
          thisFunType <- gets rsCurrentFunctionType
          -- Recover statements
          Fold.traverse_ recoverStmt (stmtsNonterm stmts)
          -- Get call target
          (callTarget, callX86FunType) <- recoverCallTarget proc_state
          -- Evaluate call arguments
          args <- evalFunctionArgs proc_state callX86FunType
          intrs   <- mkIntReturnVars callX86FunType
          floatrs <- mkFloatReturnVars callX86FunType
          let ri = fmap Some intrs ++ fmap Some floatrs
          -- Add function
          let callFunType = resolveX86FunctionType callX86FunType
          addFnStmt (FnCall callTarget callFunType args ri)

          -- Create block
          let ri2 = take (fnNIntRets thisFunType)
                         (fmap (Some . FnReturn) intrs   ++ repeat (Some (FnUndefined (BVTypeRepr n64))))
                 ++ take (fnNFloatRets thisFunType)
                         (fmap (Some . FnReturn) floatrs ++ repeat (Some (FnUndefined zmmFloatRepr)))
          fb <- mkBlock lbl phis (FnRet ri2) MapF.empty
          return $! blockInfo & addFnBlock fb
    PLTStub _ _ _ -> do
      -- Recover statements
      Fold.traverse_ recoverStmt (stmtsNonterm stmts)
      Loc.error $ "Cannot recover functions with PLT stubs."
    ParsedTranslateError _ -> do
      Loc.error "Cannot recover function in blocks where translation error occurs."
    ClassifyFailure _ ->
      Loc.error $ "Classification failed in Recovery"
    ParsedIte c tblock fblock -> do
      -- Recover statements
      Fold.traverse_ recoverStmt (stmtsNonterm stmts)
      -- Recover term statement
      cv <- recoverValue c
      fb <- mkBlock lbl phis
                    (FnBranch cv (lbl { labelIndex = stmtsIdent tblock })
                                 (lbl { labelIndex = stmtsIdent fblock }))
                    MapF.empty
      xr <- recoverBlock registerUseMap [] b tblock (blockInfo & addFnBlock fb)
      recoverBlock registerUseMap [] b fblock xr

    ParsedJump proc_state tgt_addr -> do
      -- Recover statements
      Fold.traverse_ recoverStmt (stmtsNonterm stmts)
      -- Recover term statement
      let go :: MapF X86Reg (FnRegValue X86_64)
             -> Some X86Reg
             -> Recover ids (MapF X86Reg (FnRegValue X86_64))
          go m (Some r) = do
            v <- recoverRegister proc_state r
            return $ MapF.insert r (FnRegValue v) m

      let provides = Map.findWithDefault Set.empty tgt_addr registerUseMap
      regs' <- foldM go MapF.empty provides

      let tgt_lbl = mkRootBlockLabel tgt_addr
      bl <- mkBlock lbl phis (FnJump tgt_lbl) regs'
      pure $! blockInfo & addFnBlock bl

    ParsedReturn proc_state -> do
      -- Recover statements
      Fold.traverse_ recoverStmt (stmtsNonterm stmts)
      -- Recover term stmt
      ft <- gets rsCurrentFunctionType
      grets' <- mapM (recoverRegister proc_state) (ftIntRetRegs ft)
      frets' <- forM (ftFloatRetRegs ft) $ \r -> do
        x <- recoverRegister proc_state r
        v <- bitcast x (UnpackBits n8 n64)
        bitcast v (VecEqCongruence n8 (ToFloat DoubleFloatRepr))

      let ri = fmap Some grets' ++ fmap Some frets'
      bl <- mkBlock lbl phis (FnRet ri) MapF.empty
      pure $! blockInfo & addFnBlock bl

    ParsedArchTermStmt ts proc_state next_addr -> do
      -- Recover statements
      Fold.traverse_ recoverStmt (stmtsNonterm stmts)
      -- Recover term statement
      fb <- recoverX86TermStmt registerUseMap phis lbl ts proc_state next_addr
      pure $! blockInfo & addFnBlock fb

    ParsedLookupTable proc_state idx vec -> do
      -- Recover statements
      Fold.traverse_ recoverStmt (stmtsNonterm stmts)
      -- Recover term statement
      let go :: MapF X86Reg (FnRegValue X86_64)
             -> Some X86Reg
             -> Recover ids (MapF X86Reg (FnRegValue X86_64))
          go m (Some r) = do
            v <- recoverRegister proc_state r
            return $ MapF.insert r (FnRegValue v) m

      let getRegs next_addr = Map.findWithDefault Set.empty next_addr registerUseMap
      let provides = Set.unions (getRegs <$> V.toList vec)
      regs' <- foldM go MapF.empty provides

      idx'   <- recoverValue idx

      bl <-  mkBlock lbl phis (FnLookupTable idx' vec) regs'
      pure $! blockInfo & addFnBlock bl

------------------------------------------------------------------------
-- allocateStackFrame
allocateStackFrame :: forall ids
                   . StatementList X86_64 ids
                   -> StackDepthValue X86_64 ids
                   -> Recover ids (FnValue X86_64 (BVType 64))
allocateStackFrame stmts s = do
  -- Compute assignments needed for evaluating dynamic part of stack
  let vals = (Some . stackDepthOffsetValue) <$> Set.toList (dynamicPart s)
  case valueDependencies vals of
    Left msg -> throwError $ "Could not allocate stack frame: " ++ msg
    Right asgns -> do
      -- Resolve each assignment in initial block.
      let goStmt :: Set (Some (AssignId ids)) -> Stmt X86_64 ids -> Recover ids (Set (Some (AssignId ids)))
          goStmt depSet (AssignStmt asgn)
            | Set.member (Some (assignId asgn)) depSet = do
                recoverAssign asgn
                pure $! Set.delete (Some (assignId asgn)) depSet
          goStmt depSet _ = return depSet
      remaining_asgns <- foldlM goStmt asgns (stmtsNonterm stmts)
      when (not (Set.null remaining_asgns)) $ do
        throwError $ "Found unsupported symbolic stack references: " ++ show remaining_asgns
  let doOneDelta :: StackDepthOffset X86_64 ids
                 -> Recover ids (FnValue X86_64 (BVType 64))
                 -> Recover ids (FnValue X86_64 (BVType 64))
      doOneDelta (Pos _) _   = Loc.error "Saw positive stack delta"
      doOneDelta (Neg x) m_v = do
        v0 <- m_v
        v  <- recoverValue x
        evalAssignRhs $ FnEvalApp $ BVAdd knownNat v v0
  let sz0 = toInteger (negate $ staticPart s)
  szv <- foldr doOneDelta (return (FnConstantValue n64 sz0)) (dynamicPart s)
  alloc <- evalAssignRhs $ FnAlloca szv
  evalAssignRhs $ FnEvalApp $ BVAdd knownNat alloc szv

------------------------------------------------------------------------
-- recoverFunction

-- | Generate phi bindings
makePhis :: [BlockLabel 64]
            -- ^ Predecessors for this block
         -> MapF X86Reg FnPhiVar
            -- ^ Map from registers to variable associated with them.
         -> [Some (PhiBinding X86Reg)]
makePhis preds regs = go <$> MapF.toList regs
  where go :: MapF.Pair X86Reg FnPhiVar -> Some (PhiBinding X86Reg)
        go (MapF.Pair r phi_var) = Some (PhiBinding phi_var ((,r) <$> preds))

recoverInnerBlock :: DiscoveryFunInfo X86_64 ids
                  -> DemandedUseMap
                  -> FunPredMap 64
                  -> RecoveredBlockInfo
                  -> MemSegmentOff 64
                  -> Recover ids RecoveredBlockInfo
recoverInnerBlock fInfo blockRegs blockPreds blockInfo addr = do
  let mkIdFromReg :: MapF X86Reg FnPhiVar
                  -> Some X86Reg
                  -> Recover ids (MapF X86Reg FnPhiVar)
      mkIdFromReg m (Some r) = do
        next_id <- freshId
        pure $! MapF.insert r (FnPhiVar next_id (typeRepr r)) m
  regs1 <-
    case Map.lookup addr blockRegs of
      Nothing -> do
        addWarning ("WARNING: No regs for " ++ show addr)
        pure MapF.empty
      Just x -> do
        foldM mkIdFromReg MapF.empty x
  rsCurRegs .= fmapF (FnRegValue . FnPhiValue) regs1
   -- Get predecessors for this block.
  let Just preds = Map.lookup addr blockPreds
   -- Generate phi nodes from predecessors and registers that this block refers to.
  let phis = makePhis preds regs1
  let Just reg = Map.lookup addr (fInfo^.parsedBlocks)
  recoverBlock blockRegs phis reg (blockStatementList reg) blockInfo

resolveX86FunctionType :: X86FunctionType -> FunctionType X86_64
resolveX86FunctionType ft =
  FunctionType { fnArgTypes = mapSome typeRepr <$> ftArgRegs ft
               , fnReturnTypes = replicate (fnNIntRets   ft) (Some (BVTypeRepr n64))
                              ++ replicate (fnNFloatRets ft) (Some zmmFloatRepr)
               }

-- | Recover the function at a given address.
--
-- Returns either an error message with a fatal error, or a list of warnings and a function.
recoverFunction :: forall ids
                .  SyscallPersonality
                -> Map (MemSegmentOff 64) X86FunctionType
                -> Memory 64
                -> DiscoveryFunInfo X86_64 ids
                -> Either String ([String], Function X86_64)
recoverFunction sysp fArgs mem fInfo = do

  let a = discoveredFunAddr fInfo
  let blockPreds = funBlockPreds fInfo

  let (warn, cft) =
        case Map.lookup a fArgs of
          Just r -> ([], r)
          Nothing -> (["Missing type for " ++ show a], ftMaximumFunctionType)

  let (usedAssigns, blockRegs)
        = registerUse mem sysp fArgs fInfo cft blockPreds


  let insReg i (Some r) = MapF.insert r (FnRegValue (FnRegArg r i))
  let insCalleeSaved (Some r) = MapF.insert r (CalleeSaved r)

  let initRegs = MapF.empty
               & flip (ifoldr insReg)        (ftArgRegs cft)
               & flip (foldr insCalleeSaved) x86CalleeSavedRegs
                 -- Set df to 0 at function start.
               & MapF.insert DF (FnRegValue (FnConstantBool False))

  let rs = RS { rsMemory        = mem
              , rsInterp = fInfo
              , _rsNextAssignId = FnAssignId 0
              , _rsCurLabel  = GeneratedBlock a 0
              , _rsCurStmts  = Seq.empty
              , _rsAssignMap = MapF.empty
              , _rsCurRegs   = initRegs
              , rsSyscallPersonality = sysp
              , rsAssignmentsUsed = usedAssigns
              , rsCurrentFunctionType = cft
              , rsFunctionArgs = fArgs
              , rsWarnings     = warn
              }

  evalRecover rs $ do
    -- The first block is special as it needs to allocate space for
    -- the block stack area.  It should also not be in blockPreds (we
    -- assume no recursion/looping through the initial block)

    -- Make the alloca and init rsp.  This is the only reason we
    -- need rsCurStmts

    let Just b = Map.lookup a (fInfo^.parsedBlocks)
    let stmts = blockStatementList b

    case maximumStackDepth fInfo of
      Right depths ->
        case Set.toList depths of
          [] -> do
            addWarning "WARNING: no stack use detected"
            pure ()
          [s] -> do
            when (not (null (dynamicPart s)) || staticPart s /= 0) $ do
              spTop <- allocateStackFrame stmts s
              rsCurRegs %= MapF.insert sp_reg (FnRegValue spTop)
          _ -> throwError $ "non-singleton stack depth"
      Left msg -> throwError $ "maximumStackDepth: " ++ msg

    r0 <- recoverBlock blockRegs [] b stmts emptyRecoveredBlockInfo
    rf <- foldM (recoverInnerBlock fInfo blockRegs blockPreds) r0 (Map.keys blockPreds)

    pure Function { fnAddr = a
                  , fnType = resolveX86FunctionType cft
                  , fnName = discoveredFunName fInfo
                  , fnBlocks = Map.elems rf
                  }
