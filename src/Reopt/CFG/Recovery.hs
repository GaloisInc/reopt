{-|
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
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
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
import           Data.String
import qualified Data.Vector as V
import           GHC.Stack
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.CFG
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



addrBlockLabel :: MemWidth w => MemSegmentOff w -> FnBlockLabel
addrBlockLabel o = fromString $
  let a = segoffAddr o
   in "block_" ++ show (addrBase a) ++ "_" ++ showHex (memWordToUnsigned (addrOffset a)) ""

------------------------------------------------------------------------
-- RecoverState

-- | State used for recovering a function.
data RecoverState ids = RS { rsMemory        :: !(Memory 64)
                           , rsInterp        :: !(DiscoveryFunInfo X86_64 ids)
                           , _rsNextAssignId :: !FnAssignId
                           , _rsAssignMap    :: !(MapF (AssignId ids) (FnValue X86_64))

                             -- Local state
                           , _rsCurAddr  :: !(MemSegmentOff 64)
                           , _rsCurStmts  :: !(Seq (FnStmt X86_64))

                           , _rsCurRegs   :: !(MapF X86Reg (FnRegValue X86_64))
                             -- ^ This maps registers to the
                             -- associated value at the start of the
                             -- block after any stack allocations have
                             -- been performed.
                           , rsSyscallPersonality  :: !SyscallPersonality
                             -- ^ System call personality
                           , rsAssignmentsUsed     :: !(Set (Some (AssignId ids)))
                           , rsCurrentFunctionType :: !X86FunTypeInfo
                             -- ^ The type of the function being recovered.
                           , rsFunctionArgs
                               :: !(Map (MemSegmentOff 64) (BSC.ByteString, X86FunTypeInfo))
                           , rsWarnings :: ![String]
                             -- ^ List of warnings added so far.
                           }

rsNextAssignId :: Simple Lens (RecoverState ids) FnAssignId
rsNextAssignId = lens _rsNextAssignId (\s v -> s { _rsNextAssignId = v })

rsCurAddr :: Simple Lens (RecoverState ids) (MemSegmentOff 64)
rsCurAddr = lens _rsCurAddr (\s v -> s { _rsCurAddr = v })

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

-- | Monad for recovering code in a function.
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

-- | Create a fresh return variable given the type id.
mkReturnVar :: TypeRepr tp -> Recover ids (FnReturnVar tp)
mkReturnVar tp = (`FnReturnVar` tp) <$> freshId

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
recoverValue :: HasCallStack
             => Value X86_64 ids tp
             -> Recover ids (FnValue X86_64 tp)
recoverValue v = do
  s <- get
  let interpState = rsInterp s
  let mem = rsMemory s
  let assignMap   = s^.rsAssignMap
  case v of
    RelocatableValue w addr ->
      case asSegmentOff mem addr of
        Nothing ->
          case asAbsoluteAddr addr of
            Just absAddr -> pure $ FnConstantValue (addrWidthNatRepr w) (toInteger absAddr)
            Nothing -> pure $ FnValueUnsupported ("Relative addr " ++ show addr) (addrWidthTypeRepr w)

        Just addrRef -> do
          let seg = segoffSegment addrRef
          case () of
            _ | segmentFlags seg `Perm.hasPerm` Perm.execute
              , Just (nm, ft) <- Map.lookup addrRef (rsFunctionArgs s) -> do
                  pure $! FnFunctionEntryValue (resolveX86FunctionType ft) nm

            _ | Map.member addrRef (interpState^.parsedBlocks) -> do
                  let msg = "Do not support functions that reference block addresses."
                  pure $! FnValueUnsupported msg (addrWidthTypeRepr w)

              | segmentFlags seg `Perm.hasPerm` Perm.write -> do
                  pure $! FnGlobalDataAddr addrRef

              -- FIXME: do something more intelligent with rodata?
              | segmentFlags seg `Perm.hasPerm` Perm.read -> do
                  pure $! FnGlobalDataAddr addrRef

              | otherwise -> do
                  pure $! FnValueUnsupported ("segment pointer " ++ show addr) (typeRepr v)

    SymbolValue w sym ->
      pure $ FnValueUnsupported ("Symbol references " ++ show sym) (addrWidthTypeRepr w)
    BoolValue b ->
      pure $ FnConstantBool b

    BVValue w i ->
      pure $ FnConstantValue w i

    AssignedValue asgn -> do
      case MapF.lookup (assignId asgn) assignMap of
        Just rval -> pure rval
        Nothing -> error $ "Encountered uninitialized assignment: " ++ show (assignId asgn) ++ "\n"
          ++ show (MapF.keys assignMap)
    Initial reg -> do
      case MapF.lookup reg (s^.rsCurRegs) of
        Nothing ->
          pure $! FnValueUnsupported ("Initial register " ++ show reg) (typeRepr reg)
        Just (CalleeSaved _) ->
          -- trace ("recoverValue unexpectedly encountered callee saved register: " ++ show reg) $ do
          pure $! FnValueUnsupported ("Initial (callee) register " ++ show reg) (typeRepr reg)
        Just (FnRegValue v' pr) -> do
          case testEquality (widthEqSource pr) (widthEqTarget pr) of
            Just Refl -> do
              pure v'
            Nothing -> do
              bitcast v' pr

------------------------------------------------------------------------
-- recoverRegister

recoverRegister :: HasCallStack
                => RegState X86Reg (Value X86_64 ids)
                -> X86Reg tp
                -> Recover ids (FnValue X86_64 tp)
recoverRegister regs r = do
  recoverValue (regs^. boundValue r)

recoverCallTarget :: HasCallStack
                  => RegState X86Reg (Value X86_64 ids)
                  -> Recover ids (FnValue X86_64 (BVType 64), X86FunTypeInfo)
recoverCallTarget regState = do
  s <- get
  let mem = rsMemory s
  let v = regState ^. boundValue ip_reg
  case v of
    RelocatableValue _ addr
      | Just addrRef <- asSegmentOff mem addr
      , Just (nm, ft) <- Map.lookup addrRef (rsFunctionArgs s) -> do
          pure (FnFunctionEntryValue (resolveX86FunctionType ft) nm, ft)
    _ -> do
      callTgt <- recoverValue v
      pure (callTgt, maximumFunTypeInfo)

------------------------------------------------------------------------
-- recoverStmt

-- | Add statements for the assignment
recoverAssign :: HasCallStack => Assignment X86_64 ids tp -> Recover ids ()
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

recoverWrite :: Value X86_64 ids (BVType 64) -> Value X86_64 ids tp -> Recover ids ()
recoverWrite addr val = do
  r_addr <- recoverValue addr
  r_val  <- recoverValue val
  addFnStmt $ FnWriteMem r_addr r_val

recoverCondWrite :: Value X86_64 ids BoolType
                 -> Value X86_64 ids (BVType 64)
                 -> Value X86_64 ids tp
                 -> MemRepr tp
                 -> Recover ids ()
recoverCondWrite cond addr val memRepr = do
  rCond <- recoverValue cond
  rAddr <- recoverValue addr
  rVal  <- recoverValue val
  addFnStmt $ FnCondWriteMem rCond rAddr rVal memRepr

-- | This should add code as needed to support the statement.
recoverStmt :: Stmt X86_64 ids -> Recover ids ()
recoverStmt s = do
  regs <- use rsCurRegs
  case s of
    AssignStmt asgn -> do
      usedAssigns <- gets rsAssignmentsUsed
      -- Only add assignment if it is used.
      when (Some (assignId asgn) `Set.member` usedAssigns) $ do
        recoverAssign asgn
    WriteMem addr _ val
      | Initial reg <- val
      , Just (CalleeSaved _saved_reg) <- MapF.lookup reg regs -> do
          case asStackAddrOffset addr of
            Just _int_addr_off -> do
              -- modifyCurStack $ recordCalleeSavedWrite int_addr_off saved_reg
              return ()
            Nothing -> do
              addWarning "Could not interpret callee saved offset"
      | otherwise -> do
        recoverWrite addr val
    CondWriteMem cond addr memRepr val
      | Initial reg <- val
      , Just (CalleeSaved _saved_reg) <- MapF.lookup reg regs -> do
          case asStackAddrOffset addr of
            Just _int_addr_off -> do
              -- FIXME: Record something about this.
              -- modifyCurStack $ recordCalleeSavedWrite int_addr_off saved_reg
              return ()
            Nothing -> do
              addWarning "Could not interpret callee saved offset"
              recoverCondWrite cond addr val memRepr
      | otherwise -> do
        recoverCondWrite cond addr val memRepr
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
type RecoveredBlockInfo = Map (MemSegmentOff 64) (FnBlock X86_64)

emptyRecoveredBlockInfo :: RecoveredBlockInfo
emptyRecoveredBlockInfo = Map.empty

addFnBlock :: MemSegmentOff 64 -> FnBlock X86_64 -> RecoveredBlockInfo -> RecoveredBlockInfo
addFnBlock a b info = Map.insert a b info

------------------------------------------------------------------------
-- recoverBlock

bitcast :: FnValue X86_64 i -> WidthEqProof i o -> Recover ids (FnValue X86_64 o)
bitcast x p = evalAssignRhs $ FnEvalApp (Bitcast x p)

-- | Return the value associated with a register after a function call.
getPostCallValue :: MemSegmentOff 64
                 -> RegState X86Reg (Value X86_64 ids)
                     -- ^ Value of registers before syscall
                 -> X86Reg tp
                 -> Recover ids (FnValue X86_64 tp)
getPostCallValue addr regs r = do
  case r of
    _ | Just Refl <- testEquality r sp_reg -> do
        spv <- recoverRegister regs sp_reg
        evalAssignRhs $ FnEvalApp $ BVAdd knownNat spv (FnConstantValue n64 8)

   -- df is 0 after a function call.
    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False

    _ | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister regs r
    _ -> do
      addWarning ("WARNING: Nothing known about register "
                   ++ show r ++ " at " ++ show addr ++ "\n"
                 )
      let nm = "post-call register " ++ show r
      return $! FnValueUnsupported nm (typeRepr r)

-- | Get value for register after a system call.
--
-- This is subtly different to that for function calls.
getPostSyscallValue :: MemSegmentOff 64
                       -- ^ Label of block where we syscall occurred.
                    -> RegState X86Reg (Value X86_64 ids)
                       -- ^ Value of registers before syscall
                    -> X86Reg tp
                    -> Recover ids (FnValue X86_64 tp)
getPostSyscallValue addr proc_state r =
  case r of
    _ | Just Refl <- testEquality r sp_reg -> do
          recoverRegister proc_state sp_reg
      | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister proc_state r

    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False

    _ -> do
      addWarning ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show addr)
      return (FnValueUnsupported ("post-syscall register " ++ show r) (typeRepr r))

mkBlock :: MemSegmentOff 64
        -> [MemSegmentOff 64]
        -- ^ Predecessors for this block
        -> MapF X86Reg FnPhiVar
        -- ^ Map from registers to variable associated with them.
        -> FnTermStmt X86_64
        -> MapF X86Reg (FnValue X86_64)
           -- ^ Map from X86 register to the value associated with that register.
        -> Recover ids (FnBlock X86_64)
mkBlock addr preds phiVars tm regMap = do
  curStmts <- use rsCurStmts
  rsCurStmts .= Seq.empty
  return $! FnBlock { fbLabel = addrBlockLabel addr
                    , fbPrevBlocks = addrBlockLabel <$> preds
                    , fbPhiMap = phiVars
                    , fbStmts  = toList curStmts
                    , fbTerm   = tm
                    , fbRegMap = regMap
                    }

recoverX86TermStmt :: forall ids
                   .  DemandedUseMap
                   -- ^ Map from address to registers that address will read.
                   -> [MemSegmentOff 64]
                   -- ^ Predecessors for this block
                   -> MapF X86Reg FnPhiVar
                   -- ^ Map from registers to variable associated with them.
                   -> MemSegmentOff 64
                   -> X86TermStmt ids
                   -> RegState (ArchReg X86_64) (Value X86_64 ids)
                      -- ^ Register value at start of block
                   -> Maybe (MemSegmentOff 64)
                   -> Recover ids (FnBlock X86_64)
recoverX86TermStmt registerUseMap preds phiVars addr tstmt regs mnext_addr = do
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
            | BVValue _ this_call_no <- regs^.boundValue syscall_num_reg
            , Just (_,_,argtypes) <- Map.lookup (fromInteger this_call_no) (spTypeInfo sysp) =
              take (length argtypes) syscallRegs
            | otherwise =
              syscallRegs

      let rregs = spResultRegisters sysp


      let mkRet :: MapF X86Reg (FnValue X86_64)
                -> Some X86Reg
                -> Recover ids (MapF X86Reg (FnValue X86_64))
          mkRet m (Some r) = do
            rv <- mkReturnVar (typeRepr r)
            return $ MapF.insert r (FnReturn rv) m

      initMap <- foldM mkRet MapF.empty rregs

      -- pull the return variables out of initMap (in order of rregs)
      let getVar :: Maybe (FnValue X86_64 tp) -> FnReturnVar tp
          getVar (Just (FnReturn rv)) = rv
          getVar _ = error "impossible"

      let rets :: [Some FnReturnVar]
          rets = map f rregs
            where f (Some r) = Some $ getVar $ MapF.lookup r initMap

      -- Fold operation to update register map with values persisted across system calls.
      let addPostSyscallValue ::  MapF X86Reg (FnValue X86_64)
            -> Some X86Reg
            -> Recover ids (MapF X86Reg (FnValue X86_64))
          addPostSyscallValue m (Some r) = do
            v <- getPostSyscallValue addr regs r
            return $ MapF.insert r v m

      call_num <- recoverRegister regs syscall_num_reg
      args'  <- mapM (recoverRegister regs) args
      addFnStmt (FnArchStmt (X86FnSystemCall call_num args' rets))
      case mnext_addr of
        Nothing -> do
          -- TODO: Fix this by adding a
          error "Recovery: Could not find system call return label"
        Just next_addr -> do
          let provides = Map.findWithDefault Set.empty next_addr registerUseMap
          regs' <- foldM addPostSyscallValue initMap (provides `Set.difference` Set.fromList rregs)
          mkBlock addr preds phiVars (FnJump (addrBlockLabel next_addr)) regs'

-- | Given a register state this interprets the function arguments.
evalFunctionArgs :: RegState X86Reg (Value X86_64 ids)
                 -> X86FunTypeInfo
                 -> Recover ids [Some (FnValue X86_64)]
evalFunctionArgs regs ft = do
  forM (ftiArgRegs ft) $ \argInfo -> do
    case argInfo of
      ArgBV64 r -> do
        Some <$> recoverRegister regs (X86_GP r)
      ArgMM512D i -> do
        v512 <- recoverRegister regs (X86_ZMMReg i)
        vv <- bitcast v512 (UnpackBits n8 n64)
        Some <$> bitcast vv (VecEqCongruence n8 (ToFloat DoubleFloatRepr))

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
stripPushReturn _addr _next_sp (CondWriteMem{}:_) =
  Left "Conditional memory write before push of return address."
stripPushReturn _addr _ (InstructionStart{}:_) =
  Left "Instruction start before return address."
stripPushReturn addr next_sp (s@Comment{}:l)= do
  (s:) <$> stripPushReturn addr next_sp l
stripPushReturn _addr _ (ExecArchStmt{}:_) =
  Left "Arch statement before push of return address."
stripPushReturn addr next_sp (s@ArchState{}:l) =
  (s:) <$> stripPushReturn addr next_sp l

evalReturnVars :: forall ids
               .  [X86RetInfo]
               -> Recover ids ([Some FnReturnVar], MapF X86Reg (FnValue X86_64))
evalReturnVars retInfo = do
  let mkRetReg :: X86RetInfo
               -> ( [Some FnReturnVar]
                  , MapF X86Reg (FnValue X86_64)
                  )
               -> Recover ids ( [Some FnReturnVar]
                              , MapF X86Reg (FnValue X86_64)
                              )
      mkRetReg ri (vars,m) = do
        aid <- freshId
        Some reg <- pure (retReg ri)
        let v = FnReturnVar { frAssignId = aid
                            , frReturnType = typeRepr reg
                            }
        pure (Some v:vars, MapF.insert reg (FnReturn v) m)
  foldrM mkRetReg ([], MapF.empty) retInfo

recoverBlock :: forall ids
             .  DemandedUseMap
                -- ^ Map from address to registers that address will read.
             -> [MemSegmentOff 64]
             -- ^ Predecessors for this block
             -> MapF X86Reg FnPhiVar
            -- ^ Map from registers to variable associated with them.
             -> ParsedBlock X86_64 ids
                -- ^ Entire block.
             -> RecoveredBlockInfo
             -> Recover ids RecoveredBlockInfo
recoverBlock registerUseMap preds phiVars b blockInfo = seq blockInfo $ do
  let addr = pblockAddr b
  -- Clear stack offsets
  rsCurAddr  .= addr

  -- Recover term statement
  case pblockTermStmt b of
    ParsedCall regs m_ret_addr -> do
      case m_ret_addr of
          -- Case split on whether this is a tail call or not so we can decide which
          -- type of statement to emit.
        -- Add a call, then jump
        Just ret_addr -> do
          -- Recover statements
          let next_sp = regs^.boundValue sp_reg
          case stripPushReturn ret_addr next_sp (reverse (pblockStmts b)) of
            Left e -> error e
            Right l -> traverse_ recoverStmt (reverse l)
          -- Get call target
          (call_tgt, fti) <- recoverCallTarget regs
          -- Evaluate call arguments
          let ftp = resolveX86FunctionType fti
          args <- evalFunctionArgs regs fti
          -- Get list of registers that next block will need to read.
          let nextRegsNeeded = Map.findWithDefault Set.empty ret_addr registerUseMap
          -- Get list of return variables for call, and map from register to value for
          -- needed return values.
          (ri,argRegs) <- evalReturnVars (ftiRetRegs fti)
          -- Add call statement
          addFnStmt (FnCall call_tgt ftp args ri)
          -- Get registers that return address needs.
          -- Get registers for next block.
          regs' <- do
            let go :: MapF X86Reg (FnValue X86_64)
                   -> Some X86Reg
                   -> Recover ids (MapF X86Reg (FnValue X86_64))
                go m (Some r) =
                  case MapF.lookup r argRegs of
                    Just v  ->
                      pure $! MapF.insert r v m
                    Nothing -> do
                      v <- getPostCallValue addr regs r
                      pure $! MapF.insert r v m
            foldM go MapF.empty nextRegsNeeded
          -- Create block that ends with jump to return address.
          fb <- mkBlock addr preds phiVars (FnJump (addrBlockLabel ret_addr)) regs'
          return $! blockInfo & addFnBlock addr fb
        Nothing -> do
          thisFunType <- gets $ rsCurrentFunctionType
          -- Recover statements
          traverse_ recoverStmt (pblockStmts b)
          -- Get call target
          (callTarget, callFunTypeInfo) <- recoverCallTarget regs
          -- Evaluate call arguments
          let callFunType = resolveX86FunctionType callFunTypeInfo
          args <- evalFunctionArgs regs callFunTypeInfo
          (ri,retRegMap) <- evalReturnVars (ftiRetRegs callFunTypeInfo)
          -- Add function call
          addFnStmt (FnCall callTarget callFunType args ri)
          let resolveReg :: X86RetInfo
                         -> Some (FnValue X86_64)
              resolveReg retInfo = do
                case retReg retInfo of
                  Some r ->
                    case MapF.lookup r retRegMap of
                      Just v -> Some v
                      Nothing -> Some (FnUndefined (typeRepr r))
          -- Create block
          let ri2 = resolveReg <$> ftiRetRegs thisFunType
          fb <- mkBlock addr preds phiVars (FnRet ri2) MapF.empty
          return $! blockInfo & addFnBlock addr fb
    PLTStub _ _ _ -> do
      -- Recover statements
      traverse_ recoverStmt (pblockStmts b)
      error $ "Cannot recover functions with PLT stubs."
    ParsedTranslateError{} -> do
      error "Cannot recover function in blocks where translation error occurs."
    ClassifyFailure{} ->
      error $ "Classification failed in Recovery"

    ParsedJump regs tgt_addr -> do
      -- Recover statements
      traverse_ recoverStmt (pblockStmts b)
      -- Recover term statement
      let go :: MapF X86Reg (FnValue X86_64)
             -> Some X86Reg
             -> Recover ids (MapF X86Reg (FnValue X86_64))
          go m (Some r) = do
            v <- recoverRegister regs r
            return $ MapF.insert r v m

      let provides = Map.findWithDefault Set.empty tgt_addr registerUseMap
      regs' <- foldM go MapF.empty provides

      bl <- mkBlock addr preds phiVars (FnJump (addrBlockLabel tgt_addr)) regs'
      pure $! blockInfo & addFnBlock addr bl

    ParsedBranch regs cond trueAddr falseAddr -> do
      -- Recover statements
      traverse_ recoverStmt (pblockStmts b)
      -- Translate condition
      condVal <- recoverValue cond
      -- Get registers that may be needed by one of the two branch targets.
      let provides = (Map.findWithDefault Set.empty trueAddr  registerUseMap)
           `Set.union` (Map.findWithDefault Set.empty falseAddr registerUseMap)
      let go :: MapF X86Reg (FnValue X86_64)
             -> Some X86Reg
             -> Recover ids (MapF X86Reg (FnValue X86_64))
          go m (Some r) = do
            v <- recoverRegister regs r
            return $ MapF.insert r v m
      -- Populate register map
      regs' <- foldM go MapF.empty provides

      let br = FnBranch condVal (addrBlockLabel trueAddr) (addrBlockLabel falseAddr)
      bl <- mkBlock addr preds phiVars br regs'
      pure $! blockInfo & addFnBlock addr bl

    ParsedReturn regs -> do
      -- Recover statements
      traverse_ recoverStmt (pblockStmts b)
      -- Recover term stmt
      fti <- gets $ rsCurrentFunctionType
      let evalRet :: X86RetInfo -> Recover ids (Some (FnValue X86_64))
          evalRet (RetBV64 r) = do
            Some <$> recoverRegister regs (X86_GP r)
          evalRet (RetMM512D i) = do
            x <- recoverRegister regs (X86_ZMMReg i)
            v <- bitcast x (UnpackBits n8 n64)
            Some <$> bitcast v (VecEqCongruence n8 (ToFloat DoubleFloatRepr))
      ri <- traverse evalRet (ftiRetRegs fti)
      bl <- mkBlock addr preds phiVars (FnRet ri) MapF.empty
      pure $! blockInfo & addFnBlock addr bl

    ParsedArchTermStmt ts regs next_addr -> do
      -- Recover statements
      traverse_ recoverStmt (pblockStmts b)
      -- Recover term statement
      fb <- recoverX86TermStmt registerUseMap preds phiVars addr ts regs next_addr
      pure $! blockInfo & addFnBlock addr fb

    ParsedLookupTable regs idx vec -> do
      -- Recover statements
      traverse_ recoverStmt (pblockStmts b)
      -- Recover term statement
      let go :: MapF X86Reg (FnValue X86_64)
             -> Some X86Reg
             -> Recover ids (MapF X86Reg (FnValue X86_64))
          go m (Some r) = do
            v <- recoverRegister regs r
            return $ MapF.insert r v m

      let getRegs next_addr = Map.findWithDefault Set.empty next_addr registerUseMap
      let provides = Set.unions (getRegs <$> V.toList vec)
      regs' <- foldM go MapF.empty provides

      idx'   <- recoverValue idx

      let lblVec = addrBlockLabel <$> vec
      bl <-  mkBlock addr preds phiVars (FnLookupTable idx' lblVec) regs'
      pure $! blockInfo & addFnBlock addr bl

------------------------------------------------------------------------
-- allocateStackFrame
allocateStackFrame :: forall ids
                   . ParsedBlock X86_64 ids
                   -> StackDepthValue X86_64 ids
                   -> Recover ids (FnValue X86_64 (BVType 64))
allocateStackFrame b s = do
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
      remaining_asgns <- foldlM goStmt asgns (pblockStmts b)
      when (not (Set.null remaining_asgns)) $ do
        throwError $ "Found unsupported symbolic stack references: " ++ show remaining_asgns
  let doOneDelta :: StackDepthOffset X86_64 ids
                 -> Recover ids (FnValue X86_64 (BVType 64))
                 -> Recover ids (FnValue X86_64 (BVType 64))
      doOneDelta (Pos _) _   = error "Saw positive stack delta"
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

recoverInnerBlock :: DiscoveryFunInfo X86_64 ids
                  -> DemandedUseMap
                  -> FunPredMap 64
                  -> RecoveredBlockInfo
                  -> MemSegmentOff 64
                  -> Recover ids RecoveredBlockInfo
recoverInnerBlock fInfo blockRegs blockPreds blockInfo addr = do
  let addPhiVar :: MapF X86Reg FnPhiVar
                -> Some X86Reg
                -> Recover ids (MapF X86Reg FnPhiVar)
      addPhiVar m (Some r) = do
        next_id <- freshId
        let v = FnPhiVar { unFnPhiVar = next_id
                         , fnPhiVarType = typeRepr r
                         }
        pure $! MapF.insert r v m

  phiVars <-
    case Map.lookup addr blockRegs of
      Nothing -> do
        addWarning ("WARNING: No regs for " ++ show addr)
        pure MapF.empty
      Just x -> do
        foldM addPhiVar MapF.empty x
  rsCurRegs .= fmapF (\v -> FnRegValue (FnPhiValue v) (WidthEqRefl (fnPhiVarType v))) phiVars
   -- Get predecessors for this block.
  let Just preds = Map.lookup addr blockPreds
   -- Generate phi nodes from predecessors and registers that this block refers to.
  let Just b = Map.lookup addr (fInfo^.parsedBlocks)
  recoverBlock blockRegs preds phiVars b blockInfo

-- | Construct a generatic function type from the x86-specific type rep.
resolveX86FunctionType :: X86FunTypeInfo -> FunctionType X86_64
resolveX86FunctionType ft =
  FunctionType { fnArgTypes = argRegTypeRepr <$> ftiArgRegs ft
               , fnReturnTypes = retRegTypeRepr <$> ftiRetRegs ft
               }

-- | Recover the function at a given address.
--
-- Returns either an error message with a fatal error, or a list of warnings and a function.
recoverFunction :: forall ids
                .  SyscallPersonality
                -> Map (MemSegmentOff 64) (BSC.ByteString, X86FunTypeInfo)
                   -- ^ Map from address to the name at that address along with type.
                -> Memory 64
                -> DiscoveryFunInfo X86_64 ids
                -> Either String ([String], Function X86_64)
recoverFunction sysp funTypeMap mem fInfo = do

  let a = discoveredFunAddr fInfo
  let blockPreds = funBlockPreds fInfo

  (nm,cfti) <-
    case Map.lookup a funTypeMap of
      Just p -> pure p
      Nothing -> Left $ "Missing type for " ++ show a

  let (usedAssigns, blockRegs)
        = registerUse mem sysp (fmap snd funTypeMap) fInfo cfti blockPreds
  -- Note the above is wrong.

  let insArg :: Int
             -> X86ArgInfo
             -> MapF X86Reg (FnRegValue X86_64)
             -> MapF X86Reg (FnRegValue X86_64)
      insArg i (ArgBV64 r) m =
        let tp = BVTypeRepr n64
         in MapF.insert (X86_GP r) (FnRegValue (FnArg i tp) (WidthEqRefl tp)) m
      insArg i (ArgMM512D r) m =
        let itp = VecTypeRepr n8 (BVTypeRepr n64)
            pr = PackBits n8 n64
         in MapF.insert (X86_ZMMReg r) (FnRegValue (FnArg i itp) pr) m

  -- This marks all the registers in the ABI that should save their
  -- value as callee saved
  let insCalleeSaved (Some r) = MapF.insert r (CalleeSaved r)

  let initRegs = MapF.empty
               & flip (ifoldr insArg) (ftiArgRegs cfti)
               & flip (foldr insCalleeSaved) x86CalleeSavedRegs
                 -- Set df to 0 at function start.
               & MapF.insert DF (FnRegValue (FnConstantBool False) (WidthEqRefl (typeRepr DF)))

  let rs = RS { rsMemory        = mem
              , rsInterp = fInfo
              , _rsNextAssignId = FnAssignId 0
              , _rsCurAddr  = a
              , _rsCurStmts  = Seq.empty
              , _rsAssignMap = MapF.empty
              , _rsCurRegs   = initRegs
              , rsSyscallPersonality = sysp
              , rsAssignmentsUsed = usedAssigns
              , rsCurrentFunctionType = cfti
              , rsFunctionArgs = funTypeMap
              , rsWarnings     = []
              }

  evalRecover rs $ do
    -- The first block is special as it needs to allocate space for
    -- the block stack area.  It should also not be in blockPreds (we
    -- assume no recursion/looping through the initial block)

    -- Make the alloca and init rsp.  This is the only reason we
    -- need rsCurStmts

    let Just b = Map.lookup a (fInfo^.parsedBlocks)

    case maximumStackDepth fInfo of
      Right depths ->
        case Set.toList depths of
          [] -> do
            addWarning "WARNING: no stack use detected"
            pure ()
          [s] -> do
            when (not (null (dynamicPart s)) || staticPart s /= 0) $ do
              spTop <- allocateStackFrame b s
              rsCurRegs %= MapF.insert sp_reg (FnRegValue spTop (WidthEqRefl (knownRepr :: TypeRepr (BVType 64))))
          _ -> throwError $ "non-singleton stack depth"
      Left msg -> throwError $ "maximumStackDepth: " ++ msg

    r0 <- recoverBlock blockRegs [] MapF.empty b emptyRecoveredBlockInfo
    rf <- foldM (recoverInnerBlock fInfo blockRegs blockPreds) r0 (Map.keys blockPreds)

    let (Just entry,restBlocks) = Map.updateLookupWithKey (\_ _ -> Nothing) a rf

    pure $! Function { fnAddr = a
                     , fnType = resolveX86FunctionType cfti
                     , fnName = nm
                     , fnEntryBlock = entry
                     , fnRestBlocks = Map.elems restBlocks
                     }
