{-|
This module provides methods for constructing functions from the basic
blocks discovered by 'Data.Macaw.Discovery'.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.Recovery
  ( recoverFunction
    -- * X86 type info
  , X86FunTypeInfo(..)
  , X86ArgInfo(..)
  , argReg
  , X86RetInfo(..)
  , retReg
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
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
import qualified Data.Vector as V
import           Data.Word
import qualified Flexdis86 as F
import           GHC.Stack
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import           Text.Printf

import           Data.Macaw.AbsDomain.StackAnalysis
import           Data.Macaw.Analysis.RegisterUse
import           Data.Macaw.CFG
import           Data.Macaw.Discovery.State
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.SyscallInfo
import           Data.Macaw.X86.X86Reg
import           Data.Macaw.X86 (x86DemandContext, x86_64CallParams)

import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86

-- | This identifies how a argument is passed into a function, or
-- a return value is passed out.
data X86ArgInfo where
  ArgBV64 :: !F.Reg64 -> X86ArgInfo
  -- ^ This identifies a 64-bit value passed as a register.
  --
  -- The register should be compatible with the ABI.
  ArgMM512D :: !Word8 -> X86ArgInfo
  -- ^ This identifies one of the zmm registers used as arguments (zmm0-7).

-- | The register types this return value is associated with.
argReg :: X86ArgInfo -> Some X86Reg
argReg (ArgBV64 r) = Some (X86_GP r)
argReg (ArgMM512D i) = Some (X86_ZMMReg i)

-- | This identifies how a return value is passed from a callee to
-- the callee.
data X86RetInfo where
  RetBV64 :: !F.Reg64 -> X86RetInfo
  -- ^ This identifies a 64-bit value returned as a register (RAX/RDX)
  --
  -- The register should be compatible with the ABI.
  RetMM512D :: !Word8 -> X86RetInfo
  -- ^ This identifies one of the two zmm registers used as argument (zmm0/1).

-- | The register types this return value is associated with.
retReg :: X86RetInfo -> Some X86Reg
retReg (RetBV64 r) = Some (X86_GP r)
retReg (RetMM512D i) = Some (X86_ZMMReg i)

-- | This describes the registers and return value of an x86_64 ABI
-- compliant function.
--
-- This representation does not support arguments that spilled on the
-- stack, but this would be a good feature to add.
--
-- It uses a list for arguments so that we can use C headers and
-- ensure the arguments appear in a particular order (e.g. from the
-- binary perspective a function that takes two integers followed by a
-- float is indistinguishable from a function that takes a float
-- followed by two integers.
data X86FunTypeInfo
   = X86FunTypeInfo { ftiArgRegs :: [X86ArgInfo]
                    , ftiRetRegs :: [X86RetInfo]
                    }

-- |  Maximum function type.
maximumFunTypeInfo :: X86FunTypeInfo
maximumFunTypeInfo =
  X86FunTypeInfo { ftiArgRegs = fmap ArgBV64 x86GPPArgumentRegs
                             ++ fmap ArgMM512D [0..7]
                 , ftiRetRegs = fmap RetBV64   [ F.RAX, F.RDX ]
                             ++ fmap RetMM512D [0,1]
                 }

toFunctionTypeRegs :: X86FunTypeInfo -> FunctionTypeRegs X86Reg
toFunctionTypeRegs ftp =
  FunctionTypeRegs { fnArgRegs    = argReg <$> ftiArgRegs ftp
                   , fnReturnRegs = retReg <$> ftiRetRegs ftp
                   }

------------------------------------------------------------------------
-- FnRegValue

data FnRegValue arch tp where
  FnValue :: !(FnValue arch i)
          -> !(WidthEqProof i o)
          -> FnRegValue arch o
     -- ^ A value assigned to a register

instance (ShowF (ArchReg arch), MemWidth (ArchAddrWidth arch)) => Pretty (FnRegValue arch tp) where
  pretty (FnValue v _)    = pretty v

$(pure [])

------------------------------------------------------------------------
-- Function recover monad

-- | Information for function recovery common to a function or module.
data FunRecoverContext ids =
  FRC { frcMemory :: !(Memory 64)
      , frcInterp :: !(DiscoveryFunInfo X86_64 ids)
      , frcSyscallPersonality  :: !SyscallPersonality
        -- ^ System call personality
      , frcFunctionArgs :: !(Map (MemSegmentOff 64) (BSC.ByteString, X86FunTypeInfo))
        -- ^ Maps addresses to their name and type information.
      , frcCurrentFunctionType :: !X86FunTypeInfo
        -- ^ The type of the function being recovered.
      , frcBlockDepMap :: !(Map (MemSegmentOff 64) (BlockInvariants X86_64 ids))
        -- ^ Maps the starting address of blocks to the dependency set
        -- for the block.
      }

-- | State for function recovery common to all blocks in a function.
data FunRecoverState = FRS { frsNextAssignId :: !FnAssignId
                           , frsWarnings :: ![String]
                             -- ^ List of warnings added so far.
                           }

-- | Monad for function recovery
newtype FunRecover ids a =
    FR { runFR :: ReaderT (FunRecoverContext ids) (StateT FunRecoverState (Except String)) a }
  deriving (Functor, Applicative, Monad, MonadError String)

runFunRecover :: FunRecoverContext ids
              -> FunRecover ids a
              -> Either String ([String], a)
runFunRecover ctx m =
  let s0 = FRS { frsNextAssignId = FnAssignId 0
               , frsWarnings = []
               }
   in case runExcept (runStateT (runReaderT (runFR m) ctx) s0) of
        Left e -> Left e
        Right (a,s) -> Right (frsWarnings s, a)

{-
--  | Report a warning
funAddWarning :: String -> FunRecover ids ()
funAddWarning msg = do
  FR $ modify' $ \frs -> frs { frsWarnings = msg : frsWarnings frs }
-}

-- | Create a fresh assign id.
funFreshId :: FunRecover ids FnAssignId
funFreshId = FR $ do
  FnAssignId nextId <- gets $ frsNextAssignId
  modify' $ \frs -> frs { frsNextAssignId = FnAssignId (nextId + 1) }
  return $! FnAssignId nextId

$(pure [])

------------------------------------------------------------------------
-- RecoverState

-- | State used for recovering a block
data RecoverState arch ids =
  RS { rsStartAddr :: !(ArchSegmentOff arch)
       -- ^ Initial block
     , rsBlockInvariants :: !(BlockInvariants arch ids)
       -- ^ Invariants inferred about block
     , rsPredBlockAddrs :: ![ArchSegmentOff arch]
       -- ^ Predecessors for this block
     , rsPhiVarMap :: !(MapF (BoundLoc (ArchReg arch)) (FnRegValue arch))
       -- ^ Maps representative locations to the associated variable.
     , rsBlockOff :: !(ArchAddrWord arch)
       -- ^ The offset in the block of the current code.
     , _rsCurStmts  :: !(Seq (FnStmt arch))
       -- ^ Statements added to block so far
       -- | Maps assignments processed so far to their values
     , _rsAssignMap    :: !(MapF (AssignId ids) (FnValue arch))
       -- | Map the index of processed used memory writes to the
       -- value stored in memory after they execute.
     , rsWriteMap  :: !(Map StmtIndex (Some (FnValue arch)))
       -- | Instruction offset and type of previous accesses
     , rsSeenMemAccessTypes :: ![(Word64, FnMemAccessType)]
       -- | Memory accesses that have not yet been processed.
     , rsPendingMemAccesses :: ![MemAccessInfo arch ids]
     }

-- | List of statements accumulated so far.
rsCurStmts :: Lens' (RecoverState arch ids) (Seq (FnStmt arch))
rsCurStmts = lens _rsCurStmts (\s v -> s { _rsCurStmts = v })

-- | Map from assignments that have been evaluated to the value bound
-- to them.
rsAssignMap :: Lens' (RecoverState arch ids)
                     (MapF (AssignId ids) (FnValue arch))
rsAssignMap = lens _rsAssignMap (\s v -> s { _rsAssignMap = v })

$(pure [])

------------------------------------------------------------------------
-- Recover

-- | Monad for recovering code in a block.
newtype Recover ids a = Recover {
    runRecover :: StateT (RecoverState X86_64 ids) (FunRecover ids) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState (RecoverState X86_64 ids)
           )

liftFunRecover :: FunRecover ids a -> Recover ids a
liftFunRecover m = Recover $ lift m

getFunCtx :: Recover ids (FunRecoverContext ids)
getFunCtx = liftFunRecover $ FR ask

{-
--  | Report a warning
addWarning :: String -> Recover ids ()
addWarning msg = liftFunRecover $ funAddWarning msg
-}

-- | Create a fresh assign id.
freshId :: Recover ids FnAssignId
freshId = liftFunRecover funFreshId

-- | Create a fresh return variable given the type id.
mkReturnVar :: TypeRepr tp -> Recover ids (FnReturnVar tp)
mkReturnVar tp = (`FnReturnVar` tp) <$> freshId

-- | Add a statement to the list of statements in current function block.
addFnStmt :: FnStmt X86_64 -> Recover ids ()
addFnStmt stmt = rsCurStmts %= (Seq.|> stmt)

$(pure [])

------------------------------------------------------------------------
-- X86FunctionType

-- | Type used in passing this argument to a function.
argRegTypeRepr :: X86ArgInfo -> Some TypeRepr
argRegTypeRepr ArgBV64{} = Some (BVTypeRepr n64)
argRegTypeRepr ArgMM512D{} = Some (VecTypeRepr n8 (FloatTypeRepr DoubleFloatRepr))

$(pure [])

-- | The register types this return value is associated with.
retRegTypeRepr :: X86RetInfo -> Some TypeRepr
retRegTypeRepr RetBV64{} = Some (BVTypeRepr n64)
retRegTypeRepr RetMM512D{} = Some (VecTypeRepr n8 (FloatTypeRepr DoubleFloatRepr))

$(pure [])

-- | Construct a generatic function type from the x86-specific type rep.
resolveX86FunctionType :: X86FunTypeInfo -> FunctionType X86_64
resolveX86FunctionType ft =
  FunctionType { fnArgTypes = argRegTypeRepr <$> ftiArgRegs ft
               , fnReturnTypes = retRegTypeRepr <$> ftiRetRegs ft
               }

$(pure [])

------------------------------------------------------------------------
-- evalAssign

-- | This evaluates the right-hand side of an assignment and returns
-- the value.
evalAssignRhs :: FnAssignRhs X86_64 (FnValue X86_64) tp -> Recover ids (FnValue X86_64 tp)
evalAssignRhs rhs = do
  fnAssign <- (`FnAssignment` rhs) <$> freshId
  seq fnAssign $ do
    addFnStmt $ FnAssignStmt fnAssign
    return $ FnAssignedValue fnAssign

$(pure [])

bitcast :: FnValue X86_64 i -> WidthEqProof i o -> Recover ids (FnValue X86_64 o)
bitcast x p = evalAssignRhs $ FnEvalApp (Bitcast x p)

$(pure [])

coerceRegValue :: FnRegValue X86_64 tp -> Recover ids (FnValue X86_64 tp)
coerceRegValue (FnValue v' pr) =
  case testEquality (widthEqSource pr) (widthEqTarget pr) of
    Just Refl -> pure v'
    Nothing -> bitcast v' pr

------------------------------------------------------------------------
-- recoverValue

unsupportedFnValue :: String -> TypeRepr tp -> Recover ids (FnValue X86_64 tp)
unsupportedFnValue nm _repr = do
  throwError $ "Cannot create value: " ++ nm


-- | Recover a constant.
recoverCValue :: HasCallStack
              => CValue X86_64 tp
              -> Recover ids (FnValue X86_64 tp)
recoverCValue cv =
  case cv of
    BVCValue w i ->
      pure $ FnConstantValue w i
    BoolCValue b ->
      pure $ FnConstantBool b
    RelocatableCValue w addr -> do
      funCtx <- getFunCtx
      let mem = frcMemory funCtx
      let interpState = frcInterp funCtx
      case asSegmentOff mem addr of
        Nothing ->
          case asAbsoluteAddr addr of
            Just absAddr -> pure $ FnConstantValue (addrWidthNatRepr w) (toInteger absAddr)
            Nothing -> unsupportedFnValue ("Relative addr " ++ show addr) (addrWidthTypeRepr w)

        Just addrRef -> do
          case () of
            _ | Just (nm, ft) <- Map.lookup addrRef (frcFunctionArgs funCtx) -> do
                  pure $! FnFunctionEntryValue (resolveX86FunctionType ft) nm

            _ | Map.member addrRef (interpState^.parsedBlocks) -> do
                  let msg = "Do not support functions that reference block addresses."
                  unsupportedFnValue msg (addrWidthTypeRepr w)

                -- Turn address into hardcoded constant.
              | Just fixedAddr <- segoffAsAbsoluteAddr addrRef -> do
                  pure $ FnConstantValue n64 (fromIntegral fixedAddr)

              | otherwise -> do
                  unsupportedFnValue ("segment pointer " ++ show addr) (typeRepr cv)

    SymbolCValue w sym ->
      unsupportedFnValue ("Symbol references " ++ show sym) (addrWidthTypeRepr w)

$(pure [])

recoverAssignId :: AssignId ids tp -> Recover ids (FnValue X86_64 tp)
recoverAssignId aid = do
  assignMap <- use rsAssignMap
  case MapF.lookup aid assignMap of
    Just rval -> pure rval
    Nothing ->
      error $ "Encountered uninitialized assignment: " ++ show aid ++ "\n"
              ++ show (MapF.keys assignMap)

-- | Recover a stack value
recoverValue :: HasCallStack
             => Value X86_64 ids tp
             -> Recover ids (FnValue X86_64 tp)
recoverValue v = do
  case v of
    CValue cv ->
      recoverCValue cv
    Initial reg -> do
      s <- get
      case MapF.lookup (RegLoc reg) (rsPhiVarMap s) of
        Nothing ->
          unsupportedFnValue ("Initial register " ++ show reg) (typeRepr reg)
        Just rv ->
          coerceRegValue rv
    AssignedValue asgn ->
      recoverAssignId (assignId asgn)

$(pure [])

------------------------------------------------------------------------
-- recoverRegister

recoverRegister :: HasCallStack
                => RegState X86Reg (Value X86_64 ids)
                -> X86Reg tp
                -> Recover ids (FnValue X86_64 tp)
recoverRegister regs r = do
  recoverValue (regs^. boundValue r)

$(pure [])

------------------------------------------------------------------------
-- recoverCallTarget

recoverCallTarget :: HasCallStack
                  => RegState X86Reg (Value X86_64 ids)
                  -> Recover ids (FnValue X86_64 (BVType 64), X86FunTypeInfo)
recoverCallTarget regState = do
  funCtx <- getFunCtx
  let mem = frcMemory funCtx
  let v = regState ^. boundValue ip_reg
  case v of
    RelocatableValue _ addr
      | Just addrRef <- asSegmentOff mem addr
      , Just (nm, ft) <- Map.lookup addrRef (frcFunctionArgs funCtx) -> do
          pure (FnFunctionEntryValue (resolveX86FunctionType ft) nm, ft)
    _ -> do
      callTgt <- recoverValue v
      pure (callTgt, maximumFunTypeInfo)

------------------------------------------------------------------------
-- recoverStmt

$(pure [])

checkAssignmentUnused :: HasCallStack => AssignId ids tp -> Recover ids ()
checkAssignmentUnused aid = do
  mSeen <- uses rsAssignMap (MapF.lookup aid)
  case mSeen of
    Nothing -> return ()
    Just _ -> error $ "internal: Assign " ++ show aid ++ " already seen"

-- | Associate assign id with the reuslt
setAssignVal :: HasCallStack
             => AssignId ids tp
             -> FnValue X86_64 tp
             -> Recover ids ()
setAssignVal aid rval = do
  checkAssignmentUnused aid
  rsAssignMap %= MapF.insert aid rval

setAssignRhs :: HasCallStack
             => AssignId ids tp
             -> FnAssignRhs X86_64 (FnValue X86_64) tp
             -> Recover ids ()
setAssignRhs aid rhs = do
  rval <- evalAssignRhs rhs
  setAssignVal aid rval

$(pure [])

whenAssignUsed :: AssignId ids tp -> Recover ids () -> Recover ids ()
whenAssignUsed aid m = do
  inv <- gets rsBlockInvariants
  when (biAssignIdUsed aid inv) m

-- | Get information about next memory access
popMemAccessInfo :: Recover ids (MemAccessInfo X86_64 ids)
popMemAccessInfo = do
  s <- get
  case rsPendingMemAccesses s of
    [] -> error "popMemAccessInfo invalid"
    a:r -> do
      put $! s { rsPendingMemAccesses = r }
      pure a

setBlockOff :: MemWord 64 -> Recover ids ()
setBlockOff o = do
  modify $ \s -> s { rsBlockOff = o }

pushMemAccessType :: FnMemAccessType -> Recover ids ()
pushMemAccessType tp = do
  o <- gets $ fromIntegral . rsBlockOff
  modify $ \s -> s { rsSeenMemAccessTypes = (o,tp):rsSeenMemAccessTypes s }

-- | Recover computation needed to assign correct value to assignment
-- identifier.
recoverAssign :: HasCallStack
              => Assignment X86_64 ids tp
              -> Recover ids ()
recoverAssign asgn = do
  let aid = assignId asgn
  case assignRhs asgn of
    EvalApp app -> do
      whenAssignUsed aid $ do
        setAssignRhs aid =<< (FnEvalApp <$> traverseFC recoverValue app)
    SetUndefined tp ->
      whenAssignUsed aid $ do
        setAssignRhs aid (FnSetUndefined tp)
    EvalArchFn f _ -> do
      whenAssignUsed aid $ do
        fval <- traverseFC recoverValue f
        setAssignRhs aid (FnEvalArchFn fval)

    ReadMem addr memRepr -> do
      access <- popMemAccessInfo
      case access of
        NotFrameAccess -> do
          pushMemAccessType HeapAccess
          addrVal <- recoverValue addr
          setAssignRhs aid (FnReadMem addrVal (typeRepr memRepr))
        FrameReadInitAccess _o d -> do
          pushMemAccessType StackAccess
          whenAssignUsed aid $ do
            case d of
              ValueRegUseStackOffset _ -> do
                error "Stack pointer escape."
              FnStartRegister _r -> do
                error "Read callee saved register."
              RegEqualLoc l ->
                case testEquality (typeRepr l) (typeRepr memRepr) of
                  Nothing -> error "Incorrect type"
                  Just Refl -> do
                    m <- gets rsPhiVarMap
                    case MapF.lookup l m of
                      Nothing -> error "Uninitialized phi variable."
                      Just rv -> do
                        v <- coerceRegValue rv
                        setAssignVal aid v
        FrameReadWriteAccess _o writeIdx -> do
          pushMemAccessType StackAccess
          whenAssignUsed aid $ do
            m <- gets rsWriteMap
            case Map.lookup writeIdx m of
              Nothing -> error "Could not find value."
              Just (Some v) ->
                case testEquality (typeRepr v) (typeRepr memRepr) of
                  Just Refl ->
                    setAssignVal aid v
                  Nothing ->
                    error "Invalid type for write access."
        FrameReadOverlapAccess _ -> do
          pushMemAccessType StackAccess
          whenAssignUsed aid $ do
            error "Stack read at an overlapping offset."
        FrameWriteAccess{} -> error "Expected read access"
        FrameCondWriteAccess{} -> error "Expected read access"
        FrameCondWriteOverlapAccess{} -> error "Expected read access"
    CondReadMem _tp _cond _addr _def -> do
      error "Conditional reads are not yet supported."

$(pure [])

whenWriteUsed :: StmtIndex -> Recover ids () -> Recover ids ()
whenWriteUsed idx m = do
  inv <- gets rsBlockInvariants
  when (biWriteUsed idx inv) m

-- | This should add code as needed to support the statement.
recoverStmt :: StmtIndex -- ^ Index of statement
            -> Stmt X86_64 ids
            -> Recover ids ()
recoverStmt stmtIdx stmt = do
  case stmt of
    AssignStmt asgn -> do
      recoverAssign asgn
    WriteMem addr _memRepr val -> do
      ainfo <- popMemAccessInfo
      case ainfo of
        NotFrameAccess -> do
          pushMemAccessType HeapAccess
          rAddr <- recoverValue addr
          rVal  <- recoverValue val
          addFnStmt $ FnWriteMem rAddr rVal
        FrameReadInitAccess{} -> error "Expected write access"
        FrameReadWriteAccess{} -> error "Expected write access"
        FrameReadOverlapAccess{} -> error "Expected write access"
        FrameCondWriteAccess{} -> error "Expected write access"
        FrameCondWriteOverlapAccess{} -> error "Expected write access"
        FrameWriteAccess _o -> do
          pushMemAccessType StackAccess
          whenWriteUsed stmtIdx $ do
            rval <- recoverValue val
            modify $ \s -> s { rsWriteMap = Map.insert stmtIdx (Some rval) (rsWriteMap s) }

    CondWriteMem _cond _addr _memRepr _val -> do
      error $ "Conditional writes are not yet supported."
    Comment msg -> do
      addFnStmt $ FnComment msg
    ExecArchStmt astmt0 -> do
      -- Architecture-specific statements are assumed to always
      -- have side effects.
      astmt <- traverseF recoverValue astmt0
      addFnStmt (FnArchStmt (X86FnStmt astmt))
    InstructionStart o _ -> do
      setBlockOff o
    ArchState _ _ -> do
      pure ()

------------------------------------------------------------------------
-- Jump target

-- | Resolve a phi value from an inferred value.
resolveInferValue :: HasCallStack
                  => InferValue X86_64 ids tp
                  -> Recover ids (Some (FnValue X86_64))
resolveInferValue phiVal =
  case phiVal of
    IVDomain d ->
      case d of
        ValueRegUseStackOffset _ -> throwError "Stack offset escaped."
        FnStartRegister _ -> throwError "Callee-saved register escaped."
        RegEqualLoc l -> do
          m <- gets rsPhiVarMap
          case MapF.lookup l m of
            Nothing -> do
              addr <- gets rsStartAddr
              error $ printf "%s. Missing value for %s." (show addr) (show (pretty l))
            Just rv -> Some <$> coerceRegValue rv
    IVAssignValue aid -> do
      Some <$> recoverAssignId aid
    IVCValue cv -> Some <$> recoverCValue cv
    IVCondWrite idx repr  -> do
      m <- gets rsWriteMap
      case Map.lookup idx m of
        Nothing -> error "Could not find write value."
        Just (Some v) ->
          case testEquality (typeRepr repr) (typeRepr v) of
            Nothing -> error "Invalid type"
            Just Refl -> pure (Some v)

-- | Get the phi variable information for the block that starts at the
-- given address.
getBlockInvariants :: MemSegmentOff 64
                   -> FunRecover ids (BlockInvariants X86_64 ids)
getBlockInvariants addr = do
  ctx <- FR ask
  let emsg = "internal: Could not find phi variables for " ++ show addr ++ "."
  pure $! Map.findWithDefault (error emsg) addr (frcBlockDepMap ctx)

-- | Given an address to jump to (which should be the start of a
-- block), this generates a jump target with both the address and phi
-- values to initialize the phi variables in the target.
recoverJumpTarget :: forall ids
                  .  HasCallStack
                  => MapF X86Reg (FnValue X86_64)
                  -> MemSegmentOff 64 -- ^ Address to jump to
                  -> Recover ids (FnJumpTarget X86_64)
recoverJumpTarget retVarMap tgtAddr = do
  thisAddr <- gets $ rsStartAddr
  -- Get invariant info for target address.
  tgtInv <- liftFunRecover $ getBlockInvariants tgtAddr
  let postValues =
        let emsg = "Could not find post values for target."
         in Map.findWithDefault (error emsg) thisAddr (biPredPostValues tgtInv)
  let recoverVec :: Some (BoundLoc X86Reg)
                 -> Recover ids (Some (FnValue X86_64))
      recoverVec (Some (RegLoc r))
        | Just v <- MapF.lookup r retVarMap =
            pure (Some v)
      recoverVec (Some l) = resolveInferValue (pvmFind l postValues)
  values <- traverse recoverVec (V.fromList (biPhiLocs tgtInv))
  pure $! FnJumpTarget { fnJumpLabel = fnBlockLabelFromAddr tgtAddr
                       , fnJumpPhiValues = values
                       }

$(pure [])

------------------------------------------------------------------------
-- RecoveredBlockInfo

-- | Builds a mapping from labels to recovered function blocks and a
-- mapping from the block label to the register map resulting from
-- block.
type RecoveredBlockInfo = Map (MemSegmentOff 64) (FnBlock X86_64)

emptyRecoveredBlockInfo :: RecoveredBlockInfo
emptyRecoveredBlockInfo = Map.empty

addFnBlock :: FnBlock X86_64
           -> RecoveredBlockInfo
           -> RecoveredBlockInfo
addFnBlock b info =
  let a = fnBlockLabelAddr (fbLabel b)
   in Map.insert a b info

$(pure [])

------------------------------------------------------------------------
-- recoverBlock

{-
-- | Return the value associated with a register after a function call.
getPostCallValue :: RegState X86Reg (Value X86_64 ids)
                     -- ^ Value of registers before syscall
                 -> MapF X86Reg (FnValue X86_64)
                    -- ^ Return values
                 -> X86Reg tp
                 -> Recover ids (FnValue X86_64 tp)
getPostCallValue regs retRegMap r = do
  case r of
    _ | Just v <- MapF.lookup r retRegMap -> do
          pure v
    _ | Just Refl <- testEquality r sp_reg -> do
          spv <- recoverRegister regs sp_reg
          evalAssignRhs $ FnEvalApp $ BVAdd knownNat spv (FnConstantValue n64 8)

   -- df is 0 after a function call.
    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False
    -- Callee-saved registers are preserved.
    _ | Some r `Set.member` x86CalleeS*aavedRegs ->
        recoverRegister regs r
    _ -> do
      addr <- gets $ rsStartAddr
      addWarning ("WARNING: Nothing known about register "
                   ++ show r ++ " at " ++ show addr ++ "\n"
                 )
      let nm = "post-call register " ++ show r
      unsupportedFnValue nm (typeRepr r)
-}

$(pure [])

{-
-- | Get value for register after a system call.
--
-- This is subtly different to that for function calls.
getPostSyscallValue :: RegState X86Reg (Value X86_64 ids)
                       -- ^ Value of registers before syscall
                    -> X86Reg tp
                    -> Recover ids (FnValue X86_64 tp)
getPostSyscallValue regs r =
  case r of
    _ | Just Refl <- testEquality r sp_reg -> do
          recoverRegister regs sp_reg
      | Some r `Set.member` x86CalleeSavedRegs ->
        recoverRegister regs r

    _ | Just Refl <- testEquality r DF -> return $ FnConstantBool False

    _ -> do
      addr <- gets $ rsStartAddr
      addWarning ("WARNING: Nothing known about register " ++ show r ++ " at " ++ show addr)
      unsupportedFnValue ("post-syscall register " ++ show r) (typeRepr r)
-}

recoverX86TermStmt :: forall ids
                   .  X86TermStmt ids
                   -> RegState (ArchReg X86_64) (Value X86_64 ids)
                      -- ^ Register value at start of block
                   -> Maybe (MemSegmentOff 64)
                   -> Recover ids (FnTermStmt X86_64)
recoverX86TermStmt tstmt regs mnext_addr =
  case tstmt of
    Hlt ->
      throwError "hlt is not supported in function recovery."
    UD2 -> do
      throwError "ud2 is not supported in function recovery."
    X86Syscall -> do
      sysp <- frcSyscallPersonality <$> getFunCtx

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

      call_num <- recoverValue (regs^.boundValue syscall_num_reg)
      args'  <- mapM (recoverRegister regs) args
      addFnStmt (FnArchStmt (X86FnSystemCall call_num args' rets))
      case mnext_addr of
        Nothing -> do
          -- TODO: Fix this by adding a
          error "Recovery: Could not find system call return label"
        Just nextAddr -> do
          FnJump <$> recoverJumpTarget MapF.empty nextAddr

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

-- | This contains the list of return variables for a call and a map from registers
-- to value returned.
--
-- This does not support stack return values.
type ReturnInfo arch = ([Some FnReturnVar], MapF (ArchReg arch) (FnValue arch))

-- | Analyze list of return values to create information needed to
--  resolve return.
evalReturnVars :: forall ids
               .  [X86RetInfo]
               -> Recover ids (ReturnInfo X86_64)
evalReturnVars retInfo = do
  let mkRetReg :: X86RetInfo
               -> ReturnInfo X86_64
               -> Recover ids (ReturnInfo X86_64)
      mkRetReg ri (vars,m) = do
        aid <- freshId
        Some reg <- pure (retReg ri)
        let v = FnReturnVar { frAssignId = aid
                            , frReturnType = typeRepr reg
                            }
        pure (Some v:vars, MapF.insert reg (FnReturn v) m)
  foldrM mkRetReg ([], MapF.empty) retInfo

$(pure [])

recoverStmts :: StmtIndex -> [Stmt X86_64 ids] -> Recover ids ()
recoverStmts _ [] = pure ()
recoverStmts stmtIdx (n:r) = do
  recoverStmt stmtIdx n
  recoverStmts (stmtIdx+1) r

$(pure [])

-- | Generate FnBlock fro mparsed block
recoverBlock :: forall ids
             .  ParsedBlock X86_64 ids
                -- ^ Entire block.
             -> Recover ids (FnTermStmt X86_64)
recoverBlock b = do
  -- Recover statements
  --
  -- Note that this will process the write of the return address
  -- of the stack, but this should not count as a demanded value
  -- and not affect the LLVM.  However, it is needed to ensure
  -- annotations are generated for all memory events.
  recoverStmts 0 (pblockStmts b)
  -- Block recovery may need to strip statements, so we case split on terminal statement.
  case pblockTermStmt b of
    -- Case split on whether this is a tail call or not so we can decide which
    -- type of statement to emit.
    ParsedCall regs (Just retAddr) -> do
      -- Get call target
      (call_tgt, fti) <- recoverCallTarget regs
      -- Evaluate call arguments
      let ftp = resolveX86FunctionType fti
      args <- evalFunctionArgs regs fti
      -- Get list of return variables for call, and map from register to value for
      -- needed return values.
      (ri, retRegMap) <- evalReturnVars (ftiRetRegs fti)
      -- Add call statement
      addFnStmt (FnCall call_tgt ftp args ri)
      -- Create block that ends with jump to return address.
      FnJump <$> recoverJumpTarget retRegMap retAddr
    -- Handle tail call.
    ParsedCall regs Nothing -> do
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
          resolveReg retInfo =
            case retReg retInfo of
              Some r ->
                case MapF.lookup r retRegMap of
                  Just v -> Some v
                  Nothing -> Some (FnUndefined (typeRepr r))
      -- Create block
      thisFunType <- frcCurrentFunctionType <$> getFunCtx
      let ri2 = resolveReg <$> ftiRetRegs thisFunType
      pure (FnRet ri2)
    PLTStub _ _ _ -> do
      error $ "Cannot recover functions with PLT stubs."
    ParsedTranslateError{} -> do
      error "Cannot recover function in blocks where translation error occurs."
    ClassifyFailure{} ->
      error $ "Classification failed in Recovery"

    ParsedJump _regs tgtAddr -> do
      -- Get target
      tgt  <- recoverJumpTarget MapF.empty tgtAddr
      pure (FnJump tgt)

    ParsedBranch _regs cond trueAddr falseAddr -> do
      -- Translate condition
      condVal <- recoverValue cond
      -- Get registers that may be needed by one of the two branch targets.
      trueTgt  <- recoverJumpTarget MapF.empty trueAddr
      falseTgt <- recoverJumpTarget MapF.empty falseAddr
      pure (FnBranch condVal trueTgt falseTgt)

    ParsedLookupTable _regs idx vec -> do
      -- Recover term statement
      idx'   <- recoverValue idx
      tgtVec <- traverse (recoverJumpTarget MapF.empty) vec
      pure (FnLookupTable idx' tgtVec)

    ParsedReturn regs -> do
      -- Recover term stmt
      fti <- frcCurrentFunctionType <$> getFunCtx
      let evalRet :: X86RetInfo -> Recover ids (Some (FnValue X86_64))
          evalRet (RetBV64 r) = do
            Some <$> recoverRegister regs (X86_GP r)
          evalRet (RetMM512D i) = do
            x <- recoverRegister regs (X86_ZMMReg i)
            v <- bitcast x (UnpackBits n8 n64)
            Some <$> bitcast v (VecEqCongruence n8 (ToFloat DoubleFloatRepr))
      ri <- traverse evalRet (ftiRetRegs fti)
      pure (FnRet ri)

    ParsedArchTermStmt ts regs nextAddr -> do
      -- Recover term statement
      recoverX86TermStmt ts regs nextAddr

$(pure [])

evalRecover :: forall ids
            .  ParsedBlock X86_64 ids
            -> BlockInvariants X86_64 ids
               -- ^ Invariants for block
            -> [MemSegmentOff 64]
               -- ^ Predecessors of current
            -> V.Vector (Some (FnPhiVar X86_64))
            -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
               -- ^ Map from locations to values.
            -> FunRecover ids (FnBlock X86_64)
evalRecover b inv preds phiVars varMap = do
  let s0 = RS { rsStartAddr = pblockAddr b
              , rsBlockInvariants = inv
              , rsPredBlockAddrs = preds
              , rsPhiVarMap = varMap
              , rsBlockOff = 0
              , _rsCurStmts = Seq.empty
              , _rsAssignMap = MapF.empty
              , rsWriteMap = Map.empty
              , rsSeenMemAccessTypes = []
              , rsPendingMemAccesses = biMemAccessList inv
              }

  pr <- case pblockPrecond b of
          Left _e -> throwError "Could not resolve block precondition."
          Right pr -> pure pr

  (tm, s) <- runStateT (runRecover (recoverBlock b)) s0

  return $! FnBlock { fbLabel = fnBlockLabelFromAddr (pblockAddr b)
                    , fbPrecond = pr
                    , fbSize  = fromIntegral (blockSize b)
                    , fbPrevBlocks = fnBlockLabelFromAddr <$> preds
                    , fbInvariants = biInvariantList inv
                    , fbPhiVars = phiVars
                    , fbStmts  = toList (s^.rsCurStmts)
                    , fbTerm   = tm
                    , fbMemInsnAddrs = V.fromList (reverse (rsSeenMemAccessTypes s))
                    }

$(pure [])

------------------------------------------------------------------------
-- recoverFunction


-- | Introduce a new phi var for a class, and updae location map.
addPhiVarForClass :: BlockInvariants X86_64 ids
                  -> ([Some (FnPhiVar X86_64)], MapF (BoundLoc X86Reg) (FnRegValue X86_64))
                     -- ^ Previously constructed location map.
                  -> Some (BoundLoc X86Reg)
                     -- ^ Class to add
                  -> FunRecover ids ([Some (FnPhiVar X86_64)], MapF (BoundLoc X86Reg) (FnRegValue X86_64))
addPhiVarForClass inv (vl,m0) (Some phiLoc) = do
  vnm <- funFreshId
  let tp = typeRepr phiLoc
  let ll = llValues (MapF.findWithDefault (LL []) phiLoc (biLocMap inv))
  let phiVar = FnPhiVar { unFnPhiVar = vnm
                        , fnPhiVarType = tp
                        , fnPhiVarRep  = phiLoc
                        , fnPhiVarLocations = ll
                        }
  let pr = WidthEqRefl tp
  let v = FnValue (FnPhiValue phiVar) pr
  let isReg (RegLoc _) = True
      isReg (StackOffLoc _ _) = False
  let ins m l = MapF.insert l v m
  let m' = foldl' ins m0 (phiLoc : filter isReg ll)
  seq m' $ pure (Some phiVar:vl, m')

recoverInnerBlock :: RecoveredBlockInfo
                  -> MemSegmentOff 64
                  -> FunRecover ids RecoveredBlockInfo
recoverInnerBlock blockInfo addr = do
  fInfo <- frcInterp <$> FR ask
  inv <- getBlockInvariants addr
  (phiVars, locMap) <- foldlM (addPhiVarForClass inv) ([],MapF.empty) (biPhiLocs inv)
   -- Get predecessors for this block.
  let Just preds = Map.lookup addr (funBlockPreds fInfo)
   -- Generate phi nodes from predecessors and registers that this block refers to.
  let Just b = Map.lookup addr (fInfo^.parsedBlocks)
  let phiVarVec = V.fromList (reverse phiVars)
  fb <- evalRecover b inv preds phiVarVec locMap
  return $! blockInfo & addFnBlock fb

$(pure [])

{-

-- | Maps Initial state used to build class vector.
type EqClassState r = MapF (BoundLoc r) (BlockEqClass r)

eqClassVec :: EqClassState r -> BlockEqClassVec r
eqClassVec m = V.fromList (toListF Some m)

emptyClassState :: EqClassState r
emptyClassState = MapF.empty

-- | Adds location to equivalence state.
addDemandedLocation :: ( MemWidth (ArchAddrWidth arch)
                       , OrdF (ArchReg arch)
                       , HasRepr (ArchReg arch) TypeRepr
                       )
                    => BlockStartConstraints arch
                    -> EqClassState (ArchReg arch)
                    -> Some (BoundLoc (ArchReg arch))
                    -> EqClassState (ArchReg arch)
addDemandedLocation cns m (Some loc) =
  case blockLocRepAndCns cns loc of
    (_,Just _) -> m
    (rep,Nothing) ->
      let f _ old = addLocToClass loc old
       in MapF.insertWith f rep (initClass loc) m

-- | This cross references the locations demanded by a block with the
-- inferred equivalences between locations to generate a vector
-- describing each Phi nodes a block will use for inputs.
eqClassVecFromDeps :: ( MemWidth (ArchAddrWidth arch)
                      , OrdF (ArchReg arch)
                      , HasRepr (ArchReg arch) TypeRepr
                      )
                   => Map (MemSegmentOff (ArchAddrWidth arch))
                          (BlockInvariants arch ids)
                      -- ^ Map from block addresses to dependencies.
                   -> ParsedBlock arch ids
                      -- ^ block address
                   -> BlockEqClassVec (ArchReg arch)
eqClassVecFromDeps invMap b =
  -- Lookup dependencies for block we are jumping to.
  case Map.lookup (pblockAddr b) invMap of
    Nothing ->
      error $ "No dependencies for " ++ show (pblockAddr b) ++ "."
    Just inv ->
      let bnds = intBndsMap (blockJumpBounds b)
       in eqClassVec $
            foldl (addDemandedLocation (initBndsMap bnds)) emptyClassState (invDemandedLocSet inv)
-}

x86TermStmtNext :: StartInferContext X86_64
                -> InferState X86_64 ids
                -> X86TermStmt ids
                -> RegState X86Reg (Value X86_64 ids)
                -> Either (RegisterUseError X86_64) (PostValueMap X86_64 ids, BlockStartConstraints X86_64)
x86TermStmtNext cns s X86Syscall regs =
  postCallConstraints x86_64CallParams cns s regs
x86TermStmtNext _ _ Hlt _ = error "Hlt has no successor."
x86TermStmtNext _ _ UD2 _ = error "UD2 has no successor."

x86TermStmtUsage :: SyscallPersonality
                 -> X86TermStmt ids
                 -> RegState (ArchReg X86_64) (Value X86_64 ids)
                 -> BlockUsageSummary X86_64 ids
                 -> Either (RegisterUseError X86_64) (RegDependencyMap X86_64 ids)
x86TermStmtUsage sysp X86Syscall regs s = do
  let cns = blockUsageStartConstraints s
      cache = assignDeps s
      setSavedReg m (Some r) = setRegDep r (valueDeps cns cache (regs^.boundValue r)) m
      clearReg m (Some r) = setRegDep r mempty m
      m0 = setSavedReg mempty (Some sp_reg)
      m1 = foldl' setSavedReg m0 x86CalleeSavedRegs
      m2 = foldl clearReg m1 (spResultRegisters sysp)
   in Right m2
x86TermStmtUsage _ Hlt _ _ = pure mempty
x86TermStmtUsage _ UD2 _ _ = pure mempty

-- | Recover the function at a given address.
--
-- Returns either an error message with a fatal error, or a list of
-- warnings and a function.
recoverFunction :: forall ids
                .  SyscallPersonality
                -> Map (MemSegmentOff 64) (BSC.ByteString, X86FunTypeInfo)
                   -- ^ Map from address to the name at that address along with type.
                -> Memory 64
                -> DiscoveryFunInfo X86_64 ids
                -> Either String ([String], Function X86_64)
recoverFunction sysp funTypeMap mem fInfo = do
  -- Get address of function entry point
  let entryAddr = discoveredFunAddr fInfo

  -- Get name and type information inferred about fthis function.
  (nm,cfti) <-
    case Map.lookup entryAddr funTypeMap of
      Just p -> pure p
      Nothing -> Left $ "Missing type for " ++ show entryAddr ++ "."

  -- Get map from block start addresses to start addresses of blocks
  -- that jump to them.
  let blockPreds = funBlockPreds fInfo

  -- Compute map from block starting addresses to the dependicies required to run block.
  let useCtx = RegisterUseContext
               { archCallParams = x86_64CallParams
               , archPostTermStmtInvariants = x86TermStmtNext
               , defaultCallRegs = toFunctionTypeRegs maximumFunTypeInfo
               , calleeSavedRegisters = [ Some RBP
                                        , Some RBX
                                        , Some R12
                                        , Some R13
                                        , Some R14
                                        , Some R15
                                        ]
               , callScratchRegisters = []
               , returnRegisters = retReg <$> ftiRetRegs cfti
               , reguseTermFn = x86TermStmtUsage sysp
               , functionArgFn = \a -> do
                   segOff <- asSegmentOff mem a
                   toFunctionTypeRegs . snd <$> Map.lookup segOff funTypeMap
               , demandContext = x86DemandContext
               }
  invMap <-
    case runExcept (registerUse useCtx fInfo blockPreds) of
      Left e -> throwError (show e)
      Right v -> pure v

  let funCtx = FRC { frcMemory = mem
                   , frcInterp = fInfo
                   , frcSyscallPersonality = sysp
                   , frcCurrentFunctionType = cfti
                   , frcFunctionArgs = funTypeMap
                   , frcBlockDepMap = invMap
                   }
  runFunRecover funCtx $ do
    let Just b = Map.lookup entryAddr (fInfo^.parsedBlocks)

    let insArg :: Int
               -> X86ArgInfo
               -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
               -> MapF (BoundLoc X86Reg) (FnRegValue X86_64)
        insArg i (ArgBV64 r) m =
          let tp = BVTypeRepr n64
           in MapF.insert (RegLoc (X86_GP r)) (FnValue (FnArg i tp) (WidthEqRefl tp)) m
        insArg i (ArgMM512D r) m =
          let itp = VecTypeRepr n8 (BVTypeRepr n64)
              pr = PackBits n8 n64
           in MapF.insert (RegLoc (X86_ZMMReg r)) (FnValue (FnArg i itp) pr) m

    -- Compute registers for first block
    let initRegs :: MapF (BoundLoc X86Reg) (FnRegValue X86_64)
        initRegs
          = MapF.empty
          & flip (ifoldr insArg) (ftiArgRegs cfti)
          -- Set df to 0 at function start.
          & MapF.insert (RegLoc DF) (FnValue (FnConstantBool False) (WidthEqRefl (typeRepr DF)))

    entryInv <- getBlockInvariants entryAddr
    fb <- evalRecover b entryInv [] V.empty initRegs
    let r0 = emptyRecoveredBlockInfo & addFnBlock fb

    rf <- foldM recoverInnerBlock r0 (Map.keys (funBlockPreds fInfo))

    let (Just entry,restBlocks) =
          Map.updateLookupWithKey (\_ _ -> Nothing) entryAddr rf

    pure $! Function { fnAddr = entryAddr
                     , fnType = resolveX86FunctionType cfti
                     , fnName = nm
                     , fnEntryBlock = entry
                     , fnRestBlocks = Map.elems restBlocks
                     }
