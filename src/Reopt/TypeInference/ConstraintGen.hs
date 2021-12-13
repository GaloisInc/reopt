{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module provides a constraint generation pass for inferring the
-- types of Macaw programs, inspired by TIE.
--
-- Partially derived from Data.Macaw.Analysis.FunctionArgs

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Reopt.TypeInference.ConstraintGen where

import           Control.Lens
import           Control.Lens.TH                 (makeLenses)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (isJust)

import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some

import           Data.Macaw.Analysis.RegisterUse (BlockInvariants (..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import           Data.Macaw.Types

-- This algorithm proceeds in stages:
-- 1. Give type variables to the arguments to all functions
-- 2. For each function:
--  a. Give type variables to the arguments to all blocks
--  b. Generate constraints per-block

-- Note we need register use information at the function level (to
-- generate function types) and at the block level (basically phi
-- nodes, so we don't unify the types of dead values).

-- ------------------------------------------------------------
-- Core types

type RegType arch = Map (Some (ArchReg arch)) IType

data FunType arch = FunType {
    funArgs :: RegType arch
  , funRets :: RegType arch
}

newtype TyVar = TyVar { getTyVar :: Int }
  deriving (Eq, Ord, Show)

data IType =
  ITyVar TyVar
  | IPtr
  | IBV Int
  deriving Show

-- This is the free type for constraints, should be replace by the
--  type from the constraint solver.

data Constraint =
  CEq IType IType -- ^ The types must be equal
  | CAddrWidthAdd IType IType IType
  -- ^ At most one type may be a ptr, first type is the result
  | CAddrWidthSub IType IType IType
  -- ^ The rhs may not be a ptr if the lhs is a bv, first type is the result
  | CBVNotPtr IType -- ^ The argument cannot be a ptr (so should be a bv)
  | CIsPtr IType    -- ^ The type must point to something
  deriving Show
  
-- -----------------------------------------------------------------------------
-- Monad

-- This tracks e.g. fresh type variable names, along with known
-- types/function types.

newtype Warning = Warning String

data CGenContext arch ids = CGenContext {
  -- Global state
  cgenFunTypes          :: Map (ArchSegmentOff arch) (FunType arch)
  , cgenMemory          :: Memory (ArchAddrWidth arch)
  
  -- Function state
  , cgenCurrentFun :: FunType arch  
  , cgenBlockRegTypes :: Map (ArchSegmentOff arch) (RegType arch)
  
  -- Current block state
  , cgenCurrentBlockRegs  :: RegType arch

}

data CGenState arch ids = CGenState {
    _nextFreeTyVar :: Int -- FIXME: use Nonce?
  , _assignTypes   :: Map (Some (AssignId ids)) IType

  -- | Offset of the current instruction, used (not right now) for
  -- tagging constraints and warnings.
  , _curOffset     :: ArchAddrWord arch 
  , _warnings      :: [Warning]
  , _constraints   :: [Constraint] -- in reverse gen order
}

makeLenses ''CGenState

newtype CGenM arch ids a =
  CGenM { getCGenM :: ReaderT (CGenContext arch ids) (State (CGenState arch ids)) a }
  deriving (Functor, Applicative, Monad)

-- ------------------------------------------------------------
-- Monad operations

-- | Warn about a missing feature (for example).
warn :: String -> CGenM arch ids ()
warn s = CGenM $ warnings <>= Warning s

-- '<<+=' increment var, return old value

-- | Returns a fresh type var.
freshTyVar :: CGenM arch ids TyVar
freshTyVar = CGenM $ TyVar <$> (nextFreeTyVar <<+= 1)

-- | Creates a new type variable and associates it with the assignment
freshForAssignment :: Assignment arch ids tp -> CGenM arch ids TyVar
freshForAssignment a = do
  tyv <- freshTyVar
  -- update assignment type map
  CGenM $ assignTypes . at (Some (assignId a)) ?= ITyVar tyv
  pure tyv

assignmentType :: Assignment arch ids tp -> CGenM arch ids IType
assignmentType a = do
  m_ty <- CGenM $ use (assignTypes . at (Some (assignId a)))
  case m_ty of
    Nothing -> error "Missing assignment type"
    Just ty -> pure ty

initialRegType :: ArchReg arch tp -> CGenM arch ids IType
initialRegType r = do
  m_ty <- CGenM $ asks (Map.lookup (Some r) . cgenCurrentBlockRegs)
  case m_ty of
    Nothing -> error "Missing initial register"
    Just ty -> pure ty

emitConstraint :: Constraint -> CGenM arch ids ()
emitConstraint c = CGenM $ constraints %= (:) c

addrWidth :: CGenM arch ids (NatRepr (ArchAddrWidth arch))
addrWidth = CGenM $ asks (memAddrWidth . cgenMemory)

currentOffset :: CGenM arch ids (ArchAddrWord arch)
currentOffset = CGenM $ use curOffset

setCurrentOffset :: ArchAddrWord arch -> CGenM arch ids ()
setCurrentOffset off = CGenM $ curOffset .= off

currentFunType :: CGenM arch ids (FunType arch)
currentFunType = CGenM $ asks cgenCurrentFun

regTypeForBlock :: ArchSegmentOff arch -> CGenM arch ids (RegType arch)
regTypeForBlock blockAddr = do
  m_rtyp <- CGenM $ asks (Map.lookup blockAddr . cgenBlockRegTypes)
  case m_rtyp of
    Nothing -> error "Missing block type"
    Just rtyp -> pure rtyp

funTypeAtAddr :: Value arch (BVType (ArchAddrWidth addr))
              -> CGenM arch ids (Maybe (FunType arch))
funTypeAtAddr addr = do
  mem    <- CGenM $ asks cgenMemory
  ftypes <- CGenM $ asks cgenFunTypes
  pure (flip Map.lookup ftypes =<< valueAsSegmentOff mem ipVal)

-- -----------------------------------------------------------------------------
-- Imported API

-- The representation of constraints is still being figured out, so
-- these functions abstract over the precise representation.

-- ------------------------------------------------------------
-- Constraints

emitEq :: IType -> IType -> CGenM arch ids ()
emitEq t1 t2 = emitConstraint (CEq t1 t2)

-- | Emits an add which may be a pointer add
emitPtrAdd :: IType -> IType -> IType -> CGenM arch ids ()
emitPtrAdd rty t1 t2 = emitConstraint (CAddrWidthAdd rty t1 t2)

-- | Emits a sub which may return a pointer
emitPtrSub :: IType -> IType -> IType -> CGenM arch ids ()
emitPtrSub rty t1 t2 = emitConstraint (CAddrWidthSub rty t1 t2)

-- | Emits a constraint that the argument isn't a pointer
emitNotPtr :: IType -> CGenM arch ids ()
emitNotPtr t = emitConstraint (CBVNotPtr t)

emitPtr :: IType -> CGenM arch ids ()
emitPtr t = emitConstraint (CIsPtr t)

-- -----------------------------------------------------------------------------
-- Core algorithm

genValue :: Value arch ids tp -> CGenM arch ids IType
genValue v =
  case v of
    CValue {} -> do
      warn "Ignoring constant value"
      ITyVar <$> freshTyVar
    AssignedValue a -> assignmentType a
    Initial r       -> initialRegType r

-- | Generate constraints for an App.  The first argument is the
-- output (result) type.
genApp :: IType -> App (Value arch ids) tp -> CGenM arch ids ()
genApp ty app = 
  case app of
    Eq l r    -> vEq l r
    Mux _ _ l r -> vEq l r

    -- We don't generate any further constraints for boolean ops
    AndApp {} -> pure ()
    OrApp  {} -> pure ()
    NotApp {} -> pure ()
    XorApp {} -> pure ()

    -- We currently ignore tuples
    MkTuple {} -> warn "Ignoring MkTyple"
    TupleField {} -> warn "Ignoring TupleField"

    -- We currently ignore vectors
    ExtractElement {} -> warn "Ignoring ExtractElement"
    InsertElement {}  -> warn "Ignoring InsertElement"

    -- BV size operations
    -- FIXME: for now we assume we can only do these on bitvecs (not pointers)
    Trunc v _ -> nonptrUnOp v
    SExt  v _ -> nonptrUnOp v
    UExt  v _ -> nonptrUnOp v

    -- FIXME: Not sure what to do with these, for now leave underconstrained.
    Bitcast {} -> warn "Ignoring Bitcast"

    -- BV Arithmetic
    --
    -- In general we don't need to do much, as most are just bv ops.
    -- Add and Sub are the main exceptions
    BVAdd sz l r -> do
      addrw <- addrWidth
      when (isJust (testEquality addrw sz))
        (join $ emitPtrAdd ty <$> genValue l <*> genValue r)
      -- non-64 bit adds can't generate a pointer so we don't need to
      -- generate that constraint
      
    -- FIXME: should this be considered another add?
    BVAdc _ l r _c -> nonptrBinOp l r
    
    BVSub sz l r -> do
      addrw <- addrWidth
      when (isJust (testEquality addrw sz))
        (join $ emitPtrSub ty <$> genValue l <*> genValue r)
      
    BVSbb _ l r _ -> nonptrBinOp l r
    BVMul _ l r   -> nonptrBinOp l r

    -- We are allowed to compare pointers, and test their bits 
    BVUnsignedLe {} -> pure ()
    BVUnsignedLt {} -> pure ()
    BVSignedLe {} -> pure ()
    BVSignedLt {} -> pure ()
    BVTestBit {}  -> pure ()

    BVComplement _ v -> nonptrUnOp v
    -- FIXME: this could be a pointer op to mask bits off?
    BVAnd _ l r      -> nonptrBinOp l r
    
    -- FIXME: this could be a pointer op to do add on aligned ptrs?
    BVOr  _ l r      -> nonptrBinOp l r
    BVXor _ l r      -> nonptrBinOp l r
    BVShl _ l r      -> nonptrBinOp l r
    BVShr _ l r      -> nonptrBinOp l r
    BVSar _ l r      -> nonptrBinOp l r

    -- FIXME: e.g. l could be a ptr?  Not sure why you would use this though
    UadcOverflows l r _ -> nonptrBinCmp l r
    SadcOverflows l r _ -> nonptrBinCmp l r
    UsbbOverflows l r _ -> nonptrBinCmp l r
    SsbbOverflows l r _ -> nonptrBinCmp l r

    PopCount _ v     -> nonptrUnOp v
    ReverseBytes _ v -> nonptrUnOp v
    Bsf _ v          -> nonptrUnOp v
    Bsr _ v          -> nonptrUnOp v
  where
    vEq l r = do
      l_ty <- genValue l
      r_ty <- genValue r
      emitEq l_ty r_ty
      emitEq ty l_ty -- r_ty == ty by transitivity, so we don't add it

    nonptrUnOp :: forall n arch ids. Value arch ids (BVType n) -> CGenM arch ids ()
    nonptrUnOp v = do
      emitNotPtr =<< genValue v
      emitNotPtr ty

    -- | The result and arguments have to be bitvecs (i.e., not ptrs).
    -- We don't relate the sizes as that is given by the macaw type at
    -- the moment.
    nonptrBinOp :: forall n m arch ids.
      Value arch ids (BVType n) -> Value arch ids (BVType m) -> CGenM arch ids ()
    nonptrBinOp l r = do
      emitNotPtr =<< genValue l
      emitNotPtr =<< genValue r
      emitNotPtr ty
      
    nonptrBinCmp :: forall n m arch ids.
      Value arch ids (BVType n) -> Value arch ids (BVType m) -> CGenM arch ids ()
    nonptrBinCmp l r = do
      emitNotPtr =<< genValue l
      emitNotPtr =<< genValue r
   
genAssignment :: Assignment arch ids tp -> CGenM arch ids ()
genAssignment a = do
  ty <- ITyVar <$> freshForAssignment a
  case assignRhs a of
    EvalApp app -> genApp ty app
    SetUndefined {} -> pure () -- no constraints generated
    ReadMem ptr _sz -> emitPtr =<< genValue ptr
    CondReadMem _sz _cond ptr def -> do
      emitPtr =<< genValue ptr
      emitEq ty =<< genValue def
      
    EvalArchFn _afn _ty -> warn "ignoring EvalArchFn"

-- The offset argument is used by call term stmts
genStmt :: Stmt arch ids
        -> CGenM arch ids ()
genStmt stmt = do
  -- ctx <- asks $ demandInfoCtx . archDemandInfo
  case stmt of
    AssignStmt a -> do
      genAssignment a
    WriteMem addr _ _v -> do
      emitPtr =<< genValue addr
    CondWriteMem _cond addr _ _v -> do
      emitPtr =<< genValue addr
    InstructionStart off' _ -> setCurrentOffset off'
    Comment _ -> pure ()
    ExecArchStmt _astmt   -> warn "Ignoring ExecArchStmt"
    ArchState _addr _assn -> warn "Ignoring ArchSate"

-- | Matches up the argument registers with the destination.
genRegType :: RegState (ArchReg arch) (Value arch ids)
           -> RegType arch
           -> CGenM arch ids IType
genRegType regs rty = traverse_ go (Map.toList rty)
  where
    go (Some r, ty) = emitEq ty =<< genValue (regs ^. boundValue r)

-- | Used at the end of a block on a control transfer.  In essence
-- this unifies the block arguments with the values from the current block.
genBlockTransfer :: RegState (ArchReg arch) (Value arch ids)
                 -> ArchSegmentOff arch
                 -> CGenM arch ids ()
genBlockTransfer regs tgt = genRegType regs =<< regTypeForBlock tgt

genCall :: RegState (ArchReg arch) (Value arch ids)
        -> Maybe (ArchSegmentOff arch)
        -- ^ Address to return to or `Nothing` for tail call.
        -> CGenM arch ids ()
genCall finalRegs mRetAddr = do
  let ipVal = finalRegs ^. boundValue ip_reg
  m_ftyp <- funTypeAtAddr ipVal
  
  case m_ftyp of
    Nothing -> warn "Couldn't determine target of call"
    Just ftyp -> do
      -- Arguments
      genRegType finalRegs (funArgs ftyp)
      
      -- If the function returns we need to transfer to the next
      -- block, after updating any return registers.
      case mRetAddr of
        -- If this is a tail call we need to unify the return type of
        -- the current function with that of the called function.
        -- Note that the called function may return _more_ values that
        -- the current function does.
        Nothing -> do
          cur_ret <- funRets <$> currentFunType
          let both = Map.elems $ Map.intersectionWith (,) cur_ret (funRets ftyp)
          mapM_ (curry emitEq) both

        -- In the case that we return to a block, we need to transfer
        -- the result registers along with callee-saved registers.
        -- Note that this has already been done when figuring out the
        -- block arguments, so we can just use the registers expected
        -- by the target block.
        Just raddr -> do
          tgtTy <- regTypeForBlock raddr

          -- Preserved
          let tgtTy' = tgtTy `Map.difference` funRets ftyp
          genRegType finalRegs tgtTy'

          -- Return
          let both = Map.elems $ Map.intersectionWith (,) tgtTy (funRets ftyp)
          mapM_ (curry emitEq) both
           
genBlock :: ParsedBlock arch ids -- ^ Current block
         -> BlockInvariants arch ids
         -> CGenM arch ids ()
genBlock b invs = do
  -- let blockAddr = pblockAddr b

  -- Generate constraints (and type vars) for the stmts
  mapM_ genStmt (pblockStmts b)
  case pblockTermStmt b of
    ParsedCall regs mRetAddr -> 
      genCall regs mRetAddr       
      
    PLTStub {} -> warn "Ignoring PLTStub"

    ParsedJump regs tgtAddr -> do
      genBlockTransfer regs tgtAddr

    ParsedBranch regs _cond trueAddr falseAddr -> do
      genBlockTransfer regs trueAddr
      genBlockTransfer regs falseAddr

    ParsedLookupTable _layout regs _lookupIdx tgtV ->
      traverse_ genBlockTransfer tgtV

    ParsedReturn regs -> do
      funTy <- currentFunType
      genRegType regs (funRets funTy)

    ParsedArchTermStmt tstmt regs _nextAddr ->
      warn "Ignoring ParsedArchTermStmt"
    ParsedTranslateError _ ->
      warn "Ignoring ParsedTranslateError"
    ClassifyFailure _ _ ->
      warn "Ignoring ClassifyFailure"


  

