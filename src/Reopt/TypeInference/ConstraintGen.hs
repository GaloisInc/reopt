{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides a constraint generation pass for inferring the types
-- of Reopt (FnRep) programs, inspired by TIE.
module Reopt.TypeInference.ConstraintGen (
  FunctionTypeTyVars (..),
  Ty,
  FTy,
  ModuleConstraints (..),
  genModuleConstraints,
  showInferredTypes,
) where

import Control.Lens ((<>=), (?=), (^?))
import Control.Lens qualified as L
import Control.Monad (join, mapAndUnzipM, zipWithM_)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Bits (testBit)
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Traversable (for)
import Data.Vector qualified as V
import Prettyprinter qualified as PP

import Data.Macaw.CFG (
  App (..),
  ArchAddrWidth,
  ArchFn,
  ArchSegmentOff,
 )
import Data.Macaw.Memory (
  MemSegment,
  MemSegmentOff (..),
  Memory,
  absoluteAddr,
  addrWidthClass,
  asSegmentOff,
  memAddrWidth,
  memSegments,
  memWidth,
  memWord,
  resolveAbsoluteAddr,
 )
import Data.Macaw.Types (
  BVType,
  TypeRepr (..),
  floatInfoBits,
  typeRepr,
 )
import Data.Parameterized (FoldableF, FoldableFC)
import Data.Parameterized.NatRepr (
  NatRepr,
  intValue,
  testEquality,
  widthVal,
 )
import Data.Parameterized.Some (Some (Some), viewSome)
import Reopt.CFG.FnRep (
  FnArchConstraints,
  FnArchStmt,
  FnAssignId,
  FnAssignRhs (..),
  FnAssignment (..),
  FnBlock (..),
  FnBlockLabel,
  FnJumpTarget (..),
  FnPhiVar (unFnPhiVar),
  FnReturnVar (frAssignId),
  FnStmt (..),
  FnTermStmt (FnBranch, FnJump, FnLookupTable, FnRet, FnTailCall),
  FnValue (..),
  Function (..),
  FunctionDecl (..),
  FunctionType (..),
  RecoveredModule (..),
  fnBlocks,
 )
import Reopt.TypeInference.Solver (
  ConstraintSolution (..),
  FTy,
  OperandClass (OCOffset, OCPointer, OCSymbolic),
  RowVar,
  SolverM,
  StructName,
  Ty,
  TyVar,
  eqTC,
  isNumTC,
  numTy,
  ptrAddTC,
  ptrSubTC,
  ptrTC,
  runSolverM,
  subTypeTC,
  unifyConstraints,
  varTy,
 )
import Reopt.TypeInference.Solver qualified as S
import Reopt.TypeInference.Solver.Constraints (
  ConstraintProvenance (..),
  FnRepProvenance (..),
 )

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

data FunctionTypeTyVars = FunctionTypeTyVars
  { fttvArgs :: [TyVar]
  , fttvRet :: Maybe TyVar
  }

instance Show FunctionTypeTyVars where
  show ft =
    "("
      ++ intercalate ", " (map show (fttvArgs ft))
      ++ ") -> "
      ++ maybe "_|_" show (fttvRet ft)

-- -----------------------------------------------------------------------------
-- Monad

-- This tracks e.g. fresh type variable names, along with known
-- types/function types.

newtype Warning = Warning String

instance Show Warning where
  show (Warning w) = w

-- | Context available throughout all of the constraint generation
data CGenGlobalContext arch = CGenGlobalContext
  { _cgenMemory :: Memory (ArchAddrWidth arch)
  , _cgenMemoryRegions :: Map (MemSegment (ArchAddrWidth arch)) RowVar
  -- ^ The map from memory segments to their row types.
  }

L.makeLenses ''CGenGlobalContext

-- | Context available when generating constraints for a given module
data CGenModuleContext arch = CGenModuleContext
  { _cgenFunTypes :: Map (ArchSegmentOff arch) FunctionTypeTyVars
  , _cgenNamedFunTypes :: Map BSC.ByteString FunctionTypeTyVars
  -- ^ External functions (only named).
  , -- (keep this last for convenient partial application)

    _cgenGlobalContext :: CGenGlobalContext arch
  -- ^ Enclosing global context
  }

L.makeLenses ''CGenModuleContext

-- | Context available when generating constraints for a given function
data CGenFunctionContext arch = CGenFunctionContext
  { _cgenCurrentFun :: FunctionTypeTyVars
  -- ^ TODO
  , _cgenCurrentFunName :: BSC.ByteString
  -- ^ Identifier for the current function
  , -- TODO (val) could this be just TyVars?  Does it become obsolete when we
    -- resolve TyVars?
    _cgenBlockPhiTypes :: Map (FnBlockLabel (ArchAddrWidth arch)) [Ty]
  , -- (keep this last for convenient partial application)

    _cgenModuleContext :: CGenModuleContext arch
  -- ^ Enclosing module context
  }

L.makeLenses ''CGenFunctionContext

-- | Context available when generating constraints for a given block.  At the
-- moment, I managed to make it so that we don't need anything special, but it's
-- nice to set this up for being future-proof.
data CGenBlockContext arch = CGenBlockContext
  { _cgenConstraintProv :: ConstraintProvenance
  -- ^ The provenance to use when generating constraints
  , -- (keep this last for convenient partial application)

    _cgenFunctionContext :: CGenFunctionContext arch
  -- ^ Enclosing function context
  }

L.makeLenses ''CGenBlockContext

data CGenState arch = CGenState
  { _assignTyVars :: Map BSC.ByteString (Map FnAssignId TyVar)
  , -- , -- | Offset of the current instruction, used (not right now) for tagging
    --   -- constraints and warnings.
    --   _curOffset     :: ArchAddrWord arch
    _warnings :: [Warning]
  }

L.makeLenses ''CGenState

newtype CGenM ctx arch a = CGenM
  { _getCGenM ::
      Reader.ReaderT
        (ctx arch)
        (StateT (CGenState arch) SolverM)
        a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , Reader.MonadReader (ctx arch)
    , MonadState (CGenState arch)
    )

withinContext ::
  (outer arch -> inner arch) ->
  CGenM inner arch a ->
  CGenM outer arch a
withinContext f (CGenM m) = CGenM (Reader.withReaderT f m)

inSolverM :: SolverM a -> CGenM ctxt arch a
inSolverM = CGenM . lift . lift

runCGenM ::
  Memory (ArchAddrWidth arch) ->
  Bool ->
  Bool ->
  CGenM CGenGlobalContext arch a ->
  a
runCGenM mem trace orig (CGenM m) = runSolverM trace orig ptrWidth $ do
  let segs = memSegments mem
  -- Allocate a row variable for each memory segment
  memRows <- Map.fromList <$> mapM (\seg -> (,) seg <$> S.freshRowVar) segs
  let ctxt0 =
        CGenGlobalContext
          { _cgenMemory = mem
          , _cgenMemoryRegions = memRows
          }
  evalStateT (Reader.runReaderT m ctxt0) st0
 where
  ptrWidth = widthVal (memWidth mem)

  st0 =
    CGenState
      { _assignTyVars = mempty
      , _warnings = mempty
      }

-- ------------------------------------------------------------
-- Monad operations

-- | Warn about a missing feature (for example).
warn :: String -> CGenM ctx arch ()
warn s = CGenM $ warnings <>= [Warning s]

-- '<<+=' increment var, return old value

atFnAssignId ::
  BSC.ByteString ->
  FnAssignId ->
  L.Lens' (CGenState arch) (Maybe TyVar)
atFnAssignId fn aId = assignTyVars . L.at fn . L.non Map.empty . L.at aId

-- | Retrieves the type variable associated to the given `FnAssignId`, if any,
-- otherwise creates and registers a fresh type variable for it.
--
-- NOTE: This does **not** use the function name present in the monadic context,
-- because we sometimes wants to resolve assign ids for other functions than the
-- one currently being analyzed.
tyVarForAssignId ::
  -- | Function defining the `FnAssignId`
  BSC.ByteString ->
  FnAssignId ->
  CGenM ctx arch TyVar
tyVarForAssignId fn aId = do
  mtv <- CGenM $ L.use (atFnAssignId fn aId)
  case mtv of
    Just tv -> pure tv
    Nothing -> do
      tyv <- freshTyVar (BSC.unpack fn <> "." <> show aId)
      CGenM $ atFnAssignId fn aId ?= tyv
      pure tyv

-- freshForCallRet :: FnReturnVar tp -> CGenM ctx arch TyVar
-- freshForCallRet a = do
--   tyv <- freshTyVar
--   -- update assignment type map
--   CGenM $ assignTypes . at (frAssignId a) ?= ITyVar tyv
--   pure tyv

-- | Returns the type variable corresponding to the given assignment identifier,
-- or creates a fresh one for it if needed.
assignIdTyVar :: BSC.ByteString -> FnAssignId -> CGenM ctx arch TyVar
assignIdTyVar fn aId = do
  -- fn <- askContext cgenCurrentFunName
  mTyVar <- CGenM $ L.use (atFnAssignId fn aId)
  case mTyVar of
    Nothing -> tyVarForAssignId fn aId
    Just tyVar -> pure tyVar

-- | Returns the associated type for a function assignment id, if any.
-- Otherwise, returns its type variable as an `ITy`.
assignIdTypeFor :: BSC.ByteString -> FnAssignId -> CGenM ctx arch Ty
assignIdTypeFor fn aId = varTy <$> assignIdTyVar fn aId

assignIdType :: FnAssignId -> CGenM CGenBlockContext arch Ty
assignIdType aId = do
  fn <- askContext (cgenFunctionContext . cgenCurrentFunName)
  assignIdTypeFor fn aId

assignmentType :: FnAssignment arch tp -> CGenM CGenBlockContext arch Ty
assignmentType = assignIdType . fnAssignId

-- We lump returns and assigns into the same map
funRetType :: FnReturnVar tp -> CGenM CGenBlockContext arch Ty
funRetType = assignIdType . frAssignId

updFunRetType :: FnReturnVar tp -> TyVar -> CGenM CGenBlockContext arch ()
updFunRetType fr tv = do
  fn <- askContext (cgenFunctionContext . cgenCurrentFunName)
  let aId = frAssignId fr
  tyVar <- assignIdTyVar fn aId
  emitSubType (varTy tv) (varTy tyVar)

phiType :: FnPhiVar arch tp -> CGenM CGenBlockContext arch Ty
phiType = assignIdType . unFnPhiVar

argumentType :: Int -> CGenM CGenBlockContext arch Ty
argumentType i = do
  tys <- fttvArgs <$> askContext (cgenFunctionContext . cgenCurrentFun)
  case tys ^? L.ix i of
    Nothing -> error "Missing argument"
    Just ty -> pure (varTy ty)

askContext :: L.Getting a (ctx arch) a -> CGenM ctx arch a
askContext = CGenM . Reader.ask . L.view

addrWidth :: CGenM CGenBlockContext arch (NatRepr (ArchAddrWidth arch))
addrWidth =
  memWidth
    <$> askContext
      ( cgenFunctionContext
          . cgenModuleContext
          . cgenGlobalContext
          . cgenMemory
      )

addrToSegmentOff ::
  FnArchConstraints arch =>
  Integer ->
  CGenM CGenBlockContext arch (Maybe (MemSegmentOff (ArchAddrWidth arch)))
addrToSegmentOff addr = do
  mem <-
    askContext
      ( cgenFunctionContext
          . cgenModuleContext
          . cgenGlobalContext
          . cgenMemory
      )
  pure (resolveAbsoluteAddr mem (memWord (fromInteger addr)))

-- currentOffset :: CGenM ctx arch (ArchAddrWord arch)
-- currentOffset = CGenM $ use curOffset

-- setCurrentOffset :: ArchAddrWord arch -> CGenM ctx arch ()
-- setCurrentOffset off = CGenM $ curOffset .= off

currentFunctionTyVars :: CGenM CGenBlockContext arch FunctionTypeTyVars
currentFunctionTyVars = askContext (cgenFunctionContext . cgenCurrentFun)

phisForBlock ::
  FnBlockLabel (ArchAddrWidth arch) ->
  CGenM CGenBlockContext arch [Ty]
phisForBlock blockAddr =
  fromMaybe (error "Missing phi type")
    . Map.lookup blockAddr
    <$> askContext (cgenFunctionContext . cgenBlockPhiTypes)

functionTypeTyVars ::
  forall arch.
  FnValue arch (BVType (ArchAddrWidth arch)) ->
  CGenM CGenBlockContext arch (Maybe FunctionTypeTyVars)
functionTypeTyVars saddr = do
  moduleContext <- askContext (cgenFunctionContext . cgenModuleContext)
  let ftypes = L.view cgenFunTypes moduleContext
  let namedftypes = L.view cgenNamedFunTypes moduleContext
  let mem = L.view (cgenGlobalContext . cgenMemory) moduleContext
  let aWidth = memAddrWidth mem

  case saddr of
    FnConstantValue _ v -> do
      -- c.f. RegisterUse.x86CallRegs
      let faddr =
            addrWidthClass aWidth $
              absoluteAddr (fromInteger v)
      -- FIXME
      pure (flip Map.lookup ftypes =<< asSegmentOff mem faddr)
    FnFunctionEntryValue _ fn -> pure (Map.lookup fn namedftypes)
    _ -> pure Nothing

--------------------------------------------------------------------------------
-- Forwarded functions (to solver)

-- | Returns a fresh type var.
freshTyVar :: String -> CGenM ctx arch TyVar
freshTyVar context =
  inSolverM (S.freshTyVar (Just context) Nothing)

maybeGlobalTC ::
  FnArchConstraints arch =>
  Ty ->
  MemSegmentOff (ArchAddrWidth arch) ->
  CGenM CGenBlockContext arch ()
maybeGlobalTC ty soff = do
  rs <-
    askContext
      ( cgenFunctionContext
          . cgenModuleContext
          . cgenGlobalContext
          . cgenMemoryRegions
      )
  let rowv = Map.findWithDefault (error "Missing segment region") (segoffSegment soff) rs
  inSolverM (S.maybeGlobalTC ty rowv (fromIntegral $ segoffOffset soff))

-- -- | Returns a fresh row var.
-- freshRowVar :: CGenM ctx arch RowVar
-- freshRowVar = inSolverM S.freshRowVar

emitEq :: ConstraintProvenance -> Ty -> Ty -> CGenM ctx arch ()
emitEq prov t1 t2 = inSolverM (eqTC prov t1 t2)

-- | Emits an add which may be a pointer add
emitPtrAddSymbolic :: ConstraintProvenance -> Ty -> Ty -> Ty -> CGenM ctx arch ()
emitPtrAddSymbolic prov rty t1 t2 = inSolverM (ptrAddTC prov rty t1 t2 OCSymbolic)

-- | ptr - N is sometimes encoded as ptr + (-N).  For very large
-- numbers (i.e., upper bit set) we negate and use the subtraction
-- constraint instead.
emitPtrAddOffset :: ConstraintProvenance -> Ty -> Ty -> Ty -> Integer -> CGenM CGenBlockContext arch ()
emitPtrAddOffset prov rty t1 t2 off = do
  awidth <- widthVal <$> addrWidth
  if testBit off (awidth - 1)
    then emitPtrSubOffset prov rty t1 t2 (2 ^ awidth - off)
    else inSolverM (ptrAddTC prov rty t1 t2 (OCOffset (fromInteger off)))

emitPtrAddGlobalPtr :: ConstraintProvenance -> Ty -> Ty -> Ty -> CGenM ctx arch ()
emitPtrAddGlobalPtr prov rty t1 t2 = inSolverM (ptrAddTC prov rty t1 t2 OCPointer)

-- | Emits an add which may be a pointer add
emitPtrSubSymbolic :: ConstraintProvenance -> Ty -> Ty -> Ty -> CGenM ctx arch ()
emitPtrSubSymbolic prov rty t1 t2 = inSolverM (ptrSubTC prov rty t1 t2 OCSymbolic)

emitPtrSubOffset :: ConstraintProvenance -> Ty -> Ty -> Ty -> Integer -> CGenM CGenBlockContext arch ()
emitPtrSubOffset prov rty t1 t2 off = do
  awidth <- widthVal <$> addrWidth
  if testBit off (awidth - 1)
    then emitPtrAddOffset prov rty t1 t2 (2 ^ awidth - off)
    else inSolverM (ptrSubTC prov rty t1 t2 (OCOffset (fromInteger off)))

emitPtrSubGlobalPtr :: ConstraintProvenance -> Ty -> Ty -> Ty -> CGenM ctx arch ()
emitPtrSubGlobalPtr prov rty t1 t2 = inSolverM (ptrAddTC prov rty t1 t2 OCPointer)

-- | First type must be a valid value of the second type (more lax than equality
-- at function/join boundaries).
emitSubType :: Ty -> Ty -> CGenM ctx arch ()
emitSubType a b = inSolverM (subTypeTC a b)

-- | Emits a constraint that the argument isn't a pointer
emitNumTy ::
  forall ctx arch.
  FnArchConstraints arch =>
  ConstraintProvenance ->
  Int ->
  Ty ->
  CGenM ctx arch ()
emitNumTy prov sz t =
  inSolverM (eqTC prov t (numTy sz))

-- pointerWidth :: forall arch. FnArchConstraints arch => Proxy arch -> Int
-- pointerWidth Proxy =
--   widthVal (addrWidthNatRepr (addrWidthRepr (Proxy :: Proxy (ArchAddrWidth arch))))

emitPtr ::
  ConstraintProvenance ->
  Ty ->
  -- | Type that is recognized as pointer
  Ty ->
  CGenM ctx arch ()
emitPtr prov pointee pointer =
  inSolverM (ptrTC prov pointee pointer)

-- emitStructPtr :: ITy -> ITy -> Integer -> Some TypeRepr -> CGenM ctx arch ()
-- emitStructPtr tr tp o sz = emitConstraint (CPointerAndOffset tr sz tp o)

-- -----------------------------------------------------------------------------
-- Core algorithm

genFnValue :: FnArchConstraints arch => FnValue arch tp -> CGenM CGenBlockContext arch Ty
genFnValue v =
  case v of
    FnUndefined{} -> punt
    FnConstantBool{} -> pure (numTy 1)
    FnConstantValue sz _ -> pure (numTy (widthVal sz))
    FnAssignedValue a -> assignmentType a
    FnPhiValue phiv -> phiType phiv
    FnReturn frv -> funRetType frv
    FnFunctionEntryValue{} -> punt
    FnArg i _ -> argumentType i
 where
  punt = do
    warn "Punting on FnValue"
    varTy <$> freshTyVar (show (PP.pretty v))

-- | Generate constraints for an App.  The first argument is the
-- output (result) type.
genApp ::
  FnArchConstraints arch =>
  (Ty, Int) ->
  App (FnValue arch) tp ->
  CGenM CGenBlockContext arch ()
genApp (ty, outSize) app = do
  prov <- Reader.asks $ L.view cgenConstraintProv
  case app of
    Eq l r -> do
      join (emitEq prov <$> genFnValue l <*> genFnValue r)
      emitNumTy prov 1 ty
    Mux _ _ l r -> do
      -- let lProv = FnRepProv $ FnValueProv l
      -- let rProv = FnRepProv $ FnValueProv r
      lTy <- genFnValue l
      emitEq prov lTy =<< genFnValue r
      emitEq prov ty lTy -- ty = rTy follows by transitivity, so we don't add it

    -- We don't generate any further constraints for boolean ops
    AndApp{} -> pure ()
    OrApp{} -> pure ()
    NotApp{} -> pure ()
    XorApp{} -> pure ()
    -- We currently ignore tuples
    MkTuple{} -> warn "Ignoring MkTyple"
    TupleField{} -> warn "Ignoring TupleField"
    -- We currently ignore vectors
    ExtractElement{} -> warn "Ignoring ExtractElement"
    InsertElement{} -> warn "Ignoring InsertElement"
    -- BV size operations
    -- FIXME: for now we assume we can only do these on bitvecs (not pointers)
    Trunc v _ -> nonptrUnOp prov v
    SExt v _ -> nonptrUnOp prov v
    UExt v _ -> nonptrUnOp prov v
    -- FIXME: Not sure what to do with these, for now leave underconstrained.
    Bitcast{} -> warn "Ignoring Bitcast"
    -- BV Arithmetic
    --
    -- In general we don't need to do much, as most are just bv ops.
    -- Add and Sub are the main exceptions
    BVAdd _sz l a@(FnAssignedValue FnAssignment{fnAssignRhs = FnAddrWidthConstant o}) -> do
      pTy <- genFnValue l
      oTy <- genFnValue a

      mseg <- addrToSegmentOff o
      case mseg of
        Nothing -> emitPtrAddOffset prov ty pTy oTy o
        Just _ -> emitPtrAddGlobalPtr prov ty pTy oTy
    BVAdd _sz a@(FnAssignedValue FnAssignment{fnAssignRhs = FnAddrWidthConstant o}) r -> do
      pTy <- genFnValue r
      oTy <- genFnValue a

      mseg <- addrToSegmentOff o
      case mseg of
        Nothing -> emitPtrAddOffset prov ty pTy oTy o
        Just _ -> emitPtrAddGlobalPtr prov ty pTy oTy
    BVAdd sz l r -> do
      addrw <- addrWidth
      if isJust (testEquality addrw sz)
        then join $ emitPtrAddSymbolic prov ty <$> genFnValue l <*> genFnValue r
        else nonptrBinOp prov l r

    -- FIXME: should this be considered another add?
    BVAdc _ l r _c -> nonptrBinOp prov l r
    BVSub _sz l a@(FnAssignedValue FnAssignment{fnAssignRhs = FnAddrWidthConstant o}) -> do
      pTy <- genFnValue l
      oTy <- genFnValue a

      mseg <- addrToSegmentOff o
      case mseg of
        Nothing -> emitPtrSubOffset prov ty pTy oTy o
        Just _ -> emitPtrSubGlobalPtr prov ty pTy oTy

    -- We don't do anything special for (maybeGlobalConst - x)
    BVSub sz l r -> do
      addrw <- addrWidth
      if isJust (testEquality addrw sz)
        then join $ emitPtrSubSymbolic prov ty <$> genFnValue l <*> genFnValue r
        else nonptrBinOp prov l r
    BVSbb _ l r _ -> nonptrBinOp prov l r
    BVMul _ l r -> nonptrBinOp prov l r
    -- We are allowed to compare pointers, and test their bits
    BVUnsignedLe{} -> pure ()
    BVUnsignedLt{} -> pure ()
    BVSignedLe{} -> pure ()
    BVSignedLt{} -> pure ()
    BVTestBit{} -> pure ()
    BVComplement _ v -> nonptrUnOp prov v
    -- FIXME: this could be a pointer op to mask bits off?
    BVAnd _ l r -> nonptrBinOp prov l r
    -- FIXME: this could be a pointer op to do add on aligned ptrs?
    BVOr _ l r -> nonptrBinOp prov l r
    BVXor _ l r -> nonptrBinOp prov l r
    BVShl _ l r -> nonptrBinOp prov l r
    BVShr _ l r -> nonptrBinOp prov l r
    BVSar _ l r -> nonptrBinOp prov l r
    -- FIXME: e.g. l could be a ptr?  Not sure why you would use this though
    UadcOverflows l r _ -> nonptrBinCmp prov l r
    SadcOverflows l r _ -> nonptrBinCmp prov l r
    UsbbOverflows l r _ -> nonptrBinCmp prov l r
    SsbbOverflows l r _ -> nonptrBinCmp prov l r
    PopCount _ v -> nonptrUnOp prov v
    ReverseBytes _ v -> nonptrUnOp prov v
    Bsf _ v -> nonptrUnOp prov v
    Bsr _ v -> nonptrUnOp prov v
 where
  nonptrUnOp ::
    forall n arch.
    FnArchConstraints arch =>
    ConstraintProvenance ->
    FnValue arch (BVType n) ->
    CGenM CGenBlockContext arch ()
  nonptrUnOp prov v = do
    emitNumTy prov (bvWidth v) =<< genFnValue v
    emitNumTy prov outSize ty

  -- \| The result and arguments have to be bitvecs (i.e., not ptrs).
  -- We don't relate the sizes as that is given by the macaw type at
  -- the moment.
  nonptrBinOp ::
    forall n m arch.
    FnArchConstraints arch =>
    ConstraintProvenance ->
    FnValue arch (BVType n) ->
    FnValue arch (BVType m) ->
    CGenM CGenBlockContext arch ()
  nonptrBinOp prov l r = do
    emitNumTy prov (bvWidth l) =<< genFnValue l
    emitNumTy prov (bvWidth r) =<< genFnValue r
    emitNumTy prov outSize ty

  nonptrBinCmp ::
    forall n m arch.
    FnArchConstraints arch =>
    ConstraintProvenance ->
    FnValue arch (BVType n) ->
    FnValue arch (BVType m) ->
    CGenM CGenBlockContext arch ()
  nonptrBinCmp prov l r = do
    emitNumTy prov (bvWidth l) =<< genFnValue l
    emitNumTy prov (bvWidth r) =<< genFnValue r

genMemOp ::
  FnArchConstraints arch =>
  Ty ->
  FnValue arch (BVType (ArchAddrWidth arch)) ->
  Some TypeRepr ->
  CGenM CGenBlockContext arch ()
genMemOp ty ptr (Some tp) = do
  prov <- Reader.asks $ L.view cgenConstraintProv
  ptrWidth <- widthVal <$> addrWidth
  emitPtr prov ty =<< genFnValue ptr
  case tp of
    BVTypeRepr n | widthVal n /= ptrWidth -> emitNumTy prov (widthVal n) ty
    _ -> pure ()

-- case ptr of
--   FnAssignedValue FnAssignment { fnAssignRhs = FnEvalApp (BVAdd _sz p q) }
--     -- We rely on macaw to have constant folded adds, so only one
--     -- will be a const.  We also need to deal with global +
--     -- computed.  This should be done in constrain translation.
--     | FnConstantValue _ o <- q -> do
--         tp <- genFnValue p
--         emitStructPtr ty tp o sz
--     | FnConstantValue _ o <- p -> do
--         tp <- genFnValue q
--         emitStructPtr ty tp o sz

--   FnAssignedValue a ->
--     emitPtr (viewSome anyTypeWidth sz) ty =<< assignmentType a

--   FnArg{} -> do
--     emitPtr (viewSome anyTypeWidth sz) ty =<< genFnValue ptr
--   FnPhiValue{} -> do
--     emitPtr (viewSome anyTypeWidth sz) ty =<< genFnValue ptr
--   _ ->
--     error (show ptr)
--     -- return ()

bvWidth :: FnArchConstraints arch => FnValue arch (BVType n) -> Int
bvWidth = bitWidth . typeRepr

genFnAssignment ::
  forall arch tp.
  FnArchConstraints arch =>
  FnAssignment arch tp ->
  CGenM CGenBlockContext arch ()
genFnAssignment a = Reader.local (L.set cgenConstraintProv prov) $ do
  fn <- askContext (cgenFunctionContext . cgenCurrentFunName)
  ty <- varTy <$> tyVarForAssignId fn (fnAssignId a)
  case rhs of
    FnSetUndefined{} -> pure () -- no constraints generated
    FnReadMem ptr sz -> genMemOp ty ptr (Some sz)
    FnCondReadMem _sz _cond ptr def -> do
      genMemOp ty ptr (Some (typeRepr def))
      emitEq prov ty =<< genFnValue def
    FnEvalApp app ->
      genApp (ty, bitWidth (typeRepr app)) app
    FnEvalArchFn _afn -> warn "ignoring EvalArchFn"
    FnAddrWidthConstant addr -> do
      m_mseg <- addrToSegmentOff addr
      case m_mseg of
        Nothing
          | addr == 0 -> pure () -- could be NULL or 0
          | otherwise -> inSolverM . isNumTC prov ty . widthVal =<< addrWidth
        Just soff -> maybeGlobalTC ty soff
 where
  rhs = fnAssignRhs a
  prov = FnRepProv $ FnAssignmentProv a

-- | This helper gives us the bitwidth of the types we can read/write from
-- memory.
bitWidth :: TypeRepr tp -> Int
bitWidth typ = case typ of
  BoolTypeRepr -> error "bitWidth: BoolType"
  BVTypeRepr nr -> fromIntegral (intValue nr)
  FloatTypeRepr fir -> fromIntegral (widthVal (floatInfoBits fir))
  TupleTypeRepr _li -> error "bitWidth: TupleType"
  VecTypeRepr _nr _tr -> error "bitWidth: VecType"

-- The offset argument is used by call term stmts
genFnStmt ::
  FnArchConstraints arch =>
  FnStmt arch ->
  CGenM CGenBlockContext arch ()
genFnStmt stmt =
  Reader.local (L.set cgenConstraintProv prov) $
    case stmt of
      FnComment _ -> pure ()
      FnAssignStmt a -> genFnAssignment a
      FnWriteMem addr v -> do
        tr <- genFnValue v
        genMemOp tr addr (Some (typeRepr v))
      FnCondWriteMem _cond addr v _ -> do
        tr <- genFnValue v
        genMemOp tr addr (Some (typeRepr v))
      FnCall fn args m_rv -> genCall fn args m_rv
      FnArchStmt _astmt -> warn "Ignoring FnArchStmt"
 where
  prov = FnRepProv $ FnStmtProv stmt

-- -- | Matches up the argument registers with the destination.
-- genRegType :: RegState (ArchReg arch) (Value arch ids)
--            -> RegType arch
--            -> CGenM ctx arch ITy
-- genRegType regs rty = traverse_ go (Map.toList rty)
--   where
--     go (Some r, ty) = emitEq ty =<< genFnValue (regs ^. boundValue r)

-- | Used at the end of a block on a control transfer.  In essence
-- this unifies the block arguments with the values from the current block.
genBlockTransfer ::
  FnArchConstraints arch =>
  FnJumpTarget arch ->
  CGenM CGenBlockContext arch ()
genBlockTransfer tgt = do
  phiTys <- phisForBlock (fnJumpLabel tgt)
  let phiVals = V.toList (fnJumpPhiValues tgt)
  zipWithM_ go phiVals phiTys
 where
  go (Some v) ty = join (emitSubType <$> genFnValue v <*> pure ty)

genCall ::
  FnArchConstraints arch =>
  FnValue arch (BVType (ArchAddrWidth arch)) ->
  -- | arguments
  [Some (FnValue arch)] ->
  -- | Name of return value
  Maybe (Some FnReturnVar) ->
  CGenM CGenBlockContext arch ()
genCall fn args m_ret = do
  m_ftyp <- functionTypeTyVars fn

  case m_ftyp of
    Nothing ->
      warn
        ( "Couldn't determine target of call to "
            ++ show (PP.pretty fn)
            ++ " ("
            ++ show fn
            ++ ")"
        )
    Just ftyp -> do
      -- Arguments
      zipWithM_ go args (fttvArgs ftyp)

      -- Return
      case (m_ret, fttvRet ftyp) of
        (Just (Some rv), Just rty) -> updFunRetType rv rty
        (Just _, _) -> warn "Missing return type"
        _ -> pure ()
 where
  go (Some v) ty = join (emitSubType <$> genFnValue v <*> pure (varTy ty))

genFnBlock ::
  FnArchConstraints arch =>
  -- | Current block
  FnBlock arch ->
  CGenM CGenFunctionContext arch ()
genFnBlock b = do
  withinContext (CGenBlockContext BlockProv) $ do
    -- let blockAddr = pblockAddr b

    -- Generate constraints (and type vars) for the stmts
    mapM_ genFnStmt (fbStmts b)
    case fbTerm b of
      FnJump tgtAddr ->
        genBlockTransfer tgtAddr
      FnBranch _cond trueAddr falseAddr -> do
        genBlockTransfer trueAddr
        genBlockTransfer falseAddr
      FnLookupTable _lookupIdx tgtV ->
        traverse_ genBlockTransfer tgtV
      FnRet m_v -> do
        fty <- currentFunctionTyVars
        case (m_v, fttvRet fty) of
          (Just (Some v), Just ty) -> emitSubType (varTy ty) =<< genFnValue v
          (Nothing, Nothing) -> pure ()
          _ -> warn "Mismatch between return type and return value"
      FnTailCall fn args -> do
        fty <- currentFunctionTyVars
        m_calledFTy <- functionTypeTyVars fn
        genCall fn args Nothing
        case (fttvRet fty, fttvRet <$> m_calledFTy) of
          (_, Nothing) -> warn "Unknown function"
          (Just rty, Just (Just rty')) -> emitSubType (varTy rty) (varTy rty')
          (Just _, _) -> warn "Mismatch between return type and return type in tail call"
          _ -> pure ()

genFunction ::
  FnArchConstraints arch =>
  Function arch ->
  CGenM CGenModuleContext arch ()
genFunction fn = do
  cFun <-
    Map.findWithDefault (error "Missing function") (fnAddr fn)
      <$> askContext cgenFunTypes
  let mkPhis b =
        let phiVars = viewSome unFnPhiVar <$> V.toList (fbPhiVars b)
         in let mkPhiVar aId = varTy <$> tyVarForAssignId (fnName fn) aId
             in (,) (fbLabel b) <$> mapM mkPhiVar phiVars
  bphis <- Map.fromList <$> mapM mkPhis (fnBlocks fn)
  withinContext
    (CGenFunctionContext cFun (fnName fn) bphis)
    (mapM_ genFnBlock (fnBlocks fn))

-- Allocates fresh type variables for a function type.
freshFunctionTypeTyVars ::
  -- | Name of the function
  BSC.ByteString ->
  FunctionType arch ->
  CGenM ctx arch FunctionTypeTyVars
freshFunctionTypeTyVars fn ft = do
  let fnStr = BSC.unpack fn
  args <- for (zip (fnArgTypes ft) [(0 :: Int) ..]) (\(_, i) -> freshTyVar (fnStr <> ".arg" <> show i))
  ret <- traverse (const (freshTyVar (fnStr <> ".ret"))) (fnReturnType ft)
  pure (FunctionTypeTyVars args ret)

-- | While initially we were creating one type variable per argument/return, we
-- would now be able to cleanly separate the type as expected by callees from
-- the type as manipulated internally.  To do so, we create **two** type
-- variables per argument/return: one from the point of view of callers, and one
-- from the point of view of this function.
-- callerCalleeTyVars ::
--   -- | Name of the function
--   BSC.ByteString ->
--   FunctionType arch -> CGenM ctx arch FunctionTypeTyVars
-- callerCalleeTyVars fn ft =
--   CallerCalleeTyVars <$> functionTypeTyVars fn ft <*> functionTypeTyVars fn ft
data ModuleConstraints arch = ModuleConstraints
  { mcFunTypes :: Map (ArchSegmentOff arch) FunctionTypeTyVars
  -- ^ Types for global functions
  , mcExtFunTypes :: Map BSC.ByteString FunctionTypeTyVars
  -- ^ Types for the named external functions
  , mcAssignTyVars :: Map BSC.ByteString (Map FnAssignId TyVar)
  -- ^ A mapping from `FnAssignId` to their known type (either a fresh type
  -- variable, or the type witnessed at function call boundaries), for each
  -- function.  Because `FnAssignId` are only unique per-function, the mapping
  -- is on a per-function basis (using the `ByteString` name of the function
  -- as key).
  , mcWarnings :: [Warning]
  -- ^ Warnings gathered during constraint generation
  , -- , -- | The actual constraints
    --   mcTyConstraints :: [TyConstraint]

    mcTypeMap :: Map TyVar FTy
  -- ^ The final mapping of type variables to their inferred type
  , mcNamedTypes :: [(StructName, FTy)]
  -- ^ Type names used by @mcTypeMap@
  }

showInferredTypes :: ModuleConstraints arch -> String
showInferredTypes mc =
  unlines (showMapping <$> Map.assocs (mcTypeMap mc))
 where
  showMapping :: (TyVar, FTy) -> String
  showMapping (tv, ty) = concat [show (PP.pretty tv), " : ", show (PP.pretty ty)]

genModuleConstraints ::
  FnArchConstraints arch =>
  FoldableF (FnArchStmt arch) =>
  FoldableFC (ArchFn arch) =>
  RecoveredModule arch ->
  Memory (ArchAddrWidth arch) ->
  Bool ->
  Bool ->
  ModuleConstraints arch
genModuleConstraints m mem trace orig = runCGenM mem trace orig $ do
  -- allocate type variables for functions without types
  -- FIXME: we currently ignore hints

  -- traceM (show (memSegmentMap mem))

  let doDecl d = do
        fty <- freshFunctionTypeTyVars (funDeclName d) (funDeclType d)
        -- Declarations don't need a different type variable when called since
        -- we don't analyze their body
        -- let cc = CallerCalleeTyVars fty fty
        pure ((funDeclAddr d, fty), (funDeclName d, fty))

  (declAddrs, declSyms) <- mapAndUnzipM doDecl (recoveredDecls m)

  -- Sometimes definitions are called via their name.
  let doDef d = do
        fty <- freshFunctionTypeTyVars (fnName d) (fnType d)
        pure ((fnAddr d, fty), (fnName d, fty))

  (defAddrs, defSyms) <- mapAndUnzipM doDef (recoveredDefs m)

  let
    symMap = Map.fromList (defSyms ++ declSyms)
    addrMap = Map.fromList (defAddrs ++ declAddrs)

  withinContext
    (CGenModuleContext addrMap symMap)
    (mapM_ genFunction (recoveredDefs m))

  -- FIXME: abstract
  tyVars <- CGenM $ L.use assignTyVars
  warns <- CGenM $ L.use warnings

  tyMap <- inSolverM unifyConstraints

  pure
    ModuleConstraints
      { mcFunTypes = addrMap
      , mcExtFunTypes = symMap
      , mcAssignTyVars = tyVars
      , mcWarnings = warns
      , mcTypeMap = csTyVars tyMap
      , mcNamedTypes = csNamedStructs tyMap
      }
