{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- This module provides a constraint generation pass for inferring the
-- types of Reopt (FnRep) programs, inspired by TIE.
--

module Reopt.TypeInference.ConstraintGen
  ( Constraint
  , FunType(..)
  , ITy, FTy
  , ModuleConstraints(..)
  , genModuleConstraints
  , showInferredTypes
  , tyConstraint
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict (State, runState)
import qualified Data.ByteString.Char8      as BSC
import           Data.Foldable              (traverse_)
import           Data.List                  (intercalate)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Parameterized         (FoldableF, FoldableFC)
import qualified Data.Vector                as V

import           Data.Parameterized.NatRepr (NatRepr, intValue, testEquality, widthVal)
import           Data.Parameterized.Some    (Some(Some), viewSome)

import           Reopt.CFG.FnRep
import           Reopt.TypeInference.Constraints
import qualified Prettyprinter as PP

import           Data.Macaw.CFG             (App (..), ArchAddrWidth, ArchFn,
                                             ArchSegmentOff, memReprBytes)
import           Data.Macaw.Memory          (Memory, absoluteAddr,
                                             addrWidthClass, asSegmentOff,
                                             memAddrWidth, memWidth)
import           Data.Macaw.Types           (BVType, TypeRepr (..), floatInfoBits, typeRepr)

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

-- FIXME: arch not required here
data FunType arch = FunType {
    funTypeArgs :: [TyVar]
  , funTypeRet  :: Maybe TyVar
}

instance Show (FunType arch) where
  show ft =
    "(" ++ intercalate ", " (map show (funTypeArgs ft)) ++ ") -> " ++
    maybe "_|_" show (funTypeRet ft)

varITy :: TyVar -> ITy
varITy = UnknownTy

-- This is the free type for constraints, should be replace by the
--  type from the constraint solver.

data Constraint =
  CEq ITy ITy -- ^ The types must be equal
  | CAddrWidthAdd ITy ITy ITy
  -- ^ At most one type may be a ptr, first type is the result
  | CAddrWidthSub ITy ITy ITy
  -- ^ The rhs may not be a ptr if the lhs is a bv, first type is the result
  | CBVNotPtr Int ITy -- ^ The argument cannot be a ptr (so should be a bv)
  | CIsPtr Int ITy -- ^ The type must point to something
  | CAmbiguousBV ITy -- ^ This is a bitvector the size of an address, but we know nothing more

instance PP.Pretty Constraint where
  pretty = \case
    CEq t1 t2 -> PP.hsep [PP.pretty t1, "=", PP.pretty t2]
    CAddrWidthAdd t1 t2 t3 -> PP.hsep [PP.pretty t1, "=", PP.pretty t2, "+", PP.pretty t3]
    CAddrWidthSub t1 t2 t3 -> PP.hsep [PP.pretty t1, "=", PP.pretty t2, "-", PP.pretty t3]
    CBVNotPtr sz t -> PP.hsep ["bv" <> PP.pretty sz, PP.pretty t]
    CIsPtr sz t -> PP.hsep ["ptr@" <> PP.pretty sz, PP.pretty t]
    CAmbiguousBV t -> PP.hsep ["bv-or-ptr?", PP.pretty t]

instance Show Constraint where
  show = show . PP.pretty

-- -----------------------------------------------------------------------------
-- Monad

-- This tracks e.g. fresh type variable names, along with known
-- types/function types.

newtype Warning = Warning String

instance Show Warning where
  show (Warning w) = w

-- | Context available throughout all of the constraint generation
newtype CGenGlobalContext arch = CGenGlobalContext
  { _cgenMemory      :: Memory (ArchAddrWidth arch)
  }

makeLenses ''CGenGlobalContext

-- | Context available when generating constraints for a given module
data CGenModuleContext arch = CGenModuleContext
  { _cgenFunTypes    :: Map (ArchSegmentOff arch) (FunType arch)
  , -- | External functions (only named).
    _cgenExtFunTypes :: Map BSC.ByteString (FunType arch)

  -- (keep this last for convenient partial application)

  , -- | Enclosing global context
    _cgenGlobalContext :: CGenGlobalContext arch
  }

makeLenses ''CGenModuleContext

-- | Context available when generating constraints for a given function
data CGenFunctionContext arch = CGenFunctionContext
  { -- | TODO
    _cgenCurrentFun     :: FunType arch
  , -- | Identifier for the current function
    _cgenCurrentFunName :: BSC.ByteString
  , -- |

    -- TODO (val) could this be just TyVars?  Does it become obsolete when we
    -- resolve TyVars?
    _cgenBlockPhiTypes  :: Map (FnBlockLabel (ArchAddrWidth arch)) [ITy]

  -- (keep this last for convenient partial application)

  , -- | Enclosing module context
    _cgenModuleContext  :: CGenModuleContext arch
  }

makeLenses ''CGenFunctionContext

-- | Context available when generating constraints for a given block.  At the
-- moment, I managed to make it so that we don't need anything special, but it's
-- nice to set this up for being future-proof.
newtype CGenBlockContext arch = CGenBlockContext
  {
    -- (keep this last for convenient partial application)

    -- | Enclosing function context
    _cgenFunctionContext :: CGenFunctionContext arch
  }

makeLenses ''CGenBlockContext

data CGenState arch = CGenState {
    _nextFreeTyVar  :: Int -- FIXME: use Nonce?
  , _nextFreeRowVar :: Int -- FIXME: use Nonce?
  , _assignTyVars   :: Map BSC.ByteString (Map FnAssignId TyVar)
  , _tyVarTypes     :: Map TyVar ITy
  -- | Offset of the current instruction, used (not right now) for
  -- tagging constraints and warnings.
  -- , _curOffset     :: ArchAddrWord arch
  , _warnings      :: [Warning]
  , _constraints   :: [Constraint] -- in reverse gen order
}

makeLenses ''CGenState

newtype CGenM ctx arch a =
  CGenM { _getCGenM :: ReaderT (ctx arch) (State (CGenState arch)) a }
  deriving (Functor, Applicative, Monad)

withinContext ::
  (outer arch -> inner arch) ->
  CGenM inner arch a ->
  CGenM outer arch a
withinContext f (CGenM m) = CGenM (withReaderT f m)

runCGenM :: Memory (ArchAddrWidth arch) ->
            CGenM CGenGlobalContext arch a ->
            (a, CGenState arch)
runCGenM mem (CGenM m) = runState (runReaderT m (CGenGlobalContext mem)) st0
  where
    st0 = CGenState { _nextFreeTyVar = 0
                    , _nextFreeRowVar = 0
                    , _assignTyVars  = mempty
                    , _tyVarTypes    = mempty
                    , _warnings      = mempty
                    , _constraints   = mempty
                    }


-- ------------------------------------------------------------
-- Monad operations

-- | Warn about a missing feature (for example).
warn :: String -> CGenM ctx arch ()
warn s = CGenM $ warnings <>= [Warning s]

-- '<<+=' increment var, return old value

-- | Returns a fresh type var.
freshTyVar :: String -> CGenM ctx arch TyVar
freshTyVar context =
  CGenM $ do
    tv <- nextFreeTyVar <<+= 1
    let tyv = TyVar tv context
    return tyv

-- | Returns a fresh row var. N.B. IMPORTANT: this should be used for any record types
-- in constraints to represent possible additional fields. And... remove `_` prefix
-- when doing so =)
_freshRowVar :: CGenM ctx arch RowVar
_freshRowVar = CGenM $ RowVar <$> (nextFreeRowVar <<+= 1)

-- | Converts the limited `Constraint` grammar to the more general
-- `TyConstraint` type used for constraint solving.
tyConstraint ::
  -- | Size of pointers
  Int ->
  Constraint ->
  CGenM ctx arch TyConstraint
tyConstraint ptrSize = \case
  CEq t1 t2 -> pure $ eqTC t1 t2
  CAddrWidthAdd ret lhs rhs -> maybePtrBinop ret lhs rhs
  CAddrWidthSub ret lhs rhs -> maybePtrBinop ret lhs rhs
  CBVNotPtr sz t -> pure $ eqTC t (NumTy sz)
  CIsPtr sz t ->
    if sz == ptrSize
      then do
        pointee <- UnknownTy <$> freshTyVar "pointee"
        pointeePointee <- UnknownTy <$> freshTyVar "pointee pointee"
        pure $ andTC [ eqTC t (PtrTy pointee)
                     , orTC [ eqTC pointee (NumTy sz)
                            , eqTC pointee (PtrTy pointeePointee)
                            ]
                     ]
      else pure $ eqTC t (PtrTy (NumTy sz))
  CAmbiguousBV t -> do
    tv <- UnknownTy <$> freshTyVar "ambiguous bitvector"
    pure $ orTC [eqTC t (NumTy ptrSize), eqTC t (PtrTy tv)]
  where
    maybePtrBinop ret lhs rhs = do
      pteeIn <- UnknownTy <$> freshTyVar "pointee (in)"
      pteeOut <- UnknownTy <$> freshTyVar "pointee (out)"
      pure $ orTC
        [ andTC [eqTC ret (NumTy ptrSize), eqTC lhs (NumTy ptrSize), eqTC rhs (NumTy ptrSize)]
        , andTC [eqTC ret (PtrTy pteeOut), eqTC lhs (PtrTy pteeIn),  eqTC rhs (NumTy ptrSize)]
        , andTC [eqTC ret (PtrTy pteeOut), eqTC lhs (NumTy ptrSize), eqTC rhs (PtrTy pteeIn)]
        ]

atFnAssignId ::
  BSC.ByteString ->
  FnAssignId ->
  Lens' (CGenState arch) (Maybe TyVar)
atFnAssignId fn aId = assignTyVars . at fn . non Map.empty . at aId


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
  mtv <- CGenM $ use (atFnAssignId fn aId)
  case mtv of
    Just tv -> pure tv
    Nothing -> do
      tyv <- freshTyVar (show aId <> " in " <> show fn)
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
  mTyVar <- CGenM $ use (atFnAssignId fn aId)
  case mTyVar of
    Nothing -> tyVarForAssignId fn aId
    Just tyVar -> pure tyVar

-- | Returns the associated type for a function assignment id, if any.
-- Otherwise, returns its type variable as an `ITy`.
assignIdTypeFor :: BSC.ByteString -> FnAssignId -> CGenM ctx arch ITy
assignIdTypeFor fn aId = do
  tyVar <- assignIdTyVar fn aId
  CGenM $ use (tyVarTypes . at tyVar) <&> fromMaybe (varITy tyVar)

assignIdType :: FnAssignId -> CGenM CGenBlockContext arch ITy
assignIdType aId = do
  fn <- askContext (cgenFunctionContext . cgenCurrentFunName)
  assignIdTypeFor fn aId

assignmentType :: FnAssignment arch tp -> CGenM CGenBlockContext arch ITy
assignmentType = assignIdType . fnAssignId

-- We lump returns and assigns into the same map
funRetType :: FnReturnVar tp -> CGenM CGenBlockContext arch ITy
funRetType = assignIdType . frAssignId

updFunRetType :: FnReturnVar tp -> ITy -> CGenM CGenBlockContext arch ()
updFunRetType fr ty = do
  fn <- askContext (cgenFunctionContext . cgenCurrentFunName)
  let aId = frAssignId fr
  tyVar <- assignIdTyVar fn aId
  CGenM $ tyVarTypes . at tyVar ?= ty

phiType :: FnPhiVar arch tp -> CGenM CGenBlockContext arch ITy
phiType = assignIdType . unFnPhiVar

argumentType :: Int -> CGenM CGenBlockContext arch ITy
argumentType i = do
  tys <- funTypeArgs <$> askContext (cgenFunctionContext . cgenCurrentFun)
  case tys ^? ix i of
    Nothing -> error "Missing argument"
    Just ty -> pure (UnknownTy ty)

emitConstraint :: Constraint -> CGenM ctx arch ()
emitConstraint c = CGenM $ constraints %= (:) c

askContext :: Getting a (ctx arch) a -> CGenM ctx arch a
askContext = CGenM . ask . view

addrWidth :: CGenM CGenBlockContext arch (NatRepr (ArchAddrWidth arch))
addrWidth = memWidth <$> askContext ( cgenFunctionContext . cgenModuleContext
                                    . cgenGlobalContext . cgenMemory)

-- currentOffset :: CGenM ctx arch (ArchAddrWord arch)
-- currentOffset = CGenM $ use curOffset

-- setCurrentOffset :: ArchAddrWord arch -> CGenM ctx arch ()
-- setCurrentOffset off = CGenM $ curOffset .= off

currentFunType :: CGenM CGenBlockContext arch (FunType arch)
currentFunType = askContext (cgenFunctionContext . cgenCurrentFun)

phisForBlock :: FnBlockLabel (ArchAddrWidth arch)
             -> CGenM CGenBlockContext arch [ITy]
phisForBlock blockAddr =
  fromMaybe (error "Missing phi type")
    . Map.lookup blockAddr
    <$> askContext (cgenFunctionContext . cgenBlockPhiTypes)

funTypeAtAddr :: forall arch.
  FnValue arch (BVType (ArchAddrWidth arch)) ->
  CGenM CGenBlockContext arch (Maybe (FunType arch))
funTypeAtAddr saddr = do
  moduleContext <- askContext (cgenFunctionContext . cgenModuleContext)
  let ftypes    = view cgenFunTypes moduleContext
  let extftypes = view cgenExtFunTypes moduleContext
  let mem       = view (cgenGlobalContext . cgenMemory) moduleContext
  let aWidth    = memAddrWidth mem

  case saddr of
    FnConstantValue _ v -> do
      -- c.f. RegisterUse.x86CallRegs
      let faddr = addrWidthClass aWidth $
                  absoluteAddr (fromInteger v)
      -- FIXME
      pure (flip Map.lookup ftypes =<< asSegmentOff mem faddr)
    FnFunctionEntryValue _ fn -> pure (Map.lookup fn extftypes)
    _ -> pure Nothing


-- -----------------------------------------------------------------------------
-- Imported API

-- The representation of constraints is still being figured out, so
-- these functions abstract over the precise representation.

-- ------------------------------------------------------------
-- Constraints

emitEq :: ITy -> ITy -> CGenM ctx arch ()
emitEq t1 t2 = emitConstraint (CEq t1 t2)

-- | Emits an add which may be a pointer add
emitPtrAdd :: ITy -> ITy -> ITy -> CGenM ctx arch ()
emitPtrAdd rty t1 t2 = emitConstraint (CAddrWidthAdd rty t1 t2)

-- | Emits a sub which may return a pointer
emitPtrSub :: ITy -> ITy -> ITy -> CGenM ctx arch ()
emitPtrSub rty t1 t2 = emitConstraint (CAddrWidthSub rty t1 t2)

-- | Emits a constraint that the argument isn't a pointer
emitNotPtr :: Int -> ITy -> CGenM ctx arch ()
emitNotPtr sz t = emitConstraint (CBVNotPtr sz t)

emitPtr ::
  -- | Bit size of the pointee
  Int ->
  -- | Type that is recognized as pointer
  ITy ->
  CGenM ctx arch ()
emitPtr pointeeSize pointer = emitConstraint (CIsPtr pointeeSize pointer)

-- -----------------------------------------------------------------------------
-- Core algorithm

genFnValue :: FnArchConstraints arch => FnValue arch tp -> CGenM CGenBlockContext arch ITy
genFnValue v =
  case v of
    FnUndefined {}          -> punt
    FnConstantBool {}       -> punt
    FnConstantValue {}      -> punt
    FnAssignedValue a       -> assignmentType a
    FnPhiValue phiv         -> phiType phiv
    FnReturn frv            -> funRetType frv
    FnFunctionEntryValue {} -> punt
    FnArg i _               -> argumentType i
  where
    punt = do
      warn "Punting on FnValue"
      varITy <$> freshTyVar (show v)

-- | Generate constraints for an App.  The first argument is the
-- output (result) type.
genApp ::
  FnArchConstraints arch =>
  (ITy, Int) -> App (FnValue arch) tp -> CGenM CGenBlockContext arch ()
genApp (ty, outSize) app =
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
        (join $ emitPtrAdd ty <$> genFnValue l <*> genFnValue r)
      -- non-64 bit adds can't generate a pointer so we don't need to
      -- generate that constraint

    -- FIXME: should this be considered another add?
    BVAdc _ l r _c -> nonptrBinOp l r

    BVSub sz l r -> do
      addrw <- addrWidth
      when (isJust (testEquality addrw sz))
        (join $ emitPtrSub ty <$> genFnValue l <*> genFnValue r)

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
    vEq :: forall arch tp tp'.
      FnArchConstraints arch =>
      FnValue arch tp -> FnValue arch tp' -> CGenM CGenBlockContext arch ()
    vEq l r = do
      l_ty <- genFnValue l
      r_ty <- genFnValue r
      emitEq l_ty r_ty
      emitEq ty l_ty -- r_ty == ty by transitivity, so we don't add it

    nonptrUnOp :: forall n arch.
      FnArchConstraints arch =>
      FnValue arch (BVType n) -> CGenM CGenBlockContext arch ()
    nonptrUnOp v = do
      emitNotPtr (bvWidth v) =<< genFnValue v
      emitNotPtr outSize ty

    -- | The result and arguments have to be bitvecs (i.e., not ptrs).
    -- We don't relate the sizes as that is given by the macaw type at
    -- the moment.
    nonptrBinOp :: forall n m arch.
      FnArchConstraints arch =>
      FnValue arch (BVType n) -> FnValue arch (BVType m) -> CGenM CGenBlockContext arch ()
    nonptrBinOp l r = do
      emitNotPtr (bvWidth l) =<< genFnValue l
      emitNotPtr (bvWidth r) =<< genFnValue r
      emitNotPtr outSize ty

    nonptrBinCmp :: forall n m arch.
      FnArchConstraints arch =>
      FnValue arch (BVType n) -> FnValue arch (BVType m) -> CGenM CGenBlockContext arch ()
    nonptrBinCmp l r = do
      emitNotPtr (bvWidth l) =<< genFnValue l
      emitNotPtr (bvWidth r) =<< genFnValue r


bvWidth :: FnArchConstraints arch => FnValue arch (BVType n) -> Int
bvWidth = bitWidth . typeRepr


genFnAssignment ::
  FnArchConstraints arch =>
  FnAssignment arch tp -> CGenM CGenBlockContext arch ()
genFnAssignment a = do
  fn <- askContext (cgenFunctionContext . cgenCurrentFunName)
  ty <- varITy <$> tyVarForAssignId fn (fnAssignId a)
  case fnAssignRhs a of
    FnSetUndefined {} -> pure () -- no constraints generated
    FnReadMem ptr sz -> do
      emitPtr (bitWidth sz) =<< genFnValue ptr
      emitEq ty ty
    FnCondReadMem sz _cond ptr def -> do
      emitPtr (8 * fromIntegral (memReprBytes sz)) =<< genFnValue ptr
      emitEq ty =<< genFnValue def

    FnEvalApp app -> genApp (ty, bitWidth (typeRepr app)) app

    FnEvalArchFn _afn -> warn "ignoring EvalArchFn"


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
  case stmt of
    FnComment _ -> pure ()
    FnAssignStmt a -> genFnAssignment a
    FnWriteMem addr v -> emitPtr (bitWidth (typeRepr v)) =<< genFnValue addr
    FnCondWriteMem _cond addr v _ -> emitPtr (bitWidth (typeRepr v)) =<< genFnValue addr
    FnCall fn args m_rv -> genCall fn args m_rv
    FnArchStmt _astmt   -> warn "Ignoring FnArchStmt"

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
  FnJumpTarget arch -> CGenM CGenBlockContext arch ()
genBlockTransfer tgt = do
  phiTys <- phisForBlock (fnJumpLabel tgt)
  let phiVals = V.toList (fnJumpPhiValues tgt)
  zipWithM_ go phiVals phiTys
  where
    go (Some v) ty = emitEq ty =<< genFnValue v

genCall :: FnArchConstraints arch =>
           FnValue arch (BVType (ArchAddrWidth arch)) ->
           -- | arguments
           [ Some (FnValue arch) ] ->
           -- | Name of return value
           Maybe (Some FnReturnVar) ->
           CGenM CGenBlockContext arch ()
genCall fn args m_ret = do
  m_ftyp <- funTypeAtAddr fn

  case m_ftyp of
    Nothing -> warn "Couldn't determine target of call"
    Just ftyp -> do
      -- Arguments
      zipWithM_ go args (funTypeArgs ftyp)

      -- Return
      case (m_ret, funTypeRet ftyp) of
        (Just (Some rv), Just rty) -> updFunRetType rv (UnknownTy rty)
        (Just _, _) -> warn "Missing return type"
        _ -> pure ()

  where
    go (Some v) ty = emitEq (UnknownTy ty) =<< genFnValue v

genFnBlock ::
  FnArchConstraints arch =>
  -- | Current block
  FnBlock arch ->
  CGenM CGenFunctionContext arch ()
genFnBlock b = do
  withinContext CGenBlockContext $ do
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
        fty <- currentFunType
        case (m_v, funTypeRet fty) of
          (Just (Some v), Just ty) -> emitEq (UnknownTy ty) =<< genFnValue v
          (Nothing, Nothing) -> pure ()
          _ -> warn "Mismatch between return type and return value"

      FnTailCall fn args -> do
        fty <- currentFunType
        m_calledFTy <- funTypeAtAddr fn
        genCall fn args Nothing
        case (funTypeRet fty, funTypeRet <$> m_calledFTy) of
          (_, Nothing) -> warn "Unknown function"
          (Just rty, Just (Just rty')) -> emitEq (UnknownTy rty) (UnknownTy rty')
          (Just _, _) -> warn "Mismatch between return type and return type in tail call"
          _ -> pure ()

genFunction ::
  FnArchConstraints arch =>
  -- Map (FnBlockLabel (ArchAddrWidth arch)) [ITy] ->
  Function arch -> CGenM CGenModuleContext arch ()
genFunction fn {- blockPhis -} = do
  cFun <-
    Map.findWithDefault (error "Missing function") (fnAddr fn)
    <$> askContext cgenFunTypes
  let mkPhis b =
        let phiVars = viewSome unFnPhiVar <$> V.toList (fbPhiVars b) in
        let mkPhiVar aId = varITy <$> tyVarForAssignId (fnName fn) aId in
        (,) (fbLabel b) <$> mapM mkPhiVar phiVars
  bphis <- Map.fromList <$> mapM mkPhis (fnBlocks fn)
  withinContext
    (CGenFunctionContext cFun (fnName fn) bphis)
    (mapM_ genFnBlock (fnBlocks fn))

-- Allocates TyVars for the arguments and return type
functionTypeToFunType :: FunctionType arch -> CGenM ctx arch (FunType arch)
functionTypeToFunType ft = do
  args <- replicateM (length (fnArgTypes ft)) (freshTyVar "arg of some function")
  ret  <- traverse (const (freshTyVar "some return type")) (fnReturnType ft)
  pure (FunType args ret)

data ModuleConstraints arch = ModuleConstraints
  { -- | Types for global functions
    mcFunTypes :: Map (ArchSegmentOff arch) (FunType arch)
    -- | Types for the named external functions
  , mcExtFunTypes :: Map BSC.ByteString (FunType arch)
    -- | A mapping from `FnAssignId` to their known type (either a fresh type
    -- variable, or the type witnessed at function call boundaries), for each
    -- function.  Because `FnAssignId` are only unique per-function, the mapping
    -- is on a per-function basis (using the `ByteString` name of the function
    -- as key).
  , mcAssignTyVars :: Map BSC.ByteString (Map FnAssignId TyVar)
    -- | Warnings gathered during constraint generation
  , mcWarnings :: [Warning]
    -- | The actual constraints
  , mcConstraints :: [TyConstraint]
    -- | The final mapping of type variables to their inferred type
  , mcTypeMap :: Map TyVar FTy
}


showInferredTypes :: ModuleConstraints arch -> String
showInferredTypes mc =
  unlines (showMapping <$> Map.assocs (mcTypeMap mc))
  where
    showMapping :: (TyVar, FTy) -> String
    showMapping (tv, ty) = concat [ show (PP.pretty tv), " : ", show ty ]


genModuleConstraints ::
  FnArchConstraints arch =>
  FoldableF (FnArchStmt arch) =>
  FoldableFC (ArchFn arch) =>
  RecoveredModule arch ->
  Memory (ArchAddrWidth arch) ->
  ModuleConstraints arch
genModuleConstraints m mem = fst $ runCGenM mem $ do
  -- allocate type variables for functions without types
  -- FIXME: we currently ignore hints

  let doDecl d = do
        fty <- functionTypeToFunType (funDeclType d)
        pure ((funDeclAddr d, fty), (funDeclName d, fty))

  (declAddrs, declSyms) <- unzip <$> mapM doDecl (recoveredDecls m)
  -- FIXME: how do symbolic calls work?
  defAddrs <- mapM (\f -> (,) (fnAddr f) <$> functionTypeToFunType (fnType f)) (recoveredDefs m)

  let symMap = Map.fromList declSyms -- FIXME: not sure these are the correct functinos
      addrMap = Map.fromList (defAddrs ++ declAddrs)

  withinContext
    (CGenModuleContext addrMap symMap)
    (mapM_ genFunction (recoveredDefs m))

  -- FIXME: abstract
  tyVars <- CGenM $ use assignTyVars
  warns <- CGenM $ use warnings

  cstrs <- mapM (tyConstraint (widthVal (memWidth mem))) =<< CGenM (use constraints)

  pure ModuleConstraints { mcFunTypes     = addrMap
                         , mcExtFunTypes  = symMap
                         , mcAssignTyVars = tyVars
                         , mcWarnings     = warns
                         , mcConstraints  = cstrs
                         , mcTypeMap      = unifyConstraints cstrs
                         }
