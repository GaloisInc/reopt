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
  ( FunType(..)
  , Ty, FTy
  , ModuleConstraints(..)
  , genModuleConstraints
  , showInferredTypes
  ) where

import           Control.Lens                    (At (at), Getting, Ixed (ix),
                                                  Lens', makeLenses, non, use,
                                                  view, (%=), (<&>), (<<+=),
                                                  (<>=), (?=), (^?))
import           Control.Monad.Reader            (MonadReader (ask),
                                                  ReaderT (..), join, when,
                                                  withReaderT, zipWithM_)
import           Control.Monad.State.Strict      (State, runState)
import qualified Data.ByteString.Char8           as BSC
import           Data.Foldable                   (traverse_)
import           Data.List                       (intercalate)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, isJust)
import           Data.Proxy                      (Proxy (Proxy))
import           Data.Traversable                (for)
import qualified Data.Vector                     as V
import qualified Prettyprinter                   as PP

import           Data.Macaw.CFG                  (App (..), ArchAddrWidth,
                                                  ArchFn, ArchSegmentOff)
import           Data.Macaw.Memory               (Memory, absoluteAddr,
                                                  addrWidthClass,
                                                  addrWidthNatRepr,
                                                  addrWidthRepr, asSegmentOff,
                                                  memAddrWidth, memWidth)
import           Data.Macaw.Types                (BVType, TypeRepr (..),
                                                  floatInfoBits, typeRepr)
import           Data.Parameterized              (FoldableF, FoldableFC)
import           Data.Parameterized.NatRepr      (NatRepr, intValue,
                                                  testEquality, widthVal)
import           Data.Parameterized.Some         (Some (Some), viewSome)
import           Reopt.CFG.FnRep                 (FnArchConstraints, FnArchStmt,
                                                  FnAssignId,
                                                  FnAssignRhs (..),
                                                  FnAssignment (..),
                                                  FnBlock (..), FnBlockLabel,
                                                  FnJumpTarget (..),
                                                  FnPhiVar (unFnPhiVar),
                                                  FnReturnVar (frAssignId),
                                                  FnStmt (..),
                                                  FnTermStmt (FnBranch, FnJump, FnLookupTable, FnRet, FnTailCall),
                                                  FnValue (..), Function (..),
                                                  FunctionDecl (..),
                                                  FunctionType (..),
                                                  RecoveredModule (..),
                                                  fnBlocks)
import           Reopt.TypeInference.Constraints.Solving
  (TyVar, Ty, FTy, varTy, eqTC, ptrTC, SolverM
  , runSolverM, numTy, unifyConstraints)
import qualified Reopt.TypeInference.Constraints.Solving as S
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (lift)

-- This algorithm proceeds in stages:
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

-- varITy :: TyVar -> ITy
-- varITy = UnknownTy

-- -- This is the free type for constraints, should be replace by the
-- --  type from the constraint solver.

-- data PointerInformation
--   = NotAPointer
--   | CouldBeAPointer
--   | IsAPointerTo
--     Int -- ^ Pointee size
--     ITy -- ^ Pointee type

-- data BVInformation
--   = BVNotAddrWidth
--   | BVAddrWidth PointerInformation

-- data Constraint
--   = CEq -- ^ The types must be equal
--     ITy ITy
--   | CAddrWidthAdd -- ^ At most one type may be a pointer
--     ITy -- ^ Result
--     ITy -- ^ First addend
--     (Either Integer ITy) -- ^ Second addend
--   | CAddrWidthSub -- ^ The RHS may not be a pointer if the LHS is a bitvector
--     ITy -- ^ Result
--     ITy -- ^ Minuend
--     ITy -- ^ Subtrahend
--   | CBV -- ^ Bitvector
--     Int -- ^ Bitvector size
--     BVInformation
--     ITy
--   | CPointerAndOffset
--     -- ^ @CPointerAndOffset r sz p o@ means @r@ of size @sz@ is read at address @p +
--     -- o@, indicating that either @p@ is a pointer and @o@ an offset, or @o@ is
--     -- a global address and @p@ an offset.
--     ITy             -- ^ Pointee type
--     (Some TypeRepr) -- ^ Pointee size
--     ITy             -- ^ Computed operand  (pointer or offset)
--     Integer         -- ^ Immediate operand (offset or pointer)

-- instance PP.Pretty Constraint where
--   pretty = \case
--     CEq t1 t2 -> PP.hsep [PP.pretty t1, "=", PP.pretty t2]
--     CAddrWidthAdd t1 t2 (Left o) -> PP.hsep [PP.pretty t1, "=", PP.pretty t2, "+", PP.pretty o]
--     CAddrWidthAdd t1 t2 (Right t3) -> PP.hsep [PP.pretty t1, "=", PP.pretty t2, "+", PP.pretty t3]
--     CAddrWidthSub t1 t2 t3 -> PP.hsep [PP.pretty t1, "=", PP.pretty t2, "-", PP.pretty t3]
--     CBV sz BVNotAddrWidth t -> PP.hsep [PP.pretty t, ":", "bv" <> PP.pretty sz]
--     CBV sz (BVAddrWidth NotAPointer) t -> PP.hsep [PP.pretty t, ":", "bv" <> PP.pretty sz]
--     CBV sz (BVAddrWidth CouldBeAPointer) t ->
--       PP.hsep [PP.pretty t, ":", "bv" <> PP.pretty sz <> "-or-ptr"]
--     CBV _ (BVAddrWidth (IsAPointerTo sz ty)) t ->
--       PP.hsep [PP.pretty t, ": ptr to (" <> PP.pretty ty <> ", " <> "size " <> PP.pretty sz <> ")"]
--     CPointerAndOffset tr (Some sz) tp o ->
--       PP.hsep ["*(" <> PP.pretty tp, "+", PP.pretty o <> ") : ", PP.pretty tr, "(" <> PP.pretty sz <> ")"]

-- instance Show Constraint where
--   show = show . PP.pretty

-- data TyConstraintOptions m =
--   TyConstraintOptions
--   { -- | How to generate a type to describe what a pointer points at
--     tyConGenPtrTgt :: String -> m ITy
--     -- | How to generate a row
--   , tyConGenRowVar :: m RowVar
--   }


-- | Size in bits of an unconstrained @TypeRepr tp@.  Currently only handling
-- bitvectors and floats.
anyTypeWidth :: TypeRepr tp -> Int
anyTypeWidth = \case
  BoolTypeRepr -> error "Unexpected in anyTypeWidth: BoolTypeRepr"
  BVTypeRepr sz -> fromIntegral (intValue sz)
  FloatTypeRepr fir -> fromIntegral (widthVal (floatInfoBits fir))
  TupleTypeRepr{} -> error "Unexpected in anyTypeWidth: TupleTypeRepr"
  VecTypeRepr{} -> error "Unexpected in anyTypeWidth: VecTypeRepr"


-- -- | Converts the limited `Constraint` grammar to the more general
-- -- `TyConstraint` type used for constraint solving.
-- tyConstraint ::
--   Monad m =>
--   TyConstraintOptions m ->
--   -- | Size of pointers
--   Int ->
--   Constraint -> m TyConstraint
-- tyConstraint opts ptrSz = \case
--   CEq t1 t2 -> pure $ eqTC t1 t2

--   CAddrWidthAdd ret lhs (Left o) -> do
--     -- tLHS <- tyConGenPtrTgt opts "constant addition variable input"
--     rLHS <- tyConGenRowVar opts
--     -- tRet <- tyConGenPtrTgt opts "constant addition return"
--     rRet <- tyConGenRowVar opts
--     pure $ orTC
--       -- Either everything is numeric
--       [ andTC [isNumTC ptrSz ret, isNumTC ptrSz lhs]
--       -- Or one is a pointer and one is an offset
--       , isPointerWithOffsetTC (lhs, rLHS) (ret, rRet) (Offset (fromIntegral o))
--       ]

--   CAddrWidthAdd ret lhs (Right rhs) -> do
--     ty <- tyConGenPtrTgt opts "pointer arithmetic"
--     pure $ orTC
--       -- Either everything is numeric
--       [ andTC [isNumTC ptrSz ret, isNumTC ptrSz lhs, isNumTC ptrSz rhs]
--       -- Or one is a pointer and one is an offset
--       , andTC [isPtrTC ret ty, orTC [ andTC [ isPtrTC lhs ty, isNumTC ptrSz rhs]
--                                     , andTC [ isNumTC ptrSz lhs, isPtrTC rhs ty]
--                                     ]
--               ]
--       ]

--   CAddrWidthSub ret lhs rhs -> do
--     ty <- tyConGenPtrTgt opts "pointer arithmetic"
--     pure $ orTC
--       -- Either everything is numeric
--       [ andTC [isNumTC ptrSz ret, isNumTC ptrSz lhs, isNumTC ptrSz rhs]
--       -- Or one is a pointer and one is an offset
--       , andTC [isPtrTC ret ty, orTC [ andTC [ isPtrTC lhs ty, isNumTC ptrSz rhs]
--                                     , andTC [ isNumTC ptrSz lhs, isPtrTC rhs ty]
--                                     ]
--               ]
--       ]

--   CBV sz BVNotAddrWidth t -> pure (isNumTC sz t)
--   CBV sz (BVAddrWidth NotAPointer) t -> pure (isNumTC sz t)
--   CBV sz (BVAddrWidth CouldBeAPointer) t -> bitvectorConstraint sz t
--   CBV _ (BVAddrWidth (IsAPointerTo pointeeSize pointeeType)) t -> do
--     pointeeConstraints <- bitvectorConstraint pointeeSize pointeeType
--     pointerConstraints <- isOffsetTC t 0 pointeeType <$> tyConGenRowVar opts
--     return (andTC [pointeeConstraints, pointerConstraints])
--   CPointerAndOffset fieldType fieldSize pointerType offset -> do
--     -- Essentially we are seeing an operation:
--     -- r := *(p + o)
--     -- which means either:
--     -- p is a pointer, and o a numeric offset,
--     -- or:
--     -- o is a constant pointer, and p is an offset.
--     -- freshTgtIfFieldIsPtr <- tyConGenPtrTgt opts "TODO: what to call this?"
--     row <- tyConGenRowVar opts
--     let fieldSz = viewSome anyTypeWidth fieldSize
--     pointeeConstraints <- bitvectorConstraint fieldSz fieldType
--     pure $
--       andTC
--       [ pointeeConstraints
--       , orTC
--         [ isOffsetTC pointerType (fromInteger offset) fieldType row
--         , isNumTC ptrSz pointerType
--         ]
--       ]
--  where

--     -- Emits a constraint appropriate for a bitvector of the given size, knowing
--     -- no additional information about it.
--     bitvectorConstraint sz t = do
--       if sz == ptrSz
--         then do
--           tgt <- tyConGenPtrTgt opts "potential target of ambiguous bitvector"
--           pure $ orTC [isNumTC ptrSz t, isPtrTC t tgt]
--         else pure (isNumTC sz t)

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
    _cgenBlockPhiTypes  :: Map (FnBlockLabel (ArchAddrWidth arch)) [Ty]

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
    _assignTyVars   :: Map BSC.ByteString (Map FnAssignId TyVar)
  -- | Offset of the current instruction, used (not right now) for
  -- tagging constraints and warnings.
  -- , _curOffset     :: ArchAddrWord arch
  , _warnings      :: [Warning]
}

makeLenses ''CGenState

newtype CGenM ctx arch a =
  CGenM { _getCGenM :: ReaderT (ctx arch)
                       (StateT (CGenState arch) SolverM) a }
  deriving (Functor, Applicative, Monad)

withinContext ::
  (outer arch -> inner arch) ->
  CGenM inner arch a ->
  CGenM outer arch a
withinContext f (CGenM m) = CGenM (withReaderT f m)

inSolverM :: SolverM a -> CGenM ctxt arch a
inSolverM = CGenM . lift . lift

runCGenM :: Memory (ArchAddrWidth arch) ->
            CGenM CGenGlobalContext arch a ->
            a
runCGenM mem (CGenM m) =
  runSolverM (evalStateT (runReaderT m (CGenGlobalContext mem)) st0)
  where
    st0 = CGenState { _assignTyVars  = mempty
                    , _warnings      = mempty
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
  mTyVar <- CGenM $ use (atFnAssignId fn aId)
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
  emitEq (varTy tv) (varTy tyVar) -- FIXME: subtype?

phiType :: FnPhiVar arch tp -> CGenM CGenBlockContext arch Ty
phiType = assignIdType . unFnPhiVar

argumentType :: Int -> CGenM CGenBlockContext arch Ty
argumentType i = do
  tys <- funTypeArgs <$> askContext (cgenFunctionContext . cgenCurrentFun)
  case tys ^? ix i of
    Nothing -> error "Missing argument"
    Just ty -> pure (varTy ty)

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
             -> CGenM CGenBlockContext arch [Ty]
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

--------------------------------------------------------------------------------
-- Forwarded functions (to solver)

-- | Returns a fresh type var.
freshTyVar :: String -> CGenM ctx arch TyVar
freshTyVar context =
  inSolverM (S.freshTyVar (Just context) Nothing)

emitEq :: Ty -> Ty -> CGenM ctx arch ()
emitEq t1 t2 = inSolverM (eqTC t1 t2)

-- | Emits an add which may be a pointer add
emitPtrAddSymbolic :: Ty -> Ty -> Ty -> CGenM ctx arch ()
emitPtrAddSymbolic _rty _t1 _t2 = undefined  -- FIXME

emitPtrAddOffset :: Ty -> Ty -> Ty -> Integer -> CGenM ctx arch ()
emitPtrAddOffset _rty _t1 _t2 _off = undefined  -- FIXME

-- | Emits a sub which may return a pointer
emitPtrSub :: Ty -> Ty -> Ty -> CGenM ctx arch ()
emitPtrSub _rty _t1 _t2 = undefined -- FIXME

-- | Emits a constraint that the argument isn't a pointer
emitNotPtr ::
  forall ctx arch.
  FnArchConstraints arch =>
  Int -> Ty -> CGenM ctx arch ()
emitNotPtr sz t =
  inSolverM (eqTC t (numTy sz))

pointerWidth :: forall arch. FnArchConstraints arch => Proxy arch -> Int
pointerWidth Proxy =
  widthVal (addrWidthNatRepr (addrWidthRepr (Proxy :: Proxy (ArchAddrWidth arch))))

emitPtr ::
  Ty ->
  -- | Type that is recognized as pointer
  Ty ->
  CGenM ctx arch ()
emitPtr pointee pointer =
  inSolverM (ptrTC pointee pointer)

-- emitStructPtr :: ITy -> ITy -> Integer -> Some TypeRepr -> CGenM ctx arch ()
-- emitStructPtr tr tp o sz = emitConstraint (CPointerAndOffset tr sz tp o)

-- -----------------------------------------------------------------------------
-- Core algorithm

genFnValue :: FnArchConstraints arch => FnValue arch tp -> CGenM CGenBlockContext arch Ty
genFnValue v =
  case v of
    FnUndefined {}          -> punt
    FnConstantBool {}       -> pure (numTy 1)
    FnConstantValue sz _    -> pure (numTy (widthVal sz))
    FnAssignedValue a       -> assignmentType a
    FnPhiValue phiv         -> phiType phiv
    FnReturn frv            -> funRetType frv
    FnFunctionEntryValue {} -> punt
    FnArg i _               -> argumentType i
  where
    punt = do
      warn "Punting on FnValue"
      varTy <$> freshTyVar (show (PP.pretty v))

-- | Generate constraints for an App.  The first argument is the
-- output (result) type.
genApp ::
  FnArchConstraints arch =>
  (Ty, Int) -> App (FnValue arch) tp -> CGenM CGenBlockContext arch ()
genApp (ty, outSize) app =
  case app of

    Eq l r -> do
      join (emitEq <$> genFnValue l <*> genFnValue r)
      emitNotPtr 1 ty

    Mux _ _ l r -> do
      lTy <- genFnValue l
      emitEq lTy =<< genFnValue r
      emitEq ty lTy -- ty = rTy follows by transitivity, so we don't add it

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
    BVAdd _sz l (a@(FnAssignedValue FnAssignment { fnAssignRhs = FnAddrWidthConstant o })) -> do
      pTy <- genFnValue l
      oTy <- genFnValue a
      emitPtrAddOffset ty pTy oTy o
    BVAdd _sz (a@(FnAssignedValue FnAssignment { fnAssignRhs = FnAddrWidthConstant o }) ) r -> do
      pTy <- genFnValue r
      oTy <- genFnValue a
      emitPtrAddOffset ty pTy oTy o

    BVAdd sz l r -> do
      addrw <- addrWidth
      if isJust (testEquality addrw sz)
        then join $ emitPtrAddSymbolic ty <$> genFnValue l <*> genFnValue r
        else nonptrBinOp l r

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


genMemOp ::
  FnArchConstraints arch =>
  Ty -> FnValue arch (BVType (ArchAddrWidth arch)) -> Some TypeRepr ->
  CGenM CGenBlockContext arch ()
-- FIXME: do we need sz here?  
genMemOp ty ptr _sz = emitPtr ty =<< genFnValue ptr

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
  FnArchConstraints arch =>
  FnAssignment arch tp -> CGenM CGenBlockContext arch ()
genFnAssignment a = do
  fn <- askContext (cgenFunctionContext . cgenCurrentFunName)
  ty <- varTy <$> tyVarForAssignId fn (fnAssignId a)
  case fnAssignRhs a of
    FnSetUndefined {} -> pure () -- no constraints generated
    FnReadMem ptr sz -> genMemOp ty ptr (Some sz)
    FnCondReadMem _sz _cond ptr def -> do
      genMemOp ty ptr (Some (typeRepr def))
      emitEq ty =<< genFnValue def
    FnEvalApp app -> genApp (ty, bitWidth (typeRepr app)) app
    FnEvalArchFn _afn -> warn "ignoring EvalArchFn"
    -- no constraints generated here, we will look at this assignment
    -- in context (i.e., on an add or a load).
    FnAddrWidthConstant {} -> pure ()

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
    FnWriteMem addr v -> do
      tr <- genFnValue v
      genMemOp tr addr (Some (typeRepr v))
    FnCondWriteMem _cond addr v _ -> do
      tr <- genFnValue v
      genMemOp tr addr (Some (typeRepr v))
    FnCall fn args m_rv -> genCall fn args m_rv
    FnArchStmt _astmt -> warn "Ignoring FnArchStmt"

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
        (Just (Some rv), Just rty) -> updFunRetType rv rty
        (Just _, _) -> warn "Missing return type"
        _ -> pure ()

  where
    go (Some v) ty = emitEq (varTy ty) =<< genFnValue v

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
          (Just (Some v), Just ty) -> emitEq (varTy ty) =<< genFnValue v
          (Nothing, Nothing) -> pure ()
          _ -> warn "Mismatch between return type and return value"

      FnTailCall fn args -> do
        fty <- currentFunType
        m_calledFTy <- funTypeAtAddr fn
        genCall fn args Nothing
        case (funTypeRet fty, funTypeRet <$> m_calledFTy) of
          (_, Nothing) -> warn "Unknown function"
          (Just rty, Just (Just rty')) -> emitEq (varTy rty) (varTy rty')
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
        let mkPhiVar aId = varTy <$> tyVarForAssignId (fnName fn) aId in
        (,) (fbLabel b) <$> mapM mkPhiVar phiVars
  bphis <- Map.fromList <$> mapM mkPhis (fnBlocks fn)
  withinContext
    (CGenFunctionContext cFun (fnName fn) bphis)
    (mapM_ genFnBlock (fnBlocks fn))

-- Allocates TyVars for the arguments and return type
functionTypeToFunType ::
  -- | Name of the function
  BSC.ByteString ->
  FunctionType arch -> CGenM ctx arch (FunType arch)
functionTypeToFunType fn ft = do
  let fnStr = BSC.unpack fn
  args <- for (zip (fnArgTypes ft) [(0 :: Int)..]) (\(_, i) -> freshTyVar (fnStr <> ".arg" <> show i))
  ret  <- traverse (const (freshTyVar (fnStr <> ".ret"))) (fnReturnType ft)
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
  -- , mcTyConstraints :: [TyConstraint]
    -- | The final mapping of type variables to their inferred type
  , mcTypeMap :: Map TyVar FTy
}


showInferredTypes :: ModuleConstraints arch -> String
showInferredTypes mc =
  unlines (showMapping <$> Map.assocs (mcTypeMap mc))
  where
    showMapping :: (TyVar, FTy) -> String
    showMapping (tv, ty) = concat [ show (PP.pretty tv), " : ", show (PP.pretty ty) ]


genModuleConstraints ::
  FnArchConstraints arch =>
  FoldableF (FnArchStmt arch) =>
  FoldableFC (ArchFn arch) =>
  RecoveredModule arch ->
  Memory (ArchAddrWidth arch) ->
  ModuleConstraints arch
genModuleConstraints m mem = runCGenM mem $ do
  -- allocate type variables for functions without types
  -- FIXME: we currently ignore hints

  let doDecl d = do
        fty <- functionTypeToFunType (funDeclName d) (funDeclType d)
        pure ((funDeclAddr d, fty), (funDeclName d, fty))

  (declAddrs, declSyms) <- unzip <$> mapM doDecl (recoveredDecls m)
  -- FIXME: how do symbolic calls work?
  defAddrs <- mapM (\f -> (,) (fnAddr f) <$> functionTypeToFunType (fnName f) (fnType f)) (recoveredDefs m)

  let symMap = Map.fromList declSyms -- FIXME: not sure these are the correct functinos
      addrMap = Map.fromList (defAddrs ++ declAddrs)

  withinContext
    (CGenModuleContext addrMap symMap)
    (mapM_ genFunction (recoveredDefs m))

  -- FIXME: abstract
  tyVars <- CGenM $ use assignTyVars
  warns <- CGenM $ use warnings

  tyMap <- inSolverM unifyConstraints

  pure ModuleConstraints { mcFunTypes      = addrMap
                         , mcExtFunTypes   = symMap
                         , mcAssignTyVars  = tyVars
                         , mcWarnings      = warns
                         , mcTypeMap       = tyMap
                         }
