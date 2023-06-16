{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Solver.Monad (
  addCondEq,
  addEqC,
  addEqRowC,
  addRowExprEq,
  addSubRow,
  addSubType,
  addTyVarEq,
  addTyVarEq',
  condEnabled,
  Conditional (..),
  Conditional',
  Conjunction (..),
  ConstraintSolvingState (..),
  defineRowVar,
  defineTyVar,
  Disjunction (..),
  freshRowVar,
  freshRowVarFM,
  lookupRowExpr,
  lookupRowExprRep,
  lookupTyVar,
  lookupTyVarRep,
  Pattern (..),
  PatternRHS (..),
  popField,
  ppConstraintSolvingStateProvs,
  Schematic (..),
  SolverM,
  traceConstraintOrigins,
  traceUnification,
  undefineRowVar,
  undefineTyVar,
  unsafeUnifyRowVars,
  unsafeUnifyTyVars,
  freshTyVar,
  freshTyVar',
  runSolverM,
  ptrWidthNumTy,
  withFresh,
) where

import Control.Lens (Lens', use, (%%=), (%=), (.=), (<<+=))
import Control.Monad.State (MonadState, State, evalState)
import Data.Foldable (asum)
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Prettyprinter qualified as PP

import Reopt.TypeInference.Solver.Constraints (
  ConstraintProvenance,
  EqC (EqC),
  EqRowC (EqRowC),
  SubRowC,
  SubTypeC,
  ppEqCWithProv,
  pattern (:<:),
 )
import Reopt.TypeInference.Solver.RowVariables (
  FieldMap,
  Offset,
  RowExpr (RowExprShift, RowExprVar),
  RowInfo (..),
  RowVar (RowVar),
  rowExprShift,
  rowExprVar,
 )
import Reopt.TypeInference.Solver.TypeVariables (TyVar (TyVar))
import Reopt.TypeInference.Solver.Types (
  ITy (..),
  ITy',
  TyF (..),
 )
import Reopt.TypeInference.Solver.UnionFindMap (UnionFindMap)
import Reopt.TypeInference.Solver.UnionFindMap qualified as UM

type Conditional' = Conditional ([EqC], [EqRowC])

data ConstraintSolvingState = ConstraintSolvingState
  { ctxEqCs :: [EqC]
  , ctxEqRowCs :: [EqRowC]
  , ctxCondEqs :: [Conditional']
  , ctxSubRowCs :: [SubRowC]
  , ctxSubTypeCs :: [SubTypeC]
  , nextTraceId :: Int
  , nextRowVar :: Int
  , nextTyVar :: Int
  , ptrWidth :: Int
  -- ^ The width of a pointer, in bits.  This can go away when
  -- tyvars have an associated size, it is only used for PtrAddC
  -- solving.
  , ctxTyVars :: UnionFindMap TyVar TyVar ITy'
  -- ^ The union-find data-structure mapping each tyvar onto its
  -- representative tv.  If no mapping exists, it is a self-mapping.
  , ctxRowVars :: UnionFindMap RowVar RowInfo (FieldMap TyVar)
  , -- Debugging
    ctxTraceUnification :: Bool
  , ctxTraceConstraintOrigins :: Bool
  }
  deriving (Generic)

emptyContext :: Int -> Bool -> Bool -> ConstraintSolvingState
emptyContext w trace orig =
  ConstraintSolvingState
    { ctxEqCs = []
    , ctxEqRowCs = []
    , ctxCondEqs = []
    , ctxSubRowCs = []
    , ctxSubTypeCs = []
    , nextTraceId = 0
    , nextRowVar = 0
    , nextTyVar = 0
    , ptrWidth = w
    , ctxTyVars = UM.empty
    , ctxRowVars = UM.empty
    , ctxTraceUnification = trace
    , ctxTraceConstraintOrigins = orig
    }

newtype SolverM a = SolverM
  { getSolverM :: State ConstraintSolvingState a
  }
  deriving (Applicative, Functor, Monad, MonadState ConstraintSolvingState)

runSolverM :: Bool -> Bool -> Int -> SolverM a -> a
runSolverM b o w = flip evalState (emptyContext w b o) . getSolverM

--------------------------------------------------------------------------------
-- Adding constraints

addEqC :: EqC -> SolverM ()
addEqC eqc = field @"ctxEqCs" %= (eqc :)

addTyVarEq :: ConstraintProvenance -> TyVar -> ITy -> SolverM ()
addTyVarEq prov tv1 tv2 = addEqC (EqC tv1 tv2 prov)

addTyVarEq' :: ConstraintProvenance -> TyVar -> TyVar -> SolverM ()
addTyVarEq' prov tv1 tv2 = addTyVarEq prov tv1 (VarTy tv2)

addEqRowC :: EqRowC -> SolverM ()
addEqRowC eqc = field @"ctxEqRowCs" %= (eqc :)

addRowExprEq :: RowExpr -> RowExpr -> SolverM ()
addRowExprEq r1 r2 = addEqRowC (EqRowC r1 r2)

addCondEq :: Conditional' -> SolverM ()
addCondEq cs =
  field @"ctxCondEqs" %= (cs :)

addSubType :: TyVar -> TyVar -> SolverM ()
addSubType a b = field @"ctxSubTypeCs" %= ((a :<: b) :)

addSubRow :: RowExpr -> RowExpr -> SolverM ()
addSubRow a b = field @"ctxSubRowCs" %= ((a :<: b) :)

--------------------------------------------------------------------------------
-- Getting constraints

popField :: Lens' ConstraintSolvingState [a] -> SolverM (Maybe a)
popField fld =
  fld %%= \case
    [] -> (Nothing, [])
    (c : cs) -> (Just c, cs)

_dequeueEqC :: SolverM (Maybe EqC)
_dequeueEqC = popField (field @"ctxEqCs")

_dequeueEqRowC :: SolverM (Maybe EqRowC)
_dequeueEqRowC = popField (field @"ctxEqRowCs")

_dequeueSubTypeC :: SolverM (Maybe SubTypeC)
_dequeueSubTypeC = popField (field @"ctxSubTypeCs")

--------------------------------------------------------------------------------
-- Operations over type variable state

freshRowVar :: SolverM RowVar
freshRowVar = RowVar <$> (field @"nextRowVar" <<+= 1)

_freshRowVarE :: RowExpr -> SolverM RowVar
_freshRowVarE e = do
  rowv <- freshRowVar
  unsafeUnifyRowVars e rowv
  pure rowv

freshRowVarFM :: FieldMap TyVar -> SolverM RowVar
freshRowVarFM fm = do
  rowv <- freshRowVar
  defineRowVar rowv fm
  pure rowv

-- | @unsafeUnifyRowVars root leaf@ will make @root@ the new equiv. rep
-- for @leaf@.  Note that both root and leaf should be the reps. of
-- their corresponding equivalence classes.
unsafeUnifyRowVars :: RowExpr -> RowVar -> SolverM ()
unsafeUnifyRowVars root leaf = field @"ctxRowVars" %= UM.unify ki leaf
 where
  ki = RowInfo{riShift = rowExprShift root, riRowVar = rowExprVar root}

-- | Always define a row variable, even if it has a def.
defineRowVar :: RowVar -> FieldMap TyVar -> SolverM ()
defineRowVar rowv fm = field @"ctxRowVars" %= UM.insert rowv fm

undefineRowVar :: RowVar -> SolverM ()
undefineRowVar rv = field @"ctxRowVars" %= UM.delete rv

lookupRowVarRep :: RowVar -> SolverM (Offset, RowVar)
lookupRowVarRep rv = do
  ri <- field @"ctxRowVars" %%= UM.lookupRep rv
  pure (riShift ri, riRowVar ri)

lookupRowExprRep :: RowExpr -> SolverM RowExpr
lookupRowExprRep re = do
  (o, rv) <- lookupRowVarRep (rowExprVar re)
  let o' = o + rowExprShift re
  pure (RowExprShift o' rv)

lookupRowVar :: RowVar -> SolverM (Offset, RowVar, Maybe (FieldMap TyVar))
lookupRowVar rv = do
  (ri, fm) <- field @"ctxRowVars" %%= UM.lookup rv
  pure (riShift ri, riRowVar ri, fm)

lookupRowExpr :: RowExpr -> SolverM (RowExpr, Maybe (FieldMap TyVar))
lookupRowExpr re = do
  (o, rv, m_fm) <- lookupRowVar (rowExprVar re)
  let o' = o + rowExprShift re
  pure (RowExprShift o' rv, m_fm)

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class.  This also updates the eqv. map to
-- amortise lookups.
lookupTyVarRep :: TyVar -> SolverM TyVar
lookupTyVarRep tv0 = field @"ctxTyVars" %%= UM.lookupRep tv0

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class, and the definition for that type
-- var, if any.
lookupTyVar :: TyVar -> SolverM (TyVar, Maybe ITy')
lookupTyVar tv = field @"ctxTyVars" %%= UM.lookup tv

-- | Always return a new type variable.
freshTyVar' :: Maybe String -> SolverM TyVar
freshTyVar' orig = flip TyVar orig <$> (field @"nextTyVar" <<+= 1)

freshTyVar :: Maybe String -> Maybe ITy -> SolverM TyVar
freshTyVar orig Nothing = freshTyVar' orig
freshTyVar _orig (Just (VarTy v)) = pure v -- Don't allocate, just return the equiv. var.
freshTyVar orig (Just (ITy ty)) = do
  tyv <- freshTyVar' orig
  defineTyVar tyv ty
  pure tyv

-- | Always define a type variable, even if it has a def.
defineTyVar :: TyVar -> ITy' -> SolverM ()
defineTyVar tyv ty = field @"ctxTyVars" %= UM.insert tyv ty

undefineTyVar :: TyVar -> SolverM ()
undefineTyVar ty = field @"ctxTyVars" %= UM.delete ty

-- | @unsafeUnifyTyVars root leaf@ will make @root@ the new equiv. rep
-- for @leaf@.  Note that both root and leaf should be the reps. of
-- their corresponding equivalence classes.
unsafeUnifyTyVars :: TyVar -> TyVar -> SolverM ()
unsafeUnifyTyVars root leaf = field @"ctxTyVars" %= UM.unify root leaf

--------------------------------------------------------------------------------
-- Other stuff

ptrWidthNumTy :: SolverM ITy'
ptrWidthNumTy = NumTy <$> use (field @"ptrWidth")

_setTraceUnification :: Bool -> SolverM ()
_setTraceUnification b = field @"ctxTraceUnification" .= b

traceUnification :: SolverM Bool
traceUnification = use (field @"ctxTraceUnification")

traceConstraintOrigins :: SolverM Bool
traceConstraintOrigins = use (field @"ctxTraceConstraintOrigins")

--------------------------------------------------------------------------------
-- Conditional constraints

data Schematic a = Schematic a | DontCare

instance PP.Pretty a => PP.Pretty (Schematic a) where
  pretty (Schematic a) = "?" <> PP.pretty a
  pretty DontCare = "_"

-- data Let v e = Let { letBinder :: Schematic v, letBody :: e }
-- instance (PP.Pretty v, PP.Pretty e) => PP.Pretty (Let v e) where
--   pretty (Let v e) = "let " <> PP.pretty v <> " := " <> PP.pretty e

-- | Does the definition of the tyvar on the LHS match the pattern on
-- the RHS, instantiating schematics as required.
data PatternRHS
  = IsPtr (Schematic RowVar)
  | IsNum
  | IsConflict

instance PP.Pretty PatternRHS where
  pretty (IsPtr sv) = "PtrTy " <> PP.pretty sv
  pretty IsNum = "NumTy _"
  pretty IsConflict = "ConflictTy _"

data Pattern = Pattern
  { patVar :: TyVar
  , patRHS :: PatternRHS
  }

instance PP.Pretty Pattern where
  pretty (Pattern v r) = PP.pretty v <> " =?= " <> PP.pretty r

newtype Conjunction a = Conjunction {getConjuncts :: [a]}
  deriving (Eq, Foldable, Functor, Ord, Traversable)
newtype Disjunction a = Disjunction {getDisjuncts :: [a]}
  deriving (Eq, Foldable, Functor, Ord, Traversable)

instance PP.Pretty a => PP.Pretty (Conjunction a) where
  pretty = PP.list . map PP.pretty . getConjuncts

instance PP.Pretty a => PP.Pretty (Disjunction a) where
  pretty = PP.list . map PP.pretty . getDisjuncts

-- FIXME: this is pretty arcane
data Conditional a = Conditional
  { cName :: String
  , cGuard :: Disjunction (Conjunction Pattern)
  -- ^ Disjunction of conjunction of linear patterns -- each schematic should
  -- occur on one RHS only.
  , cConstraints :: a
  }

-- | Simple matching --- the pattern matches, or it doesn't.  A more
-- advanced version of this might determine that a pattern can never
-- match.  This returns a mapping from schematics to their unifiers if
-- one exists.
condEnabled :: Conditional c -> SolverM (Maybe [EqRowC])
condEnabled c = asum <$> mapM matchAll (cGuard c)
 where
  matchAll (Conjunction pats) = fmap concat . sequenceA <$> mapM match pats
  match pat = do
    (_, m_ty) <- lookupTyVar (patVar pat)
    pure (matchRHS (patRHS pat) =<< m_ty)

  matchRHS rhs ty =
    case (rhs, ty) of
      (IsPtr sv, PtrTy r) -> Just (bindSchem sv r)
      (IsNum, NumTy _) -> Just []
      (IsConflict, ConflictTy _) -> Just []
      _ -> Nothing

  bindSchem (Schematic r) re = [EqRowC (RowExprVar r) re]
  bindSchem _ _ = []

instance PP.Pretty a => PP.Pretty (Conditional a) where
  pretty c =
    PP.vcat
      [ PP.pretty (cName c)
      , PP.pretty (cGuard c)
      , "⊢"
      , PP.pretty (cConstraints c)
      ]

-- | Pretty-print a `Conditional'` value, including constraint provenance.
ppConditional'WithProv :: Conditional' -> PP.Doc a
ppConditional'WithProv c =
  PP.vcat
    [ PP.pretty (cName c)
    , PP.pretty (cGuard c)
    , "⊢"
    , PP.tupled
        [ PP.align $ PP.list $ map ppEqCWithProv x
        , PP.pretty y
        ]
    ]
 where
  (x, y) = cConstraints c

class CanFresh t where
  makeFresh :: SolverM t

instance CanFresh RowVar where
  makeFresh = freshRowVar

instance CanFresh RowExpr where
  makeFresh = RowExprVar <$> freshRowVar

instance CanFresh TyVar where
  makeFresh = freshTyVar Nothing Nothing

instance CanFresh a => CanFresh (Schematic a) where
  makeFresh = Schematic <$> makeFresh

class WithFresh t where
  type Result t
  withFresh :: t -> SolverM (Result t)

instance WithFresh (SolverM a) where
  type Result (SolverM a) = a
  withFresh m = m

instance (CanFresh a, WithFresh b) => WithFresh (a -> b) where
  type Result (a -> b) = Result b
  withFresh f = do
    v <- makeFresh
    withFresh (f v)

instance WithFresh EqC where
  type Result EqC = EqC
  withFresh = pure

--------------------------------------------------------------------------------
-- Instances

instance PP.Pretty ConstraintSolvingState where
  pretty ctx =
    PP.vsep $
      section "Equalities" (map PP.pretty $ ctxEqCs ctx)
        <> section "Row equalities" (map PP.pretty $ ctxEqRowCs ctx)
        <> section "Conditionals" (map PP.pretty $ ctxCondEqs ctx)
        <> section "Sub-row constraints" (map PP.pretty $ ctxSubRowCs ctx)
        <> section "Sub-type constraints" (map PP.pretty $ ctxSubTypeCs ctx)
        <> [ PP.pretty (ctxTyVars ctx)
           , PP.pretty (ctxRowVars ctx)
           ]

-- | Pretty-print a 'ConstraintSolvingState', including provenance information.
ppConstraintSolvingStateProvs :: ConstraintSolvingState -> PP.Doc a
ppConstraintSolvingStateProvs ctx =
  PP.vsep $
    section "Equality constraints" (map ppEqCWithProv $ ctxEqCs ctx)
      -- This produces an overwhelming amount of output, so it is disabled by
      -- default. Be prepared if you choose to opt into this.
      <> section "Conditionals" (map ppConditional'WithProv $ ctxCondEqs ctx)

section :: PP.Doc ann -> [PP.Doc ann] -> [PP.Doc ann]
section title entries =
  [ title <> ":"
  , PP.indent 2 $ PP.list entries
  ]

_shiftOffsets :: Offset -> Map Offset v -> Map Offset v
_shiftOffsets 0 m = m
_shiftOffsets o m =
  Map.fromList [(k - o, v) | (k, v) <- Map.toList m]
