{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Reopt.TypeInference.Solver.Monad where

import Control.Lens (Lens', use, (%%=), (%=), (.=), (<<+=), (^.))
import Control.Monad.State.Strict (MonadState, State, evalState)
import Data.Foldable (asum)
import Data.Generics.Product (field)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Prettyprinter qualified as PP
import Reopt.TypeInference.Solver.Constraints (
  EqC (..),
  EqRowC (..),
  SubRowC,
  SubTypeC,
  pattern (:<:),
 )
import Reopt.TypeInference.Solver.RowVariables (
  FieldMap,
  Offset,
  RowExpr (RowExprShift, RowExprVar),
  RowInfo (..),
  RowVar (RowVar, rowVarInt),
  rowExprShift,
  rowExprVar,
 )
import Reopt.TypeInference.Solver.TypeVariables (TyVar (TyVar, tyVarInt))
import Reopt.TypeInference.Solver.Types (
  ITy (..),
  ITy',
  TyF (..),
 )
import Reopt.TypeInference.Solver.UnionFindMap (RevocableUnionFindMap)
import Reopt.TypeInference.Solver.UnionFindMap qualified as UM

type Conditional' = Conditional ([EqC], [EqRowC])

data RevokePolicy
  = NeverRevoke
  | RevokeWith (NonEmpty TyVar)
  deriving (Eq, Ord)

revokeWith :: [TyVar] -> RevokePolicy
revokeWith [] = NeverRevoke
revokeWith (h : t) = RevokeWith (h :| t)

instance PP.Pretty RevokePolicy where
  pretty NeverRevoke = "NeverRevoke"
  pretty (RevokeWith tv) = PP.hsep ["RevokeWith", PP.pretty tv]

data Revocable a = Revocable
  { revokePolicy :: RevokePolicy
  , datum :: a
  }
  deriving (Foldable, Functor, Generic, Traversable)

instance PP.Pretty a => PP.Pretty (Revocable a) where
  pretty = PP.pretty . datum

data ConstraintSet = ConstraintSet
  { ctxEqCs :: [Revocable EqC]
  , ctxEqRowCs :: [Revocable EqRowC]
  , ctxCondEqs :: [Conditional']
  , ctxSubRowCs :: [Revocable SubRowC]
  , ctxSubTypeCs :: [Revocable SubTypeC]
  }
  deriving (Generic)

serializeConstraintSet :: ConstraintSet -> PP.Doc ()
serializeConstraintSet s =
  PP.vcat $
    map serializeRevocableEqC (s ^. field @"ctxEqCs")
      <> map serializeRevocableEqRowC (s ^. field @"ctxEqRowCs")
      <> map serializeCondEq (s ^. field @"ctxCondEqs")
      <> map serializeRevocableSubRowC (s ^. field @"ctxSubRowCs")
      <> map serializeRevocableSubTypeC (s ^. field @"ctxSubTypeCs")
 where
  serializeRowVar rv = PP.hcat ["ρ", PP.pretty (rowVarInt rv)]
  serializeTyVar tv = PP.hcat ["τ", PP.pretty (tyVarInt tv)]

  serializeITy (VarTy tv) = serializeTyVar tv
  serializeITy (ITy (NumTy i)) = PP.hcat ["num[", PP.pretty i, "]"]
  serializeITy (ITy (PtrTy p)) = PP.hcat ["*(", serializeRowExpr p, ")"]
  serializeITy (ITy (ConflictTy i)) = PP.hcat ["conflict", PP.pretty i]
  serializeITy (ITy (TupleTy{})) = "TODO:tuple"
  serializeITy (ITy (VecTy{})) = "TODO:vec"

  serializeRevocableEqC = serializeEqC . datum
  serializeRevocableEqRowC = serializeEqRowC . datum
  serializeRevocableSubRowC = serializeSubRowC . datum
  serializeRevocableSubTypeC = serializeSubTypeC . datum

  serializeEqC c = PP.hsep [serializeTyVar (eqLHS c), "=", serializeITy (eqRHS c)]

  serializeRowExpr (RowExprVar rv) = serializeRowVar rv
  serializeRowExpr (RowExprShift o rv) = PP.hcat [serializeRowVar rv, " + ", PP.pretty o]

  serializeEqRowC c = PP.hsep [serializeRowExpr (eqRowLHS c), "=", serializeRowExpr (eqRowRHS c)]

  serializeCondEq c =
    let (eqCs, eqRowCs) = cConstraints c
     in PP.hcat $
          PP.punctuate " | " (map serializeConjunction (getDisjuncts (cGuard c)))
            ++ [PP.hsep $ " ⊢" : PP.punctuate PP.comma (map serializeEqC eqCs <> map serializeEqRowC eqRowCs)]

  serializeConjunction c =
    PP.encloseSep "{" "}" ", " $ map serializePattern (getConjuncts c)

  serializePattern p = PP.hsep [serializeTyVar (patVar p), "=", serializePatternRHS (patRHS p)]

  serializePatternRHS (IsPtr DontCare) = "*(_)"
  serializePatternRHS (IsPtr (Schematic rv)) = PP.hcat ["*(", serializeRowVar rv, ")"]
  serializePatternRHS IsNum = "num[?]"
  serializePatternRHS IsConflict = "conflict"

  serializeSubRowC (l :<: r) = PP.hsep [serializeRowExpr l, "<:", serializeRowExpr r]

  serializeSubTypeC (l :<: r) = PP.hsep [serializeTyVar l, "<:", serializeTyVar r]

data ConstraintSolvingState = ConstraintSolvingState
  { constraintSet :: ConstraintSet
  , nextTraceId :: Int
  , nextRowVar :: Int
  , nextTyVar :: Int
  , ptrWidth :: Int
  -- ^ The width of a pointer, in bits.  This can go away when tyvars have an
  -- associated size, it is only used for PtrAddC solving.
  , ctxTyVars :: RevocableUnionFindMap RevokePolicy TyVar TyVar ITy'
  -- ^ The union-find data-structure mapping each tyvar onto its representative
  -- tv.  If no mapping exists, it is a self-mapping.
  , ctxRowVars :: RevocableUnionFindMap RevokePolicy RowVar RowInfo (FieldMap TyVar)
  , -- Debugging
    ctxTraceUnification :: Bool
  }
  deriving (Generic)

emptyContext :: Int -> Bool -> ConstraintSolvingState
emptyContext ptrWidth shouldTrace =
  ConstraintSolvingState
    { constraintSet =
        ConstraintSet
          { ctxEqCs = []
          , ctxEqRowCs = []
          , ctxCondEqs = []
          , ctxSubRowCs = []
          , ctxSubTypeCs = []
          }
    , nextTraceId = 0
    , nextRowVar = 0
    , nextTyVar = 0
    , ptrWidth
    , ctxTyVars = UM.empty
    , ctxRowVars = UM.empty
    , ctxTraceUnification = shouldTrace
    }

newtype SolverM a = SolverM
  { getSolverM :: State ConstraintSolvingState a
  }
  deriving (Applicative, Functor, Monad, MonadState ConstraintSolvingState)

runSolverM :: Int -> Bool -> SolverM a -> a
runSolverM w b = flip evalState (emptyContext w b) . getSolverM

--------------------------------------------------------------------------------
-- Adding constraints

addEqC :: RevokePolicy -> EqC -> SolverM ()
addEqC rp eqc = field @"constraintSet" . field @"ctxEqCs" %= (Revocable rp eqc :)

addTyVarEq :: RevokePolicy -> TyVar -> ITy -> SolverM ()
addTyVarEq rp tv1 tv2 = addEqC rp (EqC tv1 tv2)

addTyVarEq' :: RevokePolicy -> TyVar -> TyVar -> SolverM ()
addTyVarEq' rp tv1 tv2 = addTyVarEq rp tv1 (VarTy tv2)

addEqRowC :: RevokePolicy -> EqRowC -> SolverM ()
addEqRowC rp eqc = field @"constraintSet" . field @"ctxEqRowCs" %= (Revocable rp eqc :)

addRowExprEq :: RevokePolicy -> RowExpr -> RowExpr -> SolverM ()
addRowExprEq rp r1 r2 = addEqRowC rp (EqRowC r1 r2)

addCondEq :: Conditional' -> SolverM ()
addCondEq cs = field @"constraintSet" . field @"ctxCondEqs" %= (cs :)

addSubType :: RevokePolicy -> TyVar -> TyVar -> SolverM ()
addSubType rp a b = field @"constraintSet" . field @"ctxSubTypeCs" %= (Revocable rp (a :<: b) :)

addSubRow :: RevokePolicy -> RowExpr -> RowExpr -> SolverM ()
addSubRow rp a b = field @"constraintSet" . field @"ctxSubRowCs" %= (Revocable rp (a :<: b) :)

--------------------------------------------------------------------------------
-- Getting constraints

popField :: Lens' ConstraintSolvingState [a] -> SolverM (Maybe a)
popField fld =
  fld %%= \case
    [] -> (Nothing, [])
    (c : cs) -> (Just c, cs)

dequeueEqC :: SolverM (Maybe (Revocable EqC))
dequeueEqC = popField (field @"constraintSet" . field @"ctxEqCs")

dequeueEqRowC :: SolverM (Maybe (Revocable EqRowC))
dequeueEqRowC = popField (field @"constraintSet" . field @"ctxEqRowCs")

dequeueSubTypeC :: SolverM (Maybe (Revocable SubTypeC))
dequeueSubTypeC = popField (field @"constraintSet" . field @"ctxSubTypeCs")

--------------------------------------------------------------------------------
-- Operations over type variable state

freshRowVar :: SolverM RowVar
freshRowVar = RowVar <$> (field @"nextRowVar" <<+= 1)

freshRowVarE :: RowExpr -> SolverM RowVar
freshRowVarE e = do
  rowv <- freshRowVar
  unsafeUnifyRowVars NeverRevoke e rowv
  pure rowv

freshRowVarFM :: FieldMap TyVar -> SolverM RowVar
freshRowVarFM fm = do
  rowv <- freshRowVar
  defineRowVar rowv fm
  pure rowv

{- | @unsafeUnifyRowVars root leaf@ will make @root@ the new equiv. rep
for @leaf@.  Note that both root and leaf should be the reps. of
their corresponding equivalence classes.
-}
unsafeUnifyRowVars :: RevokePolicy -> RowExpr -> RowVar -> SolverM ()
unsafeUnifyRowVars orig root leaf = field @"ctxRowVars" %= UM.unify orig ki leaf
 where
  ki = RowInfo{riShift = rowExprShift root, riRowVar = rowExprVar root}

-- | Always define a row variable, even if it has a def.
defineRowVar :: RowVar -> FieldMap TyVar -> SolverM ()
defineRowVar rowv fm = field @"ctxRowVars" %= UM.insert rowv fm

undefineRowVar :: RowVar -> SolverM ()
undefineRowVar rv = field @"ctxRowVars" %= UM.delete rv

lookupRowVarRep :: RowVar -> SolverM (Offset, RowVar)
lookupRowVarRep rv = do
  ri <- field @"ctxRowVars" %%= snd . UM.lookupRepFast rv
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

{- | Lookup a type variable, returns the representative of the
corresponding equivalence class.  This also updates the eqv. map to
amortise lookups.
-}
lookupTyVarRep :: TyVar -> SolverM TyVar
lookupTyVarRep tv0 = field @"ctxTyVars" %%= snd . UM.lookupRepFast tv0

{- | Lookup a type variable, returns the representative of the
corresponding equivalence class, and the definition for that type
var, if any.
-}
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

{- | @unsafeUnifyTyVars root leaf@ will make @root@ the new equiv. rep
for @leaf@.  Note that both root and leaf should be the reps. of
their corresponding equivalence classes.
-}
unsafeUnifyTyVars :: RevokePolicy -> TyVar -> TyVar -> SolverM ()
unsafeUnifyTyVars orig root leaf = field @"ctxTyVars" %= UM.unify orig root leaf

--------------------------------------------------------------------------------
-- Other stuff

ptrWidthNumTy :: SolverM ITy'
ptrWidthNumTy = NumTy <$> use (field @"ptrWidth")

setTraceUnification :: Bool -> SolverM ()
setTraceUnification b = field @"ctxTraceUnification" .= b

traceUnification :: SolverM Bool
traceUnification = use (field @"ctxTraceUnification")

--------------------------------------------------------------------------------
-- Conditional constraints

data Schematic a = Schematic a | DontCare
  deriving (Eq, Ord)

instance PP.Pretty a => PP.Pretty (Schematic a) where
  pretty (Schematic a) = "?" <> PP.pretty a
  pretty DontCare = "_"

-- data Let v e = Let { letBinder :: Schematic v, letBody :: e }
-- instance (PP.Pretty v, PP.Pretty e) => PP.Pretty (Let v e) where
--   pretty (Let v e) = "let " <> PP.pretty v <> " := " <> PP.pretty e

{- | Does the definition of the tyvar on the LHS match the pattern on the RHS,
 - instantiating schematics as required.
-}
data PatternRHS
  = IsPtr (Schematic RowVar)
  | IsNum
  | IsConflict
  deriving (Eq, Ord)

instance PP.Pretty PatternRHS where
  pretty (IsPtr sv) = "PtrTy " <> PP.pretty sv
  pretty IsNum = "NumTy _"
  pretty IsConflict = "ConflictTy _"

data Pattern = Pattern
  { patVar :: TyVar
  , patRHS :: PatternRHS
  }
  deriving (Eq, Ord)

instance PP.Pretty Pattern where
  pretty (Pattern v r) = PP.pretty v <> " =?= " <> PP.pretty r

newtype Conjunction a = Conjunction {getConjuncts :: [a]}
  deriving (Eq, Foldable, Functor, Ord, Traversable)
newtype Disjunction a = Disjunction {getDisjuncts :: [a]}
  deriving (Eq, Foldable, Functor, Ord, Traversable)

instance PP.Pretty a => PP.Pretty (Conjunction a) where
  pretty = PP.pretty . getConjuncts

instance PP.Pretty a => PP.Pretty (Disjunction a) where
  pretty = PP.pretty . getDisjuncts

-- FIXME: this is pretty arcane
data Conditional a = Conditional
  { cName :: String
  , cGuard :: Disjunction (Conjunction Pattern)
  -- ^ Disjunction of conjunction of linear patterns -- each schematic should
  -- occur on one RHS only.
  , cConstraints :: a
  }

instance Eq a => Eq (Conditional a) where
  ca == cb = cGuard ca == cGuard cb && cConstraints ca == cConstraints cb

instance Ord a => Ord (Conditional a) where
  compare ca cb =
    compare (cGuard ca, cConstraints ca) (cGuard cb, cConstraints cb)

{- | Simple matching --- the pattern matches, or it doesn't.  A more advanced
  version of this might determine that a pattern can never match.  This returns
  a mapping from schematics to their unifiers if one exists.
-}
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
    PP.pretty (cName c)
      <> ": "
      <>
      -- FIXME, could be nicer.
      PP.list (getDisjuncts (fmap (PP.list . getConjuncts . fmap PP.pretty) (cGuard c)))
      <> " |- "
      <> PP.pretty (cConstraints c)

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

instance PP.Pretty ConstraintSet where
  pretty s =
    let row title entries = title PP.<+> PP.align (PP.list entries)
     in PP.vsep
          [ row "EqCs" $ map PP.pretty $ ctxEqCs s
          , row "EqRowCs" $ map PP.pretty $ ctxEqRowCs s
          , row "CondEqs" $ map PP.pretty $ ctxCondEqs s
          , row "SubRowCs" $ map PP.pretty $ ctxSubRowCs s
          , row "SubTypeCs" $ map PP.pretty $ ctxSubTypeCs s
          ]

instance PP.Pretty ConstraintSolvingState where
  pretty ctx =
    let s = constraintSet ctx
     in PP.vsep
          [ PP.pretty s
          , PP.pretty (ctxTyVars ctx)
          , PP.pretty (ctxRowVars ctx)
          ]

shiftOffsets :: Offset -> Map Offset v -> Map Offset v
shiftOffsets 0 m = m
shiftOffsets o m =
  Map.fromList [(k - o, v) | (k, v) <- Map.toList m]
