{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Reopt.TypeInference.Solver.Monad where

import           Control.Lens          (Lens', use, (%%=), (%=), (.=), (<<+=))
import           Control.Monad.State   (MonadState, State, evalState)
import           Data.Generics.Product (field)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           GHC.Generics          (Generic)
import qualified Prettyprinter         as PP

import           Reopt.TypeInference.Solver.Constraints   (EqC (EqC),
                                                           EqRowC (EqRowC))
import           Reopt.TypeInference.Solver.RowVariables  (FieldMap,
                                                           Offset (Offset),
                                                           RowExpr (RowExprShift, RowExprVar),
                                                           RowInfo (..),
                                                           RowVar (RowVar),
                                                           rowExprShift,
                                                           rowExprVar)
import           Reopt.TypeInference.Solver.TypeVariables (TyVar (TyVar))
import           Reopt.TypeInference.Solver.Types         (ITy (..), ITy',
                                                           TyF (NumTy))
import           Reopt.TypeInference.Solver.UnionFindMap  (UnionFindMap)
import qualified Reopt.TypeInference.Solver.UnionFindMap  as UM

data ConstraintSolvingState = ConstraintSolvingState
  { ctxEqCs    :: [EqC],
    ctxEqRowCs :: [EqRowC],
    ctxCondEqs :: [Conditional],

    nextTraceId :: Int,
    nextRowVar :: Int,
    nextTyVar  :: Int,

    -- | The width of a pointer, in bits.  This can go away when
    -- tyvars have an associated size, it is only used for PtrAddC
    -- solving.
    ptrWidth :: Int,

    -- | The union-find data-structure mapping each tyvar onto its
    -- representative tv.  If no mapping exists, it is a self-mapping.

    ctxTyVars :: UnionFindMap TyVar TyVar ITy',
    ctxRowVars :: UnionFindMap RowVar RowInfo (FieldMap TyVar),

    -- Debugging
    ctxTraceUnification :: Bool

  }
  deriving (Generic)

emptyContext :: Int -> Bool -> ConstraintSolvingState
emptyContext w trace = ConstraintSolvingState
  { ctxEqCs        = []
  , ctxEqRowCs     = []
  , ctxCondEqs     = []
  , nextTraceId    = 0
  , nextRowVar     = 0
  , nextTyVar      = 0
  , ptrWidth       = w
  , ctxTyVars      = UM.empty
  , ctxRowVars     = UM.empty
  , ctxTraceUnification = trace
  }

newtype SolverM a = SolverM
  { getSolverM :: State ConstraintSolvingState a
  }
  deriving (Applicative, Functor, Monad, MonadState ConstraintSolvingState)

runSolverM :: Bool -> Int -> SolverM a -> a
runSolverM b w = flip evalState (emptyContext w b) . getSolverM

--------------------------------------------------------------------------------
-- Adding constraints

addTyVarEq :: TyVar -> ITy -> SolverM ()
addTyVarEq tv1 tv2 = field @"ctxEqCs" %= (EqC tv1 tv2 :)

addTyVarEq' :: TyVar -> TyVar -> SolverM ()
addTyVarEq' tv1 tv2 = addTyVarEq tv1 (VarTy tv2)

addRowExprEq :: RowExpr -> RowExpr -> SolverM ()
addRowExprEq r1 r2 = field @"ctxEqRowCs" %= (EqRowC r1 r2 :)

addCondEq :: Conditional -> SolverM ()
addCondEq cs  =
  field @"ctxCondEqs" %= (cs :)

--------------------------------------------------------------------------------
-- Getting constraints

popField :: Lens' ConstraintSolvingState [a] -> SolverM (Maybe a)
popField fld =
  fld %%= \case
    [] -> (Nothing, [])
    (c : cs) -> (Just c, cs)

dequeueEqC :: SolverM (Maybe EqC)
dequeueEqC = popField (field @"ctxEqCs")

dequeueEqRowC :: SolverM (Maybe EqRowC)
dequeueEqRowC = popField (field @"ctxEqRowCs")

--------------------------------------------------------------------------------
-- Operations over type variable state

freshRowVar :: SolverM RowVar
freshRowVar = RowVar <$> (field @"nextRowVar" <<+= 1)

freshRowVarE :: RowExpr -> SolverM RowVar
freshRowVarE e = do
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
    ki = RowInfo { riShift = rowExprShift root, riRowVar = rowExprVar root }

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
freshTyVar orig  (Just (ITy ty)) = do
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

shiftStructuralInformationBy :: Integer -> Map Offset v -> Map Offset v
shiftStructuralInformationBy o =
  Map.fromList . concatMap retainPositiveOffsets . Map.toList
  where
    retainPositiveOffsets (Offset a, ty) =
      let newOffset = fromIntegral a + o
       in [(Offset (fromIntegral newOffset), ty) | newOffset >= 0]

setTraceUnification :: Bool -> SolverM ()
setTraceUnification b = field @"ctxTraceUnification" .= b

traceUnification :: SolverM Bool
traceUnification = use (field @"ctxTraceUnification")


--------------------------------------------------------------------------------
-- Conditional constraints

class CanFresh t where
  makeFresh :: SolverM t

instance CanFresh RowVar where
  makeFresh = freshRowVar

instance CanFresh RowExpr where
  makeFresh = RowExprVar <$> freshRowVar


instance CanFresh TyVar where
  makeFresh = freshTyVar Nothing Nothing

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
  withFresh v = pure v 

-- instance CanFresh RowVar where
--   makeFresh = freshRowVar

-- instance CanFresh TyVar where
--   makeFresh = freshTyVar Nothing Nothing

-- instance (CanFresh a, CanFresh b) => CanFresh (a, b) where
--   makeFresh = (,) <$> makeFresh <*> makeFresh

-- instance (CanFresh a, CanFresh b, CanFresh c) => CanFresh (a, b, c) where
--   makeFresh = (,,) <$> makeFresh <*> makeFresh <*> makeFresh

-- instance (CanFresh a, CanFresh b, CanFresh c, CanFresh d) =>
--          CanFresh (a, b, c, d) where
--   makeFresh = (,,,) <$> makeFresh <*> makeFresh <*> makeFresh <*> makeFresh

-- instance (CanFresh a, CanFresh b, CanFresh c, CanFresh d, CanFresh e) =>
--          CanFresh (a, b, c, d, e) where
--   makeFresh = (,,,,) <$> makeFresh <*> makeFresh <*> makeFresh <*> makeFresh <*> makeFresh

-- FIXME: this is pretty arcane
data Conditional = Conditional
  { cName           :: String
  -- | This says whether the conditional is enabled, disabled, or
  -- delayed.  Once a condition is disabled, it is never examined
  -- again.
  , cEnabled        :: SolverM (Maybe Bool)
  , cAddConstraints :: SolverM ()
  }

-- -- | Returns Nothing if we do not have enough information to try, True
-- -- if enabled, False otherswise.
-- condEnabled :: Conditional c -> SolverM (Maybe Bool)
-- condEnabled c = do
--   ms <- traverse (fmap snd . lookupTyVar (cDomain c)
--   let m_varMap = sequenceA [ (,) v <$> m | (v, m) <- zip (cDomain c) ms ]
--   pure (cPredicate c <$> m_varMap)

instance PP.Pretty Conditional where
  pretty c = PP.pretty (cName c)
  
--------------------------------------------------------------------------------
-- Instances

instance PP.Pretty ConstraintSolvingState where
  pretty ctx =
    let row title entries = title PP.<+> PP.align (PP.list entries)
     in PP.vsep
          [ row "EqCs" $ map PP.pretty $ ctxEqCs ctx,
            row "EqRowCs" $ map PP.pretty $ ctxEqRowCs ctx,
            row "CondEqs" $ map PP.pretty $ ctxCondEqs ctx,
            PP.pretty (ctxTyVars ctx),
            PP.pretty (ctxRowVars ctx)
          ]

shiftOffsets :: Offset -> Map Offset v -> Map Offset v
shiftOffsets 0 m = m
shiftOffsets o m =
  Map.fromList [ (k - o, v) | (k, v) <- Map.toList m ]
