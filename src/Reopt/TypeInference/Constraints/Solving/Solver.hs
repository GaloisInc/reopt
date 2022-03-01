{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Constraints.Solving.Solver
  ( unifyConstraints,
  )
where

import Control.Lens ((<<+=), (<<.=), (.=))
import Control.Monad (when)
import Control.Monad.State (MonadState (get), gets)
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import qualified Prettyprinter as PP
import Data.Foldable (traverse_)

import Data.Graph (SCC(..))
import Data.Graph.SCC (stronglyConnCompR)

import Reopt.TypeInference.Constraints.Solving.Constraints
  ( EqC (..),
    EqRowC (EqRowC),
  )
import Reopt.TypeInference.Constraints.Solving.Monad
  ( ConstraintSolvingMonad,
    ConstraintSolvingState (..),
    dequeueEqC, dequeueEqRowC, lookupTyVar, unsafeUnifyTyVars, undefineTyVar, addTyVarEq,
  )
import Reopt.TypeInference.Constraints.Solving.RowVariableSubstitution
  (substRowVarInEqRowC, substRowVarInITy, unifyRecTy)
import Reopt.TypeInference.Constraints.Solving.RowVariables
  ( NoRow (NoRow),    
  )
import Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar)
import Reopt.TypeInference.Constraints.Solving.Types
  ( FreeTyVars (freeTyVars),
    TyF (..),
    FTy (..),
    ITy'
  )


-- | Set to @True@ to enable tracing in unification
traceUnification :: Bool
traceUnification = True

-- | Unify the given constraints, returning a conservative type map for all type
-- variables.

-- FIXME: probably want to export the Eqv map somehow
unifyConstraints :: ConstraintSolvingMonad (Map TyVar FTy)
unifyConstraints = do
  processAtomicConstraints
  m <- gets ctxTyVarDefs 
  pure (finalizeTypeDefs m)

finalizeTypeDefs :: Map TyVar ITy' -> Map TyVar FTy
finalizeTypeDefs m = res
  where
    -- We use this in it's definition, but this is OK as we check for
    -- cycles via the SCC checks below.  We could also just fold the
    -- intermediate result through goSCC
    res = Map.fromList (map goSCC sccs)
    resolveOne t = Map.findWithDefault UnknownTy t res
    
    goSCC (AcyclicSCC (ty, tv, _)) = (tv, FTy $ resolveOne <$> ty)
    goSCC (CyclicSCC _ ) = error "FIXME: cycles detected"

    sccs :: [ SCC (TyF NoRow TyVar, TyVar, [TyVar]) ]
    sccs  = stronglyConnCompR nodes
    
    nodes = [ (finalizeRowVar ty, tv, Set.toList (freeTyVars ty))
            | (tv, ty) <- Map.toList m ]
    
finalizeRowVar :: TyF r f -> TyF NoRow f
finalizeRowVar (NumTy sz) = NumTy sz
finalizeRowVar (PtrTy t)  = PtrTy t
finalizeRowVar (RecTy flds _) = RecTy flds NoRow

-- | @traceContext description ctx ctx'@ reports how the context changed via @trace@.
traceContext :: PP.Doc () -> ConstraintSolvingMonad () -> ConstraintSolvingMonad ()
traceContext description action = do
  tId <- field @"nextTraceId" <<+= 1
  when traceUnification $ do
    stateBefore <- get
    let msg =
          PP.vsep
            [ PP.hsep [">>>", PP.parens (PP.pretty tId), description],
              PP.indent 4 $ PP.pretty stateBefore
            ]
    trace (show msg) (pure ())
  action
  when traceUnification $ do
    stateAfter <- get
    let msg =
          PP.vsep
            [ PP.hsep ["<<< ", PP.parens (PP.pretty tId), description],
              PP.indent 4 $ PP.pretty stateAfter
            ]
    trace (show msg) (return ())

substEqRowC :: EqRowC -> ConstraintSolvingMonad ()
substEqRowC (EqRowC r1 os r2) = do
  -- The only places that we have row vars are in the tyvar defs, and
  -- in the row eqs.
  
  defs  <- field @"ctxTyVarDefs" <<.= mempty -- get defs and clear them
  defs' <- traverse (substRowVarInITy r1 os r2) defs
  field @"ctxTyVarDefs" .= defs'

  rEqs  <- field @"ctxEqRowCs" <<.= mempty -- get row eqs and clear them
  -- This will add back any we still need
  traverse_ (substRowVarInEqRowC r1 os r2) rEqs

-- | Process all atomic (i.e., non-disjunctive) constraints, updating the
-- context with each.
processAtomicConstraints :: ConstraintSolvingMonad ()
processAtomicConstraints = traceContext "processAtomicConstraints" $ do
  dequeueEqC >>= \case
    Just c -> unifyTyVars c >> processAtomicConstraints
    Nothing ->
      dequeueEqRowC >>= \case
        Just c -> substEqRowC c >> processAtomicConstraints
        Nothing -> return ()

-- | Unify two type variables, both of which may have definitions, in
-- which case we unify their definitions as well.
unifyTyVars :: EqC -> ConstraintSolvingMonad ()
unifyTyVars EqC { eqLhs = tv1, eqRhs = tv2 } = do
  (tv1', m_ty1) <- lookupTyVar tv1
  (tv2', m_ty2) <- lookupTyVar tv2
  case (m_ty1, m_ty2) of
    _ | tv1' == tv2' -> pure () -- trivial up to eqv.
    (_, Nothing)         -> unsafeUnifyTyVars tv1' tv2'
    (Nothing, _)         -> unsafeUnifyTyVars tv2' tv1'
    (Just ty1, Just ty2) -> do
      unsafeUnifyTyVars tv1' tv2'
      undefineTyVar tv2'
      unifyTypes tv1' ty1 ty2

-- | @unifyTypes tv1 t1 t2@ unifies the types @t1@ and @t2@
-- named by the type variable @tv@.
unifyTypes :: TyVar -> ITy' -> ITy' -> ConstraintSolvingMonad ()
unifyTypes tv ty1 ty2 =
  case (ty1, ty2) of
    (NumTy i, NumTy i')
      | i == i'   -> pure ()
      | otherwise -> error "Mismatch in type widths"
        
    (PtrTy tv1', PtrTy tv2') -> addTyVarEq tv1' tv2'

    (RecTy fs1 r1, RecTy fs2 r2) -> unifyRecTy fs1 r1 fs2 r2

    -- Unification failure, we need to report a conflict.
    _ -> error $ "FIXME: conflict detected at " ++ show (PP.pretty tv)

-- | Creates the assertion that the given row expression should have a given
-- type at a given offset.  Handles the necessary shifting if the row expression
-- is itself a shift expression.
-- rowExprContains :: RowExpr -> Offset -> ITy -> TyConstraint
-- rowExprContains (RowExprVar r) o t = inRowTC r o t
-- rowExprContains (RowExprShift a r) b t = inRowTC r (b - a) t

-- shift 10 { 0 : t1 } [5] = t2
-- { 10 : t1 } [5] = t2
-- { 5 : t2, 10 : t1 }
-- shift 10 { -5 : t1, 10 : t1 } [5] = t2

-- (shift a r)[b] = t  --->   r[a - b] = t

-- -- | @decomposeEqC t1 t2@ returns constraints implied from @EqP t1 t2@.
-- decomposeEqC :: EqC -> ConstraintSolvingMonad [TyConstraint]
-- decomposeEqC (EqC lhs rhs) = go lhs rhs
--   where
--     go :: ITy -> ITy -> ConstraintSolvingMonad [TyConstraint]
--     go UnknownTy {} _ = pure []
--     go _ UnknownTy {} = pure []
--     go (PtrTy t1) (PtrTy t2) = pure [eqTC t1 t2]
--     go (RecTy fs1 r1) (RecTy fs2 r2)
--       | Map.null fs1 && Map.null fs2 = pure [eqRowTC r1 mempty r2]
--       | Map.null fs1 = pure [eqRowTC r1 fs2 r2]
--       | Map.null fs2 = pure [eqRowTC r2 fs1 r1]
--       | otherwise = do
--         let -- shared fields must be equal
--             fldPs =
--               map (uncurry eqTC) $
--                 Map.elems $
--                   Map.intersectionWith (,) fs1 fs2
--             -- fields from fs2 not in fs1 must appear in r1
--             r1Ps =
--               map (uncurry (rowExprContains r1)) $
--                 Map.toList $ Map.difference fs2 fs1
--             -- fields from fs1 not in fs2 must appear in r2
--             r2Ps =
--               map (uncurry (rowExprContains r2)) $
--                 Map.toList $ Map.difference fs1 fs2
       
--         -- r1 = { 8 : i8 | r3 }   and   r2 = { 0 : i8 | r3 }
--         r3 <- freshRowVar
--         let eqr1 = eqRowTC r1 (Map.difference fs2 fs1) (rowVar r3)
--         let eqr2 = eqRowTC r2 (Map.difference fs1 fs2) (rowVar r3)
--         -- let eqr1 = eqTC (RecTy mempty r1) (RecTy (Map.difference fs2 fs1) r3)
--         -- let eqr2 = eqTC (RecTy mempty r2) (RecTy (Map.difference fs1 fs2) r3)
--         pure $ eqr1 : eqr2 : fldPs ++ r1Ps ++ r2Ps
--     go (RecTy fs r) t = pure $ goRecNonRec fs r t
--     go t (RecTy fs r) = pure $ goRecNonRec fs r t
--     go _ _ = pure []
--     -- Handle the case when a record and a non-record are equal,
--     -- i.e., treat the non-record as a record with one field `0`.
--     goRecNonRec :: Map Offset ITy -> RowExpr -> ITy -> [TyConstraint]
--     goRecNonRec flds r nonRecTy = case Map.lookup 0 flds of
--       Nothing -> [inRowTC (rowExprVar r) (rowExprShift r) nonRecTy]
--       Just ty0 -> [eqTC nonRecTy ty0]
