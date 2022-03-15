{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Solver.Solver
  ( unifyConstraints
  )
where

import           Control.Lens          ((.=), (<<+=), (<<.=))
import           Control.Monad         (when, zipWithM_)
import           Control.Monad.State   (MonadState (get), put)
import           Data.Bifunctor        (first)
import           Data.Foldable         (traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Maybe            (fromMaybe, isNothing)
import           Debug.Trace           (trace)
import qualified Prettyprinter         as PP

import           Reopt.TypeInference.Solver.Constraints   (EqC (..),
                                                           EqRowC (..))
import           Reopt.TypeInference.Solver.Finalise      (ConstraintSolution,
                                                           finalizeTypeDefs)
import           Reopt.TypeInference.Solver.Monad         (Conditional (..),
                                                           SolverM,
                                                           addRowExprEq,
                                                           addTyVarEq',
                                                           defineRowVar,
                                                           defineTyVar,
                                                           dequeueEqC,
                                                           dequeueEqRowC,
                                                           lookupRowExpr,
                                                           lookupTyVar,
                                                           traceUnification,
                                                           undefineRowVar,
                                                           undefineTyVar,
                                                           unsafeUnifyRowVars,
                                                           unsafeUnifyTyVars, ConstraintSolvingState (ptrWidth), ctxTyVars)
import           Reopt.TypeInference.Solver.RowVariables  (RowExpr (..),
                                                           emptyFieldMap,
                                                           rowExprShift,
                                                           rowExprVar,
                                                           shiftFieldMap,
                                                           unifyFieldMaps)
import           Reopt.TypeInference.Solver.TypeVariables (TyVar)
import           Reopt.TypeInference.Solver.Types         (ITy (..), ITy',
                                                           TyF (..))
import qualified Data.Map.Strict as Map
import Reopt.TypeInference.Solver.UnionFindMap (eqvClasses)

-- | Unify the given constraints, returning a conservative type map for all type
-- variables.

-- FIXME: probably want to export the Eqv map somehow
unifyConstraints :: SolverM ConstraintSolution
unifyConstraints = do
  processAtomicConstraints =<< get
  finalizeTypeDefs

-- | @traceContext description ctx ctx'@ reports how the context changed via @trace@.
traceContext :: PP.Doc () -> SolverM a -> SolverM a
traceContext description action = do
  tId <- field @"nextTraceId" <<+= 1
  doTrace <- traceUnification
  when doTrace $ do
    stateBefore <- get
    let msg =
          PP.vsep
            [ PP.hsep [">>>", PP.parens (PP.pretty tId), description],
              PP.indent 4 $ PP.pretty stateBefore
            ]
    trace (show msg) (pure ())
  r <- action
  when doTrace $ do
    stateAfter <- get
    let msg =
          PP.vsep
            [ PP.hsep ["<<< ", PP.parens (PP.pretty tId), description],
              PP.indent 4 $ PP.pretty stateAfter
            ]
    trace (show msg) (return ())
  pure r

-- | Process all atomic (i.e., non-disjunctive) constraints, updating the
-- context with each.
processAtomicConstraints :: ConstraintSolvingState -> SolverM ()
processAtomicConstraints resetSt = traceContext "processAtomicConstraints" $ do
  dequeueEqC >>= \case
    Just c  -> solveEqC c >>= \case
      Just tv -> restart tv
      Nothing -> processAtomicConstraints resetSt
    Nothing -> dequeueEqRowC >>= \case
      Just c  -> solveEqRowC c >> processAtomicConstraints resetSt
      Nothing -> condEqSolver >>= \case
        True -> processAtomicConstraints resetSt
        False -> pure ()
  where
    -- We detected a conflict, we need to restart after updating state
    -- to reflect the conflict.
    restart tv = do
      oldSt <- get
      put resetSt
      -- Forget everything we know in resetSt about the eqvs for tv
      let eqs = eqvClasses (ctxTyVars oldSt)
          eqsTv = Map.findWithDefault [] tv eqs
      traverse_ undefineTyVar eqsTv
      -- FIXME: gross
      defineTyVar tv (ConflictTy (ptrWidth resetSt))
      -- FIXME: this could cause problems if we allocate tyvars after
      -- we start solving.  Because we don't, this should work.
      mapM_ (addTyVarEq' tv) eqsTv -- retain eqv class for conflict var.

      resetSt' <- get
      processAtomicConstraints resetSt'

    -- This solver will solve one at a time, which is important to get
    -- around the +p++ case.  We solve a single ptr add, then propagate
    -- the new eq and roweq constraints.
    condEqSolver = do
      ceqs <- field @"ctxCondEqs" <<.= mempty -- get constraints and c
      go [] ceqs

    restore :: [Conditional] -> SolverM ()
    restore cs = field @"ctxCondEqs" .= cs

    go acc [] = restore acc $> False -- finished here, we didn't so anything.
    go acc (c : cs) = do
      ce <- cEnabled c
      case ce of
        -- Not enough info to run yet, try the next one.
        Nothing -> go (c : acc) cs
        -- The constraint is no longer relevent, drop and keep going
        Just False  -> go acc cs
        -- The constraint(s) should be added
        Just True   -> cAddConstraints c >> restore (cs ++ acc) $> True

--------------------------------------------------------------------------------
-- Row unification

solveEqRowC :: EqRowC -> SolverM ()
solveEqRowC eqc = do
  (le, m_lfm) <- lookupRowExpr (eqRowLHS eqc)
  let lo  = rowExprShift le
      lv  = rowExprVar   le
      lfm = fromMaybe emptyFieldMap m_lfm

  (re, m_rfm) <- lookupRowExpr (eqRowRHS eqc)
  let ro  = rowExprShift re
      rv  = rowExprVar   re
      rfm = fromMaybe emptyFieldMap m_rfm

  case () of
    _ | (lo, lv) == (ro, rv) -> pure () -- trivial up to eqv.
      | lv == rv  -> error "Recursive row var equation"
      | lo < ro   -> unify (ro - lo) rv rfm lv lfm
      | otherwise -> unify (lo - ro) lv lfm rv rfm
  where
    unify delta lowv lowfm highv highfm = do
      undefineRowVar highv
      unsafeUnifyRowVars (RowExprShift delta lowv) highv
      let highfm' = shiftFieldMap delta highfm
          (lowfm', newEqs) = unifyFieldMaps lowfm highfm'
      defineRowVar lowv lowfm'
      traverse_ (uncurry addTyVarEq') newEqs

--------------------------------------------------------------------------------
-- Type unification

solveEqC :: EqC -> SolverM (Maybe TyVar)
solveEqC eqc = do
  (lv, m_lty) <- lookupTyVar (eqLhs eqc)
  (m_rv, m_rty) <- case eqRhs eqc of
    VarTy tv -> first Just <$> lookupTyVar tv
    ITy   ty -> pure (Nothing, Just ty)
  case (m_lty, m_rty) of
    -- trivial up to eqv.
    _ | m_rv == Just lv -> pure Nothing
      -- We have eqc == (lhs = SomeTy ...) and lhs ~> ConflictTy ...,
      -- so there is nothing further to do.
      | isNothing m_rv
      , Just ConflictTy {} <- m_lty -> pure Nothing

    (_, Nothing)         -> traverse_ (unsafeUnifyTyVars lv) m_rv $> Nothing
    (Nothing, Just rty)
      | Just rv <- m_rv -> unsafeUnifyTyVars rv lv $> Nothing
      -- the RHS was a term, so we define lv.
      | otherwise -> defineTyVar lv rty $> Nothing
    (Just ty1, Just ty2) -> do
      traverse_ (unsafeUnifyTyVars lv) m_rv
      traverse_ undefineTyVar m_rv
      unifyTypes lv ty1 ty2

-- | @unifyTypes tv1 t1 t2@ unifies the types @t1@ and @t2@
-- named by the type variable @tv@.
unifyTypes :: TyVar -> ITy' -> ITy' -> SolverM (Maybe TyVar)
unifyTypes tv ty1 ty2 =
  case (ty1, ty2) of
    _ | ty1 == ty2 -> pure Nothing
    (NumTy i, NumTy i')
      | i == i'   -> pure Nothing
      | otherwise ->
        trace ("Mismatch in type widths for " ++ show (PP.pretty tv) ++ ": "
               ++ show (PP.pretty ty1) ++ " and " ++ show (PP.pretty ty2))
        $ pure (Just tv) -- FIXME: this is a bit odd, as the tyvars will have different sizes

    (PtrTy rv1, PtrTy rv2) -> addRowExprEq rv1 rv2 $> Nothing

    (TupleTy ts, TupleTy ts') -> zipWithM_ addTyVarEq' ts ts' $> Nothing

    -- Should always have n1 == n2
    (VecTy n1 ty1', VecTy n2 ty2') | n1 == n2 -> addTyVarEq' ty1' ty2' $> Nothing

    -- Unification failure, including the case where one is a
    -- conflictty (but not both), we need to report a conflict.
    _ ->
      trace ("Unification failed at " ++ show (PP.pretty tv) ++ ": " ++ show (PP.pretty ty1) ++ " and " ++ show (PP.pretty ty2)) $
      pure (Just tv)
      -- pretend we saw nothing :(
      -- error $ "FIXME: conflict detected at " ++ show (PP.pretty tv)
