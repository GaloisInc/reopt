{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Solver.Solver
  ( unifyConstraints
  )
where

import           Control.Lens          ((.=), (<<+=), (<<.=), use)
import           Control.Monad         (when)
import           Control.Monad.Extra   (whenM)
import           Control.Monad.State   (MonadState (get))
import           Data.Bifunctor        (first, Bifunctor (second))
import           Data.Foldable         (traverse_)
import           Data.Function         (on)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import           Debug.Trace           (trace)
import qualified Prettyprinter         as PP

import           Reopt.TypeInference.Solver.Constraints   (EqC (..),
                                                           EqRowC (..),
                                                           pattern (:<:))
import           Reopt.TypeInference.Solver.Finalise      (ConstraintSolution,
                                                           finalizeTypeDefs)
import           Reopt.TypeInference.Solver.Monad         (Conditional (..),
                                                           Conditional',
                                                           SolverM, addEqC,
                                                           addEqRowC,
                                                           addRowExprEq,
                                                           addTyVarEq,
                                                           addTyVarEq',
                                                           condEnabled,
                                                           defineRowVar,
                                                           defineTyVar,
                                                           dequeueEqC,
                                                           dequeueEqRowC,
                                                           freshRowVar,
                                                           lookupRowExpr,
                                                           lookupTyVar,
                                                           traceUnification,
                                                           undefineRowVar,
                                                           undefineTyVar,
                                                           unsafeUnifyRowVars,
                                                           unsafeUnifyTyVars)
import           Reopt.TypeInference.Solver.RowVariables  (FieldMap (getFieldMap),
                                                           RowExpr (..),
                                                           dropFieldMap,
                                                           emptyFieldMap,
                                                           rowExprShift,
                                                           rowExprVar, rowVar,
                                                           shiftFieldMap,
                                                           unifyFieldMaps)
import           Reopt.TypeInference.Solver.TypeVariables (TyVar)
import           Reopt.TypeInference.Solver.Types         (ITy (..), ITy',
                                                           TyF (..))

-- | Unify the given constraints, returning a conservative type map for all type
-- variables.

-- FIXME: probably want to export the Eqv map somehow
unifyConstraints :: SolverM ConstraintSolution
unifyConstraints = do
  processAtomicConstraints
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

asRecordPointer :: TyVar -> SolverM (Maybe (RowExpr, FieldMap TyVar))
asRecordPointer v = do
  lookupTyVar v >>= \case
    (_, Just (PtrTy r)) -> Just . second (fromMaybe emptyFieldMap) <$> lookupRowExpr r
    _ -> return Nothing

-- | Returns @True@ if we found a subtype constraint that we could extract new
-- information from.  We never remove subtype constraints: every time we learn
-- information about offsets of the supertype, we must propagate that
-- information to the subtype.
propagateSubTypeC :: SolverM Bool
propagateSubTypeC = go =<< use (field @"ctxSubTypeCs")
  where
    go [] = return False
    go ((lhs :<: rhs) : cs) = do
      asRecordPointer rhs >>= \case
        Just (rhsRep, rhsFM) -> do
          let rhsOff = rowExprShift rhsRep
          asRecordPointer lhs >>= \case
            Just (lhsRep, lhsFM)
              -- Nothing to do in this case (we could drop the constraint)
              | lhsRep == rhsRep -> go cs
              -- If there is some recursion going on, unification would result
              -- in an infinite loop.
              | on (==) rowExprVar lhsRep rhsRep -> go cs
              | otherwise -> do
                let
                  lhsOff = rowExprShift lhsRep
                  lhsKeys = Map.keys (getFieldMap lhsFM)
                  rhsFMAdjusted = shiftFieldMap lhsOff (dropFieldMap rhsOff rhsFM)
                  rhsKeys = Map.keys (getFieldMap rhsFMAdjusted)
                  (unified, overlaps) = unifyFieldMaps lhsFM rhsFMAdjusted
                defineRowVar (rowExprVar lhsRep) unified
                traverse_ (uncurry addTyVarEq') overlaps
                -- NOTE: We would also like to detect when unification made progress...
                if not (all (`elem` lhsKeys) rhsKeys)
                  then return True
                  else go cs
            Nothing -> do -- `b` is known to be a record pointer, but `a` is not
              row <- freshRowVar
              defineRowVar row (dropFieldMap rhsOff rhsFM)
              addTyVarEq lhs (ITy (PtrTy (rowVar row)))
              return True
        Nothing -> go cs

subTypeSolver :: SolverM ()
subTypeSolver =
  -- If we make progress, process atomic constraints again
  whenM propagateSubTypeC processAtomicConstraints

-- | Process all atomic (i.e., non-disjunctive) constraints, updating the
-- context with each.
processAtomicConstraints :: SolverM ()
processAtomicConstraints = traceContext "processAtomicConstraints" $ do
  dequeueEqC >>= \case
    Just c  -> solveEqC c >> processAtomicConstraints
    Nothing -> dequeueEqRowC >>= \case
      Just c  -> solveEqRowC c >> processAtomicConstraints
      Nothing -> condEqSolver >>= \case
        True -> processAtomicConstraints
        False -> subTypeSolver
  where
    -- This solver will solve one at a time, which is important to get
    -- around the +p++ case.  We solve a single ptr add, then propagate
    -- the new eq and roweq constraints.
    condEqSolver = do
      ceqs <- field @"ctxCondEqs" <<.= mempty -- get constraints and c
      go [] ceqs

    restore :: [Conditional'] -> SolverM ()
    restore cs = field @"ctxCondEqs" .= cs

    -- FIXME: we might want to drop conditionals when they are never
    -- going to be satisfiable.
    go acc [] = restore acc $> False -- finished here, we didn't so anything.
    go acc (c : cs) = do
      solved <- solveConditional c
      if solved
        -- Conditional fired, remove it and continue solving
        then restore (cs ++ acc) $> True
        -- Conditional couldn't be fired, try next conditionals
        else go (c : acc) cs

--------------------------------------------------------------------------------
-- Conditionals

solveConditional :: Conditional' -> SolverM Bool
solveConditional c = do
  m_newEqs <- condEnabled c
  case m_newEqs of
    Just newEqs -> do
      mapM_ addEqC eqcs
      mapM_ addEqRowC (newEqs ++ eqrowcs)
      pure True
    Nothing -> pure False
  where
    (eqcs, eqrowcs) = cConstraints c

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
      | lv == rv  -> trace "Recursive row var equation, ignoring" $ pure ()
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

solveEqC :: EqC -> SolverM ()
solveEqC eqc = do
  (lv, m_lty) <- lookupTyVar (eqLhs eqc)
  (m_rv, m_rty) <- case eqRhs eqc of
    VarTy tv -> first Just <$> lookupTyVar tv
    ITy   ty -> pure (Nothing, Just ty)
  case (m_lty, m_rty) of
    _ | m_rv == Just lv -> pure () -- trivial up to eqv.
    (_, Nothing)        -> traverse_ (unsafeUnifyTyVars lv) m_rv
    (Nothing, Just rty)
      | Just rv <- m_rv -> unsafeUnifyTyVars rv lv
      -- the RHS was a term, so we define lv.
      | otherwise -> defineTyVar lv rty
    (Just ty1, Just ty2) -> do
      traverse_ (unsafeUnifyTyVars lv) m_rv
      traverse_ undefineTyVar m_rv
      unifyTypes lv ty1 ty2

-- | @unifyTypes tv1 t1 t2@ unifies the types @t1@ and @t2@
-- named by the type variable @tv@.
unifyTypes :: TyVar -> ITy' -> ITy' -> SolverM ()
unifyTypes tv ty1 ty2 =
  case (ty1, ty2) of
    (NumTy i, NumTy i')
      | i == i'   -> pure ()
      | otherwise ->
        trace ("Mismatch in type widths for " ++ show (PP.pretty tv) ++ ": "
               ++ show (PP.pretty ty1) ++ " and " ++ show (PP.pretty ty2))
        $ pure ()

    (PtrTy rv1, PtrTy rv2) -> addRowExprEq rv1 rv2

    -- Unification failure, we need to report a conflict.
    _ ->
      trace ("Unification failed at " ++ show (PP.pretty tv) ++ ": " ++ show (PP.pretty ty1) ++ " and " ++ show (PP.pretty ty2)) $ pure ()
      -- pretend we saw nothing :(
      -- error $ "FIXME: conflict detected at " ++ show (PP.pretty tv)
