{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Reopt.TypeInference.Solver.Solver
  ( unifyConstraints
  )
where

import           Control.Lens          ((<<+=), (<<.=), Lens', (%=))
import           Control.Monad         (when)
import           Control.Monad.State   (MonadState (get))
import           Data.Bifunctor        (first)
import           Data.Foldable         (traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Maybe            (fromMaybe)
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
                                                           lookupRowExpr,
                                                           lookupTyVar,
                                                           traceUnification,
                                                           undefineRowVar,
                                                           undefineTyVar,
                                                           unsafeUnifyRowVars,
                                                           unsafeUnifyTyVars, Conditional', condEnabled, addEqC, addEqRowC, ConstraintSolvingState, popField)
import           Reopt.TypeInference.Solver.RowVariables  (RowExpr (..),
                                                           emptyFieldMap,
                                                           rowExprShift,
                                                           rowExprVar,
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
  solverLoop
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

traceContext' :: PP.Pretty v => PP.Doc () -> v -> SolverM a -> SolverM a
traceContext' msg v = traceContext (msg <> ": " <> PP.pretty v)

--------------------------------------------------------------------------------
-- Solver loop

data Retain   = Retain | Discard
  deriving Eq

data Progress = Progress | NoProgress
  deriving Eq

madeProgress :: Progress -> Bool
madeProgress Progress = True
madeProgress _        = False

solveHead :: Lens' ConstraintSolvingState [a] ->
             (a -> SolverM ()) ->
             SolverM Progress
solveHead fld doit = do
  v <- popField fld
  case v of
    Nothing -> pure NoProgress
    Just v' -> doit v' $> Progress

solveFirst :: Lens' ConstraintSolvingState [a] ->
              (a -> SolverM (Retain, Progress)) ->
              SolverM Progress
solveFirst fld solve = do
  cstrs <- fld <<.= [] -- get constraints and c
  go [] cstrs
  where
    restore cs = fld %= (++ cs)

    -- FIXME: we might want to drop constraints when they are never
    -- going to be satisfiable.
    go acc [] = restore acc $> NoProgress -- finished here, we didn't so anything.
    go acc (c : cs) = do
      (retain, progress) <- solve c
      let acc' = if retain == Retain then c : acc else acc
      if madeProgress progress
        then restore (cs ++ acc') $> Progress
        else go acc' cs

solverLoop :: SolverM ()
solverLoop = do
  keepGoing <- foldr once (pure False) solvers
  when keepGoing solverLoop
  where
    solvers = [ solveHead  (field @"ctxEqCs")    solveEqC
              , solveHead  (field @"ctxEqRowCs") solveEqRowC
              , solveFirst (field @"ctxCondEqs") solveConditional
              ]

    once m rest = do
      progress <- m
      if madeProgress progress then pure True else rest

--------------------------------------------------------------------------------
-- Conditionals

solveConditional :: Conditional' -> SolverM (Retain, Progress)
solveConditional c = traceContext' "solveConditional" c $ do
  m_newEqs <- condEnabled c
  case m_newEqs of
    Just newEqs -> do
      mapM_ addEqC eqcs
      mapM_ addEqRowC (newEqs ++ eqrowcs)
      pure (Discard, Progress)
    Nothing -> pure (Retain, NoProgress)
  where
    (eqcs, eqrowcs) = cConstraints c

--------------------------------------------------------------------------------
-- Row unification

solveEqRowC :: EqRowC -> SolverM ()
solveEqRowC eqc = traceContext' "solveEqRowC" eqc $ do
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
solveEqC eqc = traceContext' "solveEqC" eqc $ do
  (lv, m_lty) <- lookupTyVar (eqLhs eqc)
  (m_rv, m_rty) <- case eqRhs eqc of
    VarTy tv -> first Just <$> lookupTyVar tv
    ITy   ty -> pure (Nothing, Just ty)
  case (m_lty, m_rty) of
    _ | m_rv == Just lv -> pure () -- trivial up to eqv.
    (_, Nothing)         -> traverse_ (unsafeUnifyTyVars lv) m_rv
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

