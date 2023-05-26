{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Reopt.TypeInference.Solver.Solver
  ( unifyConstraints
  )
where

import           Control.Lens          (Lens', _1, view, (%=), (<<+=),
                                        (<<.=))
import           Control.Monad         (when, zipWithM_)
import           Control.Monad.Extra   (orM)
import           Control.Monad.State   (MonadState (get), put, StateT, evalStateT)
import           Data.Bifunctor        (Bifunctor (second), first)
import           Data.Foldable         (traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe, isNothing)
import qualified Data.Set              as Set
import           Debug.Trace           (trace)
import qualified Prettyprinter         as PP

import           Reopt.TypeInference.Solver.Constraints   (EqC (..),
                                                           EqRowC (..), SubRowC,
                                                           SubTypeC,
                                                           pattern (:<:),
                                                           ConstraintProvenance (..))
import           Reopt.TypeInference.Solver.Finalise      (ConstraintSolution,
                                                           finalizeTypeDefs)
import           Reopt.TypeInference.Solver.Monad         (Conditional (..),
                                                           Conditional',
                                                           ConstraintSolvingState (..),
                                                           SolverM, addEqC,
                                                           addEqRowC,
                                                           addRowExprEq,
                                                           addSubRow,
                                                           addTyVarEq,
                                                           addTyVarEq',
                                                           condEnabled,
                                                           ctxTyVars,
                                                           defineRowVar,
                                                           defineTyVar,
                                                           freshRowVar,
                                                           lookupRowExpr,
                                                           lookupRowExprRep,
                                                           lookupTyVar,
                                                           popField,
                                                           ppConstraintSolvingStateProvs,
                                                           traceConstraintOrigins,
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
import           Reopt.TypeInference.Solver.UnionFindMap  (eqvClasses)
import Control.Monad.Trans (lift)

-- | Unify the given constraints, returning a conservative type map for all type
-- variables.

unifyConstraints :: SolverM ConstraintSolution
unifyConstraints = do
  solverLoop
  finalizeTypeDefs

-- | @traceContext description ctx ctx'@ reports how the context changed via @trace@.
traceContext :: PP.Doc () -> SolverM a -> SolverM a
traceContext description action = do
  tId <- field @"nextTraceId" <<+= 1
  doTraceUnification <- traceUnification
  doTraceConstraintOrigins <- traceConstraintOrigins

  stateBefore <- get
  traceIt doTraceUnification       tId ">>>" $ PP.pretty stateBefore
  traceIt doTraceConstraintOrigins tId ">>>" $ ppConstraintSolvingStateProvs stateBefore

  r <- action

  stateAfter <- get
  traceIt doTraceUnification       tId "<<<" $ PP.pretty stateAfter
  traceIt doTraceConstraintOrigins tId "<<<" $ ppConstraintSolvingStateProvs stateAfter

  pure r
  where
    traceIt :: Bool -- Only trace if this is True
            -> Int -- The trace ID
            -> PP.Doc () -- A herald to print at the start
            -> PP.Doc () -- The pretty-printed state
            -> SolverM ()
    traceIt doTrace tId herald ppState =
      when doTrace $ do
        let msg =
              PP.vsep
                [ PP.hsep [herald, " ", PP.parens (PP.pretty tId), description],
                  PP.indent 4 ppState
                ]
        trace (show msg) (return ())

traceContext' :: PP.Pretty v => PP.Doc () -> v -> SolverM a -> SolverM a
traceContext' msg v = traceContext (msg <> ": " <> PP.pretty v)

--------------------------------------------------------------------------------
-- Solver loop

data Retain   = Retain | Discard
  deriving Eq

data Progress = Progress | NoProgress
  deriving Eq

type SolverLoopM = StateT ConstraintSolvingState SolverM

madeProgress :: Progress -> Bool
madeProgress Progress = True
madeProgress _        = False

solveHead :: Lens' ConstraintSolvingState [a] ->
             (a -> SolverM ()) ->
             SolverLoopM Bool
solveHead fld doit = lift $ do
  v <- popField fld
  case v of
    Nothing -> pure False
    Just v' -> doit v' $> True


solveHeadReset :: Lens' ConstraintSolvingState [a] ->
             (a -> SolverM (Maybe TyVar)) ->
             SolverLoopM Bool
solveHeadReset fld doit = do
  v <- lift $ popField fld
  case v of
    Nothing -> pure False
    Just v' -> do
      m_conflictTv <- lift $ doit v'
      traverse_ restart m_conflictTv
      pure True

  where
    -- We detected a conflict, we need to restart after updating state
    -- to reflect the conflict.
    restart tv = do
      trace "Conflict, restarting" $ pure ()
      resetSt <- get
      resetSt' <- lift $ do
        oldSt   <- get
        put resetSt

        -- Forget everything we know in resetSt about the eqvs for tv
        let eqs   = eqvClasses (ctxTyVars oldSt)
            eqsTv = Map.findWithDefault [] tv eqs
        traverse_ undefineTyVar eqsTv

        -- FIXME: gross
        defineTyVar tv (ConflictTy (ptrWidth resetSt))
        -- FIXME: this could cause problems if we allocate tyvars after
        -- we start solving.  Because we don't, this should work.
        mapM_ (addTyVarEq' ConflictProv tv) eqsTv -- retain eqv class for conflict var.
        get
      put resetSt'

solveFirst :: Lens' ConstraintSolvingState [a] ->
              (a -> SolverM (Retain, Progress)) ->
              SolverLoopM Bool
solveFirst fld solve = lift $ do
  cstrs <- fld <<.= [] -- get constraints and c
  go [] cstrs
  where
    restore cs = fld %= (++ cs)

    -- FIXME: we might want to drop constraints when they are never
    -- going to be satisfiable.
    go acc [] = restore acc $> False -- finished here, we didn't so anything.
    go acc (c : cs) = do
      (retain, progress) <- solve c
      let acc' = if retain == Retain then c : acc else acc
      if madeProgress progress
        then restore (cs ++ acc') $> True
        else go acc' cs

_solveAll :: Lens' ConstraintSolvingState [a] ->
              (a -> SolverM (Retain, Progress)) ->
              SolverM Bool
_solveAll fld solve = do
  cstrs <- fld <<.= [] -- get constraints and c
  go [] False cstrs
  where
    restore cs = fld %= (++ cs)

    -- FIXME: we might want to drop constraints when they are never
    -- going to be satisfiable.
    go acc progd [] = restore acc $> progd -- finished here, we didn't so anything.
    go acc progd (c : cs) = do
      (retain, progress) <- solve c
      let acc'   = if retain == Retain then c : acc else acc
          progd' = progd || madeProgress progress
      go acc' progd' cs

-- | @preprocess l f# just pre-processes the element at @l@, and so
-- does not make progress (otherwise it would loop forever).
preprocess :: Monoid a =>
              Lens' ConstraintSolvingState a ->
              (a -> SolverM a) ->
              SolverLoopM Bool
preprocess fld f = lift $ False <$ do
  cstrs <- fld <<.= mempty -- get constraints and c
  r <- f cstrs
  fld %= (<> r)

solverLoop :: SolverM ()
solverLoop = evalStateT go =<< get
  where
    go = do
      keepGoing <- orM solvers
      when keepGoing go

    solvers = [ solveHeadReset (field @"ctxEqCs")      solveEqC
              , solveHead      (field @"ctxEqRowCs")   solveEqRowC
              , solveFirst     (field @"ctxCondEqs")   solveConditional
              , solveFirst     (field @"ctxSubTypeCs") solveSubTypeC
              , preprocess     (field @"ctxSubRowCs")  resolveCycles
              , solveFirst     (field @"ctxSubRowCs")  solveSubRowC
              ]

--------------------------------------------------------------------------------
-- Loop detection

resolveCycles :: [SubRowC] -> SolverM [SubRowC]
resolveCycles subs =
  filterCyclicEdges <$> mapM (traverse lookupRowExprRep) subs

-- This will even remove r + 0 :<: r + 0, which is probably OK.
filterCyclicEdges :: [SubRowC] -> [SubRowC]
--                     ([SubC a], Map.Map a (Set.Set a), Map.Map a (Set.Set a))
filterCyclicEdges = view _1 . foldr go mempty
  where
    -- We filter out reflexive edges, this is always OK.
    go (a :<: b) r | a == b = r
    go c@(a :<: b) r@(acc, succs, preds)
      | r_a `Set.member` succs_a' = trace ("Removing cycle " ++ show (PP.pretty c)) r
      | otherwise = (c : acc, succs', preds')
      where
        -- Every pred. of a is now a pred of b and all its succs
        -- likewise for succ/preds
        preds' = updRel preds preds_b' succs_a'
        succs' = updRel succs succs_a' preds_b'

        updRel rel new keys =
          foldr (\k -> Map.insertWith Set.union k new) rel (Set.toList keys)

        succs_a' = Set.insert r_b succs_b
        preds_b' = Set.insert r_a preds_a
        preds_a  = Map.findWithDefault mempty r_a preds
        succs_b  = Map.findWithDefault mempty r_b succs

        r_a = rowExprVar a
        r_b = rowExprVar b

--------------------------------------------------------------------------------
-- Subtyping

solveSubTypeC :: SubTypeC -> SolverM (Retain, Progress)
solveSubTypeC c@(lhs :<: rhs) = traceContext' "solveSubTypeC" c $ do
  (_, m_lhsTy) <- lookupTyVar lhs
  (_, m_rhsTy) <- lookupTyVar rhs
  case (m_lhsTy, m_rhsTy) of

    (Just (PtrTy lhsRow), Just (PtrTy rhsRow)) ->
      addSubRow lhsRow rhsRow >> return (Discard, Progress)

    -- If we know that `lhs` is a pointer, we should propagate this fact to
    -- `rhs`, at it could help solve some constraints that are waiting to
    -- know whether `rhs` is a pointer.
    (Just (PtrTy lhsRow), _) -> do
      rhsRow <- rowVar <$> freshRowVar
      addTyVarEq prov rhs (ITy (PtrTy rhsRow))
      addSubRow lhsRow rhsRow
      return (Discard, Progress)

    -- We did not yet know `lhs` was a pointer.  We can initialize it
    -- as such, and let the solver propagate fields.
    (_, Just (PtrTy rhsRow)) -> do
      lhsRow <- rowVar <$> freshRowVar
      addTyVarEq prov lhs (ITy (PtrTy rhsRow))
      addSubRow lhsRow rhsRow
      return (Discard, Progress)

    -- When one side is **not ptr**, we unify the type variables.
    (_     , Just _) -> addTyVarEq' prov lhs rhs >> return (Discard, Progress)
    (Just _, _         ) -> addTyVarEq' prov lhs rhs >> return (Discard, Progress)

    -- If neither side is defined, we save this constaint for later.
    (Nothing, Nothing) -> return (Retain, NoProgress)
  where
    prov = FromSubTypeCProv

solveSubRowC :: SubRowC -> SolverM (Retain, Progress)
solveSubRowC c@(lhsRow :<: rhsRow) = traceContext' "solveSubRowC" c $ (,) Retain <$> do
  (lhsRep, lhsFM) <- second (fromMaybe emptyFieldMap) <$> lookupRowExpr lhsRow
  (rhsRep, rhsFM) <- second (fromMaybe emptyFieldMap) <$> lookupRowExpr rhsRow
  let
    lhsOff = rowExprShift lhsRep
    rhsOff = rowExprShift rhsRep
    lhsKeys = Map.keys (getFieldMap lhsFM)
    rhsFMAdjusted = shiftFieldMap lhsOff (dropFieldMap rhsOff rhsFM)
    rhsKeys = Map.keys (getFieldMap rhsFMAdjusted)
    (unified, overlaps) = unifyFieldMaps lhsFM rhsFMAdjusted
  defineRowVar (rowExprVar lhsRep) unified
  traverse_ (uncurry (addTyVarEq' FromSubRowCProv)) overlaps
  -- NOTE: We would also like to detect when unification made progress...
  if not (all (`elem` lhsKeys) rhsKeys)
    then return Progress
    else return NoProgress

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
      traverse_ (uncurry (addTyVarEq' FromEqRowCProv)) newEqs

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

    (TupleTy ts, TupleTy ts')
      | length ts == length ts' ->
          zipWithM_ (\t t' -> addTyVarEq' (UnificationProv t t') t t') ts ts' $> Nothing

    -- Should always have n1 == n2
    (VecTy n1 ty1', VecTy n2 ty2')
      | n1 == n2 -> addTyVarEq' (UnificationProv ty1' ty2') ty1' ty2' $> Nothing

    -- Unification failure, including the case where one is a
    -- conflictty (but not both), we need to report a conflict.
    _ ->
      trace ("Unification failed at " ++ show (PP.pretty tv) ++ ": " ++ show (PP.pretty ty1) ++ " and " ++ show (PP.pretty ty2)) $
      pure (Just tv)
      -- pretend we saw nothing :(
      -- error $ "FIXME: conflict detected at " ++ show (PP.pretty tv)
