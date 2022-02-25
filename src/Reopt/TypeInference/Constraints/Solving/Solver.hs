{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Constraints.Solving.Solver
  ( unifyConstraints,
  )
where

import Control.Lens (Lens', over, set, use, (<<+=))
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.State (MonadState (get, put), gets, modify)
import Data.Bifunctor (Bifunctor (second))
import Data.Either (partitionEithers)
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import qualified Prettyprinter as PP
import Reopt.TypeInference.Constraints.Solving.Constraints
  ( AndC (AndC),
    EqC (..),
    EqRowC (EqRowC),
    InRowC (InRowC),
    OrC (..),
    RowShiftC (RowShiftC),
    TyConstraint (..),
    absurdTC,
    andTC,
    eqRowTC,
    eqTC,
    inRowTC,
    orTC,
    trivialTC,
  )
import Reopt.TypeInference.Constraints.Solving.Monad
  ( CanFreshRowVar (freshRowVar),
    ConstraintSolvingMonad,
    ConstraintSolvingState (..),
    runConstraintSolvingMonad,
    substRowVarInRowShiftC,
  )
import Reopt.TypeInference.Constraints.Solving.RowVariableSubstitution (SubstRowVar (substRowVar), substRowVarInEqRowC, substRowVarInOrC)
import Reopt.TypeInference.Constraints.Solving.RowVariables
  ( NoRow (NoRow),
    RowVar,
  )
import Reopt.TypeInference.Constraints.Solving.TypeVariableSubstitution (SubstTyVar (substTyVar))
import Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar)
import Reopt.TypeInference.Constraints.Solving.Types
  ( FTy,
    FreeTyVars (freeTyVars),
    ITy,
    Offset (Offset),
    Ty (NumTy, PtrTy, RecTy, UnknownTy),
    Unknown,
    inconsistent,
    unknownTy,
  )

-- | Set to @True@ to enable tracing in unification
traceUnification :: Bool
traceUnification = True

-- | Unify the given constraints, returning a conservative type map for all type
-- variables.
unifyConstraints :: Int -> [TyConstraint] -> Map TyVar FTy
unifyConstraints nextRV initialConstraints =
  let fvs = foldr (Set.union . freeTyVars) Set.empty initialConstraints
      finalCtx = runConstraintSolvingMonad (emptyContext nextRV) (addConstraints initialConstraints >> reduceContext)
      finalTyMap = fmap (finalizeRowVar . removeTyVars) (ctxTyVarMap finalCtx)
      unknownTyMap = Map.fromSet (const unknownTy) fvs
   in Map.union finalTyMap unknownTyMap

finalizeRowVar :: Ty Unknown RowVar -> FTy
finalizeRowVar = \case
  UnknownTy {} -> unknownTy
  NumTy sz -> NumTy sz
  PtrTy t -> PtrTy (finalizeRowVar t)
  RecTy flds _ -> RecTy (finalizeRowVar <$> flds) NoRow

-- TODO: check that this is used correctly!
replaceRowVar :: RowVar -> RowVar -> RowVar -> RowVar
replaceRowVar r1 r2 r = if r == r1 then r2 else r

substRowVarInConstraintSolvingState ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  ConstraintSolvingState ->
  ConstraintSolvingMonad ConstraintSolvingState
substRowVarInConstraintSolvingState r1 r2 os ctx = do
  (rowShiftCs, eqCsFromRowShiftCs) <-
    second catMaybes
      . unzip
      <$> mapM (substRowVarInRowShiftC r1 r2 os) (ctxRowShiftCs ctx)
  let (eqCsFromEqRowCs, newEqRowCs) = partitionEithers $ substRowVarInEqRowC r1 r2 os <$> ctxEqRowCs ctx
  orCs <- mapM (substRowVarInOrC r1 r2 os) (ctxOrCs ctx)
  return $
    ConstraintSolvingState
      { ctxEqCs = (substRowVar r1 r2 os <$> ctxEqCs ctx) ++ eqCsFromEqRowCs ++ eqCsFromRowShiftCs,
        ctxInRowCs = substRowVar r1 r2 os <$> ctxInRowCs ctx,
        ctxRowShiftCs = rowShiftCs, -- substRowVarInRowShiftC r1 r2 os <$> ctxRowShiftCs ctx,
        ctxOrCs = orCs,
        ctxEqRowCs = newEqRowCs,
        ctxTyVarMap = substRowVar r1 r2 os <$> ctxTyVarMap ctx,
        ctxRowShiftMap = unifyRowVarMap r1 r2 (Set.map (second (replaceRowVar r1 r2))) (unifyRowShiftMap r1 r2) (ctxRowShiftMap ctx),
        -- Not modified because we only substitute in unprocessed constraints:
        ctxAbsurdEqCs = ctxAbsurdEqCs ctx,
        ctxOccursCheckFailures = ctxOccursCheckFailures ctx,
        nextTraceId = nextTraceId ctx,
        nextRowVar = nextRowVar ctx
      }

unifyRowShiftMap ::
  RowVar ->
  RowVar ->
  Set (Offset, RowVar) ->
  Set (Offset, RowVar) ->
  Set (Offset, RowVar)
unifyRowShiftMap r1 r2 s1 s2 = Set.map (second (replaceRowVar r1 r2)) (Set.union s1 s2)

unifyRowVarMap :: RowVar -> RowVar -> (v -> v) -> (v -> v -> v) -> Map RowVar v -> Map RowVar v
unifyRowVarMap r1 r2 unify union m = unify <$> Map.delete r1 (Map.alter insertOrAlter r2 m)
  where
    insertOrAlter Nothing = Map.lookup r1 m
    insertOrAlter (Just v) = Just (maybe v (union v) (Map.lookup r1 m))

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

-- | Does the context have any equality or subtype constraints left to process?
hasAtomicConstraints :: ConstraintSolvingState -> Bool
hasAtomicConstraints ctx =
  not
    ( and
        [ null $ ctxEqCs ctx,
          null $ ctxInRowCs ctx,
          null $ ctxRowShiftCs ctx,
          null $ ctxEqRowCs ctx
        ]
    )

popField :: Lens' ConstraintSolvingState [a] -> ConstraintSolvingMonad (Maybe a)
popField fld =
  use fld >>= \case
    [] -> return Nothing
    (c : cs) -> do
      modify $ set fld cs
      return (Just c)

dequeueEqC :: ConstraintSolvingMonad (Maybe EqC)
dequeueEqC = popField (field @"ctxEqCs")

dequeueInRowC :: ConstraintSolvingMonad (Maybe InRowC)
dequeueInRowC = popField (field @"ctxInRowCs")

dequeueRowShiftC :: ConstraintSolvingMonad (Maybe RowShiftC)
dequeueRowShiftC = popField (field @"ctxRowShiftCs")

dequeueEqRowC :: ConstraintSolvingMonad (Maybe EqRowC)
dequeueEqRowC = popField (field @"ctxEqRowCs")

addConstraints :: [TyConstraint] -> ConstraintSolvingMonad ()
addConstraints = mapM_ go
  where
    go :: TyConstraint -> ConstraintSolvingMonad ()
    go (EqTC c) = modify $ over (field @"ctxEqCs") (c :)
    go (InRowTC c) = modify $ over (field @"ctxInRowCs") (c :)
    go (RowShiftTC c) = modify $ over (field @"ctxRowShiftCs") (c :)
    go (OrTC c) = modify $ over (field @"ctxOrCs") (c :)
    go (AndTC (AndC cs)) = addConstraints cs
    go (EqRowTC c) = modify $ over (field @"ctxEqRowCs") (c :)

emptyContext :: Int -> ConstraintSolvingState
emptyContext = ConstraintSolvingState [] [] [] [] [] [] [] Map.empty Map.empty 0

-- | Is an equality constraint absurd?
absurdEqC :: EqC -> Bool
absurdEqC (EqC l r) = inconsistent l r

-- | Is an equality constraint trivial?
trivialEqC :: EqC -> Bool
trivialEqC (EqC l r) = l == r

-- | Updates the context with the given equality. This action is "atomic" in the
-- sense that it may generate some additional implied constraints, but it does
-- not also solve those as well. It handles exactly one constraint.
solveEqC :: EqC -> ConstraintSolvingMonad ()
solveEqC c
  | trivialEqC c = pure ()
  | absurdEqC c = modify $ over (field @"ctxAbsurdEqCs") (c :)
  | otherwise = do
    addConstraints =<< decomposeEqC c
    case (eqLhs c, eqRhs c) of
      (UnknownTy x, t) -> solveVarEq x t
      (t, UnknownTy x) -> solveVarEq x t
      (_, _) -> pure ()
  where
    solveVarEq :: TyVar -> ITy -> ConstraintSolvingMonad ()
    solveVarEq x t =
      if Set.member x (freeTyVars t)
        then -- if we fail the occurs check, remember that for debugging and move on
          modify $ over (field @"ctxOccursCheckFailures") (c :)
        else -- Otherwise, record x -> t in the type map
        -- and perform that substutition everywhere else.
        do
          modify $ over (field @"ctxTyVarMap") (Map.insert x t)
          modify $ substTyVar (x, t)

-- | Updates the context with an @InRowC@ constraint. Like @solveEqC@ this is an
-- "atomic" update.
solveInRowC :: InRowC -> ConstraintSolvingMonad ()
solveInRowC (InRowC r o t) = do
  r' <- freshRowVar
  addConstraints [eqRowTC r (Map.singleton o t) r']

-- | Updates the context with a @RowShiftC@ constraint. Like @solveEqC@ this is
-- an "atomic" update.
solveRowShiftC :: RowShiftC -> ConstraintSolvingMonad ()
solveRowShiftC (RowShiftC r1 n r2) =
  modify $ over (field @"ctxRowShiftMap") (Map.alter alter r1)
  where
    alter Nothing = Just (Set.singleton (n, r2))
    alter (Just s) = Just (Set.union s (Set.singleton (n, r2)))

-- | TODO
solveEqRowC :: EqRowC -> ConstraintSolvingMonad ()
solveEqRowC (EqRowC r1 os r2) = do
  -- TODO (val) There's gotta be a combinator for doing this?
  s <- get
  s' <- substRowVarInConstraintSolvingState r1 r2 os s
  put s'

-- | Process all atomic (i.e., non-disjunctive) constraints, updating the
-- context with each.
processAtomicConstraints :: ConstraintSolvingMonad ()
processAtomicConstraints = traceContext "processAtomicConstraints" $ do
  dequeueEqC >>= \case
    Just c -> solveEqC c >> processAtomicConstraints
    Nothing ->
      dequeueInRowC >>= \case
        Just c -> solveInRowC c >> processAtomicConstraints
        Nothing ->
          dequeueRowShiftC >>= \case
            Just c -> solveRowShiftC c >> processAtomicConstraints
            Nothing ->
              dequeueEqRowC >>= \case
                Just c -> solveEqRowC c >> processAtomicConstraints
                Nothing -> return ()

-- | Reduce/simplify a constraint in a given context.
reduceC :: TyConstraint -> ConstraintSolvingMonad TyConstraint
reduceC tc =
  case tc of
    EqTC c
      | absurdEqC c -> return absurdTC
      | trivialEqC c -> return trivialTC
      | otherwise -> return tc
    AndTC (AndC cs) -> andTC <$> mapM reduceC cs
    OrTC (OrC cs) -> orTC <$> mapM reduceC cs
    EqRowTC {} -> return tc
    InRowTC {} -> return tc
    RowShiftTC {} -> return tc

-- | Attempt to reduce and eliminate disjunctions in the given context. Any
-- resulting non-disjuncts are added to their respective field in the context.
reduceDisjuncts :: ConstraintSolvingMonad ()
reduceDisjuncts = traceContext "reduceDisjuncts" $ do
  disjs <- gets ctxOrCs
  modify $ set (field @"ctxOrCs") []
  mapM_ elimOr disjs
  where
    elimOr :: OrC -> ConstraintSolvingMonad ()
    elimOr (OrC ds) = addConstraints . (: []) . orTC =<< mapM reduceC ds

-- | Reduce a context by processing its atomic constraints and then attempting
-- to reduce disjucts to atomic constraints. Halts once no more atomic constraints
-- exist or can be inferred from the disjunctions.
reduceContext :: ConstraintSolvingMonad ()
reduceContext = traceContext "reduceContext" $ do
  processAtomicConstraints
  reduceDisjuncts
  whenM (gets hasAtomicConstraints) reduceContext

-- | Remove any remaining @TyVar@s, replacing them with @UnknownTy ()@.
removeTyVars :: ITy -> Ty Unknown RowVar
removeTyVars = \case
  UnknownTy _ -> unknownTy
  NumTy sz -> NumTy sz
  PtrTy ty -> PtrTy $ removeTyVars ty
  RecTy flds rv -> RecTy (fmap removeTyVars flds) rv

-- | @decomposeEqC t1 t2@ returns constraints implied from @EqP t1 t2@.
decomposeEqC :: EqC -> ConstraintSolvingMonad [TyConstraint]
decomposeEqC (EqC lhs rhs) = go lhs rhs
  where
    go :: ITy -> ITy -> ConstraintSolvingMonad [TyConstraint]
    go UnknownTy {} _ = pure []
    go _ UnknownTy {} = pure []
    go (PtrTy t1) (PtrTy t2) = pure [eqTC t1 t2]
    go (RecTy fs1 r1) (RecTy fs2 r2)
      | Map.null fs1 && Map.null fs2 = pure [eqRowTC r1 mempty r2]
      | Map.null fs1 = pure [eqRowTC r1 fs2 r2]
      | Map.null fs2 = pure [eqRowTC r2 fs1 r1]
      | otherwise = do
        let -- shared fields must be equal
            fldPs =
              map (uncurry eqTC) $
                Map.elems $
                  Map.intersectionWith (,) fs1 fs2
            -- fields from fs2 not in fs1 must appear in r1
            r1Ps =
              map (uncurry (inRowTC r1)) $
                Map.toList $ Map.difference fs2 fs1
            -- fields from fs1 not in fs2 must appear in r2
            r2Ps =
              map (uncurry (inRowTC r2)) $
                Map.toList $ Map.difference fs1 fs2
        -- We need a fresh row variable for the "remainder" of the row variables
        -- on each side when you factor out the fields mentioned across.
        --
        -- E.g. when you start with RecTy { 0 : i8 } r1 = RecTy { 8 : i8 } r2, you
        -- should conclude that there exists a r3 s. t.
        --
        -- r1 = { 8 : i8 | r3 }   and   r2 = { 0 : i8 | r3 }
        r3 <- freshRowVar
        let eqr1 = eqTC (RecTy mempty r1) (RecTy (Map.difference fs2 fs1) r3)
        let eqr2 = eqTC (RecTy mempty r2) (RecTy (Map.difference fs1 fs2) r3)
        pure $ eqr1 : eqr2 : fldPs ++ r1Ps ++ r2Ps
    go (RecTy fs r) t = pure $ goRecNonRec fs r t
    go t (RecTy fs r) = pure $ goRecNonRec fs r t
    go _ _ = pure []
    -- Handle the case when a record and a non-record are equal,
    -- i.e., treat the non-record as a record with one field `0`.
    goRecNonRec :: Map Offset ITy -> RowVar -> ITy -> [TyConstraint]
    goRecNonRec flds row nonRecTy = case Map.lookup (Offset 0) flds of
      Nothing -> [inRowTC row (Offset 0) nonRecTy]
      Just ty0 -> [eqTC nonRecTy ty0]
