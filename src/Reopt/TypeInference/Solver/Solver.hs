{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Reopt.TypeInference.Solver.Solver
  ( unifyConstraints
  , ConstraintSolution (..)

  )
where

import           Control.Lens          (_1, over, view, (%%~), (%=), (%~), (.=),
                                        (<<+=), (<<.=))
import           Control.Monad         (when)
import           Control.Monad.State   (MonadState (get), gets)
import           Data.Bifunctor        (first)
import           Data.Foldable         (traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Graph            (SCC (..), flattenSCCs)
import           Data.Graph.SCC        (stronglyConnCompR)
import           Data.List             (partition)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set
import           Debug.Trace           (trace)
import           GHC.Generics          (Generic)
import qualified Prettyprinter         as PP

import           Reopt.TypeInference.Solver.Constraints             (EqC (..),
                                                                     EqRowC (EqRowC))
import           Reopt.TypeInference.Solver.Monad                   (Conditional (..),
                                                                     ConstraintSolvingState (..),
                                                                     SolverM,
                                                                     addTyVarEq',
                                                                     defineTyVar,
                                                                     dequeueEqC,
                                                                     dequeueEqRowC,
                                                                     lookupTyVar,
                                                                     traceUnification,
                                                                     undefineTyVar,
                                                                     unsafeUnifyTyVars)
import           Reopt.TypeInference.Solver.RowVariableSubstitution (substRowVarInEqRowC,
                                                                     substRowVarInITy,
                                                                     substRowVarInITy',
                                                                     unifyRecTy)
import           Reopt.TypeInference.Solver.RowVariables            (NoRow (NoRow))
import           Reopt.TypeInference.Solver.TypeVariables           (TyVar)
import           Reopt.TypeInference.Solver.Types                   (FTy (..),
                                                                     FreeTyVars (freeTyVars),
                                                                     ITy (..),
                                                                     ITy',
                                                                     StructName,
                                                                     TyF (..),
                                                                     prettyMap,
                                                                     tyVarToStructName)
import           Reopt.TypeInference.Solver.UnionFindMap            (UnionFindMap)
import qualified Reopt.TypeInference.Solver.UnionFindMap            as UM


-- | Unify the given constraints, returning a conservative type map for all type
-- variables.

data ConstraintSolution = ConstraintSolution
  { csTyVars :: Map TyVar FTy
  , csNamedStructs :: [(StructName, FTy)]
  }
  deriving (Eq, Show, Generic)

-- FIXME: probably want to export the Eqv map somehow
unifyConstraints :: SolverM ConstraintSolution
unifyConstraints = do
  processAtomicConstraints
  m <- gets ctxTyVars
  pure (finalizeTypeDefs m)

-- FIXME: this breaks the abstraction of the UnionFindMap.
finalizeTypeDefs :: UnionFindMap TyVar ITy' -> ConstraintSolution
finalizeTypeDefs um@(UM.UnionFindMap eqvs defs) =
  over (field @"csTyVars") (Map.union eqvRes) preSoln
  where
    -- Include equivalences.
    eqvRes = Map.fromSet mkOneEqv (Map.keysSet eqvs)

    mkOneEqv k =
      let (k', _) = UM.lookupRep k um
      in resolveOne (csTyVars preSoln) k'

    preSoln = foldl goSCC (ConstraintSolution mempty mempty) sccs

    -- duplicated from below.
    resolveOne m t = Map.findWithDefault UnknownTy t m

    goSCC r (AcyclicSCC (ty, tv, _)) = finaliseTyF tv ty r
    goSCC r (CyclicSCC cs) = finaliseCyclic cs r

    sccs :: [ SCC (TyF NoRow TyVar, TyVar, [TyVar]) ]
    sccs  = stronglyConnCompR nodes

    normTyVar t = fst (UM.lookupRep t um)

    nodes = [ (ty, tv, Set.toList (freeTyVars ty))
            | (tv, ty0) <- Map.toList defs -- tv is a rep. var.
            , let ty = finalizeRowVar (normTyVar <$> ty0) ]

finaliseTyF :: TyVar -> TyF NoRow TyVar ->
               ConstraintSolution -> ConstraintSolution
finaliseTyF tv ty =
  field @"csTyVars" %~ \m -> Map.insert tv (FTy $ resolveOne m <$> ty) m
  where
    resolveOne m t = Map.findWithDefault UnknownTy t m

finaliseCyclic :: [(TyF NoRow TyVar, TyVar, [TyVar])] ->
                  ConstraintSolution -> ConstraintSolution
finaliseCyclic cs s
  -- We have cycles even after removing records, perhaps through
  -- pointers, so we just drop the types (i.e., make Unknown)
  | any isCyclic sccs =
      trace ("Ignoring too cyclic types " ++
             show (PP.list
                   [ PP.pretty n PP.<+> "=" PP.<+> PP.pretty ty
                   | (ty, n, _) <- cs
                   ])) s
  | otherwise =
      -- trace ("Keeping cyclic types " ++
      --        show (PP.list
      --              [ PP.pretty n PP.<+> "=" PP.<+> PP.pretty ty
      --              | (ty, n, _) <- cs
      --              ])) $
      over (field @"csNamedStructs") (namedDefs ++) sWithNonRecs
  where
    namedDefs = [ (tyVarToStructName tv, FTy $ resolveOne <$> ty)
                | (ty, tv, _) <- recs ]
    
    -- Perform this topologically to ensure we have defs before we see
    -- them.
    sWithNonRecs =
      foldl (\s' (ty, v, _) -> finaliseTyF v ty s') sWithNamed nonRecs'

    sWithNamed = over (field @"csTyVars") (Map.union named) s

    named = Map.fromList [ (tv, NamedStruct $ tyVarToStructName tv)
                         | (_, tv, _) <- recs ]

    (recs, nonRecs) = partition (isRecTy . view _1) cs

    sccs = stronglyConnCompR nonRecs
    nonRecs' = flattenSCCs sccs

    resolveOne t = Map.findWithDefault UnknownTy t (csTyVars sWithNonRecs)

    isRecTy RecTy {} = True
    isRecTy _        = False

    isCyclic CyclicSCC {} = True
    isCyclic _            = False

finalizeRowVar :: TyF r f -> TyF NoRow f
finalizeRowVar (NumTy sz) = NumTy sz
finalizeRowVar (PtrTy t)  = PtrTy t
finalizeRowVar (RecTy flds _) = RecTy (Map.filterWithKey (\k _ -> k >= 0) flds) NoRow

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

substEqRowC :: EqRowC -> SolverM ()
substEqRowC (EqRowC r1 os r2) = do
  -- We need to update the rhs of the EqCs, the type defs, and the row eqs

  -- This may generate new equalities so we want to mappend the
  -- results.
  eqcs  <- field @"ctxEqCs" <<.= mempty -- get defs and clear them
  eqcs' <- traverse (field @"eqRhs" %%~ substRowVarInITy r1 os r2) eqcs
  field @"ctxEqCs" %= (eqcs' ++)

  defs  <- field @"ctxTyVars" <<.= UM.empty -- get defs and clear them
  defs' <- traverse (substRowVarInITy' r1 os r2) defs
  field @"ctxTyVars" .= defs'

  rEqs  <- field @"ctxEqRowCs" <<.= mempty -- get row eqs and clear them
  -- This will add back any we still need
  traverse_ (substRowVarInEqRowC r1 os r2) rEqs

-- | Process all atomic (i.e., non-disjunctive) constraints, updating the
-- context with each.
processAtomicConstraints :: SolverM ()
processAtomicConstraints = traceContext "processAtomicConstraints" $ do
  dequeueEqC >>= \case
    Just c  -> solveEqC c >> processAtomicConstraints
    Nothing -> dequeueEqRowC >>= \case
      Just c  -> substEqRowC c >> processAtomicConstraints
      Nothing -> condEqSolver >>= \case
        True -> processAtomicConstraints
        False -> pure ()
  where
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

solveEqC :: EqC -> SolverM ()
solveEqC eqc = do
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

    (PtrTy tv1', PtrTy tv2') -> addTyVarEq' tv1' tv2'

    (RecTy fs1 r1, RecTy fs2 r2) -> unifyRecTy fs1 r1 fs2 r2

    -- Unification failure, we need to report a conflict.
    _ ->
      trace ("Unification failed at " ++ show (PP.pretty tv) ++ ": " ++ show (PP.pretty ty1) ++ " and " ++ show (PP.pretty ty2)) $ pure ()
      -- pretend we saw nothing :(
      -- error $ "FIXME: conflict detected at " ++ show (PP.pretty tv)

--------------------------------------------------------------------------------
-- PtrAdd solving

-- -- | This solves pointer-sized addition, where the result may or may
-- -- not be a pointer, dep. on its use and on the types of arguments.
-- solvePtrAdd :: PtrAddC -> SolverM (Maybe PtrAddC)
-- solvePtrAdd c = traceContext ("solvePtrAdd" PP.<+> PP.pretty c) $ do
--   (rv, m_rty) <- lookupTyVar (ptrAddResult c)
--   (lhsv, m_lhsTy) <- lookupTyVar (ptrAddLHS c)
--   (rhsv, m_rhsTy) <- lookupTyVar (ptrAddRHS c)

--   -- If we see ptr' = ptr + off, this is what we do. Note we have
--   -- m_rty = Just (PtrTy rptrv), m_lhsTy = Just (PtrTy lptrv), up to
--   -- eqv.
--   let structOffsetCase rptrv lptrv off = do
--         rowv <- freshRowVar
--         addTyVarEq rptrv (ITy $ RecTy mempty (RowExprShift off rowv))
--         addTyVarEq lptrv (ITy $ RecTy mempty (RowExprVar       rowv))

--         -- rhsv is a number
--         isNum rhsv
--         pure Nothing -- In any case, we are done with this constraint.

--   -- Note: try to avoid a default pattern here.
--   case (m_rty, m_lhsTy, m_rhsTy, ptrAddClass c) of
--     -- These shouldn't happen, added for completeness
--     (Just RecTy {}, _, _, _) -> error "Saw a recty"
--     (_, Just RecTy {}, _, _) -> error "Saw a recty"
--     (_, _, Just RecTy {}, _) -> error "Saw a recty"

--     -- Number cases, resolving the constraint
--     --  1. The args must be numbers.  We could check the other types
--     --     as this might result in extra work, but this way is a bit
--     --     simpler.
--     (Just NumTy {}, _, _, _) -> do
--       addTyVarEq' rv lhsv
--       addTyVarEq' rv rhsv
--       pure Nothing

--     --  2. The result must be a number
--     (_, Just NumTy {}, Just NumTy {}, _) -> do
--       addTyVarEq' rv lhsv
--       addTyVarEq' rv rhsv -- probably redundant, but it unifies the vars.
--       pure Nothing

--     --  3. Should be covered by 2, here to keep the completeness checker happy.
--     (_, Just NumTy {}, _, OCOffset _) -> do
--       addTyVarEq' rv lhsv
--       addTyVarEq' rv rhsv
--       pure Nothing

--     -- The *p++ case, where we are looping through via a pointer, and
--     -- we thus have a constant offset.  This boils down to x = x +
--     -- off, although there may be a bit of work to get here, and we
--     -- won't hav ethe same type variables, only the same row variable.

--     -- Handles the recursive while () *p++ case.  If we try to unify
--     --
--     -- shift k r = shift j r
--     --
--     -- k + off =/= r
--     --
--     -- The we are in the array stride case and can set the result to
--     -- the first operand (more or less?)
--     (Just (PtrTy rptv), Just (PtrTy lptv), _, OCOffset o) -> do
--       (rptv', rrec) <- lookupTyVar rptv
--       (lptv', lrec) <- lookupTyVar lptv

--       case (rrec, lrec) of
--         (Just (RecTy _ rr), Just (RecTy _ lr))
--           | rowExprVar rr == rowExprVar lr
--           , rowExprShift rr /= rowExprShift lr + o
--           -- FIXME: This is a temporary fix for *p++
--             -> trace ("Dropping add equality " ++ show (PP.pretty c)) $
--                pure Nothing
--         _ -> structOffsetCase rptv' lptv' o

--     -- Pointer argument cases, resolving the constraint.
--     --  1. The lhs is a pointer, but the rhs is symbolic.  In this
--     --     case we under constrain the output, This is probably an
--     --     array index.
--     (_, Just PtrTy {}, _, OCSymbolic) -> do
--       -- rv is a pointer, but otherwise unconstrained.
--       void $ isPtr rv -- Under constrained
--       -- rhsv is a number
--       isNum rhsv
--       pure Nothing

--     --  2. Similar to (1)
--     (_, _, Just PtrTy {}, OCSymbolic) -> do
--       -- rv is a pointer, but otherwise unconstrained.
--       void $ isPtr rv -- Under constrained
--       -- lhsv is a number
--       isNum lhsv
--       pure Nothing

--     --  3. The lhs is a pointer, and the rhs is an offset.
--     --     In this case we have
--     --       rty   = Ptr (Rec { | shift off r1})
--     --       lhsTy = Ptr (Rec { | r1})
--     --       rhsTy = num64
--     (_, Just (PtrTy ptv), _, OCOffset o) -> do
--       rptv <- isPtr rv
--       structOffsetCase rptv ptv o

--     --  4. We need the rhs to be a pointer but it is a (small)
--     --     constant.  FIXME: This should probably not happen?
--     (_, _, Just PtrTy {}, OCOffset _) -> do
--       isNum rhsv
--       pure Nothing

--     --  5. The lhs is a pointer, and the rhs is a const. pointer.
--     (_, Just PtrTy {}, _, OCPointer) -> do
--       -- This is a conflict according to our heuristic, maybe we want
--       -- to record a message?
--       isNum lhsv
--       pure Nothing

--     --  6. The rhs is a pointer, not sure if this will occur.
--     (_, _, Just PtrTy {}, OCPointer) -> do
--       void $ isPtr rv -- Under constrained
--       isNum lhsv
--       pure Nothing

--     -- Pointer result cases, resolving the constraint

--     --  1. The result is a pointer, and the rhs is an offset.  Similar to
--     --     (3) above
--     (Just (PtrTy rptv), _, _, OCOffset o) -> do
--       ptv <- isPtr lhsv
--       structOffsetCase rptv ptv o

--     --  2. The result is a pointer, the rhs is a number, the rhs is
--     --  then a pointer.
--     (Just PtrTy {}, _, Just NumTy {}, _) -> do
--       void $ isPtr lhsv -- Under constrained
--       pure Nothing

--     --  2. The result is a pointer, the lhs is a number, the rhs is
--     --  then a pointer.
--     (Just PtrTy {}, Just NumTy {}, _, _) -> do
--       void $ isPtr rhsv -- Under constrained
--       pure Nothing

--     --  3. LHS is a number, we can infer RHS is a pointer.
--     (Just PtrTy {}, _, _, OCPointer) -> do
--       void $ isPtr rhsv -- Under constrained
--       isNum lhsv
--       pure Nothing

--     -- We can't do anything for these constraints.
--     (Nothing, Nothing, Nothing, _)       -> tryLater
--     (Nothing, Just NumTy {}, Nothing, _) -> tryLater
--     (Nothing, Nothing, Just NumTy {}, _) -> tryLater
--     (Just PtrTy {}, Nothing, Nothing, _) -> tryLater
--   where
--     tryLater = pure (Just c)
--     isNum tv = addTyVarEq tv . ITy =<< ptrWidthNumTy
--     isPtr tv = do
--       tv' <- freshTyVar Nothing Nothing
--       addTyVarEq tv (ITy (PtrTy tv'))
--       pure tv'

--------------------------------------------------------------------------------
-- Instances

instance PP.Pretty ConstraintSolution where
  pretty soln =
    PP.vsep $ prettyDefs (csNamedStructs soln)
           ++ prettyMap PP.pretty PP.pretty (csTyVars soln)
    where
      prettyDefs ds =
         [ PP.pretty n PP.<+> "=" PP.<+> PP.pretty ty
         | (n, ty) <- ds
         ]

