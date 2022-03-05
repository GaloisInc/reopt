{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Solver.Solver
  ( unifyConstraints,
  )
where

import           Control.Lens          ((%%~), (%=), (.=), (<<+=), (<<.=))
import           Control.Monad         (void, when)
import           Control.Monad.State   (MonadState (get), gets)
import           Data.Bifunctor        (first)
import           Data.Foldable         (traverse_)
import           Data.Generics.Product (field)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set
import           Debug.Trace           (trace)
import qualified Prettyprinter         as PP

import           Data.Graph            (SCC (..))
import           Data.Graph.SCC        (stronglyConnCompR)

import           Reopt.TypeInference.Solver.Constraints             (EqC (..),
                                                                     EqRowC (EqRowC),
                                                                     OperandClass (..),
                                                                     PtrAddC (..))
import           Reopt.TypeInference.Solver.Monad                   (ConstraintSolvingState (..),
                                                                     SolverM,
                                                                     addTyVarEq,
                                                                     addTyVarEq',
                                                                     defineTyVar,
                                                                     dequeueEqC,
                                                                     dequeueEqRowC,
                                                                     freshRowVar,
                                                                     freshTyVar,
                                                                     lookupTyVar,
                                                                     ptrWidthNumTy,
                                                                     undefineTyVar,
                                                                     unsafeUnifyTyVars, traceUnification)
import           Reopt.TypeInference.Solver.RowVariableSubstitution (substRowVarInEqRowC,
                                                                     substRowVarInITy,
                                                                     substRowVarInITy',
                                                                     unifyRecTy)
import           Reopt.TypeInference.Solver.RowVariables            (NoRow (NoRow),
                                                                     RowExpr (..))
import           Reopt.TypeInference.Solver.TypeVariables           (TyVar)
import           Reopt.TypeInference.Solver.Types                   (FTy (..),
                                                                     FreeTyVars (freeTyVars),
                                                                     ITy (..),
                                                                     ITy',
                                                                     TyF (..))
import           Reopt.TypeInference.Solver.UnionFindMap            (UnionFindMap)
import qualified Reopt.TypeInference.Solver.UnionFindMap            as UM


-- | Unify the given constraints, returning a conservative type map for all type
-- variables.

-- FIXME: probably want to export the Eqv map somehow
unifyConstraints :: SolverM (Map TyVar FTy)
unifyConstraints = do
  processAtomicConstraints
  m <- gets ctxTyVars
  pure (finalizeTypeDefs m)

-- FIXME: this breaks the abstraction of the UnionFindMap.
finalizeTypeDefs :: UnionFindMap TyVar ITy' -> Map TyVar FTy
finalizeTypeDefs um@(UM.UnionFindMap eqvs defs) =
  Map.union eqvRes defRes -- Maps should be disjoint
  where
    -- Include equivalences.
    eqvRes = Map.fromSet mkOneEqv (Map.keysSet eqvs)
    mkOneEqv k =
      let (k', _) = UM.lookupRep k um
      in resolveOne defRes k'

    defRes = foldl goSCC mempty sccs
    resolveOne m t = Map.findWithDefault UnknownTy t m

    goSCC m  (AcyclicSCC (ty, tv, _)) = Map.insert tv (FTy $ resolveOne m <$> ty) m
    goSCC _m (CyclicSCC _ ) = error "FIXME: cycles detected"

    sccs :: [ SCC (TyF NoRow TyVar, TyVar, [TyVar]) ]
    sccs  = stronglyConnCompR nodes

    normTyVar t = fst (UM.lookupRep t um)

    nodes = [ (finalizeRowVar ty, tv, Set.toList (freeTyVars ty))
            | (tv, ty) <- Map.toList (fmap normTyVar <$> defs) ]

finalizeRowVar :: TyF r f -> TyF NoRow f
finalizeRowVar (NumTy sz) = NumTy sz
finalizeRowVar (PtrTy t)  = PtrTy t
finalizeRowVar (RecTy flds _) = RecTy (Map.filterWithKey (\k _ -> k >= 0) flds) NoRow

-- | @traceContext description ctx ctx'@ reports how the context changed via @trace@.
traceContext :: PP.Doc () -> SolverM () -> SolverM ()
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
  action
  when doTrace $ do
    stateAfter <- get
    let msg =
          PP.vsep
            [ PP.hsep ["<<< ", PP.parens (PP.pretty tId), description],
              PP.indent 4 $ PP.pretty stateAfter
            ]
    trace (show msg) (return ())

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
      Nothing -> ptrAddSolver
  where
    ptrAddSolver = do
      ptrAdds <- field @"ctxPtrAddCs" <<.= mempty -- get constraints and clear them
      m_ptrAdds' <- traverse solvePtrAdd ptrAdds
      let (didWork, ptrAdds') = foldl goOne (False, []) m_ptrAdds'
      field @"ctxPtrAddCs" .= ptrAdds'

      when didWork processAtomicConstraints

    goOne (_, acc)       Nothing       = (True, acc)
    goOne (didWork, acc) (Just ptrAdd) = (didWork, ptrAdd : acc)

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

-- | This solves pointer-sized addition, where the result may or may
-- not be a pointer, dep. on its use and on the types of arguments. 
solvePtrAdd :: PtrAddC -> SolverM (Maybe PtrAddC)
solvePtrAdd c = do
  (rv, m_rty) <- lookupTyVar (ptrAddResult c)
  (lhsv, m_lhsTy) <- lookupTyVar (ptrAddLHS c)
  (rhsv, m_rhsTy) <- lookupTyVar (ptrAddRHS c)

  -- Note: try to avoid a default pattern here.
  case (m_rty, m_lhsTy, m_rhsTy, ptrAddClass c) of
    -- These shouldn't happen, added for completeness
    (Just RecTy {}, _, _, _) -> error "Saw a recty"
    (_, Just RecTy {}, _, _) -> error "Saw a recty"
    (_, _, Just RecTy {}, _) -> error "Saw a recty"

    -- Number cases, resolving the constraint
    --  1. The args must be numbers.  We could check the other types
    --     as this might result in extra work, but this way is a bit
    --     simpler.
    (Just NumTy {}, _, _, _) -> do
      addTyVarEq' rv lhsv
      addTyVarEq' rv rhsv
      pure Nothing

    --  2. The result must be a number
    (_, Just NumTy {}, Just NumTy {}, _) -> do
      addTyVarEq' rv lhsv
      addTyVarEq' rv rhsv -- probably redundant, but it unifies the vars.
      pure Nothing

    --  3. Should be covered by 2, here to keep the completeness checker happy.
    (_, Just NumTy {}, _, OCOffset _) -> do
      addTyVarEq' rv lhsv
      addTyVarEq' rv rhsv
      pure Nothing

    -- Pointer argument cases, resolving the constraint.
    --  1. The lhs is a pointer, but the rhs is symbolic.  In this
    --     case we under constrain the output, This is probably an
    --     array index.
    (_, Just PtrTy {}, _, OCSymbolic) -> do
      -- rv is a pointer, but otherwise unconstrained.
      void $ isPtr rv -- Under constrained
      -- rhsv is a number
      isNum rhsv
      pure Nothing

    --  2. Similar to (1)
    (_, _, Just PtrTy {}, OCSymbolic) -> do
      -- rv is a pointer, but otherwise unconstrained.
      void $ isPtr rv -- Under constrained
      -- lhsv is a number
      isNum lhsv
      pure Nothing

    --  3. The lhs is a pointer, and the rhs is an offset.
    --     In this case we have
    --       rty   = Ptr (Rec { | shift off r1})
    --       lhsTy = Ptr (Rec { | r1})
    --       rhsTy = num64
    (_, Just (PtrTy ptv), _, OCOffset o) -> do
      rptv <- isPtr rv
      rowv <- freshRowVar
      addTyVarEq rptv (ITy $ RecTy mempty (RowExprShift o rowv))
      addTyVarEq ptv  (ITy $ RecTy mempty (RowExprVar     rowv))

      -- rhsv is a number
      isNum rhsv
      pure Nothing

    --  4. We need the rhs to be a pointer but it is a (small)
    --     constant.  FIXME: This should probably not happen?
    (_, _, Just PtrTy {}, OCOffset _) -> do
      isNum rhsv
      pure Nothing

    --  5. The lhs is a pointer, and the rhs is a const. pointer.
    (_, Just PtrTy {}, _, OCPointer) -> do
      -- This is a conflict according to our heuristic, maybe we want
      -- to record a message?
      isNum lhsv
      pure Nothing

    --  6. The rhs is a pointer, not sure if this will occur.
    (_, _, Just PtrTy {}, OCPointer) -> do
      void $ isPtr rv -- Under constrained
      isNum lhsv
      pure Nothing

    -- Pointer result cases, resolving the constraint

    --  1. The result is a pointer, and the rhs is an offset.  Similar to
    --     (3) above
    (Just (PtrTy rptv), _, _, OCOffset o) -> do
      ptv <- isPtr lhsv
      rowv <- freshRowVar
      addTyVarEq rptv (ITy $ RecTy mempty (RowExprShift o rowv))
      addTyVarEq ptv  (ITy $ RecTy mempty (RowExprVar     rowv))

      -- rhsv is a number
      isNum rhsv
      pure Nothing

    --  2. The result is a pointer, the rhs is a number, the rhs is
    --  then a pointer.
    (Just PtrTy {}, _, Just NumTy {}, _) -> do
      void $ isPtr lhsv -- Under constrained
      pure Nothing

    --  2. The result is a pointer, the lhs is a number, the rhs is
    --  then a pointer.  
    (Just PtrTy {}, Just NumTy {}, _, _) -> do
      void $ isPtr rhsv -- Under constrained
      pure Nothing

    --  3. LHS is a number, we can infer RHS is a pointer.    
    (Just PtrTy {}, _, _, OCPointer) -> do
      void $ isPtr rhsv -- Under constrained
      isNum lhsv
      pure Nothing

    -- We can't do anything for these constraints.
    (Nothing, Nothing, Nothing, _)       -> tryLater
    (Nothing, Just NumTy {}, Nothing, _) -> tryLater
    (Nothing, Nothing, Just NumTy {}, _) -> tryLater
    (Just PtrTy {}, Nothing, Nothing, _) -> tryLater
  where
    tryLater = pure (Just c)
    isNum tv = addTyVarEq tv . ITy =<< ptrWidthNumTy
    isPtr tv = do
      tv' <- freshTyVar Nothing Nothing
      addTyVarEq tv (ITy (PtrTy tv'))
      pure tv'



