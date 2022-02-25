{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Constraints.Solving.RowVariableSubstitution where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Reopt.TypeInference.Constraints.Solving.Constraints
  ( AndC (..),
    EqC (EqC),
    EqRowC (EqRowC),
    InRowC (InRowC),
    OrC (..),
    TyConstraint (..),
  )
import Reopt.TypeInference.Constraints.Solving.Monad
  ( ConstraintSolvingMonad,
    substRowVarInRowShiftC,
  )
import Reopt.TypeInference.Constraints.Solving.RowVariables (RowVar)
import Reopt.TypeInference.Constraints.Solving.Types
  ( ITy,
    Offset,
    Ty (..),
  )

class SubstRowVar a where
  substRowVar :: RowVar -> RowVar -> Map Offset ITy -> a -> a

instance SubstRowVar ITy where
  substRowVar r1 r2 os = \case
    UnknownTy v -> UnknownTy v
    NumTy sz -> NumTy sz
    PtrTy t -> PtrTy (substRowVar r1 r2 os t)
    RecTy flds rvar
      -- TODO: check for fields conflict
      | rvar == r1 -> RecTy (Map.union (substRowVar r1 r2 os <$> flds) os) r2
      | otherwise -> RecTy (substRowVar r1 r2 os <$> flds) rvar

instance SubstRowVar EqC where
  substRowVar r1 r2 os (EqC l r) = EqC (substRowVar r1 r2 os l) (substRowVar r1 r2 os r)

instance SubstRowVar InRowC where
  substRowVar r1 r2 os (InRowC r o t) =
    InRowC
      (if r == r1 && o `notElem` Map.keys os then r2 else r)
      o
      (substRowVar r1 r2 os t)

-- instance SubstRowVar OrC where
--   substRowVar r1 r2 os (OrC cs) = OrC (substRowVar r1 r2 os <$> cs)

-- instance SubstRowVar AndC where
--   substRowVar r1 r2 os (AndC cs) = AndC (substRowVar r1 r2 os <$> cs)

-- Until we had a row-shift term, substituting a row in an EqRowC can not
-- necessarily be represented as another EqRowC.
substRowVarInEqRowC :: RowVar -> RowVar -> Map Offset ITy -> EqRowC -> Either EqC EqRowC
-- substRowVarInEqRowC :: RowVar -> RowVar -> Map Offset ITy -> EqRowC -> (EqRowC, [InRowC])
substRowVarInEqRowC r1 r2 os (EqRowC r3 os' r4)
  -- In this case, we intend to replace r1 with {os | r2} in a constraint of the form:
  -- r1 = {os' | r4}
  -- This ought to mean the constraint:
  -- {os | r2} = {os' | r4}
  -- Now os and os' may overlap some.  For those offsets that overlap, we should
  -- check for conflicts, and otherwise omit them entirely from the resulting
  -- constraints.
  -- The remaining constraint being:
  -- {unique_os | r2} = {unique_os' | r4}
  -- which we can represent either as an EqC, or as a combination of an EqRowC
  -- and some InRowCs.
  -- Let's try an EqC for now as it makes the type simpler (no need for lists).
  | r3 == r1 =
    let uniqOs = Map.filterWithKey (\k _ -> k `notElem` Map.keys os') os
     in let uniqOs' = Map.filterWithKey (\k _ -> k `notElem` Map.keys os) os'
         in Left $ EqC (RecTy uniqOs r2) (RecTy uniqOs' r4)
  -- (EqRowC r2 os' r4, uncurry (InRowC r4) <$> filter ((`notElem` Map.keys os') . fst) (Map.assocs os))
  -- TODO: check for conflicts?
  | r4 == r1 = Right $ EqRowC r3 (Map.union os os') r4
  -- It should never be the case that both r3 and r4 are equal to r1, so this
  -- should only trigger when both are **not** r1.
  | otherwise = Right $ EqRowC r3 os' r4

substRowVarInTyConstraint ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  TyConstraint ->
  ConstraintSolvingMonad TyConstraint
substRowVarInTyConstraint r1 r2 os = \case
  EqTC c -> pure $ EqTC $ substRowVar r1 r2 os c
  InRowTC c -> pure $ InRowTC $ substRowVar r1 r2 os c
  RowShiftTC c -> do
    (rsC, meqC) <- substRowVarInRowShiftC r1 r2 os c
    return $ case meqC of
      Nothing -> RowShiftTC rsC
      Just eqC -> AndTC (AndC [RowShiftTC rsC, EqTC eqC])
  EqRowTC c -> pure $ either EqTC EqRowTC (substRowVarInEqRowC r1 r2 os c)
  OrTC orC -> OrTC <$> substRowVarInOrC r1 r2 os orC
  AndTC andC -> AndTC <$> substRowVarInAndC r1 r2 os andC

substRowVarInAndC ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  AndC ->
  ConstraintSolvingMonad AndC
substRowVarInAndC r1 r2 os (AndC cs) =
  AndC <$> mapM (substRowVarInTyConstraint r1 r2 os) cs

substRowVarInOrC ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  OrC ->
  ConstraintSolvingMonad OrC
substRowVarInOrC r1 r2 os (OrC cs) =
  OrC <$> mapM (substRowVarInTyConstraint r1 r2 os) cs
