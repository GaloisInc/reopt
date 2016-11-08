{-|
Module     : Data.Macaw.AbsDomain.Refine
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

This defines operations that use assertions to refine state.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Macaw.AbsDomain.Refine
  ( RefineConstraints
  , refineProcState
  ) where


import Control.Lens
import Data.Parameterized.Classes
import Data.Parameterized.NatRepr

import Data.Macaw.AbsDomain.AbsState
import Data.Macaw.CFG
import Data.Macaw.Memory (MemWidth)
import Data.Macaw.Types

-- | Constraints needed for refinement on abstract states.
type RefineConstraints arch
   = ( OrdF  (ArchReg arch)
     , ShowF (ArchReg arch)
     , HasRepr (ArchReg arch) TypeRepr
     , MemWidth (ArchAddrWidth arch)
     )

-- FIXME: if val \notin av then we should return bottom
-- @refineProcState v av s@ returns a processor state after we have
-- asserted that @v@ is contained in the set @AbsValue@.
refineProcState :: RefineConstraints arch
                => Value arch ids tp -- ^ Value in processor state
                -> AbsValue (ArchAddrWidth arch) tp -- ^ Abstract value to assign value.
                -> AbsProcessorState (ArchReg arch) ids
                -> AbsProcessorState (ArchReg arch) ids
refineProcState (BVValue _n _val) _av regs      = regs -- Skip refinment for literal values
refineProcState (RelocatableValue _ _) _av regs = regs -- Skip refinment for relocatable values
refineProcState (Initial r) av regs =
  regs & (absInitialRegs . boundValue r) %~ flip meet av
refineProcState (AssignedValue (Assignment a_id rhs)) av regs
  -- av is not a subset.
  | Nothing <- joinAbsValue av av_old = regs
  | otherwise = do
    -- Get joined abstract value.
    let av'    = meet av_old av
    -- Get registers after updating assignment value to av'
    let  regs'  = regs & (absAssignments . assignLens a_id) .~ av'
    case rhs of
      -- av adds new information, we need to refine any parents
      EvalApp app -> refineApp app av' regs'
      -- no parents, but update ass
      _ -> regs'
  where
    av_old = regs ^. absAssignments ^. assignLens a_id

refineApp :: RefineConstraints arch
          => App (Value arch ids) tp
          -> AbsValue (ArchAddrWidth arch) tp
          -> AbsProcessorState (ArchReg arch) ids
          -> AbsProcessorState (ArchReg arch) ids
refineApp app av regs =
  case app of

   -- If we know something about the result of a trunc, we can
   -- propagate back a subvalue.
   Trunc x sz -> refineTrunc x sz av regs

   -- Assertion "r <= x"
   BVUnsignedLt l r
     | Just b    <- asConcreteSingleton av -> refineLt l r b regs

   BVUnsignedLe l r
     | Just b    <- asConcreteSingleton av -> refineLeq l r b regs

   -- FIXME: HACK
   -- This detects r - x < 0 || r - x == 0, i.e. r <= x
   BVOr _ (getAssignApp -> Just (UsbbOverflows _ r xv@(BVValue sz x) (BVValue _ 0)))
          (getAssignApp -> Just (BVEq (getAssignApp -> Just (BVAdd _ r' y)) (BVValue _ 0)))
     | Just Refl <- testEquality r r'
     , Just Refl <- testEquality y (mkLit sz (negate x))
     , Just b    <- asConcreteSingleton av ->
       refineLeq r xv b regs

   -- FIXME: HACK
   -- This detects not (r - x < 0) && not (r - x == 0), i.e. x < r
   BVAnd _ (getAssignApp -> Just (BVComplement _
                                  (getAssignApp -> Just (UsbbOverflows _ r xv@(BVValue sz x) (BVValue _ 0)))))
           (getAssignApp -> Just (BVComplement _
                                  (getAssignApp -> Just (BVEq (getAssignApp -> Just (BVAdd _ r' y)) (BVValue _ 0)))))
     | Just Refl <- testEquality r r'
     , Just Refl <- testEquality y (mkLit sz (negate x))
     , Just b    <- asConcreteSingleton av ->
       refineLt xv r b regs

  -- Mux can let us infer the condition?
   _ -> regs
  where
    getAssignApp (AssignedValue (Assignment _ (EvalApp a))) = Just a
    getAssignApp _ = Nothing

refineTrunc :: ( (n + 1) <= n'
               , RefineConstraints arch
               )
            => BVValue arch ids n'
            -> NatRepr n
            -> AbsValue (ArchAddrWidth arch) (BVType n)
            -> AbsProcessorState (ArchReg arch) ids
            -> AbsProcessorState (ArchReg arch) ids
refineTrunc v sz av regs = refineProcState v (subValue sz av) regs

refineLt :: RefineConstraints arch
         => Value arch ids tp
         -> Value arch ids tp
         -> Integer
         -> AbsProcessorState (ArchReg arch) ids
         -> AbsProcessorState (ArchReg arch) ids
refineLt x y b regs
  -- y <= x
  | b == 0     = refineULeqTrue y x regs
  -- x < y case
  | otherwise  = refineULtTrue  x y regs

refineLeq :: RefineConstraints arch
          => Value arch ids tp
          -> Value arch ids tp
          -> Integer
          -> AbsProcessorState (ArchReg arch) ids
          -> AbsProcessorState (ArchReg arch) ids
refineLeq x y b regs
     -- y < x
    | b == 0     = refineULtTrue y x regs
    -- x <= y
    | otherwise  = refineULeqTrue x y regs

refineULeqTrue :: RefineConstraints arch
               => Value arch ids tp
               -> Value arch ids tp
               -> AbsProcessorState (ArchReg arch) ids
               -> AbsProcessorState (ArchReg arch) ids
refineULeqTrue x y regs = refineProcState x x_leq (refineProcState y y_leq regs)
  where
    (x_leq, y_leq) = abstractULeq (typeRepr x) (transferValue regs x) (transferValue regs y)
    -- check r@(a, b)
    --   | isBottom a = flip trace r $ "Bottom in refineLeq: "
    --                  ++ show (pretty regs)
    --   | isBottom b = flip trace r $ "Bottom in refineLeq: "
    --                  ++ show (pretty regs)
    --   | otherwise  = r

-- | Refine using the observation that x < y when treated as unsigned values.
refineULtTrue :: RefineConstraints arch
              => Value arch ids tp
              -> Value arch ids tp
              -> AbsProcessorState (ArchReg arch) ids
              -> AbsProcessorState (ArchReg arch) ids
refineULtTrue x y regs = refineProcState x x_lt (refineProcState y y_lt regs)
  where
    (x_lt, y_lt) = abstractULt (typeRepr x) (transferValue regs x) (transferValue regs y)
