------------------------------------------------------------------------
-- |
-- Module           : Reopt.Utils.GHCBug
-- Description      : Utilities for working around GHC bugs.
-- Copyright        : (c) Galois, Inc 2015
-- Stability        : provisional
--
-- Utilities for working around GHC bugs. They generally take the form
-- of identity functions which work around bugs in the GHC type
-- checker.
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Reopt.Utils.GHCBug where

import           GHC.TypeLits

import           Data.Parameterized.Classes

------------------------------------------------------------------------

-- | An identity function workaround for GHC bug #10507
--
-- https://ghc.haskell.org/trac/ghc/ticket/10507
nonLoopingCoerce :: (x :~: y) -> v (f x) -> v (f y)
nonLoopingCoerce Refl z = z

-- | Version of 'nonLoopingCoerce' with flipped equality.
nonLoopingCoerce' :: (y :~: x) -> v (f x) -> v (f y)
nonLoopingCoerce' Refl z = z

-- | An identity function workaround for GHC bug #10742
--
-- https://ghc.haskell.org/trac/ghc/ticket/10742
elimReflexiveTrans :: proxy n -> ((1 <=? n) ~ (1 <=? n) => a) -> a
elimReflexiveTrans _ x = x
