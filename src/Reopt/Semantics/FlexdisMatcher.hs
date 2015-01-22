------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.FlexdisMatcher
-- Description      : Pattern matches against a Flexdis86 InstructionInstance.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This contains a function "execInstruction" that steps a single Flexdis86
-- instruction.
------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Reopt.Semantics.FlexdisMatcher
  ( execInstruction
  ) where

import qualified Flexdis86 as F
import Reopt.Semantics
import Reopt.Semantics.Monad

import Data.Parameterized.NatRepr

data SomeBV v where
  SomeBV :: SupportedBVWidth v n => v (BVType n) -> SomeBV v

getValue :: FullSemantics m => F.Value -> m (SomeBV (Value m))
getValue = undefined

getBVLocation :: NatRepr n -> F.Value -> m (MLocation m (BVType n))
getBVLocation = undefined

execInstruction :: FullSemantics m => F.InstructionInstance -> m ()
execInstruction ii =
  case (F.iiOp ii, F.iiArgs ii) of
    ("add", [loc, val]) -> do
      SomeBV v <- getValue val
      l <- getBVLocation (bv_width v) loc
      exec_add l v
    _ -> fail $ "Unsupported instruction: " ++ show ii