------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.Concrete
-- Description      : Free instance for Reopt.Semantics.Monad.Semantics
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Nathan Collins <conathan@galois.com>
-- Stability        : provisional
--
-- This contains an implementation of the classes defined in
-- Reopt.Semantics.Monad that treat some class methods as
-- uninterpreted functions.
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-} -- MaybeF

module Reopt.Semantics.Concrete
       (
       ) where

import           Control.Applicative
import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.Foldable as Fold
import           Data.Maybe
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Text.PrettyPrint.ANSI.Leijen (pretty, Pretty(..))

import           Unsafe.Coerce -- Only required to work around a ghc crash


import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen (text, colon, (<>), (<+>))

import qualified Flexdis86 as Flexdis
import           Reopt.Memory
import           Reopt.Semantics.FlexdisMatcher (execInstruction)
import           Reopt.Semantics.Monad
  ( Type(..)
  , TypeRepr(..)
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import           Reopt.Semantics.Representation
import qualified Reopt.Semantics.StateNames as N
import           Reopt.Semantics.Types (TypeBits)

------------------------------------------------------------------------
-- Expr
--
-- The 'Expr' data type and width related functions are copied from /
-- based on 'Reopt.Semantics.Implementation'. We need a different
-- 'IsValue' instance, so we duplicate these definitions here, and
-- extend them where the 'App' constructors are inadequate.
--
-- To reuse more 'Expr' code directly, an alternative approach would
-- be to add another type index to 'Expr' or the 'IsValue' class.  Of
-- course, we'll want the 'Expr' pretty printer down the road, so
-- maybe the indexing is inevitable? But then we need to make the
-- 'App' constructors more uniform, eliminating the need for extra
-- constructors below in our version 'Expr'.

-- | A pure expression for isValue.
data Expr tp where
  -- An expression obtained from some value.
  ValueExpr :: !(Value tp) -> Expr tp
  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(App Expr tp) -> Expr tp

  -- Extra constructors where 'App' does not provide what we want.
  LowerHalfExpr :: (1 <= n) =>
    !(NatRepr (n+n)) -> !(Expr (BVType (n+n))) -> Expr (BVType n)
  UpperHalfExpr :: (1 <= n) =>
    !(NatRepr (n+n)) -> !(Expr (BVType (n+n))) -> Expr (BVType n)
  -- Here 'App' has 'Trunc', but its type is different; see notes at
  -- bottom of file.
  TruncExpr :: (1 <= m, m <= n) =>
    !(NatRepr m) -> Expr (BVType n) -> Expr (BVType m)
  -- Here 'app' has 'SExt', but its type is different as with 'Trunc'.
  -- But, strangely, the strict version of 'uext' is in the 'IsValue'
  -- class as 'uext'', so we can use 'App's 'UExt' there ... seems ad
  -- hoc.
  SExtExpr :: (1 <= m, m <= n) =>
    !(NatRepr n) -> Expr (BVType m) -> Expr (BVType n)

app :: App Expr tp -> Expr tp
app = AppExpr

exprType :: Expr tp -> S.TypeRepr tp
exprType (ValueExpr v) = valueType v
exprType (AppExpr a) = appType a
-- Added constructors for lower and upper half to avoid duplicating a
-- bunch of code in 'Reopt.Semantics.Implementation'.
exprType (LowerHalfExpr r _) = S.BVTypeRepr $ halfNat r
exprType (UpperHalfExpr r _) = S.BVTypeRepr $ halfNat r
exprType (TruncExpr r _) = S.BVTypeRepr r
exprType (SExtExpr r _) = S.BVTypeRepr r

-- | Return width of expression.
exprWidth :: Expr (BVType n) -> NatRepr n
exprWidth e =
  case exprType e of
    S.BVTypeRepr n -> n

-- In this instance we don't override the default implementations. If
-- we wanted to, we'd have to extend the 'App' type with the
-- corresponding constructors, or add them to 'Expr' above.
instance S.IsValue Expr where
  bv_width = exprWidth
  mux c x y = app $ Mux (exprWidth x) c x y
  bvLit n v = ValueExpr $ mkLit n (toInteger v)
  bvAdd x y = app $ BVAdd (exprWidth x) x y
  bvSub x y = app $ BVSub (exprWidth x) x y
  bvMul x y = app $ BVMul (exprWidth x) x y
  complement x = app $ BVComplement (exprWidth x) x
  x .&. y = app $ BVAnd (exprWidth x) x y
  x .|. y = app $ BVOr (exprWidth x) x y
  bvXor x y = app $ BVXor (exprWidth x) x y
  x .=. y = app $ BVEq x y
  bvSplit v = (UpperHalfExpr (exprWidth v) v, LowerHalfExpr (exprWidth v) v)
  bvShr x y = app $ BVShr (exprWidth x) x y
  bvSar x y = app $ BVSar (exprWidth x) x y
  bvShl x y = app $ BVShl (exprWidth x) x y
  bvTrunc w x = TruncExpr w x
  bvUlt x y = app $ BVUnsignedLt x y
  bvSlt x y = app $ BVUnsignedLt x y
  bvBit x y = app $ BVBit x y
  sext w x = SExtExpr w x
  uext' w x = app $ UExt x w
  even_parity x = app $ EvenParity x
  reverse_bytes x = app $ ReverseBytes (exprWidth x) x
  uadc_overflows x y c = app $ UadcOverflows (exprWidth x) x y c
  sadc_overflows x y c = app $ SadcOverflows (exprWidth x) x y c
  usbb_overflows x y c = app $ UsbbOverflows (exprWidth x) x y c
  ssbb_overflows x y c = app $ SsbbOverflows (exprWidth x) x y c
  bsf x = app $ Bsf (exprWidth x) x
  bsr x = app $ Bsr (exprWidth x) x
  isQNaN rep x = app $ FPIsQNaN rep x
  isSNaN rep x = app $ FPIsSNaN rep x
  fpAdd rep x y = app $ FPAdd rep x y
  fpAddRoundedUp rep x y = app $ FPAddRoundedUp rep x y
  fpSub rep x y = app $ FPSub rep x y
  fpSubRoundedUp rep x y = app $ FPSubRoundedUp rep x y
  fpMul rep x y = app $ FPMul rep x y
  fpMulRoundedUp rep x y = app $ FPMulRoundedUp rep x y
  fpDiv rep x y = app $ FPDiv rep x y
  fpLt rep x y = app $ FPLt rep x y
  fpEq rep x y = app $ FPEq rep x y
  fpCvt src tgt x = app $ FPCvt src x tgt
  fpCvtRoundsUp src tgt x = app $ FPCvtRoundsUp src x tgt
  fpFromBV tgt x = app $ FPFromBV x tgt
  truncFPToSignedBV tgt src x = app $ TruncFPToSignedBV src x tgt

-- ??? Why do the 'App' constructors take a 'NatRepr' argument? It can
-- always be reconstructed by the user using 'bv_width' after
-- unpacking, no?

-- ??? Why do 'Trunc' and 'bvTrunc' have slightly different constraints?
-- 'Trunc   :: (1 <= n, n+1 <= m) => ...'
-- 'bvTrunc :: (1 <= n, n   <= m) => ...'
--
-- Answer: because 'Trunc' is only used for *strict* truncations. The
-- 'testStrictLeq' function in
-- reopt.git/deps/parameterized-utils/src/Data/Parameterized/NatRepr.hs
-- is used to turn a proof of 'm <= n' into a proof of 'm < n \/ m =
-- n' and 'Trunc' is only used in cases where 'm < n', i.e. 'm+1 <=
-- n'.

-- ??? Why does 'bvTrunc' take a 'NatRepr' argument?
--
-- Answer: because it specifies the return type. Same with 'sext' and
-- 'uext'.

-- ??? Why does 'Trunc' take it's 'NatRepr' argument second? (Nearly?)
-- all the other 'NatRepr' args come first in 'App' constructors.

-- TODO: rename for consistency:
--
-- - complement -> bvComplement
-- - Trunc -> BVTrunc
