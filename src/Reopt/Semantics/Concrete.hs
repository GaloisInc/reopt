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
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-} -- MaybeF
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.Semantics.Concrete
       (
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure, Applicative)
#endif
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
import           Reopt.Object.Memory
import           Reopt.Semantics.FlexdisMatcher (execInstruction)
import           Reopt.Semantics.Monad
  ( Type(..)
  , TypeRepr(..)
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import qualified Reopt.CFG.Representation as R
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types (type_width, FloatInfo(..), TypeBits)

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
  LitExpr :: !(NatRepr n) -> Integer -> Expr (BVType n)
  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(R.App Expr tp) -> Expr tp

  -- Extra constructors where 'App' does not provide what we want.
  LowerHalfExpr :: (1 <= n) =>
    !(NatRepr (n+n)) -> !(Expr (BVType (n+n))) -> Expr (BVType n)
  UpperHalfExpr :: (1 <= n) =>
    !(NatRepr (n+n)) -> !(Expr (BVType (n+n))) -> Expr (BVType n)
  -- Here 'App' has 'Trunc', but its type is different; see notes at
  -- bottom of file.
  TruncExpr :: (1 <= m, m <= n) =>
    !(NatRepr m) -> !(Expr (BVType n)) -> Expr (BVType m)
  -- Here 'app' has 'SExt', but its type is different as with 'Trunc'.
  -- But, strangely, the strict version of 'uext' is in the 'IsValue'
  -- class as 'uext'', so we can use 'App's 'UExt' there ... seems ad
  -- hoc.
  SExtExpr :: (1 <= m, m <= n) =>
    !(NatRepr n) -> Expr (BVType m) -> Expr (BVType n)

mkLit :: NatRepr n -> Integer -> Value (BVType n)
mkLit n v = LitExpr n (v .&. mask)
  where mask = maxUnsigned n

app :: R.App Expr tp -> Expr tp
app = AppExpr

exprType :: Expr tp -> S.TypeRepr tp
exprType (LitExpr r _) = S.BVTypeRepr r
exprType (AppExpr a) = R.appType a
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
  mux c x y = app $ R.Mux (exprWidth x) c x y
  bvLit n v = mkLit n (toInteger v)
  bvAdd x y = app $ R.BVAdd (exprWidth x) x y
  bvSub x y = app $ R.BVSub (exprWidth x) x y
  bvMul x y = app $ R.BVMul (exprWidth x) x y
  complement x = app $ R.BVComplement (exprWidth x) x
  x .&. y = app $ R.BVAnd (exprWidth x) x y
  x .|. y = app $ R.BVOr (exprWidth x) x y
  bvXor x y = app $ R.BVXor (exprWidth x) x y
  x .=. y = app $ R.BVEq x y
  bvSplit v = (UpperHalfExpr (exprWidth v) v, LowerHalfExpr (exprWidth v) v)
  bvShr x y = app $ R.BVShr (exprWidth x) x y
  bvSar x y = app $ R.BVSar (exprWidth x) x y
  bvShl x y = app $ R.BVShl (exprWidth x) x y
  bvTrunc w x = TruncExpr w x
  bvUlt x y = app $ R.BVUnsignedLt x y
  bvSlt x y = app $ R.BVUnsignedLt x y
  bvBit x y = app $ R.BVBit x y
  sext w x = SExtExpr w x
  uext' w x = app $ R.UExt x w
  even_parity x = app $ R.EvenParity x
  reverse_bytes x = app $ R.ReverseBytes (exprWidth x) x
  uadc_overflows x y c = app $ R.UadcOverflows (exprWidth x) x y c
  sadc_overflows x y c = app $ R.SadcOverflows (exprWidth x) x y c
  usbb_overflows x y c = app $ R.UsbbOverflows (exprWidth x) x y c
  ssbb_overflows x y c = app $ R.SsbbOverflows (exprWidth x) x y c
  bsf x = app $ R.Bsf (exprWidth x) x
  bsr x = app $ R.Bsr (exprWidth x) x
  isQNaN rep x = app $ R.FPIsQNaN rep x
  isSNaN rep x = app $ R.FPIsSNaN rep x
  fpAdd rep x y = app $ R.FPAdd rep x y
  fpAddRoundedUp rep x y = app $ R.FPAddRoundedUp rep x y
  fpSub rep x y = app $ R.FPSub rep x y
  fpSubRoundedUp rep x y = app $ R.FPSubRoundedUp rep x y
  fpMul rep x y = app $ R.FPMul rep x y
  fpMulRoundedUp rep x y = app $ R.FPMulRoundedUp rep x y
  fpDiv rep x y = app $ R.FPDiv rep x y
  fpLt rep x y = app $ R.FPLt rep x y
  fpEq rep x y = app $ R.FPEq rep x y
  fpCvt src tgt x = app $ R.FPCvt src x tgt
  fpCvtRoundsUp src tgt x = app $ R.FPCvtRoundsUp src x tgt
  fpFromBV tgt x = app $ R.FPFromBV x tgt
  truncFPToSignedBV tgt src x = app $ R.TruncFPToSignedBV src x tgt

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
