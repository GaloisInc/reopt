------------------------------------------------------------------------
-- |
-- Module           : Reopt.Reified.Semantics
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
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-} -- MaybeF
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.Reified.Semantics
       ( execSemantics
       , ppStmts
       , ppStmt
       , Stmt(..)
       , Expr(..)
       , Variable(..)
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure, Applicative)
#endif
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer
  (censor, execWriterT, listen, tell, MonadWriter, WriterT)
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.BitVector (BV)
import qualified Data.BitVector as BV
import           Data.Functor
import           Data.Monoid (mempty)
import           Data.Parameterized.Classes (OrderingF(..), compareF, fromOrdering)
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Text.PrettyPrint.ANSI.Leijen
  ((<>), (<+>), indent, parens, pretty, text, tupled, vsep, Doc, Pretty(..))

import           GHC.Float (float2Double, double2Float)

import           Reopt.Semantics.Monad
  ( Type(..)
  , TypeRepr(..)
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import qualified Reopt.CFG.Representation as R
import qualified Reopt.Machine.StateNames as N
import qualified Reopt.Concrete.MachineState as CS
import           Reopt.Machine.Types ( FloatInfo(..), FloatInfoRepr, FloatType
                                     , floatInfoBits, n1, n80
                                     )

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


-- | Variables and corresponding instances
data Variable tp = Variable !(TypeRepr tp) !Name
type Name = String

instance TestEquality Variable where
  (Variable tp1 n1) `testEquality` (Variable tp2 n2) = do
    Refl <- testEquality tp1 tp2
    return Refl

instance MapF.OrdF Variable where
  (Variable tp1 n1) `compareF` (Variable tp2 n2) =
    case (tp1 `compareF` tp2, n1 `compare` n2) of
      (LTF,_) -> LTF
      (GTF,_) -> GTF
      (EQF,o) -> fromOrdering o

instance Ord (Variable tp) where
  (Variable _ n1) `compare` (Variable _ n2) =
    n1 `compare` n2

instance Eq (Variable tp) where
  (Variable _ n1) == (Variable _ n2) =
    n1 == n2

-- | A pure expression for isValue.
data Expr tp where
  -- An expression obtained from some value.
  LitExpr :: !(NatRepr n) -> Integer -> Expr (BVType n)
  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(R.App Expr tp) -> Expr tp

  -- Extra constructors where 'App' does not provide what we want.
  --
  -- Here 'App' has 'Trunc', but its type is different; see notes at
  -- bottom of file.
  TruncExpr :: (1 <= m, m <= n) =>
    !(NatRepr m) -> !(Expr (BVType n)) -> Expr (BVType m)
  -- Here 'app' has 'SExt', but its type is different as with 'Trunc'.
  -- But, strangely, the strict version of 'uext' is in the 'IsValue'
  -- class as 'uext'', so we can use 'App's 'UExt' there ... seems ad
  -- hoc.
  SExtExpr :: (1 <= m, m <= n) =>
    !(NatRepr n) -> !(Expr (BVType m)) -> Expr (BVType n)
  --
  -- A variable.
  -- Not doing anything fancy with names for now; can use 'unbound'
  -- later.
  VarExpr :: Variable tp -> Expr tp

instance TestEquality Expr where
  testEquality e1 e2 = testEquality (exprType e1) (exprType e2)

instance MapF.EqF Expr where
  LitExpr nr1 i1 `eqF` LitExpr nr2 i2 = i1 == i2
  AppExpr app1 `eqF` AppExpr app2 = app1 == app2
  TruncExpr nr1 e1 `eqF` TruncExpr nr2 e2 
    | Just Refl <- e1 `testEquality` e2 = e1 `MapF.eqF` e2
  SExtExpr nr1 e1 `eqF` SExtExpr nr2 e2
    | Just Refl <- e1 `testEquality` e2 = e1 `MapF.eqF` e2
  VarExpr v1 `eqF` VarExpr v2 = v1 == v2
  _ `eqF` _ = False

instance Eq (Expr tp) where
  e1 == e2 = e1 `R.eqF` e2

instance MapF.OrdF Expr where
  LitExpr nr1 i1 `compareF` LitExpr nr2 i2
    | Just Refl <- nr1 `testEquality` nr2 = 
      case i1 `compare` i2 of
        LT -> LTF
        EQ -> EQF
        GT -> GTF
    | otherwise = case nr1 `compareF` nr2 of
        LTF -> LTF
        EQF -> EQF
        GTF -> GTF
  LitExpr _ _ `compareF` _ = GTF
  _ `compareF` LitExpr _ _ = LTF
  AppExpr app1 `compareF` AppExpr app2 = app1 `compareF` app2
  AppExpr _ `compareF` _ = GTF
  _ `compareF` AppExpr _ = LTF
  TruncExpr nr1 e1 `compareF` TruncExpr nr2 e2 
    | Just Refl <- testEquality nr1 nr2 = case e1 `compareF` e2 of
        LTF -> LTF
        EQF -> EQF
        GTF -> GTF
    | otherwise = case nr1 `compareF` nr2 of
        LTF -> LTF
        EQF -> EQF
        GTF -> GTF
  TruncExpr _  _ `compareF` _ = GTF
  _ `compareF` TruncExpr _ _ = LTF
  SExtExpr nr1 e1 `compareF` SExtExpr nr2 e2 
    | Just Refl <- testEquality nr1 nr2 = case e1 `compareF` e2 of
        LTF -> LTF
        EQF -> EQF
        GTF -> GTF
    | LTF <- nr1 `compareF` nr2 = LTF
    | GTF <- nr1 `compareF` nr2 = GTF
    | EQF <- nr1 `compareF` nr2 = EQF
  SExtExpr _  _ `compareF` _ = GTF
  _ `compareF` SExtExpr _ _ = LTF
  VarExpr v1 `compareF` VarExpr v2 = v1 `compareF` v2

instance Ord (Expr tp) where
  e1 `compare` e2 = compareFin e1 e2

mkLit :: NatRepr n -> Integer -> Expr (BVType n)
mkLit n v = LitExpr n (v .&. mask)
  where mask = maxUnsigned n

app :: R.App Expr tp -> Expr tp
app = AppExpr

exprType :: Expr tp -> S.TypeRepr tp
exprType (LitExpr r _) = S.BVTypeRepr r
exprType (AppExpr a) = R.appType a
exprType (TruncExpr r _) = S.BVTypeRepr r
exprType (SExtExpr r _) = S.BVTypeRepr r
exprType (VarExpr (Variable r _)) = r -- S.BVTypeRepr r

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
  bvSplit :: forall n. (1 <= n)
          => Expr (BVType (n + n))
          -> (Expr (BVType n), Expr (BVType n))
  bvSplit v = withAddPrefixLeq hn hn ( app (R.UpperHalf hn v)
                                     , TruncExpr        hn v)
    where hn = halfNat (exprWidth v) :: NatRepr n
  bvShr x y = app $ R.BVShr (exprWidth x) x y
  bvSar x y = app $ R.BVSar (exprWidth x) x y
  bvShl x y = app $ R.BVShl (exprWidth x) x y
  bvTrunc w x = TruncExpr w x
  bvUlt x y = app $ R.BVUnsignedLt x y
  bvSlt x y = app $ R.BVSignedLt x y
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

------------------------------------------------------------------------
-- Statements.

type MLocation = S.Location (Expr (BVType 64))

data NamedStmt where

-- | Potentially side-effecting operations, corresponding the to the
-- 'S.Semantics' class.
data Stmt where
  MakeUndefined :: Variable tp -> TypeRepr tp -> Stmt
  Get :: Variable tp -> MLocation tp -> Stmt
  -- The remaining constructors correspond to the 'S.Semantics'
  -- operations; the arguments are documented there and in
  -- 'Reopt.CFG.Representation.Stmt'.
  (:=) :: MLocation tp -> Expr tp -> Stmt
  Ifte_ :: Expr BoolType -> [Stmt] -> [Stmt] -> Stmt
  MemCopy :: Integer
          -> Expr (BVType 64)
          -> Expr (BVType 64)
          -> Expr (BVType 64)
          -> Expr BoolType
          -> Stmt
  MemCmp :: Variable (BVType 64)
         -> Integer
         -> Expr (BVType 64)
         -> Expr (BVType 64)
         -> Expr (BVType 64)
         -> Expr BoolType
         -> Stmt
  MemSet :: Expr (BVType 64) -> Expr (BVType n) -> Expr (BVType 64) -> Stmt
  Primitive :: S.Primitive -> Stmt
  GetSegmentBase :: Variable (BVType 64) -> S.Segment -> Stmt
  BVDiv :: (1 <= n)
        => (Variable (BVType n), Variable (BVType n))
        -> Expr (BVType (n+n))
        -> Expr (BVType n)
        -> Stmt
  BVSignedDiv :: (1 <= n)
              => (Variable (BVType n), Variable (BVType n))
              -> Expr (BVType (n+n))
              -> Expr (BVType n)
              -> Stmt
  Exception :: Expr BoolType
            -> Expr BoolType
            -> S.ExceptionClass
            -> Stmt
  X87Push :: Expr (S.FloatType X86_80Float) -> Stmt
  X87Pop  :: Stmt

instance Eq Stmt where
  MakeUndefined v1 tp1 == MakeUndefined v2 tp2 
    | Just Refl <- testEquality v1 v2 = v1 == v2
  Get v1 l1 == Get v2 l2
    | Just Refl <- testEquality v1 v2 = v1 == v2 && l1 == l2 
  (l1 := e1) == (l2 := e2) 
    | Just Refl <- testEquality e1 e2 = l1 == l2 && e1 == e2
  Ifte_ e1 s1 s1' == Ifte_ e2 s2 s2' = e1 == e2 && s1 == s2 && s1' == s2'
  MemCopy i e1 e2 e3 e4 == MemCopy i' e1' e2' e3' e4' =
    i == i' && e1 == e1' && e2 == e2' && e3 == e3' && e4 == e4'
  MemCmp v i e1 e2 e3 e4 == MemCmp v' i' e1' e2' e3' e4' 
    | Just Refl <- testEquality v v' = v == v' && i == i' && e1 == e1' &&
                                       e2 == e2' &&  e3 == e3' && e4 == e4'
  MemSet e1 e2 e3 == MemSet e1' e2' e3' 
    | Just Refl <- testEquality e2 e2' = e1 == e1' && e2 == e2' && e3 == e3'
  Primitive p1 == Primitive p2 = p1 == p2
  GetSegmentBase v s == GetSegmentBase v' s' = v == v' && s == s'
  BVDiv (v1, v2) e1 e2 == BVDiv (v1', v2') e1' e2'
    | Just Refl <- testEquality e2 e2' = v1 == v1' && v2 ==v2' && e1 == e1' && e2 == e2' 
  BVSignedDiv (v1, v2) e1 e2 == BVSignedDiv (v1', v2') e1' e2' 
    | Just Refl <- testEquality e2 e2' = v1 == v1' && v2 ==v2' && e1 == e1' &&
                                         e2 == e2'
  Exception e1 e2 ec == Exception e1' e2' ec' = e1 == e1' && e2 == e2' && ec == ec'
  X87Push e == X87Push e' = e == e'
  X87Pop == X87Pop = True
  _ == _ = False

compareF' :: MapF.OrdF f => forall (a :: Type) . forall (b :: Type). f a -> f b -> Ordering -> Ordering
compareF' a b def = case compareF a b of
  GTF -> GT
  EQF -> def
  LTF -> LT

compareFin :: MapF.OrdF f => forall (a :: Type) . forall (b :: Type). f a -> f b -> Ordering
compareFin a b = compareF' a b EQ

compare' :: Ord o => o -> o -> Ordering -> Ordering
compare' a b def = case compare a b of
  GT -> GT
  EQ -> def
  LT -> LT

instance Ord Stmt where  
  MakeUndefined v1 tp1 `compare` MakeUndefined v2 tp2 = compareFin v1 v2
  MakeUndefined _ _ `compare` _ = GT
  _ `compare` MakeUndefined _ _ = LT
  Get v1 l1 `compare` Get v2 l2 = compareF' v1 v2 $ compareFin l1 l2
  Get _ _ `compare` _ = GT
  _ `compare` Get _ _ = LT
  (l1 := e1) `compare` (l2 := e2) = compareF' l1 l2 $ compareFin e1 e2
  (_ := _) `compare` _ = GT
  _ `compare` (_ := _) = LT
  Ifte_ e1 s1 s1' `compare` Ifte_ e2 s2 s2' =
    compareF' e1 e2 $ compare' s1 s2 $ compare s1' s2'
  Ifte_ _ _ _ `compare` _ = GT
  _ `compare` Ifte_ _ _ _ = LT
  MemCopy i e1 e2 e3 e4 `compare` MemCopy i' e1' e2' e3' e4' =
    compare' i i' $ compareF' e1 e1' $ compareF' e2 e2' $ compareF' e3 e3' $
      compareFin e4 e4'
  MemCopy{} `compare` _ = GT
  _ `compare` MemCopy{} = LT
  MemCmp v i e1 e2 e3 e4 `compare` MemCmp v' i' e1' e2' e3' e4' =
    compareF' v v' $ compare' i i' $ compareF' e1 e1' $ compareF' e2 e2' $
      compareF' e3 e3' $ compareFin e4 e4'
  MemCmp{} `compare` _ = GT
  _ `compare` MemCmp{} = LT
  MemSet e1 e2 e3 `compare` MemSet e1' e2' e3' =
    compareF' e1 e1' $ compareF' e2 e2' $ compareFin e3 e3'
  MemSet{} `compare` _ = GT
  _ `compare` MemSet{} = LT
  Primitive p1 `compare` Primitive p2 = p1 `compare` p2
  Primitive _ `compare` _ = GT
  _ `compare` Primitive _ = LT
  GetSegmentBase v s `compare` GetSegmentBase v' s' = 
    compare' v v' $ compare s s'
  GetSegmentBase{} `compare` _ = GT
  _ `compare` GetSegmentBase{} = LT
  BVDiv (v1, v2) e1 e2 `compare` BVDiv (v1', v2') e1' e2' =
    compareF' v1 v1' $ compareF' v2 v2' $ compareF' e1 e1' $ compareFin e2 e2'
  BVDiv{} `compare` _ = GT
  _ `compare` BVDiv{} = LT
  BVSignedDiv (v1, v2) e1 e2 `compare` BVSignedDiv (v1', v2') e1' e2' =
    compareF' v1 v1' $ compareF' v2 v2' $ compareF' e1 e1' $ compareFin e2 e2'
  BVSignedDiv{} `compare` _ = GT
  _ `compare` BVSignedDiv{} = LT
  Exception e1 e2 ec `compare` Exception e1' e2' ec' = 
    compareF' e1 e1' $ compareF' e2 e2' $ compare ec ec'
  Exception{} `compare` _ = GT
  _ `compare` Exception{} = LT
  X87Push e `compare` X87Push e' = compareFin e e'
  X87Push{} `compare` _ = GT
  _ `compare` X87Push{} = LT
  X87Pop `compare` X87Pop = EQ


------------------------------------------------------------------------
-- Semantics monad instance.

-- | An 'S.Semantics' monad.
--
-- We collect effects in a 'Writer' and use 'State' to generate fresh
-- names.
newtype Semantics a =
  Semantics { runSemantics :: WriterT [Stmt] (State Integer) a }
  deriving (Functor, Monad, MonadState Integer, MonadWriter [Stmt])

-- | Execute a 'Semantics' computation, returning its effects.
execSemantics :: Semantics a -> [Stmt]
execSemantics = flip evalState 0 . execWriterT . runSemantics

type instance S.Value Semantics = Expr

#if !MIN_VERSION_base(4,8,0)
instance Applicative Semantics where
  pure = return
  (<*>) = ap
#endif

-- | Generate a fresh variable with basename 'basename'.
fresh :: MonadState Integer m => String -> m String
fresh basename = do
  x <- get
  put (x + 1)
  return $ basename ++ show x


-- FIXME: Move
addIsLeqLeft1' :: forall f g n m. LeqProof (n + n) m ->
                  f (BVType n) -> g m
                  -> LeqProof n m
addIsLeqLeft1' prf _v _v' = addIsLeqLeft1 prf

-- | Interpret 'S.Semantics' operations into 'Stmt's.
--
-- Operations that return 'Value's return fresh variables; we track
-- the relation between variables and the 'Stmt's they bind to using
-- 'NamedStmt's.
instance S.Semantics Semantics where
  make_undefined t = do
    name <- fresh "undef"
    let var = Variable t name
    tell [MakeUndefined var t]
    return $ VarExpr var

  get l = do
    name <- fresh "get"
    let var = Variable (S.loc_type l) name
    tell [Get var l]
    return $ VarExpr var

  -- sjw: This is a huge hack, but then again, so is the fact that it
  -- happens at all.  According to the ISA, assigning a 32 bit value
  -- to a 64 bit register performs a zero extension so the upper 32
  -- bits are zero.  This may not be the best place for this, but I
  -- can't think of a nicer one ...
  --
  -- TODO(conathan): verify that this is sufficient. E.g., what is
  -- supposed to happen for @UpperHalf (LowerHalf (Register _))@? That
  -- won't get special treatment here, but maybe it also needs the
  -- upper 32 bits to be zeroed?
  (S.LowerHalf loc@(S.Register (N.GPReg _))) .= v =
    -- FIXME: doing this the obvious way breaks GHC
    --     case addIsLeqLeft1' LeqProof v S.n64 of ...
    --
    -- ghc: panic! (the 'impossible' happened)
    --     (GHC version 7.8.4 for x86_64-apple-darwin):
    --   	tcIfaceCoAxiomRule Sub0R
    --
    case testLeq (S.bv_width v) S.n64 of
     Just LeqProof -> tell [loc := S.uext knownNat v]
     Nothing -> error "impossible"

  l .= v = tell [l := v]

  ifte_ c trueBranch falseBranch = do
    trueStmts <- collectAndForget trueBranch
    falseStmts <- collectAndForget falseBranch
    tell [Ifte_ c trueStmts falseStmts]
    where
      -- | Run a subcomputation and collect and return the writes.
      --
      -- In the enclosing computation, the state changes persist and
      -- the writes are forgotten.
      collectAndForget :: MonadWriter w m => m a -> m w
      collectAndForget = liftM snd . censor (const mempty) . listen
      -- More obvious / less abstract version:
      {-
      collectAndForget :: Semantics a -> Semantics [Stmt]
      collectAndForget = Semantics . lift . execWriterT . runSemantics
      -}

  memcopy i v1 v2 v3 b = tell [MemCopy i v1 v2 v3 b]

  memset v1 v2 v3 = tell [MemSet v1 v2 v3]

  memcmp r v1 v2 v3 v4 = do
    name <- fresh "memcmp"
    let var = Variable S.knownType name
    tell [MemCmp var r v1 v2 v3 v4]
    return $ VarExpr var

  primitive p = tell [Primitive p]

  getSegmentBase seg = do
    name <- fresh $ show seg ++ "_base"
    let var = Variable S.knownType name
    tell [GetSegmentBase var seg]
    return $ VarExpr var

  bvDiv v1 v2 = do
    nameQuot <- fresh "divQuot"
    nameRem <- fresh "divRem"
    let varQuot = Variable r nameQuot
    let varRem = Variable r nameRem
    tell [BVDiv (varQuot, varRem) v1 v2]
    return (VarExpr varQuot, VarExpr varRem)
    where
      r = exprType v2

  bvSignedDiv v1 v2 = do
    nameQuot <- fresh "sdivQuot"
    nameRem <- fresh "sdivRem"
    let varQuot = Variable r nameQuot
    let varRem = Variable r nameRem
    tell [BVSignedDiv (varQuot, varRem) v1 v2]
    return (VarExpr varQuot, VarExpr varRem)
    where
      r = exprType v2

  exception v1 v2 c = tell [Exception v1 v2 c]

  x87Push v = tell [X87Push v]

  x87Pop = tell [X87Pop]

------------------------------------------------------------------------
-- Pretty printing.

ppExpr :: Expr a -> Doc
ppExpr e = case e of
  LitExpr n i -> parens $ R.ppLit n i
  AppExpr app -> R.ppApp ppExpr app
  TruncExpr n e -> R.sexpr "trunc" [ ppExpr e, R.ppNat n ]
  SExtExpr n e -> R.sexpr "sext" [ ppExpr e, R.ppNat n ]
  VarExpr (Variable _ x) -> text x

-- | Pretty print 'S.Location'.
--
-- Going back to pretty names for subregisters is pretty ad hoc;
-- see table at http://stackoverflow.com/a/1753627/470844. E.g.,
-- instead of @%ah@, we produce @(upper_half (lower_half (lower_half %rax)))@.
ppLocation :: forall addr tp. (addr -> Doc) -> S.Location addr tp -> Doc
ppLocation ppAddr l = S.elimLocation ppMemCont ppRegCont ppX87Cont l
  where
    ppMemCont :: forall tp'.
                 (Integer, Integer) -> Integer -> (addr, TypeRepr tp') -> Doc
    ppMemCont = ppSubrange (ppAddr . fst)
    ppRegCont :: (Integer, Integer) -> Integer -> N.RegisterName cl -> Doc
    ppRegCont = ppSubrange (\r -> text $ "%" ++ show r)
    ppX87Cont = ppSubrange (\i -> text $ "x87_stack@" ++ show i)
    -- | Print subrange as Python-style slice @<location>[<low>:<high>]@.
    --
    -- The low bit is inclusive and the high bit is exclusive, but I
    -- can't bring myself to generate @<reg>[<low>:<high>)@ :)
    ppSubrange pp (low, high) width x =
      if width == high
      then pp x
      else pp x <> text ("[" ++ show low ++ ":" ++ show high ++ "]")

ppMLocation :: MLocation tp -> Doc
ppMLocation = ppLocation ppExpr

ppStmts :: [Stmt] -> Doc
ppStmts = vsep . map ppStmt

ppStmt :: Stmt -> Doc
ppStmt s = case s of
  -- Named.
  MakeUndefined (Variable _ x) _ -> ppNamedStmt [x] $ text "make_undefined"
  Get (Variable _ x) l -> ppNamedStmt [x] $ R.sexpr "get" [ ppMLocation l ]
  BVDiv (Variable _ x, Variable _ y) v1 v2 ->
    ppNamedStmt [x,y] $ R.sexpr "bv_div" [ ppExpr v1, ppExpr v2 ]
  BVSignedDiv (Variable _ x, Variable _ y) v1 v2 ->
    ppNamedStmt [x,y] $ R.sexpr "bv_signed_div" [ ppExpr v1, ppExpr v2 ]
  MemCmp (Variable _ x) n v1 v2 v3 v4 ->
    ppNamedStmt [x] $
      R.sexpr "memcmp" [ pretty n, ppExpr v1, ppExpr v2, ppExpr v3, ppExpr v4 ]
  GetSegmentBase (Variable _ x) seg ->
    ppNamedStmt [x] $ R.sexpr "get_segment_base" [ text $ show seg ]

  -- Unamed.
  l := e -> ppMLocation l <+> text ":=" <+> ppExpr e
  Ifte_ v t f -> vsep
    [ text "if" <+> ppExpr v
    , text "then"
    ,   indent 2 (ppStmts t)
    , text "else"
    ,   indent 2 (ppStmts f)
    ]
  MemCopy i v1 v2 v3 b -> R.sexpr "memcopy" [ pretty i, ppExpr v1, ppExpr v2, ppExpr v3, ppExpr b ]
  MemSet v1 v2 v3 -> R.sexpr "memset" [ ppExpr v1, ppExpr v2, ppExpr v3 ]
  Primitive p -> pretty p
  Exception v1 v2 e -> R.sexpr "exception" [ ppExpr v1, ppExpr v2, text $ show e ]
  X87Push v -> R.sexpr "x87_push" [ ppExpr v ]
  X87Pop -> text "x87_pop"
  where
    ppNamedStmt :: [Name] -> Doc -> Doc
    ppNamedStmt [] _ = error "ppNamedStmt: bug! Named stmts have names!"
    ppNamedStmt [x] d =
      text x <+> text "<-" <+> d
    ppNamedStmt names d =
      tupled (map text names) <+> text "<-" <+> d

instance Pretty Stmt where
  pretty = ppStmt
