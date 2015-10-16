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
       , ppExpr
       , Stmt(..)
       , Expr(..)
       , Variable(..)
       ) where

import           Control.Monad.Cont
import           Control.Monad.State.Strict
import           Control.Monad.Writer
  (censor, execWriterT, listen, tell, MonadWriter, WriterT)
import           Data.Bits

import           Data.Parameterized.Classes (OrderingF(..), compareF, fromOrdering)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Text.PrettyPrint.ANSI.Leijen
  ((<+>), indent, parens, pretty, text, tupled, vsep, Doc, Pretty(..))

import           Reopt.Concrete.MachineState (Value)
import qualified Reopt.Concrete.MachineState as CS
import           Reopt.Semantics.Monad
  ( Type(..)
  , TypeRepr(..)
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import qualified Reopt.CFG.Representation as R
import           Reopt.Machine.Types (FloatInfo(..))

-- import Debug.Trace

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
    guard (n1 == n2)
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
  -- A embedded value.
  ValueExpr :: !(Value tp) -> Expr tp
  -- An expression obtained from some value.
  LitExpr :: (1 <= n) => !(NatRepr n) -> Integer -> Expr (BVType n)

  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(R.App Expr tp) -> Expr tp
  --
  -- A variable.
  -- Not doing anything fancy with names for now; can use 'unbound'
  -- later.
  VarExpr :: Variable tp -> Expr tp

instance TestEquality Expr where
  testEquality e1 e2 = testEquality (exprType e1) (exprType e2)

instance MapF.EqF Expr where
  LitExpr _nr1 i1 `eqF` LitExpr _nr2 i2 = i1 == i2
  AppExpr app1 `eqF` AppExpr app2 = app1 == app2
  VarExpr v1 `eqF` VarExpr v2 = v1 == v2
  ValueExpr v1 `eqF` ValueExpr v2 = v1 == v2
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
  VarExpr v1 `compareF` VarExpr v2 = v1 `compareF` v2
  _ `compareF` VarExpr _ = LTF
  ValueExpr v1 `compareF` ValueExpr v2 = v1 `compareF` v2  
  _ `compareF` ValueExpr _ = LTF
  
instance Ord (Expr tp) where
  e1 `compare` e2 = compareFin e1 e2

mkLit :: (1 <= n) => NatRepr n -> Integer -> Expr (BVType n)
mkLit n v = LitExpr n (v .&. mask)
  where mask = maxUnsigned n

app :: R.App Expr tp -> Expr tp
app = AppExpr

exprType :: Expr tp -> S.TypeRepr tp
exprType (ValueExpr v) = CS.asTypeRepr v
exprType (LitExpr r _) = S.BVTypeRepr r
exprType (AppExpr a) = R.appType a
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
  bvSplit v = withLeqProof (leqAdd2 (leqRefl hn) (LeqProof :: LeqProof 1 n)) 
                            ( app (R.UpperHalf hn v)
                            , app (R.Trunc     v hn))
    where hn = halfNat (exprWidth v) :: NatRepr n
  bvShr x y = app $ R.BVShr (exprWidth x) x y
  bvSar x y = app $ R.BVSar (exprWidth x) x y
  bvShl x y = app $ R.BVShl (exprWidth x) x y
  bvTrunc' w x = app $ R.Trunc x w
  bvUlt x y = app $ R.BVUnsignedLt x y
  bvSlt x y = app $ R.BVSignedLt x y
  bvBit x y = app $ R.BVTestBit x y
  sext' w x = app $ R.SExt x w
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

------------------------------------------------------------------------
-- Statements.

type MLocation = S.Location (Expr (BVType 64))

-- | Potentially side-effecting operations, corresponding the to the
-- 'S.Semantics' class.
data Stmt where
  MakeUndefined :: Variable tp -> TypeRepr tp -> Stmt
  Get :: Variable tp -> MLocation tp -> Stmt
  Let :: Variable tp -> Expr tp -> Stmt
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
  BVQuot, BVRem, BVSignedQuot, BVSignedRem ::
       (1 <= n)
    => Variable (BVType n)
    -> Expr (BVType n)
    -> Expr (BVType n)
    -> Stmt
  Exception :: Expr BoolType
            -> Expr BoolType
            -> S.ExceptionClass
            -> Stmt
  X87Push :: Expr (S.FloatType X86_80Float) -> Stmt
  X87Pop  :: Stmt

instance Eq Stmt where
  MakeUndefined v1 _tp1 == MakeUndefined v2 _tp2 
    | Just Refl <- testEquality v1 v2 = v1 == v2
  Get v1 l1 == Get v2 l2
    | Just Refl <- testEquality v1 v2 = l1 == l2
  Let v1 e1 == Let v2 e2
    | Just Refl <- testEquality v1 v2 = e1 == e2
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
  BVQuot v1 e1 e2 == BVQuot v1' e1' e2'
    | Just Refl <- testEquality e2 e2' = v1 == v1' && e1 == e1' && e2 == e2' 
  BVRem v1 e1 e2 == BVRem v1' e1' e2'
    | Just Refl <- testEquality e2 e2' = v1 == v1' && e1 == e1' && e2 == e2' 
  BVSignedQuot v1 e1 e2 == BVSignedQuot v1' e1' e2' 
    | Just Refl <- testEquality e2 e2' = v1 == v1' && e1 == e1' &&
                                         e2 == e2'
  BVSignedRem v1 e1 e2 == BVSignedRem v1' e1' e2' 
    | Just Refl <- testEquality e2 e2' = v1 == v1' && e1 == e1' &&
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
  MakeUndefined v1 _tp1 `compare` MakeUndefined v2 _tp2 = compareFin v1 v2
  MakeUndefined _ _ `compare` _ = GT
  _ `compare` MakeUndefined _ _ = LT
  Get v1 l1 `compare` Get v2 l2 = compareF' v1 v2 $ compareFin l1 l2
  Get _ _ `compare` _ = GT
  _ `compare` Get _ _ = LT

  Let v1 e1 `compare` Let v2 e2 = compareF' v1 v2 $ compareFin e1 e2
  Let _ _ `compare` _ = GT
  _ `compare` Let _ _ = LT

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
  BVQuot v1 e1 e2 `compare` BVQuot v1' e1' e2' =
    compareF' v1 v1' $ compareF' e1 e1' $ compareFin e2 e2'
  BVQuot{} `compare` _ = GT
  _ `compare` BVQuot{} = LT
  BVRem v1 e1 e2 `compare` BVRem v1' e1' e2' =
    compareF' v1 v1' $ compareF' e1 e1' $ compareFin e2 e2'
  BVRem{} `compare` _ = GT
  _ `compare` BVRem{} = LT
  BVSignedQuot v1 e1 e2 `compare` BVSignedQuot v1' e1' e2' =
    compareF' v1 v1' $ compareF' e1 e1' $ compareFin e2 e2'
  BVSignedQuot{} `compare` _ = GT
  _ `compare` BVSignedQuot{} = LT
  BVSignedRem v1 e1 e2 `compare` BVSignedRem v1' e1' e2' =
    compareF' v1 v1' $ compareF' e1 e1' $ compareFin e2 e2'
  BVSignedRem{} `compare` _ = GT
  _ `compare` BVSignedRem{} = LT
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
  deriving (Functor, Applicative, Monad, MonadState Integer, MonadWriter [Stmt])

-- | Execute a 'Semantics' computation, returning its effects.
execSemantics :: Semantics a -> [Stmt]
execSemantics = flip evalState 0 . execWriterT . runSemantics

type instance S.Value Semantics = Expr

-- | Generate a fresh variable with basename 'basename'.
fresh :: MonadState Integer m => String -> m String
fresh basename = do
  x <- get
  put (x + 1)
  return $ basename ++ show x

-- | Helper for 'S.Semantics' instance below.
bvBinOp ::
     (Variable ('BVType n) -> Expr ('BVType n) -> Expr ('BVType n) -> Stmt)
  -> String
  -> Expr ('BVType n)
  -> Expr ('BVType n)
  -> Semantics (Expr ('BVType n))
bvBinOp op varName v1 v2 = do
  varName' <- fresh varName
  let var = Variable (exprType v2) varName'
  tell [op var v1 v2]
  return (VarExpr var)


freshVar :: String -> TypeRepr tp -> Semantics (Variable tp)
freshVar name tp = Variable tp <$> fresh name

-- | Interpret 'S.Semantics' operations into 'Stmt's.
--
-- Operations that return 'Value's return fresh variables; we track
-- the relation between variables and the 'Stmt's they bind to using
-- 'NamedStmt's.
instance S.Semantics Semantics where
  make_undefined t = do
    var <- freshVar "undef" t
    tell [MakeUndefined var t]
    return $ VarExpr var

  get l = do
    var <- freshVar "get" (S.loc_type l)
    expandMemOps (Get var l)
    -- tell [Get var l]
    return $ VarExpr var

  l .= v = expandMemOps (l := v)

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
    var <- freshVar "memcmp" S.knownType
    tell [MemCmp var r v1 v2 v3 v4]
    return $ VarExpr var

  primitive p = tell [Primitive p]

  getSegmentBase seg = do
    var <- freshVar (show seg ++ "_base") S.knownType
    tell [GetSegmentBase var seg]
    return $ VarExpr var

  bvQuot = bvBinOp BVQuot "quot"
  bvRem = bvBinOp BVRem "rem"
  bvSignedQuot = bvBinOp BVSignedQuot "sQuot"
  bvSignedRem = bvBinOp BVSignedRem "sRem"

  exception v1 v2 c = tell [Exception v1 v2 c]

  x87Push v = tell [X87Push v]

  x87Pop = tell [X87Pop]

------------------------------------------------------------------------
-- Expanding multi-byte memory operations to multiple single byte
-- operations

-- This doesn't exactly interleave, although it could ...

{-
wrapSemantics :: Semantics a -> (Stmt -> Semantics ()) -> Semantics a
wrapSemantics m f = do
  (v, r) <- censor (const mempty) . listen $ m
  mapM_ f r
  return v
-}

data P n where
  ZeroCase    :: P 0
  NonZeroCase :: (1 <= n) => Semantics (Expr (BVType (n * 8))) -> P n

offsetAddr :: Integer -> Expr (BVType 64) -> Expr (BVType 64)
offsetAddr bits addr
  | bits `mod` 8 == 0 = AppExpr (R.BVAdd knownNat addr
                                 (LitExpr knownNat $ bits `div` 8))
  | otherwise = error "Addr bits isn't a multiple of 8"

-- This needs to be here for the fresh name generation ...
expandRead :: Variable tp -> Expr (BVType 64) -> TypeRepr tp
           -> Semantics ()
expandRead v addr (BVTypeRepr nr) =
  withDivModNat nr S.n8 $ \divn modn ->
    case testEquality modn (knownNat :: NatRepr 0) of
      Nothing   -> error "Addr width not a multiple of 8"
      Just Refl -> 
        case natRec divn ZeroCase go of
          ZeroCase      -> error "Zero case"
          NonZeroCase m -> do e <- m
                              tell [Let v e]
  where
    getOne :: Integer -> Semantics (Expr (BVType 8))
    getOne n = do
      v' <- freshVar "expandRead" S.knownType
      let addr' = offsetAddr (n * 8) addr
      tell [Get v' (S.MemoryAddr addr' S.knownType)]
      return $ VarExpr v'

    go :: forall n'. NatRepr n' -> P n' -> P (n' + 1)
    go nr' ZeroCase  = NonZeroCase $ getOne (natValue nr')
    go nr' (NonZeroCase m) =
        withAddPrefixLeq nr' S.n1 $
        withAddMulDistribRight nr' S.n1 S.n8 $
        NonZeroCase $ do
          mv <- m
          v'  <- getOne (natValue nr')
          let sz = natMultiply nr' S.n8
              e  = AppExpr (R.ConcatV sz S.n8 mv v')
          res_v <- freshVar "expandRead" (exprType e)
          tell [Let res_v e]
          return $ VarExpr res_v

expandWrite :: forall n. (9 <= n) =>
               Expr (BVType 64) -> TypeRepr (BVType n) -> Expr (BVType n)
            -> Semantics ()
expandWrite addr (BVTypeRepr nr) v =
  withDivModNat nr S.n8 $ \divn modn ->
    case ( testEquality modn (knownNat :: NatRepr 0)
         , isZeroNat divn)  of
      (Nothing, _) -> error "Addr width not a multiple of 8"
      (_, ZeroNat) -> error "Addr width is zero" -- (shoudn't happen)
      (Just Refl, NonZeroNat) ->
        case leqTrans (LeqProof :: LeqProof 1 9) (LeqProof :: LeqProof 9 n) of
          (LeqProof :: LeqProof 1 n) -> sequence_ (go divn)
  where
    go :: (1 <= n) => NatRepr (divn_pred + 1) -> [Semantics ()]
    go divn = natForEach (knownNat :: NatRepr 0) (predNat divn) $ \n ->
           let addr'   = offsetAddr (natValue n * 8) addr
               v_shift = AppExpr (R.BVShr sz v (LitExpr sz (natValue n * 8)))
               v'      = AppExpr (R.Trunc v_shift S.n8)
           in
              tell [ S.MemoryAddr addr' S.knownType := v' ]
    sz = exprWidth v

expandMemOps :: Stmt -> Semantics ()
expandMemOps stmt@(Get v l)
  | S.BVTypeRepr nr <- (S.loc_type l)
  , Just LeqProof <- testLeq (knownNat :: NatRepr 9) nr =
      case l of
        S.MemoryAddr addr _tp ->  do
          addrv <- freshVar "addr" S.knownType
          tell [Let addrv addr]
          expandRead v (VarExpr addrv) (S.loc_type l)
        _ -> tell [stmt]

expandMemOps stmt@(l := e) =
  case l of
    S.MemoryAddr addr _tp 
      | S.BVTypeRepr nr <- S.loc_type l
      , Just LeqProof <- testLeq (knownNat :: NatRepr 9) nr -> do
        addrv <- freshVar "addr" S.knownType
        valv  <- freshVar "val" (S.BVTypeRepr nr)
        tell [ Let addrv addr
             , Let valv e ]
        expandWrite (VarExpr addrv) (BVTypeRepr nr) (VarExpr valv)
    _ -> tell [stmt]

expandMemOps stmt = tell [stmt]      

------------------------------------------------------------------------
-- Pretty printing.

ppExpr :: Expr a -> Doc
ppExpr e = case e of
  ValueExpr v -> pretty v
  LitExpr n i -> parens $ R.ppLit n i
  AppExpr app' -> R.ppApp ppExpr app'
  VarExpr (Variable _ x) -> text x

ppMLocation :: MLocation tp -> Doc
ppMLocation = S.ppLocation ppExpr

ppStmts :: [Stmt] -> Doc
ppStmts = vsep . map ppStmt

ppStmt :: Stmt -> Doc
ppStmt s = case s of
  -- Named.
  MakeUndefined (Variable _ x) _ -> ppNamedStmt [x] $ text "make_undefined"
  Get (Variable _ x) l -> ppNamedStmt [x] $ R.sexpr "get" [ ppMLocation l ]
  Let (Variable _ x) e -> ppNamedStmt [x] $ ppExpr e
  BVQuot x v1 v2 -> ppBinOp "bv_quot" x v1 v2
  BVRem x v1 v2 -> ppBinOp "bv_rem" x v1 v2
  BVSignedQuot x v1 v2 -> ppBinOp "bv_signed_quot" x v1 v2
  BVSignedRem x v1 v2 -> ppBinOp "bv_signed_rem" x v1 v2
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
    ppBinOp :: String
      -> Variable ('BVType n)
      -> Expr ('BVType n)
      -> Expr ('BVType n)
      -> Doc
    ppBinOp op (Variable _ x) v1 v2 =
      ppNamedStmt [x] $ R.sexpr op [ ppExpr v1, ppExpr v2 ]

instance Pretty Stmt where
  pretty = ppStmt
