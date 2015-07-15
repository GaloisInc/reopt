------------------------------------------------------------------------
-- |
-- Module           : Reopt.Concrete.Semantics
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Reopt.Concrete.Semantics
       ( execSemantics
       , evalStmt
       , ppStmts
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

------------------------------------------------------------------------
-- Expression evaluation

type Env = MapF Variable CS.Value

-- `c` in this context means `concrete value`.
evalExpr :: (MonadReader Env m, Applicative m) => Expr tp -> m (CS.Value tp)
evalExpr (LitExpr nr i) = return $ CS.Literal bVec
  where
    bVec = CS.bitVector nr (BV.bitVec bitWidth i)
    bitWidth = fromInteger (natValue nr)

-- TruncExpr and SExtExpr differ from App's Trunc & SExt only in the types.
-- These allow for trivial truncations & extensions, where App does not.
evalExpr (TruncExpr nr e) =
  return . CS.liftValue (truncBV nr) nr =<< evalExpr e

evalExpr (SExtExpr nr e) =
  return . CS.liftValue (sExtBV nr) nr =<< evalExpr e

evalExpr (VarExpr var@(Variable _ name)) = do
  maybeVal <- asks (MapF.lookup var)
  let msg = "Bug: unbound variable " ++ name ++ " in expr"
  maybe (error msg) return maybeVal

evalExpr (AppExpr a) = do
  a' <- R.traverseApp evalExpr a
  return $ case a' of
    -- Mux is if-then-else
    R.Mux nr c1 c2 c3 -> CS.liftValue3 doMux nr c1 c2 c3

    -- Resize ops
    R.MMXExtend c -> let ones = BV.ones 16
                      in CS.liftValue (BV.# ones) extPrecisionNatRepr c
    R.ConcatV nr c1 c2 -> CS.liftValue2 (BV.#) (addNat nr nr) c1 c2
    R.UpperHalf nr c -> CS.liftValue (upperBV nr) nr c
    R.Trunc c nr -> CS.liftValue (truncBV nr) nr c
    R.SExt c nr -> CS.liftValue (sExtBV nr) nr c
    R.UExt c nr -> CS.liftValue (uExtBV nr) nr c

    -- Boolean ops
    R.AndApp c1 c2 -> CS.liftValue2 (.&.) boolNatRepr c1 c2
    R.OrApp c1 c2 -> CS.liftValue2 (.|.) boolNatRepr c1 c2
    R.NotApp c -> CS.liftValue (complement) boolNatRepr c

    -- Arithmetic ops
    R.BVAdd nr c1 c2 -> CS.liftValue2 (+) nr c1 c2
    R.BVSub nr c1 c2 -> CS.liftValue2 (-) nr c1 c2
    R.BVMul nr c1 c2 -> CS.liftValue2 (*) nr c1 c2
    R.BVDiv nr c1 c2 -> error "Impossible: BVDiv should be unreachable"
    R.BVMod nr c1 c2 -> error "Impossible: BVMod should be unreachable"
    R.BVSignedDiv nr c1 c2 -> error "Impossible: BVSignedDiv should be unreachable"
    R.BVSignedMod nr c1 c2 -> error "Impossible: BVSignedMod should be unreachable"

    -- Comparisons
    R.BVUnsignedLt c1 c2 -> CS.liftValue2 (predBV (BV.<.)) boolNatRepr c1 c2
    R.BVSignedLt c1 c2 -> CS.liftValue2 (predBV (BV.slt)) boolNatRepr c1 c2

    R.BVBit c1 c2 -> CS.liftValue2 bitIdx boolNatRepr c1 c2

    -- Bit vector ops
    R.BVComplement nr c -> CS.liftValue (complement) nr c
    R.BVAnd nr c1 c2 -> CS.liftValue2 (.&.) nr c1 c2
    R.BVOr nr c1 c2 -> CS.liftValue2 (.|.) nr c1 c2
    R.BVXor nr c1 c2 -> CS.liftValue2 (xor) nr c1 c2
    R.BVShl nr c1 c2 -> CS.liftValue2 (BV.shl) nr c1 c2
    R.BVShr nr c1 c2 -> CS.liftValue2 (BV.shr) nr c1 c2
    R.BVSar nr c1 c2 -> CS.liftValue2 (BV.ashr) nr c1 c2
    R.BVEq c1 c2 -> CS.liftValue2 (predBV (BV.==.)) boolNatRepr c1 c2

    R.EvenParity c -> CS.liftValue isEvenParity boolNatRepr c
    R.ReverseBytes nr c -> CS.liftValue BV.reverse nr c
    R.UadcOverflows _nr c1 c2 carryBit ->
      CS.liftValue3 checkUadcOverflow boolNatRepr c1 c2 carryBit
    R.SadcOverflows _nr c1 c2 carryBit ->
      CS.liftValue3 checkSadcOverflow boolNatRepr c1 c2 carryBit
    R.UsbbOverflows _nr c1 c2 borrowBit ->
      CS.liftValue3 checkUsbbOverflow boolNatRepr c1 c2 borrowBit
    R.SsbbOverflows _nr c1 c2 borrowBit ->
      CS.liftValue3 checkSsbbOverflow boolNatRepr c1 c2 borrowBit

    R.Bsf nr c -> CS.liftValueMaybe (bsf nr) nr c
    R.Bsr nr c -> CS.liftValueMaybe (bsr nr) nr c


    --       _~
    --    _~ )_)_~
    --    )_))_))_)
    --    _!__!__!_
    --    \_______/
    -- ~~~~~~~~~~~~~~~
    -- Floating point
    -- (Pirate ship to indicate these are treacherous waters. Arrr.)
    -- ===============
    --
    -- XXX These are defined using simply isNaN because SNaN is a "signaling"
    -- NaN which triggers a hardware exception. We're punting on this for now.
    R.FPIsQNaN fr c -> liftFPPred isNaN fr c
    R.FPIsSNaN fr c -> liftFPPred isNaN fr c

    -- Arith
    R.FPAdd fr c1 c2 -> liftFP2 (+) fr c1 c2
    R.FPSub fr c1 c2 -> liftFP2 (-) fr c1 c2
    R.FPMul fr c1 c2 -> liftFP2 (-) fr c1 c2
    R.FPDiv fr c1 c2 -> liftFP2 (/) fr c1 c2

    -- XXX For now we return `Undefined` for whether a given PF (precision fault)
    -- was due to rounding up or down. This means the C1 x87 FPU flag will be
    -- Undefined, so we'll see if that's problematic.
    R.FPAddRoundedUp _fr c1 c2 ->
      CS.liftValueMaybe2 (\_ _ -> Nothing) boolNatRepr c1 c2
    R.FPSubRoundedUp _fr c1 c2 ->
      CS.liftValueMaybe2 (\_ _ -> Nothing) boolNatRepr c1 c2
    R.FPMulRoundedUp _fr c1 c2 ->
      CS.liftValueMaybe2 (\_ _ -> Nothing) boolNatRepr c1 c2
    R.FPCvtRoundsUp _fr1 c _fr2 ->
      CS.liftValueMaybe (const Nothing) boolNatRepr c

    -- Tests
    R.FPLt fr c1 c2 -> liftFPPred2 (<)  fr c1 c2
    R.FPEq fr c1 c2 -> liftFPPred2 (==) fr c1 c2

    -- Conversion
    R.FPCvt fr1 c fr2 -> convertFP fr1 fr2 c
    R.FPFromBV fr c -> convertBVtoFP fr c
    -- XXX FIXME: If a conversion is out of the range of the bitvector, we
    -- should raise a floating point exception. If that is masked, we should
    -- return -1 as a BV.
    R.TruncFPToSignedBV fr c nr -> liftFPtoBV (truncateIfValid nr) fr nr c

------------------------------------------------------------------------
-- Statement evaluation

-- | A version of 'evalExpr' for use in the state monad of 'evalStmt'.
evalExpr' :: (Applicative m, MonadState Env m) => Expr tp -> m (CS.Value tp)
evalExpr' e = runReader (evalExpr e) <$> get

extendEnv :: MonadState Env m => Variable tp -> CS.Value tp -> m ()
extendEnv x v = modify (MapF.insert x v)

-- | Slice a subrange from a 'BV'.
--
-- This is a wrapper around 'BV.@@' which differs by supporting empty
-- slices.
(@@) :: Integral ix => BV -> (ix, ix) -> BV
bv @@ (high, low) =
  if high >= low
  then bv BV.@@ (high, low)
  else empty
  where
    empty = BV.zeros 0

evalStmt :: forall m. (Applicative m, CS.MonadMachineState m, MonadState Env m) => Stmt -> m ()
evalStmt (MakeUndefined x tr) =
  extendEnv x (CS.Undefined tr)
evalStmt (Get x l) =
  -- Force 'tp' to be a 'BVType n'.
  case S.loc_type l of
  BVTypeRepr _ -> do

  let nr = S.loc_width l

  let memCont :: forall tp0 i.
                 Integer ~ i
              => (i, i) -> i -> (Expr (BVType 64), TypeRepr tp0) -> m ()
      memCont (low, high) _width (addr, BVTypeRepr nr0) = do
        vaddr <- evalExpr' addr
        case vaddr of
          CS.Undefined _ -> error "evalStmt: undefined address in 'Get'!"
          CS.Literal bvaddr -> do
            let a = CS.Address nr0 bvaddr
            v0 <- CS.getMem a
            let v1 = CS.liftValue (sliceBV (low, high)) nr v0
            extendEnv x v1

  let regCont :: forall cl i.
                 Integer ~ i
              => (i, i) -> i -> N.RegisterName cl -> m ()
      regCont (low, high) _width rn =
        -- Force 'tp' to be a 'BVType n'.
        case S.loc_type l of
        BVTypeRepr _ -> do
        v0 <- CS.getReg rn
        let v1 = CS.liftValue (sliceBV (low, high)) nr v0
        extendEnv x v1

  let x87Cont :: forall i. Integer ~ i => (i, i) -> i -> Int -> m()
      x87Cont (low, high) width i = 
        case S.loc_type l 
          of BVTypeRepr n -> do
             topReg <- CS.getReg N.X87TopReg
             case topReg
               of CS.Literal bv -> do
                    let top = BV.nat $ snd $ CS.unBitVector bv
                    regCont (low, high) width (N.X87FPUReg (
                     fromIntegral top + i `mod` 8))
                  CS.Undefined _ -> extendEnv x $ CS.Undefined $ BVTypeRepr n

  S.elimLocation memCont regCont x87Cont l
  where
    sliceBV :: Integer ~ i
            => (i, i) -> BV -> BV
    sliceBV (low, high) super = super @@ (high - 1, low)

evalStmt (BVDiv (xQuot, xRem) ns1 ns2) = do
  v1 <- evalExpr' ns1
  v2 <- evalExpr' ns2
  let tr = CS.asTypeRepr v2
      (q,r) = case (v1,v2) of
                (_, CS.Literal (CS.unBitVector -> (nr,bv)))
                  -> if BV.nat bv == 0
                        then error "evalStmt: BVDiv: #DE"
                        -- XXX: We use quot/rem & div/mod interchangeably,
                        --      but they round differently.
                        else ( CS.liftValue2 (takeSecondWidth div) nr v1 v2
                             , CS.liftValue2 (takeSecondWidth mod) nr v1 v2
                             )
                _ -> (CS.Undefined tr, CS.Undefined tr)
  extendEnv xQuot q
  extendEnv xRem r
evalStmt (BVSignedDiv (xQuot, xRem) ns1 ns2) = do
  v1 <- evalExpr' ns1
  v2 <- evalExpr' ns2
  let tr = CS.asTypeRepr v2
      (q,r) = case (v1,v2) of
                (_, CS.Literal (CS.unBitVector -> (nr,bv)))
                  -> if BV.nat bv == 0
                        then error "evalStmt: BVSignedDiv: #DE"
                        -- XXX: We use quot/rem & div/mod interchangeably,
                        --      but they round differently.
                        else ( CS.liftValue2 (takeSecondWidth BV.sdiv) nr v1 v2
                             , CS.liftValue2 (takeSecondWidth BV.smod) nr v1 v2
                             )
                _ -> (CS.Undefined tr, CS.Undefined tr)
  extendEnv xQuot q
  extendEnv xRem r
-- Based on 'MemCopy' eval below.
evalStmt (MemCmp x bytes compares src dst reversed) = do
  case bytes of
    1 -> go S.n8
    2 -> go S.n16
    4 -> go S.n32
    8 -> go S.n64
    _ -> error "evalStmt: MemCmp: unsupported number of bytes!"
  where
    go :: NatRepr n -> m ()
    go nr = do
      [vcompares, vsrc, vdst] <- mapM evalExpr' [compares, src, dst]
      vreversed <- evalExpr' reversed
      let srcAddrs = addressSequence vsrc nr vcompares vreversed
      let dstAddrs = addressSequence vdst nr vcompares vreversed

      matches <- forM (zip srcAddrs dstAddrs) $ \(s, d) -> do
        l <- CS.getMem s
        r <- CS.getMem d
        return $ if l == r then 1 else 0

      let lit :: CS.Value (BVType 64)
          lit = CS.Literal $ CS.bitVector knownNat (sum matches)
      extendEnv x lit
evalStmt (GetSegmentBase x seg) = do
  base <- CS.getSegmentBase seg
  extendEnv x base

-- Strategy for handling subregion writes: read the current value of
-- the full memory or register underlying the subregion in 'l',
-- redefine the subregion 'l' of the current value with the given
-- value in 'e', write back the updated value.
--
-- For example, the subregister write
--
--   %rax[8:16] := e
--
-- is effected by
--
--   v0 <- getReg %rax
--   let v1 = v0[0:8] ++ e ++ v0[16:64]
--   setReg %rax v1
--
evalStmt (l := e) =
  -- Force 'tp' to be a 'BVType n'.
  case S.loc_type l of
  BVTypeRepr _ -> do

  ve <- evalExpr' e
  let memCont :: forall tp i.
                 Integer ~ i
              => (i, i) -> i -> (Expr (BVType 64), TypeRepr tp) -> m ()
      memCont (low, high) width (addr, BVTypeRepr nr) = do
        vaddr <- evalExpr' addr
        case vaddr of
          -- Alternatively, we could mark memory values known to
          -- the machine state monad 'm' as 'Undefined' here.
          CS.Undefined _ -> error "evalStmt: undefined address in (:=)!"
          CS.Literal bvaddr -> do
            let a = CS.Address nr bvaddr
            v0 <- CS.getMem a
            let v1 = CS.liftValue2 (combineBV (low, high) width) nr v0 ve
            CS.setMem a v1
  let regCont :: forall cl i.
                 Integer ~ i
              => (i, i) -> i -> N.RegisterName cl -> m ()
      regCont (low, high) width rn = do
        let nr = N.registerWidth rn
        v0 <- CS.getReg rn
        let v1 = CS.liftValue2 (combineBV (low, high) width) nr v0 ve
        CS.setReg rn v1
  let x87Cont (low, high) width i =
        regCont (low, high) width (N.X87FPUReg i)
  let x87Cont :: forall i. Integer ~ i => (i, i) -> i -> Int -> m()
      x87Cont (low, high) width i = 
        case S.loc_type l 
          of BVTypeRepr n -> do
             topReg <- CS.getReg N.X87TopReg
             case topReg
               of CS.Literal bv -> do
                    let top = BV.nat $ snd $ CS.unBitVector bv
                    regCont (low, high) width (N.X87FPUReg (
                     fromIntegral top + i `mod` 8))
                  CS.Undefined _ -> do
                    -- undefine all the floating point registers, I guess?
                    mapM_ 
                      (\reg -> CS.setReg reg 
                               (CS.Undefined $ N.registerType reg))
                      N.x87FPURegs
  S.elimLocation memCont regCont x87Cont l
  where
    combineBV :: Integer ~ i
              => (i, i) -> i -> BV -> BV -> BV
    combineBV (low, high) width super sub =
      (super @@ (width - 1, high)) BV.#
      sub BV.#
      (super @@ (low - 1, 0))

evalStmt (Ifte_ c t f) = do
  vc <- evalExpr' c
  case vc of
    CS.Undefined _ -> error "evalStmt: Ifte_: undefined condition!"
    CS.Literal (CS.unBitVector -> (_, bv)) -> do
      -- All names in the environment are only defined once, and usage
      -- of names in the enviroment is constrained by scoping in the
      -- meta language, Haskell, so this save and restore of the
      -- environment here should be technically unnecessary.
      env0 <- get
      if BV.nat bv /= 0
      then mapM_ evalStmt t
      else mapM_ evalStmt f
      put env0
evalStmt (MemCopy bytes copies src dst reversed) = do
  case bytes of
    1 -> go S.n8
    2 -> go S.n16
    4 -> go S.n32
    8 -> go S.n64
    _ -> error "evalStmt: MemCopy: unsupported number of bytes!"
  where
    -- Construct source and destination address sequences of type
    -- @CS.Address (BVType (bytes * 8))@ and do the copies.  The
    -- address type depends on the incoming integer 'bytes', so we
    -- can't type the addresses in general; if 'bytes' were a
    -- 'NatRepr' we could.
    go :: NatRepr n -> m ()
    go nr = do
      [vcopies, vsrc, vdst] <- mapM evalExpr' [copies, src, dst]
      vreversed <- evalExpr' reversed
      let srcAddrs = addressSequence vsrc nr vcopies vreversed
      let dstAddrs = addressSequence vdst nr vcopies vreversed

      forM_ (zip srcAddrs dstAddrs) $ \(s, d) -> do
        CS.setMem d =<< CS.getMem s
evalStmt (MemSet n v a) = do
  vn <- evalExpr' n
  vv <- evalExpr' v
  va <- evalExpr' a
  let addrs = addressSequence va (CS.width vv) vn (CS.Literal CS.false)
  forM_ addrs $ \addr -> do
    CS.setMem addr vv
evalStmt (Primitive p) = CS.primitive p
-- FIXME: in Exception and BVDiv/BVSigned we use error, but we may want
-- more real exception support in the MachineState monad in the future.
evalStmt (Exception s1 s2 s3) = error "evalStmt: Exception: unimplemented"
evalStmt (X87Push s) = do
  let top = N.X87TopReg
  vTop <- CS.getReg top
  let vTop' = CS.liftValueSame ((-) 1) vTop
  CS.setReg top vTop'
  case vTop' of
    CS.Undefined _ -> error "evalStmt: X87Push: Undefined Top index"
    CS.Literal (CS.unBitVector -> (_, bv)) -> do
      let idx = fromIntegral $ BV.uint bv
      if idx > 7
         then error "evalStmt: X87Push: index out of bounds"
         else CS.setReg (N.X87FPUReg idx) =<< evalExpr' s
evalStmt X87Pop = do
  let top = N.X87TopReg
  vTop <- CS.getReg top
  CS.setReg top $ CS.liftValueSame (+1) vTop

-- | Convert a base address, increment (in bits), and count, into a sequence of
-- addresses.
--
-- TODO(conathan): move into 'MachineState' and refactor
-- 'byteAddresses' in terms of this.
addressSequence :: forall n.
                   CS.Value (BVType 64)
                -> NatRepr n
                -> CS.Value (BVType 64)
                -> CS.Value BoolType
                -> [CS.Address (BVType n)]
addressSequence (CS.Literal baseB) nr (CS.Literal countB) (CS.Literal reversedB) =
  [ CS.modifyAddr (incBv k) baseAddr
  | k <- [0..count - 1] ]
  where
    baseAddr :: CS.Address (BVType n)
    baseAddr = CS.Address nr baseB
    -- | Increment 'BV' by given number of byte-steps.
    incBv :: Integer -> BV -> BV
    incBv k = op (BV.bitVec 64 (k * byteInc))
      where
        op = if reversed == 1 then (-) else (+)
    -- | Convert bit increment to byte increment.
    byteInc :: Integer
    byteInc =
      if natValue nr `mod` 8 /= 0
      then error "addressSequence: requested number of bits is not a multiple of 8!"
      else natValue nr `div` 8
    count, reversed :: Integer
    count = CS.nat countB
    reversed = CS.nat reversedB
addressSequence _ _ _ _ = error "addressSequence: undefined argument!"

------------------------------------------------------------------------

boolNatRepr :: NatRepr 1
boolNatRepr =  n1

extPrecisionNatRepr :: NatRepr 80
extPrecisionNatRepr = n80



------------------------------------------------------------------------
-- Helper functions ----------------------------------------------------
------------------------------------------------------------------------

-- Truncate the result to the width of the second bitvector.
takeSecondWidth :: (BV -> BV -> BV) -> BV -> BV -> BV
takeSecondWidth f bv1 bv2 =
  if resUInt >= 2^w2
     then error "takeSecondWidth: result does not fit in allowed width"
     else BV.bitVec w2 resUInt
  where
    w2 = BV.width bv2
    res = f bv1 bv2
    resUInt = BV.uint res

doMux :: BV -> BV -> BV -> BV
doMux tst thn els = case BV.toBits tst of
  [b] -> if b then thn else els
  _   -> error "Impossible: type mismatch with BV"

sExtBV :: NatRepr n -> BV -> BV
sExtBV nr bv = BV.signExtend diff bv
  where
    diff = (fromInteger (natValue nr)) - BV.width bv

uExtBV :: NatRepr n -> BV -> BV
uExtBV nr bv = BV.zeroExtend diff bv
  where
    diff = (fromInteger (natValue nr)) - BV.width bv

upperBV :: NatRepr n -> BV -> BV
upperBV nr = BV.most (fromInteger (natValue nr) :: Int)

truncBV :: NatRepr n -> BV -> BV
truncBV nr = BV.least (fromInteger (natValue nr) :: Int)

bitIdx :: BV -> BV -> BV
bitIdx x i = BV.fromBool $ BV.index (BV.uint i) x


-- Wraps the result of a predicate into a BV
predBV :: (BV -> BV -> Bool) -> BV -> BV -> BV
predBV f a b = BV.fromBool $ f a b

isEvenParity :: BV -> BV
isEvenParity bv = BV.fromBool isEven
  where
    isEven = 0 == (trueCount `mod` 2)
    trueCount = length $ filter id (BV.toBits bv)

checkUadcOverflow :: BV -> BV -> BV -> BV
checkUadcOverflow a b carry = BV.fromBool didOverflow
  where
    didOverflow = total >= (2 ^ bitWidth)
    bitWidth = max (BV.width a) (BV.width b)
    total = sum $ map BV.uint [a,b,carry]

checkSadcOverflow :: BV -> BV -> BV -> BV
checkSadcOverflow a b carry = BV.fromBool didUnderOverflow
  where
    didUnderOverflow = total >= (2 ^ (bitWidth-1)) || total < (- (2 ^ (bitWidth-1)))
    bitWidth = max (BV.width a) (BV.width b)
    total = sum $ map BV.int [a,b,carry]

checkUsbbOverflow :: BV -> BV -> BV -> BV
checkUsbbOverflow a b borrow = BV.fromBool didUnderflow
  where
    didUnderflow = total < 0
    bitWidth = max (BV.width a) (BV.width b)
    total = foldl1 (-) $ map BV.uint [a,b,borrow]

checkSsbbOverflow :: BV -> BV -> BV -> BV
checkSsbbOverflow a b borrow = BV.fromBool didUnderOverflow
  where
    didUnderOverflow = total >= (2 ^ (bitWidth-1)) || total < (- (2 ^ (bitWidth-1)))
    bitWidth = max (BV.width a) (BV.width b)
    total = foldl1 (-) $ map BV.int [a,b,borrow]

-- Index of least significant non-zero bit
bsf :: NatRepr n -> BV -> Maybe BV
bsf nr bv = case BV.nat bv of
  0 -> Nothing
  _ -> Just . BV.bitVec destWidth $ BV.lsb1 bv
  where
    destWidth = fromInteger $ natValue nr :: Int

-- Index of most significant non-zero bit
bsr :: NatRepr n -> BV -> Maybe BV
bsr nr bv = case BV.nat bv of
  0 -> Nothing
  _ -> Just . BV.bitVec destWidth $ BV.msb1 bv
  where
    destWidth = fromInteger $ natValue nr :: Int

truncateIfValid :: RealFloat a
                => NatRepr n -> a -> Integer
truncateIfValid nr c = if -(2^width) <= i || i < (2^width)
                          then i
                          else -1
  where
    i     = truncate c
    width = natValue nr


---
-- Float madness
---
--
-- XXX For now we are punting on 16, 80, and 128 bit floats. We're just
-- using GHC's Float & Double types. We are also punting on rounding modes,
-- since we assume those will be rarely used and don't want to invest
-- energy if it is not necessary. We will just use the GHC default behavior,
-- which (I believe) is round-to-nearest. It is hard to find good information
-- about this though.
liftFPtoBV :: (forall a. (RealFloat a) => (a -> Integer))
           -> FloatInfoRepr flt
           -> NatRepr n
           -> CS.Value (FloatType flt)
           -> CS.Value (BVType n)
liftFPtoBV f fr nr = CS.liftValue wrap nr
  where
    width = fromIntegral $ natValue nr
    --
    wrap :: BV -> BV
    wrap bv = case natValue (floatInfoBits fr) of
      32 -> let fromBV :: BV -> Float
                fromBV = wordToFloat . fromInteger . BV.int
             in BV.bitVec width $ f (fromBV bv)

      64 -> let fromBV :: BV -> Double
                fromBV = wordToDouble . fromInteger . BV.int
             in BV.bitVec width $ f (fromBV bv)

      _  -> error "Sorry, 32 or 64 bit floats only"


liftFP2 :: (forall a. (Floating a, Num a) => (a -> a -> a))
        -> FloatInfoRepr flt
        -> CS.Value (FloatType flt)
        -> CS.Value (FloatType flt)
        -> CS.Value (FloatType flt)
liftFP2 f fr = CS.liftValue2 wrap2 nr
  where
    nr = floatInfoBits fr
    --
    wrap2 :: BV -> BV -> BV
    wrap2 bv1 bv2 = case natValue nr of
      32 -> let toBV :: Float -> BV
                toBV = BV.bitVec w . fromIntegral . floatToWord
                fromBV :: BV -> Float
                fromBV = wordToFloat . fromInteger . BV.int
             in toBV $ f (fromBV bv1) (fromBV bv2)

      64 -> let toBV :: Double -> BV
                toBV = BV.bitVec w . fromIntegral . doubleToWord
                fromBV :: BV -> Double
                fromBV = wordToDouble . fromInteger . BV.int
             in toBV $ f (fromBV bv1) (fromBV bv2)

      _  -> error "Sorry, 32 or 64 bit floats only"
      where
        w = max (BV.width bv1) (BV.width bv2)

convertBVtoFP :: CS.Value (BVType n)
              -> FloatInfoRepr flt
              -> CS.Value (FloatType flt)
convertBVtoFP c fr = CS.liftValue wrap nr c
  where
    nr = floatInfoBits fr
    width = fromIntegral $ natValue nr
    --
    wrap :: BV -> BV
    wrap bv = case width of
      32 -> let toBV :: Float -> BV
                toBV = BV.bitVec width . fromIntegral . floatToWord
                mkFP :: BV -> Float
                mkFP = fromInteger . BV.int
             in toBV $ mkFP bv

      64 -> let toBV :: Double -> BV
                toBV = BV.bitVec width . fromIntegral . doubleToWord
                mkFP :: BV -> Double
                mkFP = fromInteger . BV.int
             in toBV $ mkFP bv

      _  -> error "Sorry, 32 or 64 bit floats only"

convertFP :: FloatInfoRepr flt1
          -> FloatInfoRepr flt2
          -> CS.Value (FloatType flt1)
          -> CS.Value (FloatType flt2)
convertFP fr1 fr2 = CS.liftValue wrap nr2
  where
    nr1 = floatInfoBits fr1
    nr2 = floatInfoBits fr2
    destWidth = fromIntegral $ natValue nr2
    --
    wrap :: BV -> BV
    wrap bv = case (natValue nr1, natValue nr2) of
      (32,64) -> let toBV :: Double -> BV
                     toBV = BV.bitVec destWidth . fromIntegral . doubleToWord
                     fromBV :: BV -> Float
                     fromBV = wordToFloat . fromInteger . BV.int
                  in toBV $ float2Double (fromBV bv)

      (64,32) -> let toBV :: Float -> BV
                     toBV = BV.bitVec destWidth . fromIntegral . floatToWord
                     fromBV :: BV -> Double
                     fromBV = wordToDouble . fromInteger . BV.int
                  in toBV $ double2Float (fromBV bv)

      _       -> error "Sorry, can only convert between 32 & 64 bit floats"


---
-- Predicates
---
liftFPPred :: (forall a. (RealFloat a) => (a -> Bool))
           -> FloatInfoRepr flt
           -> CS.Value (FloatType flt)
           -> CS.Value BoolType
liftFPPred f fr = liftFPtoBV f' fr boolNatRepr
  where
    f' :: (forall a. (RealFloat a) => (a -> Integer))
    f' x = if (f x) then 1 else 0

liftFPPred2 :: (forall a. (Eq a, Ord a) => (a -> a -> Bool))
            -> FloatInfoRepr flt
            -> CS.Value (FloatType flt)
            -> CS.Value (FloatType flt)
            -> CS.Value BoolType
liftFPPred2 f fr = CS.liftValue2 wrap2 boolNatRepr
  where
    wrap2 :: BV -> BV -> BV
    wrap2 bv1 bv2 = case natValue (floatInfoBits fr) of
      32 -> let fromBV :: BV -> Float
                fromBV = wordToFloat . fromInteger . BV.int
             in BV.fromBool $ f (fromBV bv1) (fromBV bv2)

      64 -> let fromBV :: BV -> Double
                fromBV = wordToDouble . fromInteger . BV.int
             in BV.fromBool $ f (fromBV bv1) (fromBV bv2)

      _  -> error "Sorry, 32 or 64 bit floats only"

