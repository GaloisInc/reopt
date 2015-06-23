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
       ( execSemantics
       , ppStmts
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure, Applicative)
#endif
import           Control.Arrow ((***))
import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer
  (censor, execWriterT, listen, tell, MonadWriter, WriterT)
import           Data.Bits
import qualified Data.BitVector as BV
import qualified Data.Foldable as Fold
import           Data.Functor
import           Data.Maybe
import           Data.Monoid (mempty)
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Text.PrettyPrint.ANSI.Leijen
  ((<>), (<+>), colon, indent, line, parens, pretty, text, tupled, vsep, Doc, Pretty(..))

import           Data.Word
import           Numeric (showHex)

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
import qualified Reopt.Semantics.ConcreteState as CS
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

type Name = String

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
    !(NatRepr n) -> !(Expr (BVType m)) -> Expr (BVType n)
  -- A variable.
  --
  -- Not doing anything fancy with names for now; can use 'unbound'
  -- later.
  VarExpr :: Variable tp -> Expr tp

data Variable tp = Variable !(TypeRepr tp) !Name

mkLit :: NatRepr n -> Integer -> Expr (BVType n)
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

------------------------------------------------------------------------
-- Statements.

type MLocation = S.Location (Expr (BVType 64))

-- | Potentially side-effecting operations, corresponding the to the
-- 'S.Semantics' class.
data Stmt where
  -- | Bind the results of a statement to names.
  --
  -- Some statements, e.g. 'bvDiv', return multiple results, so we
  -- bind a list of 'Name's here.
  NamedStmt :: [Name] -> Stmt -> Stmt

  -- The remaining constructors correspond to the 'S.Semantics
  -- operations'.
  MakeUndefined :: TypeRepr tp -> Stmt
  Get :: MLocation tp -> Stmt
  (:=) :: MLocation tp -> Expr tp -> Stmt
  Ifte_ :: Expr BoolType -> [Stmt] -> [Stmt] -> Stmt
  MemMove :: Int
          -> Expr (BVType 64)
          -> Expr (BVType 64)
          -> Expr (BVType 64)
          -> Bool
          -> Stmt
  MemSet :: Expr (BVType 64) -> Expr (BVType n) -> Expr (BVType 64) -> Stmt
  MemCmp :: NatRepr n
         -> Expr (BVType 64)
         -> Expr BoolType
         -> Expr (BVType 64)
         -> Expr (BVType 64)
         -> Stmt
  Syscall :: Stmt
  BVDiv :: (1 <= n)
        => Expr (BVType (n+n))
        -> Expr (BVType n)
        -> Stmt
  BVSignedDiv :: (1 <= n)
              => Expr (BVType (n+n))
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

-- | Interpret 'S.Semantics' operations into 'Stmt's.
--
-- Operations that return 'Value's return fresh variables; we track
-- the relation between variables and the 'Stmt's they bind to using
-- 'NamedStmt's.
instance S.Semantics Semantics where
  make_undefined t = do
    name <- fresh "undef"
    tell [NamedStmt [name] (MakeUndefined t)]
    return $ VarExpr (Variable t name)

  get l = do
    name <- fresh "get"
    tell [NamedStmt [name] (Get l)]
    return $ VarExpr (Variable (S.loc_type l) name)

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

  memmove i v1 v2 v3 b = tell [MemMove i v1 v2 v3 b]

  memset v1 v2 v3 = tell [MemSet v1 v2 v3]

  memcmp r v1 v2 v3 v4 = do
    name <- fresh "memcmp"
    tell [NamedStmt [name] (MemCmp r v1 v2 v3 v4)]
    return $ VarExpr (Variable S.knownType name)

  syscall = tell [Syscall]

  bvDiv v1 v2 = do
    nameQuot <- fresh "divQuot"
    nameRem <- fresh "divRem"
    tell [NamedStmt [nameQuot, nameRem] (BVDiv v1 v2)]
    return (VarExpr (Variable r nameQuot), VarExpr (Variable r nameRem))
    where
      r = exprType v2

  bvSignedDiv v1 v2 = do
    nameQuot <- fresh "sdivQuot"
    nameRem <- fresh "sdivRem"
    tell [NamedStmt [nameQuot, nameRem] (BVSignedDiv v1 v2)]
    return (VarExpr (Variable r nameQuot), VarExpr (Variable r nameRem))
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
  LowerHalfExpr n e -> R.sexpr "lower_half" [ ppExpr e, R.ppNat n ]
  UpperHalfExpr n e -> R.sexpr "upper_half" [ ppExpr e, R.ppNat n ]
  TruncExpr n e -> R.sexpr "trunc" [ ppExpr e, R.ppNat n ]
  SExtExpr n e -> R.sexpr "sext" [ ppExpr e, R.ppNat n ]
  VarExpr (Variable _ x) -> text x

-- | Pretty print 'S.Location'.
--
-- Going back to pretty names for subregisters is pretty ad hoc;
-- see table at http://stackoverflow.com/a/1753627/470844. E.g.,
-- instead of @%ah@, we produce @(upper_half (lower_half (lower_half %rax)))@.
ppLocation :: (addr -> Doc) -> S.Location addr tp -> Doc
ppLocation ppAddr l = case l of
  S.MemoryAddr addr _ -> ppAddr addr
  S.Register r -> text $ "%" ++ show r
  S.TruncLoc _ _ -> ppSubregister l
  S.LowerHalf _ -> ppSubregister l
  S.UpperHalf _ -> ppSubregister l
  S.X87StackRegister i -> text $ "x87_stack@" ++ show i
  where
    -- | Print subregister as Python-style slice @<reg>[<low>:<high>]@.
    --
    -- The low bit is inclusive and the high bit is exclusive, but I
    -- can't bring myself to generate @<reg>[<low>:<high>)@ :)
    ppSubregister :: S.Location addr tp -> Doc
    ppSubregister l =
      r <> text ("[" ++ show low ++ ":" ++ show high ++ "]")
      where
        (r, low, high) = go l

    -- | Return pretty-printed register and subrange bounds.
    go :: S.Location addr tp -> (Doc, Integer, Integer)
    go (S.TruncLoc l n) = truncLoc n $ go l
    go (S.LowerHalf l) = lowerHalf $ go l
    go (S.UpperHalf l) = upperHalf $ go l
    go (S.Register r) = (text $ "%" ++ show r, 0, natValue $ N.registerWidth r)
    go _ = error "ppLocation.go: unexpected constructor"

    -- Transformations on subranges.
    truncLoc :: NatRepr n -> (Doc, Integer, Integer) -> (Doc, Integer, Integer)
    truncLoc n (r, low, _high) = (r, low, low + natValue n)
    lowerHalf, upperHalf :: (Doc, Integer, Integer) -> (Doc, Integer, Integer)
    lowerHalf (r, low, high) = (r, low, (low + high) `div` 2)
    upperHalf (r, low, high) = (r, (low + high) `div` 2, high)

ppMLocation :: MLocation tp -> Doc
ppMLocation = ppLocation ppExpr

ppStmts :: [Stmt] -> Doc
ppStmts = vsep . map ppStmt

ppStmt :: Stmt -> Doc
ppStmt s = case s of
  NamedStmt names s -> text "let" <+> tupled (map text names) <+> text "=" <+> ppStmt s
  MakeUndefined _ -> text "make_undefined"
  Get l -> R.sexpr "get" [ ppMLocation l ]
  l := e -> ppMLocation l <+> text ":=" <+> ppExpr e
  Ifte_ v t f -> vsep
    [ text "if" <+> ppExpr v
    , text "then"
    ,   indent 2 (ppStmts t)
    , text "else"
    ,   indent 2 (ppStmts f)
    ]
  MemMove i v1 v2 v3 b -> R.sexpr "memmove" [ pretty i, ppExpr v1, ppExpr v2, ppExpr v3, pretty b ]
  MemSet v1 v2 v3 -> R.sexpr "memset" [ ppExpr v1, ppExpr v2, ppExpr v3 ]
  MemCmp n v1 v2 v3 v4 -> R.sexpr "memcmp" [ R.ppNat n, ppExpr v1, ppExpr v2, ppExpr v3, ppExpr v4 ]
  Syscall -> text "syscall"
  BVDiv v1 v2 -> R.sexpr "bv_div" [ ppExpr v1, ppExpr v2 ]
  BVSignedDiv v1 v2 -> R.sexpr "bv_signed_div" [ ppExpr v1, ppExpr v2 ]
  Exception v1 v2 e -> R.sexpr "exception" [ ppExpr v1, ppExpr v2, text $ show e ]
  X87Push v -> R.sexpr "x87_push" [ ppExpr v ]
  X87Pop -> text "x87_pop"

instance Pretty Stmt where
  pretty = ppStmt



evalStmt :: MonadMachineState m => Stmt -> m ()
evalStmt = undefined

evalExpr :: (MonadReader Env m, Applicative m) => Expr tp -> m (CS.Value tp)
evalExpr (AppExpr a) = do
  a' <- R.traverseApp evalExpr a
  return $ case a' of
    R.BVAdd   nr c1 c2 -> CS.liftValue2 (+)    nr             c1 c2
    R.ConcatV nr c1 c2 -> CS.liftValue2 (BV.#) (addNat nr nr) c1 c2

type Env = MapF Variable CS.Value

class Monad m => MonadMachineState m where

