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
import           Data.Parameterized.Classes (OrderingF(..), OrdF, compareF, fromOrdering)
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
import qualified Reopt.Concrete.MachineState as CS
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

data NamedStmt where
  MakeUndefined :: TypeRepr tp -> NamedStmt
  Get :: MLocation tp -> NamedStmt
  BVDiv :: (1 <= n)
        => Expr (BVType (n+n))
        -> Expr (BVType n)
        -> NamedStmt
  BVSignedDiv :: (1 <= n)
              => Expr (BVType (n+n))
              -> Expr (BVType n)
              -> NamedStmt
  MemCmp :: NatRepr n
         -> Expr (BVType 64)
         -> Expr BoolType
         -> Expr (BVType 64)
         -> Expr (BVType 64)
         -> NamedStmt

-- | Potentially side-effecting operations, corresponding the to the
-- 'S.Semantics' class.
data Stmt where
  -- | Bind the results of a statement to names.
  --
  -- Some statements, e.g. 'bvDiv', return multiple results, so we
  -- bind a list of 'Name's here.
  NamedStmt :: [Name] -> NamedStmt -> Stmt

  -- The remaining constructors correspond to the 'S.Semantics
  -- operations'.
  (:=) :: MLocation tp -> Expr tp -> Stmt
  Ifte_ :: Expr BoolType -> [Stmt] -> [Stmt] -> Stmt
  MemMove :: Int
          -> Expr (BVType 64)
          -> Expr (BVType 64)
          -> Expr (BVType 64)
          -> Bool
          -> Stmt
  MemSet :: Expr (BVType 64) -> Expr (BVType n) -> Expr (BVType 64) -> Stmt
  Syscall :: Stmt
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
    tell [NamedStmt [name] (MakeUndefined t)]
    return $ VarExpr (Variable t name)

  get l = do
    name <- fresh "get"
    tell [NamedStmt [name] (Get l)]
    return $ VarExpr (Variable (S.loc_type l) name)

  -- sjw: This is a huge hack, but then again, so is the fact that it
  -- happens at all.  According to the ISA, assigning a 32 bit value
  -- to a 64 bit register performs a zero extension so the upper 32
  -- bits are zero.  This may not be the best place for this, but I
  -- can't think of a nicer one ...
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

  memmove i v1 v2 v3 b = tell [MemMove i v1 v2 v3 b]

  memset v1 v2 v3 = tell [MemSet v1 v2 v3]

  memcmp r v1 v2 v3 v4 = do
    name <- fresh "memcmp"
    tell [NamedStmt [name] (MemCmp r v1 v2 v3 v4)]
    -- return $ VarExpr (Variable S.knownType name)

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
  TruncExpr n e -> R.sexpr "trunc" [ ppExpr e, R.ppNat n ]
  SExtExpr n e -> R.sexpr "sext" [ ppExpr e, R.ppNat n ]
  VarExpr (Variable _ x) -> text x

-- | Pretty print 'S.Location'.
--
-- Going back to pretty names for subregisters is pretty ad hoc;
-- see table at http://stackoverflow.com/a/1753627/470844. E.g.,
-- instead of @%ah@, we produce @(upper_half (lower_half (lower_half %rax)))@.
ppLocation :: forall addr tp. (addr -> Doc) -> S.Location addr tp -> Doc
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
    ppSubregister :: forall tp. S.Location addr tp -> Doc
    ppSubregister l =
      r <> text ("[" ++ show low ++ ":" ++ show high ++ "]")
      where
        (r, low, high) = go l

    -- | Return pretty-printed register and subrange bounds.
    go :: forall tp. S.Location addr tp -> (Doc, Integer, Integer)
    go (S.TruncLoc l n) = truncLoc n $ go l
    go (S.LowerHalf l) = lowerHalf $ go l
    go (S.UpperHalf l) = upperHalf $ go l
    go (S.Register r) = (text $ "%" ++ show r, 0, natValue $ N.registerWidth r)
    go (S.MemoryAddr addr (BVTypeRepr nr)) = (ppAddr addr, 0, natValue nr)
    go (S.MemoryAddr _ _) = error "ppLocation.go: address of non 'BVType n' type!"


    -- Transformations on subranges.
    truncLoc :: NatRepr n -> (Doc, Integer, Integer) -> (Doc, Integer, Integer)
    truncLoc n (r, low, _high) = (r, low, low + natValue n)
    lowerHalf, upperHalf :: (Doc, Integer, Integer) -> (Doc, Integer, Integer)
    lowerHalf (r, low, high) = (r, low, (low + high) `div` 2)
    upperHalf (r, low, high) = (r, (low + high) `div` 2, high)

ppMLocation :: MLocation tp -> Doc
ppMLocation = ppLocation ppExpr

ppNamedStmt :: NamedStmt -> Doc
ppNamedStmt s = case s of
  MakeUndefined _ -> text "make_undefined"
  Get l -> R.sexpr "get" [ ppMLocation l ]
  BVDiv v1 v2 -> R.sexpr "bv_div" [ ppExpr v1, ppExpr v2 ]
  BVSignedDiv v1 v2 -> R.sexpr "bv_signed_div" [ ppExpr v1, ppExpr v2 ]
  MemCmp n v1 v2 v3 v4 ->
    R.sexpr "memcmp" [ R.ppNat n, ppExpr v1, ppExpr v2, ppExpr v3, ppExpr v4 ]

ppStmts :: [Stmt] -> Doc
ppStmts = vsep . map ppStmt

ppStmt :: Stmt -> Doc
ppStmt s = case s of
  NamedStmt names s' ->
    text "let" <+> tupled (map text names) <+> text "=" <+> ppNamedStmt s'
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
  Syscall -> text "syscall"
  Exception v1 v2 e -> R.sexpr "exception" [ ppExpr v1, ppExpr v2, text $ show e ]
  X87Push v -> R.sexpr "x87_push" [ ppExpr v ]
  X87Pop -> text "x87_pop"

instance Pretty Stmt where
  pretty = ppStmt

------------------------------------------------------------------------
-- Expression evaluation

type Env = MapF Variable CS.Value

evalExpr :: (MonadReader Env m, Applicative m) => Expr tp -> m (CS.Value tp)
evalExpr (LitExpr nr i) = return $ CS.Literal bVec
  where
    bVec = CS.bitVector nr (BV.bitVec bitWidth i)
    bitWidth = fromInteger (natValue nr)

evalExpr (TruncExpr nr e) = do
  c <- evalExpr e
  let bitWidth :: Int
      bitWidth = fromInteger (natValue nr)
  return $ CS.liftValue (BV.least bitWidth) nr c

evalExpr (SExtExpr nr e) = do
  c <- evalExpr e
  let -- Desired length.
      bitWidth :: Int
      bitWidth = fromInteger (natValue nr)
      -- Grow the BV by the difference to desired length.
      sExtGrow :: BV.BV -> BV.BV
      sExtGrow bv = let currentBitWidth = BV.width bv
                        diff = bitWidth - currentBitWidth
                    in BV.signExtend diff bv
  return $ CS.liftValue sExtGrow nr c

evalExpr (VarExpr var) = do
  maybeVal <- asks (MapF.lookup var)
  let msg = "Bug: unbound variable in expr"
  maybe (error msg) return maybeVal

evalExpr (AppExpr a) = do
  a' <- R.traverseApp evalExpr a
  return $ case a' of
    R.BVAdd   nr c1 c2 -> CS.liftValue2 (+)    nr             c1 c2
    R.ConcatV nr c1 c2 -> CS.liftValue2 (BV.#) (addNat nr nr) c1 c2

------------------------------------------------------------------------
-- Statement evaluation

-- | A version of 'evalExpr' for use in the state monad of 'evalStmt'.
evalExpr' :: (Applicative m, MonadState Env m) => Expr tp -> m (CS.Value tp)
evalExpr' e = runReader (evalExpr e) <$> get

extendEnv :: MonadState Env m => Variable tp -> CS.Value tp -> m ()
extendEnv x v = modify (MapF.insert x v)

evalStmt :: (Applicative m, CS.MonadMachineState m, MonadState Env m) => Stmt -> m ()
evalStmt (NamedStmt names (MakeUndefined ns)) = undefined
evalStmt (NamedStmt [x] (Get l)) = do
  case l of
    S.MemoryAddr addr tr@(BVTypeRepr nr) -> do
      vaddr <- evalExpr' addr
      case vaddr of
        CS.Undefined _ -> error "evalStmt: undefined address in 'get'!"
        CS.Literal bvaddr -> do
          v <- CS.getMem (CS.Address nr bvaddr)
          extendEnv (Variable tr x) v
    S.Register rn -> do
      let tr = N.registerType rn
      v <- CS.getReg rn
      extendEnv (Variable tr x) v
    S.X87StackRegister i -> do
      let rn = N.X87FPUReg i
      let tr = N.registerType rn
      v <- CS.getReg rn
      extendEnv (Variable tr x) v
    _ -> undefined -- subregisters
evalStmt (NamedStmt names (BVDiv ns1 ns2)) = undefined
evalStmt (NamedStmt names (BVSignedDiv ns1 ns2)) = undefined
evalStmt (NamedStmt names (MemCmp ns1 ns2 ns3 ns4 ns5)) = undefined
evalStmt (l := e) = do
  ve <- evalExpr' e
  case l of
    S.MemoryAddr addr (BVTypeRepr nr) -> do
      vaddr <- evalExpr' addr
      case vaddr of
        -- Alternatively, we could mark all known memory values
        -- ('dumpMem8') as 'Undefined' here. It would be more accurate
        -- to mark *all* of memory as undefined.
        CS.Undefined _ -> error "evalStmt: undefined address in (:=)!"
        CS.Literal bvaddr -> CS.setMem (CS.Address nr bvaddr) ve
    S.Register rn -> CS.setReg rn ve
    S.X87StackRegister i -> CS.setReg (N.X87FPUReg i) ve
    _ -> undefined -- subregisters
evalStmt (Ifte_ s1 s2 s3) = undefined
evalStmt (MemMove s1 s2 s3 s4 s5) = undefined
evalStmt (MemSet s1 s2 s3) = undefined
evalStmt Syscall = undefined
evalStmt (Exception s1 s2 s3) = undefined
evalStmt (X87Push s) = undefined
evalStmt X87Pop = undefined

