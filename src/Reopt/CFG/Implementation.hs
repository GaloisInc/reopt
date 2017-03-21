{-
Module           : Reopt.Semantics.Implementation
Copyright        : (c) Galois, Inc 2015
Maintainer       : Joe Hendrix <jhendrix@galois.com>

This contains an implementation of the classes defined in Reopt.Semantics.Monad
that map  a control flow graph from a program using the semantics.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Werror #-}
module Reopt.CFG.Implementation
       ( x86_64_freeBSD_info
       , x86_64_linux_info
       , freeBSD_syscallPersonality
       , linux_syscallPersonality
       , disassembleBlock
       , X86TranslateError(..)
       ) where

import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import qualified Flexdis86 as Flexdis
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..))

import           Data.Macaw.AbsDomain.AbsState
       ( AbsBlockState
       , setAbsIP

       , absRegState
       , StackEntry(..)
       , concreteStackOffset
       , AbsValue(..)
       , top
       , stridedInterval
       , asConcreteSingleton
       , startAbsStack
       , hasMaximum
       , AbsProcessorState
       , transferValue
       ,
       )
import qualified Data.Macaw.AbsDomain.StridedInterval as SI
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.Architecture.Syscall
import           Data.Macaw.CFG
import           Data.Macaw.Memory
import           Data.Macaw.Memory.Flexdis86
import           Data.Macaw.Types (BVType, knownType, typeRepr)

import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.SysDeps.FreeBSDGenerated as FreeBSD
import           Reopt.Machine.SysDeps.LinuxGenerated as Linux
import           Reopt.Machine.X86State
import           Reopt.Semantics.FlexdisMatcher (execInstruction)
import           Reopt.Semantics.Monad
  ( BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S

------------------------------------------------------------------------
-- Expr

-- | A pure expression for isValue.
data Expr ids tp where
  -- An expression obtained from some value.
  ValueExpr :: !(Value X86_64 ids tp) -> Expr ids tp
  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(App (Expr ids) tp) -> Expr ids tp

instance Show (Expr ids tp) where
  show (ValueExpr v) = show v
  show AppExpr{} = "app"

instance MapF.ShowF (Expr ids) where
  showF = show

instance Eq (Expr ids tp) where
  (==) = \x y -> isJust (testEquality x y)

instance TestEquality (Expr ids) where
  testEquality (ValueExpr x) (ValueExpr y) = do
    Refl <- testEquality x y
    return Refl
  testEquality (AppExpr x) (AppExpr y) = do
    Refl <- testEquality x y
    return Refl
  testEquality _ _ = Nothing

asApp :: Expr ids tp -> Maybe (App (Expr ids) tp)
asApp (AppExpr   a) = Just a
asApp (ValueExpr v) = mapApp ValueExpr <$> valueAsApp v

app :: App (Expr ids) tp -> (Expr ids) tp
app = AppExpr

exprType :: Expr ids tp -> S.TypeRepr tp
exprType (ValueExpr v) = typeRepr v
exprType (AppExpr a) = appType a

-- | Return width of expression.
exprWidth :: Expr ids (BVType n) -> NatRepr n
exprWidth e =
  case exprType e of
    S.BVTypeRepr n -> n

asBVLit :: Expr ids tp -> Maybe Integer
asBVLit (ValueExpr (BVValue _ v)) = Just v
asBVLit _ = Nothing

ltProof :: forall f n m . (n+1 <= m) => f n -> f m -> LeqProof n m
ltProof _ _ = leqTrans lt LeqProof
  where lt :: LeqProof n (n+1)
        lt = leqAdd LeqProof S.n1

bvSle :: Expr ids (BVType n) -> Expr ids (BVType n) -> Expr ids BoolType
bvSle x y = app (BVSignedLe x y)

bvUle :: Expr ids (BVType n) -> Expr ids (BVType n) -> Expr ids BoolType
bvUle x y = app (BVUnsignedLe x y)

-- | Negate expression while eliminating double negations
boolNot :: Expr ids BoolType -> Expr ids BoolType
boolNot (asApp -> Just (NotApp b)) = b
boolNot b = app (NotApp b)

instance S.IsValue (Expr ids) where

  bv_width = exprWidth

  mux c x y
    | Just 1 <- asBVLit c = x
    | Just 0 <- asBVLit c = y
    | x == y = x
    | Just (BVComplement _ cn) <- asApp c = app $ Mux (exprWidth x) cn y x
    | otherwise = app $ Mux (exprWidth x) c x y

  bvLit n v = ValueExpr $ mkLit n (toInteger v)
  bvAdd x y
      -- Eliminate add 0
    | Just 0 <- asBVLit y = x
    | Just 0 <- asBVLit x = y

      -- Constant folding.
    | ValueExpr (BVValue w xv) <- x
    , Just yv <- asBVLit y
    = bvLit w (xv + yv)

    | ValueExpr (RelocatableValue w a) <- x
    , Just o <- asBVLit y
    = ValueExpr (RelocatableValue w (a + fromInteger o))

    | ValueExpr (RelocatableValue w a) <- y
    , Just o <- asBVLit x
    = ValueExpr (RelocatableValue w (a + fromInteger o))

      -- Shift constants to right-hand-side.
    | Just _ <- asBVLit x = S.bvAdd y x

      -- Reorganize addition by constant to offset.
    | Just (BVAdd w x_base (asBVLit -> Just x_off)) <- asApp x
    , Just y_off <- asBVLit y
    = S.bvAdd x_base (bvLit w (x_off + y_off))

    | Just (BVAdd w y_base (asBVLit -> Just y_off)) <- asApp y
    , Just x_off <- asBVLit x
    = S.bvAdd y_base (bvLit w (x_off + y_off))

    | otherwise = app $ BVAdd (exprWidth x) x y

  bvSub x y
    | Just yv <- asBVLit y = S.bvAdd x (bvLit (exprWidth x) (negate yv))
    | otherwise = app $ BVSub (exprWidth x) x y

  bvMul x y
    | Just 0 <- asBVLit x = x
    | Just 1 <- asBVLit x = y
    | Just 0 <- asBVLit y = y
    | Just 1 <- asBVLit y = x

    | Just xv <- asBVLit x, Just yv <- asBVLit y =
      bvLit (exprWidth x) (xv * yv)
    | otherwise = app $ BVMul (exprWidth x) x y

  complement x
    | Just xv <- asBVLit x = bvLit (exprWidth x) (complement xv)
      -- not (y < z) = y >= z = z <= y
    | Just (BVUnsignedLt y z) <- asApp x = bvUle z y
      -- not (y <= z) = y > z = z < y
    | Just (BVUnsignedLe y z) <- asApp x = S.bvUlt z y
      -- not (y < z) = y >= z = z <= y
    | Just (BVSignedLt y z) <- asApp x = bvSle z y
      -- not (y <= z) = y > z = z < y
    | Just (BVSignedLe y z) <- asApp x = S.bvSlt z y
      -- not (not p) = p
    | Just (BVComplement _ y) <- asApp x = y
    | otherwise = app $ BVComplement (exprWidth x) x

  x .&. y
    | Just xv <- asBVLit x, Just yv <- asBVLit y =
      bvLit (exprWidth x) (xv .&. yv)
      -- Eliminate and when one argument is maxUnsigned
    | Just xv <- asBVLit x, xv == maxUnsigned (exprWidth x) = y
    | Just yv <- asBVLit y, yv == maxUnsigned (exprWidth x) = x
      -- Cancel when and with 0.
    | Just 0 <- asBVLit x = x
    | Just 0 <- asBVLit y = y
      -- Idempotence
    | x == y = x

      -- Make literal the second argument (simplifies later cases)
    | isJust (asBVLit x) = assert (isNothing (asBVLit y)) $ y S..&. x

      --(x1 .&. x2) .&. y = x1 .&. (x2 .&. y) -- Only apply when x2 and y is a lit
    | isJust (asBVLit y)
    , Just (BVAnd _ x1 x2) <- asApp x
    , isJust (asBVLit x2) =
      x1 S..&. (x2 S..&. y)

      -- (x1 .|. x2) .&. y = (x1 .&. y) .|. (x2 .&. y) -- Only apply when y and x2 is a lit.
    | isJust (asBVLit y)
    , Just (BVOr _ x1 x2) <- asApp x
    ,  isJust (asBVLit x2) =
      (x1 S..&. y) S..|. (x2 S..&. y)
      -- x .&. (y1 .|. y2) = (y1 .&. x) .|. (y2 .&. x) -- Only apply when x and y2 is a lit.
    | isJust (asBVLit x)
    , Just (BVOr _ y1 y2) <- asApp y
    , isJust (asBVLit y2) =
      (y1 S..&. x) S..|. (y2 S..&. x)

      -- x1 <= x2 & x1 ~= x2 = x1 < x2
    | Just (BVUnsignedLe x1 x2) <- asApp x
    , Just (BVComplement _ yc) <- asApp y
    , Just (BVEq y1 y2) <- asApp yc
    , Just Refl <- testEquality (exprWidth x1) (exprWidth y1)
    , ((x1,x2) == (y1,y2) || (x1,x2) == (y2,y1)) =
      S.bvUlt x1 x2

      -- x1 ~= x2 & x1 <= x2 => x1 < x2
    | Just (BVUnsignedLe y1 y2) <- asApp y
    , Just (BVComplement _ xc) <- asApp x
    , Just (BVEq x1 x2) <- asApp xc
    , Just Refl <- testEquality (exprWidth x1) (exprWidth y1)
    , ((x1,x2) == (y1,y2) || (x1,x2) == (y2,y1)) =
      S.bvUlt y1 y2

      -- Default case
    | otherwise = app $ BVAnd (exprWidth x) x y

  x .|. y
    | Just xv <- asBVLit x, Just yv <- asBVLit y =
      bvLit (exprWidth x) (xv .|. yv)
      -- Cancel or when one argument is maxUnsigned
    | Just xv <- asBVLit x, xv == maxUnsigned (exprWidth x) = x
    | Just yv <- asBVLit y, yv == maxUnsigned (exprWidth x) = y
      -- Eliminate "or" when one argument is 0
    | Just 0 <- asBVLit x = y
    | Just 0 <- asBVLit y = x
      -- Idempotence
    | x == y = x

      -- Rewrite "x < y | x == y" to "x <= y"
    | Just (BVUnsignedLt x1 x2) <- asApp x
    , Just (BVEq y1 y2) <- asApp y
    , Just Refl <- testEquality (exprWidth x1) (exprWidth y1)
    , (x1,x2) == (y1,y2) || (x1,x2) == (y2,y1)
    = bvUle x1 x2

      -- Default case
    | otherwise = app $ BVOr (exprWidth x) x y

  bvXor x y
      -- Eliminate xor with 0.
    | Just 0 <- asBVLit x = y
    | Just 0 <- asBVLit y = x
      -- Eliminate xor with self.
    | x == y = bvLit (exprWidth x) (0::Integer)
      -- If this is a single bit comparison with a constant, resolve to Boolean operation.
    | ValueExpr (BVValue w v) <- y
    , Just Refl <- testEquality w S.n1 =
      if v /= 0 then boolNot x else x
      -- Default case.
    | otherwise = app $ BVXor (exprWidth x) x y

  x .=. y
    | x == y = S.true
      -- Statically resolve two literals
    | Just xv <- asBVLit x, Just yv <- asBVLit y = S.boolValue (xv == yv)
      -- Move constant to second argument (We implicitly know both x and y are not a constant due to previous case).
    | Just _ <- asBVLit x, Nothing <- asBVLit y = y S..=. x
      -- If this is a single bit comparison with a constant, resolve to Boolean operation.
    | ValueExpr (BVValue w v) <- y
    , Just Refl <- testEquality w S.n1 =
      if v /= 0 then x else boolNot x
      -- Rewrite "base + offset = constant" to "base = constant - offset".
    | Just (BVAdd w x_base (asBVLit -> Just x_off)) <- asApp x
    , Just yv <- asBVLit y =
      app $ BVEq x_base (bvLit w (yv - x_off))
      -- Rewrite "u - v == c" to "u = c + v".
    | Just (BVSub _ x_1 x_2) <- asApp x = x_1 S..=. S.bvAdd y x_2
      -- Rewrite "c == u - v" to "u = c + v".
    | Just (BVSub _ y_1 y_2) <- asApp y = y_1 S..=. S.bvAdd x y_2
      -- General case
    | otherwise = app $ BVEq x y

  -- | Splits a bit vectors into two
  -- bvSplit :: v (BVType (n + n)) -> (v (BVType n), v (BVType n))
  bvSplit v = (upperHalf v, lowerHalf v)

  -- | Shifts, the semantics is undefined for shifts >= the width of the first argument
  -- bvShr, bvSar, bvShl :: v (BVType n) -> v (BVType log_n) -> v (BVType n)
  bvShr x y
    | Just 0 <- asBVLit y = x
    | Just 0 <- asBVLit x = x
    | otherwise = app $ BVShr (exprWidth x) x y
  bvSar x y = app $ BVSar (exprWidth x) x y

  bvShl x y
    | Just 0 <- asBVLit y = x

    | Just xv <- asBVLit x
    , Just yv <- asBVLit y =
      assert (yv <= toInteger (maxBound :: Int)) $
        bvLit (exprWidth x) (xv `shiftL` fromInteger yv)

      -- Replace "(x >> c) << c" with (x .&. - 2^c)
    | Just yv <- asBVLit y
    , Just (BVShr w x_base (asBVLit -> Just x_shft)) <- asApp x
    , x_shft == yv =
      x_base S..&. bvLit w (negate (2^x_shft) ::Integer)

    | Just yv <- asBVLit y
    , yv >= natValue (exprWidth x) = bvLit (exprWidth x) (0 :: Integer)

    | otherwise = app $ BVShl (exprWidth x) x y

  bvTrunc' w e0
    | Just v <- asBVLit e0 =
      bvLit w v
    | Just Refl <- testEquality (exprWidth e0) w =
      e0
    | Just (MMXExtend e) <- asApp e0
    , Just Refl <- testEquality w n64 =
      e
    -- WARNING: the 'ConcatV' here appears to be little endian,
    -- whereas 'bvCat' in 'IsValue' is big endian. Is this a bug?
    | Just (ConcatV lw _ l _) <- asApp e0
    , Just LeqProof <- testLeq w lw =
      S.bvTrunc w l
    | Just (UExt e _) <- asApp e0 =
      case testLeq w (S.bv_width e) of
        -- Check if original value width is less than new width.
        Just LeqProof -> S.bvTrunc w e
        Nothing ->
          -- Runtime check to wordaround GHC typechecker
          case testLeq (S.bv_width e) w of
            Just LeqProof -> S.uext w e
            Nothing -> error "bvTrunc internal error"
      -- Trunc (x .&. y) w = trunc x w .&. trunc y w
    | Just (BVAnd _ x y) <- asApp e0 =
      let x' = S.bvTrunc' w x
          y' = S.bvTrunc' w y
       in x' S..&. y'
      -- Trunc (x .|. y) w = trunc x w .|. trunc y w
    | Just (BVOr _ x y) <- asApp e0 =
      let x' = S.bvTrunc' w x
          y' = S.bvTrunc' w y
       in x' S..|. y'
      -- trunc (Trunc e w1) w2 = trunc e w2
    | Just (Trunc e _) <- asApp e0 =
      -- Runtime check to workaround GHC typechecker.
      case testLeq w (exprWidth e) of
        Just LeqProof -> S.bvTrunc w e
        Nothing -> error "bvTrunc given bad width"
      -- Default case
    | otherwise = app (Trunc e0 w)

  bvUlt x y
    | Just xv <- asBVLit x, Just yv <- asBVLit y = S.boolValue (xv < yv)
    | x == y = S.false
    | otherwise = app $ BVUnsignedLt x y

  bvSlt x y
    | Just xv <- asBVLit x, Just yv <- asBVLit y = S.boolValue (xv < yv)
    | x == y = S.false
    | otherwise = app $ BVSignedLt x y

  bvBit x y
    | Just xv <- asBVLit x
    , Just yv <- asBVLit y =
      S.boolValue (xv `testBit` fromInteger yv)
    | Just (Trunc xe w) <- asApp x
    , Just yv <- asBVLit y = assert (0 <= yv && yv < natValue w) $
      S.bvBit xe y

    | Just (ConcatV lw _lw' x_low x_high) <- asApp x
    , Just yv <- asBVLit y = assert (0 <= yv && yv < 2*natValue lw) $
      if yv >= natValue lw then
        S.bvBit x_high (S.bvLit (exprWidth y) (yv - natValue lw))
      else
        S.bvBit x_low y

    | otherwise =
      app $ BVTestBit x y

  sext' w e0
      -- Collapse duplicate extensions.
    | Just (SExt e w0) <- asApp e0 = do
      let we = S.bv_width e
      withLeqProof (leqTrans (ltProof we w0) (ltProof w0 w)) $
        S.sext w e
    | otherwise = app (SExt e0 w)

  uext' w e0
      -- Literal case
    | Just v <- asBVLit e0 =
      let w0 = S.bv_width e0
       in withLeqProof (leqTrans (leqProof S.n1 w0) (ltProof w0 w)) $
            bvLit w v
      -- Collapse duplicate extensions.
    | Just (UExt e w0) <- asApp e0 = do
      let we = S.bv_width e
      withLeqProof (leqTrans (ltProof we w0) (ltProof w0 w)) $
        S.uext w e

{-
      -- Convert @uext w (trunc w0 e)@ into @e .&. (2^w0 - 1)@
      -- We have disabled this because our abstract domains are not as precise.
    | Just (Trunc e w0) <- asApp e0
    , Just Refl <- testEquality (S.bv_width e) w = do
      withLeqProof (leqTrans (leqProof S.n1 w0) (ltProof w0 w)) $
        e S..&. bvLit w (maxUnsigned w0)
-}

      -- Default case
    | otherwise = app (UExt e0 w)

  even_parity x
    | Just xv <- asBVLit x =
        let go 8 r = r
            go i r = go (i+1) $! (xv `testBit` i /= r)
         in S.boolValue (go 0 True)
    | otherwise = app $ EvenParity x
  reverse_bytes x = app $ ReverseBytes (exprWidth x) x

  uadc_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
    | otherwise = app $ UadcOverflows (exprWidth x) x y c
  sadc_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
    | otherwise = app $ SadcOverflows (exprWidth x) x y c

  usbb_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
      -- If the borrow bit is zero, this is equivalent to unsigned x < y.
    | Just 0 <- asBVLit c = S.bvUlt x y
    | otherwise = app $ UsbbOverflows (exprWidth x) x y c

  ssbb_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
      -- If the borrow bit is zero, this is equivalent to signed x < y.
      -- FIXME: not true? | Just 0 <- asBVLit c = app $ BVSignedLt x y
    | otherwise = app $ SsbbOverflows (exprWidth x) x y c

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

  fpCvt src tgt x =
    case testEquality src tgt of
      Just Refl -> x
      _ -> app $ FPCvt src x tgt
  fpCvtRoundsUp src tgt x = app $ FPCvtRoundsUp src x tgt

  fpFromBV tgt x = app $ FPFromBV x tgt
  truncFPToSignedBV tgt src x = app $ TruncFPToSignedBV src x tgt

------------------------------------------------------------------------
-- GenState

-- | A block that we have not yet finished.
data PreBlock ids = PreBlock { pBlockLabel  :: !(BlockLabel 64)
                             , _pBlockStmts :: !(Seq (Stmt X86_64 ids))
                             , _pBlockState :: !(RegState X86Reg (Value X86_64 ids))
                             , _pBlockApps  :: !(MapF (App (Value X86_64 ids)) (Assignment X86_64 ids))
                             }

pBlockStmts :: Simple Lens (PreBlock ids) (Seq (Stmt X86_64 ids))
pBlockStmts = lens _pBlockStmts (\s v -> s { _pBlockStmts = v })

pBlockState :: Simple Lens (PreBlock ids) (RegState X86Reg (Value X86_64 ids))
pBlockState = lens _pBlockState (\s v -> s { _pBlockState = v })

pBlockApps  :: Simple Lens (PreBlock ids) (MapF (App (Value X86_64 ids)) (Assignment X86_64 ids))
pBlockApps = lens _pBlockApps (\s v -> s { _pBlockApps = v })

-- | A tagged maybe
data MaybeF (t :: Bool) a where
  NothingF :: MaybeF 'False a
  JustF    :: a -> MaybeF 'True a

_JustF :: Lens (MaybeF 'True a) (MaybeF 'True b) a b
_JustF = lens (\(JustF v) -> v) (\_ v -> JustF v)

-- | A state used for the block generator.
data GenState st_s ids tag = GenState
       { assignIdGen :: !(NonceGenerator (ST st_s) ids)
         -- ^ 'NonceGenerator' for generating 'AssignId's
       , _nextBlockID  :: !Word64
         -- ^ Index of next block
       , _frontierBlocks :: !(Seq (Block X86_64 ids))
         -- ^ Blocks added to CFG
       , _blockState     :: !(MaybeF tag (PreBlock ids))
         -- ^ Blocks generated so far
       }

-- | Control flow blocs generated so far.
nextBlockID :: Simple Lens (GenState st_s ids tag) Word64
nextBlockID = lens _nextBlockID (\s v -> s { _nextBlockID = v })

-- | Blocks that are not in CFG that end with a FetchAndExecute,
-- which we need to analyze to compute new potential branch targets.
frontierBlocks :: Simple Lens (GenState st_s ids tag) (Seq (Block X86_64 ids))
frontierBlocks = lens _frontierBlocks (\s v -> s { _frontierBlocks = v })

-- | Blocks that are not in CFG that end with a FetchAndExecute,
-- which we need to analyze to compute new potential branch targets.
blockState :: Lens (GenState st_s ids a) (GenState st_s ids b)
              (MaybeF a (PreBlock ids)) (MaybeF b (PreBlock ids))
blockState = lens _blockState (\s v -> s { _blockState = v })

emptyPreBlock :: RegState X86Reg (Value X86_64 ids)
              -> BlockLabel 64
              -> PreBlock ids
emptyPreBlock s lbl =
  PreBlock { pBlockLabel = lbl
           , _pBlockStmts = Seq.empty
           , _pBlockApps  = MapF.empty
           , _pBlockState = s
           }

emptyGenState :: NonceGenerator (ST st_s) ids -> GenState st_s ids 'False
emptyGenState nonce_gen =
  GenState { assignIdGen = nonce_gen
           , _nextBlockID    = 1
           , _frontierBlocks = Seq.empty
           , _blockState     = NothingF
           }

curX86State :: Simple Lens (GenState st_s ids 'True) (RegState X86Reg (Value X86_64 ids))
curX86State = blockState . _JustF . pBlockState

-- | Finishes the current block, if it is started.
finishBlock' :: PreBlock ids
             -> (RegState X86Reg (Value X86_64 ids) -> TermStmt X86_64 ids)
             -> Block X86_64 ids
finishBlock' pre_b term =
  Block { blockLabel = pBlockLabel pre_b
        , blockStmts = Fold.toList (pre_b^.pBlockStmts)
        , blockTerm  = term (pre_b^.pBlockState)
        }

-- | Finishes the current block, if it is started.
finishBlock :: (RegState X86Reg (Value X86_64 ids) -> TermStmt X86_64 ids)
            -> (GenState st_s ids a -> GenState st_s ids 'False)
finishBlock term st =
  case st^.blockState of
   NothingF    -> st
   JustF pre_b -> seq b $
       st & frontierBlocks %~ (Seq.|> b)
          & blockState .~ NothingF
     where
       b = finishBlock' pre_b term

------------------------------------------------------------------------
-- X86Generator

-- | X86Generator is used to construct basic blocks from a stream of instructions
-- using the semantics.
--
-- It is implemented as a state monad in a continuation passing style so that
-- we can perform symbolic branches.
--
-- This returns either a failure message or the next state.
newtype X86Generator st_s ids a =
  X86G { unX86G ::
           ContT (Some (GenState st_s ids))
           (ReaderT (GenState st_s ids 'True)
            (ExceptT Text (ST st_s))) a
       }
  deriving (Functor, Applicative)

-- The main reason for this definition to be given explicitly is so that fail
-- uses throwError instead of the underlying fail in ST
instance Monad (X86Generator st_s ids) where
  return v = seq v $ X86G $ return v
  (X86G m) >>= h = X86G $ m >>= \v -> seq v (unX86G (h v))
  X86G m >> X86G n = X86G $ m >> n
  fail msg = seq t $ X86G $ ContT $ \_ -> throwError t
    where t = Text.pack msg

type instance S.Value (X86Generator st_s ids) = Expr ids

-- | The type of an 'X86Generator' continuation
type X86GCont st_s ids a
  =  a
  -> GenState st_s ids 'True
  -> ExceptT Text (ST st_s) (Some (GenState st_s ids))

-- | Run an 'X86Generator' starting from a given state
runX86Generator :: X86GCont st_s ids a
                -> GenState st_s ids 'True
                -> X86Generator st_s ids a
                -> ExceptT Text (ST st_s) (Some (GenState st_s ids))
runX86Generator k st (X86G m) = runReaderT (runContT m (ReaderT . k)) st

-- | Capture the current continuation and 'GenState' in an 'X86Generator'
shiftX86GCont :: (X86GCont st_s ids a
                  -> GenState st_s ids 'True
                  -> ExceptT Text (ST st_s) (Some (GenState st_s ids)))
              -> X86Generator st_s ids a
shiftX86GCont f =
  X86G $ ContT $ \k -> ReaderT $ \s -> f (runReaderT . k) s

modGenState :: State (GenState st_s ids 'True) a -> X86Generator st_s ids a
modGenState m = X86G $ ContT $ \c -> ReaderT $ \s -> uncurry (runReaderT . c) (runState m s)

modState :: State (RegState X86Reg (Value X86_64 ids)) a -> X86Generator st_s ids a
modState m = modGenState $ do
  s <- use curX86State
  let (r,s') = runState m s
  curX86State .= s'
  return r

-- | Create a new assignment identifier
newAssignID :: X86Generator st_s ids (AssignId ids tp)
newAssignID = do
  gs <- X86G $ ask
  liftM AssignId $ X86G $ lift $ lift $ lift $ freshNonce $ assignIdGen gs

addStmt :: Stmt X86_64 ids -> X86Generator st_s ids ()
addStmt stmt = seq stmt $
  modGenState $ blockState . _JustF . pBlockStmts %= (Seq.|> stmt)


addAssignment :: AssignRhs X86_64 ids tp
              -> X86Generator st_s ids (Assignment X86_64 ids tp)
addAssignment rhs = do
  l <- newAssignID
  let a = Assignment l rhs
  addStmt $ AssignStmt a
  return a

addArchFn :: ArchFn X86_64 ids tp
          -> X86Generator st_s ids (Assignment X86_64 ids tp)
addArchFn fn = addAssignment (EvalArchFn fn (typeRepr fn))

-- | This function does a top-level constant propagation/constant reduction.
-- We assume that the leaf nodes have also been propagated (i.e., we only operate
-- at the outermost term)

-- FIXME: make less ad-hoc
constPropagate :: forall ids tp. App (Value X86_64 ids) tp -> Maybe (Value X86_64 ids tp)
constPropagate v =
  case v of
   BVAnd _ l r
     | Just _ <- testEquality l r -> Just l
   BVAnd sz l r                   -> binop (.&.) sz l r
   -- Units
   BVAdd _  l (BVValue _ 0)       -> Just l
   BVAdd _  (BVValue _ 0) r       -> Just r
   BVAdd sz l r                   -> binop (+) sz l r
   BVMul _  l (BVValue _ 1)       -> Just l
   BVMul _  (BVValue _ 1) r       -> Just r

   UExt  (BVValue _ n) sz         -> Just $ mkLit sz n

   -- Word operations
   Trunc (BVValue _ x) sz         -> Just $ mkLit sz x

   -- Boolean operations
   BVUnsignedLt l r               -> boolop (<) l r
   BVEq l r                       -> boolop (==) l r
   BVComplement sz x              -> unop complement sz x
   _                              -> Nothing
  where
    boolop :: (Integer -> Integer -> Bool)
           -> Value X86_64 ids utp -> Value X86_64 ids utp -> Maybe (Value X86_64 ids BoolType)
    boolop f (BVValue _ l) (BVValue _ r) = Just $ mkLit knownNat (if f l r then 1 else 0)
    boolop _ _ _ = Nothing

    unop :: (tp ~ BVType n)
         => (Integer -> Integer)
         -> NatRepr n -> Value X86_64 ids tp -> Maybe (Value X86_64 ids tp)
    unop f sz (BVValue _ l)  = Just $ mkLit sz (f l)
    unop _ _ _               = Nothing

    binop :: (tp ~ BVType n) => (Integer -> Integer -> Integer)
          -> NatRepr n
          -> Value X86_64 ids tp
          -> Value X86_64 ids tp
          -> Maybe (Value X86_64 ids tp)
    binop f sz (BVValue _ l) (BVValue _ r) = Just $ mkLit sz (f l r)
    binop _ _ _ _                          = Nothing

evalApp :: App (Value X86_64 ids) tp -> X86Generator st_s ids (Value X86_64 ids tp)
evalApp a = do
  case constPropagate a of
    Nothing -> do
      m <- modGenState $ use (blockState . _JustF . pBlockApps)
      case MapF.lookup a m of
        Nothing -> do
          r <- addAssignment (EvalApp a)
          modGenState $ (blockState . _JustF . pBlockApps) %= MapF.insert a r
          return (AssignedValue r)
        Just r -> return (AssignedValue r)
    Just v  -> return v

eval :: Expr ids tp -> X86Generator st_s ids (Value X86_64 ids tp)
eval (ValueExpr v) = return v
eval (AppExpr a) = evalApp =<< traverseApp eval a

-- | Type for addresses.
type AddrExpr ids = Expr ids (BVType 64)

------------------------------------------------------------------------
-- Location

type ImpLocation ids tp = S.Location (AddrExpr ids) tp

getX87Top :: X86Generator st_s ids Int
getX87Top = do
  top_val <- modState $ use $ x87TopReg
  case top_val of
    -- Validate that i is less than top and top +
    BVValue _ (fromInteger -> topv) ->
      return topv
    _ -> fail $ "Unsupported value for top register " ++ show (pretty top_val)

getX87Offset :: Int -> X86Generator st_s ids Int
getX87Offset i = do
  topv <- getX87Top
  unless (0 <= topv + i && topv + i <= 7) $ do
    fail $ "Illegal floating point index"
  return $! topv + i

readLoc :: X86PrimLoc tp -> X86Generator st_s ids (Expr ids tp)
readLoc l = ValueExpr . AssignedValue <$> addArchFn (ReadLoc l)

getLoc :: ImpLocation ids tp -> X86Generator st_s ids (Expr ids tp)
getLoc (l0 :: ImpLocation ids tp) =
  case l0 of
    S.MemoryAddr w tp -> do
      addr <- eval w
      ValueExpr . AssignedValue <$> addAssignment (ReadMem addr tp)
    S.Register (rv :: S.RegisterView cl b n) -> do
      let readLoc' :: forall st_s'. X86PrimLoc (N.RegisterType cl) ->
                      X86Generator st_s' ids (Expr ids tp)
          readLoc' l = S.registerViewRead rv <$> readLoc l
      let r_nm = S.registerViewReg rv
      case r_nm of
       -- N.ControlReg {} -> addStmt $ Val (ControlLoc r) v
       -- N.DebugReg {}   -> addStmt $ Write (DebugLoc r)   v
       N.SegmentReg {}
         | r_nm == N.fs -> readLoc' FS
         | r_nm == N.gs -> readLoc' GS
         -- Otherwise registers are 0.
         | otherwise ->
           fail $ "On x86-64 registers other than fs and gs may not be read."
       N.X87PC -> readLoc' X87_PC
       N.X87RC -> readLoc' X87_RC
       _ -> do
         let Just r = x86Reg r_nm
         modState $
            S.registerViewRead rv . ValueExpr <$> use (boundValue r)
    -- TODO
    S.X87StackRegister i -> do
      idx <- getX87Offset i
      e <- modState $ use $ boundValue (X87_FPUReg idx)
      -- TODO: Check tag register is assigned.
      return $! ValueExpr e

lowerHalf :: forall ids n . (1 <= n) => Expr ids (BVType (n+n)) -> Expr ids (BVType n)
lowerHalf e =
     -- Workaround for GHC typechecker
     withLeqProof (addIsLeq half_width half_width) $ do
       S.bvTrunc half_width e
  where half_width :: NatRepr n
        half_width = halfNat (exprWidth e)

n64 :: NatRepr 64
n64 = knownNat

-- | Get the upper half of a bitvector.
upperHalf :: forall ids n . (1 <= n) => Expr ids (BVType (n+n)) -> Expr ids (BVType n)
-- Handle concrete values
upperHalf (ValueExpr (BVValue w i)) = h
   where half_width = halfNat w
         h = bvLit half_width (i `shiftR` widthVal half_width)
upperHalf e =
   case asApp e of
      -- Handle expression concatenation.
      -- N.B. We use unsafe coerce due to GHC failing to match the (n+n) in upperHalf
      -- to the (n+n) bound in ConcatV.
      Just (ConcatV lw lw' _ h)
        | Just Refl <- testEquality lw lw' -> do
        case testEquality half_width (exprWidth h) of
          Just Refl -> h
          Nothing -> error "upper half given illegal widths."
      -- Introduce split operations
      _ ->
        -- Workaround for GHC typechecker
        withLeqProof (addIsLeq half_width half_width) $
          app (UpperHalf half_width e)
  where half_width :: NatRepr n
        half_width = halfNat (exprWidth e)

-- | Assign a value to a location
setLoc :: forall ids st_s tp. ImpLocation ids tp -> Value X86_64 ids tp ->
          X86Generator st_s ids ()
setLoc (loc :: ImpLocation ids tp) v =
  case loc of
   S.MemoryAddr w _ -> do
     addr <- eval w
     addStmt $ WriteMem addr v

   S.Register (rv :: S.RegisterView cl b n) -> do
     let writeReg :: forall st_s' . X86PrimLoc (N.RegisterType cl) ->
                     X86Generator st_s' ids ()
         writeReg reg = do
           v0 <- readLoc reg
           v1 <- eval $ S.registerViewWrite rv v0 (ValueExpr v)
           addStmt $ ExecArchStmt $ WriteLoc reg v1
     let r_nm = S.registerViewReg rv
     case r_nm of
       N.ControlReg {} -> writeReg (ControlLoc r_nm)
       N.DebugReg {}   -> writeReg (DebugLoc r_nm)
       N.SegmentReg {}
         | r_nm == N.fs -> writeReg FS
         | r_nm == N.gs -> writeReg GS
         -- Otherwise registers are 0.
         | otherwise ->
             fail $ "On x86-64 registers other than fs and gs may not be set."
       N.X87PC -> writeReg X87_PC
       N.X87RC -> writeReg X87_RC
       _ -> do
         let Just r = x86Reg r_nm
         v0 <- modState $ ValueExpr <$> use (boundValue r)
         v1 <- eval $ S.registerViewWrite rv v0 (ValueExpr v)
         modState $ boundValue r .= v1
   S.X87StackRegister i -> do
     off <- getX87Offset i
     modState $ boundValue (X87_FPUReg off) .= v

mkBlockLabel :: SegmentedAddr 64
             -> GenState st_s ids any
             -> (BlockLabel 64, GenState st_s ids any)
mkBlockLabel a s0 = (lbl, s1)
  where (b_id, s1) = s0 & nextBlockID <<+~ 1
        lbl = GeneratedBlock a b_id

-- | Helper function for 'S.Semantics' instance below.
bvBinOp :: (NatRepr n
            -> Value X86_64 ids tp
            -> BVValue X86_64 ids n
            -> App (Value X86_64 ids) tp1)
        -> Expr ids tp
        -> Expr ids (BVType n)
        -> X86Generator st_s ids (Expr ids tp1)
bvBinOp op' x y = do
  let w = exprWidth y
  xv <- eval x
  yv <- eval y
  zv <- evalApp (op' w xv yv)
  return $ ValueExpr zv


instance S.Semantics (X86Generator st_s ids) where
  make_undefined (S.BVTypeRepr n) =
    ValueExpr . AssignedValue <$> addAssignment (SetUndefined n)

  -- Get value of a location.
  get = getLoc

  l .= e = setLoc l =<< eval e

  -- Implement ifte_
  -- Note that this implementation will run any code appearing after the ifte_
  -- twice, once for the true branch and once for the false branch.
  --
  -- This could be changed to run the code afterwards once, but the cost would be
  -- defining a way to merge processor states from the different branches, and making
  -- sure that expression assignments generated in one branch were not referred to in
  -- another branch.
  --
  -- One potential design change, not implemented here, would be to run both branches,
  -- up to the point where they merge, and if the resulting PC is in the same location,
  -- to merge in that case, otherwise to run them separately.
  --
  -- This would support the cmov instruction, but result in divergence for branches, which
  -- I think is what we want.
  ifte_ c_expr t f = eval c_expr >>= go
    where
      go (BVValue _ 1) = t
      go (BVValue _ 0) = f
      go cond =
        shiftX86GCont $ \c s0 -> do
          let p_b = s0 ^. (blockState . _JustF)
          let st = p_b^.pBlockState
          let a  = labelAddr (pBlockLabel p_b)
          let (t_block_label, s1) = mkBlockLabel a s0
          let s2 = s1 & blockState .~ JustF (emptyPreBlock st t_block_label)
                      & frontierBlocks .~ Seq.empty
          -- Run true block.
          Some (finishBlock FetchAndExecute -> s3) <- runX86Generator c s2 t
          -- Run false block
          let (f_block_label, s4) = mkBlockLabel a s3
          let s5 = s4 & blockState .~ JustF (emptyPreBlock st f_block_label)
                      & frontierBlocks .~ Seq.empty
          Some (finishBlock FetchAndExecute -> s6) <- runX86Generator c s5 f

          -- Join results together.
          let fin_b = finishBlock' p_b (\_ -> Branch cond t_block_label f_block_label)
          seq fin_b $ do
          return $ Some $
            s6 & frontierBlocks .~ (s0^.frontierBlocks Seq.|> fin_b)
                                   Seq.>< s3^.frontierBlocks
                                   Seq.>< s6^.frontierBlocks
               & blockState .~ NothingF

  memcopy val_sz count src dest is_reverse = do
    count_v <- eval count
    src_v   <- eval src
    dest_v  <- eval dest
    is_reverse_v <- eval is_reverse
    addStmt $ ExecArchStmt $ MemCopy val_sz count_v src_v dest_v is_reverse_v

  memcmp sz count src dest is_reverse = do
    count_v <- eval count
    is_reverse_v <- eval is_reverse
    src_v   <- eval src
    dest_v  <- eval dest
    ValueExpr . AssignedValue
      <$> addArchFn (MemCmp sz count_v src_v dest_v is_reverse_v)

  memset count val dest df = do
    count_v <- eval count
    val_v   <- eval val
    dest_v  <- eval dest
    df_v    <- eval df
    addStmt $ ExecArchStmt $ MemSet count_v val_v dest_v df_v

  rep_scas True is_reverse sz val buf count = do
    val_v   <- eval val
    buf_v   <- eval buf
    count_v <- eval count
    is_reverse_v <- eval is_reverse
    case is_reverse_v of
      BVValue _ 0 -> do
        ValueExpr . AssignedValue <$> addArchFn (RepnzScas sz val_v buf_v count_v)
      _ -> do
        fail $ "Unsupported rep_scas value " ++ show is_reverse_v
  rep_scas False _is_reverse _sz _val _buf _count = do
    fail $ "Semantics only currently supports finding elements."

  primitive S.Syscall = do
    shiftX86GCont $ \_ s0 -> do
      -- Get last block.
      let p_b = s0 ^. (blockState . _JustF)
      -- Create finished block.
      let fin_b = finishBlock' p_b Syscall
      seq fin_b $ do
      -- Return early
      return $ Some $ s0 & frontierBlocks %~ (Seq.|> fin_b)
                         & blockState .~ NothingF

  primitive S.CPUID = do
    rax_val <- modState $ use $ boundValue rax_reg
    let n32 = knownNat :: NatRepr 32
    eax_val <- eval (S.bvTrunc' n32  (ValueExpr rax_val))
    -- Call CPUID and get a 128-bit value back.
    res <- ValueExpr . AssignedValue <$> addArchFn (CPUID eax_val)
    S.reg_low32 N.rax S..= lowerHalf (lowerHalf res)
    S.reg_low32 N.rbx S..= upperHalf (lowerHalf res)
    S.reg_low32 N.rcx S..= lowerHalf (upperHalf res)
    S.reg_low32 N.rdx S..= upperHalf (upperHalf res)

  primitive S.RDTSC = do
    res <- ValueExpr . AssignedValue <$> addArchFn RDTSC
    S.reg_low32 N.rdx S..= upperHalf res
    S.reg_low32 N.rax S..= lowerHalf res
  primitive S.XGetBV = do
    rcx_val <- modState $ use $ boundValue rcx_reg
    let n32 = knownNat :: NatRepr 32
    ecx_val <- eval (S.bvTrunc' n32  (ValueExpr rcx_val))
    res <- ValueExpr . AssignedValue <$> addArchFn (XGetBV ecx_val)
    S.reg_low32 N.rdx S..= upperHalf res
    S.reg_low32 N.rax S..= lowerHalf res

  pshufb w x y = do
    x_val <- eval x
    y_val <- eval y
    ValueExpr . AssignedValue <$> addArchFn (PShufb w x_val y_val)

  getSegmentBase seg =
    case seg of
      Flexdis.FS -> ValueExpr . AssignedValue <$> addArchFn ReadFSBase
      Flexdis.GS -> ValueExpr . AssignedValue <$> addArchFn ReadGSBase
      _ ->
        error $ "Reopt.CFG.Implementation.getSegmentBase " ++ show seg ++ ": unimplemented!"

  bvQuot       = bvBinOp BVQuot
  bvSignedQuot = bvBinOp BVSignedQuot
  bvRem        = bvBinOp BVRem
  bvSignedRem  = bvBinOp BVSignedRem

  -- exception :: Value m BoolType    -- mask
  --            -> Value m BoolType -- predicate
  --            -> ExceptionClass
  --            -> m ()
  exception m p c = do
    S.ifte_ (S.complement m S..&. p)
            (addStmt (PlaceHolderStmt [] $ "Exception " ++ (show c)))
            (return ())

  x87Push e = do
    v <- eval e
    topv <- getX87Top
    let new_top = (topv - 1) .&. 0x7
    modState $ do
      -- TODO: Update tagWords
      -- Store value at new top
      boundValue (X87_FPUReg new_top) .= v
      -- Update top
      x87TopReg .= BVValue knownNat (toInteger new_top)
  x87Pop = do
    topv <- getX87Top
    let new_top = (topv + 1) .&. 0x7
    modState $ do
      -- Update top
      x87TopReg .= BVValue knownNat (toInteger new_top)

    return ()

initGenState :: NonceGenerator (ST st_s) ids
             -> SegmentedAddr 64
             -> RegState X86Reg (Value X86_64 ids)
             -> GenState st_s ids 'True
initGenState nonce_gen ip s
    = finishBlock FetchAndExecute st
    & blockState .~ JustF (emptyPreBlock s lbl)
  where lbl = GeneratedBlock ip 0
        st = emptyGenState nonce_gen

-- | Describes the reason the translation error occured.
data X86TranslateErrorReason
   = DecodeError (MemoryError 64)
     -- ^ A memory error occured in decoding with Flexdis
   | UnsupportedInstruction Flexdis.InstructionInstance
     -- ^ The instruction is not supported by the translator
   | ExecInstructionError Flexdis.InstructionInstance Text
     -- ^ An error occured when trying to translate the instruction

-- | Describes an error that occured in translation
data X86TranslateError = X86TranslateError { transErrorAddr :: !(SegmentedAddr 64)
                                           , transErrorReason :: !X86TranslateErrorReason
                                           }

instance Show X86TranslateError where
  show err =
      case transErrorReason err of
        DecodeError me ->
          "Memory error at " ++ addr ++ ": " ++ show me
        UnsupportedInstruction i ->
          "Unsupported instruction at " ++ addr ++ ": " ++ show i
        ExecInstructionError i msg ->
          "Error in interpretting instruction at " ++ addr ++ ": " ++ show i ++ "\n  "
          ++ Text.unpack msg
    where addr = show (transErrorAddr err)

-- | Disassemble block, returning list of blocks read so far, ending PC, and an optional error.
-- and ending PC.
disassembleBlockImpl :: forall st_s ids
                     .  Memory 64
                     -- ^ Memory to use for disassembling block
                     -> GenState st_s ids 'True
                     -- ^ State information for disassembling.
                     -> (SegmentedAddr 64 -> Bool)
                     -- ^ This function should return true if
                     -- we should keep disassembling when we step
                     -- to the given address with the functions.
                     -> SegmentedAddr 64
                     -- ^ Address to disassemble from.
                     -> ST st_s (GenState st_s ids 'False, SegmentedAddr 64, Maybe X86TranslateError)
disassembleBlockImpl mem gs contFn addr = do
  let returnWithError rsn =
        let err = X86TranslateError addr rsn
            gs3 = finishBlock (\s -> TranslateError s (Text.pack (show err))) gs
         in seq gs3 $ return (gs3, addr, Just err)
  case readInstruction mem addr of
    Left msg -> do
      returnWithError (DecodeError msg)

    Right (i, next_ip) -> do
      let next_ip_val :: BVValue X86_64 ids 64
          next_ip_val = RelocatableValue n64 (addrValue next_ip)

      let next_ip_word = fromIntegral (addrValue next_ip)

      case execInstruction (ValueExpr next_ip_val) i of
        Nothing -> do
          returnWithError (UnsupportedInstruction i)
        Just exec -> do
          gsr <-
            runExceptT $ runX86Generator (\() s -> return (Some s)) gs $ do
                let line = show addr ++ ": " ++ show (Flexdis.ppInstruction next_ip_word i)
                addStmt (Comment (Text.pack line))
                exec
          case gsr of
            Left msg -> do
              returnWithError (ExecInstructionError i msg)
            Right (Some gs2) -> do
              case gs2 ^. blockState of
                -- If next ip is exactly the next_ip_val then keep running.
                JustF p_b | Seq.null (gs2^.frontierBlocks)
                          , v <- p_b^.(pBlockState . curIP)
                          , v == next_ip_val
                            -- Check to see if we should continue
                          , contFn next_ip ->
                  disassembleBlockImpl mem gs2 contFn next_ip
                _ -> do
                  let gs3 = finishBlock FetchAndExecute gs2
                  seq gs3 $ return (gs3, next_ip, Nothing)

-- | Disassemble block, returning either an error, or a list of blocks
-- and ending PC.
disassembleBlock :: forall s
                 .  NonceGenerator (ST s) s
                 -> Memory 64
                 -> (SegmentedAddr 64 -> Bool)
                    -- ^ Predicate that tells when to continue.
                 -> ExploreLoc
                 -> ST s ([Block X86_64 s], SegmentedAddr 64, Maybe X86TranslateError)
disassembleBlock nonce_gen mem contFn loc = do
  let gs = initGenState nonce_gen (loc_ip loc) (initX86State loc)
  r <- disassembleBlockImpl mem gs contFn (loc_ip loc)
  case r of
    (gs', ip, maybeError) -> pure $ (Fold.toList (gs'^.frontierBlocks), ip, maybeError)

-- | The abstract state for a function begining at a given address.
fnBlockState :: SegmentedAddr 64 -> AbsBlockState X86Reg
fnBlockState addr =
  top & setAbsIP addr
      & absRegState . boundValue sp_reg .~ concreteStackOffset addr 0
        -- x87 top register points to top of stack.
      & absRegState . x87TopReg         .~ FinSet (Set.singleton 7)
        -- Direction flag is initially zero.
      & absRegState . boundValue df_reg .~ FinSet (Set.singleton 0)
      & startAbsStack .~ Map.singleton 0 (StackEntry (S.BVTypeRepr n64) ReturnAddr)

preserveFreeBSDSyscallReg :: X86Reg tp -> Bool
preserveFreeBSDSyscallReg r
  | Just Refl <- testEquality r cf_reg  = False
  | Just Refl <- testEquality r rax_reg = False
  | otherwise = True

-- | Linux preserves the same registers the x86_64 ABI does
linuxSystemCallPreservedRegisters :: Set.Set (Some X86Reg)
linuxSystemCallPreservedRegisters = x86CalleeSavedRegs


-- | Transfer some type into an abstract value given a processor state.
transferAbsValue :: AbsProcessorState X86Reg ids
                 -> X86PrimFn ids tp
                 -> AbsValue 64 tp
transferAbsValue r f =
  case f of
    ReadLoc _  -> TopV
    ReadFSBase -> TopV
    ReadGSBase -> TopV
    CPUID _    -> TopV
    RDTSC      -> TopV
    XGetBV _   -> TopV
    PShufb{}   -> TopV
      -- We know only that it will return up to (and including(?)) cnt
    MemCmp _sz cnt _src _dest _rev
      | Just upper <- hasMaximum knownType (transferValue r cnt) ->
          stridedInterval $ SI.mkStridedInterval knownNat False 0 upper 1
      | otherwise -> TopV
    RepnzScas _sz _val _buf cnt
      | Just upper <- hasMaximum knownType (transferValue r cnt) ->
          stridedInterval $ SI.mkStridedInterval knownNat False 0 upper 1
      | otherwise -> TopV

-- | Disassemble block, returning either an error, or a list of blocks
-- and ending PC.
disassembleBlockFromAbsState :: forall s
                 .  NonceGenerator (ST s) s
                 -> Memory 64
                    -- ^ Memory to use for disassembling
                 -> (SegmentedAddr 64 -> Bool)
                    -- ^ Predicate that tells when to continue.
                 -> SegmentedAddr 64
                    -- ^ Address to disassemble at
                 -> AbsBlockState X86Reg
                    -- ^ Abstract state of processor for defining state.
                 -> ST s ([Block X86_64 s], SegmentedAddr 64, Maybe String)
disassembleBlockFromAbsState nonce_gen mem contFn addr ab =
  -- Parse the x87 state
  case asConcreteSingleton (ab^.absRegState^.x87TopReg) of
    Nothing -> pure $ ([], addr, Just "Could not determine height of X87 stack.")
    Just t -> do
      case asConcreteSingleton (ab^.absRegState^.boundValue df_reg) of
        Nothing -> pure $ ([], addr, Just $ "Could not determine df flag " ++ show (ab^.absRegState^.boundValue df_reg))
        Just d -> do
          let loc = ExploreLoc { loc_ip = addr
                               , loc_x87_top = fromInteger t
                               , loc_df_flag = d /= 0
                               }
          (blocks, addr', maybeError) <- disassembleBlock nonce_gen mem contFn loc
          pure $! (blocks, addr', show <$> maybeError)

freeBSD_syscallPersonality :: SyscallPersonality X86_64
freeBSD_syscallPersonality =
  SyscallPersonality { spTypeInfo = FreeBSD.syscallInfo
                     , spResultRegisters = [ Some rax_reg, Some cf_reg ]
                     }

-- | Architecture information for X86_64.
x86_64_freeBSD_info :: ArchitectureInfo X86_64
x86_64_freeBSD_info =
  ArchitectureInfo { archAddrWidth      = Addr64
                   , jumpTableEntrySize = 8
                   , disassembleFn      = disassembleBlockFromAbsState
                   , fnBlockStateFn     = \_ -> fnBlockState
                   , preserveRegAcrossCall    = \r -> Set.member (Some r) x86CalleeSavedRegs
                   , preserveRegAcrossSyscall = preserveFreeBSDSyscallReg
                   , callStackDelta     = x86StackDelta
                   , absEvalArchFn      = transferAbsValue
                   }

linux_syscallPersonality :: SyscallPersonality X86_64
linux_syscallPersonality =
  SyscallPersonality { spTypeInfo = Linux.syscallInfo
                     , spResultRegisters = [Some rax_reg]
                     }

-- | Architecture information for X86_64.
x86_64_linux_info :: ArchitectureInfo X86_64
x86_64_linux_info =
  ArchitectureInfo { archAddrWidth      = Addr64
                   , jumpTableEntrySize = 8
                   , disassembleFn      = disassembleBlockFromAbsState
                   , fnBlockStateFn     = \_ -> fnBlockState
                   , preserveRegAcrossCall    = \r -> Set.member (Some r) x86CalleeSavedRegs
                   , preserveRegAcrossSyscall = \r -> Set.member (Some r) linuxSystemCallPreservedRegisters
                   , callStackDelta     = x86StackDelta
                   , absEvalArchFn      = transferAbsValue
                   }
