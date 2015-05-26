------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.Implementation
-- Description      : Control Flow Graph extraction definitions
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This contains an implementation of the classes defined in Reopt.Semantics.Monad
-- that extract a control flow graph from a program using the semantics.
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
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

module Reopt.Semantics.Implementation
       ( -- Threaded global state
         GlobalGenState
       , emptyGlobalGenState
         -- The main disassemble function
       , disassembleBlock
       , ExploreLoc(..)
       , rootLoc
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
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import           Reopt.Semantics.Representation
import qualified Reopt.Semantics.StateNames as N
import           Reopt.Semantics.Types (TypeBits)

------------------------------------------------------------------------
-- Expr

-- | A pure expression for isValue.
data Expr tp where
  -- An expression obtained from some value.
  ValueExpr :: !(Value tp) -> Expr tp
  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(App Expr tp) -> Expr tp

instance Eq (Expr tp) where
  (==) = \x y -> isJust (testEquality x y)

instance TestEquality Expr where
  testEquality (ValueExpr x) (ValueExpr y) = do
    Refl <- testEquality x y
    return Refl
  testEquality (AppExpr x) (AppExpr y) = do
    Refl <- testEquality x y
    return Refl
  testEquality _ _ = Nothing

asApp :: Expr tp -> Maybe (App Expr tp)
asApp (AppExpr   a) = Just a
asApp (ValueExpr v) = mapApp ValueExpr <$> valueAsApp v

app :: App Expr tp -> Expr tp
app = AppExpr

exprType :: Expr tp -> S.TypeRepr tp
exprType (ValueExpr v) = valueType v
exprType (AppExpr a) = appType a

-- | Return width of expression.
exprWidth :: Expr (BVType n) -> NatRepr n
exprWidth e =
  case exprType e of
    S.BVTypeRepr n -> n

asBVLit :: Expr tp -> Maybe Integer
asBVLit (ValueExpr (BVValue _ v)) = Just v
asBVLit _ = Nothing

ltProof :: forall f n m . (n+1 <= m) => f n -> f m -> LeqProof n m
ltProof _ _ = leqTrans lt LeqProof
  where lt :: LeqProof n (n+1)
        lt = leqAdd LeqProof S.n1

instance S.IsValue Expr where

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

  bvDiv       x y = app $ BVDiv       (exprWidth x) x y
  bvSignedDiv x y = app $ BVSignedDiv (exprWidth x) x y
  bvMod       x y = app $ BVMod       (exprWidth x) x y
  bvSignedMod x y = app $ BVSignedMod (exprWidth x) x y

  complement x
    | Just xv <- asBVLit x = bvLit (exprWidth x) (complement xv)
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
      -- Default case
    | otherwise = app $ BVAnd (exprWidth x) x y

  x .|. y
    | Just xv <- asBVLit x, Just yv <- asBVLit y =
      bvLit (exprWidth x) (xv .|. yv)
      -- Cancel or when one argument is maxUnsigned
    | Just xv <- asBVLit x, xv == maxUnsigned (exprWidth x) = x
    | Just yv <- asBVLit y, yv == maxUnsigned (exprWidth x) = y
      -- Eliminate or when one argument is 0
    | Just 0 <- asBVLit x = y
    | Just 0 <- asBVLit y = x
      -- Idempotence
    | x == y = x
      -- Default case
    | otherwise = app $ BVOr (exprWidth x) x y

  bvXor x y
      -- Eliminate xor with 0.
    | Just 0 <- asBVLit x = y
    | Just 0 <- asBVLit y = x
      -- Eliminate xor with self.
    | x == y = bvLit (exprWidth x) (0::Integer)
      -- Default case.
    | otherwise = app $ BVXor (exprWidth x) x y

  x .=. y
    | Just xv <- asBVLit x, Just yv <- asBVLit y = S.boolValue (xv == yv)
    | x == y = S.true
    | otherwise = app $ BVEq x y

  -- | Splits a bit vectors into two
  -- bvSplit :: v (BVType (n + n)) -> (v (BVType n), v (BVType n))
  bvSplit v = (upperHalf v, lowerHalf v)

  -- | Shifts, the semantics is undefined for shifts >= the width of the first argument
  -- bvShr, bvSar, bvShl :: v (BVType n) -> v (BVType log_n) -> v (BVType n)
  bvShr x y
    | Just 0 <- asBVLit y = x
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

    | otherwise = app $ BVShl (exprWidth x) x y

  bvTrunc w e0
      -- Constant propagation
    | Just v <- asBVLit e0 = bvLit w v
      -- Eliminate redundant trunc
    | Just Refl <- testEquality (exprWidth e0) w =
      e0
      -- Eliminate MMXExtend
    | Just (MMXExtend e) <- asApp e0
    , Just Refl <- testEquality w n64 =
      e

    | Just (ConcatV lw l _) <- asApp e0
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

      -- Simplify truncation.
    | Just (Trunc e _) <- asApp e0 =
      -- Runtime check to workaround GHC typechecker.
      case testLeq w (exprWidth e) of
        Just LeqProof -> S.bvTrunc w e
        Nothing -> error "bvTrunc given bad width"

    | otherwise =
      case testStrictLeq w (exprWidth e0) of
        Left LeqProof -> app (Trunc e0 w)
        Right Refl -> e0

  bvUlt x y
    | Just xv <- asBVLit x, Just yv <- asBVLit y = S.boolValue (xv < yv)
    | x == y = S.false
    | otherwise = app $ BVUnsignedLt x y

  bvSlt x y
    | Just xv <- asBVLit x, Just yv <- asBVLit y = S.boolValue (xv < yv)
    | x == y = S.false
    | otherwise = app $ BVUnsignedLt x y

  bvBit x y
    | Just xv <- asBVLit x
    , Just yv <- asBVLit y =
      S.boolValue (xv `testBit` fromInteger yv)
    | Just (Trunc xe w) <- asApp x
    , Just yv <- asBVLit y = assert (0 <= yv && yv < natValue w) $
      S.bvBit xe y

    | Just (ConcatV lw x_low x_high) <- asApp x
    , Just yv <- asBVLit y = assert (0 <= yv && yv < 2*natValue lw) $
      if yv >= natValue lw then
        S.bvBit x_high (S.bvLit (exprWidth y) (yv - natValue lw))
      else
        S.bvBit x_low y

    | otherwise =
      app $ BVBit x y

  sext w e0
    | Just (SExt e w0) <- asApp e0 = do
      -- Runtime check to wordaround GHC typechecker
      let we = S.bv_width e
      withLeqProof (leqTrans (addIsLeq we (knownNat :: NatRepr 1))
                             (leqTrans (leqProof (incNat we) w0) (leqProof w0 w))) $
        S.sext w e
    | otherwise =
      case testStrictLeq (exprWidth e0) w of
        Left LeqProof -> app (SExt e0 w)
        Right Refl -> e0

  uext' w e0
    | Just v <- asBVLit e0 =
      let w0 = S.bv_width e0
       in withLeqProof (leqTrans (leqProof (knownNat :: NatRepr 1) w0) (ltProof w0 w)) $
            bvLit w v
      -- Collapse duplicate extensions.
    | Just (UExt e w0) <- asApp e0 = do
      let we = S.bv_width e
      withLeqProof (leqTrans (ltProof we w0) (ltProof w0 w)) $
        S.uext w e
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
    | otherwise = app $ UsbbOverflows (exprWidth x) x y c

  ssbb_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
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

  fpCvt src tgt x = app $ FPCvt src x tgt
  fpCvtRoundsUp src tgt x = app $ FPCvtRoundsUp src x tgt

  fpFromBV tgt x = app $ FPFromBV x tgt
  truncFPToSignedBV tgt src x = app $ TruncFPToSignedBV src x tgt

------------------------------------------------------------------------
-- GenState

-- | Global for the entire program.
data GlobalGenState = GlobalGenState
       { -- | Index of next assignment identifier to use.
         -- (all used assignment indices must be less than this).
         _nextAssignId :: !AssignId
       }

emptyGlobalGenState :: GlobalGenState
emptyGlobalGenState = GlobalGenState { _nextAssignId = 0
                                     }

-- | Number of assignments so far.
nextAssignId :: Simple Lens GlobalGenState AssignId
nextAssignId = lens _nextAssignId (\s v -> s { _nextAssignId = v })

-- | A block that we have not yet finished.
data PreBlock = PreBlock { pBlockLabel :: !BlockLabel
                         , _pBlockStmts :: !(Seq Stmt)
                         , _pBlockState :: !(X86State Value)
                         , _pBlockApps  :: !(MapF (App Value) Assignment)
                         }

pBlockStmts :: Simple Lens PreBlock (Seq Stmt)
pBlockStmts = lens _pBlockStmts (\s v -> s { _pBlockStmts = v })

pBlockState :: Simple Lens PreBlock (X86State Value)
pBlockState = lens _pBlockState (\s v -> s { _pBlockState = v })

pBlockApps  :: Simple Lens PreBlock (MapF (App Value) Assignment)
pBlockApps = lens _pBlockApps (\s v -> s { _pBlockApps = v })

-- | A tagged maybe
data MaybeF (t :: Bool) a where
  NothingF :: MaybeF 'False a
  JustF    :: a -> MaybeF 'True a

_JustF :: Lens (MaybeF 'True a) (MaybeF 'True b) a b
_JustF = lens (\(JustF v) -> v) (\_ v -> JustF v)

-- | Local to block discovery.
data GenState tag = GenState
       { -- | The global state
         _globalGenState :: !GlobalGenState
         -- | Index of next block
       , _nextBlockID  :: !Word64
         -- | Blocks added to CFG.
       , _frontierBlocks :: !(Seq Block)
       , _blockState     :: !(MaybeF tag PreBlock)
       }

globalGenState :: Simple Lens (GenState tag) GlobalGenState
globalGenState = lens _globalGenState (\s v -> s { _globalGenState = v })

-- | Control flow blocs generated so far.
nextBlockID :: Simple Lens (GenState tag) Word64
nextBlockID = lens _nextBlockID (\s v -> s { _nextBlockID = v })

-- | Blocks that are not in CFG that end with a FetchAndExecute,
-- which we need to analyze to compute new potential branch targets.
frontierBlocks :: Simple Lens (GenState tag) (Seq Block)
frontierBlocks = lens _frontierBlocks (\s v -> s { _frontierBlocks = v })

-- | Blocks that are not in CFG that end with a FetchAndExecute,
-- which we need to analyze to compute new potential branch targets.
blockState :: Lens (GenState a) (GenState b) (MaybeF a PreBlock) (MaybeF b PreBlock)
blockState = lens _blockState (\s v -> s { _blockState = v })

emptyPreBlock :: X86State Value
              -> BlockLabel
              -> PreBlock
emptyPreBlock s lbl =
  PreBlock { pBlockLabel = lbl
           , _pBlockStmts = Seq.empty
           , _pBlockApps  = MapF.empty
           , _pBlockState = s
           }

emptyGenState :: GlobalGenState -> GenState 'False
emptyGenState st =
  GenState { _globalGenState = st
           , _nextBlockID    = 1
           , _frontierBlocks = Seq.empty
           , _blockState     = NothingF
           }

curX86State :: Simple Lens (GenState 'True) (X86State Value)
curX86State = blockState . _JustF . pBlockState

-- | Finishes the current block, if it is started.
finishBlock' :: PreBlock
             -> (X86State Value -> TermStmt)
             -> Block
finishBlock' pre_b term =
  Block { blockLabel = pBlockLabel pre_b
        , blockStmts = Fold.toList (pre_b^.pBlockStmts)
        , blockCache = pre_b^.pBlockApps
        , blockTerm  = term (pre_b^.pBlockState)
        }

-- | Finishes the current block, if it is started.
finishBlock :: (X86State Value -> TermStmt)
            -> (GenState a -> GenState 'False)
finishBlock term st =
  case st^.blockState of
   NothingF    -> st
   JustF pre_b -> st & frontierBlocks %~ (Seq.|> b)
                     & blockState .~ NothingF
     where
       b = finishBlock' pre_b term

-- | Starts a new block.  If there is a current block it will finish
-- it with FetchAndExecute
startBlock :: X86State Value -> BlockLabel -> (GenState a -> GenState 'True)
startBlock s lbl st =
  finishBlock FetchAndExecute st & blockState .~ JustF (emptyPreBlock s lbl)

------------------------------------------------------------------------
-- X86Generator

-- | X86Generator is used to construct basic blocks from a stream of instructions
-- using the semantics.
--
-- It is implemented as a state monad in a continuation passing style so that
-- we can perform symbolic branches.
newtype X86Generator a = X86G { unX86G :: (a -> GenState 'True -> Some GenState)
                                       -> GenState 'True
                                       -> Some GenState
                              }

instance Functor X86Generator where
  fmap = liftM

instance Applicative X86Generator where
  pure = return
  (<*>) = ap

instance Monad X86Generator where
  return v = X86G $ \c -> c v
  m >>= h = X86G $ \c -> unX86G m (\mv -> unX86G (h mv) c)
  fail = error

type instance S.Value X86Generator = Expr

-- | Run X86Generator starting from the given state.
runX86Generator :: GenState 'True -> X86Generator () -> Some GenState
runX86Generator st m = unX86G m (\() -> Some) st

modGenState :: State (GenState 'True) a -> X86Generator a
modGenState m = X86G $ \c s -> uncurry c (runState m s)

modState :: State (X86State Value) a -> X86Generator a
modState m = modGenState $ do
  s <- use curX86State
  let (r,s') = runState m s
  curX86State .= s'
  return r

-- | Create a new identity for
newAssignId :: X86Generator AssignId
newAssignId = modGenState $ globalGenState . nextAssignId <<+= 1

addStmt :: Stmt -> X86Generator ()
addStmt stmt = modGenState $ blockState . _JustF . pBlockStmts %= (Seq.|> stmt)

addAssignment :: AssignRhs tp -> X86Generator (Assignment tp)
addAssignment rhs = do
  l <- newAssignId
  let a = Assignment l rhs
  addStmt $ AssignStmt a
  return a

-- | This function does a top-level constant propagation/constant reduction.
-- We assuem that the leaf nodes have also been propagated (i.e., we only operate
-- at the outermost term)

-- FIXME: make less ad-hoc
constPropagate :: forall tp. App Value tp -> Maybe (Value tp)
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
    boolop :: (tp ~ BoolType) => (Integer -> Integer -> Bool)
              -> Value n -> Value n -> Maybe (Value BoolType)
    boolop f (BVValue _ l) (BVValue _ r) = Just $ mkLit knownNat (if f l r then 1 else 0)
    boolop _ _ _ = Nothing

    unop :: (tp ~ BVType n) => (Integer -> Integer)
             -> NatRepr n -> Value tp -> Maybe (Value tp)
    unop f sz (BVValue _ l)  = Just $ mkLit sz (f l)
    unop _ _ _               = Nothing

    binop :: (tp ~ BVType n) => (Integer -> Integer -> Integer)
             -> NatRepr n -> Value tp -> Value tp -> Maybe (Value tp)
    binop f sz (BVValue _ l) (BVValue _ r) = Just $ mkLit sz (f l r)
    binop _ _ _ _                          = Nothing

evalApp :: App Value tp  -> X86Generator (Value tp)
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

eval :: Expr tp -> X86Generator (Value tp)
eval (ValueExpr v) = return v
eval (AppExpr a) = evalApp =<< traverseApp eval a

-- | Type for addresses.
type AddrExpr = Expr (BVType 64)

------------------------------------------------------------------------
-- Location

type ImpLocation tp = S.Location AddrExpr tp

getX87Offset :: Int -> X86Generator Int
getX87Offset i = do
  top_val <- modState $ use $ x87TopReg
  case top_val of
    BVValue _ (fromInteger -> top) | i <= top, top <= i + 7 -> do
      return (top - i)
    _ -> fail $ "Unsupported value for top register " ++ show (pretty top_val)

readLoc :: StmtLoc tp -> X86Generator (Expr tp)
readLoc l = ValueExpr . AssignedValue <$> addAssignment (Read l)

getLoc :: ImpLocation tp -> X86Generator (Expr tp)
getLoc l0 =
  case l0 of
    S.MemoryAddr w tp -> do
      addr <- eval w
      readLoc (MemLoc addr tp)
    S.Register r ->
      case r of
       -- N.ControlReg {} -> addStmt $ Val (ControlLoc r) v
       -- N.DebugReg {}   -> addStmt $ Write (DebugLoc r)   v
       N.SegmentReg {}
         | r == N.fs -> readLoc FS
         | r == N.gs -> readLoc GS
         -- Otherwise registers are 0.
         | otherwise ->
             fail $ "On x86-64 registers other than fs and gs may not be set."
       -- S.MMXReg {} -> do
       --   e <- modState $ ValueExpr <$> use (register r)
       --   ValueExpr <$> eval (S.bvTrunc knownNat e)
       N.X87PC ->  readLoc X87_PC
       N.X87RC ->  readLoc X87_RC
       _ -> modState $ ValueExpr <$> use (register r)

    S.LowerHalf l -> lowerHalf <$> getLoc l
    S.UpperHalf l -> upperHalf <$> getLoc l
    S.TruncLoc l w -> do
      withLeqProof (ltProof w (S.loc_width l)) $
        S.bvTrunc w <$> getLoc l

    -- TODO
    S.X87StackRegister i -> do
      v <- modState $ use $ x87Regs
      off <- getX87Offset i
      return (ValueExpr (v V.! off))


lowerHalf :: forall n . (1 <= n) => Expr (BVType (n+n)) -> Expr (BVType n)
lowerHalf e =
     -- Workaround for GHC typechecker
     withLeqProof (addIsLeq half_width half_width) $ do
       S.bvTrunc half_width e
  where half_width :: NatRepr n
        half_width = halfNat (exprWidth e)

n64 :: NatRepr 64
n64 = knownNat

-- | Get the upper half of a bitvector.
upperHalf :: forall n . (1 <= n) => Expr (BVType (n+n)) -> Expr (BVType n)
-- Handle concrete values
upperHalf (ValueExpr (BVValue w i)) = h
   where half_width = halfNat w
         h = bvLit half_width (i `shiftR` widthVal half_width)
upperHalf e =
   case asApp e of
      -- Handle expression concatenation.
      -- N.B. We use unsafe coerce due to GHC failing to match the (n+n) in upperHalf
      -- to the (n+n) bound in ConcatV.
      Just (ConcatV _ _ h) -> do
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


bvConcat :: (1 <= n) => Expr (BVType n) -> Expr (BVType n) -> Expr (BVType (n+n))
bvConcat l h
    | Just 0 <- asBVLit h =
        withLeqProof (addIsLeq w w) $ do
          S.uext (addNat w w) l
    | otherwise = app (ConcatV (exprWidth l) l h)
  where w = exprWidth l

-- | Assign a value to a location
setLoc :: forall tp. ImpLocation tp -> Value tp -> X86Generator ()
setLoc loc v0 =
  -- So x86 says that when you assign a 32 bit register, the upper bits
  -- are set to zero, which is different to the 16- and 8-bit cases.  This
  -- bit of special code checks when you are updating a 32bit register and
  -- zeroes accordingly.
  case loc of
   S.LowerHalf (S.Register r@(N.GPReg _))
     -> do -- hack to infer that n + n ~ 64 ==> n < 64
           (LeqProof :: LeqProof (TypeBits tp) 64) <- return (unsafeCoerce (LeqProof :: LeqProof 0 0)) -- return (addIsLeqLeft1 (LeqProof :: LeqProof (TypeBits tp + TypeBits tp) 64))
           zext_v <- eval $ S.uext n64 (ValueExpr v0)
           modState $ register r .= zext_v
   _ -> go loc v0
  where
    go :: forall tp'. ImpLocation tp' -> Value tp' -> X86Generator ()
    go l0 v =
      case l0 of
       S.MemoryAddr w _ -> do
         addr <- eval w
         addStmt $ Write (MemLoc addr (valueType v)) v

       S.Register r ->
         case r of
           N.ControlReg {} -> addStmt $ Write (ControlLoc r) v
           N.DebugReg {}   -> addStmt $ Write (DebugLoc r)   v
           N.SegmentReg {}
             | r == N.fs -> addStmt $ Write FS v
             | r == N.gs -> addStmt $ Write GS v
             -- Otherwise registers are 0.
             | otherwise ->
                 fail $ "On x86-64 registers other than fs and gs may not be set."
           N.X87PC -> addStmt $ Write X87_PC v
           N.X87RC -> addStmt $ Write X87_RC v
           -- FIXME: sort this out
           -- S.MMXReg {} -> do
           --   ext_v <- evalApp (MMXExtend v)
           --   modState $ register r .= ext_v
           _ -> modState $ register r .= v
       S.TruncLoc l w -> do
         b <- getLoc l
         let lw = S.loc_width l
         -- Build mask containing only upper most lw - w bits
         case isPosNat lw of
           Nothing -> fail "Illegal width to TruncLoc"
           Just LeqProof -> do
             let mask = bvLit lw (complement (maxUnsigned w))
             let old_part = mask S..&. b
             go l =<< eval (old_part S..|. S.uext' lw (ValueExpr v))
       S.LowerHalf l -> do
         b <- getLoc l
         go l =<< eval (bvConcat (ValueExpr v) (upperHalf b))
       S.UpperHalf l -> do
         b <- getLoc l
         go l =<< eval (bvConcat (lowerHalf b) (ValueExpr v))
       S.X87StackRegister i -> do
         off <- getX87Offset i
         modState $ x87Regs . ix off .= v


instance S.Semantics X86Generator where
  make_undefined (S.BVTypeRepr n) =
    ValueExpr . AssignedValue <$> addAssignment (SetUndefined n)

  -- Get value of a location.
  get l = getLoc l

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
        X86G $ \c s0 -> do
          let p_b = s0 ^. (blockState . _JustF)
          let last_block_id = pBlockLabel p_b
          let st = p_b^.pBlockState
          let (t_block_id, s1) = s0 & nextBlockID <<+~ 1
          let t_block_label = GeneratedBlock (blockParent last_block_id) t_block_id
          let s2 = s1 & blockState .~ JustF (emptyPreBlock st t_block_label)
                      & frontierBlocks .~ Seq.empty
          -- Run true block.
          case unX86G t c s2 of
            Some (finishBlock FetchAndExecute -> s3)
             | otherwise -> do
              -- Run false block
              let (f_block_id, s4) = s3 & nextBlockID <<+~ 1
              let f_block_label = GeneratedBlock (blockParent last_block_id) f_block_id
              let s5 = s4 & blockState .~ JustF (emptyPreBlock st f_block_label)
                          & frontierBlocks .~ Seq.empty

              case unX86G f c s5 of
                Some (finishBlock FetchAndExecute -> s6) -> do
                  -- Note that startBlock flushes the current block if required.
                  let fin_b = finishBlock' p_b (\_ -> Branch cond t_block_label f_block_label)
                  Some $ s6 & frontierBlocks .~ (s0^.frontierBlocks Seq.|> fin_b)
                                         Seq.>< s3^.frontierBlocks
                                         Seq.>< s6^.frontierBlocks
                            & blockState .~ NothingF

  memmove _n count src dest =
    do vs <- sequence [ Some <$> eval count
                      , Some <$> eval src
                      , Some <$> eval dest ]
       addStmt (PlaceHolderStmt vs "memmove")

  memset count v dest =
    do vs <- sequence [ Some <$> eval count
                      , Some <$> eval v
                      , Some <$> eval dest ]
       addStmt (PlaceHolderStmt vs "memset")

  syscall v = do v' <- eval v
                 addStmt (PlaceHolderStmt [Some v'] "Syscall")

  -- exception :: Value m BoolType    -- mask
  --            -> Value m BoolType -- predicate
  --            -> ExceptionClass
  --            -> m ()
  exception m p c = S.ifte_ (S.complement m S..&. p)
                    (addStmt (PlaceHolderStmt [] $ "Exception " ++ (show c)))
                    (return ())

  x87Push _ = return ()
  x87Pop = return ()

-- | A location to explore
data ExploreLoc
   = ExploreLoc { loc_ip :: CodeAddr
                  -- ^ IP address.
                , loc_x87_top :: Int
                  -- ^ Top of x86 address.
                }
 deriving (Eq, Ord)

instance Pretty ExploreLoc where
  pretty loc = text $ showHex (loc_ip loc) ""

rootLoc :: CodeAddr -> ExploreLoc
rootLoc ip = ExploreLoc { loc_ip = ip
                        , loc_x87_top = 7
                        }

initX86State :: ExploreLoc -- ^ Location to explore from.
             -> X86State Value
initX86State loc = mkX86State Initial
                   & curIP     .~ mkLit knownNat (toInteger (loc_ip loc))
                   & x87TopReg .~ mkLit knownNat (toInteger (loc_x87_top loc))

data GenError = DecodeError (MemoryError Word64)
              | DisassembleError Flexdis.InstructionInstance
                deriving Show

-- | Disassemble block, returning either an error, or a list of blocks
-- and ending PC.
disassembleBlock :: Memory Word64
                 -> (CodeAddr -> Bool)
                 -> ExploreLoc -- ^ Location to explore from.
                 -> StateT GlobalGenState (Either GenError)
                           ([Block], CodeAddr)
disassembleBlock mem contFn loc = do
  let lbl = GeneratedBlock (loc_ip loc) 0
  gs <- gets (startBlock (initX86State loc) lbl . emptyGenState)
  (gs', ip) <- lift $ disassembleBlock' mem gs contFn (loc_ip loc)
  put (gs' ^. globalGenState)
  return (Fold.toList (gs'^.frontierBlocks), ip)

{-
getExploreLocs :: X86State -> [ExploreLoc]
getExploreLocs s =
  case (s^.curIP, s^.x87TopReg) of
    (BVValue _ ip, BVValue _ top) -> [loc]
      where loc = ExploreLoc { loc_ip = fromInteger ip
                             , loc_x87_top = fromInteger top
                             }
    _ -> []

getFrontierNext :: [Block] -> Set ExploreLoc
getFrontierNext = Fold.foldl' f Set.empty
  where f locs b =
          case blockTerm b of
            FetchAndExecute s -> Fold.foldl' (flip Set.insert) locs (getExploreLocs s)
            _ -> locs
-}

-- | Disassemble block, returning either an error, or a list of blocks
-- and ending PC.
disassembleBlock' :: Memory Word64
                     -- ^ Memory to use for disassembling block
                  -> GenState 'True
                     -- ^ State information for disassembling.
                  -> (CodeAddr -> Bool)
                     -- ^ This function should return true if
                     -- we should keep disassembling when we step
                     -- to the given address with the functions.
                  -> CodeAddr
                     -- ^ Address to disassemble from.
                  -> Either GenError (GenState 'False, CodeAddr)
disassembleBlock' mem gs contFn addr = do
  (i, next_ip) <- readInstruction mem addr
                  & _Left %~ DecodeError
  let next_ip_val = mkLit knownNat (toInteger next_ip)

  -- Update current IP
  let gs1 = gs & curX86State . curIP .~ next_ip_val

  let line = text (showHex addr "") <> colon
             <+> Flexdis.ppInstruction next_ip i

  case execInstruction i of
    Nothing -> Left (DisassembleError i)
    Just exec -> do
      let res = runX86Generator gs1 $ do
                  addStmt (Comment (show line))
                  exec
      case res of
        Some gs2 -> do
          case gs2 ^. blockState of
            -- If next ip is exactly the next_ip_val then keep running.
            JustF p_b | Seq.null (gs2^.frontierBlocks)
                      , v <- p_b^.(pBlockState . curIP)
                      , v == next_ip_val
                      , contFn next_ip ->
              disassembleBlock' mem gs2 contFn next_ip
            _ -> return (finishBlock FetchAndExecute gs2, next_ip)