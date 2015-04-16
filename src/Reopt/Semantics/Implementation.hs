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
{-# LANGUAGE DataKinds #-} -- MaybeF
{-# LANGUAGE KindSignatures #-} -- MaybeF

module Reopt.Semantics.Implementation (cfgFromAddress, readInstruction) where

import           Control.Applicative
import Control.Exception (assert)
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
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen (text, colon, (<>), (<+>))
  -- hack for instruction comment

import           Unsafe.Coerce (unsafeCoerce)

import           Debug.Trace
import qualified Flexdis86 as Flexdis
import           Reopt.Memory
import           Reopt.Semantics.CodePointerDiscovery (discoverCodePointers)
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

mkLit :: Integral a => NatRepr n -> a -> Value (BVType n)
mkLit n v = BVValue n (toInteger v .&. mask)
  where mask = maxUnsigned n

asBVLit :: Expr tp -> Maybe Integer
asBVLit (ValueExpr (BVValue _ v)) = Just v
asBVLit _ = Nothing

instance S.IsValue Expr where

  bv_width = exprWidth

  mux c x y
    | Just 1 <- asBVLit c = x
    | Just 0 <- asBVLit c = y
    | otherwise = app $ Mux (exprWidth x) c x y

  bvLit n v = ValueExpr $ mkLit n v
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
  bvMul x y = app $ BVMul (exprWidth x) x y

  bvDiv       x y = app $ BVDiv       (exprWidth x) x y
  bvSignedDiv x y = app $ BVSignedDiv (exprWidth x) x y
  bvMod       x y = app $ BVMod       (exprWidth x) x y
  bvSignedMod x y = app $ BVSignedMod (exprWidth x) x y

  complement x = app $ BVComplement (exprWidth x) x
  x .&. y   = app $ BVAnd (exprWidth x) x y
  x .|. y   = app $ BVOr  (exprWidth x) x y

  bvXor x y
      -- Eliminate xor with 0.
    | Just 0 <- asBVLit x = y
    | Just 0 <- asBVLit y = x
      -- Eliminate xor with self.
    | x == y = bvLit (exprWidth x) (0::Integer)
      -- Default case.
    | otherwise = app $ BVXor (exprWidth x) x y

  x .=. y = app $ BVEq x y

  -- | Concatentates two bit vectors
  -- bvCat :: v (BVType n) -> v (BVType n) -> v (BVType (n + n))

  -- | Splits a bit vectors into two
  -- bvSplit :: v (BVType (n + n)) -> (v (BVType n), v (BVType n))
  bvSplit v = (upperHalf v, lowerHalf v)

  -- | Rotations
  -- bvRol, bvRor :: v (BVType n) -> v (BVType log_n) -> v (BVType n)

  -- | Shifts, the semantics is undefined for shifts >= the width of the first argument
  -- bvShr, bvSar, bvShl :: v (BVType n) -> v (BVType log_n) -> v (BVType n)
  bvShr x y = app $ BVShr (exprWidth x) x y
  bvSar x y = app $ BVSar (exprWidth x) x y
  bvShl x y = app $ BVShl (exprWidth x) x y

  bvTrunc w e0
      -- Constant propagation
    | Just v <- asBVLit e0 =
      bvLit w v
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

    | otherwise = app (Trunc e0 w)

  bvLt x y = app $ BVUnsignedLt x y

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
    | Just (SExt e _) <- asApp e0 =
      -- Runtime check to wordaround GHC typechecker
      case testLeq (S.bv_width e) w of
        Just LeqProof -> S.sext w e
        Nothing -> error "sext internal error"
    | otherwise = app $ SExt e0 w

  uext w e0
    | Just (UExt e _) <- asApp e0 =
      -- Runtime check to wordaround GHC typechecker
      case testLeq (S.bv_width e) w of
        Just LeqProof -> S.uext w e
        Nothing -> error "uext internal error"
      -- Default case
    | otherwise = app $ UExt e0 w

  even_parity x
    | Just xv <- asBVLit x =
        let go 8 r = r
            go i r = go (i+1) $! (xv `testBit` i /= r)
         in S.boolValue (go 0 True)
    | otherwise = app $ EvenParity x
  reverse_bytes x = app $ ReverseBytes (exprWidth x) x

  uadc_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
    | otherwise = app $ UadcOverflows x y c
  sadc_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
    | otherwise = app $ SadcOverflows x y c

  usbb_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
    | otherwise = app $ UsbbOverflows x y c

  ssbb_overflows x y c
    | Just 0 <- asBVLit y, Just 0 <- asBVLit c = S.false
    | otherwise = app $ SsbbOverflows x y c

  bsf x = app $ Bsf (exprWidth x) x
  bsr x = app $ Bsr (exprWidth x) x

------------------------------------------------------------------------
-- GenState

-- | Global for the entire program.
data GlobalGenState = GlobalGenState
       { -- | Index of next assignment identifier to use.
         -- (all used assignment indices must be less than this).
         _nextAssignId :: !AssignId
       , _nextBlockID  :: !Word64         
       }

-- | Number of assignments so far.
nextAssignId :: Simple Lens GlobalGenState AssignId
nextAssignId = lens _nextAssignId (\s v -> s { _nextAssignId = v })

-- | Control flow blocs generated so far.
nextBlockID :: Simple Lens GlobalGenState Word64
nextBlockID = lens _nextBlockID (\s v -> s { _nextBlockID = v })

data PreBlock = PreBlock { _pBlockLabel :: !BlockLabel
                         , _pBlockStmts :: !(Seq Stmt)
                           -- | The last statement in the block.
                         , _pBlockState :: !X86State
                         , _pBlockApps  :: !(MapF (App Value) Assignment)
                         }

pBlockLabel :: Simple Lens PreBlock BlockLabel
pBlockLabel = lens _pBlockLabel (\s v -> s { _pBlockLabel = v })

pBlockStmts :: Simple Lens PreBlock (Seq Stmt)
pBlockStmts = lens _pBlockStmts (\s v -> s { _pBlockStmts = v })

pBlockState :: Simple Lens PreBlock X86State
pBlockState = lens _pBlockState (\s v -> s { _pBlockState = v })

pBlockApps  :: Simple Lens PreBlock (MapF (App Value) Assignment)
pBlockApps = lens _pBlockApps (\s v -> s { _pBlockApps = v })

data MaybeF (t :: Bool) a where
  NothingF :: MaybeF 'False a
  JustF    :: a -> MaybeF 'True a

_JustF :: Lens (MaybeF 'True a) (MaybeF 'True b) a b
_JustF = lens (\(JustF v) -> v) (\_ v -> JustF v)

-- | Local to block discovery.
data GenState tag = GenState 
       { -- | The global state
         _globalGenState :: GlobalGenState
         -- | Blocks that are not in CFG that end with a FetchAndExecute,
         -- which we need to analyze to compute new potential branch targets.
       , _frontierBlocks :: !(Seq Block)
       , _blockState     :: !(MaybeF tag PreBlock)
       }

emptyPreBlock :: X86State
                 -> BlockLabel
                 -> PreBlock
emptyPreBlock s lbl =
  PreBlock { _pBlockLabel = lbl
           , _pBlockStmts = Seq.empty
           , _pBlockApps  = MapF.empty
           , _pBlockState = s
           }
  
emptyGenState :: GlobalGenState -> GenState 'False
emptyGenState st =
  GenState { _globalGenState = st
           , _frontierBlocks = Seq.empty
           , _blockState     = NothingF
           }

globalGenState :: Simple Lens (GenState tag) GlobalGenState
globalGenState = lens _globalGenState (\s v -> s { _globalGenState = v })

-- | Blocks that are not in CFG that end with a FetchAndExecute,
-- which we need to analyze to compute new potential branch targets.
frontierBlocks :: Simple Lens (GenState tag) (Seq Block)
frontierBlocks = lens _frontierBlocks (\s v -> s { _frontierBlocks = v })

-- | Blocks that are not in CFG that end with a FetchAndExecute,
-- which we need to analyze to compute new potential branch targets.
blockState :: Lens (GenState a) (GenState b) (MaybeF a PreBlock) (MaybeF b PreBlock)
blockState = lens _blockState (\s v -> s { _blockState = v })

curX86State :: Simple Lens (GenState 'True) X86State
curX86State = blockState . _JustF . pBlockState                            

-- | Finishes the current block, if it is started.
finishBlock :: (X86State -> TermStmt) -> (GenState a -> GenState 'False)
finishBlock term st =
  case st^.blockState of
   NothingF    -> st
   JustF pre_b -> st & frontierBlocks %~ (Seq.|> b)
                     & blockState .~ NothingF
     where
       b = Block { blockLabel = pre_b^.pBlockLabel
                 , blockStmts = Fold.toList (pre_b^.pBlockStmts)
                 , blockCache = pre_b^.pBlockApps
                 , blockTerm  = term (pre_b^.pBlockState)
                 }

-- | Starts a new block.  If there is a current block it will finish
-- it with FetchAndExecute       
startBlock :: X86State -> BlockLabel -> (GenState a -> GenState 'True)
startBlock s lbl st = finishBlock FetchAndExecute st
                      & blockState .~ JustF (emptyPreBlock s lbl)

-- newtype GenM a = GenM { unGenM :: StateT GenState (Except (MemoryError Word64)) a }
--                deriving (Functor, Applicative, Monad, MonadState GenState)

-- runGenM :: GenM a -> GenState -> Either (MemoryError Word64) (a, GenState)
-- runGenM m s = runExcept (runStateT (unGenM m) s)

------------------------------------------------------------------------
-- X86Generator

newtype X86Generator a = X86G { unX86G :: (a -> GenState 'True -> Some GenState)
                                           -> GenState 'True -> Some GenState
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
runX86Generator :: GenState 'True -> X86Generator () -> Some GenState
runX86Generator st m = unX86G m (\() -> Some) st

modGenState :: State (GenState 'True) a -> X86Generator a
modGenState m = X86G $ \c s -> uncurry c (runState m s)

modState :: State X86State a -> X86Generator a
modState m = modGenState $ do
  s <- use curX86State
  let (r,s') = runState m s
  curX86State .= s'
  return r

newAssignId :: X86Generator AssignId
newAssignId = modGenState $ do
  l <- use $ globalGenState . nextAssignId
  globalGenState . nextAssignId += 1
  return l

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
   BVMul _  l (BVValue _ 1)       -> Just l
   BVMul _  (BVValue _ 1) r       -> Just r

   UExt  (BVValue _ n) sz         -> Just $ BVValue sz n
   BVAdd sz l r                   -> binop (+) sz l r

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
    boolop f (BVValue _ l) (BVValue _ r) = Just $ BVValue knownNat (if f l r then 1 else 0)
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

getLoc :: ImpLocation tp -> X86Generator (Expr tp)
getLoc l0 =
  case l0 of
    S.MemoryAddr w tp -> do
      addr <- eval w
      ValueExpr . AssignedValue <$> addAssignment (Read (MemLoc addr tp))

    S.Register r ->
      case r of
       -- N.ControlReg {} -> addStmt $ Val (ControlLoc r) v
       -- N.DebugReg {}   -> addStmt $ Write (DebugLoc r)   v
       N.SegmentReg {}
         | r == N.fs -> ValueExpr . AssignedValue <$> addAssignment (Read FS)
         | r == N.gs -> ValueExpr . AssignedValue <$> addAssignment (Read GS)
         -- Otherwise registers are 0.
         | otherwise ->
             fail $ "On x86-64 registers other than fs and gs may not be set."
       -- S.MMXReg {} -> do
       --   e <- modState $ ValueExpr <$> use (register r)
       --   ValueExpr <$> eval (S.bvTrunc knownNat e)
       _ -> modState $ ValueExpr <$> use (register r)

    S.LowerHalf l -> lowerHalf <$> getLoc l
    S.UpperHalf l -> upperHalf <$> getLoc l

lowerHalf :: forall n . Expr (BVType (n+n)) -> Expr (BVType n)
lowerHalf e =
     -- Workaround for GHC typechecker
     case testLeq half_width (exprWidth e) of
       Just LeqProof -> S.bvTrunc half_width e
       Nothing -> error "lowerHalf given bad width"
  where half_width :: NatRepr n
        half_width = halfNat (exprWidth e)

n64 :: NatRepr 64
n64 = knownNat

-- | Get the upper half of a bitvector.
upperHalf :: forall n . Expr (BVType (n+n)) -> Expr (BVType n)
-- Handle concrete values
upperHalf (ValueExpr (BVValue w i)) = h
   where half_width = halfNat w
         h = bvLit half_width (i `shiftR` widthVal half_width)
upperHalf e =
   case asApp e of
      -- Handle expression concatenation.
      -- N.B. We use unsafe coerce due to GHC failing to match the (n+n) in upperHalf
      -- to the (n+n) bound in ConcatV.
      Just (ConcatV _ _ h) -> unsafeCoerce h
      -- Introduce split operations
      _ ->
        -- Workaround for GHC typechecker
        case testLeq half_width (exprWidth e) of
          Just LeqProof -> app (UpperHalf half_width e)
          Nothing -> error "upperHalf given bad width"
  where half_width :: NatRepr n
        half_width = halfNat (exprWidth e)

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
           (LeqProof :: LeqProof (TypeBits tp) 64) <- return (addIsLeqLeft1 (LeqProof :: LeqProof (TypeBits tp + TypeBits tp) 64))
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
           -- FIXME: sort this out
           -- S.MMXReg {} -> do
           --   ext_v <- evalApp (MMXExtend v)
           --   modState $ register r .= ext_v
           _ -> modState $ register r .= v

       S.LowerHalf l -> do
         b <- getLoc l
         upper <- eval (upperHalf b)
         go l . AssignedValue =<< addAssignment (EvalApp (ConcatV (valueWidth v) v upper))
       S.UpperHalf l -> do
         b <- getLoc l
         lower <- eval (lowerHalf b)
         go l . AssignedValue =<< addAssignment (EvalApp (ConcatV (valueWidth v) lower v))

       S.X87StackRegister _ -> do
         error "setLoc X87StackRegister undefined"

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
        X86G $ \c s -> do
          let p_b = s ^. (blockState . _JustF)
              (t_block_id, s')  = s  & globalGenState . nextBlockID <<+~ 1
              (f_block_id, s'') = s' & globalGenState . nextBlockID <<+~ 1

          let last_block_id = p_b^.pBlockLabel
              t_block_label = GeneratedBlock (blockParent last_block_id) t_block_id
              f_block_label = GeneratedBlock (blockParent last_block_id) f_block_id

          -- Note that startBlock flushes the current block if required.
          let flush_current = finishBlock (const (Branch cond t_block_label f_block_label))
              run_t, run_f :: forall tp. GenState tp -> Some GenState
              run_t = unX86G t c . startBlock (p_b^.pBlockState) t_block_label
              run_f = unX86G f c . startBlock (p_b^.pBlockState) f_block_label

          -- The finishBlock here results in a new block after
          -- conditional jumps, for example (no continuing blocks)
          Some . viewSome (finishBlock FetchAndExecute)
            . viewSome run_f . run_t . flush_current $ s''

  -- exception :: Value m BoolType    -- mask
  --            -> Value m BoolType -- predicate
  --            -> ExceptionClass
  --            -> m ()
  exception m p c = S.ifte_ (S.complement m S..&. p)
                    (addStmt (PlaceHolderStmt [] $ "Exception " ++ (show c)))
                    (return ())
  syscall v = do v' <- eval v
                 addStmt (PlaceHolderStmt [Some v'] "Syscall")
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

-- | Read instruction at a given memory address.
readInstruction :: Memory Word64 -- Memory to read.
                -> Word64 -- Address to read from.
                -> Either (MemoryError Word64) (Flexdis.InstructionInstance, Word64)
readInstruction mem addr = runMemoryByteReader pf_x mem addr m
  where m = Flexdis.disassembleInstruction Flexdis.defaultX64Disassembler

-- | A location to explore
data ExploreLoc
   = ExploreLoc { loc_ip :: CodeAddr
                  -- ^ IP address.
                , loc_x87_top :: Int
                  -- ^ Top of x86 address.
                }
 deriving (Eq, Ord)

rootLoc :: CodeAddr -> ExploreLoc
rootLoc ip = ExploreLoc { loc_ip = ip
                        , loc_x87_top = 7 }

locFromGuess :: CodeAddr -> ExploreLoc
locFromGuess = rootLoc

initX86State :: ExploreLoc -- ^ Location to explore from.
             -> X86State
initX86State loc =
  X86State { _curIP = BVValue knownNat (toInteger (loc_ip loc))
           , _reg64Regs = V.generate 16 (Initial . N.GPReg)
           , _flagRegs  = V.generate 32 (Initial . N.FlagReg)
           , _x87ControlWord = V.generate 16 (Initial . N.X87ControlReg)
           , _x87StatusWord = V.generate 16 (Initial . N.X87StatusReg)
           , _x87TopReg   = BVValue knownNat (fromIntegral $ loc_x87_top loc)
           , _x87TagWords = V.generate 8 (Initial . N.X87TagReg)
           , _x87Regs = V.generate 8 (Initial . N.X87FPUReg)
           , _xmmRegs = V.generate 8 (Initial . N.XMMReg)
           }

disassembleBlock :: Memory Word64
                 -> GlobalGenState
                 -> ExploreLoc -- ^ Location to explore from.
                 -> Either (MemoryError Word64)
                           (GlobalGenState, [Block], Set ExploreLoc, CodeAddr)
disassembleBlock mem st loc =
  let lbl = DecompiledBlock (loc_ip loc)
      p_b = emptyPreBlock (initX86State loc) lbl
      gs  = emptyGenState st & blockState .~ JustF p_b      
      mk (gs', ip) = let block_list = Fold.toList (gs'^.frontierBlocks) in
                      (gs' ^. globalGenState, block_list, getFrontierNext block_list, ip)
  in mk <$> (trace ("Exploring " ++ showHex (loc_ip loc) "") $ disassembleBlock' mem gs (loc_ip loc))

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

disassembleBlock' :: Memory Word64
                  -> GenState 'True
                  -> CodeAddr
                  -> Either (MemoryError Word64)
                            (GenState 'False, CodeAddr)
disassembleBlock' mem gs addr = do
  (i, next_ip) <- readInstruction mem addr
  -- Update current IP
  let gs1 = gs & curX86State . curIP .~ BVValue knownNat (toInteger next_ip)
  let some_gs2 = runX86Generator gs1 $ do
                      let line = text (showHex addr "") <> colon
                                 <+> Flexdis.ppInstruction addr i
                      addStmt (Comment (show line))
                      execInstruction i
  case some_gs2 of
   Some gs2 -> case gs2^.blockState of
                JustF p_b | [loc] <- getExploreLocs (p_b^.pBlockState)
                          , loc_ip loc == next_ip ->
                              disassembleBlock' mem gs2 next_ip
                _ -> return (finishBlock FetchAndExecute gs2, next_ip)

-- FIXME: move
newtype Hex = Hex CodeAddr
              deriving (Eq, Ord)

mkHex :: CodeAddr -> Hex
mkHex = Hex

instance Show Hex where
  show (Hex v) = showHex v ""

mergeFrontier :: [Block] -> CFG -> CFG
mergeFrontier bs cfg = Fold.foldl' (flip insertBlock) cfg bs

recursiveDescent :: Memory Word64
                -> CFG     -- ^ CFG generated so far.
                -> GlobalGenState
                -> Set CodeAddr   -- ^ Set of addresses after blocks we stopped at.
                -> Set ExploreLoc -- ^ Set of locations explored so far.
                -> Set ExploreLoc -- ^ List of addresses to explore next.
                -> (CFG, Set CodeAddr)
recursiveDescent mem cfg st ends explored frontier
    | Set.null frontier = (cfg, ends)
    | otherwise =
        let (loc,s)         = Set.deleteFindMin frontier
            explored'       = Set.insert loc explored
            go cfg' st' ends' frontier'
              = recursiveDescent mem cfg' st' ends' explored' frontier'
        in
        case disassembleBlock mem st loc of
          Left  e  -> trace ("Skipping " ++ showHex (loc_ip loc) (": " ++ show e))
                      $ go cfg st ends s
          Right (st', blocks, next, end) ->
            let cfg' = mergeFrontier blocks cfg
                guesses   = discoverCodePointers mem cfg'
                            (DecompiledBlock (loc_ip loc)) -- FIXME
                allNext   = trace ("At " ++ show (mkHex $ loc_ip loc)
                                   ++ " guessing pointers "
                                   ++ show (Set.map mkHex guesses)
                                   ++ " with next "
                                   ++ show (Set.map (mkHex . loc_ip)  next))
                            $ next `Set.union` Set.map locFromGuess guesses
            in go cfg' st' (Set.insert end ends) (Set.union s (Set.difference allNext explored'))

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> (CFG, Set CodeAddr)
cfgFromAddress mem a =
  recursiveDescent mem emptyCFG st Set.empty
  (Set.singleton (rootLoc 0)) (Set.singleton loc)
  -- recursiveDescent mem pcfg Set.empty (Set.singleton loc)
  -- XXXXXXXX FIXME: frame_dummy has a call to 0 for some reason, so we pretend we have seen it
  -- this is a giant hack.
  where st = GlobalGenState { _nextBlockID = 0, _nextAssignId = 0 }
        loc = rootLoc a

{-
completeProgram :: Memory Word64
                -> CodeAddr
                -> CFG
completeProgram mem addr = do
  error "completeProgram undefined" mem addr
-}

{-
resolve :: Memory Word64
        -> [Word64]
        -> CFG s
        -> ST s (CFG s)
resolve mem [] pg = return pg
resolve mem (a:r) pg = do
  g <- cfgFromAddress mem a
  resolve mem r (Map.insert a g)
-}
