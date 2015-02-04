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
module Reopt.Semantics.Implementation
  ( cfgFromAddress
  , completeProgram
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.Foldable as Fold
import Data.Parameterized.NatRepr
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Data.Word

import Unsafe.Coerce (unsafeCoerce)

import qualified Flexdis86.InstructionSet as Flexdis86

import Reopt.Memory
import Reopt.Semantics.Monad
  ( Type(..)
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import Reopt.Semantics.Representation


------------------------------------------------------------------------
-- Location

{-
instance Enum FlagReg where
  toEnum 0 = CF_FLAG
  toEnum 1 = PF_FLAG
  toEnum 2 = AF_FLAG
  toEnum 3 = ZF_FLAG
  toEnum 4 = SF_FLAG
  toEnum 5 = OF_FLAG
  toEnum _ = error "Unexpected numerator."

  fromEnum CF_FLAG = 0
  fromEnum PF_FLAG = 1
  fromEnum AF_FLAG = 2
  fromEnum ZF_FLAG = 3
  fromEnum SF_FLAG = 4
  fromEnum OF_FLAG = 5
-}

{-
  af_flag = FlagReg AF_FLAG
  cf_flag = FlagReg CF_FLAG
  df_flag = FlagReg DF_FLAG
  of_flag = FlagReg OF_FLAG
  pf_flag = FlagReg PF_FLAG
  sf_flag = FlagReg SF_FLAG
  zf_flag = FlagReg ZF_FLAG
-}

------------------------------------------------------------------------
-- X86State


------------------------------------------------------------------------
-- Expr

-- | A pure expression for isValue.
data Expr tp where
  -- An expression obtained from some value.
  ValueExpr :: !(Value tp) -> Expr tp
  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(App Expr tp) -> Expr tp

app :: App Expr tp -> Expr tp
app = AppExpr

-- | Return width of expression.
exprWidth :: Expr (BVType n) -> NatRepr n
exprWidth (ValueExpr v) = valueWidth v
exprWidth (AppExpr a) = appWidth a

instance S.IsValue Expr where

  bv_width = exprWidth

  bvLit n v = ValueExpr $ BVValue n (toInteger v .&. mask)
    where mask = 2^(widthVal n) - 1
  bvAdd x y = app $ BVAdd (exprWidth x) x y
  bvSub x y = app $ BVSub (exprWidth x) x y
  bvMul x y = app $ BVMul (exprWidth x) x y

  complement x = app $ BVComplement (exprWidth x) x
  x .&. y   = app $ BVAnd (exprWidth x) x y
  x .|. y   = app $ BVOr  (exprWidth x) x y
  bvXor x y = app $ BVXor (exprWidth x) x y
  x .=. y   = app $ BVEq x y


  bvTrunc = flip truncExpr

  sext w x = app $ SExt x w
  uext w x = app $ UExt x w

  even_parity x = app $ EvenParity x
  reverse_bytes x = app $ ReverseBytes (exprWidth x) x

  uadc_overflows x y c = app $ UadcOverflows x y c
  sadc_overflows x y c = app $ SadcOverflows x y c

  usbb_overflows x y c = app $ SsbbOverflows x y c
  ssbb_overflows x y c = app $ UsbbOverflows x y c

  bsf x = app $ Bsf (exprWidth x) x
  bsr x = app $ Bsr (exprWidth x) x

  doubleAdd x y = app $ DoubleAdd x y

{-
-- | Return portion of a bitvector
bvSlice :: Expr (BVType n) -- ^ Expression to evaluate
        -> Int             -- ^ Offset in bits
        -> NatRepr m       -- ^ Type representation
        -> Expr (BVType m)
bvSlice e0 o w =
  case e0 of
    ValueExpr (BVValue n i) -> bvLit w (i `shiftR` o)
    AppExpr (

-- | Update a bitvector array at a given slice.
updateBVSlice :: Expr (BVType n) -- ^ Value to update slice of.
              -> Int             -- ^ Offset in bits.
              -> Expr (BVType m) -- ^ Value to update
              -> Expr (BVType n)
updateBVSlice b o v
   | Just Refl <- testEquality bw vw = v -- Overwriting full value.
   |

 where bw = exprWidth b
       vw = exprWidth v
       half_width =
       updateLow



  case testEquality o v of
    JustRefl ->
  -- Cases: Update whole range.
  --
undefined
-}

------------------------------------------------------------------------
-- PartialCFG

data PartialCFG = PartialCFG
       { _prevCFG      :: !CFG
       , _nextBlockID  :: !Word64
       , _nextAssignId :: !AssignId
       }

emptyPartialCFG :: PartialCFG
emptyPartialCFG =
  PartialCFG { _prevCFG = emptyCFG
             , _nextBlockID = 0
             , _nextAssignId = 0
             }

-- | Control flow blocs generated so far.
prevCFG :: Simple Lens PartialCFG CFG
prevCFG = lens _prevCFG (\s v -> s { _prevCFG = v })

-- | Control flow blocs generated so far.
nextBlockID :: Simple Lens PartialCFG Word64
nextBlockID = lens _nextBlockID (\s v -> s { _nextBlockID = v })

-- | Number of assignments so far.
nextAssignId :: Simple Lens PartialCFG AssignId
nextAssignId = lens _nextAssignId (\s v -> s { _nextAssignId = v })

------------------------------------------------------------------------
-- GenState

data GenState = GenState
       { _partialCFG   :: !PartialCFG
       , _curBlockID   :: !BlockLabel
       , _prevStmts    :: !(Seq Stmt)
       , _curX86State  :: !X86State
       }

emptyGenState :: PartialCFG
              -> BlockLabel
              -> X86State
              -> GenState
emptyGenState cfg lbl s =
  GenState { _partialCFG = cfg
           , _curBlockID = lbl
           , _prevStmts = Seq.empty
           , _curX86State = s
           }

-- | Index of current block being generated.
partialCFG :: Simple Lens GenState PartialCFG
partialCFG = lens _partialCFG (\s v -> s { _partialCFG = v })

-- | Index of current block being generated.
curBlockID :: Simple Lens GenState BlockLabel
curBlockID = lens _curBlockID (\s v -> s { _curBlockID = v })

-- | Statements seen so far in this block
prevStmts :: Simple Lens GenState (Seq Stmt)
prevStmts = lens _prevStmts (\s v -> s { _prevStmts = v })

-- | Statements seen so far.
curX86State :: Simple Lens GenState X86State
curX86State = lens _curX86State (\s v -> s { _curX86State = v })

------------------------------------------------------------------------
-- X86Generator

newtype X86Generator a = X86G { unX86G :: (a -> GenState -> PartialCFG)
                                       -> GenState
                                       -> PartialCFG
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

runX86Generator :: PartialCFG -- ^ Control flow graph built so far.
                -> CodeAddr   -- ^ Address of current block.
                -> X86State   -- ^ Initial state of processor
                -> X86Generator ()
                -> PartialCFG
runX86Generator pcfg l s m = unX86G m (\() gs -> getPartialCFG gs) gs0
  where gs0 = emptyGenState pcfg (DecompiledBlock l) s

-- | Create a block to fetch and execute from the previous statements and
-- current x86 state.
getPartialCFG :: GenState -> PartialCFG
getPartialCFG s = g & prevCFG %~ insertBlock b
  where g = s^.partialCFG
        b = Block { blockLabel = s^.curBlockID
                  , blockStmts = Fold.toList $ s^.prevStmts
                  , blockTerm = FetchAndExecute $ s^.curX86State
                  }

modGenState :: State GenState a -> X86Generator a
modGenState m = X86G $ \c s -> uncurry c (runState m s)

modState :: State X86State a -> X86Generator a
modState m = modGenState $ do
  s <- use $ curX86State
  let (r,s') = runState m s
  curX86State .= s'
  return r

newAssignId :: X86Generator AssignId
newAssignId = modGenState $ do
  l <- use $ partialCFG . nextAssignId
  partialCFG . nextAssignId += 1
  return l

addStmt :: Stmt -> X86Generator ()
addStmt stmt = modGenState $ prevStmts %= (Seq.|> stmt)

addAssignment :: AssignRhs tp -> X86Generator (Value tp)
addAssignment rhs = do
  l <- newAssignId
  let a = Assignment l rhs
  addStmt $ AssignStmt a
  return  $ AssignedValue a

eval :: Expr tp -> X86Generator (Value tp)
eval (ValueExpr v) = return v
eval (AppExpr a) = do
  av <- traverseApp eval a
  addAssignment $ EvalApp av

-- | Type for addresses.
type AddrExpr = Expr (BVType 64)

type ImpLocation tp = S.Location AddrExpr tp

getLoc :: ImpLocation tp -> X86Generator (Expr tp)
getLoc l0 =
  case l0 of
    S.FlagReg r -> do
      fmap (ValueExpr . (V.! r)) $ modState $ use flagRegs
    S.MemoryAddr w tp -> do
      addr <- eval w
      ValueExpr <$> addAssignment (ReadAddr addr tp)

    S.GPReg  r -> modState $ ValueExpr . (V.! idx) <$> use reg64Regs
      where idx = Flexdis86.reg64Idx r
    S.IPReg    -> modState $ ValueExpr <$> use curIP

    S.CReg   r -> ValueExpr <$> addAssignment (ReadControlReg r)
    S.DReg   r -> ValueExpr <$> addAssignment (ReadDebugReg r)

    S.MMXReg r -> modState $ ValueExpr . (V.! idx) <$> use mmxRegs
      where idx = Flexdis86.mmxRegIdx r

    S.SegmentReg r
      | r == Flexdis86.fs -> ValueExpr <$> addAssignment ReadFS
      | r == Flexdis86.gs -> ValueExpr <$> addAssignment ReadGS
        -- Otherwise registers are 0.
      | otherwise -> return (ValueExpr (bvValue 0))

    S.XMMReg r -> modState $ ValueExpr . (V.! idx) <$> use xmmRegs
      where idx = Flexdis86.xmmRegIdx r

    S.LowerHalf l -> lowerHalf <$> getLoc l
    S.UpperHalf l -> upperHalf <$> getLoc l

asApp :: Expr tp -> Maybe (App Expr tp)
asApp (AppExpr a) = Just a
asApp (ValueExpr (AssignedValue (Assignment _ (EvalApp a))))
  = Just (mapApp ValueExpr a)
asApp _ = Nothing

lowerHalf :: forall n . Expr (BVType (n+n)) -> Expr (BVType n)
lowerHalf e =
     -- Workaround for GHC typechecker
     case testLeq half_width (exprWidth e) of
       Just LeqProof -> truncExpr e half_width
       Nothing -> error "lowerHalf given bad width"
  where half_width :: NatRepr n
        half_width = halfNat (exprWidth e)

-- | Apply trunc to expression with simplification.
truncExpr :: (m <= n) => Expr (BVType n) -> NatRepr m -> Expr (BVType m)
truncExpr e w | Just Refl <- testEquality (exprWidth e) w = e
truncExpr e0 w =
  case asApp e0 of
    Just (ConcatV lw l _) | Just LeqProof <- testLeq w lw ->
      truncExpr l w
    Just (Trunc e _) ->
       -- Runtime check to workaround GHC typechecker.
      case testLeq w (exprWidth e) of
        Just LeqProof -> truncExpr e w
        Nothing -> error "truncExpr given bad width"
    _ -> app (Trunc e0 w)

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
setLoc :: ImpLocation tp -> Value tp -> X86Generator ()
setLoc l0 v =
  case l0 of
    S.FlagReg r -> modState $ flagRegs . ix r .= v
    S.MemoryAddr w _ -> do
      addr <- eval w
      addStmt $ WriteMem addr v
    S.GPReg r -> modState $ reg64Regs . ix idx .= v
      where idx = Flexdis86.reg64Idx r
    S.IPReg -> modState $ curIP .= v
    S.CReg r -> addStmt $ WriteControlReg r v
    S.DReg r -> addStmt $ WriteDebugReg r v
    S.MMXReg r -> modState $ mmxRegs . ix idx .= v
      where idx = Flexdis86.mmxRegIdx r
    S.SegmentReg r
      | r == Flexdis86.fs -> addStmt $ WriteFS v
      | r == Flexdis86.gs -> addStmt $ WriteGS v
        -- Otherwise registers are 0.
      | otherwise -> do
        fail $ "On x86-64 registers other than fs and gs may not be set."
    S.XMMReg r -> modState $ xmmRegs . ix idx .= v
      where idx = Flexdis86.xmmRegIdx r
    S.LowerHalf l -> do
      b <- getLoc l
      upper <- eval (upperHalf b)
      setLoc l =<< addAssignment (EvalApp (ConcatV (valueWidth v) v upper))
    S.UpperHalf l -> do
      b <- getLoc l
      lower <- eval (lowerHalf b)
      setLoc l =<< addAssignment (EvalApp (ConcatV (valueWidth v) lower v))


instance S.Semantics X86Generator where
  set_undefined l = do
    setLoc l =<< addAssignment (SetUndefined knownNat)

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
  ifte_ c_expr t f = do
    cond <- eval c_expr
    X86G $ \c s -> do
      let last_block_id = s^.curBlockID
      let last_block_stmts = Fold.toList $ s^.prevStmts

      -- Label for true branch
      let true_block_id = GeneratedBlock (s^.partialCFG^.nextBlockID)

      let t_start = emptyGenState (s^.partialCFG) true_block_id (s^.curX86State)
      -- Get state after running t followed by c
      let t_end = unX86G t c t_start
      -- Label for false branch
      let false_block_id = GeneratedBlock (t_end^.nextBlockID)
      -- Get block for code before branch now that we can get label
      -- for false branch.
      let last_block = Block { blockLabel = last_block_id
                             , blockStmts = last_block_stmts
                             , blockTerm  = Branch cond true_block_id false_block_id
                             }
      -- Update partial CFG now that we have last_block
      let f_cfg   = t_end & nextBlockID +~ 1
                          & prevCFG %~ insertBlock last_block
      -- Create block for true branch.
      let f_start = emptyGenState f_cfg false_block_id (s^.curX86State)
      -- Get final state
      let f_end = unX86G f c f_start
      --  Return since we've already run continuation in branches.
      f_end


cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> Word64
                  -- ^ Address to start disassembler form.
               -> CFG
cfgFromAddress = error "Reopt.Semantics.Implementation.cfgFromAddress undefined"
  runX86Generator emptyPartialCFG

completeProgram :: Memory Word64
                -> CodeAddr
                -> CFG
completeProgram mem addr = do
  error "completeProgram undefined" mem addr

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