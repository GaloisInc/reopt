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
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word
import Numeric (showHex)

import Unsafe.Coerce (unsafeCoerce)

import qualified Flexdis86 as Flexdis

import Reopt.Memory
import Reopt.Semantics.Monad
  ( Type(..)
  , BoolType
  , bvLit
  )
import qualified Reopt.Semantics.Monad as S
import Reopt.Semantics.Representation
import Reopt.Semantics.FlexdisMatcher (execInstruction)
import Debug.Trace

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
-- Expr

-- | A pure expression for isValue.
data Expr tp where
  -- An expression obtained from some value.
  ValueExpr :: !(Value tp) -> Expr tp
  -- An expression that is computed from evaluating subexpressions.
  AppExpr :: !(App Expr tp) -> Expr tp

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

instance S.IsValue Expr where

  bv_width = exprWidth

  mux c x y = app $ Mux (exprWidth x) c x y

  bvLit n v = ValueExpr $ BVValue n (toInteger v .&. mask)
    where mask = 2^(widthVal n) - 1
  bvAdd x y = app $ BVAdd (exprWidth x) x y
  bvSub x y = app $ BVSub (exprWidth x) x y
  bvMul x y = app $ BVMul (exprWidth x) x y

  bvDiv       x y = app $ BVDiv       (exprWidth x) x y
  bvSignedDiv x y = app $ BVSignedDiv (exprWidth x) x y
  bvMod       x y = app $ BVMod       (exprWidth x) x y
  bvSignedMod x y = app $ BVSignedMod (exprWidth x) x y

  complement x = app $ BVComplement (exprWidth x) x
  x .&. y   = app $ BVAnd (exprWidth x) x y
  x .|. y   = app $ BVOr  (exprWidth x) x y
  bvXor x y = app $ BVXor (exprWidth x) x y
  x .=. y   = app $ BVEq x y

  -- | Concatentates two bit vectors
  -- bvCat :: v (BVType n) -> v (BVType n) -> v (BVType (n + n))

  -- | Splits a bit vectors into two
  -- bvSplit :: v (BVType (n + n)) -> (v (BVType n), v (BVType n))

  -- | Rotations
  -- bvRol, bvRor :: v (BVType n) -> v (BVType log_n) -> v (BVType n)

  -- | Shifts, the semantics is undefined for shifts >= the width of the first argument
  -- bvShr, bvSar, bvShl :: v (BVType n) -> v (BVType log_n) -> v (BVType n)

  bvTrunc = flip truncExpr

  bvLt x y = app $ BVUnsignedLt x y

  bvBit x y = app $ BVBit x y

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

  -- doubleAdd x y = app $ DoubleAdd x y

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

-- | A Partial CFG as it is constructed.
data PartialCFG = PartialCFG
       { _prevCFG      :: !CFG
         -- | Blocks that are not in CFG that end with a FetchAndExecute,
         -- which we need to analyze to compute new potential branch targets.
       , _frontierBlocks :: !(Seq Block)
         -- | Index of next generated block
         -- (all used indices must be less than this).
       , _nextBlockID  :: !Word64
         -- | Index of next assignment identifier to use.
         -- (all used assignment indices must be less than this).
       , _nextAssignId :: !AssignId
         -- | List of addresses added to CFG so far.
       }

emptyPartialCFG :: PartialCFG
emptyPartialCFG =
  PartialCFG { _prevCFG = emptyCFG
             , _frontierBlocks = Seq.empty
             , _nextBlockID = 0
             , _nextAssignId = 0
             }

-- | Control flow blocs generated so far.
prevCFG :: Simple Lens PartialCFG CFG
prevCFG = lens _prevCFG (\s v -> s { _prevCFG = v })

-- | Blocks that are not in CFG that end with a FetchAndExecute,
-- which we need to analyze to compute new potential branch targets.
frontierBlocks :: Simple Lens PartialCFG (Seq Block)
frontierBlocks = lens _frontierBlocks (\s v -> s { _frontierBlocks = v })

-- | Control flow blocs generated so far.
nextBlockID :: Simple Lens PartialCFG Word64
nextBlockID = lens _nextBlockID (\s v -> s { _nextBlockID = v })

-- | Number of assignments so far.
nextAssignId :: Simple Lens PartialCFG AssignId
nextAssignId = lens _nextAssignId (\s v -> s { _nextAssignId = v })

mergeFrontier :: PartialCFG -> PartialCFG
mergeFrontier pg = pg & prevCFG %~ flip (Fold.foldl' (flip insertBlock)) frontier
                      & frontierBlocks .~ Seq.empty
  where frontier = pg^.frontierBlocks

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

newtype X86Generator r a = X86G { unX86G :: (a -> GenState -> r)
                                         -> GenState
                                         -> r
                                }

instance Functor (X86Generator r) where
  fmap = liftM

instance Applicative (X86Generator r) where
  pure = return
  (<*>) = ap

instance Monad (X86Generator r) where
  return v = X86G $ \c -> c v
  m >>= h = X86G $ \c -> unX86G m (\mv -> unX86G (h mv) c)
  fail = error

type instance S.Value (X86Generator r) = Expr

-- | Create a block to fetch and execute from the previous statements and
-- current x86 state.
getPartialCFG :: GenState -> PartialCFG
getPartialCFG s = g & frontierBlocks %~ (Seq.|> b)
  where g = s^.partialCFG
        -- Create block for final state.
        b = Block { blockLabel = s^.curBlockID
                  , blockStmts = Fold.toList $ s^.prevStmts
                  , blockTerm = FetchAndExecute $ s^.curX86State
                  }

runX86Generator :: GenState -- ^ Initial state of generator.
                -> X86Generator PartialCFG ()
                -> PartialCFG
runX86Generator gs0 m = unX86G m (\() gs -> getPartialCFG gs) gs0

modGenState :: State GenState a -> X86Generator r a
modGenState m = X86G $ \c s -> uncurry c (runState m s)

modState :: State X86State a -> X86Generator r a
modState m = modGenState $ do
  s <- use $ curX86State
  let (r,s') = runState m s
  curX86State .= s'
  return r

newAssignId :: X86Generator r AssignId
newAssignId = modGenState $ do
  l <- use $ partialCFG . nextAssignId
  partialCFG . nextAssignId += 1
  return l

addStmt :: Stmt -> X86Generator r ()
addStmt stmt = modGenState $ prevStmts %= (Seq.|> stmt)

addAssignment :: AssignRhs tp -> X86Generator r (Value tp)
addAssignment rhs = do
  l <- newAssignId
  let a = Assignment l rhs
  addStmt $ AssignStmt a
  return  $ AssignedValue a

evalApp :: App Value tp  -> X86Generator r (Value tp)
evalApp a = addAssignment (EvalApp a)

eval :: Expr tp -> X86Generator r (Value tp)
eval (ValueExpr v) = return v
eval (AppExpr a) = evalApp =<< traverseApp eval a

-- | Type for addresses.
type AddrExpr = Expr (BVType 64)

type ImpLocation tp = S.Location AddrExpr tp

getLoc :: ImpLocation tp -> X86Generator r (Expr tp)
getLoc l0 =
  case l0 of
    S.FlagReg r -> do
      fmap (ValueExpr . (V.! r)) $ modState $ use flagRegs
    S.MemoryAddr w tp -> do
      addr <- eval w
      ValueExpr <$> addAssignment (Read (MemLoc addr tp))

    S.GPReg  r -> modState $ ValueExpr . (V.! idx) <$> use reg64Regs
      where idx = Flexdis.reg64Idx r
    S.IPReg    -> modState $ ValueExpr <$> use curIP

    S.CReg   r -> ValueExpr <$> addAssignment (Read (ControlLoc r))
    S.DReg   r -> ValueExpr <$> addAssignment (Read (DebugLoc r))

    S.MMXReg r -> do
      let idx = Flexdis.mmxRegIdx r
      e <- modState $ ValueExpr . (V.! idx) <$> use x87Regs
      ValueExpr <$> eval (truncExpr e knownNat)

    S.SegmentReg r
      | r == Flexdis.fs -> ValueExpr <$> addAssignment (Read FS)
      | r == Flexdis.gs -> ValueExpr <$> addAssignment (Read GS)
        -- Otherwise registers are 0.
      | otherwise -> return (ValueExpr (bvValue 0))

    S.XMMReg r -> modState $ ValueExpr . (V.! idx) <$> use xmmRegs
      where idx = Flexdis.xmmRegIdx r

    S.LowerHalf l -> lowerHalf <$> getLoc l
    S.UpperHalf l -> upperHalf <$> getLoc l

valueAsApp :: Value tp -> Maybe (App Value tp)
valueAsApp (AssignedValue (Assignment _ (EvalApp a))) = Just a
valueAsApp _ = Nothing

asApp :: Expr tp -> Maybe (App Expr tp)
asApp (AppExpr   a) = Just a
asApp (ValueExpr v) = mapApp ValueExpr <$> valueAsApp v
asApp _ = Nothing

lowerHalf :: forall n . Expr (BVType (n+n)) -> Expr (BVType n)
lowerHalf e =
     -- Workaround for GHC typechecker
     case testLeq half_width (exprWidth e) of
       Just LeqProof -> truncExpr e half_width
       Nothing -> error "lowerHalf given bad width"
  where half_width :: NatRepr n
        half_width = halfNat (exprWidth e)

n64 :: NatRepr 64
n64 = knownNat

-- | Apply trunc to expression with simplification.
truncExpr :: (m <= n) => Expr (BVType n) -> NatRepr m -> Expr (BVType m)
truncExpr e w | Just Refl <- testEquality (exprWidth e) w = e
truncExpr e0 w =
  case asApp e0 of
    Just (MMXExtend e) | Just Refl <- testEquality w n64 ->
      e
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
setLoc :: ImpLocation tp -> Value tp -> X86Generator r ()
setLoc l0 v =
  case l0 of
    S.FlagReg r -> modState $ flagRegs . ix r .= v
    S.MemoryAddr w _ -> do
      addr <- eval w
      addStmt $ Write (MemLoc addr (valueType v)) v
    S.GPReg r -> modState $ reg64Regs . ix idx .= v
      where idx = Flexdis.reg64Idx r
    S.IPReg -> modState $ curIP .= v
    S.CReg r -> addStmt $ Write (ControlLoc r) v
    S.DReg r -> addStmt $ Write (DebugLoc r)   v
    S.MMXReg r -> do
      ext_v <- evalApp (MMXExtend v)
      let idx = Flexdis.mmxRegIdx r
      modState $ x87Regs . ix idx .= ext_v
    S.SegmentReg r
      | r == Flexdis.fs -> addStmt $ Write FS v
      | r == Flexdis.gs -> addStmt $ Write GS v
        -- Otherwise registers are 0.
      | otherwise -> do
        fail $ "On x86-64 registers other than fs and gs may not be set."
    S.XMMReg r -> modState $ xmmRegs . ix idx .= v
      where idx = Flexdis.xmmRegIdx r
    S.LowerHalf l -> do
      b <- getLoc l
      upper <- eval (upperHalf b)
      setLoc l =<< addAssignment (EvalApp (ConcatV (valueWidth v) v upper))
    S.UpperHalf l -> do
      b <- getLoc l
      lower <- eval (lowerHalf b)
      setLoc l =<< addAssignment (EvalApp (ConcatV (valueWidth v) lower v))
    S.X87StackRegister i -> do
      undefined

instance S.Semantics (X86Generator PartialCFG) where
  make_undefined (S.BVTypeRepr n) =
    ValueExpr <$> addAssignment (SetUndefined n)

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
      let t_cfg = s^.partialCFG
      let true_block_id = GeneratedBlock (t_cfg^.nextBlockID)
      let t_start = emptyGenState (t_cfg & nextBlockID +~ 1) true_block_id (s^.curX86State)
      -- Get state after running t followed by c
      let t_end :: PartialCFG
          t_end = unX86G t c t_start
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
      -- Create state for starting evaluation of false branch.
      let f_start = emptyGenState f_cfg false_block_id (s^.curX86State)
      -- Get final state
      let f_end :: PartialCFG
          f_end = unX86G f c f_start
      --  Return since we've already run continuation in branches.
      f_end

{-
Steps:
-}

{-
disassembleInstruction :: ByteReader m
                       => InstructionParser
                       -> m InstructionInstance
-}

falseBit :: Value BoolType
falseBit = BVValue knownNat 0

trueBit :: Value BoolType
trueBit = BVValue knownNat 1

initStatusWord :: Int -> X87StatusWord
initStatusWord top = X87StatusWord
  { _x87_ie = Initial_X87_IE
  , _x87_de = Initial_X87_DE
  , _x87_ze = Initial_X87_ZE
  , _x87_oe = Initial_X87_OE
  , _x87_ue = Initial_X87_UE
  , _x87_pe = Initial_X87_PE
  , _x87_ef = Initial_X87_EF
  , _x87_es = Initial_X87_ES
  , _x87_c0 = Initial_X87_C0
  , _x87_c1 = Initial_X87_C1
  , _x87_c2 = Initial_X87_C2
  , _x87_c3 = Initial_X87_C3
  , _x87_top  = BVValue knownNat (toInteger top)
  , _x87_busy = falseBit
  }




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

initX86State :: ExploreLoc -- ^ Location to explore from.
             -> X86State
initX86State loc =
  X86State { _curIP = BVValue knownNat (toInteger (loc_ip loc))
           , _reg64Regs = V.generate 16 (\i -> InitialGenReg i)
           , _flagRegs  = V.generate 32 (\i -> InitialFlag i)
           , _x87ControlWord = V.generate 16 (\i -> InitialX87ControlBit i)
           , _x87StatusWord = initStatusWord (loc_x87_top loc)
           , _x87TagWords = V.generate 8 (\i -> InitialTagWord i)
           , _x87Regs = V.generate 8 (\i -> InitialFPUReg i)
           , _xmmRegs = V.generate 8 (\i -> InitialXMMReg i)
           }

disassembleBlock :: Memory Word64
                 -> PartialCFG
                 -> ExploreLoc -- ^ Location to explore from.
                 -> (PartialCFG, Set ExploreLoc)
disassembleBlock mem g loc = do
  let gs = GenState { _partialCFG = g
                    , _curBlockID = DecompiledBlock (loc_ip loc)
                    , _prevStmts  = Seq.empty
                    , _curX86State = initX86State loc
                    }
  trace ("Exploring " ++ showHex (loc_ip loc) "") $ do
  disassembleBlock' mem gs (loc_ip loc)

getExploreLocs :: X86State -> [ExploreLoc]
getExploreLocs s =
  case (s^.curIP, s^.x87StatusWord^.x87_top) of
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
            _ -> error "Frontier node does not end with FetchAndExecute."

disassembleBlock' :: Memory Word64
                  -> GenState -- ^ Initial state
                  -> CodeAddr
                  -> (PartialCFG, Set ExploreLoc)
disassembleBlock' mem gs0 addr = do
  case readInstruction mem addr of
    Left e -> error (show e)
    Right (i,next_ip) -> do
      -- Update current IP
      let gs1 = gs0 & curX86State . curIP .~ BVValue knownNat (toInteger next_ip)
      let pcfg = runX86Generator gs1 $ execInstruction i
      case Fold.toList (pcfg^.frontierBlocks) of
        -- If we have a single block that goes to the next instruction,
        -- then we just disassemble the next block and concat it with
        -- this one.
        [b] | FetchAndExecute s <- blockTerm b
            , [loc] <- getExploreLocs s
            , loc_ip loc == next_ip -> do
          let -- Clear frontier blocks
              pcfg1 = pcfg & frontierBlocks .~ Seq.empty
              -- Create state to continue from.
              gs1 = GenState { _partialCFG = pcfg1
                             , _curBlockID = blockLabel b
                             , _prevStmts  = Seq.fromList (blockStmts b)
                             , _curX86State = s
                             }
          disassembleBlock' mem gs1 next_ip
        block_list -> (mergeFrontier pcfg, getFrontierNext block_list)

recursiveDecent :: Memory Word64
                -> PartialCFG     -- ^ CFG generated so far.
                -> Set ExploreLoc -- ^ Set of locations explored so far.
                -> Set ExploreLoc -- ^ List of addresses to explore next.
                -> CFG
recursiveDecent mem pg1 explored frontier
    | Set.null frontier = pg1^.prevCFG
    | otherwise =
        let (loc,s) = Set.deleteFindMin frontier
            (pg2, next) = disassembleBlock mem pg1 loc
            explored' = Set.insert loc explored
            frontier' = Set.union s (Set.difference next explored')
         in recursiveDecent mem pg2 explored' frontier'

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> CFG
cfgFromAddress mem a = recursiveDecent mem pcfg Set.empty (Set.singleton loc)
  where pcfg = emptyPartialCFG
        loc = ExploreLoc { loc_ip = a
                         , loc_x87_top = 7
                         }

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
