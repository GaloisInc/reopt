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
    -- debugging
  , BlockLabel(..)
  , readInstruction
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.Foldable as Fold
import           Data.Parameterized.NatRepr
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen (text, colon, (<>), (<+>))
  -- hack for instruction comment

import           Unsafe.Coerce (unsafeCoerce)

import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.Some
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

------------------------------------------------------------------------
-- Location

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
  bvSplit v = (upperHalf v, lowerHalf v)

  -- | Rotations
  -- bvRol, bvRor :: v (BVType n) -> v (BVType log_n) -> v (BVType n)

  -- | Shifts, the semantics is undefined for shifts >= the width of the first argument
  -- bvShr, bvSar, bvShl :: v (BVType n) -> v (BVType log_n) -> v (BVType n)
  bvShr x y = app $ BVShr (exprWidth x) x y
  bvSar x y = app $ BVSar (exprWidth x) x y
  bvShl x y = app $ BVShl (exprWidth x) x y

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
       , _cachedApps   :: !(MapF (App Value) Assignment)
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
           , _cachedApps = MapF.empty
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

-- | Cache mapping applications to assignment.
cachedApps :: Simple Lens GenState (MapF (App Value) Assignment)
cachedApps = lens _cachedApps (\s v -> s { _cachedApps = v })

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
                  , blockCache = s^.cachedApps
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

addAssignment :: AssignRhs tp -> X86Generator r (Assignment tp)
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
    unop f sz (BVValue _ l)  = Just $ BVValue sz ( (f l) .&. mask)
      where mask = 2^(widthVal sz) - 1
    unop _ _ _ = Nothing

    binop :: (tp ~ BVType n) => (Integer -> Integer -> Integer)
             -> NatRepr n -> Value tp -> Value tp -> Maybe (Value tp)
    binop f sz (BVValue _ l) (BVValue _ r) = Just $ BVValue sz ( (f l r) .&. mask)
      where mask = 2^(widthVal sz) - 1
    binop _ _ _ _ = Nothing

evalApp :: App Value tp  -> X86Generator r (Value tp)
evalApp a = do
  case constPropagate a of
    Nothing -> do
      m <- modGenState $ use cachedApps
      case MapF.lookup a m of
        Nothing -> do
          r <- addAssignment (EvalApp a)
          modGenState $ cachedApps %= MapF.insert a r
          return (AssignedValue r)
        Just r -> return (AssignedValue r)
    Just v  -> return v

eval :: Expr tp -> X86Generator r (Value tp)
eval (ValueExpr v) = return v
eval (AppExpr a) = evalApp =<< traverseApp eval a

-- | Type for addresses.
type AddrExpr = Expr (BVType 64)

type ImpLocation tp = S.Location AddrExpr tp

getLoc :: ImpLocation tp -> X86Generator r (Expr tp)
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
       --   ValueExpr <$> eval (truncExpr e knownNat)
       _ -> modState $ ValueExpr <$> use (register r)

    S.LowerHalf l -> lowerHalf <$> getLoc l
    S.UpperHalf l -> upperHalf <$> getLoc l

valueAsApp :: Value tp -> Maybe (App Value tp)
valueAsApp (AssignedValue (Assignment _ (EvalApp a))) = Just a
valueAsApp _ = Nothing

asApp :: Expr tp -> Maybe (App Expr tp)
asApp (AppExpr   a) = Just a
asApp (ValueExpr v) = mapApp ValueExpr <$> valueAsApp v

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
             fail $ "On x86-64 segment registers other than fs and gs may not be set."
       _ -> modState $ register r .= v

    S.LowerHalf l -> do
      b <- getLoc l
      upper <- eval (upperHalf b)
      setLoc l =<< evalApp (ConcatV (valueWidth v) v upper)
    S.UpperHalf l -> do
      b <- getLoc l
      lower <- eval (lowerHalf b)
      setLoc l =<< evalApp (ConcatV (valueWidth v) lower v)

    S.X87StackRegister _ -> do
      error "setLoc X87StackRegister undefined"

instance S.Semantics (X86Generator PartialCFG) where
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
          let last_block_id = s^.curBlockID
          let last_block_stmts = Fold.toList $ s^.prevStmts

          -- Label for true branch
          let t_cfg = s^.partialCFG
          let true_block_id = GeneratedBlock (blockParent last_block_id)
                              (t_cfg^.nextBlockID)
          let t_start = emptyGenState (t_cfg & nextBlockID +~ 1) true_block_id (s^.curX86State)
          -- Get state after running t followed by c
          let t_end :: PartialCFG
              t_end = unX86G t c t_start
          -- Label for false branch
          let false_block_id = GeneratedBlock (blockParent last_block_id)
                               (t_end^.nextBlockID)
          -- Get block for code before branch now that we can get label
          -- for false branch.
          let last_block = Block { blockLabel = last_block_id
                                 , blockStmts = last_block_stmts
                                 , blockCache = s^.cachedApps
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
                 -> PartialCFG
                 -> ExploreLoc -- ^ Location to explore from.
                 -> Either (MemoryError Word64) (PartialCFG, Set ExploreLoc, CodeAddr)
disassembleBlock mem g loc =
  let gs = GenState { _partialCFG = g
                    , _curBlockID = DecompiledBlock (loc_ip loc)
                    , _prevStmts  = Seq.empty
                    , _cachedApps = MapF.empty
                    , _curX86State = initX86State loc
                    }
  in trace ("Exploring " ++ showHex (loc_ip loc) "")
     $ disassembleBlock' mem gs (loc_ip loc)

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
            _ -> error "Frontier node does not end with FetchAndExecute."

disassembleBlock' :: Memory Word64
                  -> GenState -- ^ Initial state
                  -> CodeAddr
                  -> Either (MemoryError Word64) (PartialCFG, Set ExploreLoc, CodeAddr)
disassembleBlock' mem gs0 addr = do
  (i, next_ip) <- readInstruction mem addr
  -- Update current IP
  let gs1 = gs0 & curX86State . curIP .~ BVValue knownNat (toInteger next_ip)
  let pcfg = runX86Generator gs1 $ do
               let line = text (showHex addr "") <> colon <+> Flexdis.ppInstruction addr i
               addStmt (Comment (show line))
               execInstruction i
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
              gs2 = GenState { _partialCFG = pcfg1
                             , _curBlockID = blockLabel b
                             , _prevStmts  = Seq.fromList (blockStmts b)
                             , _cachedApps = blockCache b
                             , _curX86State = s
                             }
          disassembleBlock' mem gs2 next_ip
    block_list -> return (mergeFrontier pcfg, getFrontierNext block_list, next_ip)

-- FIXME: move
newtype Hex = Hex CodeAddr
              deriving (Eq, Ord)

mkHex :: CodeAddr -> Hex
mkHex = Hex

instance Show Hex where
  show (Hex v) = showHex v ""

recursiveDescent :: Memory Word64
                -> PartialCFG     -- ^ CFG generated so far.
                -> Set CodeAddr   -- ^ Set of addresses after blocks we stopped at.
                -> Set ExploreLoc -- ^ Set of locations explored so far.
                -> Set ExploreLoc -- ^ List of addresses to explore next.
                -> (CFG, Set CodeAddr)
recursiveDescent mem pg1 ends explored frontier
    | Set.null frontier = (pg1^.prevCFG, ends)
    | otherwise =
        let (loc,s)         = Set.deleteFindMin frontier
            explored'       = Set.insert loc explored
            go pg ends' frontier' = recursiveDescent mem pg ends' explored' frontier'
        in
        case disassembleBlock mem pg1 loc of
          Left  e  -> trace ("Skipping " ++ showHex (loc_ip loc) (": " ++ show e))
                      $ go pg1 ends s
          Right (pg2, next, end) ->
            let guesses   = discoverCodePointers mem (pg2^.prevCFG)
                            (DecompiledBlock (loc_ip loc)) -- FIXME
                allNext   = trace ("At " ++ show (mkHex $ loc_ip loc)
                                   ++ " guessing pointers "
                                   ++ show (Set.map mkHex guesses)
                                   ++ " with next "
                                   ++ show (Set.map (mkHex . loc_ip)  next))
                            $ next `Set.union` Set.map locFromGuess guesses
            in go pg2 (Set.insert end ends) (Set.union s (Set.difference allNext explored'))

cfgFromAddress :: Memory Word64
                  -- ^ Memory to use when decoding instructions.
               -> CodeAddr
                  -- ^ Location to start disassembler form.
               -> (CFG, Set CodeAddr)
cfgFromAddress mem a = -- recursiveDescent mem pcfg Set.empty (Set.singleton loc)
  -- XXXXXXXX FIXME: frame_dummy has a call to 0 for some reason, so we pretend we have seen it
  -- this is a giant hack.
  recursiveDescent mem pcfg Set.empty (Set.singleton (rootLoc 0)) (Set.singleton loc)
  where pcfg = emptyPartialCFG
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
