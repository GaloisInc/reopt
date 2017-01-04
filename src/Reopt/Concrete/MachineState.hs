{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.Concrete.MachineState
    ( module Reopt.Concrete.MachineState
    , module Reopt.Concrete.BitVector
    ) where

import           Control.Lens
import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer.Strict
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import           Text.PrettyPrint.ANSI.Leijen ((<+>), Pretty(..), text)

import qualified Data.BitVector as BV
import           Data.Macaw.CFG (RegState, mkRegState, PrettyRegValue(..))
import           Data.Macaw.Types
import           Reopt.Concrete.BitVector (BitVector, BV, bitVector, nat, unBitVector)
import qualified Reopt.Concrete.BitVector as B
import qualified Reopt.Machine.X86State as X
import           Reopt.Semantics.Monad (Primitive, Segment)

------------------------------------------------------------------------
-- Concrete values

data Value (tp :: Type) where
  Literal   :: BitVector n -> Value (BVType n)
  Undefined :: TypeRepr tp -> Value tp

instance Eq (Value tp) where
  Literal x == Literal y     = x == y
  Undefined _ == Undefined _ = True
  _ == _                     = False

instance EqF Value where
  Literal x `eqF` Literal y | Just Refl <- testEquality (B.width x) (B.width y)    = x == y
  Undefined tr1 `eqF` Undefined tr2 | Just Refl <- testEquality tr1 tr2 = True
  _ `eqF` _                     = False

instance TestEquality Value where
  Literal x `testEquality` Literal y = do
    Refl <- testEquality x y
    return Refl
  Undefined x `testEquality` Undefined y = do
    Refl <- testEquality x y
    return Refl
  _ `testEquality` _                     = Nothing

instance OrdF Value where
  compareF (Literal x) (Literal y) =
    case compareF x y of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF
  compareF (Undefined _) (Literal _) = LTF
  compareF (Literal _) (Undefined _) = GTF
  compareF (Undefined x) (Undefined y) =
    case compareF x y of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF


-- | Equal or at least one side undefined.
--
-- This is not transitive, so it doesn't make sense as the 'Eq'
-- instance.
equalOrUndef :: Value tp -> Value tp -> Bool
Literal x `equalOrUndef` Literal y = x == y
_ `equalOrUndef` _ = True

instance Ord (Value tp) where
  compare (Literal x) (Literal y) = compare x y
  compare (Undefined _) (Literal _) = LT
  compare (Literal _) (Undefined _) = GT
  compare (Undefined _) (Undefined _) = EQ

instance Show (Value tp) where
  show = show . pretty

instance Pretty (Value tp) where
  pretty (Literal x)    = text $ show x
  pretty (Undefined _) = text $ "Undefined"

instance PrettyRegValue X.X86Reg Value where
  ppValueEq (X.X86_FlagReg n) _ | not (n `elem` [0,2,4,6,7,8,9,10,11]) = Nothing
  ppValueEq (X.X87_ControlReg n) _ | not (n `elem` [0,1,2,3,4,5,12]) = Nothing
  ppValueEq r v = Just $ text (show r) <+> text "=" <+> pretty v

------------------------------------------------------------------------
-- Constants

true, false :: Value BoolType
true = Literal B.true
false = Literal B.false

------------------------------------------------------------------------
-- 'Value' combinators

-- | Lift a computation on 'BV's to a computation on 'Value's.
--
-- The result-type 'NatRepr' is passed separately and used to
-- construct the result 'Value'.
liftValue :: (BV -> BV)
           -> NatRepr n2
           -> Value (BVType n1)
           -> Value (BVType n2)
liftValue f nr (asBV -> Just v) =
  Literal $ bitVector nr (f v)
liftValue _ nr _ = Undefined (BVTypeRepr nr)

liftValue2 :: (BV -> BV -> BV)
           -> NatRepr n3
           -> Value (BVType n1)
           -> Value (BVType n2)
           -> Value (BVType n3)
liftValue2 f nr (asBV -> Just bv1) (asBV -> Just bv2) =
  Literal $ bitVector nr (f bv1 bv2)
liftValue2 _ nr _ _ = Undefined (BVTypeRepr nr)

liftValue3 :: (BV -> BV -> BV -> BV)
           -> NatRepr n4
           -> Value (BVType n1)
           -> Value (BVType n2)
           -> Value (BVType n3)
           -> Value (BVType n4)
liftValue3 f nr (asBV -> Just bv1) (asBV -> Just bv2) (asBV -> Just bv3) =
  Literal $ bitVector nr (f bv1 bv2 bv3)
liftValue3 _ nr _ _ _ = Undefined (BVTypeRepr nr)

-- Lift functions with the possibility of an undefined return value
liftValueMaybe :: (BV -> Maybe BV)
               -> NatRepr n2
               -> Value (BVType n1)
               -> Value (BVType n2)
liftValueMaybe f nr (asBV -> Just v) = case f v of
  Nothing -> Undefined (BVTypeRepr nr)
  Just bv -> Literal $ bitVector nr bv
liftValueMaybe _ nr _ = Undefined (BVTypeRepr nr)

liftValueMaybe2 :: (BV -> BV -> Maybe BV)
               -> NatRepr n3
               -> Value (BVType n1)
               -> Value (BVType n1)
               -> Value (BVType n3)
liftValueMaybe2 f nr (asBV -> Just v1) (asBV -> Just v2) =
  case f v1 v2 of
    Nothing -> Undefined (BVTypeRepr nr)
    Just bv -> Literal $ bitVector nr bv
liftValueMaybe2 _ nr _ _ = Undefined (BVTypeRepr nr)

liftValueSame :: (BV -> BV)
              -> Value (BVType n)
              -> Value (BVType n)
liftValueSame f (Literal (unBitVector -> (nr, v))) =
  Literal $ bitVector nr (f v)
liftValueSame _ u@(Undefined _) = u

asBV :: Value tp -> Maybe BV
asBV (Literal (unBitVector -> (_, bv))) = Just bv
asBV _ = Nothing

asTypeRepr :: Value tp -> TypeRepr tp
asTypeRepr (Literal (unBitVector -> (nr, _))) = BVTypeRepr nr
asTypeRepr (Undefined tr)                     = tr

------------------------------------------------------------------------
-- Operations on 'Value's

width :: Value (BVType n) -> NatRepr n
width (Literal bv) = B.width bv
width (Undefined tr) = type_width tr

-- | Concatenate two 'Value's.
(#) :: Value (BVType n1) -> Value (BVType n2) -> Value (BVType (n1 + n2))
Literal b1 # Literal b2 = Literal (b1 B.# b2)
v1 # v2 = Undefined (BVTypeRepr $ addNat (width v1) (width v2))

-- | Group a 'Value' in size 'n1' chunks.
--
-- If 'n1' does not divide 'n2', then the first chunk will be
-- zero-extended.
group :: NatRepr n1 -> Value (BVType n2) -> [Value (BVType n1)]
group nr (Literal b) = [ Literal b' | b' <- B.group nr b ]
group nr v@(Undefined _) = replicate count (Undefined (BVTypeRepr nr))
  where
    -- | The ceiling of @n2 / n1@.
    count = fromIntegral $
      (natValue (width v) + natValue nr - 1) `div` natValue nr

-- | Modify the underlying 'BV'.
--
-- The modification must not change the width.
modifyValue :: (BV -> BV) -> Value (BVType n) -> Value (BVType n)
modifyValue f (Literal b) = Literal (B.modify f b)
modifyValue _ v@(Undefined _) = v

------------------------------------------------------------------------
-- Machine state monad

data Address tp where
  Address :: NatRepr n         -- /\ Number of bits.
          -> BitVector 64      -- /\ Address of first byte.
          -> Address (BVType n)
type Address8 = Address (BVType 8)
type Value8 = Value (BVType 8)

instance Eq (Address n) where
  (Address _ x) == (Address _ y) = x == y

instance Ord (Address n) where
  compare (Address _ x) (Address _ y) = compare x y

instance Show (Address n) where
  show = show . pretty

instance Pretty (Address n) where
  pretty (Address _ bv) = text $ show bv

modifyAddr :: (BV -> BV) -> Address (BVType n) -> Address (BVType n)
modifyAddr f (Address nr bv) = Address nr (B.modify f bv)

-- | Operations on machine state.
--
-- We restrict the operations to bytes, so that the underlying memory
-- map, as returned by 'dumpMem8', can be implemented in a straight
-- forward way. We had considered making all the operations
-- polymorphic in their bitwidth, but as Robert pointed out this would
-- lead to aliasing concerns for the proposed memory map
--
-- > dumpMem :: MapF Adress Value
--
-- The bitwidth-polymorphic operations can then be defined in terms of
-- the 8-bit primitive operations.
class Monad m => MonadMachineState m where
  -- | Get a byte.
  getMem :: Address tp -> m (Value tp)
  -- | Set a byte.
  setMem :: Address tp -> Value tp -> m ()
  -- | Get the value of a register.
  getReg :: X.X86Reg tp -> m (Value tp)
  -- | Set the value of a register.
  setReg :: X.X86Reg tp -> Value tp -> m ()
  -- | Get the value of all registers.
  dumpRegs :: m (RegState X.X86Reg Value)
  -- | Update the state for a primitive.
  primitive :: Primitive -> m ()
  -- | Return the base address of the given segment.
  getSegmentBase :: Segment -> m (Value (BVType 64))

class MonadMachineState m => FoldableMachineState m where
  -- fold across all known addresses
  foldMem8 :: (Address8 -> Value8 -> a -> m a) -> a -> m a

type ConcreteMemory = M.Map Address8 Value8

newtype ConcreteStateT m a =
  ConcreteStateT {
    unConcreteStateT :: StateT (ConcreteMemory, RegState X.X86Reg Value) m a}
  deriving ( MonadState (ConcreteMemory, RegState X.X86Reg Value)
           , Functor
           , MonadTrans
           , Applicative
           , Monad
           )

runConcreteStateT :: ConcreteStateT m a
                  -> ConcreteMemory
                  -> RegState X.X86Reg Value
                  -> m (a, (ConcreteMemory ,RegState X.X86Reg Value))
runConcreteStateT (ConcreteStateT{unConcreteStateT = m}) mem regs =
  runStateT m (mem, regs)

-- | Convert address of 'n*8' bits into 'n' sequential byte addresses.
byteAddresses :: Address tp -> [Address8]
byteAddresses (Address nr bv) = addrs
  where
    -- | The 'count'-many addresses of sequential bytes composing the
    -- requested value of @count * 8@ bit value.
    addrs :: [Address8]
    addrs = [ Address knownNat $ B.modify (+ mkBv k) bv | k <- [0 .. count - 1] ]
    -- | Make a 'BV' with value 'k' using minimal bits.
    mkBv :: Integer -> BV
    mkBv k = B.bitVec 64 k
    count =
      if natValue nr `mod` 8 /= 0
      then error "byteAddresses: requested number of bits is not a multiple of 8!"
      else natValue nr `div` 8

-- TODO(conathan): weaken the constraint to @MonadReadMachineState m
-- =>@, where @MonadReadMachineState@ is a new class that only
-- includes the read operations. This eliminates the unimplemented
-- write operations in @PTraceMachineState@.
--
-- Also, rename this function to make it clear that it reads the
-- underlying state when its cache does not include the requested
-- value.
--
-- Also, this looks buggy: if we have an undefined value in our map,
-- then @val mem@ returns @Undefined@, and so we call @lift $ getMem
-- addr8@ on the underlying monad. But if we have @Undefined@ in our
-- map, then we probably want to keep it that way: it's undefined for
-- a reason. But this reraises the issue of conflating undefined
-- values with unknown values, which is also happening in my treatment
-- of primitives (I think I noted elsewhere that what I should really
-- do is give the register state a separate "needs to be reread
-- value").
getMem8 :: MonadMachineState m => Address8 -> ConcreteStateT m Value8
getMem8 addr8 = do
  (mem,_) <- get
  case val mem of Undefined _ -> lift $ getMem addr8
                  res -> return res
  where
    val mem = case M.lookup addr8 mem of
      Just x -> x
      Nothing -> Undefined (BVTypeRepr knownNat)

instance MonadMachineState m => MonadMachineState (ConcreteStateT m) where
  getMem a@(Address nr _) = do
    vs <- mapM getMem8 $ byteAddresses a

    let bvs = mapMaybe asBV vs
    -- We can't directly concat 'vs' since we can't type the
    -- intermediate concatenations.
    --
    -- The 'BV.#' is big endian -- the higher-order bits come first --
    -- so @flip BV.#@ is little endian, which is consistent with our
    -- list of values 'bvs' read in increasing address order.
    let bv = foldl (flip (BV.#)) (BV.zeros 0) bvs
    -- Return 'Undefined' if we had any 'Undefined' values in 'vs'.
    return $ if length bvs /= length vs
             then Undefined (BVTypeRepr nr)
             else Literal (bitVector nr bv)

  setMem addr@Address{} val =
    foldM (\_ (a,v) -> modify $ mapFst $ M.insert a v)  () (zip addrs $ reverse $ group (knownNat :: NatRepr 8) val) where
      mapFst f (a,b) = (f a, b)
      addrs = byteAddresses addr

  getReg reg = liftM (^.(X.boundValue reg)) dumpRegs

  -- TODO(conathan): make the concrete state a record with a lens and
  -- eliminate the tuple mapping stuff.
  setReg reg val = modify $ mapSnd $ X.boundValue reg .~ val
    where mapSnd f (a,b) = (a, f b)

  dumpRegs = liftM snd get

  -- | We implement primitives by assuming anything could have happened.
  --
  -- I.e., we forget everything we know about the machine state.
  --
  -- TODO(conathan): this is probably overly lossy: the 'Undefined's
  -- will persist. Instead, we could do the equivalent of setting
  -- memory to 'M.empty', i.e., we could force the register state to
  -- be reread. I removed some other code that caused 'Undefined' in a
  -- reg to turn into a read of the hardware.
  primitive _ = do
    let regs = mkRegState (\rn -> Undefined (typeRepr rn))
    let mem = M.empty
    put (mem, regs)

  getSegmentBase = lift . getSegmentBase

instance MonadMachineState m => MonadMachineState (ExceptT e m) where
  getMem = lift . getMem
  setMem addr val = lift $ setMem addr val
  getReg = lift . getReg
  setReg reg val = lift $ setReg reg val
  dumpRegs = lift dumpRegs
  primitive = lift . primitive
  getSegmentBase = lift . getSegmentBase

instance (MonadMachineState m) => MonadMachineState (StateT s m) where
  getMem = lift . getMem
  setMem addr val = lift $ setMem addr val
  getReg = lift . getReg
  setReg reg val = lift $ setReg reg val
  dumpRegs = lift dumpRegs
  primitive = lift . primitive
  getSegmentBase = lift . getSegmentBase

instance (MonadMachineState m) => MonadMachineState (ReaderT s m) where
  getMem = lift . getMem
  setMem addr val = lift $ setMem addr val
  getReg = lift . getReg
  setReg reg val = lift $ setReg reg val
  dumpRegs = lift dumpRegs
  primitive = lift . primitive
  getSegmentBase = lift . getSegmentBase

instance (Monoid w, MonadMachineState m) => MonadMachineState (WriterT w m) where
  getMem = lift . getMem
  setMem addr val = lift $ setMem addr val
  getReg = lift . getReg
  setReg reg val = lift $ setReg reg val
  dumpRegs = lift dumpRegs
  primitive = lift . primitive
  getSegmentBase = lift . getSegmentBase

instance MonadMachineState m => FoldableMachineState (ConcreteStateT m) where
  foldMem8 f x = do
    (mem, _) <- get
    M.foldrWithKey (\k v m -> do m' <- m; f k v m') (return x) mem

newtype NullMachineState a = NullMachineState {unNullMachineState :: Identity a}
 deriving (Functor, Applicative, Monad)

runNullMachineState :: NullMachineState a -> a
runNullMachineState (NullMachineState {unNullMachineState = Identity x}) = x

instance MonadMachineState NullMachineState where
  -- | Get a byte.
  getMem (Address w _) = NullMachineState {unNullMachineState = Identity $
    Literal $ bitVector w $ BV.bitVec (fromIntegral $ natValue w) (0 :: Int)}
  -- | Set a byte.
  setMem _ _ = NullMachineState {unNullMachineState = (Identity ())}
  -- | Get the value of a register.
  getReg reg =
    case typeRepr reg of
      BVTypeRepr w ->
        NullMachineState
        { unNullMachineState = Identity $
            Literal $ bitVector w $ BV.bitVec (fromIntegral $ natValue w) (0 :: Int)
        }
  -- | Set the value of a register.
  setReg _ _ = NullMachineState {unNullMachineState = (Identity ())}
  -- | Get the value of all registers.
  dumpRegs = NullMachineState
    { unNullMachineState = Identity $ mkRegState $ Undefined . typeRepr
    }
  -- | Update the state for a primitive.
  primitive _ = NullMachineState {unNullMachineState = (Identity ())}
  -- | Return the base address of the given segment.
  getSegmentBase _ = NullMachineState {unNullMachineState = (Identity (Undefined knownType))}
