{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Reopt.Semantics.ConcreteState
    ( module Reopt.Semantics.ConcreteState
    , module Reopt.Semantics.BitVector
    ) where

import qualified Data.Map as M
import           Text.PrettyPrint.ANSI.Leijen ((<+>), Pretty(..), text)

import           Data.Parameterized.NatRepr
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import qualified Reopt.Machine.X86State as X
import           Reopt.Semantics.BitVector (BitVector, BV, bitVector, unBitVector)
import qualified Reopt.Semantics.BitVector as B
import qualified Data.BitVector as BV
import           Control.Monad.State
import           Control.Monad.Reader

import Data.Maybe (mapMaybe)

import Control.Lens

------------------------------------------------------------------------
-- Concrete values

data Value (tp :: Type) where
  Literal   :: BitVector n -> Value (BVType n)
  Undefined :: TypeRepr tp -> Value tp

instance Eq (Value tp) where
  Literal x == Literal y     = x == y
  Undefined _ == Undefined _ = True
  _ == _                     = False

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

instance X.PrettyRegValue Value where
  ppValueEq (N.FlagReg n) _ | not (n `elem` [0,2,4,6,7,8,9,10,11]) = Nothing
  ppValueEq (N.X87ControlReg n) _ | not (n `elem` [0,1,2,3,4,5,12]) = Nothing
  ppValueEq r v = Just $ text (show r) <+> text "=" <+> pretty v

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

asBV :: Value tp -> Maybe BV
asBV (Literal (unBitVector -> (_, bv))) = Just bv
asBV _ = Nothing

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
  Address :: NatRepr n         -- ^ Number of bits.
          -> BitVector 64      -- ^ Address of first byte.
          -> Address (BVType n)
type Address8 = Address (BVType 8)
type Value8 = Value (BVType 8)

instance Eq (Address n) where
  (Address _ x) == (Address _ y) = x == y

instance Ord (Address n) where
  compare (Address _ x) (Address _ y) = compare x y

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
  getReg :: N.RegisterName cl -> m (Value (N.RegisterType cl))
  -- | Set the value of a register.
  setReg :: N.RegisterName cl -> Value (N.RegisterType cl) -> m ()
  -- | Get the value of all registers.
  dumpRegs :: m (X.X86State Value)

class MonadMachineState m => FoldableMachineState m where
  -- fold across all known addresses
  foldMem8 :: (Address8 -> Value8 -> a -> a) -> a -> m a

type ConcreteMemory = M.Map Address8 Value8
type ConcreteState = StateT (ConcreteMemory, X.X86State Value)

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
      then error "getMem: requested number of bits is not a multiple of 8!"
      else natValue nr `div` 8

getMem8 :: MonadMachineState m => Address8 -> ConcreteState m Value8
getMem8 addr8 = do
  (mem,_) <- get
  case val mem of Undefined _ -> lift $ getMem addr8
                  res -> return res
  where
    val mem = case M.lookup addr8 mem of
      Just x -> x
      Nothing -> Undefined (BVTypeRepr knownNat)

instance MonadMachineState m => MonadMachineState (ConcreteState m) where
  getMem a@(Address nr _) = do
    vs <- mapM getMem8 $ byteAddresses a
    
    let bvs = mapMaybe asBV vs
    -- We can't directly concat 'vs' since we can't type the
    -- intermediate concatenations.
    let bv = foldl (BV.#) (BV.zeros 0) bvs -- Endianness?  are we backwards?
    -- Return 'Undefined' if we had any 'Undefined' values in 'vs'.
    return $ if length bvs /= length vs
             then Undefined (BVTypeRepr nr)
             else Literal (bitVector nr bv)

  setMem addr@Address{} val = 
    foldM (\_ (a,v) -> modify $ mapFst $ M.insert a v)  () (zip addrs $ reverse $ group (knownNat :: NatRepr 8) val) where
      mapFst f (a,b) = (f a, b)
      addrs = byteAddresses addr

  getReg reg = do 
    val <- liftM (^.(X.register reg)) dumpRegs
    case val 
      of Undefined _ -> lift $ getReg reg
         _ -> return val
      
  setReg reg val = modify $ mapSnd $ X.register reg .~ val
    where mapSnd f (a,b) = (a, f b)
  dumpRegs = liftM snd get

instance (MonadMachineState m) => MonadMachineState (StateT s m) where
  getMem = lift . getMem
  setMem addr val = lift $ setMem addr val
  getReg = lift . getReg
  setReg reg val = lift $ setReg reg val
  dumpRegs = lift dumpRegs

instance (MonadMachineState m) => MonadMachineState (ReaderT s m) where
  getMem = lift . getMem
  setMem addr val = lift $ setMem addr val
  getReg = lift . getReg
  setReg reg val = lift $ setReg reg val
  dumpRegs = lift dumpRegs

instance MonadMachineState m => FoldableMachineState (ConcreteState m) where
  foldMem8 f x = liftM (M.foldrWithKey f x . fst) get


