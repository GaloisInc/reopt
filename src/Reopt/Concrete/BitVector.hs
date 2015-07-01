{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Reopt.Concrete.BitVector
  (
  -- | The constructor is not exported; use 'bitVector' and 'unBitVector'.
    BitVector
  , bitVector
  , unBitVector
  -- | Constants.
  , false
  , true
  -- | Operations.
  , (#)
  , group
  , modify
  , nat
  , width
  -- | To construct 'BV.BV' values.
  , BV
  , BV.bitVec
  ) where

import           Data.BitVector (BV)
import qualified Data.BitVector as BV
import Data.Parameterized.NatRepr
import GHC.TypeLits

------------------------------------------------------------------------
-- Bitvector type

-- | An opaque type of 'BV.BV's with type-level length indices.
data BitVector (n :: Nat) where
  BitVector :: NatRepr n -> BV -> BitVector n

-- | Smart constructor that enforces term and type level width agreement.
--
-- The underlying 'BV.BV' width must agree with the 'NatRepr n' width,
-- and there is a *partial* check that the given number of bits can
-- represent the given value.
--
-- To construct a 'BitVector' from an integer @i@ and a 'NatRepr' @nr@
-- do @'bitVector' nr ('BV.bitVec' ('natValue' nr) i)@. This should be
-- an uncommon operation; in most cases operations on 'BV' will be
-- used to construct new 'BV's from existing 'BV's.
bitVector :: NatRepr n -> BV -> BitVector n
bitVector nr v =
  if natValue nr == fromIntegral (BV.width v)
  -- Internally, 'BV' attempts to convert all values to non-negative,
  -- by adding negative values to @2^n@. We first check if this
  -- conversion to unsigned overflowed.
  --
  -- We don't detect too-small negative values in
  --
  -- @[-2^n, -2^(n-1) - 1]@
  --
  -- or too-large *signed* postive values in
  --
  -- @[2^(n-1), 2^n - 1]@
  --
  -- Note that, for positive values, being "signed" is determined by
  -- the the intent of the caller and is not tracked anywhere.
  then if BV.nat v < 0 || BV.nat v >= 2^(BV.width v)
       then error "bitVector: width is too small! Calling code is buggy!"
       else BitVector nr v
  else error "bitVector: type-level and term-level bit widths disagree! Calling code is buggy!"

-- | Deconstructor for 'BitVector'.
unBitVector :: BitVector n -> (NatRepr n, BV)
unBitVector (BitVector nr v) = (nr, v)

instance Eq (BitVector n) where
  BitVector _ x == BitVector _ y = x == y

instance Show (BitVector n) where
  show (BitVector _ x) = show x

instance Ord (BitVector n) where
  compare (BitVector _ x) (BitVector _ y) = compare x y

------------------------------------------------------------------------
-- Operations on 'BitVector's

width :: BitVector n -> NatRepr n
width (BitVector nr _) = nr

-- | Concatenate.
(#) :: BitVector n1 -> BitVector n2 -> BitVector (n1 + n2)
(BitVector nr1 bv1) # (BitVector nr2 bv2) =
  bitVector (addNat nr1 nr2) (bv1 BV.# bv2)

-- | Group into chunks of 'n1' bits.
--
-- If 'n1' does not divide 'n2', then the first chunk will be
-- zero-extended.
group :: NatRepr n1 -> BitVector n2 -> [BitVector n1]
group nr (BitVector _ bv) =
  [ bitVector nr bv' | bv' <- BV.group (natValue nr) bv ]

-- | Modify the underlying 'BV'.
--
-- The modification must not change the width.
modify :: (BV -> BV) -> BitVector n -> BitVector n
modify f (BitVector nr bv) = bitVector nr (f bv)

-- | Return the unsigned value of the underlying 'BV'.
nat :: BitVector n -> Integer
nat (BitVector _ bv) = BV.nat bv

-- | Booleans.
true, false :: BitVector 1
true = bitVector knownNat (BV.bitVec 1 1)
false = bitVector knownNat (BV.bitVec 1 0)
