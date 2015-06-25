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
  -- | Operations.
  , (#)
  , group
  , modify
  , width
  -- | To construct 'BV.BV' values.
  , BV
  , BV.bitVec
  , BV.nat
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
-- The underlying 'BV.BV' width must agree with the 'NatRepr n' width.
--
-- To construct a 'BitVector' from an integer @i@ and a 'NatRepr' @nr@
-- do @'bitVector' nr ('BV.bitVec' ('natValue' nr) i)@. This should be
-- an uncommon operation; in most cases operations on 'BV.BV' will be
-- used to construct new 'BV.BV's from existing 'BV.BV's.
bitVector :: NatRepr n -> BV -> BitVector n
bitVector nr v =
  if natValue nr == fromIntegral (BV.width v)
  then BitVector nr v
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
