{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Reopt.Semantics.BitVector where

import qualified Data.BitVector as BV
import Data.Parameterized.NatRepr
import GHC.TypeLits

data BitVector (n :: Nat) where
  BitVector :: NatRepr n -> BV.BitVector -> BitVector n

mkBitVector :: (Integral i, KnownNat n) => i -> BitVector n
mkBitVector i = 
  BitVector width $  BV.bitVec (widthVal width) i
  where
    width = knownNat

instance Eq (BitVector n) where
  BitVector _ x == BitVector _ y = x == y

instance Show (BitVector n) where
  show (BitVector _ x) = show x
