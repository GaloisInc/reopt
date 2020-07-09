{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module VCGCommon
  (  -- * SMT
    Var
  , varTerm
  , bvbinary
  , bvdecimal
  , bvhexadecimal
    -- * Memory
  , ptrSort
  , memSort
  , writeBVLE
  ) where

import qualified Data.BitVector.Sized as BV
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Builder
import           Numeric.Natural
import qualified What4.Protocol.SMTLib2.Syntax as SMT

type Var = Text

varTerm :: Var -> SMT.Term
varTerm = SMT.T . Builder.fromText

-- | Sort for pointers
ptrSort :: SMT.Sort
ptrSort = SMT.bvSort 64

memSort :: SMT.Sort
memSort = SMT.arraySort ptrSort (SMT.bvSort 8)

bvbinary :: Integer -> Natural -> SMT.Term
bvbinary x w =
  case someNat w of
    Just (Some w') | Just LeqProof <- testLeq (knownNat @1) w' -> SMT.bvbinary w' (BV.mkBV w' x)
    _ -> error "bad width"

bvdecimal :: Integer -> Natural -> SMT.Term
bvdecimal x w =
  case someNat w of
    Just (Some w') | Just LeqProof <- testLeq (knownNat @1) w' -> SMT.bvdecimal w' (BV.mkBV w' x)
    _ -> error "bad width"

bvhexadecimal :: Integer -> Natural -> SMT.Term
bvhexadecimal x w =
  case someNat w of
    Just (Some w') | Just LeqProof <- testLeq (knownNat @1) w' -> SMT.bvhexadecimal w' (BV.mkBV w' x)
    _ -> error "bad width"

-- | Read a number of bytes as a bitvector.
-- Note. This refers repeatedly to ptr so, it should be a constant.
writeBVLE :: SMT.Term
          -> SMT.Term  -- ^ Address to write
          -> SMT.Term  -- ^ Value to write
          -> Natural -- ^ Number of bytes to write.
          -> SMT.Term
writeBVLE mem ptr0 val w
    | w == 0 = error $ "writeBVLE given zero bytes"
    | otherwise = go (w-1)
  where go :: Natural -> SMT.Term
        go 0 = SMT.store mem ptr0 (SMT.extract 7 0 val)
        go i =
          let ptr = SMT.bvadd ptr0 [bvdecimal (toInteger i) 64]
           in SMT.store (go (i-1)) ptr (SMT.extract (8*i+7) (8*i) val)
