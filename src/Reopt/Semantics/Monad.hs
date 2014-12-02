{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Semantics.Monad
 ( IsAssignable(..)
 , IsValue(..)
 , IsLeq(..)
 , BV
 , Value
 , Pred
 , Assignable
 , Semantics(..)
 , IsAssignableBV
 , Bits.Bits
 , (Bits..&.)
 ) where

import Control.Applicative
import Data.Bits as Bits
import GHC.TypeLits

import Lang.Crucible.Utils.Width

------------------------------------------------------------------------
-- InterruptNo

{-
newtype InterruptNo = InterruptNo { interruptNoIdx :: Word8 }

de :: InterruptNo
de = InterruptNo 0

db :: InterruptNo
db = InterruptNo 1
-}

data BV (n :: Nat)

class IsLeq (m :: Nat) (n :: Nat) where
  leqProof :: LeqProof m n

instance IsLeq 1 16 where
  leqProof = knownLeq
instance IsLeq 4 16 where
  leqProof = knownLeq
instance IsLeq 8 16 where
  leqProof = knownLeq

instance IsLeq 1 32 where
  leqProof = knownLeq
instance IsLeq 4 32 where
  leqProof = knownLeq
instance IsLeq 8 32 where
  leqProof = knownLeq

instance IsLeq 1 64 where
  leqProof = knownLeq
instance IsLeq 4 64 where
  leqProof = knownLeq
instance IsLeq 8 64 where
  leqProof = knownLeq

------------------------------------------------------------------------
-- SemanticsMonad

class IsAssignable v where
  af_flag :: v Bool
  cf_flag :: v Bool
  of_flag :: v Bool
  pf_flag :: v Bool
  sf_flag :: v Bool
  zf_flag :: v Bool

class IsValue v where
  false :: v Bool
  true  :: v Bool

  even_parity :: v (BV 8) -> v Bool

  msb :: IsLeq 1 n => v (BV n) -> v Bool

  is_zero :: v (BV n) -> v Bool

  -- | Return least-significant nibble (4 bits).
  least_nibble :: IsLeq 4 n => v (BV n) -> v (BV 4)

  -- | Return least-significant byte.
  least_byte   :: IsLeq 8 n => v (BV n) -> v (BV 8)

  sadd_overflows :: IsLeq 1 n => v (BV n) -> v (BV n) -> v Bool
  uadd_overflows :: v (BV n) -> v (BV n) -> v Bool

  sadc_overflows :: v (BV n) -> v (BV n) -> v Bool -> v Bool
  uadc_overflows :: v (BV n) -> v (BV n) -> v Bool -> v Bool

  -- | bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsf :: v (BV n) -> v (BV n)
  -- | bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsr :: v (BV n) -> v (BV n)

type family Value (m :: * -> *) :: * -> *
type family Assignable (m :: * -> *) :: * -> *

type Pred m = Value m Bool

class ( Applicative m
      , Monad m
      , IsAssignable (Assignable m)
      , IsValue (Value m)
      ) => Semantics m where
  set_undefined :: Assignable m Bool -> m ()

  get :: Assignable m tp -> m (Value m tp)
  (.=) :: Assignable m tp -> Value m tp -> m ()

type IsAssignableBV m n
   = ( IsValue (Value m)
     , Bits (Value m (BV n))
     , Num  (Value m (BV n))
     , Semantics m
     , IsLeq 1 n
     , IsLeq 4 n
     , IsLeq 8 n
     )