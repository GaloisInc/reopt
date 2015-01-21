------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.Monad
-- Description      : This defines the typeclasses that must be implemented
--                    to obtain semantics from x86 instructions.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- Deifnes typeclasses used to implement x86 instruction semantics.
------------------------------------------------------------------------
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
 ( IsLocation(..)
 , IsValue(..)
 , IsLeq(..)
 , ValueType(..)
 , Value
 , Pred
 , Location
 , Semantics(..)
 , IsLocationBV
 , Bits.Bits
 , Bits.complement
 , (Bits..&.)
 , Flexdis86.Reg64
 ) where

import Control.Applicative
import Data.Bits as Bits
import GHC.TypeLits

import Flexdis86.InstructionSet as Flexdis86 (Reg64, XMMReg)

--import Lang.Crucible.Utils.Width (LeqProof, knownLeq)

------------------------------------------------------------------------
-- IsLeq

class IsLeq (m :: Nat) (n :: Nat) where
--  leqProof :: LeqProof m n

instance IsLeq 1 16 where
--  leqProof = knownLeq
instance IsLeq 4 16 where
--  leqProof = knownLeq
instance IsLeq 8 16 where
--  leqProof = knownLeq

instance IsLeq 1 32 where
--  leqProof = knownLeq
instance IsLeq 4 32 where
--  leqProof = knownLeq
instance IsLeq 8 32 where
--  leqProof = knownLeq

instance IsLeq 1 64 where
--  leqProof = knownLeq
instance IsLeq 4 64 where
--  leqProof = knownLeq
instance IsLeq 8 64 where
--  leqProof = knownLeq

------------------------------------------------------------------------
-- SemanticsMonad

data ValueType
  = BVType GHC.TypeLits.Nat
  | BoolType
    -- | A 64-bit floating point value in IEEE format.
  | DoubleType

-- | @IsValue@ is a class used to define types expressions.
class ( Num (v DoubleType)
      , Bits (v BoolType)
      )
      => IsValue (v  :: ValueType -> *) where
  false :: v BoolType
  true  :: v BoolType

  -- | Return true if value contains an even number of true bits.
  even_parity :: v (BVType 8) -> v BoolType

  -- | Return most significant bit of number.
  msb :: IsLeq 1 n => v (BVType n) -> v BoolType

  -- | Return true if value is zero.
  is_zero :: v (BVType n) -> v BoolType

  -- | Return least-significant nibble (4 bits).
  least_nibble :: IsLeq 4 n => v (BVType n) -> v (BVType 4)

  -- | Reverse the bytes in a bitvector expression.
  -- The parameter n should be a multiple of 8.
  reverse_bytes :: v (BVType n) -> v (BVType n)

  -- | Return least-significant byte.
  least_byte   :: IsLeq 8 n => v (BVType n) -> v (BVType 8)

  -- | Return true expression is signed add overflows.  See
  -- @sadc_overflows@ for definition.
  sadd_overflows :: IsLeq 1 n => v (BVType n) -> v (BVType n) -> v BoolType
  sadd_overflows x y = sadc_overflows x y false

  -- | Return true expression is unsigned add overflows.  See
  -- @sadc_overflows@ for definition.
  uadd_overflows :: v (BVType n) -> v (BVType n) -> v BoolType
  uadd_overflows x y = uadc_overflows x y false

  -- | Return true expression if a signed add-with carry would overflow.
  -- This holds if the sign bits of the arguments are the same, and the sign
  -- of the result is different.
  sadc_overflows :: v (BVType n) -> v (BVType n) -> v BoolType -> v BoolType

  -- | Return true expression if a unsigned add-with carry would overflow.
  uadc_overflows :: v (BVType n) -> v (BVType n) -> v BoolType -> v BoolType

  -- | bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsf :: v (BVType n) -> v (BVType n)
  -- | bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsr :: v (BVType n) -> v (BVType n)

-- | This defines expressions that need to be assignable.
class IsLocation (v :: ValueType -> *) where
  af_flag :: v BoolType
  cf_flag :: v BoolType
  df_flag :: v BoolType
  of_flag :: v BoolType
  pf_flag :: v BoolType
  sf_flag :: v BoolType
  zf_flag :: v BoolType

  -- | The bits 0 to 63 stored as a floating point double.
  xmm_low :: XMMReg -> v DoubleType

  -- | Return the high 32-bit of the location.
  low_dword :: Reg64 -> v (BVType 32)

  -- | Return the high 32-bit of the location.
  high_dword :: Reg64 -> v (BVType 32)

-- | This returns the type associated with values that can be read
-- for the semantics monad.
type family Value (m :: * -> *) :: ValueType -> *

-- | This returns the type associated with values that can be read
-- or assigned for the semantics monad.
type family Location (m :: * -> *) :: ValueType -> *

type Pred m = Value m BoolType

-- | The Semantics Monad defines all the operations needed for the x86
-- semantics.
class ( Applicative m
      , Monad m
      , IsLocation (Location m)
      , IsValue (Value m)
      ) => Semantics m where
  -- | Mark a Boolean variable as undefined.
  set_undefined :: Location m BoolType -> m ()

  -- | Read from the given location.
  get :: Location m tp -> m (Value m tp)
  -- | Assign a value to alocation.
  (.=) :: Location m tp -> Value m tp -> m ()

  -- | Modify the value at a location
  modify :: Location m tp -> (Value m tp -> Value m tp) -> m ()
  modify r f = do
    x <- get r
    r .= f x

  -- | Perform an if-then-else
  ifte_ :: Value m BoolType -> m () -> m () -> m ()

  -- | Run a step if condition holds.
  when_ :: Value m BoolType -> m () -> m ()
  when_ p x = ifte_ p x (return ())

-- | IsAssignmableBV m n is a constraint used to indicate that @m@ implements
-- Semantics, and @Value m (BV n)@ supports the operations used to assign
-- registers.
type IsLocationBV m n
   = ( IsValue (Value m)
     , Bits (Value m (BVType n))
     , Num  (Value m (BVType n))
     , Semantics m
     , IsLeq 1 n
     , IsLeq 4 n
     , IsLeq 8 n
     )
