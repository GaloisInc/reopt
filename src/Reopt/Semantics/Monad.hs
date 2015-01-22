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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Semantics.Monad
 ( -- * Type
   Type(..)
 , TypeRepr(..)
   -- * Location
 , Location(..)
 , low_dword
 , high_dword
 , cf_flag
 , pf_flag
 , af_flag
 , zf_flag
 , sf_flag
 , tf_flag
 , if_flag
 , df_flag
 , of_flag
   -- * IsLeq utility
 , IsLeq
   -- * Value operations
 , IsValue(..)
 , Pred
   -- * Semantics
 , Semantics(..)
 , Value
 , MLocation
 , IsLocationBV
 , FullSemantics
 , SupportedBVWidth
   -- * Re-exports
 , ValueBits(..)
 , ValueNum(..)
-- , Bits.complement
-- , (Bits..&.)
 , Flexdis86.Reg64
 ) where

import Control.Applicative
--import Data.Bits as Bits
import GHC.TypeLits

import Data.Parameterized.NatRepr
import Flexdis86.InstructionSet as Flexdis86 (Reg64, XMMReg)

------------------------------------------------------------------------
-- Type

data Type
  = -- | An array of bits
    BVType Nat
    -- | A Boolean vlaue
  | BoolType
    -- | A 64-bit floating point value in IEEE format.
  | DoubleType

-- | A runtime representation of @Type@ for case matching purposes.
data TypeRepr tp where
  BVTypeRepr     :: {-# UNPACK #-} !(NatRepr n) -> TypeRepr (BVType n)
  BoolTypeRepr   :: TypeRepr BoolType
  DoubleTypeRepr :: TypeRepr DoubleType

------------------------------------------------------------------------
-- Location

-- | This returns the type associated with values that can be read
-- or assigned for the semantics monad.
data Location addr (tp :: Type) where
  -- A flag register (a constant from 0 to 63).
  -- Constant defined in:
  --  Intel 64 and IA-32 Architectures Software Developer’s Manual
  --  June 2013, Vol 1.  3-15
  FlagReg :: !Int -> Location addr BoolType

  -- A location in the virtual address space of the process.
  MemoryAddr :: addr -> TypeRepr tp  -> Location addr tp

  -- | A general purpose register.
  GPReg :: Reg64 -> Location addr (BVType 64)

  -- A XMM register with type representation information.
  --
  -- We expect that the 128-bits will be used to store one of:
  -- * four 32-bit single-precision floating point numbers
  -- * two 64-bit double-precision floating point numbers (SSE2)
  -- * two 64-bit integers (SSE2)
  -- * four 32-bit integers (SSE2)
  -- * eight 16-bit short integers (SSE2)
  -- * sixteen 8-bit bytes or characters (SSE2)
  -- The source from this list is wikipedia:
  --   http://en.wikipedia.org/wiki/Streaming_SIMD_Extensions
  XMMReg:: XMMReg
        -> Location addr (BVType 128)

  -- A portion of a bitvector value.
  VecEntry :: Location addr (BVType n) -- Location of bitvector.
           -> Int         -- Bit level offset.
           -> TypeRepr tp -- Type representation
           -> Location addr tp

knownBVType :: KnownNat n => TypeRepr (BVType n)
knownBVType = BVTypeRepr knownNat

-- | Return the low 32-bits of the location.
low_dword :: Reg64 -> Location addr (BVType 32)
low_dword r = VecEntry (GPReg r) 0 knownBVType

-- | Return the high 32-bits of the location.
high_dword :: Reg64 -> Location addr (BVType 32)
high_dword r = VecEntry (GPReg r) 32 knownBVType

-- | CF flag
cf_flag :: Location addr BoolType
cf_flag = FlagReg 0

-- | PF flag
pf_flag :: Location addr BoolType
pf_flag = FlagReg 2

-- | AF flag
af_flag :: Location addr BoolType
af_flag = FlagReg 4

-- | ZF flag
zf_flag :: Location addr BoolType
zf_flag = FlagReg 6

-- | SF flag
sf_flag :: Location addr BoolType
sf_flag = FlagReg 7

-- | TF flag
tf_flag :: Location addr BoolType
tf_flag = FlagReg 8

-- | IF flag
if_flag :: Location addr BoolType
if_flag = FlagReg 9

-- | DF flag
df_flag :: Location addr BoolType
df_flag = FlagReg 10

-- | OF flag
of_flag :: Location addr BoolType
of_flag = FlagReg 11

------------------------------------------------------------------------
-- IsLeq

class IsLeq (m :: Nat) (n :: Nat) where

instance IsLeq 1 16 where
instance IsLeq 4 16 where
instance IsLeq 8 16 where

instance IsLeq 1 32 where
instance IsLeq 4 32 where
instance IsLeq 8 32 where

instance IsLeq 1 64 where
--  leqProof = knownLeq
instance IsLeq 4 64 where
--  leqProof = knownLeq
instance IsLeq 8 64 where
--  leqProof = knownLeq

------------------------------------------------------------------------
-- Values

class ValueBits tp where
  (.&.) :: tp -> tp -> tp
  (.|.) :: tp -> tp -> tp
  complement :: tp -> tp

class ValueNum tp where
  (.+) :: tp -> tp -> tp

-- | @IsValue@ is a class used to define types expressions.
class ( Num (v DoubleType)
      , ValueBits (v BoolType)
      )
      => IsValue (v  :: Type -> *) where
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

  -- | Perform a signed extension of a bitvector.
  sext :: IsLeq m n => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Perform a unsigned extension of a bitvector.
  uext :: IsLeq m n => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Performs a imul on
  mul :: v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsf :: v (BVType n) -> v (BVType n)
  -- | bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsr :: v (BVType n) -> v (BVType n)

  -- | Returns the width of a bit-vector value.
  bv_width :: v (BVType n) -> NatRepr n


------------------------------------------------------------------------
-- Monadic definition

-- | This returns the type associated with values that can be read
-- for the semantics monad.
type family Value (m :: * -> *) :: Type -> *

type Pred m = Value m BoolType

type MLocation m = Location (Value m (BVType 64))

-- | The Semantics Monad defines all the operations needed for the x86
-- semantics.
class ( Applicative m
      , Monad m
      , IsValue (Value m)
      ) => Semantics m where
  -- | Mark a Boolean variable as undefined.
  set_undefined :: MLocation m BoolType -> m ()

  -- | Read from the given location.
  get :: MLocation m tp -> m (Value m tp)
  -- | Assign a value to alocation.
  (.=) :: MLocation m tp -> Value m tp -> m ()

  -- | Modify the value at a location
  modify :: MLocation m tp -> (Value m tp -> Value m tp) -> m ()
  modify r f = do
    x <- get r
    r .= f x

  -- | Perform an if-then-else
  ifte_ :: Value m BoolType -> m () -> m () -> m ()

  -- | Run a step if condition holds.
  when_ :: Value m BoolType -> m () -> m ()
  when_ p x = ifte_ p x (return ())

-- | Defines operations that need to be supported at a specific bitwidht.
type SupportedBVWidth v n
   = ( ValueBits (v (BVType n))
     , ValueNum (v (BVType n))
     , IsLeq 1 n
     , IsLeq 4 n
     , IsLeq 8 n
     )

-- | @IsLocationBV m n@ is a constraint used to indicate that @m@
-- implements Semantics, and @Value m (BV n)@ supports the operations
-- used to assign registers.
type IsLocationBV m n
   = ( SupportedBVWidth (Value m) n
     , Semantics m
     )

-- | @This defines all the constraint that must be implemented to support
-- interpreting x86 instructions.
type FullSemantics m
   = ( Semantics m
     , SupportedBVWidth (Value m) 8
     , SupportedBVWidth (Value m) 16
     , SupportedBVWidth (Value m) 32
     , SupportedBVWidth (Value m) 64
     )
