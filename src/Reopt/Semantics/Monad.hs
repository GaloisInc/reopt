{-# LANGUAGE RankNTypes #-}
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
  , BoolType
  , DoubleType
  , FloatType
  , XMMType
  , TypeRepr(..)
  , type_width
  , KnownType(..)
    -- * Location
  , Location(..)
  , xmm_low64
  , loc_width
  , reg_low8
  , reg_high8
  , reg_low16
  , reg_low32

--  , reg_low
--  , reg_high
  , cf_flag
  , pf_flag
  , af_flag
  , zf_flag
  , sf_flag
  , tf_flag
  , if_flag
  , df_flag
  , of_flag
  , mkBVAddr
  -- ** Registers
  , rsp, rbp, r_rax, rax
    -- * IsLeq utility
  , IsLeq
  , n8, n16, n32, n64
    -- * Value operations
  , IsValue(..)
  , Pred
    -- * Semantics
  , Semantics(..)
  , Value
  , MLocation
  , SupportedBVWidth
  , IsLocationBV
  , FullSemantics
    -- * Re-exports
  , Flexdis86.Reg64
  , type (TypeLits.<=)
  ) where

import Control.Applicative
--import Data.Bits as Bits
import GHC.TypeLits as TypeLits
-- import GHC.TypeLits (type (<=) )

import Data.Parameterized.NatRepr
import Flexdis86.InstructionSet (Reg64, XMMReg)
import qualified Flexdis86.InstructionSet as Flexdis86

-- FIXME: move
n8 :: NatRepr 8
n8 = knownNat

n16 :: NatRepr 16
n16 = knownNat

n32 :: NatRepr 32
n32 = knownNat

n64 :: NatRepr 64
n64 = knownNat

------------------------------------------------------------------------
-- Type

data Type
  = -- | An array of bits
    BVType Nat

type BoolType   = BVType 1
type FloatType  = BVType 32
type DoubleType = BVType 64
type XMMType    = BVType 128

-- | A runtime representation of @Type@ for case matching purposes.
data TypeRepr tp where
  BVTypeRepr     :: {-# UNPACK #-} !(NatRepr n) -> TypeRepr (BVType n)

type_width :: TypeRepr (BVType n) -> NatRepr n
type_width (BVTypeRepr n) = n

class KnownType tp where
  knownType :: TypeRepr tp

instance KnownNat n => KnownType (BVType n) where
  knownType = BVTypeRepr knownNat

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

  -- A general purpose register.
  GPReg :: Reg64 -> Location addr (BVType 64)

  IPReg :: Location addr (BVType 64)

  CReg :: Flexdis86.ControlReg -> Location addr (BVType 64)
  DReg :: Flexdis86.DebugReg -> Location addr (BVType 64)
  MMXReg :: Flexdis86.MMXReg -> Location addr (BVType 64)
  -- JHx: Shouldn't this be a 64-bit value?
  SegmentReg :: Flexdis86.Segment -> Location addr (BVType 64)

  -- A XMM register with type representation information.
  --
  -- We expect that the 128-bits will be used to store one of:
  -- * four 32-bit single-precision floating point numbers
  -- * two 64-bit double-precision floating point numbers (SSE2)
  -- * two 64-bit integers         (SSE2)
  -- * four 32-bit integers        (SSE2)
  -- * eight 16-bit short integers (SSE2)
  -- * sixteen 8-bit bytes or characters (SSE2)
  -- The source from this list is wikipedia:
  --   http://en.wikipedia.org/wiki/Streaming_SIMD_Extensions
  XMMReg :: XMMReg
         -> Location addr (BVType 128)

  -- | Refers to the least significant half of the bitvector.
  LowerHalf :: Location addr (BVType (n+n))
            -> Location addr (BVType n)

  -- | Refers to the most significant half of the bitvector.
  UpperHalf :: Location addr (BVType (n+n))
            -> Location addr (BVType n)

{-
  -- A portion of a bitvector value.
  BVSlice :: Location addr (BVType n) -- Location of bitvector.
          -> Int         -- Bit level offset.
          -> NatRepr m   -- Type representation
          -> Location addr (BVType m)
-}

loc_width :: Location addr (BVType n) -> NatRepr n
loc_width (MemoryAddr _ tp) = type_width tp
loc_width (GPReg _) = knownNat
loc_width (DReg {}) = knownNat
loc_width (FlagReg {}) = knownNat
loc_width (CReg {}) = knownNat
loc_width (MMXReg {}) = knownNat
loc_width (SegmentReg {}) = knownNat
loc_width IPReg      = knownNat
loc_width (XMMReg _) = knownNat


xmm_low64 :: Location addr XMMType -> Location addr (BVType 64)
xmm_low64 l = LowerHalf l

{-
-- | Return the low 32-bits of the location.
low_dword :: Reg64 -> Location addr (BVType 32)
low_dword r = LowerHalf (GPReg r)
-}

-- | Return the high 32-bits of the location.
high_dword :: Reg64 -> Location addr (BVType 32)
high_dword r = UpperHalf (GPReg r)

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

-- | Tuen an address into a location of size @n
mkBVAddr :: NatRepr n -> addr -> Location addr (BVType n)
mkBVAddr sz addr = MemoryAddr addr (BVTypeRepr sz)


-- | Return low 32-bits of register e.g. rax -> eax
reg_low32 :: Reg64 -> Location addr (BVType 32)
reg_low32 r = LowerHalf (GPReg r)

-- | Return low 16-bits of register e.g. rax -> ax
reg_low16 :: Reg64 -> Location addr (BVType 16)
reg_low16 r = LowerHalf (LowerHalf (GPReg r))

-- | Return low 8-bits of register e.g. rax -> al
reg_low8 :: Reg64 -> Location addr (BVType 8)
reg_low8 r = LowerHalf (reg_low16 r)

-- | Return bits 8-15 of the register e.g. rax -> ah
reg_high8 :: Reg64 -> Location addr (BVType 8)
reg_high8 r = UpperHalf (reg_low16 r)


{-
-- | This addresses e.g. al and ah
reg_low, reg_high :: NatRepr n -> Reg64 -> Location addr (BVType n)
reg_low  n r = BVSlice (GPReg r) 0 n
reg_high n r = BVSlice (GPReg r) (widthVal n) n
-}

r_rax :: Reg64
r_rax = Flexdis86.rax

rsp, rbp, rax :: Location addr (BVType 64)
rax = GPReg Flexdis86.rax
rsp = GPReg Flexdis86.rsp
rbp = GPReg Flexdis86.rbp


------------------------------------------------------------------------
-- IsLeq

type IsLeq (m :: Nat) (n :: Nat) = (m <= n)

--instance (n <= m) => IsLeq n m where

-- instance IsLeq 1 4 where

-- instance IsLeq 8 8 where

-- instance IsLeq 1 16 where
-- instance IsLeq 4 16 where
-- instance IsLeq 8 16 where

-- instance IsLeq 1 32 where
-- instance IsLeq 4 32 where
-- instance IsLeq 8 32 where
-- instance IsLeq 16 32 where

-- instance IsLeq 1 64 where
-- --  leqProof = knownLeq
-- instance IsLeq 4 64 where
-- --  leqProof = knownLeq
-- instance IsLeq 8 64 where
-- --  leqProof = knownLeq

-- instance IsLeq 32 64 where
-- instance IsLeq 64 64 where

-- instance IsLeq 1 128 where
-- --  leqProof = knownLeq
-- instance IsLeq 4 128 where
-- --  leqProof = knownLeq
-- instance IsLeq 8 128 where
-- --  leqProof = knownLeq

-- instance IsLeq 32 128 where
-- instance IsLeq 64 128 where

------------------------------------------------------------------------
-- Values

-- | @IsValue@ is a class used to define types expressions.
class IsValue (v  :: Type -> *) where
{-
  -- JHx: I removed this, because I don't think there is a pure way to
  -- introduce undefined expressions (without some form of cheating like
  -- stable pointers).  We may want to not only know that a result is
  -- undefined, but able to distinguish between undefined values.
  undef :: v a
-}

  -- | Returns the width of a bit-vector value.
  bv_width :: v (BVType n) -> NatRepr n


  true :: v BoolType
  true = bvLit knownNat (1::Integer)

  false :: v BoolType
  false = bvLit knownNat (0::Integer)

  -- | Construct a literal bit vector.  The result is undefined if the
  -- literal does not fit withint the given number of bits.
  bvLit :: Integral a => NatRepr n -> a -> v (BVType n)

  -- | Add two bitvectors together dropping overflow.
  bvAdd :: v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Subtract two vectors, ignoring underflow.
  bvSub :: v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Performs a multiplication of two bitvector values.
  bvMul :: v (BVType n) -> v (BVType n) -> v (BVType n)

{-
  -- | Add two bitvectors together without dropping overflow.
  bvMul :: v (BVType n) -> v (BVType n) -> v (BVType (2 * n))
  bvMul x y = mul (sext w2 x) ((addNat
    where w = bv_width x
          w2 = addNat w w
-}

  -- | Bitwise complement
  complement :: v (BVType n) -> v (BVType n)

  -- | Bitwise and
  (.&.) :: v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Bitwise or
  (.|.) :: v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Exclusive or
  bvXor :: v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Equality
  (.=.) :: v (BVType n) -> v (BVType n) -> v BoolType
  bv .=. bv' = is_zero (bv `bvXor` bv')

  -- | Return true if value is zero.
  is_zero :: v (BVType n) -> v BoolType
  is_zero x = x .=. bvLit (bv_width x) (0::Integer)

  -- | Truncate the value
  bvTrunc :: (m <= n) => NatRepr m -> v (BVType n) -> v (BVType m)

  -- | Return most significant bit of number.
  msb :: (1 <= n) => v (BVType n) -> v BoolType

  -- | Perform a signed extension of a bitvector.
  sext :: (1 <= m, m <= n) => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Perform a unsigned extension of a bitvector.
  uext :: (m <= n) => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Return least-significant nibble (4 bits).
  least_nibble :: (4 <= n) => v (BVType n) -> v (BVType 4)
  least_nibble x = bvTrunc knownNat x

  -- | Return least-significant byte.
  least_byte :: (8 <= n) => v (BVType n) -> v (BVType 8)
  least_byte x = bvTrunc knownNat x

  -- | Return true if value contains an even number of true bits.
  even_parity :: v (BVType 8) -> v BoolType

  -- | Reverse the bytes in a bitvector expression.
  -- The parameter n should be a multiple of 8.
  reverse_bytes :: v (BVType n) -> v (BVType n)

  -- | Return true expression is signed add overflows.  See
  -- @sadc_overflows@ for definition.
  sadd_overflows :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
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

  -- | Return true expression if unsigned sub overflows.
  -- @usub_overflows x y@ is true when @x - y@ (interpreted as unsigned numbers),
  -- would return a negative result.
  usub_overflows :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  usub_overflows x y = usbb_overflows x y false

  -- | Return true expression is signed sub overflows.
  ssub_overflows :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  ssub_overflows x y = ssbb_overflows x y false

  -- | Return true expression if unsigned sbb overflows.
  -- @usbb_overflows x y c@ is true when @x - (y+c)@ with
  -- x,y interpreted as unsigned numbers and c a borrow bit,
  -- would return a negative result.
  usbb_overflows :: (1 <= n)
                 => v (BVType n)
                 -> v (BVType n)
                 -> v BoolType
                 -> v BoolType

  -- | Return true expression if unsigned sub overflows.
  -- @ssbb_overflows x y c@ is true when @x - (y+c)@ with
  -- x,y interpreted as signed numbers and c a borrow bit,
  -- would return a number different from the expected integer due to
  -- wrap-around.
  ssbb_overflows :: (1 <= n)
                 => v (BVType n)
                 -> v (BVType n)
                 -> v BoolType
                 -> v BoolType

  -- | bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsf :: v (BVType n) -> v (BVType n)

  -- | bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices greater than return value must be unset.
  bsr :: v (BVType n) -> v (BVType n)

  -- | Add two double precision floating point numbers.
  doubleAdd :: v DoubleType -> v DoubleType -> v DoubleType


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
type SupportedBVWidth n
   = ( IsLeq 1 n
     , IsLeq 4 n
     , IsLeq 8 n
     , KnownNat n
     )

-- | @IsLocationBV m n@ is a constraint used to indicate that @m@
-- implements Semantics, and @Value m (BV n)@ supports the operations
-- used to assign registers.
type IsLocationBV m n
   = ( SupportedBVWidth n
     , Semantics m
     )

-- | @This defines all the constraint that must be implemented to support
-- interpreting x86 instructions.
type FullSemantics m
   = ( Semantics m
     )
