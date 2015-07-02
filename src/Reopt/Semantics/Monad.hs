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
-- Defines typeclasses used to implement x86 instruction semantics.
------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Semantics.Monad
  ( -- * Type
    Type(..)
  , type BVType
  , BoolType
  , FloatType
  , XMMType
  , TypeRepr(..)
  , type_width
  , KnownType(..)
  , FloatInfo(..)
  , FloatInfoRepr(..)
  , FloatInfoBits
  , floatInfoBits
    -- * Location
  , Location(..)
  , xmm_low64
  , loc_type
  , loc_width
  , reg_low8
  , reg_high8
  , reg_low16
  , reg_low32

--  , reg_low
--  , reg_high
  , cf_loc
  , pf_loc
  , af_loc
  , zf_loc
  , sf_loc
  , tf_loc
  , if_loc
  , df_loc
  , of_loc
    -- X87
  , c0_loc
  , c1_loc
  , c2_loc
  , c3_loc

  , mkBVAddr
  , mkFPAddr
  , packWord
  , unpackWord
  -- ** Registers
  , rsp, rbp, rax, rdx, rsi, rdi, rcx, rip
  , cx,  ecx
    -- * IsLeq utility
  , IsLeq
  , n1, n8, n16, n32, n64, n80, n128
    -- * Value operations
  , IsValue(..)
  , bvKLit
  , (.+)
  , (.-)
  , (.*)
  , Pred
    -- * Semantics
  , Semantics(..)
  , Value
  , MLocation
  , SupportedBVWidth
  , IsLocationBV
  , FullSemantics
  , ExceptionClass(..)
    -- * Re-exports
  , type (TypeLits.<=)
  , type Flexdis86.OpTable.SizeConstraint(..)
  ) where

import           Control.Applicative
import           Data.Bits (shiftL)
import           Data.Proxy
import           GHC.TypeLits as TypeLits

import           Data.Parameterized.NatRepr
import           Reopt.Machine.StateNames (RegisterName, RegisterClass(..))
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types

import Flexdis86.OpTable (SizeConstraint(..))

------------------------------------------------------------------------
-- Location

-- | This returns the type associated with values that can be read
-- or assigned for the semantics monad.
data Location addr (tp :: Type) where
  -- A location in the virtual address space of the process.
  MemoryAddr :: addr -> TypeRepr tp  -> Location addr tp

  Register :: RegisterName cl -> Location addr (N.RegisterType cl)

  TruncLoc :: (1 <= n', n'+1 <= n)
           => Location addr (BVType n)
           -> NatRepr n'
           -> Location addr (BVType n')

  -- Refers to the least significant half of the bitvector.
  LowerHalf :: (1 <= n)
            => Location addr (BVType (n+n))
            -> Location addr (BVType n)
  -- Refers to the most significant half of the bitvector.
  UpperHalf :: (1 <= n)
            => Location addr (BVType (n+n))
            -> Location addr (BVType n)

  -- The register stack: the argument is an offset from the stack
  -- top, so X87Register 0 is the top, X87Register 1 is the second,
  -- and so forth.
  X87StackRegister :: !Int -> Location addr (FloatType X86_80Float)

loc_type :: Location addr tp -> TypeRepr tp
loc_type (MemoryAddr _ tp) = tp
loc_type (Register r)      = N.registerType r
loc_type (TruncLoc _ n)    = BVTypeRepr n
loc_type (LowerHalf l)     = BVTypeRepr $ halfNat (loc_width l)
loc_type (UpperHalf l)     = BVTypeRepr $ halfNat (loc_width l)
loc_type (X87StackRegister _) = knownType

loc_width :: Location addr (BVType n) -> NatRepr n
loc_width (MemoryAddr _ tp) = type_width tp
loc_width (Register r)  = N.registerWidth r
loc_width (TruncLoc _ n) = n
loc_width (LowerHalf l) = halfNat (loc_width l)
loc_width (UpperHalf l) = halfNat (loc_width l)
loc_width (X87StackRegister _) = knownNat

xmm_low64 :: Location addr XMMType -> Location addr (BVType 64)
xmm_low64 l = LowerHalf l

-- | CF flag
cf_loc :: Location addr BoolType
cf_loc = Register N.cf

-- | PF flag
pf_loc :: Location addr BoolType
pf_loc = Register N.pf

-- | AF flag
af_loc :: Location addr BoolType
af_loc = Register N.af

-- | ZF flag
zf_loc :: Location addr BoolType
zf_loc = Register N.zf

-- | SF flag
sf_loc :: Location addr BoolType
sf_loc = Register N.sf

-- | TF flag
tf_loc :: Location addr BoolType
tf_loc = Register N.tf

-- | IF flag
if_loc :: Location addr BoolType
if_loc = Register N.iflag

-- | DF flag
df_loc :: Location addr BoolType
df_loc = Register N.df

-- | OF flag
of_loc :: Location addr BoolType
of_loc = Register N.oflag

-- | x87 flags
c0_loc, c1_loc, c2_loc, c3_loc :: Location addr BoolType
c0_loc = Register N.x87c0
c1_loc = Register N.x87c1
c2_loc = Register N.x87c2
c3_loc = Register N.x87c3

-- | Tuen an address into a location of size @n
mkBVAddr :: NatRepr n -> addr -> Location addr (BVType n)
mkBVAddr sz addr = MemoryAddr addr (BVTypeRepr sz)

-- | Tuen an address into a location of size @n
mkFPAddr :: FloatInfoRepr flt -> addr -> Location addr (FloatType flt)
mkFPAddr fir addr = MemoryAddr addr (BVTypeRepr (floatInfoBits fir))

-- | Return low 32-bits of register e.g. rax -> eax
reg_low32 :: RegisterName 'GP -> Location addr (BVType 32)
reg_low32 r = LowerHalf (Register r)

-- | Return low 16-bits of register e.g. rax -> ax
reg_low16 :: RegisterName 'GP -> Location addr (BVType 16)
reg_low16 r = LowerHalf (LowerHalf (Register r))

-- | Return low 8-bits of register e.g. rax -> al
reg_low8 :: RegisterName 'GP -> Location addr (BVType 8)
reg_low8 r = LowerHalf (reg_low16 r)

-- | Return bits 8-15 of the register e.g. rax -> ah
reg_high8 :: RegisterName 'GP -> Location addr (BVType 8)
reg_high8 r = UpperHalf (reg_low16 r)

rsp, rbp, rax, rdx, rsi, rdi, rcx :: Location addr (BVType 64)
rax = Register N.rax
rsp = Register N.rsp
rbp = Register N.rbp
rdx = Register N.rdx
rsi = Register N.rsi
rdi = Register N.rdi
rcx = Register N.rcx

cx :: Location addr (BVType 16)
cx = reg_low16 N.rcx

ecx :: Location addr (BVType 32)
ecx = reg_low32 N.rcx

rip :: Location addr (BVType 64)
rip = Register N.IPReg

packWord :: forall m n. (Semantics m, 1 <= n) => N.BitPacking n -> m (Value m (BVType n))
packWord (N.BitPacking sz bits) =
  do injs <- mapM getMoveBits bits
     return (foldl1 (.|.) injs)
  where
    getMoveBits :: N.BitConversion n -> m (Value m (BVType n))
    getMoveBits (N.ConstantBit b off)
      = return $ bvLit sz (if b then 1 `shiftL` widthVal off else (0 :: Integer))
    getMoveBits (N.RegisterBit reg off)
      = do v <- uext sz <$> get (Register reg)
           return $ v `bvShl` bvLit sz (widthVal off)

unpackWord :: forall m n. (Semantics m, 1 <= n) => N.BitPacking n -> Value m (BVType n) -> m ()
unpackWord (N.BitPacking sz bits) v = mapM_ unpackOne bits
  where
    unpackOne :: N.BitConversion n -> m ()
    unpackOne N.ConstantBit{}         = return ()
    unpackOne (N.RegisterBit reg off) = do
      let res_w = N.registerWidth reg
      Register reg .= bvTrunc res_w (v `bvShr` bvLit sz (widthVal off))

------------------------------------------------------------------------
-- Values

-- | @IsValue@ is a class used to define types expressions.
class IsValue (v  :: Type -> *) where

  -- | Returns the width of a bit-vector value.
  bv_width :: v (BVType n) -> NatRepr n

  -- | Choose a value based upon the boolean (basically a pure if then else)
  mux :: v BoolType -> v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Construct a literal bit vector.  The result is undefined if the
  -- literal does not fit withint the given number of bits.
  bvLit :: (Integral a, 1 <= n) => NatRepr n -> a -> v (BVType n)

  -- | Add two bitvectors together dropping overflow.
  bvAdd :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Subtract two vectors, ignoring underflow.
  bvSub :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Performs a multiplication of two bitvector values.
  bvMul :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | 2's complement
  bvNeg :: (1 <= n) => v (BVType n) -> v (BVType n)
  bvNeg n = bvLit (bv_width n) (0 :: Int) `bvSub` n

  -- | Bitwise complement
  complement :: (1 <= n) => v (BVType n) -> v (BVType n)

  -- | Bitwise and
  (.&.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Bitwise or
  (.|.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Exclusive or
  bvXor :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Equality
  (.=.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  bv .=. bv' = is_zero (bv `bvXor` bv')

  -- | Inequality
  (.=/=.) :: (1 <= n) => v (BVType n) -> v (BVType n) -> v BoolType
  bv .=/=. bv' = complement (bv .=. bv')

  -- | Return true if value is zero.
  is_zero :: (1 <= n) => v (BVType n) -> v BoolType
  is_zero x = x .=. bvLit (bv_width x) (0::Integer)

  -- | Concatentates two bit vectors
  bvCat :: forall n . (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType (n + n))
  bvCat h l =
      case ( leqAdd (leqRefl n) n
           , dblPosIsPos le_1_n
           ) of
        (LeqProof, LeqProof) ->
          (uext n_plus_n h `bvShl` bvLit n_plus_n (widthVal $ bv_width l))
          .|. (uext n_plus_n l)
    where
      n_plus_n = addNat (bv_width l) (bv_width l)
      n :: Proxy n
      n = Proxy
      le_1_n :: LeqProof 1 n
      le_1_n = LeqProof

  -- | Splits a bit vectors into two
  bvSplit :: (1 <= n) => v (BVType (n + n)) -> (v (BVType n), v (BVType n))
  -- bvSplit v = (bvTrunc:: sz (bvShr (widthVal sz) v), bvTrunc sz v)
  --   where
  --     sz = halfNat (bv_width v)

  -- | Vectorization
  bvVectorize :: forall k n. (1 <= k) => NatRepr k -> v (BVType n) -> [v (BVType k)]
  bvVectorize sz bv
    | Just Refl <- testEquality (bv_width bv) sz = [bv]
    | Just LeqProof <- testLeq sz (bv_width bv) =
        let bvs2n :: [v (BVType (k+k))] -- solve for size (n+n), then split into size n
            bvs2n = withLeqProof (dblPosIsPos (LeqProof :: LeqProof 1 k)) $ bvVectorize (addNat sz sz) bv
        in concatMap (\v -> let (a, b) = bvSplit v in [a, b]) bvs2n

  bvUnvectorize :: forall k n. (1 <= k) => NatRepr n -> [v (BVType k)] -> v (BVType n)
  bvUnvectorize sz [x]
    | Just Refl <- testEquality (bv_width x) sz = x
  bvUnvectorize sz bvs = withLeqProof (dblPosIsPos (LeqProof :: LeqProof 1 k)) $ bvUnvectorize sz $ concatBVPairs bvs
    where concatBVPairs :: (1 <= o) => [v (BVType o)] -> [v (BVType (o+o))]
          concatBVPairs (x:y:zs) = (x `bvCat` y) : concatBVPairs zs
          concatBVPairs _ = []

  -- | Rotations
  bvRol :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)
  bvRol v n = bvShl v n .|. bvShr v bits_less_n
    where
      bits_less_n = bvSub (bvLit (bv_width v) (widthVal $ bv_width v)) n

  bvRor :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)
  bvRor v n = bvShr v n .|. bvShl v bits_less_n
    where
      bits_less_n = bvSub (bvLit (bv_width v) (widthVal $ bv_width v)) n

  -- | Shifts, the semantics is undefined for shifts >= the width of the first argument
  bvShr, bvSar, bvShl :: (1 <= n) => v (BVType n) -> v (BVType n) -> v (BVType n)

  -- | Truncate the value
  bvTrunc :: (1 <= m, m <= n) => NatRepr m -> v (BVType n) -> v (BVType m)

  -- | Unsigned less than
  bvUlt :: v (BVType n) -> v (BVType n) -> v BoolType

  -- | Signed less than
  bvSlt :: v (BVType n) -> v (BVType n) -> v BoolType

  -- | returns bit n, 0 being lsb
  bvBit :: (1 <= log_n) => v (BVType n) -> v (BVType log_n) -> v BoolType

  -- | Return most significant bit of number.
  msb :: (1 <= n) => v (BVType n) -> v BoolType
  msb v = bvBit v (bvLit (bv_width v) (widthVal (bv_width v) - 1))
     -- FIXME: should be log2 (bv_width v) here

  -- | Perform a signed extension of a bitvector.
  sext :: (1 <= m, m <= n) => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Perform a unsigned extension of a bitvector.
  --
  -- Unlike 'uext' below, this is a strict extension: the 'm+1 <= n'
  -- means 'm < n'.
  uext' :: (1 <= m, m+1 <= n) => NatRepr n -> v (BVType m) -> v (BVType n)

  -- | Perform a unsigned extension of a bitvector.
  uext :: (1 <= m, m <= n)
        => NatRepr n
        -> v (BVType m)
        -> v (BVType n)
  uext w e =
    case testStrictLeq (bv_width e) w of
      Left LeqProof -> uext' w e
      Right Refl -> e

  -- | Return least-significant nibble (4 bits).
  least_nibble :: forall n . (4 <= n) => v (BVType n) -> v (BVType 4)
  least_nibble = bvTrunc knownNat

  -- | Return least-significant byte.
  least_byte :: forall n . (8 <= n) => v (BVType n) -> v (BVType 8)
  least_byte = bvTrunc knownNat

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

  -- FP stuff

  -- | is NaN (quiet and signalling)
  isQNaN :: FloatInfoRepr flt -> v (FloatType flt) -> v BoolType
  isSNaN :: FloatInfoRepr flt -> v (FloatType flt) -> v BoolType

  isNaN :: FloatInfoRepr flt -> v (FloatType flt) -> v BoolType
  isNaN fir v = isQNaN fir v .|. isSNaN fir v

  -- | Add two floating point numbers.
  -- This can throw exceptions, including:
  -- #IA Operand is an SNaN value or unsupported format.
  --     Operands are infinities of unlike sign.
  -- #D  Source operand is a denormal value.
  -- #U  Result is too small for destination format.
  -- #O  Result is too large for destination format.
  -- #P  Value cannot be represented exactly in destination format.
  fpAdd :: FloatInfoRepr flt
        -> v (FloatType flt)
        -> v (FloatType flt)
        -> v (FloatType flt)

  -- | Indicates whether floating point representation from addition is
  -- larger than the actual value.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpAddRoundedUp :: FloatInfoRepr flt
                 -> v (FloatType flt)
                 -> v (FloatType flt)
                 -> v BoolType

  -- | Subtracts one floating point numbers from another.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpSub :: FloatInfoRepr flt
        -> v (FloatType flt)
        -> v (FloatType flt)
        -> v (FloatType flt)

  -- | Indicates whether floating point representation from subtraction
  -- is larger than the actual value.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpSubRoundedUp :: FloatInfoRepr flt
                 -> v (FloatType flt)
                 -> v (FloatType flt)
                 -> v BoolType

  -- | Multiplies two floating point numbers.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpMul :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v (FloatType flt)

  -- | Whether the result of the mul was rounded up
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpMulRoundedUp :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v BoolType

  -- | Divides two floating point numbers.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpDiv :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v (FloatType flt)

  -- | Compare if one floating is strictly less than another.
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpLt :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v BoolType

  -- | Floating point equality (equates -0 and 0)
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpEq :: FloatInfoRepr flt -> v (FloatType flt) -> v (FloatType flt) -> v BoolType
  fpEq fir v v' = complement (fpLt fir v v' .|. fpLt fir v' v)

  -- | Convert between floating point values
  -- TODO: Describe exceptions or modify usage to avoid them.
  fpCvt :: FloatInfoRepr flt -> FloatInfoRepr flt' -> v (FloatType flt) -> v (FloatType flt')

  -- | Whether roundup occurs when converting between FP formats
  fpCvtRoundsUp :: FloatInfoRepr flt -> FloatInfoRepr flt' -> v (FloatType flt) -> v BoolType

  -- | Convert a signed vitvector to a float.
  -- We assume that the floating point representation is large enough to hold
  -- all the values at that bitwidth.
  fpFromBV :: FloatInfoRepr flt -> v (BVType n) -> v (FloatType flt)

  -- | Convert a floating point value to a signed integer.
  -- * If the conversion is inexact, then the value is truncated to zero.
  -- * If the conversion is out of the range of the bitvector, then a
  --   floating point exception should be raised.
  -- * If that exception is masked, then this returns -1 (as a signed bitvector).
  truncFPToSignedBV :: NatRepr n
                    -> FloatInfoRepr flt
                    -> v (FloatType flt)
                    -> v (BVType n)


  true :: v BoolType
  true = bvLit knownNat (1::Integer)

  false :: v BoolType
  false = bvLit knownNat (0::Integer)

  boolValue :: Bool -> v BoolType
  boolValue b = if b then true else false

-- | Construct a literal bit vector.  The result is undefined if the
-- literal does not fit withint the given number of bits.
bvKLit :: (IsValue v, KnownNat n, 1 <= n) => Integer -> v (BVType n)
bvKLit = bvLit knownNat

-- | Add two bitvectors together dropping overflow.
(.+) :: (1 <= n, IsValue v) => v (BVType n) -> v (BVType n) -> v (BVType n)
(.+) = bvAdd

-- | Subtract two vectors, ignoring underflow.
(.-) :: (1 <= n, IsValue v) => v (BVType n) -> v (BVType n) -> v (BVType n)
(.-) = bvSub

-- | Performs a multiplication of two bitvector values.
(.*) :: (1 <= n, IsValue v) => v (BVType n) -> v (BVType n) -> v (BVType n)
(.*) = bvMul

infixl 7 .*
infixl 6 .+
infixl 6 .-
infix  4 .=
{-
-- Basically so I can get fromInteger, but the others might be handy too ...
instance (IsValue v, KnownNat n, 1 <= n) => Num (v (BVType n)) where
  (+) = bvAdd
  (*) = bvMul
  (-) = bvSub
  negate = bvNeg
  abs    = error "abs is unimplemented for Values"
  signum = error "signum is unimplemented for Values"
  fromInteger = bvLit knownNat
-}

------------------------------------------------------------------------
-- Monadic definition

-- | This returns the type associated with values that can be read
-- for the semantics monad.
type family Value (m :: * -> *) :: Type -> *

type Pred m = Value m BoolType

type MLocation m = Location (Value m (BVType 64))

data ExceptionClass
   = DivideError -- #DE
   | FloatingPointError
   | SIMDFloatingPointException
     -- -- | AlignmentCheck
  deriving Show

-- | The Semantics Monad defines all the operations needed for the x86
-- semantics.
class ( Applicative m
      , Monad m
      , IsValue (Value m)
      ) => Semantics m where

  -- | Create a fresh "undefined" value.
  make_undefined :: TypeRepr tp -> m (Value m tp)

  -- | Mark a Boolean variable as undefined.
  set_undefined :: KnownType tp => MLocation m tp -> m ()
  set_undefined l = do
    u <- make_undefined knownType
    l .= u

  -- | Read from the given location.
  get :: MLocation m tp -> m (Value m tp)
  -- | Assign a value to alocation.
  (.=) :: MLocation m tp -> Value m tp -> m ()

  -- | Modify the value at a location
  modify :: (Value m tp -> Value m tp) -> MLocation m tp -> m ()
  modify f r = do
    x <- get r
    r .= f x

  -- | Perform an if-then-else
  ifte_ :: Value m BoolType -> m () -> m () -> m ()

  -- | Run a step if condition holds.
  when_ :: Value m BoolType -> m () -> m ()
  when_ p x = ifte_ p x (return ())

  -- | Run a step if condition is false.
  unless_ :: Value m BoolType -> m () -> m ()
  unless_ p x = ifte_ p (return ()) x

  -- FIXME: use location instead?
  -- | Move n bits at a time, with count moves
  --
  -- Semantic sketch. The effect on memory should be like @memcopy@
  -- below, not like @memcopy2@. These sketches ignore the issue of
  -- copying in chunks of size `bytes`, which should only be an
  -- efficiency concern.
  --
  -- @
  -- void memcopy(int bytes, int copies, char *src, char *dst, int reversed) {
  --   int maybeFlip = reversed ? -1 : 1;
  --   for (int c = 0; c < copies; ++c) {
  --     for (int b = 0; b < bytes; ++b) {
  --       int offset = maybeFlip * (b + c * bytes);
  --       *(dst + offset) = *(src + offset);
  --     }
  --   }
  -- }
  -- @
  --
  -- Compare with:
  --
  -- @
  -- void memcopy2(int bytes, int copies, char *src, char *dst, int reversed) {
  --   int maybeFlip = reversed ? -1 : 1;
  --   /* The only difference from `memcopy` above: here the same memory is
  --      copied whether `reversed` is true or false -- only the order of
  --      copies changes -- whereas above different memory is copied for
  --      each direction. */
  --   if (reversed) {
  --     /* Start at the end and work backwards. */
  --     src += copies * bytes - 1;
  --     dst += copies * bytes - 1;
  --   }
  --   for (int c = 0; c < copies; ++c) {
  --     for (int b = 0; b < bytes; ++b) {
  --       int offset = maybeFlip * (b + c * bytes);
  --       *(dst + offset) = *(src + offset);
  --     }
  --   }
  -- }
  -- @
  memcopy :: Integer
             -- ^ Number of bytes to copy at a time (1,2,4,8)
          -> Value m (BVType 64)
             -- ^ Number of values to move.
          -> Value m (BVType 64)
             -- ^ Start of source buffer
          -> Value m (BVType 64)
             -- ^ Start of destination buffer.
          -> Value m BoolType
             -- ^ Flag indicates direction of move:
             -- True means we should decrement buffer pointers after each copy.
             -- False means we should increment the buffer pointers after each copy.
          -> m ()

  -- | Compare the memory regions.  Returns the number of elements which are
  -- identical.  If the direction is 0 then it is increasing, otherwise decreasing.
  --
  -- See `memcopy` above for explanation of which memory regions are
  -- compared: the regions copied there are compared here.
  memcmp :: Integer
            -- ^ Number of bytes to compare at a time {1, 2, 4, 8}
            -> Value m (BVType 64)
            -- ^ Number of elementes to compare
            -> Value m (BVType 64)
            -- ^ Pointer to first buffer
            -> Value m (BVType 64)
            -- ^ Pointer to second buffer
            -> Value m BoolType
             -- ^ Flag indicates direction of copy:
             -- True means we should decrement buffer pointers after each copy.
             -- False means we should increment the buffer pointers after each copy.
            -> m (Value m (BVType 64))

  -- | Set memory to the given value, for the number of words (nbytes
  -- = count * bv_width v)
  memset :: Value m (BVType 64)
            -- ^ Number of values to set
            -> Value m (BVType n)
            -- ^ Value to set
            -> Value m (BVType 64)
            -- ^ Pointer to buffer to set
            -> m ()

  -- | execute the system call instruction.
  syscall :: m ()

  -- | Performs an unsigned division.  It rounds the result to zero,
  -- and returns both the quotient and remainder.
  -- This throws an #de exception if the denominator is zero, or if the
  -- quotient is too large to fit into the result.
  bvDiv :: (1 <= n)
        => Value m (BVType (n+n))
        -> Value m (BVType n)
        -> m ( Value m (BVType n)
             , Value m (BVType n)
             )

  -- | Signed divison.  It rounds the result to zero, and returns both the
  -- quotient and remainder.
  -- This throws an #de exception if the denominator is zero, or if the
  -- (signed) quotient is too large to fit into the result.
  bvSignedDiv :: (1 <= n)
              => Value m (BVType (n+n))
              -> Value m (BVType n)
              -> m ( Value m (BVType n)
                   , Value m (BVType n)
                   )

  -- | raises an exception if the predicate is true and the mask is false
  exception :: Value m BoolType    -- mask
            -> Value m BoolType -- predicate
            -> ExceptionClass
            -> m ()

  -- FIXME: those should also mutate the underflow/overflow flag and
  -- related state.

  -- | X87 support --- these both affect the register stack for the
  -- x87 state.
  x87Push :: Value m (FloatType X86_80Float) -> m ()
  x87Pop  :: m ()

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
