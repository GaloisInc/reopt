-- |
-- Module           : Reopt.Semantics.Types
-- Description      : This defines the types of machine words
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- The type of machine words, including bit vectors and floating point
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.Macaw.Types
  ( module Data.Macaw.Types -- export everything
  , GHC.TypeLits.Nat
  , Data.Parameterized.NatRepr.NatRepr(..)
  , Data.Parameterized.NatRepr.knownNat
  ) where

import Data.Parameterized.Classes
import Data.Parameterized.NatRepr
import GHC.TypeLits
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- FIXME: move
n0 :: NatRepr 0
n0 = knownNat

n1 :: NatRepr 1
n1 = knownNat

n4 :: NatRepr 4
n4 = knownNat

n5 :: NatRepr 5
n5 = knownNat

n8 :: NatRepr 8
n8 = knownNat

n16 :: NatRepr 16
n16 = knownNat

n32 :: NatRepr 32
n32 = knownNat

n64 :: NatRepr 64
n64 = knownNat

n80 :: NatRepr 80
n80 = knownNat

n128 :: NatRepr 128
n128 = knownNat


------------------------------------------------------------------------
-- Type

data Type
  = -- | An array of bits
    BVType Nat

type family TypeBits (tp :: Type) :: Nat where
  TypeBits (BVType n) = n

type BVType = 'BVType

type BoolType   = BVType 1
type XMMType    = BVType 128

-- | A runtime representation of @Type@ for case matching purposes.
data TypeRepr (tp :: Type) where
  BVTypeRepr :: !(NatRepr n) -> TypeRepr (BVType n)

type_width :: TypeRepr (BVType n) -> NatRepr n
type_width (BVTypeRepr n) = n

instance TestEquality TypeRepr where
  testEquality (BVTypeRepr m) (BVTypeRepr n) = do
    Refl <- testEquality m n
    return Refl

instance OrdF TypeRepr where
  compareF (BVTypeRepr m) (BVTypeRepr n) = do
    case compareF m n of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

class KnownType tp where
  knownType :: TypeRepr tp

instance KnownNat n => KnownType (BVType n) where
  knownType = BVTypeRepr knownNat

------------------------------------------------------------------------
-- IsLeq

type IsLeq (m :: Nat) (n :: Nat) = (m <= n)

------------------------------------------------------------------------
-- Floating point sizes

-- | This data kind describes the styles of floating-point values understood
--   by recent LLVM bytecode formats.  This consist of the standard IEEE 754-2008
--   binary floating point formats, as well as the X86 extended 80-bit format
--   and the double-double format.
data FloatInfo where
  DoubleFloat       :: FloatInfo  --  64 bit binary IEE754
  SingleFloat       :: FloatInfo  --  32 bit binary IEE754
  X86_80Float       :: FloatInfo  -- X86 80-bit extended floats
  QuadFloat         :: FloatInfo  -- 128 bit binary IEE754
  HalfFloat         :: FloatInfo  --  16 bit binary IEE754

type SingleFloat = 'SingleFloat
type DoubleFloat = 'DoubleFloat
type X86_80Float = 'X86_80Float
type QuadFloat = 'QuadFloat
type HalfFloat = 'HalfFloat

data FloatInfoRepr (flt::FloatInfo) where
  DoubleFloatRepr       :: FloatInfoRepr DoubleFloat
  SingleFloatRepr       :: FloatInfoRepr SingleFloat
  X86_80FloatRepr       :: FloatInfoRepr X86_80Float
  QuadFloatRepr         :: FloatInfoRepr QuadFloat
  HalfFloatRepr         :: FloatInfoRepr HalfFloat

instance TestEquality FloatInfoRepr where
  testEquality x y = orderingF_refl (compareF x y)

instance OrdF FloatInfoRepr where
  compareF DoubleFloatRepr DoubleFloatRepr = EQF
  compareF DoubleFloatRepr _               = LTF
  compareF _               DoubleFloatRepr = GTF

  compareF SingleFloatRepr SingleFloatRepr = EQF
  compareF SingleFloatRepr _               = LTF
  compareF _               SingleFloatRepr = GTF

  compareF X86_80FloatRepr X86_80FloatRepr = EQF
  compareF X86_80FloatRepr _               = LTF
  compareF _               X86_80FloatRepr = GTF

  compareF QuadFloatRepr   QuadFloatRepr   = EQF
  compareF QuadFloatRepr   _               = LTF
  compareF _               QuadFloatRepr   = GTF

  compareF HalfFloatRepr   HalfFloatRepr   = EQF

instance Pretty (FloatInfoRepr flt) where
  pretty DoubleFloatRepr = text "double"
  pretty SingleFloatRepr = text "single"
  pretty X86_80FloatRepr = text "x87_80"
  pretty QuadFloatRepr   = text "quad"
  pretty HalfFloatRepr   = text "half"


type family FloatInfoBits (flt :: FloatInfo) :: Nat where
  FloatInfoBits HalfFloat         = 16
  FloatInfoBits SingleFloat       = 32
  FloatInfoBits DoubleFloat       = 64
  FloatInfoBits QuadFloat         = 128
  FloatInfoBits X86_80Float       = 80

type FloatType flt = BVType (FloatInfoBits flt)

-- type instance FloatInfoBits DoubleDoubleFloat =

floatInfoBits :: FloatInfoRepr flt -> NatRepr (FloatInfoBits flt)
floatInfoBits fir =
  case fir of
    HalfFloatRepr         -> knownNat
    SingleFloatRepr       -> knownNat
    DoubleFloatRepr       -> knownNat
    QuadFloatRepr         -> knownNat
    X86_80FloatRepr       -> knownNat

floatTypeRepr :: FloatInfoRepr flt -> TypeRepr (BVType (FloatInfoBits flt))
floatTypeRepr fir = BVTypeRepr (floatInfoBits fir)

------------------------------------------------------------------------
--

-- | A multi-parameter type class that allows one to represent that a
-- parameterized type value has some representative type such as a TypeRepr.
class HasRepr (f :: k -> *) (v :: k -> *)  | f -> v where
  typeRepr :: f tp -> v tp
