------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.StateNames
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>, Simon Winwood <sjw@galois.com>
--
-- This defines the data types needed to names the various x86 registers
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wwarn #-}
module Reopt.Machine.StateNames
  ( ProgramCounter
  , GP
  , Flag
  , Segment
  , Control
  , Debug

  , X87_FPU
  , X87_Status
  , X87_Top
  , X87_Tag
  , X87_ControlMask
  , X87_Control
  , XMM

    -- * X87_ControlReg
  , X87_ControlReg
  , x87_IM
  , x87_DM
  , x87_ZM
  , x87_OM
  , x87_UM
  , x87_PM
  , x87_PC
  , x87_RC
  , x87_X

    -- * RegisterName

  , RegisterName(..)

  , BitConversion(..)
  , BitPacking(..)
  , registerWidth

  , rax
  , rbx
  , rcx
  , rdx
  , rsp
  , rbp
  , rsi
  , rdi
  , rip

  , flagNames
  , gpNames
  , gpFromFlexdis
  , xmmFromFlexdis
  , x87StatusNames

  , cf
  , pf
  , af
  , zf
  , sf
  , tf
  , iflag
  , df
  , oflag
    -- * X87 status flags
  , pattern X87IE
  , pattern X87DE
  , pattern X87ZE
  , pattern X87OE
  , pattern X87UE
  , pattern X87PE
  , pattern X87EF
  , pattern X87ES
  , pattern X87C0
  , pattern X87C1
  , pattern X87C2
  , pattern X87C3
  ) where

import           Data.Macaw.Types
import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import qualified Data.Vector as V
import qualified Flexdis86 as F

-- Widths of common types
type ProgramCounter  = BVType 64
type GP              = BVType 64
type Flag            = BVType 1
type Segment         = BVType 16
type Control         = BVType 64
type Debug           = BVType 64
type X87_FPU         = BVType 80
type X87_Status      = BVType 1
type X87_Top         = BVType 3
type X87_Tag         = BVType 2
type X87_ControlMask = BVType 1
type X87_Control     = BVType 2
type XMM             = BVType 128

--type RegisterClassBits tp = TypeBits tp

------------------------------------------------------------------------
-- X87_ControlReg

-- | One of 16 X87 control registrs
data X87_ControlReg w = X87_ControlReg !Int !(NatRepr w)

instance TestEquality X87_ControlReg where
  testEquality (X87_ControlReg xi xw) (X87_ControlReg yi yw) =
    if xi == yi then
      testEquality xw yw
     else
      Nothing

instance OrdF X87_ControlReg where
  compareF (X87_ControlReg xi xw) (X87_ControlReg yi yw) =
    case compare xi yi of
      LT -> LTF
      GT -> GTF
      EQ -> compareF xw yw

instance HasRepr X87_ControlReg NatRepr where
  typeRepr (X87_ControlReg _ w) = w

instance Show (X87_ControlReg w) where
  show (X87_ControlReg i _) =
    case i of
      0 -> "im"
      1 -> "dm"
      2 -> "zm"
      3 -> "om"
      4 -> "um"
      5 -> "pm"
      8 -> "pc"
      10 -> "rc"
      12 -> "x"
      _  -> "reserved"

x87_IM :: X87_ControlReg 1
x87_IM = X87_ControlReg  0 n1

x87_DM :: X87_ControlReg 1
x87_DM = X87_ControlReg  1 n1

x87_ZM :: X87_ControlReg 1
x87_ZM = X87_ControlReg  2 n1

x87_OM :: X87_ControlReg 1
x87_OM = X87_ControlReg  3 n1

x87_UM :: X87_ControlReg 1
x87_UM = X87_ControlReg  4 n1

x87_PM :: X87_ControlReg 1
x87_PM = X87_ControlReg  5 n1

-- | X87 precision control field.
--
-- Values are:
-- 00 Single Precision (24 bits)
-- 01 Reserved
-- 10 Double Precision (53 bits)
-- 11 Double Extended Precision (64 bits)
x87_PC :: X87_ControlReg 2
x87_PC = X87_ControlReg  8 knownNat

-- | X87 rounding control field.  Values are:
--
-- 00 Round to nearest (even)
-- Rounded result is the closest to the infinitely precise result. If two
-- values are equally close, the result is the even value (that is, the one
-- with the least-significant bit of zero). Default
--
-- 01 Round down (toward −∞)
-- Rounded result is closest to but no greater than the infinitely precise result.
--
-- 10 Round up (toward +∞)
-- Rounded result is closest to but no less than the infinitely precise result.
--
-- 11 Round toward zero (Truncate)
-- Rounded result is closest to but no greater in absolute value than the
-- infinitely precise result.
x87_RC :: X87_ControlReg 2
x87_RC = X87_ControlReg 10 knownNat

x87_X :: X87_ControlReg 1
x87_X  = X87_ControlReg 12 n1

------------------------------------------------------------------------
-- RegisterName

data RegisterName tp
   = (tp ~ BVType 64)  => X86_IP
   | (tp ~ BVType 64)  => X86_GP !Int
     -- ^ One of 16 general purpose registers
   | (tp ~ BVType 1)   => X86_FlagReg !Int
     -- ^ One of 32 initial flag registers.
   | (tp ~ BVType 1)   => X87_StatusReg !Int
     -- ^ One of 16 x87 status registers
   | (tp ~ BVType 3)   => X87_TopReg
     -- X87 tag register.
   | (tp ~ BVType 2)   => X87_TagReg !Int
      -- One of 8 fpu/mmx registers.
   | (tp ~ BVType 80)  => X87_FPUReg !F.MMXReg
     -- One of 8 XMM registers
   | (tp ~ BVType 128) => X86_XMMReg   !Int

instance ShowF RegisterName where
  showF = show

instance Show (RegisterName cl) where
  show X86_IP          = "rip"
  show (X86_GP n)      = nm
    where Just nm = gpNames V.!? n
  show (X86_FlagReg n)    = nm
    where Just nm = flagNames V.!? n
  show (X87_StatusReg n) = nm
    where Just nm = x87StatusNames V.!? n
  show X87_TopReg      = "x87top"
  show (X87_TagReg n)  = "tag" ++ show n
  show (X87_FPUReg n)  = show n
  show (X86_XMMReg n)     = "xmm" ++ show n

instance TestEquality RegisterName where
  testEquality x y = orderingIsEqual (compareF x y)
    where
      -- FIXME: copied from Representation.hs, move
      orderingIsEqual :: OrderingF (x :: k) (y :: k) -> Maybe (x :~: y)
      orderingIsEqual o =
        case o of
         LTF -> Nothing
         EQF -> Just Refl
         GTF -> Nothing

instance Eq (RegisterName cl) where
  r == r'
    | Just _ <- testEquality r r' = True
    | otherwise = False

instance OrdF RegisterName where
  compareF X86_IP            X86_IP            = EQF
  compareF X86_IP            _                 = LTF
  compareF _                 X86_IP            = GTF

  compareF (X86_GP n)        (X86_GP n')        = fromOrdering (compare n n')
  compareF X86_GP{}           _                 = LTF
  compareF _                 X86_GP{}           = GTF

  compareF (X86_FlagReg n)   (X86_FlagReg n')   = fromOrdering (compare n n')
  compareF X86_FlagReg{}         _              = LTF
  compareF _                 X86_FlagReg{}      = GTF

  compareF (X87_StatusReg n) (X87_StatusReg n') = fromOrdering (compare n n')
  compareF X87_StatusReg{}    _                 = LTF
  compareF _                 X87_StatusReg{}    = GTF

  compareF X87_TopReg         X87_TopReg        = EQF
  compareF X87_TopReg         _                 = LTF
  compareF _                 X87_TopReg         = GTF

  compareF (X87_TagReg n)     (X87_TagReg n')     = fromOrdering (compare n n')
  compareF X87_TagReg{}       _                  = LTF
  compareF _                 X87_TagReg{}        = GTF

  compareF (X87_FPUReg n)     (X87_FPUReg n')     = fromOrdering (compare n n')
  compareF X87_FPUReg{}       _                  = LTF
  compareF _                 X87_FPUReg{}        = GTF

  compareF (X86_XMMReg n)        (X86_XMMReg n')        = fromOrdering (compare n n')

instance Ord (RegisterName cl) where
  a `compare` b = case a `compareF` b of
    GTF -> GT
    EQF -> EQ
    LTF -> LT

registerWidth :: RegisterName cl -> NatRepr (TypeBits cl)
registerWidth X86_IP           = knownNat
registerWidth X86_GP{}         = knownNat
registerWidth X86_FlagReg{}       = knownNat
registerWidth X87_StatusReg{}  = knownNat
registerWidth X87_TopReg       = knownNat
registerWidth X87_TagReg{}     = knownNat
registerWidth X87_FPUReg{}     = knownNat
registerWidth X86_XMMReg{}        = knownNat

------------------------------------------------------------------------
-- Exported constructors and their conversion to words

-- | A description of how a sub-word may be extracted from a word. If a bit isn't
-- constant or from a register it is reserved.
data BitConversion n = forall m n'. (1 <= n', n' <= n)
                       => RegisterBit (RegisterName (BVType n')) (NatRepr m)
                     | forall m. (m + 1 <= n) => ConstantBit Bool (NatRepr m)

-- | A description of how a particular status word is packed/unpacked into sub-bits
data BitPacking (n :: Nat) = BitPacking (NatRepr n) [BitConversion n]


-- | Instruction Pointer
rip :: RegisterName ProgramCounter
rip = X86_IP

-- | GPRs
rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp :: RegisterName GP
rax = X86_GP 0
rcx = X86_GP 1
rdx = X86_GP 2
rbx = X86_GP 3
rsp = X86_GP 4
rbp = X86_GP 5
rsi = X86_GP 6
rdi = X86_GP 7

r8 :: RegisterName GP
r8 = X86_GP 8

r9 :: RegisterName GP
r9 = X86_GP 9

r10 :: RegisterName GP
r10 = X86_GP 10

r11 :: RegisterName GP
r11 = X86_GP 11

r12 :: RegisterName GP
r12 = X86_GP 12

r13 :: RegisterName GP
r13 = X86_GP 13

r14 :: RegisterName GP
r14 = X86_GP 14

r15 :: RegisterName GP
r15 = X86_GP 15

gpNames :: V.Vector String
gpNames = V.fromList $
  ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"]
  ++ map ((++) "r" . show) [(8 :: Int) .. 15]

-- | Flags
cf, pf, af, zf, sf, tf, iflag, df, oflag :: RegisterName Flag
cf    = X86_FlagReg 0
pf    = X86_FlagReg 2
af    = X86_FlagReg 4
zf    = X86_FlagReg 6
sf    = X86_FlagReg 7
tf    = X86_FlagReg 8
iflag = X86_FlagReg 9
df    = X86_FlagReg 10
oflag = X86_FlagReg 11

flagNames :: V.Vector String
flagNames = V.fromList
  [ "cf", "RESERVED", "pf", "RESERVED", "af", "RESERVED"
  , "zf", "sf",       "tf", "if",       "df", "of"
  , "iopl", "nt", "SBZ", "rf", "vm", "ac", "vif", "vip", "id"
  ]

-- | x87 flags
pattern X87IE :: RegisterName X87_Status
pattern X87IE = X87_StatusReg 0

pattern X87DE :: RegisterName X87_Status
pattern X87DE = X87_StatusReg 1

pattern X87ZE :: RegisterName X87_Status
pattern X87ZE = X87_StatusReg 2

pattern X87OE :: RegisterName X87_Status
pattern X87OE = X87_StatusReg 3

pattern X87UE :: RegisterName X87_Status
pattern X87UE = X87_StatusReg 4

pattern X87PE :: RegisterName X87_Status
pattern X87PE = X87_StatusReg 5

pattern X87EF :: RegisterName X87_Status
pattern X87EF = X87_StatusReg 6

pattern X87ES :: RegisterName X87_Status
pattern X87ES = X87_StatusReg 7

pattern X87C0 :: RegisterName X87_Status
pattern X87C0 = X87_StatusReg 8

pattern X87C1 :: RegisterName X87_Status
pattern X87C1 = X87_StatusReg 9

pattern X87C2 :: RegisterName X87_Status
pattern X87C2 = X87_StatusReg 10

pattern X87C3 :: RegisterName X87_Status
pattern X87C3 = X87_StatusReg 14

x87StatusNames :: V.Vector String
x87StatusNames = V.fromList $
  [ "ie", "de", "ze", "oe",       "ue",       "pe",       "ef", "es"
  , "c0", "c1", "c2", "RESERVED", "RESERVED", "RESERVED", "c3", "RESERVED"
  ]

------------------------------------------------------------------------
-- Conversions from flexdis state names

-- This allows us to perhaps compact the representation so we are not
-- reliant on the layout of x86 registers/bits

xmmFromFlexdis :: F.XMMReg -> RegisterName XMM
xmmFromFlexdis xmm    = X86_XMMReg (fromIntegral $ F.xmmRegNo xmm)

gpFromFlexdis :: F.Reg64 -> RegisterName GP
gpFromFlexdis r = X86_GP (fromIntegral $ F.reg64No r)
