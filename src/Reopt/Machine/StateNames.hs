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
  , mmxFromFlexdis
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
import           GHC.TypeLits

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

data RegisterName cl
   = (cl ~ ProgramCounter) => IPReg
   | (cl ~ GP) => GPReg !Int
     -- ^ One of 16 general purpose registers
   | (cl ~ Flag)    => FlagReg !Int
     -- ^ One of 32 initial flag registers.
   | (cl ~ X87_Status) => X87StatusReg !Int
     -- ^ One of 16 x87 status registers
   | (cl ~ X87_Top)    => X87TopReg
     -- X87 tag register.
   | (cl ~ X87_Tag) => X87TagReg !Int
      -- One of 8 fpu/mmx registers.
   | (cl ~ X87_FPU) => X87FPUReg !Int
     -- One of 8 XMM registers
   | (cl ~ XMM)   => XMMReg   !Int

x87StatusRegs :: [RegisterName X87_Status]
x87StatusRegs = [X87StatusReg i | i <- [0..15]]

x87TagRegs :: [RegisterName X87_Tag]
x87TagRegs = [X87TagReg i | i <- [0..7]]

x87FPURegs :: [RegisterName X87_FPU]
x87FPURegs = [X87FPUReg i | i <- [0..7]]

xmmRegs :: [RegisterName XMM]
xmmRegs = [XMMReg i | i <- [0..7]]

instance ShowF RegisterName where
  showF = show

instance Show (RegisterName cl) where
  show IPReg          = "rip"
  show (GPReg n)      = nm
    where Just nm = gpNames V.!? n
  show (FlagReg n)    = nm
    where Just nm = flagNames V.!? n
  show (X87StatusReg n) = nm
    where Just nm = x87StatusNames V.!? n
  show X87TopReg      = "x87top"
  show (X87TagReg n)  = "tag" ++ show n
  show (X87FPUReg n)  = "fpu" ++ show n
  show (XMMReg n)     = "xmm" ++ show n

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
  compareF IPReg             IPReg              = EQF
  compareF (GPReg n)         (GPReg n')         = fromOrdering (compare n n')
  compareF (FlagReg n)       (FlagReg n')       = fromOrdering (compare n n')
  compareF (X87StatusReg n)  (X87StatusReg n')  = fromOrdering (compare n n')
  compareF X87TopReg         X87TopReg          = EQF
  compareF (X87TagReg n)     (X87TagReg n')     = fromOrdering (compare n n')
  compareF (X87FPUReg n)     (X87FPUReg n')     = fromOrdering (compare n n')
  compareF (XMMReg n)        (XMMReg n')        = fromOrdering (compare n n')
  compareF IPReg             _                  = LTF
  compareF _                 IPReg              = GTF

  compareF GPReg{}           _                  = LTF
  compareF _                 GPReg{}            = GTF

  compareF FlagReg{}         _                  = LTF
  compareF _                 FlagReg{}          = GTF

  compareF X87StatusReg{}    _                  = LTF
  compareF _                 X87StatusReg{}     = GTF

  compareF X87TopReg         _                  = LTF
  compareF _                 X87TopReg          = GTF

  compareF X87TagReg{}       _                  = LTF
  compareF _                 X87TagReg{}        = GTF

  compareF X87FPUReg{}       _                  = LTF
  compareF _                 X87FPUReg{}        = GTF

  compareF XMMReg{}          _                  = LTF
  compareF _                 XMMReg{}           = GTF

instance Ord (RegisterName cl) where
  a `compare` b = case a `compareF` b of
    GTF -> GT
    EQF -> EQ
    LTF -> LT

registerWidth :: RegisterName cl -> NatRepr (TypeBits cl)
registerWidth IPReg           = knownNat
registerWidth GPReg{}         = knownNat
registerWidth FlagReg{}       = knownNat
registerWidth X87StatusReg{}  = knownNat
registerWidth X87TopReg       = knownNat
registerWidth X87TagReg{}     = knownNat
registerWidth X87FPUReg{}     = knownNat
registerWidth XMMReg{}        = knownNat

--type RegisterType cl = cl

registerType :: RegisterName cl -> TypeRepr cl
registerType IPReg           = knownType
registerType GPReg{}         = knownType
registerType FlagReg{}       = knownType
registerType X87StatusReg{}  = knownType
registerType X87TopReg       = knownType
registerType X87TagReg{}     = knownType
registerType X87FPUReg{}     = knownType
registerType XMMReg{}        = knownType

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
rip = IPReg

-- | GPRs
rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp :: RegisterName GP
rax = GPReg 0
rcx = GPReg 1
rdx = GPReg 2
rbx = GPReg 3
rsp = GPReg 4
rbp = GPReg 5
rsi = GPReg 6
rdi = GPReg 7

r8 :: RegisterName GP
r8 = GPReg 8

r9 :: RegisterName GP
r9 = GPReg 9

r10 :: RegisterName GP
r10 = GPReg 10

r11 :: RegisterName GP
r11 = GPReg 11

r12 :: RegisterName GP
r12 = GPReg 12

r13 :: RegisterName GP
r13 = GPReg 13

r14 :: RegisterName GP
r14 = GPReg 14

r15 :: RegisterName GP
r15 = GPReg 15

gpNames :: V.Vector String
gpNames = V.fromList $
  ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"]
  ++ map ((++) "r" . show) [(8 :: Int) .. 15]

-- | Flags
cf, pf, af, zf, sf, tf, iflag, df, oflag :: RegisterName Flag
cf    = FlagReg 0
pf    = FlagReg 2
af    = FlagReg 4
zf    = FlagReg 6
sf    = FlagReg 7
tf    = FlagReg 8
iflag = FlagReg 9
df    = FlagReg 10
oflag = FlagReg 11

flagNames :: V.Vector String
flagNames = V.fromList
  [ "cf", "RESERVED", "pf", "RESERVED", "af", "RESERVED"
  , "zf", "sf",       "tf", "if",       "df", "of"
  , "iopl", "nt", "SBZ", "rf", "vm", "ac", "vif", "vip", "id"
  ]

-- FIXME: missing the system flags
flagBitPacking :: BitPacking 64
flagBitPacking = BitPacking knownNat $ flagBits ++ constants
  where
    -- flags     = [cf, pf, af, zf, sf, tf, iflag, df, oflag]
    flagBits  = [ RegisterBit cf    (knownNat :: NatRepr 0)
                , RegisterBit pf    (knownNat :: NatRepr 2)
                , RegisterBit af    (knownNat :: NatRepr 4)
                , RegisterBit zf    (knownNat :: NatRepr 6)
                , RegisterBit sf    (knownNat :: NatRepr 7)
                , RegisterBit tf    (knownNat :: NatRepr 8)
                , RegisterBit iflag (knownNat :: NatRepr 9 )
                , RegisterBit df    (knownNat :: NatRepr 10)
                , RegisterBit oflag (knownNat :: NatRepr 11)
                ]

    constants = [ ConstantBit True  (knownNat :: NatRepr 1)
                , ConstantBit False (knownNat :: NatRepr 3)
                , ConstantBit False (knownNat :: NatRepr 5)
                , ConstantBit False (knownNat :: NatRepr 15)
                , ConstantBit False (knownNat :: NatRepr 22)
                , ConstantBit False (knownNat :: NatRepr 23)
                , ConstantBit False (knownNat :: NatRepr 24)
                , ConstantBit False (knownNat :: NatRepr 25)
                , ConstantBit False (knownNat :: NatRepr 26)
                , ConstantBit False (knownNat :: NatRepr 27)
                , ConstantBit False (knownNat :: NatRepr 28)
                , ConstantBit False (knownNat :: NatRepr 29)
                , ConstantBit False (knownNat :: NatRepr 30)
                , ConstantBit False (knownNat :: NatRepr 31)
                ]

-- | x87 flags
pattern X87IE :: RegisterName X87_Status
pattern X87IE = X87StatusReg 0

pattern X87DE :: RegisterName X87_Status
pattern X87DE = X87StatusReg 1

pattern X87ZE :: RegisterName X87_Status
pattern X87ZE = X87StatusReg 2

pattern X87OE :: RegisterName X87_Status
pattern X87OE = X87StatusReg 3

pattern X87UE :: RegisterName X87_Status
pattern X87UE = X87StatusReg 4

pattern X87PE :: RegisterName X87_Status
pattern X87PE = X87StatusReg 5

pattern X87EF :: RegisterName X87_Status
pattern X87EF = X87StatusReg 6

pattern X87ES :: RegisterName X87_Status
pattern X87ES = X87StatusReg 7

pattern X87C0 :: RegisterName X87_Status
pattern X87C0 = X87StatusReg 8

pattern X87C1 :: RegisterName X87_Status
pattern X87C1 = X87StatusReg 9

pattern X87C2 :: RegisterName X87_Status
pattern X87C2 = X87StatusReg 10

pattern X87C3 :: RegisterName X87_Status
pattern X87C3 = X87StatusReg 14

x87StatusNames :: V.Vector String
x87StatusNames = V.fromList $
  [ "ie", "de", "ze", "oe",       "ue",       "pe",       "ef", "es"
  , "c0", "c1", "c2", "RESERVED", "RESERVED", "RESERVED", "c3", "RESERVED"
  ]

-- FIXME: what about Busy bit
x87StatusBitPacking :: BitPacking 16
x87StatusBitPacking = BitPacking knownNat $ flags
                      ++ [RegisterBit X87TopReg (knownNat :: NatRepr 11)]
  where
    flags = [ RegisterBit X87IE (knownNat :: NatRepr 0 )
            , RegisterBit X87DE (knownNat :: NatRepr 1 )
            , RegisterBit X87ZE (knownNat :: NatRepr 2 )
            , RegisterBit X87OE (knownNat :: NatRepr 3 )
            , RegisterBit X87UE (knownNat :: NatRepr 4 )
            , RegisterBit X87PE (knownNat :: NatRepr 5 )
            , RegisterBit X87EF (knownNat :: NatRepr 6 )
            , RegisterBit X87ES (knownNat :: NatRepr 7 )
            , RegisterBit X87C0 (knownNat :: NatRepr 8 )
            , RegisterBit X87C1 (knownNat :: NatRepr 9 )
            , RegisterBit X87C2     (knownNat :: NatRepr 10)
            , RegisterBit X87TopReg (knownNat :: NatRepr 11)
            , RegisterBit X87C3     (knownNat :: NatRepr 14)
            ]

------------------------------------------------------------------------
-- Conversions from flexdis state names

-- This allows us to perhaps compact the representation so we are not
-- reliant on the layout of x86 registers/bits

-- FIXME: should we expose this?
mmxFromFlexdis :: F.MMXReg -> RegisterName X87_FPU
mmxFromFlexdis mmx    = X87FPUReg (fromIntegral $ F.mmxRegNo mmx)

xmmFromFlexdis :: F.XMMReg -> RegisterName XMM
xmmFromFlexdis xmm    = XMMReg (fromIntegral $ F.xmmRegNo xmm)

gpFromFlexdis :: F.Reg64 -> RegisterName GP
gpFromFlexdis r = GPReg (fromIntegral $ F.reg64No r)
