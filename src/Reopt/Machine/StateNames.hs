------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.StateNames
-- Description      : Defines basic data types used for naming x86 state
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This defines the data types needed to names the various x86 registers
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-} -- for Eq
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternGuards #-}

module Reopt.Machine.StateNames where


import Data.Parameterized.Classes
import qualified Data.Vector as V
import GHC.TypeLits

import qualified Flexdis86 as F
import Reopt.Machine.Types

data RegisterClass = ProgramCounter
                   | GP | Flag | Segment | Control
                   | Debug
                   | X87_Tag | X87_FPU | X87_Status | X87_Control | X87_Top | X87_ControlMask
                   | XMM

data RegisterClassRepr (r :: RegisterClass) where
  ProgramCounterRepr  :: RegisterClassRepr 'ProgramCounter
  GPRepr              :: RegisterClassRepr 'GP
  FlagRepr            :: RegisterClassRepr 'Flag
  SegmentRepr         :: RegisterClassRepr 'Segment
  ControlRepr         :: RegisterClassRepr 'Control
  DebugRepr           :: RegisterClassRepr 'Debug
  X87_FPURepr         :: RegisterClassRepr 'X87_FPU
  X87_StatusRepr      :: RegisterClassRepr 'X87_Status
  X87_TopRepr         :: RegisterClassRepr 'X87_Top
  X87_TagRepr         :: RegisterClassRepr 'X87_Tag
  X87_ControlRepr     :: RegisterClassRepr 'X87_Control
  X87_ControlMaskRepr :: RegisterClassRepr 'X87_ControlMask
  XMMRepr             :: RegisterClassRepr 'XMM

{-
type family NRegisters (r :: RegisterClass) :: Nat where
  NRegisters ProgramCounter = 1
  NRegisters GP             = 16
  NRegisters Flag           = 64
  NRegisters Segment        = 6
  NRegisters Debug          = 8
  NRegisters X87_FPU        = 8
  NRegisters X87_Status     = 16
  NRegisters X87_Tag        = 8
  NRegisters X87_Control    = 16
  NRegisters XMM            = 16

nRegisters :: RegisterClassRepr r -> NatRepr (NRegisters r)
nRegisters ProgramCounterRepr = knownNat
nRegisters GPRepr             = knownNat
nRegisters FlagRepr           = knownNat
nRegisters SegmentRepr        = knownNat
nRegisters DebugRepr          = knownNat
nRegisters X87_FPURepr        = knownNat
nRegisters X87_FlagRepr       = knownNat
nRegisters X87_TagRepr        = knownNat
nRegisters X87_ControlRepr    = knownNat
nRegisters XMMRepr            = knownNat
-}

type family RegisterClassBits (cl :: RegisterClass) :: Nat where
  RegisterClassBits 'ProgramCounter = 64
  RegisterClassBits 'GP             = 64
  RegisterClassBits 'Flag           = 1
  RegisterClassBits 'Segment        = 16
  RegisterClassBits 'Debug          = 64
  RegisterClassBits 'Control        = 64
  RegisterClassBits 'X87_FPU         = 80
  RegisterClassBits 'X87_Status      = 1
  RegisterClassBits 'X87_Top         = 3
  RegisterClassBits 'X87_Tag         = 2
  RegisterClassBits 'X87_ControlMask = 1
  RegisterClassBits 'X87_Control     = 2
  RegisterClassBits 'XMM             = 128


type RegisterType (cl :: RegisterClass) = BVType (RegisterClassBits cl)

data RegisterName cl where
  IPReg      :: RegisterName 'ProgramCounter

  GPReg      :: !Int -> RegisterName 'GP

  SegmentReg :: !Int -> RegisterName 'Segment

  -- One of 32 initial flag registers.
  FlagReg    :: !Int -> RegisterName 'Flag

  ControlReg :: !Int -> RegisterName 'Control

  X87StatusReg :: !Int -> RegisterName 'X87_Status
  X87TopReg    :: RegisterName 'X87_Top

  -- FIXME: These are currently read-only
  X87ControlReg :: !Int -> RegisterName 'X87_ControlMask

  X87PC      :: RegisterName 'X87_Control
  X87RC      :: RegisterName 'X87_Control

  -- X87 tag register.
  X87TagReg :: !Int -> RegisterName 'X87_Tag

  -- One of 8 fpu/mmx registers.
  X87FPUReg :: !Int -> RegisterName 'X87_FPU

  -- One of 8 XMM registers
  XMMReg :: !Int -> RegisterName 'XMM

  DebugReg :: !Int -> RegisterName 'Debug

gpRegs :: [RegisterName 'GP]
gpRegs = [GPReg i | i <- [0..15]]

flagRegs :: [RegisterName 'Flag]
flagRegs = [FlagReg i | i <- [0,2,4,6,7,8,9,10,11]]

x87StatusRegs :: [RegisterName 'X87_Status]
x87StatusRegs = [X87StatusReg i | i <- [0..15]]

x87ControlRegs :: [RegisterName 'X87_ControlMask]
x87ControlRegs = [X87ControlReg i | i <- [0..15]]

x87TagRegs :: [RegisterName 'X87_Tag]
x87TagRegs = [X87TagReg i | i <- [0..7]]

x87FPURegs :: [RegisterName 'X87_FPU]
x87FPURegs = [X87FPUReg i | i <- [0..7]]

xmmRegs :: [RegisterName 'XMM]
xmmRegs = [XMMReg i | i <- [0..7]]

instance ShowF RegisterName where
  showF = show

instance Show (RegisterName cl) where
  show IPReg          = "rip"
  show (GPReg n)      = nm
    where Just nm = gpNames V.!? n
  show (SegmentReg n) = nm
    where Just nm = segmentNames V.!? n
  show (FlagReg n)    = nm
    where Just nm = flagNames V.!? n
  show (ControlReg n) = "cr" ++ show n
  show (X87StatusReg n) = nm
    where Just nm = x87StatusNames V.!? n
  show X87TopReg      = "x87top"
  show (X87ControlReg n)  = nm
    where Just nm = x87ControlNames V.!? n
  show X87PC          = "pc"
  show X87RC          = "rc"
  show (X87TagReg n)  = "tag" ++ show n
  show (X87FPUReg n)  = "fpu" ++ show n
  show (XMMReg n)     = "xmm" ++ show n
  show (DebugReg n)   = "dr"  ++ show n

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
  compareF (SegmentReg n)    (SegmentReg n')    = fromOrdering (compare n n')
  compareF (FlagReg n)       (FlagReg n')       = fromOrdering (compare n n')
  compareF (ControlReg n)    (ControlReg n')    = fromOrdering (compare n n')
  compareF (X87StatusReg n)  (X87StatusReg n')  = fromOrdering (compare n n')
  compareF X87TopReg         X87TopReg          = EQF
  compareF (X87ControlReg n) (X87ControlReg n') = fromOrdering (compare n n')
  compareF X87PC             X87PC              = EQF
  compareF X87RC             X87RC              = EQF
  compareF (X87TagReg n)     (X87TagReg n')     = fromOrdering (compare n n')
  compareF (X87FPUReg n)     (X87FPUReg n')     = fromOrdering (compare n n')
  compareF (XMMReg n)        (XMMReg n')        = fromOrdering (compare n n')
  compareF (DebugReg n)      (DebugReg n')      = fromOrdering (compare n n')
  compareF IPReg             _                  = LTF
  compareF _                 IPReg              = GTF

  compareF GPReg{}           _                  = LTF
  compareF _                 GPReg{}            = GTF

  compareF SegmentReg{}      _                  = LTF
  compareF _                 SegmentReg{}       = GTF

  compareF FlagReg{}         _                  = LTF
  compareF _                 FlagReg{}          = GTF

  compareF ControlReg{}      _                  = LTF
  compareF _                 ControlReg{}       = GTF

  compareF X87StatusReg{}    _                  = LTF
  compareF _                 X87StatusReg{}     = GTF

  compareF X87TopReg         _                  = LTF
  compareF _                 X87TopReg          = GTF

  compareF X87ControlReg{}   _                  = LTF
  compareF _                 X87ControlReg{}    = GTF

  compareF X87PC             _                  = LTF
  compareF _                 X87PC              = GTF

  compareF X87RC             _                  = LTF
  compareF _                 X87RC              = GTF

  compareF X87TagReg{}       _                  = LTF
  compareF _                 X87TagReg{}        = GTF

  compareF X87FPUReg{}       _                  = LTF
  compareF _                 X87FPUReg{}        = GTF

  compareF XMMReg{}          _                  = LTF
  compareF _                 XMMReg{}           = GTF

  -- compareF DebugReg{}        _                  = LTF
  -- compareF _                 DebugReg{}         = GTF

instance Eq (RegisterName cl) where
  a == b = case a `compareF` b of
    GTF -> False
    EQF -> True
    LTF -> False

instance Ord (RegisterName cl) where
  a `compare` b = case a `compareF` b of
    GTF -> GT
    EQF -> EQ
    LTF -> LT

registerWidth :: RegisterName cl -> NatRepr (RegisterClassBits cl)
registerWidth IPReg           = knownNat
registerWidth GPReg{}         = knownNat
registerWidth FlagReg{}       = knownNat
registerWidth ControlReg{}    = knownNat
registerWidth X87ControlReg{} = knownNat
registerWidth X87StatusReg{}  = knownNat
registerWidth X87TopReg       = knownNat
registerWidth X87PC           = knownNat
registerWidth X87RC           = knownNat
registerWidth X87TagReg{}     = knownNat
registerWidth X87FPUReg{}     = knownNat
registerWidth XMMReg{}        = knownNat
registerWidth SegmentReg{}    = knownNat
registerWidth DebugReg{}      = knownNat

registerType :: RegisterName cl -> TypeRepr (RegisterType cl)
registerType IPReg           = knownType
registerType GPReg{}         = knownType
registerType FlagReg{}       = knownType
registerType ControlReg{}    = knownType
registerType X87ControlReg{} = knownType
registerType X87StatusReg{}  = knownType
registerType X87TopReg       = knownType
registerType X87PC           = knownType
registerType X87RC           = knownType
registerType X87TagReg{}     = knownType
registerType X87FPUReg{}     = knownType
registerType XMMReg{}        = knownType
registerType SegmentReg{}    = knownType
registerType DebugReg{}      = knownType

------------------------------------------------------------------------
-- Exported constructors and their conversion to words

-- | A description of how a particular status word is packed/unpacked into sub-bits
data BitPacking (n :: Nat) = BitPacking (NatRepr n) [BitConversion n]

-- | A description of how a sub-word may be extracted from a word. If a bit isn't
-- constant or from a register it is reserved.
data BitConversion n = -- forall cl m. (m + TypeBits (RegisterType cl) <= n)
                      forall cl m n'. (RegisterType cl ~ BVType n', 1 <= n', n' <= n)
                       => RegisterBit (RegisterName cl) (NatRepr m)
                     | forall m. (m + 1 <= n) => ConstantBit Bool (NatRepr m)

-- | Instruction Pointer
rip :: RegisterName 'ProgramCounter
rip = IPReg

-- | GPRs
rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp :: RegisterName 'GP
rax = GPReg 0
rcx = GPReg 1
rdx = GPReg 2
rbx = GPReg 3
rsp = GPReg 4
rbp = GPReg 5
rsi = GPReg 6
rdi = GPReg 7

r8 :: RegisterName 'GP
r8 = GPReg 8

r9 :: RegisterName 'GP
r9 = GPReg 9

r10 :: RegisterName 'GP
r10 = GPReg 10

r11 :: RegisterName 'GP
r11 = GPReg 11

r12 :: RegisterName 'GP
r12 = GPReg 12

r13 :: RegisterName 'GP
r13 = GPReg 13

r14 :: RegisterName 'GP
r14 = GPReg 14

r15 :: RegisterName 'GP
r15 = GPReg 15


gpCount :: Int
gpCount = 16

gpNames :: V.Vector String
gpNames = V.fromList $
  ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"]
  ++ map ((++) "r" . show) [(8 :: Int) .. 15]

-- | Segments
es, cs, ss, ds, fs, gs :: RegisterName 'Segment
es = SegmentReg 0
cs = SegmentReg 1
ss = SegmentReg 2
ds = SegmentReg 3
fs = SegmentReg 4
gs = SegmentReg 5

segmentNames :: V.Vector String
segmentNames = V.fromList ["es", "cs", "ss", "ds", "fs", "gs"]

-- | Flags
cf, pf, af, zf, sf, tf, iflag, df, oflag :: RegisterName 'Flag
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
x87ie, x87de, x87ze, x87oe, x87ue, x87pe, x87ef, x87es,
  x87c0, x87c1, x87c2, x87c3 :: RegisterName 'X87_Status
x87ie = X87StatusReg 0
x87de = X87StatusReg 1
x87ze = X87StatusReg 2
x87oe = X87StatusReg 3
x87ue = X87StatusReg 4
x87pe = X87StatusReg 5
x87ef = X87StatusReg 6
x87es = X87StatusReg 7
x87c0 = X87StatusReg 8
x87c1 = X87StatusReg 9
x87c2 = X87StatusReg 10
x87c3 = X87StatusReg 14

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
    flags = [ RegisterBit x87ie (knownNat :: NatRepr 0 )
            , RegisterBit x87de (knownNat :: NatRepr 1 )
            , RegisterBit x87ze (knownNat :: NatRepr 2 )
            , RegisterBit x87oe (knownNat :: NatRepr 3 )
            , RegisterBit x87ue (knownNat :: NatRepr 4 )
            , RegisterBit x87pe (knownNat :: NatRepr 5 )
            , RegisterBit x87ef (knownNat :: NatRepr 6 )
            , RegisterBit x87es (knownNat :: NatRepr 7 )
            , RegisterBit x87c0 (knownNat :: NatRepr 8 )
            , RegisterBit x87c1 (knownNat :: NatRepr 9 )
            , RegisterBit x87c2 (knownNat :: NatRepr 10)
            , RegisterBit X87TopReg (knownNat :: NatRepr 11)
            , RegisterBit x87c3 (knownNat :: NatRepr 14)
            ]

x87ControlNames :: V.Vector String
x87ControlNames = V.fromList $
  ["im", "dm", "zm", "om", "um", "pm"]
  ++ replicate 6 "RESERVED" -- includes rc and pc
  ++ ["x"]
  ++ replicate 3 "RESERVED"

x87im, x87dm, x87zm, x87om, x87um, x87pm, x87x :: RegisterName 'X87_ControlMask
x87im = X87ControlReg 0
x87dm = X87ControlReg 1
x87zm = X87ControlReg 2
x87om = X87ControlReg 3
x87um = X87ControlReg 4
x87pm = X87ControlReg 5
x87x  = X87ControlReg 12

x87ControlBitPacking :: BitPacking 16
x87ControlBitPacking = BitPacking knownNat flags
  where
    flags = [ RegisterBit x87im (knownNat :: NatRepr 0 )
            , RegisterBit x87dm (knownNat :: NatRepr 1 )
            , RegisterBit x87zm (knownNat :: NatRepr 2 )
            , RegisterBit x87om (knownNat :: NatRepr 3 )
            , RegisterBit x87um (knownNat :: NatRepr 4 )
            , RegisterBit x87pm (knownNat :: NatRepr 5 )
            , RegisterBit X87PC (knownNat :: NatRepr 8 )
            , RegisterBit X87RC (knownNat :: NatRepr 10)
            , RegisterBit x87x  (knownNat :: NatRepr 12)
            ]

------------------------------------------------------------------------
-- Conversions from flexdis state names

-- This allows us to perhaps compact the representation so we are not
-- reliant on the layout of x86 registers/bits

-- FIXME: maybe these should be in FlexdisMatcher.hs?
controlFromFlexdis :: F.ControlReg -> RegisterName 'Control
controlFromFlexdis cr = ControlReg (fromIntegral $ F.controlRegNo cr)

debugFromFlexdis :: F.DebugReg -> RegisterName 'Debug
debugFromFlexdis dr   = DebugReg (fromIntegral $ F.debugRegNo dr)

-- FIXME: should we expose this?
mmxFromFlexdis :: F.MMXReg -> RegisterName 'X87_FPU
mmxFromFlexdis mmx    = X87FPUReg (fromIntegral $ F.mmxRegNo mmx)

xmmFromFlexdis :: F.XMMReg -> RegisterName 'XMM
xmmFromFlexdis xmm    = XMMReg (fromIntegral $ F.xmmRegNo xmm)

segmentFromFlexdis :: F.Segment -> RegisterName 'Segment
segmentFromFlexdis s  = SegmentReg (fromIntegral $ F.segmentRegNo s)

gpFromFlexdis :: F.Reg64 -> RegisterName 'GP
gpFromFlexdis r       = GPReg (fromIntegral $ F.reg64No r)
