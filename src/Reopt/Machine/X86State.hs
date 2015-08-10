{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Machine.X86State
  ( X86State
  , mkX86State
  , mkX86StateM
  , curIP
  , reg64Regs
  , flagRegs
  , x87ControlWord
  , x87StatusWord
  , x87TopReg
  , x87TagWords
  , x87Regs
  , xmmRegs
    -- * Combinators
  , register
  , foldX86StateValue
  , zipWithX86State
  , mapX86State
  , cmpX86State
    -- * Equality
  , EqF(..)
    -- * Pretty printing
  , PrettyRegValue(..)
  , X87StatusWord
    -- * Utilities
  , x86StateRegisters
  ) where

import           Control.Lens
import           Data.Maybe

import           Data.Parameterized.Classes (EqF(..))
import           Data.Parameterized.Some
import qualified Data.Vector as V
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Utils.PrettyPrint

------------------------------------------------------------------------
-- X87StatusWord

{-
vec :: Simple Lens

x87_ie :: Simple Lens X87StatusWord (Value BoolType)
x87_ie = vec 0

x87_ie :: Simple Lens X87StatusWord (Value BoolType)
x87_ie = vec 0


x87_de :: Simple Lens X87StatusWord (Value BoolType)
x87_de = lens _x87_de (\s v -> s { _x87_de = v })

x87_ze :: Simple Lens X87StatusWord (Value BoolType)
x87_ze = lens _x87_ze (\s v -> s { _x87_ze = v })

x87_oe :: Simple Lens X87StatusWord (Value BoolType)
x87_oe = lens _x87_oe (\s v -> s { _x87_oe = v })

x87_ue :: Simple Lens X87StatusWord (Value BoolType)
x87_ue = lens _x87_ue (\s v -> s { _x87_ue = v })

x87_pe :: Simple Lens X87StatusWord (Value BoolType)
x87_pe = lens _x87_pe (\s v -> s { _x87_pe = v })

x87_ef :: Simple Lens X87StatusWord (Value BoolType)
x87_ef = lens _x87_ef (\s v -> s { _x87_ef = v })

x87_es :: Simple Lens X87StatusWord (Value BoolType)
x87_es = lens _x87_es (\s v -> s { _x87_es = v })

x87_c0 :: Simple Lens X87StatusWord (Value BoolType)
x87_c0 = lens _x87_c0 (\s v -> s { _x87_c0 = v })

x87_c1 :: Simple Lens X87StatusWord (Value BoolType)
x87_c1 = lens _x87_c1 (\s v -> s { _x87_c1 = v })

x87_c2 :: Simple Lens X87StatusWord (Value BoolType)
x87_c2 = lens _x87_c2 (\s v -> s { _x87_c2 = v })

x87_c3 :: Simple Lens X87StatusWord (Value BoolType)
x87_c3 = lens _x87_c3 (\s v -> s { _x87_c3 = v })

x87_top :: Simple Lens X87StatusWord (Value (BVType 3))
x87_top = lens _x87_top (\s v -> s { _x87_top = v })

x87_busy :: Simple Lens X87StatusWord (Value BoolType)
x87_busy = lens _x87_busy (\s v -> s { _x87_busy = v })
-}

type X87StatusWord f = V.Vector (f BoolType)


-- | List of registers stored in X86State
x86StateRegisters :: [Some N.RegisterName]
x86StateRegisters
  = [Some N.IPReg]
  ++ (Some <$> N.gpRegs)
  ++ (Some <$> N.flagRegs)
  ++ (Some <$> N.x87ControlRegs)
  ++ (Some <$> N.x87StatusRegs)
  ++ [Some N.X87TopReg]
  ++ (Some <$> N.x87TagRegs)
  ++ (Some <$> N.x87FPURegs)
  ++ (Some <$> N.xmmRegs)

------------------------------------------------------------------------
-- X86State

-- | This represents the state of the processor registers after some
-- execution.
data X86State f = X86State
     { _curIP  :: !(f (BVType 64))
       -- 16 general purposes registers.
     , _reg64Regs :: !(V.Vector (f (BVType 64)))
       -- 32 individual bits in the flags register.
     , _flagRegs  :: !(V.Vector (f BoolType))
       -- The 16-bit x87 control word.
       -- See:
       --   Intel® 64 and IA-32 Architectures Software Developer’s Manual
       --   Volume 1:
       --   Section 8.1.5
     , _x87ControlWord :: !(V.Vector (f BoolType))
       -- The x87 status word.
     , _x87StatusWord :: !(X87StatusWord f)
     , _x87TopReg     :: !(f (BVType 3))
       -- The 8 x87 tag words
       -- (used to indicate the status of different FPU registers).
     , _x87TagWords :: !(V.Vector (f (BVType 2)))
       -- The 8 x87 FPU regs (MMX registers alias these).
       -- MMX registers alias these with (for example).
       -- MM3 maps to R3.
     , _x87Regs   :: !(V.Vector (f (BVType  80)))
       -- One of 8 128-bit XMM registers
     , _xmmRegs   :: !(V.Vector (f (BVType 128)))
     }

-- | the value of the current instruction pointer.
curIP :: Simple Lens (X86State f) (f (BVType 64))
curIP = lens _curIP (\s v -> s { _curIP = v })

-- | Assignments to the 16 general-purpose registers.
reg64Regs :: Simple Lens (X86State f) (V.Vector (f (BVType 64)))
reg64Regs = lens _reg64Regs (\s v -> s { _reg64Regs = v })

-- | 32 individual bits in the flags register.
flagRegs :: Simple Lens (X86State f) (V.Vector (f BoolType))
flagRegs = lens _flagRegs (\s v -> s { _flagRegs = v })

-- | The current x87 control word.
x87ControlWord :: Simple Lens (X86State f) (V.Vector (f BoolType))
x87ControlWord = lens _x87ControlWord (\s v -> s { _x87ControlWord = v })

-- | The current x87 status word.
x87StatusWord :: Simple Lens (X86State f) (V.Vector (f BoolType))
x87StatusWord = lens _x87StatusWord (\s v -> s { _x87StatusWord = v })

-- | The 8 x87 tag words
-- used to indicate the status of different FPU registers.
x87TagWords :: Simple Lens (X86State f) (V.Vector (f (BVType 2)))
x87TagWords = lens _x87TagWords (\s v -> s { _x87TagWords = v })

-- | the value oDebugReg{}  f the current instruction pointer.
x87TopReg :: Simple Lens (X86State f) (f (BVType 3))
x87TopReg = lens _x87TopReg (\s v -> s { _x87TopReg = v })

-- | Assignments to the 8 80-bit FPU registers.
x87Regs :: Simple Lens (X86State f) (V.Vector (f (BVType 80)))
x87Regs = lens _x87Regs (\s v -> s { _x87Regs = v })

-- | Assignments to the 16 128-bit XMM registers.
xmmRegs :: Simple Lens (X86State f) (V.Vector (f (BVType 128)))
xmmRegs = lens _xmmRegs (\s v -> s { _xmmRegs = v })

register :: forall f cl
          . N.RegisterName cl
         -> Simple Lens (X86State f) (f (N.RegisterType cl))
register reg =
   case reg of
      N.IPReg           -> curIP
      N.GPReg n         -> reg64Regs . idx n
      N.FlagReg n       -> flagRegs . idx n
      N.X87ControlReg n -> x87ControlWord . idx n
      N.X87StatusReg n  -> x87StatusWord . idx n
      N.X87TopReg       -> x87TopReg
      N.X87TagReg n     -> x87TagWords . idx n
      N.X87FPUReg n     -> x87Regs . idx n
      N.XMMReg n        -> xmmRegs . idx n
      -- SegmentReg
      -- DebugReg
      -- ControlReg
      _               -> error $ "Unexpected reg: " ++ show reg
  where
    idx :: forall tp. Int -> Lens' (V.Vector (f tp)) (f tp)
    idx n = lens (V.! n) (\s v -> (ix n .~ v) s)

mkX86StateM :: (Applicative m, Monad m)
            => (forall cl. N.RegisterName cl -> m (f (N.RegisterType cl)))
            -> m (X86State f)
mkX86StateM f =
  X86State <$> f N.IPReg
           <*> V.generateM 16 (f . N.GPReg)
           <*> V.generateM 16 (f . N.FlagReg)
           <*> V.generateM 16 (f . N.X87ControlReg)
           <*> V.generateM 16 (f . N.X87StatusReg)
           <*> f N.X87TopReg
           <*> V.generateM 8 (f . N.X87TagReg)
           <*> V.generateM 8 (f . N.X87FPUReg)
           <*> V.generateM 16 (f . N.XMMReg)


mkX86State :: (forall cl. N.RegisterName cl -> f (N.RegisterType cl)) -> X86State f
mkX86State f = runIdentity (mkX86StateM (return . f))

------------------------------------------------------------------------
-- Combinators

foldX86StateValue :: Monoid a => (forall u. f u -> a) -> X86State f -> a
foldX86StateValue f s = f (s^.curIP)
                        `mappend` foldMap f (s^.reg64Regs)
                        `mappend` foldMap f (s^.flagRegs)
                        `mappend` foldMap f (s^.x87ControlWord)
                        `mappend` foldMap f (s^.x87StatusWord)
                        `mappend` foldMap f (s^.x87TagWords)
                        `mappend` foldMap f (s^.x87Regs)
                        `mappend` foldMap f (s^.xmmRegs)

zipWithX86State :: (forall u. f u -> g u -> h u)
                -> X86State f
                -> X86State g
                -> X86State h
zipWithX86State f x y = mkX86State (\r -> f (x ^. register r) (y ^. register r))

mapX86State :: (forall u. f u -> g u)
            -> X86State f
            -> X86State g
mapX86State f x = mkX86State (\r -> f (x ^. register r))

vectorCompare :: (a -> b -> Bool) -> V.Vector a -> V.Vector b -> Bool
vectorCompare f x y = V.and $ V.zipWith f x y

instance EqF f => Eq (X86State f) where
  s == s' = cmpX86State eqF s s'

cmpX86State :: (forall u. f u -> g u -> Bool)
            -> X86State f
            -> X86State g
            -> Bool
cmpX86State r s s' =
  (s^.curIP) `r` (s'^.curIP)
  && vectorCompare r (s^.reg64Regs)      (s'^.reg64Regs)
  && vectorCompare r (s^.flagRegs)       (s'^.flagRegs)
  && vectorCompare r (s^.x87ControlWord) (s'^.x87ControlWord)
  && vectorCompare r (s^.x87StatusWord)  (s'^.x87StatusWord)
  && vectorCompare r (s^.x87TagWords)    (s'^.x87TagWords)
  && vectorCompare r (s^.x87Regs)        (s'^.x87Regs)
  && vectorCompare r (s^.xmmRegs)        (s'^.xmmRegs)

------------------------------------------------------------------------
-- Pretty printing

class PrettyRegValue (f :: Type -> *) where
  ppValueEq :: N.RegisterName cl -> f (N.RegisterType cl) -> Maybe Doc

recv :: PrettyRegValue f
     => (Int -> N.RegisterName cl)
     -> V.Vector (f (N.RegisterType cl))
     -> [Maybe Doc]
recv mkR v = f <$> [0..V.length v - 1]
  where f i = ppValueEq (mkR i) (v V.! i)


instance PrettyRegValue f => Pretty (X86State f) where
  pretty s =
    bracketsep $ catMaybes ([ ppValueEq N.rip (s^.curIP)]
                            ++ recv N.GPReg (s^.reg64Regs)
                            ++ recv N.FlagReg (s^.flagRegs)
                            ++ recv N.X87ControlReg (s^.x87ControlWord)
                            ++ recv N.X87StatusReg (s^.x87StatusWord)
                            ++ recv N.X87TagReg (s^.x87TagWords)
                            ++ recv N.X87FPUReg (s^.x87Regs)
                            ++ recv N.XMMReg (s^.xmmRegs))
