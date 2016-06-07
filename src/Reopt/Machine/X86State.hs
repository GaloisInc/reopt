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
  , mapX86StateM
  , mapX86StateWithM
  , cmpX86State
    -- * Equality
  , EqF(..)
    -- * Pretty printing
  , PrettyRegValue(..)
  , X87StatusWord
    -- * Utilities
  , x86StateRegisters
  , x86CalleeSavedRegisters
  , x86SyscallArgumentRegisters
  , x86SyscallNoRegister
  , x86ArgumentRegisters
  , x86FloatArgumentRegisters
  , x86ResultRegisters
  , x86FloatResultRegisters
  ) where

import           Control.Lens
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Parameterized.Classes (EqF(..))
import           Data.Parameterized.Some

import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Utils.PrettyPrint

------------------------------------------------------------------------
-- X87StatusWord

type X87StatusWord f = V.Vector (f BoolType)


-- FIXME: should these be in StateNames?
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

-- | List of registers that a callee must save.
x86CalleeSavedRegisters :: Set (Some N.RegisterName)
x86CalleeSavedRegisters = Set.fromList $
  [ -- Some N.rsp sjw: rsp is special
  Some N.rbp
  , Some N.rbx
  , Some N.r12
  , Some N.r13
  , Some N.r14
  , Some N.r15
  ]

x86ArgumentRegisters :: [N.RegisterName 'N.GP]
x86ArgumentRegisters = [N.rdi, N.rsi, N.rdx, N.rcx, N.r8, N.r9]

-- The ABI defines these (http://www.x86-64.org/documentation/abi.pdf)
-- Syscalls clobber rcx and r11, but we don't really care about these
-- anyway.
x86SyscallArgumentRegisters :: [N.RegisterName 'N.GP]
x86SyscallArgumentRegisters = [N.rdi, N.rsi, N.rdx, N.r10, N.r8, N.r9]

x86SyscallNoRegister :: N.RegisterName 'N.GP
x86SyscallNoRegister = N.rax

x86FloatArgumentRegisters :: [N.RegisterName 'N.XMM]
x86FloatArgumentRegisters = map N.XMMReg [0..7]

x86ResultRegisters :: [N.RegisterName 'N.GP]
x86ResultRegisters = [ N.rax, N.rdx ]

x86FloatResultRegisters :: [N.RegisterName 'N.XMM]
x86FloatResultRegisters = [ (N.XMMReg 0) ]

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

mapX86StateM :: Monad m =>
                (forall u. f u -> m (g u))
             -> X86State f
             -> m (X86State g)
mapX86StateM f x = mkX86StateM (\r -> f (x ^. register r))


mapX86StateWithM :: Monad m =>
                   (forall cl. N.RegisterName cl -> f (N.RegisterType cl)
                    -> m (g (N.RegisterType cl)))
                 -> X86State f
                 -> m (X86State g)
mapX86StateWithM f x = mkX86StateM (\r -> f r (x ^. register r))

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

-- | This class provides a way of optionallly pretty printing the contents
-- of a register or omitting them.
class PrettyRegValue (f :: Type -> *) where
  -- | ppValueEq should return a doc if the contents of the given register
  -- should be printed, and Nothing if the contents should be ignored.
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

instance PrettyRegValue f => Show (X86State f) where
  show s = show (pretty s)
