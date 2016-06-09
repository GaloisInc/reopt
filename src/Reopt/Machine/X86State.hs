{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Machine.X86State
  ( X86State
  , mkX86State
  , mkX86StateM
  , curIP
  , x87TopReg
    -- * Combinators
  , foldX86StateValue
  , zipWithX86State
  , mapX86State
  , mapX86StateM
  , mapX86StateWithM
  , cmpX86State
    -- * X86Reg
  , X86Reg(..)
  , x86Reg
  , ip_reg
  , sp_reg
  , rax_reg
  , rbx_reg
  , rcx_reg
  , rdx_reg
  , r11_reg
  , cf_reg
  , df_reg
  , boundValue
    -- * Equality
  , EqF(..)
    -- * Pretty printing
  , PrettyRegValue(..)
    -- * Architecture
  , Reopt.CFG.Representation.X86_64
    -- * Utilities
  , gpRegList
  , x87FPURegList

  , x86StateRegs
  , x86CalleeSavedRegs
  , x86ArgumentRegs
  , x86FloatArgumentRegs
  , x86ResultRegs
  , x86FloatResultRegs
  , x86SyscallArgumentRegs
  ) where

import           Control.Lens
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Set (Set)
import qualified Data.Set as Set

import           Reopt.CFG.Representation
import           Reopt.Machine.Types

------------------------------------------------------------------------
-- Combinators

foldX86StateValue :: Monoid a => (forall u. f u -> a) -> X86State f -> a
foldX86StateValue f (RegState m) = foldMapF f m

zipWithX86State :: (forall u. f u -> g u -> h u)
                -> X86State f
                -> X86State g
                -> X86State h
zipWithX86State f x y = mkX86State (\r -> f (x ^. boundValue r) (y ^. boundValue r))

mapX86State :: (forall u. f u -> g u)
            -> X86State f
            -> X86State g
mapX86State f (RegState x) = RegState (fmapF f x)

mapX86StateM :: Monad m
             => (forall u. f u -> m (g u))
             -> X86State f
             -> m (X86State g)
mapX86StateM f (RegState x) = RegState <$> traverseF f x

mapX86StateWithM :: Monad m
                 => (forall tp. X86Reg tp -> f tp -> m (g tp))
                 -> X86State f
                 -> m (X86State g)
mapX86StateWithM f (RegState m) = RegState <$> MapF.traverseWithKey f m

------------------------------------------------------------------------
-- Register names

-- | List of registers that a callee must save.
x86CalleeSavedRegs :: Set (Some X86Reg)
x86CalleeSavedRegs = Set.fromList $
  [ -- Some N.rsp sjw: rsp is special
    Some rbp_reg
  , Some rbx_reg
  , Some r12_reg
  , Some r13_reg
  , Some r14_reg
  , Some r15_reg
  ]

x86ArgumentRegs :: [X86Reg (BVType 64)]
x86ArgumentRegs = [rdi_reg, rsi_reg, rdx_reg, rcx_reg, r8_reg, r9_reg]

x86FloatArgumentRegs :: [X86Reg (BVType 128)]
x86FloatArgumentRegs =  X86_XMMReg <$> [0..7]

x86ResultRegs :: [X86Reg (BVType 64)]
x86ResultRegs = [ rax_reg, rdx_reg ]

x86FloatResultRegs :: [X86Reg (BVType 128)]
x86FloatResultRegs = [ X86_XMMReg 0 ]

-- The ABI defines these (http://www.x86-64.org/documentation/abi.pdf)
-- Syscalls clobber rcx and r11, but we don't really care about these
-- anyway.
x86SyscallArgumentRegs :: [X86Reg (BVType 64)]
x86SyscallArgumentRegs = [rdi_reg, rsi_reg, rdx_reg, r10_reg, r8_reg, r9_reg ]
