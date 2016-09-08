{-|
Module     : Data.Macaw.Architecture.Info
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

This defines the architecture-specific information needed for code discovery.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Data.Macaw.Architecture.Info
  ( AddrWidthRepr(..)
  , addrWidthNatRepr
  , ArchitectureInfo(..)
  , ReadAddrFn
  , DisassembleFn
  ) where

import Control.Monad.ST
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce

import Data.Macaw.AbsDomain.AbsState
import Data.Macaw.CFG
import Data.Macaw.Memory

------------------------------------------------------------------------
-- ArchitectureInfo

-- | A function for reading an address from memory
type ReadAddrFn w
   = Memory w
   -> w
   -> Either (MemoryError w) w

-- | Function for disassembling a block.
--
-- A block is defined as a contiguous region of code with a single known
-- entrance and potentially multiple exits.
--
-- This returns either an error or the disassembled blocks and the address
-- one past the end of the block.
type DisassembleFn arch
   = forall ids
   .  NonceGenerator (ST ids) ids
   -> Memory (ArchAddr arch)
   -> (ArchAddr arch -> Bool)
   -- ^ Predicate that tells when to continue.
   -> ArchAddr arch -- ^ Address that we are disassembling from
   -> AbsBlockState (ArchReg arch)
      -- ^ Abstract state associated with address that we are disassembling
      -- from.
      --
      -- This is used for things like the height of the x87 stack.
   -> ST ids (Either String ([Block arch ids], ArchAddr arch))

data AddrWidthRepr w
   = (w ~ 32) => Addr32
   | (w ~ 64) => Addr64

addrWidthNatRepr :: AddrWidthRepr w -> NatRepr w
addrWidthNatRepr Addr32 = knownNat
addrWidthNatRepr Addr64 = knownNat

-- | This records architecture specific functions for analysis.
data ArchitectureInfo arch
   = ArchitectureInfo
     { archAddrWidth :: !(AddrWidthRepr (RegAddrWidth (ArchReg arch)))
       -- ^ Architecture address width.
     , jumpTableEntrySize :: !(ArchAddr arch)
       -- ^ The size of each entry in a jump table.
     , callStackDelta :: !Integer
       -- ^ The shift that the stack moves with a call.
     , readJumpTableEntry :: !(ReadAddrFn (ArchAddr arch))
       -- ^ This reads a value in memory that appears to be a jump table entry.
       --
       -- This function does not allow the endianess to be changed dynamically as ARM allows.
     , disassembleFn :: !(DisassembleFn arch)
       -- ^ Function for disasembling a block.
     , fnBlockStateFn :: !(Memory (ArchAddr arch)
                           -> ArchAddr arch
                           -> AbsBlockState (ArchReg arch))
       -- ^ Creates an abstract block state for representing the beginning of a
       -- function.
     , postSyscallFn :: !(AbsBlockState (ArchReg arch)
                          -> ArchAddr arch
                          -> AbsBlockState (ArchReg arch))
       -- ^ Transfer function that maps abstract state before system call to
       -- abstract state after system call.
       --
       -- The first argument contains the first abstract state, and the
       -- second contains the address that we are jumping to.
     , postCallAbsStateFn :: !(AbsBlockState (ArchReg arch)
                               -> ArchAddr arch
                               -> AbsBlockState (ArchReg arch))
       -- ^ Abstract state after a function call.
     , absEvalArchFn :: !(forall ids tp
                          .  AbsProcessorState (ArchReg arch) ids
                          -> ArchFn arch ids tp
                          -> AbsValue (RegAddrWidth (ArchReg arch)) tp)
       -- ^ Evaluates an architecture-specific function
     }
