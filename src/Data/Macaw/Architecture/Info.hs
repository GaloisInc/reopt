{-|
Module     : Data.Macaw.Architecture.Info
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

This defines the architecture-specific information needed for code discovery.
-}
{-# LANGUAGE RankNTypes #-}
module Data.Macaw.Architecture.Info
  ( ArchitectureInfo(..)
  , ReadAddrFn
  , DisassembleFn
  ) where

import Control.Monad.ST
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce

import Data.Macaw.CFG
import Data.Macaw.AbsDomain.AbsState
import Data.Macaw.Memory


------------------------------------------------------------------------
-- ArchitectureInfo

-- | A function for reading an address from memory
type ReadAddrFn w = Memory w -> ElfSegmentFlags -> w -> Either (MemoryError w) w

-- | Function for disassembling a block
type DisassembleFn arch
   = forall ids
   .  NonceGenerator (ST ids) ids
   -> Memory (ArchAddr arch)
   -> (ArchAddr arch -> Bool)
   -- ^ Predicate that tells when to continue.
   -> ArchAddr arch -- ^ Address that we are disassembling from
   -> AbsBlockState (ArchReg arch)
      -- ^ Abstract state associated with address.
      --
      -- This is used for things like the height of the x87 stack.
   -> ST ids (Either String ([Block arch ids], ArchAddr arch))

-- | This records architecture specific functions for analysis.
data ArchitectureInfo arch
   = ArchitectureInfo
     { archAddrWidth :: !(NatRepr (RegAddrWidth (ArchReg arch)))
       -- ^ Architecture address width.
     , jumpTableEntrySize :: !(ArchAddr arch)
       -- ^ The size of each entry in a jump table.
     , readAddrInMemory :: !(ReadAddrFn (ArchAddr arch))
      -- ^ Return an address at given address.
     , memoryAlignedWords :: !(Memory (ArchAddr arch) -> [(ArchAddr arch, ArchAddr arch)])
       -- ^ Return list of aligned words in memory.
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
       -- ^ Abstract state after a system call
     , postCallAbsStateFn :: !(AbsBlockState (ArchReg arch)
                               -> ArchAddr arch
                               -> AbsBlockState (ArchReg arch))
       -- ^ Abstract state after a function call.
     , callStackDelta :: !Integer
       -- ^ The direction that the stack moves with a call.
     , absEvalArchFn :: !(forall ids tp
                          .  AbsProcessorState (ArchReg arch) ids
                          -> ArchFn arch ids tp
                          -> AbsValue (RegAddrWidth (ArchReg arch)) tp)
       -- ^ Evaluates an architecture function

     }
