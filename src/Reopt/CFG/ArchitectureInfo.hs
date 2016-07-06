{-|
Module     : Reopt.CFG.ArchitectureInfo
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

This defines the architecture-specific information needed for code discovery.
-}
{-# LANGUAGE RankNTypes #-}
module Reopt.CFG.ArchitectureInfo
  ( ArchitectureInfo(..)
  , ReadAddrFn
  , DisassembleFn
  ) where

import Data.Macaw.CFG
import Reopt.Object.Memory
import Data.Parameterized.Nonce

------------------------------------------------------------------------
-- ArchitectureInfo

-- | A function for reading an address from memory
type ReadAddrFn w = Memory w -> ElfSegmentFlags -> w -> Either (MemoryError w) w

-- | Function for disassembling a block
type DisassembleFn arch
   = forall st_s ids.
     Memory (ArchAddr arch)
   -> (ArchAddr arch -> Bool)
   -- ^ Predicate that tells when to continue.
   -> ArchCFLocation arch -- ^ Location to explore from.
   -> NonceST st_s ids (Either String ([Block arch ids], ArchAddr arch))

-- | This records architecture specific functions for analysis.
data ArchitectureInfo arch
   = ArchitectureInfo
     { stackDelta :: !Integer
       -- ^ Identifies how the stack pointer changes when a call is made.
       --
       -- e.g., On X86_64, a call instruction decrements the stack
       -- pointer by 8 after pushing the return address, so this
       -- is -8 on X86_64.
     , jumpTableEntrySize :: !(ArchAddr arch)
       -- ^ The size of each entry in a jump table.
     , readAddrInMemory :: !(ReadAddrFn (ArchAddr arch))
      -- ^ Return an address at given address.
     , disassembleFn :: !(DisassembleFn arch)
       -- ^ Function for disasembling a block.
     }
