{-|
Module     : Reopt.CFG.ArchitectureInfo
Copyright  : (c) Galois, Inc 2016
Maintainer : jhendrix@galois.com

This defines the architecture-specific information needed for code discovery.
-}
module Reopt.CFG.ArchitectureInfo
  ( ArchitectureInfo(..)
  , ReadAddrFn
  , DisassembleFn
  ) where

import Data.Macaw.CFG
import Reopt.Object.Memory

------------------------------------------------------------------------
-- ArchitectureInfo

-- | A function for reading an address from memory
type ReadAddrFn w = Memory w -> ElfSegmentFlags -> w -> Either (MemoryError w) w

-- | Function for disassembling a block
type DisassembleFn a
   = AssignId
     -- ^ Index to use for next assignment
   -> Memory (ArchAddr a)
   -> (ArchAddr a -> Bool)
   -- ^ Predicate that tells when to continue.
   -> ArchCFLocation a -- ^ Location to explore from.
   -> Either String ([Block a], ArchAddr a, AssignId)

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
