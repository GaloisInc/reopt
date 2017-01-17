module Data.Macaw.FnExtraction
  ( FunCFG(..)
  , FunBlock(..)
  , FunTerm(..)
  ) where

type RegMap arch ids = RegState (ArchReg arch) (Value arch ids)

data FunTermStmt arch ids
  = FunJump !(RegState (ArchReg arch) (Value arch ids)) !(ArchSegmentedAddr arch)
    -- ^ A jump within a block
  | FunBranch   !(Value arch ids BoolType) !(ArchLabel arch) !(ArchLabel arch)
    -- ^ A Boolean condition that branches to one label or the other.
  | FunSyscall  !(RegMap arch ids) !(ArchSegmentedAddr arch)
    -- ^ A system call that invokes the given arguments, and returns to the given address.
  | FunCall     !(RegMap arch ids) !(ArchSegentedAddr arch ids)
    -- ^ A function call that jumps to the address given, and returns to the given address.
  | FunTailCall !(RegMap arch ids) !(ArchSegentedAddr arch ids)
    -- ^ A function call that jumps to the address given, and doesn't return
     -- | A lookup table that branches to the given locations.
   | ParsedLookupTable !(RegState (ArchReg arch) (Value arch ids))
                       !(BVValue arch ids (ArchAddrWidth arch))
                       !(V.Vector (ArchSegmentedAddr arch))
     -- | A tail cthat branches to the given locations.
   | ParsedReturn !(RegState (ArchReg arch) (Value arch ids)) !(Seq (Stmt arch ids))
   | ParsedSyscall !(RegState (ArchReg arch) (Value arch ids))
                   !(ArchSegmentedAddr arch)
   | ParsedTranslateError !Text
     -- ^ An error occured in translating the block

data FunBlock arch ids =
  FunBlock { funblockLabel :: !(ArchLabel arch)
           , funblockStmts :: !([Stmt arch ids])
           , funblockTerm  :: !(FunTermStmt arch ids)
           }

data FunCFG arch ids
   = FunCFG { funcfgBlocks :: !(Map (ArchLabel arch) (FunBlock arch ids))
            }
