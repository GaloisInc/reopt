-- | Defines the X86-specific definitions needed in the function representation.
module Reopt.CFG.FnRep.X86 (
  X86FnStmt (..),
) where

import Data.Macaw.CFG (IsArchStmt (..))
import Data.Macaw.X86.ArchTypes (X86Stmt, X86_64)
import Data.Parameterized.TraversableF (FoldableF (foldrF))

import Reopt.CFG.FnRep (FnArchStmt)

newtype X86FnStmt f = X86FnStmt (X86Stmt f)

type instance FnArchStmt X86_64 = X86FnStmt

instance IsArchStmt X86FnStmt where
  ppArchStmt pp (X86FnStmt s) = ppArchStmt pp s

instance FoldableF X86FnStmt where
  foldrF f s (X86FnStmt t) = foldrF f s t
