{-|
Defines the X86-specific definitions needed in the function representation.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Reopt.CFG.FnRep.X86
  ( -- * X86 specific
   X86FnStmt(..)
  ) where


import Data.Macaw.CFG (IsArchStmt(..))
import Data.Macaw.X86.ArchTypes (X86_64, X86Stmt)
import Data.Parameterized.TraversableF

import Reopt.CFG.FnRep

------------------------------------------------------------------------
-- X86ArchStmt

data X86FnStmt f = X86FnStmt (X86Stmt f)

type instance FnArchStmt X86_64 = X86FnStmt

instance IsArchStmt X86FnStmt where
  ppArchStmt pp (X86FnStmt s) = ppArchStmt pp s

instance FoldableF X86FnStmt where
  foldrF f s (X86FnStmt t) = foldrF f s t
