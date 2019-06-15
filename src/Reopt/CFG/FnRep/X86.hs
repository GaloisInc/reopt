{-|
Defines the X86-specific definitions needed in the function representation.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Reopt.CFG.FnRep.X86
  ( -- * X86 specific
   X86FnStmt(..)
  ) where


import Data.Macaw.CFG (IsArchStmt(..))
import Data.Macaw.Types
import Data.Macaw.X86.ArchTypes (X86_64, X86Stmt)
import Data.Parameterized.Some
import Data.Parameterized.TraversableF
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Reopt.CFG.FnRep

commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')

------------------------------------------------------------------------
-- X86ArchStmt

data X86FnStmt f
   = X86FnStmt (X86Stmt f)
   | X86FnSystemCall !(f (BVType 64)) ![f (BVType 64)] ![Some FnReturnVar]
     -- ^ A system call with the call number, arguments, return
     -- variable, and block to jump to when it terminates.
     --
     -- TODO: See if we can migrate this to @FnStmt@.

type instance FnArchStmt X86_64 = X86FnStmt

instance IsArchStmt X86FnStmt where
  ppArchStmt pp (X86FnStmt s) = ppArchStmt pp s
  ppArchStmt pp (X86FnSystemCall call_no args rets) =
    let ret_docs = viewSome pretty <$> rets
     in parens (commas ret_docs)
            <+> text ":=" <+> text "syscall"
            <+> pp call_no <> parens (commas (pp <$> args))

instance FoldableF X86FnStmt where
  foldrF f s (X86FnStmt t) = foldrF f s t
  foldrF f s (X86FnSystemCall call_no args _rets) =
    f call_no (foldr f s args)
