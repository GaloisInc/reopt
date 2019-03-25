{-|
Defines the X86-specific definitions needed in the function representation.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Reopt.CFG.FnRep.X86
  ( -- * X86 specific
    X86FunctionType(..)
  , ftMaximumFunctionType
  , ftArgRegs
  , ftIntRetRegs
  , ftFloatRetRegs
  , X86FnStmt(..)
  ) where


import Data.Macaw.CFG (IsArchStmt(..))
import Data.Macaw.Types
import Data.Macaw.X86.ArchTypes (X86_64, X86Stmt)
import Data.Macaw.X86.X86Reg
  ( X86Reg
  , x86ArgumentRegs
  , x86ResultRegs
  , x86FloatArgumentRegs
  , x86FloatResultRegs
  )
import Data.Parameterized.Some
import Data.Parameterized.TraversableF
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Reopt.CFG.FnRep

commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')


------------------------------------------------------------------------
-- X86FunctionType

-- | This describes the expected arguments and return types of the function.
--
-- This is X86_64 specific.
data X86FunctionType =
  X86FunctionType { fnNIntArgs   :: !Int
                  , fnNFloatArgs :: !Int
                  , fnNIntRets   :: !Int
                  , fnNFloatRets :: !Int
                  }
  deriving (Ord, Eq, Show)

instance Pretty X86FunctionType where
  pretty f = parens (commaSepRegs (ftArgRegs f))
             <+> text "->"
             <+> parens (int (fnNIntRets f) <> comma <+> int (fnNFloatRets f))
   where commaSepRegs :: [Some X86Reg] -> Doc
         commaSepRegs [] = text ""
         commaSepRegs [Some r] = text (show r)
         commaSepRegs (Some r:l) = text (show r) <> comma <+> commaSepRegs l

-- Convenience functions
ftMaximumFunctionType :: X86FunctionType
ftMaximumFunctionType = X86FunctionType { fnNIntArgs = length x86ArgumentRegs
                                        , fnNFloatArgs = length x86FloatArgumentRegs
                                        , fnNIntRets = length x86ResultRegs
                                        , fnNFloatRets = length x86FloatResultRegs
                                        }


-- | Return the registers used to pass arguments.
ftArgRegs :: X86FunctionType -> [Some X86Reg]
ftArgRegs ft = (Some <$> take (fnNIntArgs ft) x86ArgumentRegs)
            ++ (Some <$> take (fnNFloatArgs ft) x86FloatArgumentRegs)

ftIntRetRegs :: X86FunctionType -> [X86Reg (BVType 64)]
ftIntRetRegs ft = take (fnNIntRets ft) x86ResultRegs

ftFloatRetRegs :: X86FunctionType -> [X86Reg (BVType 512)]
ftFloatRetRegs ft = take (fnNFloatRets ft) x86FloatResultRegs

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
