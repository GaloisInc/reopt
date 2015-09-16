{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Reopt.CFG.FnRep
   ( FnAssignment(..)
   , FnAssignRhs(..)
   , FnValue(..)
   , Function(..)
   , FnBlock(..)
   , FnStmt(..)
   , FnTermStmt(..)
   ) where

import Data.Parameterized.Some
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))


import Reopt.CFG.Representation(App(..), AssignId, BlockLabel, CodeAddr
                               , ppApp, ppLit, ppAssignId, sexpr)
import Reopt.Machine.Types

commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')

data FnAssignment tp
   = FnAssignment { fnAssignId :: !AssignId
                  , fnAssignRhs :: !(FnAssignRhs tp)
                  }

instance Pretty (FnAssignment tp) where
  pretty (FnAssignment lhs rhs) = ppAssignId lhs <+> text ":=" <+> pretty rhs

-- | The right-hand side of a function assingment statement.
data FnAssignRhs (tp :: Type) where
  -- An expression with an undefined value.
  FnSetUndefined :: !(NatRepr n) -- Width of undefined value.
                 -> FnAssignRhs (BVType n)
  FnReadMem :: !(FnValue (BVType 64))
            -> !(TypeRepr tp)
            -> FnAssignRhs tp
  FnEvalApp :: !(App FnValue tp)
            -> FnAssignRhs tp
  FnAlloca :: !(FnValue (BVType 64))
           -> FnAssignRhs (BVType 64)

ppFnAssignRhs :: (forall u . FnValue u -> Doc)
                 -> FnAssignRhs tp
                 -> Doc
ppFnAssignRhs _  (FnSetUndefined w) = text "undef ::" <+> brackets (text (show w))
ppFnAssignRhs _  (FnReadMem loc _)  = text "*" <> pretty loc
ppFnAssignRhs pp (FnEvalApp a) = ppApp pp a
ppFnAssignRhs _pp (FnAlloca sz) = sexpr "alloca" [pretty sz]

instance Pretty (FnAssignRhs tp) where
  pretty = ppFnAssignRhs pretty

-- | A function value.
data FnValue (tp :: Type) where
  FnValueUnsupported :: FnValue tp
  FnConstantValue :: !(NatRepr n) -> !Integer -> FnValue (BVType n)
  -- Value from an assignment statement.
  FnAssignedValue :: !(FnAssignment tp) -> FnValue tp
  -- The entry pointer to a function.
  FnFunctionEntryValue :: !Word64 -> FnValue (BVType 64)
  -- A pointer to an internal block at the given address.
  FnBlockValue :: !Word64 -> FnValue (BVType 64)
  -- Value is an interget argument passed via a register.
  FnIntArg   :: !Int -> FnValue (BVType 64)
  -- Value is a function argument passed via a floating point XMM
  -- register.
  FnFloatArg :: !Int -> FnValue (BVType 128)
  -- A global address
  FnGlobalDataAddr :: !Word64 -> FnValue (BVType 64)

instance Pretty (FnValue tp) where
  pretty FnValueUnsupported       = text "unsupported"
  pretty (FnConstantValue sz n)   = ppLit sz n
  pretty (FnAssignedValue assign)   = ppAssignId (fnAssignId assign)
  pretty (FnFunctionEntryValue n) = text "FunctionEntry"
                                    <> parens (pretty $ showHex n "")
  pretty (FnBlockValue n)         = text "BlockValue"
                                    <> parens (pretty $ showHex n "")
  pretty (FnIntArg n)             = text "arg" <> int n
  pretty (FnFloatArg n)           = text "fparg" <> int n
  pretty (FnGlobalDataAddr addr)  = text "data@"
                                    <> parens (pretty $ showHex addr "")

------------------------------------------------------------------------
-- Function definitions

data Function = Function { fnAddr :: CodeAddr
                         , fnBlocks :: [FnBlock]
                         }

instance Pretty Function where
  pretty fn =
    text "function " <+> pretty (showHex (fnAddr fn) "")
    <$$>
    lbrace
    <$$>
    (nest 4 $ vcat (pretty <$> fnBlocks fn))
    <$$>
    rbrace


data FnBlock
   = FnBlock { fbLabel :: !BlockLabel
             , fbStmts :: ![FnStmt]
             , fbTerm :: !(FnTermStmt)
             }

instance Pretty FnBlock where
  pretty b =
    pretty (fbLabel b) <$$>
    indent 2 (vcat (pretty <$> fbStmts b) <$$> pretty (fbTerm b))

data FnStmt
  = forall tp . FnWriteMem !(FnValue (BVType 64)) !(FnValue tp)
    -- | A comment
  | FnComment !Text
    -- | An assignment statement
  | forall tp . FnAssignStmt !(FnAssignment tp)


instance Pretty FnStmt where
  pretty s =
    case s of
      FnWriteMem addr val -> text "*" <> parens (pretty addr) <+> text "=" <+> pretty val
      FnComment msg -> text "#" <+> text (Text.unpack msg)
      FnAssignStmt assign -> pretty assign

data FnTermStmt
   = FnJump !BlockLabel
   | FnRet      
   | FnBranch !(FnValue BoolType) !BlockLabel !BlockLabel
     -- ^ A branch to a block within the function
   | FnCall !(FnValue (BVType 64)) [Some FnValue] BlockLabel
     -- ^ A call statement to the given location with the arguments listed that
     -- returns to the label.
   | FnTermStmtUndefined

instance Pretty FnTermStmt where
  pretty s =
    case s of
      FnBranch c x y -> text "branch" <+> pretty c <+> pretty x <+> pretty y
      FnJump lbl -> text "jump" <+> pretty lbl
      FnRet -> text "return"
      FnCall f args lbl ->
        let arg_docs = viewSome pretty <$> args
         in text "call" <+> pretty f <> parens (commas arg_docs) <+> pretty lbl
      FnTermStmtUndefined -> text "undefined term"
