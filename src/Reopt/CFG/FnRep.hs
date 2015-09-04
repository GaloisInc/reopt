{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Reopt.CFG.FnRep
   ( FnAssignment(..)
   , FnAssignRhs(..)
   , FnValue(..)
   ) where

import Data.Word
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))


import Reopt.CFG.Representation(App(..), AssignId)
import Reopt.Machine.Types

data FnAssignment tp
   = FnAssignment { fnAssignId :: !AssignId
                  , fnAssignRhs :: !(FnAssignRhs tp)
                  }

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

-- | A function value.
data FnValue (tp :: Type) where
  FnValueUnsupported :: FnValue tp
  FnConstantValue :: NatRepr n -> Integer -> FnValue (BVType n)
  -- Value from an assignment statement.
  FnAssignedValue :: !(FnAssignment tp) -> FnValue tp
  -- The entry pointer to a function.
  FnFunctionEntryValue :: Word64 -> FnValue (BVType 64)
  -- A pointer to an internal block.
  FnBlockValue :: Word64 -> FnValue (BVType 64)
  -- This register comes from an integer argument.
  FnIntArg :: Int -> FnValue (BVType 64)
  -- This register comes from a floating point XMM argument.
  FnFloatArg :: Int -> FnValue (BVType 128)

instance Pretty (FnValue tp) where
  pretty FnValueUnsupported = text "unsupported"
