{-|
Copyright        : (c) Galois, Inc 2015-2017
Maintainer       : Simon Winwood <sjw@galois.com>

This defines the core type to introduce function semantics.
-}
{-# LANGUAGE RankNTypes #-}
module Reopt.Semantics.InstructionDef
  ( InstructionDef
  , InstructionSemantics(..)
  , defVariadic
  ) where

import qualified Flexdis86 as F
import           Reopt.Semantics.Monad

-- This is a wrapper around the semantics of an instruction.
newtype InstructionSemantics
      = InstructionSemantics { _unInstructionSemantics
                               :: forall m. Semantics m
                               => F.InstructionInstance
                               -> m ()
                             }

-- | The information needed to define an instruction semantics.
type InstructionDef = (String, InstructionSemantics)

-- | Create a instruction that potentially takes any number of arguments.
defVariadic :: String
            -> (forall m. Semantics m => F.LockPrefix -> [F.Value] -> m ())
            -> InstructionDef
defVariadic mnemonic f =
  (mnemonic, InstructionSemantics (\ii -> f (F.iiLockPrefix ii) (fst <$> F.iiArgs ii)))
