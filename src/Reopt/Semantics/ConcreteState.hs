{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module Reopt.Semantics.ConcreteState where
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Machine.X86State
import           Reopt.Semantics.BitVector
import           Text.PrettyPrint.ANSI.Leijen

data ConcreteValue (tp :: Type) where
  ConcreteValue :: BitVector n -> ConcreteValue (BVType n)

instance Eq (ConcreteValue tp) where
  ConcreteValue x == ConcreteValue y = x == y

instance Show (ConcreteValue tp) where
  show = show . pretty

instance Pretty (ConcreteValue tp) where
  pretty (ConcreteValue x) = text $ show x

instance PrettyRegValue ConcreteValue where
  ppValueEq (N.FlagReg n) _ | not (n `elem` [0,2,4,6,7,8,9,10,11]) = Nothing
  ppValueEq (N.X87ControlReg n) _ | not (n `elem` [0,1,2,3,4,5,12]) = Nothing
  ppValueEq r v = Just $ text (show r) <+> text "=" <+> pretty v

