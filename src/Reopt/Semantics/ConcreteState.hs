{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}


module Reopt.Semantics.ConcreteState
    ( module Reopt.Semantics.ConcreteState
    , module Reopt.Semantics.BitVector
    ) where

import           Data.BitVector (BV)
import           Text.PrettyPrint.ANSI.Leijen ((<+>), Pretty(..), text)

import           Data.Parameterized.NatRepr
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Machine.X86State
import           Reopt.Semantics.BitVector

------------------------------------------------------------------------
-- Concrete values

data Value (tp :: Type) where
  Literal   :: BitVector n -> Value (BVType n)
  Undefined :: TypeRepr tp -> Value tp

instance Eq (Value tp) where
  Literal x == Literal y = x == y
  -- XXX Should undefined values be equal to everything?
  _ == _                             = True

instance Show (Value tp) where
  show = show . pretty

instance Pretty (Value tp) where
  pretty (Literal x)    = text $ show x
  pretty (Undefined _) = text $ "Undefined"

instance PrettyRegValue Value where
  ppValueEq (N.FlagReg n) _ | not (n `elem` [0,2,4,6,7,8,9,10,11]) = Nothing
  ppValueEq (N.X87ControlReg n) _ | not (n `elem` [0,1,2,3,4,5,12]) = Nothing
  ppValueEq r v = Just $ text (show r) <+> text "=" <+> pretty v

------------------------------------------------------------------------
-- 'Value' combinators

-- | Lift a computation on 'BV's to a computation on 'Value's.
--
-- The result-type 'NatRepr' is passed separately and used to
-- construct the result 'Value'.
liftValue :: (BV -> BV)
           -> NatRepr n2
           -> Value (BVType n1)
           -> Value (BVType n2)
liftValue f nr (asBV -> Just v) =
  Literal $ bitVector nr (f v)
liftValue _ nr _ = Undefined (BVTypeRepr nr)

liftValue2 :: (BV -> BV -> BV)
           -> NatRepr n3
           -> Value (BVType n1)
           -> Value (BVType n2)
           -> Value (BVType n3)
liftValue2 f nr (asBV -> Just bv1) (asBV -> Just bv2) =
  Literal $ bitVector nr (f bv1 bv2)
liftValue2 _ nr _ _ = Undefined (BVTypeRepr nr)

asBV :: Value tp -> Maybe BV
asBV (Literal (unBitVector -> (_, bv))) = Just bv
asBV _ = Nothing

