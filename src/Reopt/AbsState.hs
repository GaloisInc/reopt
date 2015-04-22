{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Reopt.AbsState
  ( AbsBlockState
  , absX86State
  , AbsValue
  , abstractSingleton
  , concretize
  , asConcreteSingleton
  , AbsDomain(..)
  , AbsRegs
  , initAbsRegs
  , finalAbsBlockState
  , addAssignment
  , transferValue
  , transferRHS
  ) where

import Control.Applicative ( (<$>) )
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe
import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.NatRepr
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Reopt.Semantics.Types
import Reopt.Semantics.Representation
import Reopt.X86State

------------------------------------------------------------------------
-- Abstract states


class Eq d => AbsDomain d where
  -- | The top element
  top :: d

  -- | A partial ordering over d.  forall x. x `leq` top
  leq :: d -> d -> Bool
  leq x y =
    case joinD y x of
      Nothing -> True
      Just _ -> False

  -- | Least upper bound (always defined, as we have top)
  lub :: d -> d -> d
  lub x y = case joinD x y of
              Nothing -> x
              Just r -> r

  -- | Join the old and new states and return the updated state iff
  -- the result is larger than the old state.
  joinD :: d -> d -> Maybe d
  joinD old new
    | new `leq` old = Nothing
    | otherwise     = Just $ lub old new

  {-# MINIMAL (top, ((leq,lub) | joinD)) #-}


type ValueSet = Set Integer

------------------------------------------------------------------------
-- AbsValue

data AbsValue (tp :: Type) where
  AbsValue :: !ValueSet -> AbsValue (BVType n)
  StackAddr :: !ValueSet -> AbsValue (BVType 64)
  TopV :: AbsValue tp

instance Eq (AbsValue tp) where
  AbsValue x == AbsValue y = x == y
  StackAddr x == StackAddr y = x == y
  TopV == TopV = True
  _ == _ = False

instance EqF AbsValue where
  eqF = (==)

-- | Returns a set of concrete integers that this value may be.
-- This function will neither return the complete set or an
-- known under-approximation.
concretize :: AbsValue tp -> Maybe (Set Integer)
concretize TopV         = Nothing
concretize (AbsValue s) = Just s

-- | Return single value is the abstract value can only take on one value.
asConcreteSingleton :: AbsValue tp -> Maybe Integer
asConcreteSingleton (AbsValue s) =
  case Set.toList s of
    [v] -> Just v
    _ -> Nothing
asConcreteSingleton TopV = Nothing

instance AbsDomain (AbsValue tp) where
  top = TopV

  leq _ TopV = True
  leq TopV _ = False
  leq (AbsValue v) (AbsValue v') = v `Set.isSubsetOf` v'

  lub _ TopV = TopV
  lub TopV _ = TopV
  lub (AbsValue v) (AbsValue v') = AbsValue $ v `Set.union` v'

-- | Print a list of Docs vertically separated.
instance PrettyRegValue AbsValue where
  ppValueEq r v = pp <$> concretize v
    where
      pp vs = text (show r) <+> text "="
        <+> encloseSep lbrace rbrace comma (map (\v' -> text (showHex v' "")) (Set.toList vs))

abstractSingleton :: Integer -> AbsValue (BVType n)
abstractSingleton = AbsValue . Set.singleton

------------------------------------------------------------------------
-- AbsBlockState

newtype AbsBlockState = AbsBlockState { _absX86State :: X86State AbsValue }
  deriving Eq

absX86State :: Simple Lens AbsBlockState (X86State AbsValue)
absX86State = lens _absX86State (\s v -> s { _absX86State = v })

instance AbsDomain AbsBlockState where
  top = AbsBlockState $ mkX86State (\_ -> top)
  leq (AbsBlockState x) (AbsBlockState y)
    = cmpX86State leq x y
  lub (AbsBlockState x) (AbsBlockState y)
    = AbsBlockState $ zipWithX86State lub x y

instance Pretty AbsBlockState where
  pretty (AbsBlockState s) = pretty s

------------------------------------------------------------------------
-- AbsRegs

-- | This is used to cache all changes to a state within a block.
data AbsRegs = AbsRegs { absInitial :: !AbsBlockState
                         , _absAssignments :: !(MapF Assignment AbsValue)
                         }

initAbsRegs :: AbsBlockState -> AbsRegs
initAbsRegs s = AbsRegs s MapF.empty

absAssignments :: Simple Lens AbsRegs (MapF Assignment AbsValue)
absAssignments = lens _absAssignments (\s v -> s { _absAssignments = v })

addAssignment :: Assignment tp -> AbsRegs -> AbsRegs
addAssignment a c = c & absAssignments %~ MapF.insert a (transferRHS c (assignRhs a))

finalAbsBlockState :: AbsRegs -> X86State Value -> AbsBlockState
finalAbsBlockState c s = AbsBlockState $ mkX86State $ \r ->
                                 transferValue c (s ^. register r)

------------------------------------------------------------------------
-- Transfer functions

transferValue :: AbsRegs
              -> Value tp
              -> AbsValue tp
transferValue c v =
  case v of
   BVValue _ i -> abstractSingleton i
   -- Invariant: v is in m
   AssignedValue a ->
     fromMaybe (error $ "Missing assignment for " ++ show (assignId a))
               (MapF.lookup a (c^.absAssignments))
   Initial r -> absInitial c ^. (absX86State . register r)

transferApp :: AbsRegs
            -> App Value tp
            -> AbsValue tp
transferApp _ _ = top

type_width' :: TypeRepr tp -> Int
type_width' (BVTypeRepr n) = widthVal n

transferRHS :: forall tp
            .  AbsRegs
            -> AssignRhs tp
            -> AbsValue tp
transferRHS m rhs =
  case rhs of
    EvalApp app    -> transferApp m app
    SetUndefined _ -> top
    Read _         -> top