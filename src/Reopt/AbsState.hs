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
  , AbsDomain(..)
  , AbsCache
  , initAbsCache
  , finalAbsBlockState
  , addAssignment
  , transferValue
  , transferApp
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

------------------------------------------------------------------------
-- AbsValue

data AbsValue (tp :: Type)
   = AbsValue !(Set Integer)
   | TopV

instance Eq (AbsValue tp) where
  AbsValue x == AbsValue y = x == y
  TopV == TopV = True
  _ == _ = False

instance EqF AbsValue where
  eqF = (==)

-- | Returns values that a abstract domain can be.
concretize :: AbsValue tp -> Maybe (Set Integer)
concretize TopV         = Nothing
concretize (AbsValue s) = Just s

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

abstractSingleton :: Integer -> AbsValue tp
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
-- AbsCache

-- | This is used to cache all changes to a state within a block.
data AbsCache = AbsCache { absInitial :: !AbsBlockState
                         , _absAssignments :: !(MapF Assignment AbsValue) }

initAbsCache :: AbsBlockState -> AbsCache
initAbsCache s = AbsCache s MapF.empty

absAssignments :: Simple Lens AbsCache (MapF Assignment AbsValue)
absAssignments = lens _absAssignments (\s v -> s { _absAssignments = v })

addAssignment :: Assignment tp -> AbsCache -> AbsCache
addAssignment a c = c & absAssignments %~ MapF.insert a (transferRHS c (assignRhs a))

finalAbsBlockState :: AbsCache -> X86State Value -> AbsBlockState
finalAbsBlockState c s = AbsBlockState $ mkX86State $ \r ->
                                 transferValue c (s ^. register r)

------------------------------------------------------------------------
-- Transfer functions

transferValue :: AbsCache
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

transferApp :: AbsCache
            -> App Value tp
            -> AbsValue tp
transferApp _ _ = top

type_width' :: TypeRepr tp -> Int
type_width' (BVTypeRepr n) = widthVal n

transferRHS :: forall tp
            .  AbsCache
            -> AssignRhs tp
            -> AbsValue tp
transferRHS m rhs =
  case rhs of
    EvalApp app    -> transferApp m app
    SetUndefined _ -> top
    Read _         -> top
