{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.AbsState
  ( AbsBlockState
  , absX86State
  , absBlockDiff
  , AbsValue(..)
  , ppAbsValue
  , abstractSingleton
  , concretize
  , asConcreteSingleton
  , AbsDomain(..)
  , AbsRegs
  , absInitial
  , initAbsRegs
  , absAssignments
  , finalAbsBlockState
  , addAssignment
  , transferValue
  , transferRHS
  ) where

import Control.Applicative ( (<$>) )
import Control.Exception (assert)
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe
import Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Reopt.Semantics.StateNames as N
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
  StackOffset :: !ValueSet -> AbsValue (BVType 64)
  TopV :: AbsValue tp

instance Eq (AbsValue tp) where
  AbsValue x    == AbsValue y    = x == y
  StackOffset s == StackOffset t = s == t
  TopV          == TopV          = True
  _             ==               _ = False

instance EqF AbsValue where
  eqF = (==)

-- | Returns a set of concrete integers that this value may be.
-- This function will neither return the complete set or an
-- known under-approximation.
concretize :: AbsValue tp -> Maybe (Set Integer)
concretize TopV         = Nothing
concretize (StackOffset s) = Nothing
concretize (AbsValue s) = Just s

-- | Return single value is the abstract value can only take on one value.
asConcreteSingleton :: AbsValue tp -> Maybe Integer
asConcreteSingleton v =
  case Set.toList <$> concretize v of
    Just [e] -> Just e
    _ -> Nothing

instance AbsDomain (AbsValue tp) where
  top = TopV

{-
  leq _ TopV = True
  leq TopV _ = False
  leq (StackOffset s) (StackOffset t) = s `Set.isSubsetOf` t
  leq (AbsValue v) (AbsValue v') = v `Set.isSubsetOf` v'
  leq _ _ = False

  lub (StackOffset s) (StackOffset t) = StackOffset $ s `Set.union` t
  lub (AbsValue v) (AbsValue v') = AbsValue $ v `Set.union` v'
  lub _ _ = TopV
-}

  -- | Join the old and new states and return the updated state iff
  -- the result is larger than the old state.
  joinD TopV _ = Nothing
  joinD (StackOffset old) (StackOffset new)
      | new `Set.isSubsetOf` old = Nothing
      | Set.size r > 5 = Just TopV
      | otherwise = Just (StackOffset r)
    where r = Set.union old new
  joinD (AbsValue old) (AbsValue new)
      | new `Set.isSubsetOf` old = Nothing
      | Set.size r > 5 = Just TopV
      | otherwise = Just (AbsValue r)
    where r = Set.union old new
  joinD _ _ = Just TopV


trunc :: (v+1 <= u) => AbsValue (BVType u) -> NatRepr v -> AbsValue (BVType v)
trunc (AbsValue s) w = AbsValue (Set.map (toUnsigned w) s)
trunc (StackOffset _) _ = TopV
trunc TopV _ = TopV

uext :: (u+1 <= v) => AbsValue (BVType u) -> NatRepr v -> AbsValue (BVType v)
uext (AbsValue s) w = AbsValue s
uext (StackOffset _) _ = TopV
uext TopV _ = TopV

bvadd :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
{-
bvadd w (AbsValue t) (StackOffset s) | [o] <- Set.toList t = do
  StackOffset $ Set.map (addOff w o) s
-}
bvadd w (StackOffset s) (AbsValue t) | [o] <- Set.toList t = do
  StackOffset $ Set.map (addOff w o) s

{-
bvadd w (StackOffset s) (AbsValue t) = StackOffset $ Set.fromList $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x + y))
-}
bvadd _ _ _ = TopV

setL :: (Set Integer -> AbsValue (BVType n)) -> [Integer] -> AbsValue (BVType n)
setL c l | length l > 5 = TopV
         | otherwise = c (Set.fromList l)

bvsub :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
bvsub w (AbsValue s) (AbsValue t) = setL AbsValue $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x - y))
bvsub w (StackOffset s) (AbsValue t) = setL StackOffset $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x - y))
bvsub w _ StackOffset{} = TopV
bvsub _ TopV _ = TopV
bvsub _ _ TopV = TopV

bvmul :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
bvmul w (AbsValue s) (AbsValue t) = setL AbsValue $ do
  x <- Set.toList s
  y <- Set.toList t
  return (x * y)
bvmul _ _ _ = TopV

instance Pretty (AbsValue tp) where
  pretty TopV = text "top"
  pretty (AbsValue s) = ppIntegerSet s
  pretty (StackOffset s) = text "rsp_0 +" <+> ppIntegerSet s

ppIntegerSet ::  Set Integer -> Doc
ppIntegerSet vs = encloseSep lbrace rbrace comma (map ppv (Set.toList vs))
  where ppv v' = assert (v' >= 0) $ text (showHex v' "")


ppAbsValue :: AbsValue tp -> Maybe Doc
ppAbsValue TopV = Nothing
ppAbsValue v = Just (pretty v)

-- | Print a list of Docs vertically separated.
instance PrettyRegValue AbsValue where
  ppValueEq r TopV = Nothing
  ppValueEq r v = Just (text (show r) <+> text "=" <+> pretty v)

abstractSingleton :: NatRepr n -> Integer -> AbsValue (BVType n)
abstractSingleton n i
  | 0 <= i && i <= maxUnsigned n = AbsValue (Set.singleton i)
  | otherwise = error $ "abstractSingleton given bad value: " ++ show i ++ " " ++ show n

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

instance Show AbsBlockState where
  show s = show (pretty s)


absBlockDiff :: AbsBlockState -> AbsBlockState -> [Some N.RegisterName]
absBlockDiff (AbsBlockState x) (AbsBlockState y) = filter isDifferent x86StateRegisters
  where isDifferent (Some n) = x^.register n /= y^.register n

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

addOff :: NatRepr w -> Integer -> Integer -> Integer
addOff w o v = toUnsigned w (o + v)

subOff :: NatRepr w -> Integer -> Integer -> Integer
subOff w o v = toUnsigned w (o - v)

-- | Return state for after vlaue has run.
finalAbsBlockState :: AbsRegs -> X86State Value -> AbsBlockState
finalAbsBlockState c s = AbsBlockState $ mkX86State go
  where go :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
        go =
          case transferValue c (s^.register N.rsp) of
            StackOffset offsets | [o] <- Set.toList offsets ->
              if o == 0 then
                \r -> transferValue c (s ^. register r)
              else
                \r ->
                  case transferValue c (s^.register r) of
                    StackOffset t -> StackOffset (Set.map (\v -> subOff n64 v o) t)
                    v -> v
            _ -> \r ->
                  case transferValue c (s^.register r) of
                    StackOffset t -> TopV
                    v -> v

------------------------------------------------------------------------
-- Transfer functions

transferValue :: AbsRegs
              -> Value tp
              -> AbsValue tp
transferValue c v =
  case v of
   BVValue w i
     | 0 <= i && i <= maxUnsigned w -> abstractSingleton w i
     | otherwise -> error $ "transferValue given illegal value " ++ show (pretty v)
   -- Invariant: v is in m
   AssignedValue a ->
     fromMaybe (error $ "Missing assignment for " ++ show (assignId a))
               (MapF.lookup a (c^.absAssignments))
   Initial r
     | Just Refl <- testEquality r N.rsp -> do
       StackOffset (Set.singleton 0)
     | otherwise -> absInitial c ^. (absX86State . register r)

transferApp :: AbsRegs
            -> App Value tp
            -> AbsValue tp
transferApp r a =
  case a of
    Trunc v w -> trunc (transferValue r v) w
    UExt  v w -> uext  (transferValue r v) w
    BVAdd w x y -> bvadd w (transferValue r x) (transferValue r y)
    BVSub w x y -> bvsub w (transferValue r x) (transferValue r y)
    BVMul w x y -> bvmul w (transferValue r x) (transferValue r y)
    _ -> top

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