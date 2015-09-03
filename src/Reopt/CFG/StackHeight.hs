{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.StackHeight
  ( StackDelta
  , initStackDelta
  , valueAsStackDelta
  , mergeStackDelta
  , stackDeltaAllocSize
  , FnStack
  , initFnStack
  , stackOffsetAddr
  , recordCalleeSavedWrite
  , recordStackAlloca
  ) where

import Data.Int
import qualified Data.Map.Strict as Map
import Data.Parameterized.NatRepr
import Data.Set (Set)
import qualified Data.Set as Set
--import Data.Type.Equality

import Reopt.CFG.FnRep
import Reopt.CFG.Representation
import Reopt.Machine.Types
import Reopt.Machine.StateNames (RegisterName, RegisterClass(..), rsp)
import Debug.Trace

------------------------------------------------------------------------
-- WeightedValueMap

type WeightedValueMap = (Map.Map (Value (BVType 64)) Integer, Integer)

concreteMap :: Integer -> WeightedValueMap
concreteMap v = (Map.empty, v)

singleMap :: Value (BVType 64) -> WeightedValueMap
singleMap v = (Map.singleton v 1, 0)

addN64 :: Integer -> Integer -> Integer
addN64 x y = toSigned n64 (x + y)

sumMap2 :: WeightedValueMap -> WeightedValueMap -> WeightedValueMap
sumMap2 (xm,xo) (ym,yo) =(rm,toSigned n64 (xo+yo))
  where rm = Map.unionWith addN64 xm ym

diffMap2 :: WeightedValueMap -> WeightedValueMap -> WeightedValueMap
diffMap2 (xm,xo) (ym,yo) =(rm,toSigned n64 (xo+yo))
  where rm = Map.unionWith addN64 xm (negate <$> ym)

mulMap :: Integer -> WeightedValueMap -> WeightedValueMap
mulMap (toSigned n64 -> i) (m,c) = (g <$> m, g c)
  where g v = toSigned n64 (i * v)

--minMap :: WeightedValueMap -> WeightedValueMap -> WeightedValueMap
--minMap

------------------------------------------------------------------------
-- StackDelta

-- | Describe the amount of space needed to allocate for the stack.
-- The first parameter is a constant, the remaining values are the
-- set of stack locations accessed.  The height of the stack must be
-- at least as large as each value in the set.
data StackDelta = StackDelta Int64 (Set (Value (BVType 64)))
  deriving (Show)

initStackDelta :: FnStack -> StackDelta
initStackDelta _ = StackDelta 0 Set.empty

-- | Create a stack height from a single value.
valueAsStackDelta :: Value (BVType 64) -> StackDelta
valueAsStackDelta x
  | Just xc <- asInt64Constant x = StackDelta (min xc 0) Set.empty
  | otherwise = StackDelta 0 (Set.singleton x)

data StackDiff
   = StackDiff WeightedValueMap
     -- ^ A stack pointer with a weighted list of values.
   | NoStackDiff WeightedValueMap
     -- ^ A list of values that are not sums.

mulStackDiff :: Integer -> Value (BVType 64) -> StackDiff
mulStackDiff 0 _ = NoStackDiff (concreteMap 0)
mulStackDiff 1 v = parseStackPointer v
mulStackDiff c v =
  case parseStackPointer v of
    NoStackDiff m -> NoStackDiff (mulMap c m)
    StackDiff _ -> NoStackDiff (Map.singleton v (toSigned n64 c), 0)

addStackDiffOffset :: StackDiff -> Int64 -> StackDiff
addStackDiffOffset (StackDiff (m,o))   p = StackDiff (m,o+toInteger p)
addStackDiffOffset (NoStackDiff (m,o)) p = NoStackDiff (m,o+toInteger p)

parseStackPointer :: Value (BVType 64) -> StackDiff
parseStackPointer addr
  | Just (BVAdd _ x y) <- valueAsApp addr =
     case (parseStackPointer x, parseStackPointer y) of
       (StackDiff _, StackDiff _) -> trace "Adding two stack offsets" $
         NoStackDiff $ singleMap addr
       (StackDiff xd, NoStackDiff yd) ->
         StackDiff $ sumMap2 xd yd
       (NoStackDiff xd, StackDiff yd) ->
         StackDiff $ sumMap2 xd yd
       (NoStackDiff xd, NoStackDiff yd) ->
         NoStackDiff $ sumMap2 xd yd
  | Just (BVSub _ x y) <- valueAsApp addr =
     case (parseStackPointer x, parseStackPointer y) of
       (_, StackDiff _) -> trace "Subtracting stack diff" $
         NoStackDiff $ singleMap addr
       (StackDiff xd, NoStackDiff yd) ->
         StackDiff $ xd `diffMap2` yd
       (NoStackDiff xd, NoStackDiff yd) ->
         NoStackDiff $ xd `diffMap2` yd
  | Just (BVMul _ (BVValue _ x) y) <- valueAsApp addr =
      mulStackDiff x y
  | Just (BVMul _ x (BVValue _ y)) <- valueAsApp addr =
      mulStackDiff y x
  | BVValue _ i <- addr =
      NoStackDiff $ concreteMap i
  | Initial n <- addr, Just Refl <- testEquality n rsp =
      StackDiff (concreteMap 0)
  | otherwise = NoStackDiff $ singleMap addr

parseStackPointer addr
  | Just (BVAdd _ x y) <- valueAsApp addr =
     case (parseStackPointer x, parseStackPointer y) of
       (StackDiff _, StackDiff _) -> trace "Adding two stack offsets" $
         NoStackDiff $ singleMap addr
       (StackDiff xd, NoStackDiff yd) ->
         StackDiff $ sumMap2 xd yd
       (NoStackDiff xd, StackDiff yd) ->
         StackDiff $ sumMap2 xd yd
       (NoStackDiff xd, NoStackDiff yd) ->
         NoStackDiff $ sumMap2 xd yd
  | Just (BVSub _ x y) <- valueAsApp addr =
     case (parseStackPointer x, parseStackPointer y) of
       (_, StackDiff _) -> trace "Subtracting stack diff" $
         NoStackDiff $ singleMap addr
       (StackDiff xd, NoStackDiff yd) ->
         StackDiff $ xd `diffMap2` yd
       (NoStackDiff xd, NoStackDiff yd) ->
         NoStackDiff $ xd `diffMap2` yd
  | Just (BVMul _ (BVValue _ x) y) <- valueAsApp addr =
      mulStackDiff x y
  | Just (BVMul _ x (BVValue _ y)) <- valueAsApp addr =
      mulStackDiff y x
  | BVValue _ i <- addr =
      NoStackDiff $ concreteMap i
  | Initial n <- addr, Just Refl <- testEquality n rsp =
      StackDiff (concreteMap 0)
  | otherwise = NoStackDiff $ singleMap addr

{-
-- | This is called to update the state whenever there is an assignment.
updateStackDeltaAssign :: Assignment tp
                       -> StackDelta
                       -> StackDelta
updateStackDeltaAssign _ = id
-}

-- | Conjoin two stack heights to compute the maximum height.
mergeStackDelta :: Value (BVType 64)
                -> Int64 -- ^ Offset for value (added to v
                -> Bool
                -> StackDelta
                -> StackDelta
mergeStackDelta v offset knownStackPointer d@(StackDelta xc xs) =
  case parseStackPointer v `addStackDiffOffset` offset of
    StackDiff (m,yc)
      | Map.null m -> StackDelta (min xc (fromInteger yc)) xs
    StackDiff _ ->
      trace ("Symbolic stack pointer:\n" ++ show (ppValueAssignments v)) $ d
    _
      | knownStackPointer ->
        trace ("Could not interpret stack pointer:\n" ++ show (ppValueAssignments v)) $ d
      | otherwise -> d

{-
mergeStackDelta v knownStackPointer d@(StackDelta xc xs) =
  case asStackAddrOffset v of
    Just offset ->
      case asInt64Constant offset of
        Just yc -> StackDelta (min xc yc) xs
        Nothing -> trace ("Symbolic stack delta " ++ show offset) $
          StackDelta xc (Set.insert offset xs)
    Nothing
      | knownStackPointer ->
        trace ("Could not interpret stack pointer " ++ show v) $ d
      | otherwise -> d
-}

stackDeltaAllocSize :: StackDelta -> Maybe (FnValue (BVType 64))
stackDeltaAllocSize h@(StackDelta c s)
  | c == 0 && Set.null s = Nothing
  | Set.null s =
    Just $! FnConstantValue n64 (toInteger (negate c))
  | otherwise =
    trace ("Unsupported stack delta\n" ++ show h)  $ Nothing

------------------------------------------------------------------------
-- FnStack

-- | This stores information about the current stack when working on function
-- identification.
data FnStack = UndefinedFnStack

initFnStack :: FnStack
initFnStack = UndefinedFnStack

-- | Given information about the stack and a offset into the stack, return
-- a function value denoting the given location.
stackOffsetAddr :: FnStack -> Value (BVType 64) -> FnValue (BVType 64)
stackOffsetAddr _ _ = trace "stackOffsetAddr unsupported" $
 FnValueUnsupported

-- | Record a register as being callee saved.
recordCalleeSavedWrite :: Value (BVType 64) -- ^ Offset in stack
                       -> RegisterName 'GP  -- ^ Register that is saved.
                       -> FnStack -- ^ Current stack
                       -> FnStack
recordCalleeSavedWrite _ _ s = s

-- | Record an allocation with the given type to the stack.
recordStackAlloca :: FnValue (BVType 64) -> FnStack -> FnStack
recordStackAlloca _ s = s
