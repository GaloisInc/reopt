{-|
Module      : Reopt.CFG.StackHeight
Copyright   : (c) Galois Inc, 2015
Maintainer  : jhendrix@galois.com

This module provides methods for representing the layout of the stack
within a function, and is used in function recovery.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.StackHeight
  ( StackDelta
  , initStackDelta
  , recordPotentialStackOffset
  , stackDeltaAllocSize
  , FnStack
  , initFnStack
  , stackOffsetAddr
  , recordCalleeSavedWrite
  , recordStackAlloca
  , stackArgs
  ) where

import Control.Exception (assert)
import Data.Bits
import Data.Int
import Data.Parameterized.NatRepr
import Data.Word

import Debug.Trace
import Reopt.CFG.FnRep
import Data.Macaw.CFG
import Reopt.Machine.StateNames (RegisterName, RegisterClass(..))
import Data.Macaw.Types
import Reopt.Machine.X86State

{-
------------------------------------------------------------------------
-- WeightedValueMap

-- | A data structured used to describe values that can be expressed as a
-- sum where each subterm is either a concrete value, or a concrete value
-- times a value with type @Value (BVType 64)@.
type WeightedValueMap = (Map.Map (Value X86_64 (BVType 64)) Integer, Integer)

concreteMap :: Integer -> WeightedValueMap
concreteMap v = (Map.empty, v)

singleMap :: Value X86_64 (BVType 64) -> WeightedValueMap
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

-}

{-
------------------------------------------------------------------------
-- StackDiff

data StackDiff
   = StackDiff WeightedValueMap
     -- ^ A stack pointer with a weighted list of values.
   | NoStackDiff WeightedValueMap
     -- ^ A list of values that are not sums.

mulStackDiff :: Integer -> Value X86_64 (BVType 64) -> StackDiff
mulStackDiff 0 _ = NoStackDiff (concreteMap 0)
mulStackDiff 1 v = parseStackPointer v
mulStackDiff c v =
  case parseStackPointer v of
    NoStackDiff m -> NoStackDiff (mulMap c m)
    StackDiff _ -> NoStackDiff (Map.singleton v (toSigned n64 c), 0)

addStackDiffOffset :: StackDiff -> Int64 -> StackDiff
addStackDiffOffset (StackDiff (m,o))   p = StackDiff (m,o+toInteger p)
addStackDiffOffset (NoStackDiff (m,o)) p = NoStackDiff (m,o+toInteger p)

parseStackPointer :: Value X86_64 (BVType 64) -> StackDiff
parseStackPointer addr
  | Just (BVAdd _ x y) <- valueAsApp addr =
     case (parseStackPointer x, parseStackPointer y) of
       (StackDiff _, StackDiff _) -> trace "WARNING: Adding two stack offsets" $
         NoStackDiff $ singleMap addr
       (StackDiff xd, NoStackDiff yd) ->
         StackDiff $ sumMap2 xd yd
       (NoStackDiff xd, StackDiff yd) ->
         StackDiff $ sumMap2 xd yd
       (NoStackDiff xd, NoStackDiff yd) ->
         NoStackDiff $ sumMap2 xd yd
  | Just (BVSub _ x y) <- valueAsApp addr =
     case (parseStackPointer x, parseStackPointer y) of
       (_, StackDiff _) -> trace "WARNING: Subtracting stack diff" $
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
-}

------------------------------------------------------------------------
-- SallocBase

-- | This is a list that represents the
data SallocBase = SallocBase [BVValue X86_64 64]

instance Show SallocBase where
  show (SallocBase x) = show (ppValueAssignmentList x)


asSallocBase :: BVValue X86_64 64 -> Maybe SallocBase
asSallocBase (valueAsApp -> Just (BVAdd _ x (BVValue w o))) = do
  SallocBase r <- asSallocBase x
  let neg_o = BVValue w (negate o .&. (toInteger (maxBound :: Word64)))
  return $! SallocBase (neg_o:r)
asSallocBase (valueAsApp -> Just (BVSub _ x y)) = do
  SallocBase r <- asSallocBase x
  return $! SallocBase (y:r)
asSallocBase (Initial r) | Just Refl <- testEquality r sp_reg  =
  return $! SallocBase []
asSallocBase _ = Nothing


stackIncrementBound :: SallocBase -> SallocBase -> SallocBase
stackIncrementBound (SallocBase x) (SallocBase y)
   | yl >= xl = f (yl - xl) x y
   | otherwise = assert (xl > yl) $ f (xl - yl) y x
  where xl = length x
        yl = length y
        f l u v
          | BVValue w uc : ur <- u
          , BVValue _ vc : vr <- drop l v
          , ur == vr =
            SallocBase (BVValue w (max uc vc) : ur)

          | u == drop l v = SallocBase v
          | otherwise = trace ("stackIncrementBound unsupported:\n"
                               ++ show (ppValueAssignmentList x) ++ "\n"
                               ++ show (ppValueAssignmentList y)) $
                        SallocBase v

subSallocBaseOffset :: SallocBase -> Int64 -> SallocBase
subSallocBaseOffset d 0 = d
subSallocBaseOffset (SallocBase (BVValue w o:r)) c | o >= toInteger c =
  SallocBase $ BVValue w (o-toInteger c) : r
subSallocBaseOffset d _ = trace "subSallocBaseOffset undefined" d

------------------------------------------------------------------------
-- StackDelta

-- | Describe the amount of space needed to allocate for the stack.
-- The first parameter is a constant, the remaining values are the
-- set of stack locations accessed.  The height of the stack must be
-- at least as large as each value in the set.
data StackDelta = StackDelta !SallocBase
  deriving (Show)

initStackDelta :: FnStack -> StackDelta
initStackDelta _ = StackDelta (SallocBase [])

{-
-- | This is called to update the state whenever there is an assignment.
updateStackDeltaAssign :: Assignment tp
                       -> StackDelta
                       -> StackDelta
updateStackDeltaAssign _ = id
-}

-- | The Conjoin two stack heights to compute the maximum height.
recordPotentialStackOffset :: BVValue X86_64 64
                              -- ^ The value
                           -> Int64 -- ^ A concrete offset added to value.
                           -> Bool -- ^ A Boolean flag that indicates if this should be astack offset.
                           -> StackDelta
                              -- ^ The current stack deltaa
                           -> StackDelta
recordPotentialStackOffset v offset knownStackPointer (StackDelta d) =
  case asSallocBase v of
    Just i ->
      let u = stackIncrementBound d (subSallocBaseOffset i offset)
       in trace ("Stack increment pointer:\n" ++ show (ppValueAssignments v)) $ (StackDelta u)
    Nothing
      | knownStackPointer ->
        trace ("Could not interpret stack pointer:\n" ++ show (ppValueAssignments v)) $
        StackDelta d
      | otherwise -> StackDelta d

{-
recordPotentialStackOffset v knownStackPointer d@(StackDelta xc xs) =
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
stackDeltaAllocSize h@(StackDelta (SallocBase d))
  | length d == 0 = Nothing
  | [ BVValue _ c ] <- d =
    Just $! FnConstantValue n64 c
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
stackOffsetAddr :: FnStack -> BVValue X86_64 64 -> FnValue (BVType 64)
stackOffsetAddr _ _ = trace "stackOffsetAddr unsupported" $
 FnValueUnsupported "stackOffsetAddr unsupported" knownType

-- | Record a register as being callee saved.
recordCalleeSavedWrite :: BVValue X86_64 64 -- ^ Offset in stack
                       -> RegisterName 'GP  -- ^ Register that is saved.
                       -> FnStack -- ^ Current stack
                       -> FnStack
recordCalleeSavedWrite _ _ s = s

-- | Record an allocation with the given type to the stack.
recordStackAlloca :: FnValue (BVType 64) -> FnStack -> FnStack
recordStackAlloca _ s = s

-- | Get stack arguments
stackArgs :: FnStack -> [Some FnValue]
stackArgs _ = trace "stackArgs is not yet implemented." []
