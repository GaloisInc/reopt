{-# LANGUAGE PatternGuards #-}
module Reopt.Relinker.RangeSet
  ( RangeSet
  , empty
  , contains
  , overlaps
  , insert
  ) where

import Control.Exception (assert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A set of ranges.
newtype RangeSet w = RangeSet (Map w w)

-- | An empty set of ranges.
empty :: RangeSet w
empty = RangeSet Map.empty

contains :: Ord w => w -> RangeSet w -> Bool
contains w (RangeSet m) =
  case Map.lookupLE w m of
    Nothing ->  False
    Just (l,h) -> l <= w && w <= h

-- | Return true if there is a overlap between the range set and the given range.
overlaps :: Ord w => w -> w -> RangeSet w -> Bool
overlaps l h (RangeSet m)
  | l > h = False
  | otherwise =
     case Map.lookupLE h m of
       Just (_ll,lh) -> lh >= l
       Nothing -> False

-- | Insert the given range into the set.
insert :: Ord w => w -> w -> RangeSet w -> RangeSet w
insert l h (RangeSet m)

   | l > h = RangeSet m
     -- Check for overlap.
   | Just (ll,lh) <- Map.lookupLE h m -- Find highest range less-than or equal to h.
   , lh >= l =                        -- Check if the range if above the lower bound.
     -- Check if the current range already subsumes inserted range.
     if ll <= l && h <= lh then
       RangeSet m
      else
       -- Delete this range, but expand the current insertion point to cover it.
       insert (min l ll) (max h lh) (RangeSet (Map.delete ll m))
   | otherwise = assert (l <= h) $
     RangeSet $ Map.insert l h m
