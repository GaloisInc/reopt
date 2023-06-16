module Reopt.Analysis.Domains.DiffEquations (
  DiffEquations,
  empty,
  addEq,
  joinC,
  relateVars,
) where

import Control.Lens
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)

-- | @intersectSame x y@ returns a map containing only bindings
-- that are equivalent in @x@ and @y@.
intersectSame :: (Ord k, Eq a) => Map k a -> Map k a -> Map k a
intersectSame = Map.mergeWithKey f (const Map.empty) (const Map.empty)
 where
  f _ x y
    | x == y = Just x
    | otherwise = Nothing
{-# INLINE intersectSame #-}

------------------------------------------------------------------------
-- Diff

-- | @Diff v c@ denotes the value v + c
data Diff v = Diff
  { _diffVar :: !v
  , _diffOffset :: !Integer
  }
  deriving (Eq, Ord, Show)

diffOffset :: Simple Lens (Diff v) Integer
diffOffset = lens _diffOffset (\s v -> s{_diffOffset = v})

-- addDiffc :: Integer -> Diff v -> Diff v
-- addDiffc c = diffOffset +~ c

------------------------------------------------------------------------
-- DiffEquations

-- | @DiffEquations v@ represents a set of equations of the form
-- @x = y + c@ where @c@ is an integer.
data DiffEquations v = DiffEquations
  { _eqRepMap :: !(Map v (Diff v))
  , _eqRepSize :: !(Map v Word64)
  , _eqRevMap :: !(Map v [Diff v])
  }
  deriving (Show)

-- | Return the empty set of equations.
empty :: DiffEquations v
empty =
  DiffEquations
    { _eqRepMap = Map.empty
    , _eqRepSize = Map.empty
    , _eqRevMap = Map.empty
    }

-- | Maps each non-representative to the representative
eqRepMap :: Simple Lens (DiffEquations v) (Map v (Diff v))
eqRepMap = lens _eqRepMap (\s v -> s{_eqRepMap = v})

-- | Maps each representative to teh size of its equivalence class.
eqRepSize :: Simple Lens (DiffEquations v) (Map v Word64)
eqRepSize = lens _eqRepSize (\s v -> s{_eqRepSize = v})

-- | Maps each variable that has been a representative to the
-- variables it was a representative for.
-- It should be the case that x |-> [Diff y d] if x = y + d
eqRevMap :: Simple Lens (DiffEquations v) (Map v [Diff v])
eqRevMap = lens _eqRevMap (\s v -> s{_eqRevMap = v})

-- | Return complete list of variables in constraints
-- equal to at least one other variable.
eqVars :: Ord v => DiffEquations v -> Set v
eqVars p = Set.union reps unreps
 where
  reps = Map.keysSet (p ^. eqRepSize)
  unreps = Map.keysSet (p ^. eqRepMap)

------------------------------------------------------------------------
-- Functions

{-# INLINE getRep #-}
getRep :: Ord v => v -> DiffEquations v -> Diff v
getRep v = getRep' v 0

{-# INLINEABLE getRep' #-}

-- | Get a representive fo the difference problem
getRep' :: Ord v => v -> Integer -> DiffEquations v -> Diff v
getRep' v c p = seq c $ do
  case Map.lookup v (p ^. eqRepMap) of
    Nothing -> Diff v c
    Just (Diff r d) -> getRep' r (c + d) p

-- | Return size of equivalence class for representative var.
repSize :: Ord v => v -> DiffEquations v -> Word64
repSize v p = Map.findWithDefault 1 v (p ^. eqRepSize)

mapPrepend :: Ord k => k -> a -> Map k [a] -> Map k [a]
mapPrepend k v = Map.alter (\o -> Just (v : fromMaybe [] o)) k

{-# INLINEABLE addEq #-}

-- | Add a difference equation to constraint set.
addEq :: Ord v => v -> v -> Integer -> DiffEquations v -> Maybe (DiffEquations v)
addEq x y c p = do
  let Diff xr xo = getRep' x 0 p
      Diff yr yo = getRep' y c p
  if xr /= yr
    then do
      let x_sz = repSize xr p
          y_sz = repSize yr p
      -- Merge yr into xr
      if x_sz >= y_sz
        then
          Just $!
            p
              & eqRepMap %~ Map.insert yr (Diff xr (xo - yo))
              & eqRepSize %~ Map.insert xr (x_sz + y_sz) . Map.delete yr
              & eqRevMap %~ mapPrepend xr (Diff yr (yo - xo))
        else
          Just $!
            p
              & eqRepMap %~ Map.insert xr (Diff yr (yo - xo))
              & eqRepSize %~ Map.insert yr (x_sz + y_sz) . Map.delete xr
              & eqRevMap %~ mapPrepend yr (Diff xr (xo - yo))
    else
      if xo == yo
        then Just p -- This is redundent
        else Nothing -- This is a failure.

{-# INLINE addCCSet #-}
addCCSet :: Ord v => v -> Map v Integer -> DiffEquations v -> DiffEquations v
addCCSet x m0 p0 = Map.foldlWithKey f p0 m0
 where
  f p y d = p'
   where
    p' = fromJust (addEq x y d p)

-- | @relateVars x y@ returns an integer `d` such that @x = y + d@ if we can
-- prove such a `d` exists.
{-# INLINEABLE relateVars #-}
relateVars :: Ord v => v -> v -> DiffEquations v -> Maybe Integer
relateVars x y p
  | xr == yr = Just (xo - yo)
  | otherwise = Nothing
 where
  Diff xr xo = getRep x p
  Diff yr yo = getRep y p

-- | @ccSet v p@ returns the set of all variables in `p` other than `v` that are
-- provably equivalent to `v` + some constant.
{-# INLINEABLE ccSet #-}
ccSet :: Ord v => v -> DiffEquations v -> Map v Integer
ccSet v p = Map.delete v (ccSet' [Diff vr vo] p (Map.singleton vr vo))
 where
  Diff vr vo = getRep v p

{-# INLINEABLE ccSet' #-}
ccSet' :: Ord v => [Diff v] -> DiffEquations v -> Map v Integer -> Map v Integer
ccSet' [] _ m = m
ccSet' (Diff _h o : r) p m =
  let
    -- l0 = Map.findWithDefault [] h (p^.eqRevMap)
    -- Add offset o to differences.
    l = (diffOffset +~ o) <$> l
   in
    ccSet' (l ++ r) p (foldl' (\s (Diff v d) -> Map.insert v d s) m l)

-- | @join old new@ returns `Nothing` if all the constraints in `old` are
-- implied by constraints in `new`.  Otherwise, it returns the constraints
-- implied by both `old` and `new`.
{-# INLINEABLE joinC #-}
joinC ::
  Ord v =>
  DiffEquations v ->
  DiffEquations v ->
  Maybe (DiffEquations v)
joinC x y = join' s x y empty False
 where
  s = Set.intersection (eqVars x) (eqVars y)

{-# INLINEABLE join' #-}
join' ::
  Ord v =>
  Set v ->
  -- | "Old constraints"
  DiffEquations v ->
  -- | "New constraints"
  DiffEquations v ->
  -- | Result
  DiffEquations v ->
  -- | If we needed to drop constraints in old constraints in building result.
  Bool ->
  Maybe (DiffEquations v)
join' vars0 old new res dropped = seq res $
  case Set.minView vars0 of
    Nothing
      | dropped -> Just res
      | otherwise -> Nothing
    Just (h, vars) ->
      join' vars' old new res' dropped'
     where
      h_old = ccSet h old
      h_new = ccSet h new
      h_res = intersectSame h_old h_new
      vars' = vars `Set.difference` Map.keysSet h_res
      res' = addCCSet h h_res res
      dropped' = dropped || Map.size h_res < Map.size h_old
