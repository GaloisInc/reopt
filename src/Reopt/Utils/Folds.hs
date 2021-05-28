module Reopt.Utils.Folds (sumBy) where

-- | Sum a map over a structure.
--
-- `sumBy f t = sum (fmap f t)`
sumBy :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumBy f = foldl (\z e -> z + f e) 0