module Reopt.Utils.Hex (Hex(..)) where

import Numeric (showHex)

newtype Hex a = Hex a
  deriving (Eq, Ord)

instance (Integral a, Show a) => Show (Hex a) where
  show (Hex v) | v >= 0 = showHex v ""
