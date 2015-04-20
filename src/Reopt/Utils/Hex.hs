module Reopt.Utils.Hex (Hex(..)) where
import Numeric (showHex)

newtype Hex = Hex Integer
  deriving (Eq, Ord)

instance Show Hex where
  show (Hex v) = showHex v ""
