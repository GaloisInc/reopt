{-|
This module contains constants used as assumptions in relinking.
-}
module Reopt.Relinker.Constants
  ( overflowOffsetMultiple
  , pageSize
  ) where


-- | Multiple for offset of overflow code section
overflowOffsetMultiple :: Int
overflowOffsetMultiple = 16

-- | Page size (new program headers are aligned to page boundaries)
pageSize :: Int
pageSize = 0x1000