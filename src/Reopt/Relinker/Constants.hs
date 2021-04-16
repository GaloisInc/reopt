{-|
This module contains constants and general functions used as assumptions in relinking.
-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.Relinker.Constants
  ( overflowOffsetMultiple
  , pageSize
  , newCodeSectionName
  ) where

import qualified Data.ByteString as BS

-- | Multiple for offset of overflow code section
overflowOffsetMultiple :: Int
overflowOffsetMultiple = 16

-- | Page size (new program headers are aligned to page boundaries)
pageSize :: Int
pageSize = 0x1000


newCodeSectionName :: BS.ByteString
newCodeSectionName = ".text.reopt"