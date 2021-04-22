{-|
This module contains constants and general functions used as assumptions in relinking.
-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.Relinker.Constants
  ( overflowOffsetMultiple
  , pageSize
  , newCodeSectionName
  , newRodataSectionName
  , infoIsShdrIndex
  ) where

import qualified Data.ByteString as BS
import qualified Data.ElfEdit as Elf

-- | Multiple for offset of overflow code section
overflowOffsetMultiple :: Int
overflowOffsetMultiple = 16

-- | Page size (new program headers are aligned to page boundaries)
pageSize :: Int
pageSize = 0x1000

newCodeSectionName :: BS.ByteString
newCodeSectionName = ".text.reopt"

newRodataSectionName :: BS.ByteString
newRodataSectionName = ".rodata.reopt"

-- | Return true if the index of this section should refer to a section
-- index
infoIsShdrIndex :: Elf.Shdr BS.ByteString v  -> Bool
infoIsShdrIndex shdr =
  let nm = Elf.shdrName shdr
   in (Elf.shdrType shdr == Elf.SHT_RELA || nm == ".rela.plt")
      && nm /= ".rela.dyn"