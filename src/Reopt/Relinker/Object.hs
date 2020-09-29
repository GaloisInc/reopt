{-|
This contains defines operations to work with the object containing new code.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
module Reopt.Relinker.Object
  ( ObjectSectionIndex
    -- * Object symbol table support.
  , ObjectSymbolTableEntry(..)
  , objectEntryName
  , objectEntryStringName
  ) where

import qualified Data.ByteString.Char8 as BSC
import           Data.ElfEdit
import           Data.Word

-- | Identifies this should be a section index in the object file
-- being merged in.
type ObjectSectionIndex = Word16

------------------------------------------------------------------------
-- ObjectSymbolTableEntry

-- | A symbol table entry in the object with the new code.
newtype ObjectSymbolTableEntry w = OSTE (ElfSymbolTableEntry BSC.ByteString (ElfWordType w))

-- | Return the symbol name.
objectEntryName :: ObjectSymbolTableEntry w -> BSC.ByteString
objectEntryName (OSTE sym) = steName sym

-- | Return the name of a symbol as a string (or <unamed symbol> if not defined).
objectEntryStringName :: ObjectSymbolTableEntry w -> String
objectEntryStringName (OSTE sym)
  | BSC.null (steName sym) = "<unnamed symbol>"
  | otherwise = BSC.unpack (steName sym)
