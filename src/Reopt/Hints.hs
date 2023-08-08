-- |
-- This defines a JSON format for providing hints to Reopt to help its
-- analysis.
module Reopt.Hints (
  FunIdent (..),
  resolveSymName,
) where

import Data.ByteString.Char8 qualified as BSC
import Data.Hashable (Hashable (hashWithSalt))
import Data.Word (Word64)
import Numeric (readHex)

-- | A function identifier
data FunIdent
  = -- | A function identified by an offset in the virtual address
    -- space.
    AddrIdent !Word64
  | -- | A function identified by a symbol name.
    SymbolIdent !BSC.ByteString
  deriving (Eq)

instance Hashable FunIdent where
  hashWithSalt i (AddrIdent w) = hashWithSalt (hashWithSalt i (0 :: Int)) w
  hashWithSalt i (SymbolIdent w) = hashWithSalt (hashWithSalt i (1 :: Int)) w

-- | Resolve a hex string or other string as a address of symbol name.
resolveSymName :: String -> FunIdent
resolveSymName ('0' : 'x' : nm) | [(w, "")] <- readHex nm = AddrIdent w
resolveSymName ('0' : 'X' : nm) | [(w, "")] <- readHex nm = AddrIdent w
resolveSymName nm = SymbolIdent (BSC.pack nm)
