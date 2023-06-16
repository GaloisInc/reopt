{-# LANGUAGE OverloadedStrings #-}

-- | Declares the data structured used to map object file and executable.
module Reopt.Relinker.Relations (
  MergeRelations (..),
  ObjFunDef (..),
  ObjFunRef (..),
) where

import Data.Aeson.Types qualified as A
import Data.ByteString.UTF8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Data.Word
import Numeric (showHex)

newtype UTF8 = UTF8 BS.ByteString

instance A.FromJSON UTF8 where
  parseJSON (A.String t) = pure (UTF8 (T.encodeUtf8 t))
  parseJSON _ = fail "Expected string"

instance A.ToJSON UTF8 where
  toJSON (UTF8 x) = A.String (T.decodeUtf8 x)

-- | @findDuplicateFields f y@ looks for two elements @x@ and @y@ such that
-- @f x == f y@.  If it finds them, then it returns a pair @Just (x,y)@ satisfying
-- that constraint.  Otherwise, it returns nothing.
findDuplicateFields :: forall t a k. (Foldable t, Ord k) => (a -> k) -> t a -> Maybe (a, a)
findDuplicateFields proj t = foldr f (const Nothing) t Map.empty
 where
  f :: a -> (Map k a -> Maybe (a, a)) -> Map k a -> Maybe (a, a)
  f y next seen = do
    let v = proj y
    case Map.lookup v seen of
      Nothing -> next (Map.insert v y seen)
      Just x -> Just (x, y)

--------------------------------------------------------------------------------
-- ObjFunDef

-- | Information about a function present in original binary and
-- expected in object code.
data ObjFunDef = ObjFunDef
  { ofdObjName :: !BS.ByteString
  -- ^ Name of function in object file.
  , ofdBinAddr :: !Word64
  -- ^ Address of function in binary.
  --
  -- This should be unique and a valid address in new binary.
  , ofdBinSize :: !Word64
  -- ^ Number of bytes available in binary to overwrite
  -- original function.
  }

instance A.FromJSON ObjFunDef where
  parseJSON = A.withObject "funInfo" $ \o -> do
    UTF8 objName <- o A..: "objName"
    binAddr <- o A..: "binAddr"
    binSize <- o A..: "binSize"
    pure $!
      ObjFunDef
        { ofdObjName = objName
        , ofdBinAddr = binAddr
        , ofdBinSize = binSize
        }

instance A.ToJSON ObjFunDef where
  toJSON x =
    A.object
      [ "objName" A..= UTF8 (ofdObjName x)
      , "binAddr" A..= ofdBinAddr x
      , "binSize" A..= ofdBinSize x
      ]

--------------------------------------------------------------------------------
-- ObjFunRef

data ObjFunRef = ObjFunRef
  { ofrObjName :: !BS.ByteString
  , ofrBinAddr :: !Word64
  }

instance A.FromJSON ObjFunRef where
  parseJSON = A.withObject "funInfo" $ \o -> do
    UTF8 objName <- o A..: "objName"
    binAddr <- o A..: "binAddr"
    pure $!
      ObjFunRef
        { ofrObjName = objName
        , ofrBinAddr = binAddr
        }

instance A.ToJSON ObjFunRef where
  toJSON x =
    A.object
      [ "objName" A..= UTF8 (ofrObjName x)
      , "binAddr" A..= ofrBinAddr x
      ]

--------------------------------------------------------------------------------
-- MergeRelations

-- | Information for merging binary and object file.
data MergeRelations = MergeRelations
  { mrObjectFuns :: !(V.Vector ObjFunDef)
  -- ^ Functions in object file and their relation to the object.
  , mrUndefinedFuns :: !(V.Vector ObjFunRef)
  -- ^ Functions in the object file and their address in binary.
  }

instance A.FromJSON MergeRelations where
  parseJSON = A.withObject "mergeRelations" $ \o -> do
    defs <- o A..: "definedFuns"
    refs <- o A..: "referencedFuns"
    -- Check names for duplicates
    let names = fmap ofdObjName defs <> fmap ofrObjName refs
    case findDuplicateFields id names of
      Nothing -> pure ()
      Just (x, _) -> do
        fail $ "Found duplicate name " ++ BS.toString x
    -- Check addresses for duplicates
    let addrs = fmap ofdBinAddr defs <> fmap ofrBinAddr refs
    case findDuplicateFields id addrs of
      Nothing -> pure ()
      Just (x, _) -> do
        fail $ "Found duplicate address 0x" ++ showHex x ""
    pure $!
      MergeRelations
        { mrObjectFuns = defs
        , mrUndefinedFuns = refs
        }

instance A.ToJSON MergeRelations where
  toJSON x =
    A.object
      [ "definedFuns" A..= mrObjectFuns x
      , "referencedFuns" A..= mrUndefinedFuns x
      ]
