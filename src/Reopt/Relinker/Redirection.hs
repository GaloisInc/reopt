{-|
Module      : Reopt.Relinker.Redirection
Copyright   : (c) Galois Inc, 2015
Maintainer  : jhendrix@galois.com

This module defines the 'CodeRedirection' datatype and its
JSON format.  This is used by the 
-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.Relinker.Redirection
  ( CodeRedirection(..)
  , PhdrIndex
  ) where

import           Data.Aeson.Types
  ( typeMismatch
  , FromJSON(..)
  , ToJSON(..)
  , (.=)
  , (.:)
  , Parser
  , Value(..)
  , object
  )
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (unpack)
import           Data.String (fromString)
import           Data.Word

-- | Index of program header in an elf file.
type PhdrIndex = Word16

-- | A code redirection used during relinking that
-- maps a code location in a binary to the replacement code in
-- the new object code.
--
-- The original location is identified by the index of the loadable program
-- header that it resides in along with the file offset.
--
-- The target location is identified by the name of the symbol.
data CodeRedirection w
   = CodeRedirection { redirSourcePhdr :: !PhdrIndex
                     , redirSourceOffset :: !w
                       -- ^ Offset in phdr where we should write file.
                     , redirTarget :: !BS.ByteString
                       -- ^ Target symbol table name.
                     } deriving (Show)

instance Integral w => ToJSON (CodeRedirection w) where
  toJSON redir =
    object
      [ "phdr"   .= redirSourcePhdr redir
      , "offset" .= toInteger (redirSourceOffset redir)
      , "target" .= unpack (redirTarget redir)
      ]

instance Num w => FromJSON (CodeRedirection w) where
  parseJSON (Object o) =
    CodeRedirection
      <$> o .: "phdr"
      <*> (fromIntegral <$> (o .: "offset" :: Parser Integer))
      <*> (fromString <$> (o .: "target"))
  parseJSON v = typeMismatch "CodeRedirection" v
