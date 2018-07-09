{-|
Copyright   : (c) Galois Inc, 2015-2018
Maintainer  : jhendrix@galois.com

This module is used to insert jumps to new code
locations into existing code.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.Relinker.Redirection
  ( CodeRedirection(..)
  , mkCodeAddrMap
  , ResolvedCodeRedir(..)
  , resolveCodeRedirection
  , isFuncSymbol
  , insertStaticRedirs
  ) where

import           Control.Monad
import           Control.Monad.ST
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
import qualified Data.ByteString.Internal
import           Data.ElfEdit
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Data.Word
import           GHC.Stack (HasCallStack)

import           Reopt.Relinker.Object
  ( ObjectSectionIndex
  )

-- | Write bytestring to bitvector at given offset.
writeBS :: HasCallStack => SV.MVector s Word8 -> Int -> BS.ByteString -> ST s ()
writeBS mv base bs = do
  let len = BS.length bs
  when (SMV.length mv < base + len) $ do
    error $ "Bytestring overflows buffer."
  forM_ [0..len-1] $ \i -> do
    SMV.write mv (base+i) (bs `BS.index` i)


------------------------------------------------------------------------
-- Definitions

-- | A code redirection used during relinking that maps a code location in a
-- binary to the replacement code in the new object code.
--
-- The original location is identified by the index of the loadable program
-- header that it resides in along with the file offset.
--
-- The target location is identified by the name of the symbol.
data CodeRedirection w
   = CodeRedirection { redirSourceOffset :: !w
                       -- ^ Virtual address within code segment where we should change.
                     , redirSourceSize :: !Int
                       -- ^ Number of bytes available at source offset to write redirection.
                     , redirTarget :: !BS.ByteString
                       -- ^ Name of symbol in object file.
                     } deriving (Show)

instance Integral w => ToJSON (CodeRedirection w) where
  toJSON redir =
    object
      [ "offset"          .= toInteger (redirSourceOffset redir)
      , "bytes_available" .= (redirSourceSize redir)
      , "target"          .= unpack (redirTarget redir)
      ]

instance Num w => FromJSON (CodeRedirection w) where
  parseJSON (Object o) =
    CodeRedirection
      <$> (fromIntegral <$> (o .: "offset" :: Parser Integer))
      <*> (o .: "bytes_available")
      <*> (fromString <$> (o .: "target"))
  parseJSON v = typeMismatch "CodeRedirection" v

-- | Return true if this symbol table entry appears correcponds to a
-- globally defined function.
isFuncSymbol :: ElfSymbolTableEntry a -> Bool
isFuncSymbol e
  =  steType e == STT_FUNC
  && steBind e == STB_GLOBAL
  && steIndex e /= SHN_UNDEF
  && steIndex e < SHN_LORESERVE


-- | Resolve address of symbols defined in their code region.
resolveDefinedAddr :: Map ObjectSectionIndex a
                   -- ^ Maps object section indices to the associated address.
                   -> ElfSymbolTableEntry a
                   -> (BS.ByteString, a)
resolveDefinedAddr secMap e =
  case Map.lookup (fromElfSectionIndex (steIndex e)) secMap of
    Nothing -> error $ "Could not find index of section " ++ show (steIndex e)
    Just addr -> (steName e, addr)

-- | Generate a map from functions defined in the object file to their
-- address in the new binary.  This will only point to new code.
mkCodeAddrMap :: Map ObjectSectionIndex a
                 -- ^ Maps object section indices to their name and associated address.
              -> V.Vector (ElfSymbolTableEntry a)
                 -- ^ Symbol table entries
              -> Map BS.ByteString a
mkCodeAddrMap secMap entries
    | Map.size m == length addrList = m
    | otherwise = error "Duplicate function names detected in object file."
  where addrList = fmap (resolveDefinedAddr secMap)
                 $ V.toList entries
        m = Map.fromList addrList

data ResolvedCodeRedir a
   = ResolvedCodeRedir { resolvedSourceAddr :: !a
                         -- ^ Virtual address of source within this.
                       , resolvedContents   :: !BS.ByteString
                       }

resolveCodeRedirection :: (a -> BS.ByteString)
                       -- ^ Function for creating jump to given address.
                       -> Map BS.ByteString a
                       -- ^ Maps names of symbols defined in the object file to
                       -- their address in the new binary.  This will only point to
                       -- new code.
                       -> CodeRedirection a
                       -- ^ List of redirections to apply
                       -> Maybe (ResolvedCodeRedir a)
resolveCodeRedirection mkJump codeAddrMap redirEntry
    -- Only apply redirection when there is enough space to write the code.
   | BS.length jmp <= redirSourceSize redirEntry = Just redir
   | otherwise = Nothing
  where nm = redirTarget redirEntry
        jmp =
          case Map.lookup nm codeAddrMap of
            Just tgt -> mkJump tgt
            Nothing -> error $ "Could not find symbol name " ++ unpack nm ++ "."
        redir =
          ResolvedCodeRedir { resolvedSourceAddr = redirSourceOffset redirEntry
                            , resolvedContents = jmp
                            }


-- | This takes a bytestring in the original binary and updates it with relocations
-- to point to the new binary.
insertStaticRedirs :: (HasCallStack, Integral a)
                   => [ResolvedCodeRedir a]
                   -- ^ List of redirections to apply
                   -> a
                   -- ^ Base address of this bytestring within the file.
                   --
                   -- This is subtracted from redirection source offset to compute
                   -- offset in bytestring to apply this to.
                   -> BS.ByteString
                   -> BS.ByteString
insertStaticRedirs [] _ bs = bs
insertStaticRedirs redirs base bs = runST $ do
  let len :: Int
      len = BS.length bs
  mv <- SMV.new len
  -- Copy original bytes into bytestring
  writeBS mv 0 bs
  -- Apply relocations.
  forM_ redirs $ \redir -> do
    let off = resolvedSourceAddr redir
    when (base <= off && off - base < fromIntegral len) $ do
      writeBS mv (fromIntegral (off - base)) (resolvedContents redir)

  let SMV.MVector _ fp = mv
  return $! Data.ByteString.Internal.fromForeignPtr fp 0 len
