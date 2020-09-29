{-|
This module is used to insert jumps to new code
locations into existing code.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Reopt.Relinker.Redirection
  ( CodeRedirection(..)
  , isFuncSymbol
  , ResolvedCodeRedirs
  , emptyResolvedCodeRedirs
  , resolveCodeRedirections
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
import           Data.String (fromString)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Data.Word
import           GHC.Stack (HasCallStack)


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
   = CodeRedirection { redirSourceVAddr :: !w
                       -- ^ Virtual address within code segment where we should change.
                     , redirSourceSize :: !Int
                       -- ^ Number of bytes available at source offset to write redirection.
                     , redirTarget :: !BS.ByteString
                       -- ^ Name of symbol in object file.
                     } deriving (Show)

instance Integral w => ToJSON (CodeRedirection w) where
  toJSON redir =
    object
      [ "vaddr"           .= toInteger (redirSourceVAddr redir)
      , "bytes_available" .= (redirSourceSize redir)
      , "target"          .= unpack (redirTarget redir)
      ]

instance Num w => FromJSON (CodeRedirection w) where
  parseJSON (Object o) =
    CodeRedirection
      <$> (fromIntegral <$> (o .: "vaddr" :: Parser Integer))
      <*> (o .: "bytes_available")
      <*> (fromString <$> (o .: "target"))
  parseJSON v = typeMismatch "CodeRedirection" v

-- | Return true if this symbol table entry appears correcponds to a
-- globally defined function.
isFuncSymbol :: ElfSymbolTableEntry nm v -> Bool
isFuncSymbol e
  =  steType e == STT_FUNC
  && steBind e == STB_GLOBAL
  && steIndex e /= SHN_UNDEF
  && steIndex e < SHN_LORESERVE

data ResolvedCodeRedir a
   = ResolvedCodeRedir { resolvedSourceVAddr :: !a
                         -- ^ Virtual address of source within this.
                       , resolvedContents   :: !BS.ByteString
                       }

-- | Resolve a code redirection using information from where symbols
-- redirect to.
resolveCodeRedirection :: (a -> BS.ByteString)
                       -- ^ Function for creating jump to given address.
                       -> (BS.ByteString -> Maybe a)
                       -- ^ Function that given the name of a symbol
                       -- returns the address of that symbol in the
                       -- new binary, or `Nothing` if the symbol is
                       -- not in the object file.
                       -> CodeRedirection a
                       -- ^ Redirections to apply
                       -> Either BS.ByteString [ResolvedCodeRedir a]
resolveCodeRedirection mkJump resolveAddr redirEntry = do
  let nm = redirTarget redirEntry
  tgt <-
    case resolveAddr nm of
      Just tgt -> Right tgt
      Nothing -> Left $! nm
  let jmp = mkJump tgt
  if BS.length jmp > redirSourceSize redirEntry then
    pure $! []
   else
    let redir = ResolvedCodeRedir { resolvedSourceVAddr = redirSourceVAddr redirEntry
                                  , resolvedContents = jmp
                                  }
     in seq redir $ (pure $! [redir])


-- | Code redirections to apply to replace code in the original binary
-- with new code.
newtype ResolvedCodeRedirs a = RCR [ResolvedCodeRedir a]

emptyResolvedCodeRedirs :: ResolvedCodeRedirs a
emptyResolvedCodeRedirs = RCR []

-- | Generate resolved code redirections for inserting jumps from original
-- binary code into new code.
--
-- If we cannot find the address of a symbol in the object file that we
-- expect to be present, this will return the name of the symbol that
-- we could not find in the left field, otherwise it returns all the
-- code redirections.
resolveCodeRedirections :: (Word64 -> BS.ByteString)
                        -- ^ Function for creating jump to given offset.
                        -> [CodeRedirection Word64]
                        -- ^ List of redirections generated from
                        -- recovered functions.
                        -> (BS.ByteString -> Maybe (ElfWordType 64))
                       -- ^ Function that given the name of a symbol
                        -- returns the address of that symbol in the
                        -- new binary, or `Nothing` if the symbol is
                        -- not in the object file.
                        -> Either BS.ByteString (ResolvedCodeRedirs (ElfWordType 64))
resolveCodeRedirections mkJump redirs resolveAddr = do
  RCR . concat <$> traverse (resolveCodeRedirection mkJump resolveAddr) redirs

-- | This takes a bytestring in the original binary and updates it with relocations
-- to point to the new binary.
insertStaticRedirs :: (HasCallStack, Integral a)
                   => ResolvedCodeRedirs a
                   -- ^ List of redirections to apply
                   -> a
                   -- ^ Base address of this bytestring within the file.
                   --
                   -- This is subtracted from redirection source offset to compute
                   -- offset in bytestring to apply this to.
                   -> BS.ByteString
                   -> BS.ByteString
insertStaticRedirs (RCR []) _ bs = bs
insertStaticRedirs (RCR redirs) base bs = runST $ do
  let len :: Int
      len = BS.length bs
  mv <- SMV.new len
  -- Copy original bytes into bytestring
  writeBS mv 0 bs
  -- Apply relocations.
  forM_ redirs $ \redir -> do
    let vaddr = resolvedSourceVAddr redir
    when (base <= vaddr && vaddr - base < fromIntegral len) $ do
      writeBS mv (fromIntegral (vaddr - base)) (resolvedContents redir)

  let SMV.MVector _ fp = mv
  return $! Data.ByteString.Internal.fromForeignPtr fp 0 len
