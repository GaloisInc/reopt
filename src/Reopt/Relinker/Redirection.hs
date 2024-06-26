{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module is used to insert jumps to new code locations into existing
-- code.
module Reopt.Relinker.Redirection (
  CodeRedirection (..),
  isFuncSymbol,
  ResolvedCodeRedirs,
  emptyResolvedCodeRedirs,
  resolveCodeRedirections,
  insertStaticRedirs,
) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Aeson.Types (
  FromJSON (..),
  Parser,
  ToJSON (..),
  Value (..),
  object,
  typeMismatch,
  (.:),
  (.=),
 )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Internal qualified
import Data.ElfEdit qualified as Elf
import Data.String (fromString)
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as SMV
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack)

-- | Write bytestring to bitvector at given offset.
writeBS :: HasCallStack => SV.MVector s Word8 -> Int -> BS.ByteString -> ST s ()
writeBS mv base bs = do
  let len = BS.length bs
  when (SMV.length mv < base + len) $ do
    error "Bytestring overflows buffer."
  forM_ [0 .. len - 1] $ \i -> do
    SMV.write mv (base + i) (bs `BS.index` i)

------------------------------------------------------------------------
-- Definitions

-- | A code redirection used during relinking that maps a code location in a
-- binary to the replacement code in the new object code.
--
-- The original location is identified by the index of the loadable program
-- header that it resides in along with the file offset.
--
-- The target location is identified by the name of the symbol.
data CodeRedirection w = CodeRedirection
  { redirSourceVAddr :: !w
  -- ^ Virtual address within code segment where we should change.
  , redirSourceSize :: !Int
  -- ^ Number of bytes available at source offset to write redirection.
  , redirTarget :: !BS.ByteString
  -- ^ Name of symbol in object file.
  }
  deriving (Show)

instance Integral w => ToJSON (CodeRedirection w) where
  toJSON :: Integral w => CodeRedirection w -> Value
  toJSON redir =
    object
      [ "vaddr" .= toInteger (redirSourceVAddr redir)
      , "bytes_available" .= redirSourceSize redir
      , "target" .= unpack (redirTarget redir)
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
isFuncSymbol :: Elf.SymtabEntry nm v -> Bool
isFuncSymbol e =
  Elf.steType e == Elf.STT_FUNC
    && Elf.steBind e == Elf.STB_GLOBAL
    && Elf.steIndex e /= Elf.SHN_UNDEF
    && Elf.steIndex e < Elf.SHN_LORESERVE

data ResolvedCodeRedir a = ResolvedCodeRedir
  { resolvedSourceVAddr :: !a
  -- ^ Virtual address of source within this.
  , resolvedContents :: !BS.ByteString
  }

-- | Resolve a code redirection using information from where symbols redirect
-- to.
resolveCodeRedirection ::
  -- | Function for creating jump to given address.
  (a -> BS.ByteString) ->
  -- | Function that given the name of a symbol returns the address of that
  -- symbol in the new binary, or `Nothing` if the symbol is not in the object
  -- file.
  (BS.ByteString -> Maybe a) ->
  -- | Redirections to apply
  CodeRedirection a ->
  Either BS.ByteString [ResolvedCodeRedir a]
resolveCodeRedirection mkJump resolveAddr redirEntry = do
  let nm = redirTarget redirEntry
  tgt <-
    case resolveAddr nm of
      Just tgt -> Right tgt
      Nothing -> Left $! nm
  let jmp = mkJump tgt
  if BS.length jmp > redirSourceSize redirEntry
    then pure []
    else
      let redir =
            ResolvedCodeRedir
              { resolvedSourceVAddr = redirSourceVAddr redirEntry
              , resolvedContents = jmp
              }
       in seq redir (pure [redir])

-- | Code redirections to apply to replace code in the original binary with new
-- code.
newtype ResolvedCodeRedirs a = RCR [ResolvedCodeRedir a]

emptyResolvedCodeRedirs :: ResolvedCodeRedirs a
emptyResolvedCodeRedirs = RCR []

-- | Generate resolved code redirections for inserting jumps from original
-- binary code into new code.
--
-- If we cannot find the address of a symbol in the object file that we expect
-- to be present, this will return the name of the symbol that we could not find
-- in the left field, otherwise it returns all the code redirections.
resolveCodeRedirections ::
  -- | Function for creating jump to given offset.
  (Word64 -> BS.ByteString) ->
  -- | List of redirections generated from
  -- recovered functions.
  [CodeRedirection Word64] ->
  -- | Function that given the name of a symbol
  -- returns the address of that symbol in the
  -- new binary, or `Nothing` if the symbol is
  -- not in the object file.
  (BS.ByteString -> Maybe (Elf.ElfWordType 64)) ->
  Either BS.ByteString (ResolvedCodeRedirs (Elf.ElfWordType 64))
resolveCodeRedirections mkJump redirs resolveAddr = do
  RCR . concat <$> traverse (resolveCodeRedirection mkJump resolveAddr) redirs

-- | This takes a bytestring in the original binary and updates it with
-- relocations to point to the new binary.
insertStaticRedirs ::
  (HasCallStack, Integral a) =>
  -- | List of redirections to apply
  ResolvedCodeRedirs a ->
  -- | Base address of this bytestring within the file.
  --
  -- This is subtracted from redirection source offset to compute offset in
  -- bytestring to apply this to.
  a ->
  BS.ByteString ->
  BS.ByteString
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
