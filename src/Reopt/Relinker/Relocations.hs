{-|
This module is used to relocate object code into the new address.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Relinker.Relocations
  ( ObjectSectionIndex
  , RelocInfo
  , mkRelocInfo
  , performRelocs
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal
import qualified Data.ElfEdit as Elf
import           Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Data.Word
import           GHC.Stack
import           Numeric (showHex)
import           Text.Printf

-- | Identifies this should be a section index in the object file
-- being merged in.
type ObjectSectionIndex = Word16


-- | Index of symbol in the object file.
type ObjectSymbolTableIndex = Word32

newtype RelocInfo a =  RelocInfo (ObjectSymbolTableIndex -> Either String a)
-- ^ Maps indices of symbols defined in the original binary to their
-- name and address.
--
-- The name is just used for error reporting.  For section symbols it
-- refer to the name of the section, and for other symbols it should
-- be the name of the symbol.
--
-- The symbols may be in the code or data section.

-- | Get the address of a symbol table if it is mapped in the section map.
resolveRelocEntry :: (Eq a, Num a)
                  => (BS.ByteString -> Maybe a)
                     -- ^ Map from undefined symbol name to address in original binary (if known).
                  -> (ObjectSectionIndex -> Maybe a)
                     -- ^ Maps object section indices to the associated address.
                     --
                     -- Returns @Nothing@ if the object section index
                     -- is invalid as it is not loaded or out of
                     -- range.
                  -> ObjectSymbolTableIndex
                     -- ^ Index of symbol table in elf.
                  -> Elf.SymtabEntry BS.ByteString a
                  -- ^ The symbol table entry in the new object.
                  -> Either String a
resolveRelocEntry undefMap secMap idx sym = do
  let secIdx = Elf.fromElfSectionIndex (Elf.steIndex sym)
  case Elf.steType sym of
    -- Skip file symbols as they just contain the name of the file used to create the object.
    Elf.STT_FILE ->
      Left $ "Relocations targetting file symbols are not supported."
    Elf.STT_SECTION
      | Elf.steValue sym /= 0 ->
        Left "resolveRelocEntry expects section names to have 0 offset."
      | Elf.steIndex sym == Elf.SHN_UNDEF ->
         Left $ "Reference to undefined section."
         -- Section symbol indices allowed to be unmapped.
      | otherwise ->
        case secMap secIdx of
          Just r -> Right r
          Nothing -> Left $ "Could not resolve elocation with section index " <> show secIdx
    tp -> do
      when (tp `notElem` [Elf.STT_FUNC, Elf.STT_NOTYPE]) $ do
        Left $ "resolveRelocEntry does not support symbol with type " ++ show tp ++ "."
      if Elf.steIndex sym == Elf.SHN_UNDEF then
        case undefMap (Elf.steName sym) of
          Nothing -> Left $ "Unknown undefined symbol " <> BSC.unpack (Elf.steName sym)
          Just a -> Right a
       else
        case secMap secIdx of
          Just base -> Right (base + Elf.steValue sym)
          Nothing -> Left $ printf "Symbol table entry %d refers to unmapped section index." idx

-- | This generates the relocation information.
mkRelocInfo :: forall a
            .  (Eq a, Num a)
            => (BS.ByteString -> Maybe a)
               -- ^ Map from undefined symbol name to address in original binary (if known).
            -> (ObjectSectionIndex -> Maybe a)
               -- ^ Maps object section indices to the name and associated address.
            -> V.Vector (Elf.SymtabEntry BS.ByteString a)
               -- ^ Symbol table entries in object.
            -> RelocInfo a
mkRelocInfo undefMap secMap entries = RelocInfo f
  where f :: ObjectSymbolTableIndex -> Either String a
        f i = do
          when (fromIntegral i >= V.length entries) $
            Left "Invalid symbol index."
          let e = entries V.! fromIntegral i
          resolveRelocEntry undefMap secMap i e

write32LSB :: SMV.MVector s Word8 -> Word64 -> Word32 -> ST s ()
write32LSB v a c = do
  -- Assert a+3 doesn't overflow.
  when (SMV.length v < 4 || a >= fromIntegral (SMV.length v) - 3) $
    error "Illegal relative addr in relocation."
  let i = fromIntegral a
  SMV.write v i     $ fromIntegral c
  SMV.write v (i+1) $ fromIntegral (c `shiftR`  8)
  SMV.write v (i+2) $ fromIntegral (c `shiftR` 16)
  SMV.write v (i+3) $ fromIntegral (c `shiftR` 24)

-- | Perform a relocation listed in the new object.
performReloc :: RelocInfo Word64
                -- ^ Maps indices of symbols in the original binary to
                -- their name and address.
             -> Word64
                -- ^ Address of this section.
             -> SV.MVector s Word8
                -- ^ Contents of elf section we are apply this to.
             -> Elf.RelaEntry Elf.X86_64_RelocationType
                -- ^ The relocation entry.
             -> ExceptT String (ST s) ()
performReloc (RelocInfo symbolAddrMap) thisAddr mv rela = do
  -- Virtual address to update
  let vaddr = Elf.relaAddr rela
  -- Get the address of a symbol
  case symbolAddrMap (Elf.relaSym rela) of
    Left _ -> do
      throwError $ "Could not resolve " ++ show (Elf.relaSym rela)
    Right symAddr -> do
      -- Relocation addend
      let addend = Elf.relaAddend rela
      -- Get PC offset
      let pc_offset = thisAddr + vaddr
      -- Parse on type
      case Elf.relaType rela of
        Elf.R_X86_64_PC32 -> do
          let res64 :: Word64
              res64 = symAddr + fromIntegral addend - pc_offset
          let res32 :: Word32
              res32 = fromIntegral res64
          lift $ write32LSB mv vaddr res32
        Elf.R_X86_64_32 -> do
          let res64 = symAddr + fromIntegral addend :: Word64
          let res32 = fromIntegral res64 :: Word32
          when (fromIntegral res32 /= res64) $ do
            throwError $ "Relocation at " <> showHex vaddr " does not safely zero extend."
          lift $ write32LSB mv vaddr res32
        Elf.R_X86_64_32S -> do
          let res64 = fromIntegral symAddr + addend :: Int64
              res32 = fromIntegral res64 :: Int32
          when (fromIntegral res32 /= res64) $
            throwError $ "Relocation at " <> showHex vaddr " does not safely sign extend."
          lift $ write32LSB mv vaddr (fromIntegral res32)
        _ -> do
          throwError "Relocation not supported"

performRelocs :: HasCallStack
              => RelocInfo (Elf.ElfWordType 64)
                -- ^ Maps indices of symbols in the original binary to
                -- their name and address.
              -> Word64 -- ^ Base address of this section.
              -> BS.ByteString
                 -- ^ Data in section that we are applying relocations to.
              -> [Elf.RelaEntry Elf.X86_64_RelocationType]
                 -- ^ Relocation entries in original binary to apply.
              -> Either String BS.ByteString
performRelocs relocInfo thisAddr dta relocs = runST $ runExceptT $ do
  let len = BS.length dta
  mv <- lift $ SMV.new len
  -- Copy original bytes into bytestring (cannot overflow)
  lift $ forM_ [0..len-1] $ \i -> SMV.write mv i (dta `BS.index` i)
  -- Update using relocations
  mapM_ (performReloc relocInfo thisAddr mv) relocs
  let SMV.MVector _ fp = mv
  pure $! Data.ByteString.Internal.fromForeignPtr fp 0 len
