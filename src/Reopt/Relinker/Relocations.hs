{-|
This module is used to relocate object code into the new address.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
module Reopt.Relinker.Relocations
  ( RelocInfo
  , mkRelocInfo
  , performRelocs
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal
import           Data.ElfEdit
import           Data.Int
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Data.Word
import           GHC.Stack
import           Numeric (showHex)
import           Text.Printf

import           Reopt.Relinker.Object
  ( ObjectSectionIndex
  )

-- | Index of symbol in the object file.
type ObjectSymbolTableIndex = Word32

-- | Write bytestring to bitvector at given offset.
writeBS :: HasCallStack => SV.MVector s Word8 -> Int -> BS.ByteString -> ST s ()
writeBS mv base bs = do
  let len = BS.length bs
  when (SMV.length mv < base + len) $ do
    error $ "Bytestring overflows buffer."
  forM_ [0..len-1] $ \i -> do
    SMV.write mv (base+i) (bs `BS.index` i)

write32LSB :: SMV.MVector s Word8 -> Word64 -> Word32 -> ST s ()
write32LSB v a c = do
  -- Assert a+3 doesn't overflow.
  unless (a <= maxBound-3) $ do
    error "Illegal address in write32LSB"
  let i = fromIntegral a
  SMV.write v i     $ fromIntegral c
  SMV.write v (i+1) $ fromIntegral (c `shiftR`  8)
  SMV.write v (i+2) $ fromIntegral (c `shiftR` 16)
  SMV.write v (i+3) $ fromIntegral (c `shiftR` 24)

type RelocInfo a =  Map ObjectSymbolTableIndex (BS.ByteString, a)
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
                  => Map ObjectSectionIndex (BS.ByteString, a)
                  -- ^ Maps object section indices to the name and associated address.
                  -> ObjectSymbolTableIndex
                  -> ElfSymbolTableEntry BS.ByteString a
                  -- ^ The symbol table entry in the new object.
                  -> Maybe (BS.ByteString, a)
resolveRelocEntry secMap idx sym = do
  let secIdx = fromElfSectionIndex (steIndex sym)
  case steType sym of
    -- Skip file symbols as they just contain the name of the file used to create the object.
    STT_FILE -> Nothing
    STT_SECTION
      | steValue sym /= 0 ->
        error "resolveRelocEntry expects section names to have 0 offset."
      | steIndex sym == SHN_UNDEF ->
         error $ "Reference to undefined section."
         -- Section symbol indices allowed to be unmapped.
      | otherwise -> Map.lookup secIdx secMap
    STT_FUNC
      --  JHx Note: The inability to resolve undef symbol is likely wrong.
      --
      -- We need to be able to look them up in the original binary
      -- including symbols and reopt generated ones of the form
      -- reopt_XX_0x__ that were declared by reopt to denote addresses
      -- in the binary
      | steIndex sym == SHN_UNDEF ->
        Nothing
      | otherwise -> do
        case Map.lookup secIdx secMap of
          Just (_, base) -> Just (steName sym, base + steValue sym)
          Nothing -> error $ printf "Symbol table entry %d refers to unmapped section index." idx
    STT_NOTYPE
      | steIndex sym == SHN_UNDEF -> Nothing
      | otherwise -> do
        case Map.lookup secIdx secMap of
          Just (_, base) -> Just (steName sym, base + steValue sym)
          Nothing -> error $ printf "Symbol table entry %d refers to unmapped section index." idx
    tp -> error $ "resolveRelocEntry does not support symbol with type " ++ show tp ++ "."

-- | This generates the relocation information.
mkRelocInfo :: (Eq a, Num a)
            => Map ObjectSectionIndex (BS.ByteString, a)
               -- ^ Maps object section indices to the name and associated address.
            -> V.Vector (ElfSymbolTableEntry BS.ByteString a)
               -- ^ Symbol table entries in object.
            -> RelocInfo a
mkRelocInfo secMap entries
    | Map.size m == length addrList = m
    | otherwise = error "Duplicate function names detected in object file."
  where addrList =
          [ (idx, nameAddr)
          | (idx,e) <- zip [0..] (V.toList entries)
          , nameAddr <- maybeToList (resolveRelocEntry secMap idx e)
          ]
        m = Map.fromList addrList


-- | Perform a relocation listed in the new object.
performReloc :: RelocInfo (ElfWordType 64)
                -- ^ Maps indices of symbols in the original binary to
                -- their name and address.
             -> ElfWordType 64
                -- ^ Address of this section.
             -> SV.MVector s Word8
                -- ^ Contents of elf section we are apply this to.
             -> RelaEntry X86_64_RelocationType
                -- ^ The relocation entry.
             -> ST s ()
performReloc symbolAddrMap thisAddr mv rela = do
  -- Virtual address to update
  let vaddr = relaAddr rela

  -- Get the address of a symbol
  case Map.lookup (relaSym rela) symbolAddrMap of
    Nothing -> do
      error $ "Could not resolve " ++ show (relaSym rela)
    Just (symName, symAddr) -> do
      when (symAddr < 0) $ error "performReloc given negative value"
      -- Relocation addend
      let addend = relaAddend rela
      -- Get PC offset
      let pc_offset = thisAddr + vaddr
      -- Parse on type
      case relaType rela of
        R_X86_64_PC32 -> do
          let res64 :: Word64
              res64 = symAddr + fromIntegral addend - pc_offset
          let res32 :: Word32
              res32 = fromIntegral res64
          write32LSB mv vaddr res32
        R_X86_64_32 -> do
          let res64 = symAddr + fromIntegral addend :: Word64
          let res32 = fromIntegral res64 :: Word32
          when (fromIntegral res32 /= res64) $ do
            error $ "Relocation of " ++ BSC.unpack symName
             ++ " at " ++ showHex symAddr " + " ++ show addend
             ++ " does not safely zero extend."
          write32LSB mv vaddr res32
        R_X86_64_32S -> do
          let res64 = fromIntegral symAddr + addend :: Int64
              res32 = fromIntegral res64 :: Int32
          when (fromIntegral res32 /= res64) $
            error $ "Relocation of " ++ BSC.unpack symName
             ++ " at " ++ showHex symAddr " + " ++ show addend
             ++ " does not safely sign extend."
          write32LSB mv vaddr (fromIntegral res32)
        _ -> do
          error "Relocation not supported"

performRelocs :: HasCallStack
              => RelocInfo (ElfWordType 64)
                -- ^ Maps indices of symbols in the original binary to
                -- their name and address.
              -> Word64 -- ^ Base address of this section.
              -> BS.ByteString
                 -- ^ Data in section that we are applying relocations to.
              -> [RelaEntry X86_64_RelocationType]
                 -- ^ Relocation entries in original binary to apply.
              -> BS.ByteString
performRelocs relocInfo thisAddr dta relocs = runST $ do
  let len = BS.length dta
  mv <- SMV.new len
  -- Copy original bytes into bytestring
  writeBS mv 0 dta
  -- Updpate using relocations
  mapM_ (performReloc relocInfo thisAddr mv) relocs
  let SMV.MVector _ fp = mv
  pure $! Data.ByteString.Internal.fromForeignPtr fp 0 len
