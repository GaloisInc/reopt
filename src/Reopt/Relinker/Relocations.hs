{-# LANGUAGE OverloadedStrings #-}

-- | This module is used to relocate object code into the new address.
module Reopt.Relinker.Relocations (
  ObjectSectionIndex,
  RelocInfo,
  mkRelocInfo,
  findRelaSection,
  performRelocs,
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  runExceptT,
 )
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Bits (Bits (shiftR))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Internal qualified
import Data.ElfEdit qualified as Elf
import Data.Int (Int32, Int64)
import Data.Map qualified as Map
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as SMV
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Text.Printf (printf)

import Reopt.Utils.Flags (hasFlags)

-- | Identifies this should be a section index in the object file
-- being merged in.
type ObjectSectionIndex = Word16

-- | Index of symbol in the object file.
type ObjectSymbolTableIndex = Word32

-- | Information needed to resolve relocations in a symbol table.
data RelocInfo a = RelocInfo
  { relocUndefMap :: !(BS.ByteString -> Maybe a)
  -- ^ Map from undefined symbol name to address in original binary (if known).
  , relocSectAddrMap :: !(ObjectSectionIndex -> Maybe a)
  -- ^ Maps object section indices to the associated address.
  --
  -- Returns @Nothing@ if the object section index
  -- is invalid as it is not loaded or out of
  -- range.
  , relocSymbolTable :: !(V.Vector (Elf.SymtabEntry BS.ByteString a))
  -- ^ Symbol table entries in object.
  , relocSectionMap :: !(Map.Map Word16 Word16)
  -- ^ Maps section indices to the relocation entries for them.
  }

-- | Return index of section with relocations for given section.
findRelaSection :: RelocInfo a -> Word16 -> Maybe Word16
findRelaSection r idx = Map.lookup idx (relocSectionMap r)

-- | Get the address of a symbol table if it is mapped in the section map.
resolveRelocEntry ::
  (Eq a, Num a) =>
  -- | Map from undefined symbol name to address in original binary (if known).
  (BS.ByteString -> Maybe a) ->
  -- | Maps object section indices to the associated address.
  --
  -- Returns @Nothing@ if the object section index
  -- is invalid as it is not loaded or out of
  -- range.
  (ObjectSectionIndex -> Maybe a) ->
  -- | Index of symbol table in elf.
  ObjectSymbolTableIndex ->
  -- | The symbol table entry in the new object.
  Elf.SymtabEntry BS.ByteString a ->
  Either String a
resolveRelocEntry undefMap secMap _idx sym = do
  let secIdx = Elf.fromElfSectionIndex (Elf.steIndex sym)
  case Elf.steType sym of
    -- Skip file symbols as they just contain the name of the file used to create the object.
    Elf.STT_FILE ->
      Left "Relocations to file symbols are not supported."
    Elf.STT_SECTION
      | Elf.steValue sym /= 0 ->
          Left "resolveRelocEntry expects section names to have 0 offset."
      | Elf.steIndex sym == Elf.SHN_UNDEF ->
          Left "Reference to undefined section."
      -- Section symbol indices allowed to be unmapped.
      | otherwise ->
          case secMap secIdx of
            Just r -> Right r
            Nothing -> Left $ "Could not resolve relocation with section index " <> show secIdx
    tp -> do
      when (tp `notElem` [Elf.STT_FUNC, Elf.STT_NOTYPE]) $ do
        Left $ "resolveRelocEntry does not support symbol with type " ++ show tp ++ "."
      if Elf.steIndex sym == Elf.SHN_UNDEF
        then case undefMap (Elf.steName sym) of
          Nothing -> Left $ "Unknown undefined symbol " <> BSC.unpack (Elf.steName sym)
          Just a -> Right a
        else case secMap secIdx of
          Just base -> Right (base + Elf.steValue sym)
          Nothing ->
            Left $
              printf
                "Unmapped section index %s in symbol table entry %s."
                (show secIdx)
                (BSC.unpack (Elf.steName sym))

-- | This generates the relocation information.
mkRelocInfo ::
  forall a.
  (Eq a, Num a, Bits a) =>
  -- | Map from undefined symbol name to address in original binary (if known).
  (BS.ByteString -> Maybe a) ->
  -- | Maps object section indices that are loaded to the name and associated address.
  (ObjectSectionIndex -> Maybe a) ->
  -- | Symbol table entries in object.
  V.Vector (Elf.SymtabEntry BS.ByteString a) ->
  -- | Section headers
  V.Vector (Elf.Shdr BSC.ByteString a) ->
  Either String (RelocInfo a)
mkRelocInfo undefMap secMap entries shdrs = do
  let insRelocShdr ::
        Int ->
        Elf.Shdr BSC.ByteString a ->
        (Map.Map Word16 Word16 -> Either String (Map.Map Word16 Word16)) ->
        (Map.Map Word16 Word16 -> Either String (Map.Map Word16 Word16))
      insRelocShdr i shdr cont m
        | Elf.shdrType shdr == Elf.SHT_RELA = do
            let nm = BSC.unpack (Elf.shdrName shdr) ++ "(" ++ show i ++ ")"
            let link = Elf.shdrInfo shdr
            when (link == 0 || fromIntegral link >= V.length shdrs) $ do
              Left $ printf "Section %s link %s does not reference a section." nm (show link)
            let tgt = shdrs V.! fromIntegral link
            when (".rela" <> Elf.shdrName tgt /= Elf.shdrName shdr) $ do
              Left $
                printf
                  "Relocations %s targets section with unexpected name %s."
                  nm
                  (BSC.unpack (Elf.shdrName tgt))
            unless (Elf.shdrFlags tgt `hasFlags` Elf.shf_alloc) $ do
              Left "Relocations applied to section that is not loaded."
            cont $! Map.insert (fromIntegral link) (fromIntegral i) m
        | otherwise =
            cont m
  m <- V.ifoldr insRelocShdr Right shdrs Map.empty
  pure $!
    RelocInfo
      { relocUndefMap = undefMap
      , relocSectAddrMap = secMap
      , relocSymbolTable = entries
      , relocSectionMap = m
      }

write32LSB :: SMV.MVector s Word8 -> Word64 -> Word32 -> ExceptT String (ST s) ()
write32LSB v a c = do
  -- Assert a+3 doesn't overflow.
  when (SMV.length v < 4 || a > fromIntegral (SMV.length v) - 4) $
    throwError "Illegal relative addr in relocation."
  let i = fromIntegral a
  lift $ do
    SMV.write v i $ fromIntegral c
    SMV.write v (i + 1) $ fromIntegral (c `shiftR` 8)
    SMV.write v (i + 2) $ fromIntegral (c `shiftR` 16)
    SMV.write v (i + 3) $ fromIntegral (c `shiftR` 24)

write64LSB :: SMV.MVector s Word8 -> Word64 -> Word64 -> ExceptT String (ST s) ()
write64LSB v a c = do
  -- Assert a+3 doesn't overflow.
  when (SMV.length v < 8 || a > fromIntegral (SMV.length v) - 8) $
    throwError "Illegal relative addr in relocation."
  let i :: Int
      i = fromIntegral a
  let w j = SMV.write v (i + j) $ fromIntegral (c `shiftR` 8 * fromIntegral j)
  lift $ w 0 >> w 1 >> w 2 >> w 3 >> w 4 >> w 5 >> w 6 >> w 7

-- | Perform a relocation listed in the new object.
performReloc ::
  -- | Maps indices of symbols in the original binary to
  -- their name and address.
  RelocInfo Word64 ->
  -- | Address of this section.
  Word64 ->
  -- | Contents of elf section we are apply this to.
  SV.MVector s Word8 ->
  -- | The relocation entry.
  Elf.RelaEntry Elf.X86_64_RelocationType ->
  ExceptT String (ST s) ()
performReloc relocs thisAddr mv rela = do
  -- Virtual address to update
  let vaddr = Elf.relaAddr rela

  let symIdx = Elf.relaSym rela
  let entries = relocSymbolTable relocs
  when (fromIntegral symIdx >= V.length entries) $ do
    throwError $ "Invalid relocation symbol index " ++ show symIdx
  let symEntry = entries V.! fromIntegral symIdx
  -- Get the address of a symbol
  symAddr <-
    case resolveRelocEntry (relocUndefMap relocs) (relocSectAddrMap relocs) symIdx symEntry of
      Left msg -> throwError msg
      Right symAddr -> pure symAddr
  -- Relocation addend
  let addend = Elf.relaAddend rela
  -- Get PC offset
  let pc_offset = thisAddr + vaddr
  -- Action to perform for PC32 and PLT32 relocations
  let performPC32 = do
        -- Compute PC relative address
        let res64 :: Word64
            res64 = symAddr + fromIntegral addend - pc_offset
        -- Compute 32bit
        let res32 :: Word32
            res32 = fromIntegral res64
        write32LSB mv vaddr res32
  -- Parse on type
  case Elf.relaType rela of
    Elf.R_X86_64_PC32 -> performPC32
    Elf.R_X86_64_PLT32 -> performPC32
    Elf.R_X86_64_32 -> do
      let res64 = symAddr + fromIntegral addend :: Word64
      let res32 = fromIntegral res64 :: Word32
      when (fromIntegral res32 /= res64) $ do
        throwError $ "Relocation at " <> showHex vaddr " does not safely zero extend."
      write32LSB mv vaddr res32
    Elf.R_X86_64_32S -> do
      let res64 = fromIntegral symAddr + addend :: Int64
          res32 = fromIntegral res64 :: Int32
      when (fromIntegral res32 /= res64) $
        throwError $
          "Relocation at " <> showHex vaddr " does not safely sign extend."
      write32LSB mv vaddr (fromIntegral res32)
    Elf.R_X86_64_64 -> do
      let res64 = symAddr + fromIntegral addend
      write64LSB mv vaddr res64
    tp -> do
      throwError $ printf "Relocation type %s is not supported" (show tp)

performRelocs ::
  HasCallStack =>
  -- | Maps indices of symbols in the original binary to
  -- their name and address.
  RelocInfo (Elf.ElfWordType 64) ->
  -- | Base address of this section.
  Word64 ->
  -- | Data in section that we are applying relocations to.
  BS.ByteString ->
  -- | Relocation entries in original binary to apply.
  [Elf.RelaEntry Elf.X86_64_RelocationType] ->
  Either (Int, String) BS.ByteString
performRelocs relocInfo thisAddr dta relocs = runST $ runExceptT $ do
  let len = BS.length dta
  mv <- lift $ SMV.new len
  -- Copy original bytes into bytestring (cannot overflow)
  lift $ forM_ [0 .. len - 1] $ \i -> SMV.write mv i (dta `BS.index` i)
  -- Update using relocations
  let reloc idx rela = do
        m <- lift $ runExceptT $ performReloc relocInfo thisAddr mv rela
        case m of
          Left msg -> throwError (idx, msg)
          Right r -> pure r
  V.imapM_ reloc (V.fromList relocs)
  let SMV.MVector _ fp = mv
  pure $! Data.ByteString.Internal.fromForeignPtr fp 0 len
