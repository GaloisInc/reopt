{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Relinker.NewBinary
  ( -- * Layout
    LayoutCtx(..)
  , NewBinaryLayout(..)
  , nblFindNewOffset
  , layoutNewBinary
    -- * Building
  , BuildCtx(..)
  , buildNewBinary
  ) where

import           Control.Monad.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.Map as Map
import qualified Data.ElfEdit as Elf
import           Data.Word

import           GHC.TypeNats (Nat)
import qualified Reopt.Relinker.Binary as Orig
import Reopt.Relinker.Constants ( pageSize )

nextMultiple :: Int -> Int -> Int
nextMultiple off n = n * ((off + (n-1)) `div` n)

-- | Compute next offset given alignment constraint.
inferNextOff :: Int -> Orig.OffsetConstraint -> Int
inferNextOff off (Orig.FileOffsetMultiple n) = nextMultiple off n
inferNextOff off (Orig.LoadSegmentConstraint addr) = off + diff
  where mask = pageSize - 1
        diff = (pageSize + (addr .&. mask) - (off .&. mask)) .&. mask

-------------------------------------------------------------------------------
-- Layout

data LayoutCtx w = LayoutCtx
  { lctxClass :: !(Elf.ElfClass w)
  , lctxPhdrCount :: !Word16
  , lctxShdrCount :: !Word16
  , lctxShstrtabSize :: !Int
  , lctxStrtabSize :: !Int
  , lctxSymtabSize :: !Int
  , lctxOverflowInsertOffset :: !Int
  , lctxOverflowSize :: !Int
  }

-- | Layout information in new binary
data NewBinaryLayout (w :: Nat) =
  NewBinaryLayout { nblPhdrTableOffset :: !(Maybe (Elf.FileOffset (Elf.ElfWordType w)))
                  , nblShdrTableOffset :: !(Maybe (Elf.FileOffset (Elf.ElfWordType w)))
--                  , nblShstrtabOffset :: !(Maybe  (Elf.FileOffset (Elf.ElfWordType w)))
--                  , nblSymtabOffset :: !(Maybe  (Elf.FileOffset (Elf.ElfWordType w)))
--                  , nblStrtabOffset :: !(Maybe  (Elf.FileOffset (Elf.ElfWordType w)))
                  , nblFileStartMap :: !(Map.Map Int Int)
                    -- ^ Sparse map from file offsets for start of region in original file to their corresponding new
                    -- offset in new binary
                  }

-- | Map file offset in binary to new offset.
nblFindNewOffset :: Integral (Elf.ElfWordType w)
                 => NewBinaryLayout w
                 -> Elf.FileOffset (Elf.ElfWordType w)
                 -> Elf.FileOffset (Elf.ElfWordType w)
nblFindNewOffset l (Elf.FileOffset o) =
  case Map.lookupLE (fromIntegral o) (nblFileStartMap l) of
    Nothing -> Elf.FileOffset o
    Just (_, d) -> Elf.FileOffset (o + fromIntegral d)


-- | Update file start map with information to record original to new mapping
insertLayoutFileDelta :: Int -> Int -> NewBinaryLayout w -> NewBinaryLayout w
insertLayoutFileDelta origOff newOff l = do
  let m = nblFileStartMap l
      lastDelta = maybe 0 snd (Map.lookupMax m)
      curDelta = newOff - origOff
      l' = l { nblFileStartMap = Map.insert origOff curDelta m }
   in if curDelta == lastDelta then l else l'

layoutNewBinary' :: LayoutCtx w
                 -> Int -- ^ File offset in new binary
                 -> NewBinaryLayout w
                 -> [Orig.ElfRegion] -- ^ Remaining regions
                 -> Either String (NewBinaryLayout w)
layoutNewBinary' _ _off l [] = pure l
layoutNewBinary' ctx off l0 (Orig.ElfRegion binOff binSize cns src:rest) = do
  let cl = lctxClass ctx
  Elf.elfClassInstances (lctxClass ctx) $ do
    -- Compute next offset
    let off' = inferNextOff off cns
    -- Add layout delta information
    let l = insertLayoutFileDelta binOff off' l0
    -- Infer new layout for souce region and size.
    (l', contentsSize) <-
      case src of
        Orig.SrcSpecialRegion reg ->
          case reg of
            Orig.Ehdr -> do
              pure (l, fromIntegral (Elf.ehdrSize cl))
            Orig.PhdrTable -> do
              let sz = fromIntegral (lctxPhdrCount ctx) * fromIntegral (Elf.phdrEntrySize cl)
              case nblPhdrTableOffset l of
                Nothing -> pure ()
                Just _ -> Left "Program header table already defined."
              let l' = l { nblPhdrTableOffset = Just (Elf.FileOffset (fromIntegral off')) }
              pure (l', sz)
            Orig.ShdrTable -> do
              let sz = fromIntegral (lctxShdrCount ctx) * fromIntegral (Elf.shdrEntrySize cl)
              case nblShdrTableOffset l of
                Nothing -> pure ()
                Just _ -> Left "Section header table already defined."
              let l' = l { nblShdrTableOffset = Just (Elf.FileOffset (fromIntegral off')) }
              pure (l', sz)
            Orig.Interpreter -> do
              pure (l, binSize)
            Orig.Shstrtab -> do
              pure (l, lctxShstrtabSize ctx)
            Orig.Strtab -> do
              pure (l, lctxStrtabSize ctx)
            Orig.Symtab -> do
              pure (l, lctxSymtabSize ctx)
        Orig.Code _ ->
          pure (l, binSize)
        Orig.Data ->
          pure (l, binSize)
    let overflowSize
          | binOff + binSize == lctxOverflowInsertOffset ctx = lctxOverflowSize ctx
          | otherwise = 0
    let nextOff = off' + contentsSize + overflowSize
    layoutNewBinary' ctx nextOff l' rest

-- | Generate new binary layout.
layoutNewBinary :: LayoutCtx w
                -> Orig.ElfContentLayout
                -> Either String (NewBinaryLayout w)
layoutNewBinary ctx l = do
  let s = NewBinaryLayout { nblPhdrTableOffset = Nothing
                          , nblShdrTableOffset = Nothing
                          , nblFileStartMap   = Map.empty
                          }
  layoutNewBinary' ctx 0 s (Orig.eclFileRegions l)

-------------------------------------------------------------------------------
-- Build

-- Informaton for constructing new binary
data BuildCtx w = BuildCtx
  { bctxHeaderInfo :: !(Elf.ElfHeaderInfo w)
  , bctxEhdr       :: !(Elf.Ehdr w)
  , bctxExtendedSegmentMap :: !(Map.Map Word16 (Elf.ElfWordType w))
    -- ^ Maps section indices that change to the new size.
  , bctxShdrCount  :: !Word16
  , bctxShdrTable  :: !Bld.Builder
  , bctxShstrtab   :: !BS.ByteString
  , bctxStrtab     :: !BS.ByteString
  , bctxSymtab     :: !Bld.Builder
  , bctxSymtabSize :: !Int
  , bctxCodeMap :: !(Word64 -> Int -> Int -> Except String Bld.Builder)
  , bctxFileOffsetFn :: !(Elf.FileOffset (Elf.ElfWordType w) -> Elf.FileOffset (Elf.ElfWordType w))
    -- ^ Maps file offsets in original binary to code in new binary.
  , bctxOverflowInsertOffset :: !Int
  , bctxOverflowContentsAndSize :: !(Bld.Builder, Int)
  }

-----------------------------------------------------------------------
-- Make new program header table.

-- | Create program heade table.
mkNewPhdrTable :: forall w
               .  BuildCtx w
               -> Bld.Builder
mkNewPhdrTable ctx = gen 0
  where binHeaderInfo = bctxHeaderInfo ctx
        hdr = Elf.header binHeaderInfo
        cl = Elf.headerClass hdr
        elfDta = Elf.headerData hdr

        indexSizeMap = bctxExtendedSegmentMap ctx

        mkPhdr :: Word16 -> Elf.Phdr w
        mkPhdr idx = Elf.elfClassInstances cl $ do
          let phdr0 = Elf.phdrByIndex binHeaderInfo idx
          let off' =  bctxFileOffsetFn ctx (Elf.phdrFileStart phdr0)
          let phdr1 = phdr0 { Elf.phdrFileStart = off' }
          case Map.lookup idx indexSizeMap of
            Nothing -> phdr1
            Just sz -> phdr1 { Elf.phdrFileSize = sz, Elf.phdrMemSize = sz }

        gen i | i >= Elf.phdrCount binHeaderInfo = mempty
              | otherwise = Elf.encodePhdr cl elfDta (mkPhdr i) <> gen (i+1)

-----------------------------------------------------------------------
-- Make new section header header table.

-----------------------------------------------------------------------
-- Make new binary

buildNewBinary' :: BuildCtx w
                 -> Int -- ^ FileOffset
                 -> [Orig.ElfRegion] -- ^ Remaining regions
                 -> Bld.Builder
buildNewBinary' _ctx _off [] = mempty
buildNewBinary' ctx off (Orig.ElfRegion binOff binSize cns src:rest) = do
  let binInfo = bctxHeaderInfo ctx
  let binContents = Elf.headerFileContents binInfo
  let cl = Elf.headerClass (Elf.header binInfo)
  -- Compute next offset
  let off' = inferNextOff off cns
  -- Get padding
  let padding = Bld.byteString (BS.replicate (off' - off) 0)
  let (contents, contentsSize) =
        case src of
          Orig.SrcSpecialRegion reg ->
            case reg of
              Orig.Ehdr -> (Elf.encodeEhdr (bctxEhdr ctx), fromIntegral (Elf.ehdrSize cl))
              Orig.PhdrTable ->
                let cnt = Elf.phdrCount binInfo
                    sz = fromIntegral cnt * fromIntegral (Elf.phdrEntrySize cl)
                 in (mkNewPhdrTable ctx, sz)
              Orig.ShdrTable ->
                 let cnt = bctxShdrCount ctx
                     sz = fromIntegral cnt * fromIntegral (Elf.shdrEntrySize cl)
                  in (bctxShdrTable ctx, sz)
              Orig.Interpreter -> do
                let icontents = BS.take binSize $ BS.drop binOff binContents
                 in (Bld.byteString icontents, binSize)
              Orig.Shstrtab ->
                let s = bctxShstrtab ctx
                 in (Bld.byteString s, BS.length s)
              Orig.Strtab ->
                let s = bctxStrtab ctx
                 in (Bld.byteString s, BS.length s)
              Orig.Symtab -> (bctxSymtab ctx, bctxSymtabSize ctx)
          Orig.Code addr ->
            case runExcept $ bctxCodeMap ctx addr binOff binSize of
              Left msg -> error msg
              Right b -> (b, binSize)
          Orig.Data ->
            let origContents = BS.take binSize $ BS.drop binOff binContents
             in (Bld.byteString origContents, binSize)
  let (overflow, overflowSize)
        | binOff + binSize == bctxOverflowInsertOffset ctx =
          bctxOverflowContentsAndSize ctx
        | otherwise =
          (mempty, 0)
  let nextOff = off' + contentsSize + overflowSize
  padding <> contents <> overflow <> buildNewBinary' ctx nextOff rest

-- | Genertate new binary layout.
buildNewBinary :: BuildCtx w -> Orig.ElfContentLayout -> Bld.Builder
buildNewBinary ctx l = buildNewBinary' ctx 0 (Orig.eclFileRegions l)