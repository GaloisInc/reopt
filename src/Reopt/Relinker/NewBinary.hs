{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Relinker.NewBinary
  ( -- * Building
    BuildCtx(..)
  , buildNewBinary
    -- * Section
  , NewSectionIndex(..)
  , mkNewShdrTable
  ) where

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.Map as Map
import qualified Data.ElfEdit as Elf
import qualified Data.Vector as V
import           Data.Word

import qualified Reopt.Relinker.Binary as Orig
import           Reopt.Relinker.Constants
import           Reopt.Relinker.NewSymtab

-------------------------------------------------------------------------------
-- Build

-- Informaton for constructing new binary
data BuildCtx w = BuildCtx
  { bctxHeaderInfo :: !(Elf.ElfHeaderInfo w)
  , bctxFileOffsetFn :: !(Elf.FileOffset (Elf.ElfWordType w) -> Elf.FileOffset (Elf.ElfWordType w))
    -- ^ Maps file offsets in original binary to code in new binary.
  , bctxExtendedSegmentMap :: !(Map.Map Word16 (Elf.ElfWordType w))
    -- ^ Maps segment indices that change to the new size.

  , bctxPhdrTableOffset :: !(Elf.FileOffset (Elf.ElfWordType  w))
    -- ^ File offset for program header table.

  , bctxBinShdrs    :: !(V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w)))
    -- ^ Section headers in original binary
  , bctxSectionNameMap :: !(BS.ByteString -> Word32)
    -- ^ Maps section names to their offset.
  , bctxBinSectionIndexMap :: !(Word16 -> NewSectionIndex)

    -- ^ Map section header indices in original binary to indices in
    -- new binary.
  , bctxShdrTableOffset :: !(Elf.FileOffset (Elf.ElfWordType  w))
    -- ^ File offset to use for new section header table.
  , bctxShdrStrndx :: !Word16
    -- ^ Index of section header string table.

  , bctxCodeMap :: !(Word64 -> Int -> Int -> Either String Bld.Builder)
  , bctxOverflowCodeShdrIndex :: !Word16
    -- ^ Index to insert overflow code into.
  , bctxOverflowAddr :: !(Elf.ElfWordType  w)
    -- ^ Address of new code section (relative to base in dynamically linked files)
  , bctxOverflowSize    :: !(Elf.ElfWordType  w)
    -- ^ Code size
  , bctxOverflowInsertOffset :: !(Elf.FileOffset (Elf.ElfWordType w))
  , bctxAppendMap :: !(Map.Map (Elf.FileOffset (Elf.ElfWordType w)) [(Elf.ElfWordType w, Bld.Builder)])
    -- ^ Map end of region file offsets to number of bytes to insert and contents.
  , bctxShstrtab   :: !BS.ByteString
  , bctxNewSymtab  :: !(Maybe NewSymtab)
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

--------------------------------------------------------------------------------
-- New Section header generation

-- | Function that maps old section header link to new section header link.
bctxMapShdrLink :: BuildCtx w -> Word32 -> Word32
bctxMapShdrLink ctx link = do
  -- If link is a current section index, then we treat it as a section index and
  -- remap it.
  if 1 <= link && link < fromIntegral (Elf.shdrCount (bctxHeaderInfo ctx)) then
    let link' = fromIntegral link
     in fromIntegral $ fromNewSectionIndex $ bctxBinSectionIndexMap ctx link'
   else
    link

isSymtab :: Elf.Shdr BS.ByteString v -> Bool
isSymtab shdr = Elf.shdrName shdr == ".symtab"

isStrtab :: Elf.Shdr BS.ByteString v -> Bool
isStrtab shdr = Elf.shdrName shdr == ".strtab"

isShstrtab :: Elf.Shdr BS.ByteString v -> Bool
isShstrtab shdr = Elf.shdrName shdr == ".shstrtab"

mkNewShdr :: Num (Elf.ElfWordType w)
          => BuildCtx w
          -> Elf.Shdr BS.ByteString (Elf.ElfWordType w)
          -> Elf.Shdr Word32 (Elf.ElfWordType w)
mkNewShdr ctx shdr = do
  let info
        | Elf.shdrType shdr == Elf.SHT_RELA = bctxMapShdrLink  ctx (Elf.shdrInfo shdr)
        | isSymtab shdr =
          case bctxNewSymtab ctx of
            Nothing -> error "internal: Unexpected symtab"
            Just symtab -> newSymtabLocalCount symtab
        | otherwise = Elf.shdrInfo shdr
  let size
        | isStrtab shdr   = fromIntegral (BS.length (bctxShstrtab ctx))
        | isSymtab shdr   =
          case bctxNewSymtab ctx of
            Nothing -> error "internal: Unexpected symtab"
            Just symtab -> fromIntegral (newSymtabSize symtab)
        | isShstrtab shdr = fromIntegral (BS.length (bctxShstrtab ctx))
        | otherwise = Elf.shdrSize shdr
  shdr { Elf.shdrName = bctxSectionNameMap ctx (Elf.shdrName shdr)
       , Elf.shdrOff  = bctxFileOffsetFn ctx (Elf.shdrOff shdr)
       , Elf.shdrSize = size
       , Elf.shdrLink = bctxMapShdrLink  ctx (Elf.shdrLink shdr)
       , Elf.shdrInfo = info
       }

newBinaryCodeSection :: (Bits v, Num v)
                     => nm -- ^ Name of section
                     -> v -- ^ Address
                     -> Elf.FileOffset v
                     -> v
                     -> Elf.Shdr nm v
newBinaryCodeSection nm addr off sz =
  Elf.Shdr { Elf.shdrName  = nm
           , Elf.shdrType  = Elf.SHT_PROGBITS
           , Elf.shdrFlags = Elf.shf_alloc .|. Elf.shf_execinstr
           , Elf.shdrAddr  = addr
           , Elf.shdrOff   = Elf.alignFileOffset 16 off
           , Elf.shdrSize  = sz
           , Elf.shdrLink  = 0
           , Elf.shdrInfo  = 0
           , Elf.shdrAddrAlign = 16
           , Elf.shdrEntSize = 0
           }

mkNewShdrTable :: forall w
               .  BuildCtx w
               -> Bld.Builder
mkNewShdrTable ctx =
  let hdr =  Elf.header $ bctxHeaderInfo ctx
      cl = Elf.headerClass hdr
      elfDta = Elf.headerData hdr
      shdrs = bctxBinShdrs ctx

      renderShdr :: Int -> Elf.Shdr BS.ByteString (Elf.ElfWordType  w) -> Bld.Builder
      renderShdr i shdr = Elf.elfClassInstances cl $
        let newShdr | fromIntegral i == bctxOverflowCodeShdrIndex ctx =
                      Elf.encodeShdr cl elfDta $
                        newBinaryCodeSection
                          (bctxSectionNameMap ctx newCodeSectionName)
                          (bctxOverflowAddr ctx)
                          (bctxOverflowInsertOffset ctx)
                          (bctxOverflowSize ctx)
                    | otherwise =
                      mempty
         in newShdr <> Elf.encodeShdr cl elfDta (mkNewShdr ctx shdr)
   in foldMap id $ V.imap renderShdr shdrs

-----------------------------------------------------------------------
-- Make new binary

buildNewBinary' :: forall w
                .  BuildCtx w
                -> Int -- ^ FileOffset
                -> [Orig.ElfRegion] -- ^ Remaining regions
                -> Bld.Builder
buildNewBinary' _ctx _off [] = mempty
buildNewBinary' ctx off (Orig.ElfRegion binOff binSize cns src:rest) = do
  let binInfo = bctxHeaderInfo ctx
  let binHdr = Elf.header binInfo
  let binContents = Elf.headerFileContents binInfo
  let cl = Elf.headerClass binHdr
  Elf.elfClassInstances cl $ do
    -- Compute next offset
    let off' = Orig.inferNextOff off cns
    -- Get padding
    let padding = Bld.byteString (BS.replicate (off' - off) 0)
    let (contents, contentsSize) =
          case src of
            Orig.SrcSpecialRegion reg ->
              case reg of
                Orig.Ehdr -> do
                  -- Elf header.
                  let ehdr = Elf.Ehdr { Elf.ehdrHeader   = Elf.header binInfo
                                      , Elf.ehdrPhoff    = bctxPhdrTableOffset ctx
                                      , Elf.ehdrShoff    = bctxShdrTableOffset ctx
                                      , Elf.ehdrPhnum    = Elf.phdrCount binInfo
                                      , Elf.ehdrShnum    = Elf.shdrCount binInfo + 1
                                      , Elf.ehdrShstrndx = bctxShdrStrndx ctx
                                      }
                  (Elf.encodeEhdr ehdr, fromIntegral (Elf.ehdrSize cl))
                Orig.PhdrTable ->
                  let cnt = Elf.phdrCount binInfo
                      sz = fromIntegral cnt * fromIntegral (Elf.phdrEntrySize cl)
                  in (mkNewPhdrTable ctx, sz)
                Orig.ShdrTable ->
                  let cnt = Elf.shdrCount binInfo + 1
                      sz = fromIntegral cnt * fromIntegral (Elf.shdrEntrySize cl)
                      tbl = mkNewShdrTable ctx
                    in (tbl, sz)
                Orig.Interpreter -> do
                  let icontents = BS.take binSize $ BS.drop binOff binContents
                   in (Bld.byteString icontents, binSize)
                Orig.Shstrtab ->
                  let s = bctxShstrtab ctx
                   in (Bld.byteString s, BS.length s)
                Orig.Strtab ->
                  case bctxNewSymtab ctx of
                    Nothing -> error "internal: Unexpected strtab"
                    Just symtab ->
                      let s = newStrtabContents symtab
                       in (Bld.byteString s, BS.length s)
                Orig.Symtab ->
                  case bctxNewSymtab ctx of
                    Nothing -> error "internal: Unexpected symtab"
                    Just symtab ->
                      ( newSymtabContents symtab
                      , fromIntegral (newSymtabSize symtab)
                      )
            Orig.Code addr ->
              case bctxCodeMap ctx addr binOff binSize of
                Left msg -> error msg
                Right b -> (b, binSize)
            Orig.Data ->
              let origContents = BS.take binSize $ BS.drop binOff binContents
              in (Bld.byteString origContents, binSize)
    let binEnd :: Int
        binEnd = binOff + binSize
    let (overflow, overflowSize) = do
          let appends = Map.findWithDefault [] (fromIntegral binEnd) (bctxAppendMap ctx)
          let go prevBuffer prevCnt [] = (prevBuffer, prevCnt)
              go prevBuffer prevCnt ((sz,nextContents):r) = do
                let endOff :: Elf.FileOffset  (Elf.ElfWordType w)
                    endOff = fromIntegral (binEnd + prevCnt)
                let insertOffset = Elf.alignFileOffset 16 endOff
                let paddingCnt = fromIntegral $ Elf.fromFileOffset insertOffset - Elf.fromFileOffset endOff
                let overflowPadding = Bld.byteString (BS.replicate paddingCnt 0)
                let cnt :: Int
                    cnt = paddingCnt + fromIntegral sz
                go (prevBuffer <> overflowPadding <> nextContents) (prevCnt + cnt) r
          go mempty 0 appends
    let nextOff = off' + contentsSize + overflowSize
    padding <> contents <> overflow <> buildNewBinary' ctx nextOff rest

-- | Genertate new binary layout.
buildNewBinary :: BuildCtx w -> Orig.ElfContentLayout w -> Bld.Builder
buildNewBinary ctx l = buildNewBinary' ctx 0 (Orig.eclFileRegions l)