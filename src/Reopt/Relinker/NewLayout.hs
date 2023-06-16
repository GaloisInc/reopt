-- | Provides function for computing new layout
module Reopt.Relinker.NewLayout (
  LayoutCtx (..),
  NewBinaryLayout,
  nblFindNewOffset,
  layoutNewBinary,
) where

import Data.ElfEdit qualified as Elf
import Data.Map qualified as Map
import Data.Word (Word16)

import GHC.TypeNats (Nat)
import Reopt.Relinker.Binary qualified as Orig

-------------------------------------------------------------------------------
-- Layout

data LayoutCtx w = LayoutCtx
  { lctxClass :: !(Elf.ElfClass w)
  , lctxPhdrCount :: !Word16
  , lctxShdrCount :: !Word16
  , lctxShstrtabSize :: !Int
  , lctxStrtabSize :: !Int
  -- ^ Size of string table if defined and -1 if not
  , lctxSymtabSize :: !Int
  -- ^ Size of symbol table if defined and -1 if not.
  , lctxAppendMap :: !(Map.Map Int Int)
  -- ^ Map end of region file offsets to number of bytes to insert.
  }

-- | Layout information in new binary
newtype NewBinaryLayout (w :: Nat) = NewBinaryLayout
  { nblFileStartMap :: Map.Map Int Int
  -- ^ Sparse map from file offsets for start of region in original file
  -- to their corresponding newoffset in new binary
  }

-- | Map file offset in binary to new offset.
nblFindNewOffset ::
  Integral (Elf.ElfWordType w) =>
  NewBinaryLayout w ->
  Elf.FileOffset (Elf.ElfWordType w) ->
  Elf.FileOffset (Elf.ElfWordType w)
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
      l' = l{nblFileStartMap = Map.insert origOff curDelta m}
   in if curDelta == lastDelta then l else l'

layoutNewBinary' ::
  LayoutCtx w ->
  -- | File offset in new binary
  Int ->
  NewBinaryLayout w ->
  -- | Remaining regions
  [Orig.ElfRegion] ->
  Either String (NewBinaryLayout w)
layoutNewBinary' _ _off l [] = pure l
layoutNewBinary' ctx off l0 (Orig.ElfRegion binOff binSize cns src : rest) = do
  let cl = lctxClass ctx
  Elf.elfClassInstances (lctxClass ctx) $ do
    -- Compute next offset
    let off' = Orig.inferNextOff off cns
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
              pure (l, sz)
            Orig.ShdrTable -> do
              let sz = fromIntegral (lctxShdrCount ctx) * fromIntegral (Elf.shdrEntrySize cl)
              pure (l, sz)
            Orig.Interpreter -> do
              pure (l, binSize)
            Orig.Shstrtab -> do
              pure (l, lctxShstrtabSize ctx)
            Orig.Strtab
              | lctxStrtabSize ctx < 0 -> error "internal: Unexpected strtab"
              | otherwise ->
                  pure (l, lctxStrtabSize ctx)
            Orig.Symtab
              | lctxSymtabSize ctx < 0 -> error "internal: Unexpected symtab"
              | otherwise ->
                  pure (l, lctxSymtabSize ctx)
        Orig.Code _ ->
          pure (l, binSize)
        Orig.Data ->
          pure (l, binSize)
    let overflowSize = Map.findWithDefault 0 (binOff + binSize) (lctxAppendMap ctx)
    let nextOff = off' + contentsSize + overflowSize
    layoutNewBinary' ctx nextOff l' rest

-- | Generate new binary layout.
layoutNewBinary ::
  LayoutCtx w ->
  Orig.ElfContentLayout w ->
  Either String (NewBinaryLayout w)
layoutNewBinary ctx l = do
  let s = NewBinaryLayout{nblFileStartMap = Map.empty}
  layoutNewBinary' ctx 0 s (Orig.eclFileRegions l)
