{-|
Provides function for computing new layout
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Relinker.NewLayout
  ( LayoutCtx(..)
  , NewBinaryLayout
  , nblPhdrTableOffset
  , nblShdrTableOffset
  , nblFindNewOffset
  , layoutNewBinary
  ) where

import qualified Data.Map as Map
import qualified Data.ElfEdit as Elf
import           Data.Word

import           GHC.TypeNats (Nat)
import qualified Reopt.Relinker.Binary as Orig

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
data NewBinaryLayout (w :: Nat) =
  NewBinaryLayout
  { nblPhdrTableOffset :: !(Maybe (Elf.FileOffset (Elf.ElfWordType w)))
  , nblShdrTableOffset :: !(Maybe (Elf.FileOffset (Elf.ElfWordType w)))
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
layoutNewBinary :: LayoutCtx w
                -> Orig.ElfContentLayout w
                -> Either String (NewBinaryLayout w)
layoutNewBinary ctx l = do
  let s = NewBinaryLayout { nblPhdrTableOffset = Nothing
                          , nblShdrTableOffset = Nothing
                          , nblFileStartMap   = Map.empty
                          }
  layoutNewBinary' ctx 0 s (Orig.eclFileRegions l)
