{-# LANGUAGE FlexibleContexts #-}
module Reopt.Object.Loader
  ( readElf
  , loadExecutable
  , loadElfBySection
  , memoryForElfSegments
  , memoryForElfSections
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import Data.Elf
import Data.Monoid (mappend)

import Reopt.Object.Memory

readElf :: FilePath -> IO SomeElf
readElf path = do
  bs <- BS.readFile path
  case parseElf bs of
    Left (_,msg) -> fail $ "Parse error: " ++ msg
    Right e -> return e

loadExecutable :: FilePath -> IO SomeMemory
loadExecutable path = do
  se <- readElf path
  case se of
    Elf64 e -> Memory64 <$> memoryForElfSegments e
    Elf32 e -> Memory32 <$> memoryForElfSegments e

-- | Load an elf file into memory.  This uses the Elf segments for loading.
memoryForElfSegments :: (ElfWidth w, Monad m) => Elf w -> m (Memory w)
memoryForElfSegments e = execStateT (mapM_ insertElfSegment (renderedElfSegments e)) emptyMemory

-- | Load an elf file into memory.
insertElfSegment :: (ElfWidth w, MonadState (Memory w) m) => RenderedElfSegment w -> m ()
insertElfSegment s@(elf_seg, _) = do
  -- Load PT_LOAD elefSegments
  case elfSegmentType elf_seg of
    PT_LOAD -> do
      insertMemSegment (memSegmentForElfSegment s)
    PT_DYNAMIC -> do
      fail "Dynamic elf files are not yet supported."
    _ -> return ()

-- | Return a memory segment for elf segment if it loadable.
memSegmentForElfSegment :: Integral w
                        => RenderedElfSegment w
                        -> MemSegment w
memSegmentForElfSegment s@(seg, dta) = mseg
  where -- dta = seg^.elfSegmentData
        sz = fromIntegral $ elfSegmentMemSize seg
        fixedData
          | BS.length dta > sz = BS.take sz dta
          | otherwise = dta `mappend` BS.replicate (sz - BS.length dta) 0
        mseg = MemSegment { memBase  = elfSegmentVirtAddr seg
                          , memFlags = elfSegmentFlags seg
                          , memBytes = fixedData
                          }

loadElfBySection :: FilePath -> IO SomeMemory
loadElfBySection path = do
  se <- readElf path
  case se of
    Elf64 e -> Memory64 <$> memoryForElfSections e
    Elf32 e -> Memory32 <$> memoryForElfSections e


-- | Load allocated Elf sections into memory.
-- Normally, Elf uses segments for loading, but the segment information
-- tends to be more precise.
memoryForElfSections :: (ElfWidth w, Functor m, Monad m) => Elf w -> m (Memory w)
memoryForElfSections e = flip execStateT emptyMemory $ do
  traverseOf_ elfSections insertElfSection e

-- | Load an elf file into memory.
insertElfSection :: (ElfWidth w, MonadState (Memory w) m) => ElfSection w -> m ()
insertElfSection s =
  when (elfSectionFlags s `hasPermissions` shf_alloc) $ do
    insertMemSegment (memSegmentForElfSection s)

-- | Convert elf section flags to a segment flags.
flagsForSectionFlags :: ElfWidth w => ElfSectionFlags w -> ElfSegmentFlags
flagsForSectionFlags f = pf_r .|. write_flag .|. exec_flag
  where can_write = f `hasPermissions` shf_write
        write_flag | can_write = pf_w
                   | otherwise = pf_none
        can_exec = f `hasPermissions` shf_execinstr
        exec_flag | can_exec  = pf_x
                  | otherwise = pf_none


-- | Create memory segment from elf section.
memSegmentForElfSection :: ElfWidth w => ElfSection w -> MemSegment w
memSegmentForElfSection s = mseg
  where mseg = MemSegment { memBase  = elfSectionAddr s
                          , memFlags = flagsForSectionFlags (elfSectionFlags s)
                          , memBytes = elfSectionData s
                          }
