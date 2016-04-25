{-# LANGUAGE FlexibleContexts #-}
module Reopt.Object.Loader
  ( readElf
  , loadExecutable
  , loadElfBySection
  , memoryForElfSegments
  , memoryForElfSections
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import           Data.Elf

import           Reopt.Object.Memory

readElf :: FilePath -> IO (SomeElf Elf)
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

slice :: Integral w => Range w -> BS.ByteString -> BS.ByteString
slice (i,c) = BS.take (fromIntegral c) . BS.drop (fromIntegral i)

-- | Load an elf file into memory.  This uses the Elf segments for loading.
memoryForElfSegments :: (ElfWidth w, Monad m) => Elf w -> m (Memory w)
memoryForElfSegments e =
    execStateT (mapM_ (insertElfSegment contents) phdrs) emptyMemory
  where l = elfLayout e
        contents = L.toStrict (elfLayoutBytes l)
        phdrs    = allPhdrs l

-- | Load an elf file into memory.
insertElfSegment :: (ElfWidth w, MonadState (Memory w) m)
                 => BS.ByteString
                 -> Phdr w
                 -> m ()
insertElfSegment contents phdr = do
  case elfSegmentType (phdrSegment phdr) of
    PT_LOAD -> do
      insertMemSegment (memSegmentForElfSegment contents phdr)
    PT_DYNAMIC -> do
      fail "Dynamic elf files are not yet supported."
    _ -> return ()

-- | Return a memory segment for elf segment if it loadable.
memSegmentForElfSegment :: Integral w
                        => BS.ByteString
                           -- ^ Complete contents of Elf file.
                        -> Phdr w
                        -> MemSegment w
memSegmentForElfSegment contents phdr = mseg
  where seg = phdrSegment phdr
        dta = slice (phdrFileRange phdr) contents
        sz = fromIntegral $ phdrMemSize phdr
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
