{-# LANGUAGE FlexibleContexts #-}
module Reopt.Object.Loader
  ( readElf
  , loadExecutable
  , loadElfBySection

  , memoryForElfSegments
  , memoryForElfSections
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import           Data.ElfEdit
import           System.IO

import           Reopt.Object.Memory

ppErrors :: FilePath -> [ElfParseError w] -> IO ()
ppErrors path errl = do
  when (not (null errl)) $ do
    hPutStrLn stderr $ "Non-fatal errors during parsing " ++ path
  forM_ errl $ \e -> do
    hPutStrLn stderr $ "  " ++ show errl

-- | This reads the elf file from the given path.
--
-- As a side effect it may print warnings for errors encountered during parsing
-- to stderr.
readElf :: FilePath -> IO (SomeElf Elf)
readElf path = do
  bs <- BS.readFile path
  case parseElf bs of
    ElfHeaderError _ msg -> do
      fail $ "Could not parse Elf header: " ++ msg
    Elf32Res errl e -> do
      ppErrors path errl
      return (Elf32 e)
    Elf64Res errl e -> do
      ppErrors path errl
      return (Elf64 e)

loadExecutable :: FilePath -> IO SomeMemory
loadExecutable path = do
  se <- readElf path
  case se of
    Elf64 e -> either fail (return . Memory64) $ memoryForElfSegments e
    Elf32 e -> either fail (return . Memory32) $ memoryForElfSegments e

slice :: Integral w => Range w -> BS.ByteString -> BS.ByteString
slice (i,c) = BS.take (fromIntegral c) . BS.drop (fromIntegral i)

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

type MemLoader w = StateT (Memory w) (Except String)

-- | Load an elf file into memory.
insertElfSegment :: Integral w
                 => BS.ByteString
                 -> Phdr w
                 -> MemLoader w ()
insertElfSegment contents phdr = do
  case elfSegmentType (phdrSegment phdr) of
    PT_LOAD -> do
      insertMemSegment (memSegmentForElfSegment contents phdr)
    PT_DYNAMIC -> do
      throwError "Dynamic elf files are not yet supported."
    _ -> return ()

-- | Load an elf file into memory.  This uses the Elf segments for loading.
memoryForElfSegments :: Integral w => Elf w -> Either String (Memory w)
memoryForElfSegments e =
    runExcept $ execStateT (mapM_ (insertElfSegment contents) phdrs) emptyMemory
  where l = elfLayout e
        contents = L.toStrict (elfLayoutBytes l)
        phdrs    = allPhdrs l

loadElfBySection :: FilePath -> IO SomeMemory
loadElfBySection path = do
  se <- readElf path
  case se of
    Elf64 e -> either fail (return . Memory64) $ memoryForElfSections e
    Elf32 e -> either fail (return . Memory32) $ memoryForElfSections e

-- | Load an elf file into memory.
insertElfSection :: (Bits w, Num w, Ord w)
                 => ElfSection w
                 -> MemLoader w ()
insertElfSection s =
  when (elfSectionFlags s `hasPermissions` shf_alloc) $ do
    insertMemSegment (memSegmentForElfSection s)

-- | Load allocated Elf sections into memory.
-- Normally, Elf uses segments for loading, but the segment information
-- tends to be more precise.
memoryForElfSections :: (Bits w, Num w, Ord w) => Elf w -> Either String (Memory w)
memoryForElfSections e = runExcept $ flip execStateT emptyMemory $ do
  traverseOf_ elfSections insertElfSection e


-- | Convert elf section flags to a segment flags.
flagsForSectionFlags :: (Bits w, Num w)
                     => ElfSectionFlags w
                     -> ElfSegmentFlags
flagsForSectionFlags f = pf_r .|. write_flag .|. exec_flag
  where can_write = f `hasPermissions` shf_write
        write_flag | can_write = pf_w
                   | otherwise = pf_none
        can_exec = f `hasPermissions` shf_execinstr
        exec_flag | can_exec  = pf_x
                  | otherwise = pf_none


-- | Create memory segment from elf section.
memSegmentForElfSection :: (Bits w, Num w) => ElfSection w -> MemSegment w
memSegmentForElfSection s = mseg
  where mseg = MemSegment { memBase  = elfSectionAddr s
                          , memFlags = flagsForSectionFlags (elfSectionFlags s)
                          , memBytes = elfSectionData s
                          }
