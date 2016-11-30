{-|
Module      : Data.Macaw.Memory.ElfLoader
Copyright   : (c) Galois Inc, 2016
Maintainer  : jhendrix@galois.com

Operations for creating a view of memory from an elf file.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Macaw.Memory.ElfLoader
  ( SectionIndexMap
  , readElf
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
import           Data.IntervalMap.Strict (Interval(..), IntervalMap)
import qualified Data.IntervalMap.Strict as IMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.NatRepr
import           System.IO

import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm

type SectionIndexMap v w = Map ElfSectionIndex (SegmentedAddr w, ElfSection v)

ppErrors :: FilePath -> [ElfParseError w] -> IO ()
ppErrors path errl = do
  when (not (null errl)) $ do
    hPutStrLn stderr $ "Non-fatal errors during parsing " ++ path
  forM_ errl $ \e -> do
    hPutStrLn stderr $ "  " ++ show e

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
    Elf64 e -> either fail (return . Memory64 . snd) $ memoryForElfSegments knownNat e
    Elf32 e -> either fail (return . Memory32 . snd) $ memoryForElfSegments knownNat e

slice :: Integral w => Range w -> BS.ByteString -> BS.ByteString
slice (i,c) = BS.take (fromIntegral c) . BS.drop (fromIntegral i)

-- | Create Reopt flags from elf flags.
flagsForSegmentFlags :: ElfSegmentFlags -> Perm.Flags
flagsForSegmentFlags f
    =   flagIf pf_r Perm.read
    .|. flagIf pf_w Perm.write
    .|. flagIf pf_x Perm.execute
  where flagIf :: ElfSegmentFlags -> Perm.Flags -> Perm.Flags
        flagIf ef pf | f `hasPermissions` ef = pf
                     | otherwise = Perm.none

-- | Convert elf section flags to a segment flags.
flagsForSectionFlags :: forall w
                     .  (Num w, Bits w)
                     => ElfSectionFlags w
                     -> Perm.Flags
flagsForSectionFlags f =
    Perm.read .|. flagIf shf_write Perm.write .|. flagIf shf_execinstr Perm.execute
  where flagIf :: ElfSectionFlags w -> Perm.Flags -> Perm.Flags
        flagIf ef pf = if f `hasPermissions` ef then pf else Perm.none

-- | Return a memory segment for elf segment if it loadable.
memSegmentForElfSegment :: (Integral v, MemWidth w)
                        => SegmentIndex
                        -> BS.ByteString
                           -- ^ Complete contents of Elf file.
                        -> Phdr v
                           -- ^ Program header entry
                        -> MemSegment w
memSegmentForElfSegment idx contents phdr = mseg
  where seg = phdrSegment phdr
        dta = slice (phdrFileRange phdr) contents
        sz = fromIntegral $ phdrMemSize phdr
        fixedData
          | BS.length dta > sz = BS.take sz dta
          | otherwise = dta `mappend` BS.replicate (sz - BS.length dta) 0
        addr = fromIntegral $ elfSegmentVirtAddr seg
        flags = flagsForSegmentFlags (elfSegmentFlags seg)
        mseg = memSegment idx (Just addr) flags [ByteRegion fixedData]

-- | Create memory segment from elf section.
memSegmentForElfSection :: (Integral v, Bits v, MemWidth w)
                        => SegmentIndex
                        -> ElfSection v
                        -> MemSegment w
memSegmentForElfSection idx s = mseg
  where mseg = memSegment idx (Just base) flags [ByteRegion bytes]
        base = fromIntegral (elfSectionAddr s)
        flags = flagsForSectionFlags (elfSectionFlags s)
        bytes = elfSectionData s

------------------------------------------------------------------------
-- MemLoader

data MemLoaderState v w = MLS { _mlsIndex :: !SegmentIndex
                              , _mlsMemory :: !(Memory w)
                              , _mlsIndexMap :: !(SectionIndexMap v w)
                              }

mlsIndex :: Simple Lens (MemLoaderState v w) SegmentIndex
mlsIndex = lens _mlsIndex (\s v -> s { _mlsIndex = v })

mlsMemory :: Simple Lens (MemLoaderState v w) (Memory w)
mlsMemory = lens _mlsMemory (\s v -> s { _mlsMemory = v })

mlsIndexMap :: Simple Lens (MemLoaderState v w) (SectionIndexMap v w)
mlsIndexMap = lens _mlsIndexMap (\s v -> s { _mlsIndexMap = v })

initState :: NatRepr w -> MemLoaderState v w
initState w = MLS { _mlsIndex = 0
                  , _mlsMemory = emptyMemory w
                  , _mlsIndexMap = Map.empty
                  }

memLoaderPair :: MemLoaderState v w -> (SectionIndexMap v w, Memory w)
memLoaderPair mls = (mls^.mlsIndexMap, mls^.mlsMemory)

type MemLoader v w = StateT (MemLoaderState v w) (Except String)

overMemory :: StateT (Memory w) (Except String) a
           -> MemLoader v w a
overMemory m =
  StateT $ \mls -> do
    (r,mem') <- runStateT m (mls^.mlsMemory)
    pure (r, mls & mlsMemory .~ mem')

-- | Maps file offsets to the elf section
type ElfFileSectionMap v = IntervalMap v (ElfSection v)

-- | Load an elf file into memory.
insertElfSegment :: (Integral v, MemWidth w)
                 => ElfFileSectionMap v
                 -> BS.ByteString
                 -> Phdr v
                 -> MemLoader v w ()
insertElfSegment shdrMap contents phdr = do
  case elfSegmentType (phdrSegment phdr) of
    PT_LOAD -> do
      idx <- use mlsIndex
      mlsIndex .= idx + 1
      let seg = memSegmentForElfSegment idx contents phdr
      overMemory $ insertMemSegment seg
      let phdr_offset = fromFileOffset (phdrFileStart phdr)
      let phdr_end = phdr_offset + phdrFileSize phdr
      let l = IMap.toList $ IMap.intersecting shdrMap (IntervalCO phdr_offset phdr_end)
      forM_ l $ \(i, sec) -> do
        case i of
          IntervalCO shdr_start _ -> do
            let elfIdx = ElfSectionIndex (elfSectionIndex sec)
            when (phdr_offset > shdr_start) $ do
              fail $ "Found section header that overlaps with program header."
            let sec_offset = fromIntegral $ shdr_start - phdr_offset
            let pair = (SegmentedAddr seg sec_offset, sec)
            mlsIndexMap %= Map.insert elfIdx pair
          _ -> error "Unexpected shdr interval"
    PT_DYNAMIC -> do
      throwError "Dynamic elf files are not yet supported."
    _ -> return ()

-- | Load an elf file into memory.  This uses the Elf segments for loading.
memoryForElfSegments :: forall v w
                     .  (Integral v, MemWidth w)
                     => NatRepr w
                     -> Elf v
                     -> Either String (SectionIndexMap v w, Memory w)
memoryForElfSegments w e =
  runExcept $ fmap memLoaderPair $ flip execStateT (initState w) $ do
    let l = elfLayout e
    let contents = L.toStrict (elfLayoutBytes l)
    let intervals :: ElfFileSectionMap v
        intervals = IMap.fromList $
          [ (IntervalCO start end, sec)
          | shdr <- Map.elems (l^.shdrs)
          , let start = shdr^._3
          , let sec = shdr^._1
          , let end = start + elfSectionFileSize sec
          ]
    mapM_ (insertElfSegment intervals contents) (allPhdrs l)

-- | Load an elf file into memory.
insertElfSection :: (Integral v, Bits v, MemWidth w)
                 => ElfSection v
                 -> MemLoader v w ()
insertElfSection sec =
  when (elfSectionFlags sec `hasPermissions` shf_alloc) $ do
    idx <- use mlsIndex
    mlsIndex .= idx + 1
    let seg = memSegmentForElfSection idx sec
    overMemory $ insertMemSegment seg
    let elfIdx = ElfSectionIndex (elfSectionIndex sec)
    let pair = (SegmentedAddr seg 0, sec)
    mlsIndexMap %= Map.insert elfIdx pair

-- | Load allocated Elf sections into memory.
-- Normally, Elf uses segments for loading, but the segment information
-- tends to be more precise.
memoryForElfSections :: (Integral v, Bits v, MemWidth w)
                     => NatRepr w
                     -> Elf v
                     -> Either String (SectionIndexMap v w, Memory w)
memoryForElfSections w e =
  runExcept $ fmap memLoaderPair $ flip execStateT (initState w) $ do
    traverseOf_ elfSections insertElfSection e

loadElfBySection :: FilePath -> IO SomeMemory
loadElfBySection path = do
  se <- readElf path
  case se of
    Elf64 e -> either fail (return . Memory64 . snd) $ memoryForElfSections knownNat e
    Elf32 e -> either fail (return . Memory32 . snd) $ memoryForElfSections knownNat e
