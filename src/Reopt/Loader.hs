{-# LANGUAGE FlexibleContexts #-}
module Reopt.Loader
  ( loadExecutable
  , loadElf
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Elf
import qualified Data.Foldable as Fold
import Data.Monoid (mappend)

import Reopt.Memory

loadExecutable :: FilePath -> IO SomeMemory
loadExecutable path = do
  bs <- BS.readFile path
  case parseElf bs of
    Left (_,msg) -> fail $ "Parse error: " ++ msg
    Right (Elf64 e) -> Memory64 <$> loadElf e
    Right (Elf32 e) -> Memory32 <$> loadElf e

-- | Load an elf file into memory.
loadElf :: (ElfWidth w, Monad m) => Elf w -> m (Memory w)
loadElf e = execStateT (insertElf e) emptyMemory

-- | Load an elf file into memory.
insertElf :: (ElfWidth w, MonadState (Memory w) m) => Elf w -> m ()
insertElf e = do
  forM_ (renderedElfSegments e) $ \elf_seg -> do
    mseg <- memSegment elf_seg
    Fold.mapM_ insertMemSegment mseg

-- | Return a memory segment for elf segment if it loadable.
memSegment :: (Monad m, Integral w)
           => RenderedElfSegment w
           -> m (Maybe (MemSegment w))
memSegment seg
    | tp == PT_LOAD = return (Just mseg)
    | tp == PT_DYNAMIC = fail "Dynamic elf files are not yet supported."
    | otherwise = return Nothing
  where tp = elfSegmentType seg
        dta = seg^.elfSegmentData
        sz = fromIntegral $ elfSegmentMemSize seg
        fixedData
          | BS.length dta > sz = BS.take sz dta
          | otherwise = dta `mappend` BS.replicate (sz - BS.length dta) 0
        mseg = MemSegment { memBase  = elfSegmentVirtAddr seg
                          , memFlags = elfSegmentFlags seg
                          , memBytes = fixedData
                          }