{-# LANGUAGE FlexibleContexts #-}
module Reopt.Loader 
  ( loadElf
  ) where

import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Elf
import Data.Maybe (mapMaybe)
import Data.Monoid (mappend)

import Reopt.Memory

-- | Load an elf file into memory.
loadElf :: (ElfWidth w, MonadState (Memory w) m) => Elf w -> m ()
loadElf = mapM_ insertMemSegment
        . mapMaybe memSegment 
        . renderedElfSegments

-- | Return a memory segment for elf segment if it loadable.
memSegment :: Integral w => RenderedElfSegment w -> Maybe (MemSegment w)
memSegment seg 
    | tp == PT_LOAD = Just mseg
    | tp == PT_DYNAMIC = error "Dynamic elf libraries not yet supported."
    | otherwise = Nothing
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
