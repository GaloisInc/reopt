{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reopt.Memory 
  ( SomeMemory(..)
  , Memory
  , emptyMemory
  , insertMemSegment
  , memSegments
  , executableSegments
  , MemSegment(..)
  , isExecutable
  , ppMemSegment
  , MemoryByteReader
  , runMemoryByteReader
  , MemoryError(..)
  ) where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad.Error
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import Data.Elf
import qualified Data.Foldable as Fold
import qualified Data.IntervalMap.FingerTree as IMap
import Data.Maybe (listToMaybe)
import Data.Word

import Numeric (showHex)
import Text.PrettyPrint.Leijen hiding ((<$>))

import Reopt.ByteReader

------------------------------------------------------------------------
-- MemSegment

-- | Describes a memory segment.
data MemSegment w = MemSegment { memBase :: w
                               , memFlags :: ElfSegmentFlags
                               , memBytes :: BS.ByteString
                               }

-- | Return true if the segment is executable.
isExecutable :: MemSegment w -> Bool
isExecutable m = (memFlags m .&. pf_x) == pf_x

-- | Pretty print a memory segment.
ppMemSegment :: (Integral w, Show w) => MemSegment w -> Doc
ppMemSegment ms = 
  indent 2 $ vcat [ text "base =" <+> text (showHex (memBase ms) "")
                  , text "flags =" <+> text (show (memFlags ms))
                  , text "size =" <+>  text (showHex (BS.length (memBytes ms)) "")
                  ]

instance (Integral w, Show w) => Show (MemSegment w) where
  show = show . ppMemSegment

------------------------------------------------------------------------
-- Memory

data SomeMemory 
   = Memory32 !(Memory Word32)
   | Memory64 !(Memory Word64)

-- | The state of the memory.
newtype Memory w = Memory { _memMap :: IMap.IntervalMap w (MemSegment w) }

instance (Integral w, Show w) => Show (Memory w) where
  show (Memory m) = show (Fold.toList m)

-- | A memory with no segments.
emptyMemory :: Ord w => Memory w
emptyMemory = Memory IMap.empty

-- | Get memory segments.
memSegments :: Memory w -> [MemSegment w]
memSegments (Memory m) = Fold.toList m

-- | Get executable segments.
executableSegments :: Memory w -> [MemSegment w]
executableSegments = filter isExecutable . memSegments

-- | Returns an interval representing the range of addresses for the segment
-- if it is non-empty.
memSegmentInterval :: (Eq w, Num w) => MemSegment w -> Maybe (IMap.Interval w)
memSegmentInterval s
    | sz == 0 = Nothing
    | otherwise = Just $ IMap.Interval base (base + sz - 1)
  where base = memBase s
        sz = fromIntegral $ BS.length (memBytes s)

-- | Insert segement into memory or fail if this overlaps with another
-- segment in memory.
insertMemSegment :: (Ord w, Num w, MonadState (Memory w) m)
                 => MemSegment w -> m ()
insertMemSegment mseg =
  case memSegmentInterval mseg of
    Nothing -> return ()
    Just i -> do
      Memory m <- get
      case IMap.intersections i m of
        [] -> put $ Memory $ IMap.insert i mseg m
        _  -> fail "Overlapping loadable segments."

-- | Returns segment at given address if any.
findSegment :: Ord w => w -> Memory w -> Maybe (MemSegment w)
findSegment w (Memory m) = snd <$> listToMaybe (IMap.search w m)

------------------------------------------------------------------------
-- MemReader

data MemStream w = MS { _msNext :: !BS.ByteString
                      , _msMem  :: !(Memory w) 
                      , msAddr :: !w
                      , msPerm :: ElfSegmentFlags
                      }

-- | Type of errors that may occur when reading memory.
data MemoryError w
   = UserMemoryError String
     -- | Memory could not be read, because it was not defined.
   | AccessViolation w
     -- | Memory could not be read due to insufficient permissions.
   | PermissionsError w

instance Error (MemoryError w) where
  strMsg = UserMemoryError

instance (Integral w, Show w) => Show (MemoryError w) where
  show (UserMemoryError msg) = msg
  show (AccessViolation a)  = "Access violation at " ++ showHex a ""
  show (PermissionsError a) = "Insufficient permissions at " ++ showHex a ""

newtype MemoryByteReader w a = MBR (ErrorT (MemoryError w) (State (MemStream w)) a)
  deriving (Functor, Applicative, Monad)

instance (Integral w, Show w) => ByteReader (MemoryByteReader w) where
  readByte = do
    MS b m w reqPerm <- MBR get
    case BS.uncons b of
      Just (v,b') -> do
        let ms = MS { _msNext = b'
                    , _msMem = m
                    , msAddr = w+1
                    , msPerm = reqPerm
                    }
        MBR $ v <$ put ms
      Nothing ->
        case findSegment w m of
          Nothing -> MBR $ throwError $ AccessViolation w
          Just s -> assert (memBase s <= w) $ do
            MBR $ do
              unless (memFlags s .&. reqPerm == reqPerm) $ do
                throwError $ PermissionsError w
              -- Let d be number of bytes to drop from start of segment.
              let d = fromIntegral (w - memBase s)
              let ms = MS (BS.drop d (memBytes s)) m w reqPerm
              put ms
            readByte

-- | Create a memory stream pointing to given address, and return value 
runMemoryByteReader :: ElfSegmentFlags 
                    -> Memory w
                    -> w -> MemoryByteReader w a -> (Either (MemoryError w) a, w)
runMemoryByteReader reqPerm mem addr (MBR m) = (er, msAddr s)
  where (er,s) = runState (runErrorT m) (MS BS.empty mem addr reqPerm)