{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Reopt.Memory
  ( SomeMemory(..)
  , Memory
  , emptyMemory
  , insertMemSegment
  , memSegments
  , executableSegments
  , addrHasPermissions
  , findSegment
  , MemSegment(..)
  , isExecutable
  , ppMemSegment
  , segmentAsWord64le
  , MemoryByteReader
  , runMemoryByteReader
  , MemoryError(..)
  , readInstruction

    -- * Re-exports
  , Elf.ElfSegmentFlags
  , Elf.pf_r
  , Elf.pf_w
  , Elf.pf_x
  , Elf.hasPermissions
  ) where

import           Control.Applicative
import           Control.Exception (assert)
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Elf as Elf
import qualified Data.Foldable as Fold
import qualified Data.IntervalMap.FingerTree as IMap
import           Data.Maybe
import           Data.Word

import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Flexdis86 as Flexdis
import           Flexdis86.ByteReader

------------------------------------------------------------------------
-- MemSegment

-- | Describes a memory segment.
data MemSegment w = MemSegment { memBase :: w
                               , memFlags :: ElfSegmentFlags
                               , memBytes :: BS.ByteString
                               }

-- | Return true if the segment is executable.
isExecutable :: MemSegment w -> Bool
isExecutable s = (memFlags s `hasPermissions` pf_x)

-- | Pretty print a memory segment.
ppMemSegment :: (Integral w, Show w) => MemSegment w -> Doc
ppMemSegment ms =
  indent 2 $ vcat [ text "base =" <+> text (showHex (memBase ms) "")
                  , text "flags =" <+> text (show (memFlags ms))
                  , text "size =" <+>  text (showHex (BS.length (memBytes ms)) "")
                  ]

-- | Project segment into 64-bit words with least-significant-bit first.
segmentAsWord64le :: MemSegment w -> [Word64]
segmentAsWord64le s = go (BSL.fromChunks [memBytes s])
  where go b | BSL.length b >= 8 =
          case runGetOrFail getWord64le b of
            Left{} -> error "internal error in segmentAsWord64le"
            Right (b',_, w) -> w:go b'
        go _ = []

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

-- | Insert segment into memory or fail if this overlaps with another
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

-- | Return true if address satisfies permissions check.
addrHasPermissions :: Ord w => w -> ElfSegmentFlags -> Memory w -> Bool
addrHasPermissions w req m = fromMaybe False $ do
  s <- findSegment w m
  return (memFlags s `hasPermissions` req)

------------------------------------------------------------------------
-- MemReader

data MemStream w = MS { msNext :: !BS.ByteString
                      , msMem  :: !(Memory w)
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
  show (AccessViolation a)  | a >= 0 = "Access violation at " ++ showHex a ""
  show (PermissionsError a) | a >= 0 = "Insufficient permissions at " ++ showHex a ""

newtype MemoryByteReader w a = MBR (ErrorT (MemoryError w) (State (MemStream w)) a)
  deriving (Functor, Applicative, Monad)

instance (Integral w, Show w) => ByteReader (MemoryByteReader w) where
  readByte = do
    MS b m w reqPerm <- MBR get
    if BS.null b then
      case findSegment w m of
        Nothing -> MBR $ throwError $ AccessViolation w
        Just s -> assert (memBase s <= w) $ do
          MBR $ do
            -- Throw error when permissions check fails.
            when ((memFlags s .&. reqPerm) /= reqPerm) $ do
              throwError $ PermissionsError w
            -- Let d be number of bytes to drop from start of segment.
            let d = fromIntegral (w - memBase s)
            put MS { msNext = BS.drop d (memBytes s)
                   , msMem = m
                   , msAddr = w
                   , msPerm = reqPerm
                   }
          readByte
    else do
      let v = BS.head b
      let ms = MS { msNext = BS.tail b
                  , msMem = m
                  , msAddr = w+1
                  , msPerm = reqPerm
                  }
      MBR $ v <$ put ms

-- | Create a memory stream pointing to given address, and return pair whose
-- first element is the value read or an error, and whose second element is
-- the address of the next value to read.
runMemoryByteReader :: ElfSegmentFlags
                       -- ^ Permissions that memory accesses are expected to
                       -- satisfy.
                       -- Added so we can check for read and/or execute permission.
                    -> Memory w -- ^ Memory to read from.
                    -> w -- ^ Starting address.
                    -> MemoryByteReader w a -- ^ Byte reader to read values from.
                    -> (Either (MemoryError w) (a, w))
runMemoryByteReader reqPerm mem addr (MBR m) =
  case runState (runErrorT m) (MS BS.empty mem addr reqPerm) of
    (Left e, _) -> Left e
    (Right v, s) -> Right (v,msAddr s)

-- | Read instruction at a given memory address.
readInstruction :: Memory Word64 -- Memory to read.
                -> Word64 -- Address to read from.
                -> Either (MemoryError Word64) (Flexdis.InstructionInstance, Word64)
readInstruction mem addr = runMemoryByteReader pf_x mem addr m
  where m = Flexdis.disassembleInstruction Flexdis.defaultX64Disassembler
