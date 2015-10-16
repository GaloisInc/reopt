{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Reopt.Object.Memory
  ( SomeMemory(..)
  , Memory
  , emptyMemory
  , insertMemSegment
  , memSegments
  , memAsWord64le
  , memAsWord64le_withAddr
  , executableSegments
  , readonlySegments
  , addrPermissions
  , addrHasPermissions
  , isCodeAddr
  , isReadonlyAddr
  , findSegment
  , MemSegment(..)
  , ppMemSegment
  , segmentAsWord64le
  , segmentSize
  , MemoryByteReader
  , runMemoryByteReader
  , MemoryError(..)
  , memLookupWord8
  , memLookupWord16
  , memLookupWord32
  , memLookupWord64
  , readInstruction

    -- * Re-exports
  , Elf.ElfSegmentFlags
  , Elf.pf_none
  , Elf.pf_r
  , Elf.pf_w
  , Elf.pf_x
  , Elf.hasPermissions
  , isExecutable
  , isReadonly
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Elf as Elf
  ( ElfSegmentFlags
  , pf_none
  , pf_r
  , pf_w
  , pf_x
  , hasPermissions
  )
import qualified Data.Foldable as Fold
import qualified Data.IntervalMap.FingerTree as IMap
import           Data.Maybe
import           Data.Word

import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Flexdis86 as Flexdis
import           Flexdis86.ByteReader

------------------------------------------------------------------------
-- Utilities

bsWord16 :: BS.ByteString -> Word16
bsWord16 bs = w0 .|. w1
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL` 8


bsWord32 :: BS.ByteString -> Word32
bsWord32 bs = w0 .|. w1 .|. w2 .|. w3
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL`  8
        w2 = fromIntegral (BS.index bs 2) `shiftL` 16
        w3 = fromIntegral (BS.index bs 3) `shiftL` 24

bsWord64 :: BS.ByteString -> Word64
bsWord64 bs = w0 .|. w1 .|. w2 .|. w3 .|. w4 .|. w5 .|. w6 .|. w7
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL`  8
        w2 = fromIntegral (BS.index bs 2) `shiftL` 16
        w3 = fromIntegral (BS.index bs 3) `shiftL` 24
        w4 = fromIntegral (BS.index bs 4) `shiftL` 32
        w5 = fromIntegral (BS.index bs 5) `shiftL` 40
        w6 = fromIntegral (BS.index bs 6) `shiftL` 48
        w7 = fromIntegral (BS.index bs 7) `shiftL` 56

------------------------------------------------------------------------
-- MemSegment

-- | Describes a memory segment.
data MemSegment w = MemSegment { memBase  :: !w
                               , memFlags :: !ElfSegmentFlags
                               , memBytes :: !BS.ByteString
                               }

-- | Return true if the segment is executable.
isExecutable :: ElfSegmentFlags -> Bool
isExecutable f = f `hasPermissions` pf_x

-- | Return true if segment is read-only.
isReadonly :: ElfSegmentFlags -> Bool
isReadonly f = f .&. (pf_r .|. pf_w) == pf_r

instance (Integral w, Show w) => Show (MemSegment w) where
  show = show . ppMemSegment

-- | Pretty print a memory segment.
ppMemSegment :: (Integral w, Show w) => MemSegment w -> Doc
ppMemSegment ms =
  indent 2 $ vcat [ text "base =" <+> text (showHex (memBase ms) "")
                  , text "flags =" <+> text (show (memFlags ms))
                  , text "size =" <+>  text (showHex (BS.length (memBytes ms)) "")
                  ]

-- | Return size of segment.
segmentSize :: Num w => MemSegment w -> w
segmentSize seg = fromIntegral (BS.length (memBytes seg))

-- | Return list of aligned word 64s in the memory segments.
segmentAsWord64le :: Integral w => MemSegment w -> [Word64]
segmentAsWord64le s = go (memBytes s) cnt
  where base :: Int
        base = fromIntegral (memBase s) .&. 0x7
        cnt = (BS.length (memBytes s) - base) `shiftR` 3
        go _ 0 = []
        go b c = bsWord64 s' : go b' (c-1)
          where (s',b') = BS.splitAt 8 b

-- | Returns an interval representing the range of addresses for the segment
-- if it is non-empty.
memSegmentInterval :: (Eq w, Num w) => MemSegment w -> Maybe (IMap.Interval w)
memSegmentInterval s
    | sz == 0 = Nothing
    | otherwise = Just $ IMap.Interval base (base + sz - 1)
  where base = memBase s
        sz = fromIntegral $ BS.length (memBytes s)

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

-- | Return list of words in the memory
memAsWord64le_withAddr :: (Bits w, Integral w) => Memory w -> [(w, Word64)]
memAsWord64le_withAddr m = do
  s <- memSegments m
  let base = (memBase s + 7) `xor` 0x7
  [base,base+8..] `zip` segmentAsWord64le s

-- | Return list of words in the memory
memAsWord64le :: Integral w => Memory w -> [Word64]
memAsWord64le m = concatMap segmentAsWord64le (memSegments m)

-- | Get executable segments.
executableSegments :: Memory w -> [MemSegment w]
executableSegments = filter (isExecutable . memFlags) . memSegments

readonlySegments :: Memory w -> [MemSegment w]
readonlySegments = filter (isReadonly . memFlags) . memSegments

-- | Insert segment into memory or fail if this overlaps with another
-- segment in memory.
insertMemSegment :: (Ord w, Num w, MonadState (Memory w) m)
                 => MemSegment w -> m ()
insertMemSegment mseg = seq mseg $ do
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
addrPermissions :: Ord w => w -> Memory w -> ElfSegmentFlags
addrPermissions w m = maybe pf_none memFlags (findSegment w m)

-- | Return true if address satisfies permissions check.
addrHasPermissions :: Ord w => w -> ElfSegmentFlags -> Memory w -> Bool
addrHasPermissions w req m = addrPermissions w m `hasPermissions` req

-- | Indicates if address is a code pointer.
isCodeAddr :: Memory Word64 -> Word64 -> Bool
isCodeAddr mem val = addrPermissions val mem `hasPermissions` pf_x

-- | Return true if this is a read only address.
isReadonlyAddr :: Memory Word64 -> Word64 -> Bool
isReadonlyAddr mem val = addrPermissions val mem .&. (pf_r .|. pf_w) == pf_r

-- | Attempt to read a contiguous string of bytes from a single segment.
memSubsegment :: Integral w
              => Memory w
                 -- ^ Memory to read form
              -> ElfSegmentFlags
                 -- ^ Permissions expected during reading.
              -> w
                 -- ^ Address to read
              -> Int
                 -- ^ Number of bytes to read.
              -> Either (MemoryError w) BS.ByteString
memSubsegment m reqPerm addr n =
  case findSegment addr m of
    Nothing -> Left $! AccessViolation addr
    Just s -> assert (memBase s <= addr) $ do
      -- Let d be number of bytes to drop from start of segment.
      let d = fromIntegral (addr - memBase s)
      -- Throw error when permissions check fails.
      if (memFlags s .&. reqPerm) /= reqPerm then
        Left $! PermissionsError addr
      else if n >= BS.length (memBytes s) - d then
        Left $! AccessViolation addr
      else
        Right $! BS.take n (BS.drop d (memBytes s))

memLookupWord8 :: Integral w => Memory w -> ElfSegmentFlags -> w -> Either (MemoryError w) Word8
memLookupWord8 m reqPerm addr =
  BS.head <$> memSubsegment m reqPerm addr 1

-- | Return a word16 at given address (assume little-endian encoding)
memLookupWord16 :: Integral w => Memory w -> ElfSegmentFlags -> w -> Either (MemoryError w) Word16
memLookupWord16 m reqPerm addr = do
  bsWord16 <$> memSubsegment m reqPerm addr 2

-- | Return a word32 at given address (assume little-endian encoding)
memLookupWord32 :: Integral w => Memory w -> ElfSegmentFlags -> w -> Either (MemoryError w) Word32
memLookupWord32 m reqPerm addr = do
  bsWord32 <$> memSubsegment m reqPerm addr 4

-- | Return a word64 at given address (assume little-endian encoding)
memLookupWord64 :: Integral w => Memory w -> ElfSegmentFlags -> w -> Either (MemoryError w) Word64
memLookupWord64 m reqPerm addr = do
  bsWord64 <$> memSubsegment m reqPerm addr 8

------------------------------------------------------------------------
-- MemStream

data MemStream w = MS { msNext :: !BS.ByteString
                      , msMem  :: !(Memory w)
                      , msAddr :: !w
                      , msPerm :: !ElfSegmentFlags
                        -- ^ Permissions that memory accesses are expected to satisfy.
                      }

-- | Type of errors that may occur when reading memory.
data MemoryError w
   = UserMemoryError String
     -- | Memory could not be read, because it was not defined.
   | AccessViolation w
     -- | Memory could not be read due to insufficient permissions.
   | PermissionsError w

-- instance Error (MemoryError w) where
--   strMsg = UserMemoryError

instance (Integral w, Show w) => Show (MemoryError w) where
  show (UserMemoryError msg) = msg
  show (AccessViolation a)   = "Access violation at " ++ showHex a ""
  show (PermissionsError a)  = "Insufficient permissions at " ++ showHex a ""

newtype MemoryByteReader w a = MBR (ExceptT (MemoryError w) (State (MemStream w)) a)
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
  case runState (runExceptT m) (MS BS.empty mem addr reqPerm) of
    (Left e, _) -> Left e
    (Right v, s) -> Right (v,msAddr s)

-- | Read instruction at a given memory address.
readInstruction :: Memory Word64 -- Memory to read.
                -> Word64 -- Address to read from.
                -> Either (MemoryError Word64) (Flexdis.InstructionInstance, Word64)
readInstruction mem addr = runMemoryByteReader pf_x mem addr m
  where m = Flexdis.disassembleInstruction Flexdis.defaultX64Disassembler
