{-|
Copyright   : (c) Galois Inc, 2015-2016
Maintainer  : jhendrix@galois.com

Declares 'Memory', a type for representing segmented memory with permissions.
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Data.Macaw.Memory
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
  , isCodeAddrOrNull
  , isReadonlyAddr
  , findSegment
  , segmentOfRange
  , MemSegment(..)
  , ppMemSegment
  , segmentAsWord64le
  , segmentSize
  , MemoryError(..)
  , memLookupWord8
  , memLookupWord16le
  , memLookupWord32le
  , memLookupWord64le
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
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.ElfEdit as Elf
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

------------------------------------------------------------------------
-- Utilities

bsWord16le :: BS.ByteString -> Word16
bsWord16le bs | BS.length bs /= 2 = error "bsWord16le given bytestring with bad length."
              | otherwise         = w0 .|. w1
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL` 8


bsWord32le :: BS.ByteString -> Word32
bsWord32le bs | BS.length bs /= 4 = error "bsWord32le given bytestring with bad length."
            | otherwise = w0 .|. w1 .|. w2 .|. w3
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL`  8
        w2 = fromIntegral (BS.index bs 2) `shiftL` 16
        w3 = fromIntegral (BS.index bs 3) `shiftL` 24

bsWord64le :: BS.ByteString -> Word64
bsWord64le bs | BS.length bs /= 8 = error "bsWord64le given bytestring with bad length."
            | otherwise = w0 .|. w1 .|. w2 .|. w3 .|. w4 .|. w5 .|. w6 .|. w7
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
segmentAsWord64le s | len <= base = [] -- Degenerate case to handle very small segments.
                    | otherwise = go (memBytes s) cnt
  where base :: Int
        base = fromIntegral (memBase s) .&. 0x7
        len = BS.length (memBytes s)
        cnt = (len - base) `shiftR` 3
        go _ 0 = []
        go b c = assert (BS.length s' == 8) $ bsWord64le s' : go b' (c-1)
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

-- | Return list of 64-bit words in memory
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

-- | Return segment if range is entirely contained within a single segment
-- and 'Nothing' otherwise.
segmentOfRange :: (Num w, Ord w)
               => w -- ^ Start of range
               -> w -- ^ One past last index in range.
               -> Memory w
               -> Maybe (MemSegment w)
segmentOfRange base end mem =
  case findSegment base mem of
    Just seg | end <= memBase seg + segmentSize seg -> Just seg
    _ -> Nothing

-- | Return true if address satisfies permissions check.
addrPermissions :: Ord w => w -> Memory w -> ElfSegmentFlags
addrPermissions w m = maybe pf_none memFlags (findSegment w m)

-- | Return true if address satisfies permissions check.
addrHasPermissions :: Ord w => w -> ElfSegmentFlags -> Memory w -> Bool
addrHasPermissions w req m = addrPermissions w m `hasPermissions` req

-- | Indicates if address is a code pointer.
isCodeAddr :: Ord w => Memory w -> w -> Bool
isCodeAddr mem val = addrPermissions val mem `hasPermissions` pf_x

-- | Indicates if address is an address in code segment or null.
isCodeAddrOrNull :: (Num w, Ord w) => Memory w -> w -> Bool
isCodeAddrOrNull _ 0 = True
isCodeAddrOrNull mem a = isCodeAddr mem a

-- | Return true if this is a read only address.
isReadonlyAddr :: Ord w => Memory w -> w -> Bool
isReadonlyAddr mem val = isReadonly (addrPermissions val mem)

------------------------------------------------------------------------
-- MemoryError

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

------------------------------------------------------------------------
-- memSubsegment

-- | Attempt to read a contiguous string of bytes from a single segment.
memSubsegment :: Integral w
              => Memory w
                 -- ^ Memory to read form
              -> (ElfSegmentFlags -> Bool)
                 -- ^ Predicate to check permissions
              -> w
                 -- ^ Address to read
              -> Int
                 -- ^ Number of bytes to read.
              -> Either (MemoryError w) BS.ByteString
memSubsegment m permPred addr n =
  case findSegment addr m of
    Nothing -> Left $! AccessViolation addr
    Just s -> assert (memBase s <= addr) $ do
      -- Let d be number of bytes to drop from start of segment.
      let d = fromIntegral (addr - memBase s)
      -- Throw error when permissions check fails.
      if permPred (memFlags s) then
        Left $! PermissionsError addr
      else if n >= BS.length (memBytes s) - d then
        Left $! AccessViolation addr
      else
        Right $! BS.take n (BS.drop d (memBytes s))

-- | Return a word8 at given address encoded in little-endian.
memLookupWord8 :: Integral w
               => Memory w
               -> (ElfSegmentFlags -> Bool)
               -> w
               -> Either (MemoryError w) Word8
memLookupWord8 m permPred addr =
  BS.head <$> memSubsegment m permPred addr 1

-- | Return a word16 at given address encoded in little-endian.
memLookupWord16le :: Integral w
                  => Memory w
                  -> (ElfSegmentFlags -> Bool)
                  -> w
                  -> Either (MemoryError w) Word16
memLookupWord16le m permPred addr = do
  bsWord16le <$> memSubsegment m permPred addr 2

-- | Return a word32 at given address encoded in little-endian.
memLookupWord32le :: Integral w
                  => Memory w
                  -> (ElfSegmentFlags -> Bool)
                  -> w
                  -> Either (MemoryError w) Word32
memLookupWord32le m permPred addr = do
  bsWord32le <$> memSubsegment m permPred addr 4

-- | Return a word64 at given address encoded in little-endian.
memLookupWord64le :: Integral w
                  => Memory w
                  -> (ElfSegmentFlags -> Bool)
                  -> w
                  -> Either (MemoryError w) Word64
memLookupWord64le m permPred addr = do
  bsWord64le <$> memSubsegment m permPred addr 8
