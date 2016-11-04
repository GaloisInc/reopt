{-|
Copyright   : (c) Galois Inc, 2015-2016
Maintainer  : jhendrix@galois.com

Declares 'Memory', a type for representing segmented memory with permissions.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Macaw.Memory
  ( SomeMemory(..)
  , MemWidth
  , Memory
  , memWidth
  , emptyMemory
  , insertMemSegment
  , lookupSegment
  , memSegments
  , executableSegments
  , readonlySegments
  , Endianness(..)
  , readAddr
  , segmentOfRange
  , addrPermissions
  , isCodeAddr
  , isCodeAddrOrNull
  , absoluteAddrSegment
  , memAsAddrPairs
    -- * MemSegment operations
  , MemSegment
  , memSegment
  , SegmentIndex
  , segmentIndex
  , segmentBase
  , segmentFlags
  , segmentContents
  , ppMemSegment
  , segmentSize
  , SegmentRange(..)
--  , contentsFromList
    -- * Address and offset.
  , MemWord
  , SegmentedAddr(..)
  , addrOffset
  , addrContentsAfter
  , addrBase
  , addrValue
  , MemoryError(..)
  ) where

import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Word
import           GHC.TypeLits
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Parameterized.NatRepr

import qualified Data.Macaw.Memory.Permissions as Perm

data Endianness = BigEndian | LittleEndian

------------------------------------------------------------------------
-- Utilities

-- | Split a bytestring into an equivalent list of byte strings with a given size.
regularChunks :: Int -> BS.ByteString -> [BS.ByteString]
regularChunks sz bs
  | BS.null bs = []
  | otherwise = BS.take sz bs : regularChunks sz (BS.drop sz bs)


{-
bsWord16le :: BS.ByteString -> Word16
bsWord16le bs | BS.length bs /= 2 = error "bsWord16le given bytestring with bad length."
              | otherwise         = w0 .|. w1
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL` 8
-}

bsWord32be :: BS.ByteString -> Word32
bsWord32be bs | BS.length bs /= 4 = error "bsWord32le given bytestring with bad length."
              | otherwise = w0 .|. w1 .|. w2 .|. w3
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL`  8
        w2 = fromIntegral (BS.index bs 2) `shiftL` 16
        w3 = fromIntegral (BS.index bs 3) `shiftL` 24

bsWord32le :: BS.ByteString -> Word32
bsWord32le bs | BS.length bs /= 4 = error "bsWord32le given bytestring with bad length."
            | otherwise = w0 .|. w1 .|. w2 .|. w3
  where w0 = fromIntegral (BS.index bs 0)
        w1 = fromIntegral (BS.index bs 1) `shiftL`  8
        w2 = fromIntegral (BS.index bs 2) `shiftL` 16
        w3 = fromIntegral (BS.index bs 3) `shiftL` 24

bsWord64be :: BS.ByteString -> Word64
bsWord64be bs
    | BS.length bs /= 8 = error "bsWord64be given bytestring with bad length."
    | otherwise = w 0 .|. w 1 .|. w 2 .|. w 3 .|. w 4 .|. w 5 .|. w 6 .|. w 7
  where w i = fromIntegral (BS.index bs i) `shiftL` ((7 - i) `shiftL` 3)

bsWord64le :: BS.ByteString -> Word64
bsWord64le bs
    | BS.length bs /= 8 = error "bsWord64le given bytestring with bad length."
    | otherwise = w 0 .|. w 1 .|. w 2 .|. w 3 .|. w 4 .|. w 5 .|. w 6 .|. w 7
  where w i = fromIntegral (BS.index bs i) `shiftL` (i `shiftL` 3)




------------------------------------------------------------------------
-- MemBase


newtype MemWord (n :: Nat) = MemWord Word64
-- ^ A value in memory.

instance Show (MemWord w) where
  show (MemWord w) = showHex w ""

instance Eq (MemWord w) where
  MemWord x == MemWord y = x == y

instance Ord (MemWord w) where
  compare (MemWord x) (MemWord y) = compare x y

memWord32 :: Word64 -> MemWord 32
memWord32 x = MemWord (x .&. 0xffffffff)

instance Num (MemWord 32) where
  MemWord x + MemWord y = memWord32 $ x + y
  MemWord x - MemWord y = memWord32 $ x - y
  MemWord x * MemWord y = memWord32 $ x * y
  abs = id
  fromInteger = memWord32 . fromInteger
  negate (MemWord x) = memWord32 (negate x)
  signum (MemWord x) = memWord32 (signum x)

memWord64 :: Word64 -> MemWord 64
memWord64 = MemWord

instance Num (MemWord 64) where
  MemWord x + MemWord y = memWord64 $ x + y
  MemWord x - MemWord y = memWord64 $ x - y
  MemWord x * MemWord y = memWord64 $ x * y
  abs = id
  fromInteger = memWord64 . fromInteger
  negate (MemWord x) = memWord64 (negate x)
  signum (MemWord x) = memWord64 (signum x)

instance Enum (MemWord 32) where
  toEnum = memWord32 . fromIntegral
  fromEnum (MemWord x) = fromIntegral x

instance Enum (MemWord 64) where
  toEnum = memWord64 . fromIntegral
  fromEnum (MemWord x) = fromIntegral x

instance Real (MemWord 32) where
  toRational (MemWord x) = toRational x

instance Real (MemWord 64) where
  toRational (MemWord x) = toRational x

instance Integral (MemWord 32) where
  MemWord x `quotRem` MemWord y = (MemWord q, MemWord r)
    where (q,r) = x `quotRem` y
  toInteger (MemWord x) = toInteger x

instance Integral (MemWord 64) where
  MemWord x `quotRem` MemWord y = (MemWord q, MemWord r)
    where (q,r) = x `quotRem` y
  toInteger (MemWord x) = toInteger x


-- | A unique identifier for a segment.
type SegmentIndex = Int

class IsAddr w where
  addrSize :: p w -> MemWord w
  addrRead :: Endianness -> BS.ByteString -> MemWord w

instance IsAddr 32 where
  addrSize _ = 4
  addrRead BigEndian    = MemWord . fromIntegral . bsWord32be
  addrRead LittleEndian = MemWord . fromIntegral . bsWord32le


instance IsAddr 64 where
  addrSize _ = 8
  addrRead BigEndian    = MemWord . bsWord64be
  addrRead LittleEndian = MemWord . bsWord64le

type MemWidth w = (IsAddr w, Integral (MemWord w))

------------------------------------------------------------------------
-- SegmentRange

-- | Defines a portion of a segment.
data SegmentRange w
   = ByteRegion !BS.ByteString
   | RelocatableAddr !SegmentIndex !(MemWord w)

rangeSize :: MemWidth w => SegmentRange w -> MemWord w
rangeSize (ByteRegion bs) = fromIntegral (BS.length bs)
rangeSize (RelocatableAddr _ w) = addrSize w

------------------------------------------------------------------------
-- SegmentContents

-- | A sequence of values in the segment.
newtype SegmentContents w = SegmentContents (Map.Map (MemWord w) (SegmentRange w))

-- | Create the segment contents from a list of ranges.
contentsFromList :: MemWidth w => [SegmentRange w] -> SegmentContents w
contentsFromList elts = SegmentContents $ Map.fromList (offsets `zip` elts)
  where offsets = scanl (\s r -> s + rangeSize r) 0 elts

contentsSize :: MemWidth w => SegmentContents w -> MemWord w
contentsSize (SegmentContents m) =
  case Map.maxViewWithKey m of
    Nothing -> 0
    Just ((start, c),_) -> start + rangeSize c

-- | Read an address from the value in the segment or report a memory error.
lookupRange :: Num (MemWord w)
            => MemWord w
            -> SegmentContents w
            -> Maybe (MemWord w, SegmentRange w)
lookupRange i (SegmentContents m) = do
  (k,r) <- Map.lookupLE i m
  Just (i-k,r)

-- | Return list of contents from given word or 'Nothing' if this can't be done
-- due to a relocation.
contentsAfter :: Integral (MemWord w)
             => MemWord w
             -> SegmentContents w
             -> Maybe [SegmentRange w]
contentsAfter off (SegmentContents m) = do
  let (premap,mv,post) = Map.splitLookup off m
  case mv of
    Just v -> Just $ v : Map.elems post
    Nothing ->
      case Map.maxViewWithKey premap of
        Nothing | off == 0 -> Just []
                | otherwise -> error $ "Memory.contentsAfter invalid contents"
        Just ((pre_off, ByteRegion bs),_) ->
          let v = ByteRegion (BS.drop (fromIntegral (off - pre_off)) bs)
           in Just $ v : Map.elems post
        Just ((_, RelocatableAddr{}),_) -> Nothing

contentsList :: SegmentContents w -> [(MemWord w, SegmentRange w)]
contentsList (SegmentContents m) = Map.toList m

------------------------------------------------------------------------
-- MemSegment

-- | Describes a memory segment.
--
-- Memory segments are non-overlapping regions of memory.
data MemSegment w = MemSegment { segmentIndex :: !SegmentIndex
                                 -- ^ Unique index for this segment
                               , segmentBase  :: !(Maybe (MemWord w))
                                 -- ^ Base for this segment
                               , segmentFlags :: !Perm.Flags
                                 -- ^ Permisison flags
                               , segmentContents :: !(SegmentContents w)
                                 -- ^ Map from offsets to the contents of the segment.
                               }

-- | Create a memory segment with the given values.
memSegment :: MemWidth w
           => SegmentIndex
              -- ^ Unique index of segment
           -> Maybe (MemWord w)
              -- ^ Base address if defined
           -> Perm.Flags
              -- ^ Flags if defined
           -> [SegmentRange w]
              -- ^ Range of vlaues.
           -> MemSegment w
memSegment idx base flags contents =
  MemSegment { segmentIndex = idx
             , segmentBase = base
             , segmentFlags = flags
             , segmentContents = contentsFromList contents
             }

instance Eq (MemSegment w) where
  x == y = segmentIndex x == segmentIndex y

instance Ord (MemSegment w) where
  compare x y = compare (segmentIndex x) (segmentIndex y)

-- | Return the size of the segment data.
segmentSize :: MemWidth w => MemSegment w -> MemWord w
segmentSize = contentsSize . segmentContents

-- | Pretty print a memory segment.
ppMemSegment :: MemWidth w => MemSegment w -> Doc
ppMemSegment s =
  indent 2 $ vcat [ text "index =" <+> text (show (segmentIndex s))
                  , text "base  =" <+> text (maybe "none" show (segmentBase s))
                  , text "flags =" <+> text (show (segmentFlags s))
                  , text "size  =" <+> text (show (segmentSize s))
                  ]

instance MemWidth w => Show (MemSegment w) where
  show = show . ppMemSegment

{-
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
-}

{-
-- | Returns an interval representing the range of addresses for the segment
-- if it is non-empty.
memSegmentInterval :: (Eq w, Num w) => MemSegment w -> Maybe (IMap.Interval w)
memSegmentInterval s
    | sz == 0 = Nothing
    | otherwise = Just $ IMap.Interval base (base + sz - 1)
  where base = memBase s
        sz = fromIntegral $ BS.length (memBytes s)
-}

------------------------------------------------------------------------
-- SegmentedAddr

-- | A memory address is a reference to memory that uses an explicit segment plus
-- offset representation.
data SegmentedAddr w = SegmentedAddr { addrSegment :: !(MemSegment w)
                                     , _addrOffset  :: !(MemWord w)
                                     }
  deriving (Eq, Ord)

addrOffset :: Simple Lens (SegmentedAddr w) (MemWord w)
addrOffset = lens _addrOffset (\s v -> s { _addrOffset = v })

-- | Return the base value of an address or 0 if undefined.
addrBase :: Num (MemWord w) => SegmentedAddr w -> MemWord w
addrBase addr = fromMaybe 0 (segmentBase (addrSegment addr))

-- | Return the offset of the address after adding the base segment value if defined.
addrValue :: Num (MemWord w) => SegmentedAddr w -> MemWord w
addrValue addr = addrBase addr + addr^.addrOffset

instance Show (SegmentedAddr w) where
  showsPrec p a = showParen (p > 6) $
    showString "segment"
    . shows (segmentIndex (addrSegment a))
    . showString "+"
    . shows (a^.addrOffset)

-- | Return contents starting from location or throw a memory error if there
-- is an unaligned relocation.
addrContentsAfter :: Integral (MemWord w)
                  => SegmentedAddr w
                  -> Either (MemoryError w) [SegmentRange w]
addrContentsAfter addr =
  case contentsAfter (addr^.addrOffset) (segmentContents (addrSegment addr)) of
    Nothing -> Left (UnalignedRelocation addr)
    Just l  -> Right l

------------------------------------------------------------------------
-- Memory


-- | The state of the memory.
data Memory w = Memory { memWidth :: !(NatRepr w)
                       , memAbsoluteSegments :: !(Map.Map (MemWord w) (MemSegment w))
                       , memAllSegments      :: !(Map.Map SegmentIndex (MemSegment w))
                       }

instance MemWidth w => Show (Memory w) where
  show m = show (Fold.toList (memAllSegments m))

-- | A memory with no segments.
emptyMemory :: NatRepr w -> Memory w
emptyMemory w = Memory { memWidth = w
                       , memAbsoluteSegments = Map.empty
                       , memAllSegments = Map.empty
                       }

-- | Get memory segments.
memSegments :: Memory w -> [MemSegment w]
memSegments m = Map.elems (memAllSegments m)

-- | Return segment with given index in memory.
lookupSegment :: Memory w -> SegmentIndex -> Maybe (MemSegment w)
lookupSegment m i = Map.lookup i (memAllSegments m)

-- | Return list of address values in memory.  Each address includes the value
-- and the base.
memAsAddrPairs :: MemWidth w
               => Memory w
               -> Endianness
               -> [(SegmentedAddr w, SegmentedAddr w)]
memAsAddrPairs mem end = do
  seg <- memSegments mem
  (contents_offset,r) <- contentsList (segmentContents seg)
  let addr = SegmentedAddr seg contents_offset
  let MemWord sz = addrSize mem
  case r of
    ByteRegion bs -> assert (BS.length bs `rem` fromIntegral sz == 0) $ do
      w <- regularChunks (fromIntegral sz) bs
      let val = addrRead end w
      case Map.lookupLE val (memAbsoluteSegments mem) of
        Just (base, value_seg) | val <= base + segmentSize value_seg -> do
          let seg_val =  SegmentedAddr value_seg (val - base)
           in [(addr,seg_val)]
        _ -> []
    RelocatableAddr idx value_offset ->
      case lookupSegment mem idx of
        Just value_seg -> [(addr, SegmentedAddr value_seg value_offset)]
        Nothing -> error "memAsAddrPairs found segment without valid index."

{-
-- | Return list of words in the memory
memAsWord64le :: Integral w => Memory w -> [Word64]
memAsWord64le m = concatMap segmentAsWord64le (memSegments m)
-}

-- | Get executable segments.
executableSegments :: Memory w -> [MemSegment w]
executableSegments = filter (Perm.isExecutable . segmentFlags) . memSegments

readonlySegments :: Memory w -> [MemSegment w]
readonlySegments = filter (Perm.isReadonly . segmentFlags) . memSegments

-- | Given an absolute address, this returns a segment and offset into the segment.
absoluteAddrSegment :: MemWidth w => Memory w -> MemWord w -> Maybe (SegmentedAddr w)
absoluteAddrSegment mem addr =
  case Map.lookupLE addr (memAbsoluteSegments mem) of
    Just (base, seg) | addr < base + segmentSize seg ->
      Just $! SegmentedAddr { addrSegment = seg
                            , _addrOffset = addr - base
                            }
    _ -> Nothing

-- | Read an address from the value in the segment or report a memory error.
readAddr :: MemWidth w
         => Memory w
         -> Endianness
         -> SegmentedAddr w
         -> Either (MemoryError w) (SegmentedAddr w)
readAddr mem end addr = do
  let MemWord sz = addrSize addr
  case lookupRange (addr^.addrOffset) (segmentContents (addrSegment addr)) of
    Just (MemWord offset, ByteRegion bs)
      | offset + sz >= offset                           -- Check for no overfow
      , offset + sz < fromIntegral (BS.length bs) -> do -- Check length
        let val = addrRead end (BS.take (fromIntegral sz) (BS.drop (fromIntegral offset) bs))
        case Map.lookupLE val (memAbsoluteSegments mem) of
          Just (base, seg) | val <= base + segmentSize seg -> Right $
            SegmentedAddr { addrSegment = seg
                          , _addrOffset = val - base
                          }
          _ -> Left (InvalidAddr addr)

    -- We encountered a relocat
    Just (MemWord offset, RelocatableAddr idx a)
      | offset == 0 ->
          case lookupSegment mem idx of
             Just seg -> Right (SegmentedAddr seg a)
             Nothing -> error $ "Read address given invalid segment index."

      | otherwise ->
        Left (UnalignedRelocation addr)

    _ | otherwise ->
        Left (AccessViolation addr)

-- | Insert segment into memory or fail if this overlaps with another
-- segment in memory.
insertMemSegment :: (MemWidth w, MonadState (Memory w) m)
                 => MemSegment w -> m ()
insertMemSegment seg = do
  -- Update memAbsoluteSegments
  case segmentBase seg of
    Nothing -> pure ()
    Just base -> do
      mem <- get
      let m = memAbsoluteSegments mem
      case Map.lookupGE base m of
        Just (next,_) | next <= base + segmentSize seg -> do
          fail "Overlapping loadable segments."
        _ -> do
          put $ mem { memAbsoluteSegments = Map.insert base seg m }
  -- Update memAllSegments
  do mem <- get
     let m = memAllSegments mem
     when (Map.member (segmentIndex seg) m) $ do
       fail $ "Segment with given index already defined"
     put $ mem { memAllSegments = Map.insert (segmentIndex seg) seg m }

{-
-- | Returns segment at given address if any.
findSegment :: Ord w => w -> Memory w -> Maybe (MemSegment w)
findSegment w (Memory m) = snd <$> listToMaybe (IMap.search w m)
-}

-- | Return segment if range is entirely contained within a single segment
-- and 'Nothing' otherwise.
segmentOfRange :: MemWidth w
               => MemWord w -- ^ Start of range
               -> MemWord w -- ^ One past last index in range.
               -> Memory w
               -> Maybe (MemSegment w)
segmentOfRange base end mem =
  case Map.lookupLE base (memAbsoluteSegments mem) of
    Just (seg_base, seg) | end <= seg_base + segmentSize seg -> Just seg
    _ -> Nothing

-- | Return true if address satisfies permissions check.
addrPermissions :: MemWidth w => MemWord w -> Memory w -> Perm.Flags
addrPermissions addr mem =
  case Map.lookupLE addr (memAbsoluteSegments mem) of
    Just (base, seg) | addr < base + segmentSize seg -> segmentFlags seg
    _ -> Perm.none

{-

-- | Return true if address satisfies permissions check.
addrHasPermissions :: Ord w => w -> ElfSegmentFlags -> Memory w -> Bool
addrHasPermissions w req m = addrPermissions w m `hasPermissions` req
-}

-- | Indicates if address is a code pointer.
isCodeAddr :: MemWidth w => Memory w -> MemWord w -> Bool
isCodeAddr mem val = addrPermissions val mem `Perm.hasPerm` Perm.execute

-- | Indicates if address is an address in code segment or null.
isCodeAddrOrNull :: MemWidth w => Memory w -> MemWord w -> Bool
isCodeAddrOrNull _ (MemWord 0) = True
isCodeAddrOrNull mem a = isCodeAddr mem a

{-
-- | Return true if this is a read only address.
isReadonlyAddr :: Ord w => Memory w -> MemWord w -> Bool
isReadonlyAddr mem val = isReadonly (addrPermissions val mem)
-}

------------------------------------------------------------------------
-- MemoryError

-- | Type of errors that may occur when reading memory.
data MemoryError w
   = UserMemoryError String
   | AccessViolation (SegmentedAddr w)
     -- ^ Memory could not be read, because it was not defined.
   | PermissionsError (SegmentedAddr w)
     -- ^ Memory could not be read due to insufficient permissions.
   | UnalignedRelocation (SegmentedAddr w)
     -- ^ Read from location that partially overlaps a relocated entry
   | InvalidAddr (SegmentedAddr w)
     -- ^ The data at the given address did not refer to a valid memory location.

instance Show (MemoryError w) where
  show (UserMemoryError msg) = msg
  show (AccessViolation a)   =
    "Access violation at " ++ show a ++ "."
  show (PermissionsError a)  =
    "Insufficient permissions at " ++ show a ++ "."
  show (UnalignedRelocation a)   =
    "Attempt to read an offset of a relocation entry at " ++ show a ++ "."
  show (InvalidAddr a)   =
    "Attempt to read an offset of a relocation entry at " ++ show a ++ "."

------------------------------------------------------------------------
-- SomeMemory

data SomeMemory
   = Memory32 !(Memory 32)
   | Memory64 !(Memory 64)
