{-# LANGUAGE ScopedTypeVariables #-}

module Reopt.FunUseMap
  ( FunUseMap,
    mkFunUseMap,
    lookupFunSize,
    totalFunUseSize,
  )
where

import Control.Lens ((^.))
import Control.Monad.State
import Data.Macaw.Discovery
import Data.Macaw.Memory
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Parameterized.Some ( Some(..) )
import Data.Word ( Word64 )
import Numeric (showHex)
import Reopt.Utils.Folds ( sumBy )

-- | Type synonym to identify segment offsets that should correspond
-- to the start of a function.
type FunAddress w = MemSegmentOff w

-- | Maps offsets within a region to the end offset and starting addresses
-- of functions
type FunUseOffsetMap w = Map BlockOff (BlockOff, [FunAddress w])

type BlockOff = Word64

type BlockSize = Word64

-- | Create map with a single region
initFunUseOffsetMap :: FunAddress w -> BlockOff -> BlockOff -> FunUseOffsetMap w
initFunUseOffsetMap f off endOff = Map.singleton off (endOff, [f])

-- | Replace a region at a given block
replaceRegion ::
  BlockOff ->
  -- | New function
  FunAddress w ->
  -- | End offset of new function block
  BlockOff ->
  -- | Old functions
  [FunAddress w] ->
  -- | Size of old function blocks
  BlockOff ->
  FunUseOffsetMap w ->
  FunUseOffsetMap w
replaceRegion off f newEnd oldFuns oldEnd m =
  case compare oldEnd newEnd of
    LT ->
      Map.insert off (oldEnd, (f : oldFuns)) $
        Map.insert oldEnd (newEnd, [f]) m
    EQ -> Map.insert off (oldEnd, (f : oldFuns)) m
    GT ->
      Map.insert off (newEnd, (f : oldFuns)) $
        Map.insert newEnd (oldEnd, oldFuns) m

-- | Update funUseOffsetMap with new address
updateFunUseOffsetMap ::
  FunAddress w ->
  BlockOff ->
  -- | end offset
  BlockOff ->
  FunUseOffsetMap w ->
  FunUseOffsetMap w
updateFunUseOffsetMap f off newEnd old = do
  case Map.lookupLT newEnd old of
    Just (oldOff, (oldEnd, oldFuns)) | oldEnd > off ->
      case compare oldOff off of
        LT -> do
          let m = Map.insert oldOff (off, oldFuns) old
          replaceRegion off f newEnd oldFuns oldEnd m
        EQ -> replaceRegion off f newEnd oldFuns oldEnd old
        GT -> do
          let m = replaceRegion oldOff f newEnd oldFuns oldEnd old
          updateFunUseOffsetMap f off oldOff m
    _ -> Map.insert off (newEnd, [f]) old

-- | Maps regions indices for code to offsets witin region to the size
-- of the segment and a list of functions that occupy that region.
--
-- This maintains the invariant that the list of functions are
-- non-empty, and no regions overlap.
newtype FunUseMap w = FunUseMap (Map RegionIndex (FunUseOffsetMap w))

-- | Empty use map
emptyFunUseMap :: FunUseMap w
emptyFunUseMap = FunUseMap Map.empty

-- | @recordRegionUse f o sz@ Mark a region as belonging to a particular function
recordRegionUse ::
  FunAddress w ->
  MemSegmentOff w ->
  BlockSize ->
  FunUseMap w ->
  FunUseMap w
recordRegionUse f so sz (FunUseMap regionMap) = do
  let a = segoffAddr so
      -- Offset of block
      off :: BlockOff
      off = memWordValue (addrOffset a)
      initMap = initFunUseOffsetMap f off (off + sz)
      updMap _ old = updateFunUseOffsetMap f off (off + sz) old
   in FunUseMap (Map.insertWith updMap (addrBase a) initMap regionMap)

-- | Record memory used by block to function address
recordBlockUse ::
  FunAddress (ArchAddrWidth arch) ->
  ParsedBlock arch ids ->
  State (FunUseMap (ArchAddrWidth arch)) ()
recordBlockUse f b = do
  modify $ recordRegionUse f (pblockAddr b) (fromIntegral (blockSize b))
  -- Record jumptable backing as well.
  case pblockTermStmt b of
    ParsedLookupTable layout _ _ _ -> do
      let a = jtlBackingAddr layout
          sz = jtlBackingSize layout
      modify $ recordRegionUse f a sz
    _ -> pure ()

-- | Record memory used by block to function address
recordFunUse ::
  DiscoveryFunInfo arch ids ->
  State (FunUseMap (ArchAddrWidth arch)) ()
recordFunUse f =
  mapM_ (recordBlockUse (discoveredFunAddr f)) (f ^. parsedBlocks)

-- | Create a function use map from all functions in discover state.
mkFunUseMap :: DiscoveryState arch -> FunUseMap (ArchAddrWidth arch)
mkFunUseMap s = flip execState emptyFunUseMap $ do
  mapM_ (\(Some f) -> recordFunUse f) (s ^. funInfo)

-- | Return the number of bytes only allocated to the function.
endOwnedByFun :: FunAddress w -> BlockOff -> FunUseOffsetMap w -> BlockOff -> BlockOff
endOwnedByFun f o m regionSize =
  case Map.lookupGE o m of
    Nothing -> regionSize
    Just (s, (e, fns))
      | fns == [f] -> endOwnedByFun f e m regionSize
      | otherwise -> s

-- | Return the number of bytes starting from function entry
-- point in the segment that are exclusive allocated to function.
lookupFunSize :: MemWidth w => MemSegmentOff w -> FunUseMap w -> Word64
lookupFunSize f (FunUseMap m) =
  let seg = segoffSegment f
      a = segoffAddr f
      o = memWordValue (addrOffset a)
      err = error "internal error: Function register index"
      offMap = Map.findWithDefault err (addrBase a) m
      maxValue = memWordValue (segmentSize seg) - memWordValue (segoffOffset f)
   in case Map.lookup o offMap of
        Just _ -> endOwnedByFun f o offMap maxValue - o
        Nothing -> error $ "Unknown function " ++ showHex o ""

totalFunUseOffsetMapSize :: FunUseOffsetMap w -> Word64
totalFunUseOffsetMapSize = Map.foldlWithKey' (\z s (e, _) -> z + (e - s)) 0

-- | Return total number of bytes occupied by some function in map.
totalFunUseSize :: FunUseMap w -> Word64
totalFunUseSize (FunUseMap m) = sumBy totalFunUseOffsetMapSize m