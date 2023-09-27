{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Residual (runResidual) where

import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (newIORef)
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Data.Word (Word64)
import Numeric (showHex)

import Data.Macaw.Discovery (DiscoveryState (memory))
import Data.Macaw.Memory (MemChunk, Memory)
import Data.Macaw.Memory qualified as Mem
import Data.Macaw.Memory.Permissions qualified as Perm

import Reopt (
  LoadOptions (LoadOptions),
  RecoverX86Output (recoveredModule, summaryFailures),
  ReoptOptions,
  X86_64,
  loadOffset,
  parseElfHeaderInfo64,
  recoverX86Elf,
  resolveHeader,
  roVerboseMode,
  runReoptM,
 )
import Reopt.CFG.FnRep (
  FnBlock,
  RecoveredModule,
  fbLabel,
  fbSize,
  fnBlockLabelAddr,
  fnBlocks,
  recoveredDefs,
 )
import Reopt.Events (
  ReoptLogEvent,
  initReoptSummary,
  joinLogEvents,
  printLogEvent,
  recoverLogEvent,
 )
import Reopt.Utils.Exit (
  checkedReadFile,
  handleEitherStringWithExit,
  handleEitherWithExit,
 )

import CommandLine (
  Options,
  ResidualOptions (roClangPath, roHeader, roOutputForSpreadsheet, roPaths),
 )
import Common (findAllElfFilesInDirs)
import Data.Either (fromRight)
import Data.ElfEdit (
  ElfHeaderInfo,
  Shdr (shdrAddr, shdrName, shdrSize),
  Symtab (symtabEntries),
  SymtabEntry (steName, steValue),
  decodeHeaderSymtab,
  headerNamedShdrs,
 )
import Data.List (find, partition)
import Data.Maybe (isJust, mapMaybe)
import Flexdis86 (
  DisassembledAddr (disInstruction, disOffset),
  disassembleBuffer,
  ppInstruction,
 )
import Prettyprinter qualified as PP
import Prettyprinter.Render.String (renderString)
import Residual.Recognizers (
  ResidualExplanation (BecauseFailure),
  classifyInstrs,
  ppResidualExplanation,
 )
import Text.Printf (printf)

newtype InclusiveRange w = InclusiveRange {getInclusiveRange :: (w, w)}

instance (Integral w, Num w, Show w) => Show (InclusiveRange w) where
  show r =
    "0x" <> showHex (rangeLowerBound r) "" <> " - 0x" <> showHex (rangeUpperBound r) ""

-- sometimes nice when debugging:
-- <> " (size: " <> show (rangeSize r) <> "B)"

rangeSize :: Num a => InclusiveRange a -> a
rangeSize (getInclusiveRange -> (lo, hi)) = hi - lo + 1

rangeLowerBound :: InclusiveRange w -> w
rangeLowerBound (getInclusiveRange -> (lo, _)) = lo

rangeUpperBound :: InclusiveRange w -> w
rangeUpperBound (getInclusiveRange -> (_, hi)) = hi

inclusiveRangeFromBaseAndSize :: Num w => w -> w -> InclusiveRange w
inclusiveRangeFromBaseAndSize base size = InclusiveRange (base, base + size - 1)

ppInclusiveRange :: (Integral w, Show w) => InclusiveRange w -> String
ppInclusiveRange (getInclusiveRange -> (lo, hi)) = showHex lo "" <> " - " <> showHex hi ""

shdrInclusiveRange :: (Eq w, Ord w, Num w) => Shdr nm w -> Maybe (InclusiveRange w)
shdrInclusiveRange s =
  let addr = shdrAddr s
   in if addr == 0 || shdrSize s == 0
        then Nothing
        else Just $ InclusiveRange (shdrAddr s, shdrAddr s + shdrSize s - 1)

inInclusiveRange :: (Num w, Ord w, Show w) => InclusiveRange w -> w -> Bool
inInclusiveRange r v = rangeLowerBound r <= v && v <= rangeUpperBound r

type RangedShdr nm w = (InclusiveRange w, Shdr nm w)

rangedShdr :: (Num w, Ord w) => Shdr nm w -> Maybe (RangedShdr nm w)
rangedShdr s = (,s) <$> shdrInclusiveRange s

createLogger :: ReoptOptions -> FilePath -> IO (ReoptLogEvent arch -> IO ())
createLogger reoptOpts filePath = do
  summaryRef <- newIORef $ initReoptSummary filePath
  statsRef <- newIORef mempty
  return $
    if roVerboseMode reoptOpts
      then joinLogEvents printLogEvent (recoverLogEvent summaryRef statsRef)
      else recoverLogEvent summaryRef statsRef

performRecovery ::
  ResidualOptions ->
  ReoptOptions ->
  (a, FilePath) ->
  IO (ElfHeaderInfo 64, DiscoveryState X86_64, RecoverX86Output)
performRecovery residualOpts reoptOpts (_idx, fPath) = do
  let lOpts = LoadOptions{loadOffset = Nothing}
  let unnamedFunPrefix = BSC.pack "reopt"
  -- hPutStrLn stderr $ "[" ++ (show index) ++ " of " ++ (show totalCount)
  --                    ++ "] Analyzing " ++ fPath ++ " ..."
  bs <- checkedReadFile fPath
  annDecl <-
    runReoptM
      printLogEvent
      (resolveHeader (roHeader residualOpts) (roClangPath residualOpts))
      >>= either (error . show) return
  hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs
  logger <- createLogger reoptOpts fPath
  (_os, ds, recovOut, _, _) <-
    handleEitherWithExit
      =<< runReoptM
        logger
        (recoverX86Elf lOpts reoptOpts annDecl unnamedFunPrefix hdrInfo)
  return (hdrInfo, ds, recovOut)

data PartitionedSegments = PartitionedSegments
  { blockSegments :: [Segment]
  -- ^ Those segments that are part of discovered blocks
  , residualSegments :: [Segment]
  -- ^ Those segments that are not
  }
  deriving (Show)

computeResidualSegments ::
  DiscoveryState X86_64 ->
  RecoveredModule X86_64 ->
  IO PartitionedSegments
computeResidualSegments discoveryState recoveredModule = do
  -- Initially considering all segments residual until shown otherwise
  let allMemorySegments =
        PartitionedSegments
          { blockSegments = []
          , residualSegments = memoryToSegmentList (memory discoveryState)
          }
  let blocks = concatMap fnBlocks (recoveredDefs recoveredModule)
  let blockSeg (b :: FnBlock X86_64) =
        let addr = fnBlockLabelAddr (fbLabel b)
         in case Mem.segoffAsAbsoluteAddr addr of
              Nothing ->
                inclusiveRangeFromBaseAndSize
                  (Mem.memWordValue (Mem.segmentOffset (Mem.segoffSegment addr)) + Mem.memWordValue (Mem.segoffOffset addr))
                  (fbSize b)
              Just w -> inclusiveRangeFromBaseAndSize (Mem.memWordValue w) (fbSize b)
  let blockSegs = map blockSeg blocks
  return $ foldl registerAsBlockSegment allMemorySegments blockSegs

displayResiduals :: ResidualOptions -> PartitionedSegments -> [ResidualRangeInfo] -> IO ()
displayResiduals residualOpts parts residualInfos = do
  let (explained, unexplained) = partition (isJust . rriExplanation) residualInfos
  let whichDisplayResiduals
        | roOutputForSpreadsheet residualOpts = displayResidualsForSpreadsheet
        | otherwise = displayResidualsForHuman
  let footprints =
        Footprints
          { blocksFootprint = segmentsFootprint (blockSegments parts)
          , explainedFootprint = segmentsFootprint $ map rriRange explained
          , unexplainedFootprint = segmentsFootprint $ map rriRange unexplained
          }
  putStrLn $ whichDisplayResiduals residualInfos footprints

runResidual :: Options -> ResidualOptions -> ReoptOptions -> IO ()
runResidual _opts residualOpts reoptOpts = do
  files <- findAllElfFilesInDirs (roPaths residualOpts)
  forM_ files $ \file -> do
    (hdrInfo, discoveryState, recovOut) <- performRecovery residualOpts reoptOpts file
    let mem = memory discoveryState
    partitionedSegs <- computeResidualSegments discoveryState $ recoveredModule recovOut
    let residualInfos = constructResidualRangeInfos hdrInfo mem recovOut partitionedSegs
    displayResiduals residualOpts partitionedSegs residualInfos

segmentsFootprint :: [Segment] -> Word64
segmentsFootprint = sum . map (uncurry subtract . getInclusiveRange)

memoryToSegmentList :: Memory 64 -> [InclusiveRange Word64]
memoryToSegmentList m = map segBounds esegs
 where
  esegs = filter (Perm.isExecutable . Mem.segmentFlags) (Mem.memSegments m)
  -- assumes sorted, non-overlapping
  segBounds eseg =
    inclusiveRangeFromBaseAndSize
      (Mem.memWordValue (Mem.segmentOffset eseg))
      (Mem.memWordValue (Mem.segmentSize eseg))

--------------------------------------------------------------------------------
-- Segment lists

type Segment = InclusiveRange Word64

displayResidualsForHuman :: [ResidualRangeInfo] -> Footprints -> String
displayResidualsForHuman ranges fps =
  let
    blocks = blocksFootprint fps
    explained = explainedFootprint fps
    unexplained = unexplainedFootprint fps
   in
    unlines $
      [ "Blocks footprint:        " <> show blocks <> " bytes"
      , "Residual footprint:      " <> show (explained + unexplained) <> " bytes"
      , "→ Explained footprint:   " <> show explained <> " bytes"
      , "→ Unexplained footprint: " <> show unexplained <> " bytes"
      ]
        ++ map ppResidualRange ranges
 where
  ppResidualRange info =
    let InclusiveRange (l, u) = rriRange info
     in unlines
          [ "0x"
              ++ showHex l ""
              ++ " -- "
              ++ "0x"
              ++ showHex u ""
              ++ maybe "" ((" (" ++) . (++ ")")) (rriSymbolName info)
              ++ ":"
              ++ maybe "" (((" (" ++) . (++ ")")) . ppResidualExplanation) (rriExplanation info)
              ++ ":"
          , -- Temporarily disabling this, only useful for investigative purposes
            if False then maybe "" (ppDisSegment l) (rriInstructions info) else ""
          ]

-- | Pretty-prints a disassembled instruction, taking the block offset so as to
-- print the instruction address.
ppDisInstr :: Int -> DisassembledAddr -> String
ppDisInstr ofs da =
  offset <> " " <> instr da
 where
  offset = printf "0x%08x" (ofs + disOffset da)
  instr =
    maybe
      "???\n"
      (renderString . PP.layoutCompact . (<> PP.hardline) . ppInstruction)
      . disInstruction

-- | Pretty-prints a disassembled block
ppDisSegment :: Word64 -> [DisassembledAddr] -> String
ppDisSegment ofs = concatMap (ppDisInstr (fromIntegral ofs))

-- | Splits one segment so that no remaining segment overlaps the addresses.
splitSegmentAtAddresses :: [Word64] -> Segment -> [Segment]
splitSegmentAtAddresses addrs seg@(getInclusiveRange -> (lo, hi)) =
  case find (inInclusiveRange (InclusiveRange (lo + 1, hi))) addrs of
    Nothing -> [seg]
    Just split -> InclusiveRange (lo, split - 1) : splitSegmentAtAddresses addrs (InclusiveRange (split, hi))

-- | Splits a list of segments into a more fine-grained list of segments, based
-- on the list of addresses to split at.  This allows us to cut large residual
-- ranges along the addresses of known symbols, so that residual analysis can be
-- more precise.
splitAtAddresses :: [Word64] -> [Segment] -> [Segment]
splitAtAddresses addrs segs =
  [ final_seg
  | initial_seg <- segs
  , final_seg <- splitSegmentAtAddresses addrs initial_seg
  ]

-- >>> splitAtAddresses [2, 5, 7] (map InclusiveRange [(1, 5), (6, 7)])
-- [0x1 - 0x1,0x2 - 0x4,0x5 - 0x5,0x6 - 0x6,0x7 - 0x7]

-- | @registerAsBlockSegment p b@ registers block segment `s` as part of
-- partition `p`, tracking the remaining residual blocks.
--
-- Assumption 1: the residual segment list is sorted.
--
-- Assumption 2: the new block does not overlap with previously-registered
-- blocks.
registerAsBlockSegment :: PartitionedSegments -> InclusiveRange Word64 -> PartitionedSegments
registerAsBlockSegment part block@(getInclusiveRange -> (l, u)) =
  let
    sl = map getInclusiveRange (residualSegments part)
    (ls, us) = span (\(_, u') -> u' < l) sl
   in
    PartitionedSegments
      { blockSegments = block : blockSegments part
      , residualSegments = map InclusiveRange $ ls ++ splitSegs us
      }
 where
  splitSegs [] = []
  -- l <= u', we have the following cases, assume R is the
  -- region to delete and C is the current region
  --
  --  1. R covers C, so forget C and delete R from the rest
  --  2. R covers the beginning of C, so delete that part of C and return (rest is untouched)
  --  3. R covers the end of C, so add the bit of C at the start and process the rest
  -- 4. R is contained within C, so split C and return rest.
  splitSegs ((l', u') : rest)
    | l <= l', u' <= u = splitSegs rest
    | l <= l' = (u + 1, u') : rest
    | u' <= u = (l', l - 1) : splitSegs rest
    -- l > l', u' > u
    | otherwise = (l', l - 1) : (u + 1, u') : rest

-- >>> registerAsBlockSegment (PartitionedSegments [] [InclusiveRange (0, 10)]) (InclusiveRange (3, 5))
-- PartitionedSegments {blockSegments = [0x3 - 0x5], residualSegments = [0x0 - 0x2,0x6 - 0xa]}

chunkBytes :: MemChunk 64 -> Maybe BSC.ByteString
chunkBytes = \case
  Mem.ByteRegion bs -> Just bs
  _ -> error "chunkBytes: not a ByteRegion"

-- chunksBytes :: [MemChunk 64] -> Maybe [BSC.ByteString]
-- chunksBytes = traverse chunkBytes

-- segmentBytes :: Memory 64 -> (Word64, Word64) -> Maybe BSC.ByteString
-- segmentBytes m (l, u) = do
--   ofs <- resolveAbsoluteAddr m (memWord l)
--   case segoffContentsAfter ofs of
--     Right [mc] ->
--       BSC.take (fromInteger $ toInteger $ u - l + 1) <$> chunkBytes mc
--     _ -> Nothing

segmentInstrs :: Memory 64 -> InclusiveRange Word64 -> Maybe [DisassembledAddr]
segmentInstrs m r = do
  ofs <- Mem.resolveAbsoluteAddr m (Mem.memWord (rangeLowerBound r))
  case Mem.segoffContentsAfter ofs of
    Right chunks -> do
      let seg = Mem.forcedTakeMemChunks chunks (Mem.memWord (rangeSize r))
      concat <$> traverse (fmap disassembleBuffer . chunkBytes) seg
    _ -> Nothing

symbolAtAddress :: Maybe (Symtab 64) -> Word64 -> Maybe String
symbolAtAddress mSymTab addr =
  let symbolEntryForSegment symTab = Vec.find ((== addr) . steValue) $ symtabEntries @64 symTab
   in let symbolForSegment = fmap (BSC.unpack . steName) . symbolEntryForSegment
       in symbolForSegment =<< mSymTab

data ResidualRangeInfo = ResidualRangeInfo
  { rriRange :: InclusiveRange Word64
  , rriFootprint :: Word64
  -- ^ Just the difference between the bounds of the range, for convenience
  , rriSection :: Maybe (Shdr BSC.ByteString Word64)
  , rriSymbolName :: Maybe String
  , rriInstructions :: Maybe [DisassembledAddr]
  , rriExplanation :: Maybe ResidualExplanation
  }

classifyResidual ::
  RecoverX86Output ->
  InclusiveRange Word64 ->
  Maybe [DisassembledAddr] ->
  Maybe ResidualExplanation
classifyResidual recovOut range instrs =
  case classifyInstrs =<< instrs of
    Just expl -> Just expl
    Nothing ->
      let
        failures = Map.toList $ summaryFailures recovOut
        overlaps (k, _) = maybe False (inInclusiveRange range . Mem.memWordValue) (Mem.segoffAsAbsoluteAddr k)
       in
        BecauseFailure . snd <$> find overlaps failures

constructResidualRangeInfos ::
  ElfHeaderInfo 64 ->
  Memory 64 ->
  RecoverX86Output ->
  PartitionedSegments ->
  [ResidualRangeInfo]
constructResidualRangeInfos hdrInfo mem recovOut (residualSegments -> residuals) =
  let
    shdrs = fromRight (error "shdrs") $ headerNamedShdrs hdrInfo
    rangedShdrs = mapMaybe rangedShdr (Vec.toList shdrs)
    mSymTab = either (error . show) id <$> decodeHeaderSymtab hdrInfo
    allSymbols = map steValue . Vec.toList . symtabEntries @64 <$> mSymTab
    fineGrainedResiduals = splitAtAddresses (concat allSymbols) residuals
   in
    flip map fineGrainedResiduals $ \r ->
      let rriInstructions = segmentInstrs mem r
       in let lo = rangeLowerBound r
           in ResidualRangeInfo
                { rriRange = r
                , rriFootprint = rangeSize r
                , rriSection = snd <$> find ((`inInclusiveRange` lo) . fst) rangedShdrs
                , rriSymbolName = symbolAtAddress mSymTab lo
                , rriInstructions
                , rriExplanation = classifyResidual recovOut r rriInstructions
                }

data Footprints = Footprints
  { blocksFootprint :: Word64
  , explainedFootprint :: Word64
  , unexplainedFootprint :: Word64
  }

displayResidualsForSpreadsheet :: [ResidualRangeInfo] -> Footprints -> String
displayResidualsForSpreadsheet residualInfos fps =
  let
    blocks = blocksFootprint fps
    explained = explainedFootprint fps
    unexplained = unexplainedFootprint fps
   in
    unlines $
      [ "Discovered " <> show blocks
      , "Residuals " <> show (explained + unexplained)
      , "Explained " <> show explained
      , "Unexplained " <> show unexplained
      ]
        ++ map ppResidualInfo residualInfos
 where
  appendResidualExplanation e = " " <> ppResidualExplanation e
  appendSymbol s = " (" <> s <> ")"
  appendSectionName shdr = " [" <> BSC.unpack (shdrName shdr) <> "]"
  ppResidualInfo info =
    ppInclusiveRange (rriRange info)
      <> printf " %5dB" (rriFootprint info)
      <> maybe "" appendSectionName (rriSection info)
      <> maybe "" appendSymbol (rriSymbolName info)
      <> maybe "" appendResidualExplanation (rriExplanation info)
