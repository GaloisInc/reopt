{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Residual (runResidual) where

import           Control.Monad                 (forM_)
import qualified Data.ByteString.Char8         as BSC
import           Data.IORef                    (newIORef)
import qualified Data.Map                      as Map
import qualified Data.Vector                   as Vec
import           Data.Word                     (Word64)
import           Debug.Trace                   (trace)
import           Numeric                       (showHex)

import           Data.Macaw.Discovery          (DiscoveryState (memory))
import           Data.Macaw.Memory             (MemChunk (ByteRegion),
                                                MemSegment (segmentBase, segmentFlags, segmentOffset),
                                                MemWord (memWordValue), Memory,
                                                forcedTakeMemChunks,
                                                memSegments, memWord,
                                                resolveAbsoluteAddr,
                                                segmentSize,
                                                segoffAsAbsoluteAddr,
                                                segoffContentsAfter)
import qualified Data.Macaw.Memory.Permissions as Perm

import           Reopt                         (LoadOptions (LoadOptions),
                                                RecoverX86Output (recoveredModule, summaryFailures),
                                                ReoptOptions, X86_64,
                                                loadOffset,
                                                parseElfHeaderInfo64,
                                                recoverX86Elf, resolveHeader,
                                                roVerboseMode, runReoptM)
import           Reopt.CFG.FnRep               (FnBlock, RecoveredModule,
                                                fbLabel, fbSize,
                                                fnBlockLabelAddr, fnBlocks,
                                                recoveredDefs)
import           Reopt.Events                  (ReoptLogEvent, initReoptSummary,
                                                joinLogEvents, printLogEvent,
                                                recoverLogEvent)
import           Reopt.Utils.Exit              (checkedReadFile,
                                                handleEitherStringWithExit,
                                                handleEitherWithExit)

import           CommandLine                   (Options,
                                                ResidualOptions (roClangPath, roHeader, roOutputForSpreadsheet, roPaths))
import           Common                        (findAllElfFilesInDirs)
import           Data.Either                   (fromRight)
import           Data.ElfEdit                  (ElfHeaderInfo,
                                                Shdr (shdrAddr, shdrName, shdrSize),
                                                Symtab (symtabEntries),
                                                SymtabEntry (steName, steValue),
                                                decodeHeaderSymtab,
                                                headerNamedShdrs)
import           Data.List                     (find, partition)
import           Data.Maybe                    (isJust, mapMaybe)
import           Flexdis86                     (DisassembledAddr (disInstruction, disOffset),
                                                disassembleBuffer)
import           Flexdis86.InstructionSet      (ppInstruction)
import           Residual.Recognizers          (ResidualExplanation (BecauseFailure),
                                                classifyInstrs,
                                                ppResidualExplanation)
import           Text.PrettyPrint.ANSI.Leijen  (displayS, renderCompact)
import           Text.Printf                   (printf)

newtype InclusiveRange w = InclusiveRange { getInclusiveRange :: (w, w) }

ppInclusiveRange :: (Integral w, Show w) => InclusiveRange w -> String
ppInclusiveRange (InclusiveRange (lo, hi)) = showHex lo "" <> " - " <> showHex hi ""

shdrInclusiveRange :: (Eq w, Ord w, Num w) => Shdr nm w -> Maybe (InclusiveRange w)
shdrInclusiveRange s =
  let addr = shdrAddr s in
  if addr == 0 || shdrSize s == 0
    then Nothing
    else Just $ InclusiveRange (shdrAddr s, shdrAddr s + shdrSize s - 1)

inInclusiveRange :: (Num w, Ord w, Show w) => InclusiveRange w -> w -> Bool
inInclusiveRange (InclusiveRange (lo, hi)) v = lo <= v && v <= hi

type RangedShdr nm w = (InclusiveRange w, Shdr nm w)

rangedShdr :: (Num w, Ord w) => Shdr nm w -> Maybe (RangedShdr nm w)
rangedShdr s = (, s) <$> shdrInclusiveRange s

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
  let lOpts = LoadOptions {loadOffset = Nothing}
  let unnamedFunPrefix = BSC.pack "reopt"
  -- hPutStrLn stderr $ "[" ++ (show index) ++ " of " ++ (show totalCount)
  --                    ++ "] Analyzing " ++ fPath ++ " ..."
  bs <- checkedReadFile fPath
  annDecl <-
    runReoptM printLogEvent
      (resolveHeader (roHeader residualOpts) (roClangPath residualOpts)) >>=
        either (error . show) return
  hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs
  logger <- createLogger reoptOpts fPath
  (_os, ds, recovOut, _) <-
    handleEitherWithExit =<<
       runReoptM logger
         (recoverX86Elf lOpts reoptOpts annDecl unnamedFunPrefix hdrInfo)
  return (hdrInfo, ds, recovOut)

computeResidualSegments ::
  DiscoveryState X86_64 ->
  RecoveredModule X86_64 ->
  IO [Segment]
computeResidualSegments discoveryState recoveredModule = do
  let allMemorySegments = memoryToSegmentList (memory discoveryState)
  let blocks = concatMap fnBlocks (recoveredDefs recoveredModule)
  let blockSeg (b :: FnBlock X86_64) =
        let addr = fnBlockLabelAddr (fbLabel b) in
        case segoffAsAbsoluteAddr addr of
          Nothing -> trace ("Ignoring relative block: " <> show addr) Nothing
          Just w  -> Just (memWordValue w, memWordValue w + fbSize b - 1)
  let blockSegs = mapMaybe blockSeg blocks
  return $ foldl removeSegment allMemorySegments blockSegs

displayResiduals :: ResidualOptions -> [Segment] -> [ResidualRangeInfo] -> IO ()
displayResiduals residualOpts residuals residualInfos = do
  let (explained, unexplained) = partition (isJust . rriExplanation) residualInfos
  let residualFootprint = segmentsFootprint residuals
  let explainedFootprint = segmentsFootprint $ map (getInclusiveRange . rriRange) explained
  let unexplainedFootprint = segmentsFootprint $ map (getInclusiveRange . rriRange) unexplained
  let whichDisplayResiduals
        | roOutputForSpreadsheet residualOpts = displayResidualsForSpreadsheet
        | otherwise = displayResidualsForHuman
  putStrLn $ whichDisplayResiduals residualInfos residualFootprint explainedFootprint unexplainedFootprint

runResidual :: Options -> ResidualOptions -> ReoptOptions -> IO ()
runResidual _opts residualOpts reoptOpts = do
  files <- findAllElfFilesInDirs (roPaths residualOpts)
  forM_ files $ \ file -> do
    (hdrInfo, discoveryState, recovOut) <- performRecovery residualOpts reoptOpts file
    let mem = memory discoveryState
    residuals <- computeResidualSegments discoveryState $ recoveredModule recovOut
    let residualInfos = constructResidualRangeInfos hdrInfo mem recovOut residuals
    displayResiduals residualOpts residuals residualInfos

segmentsFootprint :: [Segment] -> Word64
segmentsFootprint = sum . map (uncurry subtract)

memoryToSegmentList :: Memory 64 -> [(Word64, Word64)]
memoryToSegmentList m = mapMaybe segBounds esegs
  where
    esegs = filter (Perm.isExecutable . segmentFlags) (memSegments m)
    -- assumes sorted, non-overlapping
    segBounds eseg
      | segmentBase eseg /= 0 = trace ("Ignoring segment with non-zero base: " <> show eseg) Nothing
      | otherwise = Just
                    ( memWordValue (segmentOffset eseg)
                    , memWordValue (segmentOffset eseg)
                      + memWordValue (segmentSize eseg)
                      - 1
                    )

--------------------------------------------------------------------------------
-- Segment lists

type Segment = (Word64, Word64)

-- byteStringtoHexString :: BSC.ByteString -> String
-- byteStringtoHexString = BSC.foldr ((<>) . printf "%02x") ""

displayResidualsForHuman :: [ResidualRangeInfo] -> Word64 -> Word64 -> Word64 -> String
displayResidualsForHuman ranges residuals explained unexplained =
  unlines $
    [ "Residual footprint:      " <> show residuals <> " bytes"
    , "→ Explained footprint:   " <> show explained <> " bytes"
    , "→ Unexplained footprint: " <> show unexplained <> " bytes"
    ]
    ++ map ppResidualRange ranges
  where
    ppResidualRange info =
      let InclusiveRange (l, u) = rriRange info in
      unlines
      [ "0x" ++ showHex l "" ++ " -- " ++ "0x" ++ showHex u ""
        ++ maybe "" ((" (" ++) . (++ ")")) (rriSymbolName info) ++ ":"
        ++ maybe "" (((" (" ++) . (++ ")")) . ppResidualExplanation) (rriExplanation info) ++ ":"
        -- Temporarily disabling this, only useful for investigative purposes
      , if False then maybe "" (ppDisSegment l) (rriInstructions info) else ""
      ]

-- | Pretty-prints a disassembled instruction, taking the block offset so as to
-- print the instruction address.
ppDisInstr :: Int -> DisassembledAddr -> String
ppDisInstr ofs da =
  offset <> " " <> instr da
  where
    offset = printf "0x%08x" (ofs + disOffset da)
    instr = maybe "???\n" (($ "\n") . displayS . renderCompact . ppInstruction) . disInstruction

-- | Pretty-prints a disassembled block
ppDisSegment :: Word64 -> [DisassembledAddr] -> String
ppDisSegment ofs = concatMap (ppDisInstr (fromIntegral ofs))

-- | Splits one segment so that no remaining segment overlaps the addresses.
splitSegmentAtAddresses :: [Word64] -> Segment -> [Segment]
splitSegmentAtAddresses addrs seg@(lo, hi) =
  case find (inInclusiveRange (InclusiveRange (lo + 1, hi))) addrs of
    Nothing    -> [seg]
    Just split -> (lo, split - 1) : splitSegmentAtAddresses addrs (split, hi)

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

-- >>> splitAtAddresses [2, 5, 7] [(1, 5), (6, 7)]
-- [(1,1),(2,4),(5,5),(6,6),(7,7)]

-- NOTE: assumes a sorted segment list
removeSegment :: [Segment] -> (Word64, Word64) -> [Segment]
removeSegment sl (l, u) = ls ++ splitSegs us
  where
    (ls, us) = span (\(_, u') -> u' < l) sl
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
      | l <= l'          = (u + 1, u') : rest
      | u' <= u          = (l', l - 1) : splitSegs rest
      -- l > l', u' > u
      | otherwise        = (l', l - 1) : (u + 1, u') : rest

chunkBytes :: MemChunk 64 -> Maybe BSC.ByteString
chunkBytes = \case
  ByteRegion bs -> Just bs
  _             -> error "chunkBytes: not a ByteRegion"

-- chunksBytes :: [MemChunk 64] -> Maybe [BSC.ByteString]
-- chunksBytes = traverse chunkBytes

-- segmentBytes :: Memory 64 -> (Word64, Word64) -> Maybe BSC.ByteString
-- segmentBytes m (l, u) = do
--   ofs <- resolveAbsoluteAddr m (memWord l)
--   case segoffContentsAfter ofs of
--     Right [mc] ->
--       BSC.take (fromInteger $ toInteger $ u - l + 1) <$> chunkBytes mc
--     _ -> Nothing

segmentInstrs :: Memory 64 -> (Word64, Word64) -> Maybe [DisassembledAddr]
segmentInstrs m (l, u) = do
  ofs <- resolveAbsoluteAddr m (memWord l)
  case segoffContentsAfter ofs of
    Right chunks -> do
      let seg = forcedTakeMemChunks chunks (memWord $ u - l + 1)
      concat <$> traverse (fmap disassembleBuffer . chunkBytes) seg
    _ -> Nothing

symbolAtAddress :: Maybe (Symtab 64) -> Word64 -> Maybe String
symbolAtAddress mSymTab addr =
  let symbolEntryForSegment symTab = Vec.find ((== addr) . steValue) $ symtabEntries @64 symTab in
  let symbolForSegment = fmap (BSC.unpack . steName) . symbolEntryForSegment in
  symbolForSegment =<< mSymTab

data ResidualRangeInfo = ResidualRangeInfo
  { rriRange        :: InclusiveRange Word64
  , -- | Just the difference between the bounds of the range, for convenience
    rriFootprint    :: Word64
  , rriSection      :: Maybe (Shdr BSC.ByteString Word64)
  , rriSymbolName   :: Maybe String
  , rriInstructions :: Maybe [DisassembledAddr]
  , rriExplanation  :: Maybe ResidualExplanation
  }

classifyResidual ::
  RecoverX86Output ->
  InclusiveRange Word64 ->
  Maybe [DisassembledAddr] ->
  Maybe ResidualExplanation
classifyResidual recovOut range instrs =
  case classifyInstrs =<< instrs of
    Just expl -> Just expl
    Nothing   ->
      let
        failures = Map.toList $ summaryFailures recovOut
        overlaps (k, _) = maybe False (inInclusiveRange range . memWordValue) (segoffAsAbsoluteAddr k)
      in
      BecauseFailure . snd <$> find overlaps failures

constructResidualRangeInfos ::
  ElfHeaderInfo 64 ->
  Memory 64 ->
  RecoverX86Output ->
  [Segment] ->
  [ResidualRangeInfo]
constructResidualRangeInfos hdrInfo mem recovOut residuals =
  let
    shdrs = fromRight (error "shdrs") $ headerNamedShdrs hdrInfo
    rangedShdrs = mapMaybe rangedShdr (Vec.toList shdrs)
    mSymTab = either (error . show) id <$> decodeHeaderSymtab hdrInfo
    allSymbols = map steValue . Vec.toList . symtabEntries @64 <$> mSymTab
    fineGrainedResiduals = splitAtAddresses (concat allSymbols) residuals
  in
  flip map fineGrainedResiduals $ \ range@(lo, hi) ->
    let rriInstructions = segmentInstrs mem range in
    ResidualRangeInfo {
      rriRange = InclusiveRange range,
      rriFootprint = hi - lo,
      rriSection = snd <$> find ((`inInclusiveRange` lo) . fst) rangedShdrs,
      rriSymbolName = symbolAtAddress mSymTab lo ,
      rriInstructions,
      rriExplanation = classifyResidual recovOut (InclusiveRange range) rriInstructions
    }

displayResidualsForSpreadsheet :: [ResidualRangeInfo] -> Word64 -> Word64 -> Word64 -> String
displayResidualsForSpreadsheet residualInfos residuals explained unexplained =
  unlines
    $
    [ "Residuals " <> show residuals
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
