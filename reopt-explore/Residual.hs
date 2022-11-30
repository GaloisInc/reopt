{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Residual (runResidual) where

import           Control.Monad                 (forM_)
import qualified Data.ByteString.Char8         as BSC
import           Data.IORef                    (newIORef)
import qualified Data.Vector                   as Vec
import           Data.Word                     (Word64)
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
                                                ReoptOptions, X86_64,
                                                loadOffset,
                                                parseElfHeaderInfo64,
                                                recoverX86Elf, resolveHeader,
                                                roVerboseMode, runReoptM)
import           Reopt.CFG.FnRep               (FnBlock, fbLabel, fbSize,
                                                fnBlockLabelAddr, fnBlocks,
                                                recoveredDefs)
import           Reopt.Events                  (initReoptSummary, joinLogEvents,
                                                printLogEvent, recoverLogEvent)
import           Reopt.Utils.Exit              (checkedReadFile,
                                                handleEitherStringWithExit,
                                                handleEitherWithExit)

import           CommandLine                   (Options,
                                                ResidualOptions (roClangPath, roHeader, roOutputForSpreadsheet, roPaths))
import           Common                        (findAllElfFilesInDirs)
import           Data.ElfEdit                  (Shdr (shdrAddr, shdrName, shdrSize),
                                                Symtab (symtabEntries),
                                                SymtabEntry (steName, steValue),
                                                decodeHeaderSymtab,
                                                headerNamedShdrs)
import           Data.List                     (find)
import           Data.Maybe                    (mapMaybe)
-- import           Debug.Trace                   (trace)
import           Flexdis86                     (DisassembledAddr (disInstruction, disOffset),
                                                disassembleBuffer)
import           Flexdis86.InstructionSet      (ppInstruction)
import           Text.PrettyPrint.ANSI.Leijen  (displayS, renderCompact)
import           Text.Printf                   (printf)

import           Residual.Recognizers          (ResidualExplanation,
                                                classifyInstrs,
                                                ppResidualExplanation)

newtype InclusiveRange w = InclusiveRange (w, w)

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

runResidual :: Options -> ResidualOptions -> ReoptOptions -> IO ()
runResidual _opts residualOpts reoptOpts = do
  files <- findAllElfFilesInDirs (roPaths residualOpts)
  mapM_ performRecovery files
  where
    lOpts = LoadOptions {loadOffset = Nothing}
    unnamedFunPrefix = BSC.pack "reopt"
    performRecovery (_idx, fPath) = do
      -- hPutStrLn stderr $ "[" ++ (show index) ++ " of " ++ (show totalCount)
      --                    ++ "] Analyzing " ++ fPath ++ " ..."
      bs <- checkedReadFile fPath
      summaryRef <- newIORef $ initReoptSummary fPath
      statsRef <- newIORef mempty
      let logger
            | roVerboseMode reoptOpts =
              joinLogEvents printLogEvent (recoverLogEvent summaryRef statsRef)
            | otherwise =
              recoverLogEvent summaryRef statsRef
      annDecl <-
        runReoptM printLogEvent
          (resolveHeader (roHeader residualOpts) (roClangPath residualOpts)) >>=
            either (error . show) return
      hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs
      let Right shdrs = headerNamedShdrs hdrInfo
      let rangedShdrs = mapMaybe rangedShdr (Vec.toList shdrs)
      let mSymTab = either (error . show) id <$> decodeHeaderSymtab hdrInfo
      (_os, ds, recMod, _, _, _logEvents) <-
        handleEitherWithExit =<<
          runReoptM logger
            (recoverX86Elf lOpts reoptOpts annDecl unnamedFunPrefix hdrInfo)

      let sl = memoryToSegmentList (memory ds)
          blocks = concatMap fnBlocks (recoveredDefs recMod)
          blockSeg (b :: FnBlock X86_64) =
            case segoffAsAbsoluteAddr (fnBlockLabelAddr (fbLabel b)) of
              Nothing -> error "relative block"
              Just w  -> (memWordValue w, memWordValue w + fbSize b - 1)
          blockSegs = map blockSeg blocks
          residuals = foldl removeSegment sl blockSegs

      if roOutputForSpreadsheet residualOpts
        then outputForSpreadsheet mSymTab (memory ds) rangedShdrs residuals
        else putStrLn (ppSegmentList mSymTab (memory ds) residuals)

memoryToSegmentList :: Memory 64 -> SegmentList
memoryToSegmentList m = SegmentList (map segBounds esegs) []
  where
    esegs = filter (Perm.isExecutable . segmentFlags) (memSegments m)
    -- assumes sorted, non-overlapping
    segBounds eseg
      | segmentBase eseg /= 0 = error "Segment has non-zero base"
      | otherwise = ( memWordValue (segmentOffset eseg)
                    , memWordValue (segmentOffset eseg)
                      + memWordValue (segmentSize eseg)
                      - 1
                    )

--------------------------------------------------------------------------------
-- Segment lists

data SegmentList = SegmentList
  { segments  :: [(Word64, Word64)]
  , explained :: [((Word64, Word64), ResidualExplanation)]
  }

-- byteStringtoHexString :: BSC.ByteString -> String
-- byteStringtoHexString = BSC.foldr ((<>) . printf "%02x") ""

ppSegmentList :: Maybe (Symtab 64) -> Memory 64 -> SegmentList -> String
ppSegmentList mSymTab m sl = unlines $
     [ "Residual segments (unexplained):" ]
  ++ map ppSegment (segments sl)
  ++ [ "Residual segments (explained)" ]
  ++ map ppExplained (explained sl)
  where
    ppSegment s@(l, u) =
      let symbolEntryForSegment symTab = Vec.find ((== l) . steValue) $ symtabEntries @64 symTab in
      let symbolForSegment = fmap (BSC.unpack . steName) . symbolEntryForSegment in
      let symbolString = symbolForSegment =<< mSymTab in
      unlines
      [ "0x" ++ showHex l "" ++ " -- " ++ "0x" ++ showHex u ""
        ++ maybe "" ((" (" ++) . (++ ")")) symbolString ++ ":"
      , maybe "" (ppDisSegment l) (segmentInstrs m s)
      ]
    ppExplained (s, e) = ppSegment s ++ " (" ++ ppResidualExplanation e ++ ")"

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

removeSegment :: SegmentList -> (Word64, Word64) -> SegmentList
removeSegment sl (l, u) = SegmentList (ls ++ splitSegs us) []
  where
    (ls, us) = span (\(_, u') -> u' < l) (segments sl)
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

outputForSpreadsheet :: Maybe (Symtab 64) -> Memory 64 -> [RangedShdr BSC.ByteString Word64] -> SegmentList -> IO ()
outputForSpreadsheet mSymTab mem rangedShdrs residuals =
  forM_ (segments residuals) $ \ (lo, hi) -> do
    putStrLn
      $ ppInclusiveRange (InclusiveRange (lo, hi))
      <> maybe "" appendSymbol (symbolAtAddress mSymTab lo)
      <> maybe "" appendSectionName (find ((`inInclusiveRange` lo) . fst) rangedShdrs)
      <> maybe "" appendResidualExplanation (classifyInstrs =<< segmentInstrs mem (lo, hi))
      -- Print the length if needed:
      -- <> maybe "" ((" " <>) . show . length) (segmentInstrs mem (lo, hi))
  where
    appendResidualExplanation e = " " <> ppResidualExplanation e
    appendSymbol s = " (" <> s <> ")"
    appendSectionName (_, shdr) = " [" <> BSC.unpack (shdrName shdr) <> "]"
