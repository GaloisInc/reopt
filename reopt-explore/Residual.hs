{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Residual (runResidual) where

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

import           CommandLine
import           Common
import           Data.Either                   (fromRight)
import           Data.ElfEdit                  (Symtab (symtabEntries),
                                                SymtabEntry (steName, steValue),
                                                decodeHeaderSymtab)
import           Flexdis86                     (DisassembledAddr (disInstruction, disOffset),
                                                disassembleBuffer)
import           Flexdis86.InstructionSet      (ppInstruction)
import           Text.PrettyPrint.ANSI.Leijen  (displayS, renderCompact)
import           Text.Printf                   (printf)


runResidual :: Options -> ResidualOptions -> ReoptOptions -> IO ()
runResidual _opts gopts ropts = do
  files <- findAllElfFilesInDirs (roPaths gopts)
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
            | roVerboseMode ropts =
              joinLogEvents printLogEvent (recoverLogEvent summaryRef statsRef)
            | otherwise =
              recoverLogEvent summaryRef statsRef
      annDecl <- fromRight (error "Could not resolve header") <$> runReoptM printLogEvent
                  (resolveHeader (roHeader gopts) (roClangPath gopts))
      hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs
      let mSymTab = either (error . show) id <$> decodeHeaderSymtab hdrInfo
      (_os, ds, recMod, _, _, _logEvents) <-
        handleEitherWithExit =<<
          runReoptM logger
            (recoverX86Elf lOpts ropts annDecl unnamedFunPrefix hdrInfo)

      let sl = memoryToSegmentList (memory ds)
          blocks = concatMap fnBlocks (recoveredDefs recMod)
          blockSeg (b :: FnBlock X86_64) =
            case segoffAsAbsoluteAddr (fnBlockLabelAddr (fbLabel b)) of
              Nothing -> error "relative block"
              Just w  -> (memWordValue w, memWordValue w + fbSize b - 1)
          blockSegs = map blockSeg blocks
          residuals = foldl removeSegment sl blockSegs

      putStrLn (ppSegmentList mSymTab (memory ds) residuals)
      pure ()

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

data ResidualExplanation
  = NopPadding

ppResidualExplanation :: ResidualExplanation -> String
ppResidualExplanation = \case
  NopPadding -> "NOP padding"

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
      -- , maybe ":-(" byteStringtoHexString (segmentBytes m s)
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
