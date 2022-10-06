{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Residual (runResidual) where

import qualified Data.ByteString.Char8 as BSC
import           Data.IORef            (newIORef)
import           Data.List             (intersperse)
import           Data.Word             (Word64)
import           Numeric               (showHex)

import           Data.Macaw.Discovery          (DiscoveryState (memory))
import           Data.Macaw.Memory             (MemSegment (segmentBase),
                                                Memory, 
                                                memSegments, memWordValue,
                                                segmentFlags, segmentOffset,
                                                segmentSize,
                                                segoffAsAbsoluteAddr)
import qualified Data.Macaw.Memory.Permissions as Perm

import           Reopt                         (LoadOptions (LoadOptions),
                                                ReoptOptions, X86_64,
                                                emptyAnnDeclarations,
                                                loadOffset,
                                                parseElfHeaderInfo64,
                                                recoverX86Elf, roVerboseMode,
                                                runReoptM)
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
      let annDecl = emptyAnnDeclarations
      hdrInfo <- handleEitherStringWithExit $ parseElfHeaderInfo64 fPath bs
      (_os, ds, recMod, _, _, _logEvents) <-
        handleEitherWithExit =<<
          (runReoptM logger $
            recoverX86Elf lOpts ropts annDecl unnamedFunPrefix hdrInfo)

      let sl = memoryToSegmentList (memory ds)
          blocks = concatMap fnBlocks (recoveredDefs recMod)
          blockSeg (b :: FnBlock X86_64) =
            case segoffAsAbsoluteAddr (fnBlockLabelAddr (fbLabel b)) of
              Nothing -> error "relative block"
              Just w  -> (memWordValue w, memWordValue w + fbSize b - 1)
          blockSegs = map blockSeg blocks
          residuals = foldl removeSegment sl blockSegs
          
      putStrLn (ppSegmentList residuals)
      pure ()

memoryToSegmentList :: Memory 64 -> SegmentList
memoryToSegmentList m = SegmentList (map segBounds esegs)
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

newtype SegmentList = SegmentList { segments :: [(Word64, Word64)] }

ppSegmentList :: SegmentList -> String
ppSegmentList sl =
  "[" ++ concat (intersperse " " [ "0x" ++ showHex l "" ++ "--" ++ "0x" ++ showHex u ""
                                 | (l, u) <- segments sl ]) ++ "]"

-- segmentList :: Word64 -> Word64 -> SegmentList
-- segmentList l u = SegmentList [(l, u)]

removeSegment :: SegmentList -> (Word64, Word64) -> SegmentList
removeSegment sl (l, u) = SegmentList (ls ++ splitSegs us)
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
