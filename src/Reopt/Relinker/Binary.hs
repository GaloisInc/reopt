{-|
This module contains functions for inferring information from binary.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Reopt.Relinker.Binary
  ( ElfContentLayout
  , ElfRegion(..)
  , eclCodePhdrIndex
  , eclFileRegions
  , eclCodeRegions
  , SpecialRegion(..)
  , OffsetConstraint(..)
  , inferNextOff
  , FileSource(..)
  , FileOffsetMap
  , inferBinaryLayout
  ) where

import           Control.Monad (when, unless, forM_)
import           Control.Monad.Except ( ExceptT, runExceptT, throwError )
import           Control.Monad.Reader ( Reader, asks, runReader )
import           Control.Monad.State.Strict ( StateT, runStateT, gets, modify )
import           Data.Bits ( Bits((.&.)) )
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as Elf
import qualified Data.Map as Map
import           Data.Maybe ( isJust )
import qualified Data.Vector as V
import           Data.Word ( Word16, Word64 )
import           Numeric (showHex)
import           Text.Printf ( printf )

import         Reopt.Relinker.Constants ( pageSize )

-- | @enumCnt b c@ returns a list with @c@ enum values starting from @b@.
enumCnt :: (Enum e, Real r) => e -> r -> [e]
enumCnt e x = if x > 0 then e : enumCnt (succ e) (x-1) else []

hasSegmentType :: Elf.Phdr w -> Elf.PhdrType -> Bool
hasSegmentType p tp = Elf.phdrSegmentType p == tp

-------------------------------------------------------------------------------
-- Layout

-- | Special region
data SpecialRegion
   = Ehdr
   | PhdrTable
   | ShdrTable
   | Interpreter
   | Shstrtab
   | Strtab
   | Symtab
 deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- OffsetConstraint

-- | A constraint on file offsets (used for padding).
data OffsetConstraint
     -- | @LoadSegmentConstraint a@ indicates if a file offset
     -- @off@ should be aligned within a page to @a@.
     --
     -- i.e., @off mod pageSize = a@.
   = LoadSegmentConstraint !Int
     -- | @FileOffsetMultiple n@ indicates file offset should be a multiple of @n@.
   | FileOffsetMultiple !Int

  deriving (Eq, Show)

mkFileOffset :: Int -> OffsetConstraint
mkFileOffset i | i < 0 = error "Illegal offset"
               | i == 0 =  FileOffsetMultiple 1
               | otherwise = FileOffsetMultiple i

unconstrained :: OffsetConstraint
unconstrained =  FileOffsetMultiple 1

isUnconstrained :: OffsetConstraint -> Bool
isUnconstrained (FileOffsetMultiple o) = o `elem` [0, 1]
isUnconstrained (LoadSegmentConstraint _) = False

-- @checkSubsumption prev new@ checks that prev subsumes new.
checkSubsumption :: Int -> OffsetConstraint -> Int -> OffsetConstraint -> Maybe OffsetConstraint
checkSubsumption po (LoadSegmentConstraint p) oo (LoadSegmentConstraint o)
  | po == oo, p == o  = Just (LoadSegmentConstraint p)
  | otherwise = Nothing
checkSubsumption po (LoadSegmentConstraint p) oo (FileOffsetMultiple o)
  | o < pageSize, ((oo - po + p) `mod` pageSize) `rem` fromIntegral o == 0 = Just (LoadSegmentConstraint p)
  | otherwise = Nothing
checkSubsumption po ( FileOffsetMultiple p) oo (LoadSegmentConstraint o)
  | po == oo, o `rem` fromIntegral p == 0 = Just (LoadSegmentConstraint o)
  | otherwise = Nothing
checkSubsumption po l@(FileOffsetMultiple p) oo r@(FileOffsetMultiple o)
  | (oo - po + p) `rem` o == 0 = Just l
  | po == oo, o `rem` p == 0 = Just r
  | otherwise = Nothing

nextMultiple :: Int -> Int -> Int
nextMultiple off n = n * ((off + (n-1)) `div` n)

-- | Compute next offset given alignment constraint.
inferNextOff :: Int -> OffsetConstraint -> Int
inferNextOff off (FileOffsetMultiple n) = nextMultiple off n
inferNextOff off (LoadSegmentConstraint addr) = off + diff
  where mask = pageSize - 1
        diff = (pageSize + (addr .&. mask) - (off .&. mask)) .&. mask


-----------------------------------------------------------------------
-- FileSource

data FileSource
   = SrcSpecialRegion !SpecialRegion
   | Data -- ^ Data copied from original binary.
   | Code !Word64 -- ^ Code region with address.
 deriving (Eq)


instance Show FileSource where
  show (Code _) = "Code"
  show Data = "Data"
  show (SrcSpecialRegion r) = show r

-----------------------------------------------------------------------
-- ElfContentLayout

-- | Elf file content layout information used for generating
-- content.
data ElfContentLayout =
  ECL { eclSpecialRegionInLoad :: !(Map.Map SpecialRegion Bool)
        -- ^ Map from special region to load
      , eclFileOffsetMap :: !(Map.Map Int (Int, OffsetConstraint, FileSource))
        -- ^ Map from region file offsets to size, constraints on offsets and source.
      , eclCodePhdrIndex :: !Word16
      , eclCodeOffset :: !Int
      , eclCodeSize   :: !Int
      }


data ElfRegion = ElfRegion { eregOff  :: !Int
                           , eregSize :: !Int
                           , eregCns  :: !OffsetConstraint
                           , eregSrc  :: !FileSource
                           }


mkElfRegion :: (Int, (Int, OffsetConstraint, FileSource)) -> ElfRegion
mkElfRegion (off, (sz, cns, src)) = ElfRegion off sz cns src

eclFileRegions :: ElfContentLayout -> [ElfRegion]
eclFileRegions l = mkElfRegion <$> Map.toList (eclFileOffsetMap l)

-- | Return code regions associated with Elf content.
eclCodeRegions :: ElfContentLayout -> [ElfRegion]
eclCodeRegions l = mkElfRegion <$> Map.toList m
  where codeOff = eclCodeOffset l
        codeEnd = codeOff + eclCodeSize l
        m = Map.takeWhileAntitone (< codeEnd)
          $ Map.dropWhileAntitone (< codeOff)
          $ eclFileOffsetMap l

-----------------------------------------------------------------------
-- Functions for inferring ElfContentLayout

data InferMContext w = IMC { imcElf :: !(Elf.ElfHeaderInfo w) }

type FileOffsetMap = Map.Map Int (Int, OffsetConstraint, FileSource)


data InferMState = IMS { imsInLoad :: !(Map.Map Int Int)
                       , imsFileOffsetMap :: !FileOffsetMap
                       , imsCodeRegion :: !(Maybe Word16)
                         -- ^ Index of program header.
                       , imsSpecialRegionOff :: !(Map.Map SpecialRegion (Int, Int))
                         -- ^ Map from special regions to their index in file.
                       }

type InferM w = ExceptT String (StateT InferMState (Reader (InferMContext w)))

withElfClassInstances :: (Elf.ElfWidthConstraints w => InferM w a) -> InferM w a
withElfClassInstances m = do
  cl <- asks $ Elf.headerClass . Elf.header . imcElf
  Elf.elfClassInstances cl m

deleteFileOffsetMapEntry :: String -> Int -> OffsetConstraint -> InferM w ()
deleteFileOffsetMapEntry nm e cns = do
  when (isUnconstrained cns) $ do
    throwError $ nm ++ " overwrites previous constraint."
  modify $ \s -> s { imsFileOffsetMap = Map.delete e (imsFileOffsetMap s) }

setFileOffsetMapEntry :: Int -> Int -> OffsetConstraint -> FileSource -> InferM w ()
setFileOffsetMapEntry o sz cns src =
  modify $ \s -> s { imsFileOffsetMap = Map.insert o (sz, cns, src) (imsFileOffsetMap s) }

-- | Mark offset of special region
setSpecialRegionOff :: String -> SpecialRegion -> Int -> Int -> InferM w ()
setSpecialRegionOff nm reg off sz = do
  mIdx <- gets $ Map.lookup reg . imsSpecialRegionOff
  case mIdx of
    Nothing -> pure ()
    Just _ -> throwError $ nm ++ " already found."
  modify $ \s -> s { imsSpecialRegionOff = Map.insert reg (off, sz) (imsSpecialRegionOff s) }

-- | Mark a region of a file as loaded (and thus changes need to be carefully done)
markLoadRegion :: Int -> Int -> InferM w ()
markLoadRegion off0 sz0 = do
  let go s e m = do
        case Map.lookupLE e m of
          Just (prevOff, prevSize) | prevOff + prevSize >= s -> do
            go (min prevOff s) (max e (prevOff + prevSize)) (Map.delete prevOff m)
          _ -> Map.insert s (e-s) m
  modify $ \s -> s { imsInLoad = go off0 (off0+sz0) (imsInLoad s) }

insertAtomic :: String -> Int -> Int -> Int -> SpecialRegion -> InferM w ()
insertAtomic nm off sz fileCns src =  withElfClassInstances $ do
  m <- gets $ Map.lookupLT (off+sz) . imsFileOffsetMap
  case m of
    Just (prevOff, (prevSize, cns, Data))
      | prevEnd <- prevOff + prevSize
      , prevEnd > off -> do
      when (prevEnd > off+sz) $ do
        setFileOffsetMapEntry (off+sz) (prevEnd - (off+sz)) unconstrained Data
      if prevOff < off then do
        setFileOffsetMapEntry  prevOff (off-prevOff) cns Data
        setFileOffsetMapEntry off sz (mkFileOffset fileCns) (SrcSpecialRegion src)
        setSpecialRegionOff nm src off sz
       else do
        deleteFileOffsetMapEntry nm prevOff cns
        insertAtomic nm off sz fileCns src
    Just (prevOff, (prevSize, _, prevSrc))
      | prevEnd <- prevOff + prevSize
      , prevEnd > off -> do
      throwError $ printf "%s overlaps with previous %s segment." nm (show prevSrc)
    _ -> do
      setFileOffsetMapEntry off sz (mkFileOffset fileCns) (SrcSpecialRegion src)
      setSpecialRegionOff nm src off sz

insertAtomicSegment :: Word16 -> Elf.Phdr w -> Int -> SpecialRegion -> InferM w ()
insertAtomicSegment idx phdr fileCns src = withElfClassInstances $ do
  let off :: Int
      off = fromIntegral (Elf.phdrFileStart phdr)
  let sz :: Int
      sz = fromIntegral (Elf.phdrFileSize phdr)
  insertAtomic ("Segment " ++ show idx) off sz fileCns src

-- | Insert data segment when we know we do not have to check for merging with
-- earlier segment.
insertDataSegment2 :: Int -> Int -> OffsetConstraint -> InferM w ()
insertDataSegment2 off sz cns = do
  m <- gets $ Map.lookupLT (off+sz) . imsFileOffsetMap
  case m of
    Just (prevOff, (prevSize, prevCns, src)) | prevEnd <- prevOff + prevSize, prevEnd > off -> do
      if prevOff <= off then
        if Data == src then do
          let newEnd = max prevEnd (off+sz)
          let newSize = newEnd - prevOff
          setFileOffsetMapEntry prevOff newSize prevCns Data
         else do
          case checkSubsumption prevOff prevCns off cns of
            Just newCns -> do
              when (newCns /= prevCns) $ do
                setFileOffsetMapEntry prevOff prevSize cns src
            Nothing -> do
              throwError $ printf "Segment 0x%s with source %s constraint %s subsumption check fails by previous 0x%s %s."
                (showHex off "") (show src) (show cns) (showHex prevOff "") (show prevCns)
          when (off+sz > prevEnd) $ do
            setFileOffsetMapEntry prevEnd (off+sz - prevEnd) unconstrained Data
       else
        if Data == src then do
          setFileOffsetMapEntry prevOff (off-prevOff) prevCns Data
          insertDataSegment2 off (max (prevEnd - off) sz) cns
         else do
          insertDataSegment2 off (prevOff - off) cns
    _ -> setFileOffsetMapEntry off sz cns Data

insertDataSegment :: Int -> Int -> OffsetConstraint -> InferM w ()
insertDataSegment off sz cns =
  when (sz /= 0) $ do
    insertDataSegment2 off sz cns

-- | Infer layout for program header.
checkPhdr :: Word16 -- ^ Index of program header.
          -> Elf.Phdr w
          -> InferM w ()
checkPhdr idx phdr
  | hasSegmentType phdr Elf.PT_PHDR = withElfClassInstances $ do
    hdrInfo <- asks imcElf
    let a = fromIntegral (Elf.phdrSegmentAlign phdr)
    when (Elf.phdrFileStart phdr /= Elf.phdrTableFileOffset hdrInfo) $ do
      throwError $ "Mismatch in program header table location."

    insertAtomicSegment idx phdr a PhdrTable
  | hasSegmentType phdr Elf.PT_INTERP = withElfClassInstances $ do
    let a = fromIntegral (Elf.phdrSegmentAlign phdr)
    insertAtomicSegment idx phdr a Interpreter

  | hasSegmentType phdr Elf.PT_LOAD = withElfClassInstances $ do

    let off = fromIntegral (Elf.phdrFileStart phdr)
    let sz = fromIntegral (Elf.phdrFileSize phdr)
    -- Mark load region
    markLoadRegion off sz
    -- Constraint
    -- Special case for program header.
    when (Elf.phdrSegmentFlags phdr .&. Elf.pf_x == Elf.pf_x) $ do
      cr <- gets imsCodeRegion
      when (isJust cr) $ do
        throwError $ "Code region already defined."
      modify $ \s -> s { imsCodeRegion = Just idx }

    let cns = LoadSegmentConstraint (fromIntegral (Elf.phdrSegmentVirtAddr phdr) .&. 0xfff)
    insertDataSegment off sz cns

  | otherwise = withElfClassInstances $ do
      let off :: Int
          off = fromIntegral (Elf.phdrFileStart phdr)
      let sz :: Int
          sz = fromIntegral (Elf.phdrFileSize phdr)
      let a = fromIntegral (Elf.phdrSegmentAlign phdr)
      insertDataSegment off sz (mkFileOffset a)

checkShdr :: Elf.Shdr BS.ByteString (Elf.ElfWordType w)
          -> InferM w ()
checkShdr shdr = withElfClassInstances $ do
  let off = fromIntegral (Elf.shdrOff shdr)
  let sz = fromIntegral (Elf.shdrFileSize shdr)
  let a = fromIntegral (Elf.shdrAddrAlign shdr)
  case Elf.shdrName shdr of
    ".shstrtab" -> do
      insertAtomic ".shstrtab" off sz a Shstrtab
    ".strtab" -> do
      insertAtomic ".strtab" off sz a Strtab
    ".symtab" -> do
      insertAtomic ".symtab" off sz a Symtab
    _ -> do
      insertDataSegment off sz (mkFileOffset a)

-- | Create map
mkSpecialRegionMap :: InferMState -> Either String (Map.Map SpecialRegion Bool)
mkSpecialRegionMap s = do
  let resolve _reg (o,sz) =
        case Map.lookupLT (o+sz) (imsInLoad s) of
          Just (prevOff, prevSize) | prevOff + prevSize > o -> do
            unless (prevOff <= o && o+sz <= prevOff + prevSize) $ do
              Left $ printf "Special region %s crosses load boundary."
            Right True
          _ ->
            Right False
  Map.traverseWithKey resolve (imsSpecialRegionOff s)

-- | @updateCodeConstraints off sz m@ updates @m@ to mark region @[off..off+sz)@
-- as executable.
--
-- This assumes that the code region has already been defined as Data, and
-- it will replace all regions with Data `FileSource` values with `Code`.
updateCodeConstraints :: Word64 -- ^ Starting address
                      -> Int -- ^ Starting file offset
                      -> Int -- ^ Number of bytes
                      -> FileOffsetMap
                      -> FileOffsetMap
updateCodeConstraints base off sz = go (off+sz)
  where go :: Int -> FileOffsetMap -> FileOffsetMap
        go e m
          | e <= off = m
          | otherwise =
            case Map.lookupLT e m of
              Nothing -> error $ "Missing data region for code segment."
              Just (regOff, (regSize, cns, src)) ->
                case src of
                  Code _ -> error $ "Unexpected code segment when inserting one."
                  SrcSpecialRegion _ -> go regOff m
                  Data
                    | regOff < off -> error $ "Skipped over first data segment."
                    | otherwise ->
                      let addr = base + fromIntegral (regOff - off)
                       in go regOff (Map.insert regOff (regSize, cns, Code addr) m)

-- | Infer layout from binary.
inferBinaryLayout :: Elf.ElfHeaderInfo w
                  -> V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w))
                  -> Either String ElfContentLayout
inferBinaryLayout elfHdr shdrs = do
  let hdr = Elf.header elfHdr
  let cl = Elf.headerClass hdr
  Elf.elfClassInstances cl $ do
    let ctx = IMC { imcElf = elfHdr }
    let esize = fromIntegral (Elf.ehdrSize cl)
    let s0 = IMS { imsInLoad = Map.empty
                 , imsFileOffsetMap = Map.singleton 0 (esize, unconstrained, SrcSpecialRegion Ehdr)
                 , imsCodeRegion = Nothing
                 , imsSpecialRegionOff = Map.singleton Ehdr (0, esize)
                 }
    let act = do
          -- Insert section header table if defined.
          when (Elf.shdrCount elfHdr > 0) $ do
            let off = fromIntegral (Elf.shdrTableFileOffset elfHdr)
            let cnt = Elf.shdrCount elfHdr
            let sz = fromIntegral cnt * fromIntegral (Elf.shdrEntrySize cl)
            insertAtomic "Section header table" off sz 1 ShdrTable
          forM_ (enumCnt 0 (Elf.phdrCount elfHdr)) $ \idx -> do
            checkPhdr idx (Elf.phdrByIndex elfHdr idx)
          mapM_ checkShdr shdrs
    let (me, s) = runReader (runStateT (runExceptT act) s0) ctx
    () <- me
    codePhdrIndex <-
      case imsCodeRegion s of
        Nothing -> Left "Could not find executable segment."
        Just r -> pure r

    let codePhdr = Elf.phdrByIndex elfHdr codePhdrIndex

    let codeAddr :: Word64
        codeAddr = fromIntegral (Elf.phdrSegmentVirtAddr codePhdr)
    let codeOff  :: Int
        codeOff  = fromIntegral (Elf.phdrFileStart codePhdr)
    let codeSize :: Int
        codeSize = fromIntegral (Elf.phdrFileSize codePhdr)

    specMap <- mkSpecialRegionMap s

    let fileOffMap = updateCodeConstraints codeAddr codeOff codeSize (imsFileOffsetMap s)

    Right $ ECL { eclSpecialRegionInLoad = specMap
                , eclFileOffsetMap = fileOffMap
                , eclCodePhdrIndex = codePhdrIndex
                , eclCodeOffset = codeOff
                , eclCodeSize = codeSize
                }


