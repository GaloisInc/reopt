{-|
Copyright   : (c) Galois Inc, 2016-2018
Maintainer  : jhendrix@galois.com

This module performs the merging between the binary and new object.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt.Relinker
  ( Reopt.Relinker.Redirection.CodeRedirection(..)
  , SymbolNameToAddrMap
  , mergeObject
    -- * Utilities
  , SectionAddrMap
  ) where

import           Control.Lens hiding (pre)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.ElfEdit
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word

import           Reopt.Relinker.Object
import           Reopt.Relinker.Redirection
import           Reopt.Relinker.Relocations

import           GHC.Stack

------------------------------------------------------------------------
-- Utilities

-- | @fixAlignment v a@ returns the smallest multiple of @a@
-- that is not less than @v@.
fixAlignment :: Integral w => w -> w -> w
fixAlignment v 0 = v
fixAlignment v 1 = v
fixAlignment v a0
    | m == 0 = c * a
    | otherwise = (c + 1) * a
  where a = fromIntegral a0
        (c,m) = v `divMod` a

{-
mapFromList :: Ord k => (a -> k) -> [a] -> Map k [a]
mapFromList proj = foldr' ins Map.empty
  where ins e = Map.insertWith (++) (proj e) [e]
-}

hasBits :: Bits x => x -> x -> Bool
x `hasBits` b = (x .&. b) == b



------------------------------------------------------------------------
-- Elf specific utilities

-- | Size of page on system
pageSize :: Word64
pageSize = 0x1000

------------------------------------------------------------------------
-- RelinkM and ObjRelocState

-- | Identifies this should be a section index in the original binary.
type BinSectionIndex = Word16

-- | Identifies this should be a section index in the new file file.
type MergedSectionIndex = Word16


-- | This is a map from the section index in the binary to the section
-- index in the output file.
--
-- The relinker preserves addresses of the original code, so we do not
-- need to adjust those.
type BinSectionIndexToMergeMap =
  Map BinSectionIndex MergedSectionIndex

{-

-- | This is a map from the section index in the object file to the
-- section index and address in the base
type ObjSectionIndexToMergeMap w =
  Map ObjectSectionIndex (MergedSectionIndex, ElfWordType w)
-}

{-
-- | Maps section index in original binary to its section in output and base
-- address of section.
binarySectionMap :: Simple Lens (ObjRelocState w) BinSectionIndexToMergeMap
binarySectionMap = lens _binarySectionMap (\s v -> s { _binarySectionMap = v })

-- | Maps section index in object to its section in output and base address of
-- section.
objSectionMap :: Simple Lens (ObjRelocState w) (ObjSectionIndexToMergeMap w)
objSectionMap = lens _objSectionMap (\s v -> s { _objSectionMap = v })
-}

{-
-- | Create a new section index for a section in the object and record the original
-- index and virtual address of the section.
bindObjSectionIndex :: MonadState (ObjRelocState w) m
                    => Word16
                    -> ElfWordType w
                    -> m Word16
bindObjSectionIndex obj_idx addr = do
  new_idx <- freshSectionIndex
  objSectionMap %= Map.insert obj_idx (new_idx, addr)
  return $! new_idx
-}


type RelinkM = Except String

------------------------------------------------------------------------
-- Code for performing relocations in new object.

-- | Find a section with the given name, type, and flags.
--
-- This returns 'Nothing' if no matching section exists, but throws an error if
-- multiple do.
checkSectionEmpty :: Elf w
                -> BS.ByteString
                   -- ^ Expected name of section
                -> RelinkM ()
checkSectionEmpty e nm = elfClassInstances (elfClass e) $ do
  case nm `findSectionByName` e of
    [sec] -> do
      when (elfSectionSize sec /= 0) $ do
       throwE $ "Relinker requires new object contains no data to load."
    []  -> pure ()
    _   -> throwE $ "Multiple " ++ BSC.unpack nm ++ " sections in object file."

{-
elfHasTLSSegment :: Elf w -> Bool
elfHasTLSSegment e =
  case filter (`segmentHasType` PT_TLS) (elfSegments e) of
    [] -> False
    [_] -> True
    _ -> error "Multiple TLS segments in original binary"
-}

elfHasTLSSection :: Elf w -> Bool
elfHasTLSSection e =
  elfClassInstances (elfClass e) $ do
  any (\s -> elfSectionFlags s `hasBits` shf_tls) (e^..elfSections)

elfGNUStackSection :: Elf w -> Maybe (ElfSection (ElfWordType w))
elfGNUStackSection e =
  case filter (\s -> elfSectionName s == ".note.GNU-stack") (e^..elfSections) of
    [] -> Nothing
    (s:_) -> Just s

------------------------------------------------------------------------
-- SectionMap

-- | Map from section indices in object to virtual address where it will be
-- loaded into binary.
type SectionAddrMap w = Map Word16 w

------------------------------------------------------------------------
-- SymbolNameToAddrMap

-- | Maps symbol names in binaries to their virtual address when loaded.
type SymbolNameToAddrMap w = Map BS.ByteString w

------------------------------------------------------------------------
-- Merger

-- | Find relocation entries in section with given name.
findRelaEntries :: Map BS.ByteString [ElfSection (ElfWordType 64)]
                   -- ^ Map from section names to sections with that name.
                -> BS.ByteString
                   -- ^ Name of section containing relocation entries.
                -> Except String [RelaEntry X86_64_RelocationType]
findRelaEntries secMap nm = do
  case fromMaybe [] (Map.lookup nm secMap) of
    -- Assume that no section means no relocations
    [] -> return []
    [s] -> except $ elfRelaEntries ELFDATA2LSB (BSL.fromStrict (elfSectionData s))
    _ -> throwE $  "Multiple " ++ BSC.unpack nm ++ " sections in object file."

-- | Infomration
data BinarySymbolTable w =
  BinarySymbolTable { binLocalSymbols :: !(V.Vector (ElfSymbolTableEntry (ElfWordType w)))
                    , binNonlocalSymbols :: !(V.Vector (ElfSymbolTableEntry (ElfWordType w)))
                    }

isLocalSymbol :: ElfSymbolTableEntry w -> Bool
isLocalSymbol sym = steBind sym == STB_LOCAL

-- | Find the symbol table in the binary.
getBinarySymbolTable :: Elf w
                       -- ^ Binary file
                     -> Except String (BinarySymbolTable w)
getBinarySymbolTable binary = do
  case elfSymtab binary of
    -- Assume that no section means no relocations
    []  -> do
      pure $! BinarySymbolTable { binLocalSymbols = V.empty
                                , binNonlocalSymbols = V.empty
                                }
    [tbl] -> do
      (local, global) <- do
        pure $ V.partition isLocalSymbol (elfSymbolTableEntries tbl)
      pure $! BinarySymbolTable { binLocalSymbols = local
                                , binNonlocalSymbols = global
                                }
    _   ->
      throwE $ "Multiple .symtab sections in input binary."

-- | Find the symbol table in the elf.
findSymbolTable :: String
                   -- ^ Type of file for error reporting.
                -> Elf w
                    -- ^ Object with relocations
                -> Except String (ElfSymbolTable (ElfWordType w))
findSymbolTable nm obj = do
  case elfSymtab obj of
    -- Assume that no section means no relocations
    []  -> throwE $ "Could not find symbol table."
    [tbl] -> return tbl
    _   -> throwE $ "Multiple .symtab sections in " ++ nm ++ " file."

-- | Check original binary satisfies preconditions.
checkOriginalBinaryAssumptions :: Monad m => Elf 64 -> m ()
checkOriginalBinaryAssumptions binary = do
  when (elfData binary /= ELFDATA2LSB) $ do
    fail $ "Expected the original binary to be least-significant bit first."
  when (elfType binary /= ET_EXEC) $ do
    fail $ "Expected the original binary is an executable."
  when (elfMachine binary /= EM_X86_64) $ do
    fail $ "Only x86 64-bit object files are supported."
  when (elfFlags binary /= 0) $ do
    fail $ "Expected elf flags in binary to be zero."
  unless (null (elfGnuRelroRegions binary)) $ do
    fail $ "Expected no PT_GNU_RELO segment in binary."

-- | Check object file satisfies preconditions.
checkObjAssumptions :: Elf 64
                    -> RelinkM ()
checkObjAssumptions obj = do
  -- Check new object properties.
  when (elfData obj /= ELFDATA2LSB) $ do
    fail $ "Expected the new binary binary to be least-significant bit first."
  when (elfType obj /= ET_REL) $ do
    fail $ "Expected a relocatable file as input."
  when (elfMachine obj /= EM_X86_64) $ do
    fail $ "Only x86 64-bit executables are supported."
  when (elfFlags obj /= 0) $ do
    fail $ "Expected elf flags in new object to be zero."
  unless (null (elfGnuRelroRegions obj)) $ do
    fail $ "Expected no PT_GNU_RELO segment in new object."
  checkSectionEmpty obj ".data"
  checkSectionEmpty obj ".bss"
  -- Check no TLS in object.
  when (elfHasTLSSection obj) $ do
    throwE $ "TLS section is not allowed in new code object."

-- | Intermediate state used when generating layout information for new binary.
data ExtractState = ExtractState { nextSectionIndex :: !MergedSectionIndex
                                   -- ^ Index for next section
                                 , codeRedirs       :: [CodeRedirection (ElfWordType 64)]
                                   -- ^ Redirections in code segment
                                 , binSectionMap    :: !BinSectionIndexToMergeMap
                                 }


-- | Monad used for extracting code from binaries.
type ExtractM = State ExtractState

-- | Create a fresh section index.
freshSectionIndex :: ExtractM MergedSectionIndex
freshSectionIndex  = do
  idx <- gets nextSectionIndex
  modify $ \s -> s { nextSectionIndex = idx + 1 }
  return idx

-- | Check that the flags of the segment match the expected flags.
checkSegFlags :: ElfSegment w -> ElfSegmentFlags -> ExtractM ()
checkSegFlags seg expected =
  when (elfSegmentFlags seg /= expected) $
    error $ "Segment " ++ show (elfSegmentFlags seg) ++ " did not have the expected flags "
      ++ show expected ++ "."

-- | Check that virtual address and "physical" address fields are same.
checkSegmentAddrsMatch :: ElfSegment 64 -> ExtractM ()
checkSegmentAddrsMatch seg = do
  when (elfSegmentVirtAddr seg /= elfSegmentPhysAddr seg) $ do
    error $ "Relinker expects  virtual and physical addresses match."

checkSectionAssumptions :: ElfSection (ElfWordType 64)
                        -> ExtractM ()
checkSectionAssumptions sec = do
  let nm = BSC.unpack $ (elfSectionName sec)
  when (elfSectionLink sec /= 0) $ do
    error $ "Expected link of " ++ nm ++ " to be 0."
  when (elfSectionInfo sec /= 0) $ do
    error $ "Expected info of " ++ nm ++ " to be 0."
  when (elfSectionEntSize sec /= 0) $ do
    error $ "Expected entry size of " ++ nm ++ " to be 0."

-- | Generate a deferred region from a region within the code segment
-- of the original binary.
copyBinaryCodeSections :: (ElfWordType 64 -> BS.ByteString)
                          -- ^ Make jump
                       -> Map BS.ByteString (ElfWordType 64)
                          -- ^ Maps names of symbols defined in the object file to
                          -- their address in the new binary.  This will only point to
                          -- new code.
                       -> [ElfDataRegion 64]
                       -- ^ Regions encountered so far.
                       -> ElfWordType 64
                       -- ^ File offset
                       -> [ElfDataRegion 64]
                       -- ^ Regions in binary left to process.
                       -> ExtractM [ElfDataRegion 64]
copyBinaryCodeSections _ _ prevRegions _ [] = do
  pure $! (reverse prevRegions)
copyBinaryCodeSections mkJump codeAddrMap prevRegions secOffset (thisRegion:restRegions) = do
 seq secOffset $ do
  case thisRegion of
    ElfDataElfHeader -> do
      error "Elf header appears in unexpected location."
    ElfDataSegmentHeaders -> do
      error "Elf segment headers appears in unexpected location."
    ElfDataSegment _seg ->
      error "Elf segment appears in unexpected location."
    ElfDataSectionHeaders ->
      error "Elf section header table appears in unexpected location."
    ElfDataSectionNameTable _ ->
      error "Elf section name table appears in unexpected location."
    ElfDataGOT _ ->
      error "Reopt does not currently support global offset table."
    ElfDataStrtab _ ->
      error "Elf string table appears in unexpected location."
    ElfDataSymtab _ ->
      error "Elf symbol table appears in unexpected location."
    ElfDataSection sec -> do

      checkSectionAssumptions sec

      -- Get memory size of section
      let msize = elfSectionSize sec
      -- Get data in section
      let dta = elfSectionData sec
      -- Get filesize of section.
      let fsize = fromIntegral (BS.length dta)

      let nm = elfSectionName sec

      when (elfSectionType sec /= SHT_PROGBITS) $ do
        error $ "Unexpected section type " ++ show (elfSectionType sec)
             ++ " in code section " ++ BSC.unpack nm ++ "."
      when (msize /= fsize) $ do
        error $ "PROGBITS section " ++ BSC.unpack nm ++ " filesize must equal memory size."

      redirs <- gets codeRedirs

      -- Add code relocations
      idx <- freshSectionIndex
      let dr = ElfSection
             { elfSectionIndex     = idx
             , elfSectionName      =
                 if Set.member nm copiedObjSectionNames then
                   ".orig" <> nm
                  else
                   nm
             , elfSectionType      = elfSectionType sec
             , elfSectionFlags     = elfSectionFlags sec
             , elfSectionAddr      = elfSectionAddr sec
             , elfSectionSize      = msize
             , elfSectionLink      = 0
             , elfSectionInfo      = 0
             , elfSectionAddrAlign = elfSectionAddrAlign sec
             , elfSectionEntSize   = 0
             , elfSectionData      = insertStaticRedirs mkJump codeAddrMap redirs secOffset dta
             }
      -- Add mapping to new region
      modify $ \s -> s { binSectionMap = Map.insert (elfSectionIndex sec) idx (binSectionMap s) }

      -- The last section
      let nextRegions = ElfDataSection dr : prevRegions
      seq dr $ seq nextRegions $
        copyBinaryCodeSections mkJump codeAddrMap nextRegions (secOffset + fsize) restRegions
    ElfDataRaw b -> do
      let dr = ElfDataRaw b
      let fsize = fromIntegral (BS.length b)
      let nextRegions = dr : prevRegions
      seq dr $ seq nextRegions $
        copyBinaryCodeSections mkJump codeAddrMap nextRegions (secOffset + fsize) restRegions


-- | This function is used to generate regions in the executable
-- segment for representing the code in the object file.
--
-- This function maps sections from the object to the new binary.
-- Sections in the object file are not renamed; instead sections in the original binary have
-- ".orig" prepended.
mkObjCodeRegions :: RelocInfo (ElfWordType 64)
                 -> [ElfDataRegion 64]
                    -- ^ New regions added so far.
                 -> [ObjectSectionInfo]
                    -- ^ Names of sections to look for and expected flags
                 -> ExtractM [ElfDataRegion 64]
mkObjCodeRegions _ prev [] = do
  pure $ reverse prev
mkObjCodeRegions relocInfo prevRegions (secInfo:rest) = do
  let idx = objsecMergedIndex secInfo
  let relaEntries :: [RelaEntry X86_64_RelocationType]
      relaEntries = objsecRelocations secInfo
  let sec = objsecOrigSection secInfo
  -- Get alignemnt for this section
  let align = elfSectionAddrAlign sec
  -- Compute address of this section
  let secAddr :: ElfWordType 64
      secAddr = objsecBaseAddr secInfo
  -- Add padding as needed to this section
  let paddingCnt = objsecPadding secInfo

  let dta = elfSectionData sec
  let newSec = ElfSection
             { elfSectionIndex = idx
             , elfSectionName = elfSectionName sec
             , elfSectionType = elfSectionType sec
             , elfSectionFlags = elfSectionFlags sec
             , elfSectionAddr  = secAddr
             , elfSectionSize  = elfSectionFileSize sec
             , elfSectionLink  = 0
             , elfSectionInfo  = 0
             , elfSectionAddrAlign = align
             , elfSectionEntSize   = 0
             , elfSectionData      = performRelocs relocInfo secAddr dta relaEntries
             }
  let newRegions = [ElfDataSection newSec] ++ paddingRegions paddingCnt ++ prevRegions
  mkObjCodeRegions relocInfo newRegions rest

copiedObjSections :: [(BS.ByteString, ElfSectionFlags (ElfWordType 64))]
copiedObjSections =
  [ (,) ".text"     (shf_alloc .|. shf_execinstr)
  , (,) ".rodata"   shf_alloc
  , (,) ".eh_frame" shf_alloc
  ]

copiedObjSectionNames :: Set BS.ByteString
copiedObjSectionNames = Set.fromList $ fst <$> copiedObjSections

-- | Resolve the code segment
resolveCodeSegment :: ElfWordType 64
                      -- ^ Required alignment for segment
                   -> (ElfWordType 64 -> BS.ByteString)
                      -- ^ Make jump
                   -> ElfSegment 64 -- ^ Code in original binary
                   -> [ObjectSectionInfo]
                      -- ^ Information about new sections
                   -> V.Vector (ElfSymbolTableEntry (ElfWordType 64))
                   -- ^ Symbol table entries in object.
                   -> ElfWordType 64
                      -- ^ Size of new code segment in bytes
                      --
                      -- Note that for code segment the file and memory size are the same.
                   -> ExtractM (ElfSegment 64)
resolveCodeSegment align mkJump binCodeSeg objSections objSymbols codeSize = do
  checkSegmentAddrsMatch binCodeSeg

  -- Get low order bits module alignment
  let segAddr = elfSegmentVirtAddr binCodeSeg
  let segAlign = segAddr .&. (align - 1)
  when (segAlign /= 0) $ do
    error "Code segment must be aligned to 0 address."

  -- Otherwise we copy segment over, but check the contents for structures we
  -- cannot support.
  checkSegFlags binCodeSeg (pf_r .|. pf_x)

  let secNameAddrMap :: Map ObjectSectionIndex (BS.ByteString, ElfWordType 64)
      secNameAddrMap
        = Map.fromList
          [ (objsecSourceIndex secInfo, (objsecName secInfo, objsecBaseAddr secInfo))
          | secInfo <- objSections
          ]

  let relocInfo = mkRelocInfo secNameAddrMap objSymbols

  let codeAddrMap = mkCodeAddrMap (snd <$> secNameAddrMap) objSymbols

  rest <-
    case toList (elfSegmentData binCodeSeg) of
      ElfDataElfHeader : ElfDataSegmentHeaders : rest -> pure rest
      ElfDataElfHeader : _ -> do
        error "Missing segment headers at start of code segment."
      _ -> do
        error "Missing elf header at start of file."

  binRegions <- copyBinaryCodeSections mkJump codeAddrMap [] 0 rest

  objRegions  <- mkObjCodeRegions relocInfo [] objSections

  pure $!
    ElfSegment
    { elfSegmentType = PT_LOAD
    , elfSegmentFlags = elfSegmentFlags binCodeSeg
    , elfSegmentIndex = 0
    , elfSegmentVirtAddr = elfSegmentVirtAddr binCodeSeg
    , elfSegmentPhysAddr = elfSegmentVirtAddr binCodeSeg
    , elfSegmentAlign = align
    , elfSegmentMemSize = ElfAbsoluteSize codeSize
    , elfSegmentData = Seq.fromList $
        [ElfDataElfHeader, ElfDataSegmentHeaders]
        ++ binRegions
        ++ objRegions
    }

paddingRegions :: ElfWordType 64 -> [ElfDataRegion 64]
paddingRegions 0 = []
paddingRegions p = [ElfDataRaw (BS.replicate (fromIntegral p) 0)]

-- | Convert the data segment in the original file to the deferred
-- segment.
resolveDataSegment :: ElfWordType 64 -- ^ Required alignment for segment.
                   -> ElfWordType 64 -- ^ File offset
                   -> ElfSegment 64  -- ^ Original data segment in bianryt
                   -> ExtractM [ElfDataRegion 64]
resolveDataSegment align foff seg = do
  checkSegmentAddrsMatch seg
  let segAddr = elfSegmentVirtAddr seg

  let mask :: ElfWordType 64
      mask = align - 1
  let addrAlign :: ElfWordType 64
      addrAlign = segAddr .&. mask
  let fileAlign :: ElfWordType 64
      fileAlign = foff .&. mask

  -- Padding is the number of bytes that need to be added to the file
  -- offset to ensure the relevant bits in the file and address
  -- offsets match.
  let padding :: ElfWordType 64
      padding = (addrAlign + align - fileAlign) .&. mask

  -- Otherwise we copy segment over, but check the contents for structures we
  -- cannot support.
  checkSegFlags seg (pf_r .|. pf_w)
  let newSeg = ElfSegment { elfSegmentType = PT_LOAD
                          , elfSegmentFlags = elfSegmentFlags seg
                          , elfSegmentIndex = 1
                          , elfSegmentVirtAddr = elfSegmentVirtAddr seg
                          , elfSegmentPhysAddr = elfSegmentPhysAddr seg
                          , elfSegmentAlign = align
                          , elfSegmentMemSize = elfSegmentMemSize seg
                          , elfSegmentData = elfSegmentData seg
                          }
  pure $! paddingRegions padding ++ [ ElfDataSegment newSeg ]


------------------------------------------------------------------------
-- IdentifySegmentResult

-- | This is the result of classifying which top-level regions in a
-- binary the relinker should care about.
data IdentifySegmentResult
   = IdentifySegmentResult { unexpectedRegionNames :: [String]
                           , seenSegments :: [ElfSegment 64]
                           }

-- | Empty result before classifying top-level regions.
emptyIdentifySegmentResult :: IdentifySegmentResult
emptyIdentifySegmentResult =
  IdentifySegmentResult { unexpectedRegionNames = []
                        , seenSegments = []
                        }

-- | Add the name of a region that we did not expect in the binary.
didNotExpectOriginalRegion :: String
                           -> IdentifySegmentResult
                           -> IdentifySegmentResult
didNotExpectOriginalRegion nm r = do
  r { unexpectedRegionNames = nm : unexpectedRegionNames r }

-- | Add a PT_LOAD segment to the result/
addLoadSegment :: ElfSegment 64
               -> IdentifySegmentResult
               -> IdentifySegmentResult
addLoadSegment seg r = r { seenSegments = seg : seenSegments r }

-- | Classify which segments are important and which are unexpected.
identifyLoadSegment :: ElfDataRegion 64
                    -- ^ Data region in input binary left to process
                 -> IdentifySegmentResult
                 -> IdentifySegmentResult
identifyLoadSegment thisRegion =
  -- Decide current action based on region
  case thisRegion of
    ElfDataElfHeader ->
      didNotExpectOriginalRegion "Elf header outside loadable segment."
    ElfDataSegmentHeaders ->
      didNotExpectOriginalRegion "Elf segment table outside loadable segment."
    ElfDataGOT _ ->
      didNotExpectOriginalRegion "top-level .got table"
    ElfDataSection _ -> do
      didNotExpectOriginalRegion "top-level section"
    -- Regions to drop
    ElfDataSectionHeaders -> id
    ElfDataSectionNameTable _ -> id
    ElfDataStrtab _ -> id
    ElfDataSymtab _ -> id
    ElfDataRaw _ -> id
    ElfDataSegment seg
      -- Process loadable segment.
      | PT_LOAD <- elfSegmentType seg -> addLoadSegment seg
      | otherwise ->
        didNotExpectOriginalRegion $ "Not loadable segment " ++ show (elfSegmentIndex seg)

-- | Create a bytestring with a jump to the immediate address.
x86_64_immediate_jmp :: Word64 -> BS.ByteString
x86_64_immediate_jmp addr = BSL.toStrict $ Bld.toLazyByteString $ mov_addr_to_r11 <> jump_r11
  where mov_addr_to_r11
          =  Bld.word8 0x49
          <> Bld.word8 0xBB
          <> Bld.word64LE addr
        jump_r11
          =  Bld.word8 0x41
          <> Bld.word8 0xFF
          <> Bld.word8 0xE3

-- | This maps a symbol entry in the binary to a symbol table entry in
-- the new file.  This functions adds the suffice "_reopt" to each
-- symbol.
--
-- Note: This returns `Nothing` when the symbol table entry may be
-- dropped.
resolveBinarySymbolTableEntry :: Num (ElfWordType 64)
                              => BinSectionIndexToMergeMap
                                 -- ^ Maps section indices in binary
                                 -- to merged section index.
                              -> Set BS.ByteString
                                 -- ^ Set of symbols in the new object file.

                              -> ElfSymbolTableEntry (ElfWordType 64)
                              -> RelinkM (Maybe (ElfSymbolTableEntry (ElfWordType 64)))
resolveBinarySymbolTableEntry sectionIdxMap usedNames ste = do
  let nm =
        case steType ste of
         STT_SECTION -> ""
         STT_FILE -> steName ste
         STT_NOTYPE -> steName ste <> "_reopt"
         STT_OBJECT -> steName ste <> "_reopt"
         STT_FUNC -> steName ste <> "_reopt"
         tp -> error $ "Symbol type " ++ show tp ++ " is not yet supported."

  when (Set.member nm usedNames) $ do
    error $ "Reopt given a binary that already uses symbol."
  case steIndex ste of
    SHN_ABS -> do
      pure $! (Just $! ste { steName = nm })
    SHN_COMMON -> do
      error "Binaries with SHN_COMMON symbols are not supported."
    SHN_UNDEF  -> do
      error "Binaries with SHN_UNDEF symbols are not supported."
    ElfSectionIndex sectionIdx -> do
      case Map.lookup sectionIdx sectionIdxMap of
        Nothing -> do
          -- We silently drop symbols that return to sections that were not copied over,
          -- but give an error on other symbols.
          if steType ste == STT_SECTION then
            -- Silently drop symbols that reference sections which were deleted, but
            -- complain about others.
            pure Nothing
           else
            error $ "Symbol in binary referenced unmapped section: " ++ show sectionIdx ++ "."
        Just idx -> do
          pure $! Just $! ste { steIndex = ElfSectionIndex idx
                              }

-- | Information about how specific sections in the object will be
-- mapped to the binary.
data ObjectSectionInfo
  = ObjectSectionInfo
  { objsecSourceIndex :: !ObjectSectionIndex
    -- ^ Index of section in object.
  , objsecMergedIndex :: !MergedSectionIndex
    -- ^ Index of section in new binary.
  , objsecPadding  :: !(ElfWordType 64)
    -- ^ Number of padding bytes to add before this section
  , objsecBaseAddr :: !(ElfWordType 64)
    -- ^ Base address of section after alignment
  , objsecRelocations :: !([RelaEntry X86_64_RelocationType])
    -- ^ Relocations to apply
  , objsecOrigSection :: !(ElfSection (ElfWordType 64))
  }

objsecName :: ObjectSectionInfo -> BS.ByteString
objsecName = elfSectionName . objsecOrigSection

-- | This function is used to generate the information needed to perform relocations.
resolveObjSectionInfo :: Map BS.ByteString [ElfSection (ElfWordType 64)]
                         -- ^ Map from section names to section(s) with that name.
                      -> [ObjectSectionInfo]
                         -- ^ Map section indices from binary to information.
                      -> MergedSectionIndex
                         -- ^ Merged section index
                      -> ElfWordType 64
                         -- ^ Base address for code (add file offset to get next address)
                      -> [(BS.ByteString, ElfSectionFlags (ElfWordType 64))]
                         -- ^ Names of sections to look for and expected flags
                      -> Except String ( [ObjectSectionInfo]
                                       , MergedSectionIndex
                                       , ElfWordType 64
                                       )
resolveObjSectionInfo  _secMap prev idx addr [] = do
  pure (reverse prev, idx, addr)
resolveObjSectionInfo secMap prevRegions idx addr ((nm, expectedFlags):rest) = do
  case fromMaybe [] (Map.lookup nm secMap) of
    [] ->
      -- Ignore missing sections
      resolveObjSectionInfo secMap prevRegions idx addr rest
    (sec:otherSections) -> do
      unless (null otherSections) $ do
        error $ "Found multiple sections in object file named " ++ BSC.unpack nm
      when (elfSectionType sec /= SHT_PROGBITS) $ do
        error $ BSC.unpack nm ++ " section expected to be type SHT_PROGBITS."
      when (elfSectionFlags sec /= expectedFlags) $ do
        error $ BSC.unpack nm ++ " section has an unexpected flags."
      let dta = elfSectionData sec
      when (toInteger (BS.length dta) /= toInteger (elfSectionSize sec)) $ do
        error $ "Object code section sizes must match data length."
      let relaEntries :: [RelaEntry X86_64_RelocationType]
          relaEntries =
            case runExcept (findRelaEntries secMap (".rela" <> nm)) of
              Left e -> error e
              Right l -> l
      let fsize = elfSectionFileSize sec
      -- Get alignemnt for this section
      let align = elfSectionAddrAlign sec
      -- Compute address of this section
      let secAddr :: ElfWordType 64
          secAddr = fixAlignment addr align
      -- Compute section information
      let secInfo
             = ObjectSectionInfo
             { objsecSourceIndex = elfSectionIndex sec
             , objsecMergedIndex = idx
             , objsecPadding  = secAddr - addr
             , objsecBaseAddr = secAddr
             , objsecRelocations = relaEntries
             , objsecOrigSection = sec
             }
      resolveObjSectionInfo secMap (secInfo:prevRegions) (idx + 1) (addr + fsize) rest

-- | Information needed to generate the new symbol table from current entries.
data MkSymbolTableContext w
   = MkSymbolTableContext
   { binSectionIndexMap :: !BinSectionIndexToMergeMap
   , objectSectionIndexMap :: !(Map ObjectSectionIndex (MergedSectionIndex, ElfWordType w))
     -- ^ Maps section index in object to new section index
     -- and base address of section.
     --
     -- Note that in object files, all sections have an address of `0`.
   }


-- | This maps a symbol entry in object to the new entry.
resolveObjectSymbolTableEntry :: Num (ElfWordType w)
                              => MkSymbolTableContext w
                              -> ElfSymbolTableEntry (ElfWordType w)
                              -> RelinkM (Maybe (ElfSymbolTableEntry (ElfWordType w)))
resolveObjectSymbolTableEntry ctx ste = do
  case steIndex ste of
    SHN_ABS -> do
      error "Object file does not allow absolute addresses."
    SHN_COMMON -> do
      error "Binaries with SHN_COMMON symbols are not supported."
    SHN_UNDEF  -> do
      --Left $ SymbolNameNotFound (steName ste)
      pure Nothing
    ElfSectionIndex objSecIdx ->
      case Map.lookup objSecIdx (objectSectionIndexMap ctx) of
        Nothing ->
          pure Nothing -- Left $ SectionIndexNotFound obj_sec_idx
        Just (idx, base) -> do
          pure $! Just $
            ste { steIndex = ElfSectionIndex idx
                , steValue = base + steValue ste
                }

-- | Create a symbol table for the new file.
mkSymbolTable :: MergedSectionIndex
                 -- ^ Index for symbol table
              -> MkSymbolTableContext 64
              -> BinarySymbolTable 64
                 -- ^ Binary symbol table information
              -> V.Vector (ElfSymbolTableEntry Word64)
                 -- ^ Symbols in the new binary.
              -> RelinkM (ElfSymbolTable (ElfWordType 64))
mkSymbolTable newSymtabIndex ctx  binSyms objSymbols = do
  let (objInputLocalSyms, objInputGlobalSyms) = V.partition isLocalSymbol objSymbols

  objLocalSyms <- fmap (V.mapMaybe id) $
    traverse (resolveObjectSymbolTableEntry ctx)
             objInputLocalSyms

  objGlobalSyms <- fmap (V.mapMaybe id) $
    traverse (resolveObjectSymbolTableEntry ctx)
             objInputGlobalSyms

  let objSymbolNames :: Set BS.ByteString
      objSymbolNames = Set.fromList
        [ steName ste
        | ste <- V.toList objLocalSyms ++ V.toList objGlobalSyms
        ]

  binLocalSyms <- fmap (V.mapMaybe id) $
    traverse (resolveBinarySymbolTableEntry (binSectionIndexMap ctx) objSymbolNames)
             (binLocalSymbols binSyms)

  binGlobalSyms <- fmap (V.mapMaybe id) $
    traverse (resolveBinarySymbolTableEntry (binSectionIndexMap ctx) objSymbolNames)
             (binNonlocalSymbols binSyms)

  pure $!
      ElfSymbolTable { elfSymbolTableIndex = newSymtabIndex
                     , elfSymbolTableEntries
                         = V.concat
                             [ binLocalSyms,  objLocalSyms
                             , binGlobalSyms, objGlobalSyms
                             ]
                     , elfSymbolTableLocalEntries
                         = fromIntegral (V.length binLocalSyms)
                         + fromIntegral (V.length objLocalSyms)
                     }

-- | This performs the actions needed to merge the new object file and
-- original code.
mergeObject' :: HasCallStack
             => Elf 64 -- ^ Existing binary
             -> Elf 64 -- ^ Object file to merge into existing binary.
             -> [CodeRedirection Word64]
                -- ^ Redirections from old binary to new code.
             -> (Word64 -> BS.ByteString)
                -- ^ Function for creating jump to given offset.
             -> RelinkM (Elf 64)
mergeObject' origBinary obj redirs mkJump = do
  -- Drop from huge page aligned addresses (0x200000) to normal page alignment.
  let elfAlign :: ElfWordType 64
      elfAlign = pageSize

  -- Check original binary properties
  checkOriginalBinaryAssumptions origBinary
  -- Check object assumptions
  checkObjAssumptions obj
  -- Check OSABI compat
  when (elfOSABI obj /= elfOSABI origBinary) $ do
    fail $ "Expected the new object to use the same OS ABI as original."

  -- Get symbols in files.
  binSymbols <- getBinarySymbolTable origBinary
  objSymbols <- elfSymbolTableEntries <$> findSymbolTable "object" obj

  -- Create GNU stack info if both binary and object have it, and the
  -- stack is non-executable in both.
  let mgnuStack =
        case () of
          () | Just binGnuStack <- elfGnuStackSegment origBinary
             , gnuStackIsExecutable binGnuStack == False
             , Just _ <- elfGNUStackSection origBinary -> do
                 Just $ GnuStack { gnuStackSegmentIndex = 2
                                 , gnuStackIsExecutable = False
                                 }
             | otherwise ->
                 Nothing

  -- Compute number of program headers
  let newPhdrCount = 2
                   + (if isJust mgnuStack then 1 else 0)

  let binaryPhdrCount :: Int
      binaryPhdrCount = elfSegmentCount origBinary
  -- We currently do not allow the number of program headers to change so that
  -- the memory layout between the input binary and output does not change.
  when (newPhdrCount /= binaryPhdrCount) $ do
    error "Relinker does not allow number of program headers to change."

  (codeSeg, dataSeg) <- do
    let origData = origBinary^.elfFileData
    let identResult = foldr identifyLoadSegment emptyIdentifySegmentResult origData
    let unex = unexpectedRegionNames identResult
    unless (null unex) $ do
      error $ "Unexpected regions: " ++ unwords unex
    case seenSegments identResult of
      [cs, ds] -> pure (cs, ds)
      _ -> do
        error $ "Expected binary to contain a code segment followed by a data segment."

  -- Get layout of object code.
  (objSections, nextMergedIdx, finalCodeAddr) <- do
    -- Generate mam from
    let secMap = Map.fromListWith (++)
                  [ (elfSectionName sec, [sec]) | sec <- obj^..elfSections ]
    let ElfAbsoluteSize codeMemSize = elfSegmentMemSize codeSeg

    let codeAddr = elfSegmentVirtAddr codeSeg + codeMemSize
    case runExcept (resolveObjSectionInfo secMap [] 1 codeAddr copiedObjSections) of
      Left msg -> error msg
      Right r -> pure r

  -- Compute size of new code segment.
  let codeSize = finalCodeAddr - elfSegmentVirtAddr codeSeg

  (loadSegments, s) <- do
    let initState = ExtractState { codeRedirs        = redirs
                                 , nextSectionIndex  = nextMergedIdx
                                 , binSectionMap     = Map.empty
                                 }
    pure $ flip runState initState $ do
      rcodeSeg <- resolveCodeSegment elfAlign mkJump codeSeg objSections objSymbols codeSize
      rdataSeg <- resolveDataSegment elfAlign codeSize dataSeg
      pure $ ElfDataSegment rcodeSeg : rdataSeg

  let loadSectionCount = nextSectionIndex s

  -- Make index for section name table.

  let symbolCtx :: MkSymbolTableContext 64
      symbolCtx =
        MkSymbolTableContext
        { binSectionIndexMap = binSectionMap s
        , objectSectionIndexMap = Map.fromList
            [ (objsecSourceIndex sec, (objsecMergedIndex sec, objsecBaseAddr sec))
            | sec <- objSections ]
        }

  -- Create symbol table
  symtab <- mkSymbolTable (loadSectionCount + 1) symbolCtx binSymbols objSymbols

  return $! Elf { elfData       = ELFDATA2LSB
                , elfClass      = ELFCLASS64
                , elfOSABI      = elfOSABI origBinary
                , elfABIVersion = 0
                , elfType       = ET_EXEC
                , elfMachine    = EM_X86_64
                , elfEntry      = elfEntry origBinary
                , elfFlags      = 0
                , _elfFileData  = Seq.fromList $
                   loadSegments
                   ++ [ ElfDataSectionNameTable loadSectionCount
                      , ElfDataSymtab symtab
                      , ElfDataStrtab (loadSectionCount + 2)
                      , ElfDataSectionHeaders
                      ]
                , elfGnuStackSegment = mgnuStack
                , elfGnuRelroRegions = []
                }

-- | This merges an existing elf binary and new object file to create a
-- combined binary.
--
-- This function only works on Elf files that meet specific requirements,
-- and may call `error` or return an error message if they do not.  The
-- general rule we have is that we call `error` if we expect every binary
-- will in practice satisfy the assumptions, and return an error message
-- if we think binaries will fail this in some cases.
--
-- The assumptions on the original binary are:
-- * There are two loadable segments.
-- * The first loadable segment is:
--   * Readable and executable
--   * Located at the beginning of the file.
--   * Contains the elf header, the program header table, and one or more loadable
--     sections (perhaps with padding between them).
--
mergeObject :: HasCallStack
            => Elf 64
               -- ^ Existing binary
            -> Elf 64
               -- ^ Object file to insert
            -> [CodeRedirection Word64]
               -- ^ Redirections from original file for new file.
            -> Either String (Elf 64)
mergeObject origBinary new_obj redirs = runExcept action
  where action = mergeObject' origBinary new_obj redirs x86_64_immediate_jmp
