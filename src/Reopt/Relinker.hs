{-|
This module performs the merging between the binary and new object.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Reopt.Relinker
  ( Reopt.Relinker.Redirection.CodeRedirection(..)
  , mergeObject
  , x86_64_immediateJump
  ) where

--import           Control.Lens hiding (pre)
import           Control.Monad.Except
--import           Control.Monad.State.Class
--import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
--import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
--import           Data.Either
import           Data.ElfEdit ()
import qualified Data.ElfEdit as Elf
import           Data.Foldable
--import           Data.Int
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
--import           Data.Maybe
--import qualified Data.Sequence as Seq
--import           Data.Set (Set)
--import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
--import           Reopt.Relinker.Object
import           Reopt.Relinker.Redirection
--import           Reopt.Relinker.Relocations
import           System.Exit
import           System.IO
--import           Text.Printf

import           GHC.Stack

nyi :: String -> a
nyi nm = error $ "Not yet implemented: " ++ nm

------------------------------------------------------------------------
-- Utilities

-- | `hasFlag x a` returns true if `x` has all the flags in `a`.
hasFlags :: Bits a => a -> a -> Bool
hasFlags x a = x .&. a == a


-- | @fixAlignment v a@ returns the smallest multiple of @a@
-- that is at least @v@.
fixAlignment :: Elf.FileOffset Word64 -> Word64 -> Elf.FileOffset Word64
fixAlignment (Elf.FileOffset v) a =
   let n = (v + a - 1) `quot` a
   in Elf.FileOffset (n* a)

------------------------------------------------------------------------
-- Elf specific utilities

-- | Size of page on system
pageSize :: Word64
pageSize = 0x1000

-- | `resolveSegmentPadding offset addr` returns the next file offset
-- @o@ not smaller than @offset@ such that `o `mod` pageSize == addr `mod` pageSize`.
resolveSegmentOffset :: Elf.FileOffset Word64 -> Word64 -> Elf.FileOffset Word64
resolveSegmentOffset (Elf.FileOffset offset) addr = Elf.FileOffset (offset + n)
  where mask = pageSize - 1
        n = (pageSize + (addr .&. mask) - (offset .&. mask)) .&. mask


------------------------------------------------------------------------
-- RelinkM and ObjRelocState

-- | Identifies this should be a section index in the original binary.
--type BinSectionIndex = Word16

-- | Identifies this should be a section index in the new file file.
--type MergedSectionIndex = Word16

{-
-- | This is a map from the section index in the binary to the section
-- index in the output file.
--
-- The relinker preserves addresses of the original code, so we do not
-- need to adjust those.
type BinSectionIndexToMergeMap =
  Map BinSectionIndex MergedSectionIndex
-}

type RelinkM = IO

relinkFail :: String -> RelinkM a
relinkFail msg = do
   hPutStrLn stderr msg
   exitFailure

------------------------------------------------------------------------
-- Merger

{-
-- | Find relocation entries in section with given name.
findRelaEntries :: IsRelocationType r
                => ElfData
                -> Map BS.ByteString [ElfSection (Elf.ElfWordType 64)]
                   -- ^ Map from section names to sections with that name.
                -> BS.ByteString
                   -- ^ Name of section containing relocation entries.
                -> RelinkM [RelaEntry r]
findRelaEntries dta secMap nm = do
  case Map.findWithDefault [] nm secMap of
    -- Assume that no section means no relocations
    [] -> return []
    [s] -> case elfRelaEntries dta (BSL.fromStrict (elfSectionData s)) of
             Left e -> relinkFail e
             Right r -> pure r
    _ -> relinkFail $  "Multiple " ++ BSC.unpack nm ++ " sections in object file."
-}

{-
isLocalSymbol :: ElfSymbolTableEntry w -> Bool
isLocalSymbol sym = steBind sym == STB_LOCAL
-}

{-
-- | Find the symbol table in the elf.
findSymbolTable :: String
                   -- ^ Type of file for error reporting.
                -> Elf w
                    -- ^ Object with relocations
                -> RelinkM (ElfSymbolTable (Elf.ElfWordType w))
findSymbolTable nm obj = do
  case elfSymtab obj of
    -- Assume that no section means no relocations
    []  -> relinkFail $ "Could not find symbol table."
    [tbl] -> return tbl
    _   -> relinkFail $ "Multiple .symtab sections in " ++ nm ++ " file."
-}

-- | Check original binary satisfies preconditions.
checkBinaryAssumptions :: Elf.ElfHeader w -> RelinkM ()
checkBinaryAssumptions binaryHdr = do
  when (Elf.headerData binaryHdr /= Elf.ELFDATA2LSB) $ do
    relinkFail $ "Expected the original binary to be least-significant bit first."
  when (Elf.headerMachine binaryHdr /= Elf.EM_X86_64) $ do
    relinkFail $ "Only x86 64-bit object files are supported."
  when (Elf.headerFlags binaryHdr /= 0) $ do
    relinkFail $ "Expected elf flags in binary to be zero."

-- | Check object file satisfies preconditions.
checkObjectHeaderAssumptions :: Elf.ElfHeader w -> RelinkM ()
checkObjectHeaderAssumptions hdr = do
  when (Elf.headerData hdr /= Elf.ELFDATA2LSB) $ do
    relinkFail $ "Expected the new binary binary to be least-significant bit first."
  when (Elf.headerType hdr /= Elf.ET_REL) $ do
    relinkFail $ "Expected a relocatable file as input."
  when (Elf.headerMachine hdr /= Elf.EM_X86_64) $ do
    relinkFail $ "Only x86 64-bit executables are supported."
  when (Elf.headerFlags hdr /= 0) $ do
    relinkFail $ "Expected elf flags in new object to be zero."

{-
-- | Intermediate state used when mapping existing binary contents to new file.
data TransBinaryState = TransBinaryState { nextSectionIndex :: !MergedSectionIndex
                                         -- ^ Index for next section
                                         , binSectionMap    :: !(Map BinSectionIndex MergedSectionIndex)
                                         -- ^ Maps binary sections to merged section.
                                         , transSectionNames :: ![BS.ByteString]
                                           -- ^ Names of sections in new binary.
                                         , transSections :: ![(ElfSection Word64, Elf.FileOffset Word64)]
                                           -- ^ Sections added so far.
                                         , transNextPhdrIndex :: !SegmentIndex
                                           -- ^ Index for next program header
                                         , transPhdrs :: ![Elf.Phdr 64]
                                           -- ^ Program headers encountered so far.
                                         }

-- | Monad used for extracting code from binaries.
type TransBinaryM = StateT TransBinaryState IO

-- | Create a fresh section index.
freshSectionIndex :: BinSectionIndex -> TransBinaryM MergedSectionIndex
freshSectionIndex idx = do
  newIdx <- gets nextSectionIndex
  modify $ \s -> s { nextSectionIndex = newIdx + 1
                   , binSectionMap = Map.insert idx newIdx (binSectionMap s)
                   }
  return newIdx

-}

{-
addTransSection :: ElfSection Word64 -> Elf.FileOffset Word64 -> TransBinaryM ()
addTransSection sec o = seq sec $ do
  modify $ \s -> s { transSectionNames = Elf.elfSectionName sec : transSectionNames s
                   , transSections = (sec, o) : transSections s
                   }
-}

{-
-- | Generate a section in the translated bianry from a section in the input.
copyBinaryLoadSection :: (BS.ByteString -> BS.ByteString)
                      -- ^ Function for renaming sections.
                      -> ResolvedCodeRedirs (Elf.ElfWordType 64)
                      -- ^ Modifications to make to existing data
                      -> Word64
                         -- ^ Virtual address
                      -> Elf.FileOffset (Elf.ElfWordType 64)
                         -- ^ File offset
                      -> ElfSection (Elf.ElfWordType 64)
                      -> TransBinaryM (ElfSection (Elf.ElfWordType 64))
copyBinaryLoadSection nmFun rredirs vaddr secOffset sec = do
  let dta = elfSectionData sec
  -- Get new index
  idx <- freshSectionIndex (elfSectionIndex sec)
  let nm =  nmFun (elfSectionName sec)
  let newSec = sec
               { elfSectionIndex     = idx
               , elfSectionName      = nm
               , elfSectionData      = insertStaticRedirs rredirs vaddr dta
               }
  addTransSection newSec secOffset
  pure $! newSec
-}

{-
-- | Generate a deferred region from a region within the code segment
-- of the original binary.
copyBinaryLoadSegmentRegions
  :: (BS.ByteString -> BS.ByteString)
     -- ^ Function for renaming sections.
  -> ResolvedCodeRedirs (Elf.ElfWordType 64)
     -- ^ Modifications to make to existing data
  -> [ElfDataRegion 64]
     -- ^ Regions encountered so far.
  -> Bld.Builder
     -- ^ Contents of sections seen so far.
  -> Word64
     -- ^ Virtual address
  -> Elf.FileOffset (Elf.ElfWordType 64)
     -- ^ File offset
  -> [ElfDataRegion 64]
  -- ^ Regions in binary left to process.
  -> TransBinaryM ([ElfDataRegion 64], Bld.Builder, Elf.FileOffset (Elf.ElfWordType 64))
copyBinaryLoadSegmentRegions _ _ prevRegions prevContent _vaddr secOffset [] = do
  pure $! (reverse prevRegions, prevContent, secOffset)
copyBinaryLoadSegmentRegions nmFun rredirs prevRegions prevContent vaddr secOffset (thisRegion:restRegions) = do
 seq secOffset $ do
  case thisRegion of
    ElfDataElfHeader -> do
      error "Elf header appears in unexpected location."
    ElfDataSegmentHeaders -> do
      error "Elf segment headers appears in unexpected location."
    ElfDataSegment seg
      | elfSegmentType seg == PT_NOTE ->
          case toList (elfSegmentData seg) of
            [ElfDataSection sec]
              | elfSectionType sec == SHT_NOTE -> do
                  -- Get new note section
                  newSec <- copyBinaryLoadSection nmFun rredirs vaddr secOffset sec
                  -- Get new note segment
                  let newSeg = seg { elfSegmentData  = Seq.fromList [ElfDataSection newSec] }
                  let nextRegions = ElfDataSegment newSeg : prevRegions
                  let nextContent = prevContent <> Bld.byteString (Elf.elfSectionData newSec)
                  let sz :: Word64
                      sz = fromIntegral (BS.length (Elf.elfSectionData newSec))
                  --  Add program header
                  modify $ \s ->
                    let idx = transNextPhdrIndex s
                        phdr = mkPhdr idx newSeg secOffset sz sz
                     in s { transNextPhdrIndex = idx + 1
                          , transPhdrs = phdr : transPhdrs s
                          }
                  -- get next address and offset
                  let nextAddr = vaddr + elfSectionSize newSec
                  let nextOffset = Elf.incOffset secOffset sz
                  -- Get offset for next data
                  seq newSeg $ seq nextRegions $
                    copyBinaryLoadSegmentRegions nmFun rredirs nextRegions nextContent nextAddr nextOffset restRegions
            _ ->
                error "Elf PT_NOTE segment must contain note section (SHT_NOTE) as the only section."
      | otherwise ->
          error $ "Did not expect to see segment " ++ show (elfSegmentIndex seg)
              ++ " inside loadable segment."
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
      -- Get new section
      newSec <- copyBinaryLoadSection nmFun rredirs vaddr secOffset sec
      -- Add new section
      let nextRegions = ElfDataSection newSec : prevRegions
      let nextContent = prevContent <> Bld.byteString (Elf.elfSectionData newSec)
      -- get next address and offset
      let nextAddr = vaddr + elfSectionSize newSec
      let nextOffset = Elf.incOffset secOffset (fromIntegral (BS.length (Elf.elfSectionData newSec)))
      -- Copy rest
      seq nextRegions $
        copyBinaryLoadSegmentRegions nmFun rredirs nextRegions nextContent nextAddr nextOffset restRegions
    ElfDataRaw b -> do
      let dr = ElfDataRaw b
      let sz :: Word64
          sz = fromIntegral (BS.length b)
      let nextRegions = dr : prevRegions
      let nextContent = prevContent <> Bld.byteString b
      -- get next address and offset
      let nextAddr = vaddr + sz
      let nextOffset = Elf.incOffset secOffset sz
      seq dr $ seq nextRegions $
        copyBinaryLoadSegmentRegions nmFun rredirs nextRegions nextContent nextAddr nextOffset restRegions
-}

{-
-- | `paddingData n` generates a builder to add `n` bytes to binary.
paddingData :: Elf.ElfWordType 64 -> Bld.Builder
paddingData p = Bld.byteString (BS.replicate (fromIntegral p) 0)
-}

{-
-- | This function generates regions in the binary for the new object code
-- using relocation information and the `ObjectSectionInfo`.
mkObjCodeData :: RelocInfo Word64
              -> Bld.Builder
              -> ObjectSectionInfo X86_64_RelocationType
                 -- ^ Names of sections to look for and expected flags
              -> TransBinaryM Bld.Builder
mkObjCodeData relocInfo prevData secInfo = do
  let sec = objsecOrigSection secInfo
  -- Get relocations
  let relaEntries :: [RelaEntry X86_64_RelocationType]
      relaEntries = objsecRelocations secInfo
  -- Compute address of this section
  let secAddr :: Elf.ElfWordType 64
      secAddr = objsecBaseAddr secInfo
  -- Make new data
  let oldData = Elf.elfSectionData sec
  let newData = performRelocs relocInfo secAddr (Elf.elfSectionData sec) relaEntries
  when (BS.length oldData /= BS.length newData) $ do
    lift $ relinkFail "mkObjCodeData relocations have incorrect size"
  -- Generate new setion
  let newSec =
        sec { elfSectionIndex = objsecMergedIndex secInfo
            , elfSectionAddr  = secAddr
            , elfSectionData = newData
            }
  addTransSection newSec (objsecFileOffset secInfo)
  pure $ prevData <> paddingData (objsecPadding secInfo) <> Bld.byteString newData
-}

------------------------------------------------------------------------
-- Binary checking

{-
-- | Check for regions in the object that we did not expect.
checkBinaryRegions :: ElfDataRegion w
                 -- ^ Data region in input binary left to process
                   -> RelinkM ()
checkBinaryRegions thisRegion =
  -- Decide current action based on region
  case thisRegion of
    ElfDataElfHeader -> do
      error "Elf header outside loadable segment."
    ElfDataSegmentHeaders ->
      error "Elf segment table outside loadable segment."
    ElfDataGOT _ ->
      error "top-level .got table"
    ElfDataSection _ -> pure ()
    -- Regions to drop
    ElfDataSectionHeaders -> pure ()
    ElfDataSectionNameTable _ -> pure ()
    ElfDataStrtab _ -> pure ()
    ElfDataSymtab _ -> pure ()
    ElfDataRaw _ -> pure ()
    ElfDataSegment seg -> do
     error $ "Additional segment: " ++ show (elfSegmentIndex seg)
-}

-- | Create a bytestring with a jump to the immediate address.
x86_64_immediateJump :: Word64 -> BS.ByteString
x86_64_immediateJump addr = BSL.toStrict $ Bld.toLazyByteString $ mov_addr_to_r11 <> jump_r11
  where mov_addr_to_r11
          =  Bld.word8 0x49
          <> Bld.word8 0xBB
          <> Bld.word64LE addr
        jump_r11
          =  Bld.word8 0x41
          <> Bld.word8 0xFF
          <> Bld.word8 0xE3

{-
-- | Size of the absolute jump above.
absoluteJumpSize :: Int
absoluteJumpSize = 13
-}

{-
-- | Relative jump for x86 64
--
-- Note. the bytestring is 5 bytes.
relJump_x86_64 :: Int32 -> BS.ByteString
relJump_x86_64 d =
  let d0 :: Word8
      d0 = fromIntegral d
      d1 :: Word8
      d1 = fromIntegral (d `shiftR`  8)
      d2 :: Word8
      d2 = fromIntegral (d `shiftR` 16)
      d3 :: Word8
      d3 = fromIntegral (d `shiftR` 24)
   in BS.pack [ 0xE9, d0, d1, d2, d3 ]
-}

{-
-- | Size of the relative jump below.
relativeJumpSize :: Int
relativeJumpSize = 5
-}

{-
-- | This maps a symbol entry in the binary to a symbol table entry in
-- the new file.  This functions adds the suffice "_reopt" to each
-- symbol.
--
-- Note: This returns `Nothing` when the symbol table entry may be
-- dropped.
resolveBinarySymbolTableEntry :: Num (Elf.ElfWordType 64)
                              => BinSectionIndexToMergeMap
                                 -- ^ Maps section indices in binary
                                 -- to merged section index.
                              -> Set BS.ByteString
                                 -- ^ Set of symbols in the new object file.

                              -> ElfSymbolTableEntry (Elf.ElfWordType 64)
                              -> RelinkM (Maybe (ElfSymbolTableEntry (Elf.ElfWordType 64)))
resolveBinarySymbolTableEntry sectionIdxMap usedNames ste = do
  let suffix = "_orig"
  let nm =
        case steType ste of
         STT_SECTION -> ""
         STT_FILE -> steName ste
         STT_NOTYPE -> steName ste <> suffix
         STT_OBJECT -> steName ste <> suffix
         STT_FUNC -> steName ste <> suffix
         tp -> error $ "Symbol type " ++ show tp ++ " is not yet supported."

  when (Set.member nm usedNames) $ do
    error $ "Reopt given a binary that already uses symbol."
  case steIndex ste of
    SHN_ABS -> do
      pure $! (Just $! ste { steName = nm })
    SHN_COMMON -> do
      pure $! (Just $! ste { steName = nm })
    SHN_UNDEF  -> do
      pure $! (Just $! ste { steName = nm })
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

-}

{-
-- | Information about how specific sections in the object will be
-- mapped to the binary.
data ObjectSectionInfo r
  = ObjectSectionInfo
  { objsecSourceIndex :: !ObjectSectionIndex
    -- ^ Index of section in object.
  , objsecMergedIndex :: !MergedSectionIndex
    -- ^ Index of section in new binary.
  , objsecPadding  :: !(RelocationWord r)
    -- ^ Number of padding bytes to add before this section
  , objsecFileOffset :: !(Elf.FileOffset (RelocationWord r))
    -- ^ File offset for section
  , objsecBaseAddr :: !(RelocationWord r)
    -- ^ Base address of section after alignment
  , objsecRelocations :: !([RelaEntry r])
    -- ^ Relocations to apply
  , objsecOrigSection :: !(ElfSection (RelocationWord r))
    -- ^ Original section in object.
  }

-- | Name of this section
objsecName :: ObjectSectionInfo r -> BSC.ByteString
objsecName = Elf.elfSectionName . objsecOrigSection
-}

{-
checkSectionFlags :: (Bits w, Integral w, Show w)
                  => ElfSection w
                  -> ElfSectionFlags w
                  -> RelinkM ()
checkSectionFlags sec expectedFlags =
  when (elfSectionFlags sec /= expectedFlags) $ do
    relinkFail $ BSC.unpack (elfSectionName sec) ++ " section has an unexpected flags: "
      ++ show (elfSectionFlags sec) ++ "."
-}

{-

-- | Get relocation entries from name of section
type RelaFn r =  BS.ByteString -> RelinkM [RelaEntry r]
-}

{-

collectObjSectionInfo1 :: RelaFn X86_64_RelocationType
                       -> MergedSectionIndex
                       -> Elf.ElfWordType 64
                       -- ^ Base address of segment.
                       -> FileOffset (Elf.ElfWordType 64)
                       -- ^ file offset for start of section
                       -> ElfSection (Elf.ElfWordType 64)
                       -> RelinkM (ObjectSectionInfo X86_64_RelocationType, FileOffset (Elf.ElfWordType 64))
collectObjSectionInfo1 relaFn idx baseAddr off sec = do
  let addr = baseAddr + fromFileOffset off
  let nm = elfSectionName sec
  when (elfSectionType sec /= SHT_PROGBITS) $ do
    relinkFail $ BSC.unpack nm ++ " section expected to be type SHT_PROGBITS."
  let dta = elfSectionData sec
  when (toInteger (BS.length dta) /= toInteger (elfSectionSize sec)) $ do
    relinkFail $ "Object code section sizes must match data length."
  relaEntries <- relaFn (elfSectionName sec)

  -- Get alignemnt for this section
  let align = elfSectionAddrAlign sec
  -- Compute address of this section
  let secAddr :: Elf.ElfWordType 64
      secAddr = fixAlignment addr align
  let off' = incOffset off (secAddr - addr)
  -- Compute section information
  let osec = ObjectSectionInfo
            { objsecSourceIndex = elfSectionIndex sec
            , objsecMergedIndex = idx
            , objsecPadding  = secAddr - addr
            , objsecFileOffset = off'
            , objsecBaseAddr = secAddr
            , objsecRelocations = relaEntries
            , objsecOrigSection = sec
            }
  pure $ (osec, FileOffset (fromFileOffset off' + elfSectionFileSize sec))

-- | This iterates over elements of a list with an action and returns the sublist of results retuerned.
mapListMaybeAccumLM :: forall m s a b
           .  Monad m
           => (s -> a -> m (s,Maybe b))
           -> s
           -> [a]
           -> m (s, [b])
mapListMaybeAccumLM f = go []
  where go :: [b] -> s -> [a] -> m (s,[b])
        go p s [] = pure (s,reverse p)
        go p s (a:r) = do
          (s',mb) <- f s a
          let p' = case mb of
                     Nothing -> p
                     Just b -> b:p
          go p' s' r

-- | This function is used to generate the information needed to perform relocations.
collectObjSectionInfo2 :: RelaFn X86_64_RelocationType
                          -- ^ Function for generating relocations for section
                       -> Elf.ElfWordType 64
                       -- ^ Base address for code
                       -> (MergedSectionIndex, FileOffset (Elf.ElfWordType 64))
                       -- ^ Merged section index and offset in file
                       -> ElfSection (Elf.ElfWordType 64)
                       -- ^ Remaining sections in file.
                       -> RelinkM ( (MergedSectionIndex, FileOffset (Elf.ElfWordType 64))
                                  , Maybe (ObjectSectionInfo X86_64_RelocationType)
                                  )
collectObjSectionInfo2 relaFn baseAddr (idx,off) sec = do
  case elfSectionName sec of
    ".text" -> do
      checkSectionFlags sec (shf_alloc .|. shf_execinstr)
      (secInfo, off') <- collectObjSectionInfo1 relaFn idx baseAddr off sec
      pure ((idx+1,off'), Just secInfo)
    ".rodata" -> do
      checkSectionFlags sec shf_alloc
      (secInfo, off') <- collectObjSectionInfo1 relaFn idx baseAddr off sec
      pure ((idx+1,off'), Just secInfo)
    ".rodata.cst16" -> do
      checkSectionFlags sec (shf_alloc .|. shf_merge)
      (secInfo, off') <- collectObjSectionInfo1 relaFn idx baseAddr off sec
      pure ((idx+1,off'), Just secInfo)
    ".eh_frame" -> do
      checkSectionFlags sec shf_alloc
      (secInfo, off') <- collectObjSectionInfo1 relaFn idx baseAddr off sec
      pure ((idx+1,off'), Just secInfo)
    ".data" -> do
      when (elfSectionSize sec /= 0) $ do
       relinkFail $ "Relinker requires new object has empty .data."
      pure ((idx,off), Nothing)
    ".bss" -> do
      when (elfSectionSize sec /= 0) $ do
       relinkFail $ "Relinker requires new object has empty .bss."
      pure ((idx,off), Nothing)
    ".note.GNU-stack" -> do
      pure ((idx,off), Nothing)
    ".symtab" -> do
      pure ((idx,off), Nothing)
    ".strtab" -> do
      pure ((idx,off), Nothing)
    ".shstrtab" -> do
      pure ((idx,off), Nothing)
    nm -> do
      relinkFail $ "Unexpected section name: " ++ show nm

-- | This function is used to generate the information needed to perform relocations.
collectObjSectionInfo :: RelaFn X86_64_RelocationType
                          -- ^ Function for generating relocations for section
                      -> Elf.ElfWordType 64
                       -- ^ Base address for code
                      -> MergedSectionIndex
                          -- ^ Merged section index
                      -> FileOffset (Elf.ElfWordType 64)
                         -- ^ Offset in file
                      -> [ElfSection (Elf.ElfWordType 64)]
                         -- ^ Remaining sections in file.
                      -> RelinkM ( [ObjectSectionInfo X86_64_RelocationType]
                                 , MergedSectionIndex
                                 , FileOffset (Elf.ElfWordType 64)
                                 )
collectObjSectionInfo relaFn baseAddr idx off secs = do
  ((idx',off'), r) <- mapListMaybeAccumLM (collectObjSectionInfo2 relaFn baseAddr) (idx, off) secs
  pure (r, idx', off)
-}

{-
-- | Information needed to generate the new symbol table from current entries.
data MkSymbolTableContext w
   = MkSymbolTableContext
   { binSectionIndexMap :: !BinSectionIndexToMergeMap
   , objectSectionIndexMap :: !(Map ObjectSectionIndex (MergedSectionIndex, Elf.ElfWordType w))
     -- ^ Maps section index in object to new section index
     -- and base address of section.
     --
     -- Note that in object files, all sections have an address of `0`.
   }
-}

{-
-- | This maps a symbol entry in object to the new entry.
resolveObjectSymbolTableEntry :: Num (Elf.ElfWordType w)
                              => MkSymbolTableContext w
                              -> ElfSymbolTableEntry (Elf.ElfWordType w)
                              -> RelinkM (Maybe (ElfSymbolTableEntry (Elf.ElfWordType w)))
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
-}

{-
-- | Create a symbol table for the new file.
--
-- Returns new symbol table, data, and map for symbol table indices.
mkSymbolTable :: MergedSectionIndex
                 -- ^ Index for symbol table
              -> MkSymbolTableContext 64
              -> Elf 64
                 -- ^ Input binary symbol table information
              -> V.Vector (ElfSymbolTableEntry Word64)
                 -- ^ Symbols in the new binary.
              -> RelinkM ( ElfSymbolTable (Elf.ElfWordType 64)
                         , BS.ByteString
                         , Map BS.ByteString Word32
                         )
mkSymbolTable newSymtabIndex symbolCtx binary objFuncSymbols = do

  resolvedObjSyms <- fmap (V.mapMaybe id) $
    traverse (resolveObjectSymbolTableEntry symbolCtx)
             objFuncSymbols


  let objSymbolNames :: Set BS.ByteString
      objSymbolNames = Set.fromList
        [ steName ste
        | ste <- V.toList resolvedObjSyms
        ]

  binSyms <-
    case elfSymtab binary of
      -- If symbol table is missing, then just use no existing symbols.
      []  -> pure V.empty
      [tbl] -> pure (elfSymbolTableEntries tbl)
      _   ->
        relinkFail $ "Multiple .symtab sections in input binary."

  newBinSyms <- fmap (V.mapMaybe id) $
    traverse (resolveBinarySymbolTableEntry (binSectionIndexMap symbolCtx) objSymbolNames)
              binSyms

  let (localSyms, globalSyms)
        = V.partition isLocalSymbol
        $ resolvedObjSyms <> newBinSyms
  let symtab = Elf.ElfSymbolTable { Elf.elfSymbolTableIndex   = newSymtabIndex
                                  , Elf.elfSymbolTableEntries = localSyms <> globalSyms
                                  , Elf.elfSymbolTableLocalEntries = fromIntegral $ V.length localSyms
                                  }
  let (strtabData, strtabMap) = stringTable $ V.toList $ steName <$> elfSymbolTableEntries symtab

  pure (symtab, strtabData, strtabMap)
-}

{-
-- | Drop raw data prefix from the r3egion list
--
-- Used to drop padding added for alignment purposes.
dropLeadingRawData :: [ElfDataRegion w] -> [ElfDataRegion w]
dropLeadingRawData (ElfDataRaw _ :r) = dropLeadingRawData r
dropLeadingRawData r = r
-}

{-
-- | Get code and data segments from binary.
getCodeAndDataSegments :: Elf w -> RelinkM (ElfSegment w, ElfSegment w)
getCodeAndDataSegments binary = do
  (cs,rest0) <-
    case toList (binary^.elfFileData) of
      ElfDataSegment cs : drest | elfSegmentType cs == PT_LOAD -> pure (cs,drest)
      _ -> relinkFail "Expected code segment at start of file."
  (ds,rest) <-
    case dropLeadingRawData rest0 of
      ElfDataSegment ds : r | elfSegmentType ds == PT_LOAD -> pure (ds,r)
      _ -> relinkFail "Expected data segment after code segment."
  traverse_ checkBinaryRegions rest
  pure (cs, ds)
-}

{-
mkPhdr :: Elf.SegmentIndex -- ^ Index of segment in this section
       -> Elf.ElfSegment 64
       -> Elf.FileOffset Word64
       -> Elf.ElfWordType 64 -- ^ File size
       -> Elf.ElfWordType 64 -- ^ Memory size
       -> Elf.Phdr 64
mkPhdr idx seg fileOff fileSize memSize =
  Elf.Phdr { Elf.phdrSegmentIndex = idx
           , Elf.phdrSegmentType = elfSegmentType seg
           , Elf.phdrSegmentFlags = elfSegmentFlags seg
           , Elf.phdrSegmentVirtAddr = elfSegmentVirtAddr seg
           , Elf.phdrSegmentPhysAddr = elfSegmentPhysAddr seg
           , Elf.phdrSegmentAlign = pageSize
           , Elf.phdrFileStart = fileOff
           , Elf.phdrFileSize  = fileSize
           , Elf.phdrMemSize   = memSize
           }
-}

-- | Strict fold over a list with an index
ifoldlM' :: forall m t a b i . (Foldable t, Monad m, Num i) => (b -> i -> a -> m b) -> b -> t a -> m b
ifoldlM' f s0 l = seq s0 $ fst <$> foldlM g (s0, 0) l
  where g :: (b,i) -> a -> m (b,i)
        g (s,i) a = do
         s' <- f s i a
         let j = i+1
         seq s' $ seq j $ pure (s',j)

{-
-- | Return a part of a bytestring after checking range returned is in bounds.
checkedSlice :: String -> Elf.Range Word64 -> BS.ByteString -> RelinkM BS.ByteString
checkedSlice msg (o,sz) f = do
  when (toInteger o + toInteger sz > toInteger (BS.length f)) $ do
    relinkFail msg
  pure $! BS.take (fromIntegral sz) (BS.drop (fromIntegral o) f)
-}

------------------------------------------------------------------------
-- Phase 2. Identify binary address information.

-- | Information from scan state
data BinaryPhdrScanState
   = InitBinaryScanState
     -- ^ Initial binary scan state
   | SeenCodePhdr !Word16 !Word64
     -- ^ Code program header seen at given index.
   | SeenDataPhdr !Word16 !(Elf.Phdr 64) !(Elf.FileOffset Word64)
     -- ^ Seen data starting at offset in data to size of data.

-- | Update initial scan information of binary state from phdr.
scanBinaryPhdr :: BinaryPhdrScanState
               -> Word16 -- ^ Index of program header
               -> Elf.Phdr 64
               -> IO BinaryPhdrScanState
scanBinaryPhdr InitBinaryScanState idx phdr = do
  case Elf.phdrSegmentType phdr of
    Elf.PT_LOAD -> do
      let flags = Elf.phdrSegmentFlags phdr
      if flags `hasFlags` Elf.pf_x then do
        pure $! SeenCodePhdr idx (Elf.phdrSegmentVirtAddr phdr + Elf.phdrMemSize phdr)
       else
        pure $! InitBinaryScanState
    _ -> pure $! InitBinaryScanState
scanBinaryPhdr (SeenCodePhdr codeIdx codeEndOff) _idx phdr = do
  case Elf.phdrSegmentType phdr of
    Elf.PT_LOAD -> do
      let flags = Elf.phdrSegmentFlags phdr
      when (flags `hasFlags` Elf.pf_x) $ do
        relinkFail "Found multiple executable segments."
      let phdrFileStart = Elf.phdrFileStart phdr
      let phdrFileEnd   = phdrFileStart `Elf.incOffset` Elf.phdrFileSize phdr
      pure $! SeenDataPhdr codeIdx phdr phdrFileEnd
    _ -> pure $! SeenCodePhdr codeIdx codeEndOff
scanBinaryPhdr (SeenDataPhdr codeIdx dataFirstPhdr dataFileEnd) _idx phdr =
  case Elf.phdrSegmentType phdr of
    Elf.PT_LOAD -> do
      let flags = Elf.phdrSegmentFlags phdr
      when (flags `hasFlags` Elf.pf_x) $ do
        relinkFail "Found multiple executable segments."
      let phdrFileStart = Elf.phdrFileStart phdr
      when (dataFileEnd > phdrFileStart) $ do
        relinkFail "Found overlapping load segment."
      let phdrFileEnd   = phdrFileStart `Elf.incOffset` Elf.phdrFileSize phdr
      pure $! SeenDataPhdr codeIdx dataFirstPhdr phdrFileEnd
    _ -> pure $! SeenDataPhdr codeIdx dataFirstPhdr dataFileEnd
------------------------------------------------------------------------
-- Phase 3.  Collect Object information

{-
-- | Contents for collecting
data ObjInfoContext
  = ObjInfoContext { objctxHeader :: !(Elf.ElfHeader 64)
                   -- ^ Header for object file
                   , objctxContents :: !BS.ByteString
                   -- ^ Contents of object file.
                   , objctxShdrs :: !(V.Vector (Elf.ShdrEntry Word32 Word64))
                     -- ^ Object section headers
                   }

-- | RELA information needed to parse relocation
data ObjRelaInfo = ObjRelaInfo { objRelaSymtabIdx :: !Word32
                                 -- ^ Index of symbol table for relocations
                                 --
                                 -- Note. has not been checked
                               , objRelaData :: !(Elf.Range Word64)
                                 -- ^ Relocation data file range
                               }

-- | Information extracted from pass over section headers in object file.
data ObjInfo =
  ObjInfo { objinfoRelaMap :: !(Map Word16 ObjRelaInfo)
            -- ^ Maps object section indices to the relocation entry information for them
            --
            -- Note. We only support RELA format.
          , objinfoSymtabs :: !(Map Word16 (V.Vector (Elf.ElfSymbolTableEntry BS.ByteString Word64)))
            -- ^ Map SYMTAB section indices to their entries.
          , objinfoSpillEnd :: !Word64
            -- ^ End address for code that spilled to new section.
          }
-}

{-
-- | This is part of the pass over object section to collect
-- informationg
collectObjShdrInfo :: ObjInfoContext
                      -- ^ Context
                   -> ObjInfo
                      -- ^ Object references
                   -> Word16
                      -- ^ This section header index
                   -> Elf.ShdrEntry Word32 Word64
                      -- ^ Section header to process
                   -> RelinkM ObjInfo
collectObjShdrInfo ctx objInfo thisIdx shdr = do
  let hdr = objctxHeader ctx
  let shdrs = objctxShdrs ctx
  let flags = Elf.shdrFlags shdr
  case Elf.shdrType shdr of
    -- Skip initial null section.
    Elf.SHT_NULL -> do
      pure objInfo
    Elf.SHT_REL -> relinkFail "REL entries not supported."
    Elf.SHT_RELA -> do
      -- Get index of section this applies to
      let tgtIdx = Elf.shdrInfo shdr
      unless (0 < tgtIdx && toInteger tgtIdx < toInteger (V.length shdrs)) $ do
        relinkFail "Invalid relocation index"

      let relaInfo = ObjRelaInfo { objRelaSymtabIdx = Elf.shdrLink shdr
                                 , objRelaData = Elf.shdrFileRange shdr
                                 }
      pure $ objInfo { objinfoRelaMap = Map.insert (fromIntegral tgtIdx) relaInfo (objinfoRelaMap objInfo) }
    Elf.SHT_SYMTAB -> do
      let strtabIdx :: Word32
          strtabIdx = Elf.shdrLink shdr
      -- check string tab index is valid
      unless (0 < strtabIdx && toInteger strtabIdx < toInteger (V.length shdrs)) $ do
        relinkFail "Invalid relocation index"
      -- Get string table section header for symtab
      let strtabShdr = shdrs V.! fromIntegral strtabIdx
      -- Get string table data
      strtabData <-
        checkedSlice "Invalid file region for strtab."
                     (Elf.shdrFileRange strtabShdr)
                     (objctxContents ctx)
      -- Get symboltable data
      symtabData <- checkedSlice "Invalid file region for symtab."
                                 (Elf.shdrFileRange shdr)
                                 (objctxContents ctx)
      -- Parse entries
      let cl = Elf.headerClass hdr
      let dta = Elf.headerData hdr
      entries <-
        case Elf.getSymbolTableEntries cl dta strtabData symtabData of
          Left msg -> relinkFail (show msg)
          Right entries -> pure entries
      -- Add symbol table to data
      seq entries $ pure $
        objInfo { objinfoSymtabs = Map.insert thisIdx entries (objinfoSymtabs objInfo) }
    Elf.SHT_PROGBITS
      | flags `hasFlags` Elf.shf_alloc -> do
          when (flags `hasFlags` Elf.shf_write) $ do
            relinkFail "Do not support writable sections."
          unless (flags `hasFlags` Elf.shf_execinstr) $ do
            relinkFail "Only support executable sections."
          when (flags `hasFlags` Elf.shf_merge) $ do
            relinkFail "Do not support mergable sections."
          when (flags `hasFlags` Elf.shf_tls) $ do
            relinkFail "Do not support TLS in object files."
          let newEnd = objinfoSpillEnd objInfo + Elf.shdrSize shdr
          pure $! objInfo { objinfoSpillEnd = newEnd }

    -- Skip other sections
    _ -> pure $! objInfo
-}

{-
-- | Information about an unused region
data UnusedRegion =
  UnusedRegion { unusedRegionStart :: !Word64
               , unusedRegionByteCount :: !Word64
               , unusedRegionSymbol :: !(Maybe BSC.ByteString)
                 -- ^ The name of the symbol that used to occupy this region.
                 --
                 -- If the relinker is tasked with replacing a symbol
                 -- The relinker will attempt to replace the
               }
-}

{-
-- | Information about a section that will be initialized after copying
-- the binary section headers.
type DeferredShdr = Maybe (Word32, Elf.ShdrEntry Word32 Word64)

type DeferredSectionLens = Lens' NewSectionInfo DeferredShdr

-- | Information about sections to add new binary.
data NewSectionInfo = NewSectionInfo
  { nsiSectionCount :: !Word32
    -- ^ Number of sections added so far (for generating new sections.
  , nsiCopyShdrs :: !(Map Word32 (Elf.ShdrEntry BS.ByteString Word64))
    -- ^ Sections to copy
  , nsiGnuHash  :: !DeferredShdr
    -- ^ ".gnu.hash" section which will be generated from symbol tables.
  , nsiSymtab   :: !DeferredShdr
    -- ^ Symbol table
  , nsiStrtab   :: !DeferredShdr
    -- ^ String table for symbol table names
  , nsiShstrtab :: !DeferredShdr
    -- ^ String table for section header names.
  }

-- | Records section header count for purposes of numbering
nsiSectionCountLens :: Lens' NewSectionInfo Word32
nsiSectionCountLens = lens nsiSectionCount (\s v -> s { nsiSectionCount = v })

nsiGnuHashLens :: DeferredSectionLens
nsiGnuHashLens = lens nsiGnuHash (\s v -> s { nsiGnuHash = v })

nsiSymtabLens :: DeferredSectionLens
nsiSymtabLens = lens nsiSymtab (\s v -> s { nsiSymtab = v })

nsiStrtabLens :: DeferredSectionLens
nsiStrtabLens = lens nsiStrtab (\s v -> s { nsiStrtab = v })

nsiShstrtabLens :: DeferredSectionLens
nsiShstrtabLens = lens nsiShstrtab (\s v -> s { nsiShstrtab = v })


-- | Action that generates a new section
type BinarySectionAction
   = NewSectionInfo
   -> Word16
   -> Elf.ShdrEntry Word32 Word64
   -> RelinkM NewSectionInfo

type BinarySectionRule = (BS.ByteString, BinarySectionAction)

sectionRule :: BS.ByteString -> BinarySectionAction -> BinarySectionRule
sectionRule nm a = (nm, a)

-- | Drop section matching name from old binary
dropSectionRule :: BS.ByteString -> BinarySectionRule
dropSectionRule nm = sectionRule nm $ \s _ _ -> do
  -- Check
  pure s

-- | Copy section matching name from old binary to new
copySectionRule :: BS.ByteString -> BinarySectionRule
copySectionRule nm = sectionRule nm $ \s _ shdr -> do
  let cnt = nsiSectionCount s
  let shdr' = shdr { Elf.shdrName = ".orig." <> nm }
  pure $ s { nsiSectionCount = cnt + 1
           , nsiCopyShdrs = Map.insert cnt shdr' (nsiCopyShdrs s)
           }

-- | Match a section with the given name and update the state to handle it later.
deferredSectionRule :: BS.ByteString -> DeferredSectionLens -> BinarySectionRule
deferredSectionRule nm l = sectionRule ".gnu.hash" $ \s _idx shdr -> do
  let cnt = nsiSectionCount s
  case s^.l of
    Just _ -> relinkFail $ printf "Duplicate section name %s." (BSC.unpack nm)
    Nothing -> pure ()
  seq cnt $ pure $ s & nsiSectionCountLens .~ cnt + 1
                     & l .~ Just (cnt, shdr)

binarySectionRules :: Map BS.ByteString BinarySectionAction
binarySectionRules = Map.fromList
  [-- Copy empty section
    copySectionRule ""
    -- Use same interpreter as binary
  , copySectionRule ".interp"
    -- Copy ABI note
  , copySectionRule ".note.ABI-tag"
  -- We drop the build-id for now, but could consider recomputing it.
  , dropSectionRule ".note.gnu.build-id"
  -- Defer generation of .gnu.hash
  , deferredSectionRule ".gnu.hash"  nsiGnuHashLens
  -- Copy dynamic symbol table (we shouldn't need to change this as all symbols
  -- in the object file should be from the object file.
  , copySectionRule ".dynsym"
  , copySectionRule ".dynstr"
    -- Copy GNU version sections unchanged
  , copySectionRule ".gnu.version"
  , copySectionRule ".gnu.version_d"
  , copySectionRule ".gnu.version_r"
    -- .rela.dyn has relocations on data which is unchanged
  , copySectionRule ".rela.dyn"
    -- Copy .plt and global offset table
  , copySectionRule ".plt"
  , copySectionRule ".got"
    -- .rela.plt has relocations on plt which is unchanged
  , copySectionRule ".rela.plt"
    -- The code in the .init and .fini sections are unchanged
    -- TODO: See if we have lifted this code.
  , copySectionRule ".init"
  , copySectionRule ".fini"
    -- Copy .text section as layout is presevered
  , copySectionRule ".text"
    -- Copy read-only data (should be unchanged)
  , copySectionRule ".rodata"
    -- Copy exception frames
  , copySectionRule ".eh_frame_hdr"
  , copySectionRule ".eh_frame"
    -- Copy .init_array and .fini_array
  , copySectionRule ".init_array"
  , copySectionRule ".fini_array"

    -- Copy data sections
  , copySectionRule ".data.rel.ro"
  , copySectionRule ".dynamic"
  , copySectionRule ".got"
  , copySectionRule ".data"
  , copySectionRule ".bss"

    -- Drop comment (which has compiler) and debug information
  , dropSectionRule ".comment"
  , dropSectionRule ".debug_aranges"
  , dropSectionRule ".debug_info"
  , dropSectionRule ".debug_abbrev"
  , dropSectionRule ".debug_line"
  , dropSectionRule ".debug_str"
  , dropSectionRule ".debug_loc"
  , dropSectionRule ".debug_ranges"
    -- Defer symbol table, string table and shstrtab
  , deferredSectionRule ".symtab" nsiSymtabLens
  , deferredSectionRule ".strtab" nsiStrtabLens
  , deferredSectionRule ".shstrtab" nsiShstrtabLens
  ]
-}

{-

TODO:
1. Recreate .gnu.hash"
2. Recreate .note.gnu-build-id"

0x0000000000000004 (HASH)               0x400308
0x000000006ffffef5 (GNU_HASH)           0x400320

-}


{-
-- | Copy binary section information into new section.
copyBinarySectionHeader :: BS.ByteString -- ^ Section name table
                        -> NewSectionInfo
                        -> Word16
                           -- ^ Section inex
                        -> Elf.ShdrEntry Word32 (Elf.ElfWordType 64)
                           -- ^ Section header
                        -> RelinkM NewSectionInfo
copyBinarySectionHeader shStrTable _secInfo shdrIndex shdr = do
  nm <-
    case Elf.lookupString (Elf.shdrName shdr) shStrTable of
      Left _e -> relinkFail $ printf "Unresolvable name for section %d." shdrIndex
      Right nm -> pure nm
  undefined nm binarySectionRules

-}
-- | Information collected during first pass over binary program header information.
data BinaryPhdrLayout
   = BinaryPhdrLayout { bplBinDataAddr        :: !Word64
                        -- ^ Address at start of data segment.
                      , bplNewDataEndOffset :: !(Elf.FileOffset Word64)
                        -- ^ end offset for data
                      , bplNewDataContent :: !Bld.Builder
                      , bplBinCodePhdrIndex :: !Word16
                        -- ^ Index of program header table.
                      }

-- | Get bytes from a section header file.
getShdrContents :: Elf.ShdrEntry nm Word64 -> Elf.ElfHeaderInfo 64 -> BS.ByteString
getShdrContents shdr hdrInfo =
  let o  = fromIntegral $ Elf.shdrOff shdr
      sz = fromIntegral $ Elf.shdrSize shdr
   in BS.take sz $ BS.drop o $ Elf.headerFileContents hdrInfo


getCodeIndex :: BinaryPhdrScanState
             -> IO Word16
getCodeIndex  r =
  case r of
    InitBinaryScanState -> relinkFail "Could not find code segment in relinking."
    SeenCodePhdr codeIdx _codeEndAddr ->
      pure codeIdx
    SeenDataPhdr codeIdx _firstDataPhdr _dataFileEnd ->
      pure codeIdx

mkBinaryPhdrLayout :: Elf.ElfHeaderInfo 64
                   -> Elf.FileOffset Word64 -- ^ File offset for end of object code.
                   -> BinaryPhdrScanState
                   -> IO BinaryPhdrLayout
mkBinaryPhdrLayout binHeaderInfo objCodeEndOffset r =
  case r of
    InitBinaryScanState -> relinkFail "Could not find code segment in relinking."
    SeenCodePhdr codeIdx codeEndAddr ->
      pure $! BinaryPhdrLayout { bplBinDataAddr = codeEndAddr
                               , bplNewDataEndOffset = objCodeEndOffset
                               , bplNewDataContent = mempty
                               , bplBinCodePhdrIndex = codeIdx
                               }
    SeenDataPhdr codeIdx firstDataPhdr dataFileEnd -> do
      -- Get virtual address of data
      let binDataVirtAddr :: Word64
          binDataVirtAddr = Elf.phdrSegmentVirtAddr firstDataPhdr
      let c :: BS.ByteString
          c = Elf.headerFileContents binHeaderInfo
      let o :: Elf.FileOffset Word64
          o = Elf.phdrFileStart firstDataPhdr
      let sz :: Word64
          sz = Elf.fromFileOffset dataFileEnd - Elf.fromFileOffset o
      -- Compute file offset in new binary.
      let o' :: Elf.FileOffset Word64
          o' = resolveSegmentOffset objCodeEndOffset binDataVirtAddr
      let padding :: Bld.Builder
          padding = Elf.alignmentPadding objCodeEndOffset o'
      let newData  :: Bld.Builder
          newData = Bld.byteString $ BS.take (fromIntegral sz) $ BS.drop (fromIntegral (Elf.fromFileOffset o)) c
      pure $! BinaryPhdrLayout { bplBinDataAddr = Elf.phdrSegmentVirtAddr firstDataPhdr
                               , bplNewDataEndOffset = Elf.incOffset o' sz
                               , bplNewDataContent = padding <> newData
                               , bplBinCodePhdrIndex = codeIdx
                               }

-- |  Resolve symbol table entry into offset.
finalizeSymtabEntryNameIndex :: HasCallStack
                             => Map BS.ByteString Word32
                             -> Elf.ElfSymbolTableEntry BS.ByteString v
                             -> Elf.ElfSymbolTableEntry Word32 v
finalizeSymtabEntryNameIndex strtabOffsetMap e =
  case Map.lookup (Elf.steName e) strtabOffsetMap of
    Nothing -> error $ "internal failure: Unexpected symbol."
    Just idx -> e { Elf.steName = idx }


-- | Replace section header name index with bytestring.
substituteShdrName :: BS.ByteString -> Elf.ShdrEntry Word32 Word64 -> IO (Elf.ShdrEntry BS.ByteString Word64)
substituteShdrName shstrtab shdr =
  case Elf.lookupString (Elf.shdrName shdr) shstrtab of
    Left _ -> relinkFail "Failed to find section header name."
    Right nm -> pure $ shdr { Elf.shdrName = nm }

-- | Replace a section shdr entry bytestring with an index.
finalizeShdrNameIndex :: HasCallStack
                      => Map BS.ByteString Word32
                      -> Elf.ShdrEntry BS.ByteString v
                      -> Elf.ShdrEntry Word32 v
finalizeShdrNameIndex strtabOffsetMap e =
  case Map.lookup (Elf.shdrName e) strtabOffsetMap of
    Nothing -> error $ "internal failure: Unexpected section header name."
    Just idx -> e { Elf.shdrName = idx }

-- | Get section header table.
getShdrTable :: Elf.ElfHeaderInfo 64 -> IO (V.Vector (Elf.ShdrEntry BS.ByteString Word64))
getShdrTable binHeaderInfo = do
  -- Index of section header entry for section name table.
  let shstrtabShdrIndex :: Word16
      shstrtabShdrIndex = Elf.shdrNameIdx binHeaderInfo

  when (shstrtabShdrIndex == 0) $ do
    relinkFail "Require non-zero shstrtab index."
  -- Sections in binary
  let rawBinShdrs :: V.Vector (Elf.ShdrEntry Word32 Word64)
      rawBinShdrs = Elf.headerSectionHeaders binHeaderInfo
  when (fromIntegral shstrtabShdrIndex < V.length rawBinShdrs) $ do
    relinkFail "Invalid binary section header table"
  let binShstrtab :: BS.ByteString
      binShstrtab = getShdrContents (rawBinShdrs V.! fromIntegral shstrtabShdrIndex) binHeaderInfo
  traverse (substituteShdrName binShstrtab) rawBinShdrs

--------------------------------------------------------------------------------
-- BinarySectionLayout

-- | Information needed to compute code layout.
data NewCodeInfo = NewCodeInfo { nciBinDataAddr :: !Word64
                                 -- ^ Start address of data section.
                               }

-- | Section information for new binary.
data NewSectionInfo
   = OldBinaryCodeSection !(Elf.ShdrEntry BS.ByteString Word64)
   | NewBinaryCodeSection
   | OldBinaryDataSection !(Elf.ShdrEntry BS.ByteString Word64)
   | NewSymtabSection     !(Elf.ShdrEntry BS.ByteString Word64)
   | NewStrtabSection     !(Elf.ShdrEntry BS.ByteString Word64)
   | NewShstrtabSection   !(Elf.ShdrEntry BS.ByteString Word64)

-- | Information needed to compute code layout.
data NewSectionRenderInfo =
  NewSectionRenderInfo
  { nsriBinSectionIndexMap :: !(Map Word16 Word16)
    -- ^ Map from section header indices in original binary to values
    -- in new binary.
  , nsriOverflowOffset :: !(Elf.FileOffset Word64)
    -- ^ Starting file offset of new code.
  , nsriOverflowAddr :: !Word64
    -- ^ Address of new code section (relative to base in dynamically linked files)
  , nsriOverflowSize    :: !Word64
    -- ^ Code size
  , nsriDataDelta :: !Word64
    -- ^ Amount to increment data
  , nsriSymtabOffset  :: !(Elf.FileOffset Word64)
  , nsriSymtabSize :: !Word64
  , nsriSymtabLocalCount :: !Word32
  , nsriStrtabIndex :: !Word16
    -- ^ Section index of string table in new file.
  , nsriStrtabOffset  :: !(Elf.FileOffset Word64)
  , nsriStrtabSize :: !Word64
  , nsriShstabOffset  :: !(Elf.FileOffset Word64)
  , nsriShstrtabSize :: !Word64
  }

-- | Function that maps old section header link to new section header link.
nsriMapShdrLink :: NewSectionRenderInfo -> Word32 -> Word32
nsriMapShdrLink nsri link =
  if link <= 0xFFFF then
    let link' = fromIntegral link
     in fromIntegral $ Map.findWithDefault link' link' (nsriBinSectionIndexMap nsri)
   else
    link

renderNewSectionInfo :: NewSectionRenderInfo
                     -> NewSectionInfo
                     -> Elf.ShdrEntry BS.ByteString Word64
renderNewSectionInfo nsri nsi =
  case nsi of
    OldBinaryCodeSection shdr ->
      let newInfo | Elf.shdrFlags shdr `hasFlags` Elf.shf_info_link =
                    nsriMapShdrLink nsri (Elf.shdrInfo shdr)
                  | otherwise = Elf.shdrInfo shdr
       in shdr { Elf.shdrLink = nsriMapShdrLink nsri (Elf.shdrLink shdr)
               , Elf.shdrInfo = newInfo
               }
    NewBinaryCodeSection ->
      Elf.ShdrEntry
      { Elf.shdrName  = newCodeSectionName
      , Elf.shdrType  = Elf.SHT_PROGBITS
      , Elf.shdrFlags = Elf.shf_alloc .|. Elf.shf_execinstr
      , Elf.shdrAddr  = nsriOverflowAddr nsri
      , Elf.shdrOff   = Elf.fromFileOffset (nsriOverflowOffset nsri)
      , Elf.shdrSize  = nsriOverflowSize nsri
      , Elf.shdrLink  = 0
      , Elf.shdrInfo  = 0
      , Elf.shdrAddrAlign = 16
      , Elf.shdrEntSize = 0
      }
    OldBinaryDataSection shdr ->
      shdr { Elf.shdrLink = nsriMapShdrLink nsri (Elf.shdrLink shdr)
           , Elf.shdrOff = Elf.shdrOff shdr + nsriDataDelta nsri
           }
    NewSymtabSection shdr ->
      shdr { Elf.shdrOff  = Elf.fromFileOffset $ nsriSymtabOffset nsri
           , Elf.shdrSize = nsriSymtabSize nsri
           , Elf.shdrLink = fromIntegral (nsriStrtabIndex nsri)
           , Elf.shdrInfo = nsriSymtabLocalCount nsri
           }
    NewStrtabSection shdr ->
      shdr { Elf.shdrOff  = Elf.fromFileOffset $ nsriStrtabOffset nsri
           , Elf.shdrSize = nsriStrtabSize nsri
           }
    NewShstrtabSection shdr ->
      shdr { Elf.shdrOff  = Elf.fromFileOffset $ nsriShstabOffset nsri
           , Elf.shdrSize = nsriShstrtabSize nsri
           }


newCodeSectionName :: BS.ByteString
newCodeSectionName = ".text.reopt"

-- | Layout information computed from scan.
data BinarySectionLayout =
  BSL { bslSectionCount :: !Word16
        -- ^ Number of sections added
      , bslSectionNames :: ![BS.ByteString]
        -- ^ Names of setions
      , bslSectionHeaders :: [NewSectionInfo]
        --- ^ List of sections in reverse order.
      , bslSectionMap :: !(Map Word16 Word16)
        -- ^ Map from section indices in binary to new section indices
        -- when they are copied over.
      , bslNeedNewCodeShdr :: !Bool
        -- ^ Indicates if we need to add a new code section header
        -- prior to data.
      , bslSymtabIndex :: !Word16
        -- ^ Index of symbol table (or zero if not found)
      , bslStrtabIndex :: !Word16
        -- ^ Index of string table (or zero if not found)
      }

addBinShdr :: BinarySectionLayout
           -> Word16
           -> BS.ByteString
           -> NewSectionInfo
           -> BinarySectionLayout
addBinShdr bsl binIdx nm secInfo = seq nm $ seq secInfo $
  let newIdx = bslSectionCount bsl
   in bsl { bslSectionCount = newIdx + 1
          , bslSectionNames = nm : bslSectionNames bsl
          , bslSectionHeaders = secInfo:bslSectionHeaders bsl
          , bslSectionMap = Map.insert binIdx newIdx (bslSectionMap bsl)
          }

-- | This is used in a fold over the section headers to infer layout
-- information.
layoutBinaryShdr :: NewCodeInfo
               -> BinarySectionLayout
               -> Word16
               -> Elf.ShdrEntry BS.ByteString Word64
               -> IO BinarySectionLayout
layoutBinaryShdr nci bsl0 idx shdr = seq idx $ do
  let bsl1 | bslNeedNewCodeShdr bsl0, Elf.shdrAddr shdr >= nciBinDataAddr nci =
                bsl0 { bslSectionCount    = bslSectionCount bsl0 + 1
                     , bslSectionNames    = newCodeSectionName : bslSectionNames bsl0
                     , bslSectionHeaders  = NewBinaryCodeSection : bslSectionHeaders bsl0
                     , bslNeedNewCodeShdr = False
                     }
           | otherwise = bsl0
  if Elf.shdrAddr shdr >= nciBinDataAddr nci then
    pure $ addBinShdr bsl1 idx (Elf.shdrName shdr) $ OldBinaryDataSection shdr
   else if Elf.shdrName shdr == ".symtab" then do
    let bsl2 = bsl1 { bslSymtabIndex = idx }
    pure $ addBinShdr bsl2 idx (Elf.shdrName shdr) $ NewSymtabSection shdr
   else if Elf.shdrName shdr == ".strtab" then do
    let bsl2 = bsl1 { bslStrtabIndex = idx }
    pure $ addBinShdr bsl2 idx (Elf.shdrName shdr) $ NewStrtabSection shdr
   else if Elf.shdrName shdr == ".shstrtab" then
    pure $ addBinShdr bsl1 idx (Elf.shdrName shdr) $ NewShstrtabSection shdr
   else
    pure $ addBinShdr bsl1 idx (Elf.shdrName shdr) $ OldBinaryCodeSection shdr

-- | This merges an existing elf binary and new object file to create a
-- combined binary.  The object file is expected to be passed with function
-- sections.
--
-- The algorithm for merging runs in multiple phases:
-- 1. Header validation Phase.
--    Validates the elf header for the binary and object files.
-- 2. Object code layout phase
--    This traverses sections from phase 2 to determine where each section
--    will fit in memory.
-- 3. Object section collection Phase.
--    Traverses the object file section header table to layout information.
--    information needed for layout.  In particular, this identifies
--    the order to add data to add to each segment
-- 4. Program header generation phase.
--    This generates the program header table.
-- 5. Segment data generation phase.
--    This generates the data in each segment in the program.
-- 6. Generate symbol table and string table data and collect information for section header table.
-- 7. Generate section header name table (.shstrtab) and section header table.
mergeObject :: HasCallStack
             => Elf.ElfHeaderInfo 64 -- ^ Existing binary
             -> Elf.ElfHeaderInfo 64 -- ^ Object file to merge into existing binary.
             -> (Int -> Maybe BS.ByteString)
                -- ^ Map from binary symbol table index to the name of the symbol
                -- in the object code.
             -> [CodeRedirection Word64]
                -- ^ Redirections from old binary to new code.
             -> IO BSL.ByteString
mergeObject binHeaderInfo objHeaderInfo objNameOfBinSymbolIndex _redirs = do
  let binHdr = Elf.header binHeaderInfo
  let objHdr = Elf.header objHeaderInfo

  -- Step 1. Header validation
  let cl     = Elf.ELFCLASS64
  let elfDta = Elf.ELFDATA2LSB

  -- Check this is an executable
  case Elf.headerType binHdr of
    Elf.ET_EXEC -> pure ()
    _ -> relinkFail $ "Expected the original binary is an executable."
  -- Check original binary properties
  checkBinaryAssumptions binHdr
  -- Check object assumptions
  checkObjectHeaderAssumptions objHdr
  -- Check OSABI compat
  when (Elf.headerOSABI objHdr /= Elf.headerOSABI binHdr) $ do
    relinkFail $ "Expected the new object to use the same OS ABI as original."

  -- Step 2. Parse binary segments to infer information to layout
  -- object code.
  let binPhdrCount :: Word16
      binPhdrCount = Elf.phdrCount binHeaderInfo
  binPhdrScanResults <- do
    let visitPhdr :: BinaryPhdrScanState -> Word16 -> IO BinaryPhdrScanState
        visitPhdr s i = seq s $ seq i $
          if i < binPhdrCount then do
            t <- scanBinaryPhdr s i (Elf.phdrByIndex binHeaderInfo i)
            visitPhdr t (i+1)
           else
            pure s
    visitPhdr InitBinaryScanState 0

  binCodePhdrIndex <- getCodeIndex binPhdrScanResults

  -- Code program header in binary
  let binCodePhdr :: Elf.Phdr 64
      binCodePhdr = Elf.phdrByIndex binHeaderInfo binCodePhdrIndex

--  let binCodeStartAddr = Elf.phdrSegmentVirtAddr binCodePhdr

  {-
  -- Step 4. Collect object section header information.
  --
  -- As part of this step, we check to see if function to be inserted that
  -- replaces an existing function can fit inside the space of the function
  -- to be replaced.  If so, we mark that function has replacing the existing
  -- one.  If not, we add the function to the set of functions that we need
  -- to find space for.
  _objInfo <- do
    -- Get section headers
    let objShdrs = Elf.headerSectionHeaders objHeaderInfo
    -- Create information needed to parse object section headers
    let ctx =
          ObjInfoContext
          { objctxHeader = objHdr
          , objctxContents = Elf.headerFileContents objHeaderInfo
          , objctxShdrs = objShdrs
          }
     -- Create initial object information.
    let initObjInfo = ObjInfo { objinfoRelaMap = Map.empty
                              , objinfoSymtabs = Map.empty
                              , objinfoSpillEnd = spillCodeStartAddr
                              }
    -- Process sections after null section up front.
    ifoldlM' (collectObjShdrInfo ctx) initObjInfo (V.drop 1 objShdrs)
-}

  -- Step 5. Copy functions in object file that cannot overwrite previous
  -- definition into new code section at end of this section.
  -- When doing this, generate jumps that need to be added.

  -- TODO: Replace this logic, with
  -- Check that the code size fits in a 32-bit signed number.  If so, we will
  -- use relative jumps, if not wwill use absolute jumps.
--  let maxCodeSize = objInfoCodeEndUpperBound objInfo - binCodeStartAddr
--  let useRelJumps = maxCodeSize <= 2^31 - 1
--  let jumpSize = if useRelJumps
--    relinkFail



  -- Step 6. Compute space consumed by inserting redirections into
  -- existing application.

  -- Step 7. Traverse binary section headers to copy to new binary

--  Let ElfAbsoluteSize codeMemSize = elfSegmentMemSize codeSeg

  {-
  let (relaPairs, nonrelaSecs) = partitionEithers
         [ case BS.stripPrefix ".rela" (elfSectionName sec) of
             Just nm -> Left (nm, [sec])
             Nothing -> Right sec
           | sec <- obj^..elfSections
         ]
  let relaSecMap = Map.fromList relaPairs
  let relaFn nm = findRelaEntries ELFDATA2LSB relaSecMap nm
  -- Get layout of object code.
  (objSections, nextMergedIdx, objCodeEnd) <- do
    collectObjSectionInfo relaFn (elfSegmentVirtAddr codeSeg) 1 (FileOffset codeMemSize) nonrelaSecs
-}

  -- Make phdr for code segment
  --let codePhdr = mkPhdr 0 codeSeg (FileOffset 0) (fromFileOffset objCodeEnd) (fromFileOffset objCodeEnd)

   -- Map from section index to name ad type.
{-
  let secNameAddrMap :: Map ObjectSectionIndex (BS.ByteString, ElfWordType 64)
      secNameAddrMap
                = Map.fromList
                  [ (objsecSourceIndex secInfo, (objsecName secInfo, objsecBaseAddr secInfo))
                  | secInfo <- objSections
                  ]
-}

  {-
  -- Resolve which offsets of code segment to insert jumps into.
  codeRedirs <- do
    -- Map from symbols to the associated address
    let objSymMap :: Map BS.ByteString (ElfSymbolTableEntry (ElfWordType 64))
        objSymMap = Map.fromList [ (steName e, e) | e <- V.toList objSymbols ]
    when (Map.size objSymMap /= V.length objSymbols) $ do
      relinkFail "Duplicate function names detected in object file."

    -- Given a symbol name from a redirection, this returns the address in the object file if known.
    let resolveFn :: BS.ByteString -> Maybe (ElfWordType 64)
        resolveFn nm = do
           e <- Map.lookup nm objSymMap
           when (not (isFuncSymbol e)) $ Nothing
           snd <$> Map.lookup (fromElfSectionIndex (steIndex e)) secNameAddrMap

    let mkJump = x86_64_immediateJump
    case resolveCodeRedirections mkJump redirs resolveFn of
      Left nm -> relinkFail $ "Could not find symbol " ++ BSC.unpack nm ++ " in object file."
      Right r -> pure r
  -}

  -- Get ehdr Size
  let ehdrSize :: Word64
      ehdrSize = fromIntegral (Elf.ehdrSize cl)

  -- End of code section
  let binCodeEndOffset :: Elf.FileOffset Word64
      binCodeEndOffset = Elf.incOffset (Elf.phdrFileStart binCodePhdr) (Elf.phdrFileSize binCodePhdr)

  -- Start of overflow section offset.
  let overflowOffset :: Elf.FileOffset Word64
      overflowOffset = Elf.alignFileOffset 16 binCodeEndOffset

  -- Number of bytes in padding between end of old code and overflow code section.
  let overflowPadding :: Word64
      overflowPadding = Elf.fromFileOffset overflowOffset - Elf.fromFileOffset binCodeEndOffset

  -- Address of start of overflow section.
  -- This is the of the binary code segment rounded up to nearest multiple of 16.
  let overflowAddr :: Word64
      overflowAddr
        = Elf.phdrSegmentVirtAddr binCodePhdr
        + Elf.phdrMemSize binCodePhdr
        + overflowPadding

  -- Number of bytes in overflow section
  let overflowSize :: Word64
      overflowSize = nyi "overflowSize"

  -- Size of object code plus any padding between end of old code and start of object code.
  let newDataDelta :: Word64
      newDataDelta = overflowPadding + overflowSize

  -- End of overflow
  let overflowEndOffset :: Elf.FileOffset Word64
      overflowEndOffset = Elf.incOffset overflowOffset overflowSize

  -- Get layout for binary data now that we know new code added.
  binPhdrLayout <-
    mkBinaryPhdrLayout binHeaderInfo overflowEndOffset binPhdrScanResults

  let newSymtabOffset :: Elf.FileOffset Word64
      newSymtabOffset = fixAlignment (bplNewDataEndOffset binPhdrLayout) (Elf.symtabAlign cl)

  -- Section headers in input binary with names
  binShdrs <- getShdrTable binHeaderInfo

  -- Section header for symbol table in binary
  (binSymtabShdr :: Elf.ShdrEntry BS.ByteString Word64) <-
    case V.find (\e -> Elf.shdrType e == Elf.SHT_SYMTAB) binShdrs of
      Nothing -> relinkFail "Could not find binary file symbol table."
      Just r -> pure r

  let binLocalSymCount :: Word32
      binLocalSymCount = Elf.shdrInfo binSymtabShdr

  let binStrtabIdx :: Int
      binStrtabIdx = fromIntegral (Elf.shdrLink binSymtabShdr)
  when (binStrtabIdx >= V.length binShdrs) $ do
    relinkFail "Invalid binary string table index."
  let binStrtab :: BS.ByteString
      binStrtab = getShdrContents (binShdrs V.! binStrtabIdx) binHeaderInfo

  let binSymtab :: BS.ByteString
      binSymtab = getShdrContents binSymtabShdr binHeaderInfo

  (binSymbols :: V.Vector (Elf.ElfSymbolTableEntry BS.ByteString Word64)) <-
    case Elf.getSymbolTableEntries cl elfDta binStrtab binSymtab of
      Left _e -> fail "Could not parse binary symbol table."
      Right syms -> pure $ syms

  -- Sections in object file.
  let objShdrs :: V.Vector (Elf.ShdrEntry Word32 Word64)
      objShdrs = Elf.headerSectionHeaders objHeaderInfo

  -- Section header for symbol table in object file
  (objSymtabShdr :: Elf.ShdrEntry Word32 Word64) <-
    case V.find (\e -> Elf.shdrType e == Elf.SHT_SYMTAB) objShdrs of
      Nothing -> relinkFail "Could not find object file symbol table."
      Just r -> pure r

  let objSymtab :: BS.ByteString
      objSymtab = getShdrContents objSymtabShdr objHeaderInfo

  -- Get object string table.
  let objStrtabIdx :: Int
      objStrtabIdx = fromIntegral (Elf.shdrLink objSymtabShdr)
  when (objStrtabIdx >= V.length objShdrs) $ do
    relinkFail "Invalid binary string table index."
  let objStrtab :: BS.ByteString
      objStrtab = getShdrContents (objShdrs V.! objStrtabIdx) objHeaderInfo

  -- Compute layout of section headers in new binary by iterating over section
  -- headers in input binary.
  binShdrIndexInfo <- do
    let newCodeInfo = NewCodeInfo { nciBinDataAddr = bplBinDataAddr binPhdrLayout
                                  }
    let bsl0 = BSL { bslSectionCount = 0
                   , bslSectionNames   = []
                   , bslSectionHeaders = []
                   , bslSectionMap = Map.empty
                   , bslNeedNewCodeShdr = True
                   , bslSymtabIndex = 0
                   , bslStrtabIndex = 0
                   }
    ifoldlM' (layoutBinaryShdr newCodeInfo) bsl0 binShdrs

  -- Map from binary section indices to new section index.
  let binSecIndexToNewIndexMap:: Map Word16 Word16
      binSecIndexToNewIndexMap = bslSectionMap binShdrIndexInfo

  -- Map from global function symbol names in object file to new section index
  -- and offset within new section.
  let objSymAddrMap :: Map BS.ByteString (Elf.ElfSectionIndex, Word64)
      objSymAddrMap = nyi "objSymAddrMap"

  -- Map symbol in the original binary to a new address.
  let mapBinSymbol :: Int -- ^ Index of symbol
                   -> Elf.ElfSymbolTableEntry BS.ByteString Word64
                   -- ^ Symbol entry in the original binary
                   -> Elf.ElfSymbolTableEntry BS.ByteString Word64
      mapBinSymbol idx e
        -- Look to see if global symbols have been moved.
        | Just nm <- objNameOfBinSymbolIndex idx
        , Just (newSecIdx, newVal) <- Map.lookup nm objSymAddrMap =
            e { Elf.steIndex  = newSecIdx
              , Elf.steValue  = newVal
              }
          -- Copy other symbols
        | otherwise =
          let binIdx = Elf.fromElfSectionIndex (Elf.steIndex e)
              newIdx = Map.findWithDefault binIdx binIdx binSecIndexToNewIndexMap
           in e { Elf.steIndex = Elf.ElfSectionIndex newIdx }

  let (newLocalSyms, newGlobalSyms) = V.splitAt (fromIntegral binLocalSymCount) $
        V.imap mapBinSymbol binSymbols

  -- Get symbols from object file.
  -- Note these are considered local
  newObjSyms <- do
    (objSymbols :: V.Vector (Elf.ElfSymbolTableEntry BS.ByteString Word64))
      <- case Elf.getSymbolTableEntries cl elfDta objStrtab objSymtab of
           Left _e -> fail "Could not parse object file symbol table."
           Right syms -> pure $ syms

    let processObjSymbol :: Elf.ElfSymbolTableEntry BS.ByteString Word64
                         -> [Elf.ElfSymbolTableEntry BS.ByteString Word64]
                         -> [Elf.ElfSymbolTableEntry BS.ByteString Word64]
        processObjSymbol e r
          -- Copy function symbols pointing to the code section that are not
          -- in the binary, but make them local and fix address.
          | Elf.steType e == Elf.STT_FUNC
          , Elf.steBind e == Elf.STB_GLOBAL
          , Just (newIdx, newVal) <- Map.lookup (Elf.steName e) objSymAddrMap =
            let e' = e { Elf.steBind   = Elf.STB_LOCAL
                       , Elf.steIndex  = newIdx
                       , Elf.steValue  = Elf.steValue e + newVal
                       }
             in e':r
            -- Drop non-function symbols
          | otherwise = r
    pure $ V.fromList $ foldr processObjSymbol [] objSymbols

  -- Get number of local symbols (used in section header table)
  let newLocalSymCount :: Word32
      newLocalSymCount = fromIntegral $ V.length newLocalSyms + V.length newObjSyms

  -- Symbols
  let newSymbols ::  V.Vector (Elf.ElfSymbolTableEntry BS.ByteString Word64)
      newSymbols = newLocalSyms <> newObjSyms <> newGlobalSyms

  -- Get end offset of symbol table.
  let newSymtabSize :: Word64
      newSymtabSize =
        let cnt :: Word64
            cnt = fromIntegral (V.length newSymbols)
            sz :: Word64
            sz  = fromIntegral (Elf.symbolTableEntrySize cl)
         in (cnt * sz)

  -- Get end offset of symbol table.
  let newStrtabOffset :: Elf.FileOffset Word64
      newStrtabOffset = Elf.incOffset newSymtabOffset newSymtabSize

  -- Get symbol string table
  let newStrtabContents :: BS.ByteString
      strtabOffsetMap :: Map BS.ByteString Word32
      (newStrtabContents, strtabOffsetMap) = Elf.stringTable $
        V.toList $ Elf.steName <$> newSymbols

  let newSymtabEntries :: V.Vector (Elf.ElfSymbolTableEntry Word32 Word64)
      newSymtabEntries = finalizeSymtabEntryNameIndex strtabOffsetMap <$> newSymbols

  -- Get section header stringtable size
  let newStrtabSize :: Word64
      newStrtabSize = fromIntegral (BS.length newStrtabContents)

  -- Get string table end offset
  let newShstrtabOffset :: Elf.FileOffset Word64
      newShstrtabOffset = Elf.incOffset newStrtabOffset newStrtabSize

  -- .shstrtab contents
  let newShstrtabContents :: BS.ByteString
      shstrtabOffsetMap :: Map BS.ByteString Word32
      (newShstrtabContents, shstrtabOffsetMap) =
        Elf.stringTable $ bslSectionNames binShdrIndexInfo

  -- Get section header stringtable size
  let newShstrtabSize :: Word64
      newShstrtabSize = fromIntegral (BS.length newShstrtabContents)

  -- Section header string table offset.
  let newShstrtabEndOffset :: Elf.FileOffset Word64
      newShstrtabEndOffset = Elf.incOffset newShstrtabOffset newShstrtabSize

  -- Get offset of section header
  let newShdrTableOffset :: Elf.FileOffset Word64
      newShdrTableOffset =
        Elf.alignFileOffset (Elf.shdrAlign cl) newShstrtabEndOffset

  outPhdrs <-
    let mkPhdr :: Int -> IO (Elf.Phdr 64)
        mkPhdr i = do
          let idx :: Word16
              idx = fromIntegral i
          let phdr = Elf.phdrByIndex binHeaderInfo idx
          -- Extend code phdr to include new data
          if idx == bplBinCodePhdrIndex binPhdrLayout then
            pure $! phdr { Elf.phdrFileSize = Elf.phdrFileSize phdr + newDataDelta
                         , Elf.phdrMemSize  = Elf.phdrMemSize  phdr + newDataDelta
                         }
          -- Adjust segments in data or later by delta
          else if Elf.phdrSegmentVirtAddr phdr >= bplBinDataAddr binPhdrLayout then
            pure $! phdr { Elf.phdrFileStart = Elf.incOffset (Elf.phdrFileStart phdr) newDataDelta
                         }
          else
            pure $! phdr
     in V.generateM (fromIntegral binPhdrCount) mkPhdr

  -- Contents of program header after elf header and program header table.
  let newBinCodeContent :: Bld.Builder
      newBinCodeContent = nyi "binCodeContent"

  -- Contents of overflow section
  let overflowContents :: Bld.Builder
      overflowContents = nyi "overflowContents"

  let renderEntries = foldMap (Elf.renderSymbolTableEntry cl elfDta)

  -- .strtab section index
  (newStrtabIndex :: Word16) <-
     case Map.lookup (bslStrtabIndex binShdrIndexInfo) (bslSectionMap binShdrIndexInfo) of
       Just i -> pure i
       Nothing -> relinkFail "Could not find .strtab index."

  -- .shstrtab section index.
  (newShstrtabIndex :: Word16) <-
     case Map.lookup (Elf.shdrNameIdx binHeaderInfo) (bslSectionMap binShdrIndexInfo) of
       Just i -> pure i
       Nothing -> relinkFail "Could not find .shstrtab index."

  -- Elf header.
  let ehdr = Elf.Ehdr { Elf.ehdrHeader   = binHdr
                      , Elf.ehdrPhoff    = Elf.FileOffset ehdrSize
                      , Elf.ehdrShoff    = newShdrTableOffset
                      , Elf.ehdrPhnum    = binPhdrCount
                      , Elf.ehdrShnum    = bslSectionCount binShdrIndexInfo
                      , Elf.ehdrShstrndx = newShstrtabIndex
                      }

  -- Section Header table
  let newShdrTable =
        let nsri :: NewSectionRenderInfo
            nsri = NewSectionRenderInfo
              { nsriBinSectionIndexMap = bslSectionMap binShdrIndexInfo
              , nsriOverflowOffset   = overflowOffset
              , nsriOverflowSize     = overflowSize
              , nsriOverflowAddr     = overflowAddr
              , nsriDataDelta        = newDataDelta
              , nsriSymtabOffset     = newSymtabOffset
              , nsriSymtabSize       = newSymtabSize
              , nsriSymtabLocalCount = newLocalSymCount
              , nsriStrtabIndex      = newStrtabIndex
              , nsriStrtabOffset     = newStrtabOffset
              , nsriStrtabSize       = newStrtabSize
              , nsriShstabOffset     = newShstrtabOffset
              , nsriShstrtabSize     = newShstrtabSize
              }

            renderShdr :: NewSectionInfo -> Bld.Builder
            renderShdr nsi =
              let shdr = finalizeShdrNameIndex shstrtabOffsetMap $
                           renderNewSectionInfo nsri nsi
               in Elf.writeShdrEntry elfDta cl shdr
            -- Render sections (not bslSectionHeaders is in reverse order)
         in foldMap renderShdr (reverse (bslSectionHeaders binShdrIndexInfo))

  -- Create final elf image.
  return $ Bld.toLazyByteString $
    -- Main elf header
    Elf.buildElfHeader ehdr
    -- Program header table
    <> Elf.buildElfSegmentHeaderTable cl elfDta (V.toList outPhdrs)
    -- Existing code
    <> newBinCodeContent
    -- New object code
    <> overflowContents
    -- Padding and data content
    <> bplNewDataContent binPhdrLayout
    -- Symbol table
    <> renderEntries newSymtabEntries
    -- string table
    <> Bld.byteString newStrtabContents
    -- Section header name table
    <> Bld.byteString newShstrtabContents
    -- section header table padding
    <> Elf.alignmentPadding newShstrtabEndOffset newShdrTableOffset
    -- Section header table
    <> newShdrTable
