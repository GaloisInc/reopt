{-|
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
  , mergeObject
  , x86_64_immediateJump
  ) where

import           Control.Lens hiding (pre)
import           Control.Monad.Except
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Either
import           Data.ElfEdit
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
import           Text.Printf
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

type RelinkM = Except String

------------------------------------------------------------------------
-- Code for performing relocations in new object.

elfHasTLSSection :: Elf w -> Bool
elfHasTLSSection e =
  elfClassInstances (elfClass e) $ do
  any (\s -> elfSectionFlags s `hasBits` shf_tls) (e^..elfSections)

------------------------------------------------------------------------
-- Merger

-- | Find relocation entries in section with given name.
findRelaEntries :: IsRelocationType r
                => ElfData
                -> Map BS.ByteString [ElfSection (ElfWordType 64)]
                   -- ^ Map from section names to sections with that name.
                -> BS.ByteString
                   -- ^ Name of section containing relocation entries.
                -> RelinkM [RelaEntry r]
findRelaEntries dta secMap nm = do
  case fromMaybe [] (Map.lookup nm secMap) of
    -- Assume that no section means no relocations
    [] -> return []
    [s] -> case elfRelaEntries dta (BSL.fromStrict (elfSectionData s)) of
             Left e -> throwError e
             Right r -> pure r
    _ -> throwError $  "Multiple " ++ BSC.unpack nm ++ " sections in object file."

isLocalSymbol :: ElfSymbolTableEntry w -> Bool
isLocalSymbol sym = steBind sym == STB_LOCAL

-- | Find the symbol table in the elf.
findSymbolTable :: String
                   -- ^ Type of file for error reporting.
                -> Elf w
                    -- ^ Object with relocations
                -> RelinkM (ElfSymbolTable (ElfWordType w))
findSymbolTable nm obj = do
  case elfSymtab obj of
    -- Assume that no section means no relocations
    []  -> throwError $ "Could not find symbol table."
    [tbl] -> return tbl
    _   -> throwError $ "Multiple .symtab sections in " ++ nm ++ " file."

-- | Check original binary satisfies preconditions.
checkBinaryAssumptions :: Elf w -> Except String ()
checkBinaryAssumptions binary = do
  when (elfData binary /= ELFDATA2LSB) $ do
    throwError $ "Expected the original binary to be least-significant bit first."
  when (elfMachine binary /= EM_X86_64) $ do
    throwError $ "Only x86 64-bit object files are supported."
  when (elfFlags binary /= 0) $ do
    throwError $ "Expected elf flags in binary to be zero."
  unless (null (elfGnuRelroRegions binary)) $ do
    throwError $ "Expected no PT_GNU_RELO segment in binary."

-- | Check object file satisfies preconditions.
checkObjectAssumptions :: Elf w
                       -> RelinkM ()
checkObjectAssumptions obj = do
  -- Check new object properties.
  when (elfData obj /= ELFDATA2LSB) $ do
    throwError $ "Expected the new binary binary to be least-significant bit first."
  when (elfType obj /= ET_REL) $ do
    throwError $ "Expected a relocatable file as input."
  when (elfMachine obj /= EM_X86_64) $ do
    throwError $ "Only x86 64-bit executables are supported."
  when (elfFlags obj /= 0) $ do
    throwError $ "Expected elf flags in new object to be zero."
  unless (null (elfGnuRelroRegions obj)) $ do
    throwError $ "Expected no PT_GNU_RELO segment in new object."
  -- Check no TLS in object.
  when (elfHasTLSSection obj) $ do
    throwError $ "TLS section is not allowed in new code object."

-- | Intermediate state used when mapping existing binary contents to new file.
data TransBinaryState = TransBinaryState { nextSectionIndex :: !MergedSectionIndex
                                   -- ^ Index for next section
                                 , binSectionMap    :: !(Map BinSectionIndex MergedSectionIndex)
                                   -- ^ Maps binary sections to merged section.
                                 }


-- | Monad used for extracting code from binaries.
type TransBinaryM = State TransBinaryState

-- | Create a fresh section index.
freshSectionIndex :: BinSectionIndex -> TransBinaryM MergedSectionIndex
freshSectionIndex idx = do
  newIdx <- gets nextSectionIndex
  modify $ \s -> s { nextSectionIndex = newIdx + 1
                   , binSectionMap = Map.insert idx newIdx (binSectionMap s)
                   }
  return newIdx

-- | Generate a deferred region from a region within the code segment
-- of the original binary.
copyBinaryLoadSegmentRegions
  :: (BS.ByteString -> BS.ByteString)
     -- ^ Function for renaming sections.
  -> ResolvedCodeRedirs (ElfWordType 64)
     -- ^ Modifications to make to existing data
  -> [ElfDataRegion 64]
     -- ^ Regions encountered so far.
  -> ElfWordType 64
     -- ^ File offset
  -> [ElfDataRegion 64]
  -- ^ Regions in binary left to process.
  -> TransBinaryM ([ElfDataRegion 64], ElfWordType 64)
copyBinaryLoadSegmentRegions _ _ prevRegions secOffset [] = do
  pure $! (reverse prevRegions, secOffset)
copyBinaryLoadSegmentRegions nmFun rredirs prevRegions secOffset (thisRegion:restRegions) = do
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
                  (newData, newOffset) <-
                    copyBinaryLoadSegmentRegions nmFun rredirs [] secOffset [ElfDataSection sec]
                  let newSeg = seg { elfSegmentData  = Seq.fromList newData }
                  let nextRegions = ElfDataSegment newSeg : prevRegions
                  seq newSeg $ seq nextRegions $
                    copyBinaryLoadSegmentRegions nmFun rredirs nextRegions newOffset restRegions
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
      -- Get data in section
      let dta = elfSectionData sec
      -- Get filesize of section.
      let fsize = fromIntegral (BS.length dta)
      -- Generate mapping from original function to new
      idx <- freshSectionIndex (elfSectionIndex sec)

      let newSec = sec
             { elfSectionIndex     = idx
             , elfSectionName      = nmFun (elfSectionName sec)
             , elfSectionData      = insertStaticRedirs rredirs secOffset dta
             }

      -- The last section
      let nextRegions = ElfDataSection newSec : prevRegions
      seq newSec $ seq nextRegions $
        copyBinaryLoadSegmentRegions nmFun rredirs nextRegions (secOffset + fsize) restRegions
    ElfDataRaw b -> do
      let dr = ElfDataRaw b
      let fsize = fromIntegral (BS.length b)
      let nextRegions = dr : prevRegions
      seq dr $ seq nextRegions $
        copyBinaryLoadSegmentRegions nmFun rredirs nextRegions (secOffset + fsize) restRegions


-- | This function generates regions in the binary for the new object code
-- using relocation information and the `ObjectSectionInfo`.
mkObjCodeRegions :: RelocInfo (ElfWordType 64)
                 -> ObjectSectionInfo X86_64_RelocationType
                    -- ^ Names of sections to look for and expected flags
                 -> [ElfDataRegion 64]
mkObjCodeRegions relocInfo secInfo
     | BS.length oldData /= BS.length newData  =
       error "mkObjCodeRegions relocatios have incorrect size"
     | otherwise
       =  paddingRegions (objsecPadding secInfo)
       ++ [ElfDataSection newSec]
  where sec = objsecOrigSection secInfo
        -- Get relocations
        relaEntries :: [RelaEntry X86_64_RelocationType]
        relaEntries = objsecRelocations secInfo
        -- Compute address of this section
        secAddr :: ElfWordType 64
        oldData = elfSectionData sec
        secAddr = objsecBaseAddr secInfo
        newData = performRelocs relocInfo secAddr oldData relaEntries

        -- Generate new setion
        newSec
          = sec
            { elfSectionIndex = objsecMergedIndex secInfo
            , elfSectionAddr  = secAddr
            , elfSectionData = newData
            }

-- | `paddingRegions n` generate a padding region to add `n` bytes to binary.
paddingRegions :: ElfWordType 64 -> [ElfDataRegion 64]
paddingRegions 0 = []
paddingRegions p = [ElfDataRaw (BS.replicate (fromIntegral p) 0)]

-- | `resolveSegmentPadding offset addr align` returns the number `n` such that
-- `(offset + n) `mod` align == addr `mod` align`.
--
-- This requires that `align` is a power of two.
resolveSegmentPadding :: (Bits a, Num a) => a -> a -> a -> a
resolveSegmentPadding offset addr align
    | align .&. mask /= 0 = error "resolveSegmentPadding given bad alignment."
    | otherwise = (align + (addr .&. mask) - (offset .&. mask)) .&. mask
  where mask = align - 1

------------------------------------------------------------------------
-- Binary checking

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
  , objsecBaseAddr :: !(RelocationWord r)
    -- ^ Base address of section after alignment
  , objsecRelocations :: !([RelaEntry r])
    -- ^ Relocations to apply
  , objsecOrigSection :: !(ElfSection (RelocationWord r))
    -- ^ Original section in object.
  }

-- | Name of the section
objsecName :: ObjectSectionInfo r -> BS.ByteString
objsecName = elfSectionName . objsecOrigSection

checkSectionFlags :: (Bits w, Integral w, Show w)
                  => ElfSection w
                  -> ElfSectionFlags w
                  -> RelinkM ()
checkSectionFlags sec expectedFlags =
  when (elfSectionFlags sec /= expectedFlags) $ do
    throwError $ BSC.unpack (elfSectionName sec) ++ " section has an unexpected flags: "
      ++ show (elfSectionFlags sec) ++ "."

collectObjSectionInfo1 :: ElfData
                       -> Map BS.ByteString [ElfSection (ElfWordType 64)]
                          -- ^ Map from section name to relocation section(s) for it.
                       -> MergedSectionIndex
                       -> ElfWordType 64
                       -- ^ Address we epxect to load this section to.
                       -> ElfSection (ElfWordType 64)
                       -> RelinkM (ObjectSectionInfo X86_64_RelocationType, ElfWordType 64)
collectObjSectionInfo1 byteOrder relaSecMap idx addr sec = do
  let nm = elfSectionName sec
  when (elfSectionType sec /= SHT_PROGBITS) $ do
    throwError $ BSC.unpack nm ++ " section expected to be type SHT_PROGBITS."
  let dta = elfSectionData sec
  when (toInteger (BS.length dta) /= toInteger (elfSectionSize sec)) $ do
    throwError $ "Object code section sizes must match data length."
  relaEntries <- findRelaEntries byteOrder relaSecMap nm
  -- Get alignemnt for this section
  let align = elfSectionAddrAlign sec
  -- Compute address of this section
  let secAddr :: ElfWordType 64
      secAddr = fixAlignment addr align
  -- Compute section information
  let osec = ObjectSectionInfo
            { objsecSourceIndex = elfSectionIndex sec
            , objsecMergedIndex = idx
            , objsecPadding  = secAddr - addr
            , objsecBaseAddr = secAddr
            , objsecRelocations = relaEntries
            , objsecOrigSection = sec
            }
  pure $ (osec, secAddr + elfSectionFileSize sec)

-- | This function is used to generate the information needed to perform relocations.
collectObjSectionInfo :: ElfData
                      -> Map BS.ByteString [ElfSection (ElfWordType 64)]
                         -- ^ Map from section name to relocation section(s) for it.
                       -> [ObjectSectionInfo X86_64_RelocationType]
                       -- ^ Map section indices from binary to information.
                      -> MergedSectionIndex
                          -- ^ Merged section index
                      -> ElfWordType 64
                       -- ^ Base address for code (add file offset to get next address)
                      -> [ElfSection (ElfWordType 64)]
                         -- ^ Remaining sections in file.
                      -> RelinkM ( [ObjectSectionInfo X86_64_RelocationType]
                                 , MergedSectionIndex
                                 , ElfWordType 64
                                 )
collectObjSectionInfo _ _ prev idx addr [] = do
  pure (reverse prev, idx, addr)
collectObjSectionInfo dta relaSecMap prevRegions idx addr (sec:rest) = do
  case elfSectionName sec of
    ".text" -> do
      checkSectionFlags sec (shf_alloc .|. shf_execinstr)
      (secInfo,addr') <- collectObjSectionInfo1 dta relaSecMap idx addr sec
      collectObjSectionInfo dta relaSecMap (secInfo:prevRegions) (idx + 1) addr' rest
    ".rodata" -> do
      checkSectionFlags sec shf_alloc
      (secInfo,addr') <- collectObjSectionInfo1 dta relaSecMap idx addr sec
      collectObjSectionInfo dta relaSecMap (secInfo:prevRegions) (idx + 1) addr' rest
    ".rodata.cst16" -> do
      checkSectionFlags sec (shf_alloc .|. shf_merge)
      (secInfo,addr') <- collectObjSectionInfo1 dta relaSecMap idx addr sec
      collectObjSectionInfo dta relaSecMap (secInfo:prevRegions) (idx + 1) addr' rest
    ".eh_frame" -> do
      checkSectionFlags sec shf_alloc
      (secInfo,addr') <- collectObjSectionInfo1 dta relaSecMap idx addr sec
      collectObjSectionInfo dta relaSecMap (secInfo:prevRegions) (idx + 1) addr' rest
    ".data" -> do
      when (elfSectionSize sec /= 0) $ do
       throwError $ "Relinker requires new object has empty .bss."
      collectObjSectionInfo dta relaSecMap prevRegions idx addr rest
    ".bss" -> do
      when (elfSectionSize sec /= 0) $ do
       throwError $ "Relinker requires new object has empty .bss."
      collectObjSectionInfo dta relaSecMap prevRegions idx addr rest
    ".note.GNU-stack" -> do
      collectObjSectionInfo dta relaSecMap prevRegions idx addr rest
    ".symtab" -> do
      collectObjSectionInfo dta relaSecMap prevRegions idx addr rest
    ".strtab" -> do
      collectObjSectionInfo dta relaSecMap prevRegions idx addr rest
    ".shstrtab" -> do
      collectObjSectionInfo dta relaSecMap prevRegions idx addr rest
    nm -> do
      error $ "Unexpected section name: " ++ show nm


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
              -> Elf 64
                 -- ^ Input binary symbol table information
              -> V.Vector (ElfSymbolTableEntry Word64)
                 -- ^ Symbols in the new binary.
              -> RelinkM (ElfSymbolTable (ElfWordType 64))
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
        throwError $ "Multiple .symtab sections in input binary."

  newBinSyms <- fmap (V.mapMaybe id) $
    traverse (resolveBinarySymbolTableEntry (binSectionIndexMap symbolCtx) objSymbolNames)
              binSyms

  let (localSyms, globalSyms)
        = V.partition isLocalSymbol
        $ resolvedObjSyms <> newBinSyms

  pure $!
      ElfSymbolTable { elfSymbolTableIndex   = newSymtabIndex
                     , elfSymbolTableEntries = localSyms <> globalSyms
                     , elfSymbolTableLocalEntries = fromIntegral $ V.length localSyms
                     }

-- | Drop raw data prefix from the region list
--
-- Used to drop padding added for alignment purposes.
dropLeadingRawData :: [ElfDataRegion w] -> [ElfDataRegion w]
dropLeadingRawData (ElfDataRaw _ :r) = dropLeadingRawData r
dropLeadingRawData r = r

-- | Get code and data segments from binary.
getCodeAndDataSegments :: Elf w -> Except String (ElfSegment w, ElfSegment w)
getCodeAndDataSegments binary = do
  (cs,rest0) <-
    case toList (binary^.elfFileData) of
      ElfDataSegment cs : drest | elfSegmentType cs == PT_LOAD -> pure (cs,drest)
      _ -> throwError "Expected code segment at start of file."
  (ds,rest) <-
    case dropLeadingRawData rest0 of
      ElfDataSegment ds : r | elfSegmentType ds == PT_LOAD -> pure (ds,r)
      _ -> throwError "Expected data segment after code segment."
  traverse_ checkBinaryRegions rest
  pure (cs, ds)

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
-- * There are two loadable segments, possibly a PT_GNU_STACK segment and no other
--   segments.
-- * The first loadable segment is:
--   * Readable and executable
--   * Located at the beginning of the file.
--   * Contains the elf header, the program header table, and one or more loadable
--     sections (perhaps with padding between them).  The memory size is the same as
--    the file size.
-- * The second loadable segment is:
--   * Readable and writable
--   * Located immediately after the code
--     segment
--   * Contains loadable sections.  The last section may be a .bss section.
mergeObject :: HasCallStack
             => Elf 64 -- ^ Existing binary
             -> Elf 64 -- ^ Object file to merge into existing binary.
             -> [CodeRedirection Word64]
                -- ^ Redirections from old binary to new code.
             -> (Word64 -> BS.ByteString)
                -- ^ Function for creating jump to given offset.
             -> Either String (Elf 64)
mergeObject binary obj redirs mkJump = runExcept $ do
  objSymbols <- elfSymbolTableEntries <$> findSymbolTable "object" obj

  -- Drop from huge page aligned addresses (0x200000) to normal page alignment.
  let elfAlign :: ElfWordType 64
      elfAlign = pageSize

  -- Check original binary properties
  checkBinaryAssumptions binary
  case eltType binary of
    ET_EXEC -> pure ()
    _ -> do
      throwError $ "Expected the original binary is an executable."


  -- Get code and data segments
  (codeSeg, dataSeg) <- getCodeAndDataSegments binary

  -- Check object assumptions
  checkObjectAssumptions obj
  -- Check OSABI compat
  when (elfOSABI obj /= elfOSABI binary) $ do
    throwError $ "Expected the new object to use the same OS ABI as original."

  -- Create GNU stack info if both binary and object have it, and the
  -- stack is non-executable in both.
  let mgnuStack = elfGnuStackSegment binary

  let noteCount = Seq.length (Seq.filter isNoteSegment (binary^.elfFileData))

      isNoteSegment (ElfDataSegment ds) = go (elfSegmentData ds)
        where
        go :: (Show (ElfWordType w), Integral (ElfWordType w))
           => Seq.Seq (ElfDataRegion w) -> Bool
        go (ElfDataSegment seg Seq.:<| _) | elfSegmentType seg == PT_NOTE = True
        go (_ Seq.:<| rest) = go rest
        go Seq.Empty = False
      isNoteSegment _ = False

  -- Compute number of program headers
  let newPhdrCount = 2
                   + (if isJust mgnuStack then 1 else 0)
                   + noteCount

  let binaryPhdrCount :: Int
      binaryPhdrCount = elfSegmentCount binary
  -- We currently do not allow the number of program headers to change so that
  -- the memory layout between the input binary and output does not change.
  when (newPhdrCount /= binaryPhdrCount) $ do
    error $ printf "internal: Expected program header count %d; got %d." newPhdrCount binaryPhdrCount

  -- Get layout of object code.
  (objSections, nextMergedIdx, finalCodeAddr) <- do
    let (relaPairs, nonrelaSecs) = partitionEithers
                  [ case BS.stripPrefix ".rela" (elfSectionName sec) of
                      Just nm -> Left (nm, [sec])
                      Nothing -> Right sec
                  | sec <- obj^..elfSections
                  ]
    let relaSecMap = Map.fromList relaPairs
    let ElfAbsoluteSize codeMemSize = elfSegmentMemSize codeSeg
    -- Get address of new codwe
    let codeAddr = elfSegmentVirtAddr codeSeg + codeMemSize
    collectObjSectionInfo ELFDATA2LSB relaSecMap [] 1 codeAddr nonrelaSecs

  -- Resolve which offsets of code segment to insert jumps into.
  codeRedirs <- do
    let secAddrMap :: Map ObjectSectionIndex (ElfWordType 64)
        secAddrMap
          = Map.fromList
            [ (objsecSourceIndex secInfo, objsecBaseAddr secInfo)
            | secInfo <- objSections
            ]
    let codeAddrMap :: Map BS.ByteString (ElfWordType 64)
        codeAddrMap = mkCodeAddrMap secAddrMap $ V.filter isFuncSymbol objSymbols

    let resolveFn :: BS.ByteString -> Maybe (ElfWordType 64)
        resolveFn nm = Map.lookup nm codeAddrMap
    case resolveCodeRedirections mkJump redirs resolveFn of
      Left nm -> throwError $ "Could not find symbol " ++ BSC.unpack nm ++ " in object file."
      Right r -> pure r

  let objRegions = concatMap (mkObjCodeRegions relocInfo) objSections
        where relocInfo = mkRelocInfo secNameAddrMap objSymbols
              secNameAddrMap :: Map ObjectSectionIndex (BS.ByteString, ElfWordType 64)
              secNameAddrMap
                = Map.fromList
                  [ (objsecSourceIndex secInfo, (objsecName secInfo, objsecBaseAddr secInfo))
                  | secInfo <- objSections
                  ]

  -- Compute size of new code segment.
  let codeSize = finalCodeAddr - elfSegmentVirtAddr codeSeg

  -- Get just list of object symbols that correspond to defined functions.
  let objFuncSymbols = V.filter isFuncSymbol objSymbols

  -- Get code segment contents
  let codeSegContents =
        case toList (elfSegmentData codeSeg) of
          ElfDataElfHeader : ElfDataSegmentHeaders : rest -> rest
          ElfDataElfHeader : _ -> do
            error "Missing segment headers at start of code segment."
          _ -> do
            error "Missing elf header at start of file."


  let initState = TransBinaryState { nextSectionIndex  = nextMergedIdx
                                   , binSectionMap     = Map.empty
                                   }

  let (loadSegments, s) = flip runState initState $ do
        -- Copy code regions to new binary
        (binCodeRegions,_) <-
          copyBinaryLoadSegmentRegions (".orig" <>) codeRedirs [] (elfSegmentVirtAddr codeSeg) codeSegContents

        -- Create new code segment
        let newCodeSeg =
              codeSeg
              { elfSegmentAlign = elfAlign
              , elfSegmentMemSize = ElfRelativeSize 0
              , elfSegmentData = Seq.fromList
                                 $  [ElfDataElfHeader, ElfDataSegmentHeaders]
                                 ++ binCodeRegions
                                 ++ objRegions
              }

        -- Get file offset for data
        let dataFileOff = codeSize

        -- Padding is the number of bytes that need to be added to the file
        -- offset to ensure the relevant bits in the file and address
        -- offsets match.
        let dataPadding = resolveSegmentPadding dataFileOff (elfSegmentVirtAddr dataSeg) elfAlign

        (binDataRegions,_) <- copyBinaryLoadSegmentRegions id emptyResolvedCodeRedirs [] 0
           (toList (elfSegmentData dataSeg))

        -- Compute resolved data segment
        let newDataSeg =
              dataSeg { elfSegmentAlign = elfAlign
                      , elfSegmentData = Seq.fromList binDataRegions
                      }
        pure $ [ElfDataSegment newCodeSeg]
          ++ paddingRegions dataPadding
          ++ [ElfDataSegment newDataSeg]


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
  let newSymtabIndex = loadSectionCount + 1

  symtab <- mkSymbolTable newSymtabIndex symbolCtx binary objFuncSymbols

  return $! binary {  elfFlags      = 0
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
