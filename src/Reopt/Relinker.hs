{-|
Module      : Reopt.Relinker
Copyright   : (c) Galois Inc, 2016
License     : AllRightsReserved
Maintainer  : jhendrix@galois.com

This module performs the merging between the binary and new object.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Reopt.Relinker
  ( Reopt.Relinker.Redirection.CodeRedirection(..)
  , mergeObject
    -- * Warnings
  , RelinkWarnings
  , unresolvedSymbols
  , hasRelinkWarnings
  ) where

import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BSL
import           Data.Elf
import           Data.Foldable
import           Data.Int
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Data.Word
import           Numeric (showHex)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Reopt.Relinker.Redirection

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

mapFromList :: Ord k => (a -> k) -> [a] -> Map k [a]
mapFromList proj = foldr' ins Map.empty
  where ins e = Map.insertWith (++) (proj e) [e]

hasBits :: Bits x => x -> x -> Bool
x `hasBits` b = (x .&. b) == b

writeBS :: SV.MVector s Word8 -> Int -> BS.ByteString -> ST s ()
writeBS mv base bs = do
  let len = BS.length bs
  when (SMV.length mv < base + len) $ do
    fail $ "Bytestring overflows buffer."
  forM_ [0..len-1] $ \i -> do
    SMV.write mv (base+i) (bs `BS.index` i)


write32_lsb :: SMV.MVector s Word8 -> Word64 -> Word32 -> ST s ()
write32_lsb v a c = do
  -- Assert there are at least
  assert (a <= -4) $ do
  let i = fromIntegral a
  SMV.write v i     $ fromIntegral c
  SMV.write v (i+1) $ fromIntegral (c `shiftR`  8)
  SMV.write v (i+2) $ fromIntegral (c `shiftR` 16)
  SMV.write v (i+3) $ fromIntegral (c `shiftR` 24)

------------------------------------------------------------------------
-- Elf specific utilities

-- | Size of page on system
page_size :: Word64
page_size = 0x1000

-- | Get the alignment that loadable segments seem to respect in a binary.
--
-- We ensure that this is at least 'page_size', but try to compute it from
-- the first loadable segment otherwise.
elfAlignment :: (Num w, Ord w) => Elf w -> w
elfAlignment e =
    case loadableSegments of
      [] -> fromIntegral page_size
      (s:_) -> max (elfSegmentAlign s) (fromIntegral page_size)
  where isLoadable s = elfSegmentType s == PT_LOAD
        loadableSegments = filter isLoadable $ elfSegments e

-- | Return the name of a symbol as a string (or <unamed symbol> if not defined).
steStringName :: ElfSymbolTableEntry w -> String
steStringName sym
  | BS.null (steName sym) = "<unnamed symbol>"
  | otherwise = BSC.unpack (steName sym)

-- | Information about section in objhect needed for computing layout
-- and performing relocations.
data SectionInfo w = SectionInfo { sectionVal   :: (ElfSection w)
                                   -- ^ Actual section
                                 , sectionReloc :: !String
                                 }

-- | Find a section with the given name, type, and flags.
--
-- This returns 'Nothing' if no matching section exists, but throws an error if
-- multiple do.
findSectionInfo :: Eq w
                => Elf w
                -> String
                   -- ^ Expected name of section
                -> ElfSectionType
                   -- ^ Expected section type.
                -> ElfSectionFlags w
                   -- ^ Expected section flags
                -> String
                    -- ^ Name of relocation section (or "" if no relocations associateD)
                -> Except String (Maybe (SectionInfo w))
findSectionInfo e nm tp flags reloc =
  case nm `findSectionByName` e of
    [sec] -> do
      when (elfSectionType sec /= tp) $ do
        throwE $ nm ++ " section has an unexpected type."
      when (elfSectionFlags sec /= flags) $ do
        throwE $ nm ++ " section has an unexpected flags."
      let info = SectionInfo sec reloc
      seq info $ (return $! Just info)
    []  -> return Nothing
    _   -> throwE $ "Multiple " ++ nm ++ " sections in object file."

-- | Return segment used to indicate the stack can be non-executable.
gnuStackSegment :: Num w => Word16 -> RelinkM (ElfSegment w)
gnuStackSegment obj_idx = do
  seg_idx <- freshSegmentIndex
  sec_idx <- remapObjSectionIndex obj_idx 0

  let sec = ElfSection { elfSectionIndex     = sec_idx
                       , elfSectionName      = ".note.GNU-stack"
                       , elfSectionType      = SHT_PROGBITS
                       , elfSectionFlags     = shf_merge
                       , elfSectionAddr      = 0
                       , elfSectionSize      = 0
                       , elfSectionLink      = 0
                       , elfSectionInfo      = 0
                       , elfSectionAddrAlign = 1
                       , elfSectionEntSize   = 0
                       , elfSectionData      = BS.empty
                       }

  return $! ElfSegment { elfSegmentType     = PT_GNU_STACK
                       , elfSegmentFlags    = pf_r .|. pf_w
                       , elfSegmentIndex    = seg_idx
                       , elfSegmentVirtAddr = 0
                       , elfSegmentPhysAddr = 0
                       , elfSegmentAlign    = 0
                       , elfSegmentMemSize  = ElfRelativeSize 0
                       , elfSegmentData     = Seq.fromList [ ElfDataSection sec ]
                       }

elfHasTLSSegment :: Elf w -> Bool
elfHasTLSSegment e =
  case filter (`segmentHasType` PT_TLS) (elfSegments e) of
    [] -> False
    [_] -> True
    _ -> error "Multiple TLS segments in original binary"

elfHasTLSSection :: (Num w, Bits w) => Elf w -> Bool
elfHasTLSSection e =
  any (\s -> elfSectionFlags s `hasBits` shf_tls) (e^..elfSections)

segmentHasType :: ElfSegment w -> ElfSegmentType -> Bool
segmentHasType s tp = elfSegmentType s == tp

-- | Return total number of segments expected in original binary.
loadableSegmentCount :: Elf w -> Int
loadableSegmentCount e = length $ filter (`segmentHasType` PT_LOAD) (elfSegments e)

elfHasGNUStackSegment :: Elf w -> Bool
elfHasGNUStackSegment e =
  any (`segmentHasType` PT_GNU_STACK) (elfSegments e)

elfGNUStackSection :: Elf w -> Maybe (ElfSection w)
elfGNUStackSection e =
  case filter (\s -> elfSectionName s == ".note.GNU-stack") (e^..elfSections) of
    [] -> Nothing
    (s:_) -> Just s

------------------------------------------------------------------------
-- RelinkWarning

-- | Warnings from relinking
newtype RelinkWarnings = RelinkWarnings
  { _unresolvedSymbols :: (Set BS.ByteString)
    -- ^ Unresolved symbols in relocations.
  }

-- | Empty relinker warnings collection.Na
emptyRelinkWarnings :: RelinkWarnings
emptyRelinkWarnings = RelinkWarnings { _unresolvedSymbols = Set.empty }

-- | Unresolved symbols in relocations.
unresolvedSymbols :: Simple Lens RelinkWarnings (Set BS.ByteString)
unresolvedSymbols = lens _unresolvedSymbols (\s v -> s { _unresolvedSymbols = v })

-- | Return true if there are any relinker warnings.
hasRelinkWarnings :: RelinkWarnings -> Bool
hasRelinkWarnings w = not (Set.null (w^.unresolvedSymbols))


instance PP.Pretty RelinkWarnings where
  pretty w = PP.vcat (ppUnresolvedSymbol <$> Set.toList (w^.unresolvedSymbols))
    where ppUnresolvedSymbol s =
            PP.text "Skipping relocations of unresolved symbol"
            PP.<+> PP.text (BSC.unpack s) PP.<> PP.text "."

------------------------------------------------------------------------
-- SymbolTable

type SymbolTable w = V.Vector (ElfSymbolTableEntry w)

-- | Offset of entry in symbol table.
type SymbolTableIndex = Word32

-- | Get symbol by name.
getSymbolByIndex :: SymbolTable w -> SymbolTableIndex -> ElfSymbolTableEntry w
getSymbolByIndex sym_table sym_index
  | sym_index >= fromIntegral (V.length sym_table) =
    error $ "Symbolic offset " ++ show sym_index ++ " is out of range."
  | otherwise = sym_table V.! fromIntegral sym_index


------------------------------------------------------------------------
-- SectionMap

-- | Map from section indices in object to virtual address where it will be
-- loaded into binary.
type SectionMap w = Map ElfSectionIndex w

------------------------------------------------------------------------
-- BinarySymbolMap

-- | Maps symbol names in binaries to their virtual address when loaded.
type BinarySymbolMap w = Map BS.ByteString w


symbolNameAddr :: ElfSymbolTableEntry w -> Maybe (BS.ByteString, w)
symbolNameAddr sym =
  if steType sym `elem` [ STT_OBJECT, STT_FUNC ] then
    Just (steName sym, steValue sym)
   else
    Nothing

-- | Create a binary symbol map from the symbol table of the elf file if it exists.
createBinarySymbolMap :: Elf w -> BinarySymbolMap w
createBinarySymbolMap binary = do
  case elfSymtab binary of
     -- Assume that no section means no relocations
    []  -> Map.empty
    [tbl] ->
      let entries = elfSymbolTableEntries tbl
       in Map.fromList $ mapMaybe symbolNameAddr $ V.toList entries
    _   -> error $ "Multple .symtab sections in bianry file."

------------------------------------------------------------------------
-- ObjectRelocationInfo

-- | Information needed to perform relocations in the new binary.
data ObjectRelocationInfo w
   = ObjectRelocationInfo
     { objectSectionMap :: !(SectionMap w)
       -- ^ Maps loaded sections in the new object to their address.
     , binarySymbolMap :: !(BinarySymbolMap w)
     }

objectSectionAddr :: String
                  -> ElfSectionIndex
                  -> SectionMap w
                  -> w
objectSectionAddr src idx m =
  case Map.lookup idx m of
    Nothing ->
      error $ src ++ "refers to an unmapped section index " ++ show idx ++ "."
    Just r -> r

-- | Get the address of a symbol table if it is mapped in the section map.
symbolAddr :: (Eq w, Num w)
           => ObjectRelocationInfo w
              -- ^ Information needed for relocations.
           -> String
              -- ^ Name of reference to a symbol
           -> ElfSymbolTableEntry w
           -> Maybe w
symbolAddr reloc_info src sym =
  case steType sym of
    STT_SECTION
      | steValue sym /= 0 ->
        error "symbolAddr expects section names to have 0 offset."
      | steIndex sym == SHN_UNDEF ->
        error "Reference to undefined section."
      | otherwise ->
         Just (objectSectionAddr src (steIndex sym) (objectSectionMap reloc_info))
    STT_FUNC
      | steIndex sym == SHN_UNDEF ->
        error "Function symbol has undefined section."
      | otherwise ->
        Just (objectSectionAddr src (steIndex sym) (objectSectionMap reloc_info))
    STT_NOTYPE
      | steIndex sym /= SHN_UNDEF ->
          error "Expected STT_NOTYPE symbol to refer to SHN_UNDEF section."
      | otherwise ->
        case Map.lookup (steName sym) (binarySymbolMap reloc_info) of
          Nothing -> Nothing
          Just addr -> Just addr
    tp -> error $ "symbolAddr does not support symbol with type " ++ show tp ++ "."

------------------------------------------------------------------------
-- Code for performing relocations in new object.

data ObjRelocState w = ObjRelinkState { _warnings :: !RelinkWarnings
                                      , _nextSectionIndex :: !Word16
                                      , _nextSegmentIndex :: !Word16
                                      , _objSectionMap    :: !(Map Word16 (Word16, w))
                                      }

emptyObjRelocState :: ObjRelocState w
emptyObjRelocState = ObjRelinkState { _warnings         = emptyRelinkWarnings
                                    , _nextSectionIndex = 1
                                    , _nextSegmentIndex = 0
                                    , _objSectionMap    = Map.empty
                                    }

warnings :: Simple Lens (ObjRelocState w) RelinkWarnings
warnings = lens _warnings (\s v -> s { _warnings = v })

nextSectionIndex :: Simple Lens (ObjRelocState w) Word16
nextSectionIndex = lens _nextSectionIndex (\s v -> s { _nextSectionIndex = v })

nextSegmentIndex :: Simple Lens (ObjRelocState w) Word16
nextSegmentIndex = lens _nextSegmentIndex (\s v -> s { _nextSegmentIndex = v })

objSectionMap :: Simple Lens (ObjRelocState w) (Map Word16 (Word16, w))
objSectionMap = lens _objSectionMap (\s v -> s { _objSectionMap = v })

-- | Create a fresh section index.
freshSectionIndex :: MonadState (ObjRelocState w) m => m Word16
freshSectionIndex  = do
  idx <- use nextSectionIndex
  nextSectionIndex += 1
  return idx

remapObjSectionIndex :: MonadState (ObjRelocState w) m => Word16 -> w -> m Word16
remapObjSectionIndex obj_idx addr = do
  new_idx <- freshSectionIndex
  objSectionMap %= Map.insert obj_idx (new_idx, addr)
  return $! new_idx

freshSegmentIndex :: MonadState (ObjRelocState w) m => m Word16
freshSegmentIndex  = do
  idx <- use nextSegmentIndex
  nextSegmentIndex += 1
  return idx

type ObjRelocM s = StateT (ObjRelocState Word64) (ST s)

-- | Perform a relocation listed in the new object.
performReloc :: ObjectRelocationInfo Word64
                -- ^ Inforation about the object and binary needed to resolve symbol
                -- addresses.
             -> SymbolTable Word64
                -- ^ A vector listing the elf symbols in the object.
                --
                -- This allows one to find the symbol associated with a
                -- given relocation.
             -> Word64
                -- ^ Base offset of this section.
             -> SV.MVector s Word8
                -- ^ Contents of elf section we are apply this to.
             -> RelaEntry X86_64_RelocationType
                -- ^ The relocation entry.
             -> ObjRelocM s ()
performReloc reloc_info sym_table this_vaddr mv reloc = do
  -- Offset to modify.
  let off = r_offset reloc :: Word64
  let sym = getSymbolByIndex sym_table (r_sym reloc)
  -- Get the address of a symbol
  case symbolAddr reloc_info "A relocation entry" sym of
    Nothing -> warnings . unresolvedSymbols %= Set.insert (steName sym)
    Just sym_val -> do
      -- Relocation addend
      let addend = r_addend reloc :: Int64
      -- Get PC offset
      let pc_offset = this_vaddr + off
      -- Parse on type
      case r_type reloc of
        R_X86_64_PC32 ->
            lift $ write32_lsb mv off res32
          where res64 = sym_val + fromIntegral addend - pc_offset :: Word64
                res32 = fromIntegral res64 :: Word32
        R_X86_64_32
          | fromIntegral res32 /= res64 ->
            error $ "Relocation of " ++ steStringName sym
             ++ " at " ++ showHex sym_val " + " ++ show addend
             ++ " does not safely zero extend."
          | otherwise ->
            lift $ write32_lsb mv off res32
          where res64 = sym_val + fromIntegral addend :: Word64
                res32 = fromIntegral res64 :: Word32
        R_X86_64_32S
          | fromIntegral res32 /= res64 ->
            error $ "Relocation of " ++ steStringName sym
             ++ " at " ++ showHex sym_val " + " ++ show addend
             ++ " does not safely sign extend."
          | otherwise ->
            lift $ write32_lsb mv off (fromIntegral res32)
          where res64 = fromIntegral sym_val + addend :: Int64
                res32 = fromIntegral res64 :: Int32
        _ -> do
          error "Relocation not supported"

performRelocs :: ObjectRelocationInfo Word64
                -- ^ Maps elf section indices in object to the base virtual address
                -- in the binary.
              -> V.Vector (ElfSymbolTableEntry Word64)
                 -- ^ Elf symbol table
              -> ElfSection Word64
                 -- ^ Section that we are applying relocation to.
              -> Word64 -- ^ Base address of this section.
              -> [RelaEntry X86_64_RelocationType]
              -> ObjRelocM s (ElfSection Word64)
performRelocs reloc_info sym_table section this_vaddr relocs = do
  let dta = elfSectionData section
  let len = BS.length dta
  mv <- SMV.new len
  -- Copy original bytes into bytestring
  lift $ writeBS mv 0 dta
  -- Updpate using relocations
  mapM_ (performReloc reloc_info sym_table this_vaddr mv) relocs


  idx <- remapObjSectionIndex (elfSectionIndex section) this_vaddr
  let SMV.MVector _ fp = mv
  let reloc_data = Data.ByteString.Internal.fromForeignPtr fp 0 len
  return $! section { elfSectionIndex = idx
                    , elfSectionAddr = this_vaddr
                    , elfSectionData = reloc_data
                    }



------------------------------------------------------------------------
-- NameToSymbolMap

-- | Maps names to symbol in binary.
type NameToSymbolMap w = Map BS.ByteString [ElfSymbolTableEntry w]

-- | Get symbol by name.
getSymbolByName :: NameToSymbolMap w -> BS.ByteString -> ElfSymbolTableEntry w
getSymbolByName m nm =
  case Map.lookup nm m of
    Just [entry] -> entry
    Just _ -> error $ "The symbol name " ++ BSC.unpack nm ++ " is ambiguous."
    Nothing -> error $ "Could not find symbol name " ++ BSC.unpack nm ++ "."

------------------------------------------------------------------------
-- ResolvedRedirs

-- | Information needed to insert jumps to new code in binary.
data ResolvedRedirs w = CR { crMkJump :: !(w -> BS.ByteString)
                             -- ^ Create a jump instruction to the given address.
                           , crRelocInfo  :: !(ObjectRelocationInfo w)
                             -- ^ Maps elf section indices in object to the base virtual address
                             -- in the binary.
                           , crSymbols :: !(NameToSymbolMap w)
                             -- ^ Maps symbol names in the object to the associated symbol.
                           , crEntries :: !(Map PhdrIndex [CodeRedirection w])
                             -- ^ Get the list of redirections to apply.
                           }

-- | This takes a bytestring in the original binary and updates it with relocations
-- to point to the new binary.
remapBytes :: forall w
           .  Integral w
           => ResolvedRedirs w
           -> [CodeRedirection w]
              -- ^ List of redirections to apply
           -> w
              -- ^ File offset in segment
           -> BS.ByteString
           -> BS.ByteString
remapBytes redirs redir_list base bs = runST $ do
  let len :: Int
      len = BS.length bs
  mv <- SMV.new len
  -- Copy original bytes into bytestring
  writeBS mv 0 bs
  -- Apply relocations.
  let reloc_info = crRelocInfo redirs
  let sym_table = crSymbols redirs
  forM_ redir_list $ \entry -> do
    let off = redirSourceOffset entry
    let sym = getSymbolByName sym_table $ redirTarget entry
    when (base <= off && off < base + fromIntegral len) $ do
      let tgt =
            case symbolAddr reloc_info "A user defined relocation" sym of
              Just r -> r
              Nothing -> error $ "Could not find symbol " ++ show (steName sym) ++ "."
      let jmp = crMkJump redirs tgt
      -- Only apply redirection when there is enough space in size.
      when (BS.length jmp < redirSourceSize entry) $ do
        writeBS mv (fromIntegral (off - base)) jmp

  let SMV.MVector _ fp = mv
  return $! Data.ByteString.Internal.fromForeignPtr fp 0 len

-- | Append a raw segment to the list segments
rawSegmentFromBuilder :: (Bits w, Integral w)
                      => ElfLayout w
                      -> ResolvedRedirs w
                      -> [CodeRedirection w]
                         -- ^ Redirections for this segment
                      -> w -- ^ Offset in segment for this data
                      -> BS.ByteString
                      -> [ElfDataRegion w]
                      -> RelinkM (w, [ElfDataRegion w])
rawSegmentFromBuilder orig_layout redirs entries off bs rest = do
  let off' = off + fromIntegral (BS.length bs)
  (off2, prev) <- mapOrigLoadableRegions orig_layout redirs entries off' rest
  return (off2, ElfDataRaw (remapBytes redirs entries off bs) : prev)

mapLoadableSection :: (Bits w, Integral w)
                   => ElfLayout w
                   -> ResolvedRedirs w
                   -> [CodeRedirection w]
                   -> w -- ^ Offset in segment for this section
                   -> ElfSection w
                   -> [ElfDataRegion w]
                   -> RelinkM (w, [ElfDataRegion w])
mapLoadableSection orig_layout redirs entries off sec rest = do
  let bs = elfSectionData sec
  let off' = off + fromIntegral (BS.length bs)
  idx <- use nextSectionIndex
  nextSectionIndex += 1
  let sec' = sec { elfSectionIndex = idx
                 , elfSectionName = ".orig" ++ elfSectionName sec
                 , elfSectionData = remapBytes redirs entries off bs
                 }
  seq sec' $ do
  (off2, prev) <- mapOrigLoadableRegions orig_layout redirs entries off' rest
  return (off2, ElfDataSection sec' : prev)

-- | This traverses elf data regions in an loadable elf segment.
mapOrigLoadableRegions :: (Bits w, Integral w)
                       => ElfLayout w
                          -- ^ Layout created for original binary.
                       -> ResolvedRedirs w
                       -> [CodeRedirection w]
                          -- ^ Redirections for segment.
                       -> w -- ^ Offset in segment for region
                       -> [ElfDataRegion w]
                       -> RelinkM (w, [ElfDataRegion w])
mapOrigLoadableRegions _ _ _ off [] =
  return (off, [])
mapOrigLoadableRegions orig_layout redirs entries off (reg:rest) =
  case reg of
    ElfDataElfHeader -> do
      when (off /= 0) $ do
        throwE $ "Elf header appeared at unexpected offset: " ++ showHex (toInteger off) "."
      let off' = off + fromIntegral (ehdrSize (elfLayoutClass orig_layout))
      (off2, prev) <- mapOrigLoadableRegions orig_layout redirs entries off' rest
      return (off2, ElfDataElfHeader : prev)
    ElfDataSegmentHeaders -> do
      let b = BSL.toStrict $ Bld.toLazyByteString $
                buildElfSegmentHeaderTable orig_layout
      rawSegmentFromBuilder orig_layout redirs entries off b rest
    -- Flatten special segments
    ElfDataSegment seg  ->
      case elfSegmentType seg of
        -- Copy TLS segments.
        PT_TLS -> do
          let subseg = toList (elfSegmentData seg)
          (off2, subseg1) <- mapOrigLoadableRegions orig_layout redirs entries off subseg
          idx <- freshSegmentIndex
          let tls_seg = seg { elfSegmentIndex = idx
                            , elfSegmentData  = Seq.fromList subseg1
                            }

          (off3, rest1) <- mapOrigLoadableRegions orig_layout redirs entries off2 rest
          return $! (off3, ElfDataSegment tls_seg : rest1)

        _ -> do
          let subseg = toList (elfSegmentData seg)
          mapOrigLoadableRegions orig_layout redirs entries off (subseg ++ rest)

    ElfDataSectionHeaders ->
      throwE "Did not expect section headers in loadable region"
    ElfDataSectionNameTable _ ->
      throwE "Did not expect section name table in loadable region"
    ElfDataGOT g ->
      mapLoadableSection orig_layout redirs entries off (elfGotSection g) rest
    ElfDataStrtab _ -> do
      throwE "Did not expect .strtab in loadable region"
    ElfDataSymtab _ -> do
      throwE "Did not expect .strtab in loadable region"
    ElfDataSection s -> do
      mapLoadableSection orig_layout redirs entries off s rest
    ElfDataRaw b ->
      rawSegmentFromBuilder orig_layout redirs entries off b rest

------------------------------------------------------------------------
-- Merger

-- | Find relocation entries in section with given name.
findRelaEntries :: Elf Word64
                   -- ^ Object with relocations
                -> String
                   -- ^ Name of section containing relocation entries.
                -> Except String [RelaEntry X86_64_RelocationType]
findRelaEntries obj nm = do
  case nm `findSectionByName` obj of
    -- Assume that no section means no relocations
    [] -> return []
    [s] -> return $! elfRelaEntries (elfData obj) (elfSectionData s)
    _ -> throwE $  "Multple " ++ show nm ++ " sections in object file."

-- | Find the symbol table in the elf.
findSymbolTable :: Elf w
                    -- ^ Object with relocations
                 -> Except String (ElfSymbolTable w)
findSymbolTable obj = do
  case elfSymtab obj of
    -- Assume that no section means no relocations
    []  -> throwE $ "Could not find symbol table."
    [tbl] -> return tbl
    _   -> throwE $ "Multiple .symtab sections in object file."

checkOriginalBinaryAssumptions :: Monad m => Elf Word64 -> m ()
checkOriginalBinaryAssumptions binary = do
  when (elfData binary /= ELFDATA2LSB) $ do
    error $ "Expected least-significant bit first elf."
  when (elfType binary /= ET_EXEC) $ do
    fail $ "Expected a relocatable file as input."
  when (elfData binary /= ELFDATA2LSB) $ do
    fail $ "Expected the original binary to be least-significant bit first."
  when (elfType binary /= ET_EXEC) $ do
    fail $ "Expected the original binary is an executable."
  when (elfRelroRange binary /= Nothing) $ do
    fail $ "Expected no PT_GNU_RELO segment in binary."
  when (elfFlags binary /= 0) $ do
    fail $ "Expected elf flags in binary to be zero."

checkObjAssumptions :: Monad m
                    => Elf Word64
                    -> ElfOSABI
                    -> m ()
checkObjAssumptions obj expected_osabi = do
  -- Check new object properties.
  when (elfData obj /= ELFDATA2LSB) $ do
    fail $ "Expected the new binary binary to be least-significant bit first."
  when (elfType obj /= ET_REL) $ do
    fail $ "Expected a relocatable file as input."
  when (elfOSABI obj /= expected_osabi) $ do
    fail $ "Expected the new object to use the same OS ABI as original."
  when (elfMachine obj /= EM_X86_64) $ do
    fail $ "Only x86 64-bit executables are supported."
  when (elfRelroRange obj /= Nothing) $ do
    fail $ "Expected no PT_GNU_RELO segment in new object."
  when (elfFlags obj /= 0) $ do
    fail $ "Expected elf flags in new object to be zero."


didNotExpectOriginalRegion :: String -> Either String a
didNotExpectOriginalRegion region_name =
  Left $ "Did not expect " ++ region_name ++ " in original binary."

data OrigSegment w = OrigSegment { origSegPadding :: !w
                                   -- ^ Number of bytes of padding before segment
                                 , origSegFileOffset :: !w
                                   -- ^ File offset of new region
                                 , origSegData :: !(ElfSegment w)
                                 }

-- | Resovlv
regionsForOrigSegment :: (Bits w, Integral w)
                      => ElfLayout w
                      -> ResolvedRedirs w
                      -> OrigSegment w
                      -> RelinkM [ElfDataRegion w]
regionsForOrigSegment orig_layout redirs oseg = do
  let padding     = origSegPadding oseg
  let seg         = origSegData oseg
  seg' <- copyOrigLoadableSegment orig_layout redirs seg
  return $! dataPadding padding ++ [ ElfDataSegment seg' ]

data OriginalBinaryInfo w = OBI { _obiFileOffset  :: !w
                                , _obiSegments :: !(Seq (OrigSegment w))
                                , _obiLastVirtAddr :: !w
                                }

-- | Current offset in file as we are parsing data.
obiFileOffset :: Simple Lens (OriginalBinaryInfo w) w
obiFileOffset = lens _obiFileOffset (\s v -> s { _obiFileOffset = v })

-- | Information about segments in the original binary that will be
-- copied into new binary.
obiSegments :: Simple Lens (OriginalBinaryInfo w) (Seq (OrigSegment w))
obiSegments = lens _obiSegments (\s v -> s { _obiSegments = v })

-- | End of last virtual address of a loadable segment in the binary.
obiLastVirtAddr :: Simple Lens (OriginalBinaryInfo w) w
obiLastVirtAddr = lens _obiLastVirtAddr (\s v -> s { _obiLastVirtAddr = v })

initOriginalBinaryInfo :: Num w => w -> OriginalBinaryInfo w
initOriginalBinaryInfo o =
  OBI { _obiFileOffset = o
      , _obiSegments = Seq.empty
      , _obiLastVirtAddr = 0
      }

copyOrigLoadableSegment :: forall w
                        .  (Bits w, Integral w)
                        => ElfLayout w
                           -- ^ Layout of original bianry
                        -> ResolvedRedirs w
                           -- ^ Redirections in code
                        -> ElfSegment w
                        -> RelinkM (ElfSegment w)
copyOrigLoadableSegment orig_layout redirs seg = do
  let sub_reg = toList (elfSegmentData seg)
  let idx = elfSegmentIndex seg
  let entries = fromMaybe [] $! Map.lookup idx (crEntries redirs)
  (_,sub_reg') <- mapOrigLoadableRegions orig_layout redirs entries 0 sub_reg
  new_idx <- freshSegmentIndex
  return $! seg { elfSegmentIndex = new_idx
                , elfSegmentData = Seq.fromList sub_reg'
                }

copyOriginalBinaryRegion :: forall w
                          . (Bits w, Integral w, Show w)
                         => ElfLayout w
                         -> OriginalBinaryInfo w
                         -> ElfDataRegion w
                         -> Either String (OriginalBinaryInfo w)
copyOriginalBinaryRegion orig_layout info reg =
  case reg of
    ElfDataElfHeader -> do
      didNotExpectOriginalRegion "Elf header outside loadable segment."
    ElfDataSegmentHeaders ->
      didNotExpectOriginalRegion "Elf segment table outside loadable segment."
    ElfDataSegment seg ->
      case elfSegmentType seg of
        PT_LOAD -> do
          let a  = elfSegmentAlign seg
              mask = a  - 1
              req_align = elfSegmentVirtAddr seg .&. mask
              act_align = (info^.obiFileOffset) .&. mask
          -- Compute amount of padding to get alignment correct.
          let padding :: w
              padding | a <= 1 = 0
                      | act_align <= req_align = req_align - act_align
                        -- Need to insert padding to wrap around.
                      | otherwise = (a - act_align) + req_align
          let oseg = OrigSegment { origSegPadding = padding
                                 , origSegFileOffset = info^.obiFileOffset
                                 , origSegData = seg
                                 }
          return $! info & obiFileOffset   +~ padding + elfRegionFileSize orig_layout reg
                         & obiSegments     %~ (Seq.|> oseg)
                         & obiLastVirtAddr %~ max (elfSegmentVirtAddr seg)
        -- Drop non-loaded segments
        _ ->
          return info
    -- Drop section headers
    ElfDataSectionHeaders ->
      return info
    -- Drop section name table
    ElfDataSectionNameTable _ ->
      return info
    ElfDataGOT _ ->
      didNotExpectOriginalRegion "top-level .got table"
    -- Drop .strtab
    ElfDataStrtab _ -> do
      return info
    -- Drop .symtab
    ElfDataSymtab _ -> do
      return info
    -- Drop unloaded sections
    ElfDataSection _ -> do
      return info
    -- Drop bytes outside a loadable segment.
    ElfDataRaw _ -> do
      return info

copyOriginalBinaryRegions :: forall w
                           . (Bits w, Integral w, Show w)
                          => Elf w
                          -> ElfLayout w
                          -> w  -- ^ Offset of file.
                          -> Either String (OriginalBinaryInfo w)
copyOriginalBinaryRegions orig_binary orig_layout base_offset = do
  let f :: OriginalBinaryInfo w
        -> ElfDataRegion w
        -> Either String (OriginalBinaryInfo w)
      f = copyOriginalBinaryRegion orig_layout

  foldlM f (initOriginalBinaryInfo base_offset) (orig_binary^.elfFileData)

-- | Make padding region if number of bytes is non-zero.
dataPadding :: Integral w => w -> [ElfDataRegion w]
dataPadding 0 = []
dataPadding z = [ ElfDataRaw (BS.replicate (fromIntegral z) 0) ]

-- | Information about new object.
data NewObjectInfo w
   = NewObjectInfo { noiElf :: !(Elf w)
                     -- ^ Elf for object
                   , noiRelocInfo :: !(ObjectRelocationInfo w)
                   , noiSymbols :: !(V.Vector (ElfSymbolTableEntry w))
                   }

type RelinkM = ExceptT String (State (ObjRelocState Word64))

liftS :: Except String a -> RelinkM a
liftS m =
  case runExcept m of
    Left e -> throwE e
    Right v -> return v

-- | Create region for section in new object.
relocateObjectSection :: NewObjectInfo Word64
                         -- ^ Information about new object
                      -> Word64
                         -- ^ Base address of segment
                      -> NewSectionBounds Word64
                         -- ^ Section if we need to.
                      -> RelinkM [ElfDataRegion Word64]
relocateObjectSection _        _             NSBUndefined{} =
  return []
relocateObjectSection obj_info base_seg_addr (NSBDefined info pad off _)
  | sectionReloc info == "" = do
      let s = sectionVal info
      let sec_addr = base_seg_addr + off
      idx <- remapObjSectionIndex (elfSectionIndex s) sec_addr
      let s'  = s { elfSectionIndex = idx
                  , elfSectionAddr = sec_addr
                  }
      return $! dataPadding pad ++ [ ElfDataSection s' ]
  | otherwise = do
      let sec        = sectionVal info
          obj        = noiElf       obj_info
          reloc_info = noiRelocInfo obj_info
          syms       = noiSymbols   obj_info
      -- Find text relocations section
      relocs <- liftS $ findRelaEntries obj (sectionReloc info)
      -- Perform relocations
      let addr = base_seg_addr + off
      s <- lift $ get
      let (reloc_sec, s') = runST $ flip runStateT s $
            performRelocs reloc_info syms sec addr relocs
      lift $ put s'
      -- Get padding to add between end of header and start of code section.
      return $! dataPadding pad ++ [ ElfDataSection reloc_sec ]

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

-- | This merges an existing elf binary and new header with a list of redirections.
mergeObject :: Elf Word64
               -- ^ Existing binary
            -> Elf Word64
               -- ^ Object file to insert
            -> [CodeRedirection Word64]
               -- ^ Redirections from original file for new file.
            -> (Either String (Elf Word64), RelinkWarnings)
mergeObject orig_binary new_obj redirs = over _2 _warnings $ runState (runExceptT action) s
  where action = mergeObject' orig_binary new_obj redirs x86_64_immediate_jmp
        s = emptyObjRelocState

data NewSectionBounds w
  = NSBDefined !(SectionInfo w) !w !w !w
    -- ^ Section index, index, amount of padding, file start, and file end.
    -- File offset is relative to new segment.
  | NSBUndefined !w
    -- ^ Offset where section would have started/ended.

nsb_end :: NewSectionBounds w -> w
nsb_end (NSBDefined _ _ _ e) = e
nsb_end (NSBUndefined o) = o


nsb_entries :: Integral w => NewSectionBounds w -> w -> [(ElfSectionIndex, w)]
nsb_entries NSBUndefined{} _ = []
nsb_entries (NSBDefined info _ start _) base =
  let idx = elfSectionIndex (sectionVal info)
   in [ (ElfSectionIndex idx, base + start) ]

-- | Returns the file start and end of a section given an index of
-- the section or nothing if it is not defined.
get_section_bounds :: Integral w
                   => w -- ^ File offset for end of last section
                   -> Maybe (SectionInfo w)
                      -- ^ Information about section (or nothing) if we don't add section.
                   -> NewSectionBounds w
get_section_bounds off Nothing  = NSBUndefined off
get_section_bounds off (Just info) = NSBDefined info pad off' (off' + sz)
  where s   = sectionVal info
        pad  = fromIntegral (off' - off)
        off' = off `fixAlignment` elfSectionAddrAlign s
        sz   = elfSectionFileSize s

-- | Returns the file start and end of a section given an index of
-- the section or nothing if it is not defined.
get_all_section_bounds :: Integral w
                          => w -- ^ File offset for end of last section
                       -> [Maybe (SectionInfo w)]
                       -- ^ Information about sections.
                       -> ([NewSectionBounds w], w)
get_all_section_bounds off [] = ([], off)
get_all_section_bounds off (i:l) =
  let bounds = get_section_bounds off i
   in over _1 (bounds:) $ get_all_section_bounds (nsb_end bounds) l


-- | Identifies errors that occur when
data RelocateSymbolError
   = CommonSymbolUnsupported
   | SymbolNameNotFound   !BS.ByteString
   | SectionIndexNotFound !Word16

elfLocalEntryCount :: V.Vector (ElfSymbolTableEntry w) -> Word32
elfLocalEntryCount v = fromIntegral $ fromMaybe (V.length v) $ V.findIndex isNotLocal v
  where isNotLocal e = steBind e /= STB_LOCAL

-- | This maps a
relocateSymbolTableEntry :: Num w
                         => Map Word16 (Word16, w)
                            -- ^ Maps section index in object to new section index
                            -- and base address of section
                         -> Map BS.ByteString (Word16, w)
                            -- ^ Maps symbol names to index plus absolute address.
                         -> ElfSymbolTableEntry w
                         -> Either RelocateSymbolError (Maybe (ElfSymbolTableEntry w))
relocateSymbolTableEntry section_idx_map symbol_name_map ste
  | steType ste == STT_NOTYPE = Right (Just ste)
  | steType ste == STT_SECTION = Right Nothing
  | otherwise = do
      (idx, val) <-
        case steIndex ste of
          SHN_ABS    -> Right (SHN_ABS, steValue ste)
          SHN_COMMON -> error "CommonSymbolUnsupported"
          SHN_UNDEF  ->
            case Map.lookup (steName ste) symbol_name_map of
              Nothing         -> Left $ SymbolNameNotFound (steName ste)
              Just (idx,addr) -> Right (ElfSectionIndex idx, addr)
          ElfSectionIndex obj_sec_idx ->
            case Map.lookup obj_sec_idx section_idx_map of
              Nothing -> Left $ SectionIndexNotFound obj_sec_idx
              Just (idx, base) -> Right (ElfSectionIndex idx, base + steValue ste)
      let ste' = EST { steName  = steName ste
                     , steType  = steType ste
                     , steBind  = STB_LOCAL
                     , steOther = steOther ste
                     , steIndex = idx
                     , steValue = val
                     , steSize  = steSize ste
                     }
      Right $! Just ste'

relocateSymbolTable :: Num w
                    => Map Word16 (Word16, w)
                    -> Map BS.ByteString (Word16, w)
                    -> Word16
                       -- ^ Index of new symbol table
                    -> ElfSymbolTable w
                    -> ([(ElfSymbolTableEntry w, RelocateSymbolError)], ElfSymbolTable w)
relocateSymbolTable section_idx_map symbol_name_map idx tbl = (fin_errs, tbl')
  where (fin_errs,fin_syms) = foldr resolveEntry ([], []) (elfSymbolTableEntries tbl)
        sym_v = V.fromList fin_syms
        resolveEntry ste (errs, syms) =
          case relocateSymbolTableEntry section_idx_map symbol_name_map ste of
            Left e -> ((ste,e):errs, syms)
            Right Nothing  -> (errs, syms)
            Right (Just e) -> (errs, e:syms)
        tbl' = ElfSymbolTable { elfSymbolTableIndex        = idx
                              , elfSymbolTableEntries      = sym_v
                              , elfSymbolTableLocalEntries = elfLocalEntryCount sym_v
                              }


mergeObject' :: Elf Word64 -- ^ Existing binary
             -> Elf Word64 -- ^ Information about object file to insert
             -> [CodeRedirection Word64] -- ^ Redirections
             -> (Word64 -> BS.ByteString)
                -- ^ Function for creating jump to given offset.
             -> RelinkM (Elf Word64)
mergeObject' orig_binary obj redirs mkJump = do
  let elf_class = ELFCLASS64

  -- Check original binary properties
  checkOriginalBinaryAssumptions orig_binary

  checkObjAssumptions obj (elfOSABI orig_binary)

  -- Find address for new code.
  let elf_align = elfAlignment orig_binary

  -- First build what we want to create

  data_sec_info <- liftS $
    findSectionInfo obj ".data" SHT_PROGBITS (shf_alloc .|. shf_write) ".rela.data"
  bss_sec_info  <- liftS $
    findSectionInfo obj ".bss"  SHT_NOBITS   (shf_alloc .|. shf_write) ""

  ----------------------------------------------------------------------
  -- First we determine the number of program headers as this is needed
  -- for layout

  -- Flag indicating whether to add GNU stack segment.
  let add_gnu_stack = elfHasGNUStackSegment orig_binary
                   && isJust (elfGNUStackSection obj)

  -- Flag indicating whether to add TLS segment
  let add_tls = elfHasTLSSegment orig_binary

  when (elfHasTLSSection obj) $ do
    throwE $ "TLS section is not allowed in new code object."
  let phdr_count = loadableSegmentCount orig_binary
                 + 1 -- We always add new code segment
                 + (if add_new_data_seg then 1 else 0)
                 + (if add_gnu_stack then 1 else 0)
                 + (if add_tls       then 1 else 0)
        where add_new_data_seg = isJust data_sec_info
                              || isJust  bss_sec_info

  let orig_layout = elfLayout orig_binary
  orig_binary_info <-
    case copyOriginalBinaryRegions orig_binary orig_layout 0 of
      Left msg -> throwE msg
      Right obi -> return obi

  let orig_binary_file_end = orig_binary_info^.obiFileOffset

  let exec_seg_header_size :: Word64
      exec_seg_header_size = fromIntegral phdr_count * fromIntegral (phdrEntrySize elf_class)

  -- Find text section
  text_sec_info     <- do
    let flags = shf_alloc .|. shf_execinstr
    liftS $ findSectionInfo obj ".text"     SHT_PROGBITS flags ".rela.text"
  rodata_sec_info   <- do
    let flags = shf_alloc
    liftS $ findSectionInfo obj ".rodata"   SHT_PROGBITS flags ".rela.rodata"
  eh_frame_sec_info <- do
    let flags = shf_alloc
    liftS $ findSectionInfo obj ".eh_frame" SHT_PROGBITS flags ".rela.eh_frame"

  -- Bounds for all sections in code segment.
  let (code_sec_bounds, new_code_seg_filesize) =
        get_all_section_bounds exec_seg_header_size
          [ text_sec_info, rodata_sec_info, eh_frame_sec_info ]

  let new_code_file_offset = orig_binary_file_end
  let new_code_file_end    = new_code_file_offset + new_code_seg_filesize

  -- Compute offset for new data and ensure it is aligned.
  -- Get bounds of ".data" section.
  let data_sec_bounds = get_section_bounds 0 data_sec_info
  -- Compute bounds of ".bss" section.
  let bss_sec_bounds = get_section_bounds (nsb_end data_sec_bounds) bss_sec_info

  let data_seg_bounds = [ data_sec_bounds, bss_sec_bounds ]

  let new_data_file_offset = new_code_file_end

  let new_code_seg_addr = (orig_binary_info^.obiLastVirtAddr) `fixAlignment` elf_align
                        + new_code_file_offset .&. (elf_align - 1)
  let new_code_seg_virt_end = new_code_seg_addr + new_code_seg_filesize
  let new_data_seg_addr = new_code_seg_virt_end `fixAlignment` elf_align
                        + new_data_file_offset .&. (elf_align - 1)

  -- Get symbols in object.
  obj_symbols <- liftS $ findSymbolTable obj

  let reloc_info = ObjectRelocationInfo { objectSectionMap = section_map
                                        , binarySymbolMap = sym_map
                                        }
        where section_map = Map.fromList $
                concatMap    (`nsb_entries` new_code_seg_addr) code_sec_bounds
                ++ concatMap (`nsb_entries` new_data_seg_addr) data_seg_bounds
              sym_map = createBinarySymbolMap orig_binary

  let resolved_redirs =
        CR { crMkJump    = mkJump
           , crRelocInfo = reloc_info
           , crSymbols   = mapFromList steName (V.toList (elfSymbolTableEntries obj_symbols))
           , crEntries   = mapFromList redirSourcePhdr redirs
           }

  orig_binary_regions <- do
    let regions = orig_binary_info^.obiSegments
    let remapSeg = regionsForOrigSegment orig_layout resolved_redirs
    concat <$> traverse remapSeg (toList regions)

  let obj_info = NewObjectInfo { noiElf = obj
                               , noiRelocInfo = reloc_info
                               , noiSymbols = elfSymbolTableEntries obj_symbols
                               }

  -- Create Elf segment
  new_exec_regions <- do
     concat <$> traverse (relocateObjectSection obj_info new_code_seg_addr)
                         code_sec_bounds


  exec_seg <- do
    seg_index <- freshSegmentIndex
    pure $! ElfSegment
      { elfSegmentType     = PT_LOAD
      , elfSegmentFlags    = pf_r .|. pf_x
      , elfSegmentIndex    = seg_index
      , elfSegmentVirtAddr = new_code_seg_addr
      , elfSegmentPhysAddr = new_code_seg_addr
      , elfSegmentAlign    = elf_align
      , elfSegmentMemSize  = ElfRelativeSize 0
      , elfSegmentData     = Seq.fromList $
        [ ElfDataSegmentHeaders ]
        ++ new_exec_regions
      }

  data_regions <-
    concat <$> traverse (relocateObjectSection obj_info new_data_seg_addr)
                        data_seg_bounds

  let new_bss_size =
        case bss_sec_bounds of
          NSBUndefined{} -> 0
          NSBDefined info _ _ _ -> elfSectionSize (sectionVal info)

  nextSegmentIndex += 1

  -- List of new load segments
  new_data_segs <- do
    case () of
      _ | null data_regions -> return []
        | otherwise -> do
            seg_index <- freshSegmentIndex
            let seg = ElfSegment
                  { elfSegmentType     = PT_LOAD
                  , elfSegmentFlags    = pf_r .|. pf_w
                  , elfSegmentIndex    = seg_index
                  , elfSegmentVirtAddr = new_data_seg_addr
                  , elfSegmentPhysAddr = new_data_seg_addr
                  , elfSegmentAlign    = elf_align
                  , elfSegmentMemSize  = ElfRelativeSize new_bss_size
                  , elfSegmentData     = Seq.fromList data_regions
                  }
            return [ ElfDataSegment seg ]

  gnu_stack_segment_headers <-
    case elfGNUStackSection obj of
      Just s | add_gnu_stack -> do
        seg <- gnuStackSegment (elfSectionIndex s)
        pure [ ElfDataSegment seg ]
      _ -> pure []

  new_shstrtab_index <- freshSectionIndex
  new_symtab_index   <- freshSectionIndex
  new_strtab_index   <- freshSectionIndex

  section_idx_map <- use objSectionMap

  let (_symtab_errs, symtab) =
         relocateSymbolTable section_idx_map symbol_name_map new_symtab_index obj_symbols
        where symbol_name_map = Map.empty

  return $! Elf { elfData       = ELFDATA2LSB
                , elfClass      = elf_class
                , elfOSABI      = elfOSABI orig_binary
                , elfABIVersion = 0
                , elfType       = ET_EXEC
                , elfMachine    = EM_X86_64
                , elfEntry      = elfEntry orig_binary
                , elfFlags      = 0
                , _elfFileData  = Seq.fromList $
                   orig_binary_regions
                   ++ dataPadding (new_code_file_offset - orig_binary_file_end)
                   ++ [ ElfDataSegment exec_seg ]
                   ++ new_data_segs
                   ++ gnu_stack_segment_headers
                   ++ [ ElfDataSectionNameTable new_shstrtab_index
                      , ElfDataSymtab symtab
                      , ElfDataStrtab new_strtab_index
                      , ElfDataSectionHeaders
                      ]
                , elfRelroRange = Nothing
                }
