{-|
Module      : Reopt.Relinker
Copyright   : (c) Galois Inc, 2016
License     : AllRightsReserved
Maintainer  : jhendrix@galois.com

This module is a start towards a binary and object merging tool.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Reopt.Relinker
  ( CodeRedirection(..)
  , mergeObject
  ) where

import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
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
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Data.Word
import           Numeric (showHex)

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

hasSectionName :: ElfSection w -> String -> Bool
s `hasSectionName` nm = elfSectionName s == nm


tryFindSectionIndex :: V.Vector (ElfSection w) -> String -> [Int]
tryFindSectionIndex sections nm =
  V.toList (V.findIndices (`hasSectionName` nm) sections)

-- | Information about section in objhect needed for computing layout
-- and performing relocations.
data SectionInfo w = SectionInfo { sectionIndex :: !Int
                                   -- ^ Index of section in binary
                                 , sectionVal   :: (ElfSection w)
                                   -- ^ Actual section
                                 , sectionReloc :: !String
                                 }

-- | Find a section with the given name and flags.
--
-- This returns 'Nothing' if no section with those flags exists, but throws
-- an error if multiple do.
findSectionInfo :: Eq w
                => V.Vector (ElfSection w)
                -> String
                   -- ^ Expected name of section
                -> ElfSectionType
                   -- ^ Expected section type.
                -> ElfSectionFlags w
                   -- ^ Expected section flags
                -> String
                    -- ^ Name of relocation section (or "" if no relocations associateD)
                 -> Except String (Maybe (SectionInfo w))
findSectionInfo sections nm tp flags reloc =
  case tryFindSectionIndex sections nm of
    [i] -> do
      let sec = sections V.! i
      when (elfSectionType sec /= tp) $ do
        throwE $ elfSectionName sec ++ " section has unexpected permissions."
      when (elfSectionFlags sec /= flags) $ do
        throwE $ elfSectionName sec ++ " section has unexpected permissions."
      let info = SectionInfo i sec reloc
      seq info $ (return $! Just info)
    []  -> return Nothing
    _   -> throwE $ "Multiple " ++ nm ++ " sections in object file."

-- | Return segment used to indicate the stack can be non-executable.
gnuStackSegment :: Num w => PhdrIndex -> ElfSegment w
gnuStackSegment idx =
  ElfSegment { elfSegmentType     = PT_GNU_STACK
             , elfSegmentFlags    = pf_r .|. pf_w
             , elfSegmentIndex    = idx
             , elfSegmentVirtAddr = 0
             , elfSegmentPhysAddr = 0
             , elfSegmentAlign    = 0
             , elfSegmentMemSize  = ElfRelativeSize 0
             , elfSegmentData     = Seq.empty
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

elfHasGNUStackSection :: Elf w -> Bool
elfHasGNUStackSection e =
  any (\s -> elfSectionName s == ".note.GNU-stack") (e^..elfSections)

findSectionFromHeaders :: String -> ElfHeaderInfo w -> [ElfSection w]
findSectionFromHeaders nm info =
  filter (\s -> elfSectionName s == nm) $ V.toList $ getSectionTable info

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
createBinarySymbolMap :: ElfHeaderInfo w -> BinarySymbolMap w
createBinarySymbolMap binary = do
  case ".symtab" `findSectionFromHeaders` binary of
     -- Assume that no section means no relocations
    []  -> Map.empty
    [s] -> Map.fromList $ mapMaybe symbolNameAddr $ getSymbolTableEntries binary s
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
                  -> ObjectRelocationInfo w
                  -> Either String w
objectSectionAddr src idx info =
  case Map.lookup idx (objectSectionMap info) of
    Nothing ->
      Left $ src ++ "refers to an unmapped section index " ++ show idx ++ "."
    Just r ->
      Right r

-- | Get the address of a symbol table if it is mapped in the section map.
symbolAddr :: (Eq w, Num w)
           => ObjectRelocationInfo w
              -- ^ Information needed for relocations.
           -> String
              -- ^ Name of reference to a symbol
           -> ElfSymbolTableEntry w
           -> Either String w
symbolAddr reloc_info src sym =
  case steType sym of
    STT_SECTION
      | steValue sym /= 0 ->
        Left "symbolAddr expects section names to have 0 offset."
      | steIndex sym == SHN_UNDEF ->
        Left "Reference to undefined section."
      | otherwise ->
          objectSectionAddr src (steIndex sym) reloc_info
    STT_FUNC
      | steIndex sym == SHN_UNDEF ->
        Left "Function symbol has undefined section."
      | otherwise ->
          objectSectionAddr src (steIndex sym) reloc_info
    STT_NOTYPE
      | steIndex sym /= SHN_UNDEF ->
          Left "Expected STT_NOTYPE symbol to refer to SHN_UNDEF section."
      | otherwise ->
        case Map.lookup (steName sym) (binarySymbolMap reloc_info) of
          Nothing -> Left $ "Could not resolve symbol " ++ BSC.unpack (steName sym) ++ "."
          Just addr -> Right addr
    tp -> error $ "symbolAddr does not support symbol with type " ++ show tp ++ "."

------------------------------------------------------------------------
-- Code for performing relocations in new object.

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
             -> ST s ()
performReloc reloc_info sym_table this_vaddr mv reloc = do
  -- Offset to modify.
  let off = r_offset reloc :: Word64
  let sym = getSymbolByIndex sym_table (r_sym reloc)
  -- Get the address of a symbol
  let sym_val =
          case symbolAddr reloc_info "A relocation entry" sym of
            Right v -> v
            Left msg -> error msg
  -- Relocation addend
  let addend = r_addend reloc :: Int64
  -- Get PC offset
  let pc_offset = this_vaddr + off
  -- Parse on type
  case r_type reloc of
    R_X86_64_PC32 ->
          write32_lsb mv off res32
      where res64 = sym_val + fromIntegral addend - pc_offset :: Word64
            res32 = fromIntegral res64 :: Word32
    R_X86_64_32
        | fromIntegral res32 /= res64 ->
          error $ "Relocation of " ++ steStringName sym
             ++ " at " ++ showHex sym_val " + " ++ show addend
             ++ " does not safely zero extend."
        | otherwise ->
          write32_lsb mv off res32
      where res64 = sym_val + fromIntegral addend :: Word64
            res32 = fromIntegral res64 :: Word32
    R_X86_64_32S
        | fromIntegral res32 /= res64 ->
          error $ "Relocation of " ++ steStringName sym
             ++ " at " ++ showHex sym_val " + " ++ show addend
             ++ " does not safely sign extend."
        | otherwise ->
          write32_lsb mv off (fromIntegral res32)
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
              -> ElfSection Word64
performRelocs reloc_info sym_table section this_vaddr relocs = runST $ do
  let dta = elfSectionData section
  let len = BS.length dta
  mv <- SMV.new len
  -- Copy original bytes into bytestring
  writeBS mv 0 dta
  -- Updpate using relocations
  mapM_ (performReloc reloc_info sym_table this_vaddr mv) relocs

  let SMV.MVector _ fp = mv
  let reloc_data = Data.ByteString.Internal.fromForeignPtr fp 0 len
  return $! section { elfSectionAddr = this_vaddr
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
              Right r -> r
              Left msg -> error msg
      writeBS mv (fromIntegral (off - base)) (crMkJump redirs tgt)

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
                      -> MapMonad (w, [ElfDataRegion w])
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
                   -> MapMonad (w, [ElfDataRegion w])
mapLoadableSection orig_layout redirs entries off sec rest = do
  let bs = elfSectionData sec
  let off' = off + fromIntegral (BS.length bs)
  let sec' = sec { elfSectionName = ".orig" ++ elfSectionName sec
                 , elfSectionData = remapBytes redirs entries off bs
                 }
  seq sec' $ do
  (off2, prev) <- mapOrigLoadableRegions orig_layout redirs entries off' rest
  return (off2, ElfDataSection sec' : prev)

newtype MapState = MapState { _nextSegmentIndex :: Word16 }

nextSegmentIndex :: Simple Lens MapState Word16
nextSegmentIndex = lens _nextSegmentIndex (\s v -> s { _nextSegmentIndex = v })

getSegmentIndex :: MapMonad Word16
getSegmentIndex = do
  i <- use nextSegmentIndex
  nextSegmentIndex += 1
  return $! i

type MapMonad = StateT MapState (Except String)

runMapMonad :: MapState -> MapMonad a -> Either String (a, MapState)
runMapMonad s m = runExcept (runStateT m s)

mapError :: String -> MapMonad a
mapError = lift . throwE

-- | This traverses elf data regions in an loadable elf segment.
mapOrigLoadableRegions :: (Bits w, Integral w)
                       => ElfLayout w
                          -- ^ Layout created for original binary.
                       -> ResolvedRedirs w
                       -> [CodeRedirection w]
                          -- ^ Redirections for segment.
                       -> w -- ^ Offset in segment for region
                       -> [ElfDataRegion w]
                       -> MapMonad (w, [ElfDataRegion w])
mapOrigLoadableRegions _ _ _ off [] =
  return (off, [])
mapOrigLoadableRegions orig_layout redirs entries off (reg:rest) =
  case reg of
    ElfDataElfHeader -> do
      when (off /= 0) $ do
        mapError $ "Elf header appeared at unexpected offset: " ++ showHex (toInteger off) "."
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
          idx <- getSegmentIndex
          let tls_seg = seg { elfSegmentIndex = idx
                            , elfSegmentData  = Seq.fromList subseg1
                            }

          (off3, rest1) <- mapOrigLoadableRegions orig_layout redirs entries off2 rest
          return $! (off3, ElfDataSegment tls_seg : rest1)

        _ -> do
          let subseg = toList (elfSegmentData seg)
          mapOrigLoadableRegions orig_layout redirs entries off (subseg ++ rest)

    ElfDataSectionHeaders ->
      lift $ throwE "Did not expect section headers in loadable region"
    ElfDataSectionNameTable ->
      lift $ throwE "Did not expect section name table in loadable region"
    ElfDataGOT g ->
      mapLoadableSection orig_layout redirs entries off (elfGotSection g) rest
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

-- | Find relocation entries in section with given name.
findSymbolTable :: ElfHeaderInfo Word64
                   -- ^ Object with relocations
                -> Except String (V.Vector (ElfSymbolTableEntry Word64))
findSymbolTable obj = do
  case ".symtab" `findSectionFromHeaders` obj of
    -- Assume that no section means no relocations
    []  -> throwE $ "Could not find symbol table."
    [s] -> return $! V.fromList $ getSymbolTableEntries obj s
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
                      -> MapMonad [ElfDataRegion w]
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
                        -> MapMonad (ElfSegment w)
copyOrigLoadableSegment orig_layout redirs seg = do
  let sub_reg = toList (elfSegmentData seg)
  let idx = elfSegmentIndex seg
  let entries = fromMaybe [] $! Map.lookup idx (crEntries redirs)
  (_,sub_reg') <- mapOrigLoadableRegions orig_layout redirs entries 0 sub_reg
  new_idx <- getSegmentIndex
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
    ElfDataSectionNameTable ->
      return info
    ElfDataGOT _ ->
      didNotExpectOriginalRegion "top-level .got table"
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

data NewObjectInfo w
  = NewObjectInfo { noiElf :: !(Elf w)
                    -- ^ Elf for object
                  , noiSections :: !(V.Vector (ElfSection w))
                    -- ^ Vector of sections in List of all sections
                  , noiRelocInfo :: !(ObjectRelocationInfo w)
                  , noiSymbols :: !(V.Vector (ElfSymbolTableEntry w))
                  }

-- | Create region for section in new object.
relocateObjectSection :: NewObjectInfo Word64
                           -- ^ Information about new object
                        -> Word64
                           -- ^ Base address of segment
                        -> NewSectionBounds Word64
                           -- ^ Section if we need to.
                        -> Except String [ElfDataRegion Word64]
relocateObjectSection _        _             NSBUndefined{} =
  return []
relocateObjectSection obj_info base_seg_addr (NSBDefined info pad off _)
  | sectionReloc info == "" = do
      let s = sectionVal info
          s'  = s { elfSectionAddr = base_seg_addr + off }
      return $! dataPadding pad ++ [ ElfDataSection s' ]
  | otherwise = do
      let sec        = sectionVal info
          obj        = noiElf       obj_info
          reloc_info = noiRelocInfo obj_info
          syms       = noiSymbols   obj_info
      -- Find text relocations section
      relocs <- findRelaEntries obj (sectionReloc info)
      -- Perform relocations
      let addr = base_seg_addr + off
      let reloc_sec = performRelocs reloc_info syms sec addr relocs
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
mergeObject :: ElfHeaderInfo Word64
               -- ^ Existing binary
            -> ElfHeaderInfo Word64
               -- ^ Object file to insert
            -> [CodeRedirection Word64]
               -- ^ Redirections from original file for new file.
            -> Either String (Elf Word64)
mergeObject orig_binary new_obj redirs =
  runExcept $ mergeObject' orig_binary new_obj redirs x86_64_immediate_jmp

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
  [ (fromIntegral (sectionIndex info), base + start)
  ]

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

mergeObject' :: ElfHeaderInfo Word64 -- ^ Existing binary
             -> ElfHeaderInfo Word64 -- ^ Information about object file to insert
             -> [CodeRedirection Word64] -- ^ Redirections
             -> (Word64 -> BS.ByteString)
                -- ^ Function for creating jump to given offset.
             -> Except String (Elf Word64)
mergeObject' orig_binary_header obj_header redirs mkJump = do
  let orig_binary = getElf orig_binary_header
  let elf_class = ELFCLASS64

  -- Check original binary properties
  checkOriginalBinaryAssumptions orig_binary

  let obj = getElf obj_header
  checkObjAssumptions obj (elfOSABI orig_binary)

  let sections = getSectionTable obj_header

  -- Find address for new code.
  let elf_align = elfAlignment orig_binary

  -- First build what we want to create

  data_sec_index <-
    findSectionInfo sections ".data" SHT_PROGBITS (shf_alloc .|. shf_write) ".rela.data"
  bss_sec_index  <-
    findSectionInfo sections ".bss"  SHT_NOBITS   (shf_alloc .|. shf_write) ""

  ----------------------------------------------------------------------
  -- First we determine the number of program headers as this is needed
  -- for layout

  -- Flag indicating whether to add GNU stack segment.
  let add_gnu_stack = elfHasGNUStackSegment orig_binary
                   && elfHasGNUStackSection obj

  -- Flag indicating whether to add TLS segment
  let add_tls = elfHasTLSSegment orig_binary

  when (elfHasTLSSection obj) $ do
    throwE $ "TLS section is not allowed in new code object."
  let phdr_count = loadableSegmentCount orig_binary
                 + 1 -- We always add new code segment
                 + (if add_new_data_seg then 1 else 0)
                 + (if add_gnu_stack then 1 else 0)
                 + (if add_tls       then 1 else 0)
        where add_new_data_seg = isJust data_sec_index
                              || isJust bss_sec_index

  let orig_layout = elfLayout orig_binary
  orig_binary_info <-
    case copyOriginalBinaryRegions orig_binary orig_layout 0 of
      Left msg -> throwE msg
      Right obi -> return obi

  let orig_binary_file_end = orig_binary_info^.obiFileOffset

  let exec_seg_header_size :: Word64
      exec_seg_header_size = fromIntegral phdr_count * fromIntegral (phdrEntrySize elf_class)

  -- Find text section
  text_sec_bounds     <- do
    let flags = shf_alloc .|. shf_execinstr
    get_section_bounds exec_seg_header_size
      <$> findSectionInfo sections ".text"     SHT_PROGBITS flags ".rela.text"
  rodata_sec_bounds   <- do
    let flags = shf_alloc
    get_section_bounds (nsb_end text_sec_bounds)
      <$> findSectionInfo sections ".rodata"   SHT_PROGBITS flags ".rela.rodata"
  eh_frame_sec_bounds <- do
    let flags = shf_alloc
    get_section_bounds (nsb_end rodata_sec_bounds)
      <$> findSectionInfo sections ".eh_frame" SHT_PROGBITS flags ".rela.eh_frame"

  let new_code_seg_filesize = nsb_end eh_frame_sec_bounds
  let new_code_file_offset = orig_binary_file_end
  let new_code_file_end    = new_code_file_offset + new_code_seg_filesize

  -- Compute offset for new data and ensure it is aligned.
  -- Get bounds of ".data" section.
  let data_sec_bounds = get_section_bounds 0 data_sec_index
  let post_data_sec_size = nsb_end data_sec_bounds
  -- Compute bounds of ".bss" section.
  let bss_sec_bounds = get_section_bounds post_data_sec_size bss_sec_index

  let new_data_file_offset = new_code_file_end

  let new_code_seg_addr = (orig_binary_info^.obiLastVirtAddr) `fixAlignment` elf_align
                        + new_code_file_offset .&. (elf_align - 1)
  let new_code_seg_virt_end = new_code_seg_addr + new_code_seg_filesize
  let new_data_seg_addr = new_code_seg_virt_end `fixAlignment` elf_align
                         + new_data_file_offset .&. (elf_align - 1)

  let reloc_info = ObjectRelocationInfo { objectSectionMap = section_map
                                        , binarySymbolMap = sym_map
                                        }
        where section_map = Map.fromList $
                nsb_entries    text_sec_bounds     new_code_seg_addr
                ++ nsb_entries rodata_sec_bounds   new_code_seg_addr
                ++ nsb_entries eh_frame_sec_bounds new_code_seg_addr
                ++ nsb_entries data_sec_bounds     new_data_seg_addr
                ++ nsb_entries bss_sec_bounds      new_data_seg_addr
              sym_map = createBinarySymbolMap orig_binary_header

  -- Get symbols in object.
  symbols     <- findSymbolTable obj_header

  let resolved_redirs =
        CR { crMkJump    = mkJump
           , crRelocInfo = reloc_info
           , crSymbols   = mapFromList steName (V.toList symbols)
           , crEntries   = mapFromList redirSourcePhdr redirs
           }

  (orig_binary_regions,seg_index) <- do
    let s0 = MapState { _nextSegmentIndex = 0 }
    let regions = orig_binary_info^.obiSegments
    let remapSeg = regionsForOrigSegment orig_layout resolved_redirs
    let doRemap = concat <$> traverse remapSeg (toList regions)

    case runMapMonad s0 doRemap of
      Left msg -> throwE msg
      Right (r,s) -> return (r,s^.nextSegmentIndex)

  let obj_info = NewObjectInfo { noiElf = obj
                               , noiSections = sections
                               , noiRelocInfo = reloc_info
                               , noiSymbols = symbols
                               }

  -- Create Elf segment
  new_exec_regions <- do
     concat <$> traverse (relocateObjectSection obj_info new_code_seg_addr)
                         [ text_sec_bounds, rodata_sec_bounds, eh_frame_sec_bounds ]

  let exec_seg = ElfSegment
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
                        [ data_sec_bounds, bss_sec_bounds ]
  let new_bss_size =
        case bss_sec_bounds of
          NSBUndefined{} -> 0
          NSBDefined info _ _ _ -> elfSectionSize (sectionVal info)

  -- List of new load segments
  let (new_data_segs, seg_index')
          | null data_regions = ([], seg_index + 1)
          | otherwise =
             ( [ ElfDataSegment seg ]
             , seg_index + 2
             )
        where seg = ElfSegment
                { elfSegmentType     = PT_LOAD
                , elfSegmentFlags    = pf_r .|. pf_w
                , elfSegmentIndex    = seg_index + 1
                , elfSegmentVirtAddr = new_data_seg_addr
                , elfSegmentPhysAddr = new_data_seg_addr
                , elfSegmentAlign    = elf_align
                , elfSegmentMemSize  = ElfRelativeSize new_bss_size
                , elfSegmentData     = Seq.fromList data_regions
                }

  let gnu_stack_segment_headers
          | add_gnu_stack = [ ElfDataSegment (gnuStackSegment seg_index') ]
          | otherwise = []

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
                   ++ [ ElfDataSectionNameTable
                      , ElfDataSectionHeaders
                      ]
                , elfRelroRange = Nothing
                }
