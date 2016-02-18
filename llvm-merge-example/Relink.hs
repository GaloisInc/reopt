{-|
Module      : Relink
Copyright   : (c) Galois Inc, 2016
License     : None
Maintainer  : jhendrix@galois.com

This module is a start towards a binary and object merging tool.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as Bld
import           Data.Elf
import           Data.Foldable
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import           Data.Word
import           System.Random

import qualified RangeSet as RS


failIfNothing :: Monad m => String -> Maybe a -> m a
failIfNothing msg Nothing  = fail msg
failIfNothing _   (Just a) = return a


readElf64 :: (BS.ByteString -> Either (ByteOffset, String) (SomeElf f))
          -> FilePath
          -> IO (f Word64)
readElf64 parseFn path = do
  b <- BS.readFile path
  case parseFn b of
    Left (_,msg) -> fail $ msg
    Right (Elf64 e) -> do
      return e
    Right (Elf32 _) -> do
      fail $ "Expected a 64-bit Elf"

data MergerState = MS { _mergerGen :: !StdGen
                      , _mergerReserved :: !(RS.RangeSet Word64)
                      }


mergerGen :: Simple Lens MergerState StdGen
mergerGen = lens _mergerGen (\s v -> s { _mergerGen = v})

mergerReserved :: Simple Lens MergerState (RS.RangeSet Word64)
mergerReserved = lens _mergerReserved (\s v -> s { _mergerReserved = v })

type Merger = State MergerState

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

reservedAddrs :: ElfLayout Word64 -> Either String (RS.RangeSet Word64)
reservedAddrs l = foldl' f (Right RS.empty) (allPhdrs l)
  where
        f :: Either String (RS.RangeSet Word64)
          -> Phdr Word64
          -> Either String (RS.RangeSet Word64)
        f (Left msg) _ = Left msg
        f (Right m) p = do
          let seg = phdrSegment p
          case elfSegmentType seg of
             PT_LOAD -> Right $ RS.insert low high m
               where low = elfSegmentVirtAddr seg
                     high = low + phdrMemSize p - 1
             PT_DYNAMIC -> Left "Dynamic elf files not yet supported."
             _ -> Right m


runMerger :: Elf Word64 -> Merger a -> IO a
runMerger e m = do
  gen <- getStdGen
  case reservedAddrs (elfLayout e) of
    Left msg -> fail msg
    Right s -> do
      let ms = MS { _mergerGen = gen
                  , _mergerReserved = s
                  }
      seq ms $ return $! evalState m ms

page_size :: Word64
page_size = 0x1000

-- | Given a region this finds a new random address to store this.
findNewAddress :: Word64
                   -- ^ Size of space to reserve
                -> Word64
                   -- ^ A power of two that is the modulus used in alignment
                   -- calculations
                -> Word64
                   -- ^ The offset in the file where this will be added.
                -> Merger Word64
findNewAddress size 0 file_offset = do
  findNewAddress size 1 file_offset
findNewAddress size align file_offset = do
  reserved <- use mergerReserved
  when (not (isPowerOf2 align)) $ do
    fail $ "Elf alignment should be power of two."
  let page_mask = page_size - 1
  -- Round up to next multiple of a page.
  let adjusted_offset = (file_offset + page_mask) .&. complement page_mask
  -- Get regions currently loaded.
  g <- use mergerGen
  -- Mask contains the low 48 order bits at a multiple of the page size.
  let low_mask = 2^(48::Word64) - 1

  let (r,g') = random g
  mergerGen .= g'
  let addr = (r .&. low_mask .&. complement (align - 1))
           .|. (adjusted_offset  .&. (align - 1))
  let isGood = addr /= 0 && not (RS.overlaps addr (addr + size) reserved)
  if isGood then do
    -- Add to reserved.
    mergerReserved %= RS.insert addr (addr + size)
    return addr
   else
    -- Try again
    findNewAddress size align file_offset

-- | 'nextMult v i' returns the smallest number greater than 'v'
-- that is a multiple of '2^i'.
nextMult :: Word64 -> Word64 -> Word64
nextMult w n = (w + (m - 1)) .&. complement m
  where m = 2^n

elfAlignment :: Num w => Elf w -> w
elfAlignment e =
    case loadableSegments of
      [] -> 0
      (s:_) -> elfSegmentAlign s
  where isLoadable s = elfSegmentType s == PT_LOAD
        loadableSegments = filter isLoadable $ elfSegments e

-- | Returns true if this is a power of two or zero.
isPowerOf2 :: Word64 -> Bool
isPowerOf2 w = w .&. (w-1) == 0

type Contents s = SMV.MVector s Word8

write32_lsb :: Contents s -> Word64 -> Word32 -> ST s ()
write32_lsb v a c = do
  -- Assert there are at least
  assert (a <= -4) $ do
  let i = fromIntegral a
  SMV.write v i     $ fromIntegral c
  SMV.write v (i+1) $ fromIntegral (c `shiftR`  8)
  SMV.write v (i+2) $ fromIntegral (c `shiftR` 16)
  SMV.write v (i+3) $ fromIntegral (c `shiftR` 24)

-- | Map from section indices in object to virtual address where it will be
-- loaded into binary.
type SectionMap w = Map ElfSectionIndex w

-- | Return address that
getSectionBase :: SectionMap w -> ElfSectionIndex -> w
getSectionBase m i = fromMaybe (error msg) $ Map.lookup i m
  where msg = "Symbol table points to section " ++ show i ++ " not being mapped to bianry."


symbolAddr :: Num w => SectionMap w -> ElfSymbolTableEntry w -> w
symbolAddr section_map sym = getSectionBase section_map (steIndex sym) + steValue sym


type SymbolTable w = V.Vector (ElfSymbolTableEntry w)

getSymbol :: SymbolTable w -> SymbolTableIndex -> ElfSymbolTableEntry w
getSymbol sym_table sym_index
  | sym_index >= fromIntegral (V.length sym_table) =
    error $ "Symbolic offset " ++ show sym_index ++ " is out of range."
  | otherwise = sym_table V.! fromIntegral sym_index

performReloc :: SectionMap Word64
                -- ^ Maps elf section indices in object to the base virtual address
                -- in the binary.
             -> SymbolTable Word64
                -- ^ Elf symbol table
             -> Word64
                -- ^ Base offset of this section.
             -> Contents s
                -- ^ Contents of elf section we are apply this to.
             -> RelaEntry X86_64_RelocationType
                -- ^ The relocation entry.
             -> ST s ()
performReloc section_map sym_table this_vaddr mv reloc = do
  -- Offset to modify.
  let off = r_offset reloc :: Word64
    -- Get location of symbol in binary.
  let sym     = getSymbol sym_table (r_sym reloc)
  let sym_val = symbolAddr section_map sym
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
          error $ "Relocation of " ++ steName sym ++ " does not safely zero extend."
        | otherwise ->
          write32_lsb mv off res32
      where res64 = sym_val + fromIntegral addend :: Word64
            res32 = fromIntegral res64 :: Word32
    R_X86_64_32S
        | fromIntegral res32 /= res64 ->
          error $ "Relocation of " ++ steName sym ++ " does not safely zero extend."
        | otherwise ->
          write32_lsb mv off (fromIntegral res32)
      where res64 = fromIntegral sym_val + addend :: Int64
            res32 = fromIntegral res64 :: Int32
    _ -> do
      error "Relocation not supported"

byteStringFromVector :: SV.Vector Word8 -> BS.ByteString
byteStringFromVector v0 = fst $ BS.unfoldrN (SV.length v0) f v0
  where f v = Just (SV.head v, SV.tail v)

performRelocs :: SectionMap Word64
                -- ^ Maps elf section indices in object to the base virtual address
                -- in the binary.
              -> V.Vector (ElfSymbolTableEntry Word64)
                 -- ^ Elf symbol table
              -> ElfSection Word64
                 -- ^ Section that we are applying relocation to.
              -> Word64 -- ^ Base address of this section.
              -> [RelaEntry X86_64_RelocationType]
              -> ElfSection Word64
performRelocs section_offsets sym_table section this_vaddr relocs = runST $ do
  let dta = elfSectionData section
  let len = BS.length dta
  mv <- SMV.new len
  -- Copy original bytes into bytestring
  writeBS mv 0 dta
  -- Updpate using relocations
  mapM_ (performReloc section_offsets sym_table this_vaddr mv) relocs

  let SMV.MVector _ fp = mv
  let reloc_data = Data.ByteString.Internal.fromForeignPtr fp 0 len
  return $! section { elfSectionAddr = this_vaddr
                    , elfSectionData = reloc_data
                    }

hasSectionName :: ElfSection w -> String -> Bool
s `hasSectionName` nm = elfSectionName s == nm


tryFindSectionIndex :: V.Vector (ElfSection w) -> String -> [Int]
tryFindSectionIndex sections nm =
  V.toList (V.findIndices (`hasSectionName` nm) sections)

findSectionIndex :: Monad m => V.Vector (ElfSection w) -> String -> m (Maybe Int)
findSectionIndex sections nm =
  case tryFindSectionIndex sections nm of
    [i] -> return $! Just i
    []  -> return Nothing
    _   -> fail $ "Multiple " ++ nm ++ " sections in object file."

-- | Find relocation entries in section with given name.
findRelaEntries :: Monad m
                => Elf Word64
                   -- ^ Object with relocations
                -> String
                   -- ^ Name of section containing relocation entries.
                -> m [RelaEntry X86_64_RelocationType]
findRelaEntries obj nm = do
  case nm `findSectionByName` obj of
    -- Assume that no section means no relocations
    [] -> return []
    [s] ->
      return $! elfRelaEntries (elfData obj) (elfSectionData s)
    _ -> fail$  "Multple " ++ show nm ++ " sections in object file."

-- | Find relocation entries in section with given name.
findSymbolTable :: Monad m
                => Elf Word64
                   -- ^ Object with relocations
                -> String
                   -- ^ Name of section containing relocation entries.
                -> m (V.Vector (ElfSymbolTableEntry Word64))
findSymbolTable obj nm = do
  case nm `findSectionByName` obj of
    -- Assume that no section means no relocations
    [] -> fail $ "Could not find symbol table."
    [s] ->
      return $! V.fromList $ getSymbolTableEntries obj s
    _ -> fail $ "Multple " ++ show nm ++ " sections in object file."

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


-- | Index of program header in an elf file.
type PhdrIndex = Word16

-- | Offset of entry in symbol table.
type SymbolTableIndex = Word32

-- | Maps address in
data CodeRedirection w
   = CodeRedirection { redirSourcePhdr :: !PhdrIndex
                     , redirSourceOffset :: !w
                       -- ^ Offset in phdr where we should write file.
                     , redirTarget :: !SymbolTableIndex
                     }

data ResolvedRedirs w = CR { crMkJump :: !(w -> BS.ByteString)
                             -- ^ Create a jump instruction to the given address.
                           , crSections :: !(SectionMap w)
                             -- ^ Maps elf section indices in object to the base virtual address
                             -- in the binary.
                           , crSymbols :: !(SymbolTable w)
                             -- ^ Elf symbol table for object
                           , crPhdrBase :: !PhdrIndex
                             -- ^ Offset to add to phds.
                           , crEntries :: !(Map PhdrIndex [CodeRedirection w])
                             -- ^ Get the list of redirections to apply.
                           }

mapFromList :: Ord k => (a -> k) -> [a] -> Map k [a]
mapFromList proj = foldr' ins Map.empty
  where ins e = Map.insertWith (++) (proj e) [e]


writeBS :: SV.MVector s Word8 -> Int -> BS.ByteString -> ST s ()
writeBS mv base bs = do
  let len = BS.length bs
  when (SMV.length mv < base + len) $ do
    fail $ "Bytestring overflows buffer."
  forM_ [0..len-1] $ \i -> do
    SMV.write mv (base+i) (bs `BS.index` i)

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
  let section_map = crSections redirs
  let sym_table = crSymbols redirs
  forM_ redir_list $ \entry -> do
    let off = redirSourceOffset entry
    let tgt = symbolAddr section_map $ getSymbol sym_table $ redirTarget entry
    when (base <= off && off < base + fromIntegral len) $ do
      writeBS mv (fromIntegral (off - base)) (crMkJump redirs tgt)

  let SMV.MVector _ fp = mv
  return $! Data.ByteString.Internal.fromForeignPtr fp 0 len



rawSegmentFromBuilder :: (Bits w, Integral w)
                      => ElfLayout w
                      -> ResolvedRedirs w
                      -> [CodeRedirection w]
                         -- ^ Redirections for this segment
                      -> w -- ^ Offset in segment for this data
                      -> BS.ByteString
                      -> [ElfDataRegion w]
                      -> Either String (w, [ElfDataRegion w])
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
                   -> Either String (w, [ElfDataRegion w])
mapLoadableSection orig_layout redirs entries off sec rest = do
  let bs = elfSectionData sec
  let off' = off + fromIntegral (BS.length bs)
  let sec' = sec { elfSectionName = ".orig" ++ elfSectionName sec
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
                       -> Either String (w, [ElfDataRegion w])
mapOrigLoadableRegions _ _ _ off [] =
  return (off, [])
mapOrigLoadableRegions orig_layout redirs entries off (reg:rest) =
  case reg of
    ElfDataElfHeader -> do
      let b = BSL.toStrict $ Bld.toLazyByteString $ buildElfHeader orig_layout
      rawSegmentFromBuilder orig_layout redirs entries off b rest
    ElfDataSegmentHeaders -> do
      let b = BSL.toStrict $ Bld.toLazyByteString $
                buildElfSegmentHeaderTable orig_layout
      rawSegmentFromBuilder orig_layout redirs entries off b rest
    -- Flatten special segments
    ElfDataSegment seg  ->
      case elfSegmentType seg of
        -- Copy TLS segment
        PT_TLS -> do
          let subseg = toList (elfSegmentData seg)
          (off2, subseg1) <- mapOrigLoadableRegions orig_layout redirs entries off subseg
          let tls_seg = seg { elfSegmentIndex = crPhdrBase redirs + elfSegmentIndex seg
                            , elfSegmentData  = Seq.fromList subseg1
                            }

          (off3, rest1) <- mapOrigLoadableRegions orig_layout redirs entries off2 rest
          return $! (off3, ElfDataSegment tls_seg : rest1)

        _ -> do
          let subseg = toList (elfSegmentData seg)
          mapOrigLoadableRegions orig_layout redirs entries off (subseg ++ rest)

    ElfDataSectionHeaders ->
      Left "Did not expect section headers in loadable region"
    ElfDataSectionNameTable ->
      Left "Did not expect section name table in loadable region"
    ElfDataGOT g ->
      mapLoadableSection orig_layout redirs entries off (elfGotSection g) rest
    ElfDataSection s -> do
      mapLoadableSection orig_layout redirs entries off s rest
    ElfDataRaw b ->
      rawSegmentFromBuilder orig_layout redirs entries off b rest

data OriginalBinaryInfo w = OBI { _obiRegions :: !(Seq (ElfDataRegion w))
                                }

obiRegions :: Simple Lens (OriginalBinaryInfo w) (Seq (ElfDataRegion w))
obiRegions = lens _obiRegions (\s v -> s { _obiRegions = v })

initOriginalBinaryInfo :: OriginalBinaryInfo w
initOriginalBinaryInfo =
  OBI { _obiRegions = Seq.empty
      }

didNotExpectOriginalRegion :: String -> Either String a
didNotExpectOriginalRegion region_name =
  Left $ "Did not expect " ++ region_name ++ " in original binary."

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


hasBits :: Bits x => x -> x -> Bool
x `hasBits` b = (x .&. b) == b

elfHasTLSSegment :: Elf w -> Bool
elfHasTLSSegment e =
  case filter (`segmentHasType` PT_TLS) (elfSegments e) of
    [] -> False
    [_] -> True
    _ -> error "Multiple TLS segments in original binary"

elfHasTLSSection :: (Num w, Bits w) => Elf w -> Bool
elfHasTLSSection e =
  any (\s -> elfSectionFlags s `hasBits` shf_tls) (e^..elfSections)

copyOriginalBinaryRegion :: forall w
                          . (Bits w, Integral w)
                         => ElfLayout w
                         -> ResolvedRedirs w
                            -- ^ Redirections in code
                         -> w -- ^ File offset of region
                         -> ElfDataRegion w
                         -> OriginalBinaryInfo w
                         -> Either String (OriginalBinaryInfo w)
copyOriginalBinaryRegion orig_layout redirs file_offset reg info =
  case reg of
    -- Drop elf data header.
    ElfDataElfHeader -> return info
    -- Drop segment headers
    ElfDataSegmentHeaders -> return info
    ElfDataSegment seg ->
      case elfSegmentType seg of
        PT_LOAD -> do
          let sub_reg = toList (elfSegmentData seg)
          let idx = elfSegmentIndex seg
          let entries = fromMaybe [] $! Map.lookup idx (crEntries redirs)
          (_,sub_reg') <- mapOrigLoadableRegions orig_layout redirs entries 0 sub_reg
          let seg' = seg { elfSegmentIndex = crPhdrBase redirs + elfSegmentIndex seg
                         , elfSegmentData = Seq.fromList sub_reg'
                         }
          -- Compute amount of padding to get alignment correct.
          let padding :: w
              padding | a <= 1 = 0
                      | act_align <= req_align = req_align - act_align
                        -- Need to insert padding to wrap around.
                      | otherwise = (a - act_align) + req_align
                where a  = elfSegmentAlign seg
                      mask = a  - 1
                      req_align = elfSegmentVirtAddr seg .&. mask
                      act_align = file_offset .&. mask

          let new_segs = dataPadding padding ++ [ ElfDataSegment seg' ]
          return $! info & obiRegions %~ (Seq.>< Seq.fromList new_segs)
        -- Drop non-loaded segments
        _ -> return info
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
                           . (Bits w, Integral w)
                          => Elf w
                          -> ResolvedRedirs w
                          -> w  -- ^ Offset of file.
                          -> Either String (OriginalBinaryInfo w)
copyOriginalBinaryRegions orig_binary redirs base_offset = do
  let orig_layout = elfLayout orig_binary
  let f :: (w, OriginalBinaryInfo w)
        -> ElfDataRegion w
        -> Either String (w, OriginalBinaryInfo w)
      f (o,info) reg = do
        info' <- copyOriginalBinaryRegion orig_layout redirs o reg info
        let o' = o + elfRegionFileSize orig_layout reg
        return (o', info')
  snd <$> foldlM f (base_offset, initOriginalBinaryInfo) (orig_binary^.elfFileData)

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

-- | Make padding region if number of bytes is non-zero.
dataPadding :: Integral w => w -> [ElfDataRegion w]
dataPadding 0 = []
dataPadding z = [ ElfDataRaw (BS.replicate (fromIntegral z) 0) ]

createRegionsForSection :: Monad m
                        => Elf Word64
                        -> SectionMap Word64
                        -> V.Vector (ElfSymbolTableEntry Word64)
                        -> ElfSectionFlags Word64
                           -- ^ Expected flags
                        -> Maybe (ElfSection Word64)
                           -- ^ Section if we need to.
                        -> String
                           -- ^ Name of relocation section.
                        -> Word64
                           -- ^ Base address of segment
                        -> Word64
                           -- ^ Offset in section
                        -> m [ElfDataRegion Word64]
createRegionsForSection _   _       _    _     Nothing _ _ _ = return []
createRegionsForSection obj sec_map syms flags (Just sec) rela_name base_seg_addr prev_size = do
  when (elfSectionType sec /= SHT_PROGBITS) $ do
    fail $ elfSectionName sec ++ " section has unexpected type."
  when (not (elfSectionFlags sec `hasBits` flags)) $ do
    fail $ elfSectionName sec ++ " section has unexpected permissions."
  -- Find text relocations section
  relocs <- findRelaEntries obj rela_name
  -- Perform relocations
  let off = prev_size `fixAlignment` elfSectionAddrAlign sec

  let addr = base_seg_addr + off
  let reloc_sec = performRelocs sec_map syms sec addr relocs
  -- Get padding to add between end of header and start of code section.
  let padding = dataPadding $ off - prev_size
  return $! padding ++ [ ElfDataSection reloc_sec ]

elfSectionFileSize :: Integral w => ElfSection w -> w
elfSectionFileSize = fromIntegral . BS.length . elfSectionData


mergeObject :: Elf Word64 -- ^ Existing binary
            -> ElfHeaderInfo Word64 -- ^ Information about object file to insert
            -> [CodeRedirection Word64] -- ^ Redirection
            -> (Word64 -> BS.ByteString)
               -- ^ Function for creating jump to given offset.
            -> Merger (Elf Word64)
mergeObject orig_binary obj_header redirs mkJump = do
  let elf_class = ELFCLASS64

  -- Check original binary properties
  checkOriginalBinaryAssumptions orig_binary

  let obj = getElf obj_header
  checkObjAssumptions obj (elfOSABI orig_binary)

  let sections = getSectionTable obj_header

  -- Find address for new code.
  let elf_align = elfAlignment orig_binary

  -- Flag indicating whether to add GNU stack segment.
  let add_gnu_stack = elfHasGNUStackSegment orig_binary
                   && elfHasGNUStackSection obj

  -- Flag indicating whether to add TLS segment
  let add_tls = elfHasTLSSegment orig_binary

  when (elfHasTLSSection obj) $ do
    fail $ "TLS section is not allowed in new code object."

  let gnu_stack_index = 2

  let new_phdr_count :: Word16
      new_phdr_count = 2 -- One for executable and one for data.
                    + (if add_gnu_stack then 1 else 0)

  let phdr_count = fromIntegral new_phdr_count
                 + (if add_tls       then 1 else 0)
                 + loadableSegmentCount orig_binary


  let exec_seg_header_size :: Word64
      exec_seg_header_size = fromIntegral (ehdrSize elf_class)
                           + fromIntegral phdr_count * fromIntegral (phdrEntrySize elf_class)

  -- Find text section
  mcode_section_index <- findSectionIndex sections ".text"

  let (text_sec_offset, post_code_file_offset)  =
        case mcode_section_index of
          Nothing -> (exec_seg_header_size, exec_seg_header_size)
          Just idx -> (off, off + sz)
            where s   = sections V.! idx
                  off = exec_seg_header_size `fixAlignment` elfSectionAddrAlign s
                  sz  = elfSectionFileSize s


  let new_code_seg_size = post_code_file_offset
              -- Get size of just new code section

  new_code_seg_addr <- findNewAddress new_code_seg_size elf_align 0

  -- Get address of new code section
  let code_section_map_entries =
        case mcode_section_index of
          Nothing -> []
          Just idx ->
            [ (,) (fromIntegral idx) (new_code_seg_addr + text_sec_offset)
            ]


  -- Get size of first loadable program header.

  let gnu_stack_segment_headers
          | add_gnu_stack = [ ElfDataSegment (gnuStackSegment gnu_stack_index) ]
          | otherwise = []

  -- Compute offset for new code and ensure it is aligned.

  let new_data_file_offset = new_code_seg_size `fixAlignment` elf_align

  -- Get size of new code section

  mdata_section_index <- findSectionIndex sections ".data"

  -- Get size of data segment post ".data" size
  let post_data_sec_size =
        case mdata_section_index of
          Nothing -> 0
          Just i -> elfSectionFileSize (sections V.! i)

  mbss_section_index  <- findSectionIndex sections ".bss"

  -- Compute size of data segment by factoring in alignment issues on ".bss"
  let new_data_seg_file_size =
        case mbss_section_index of
          Nothing -> post_data_sec_size
          Just i -> post_data_sec_size `fixAlignment` elfSectionAddrAlign
            where s = (sections V.! i)


  new_data_seg_addr <- findNewAddress new_data_seg_file_size elf_align new_data_file_offset


  let data_map_entries =
        case mdata_section_index of
          Nothing -> []
          Just idx -> [ (,) (fromIntegral idx) 0 ]

  let bss_map_entries =
        case mbss_section_index of
          Nothing -> []
          Just idx -> [ (,) (fromIntegral idx) new_data_seg_file_size ]

  -- Map sections to be mapped to
  let section_map = Map.fromList $
        code_section_map_entries
        ++ data_map_entries
        ++ bss_map_entries

  symbols     <- findSymbolTable obj ".symtab"

  new_code_regions <- do
    let msec = (sections V.!) <$> mcode_section_index
    let flags = shf_alloc .|. shf_execinstr
    let seg_base = new_code_seg_addr
    let prev_size = exec_seg_header_size
    createRegionsForSection obj section_map symbols flags msec ".rela.text" seg_base prev_size

  let exec_seg = ElfSegment
        { elfSegmentType     = PT_LOAD
        , elfSegmentFlags    = pf_r .|. pf_x
        , elfSegmentIndex    = 0
        , elfSegmentVirtAddr = new_code_seg_addr
        , elfSegmentPhysAddr = new_code_seg_addr
        , elfSegmentAlign    = elf_align
        , elfSegmentMemSize  = ElfRelativeSize 0
        , elfSegmentData     = Seq.fromList $
            gnu_stack_segment_headers
            ++ [ ElfDataElfHeader
               , ElfDataSegmentHeaders ]
            ++ new_code_regions -- Added to ensure code is aligned.
        }

  new_data_regions <- do
    let msec = (sections V.!) <$> mcode_section_index
    let flags = shf_alloc .|. shf_write
    let seg_base = new_data_seg_addr
    let prev_size = 0
    createRegionsForSection obj section_map symbols flags msec ".rela.data" seg_base prev_size

  let (new_bss_regions, new_bss_size) =
        case mbss_section_index of
          Nothing -> ([], 0)
          Just i -> (reg, elfSectionSize s)
            where s   = (sections V.! i)
                  s'  = s { elfSectionAddr = new_data_seg_addr + new_data_seg_file_size }
                  reg = dataPadding (new_data_seg_file_size - post_data_sec_size)
                        ++ [ ElfDataSection s'
                           ]

  let load_seg = ElfSegment
        { elfSegmentType     = PT_LOAD
        , elfSegmentFlags    = pf_x .|. pf_w
        , elfSegmentIndex    = 1
        , elfSegmentVirtAddr = new_data_seg_addr
        , elfSegmentPhysAddr = new_data_seg_addr
        , elfSegmentAlign    = elf_align
        , elfSegmentMemSize  = ElfRelativeSize new_bss_size
        , elfSegmentData     = Seq.fromList $
           new_data_regions
           ++ new_bss_regions
        }



  let new_regions_end = new_data_file_offset + new_data_seg_file_size

  let resolved_redirs =
        CR { crMkJump   = mkJump
           , crSections = section_map
           , crSymbols  = symbols
           , crPhdrBase = new_phdr_count
           , crEntries  = mapFromList redirSourcePhdr redirs
           }

  orig_binary_info <-
    case copyOriginalBinaryRegions orig_binary resolved_redirs new_regions_end of
      Left msg -> fail msg
      Right obi -> return obi


  let orig_binary_regions = toList $ orig_binary_info^.obiRegions

  -- Find space for data.
  -- Create new section and segment for data as needed.
  -- Extend bss if needed.
  -- Redirect binary to start execution in new_code.
  -- Find space for eh_frame
  return $! Elf { elfData       = ELFDATA2LSB
                , elfClass      = elf_class
                , elfOSABI      = elfOSABI orig_binary
                , elfABIVersion = 0
                , elfType       = ET_EXEC
                , elfMachine    = EM_X86_64
                , elfEntry      = elfEntry orig_binary
                , elfFlags      = 0
                , _elfFileData  = Seq.fromList $
                   [ ElfDataSegment exec_seg
                   , ElfDataSegment load_seg
                   ]
                   ++ orig_binary_regions
                   ++ [ ElfDataSectionNameTable
                      , ElfDataSectionHeaders
                      ]
                , elfRelroRange = Nothing
                }

main :: IO ()
main = do
  let exe_file = "original-tree"
  let obj_file = "tree.o"
  let output_file = "merged-tree"
  binary <- readElf64 parseElf exe_file

  obj_header <- readElf64 parseElfHeaderInfo obj_file


  new_binary <- runMerger binary $ do
    mergeObject binary obj_header undefined undefined

  BSL.writeFile output_file $ renderElf new_binary
  -- Write out new binary.
  return ()
