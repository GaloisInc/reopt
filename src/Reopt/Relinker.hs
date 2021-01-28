{-|
This module performs the merging between the binary and new object.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Reopt.Relinker
  ( module Relations
  , mergeObject
  , x86_64_immediateJump
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.ElfEdit ()
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word
import           GHC.Stack
import           System.Exit
import           System.IO
import           Text.Printf (printf)

import           Reopt.Relinker.Relations as Relations
import           Reopt.Relinker.Relocations

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

type RelinkM = IO

relinkFail :: String -> RelinkM a
relinkFail msg = do
   hPutStrLn stderr msg
   exitFailure

------------------------------------------------------------------------
-- Merger

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

------------------------------------------------------------------------
-- Binary checking

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

-- | Strict fold over a list with an index
ifoldlM' :: forall m t a b i . (Foldable t, Monad m, Num i) => (b -> i -> a -> m b) -> b -> t a -> m b
ifoldlM' f s0 l = seq s0 $ fst <$> foldlM g (s0, 0) l
  where g :: (b,i) -> a -> m (b,i)
        g (s,i) a = do
         s' <- f s i a
         let j = i+1
         seq s' $ seq j $ pure (s',j)

-- | Return a part of a bytestring after checking range returned is in bounds.
checkedSlice :: String -> Elf.FileRange Word64 -> BS.ByteString -> RelinkM BS.ByteString
checkedSlice msg (o,sz) f = do
  when (toInteger o + toInteger sz > toInteger (BS.length f)) $ do
    relinkFail msg
  pure $! BS.take (fromIntegral sz) (BS.drop (fromIntegral o) f)

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
scanBinaryPhdr s idx phdr = do
  case Elf.phdrSegmentType phdr of
    Elf.PT_LOAD -> do
      case s of
        InitBinaryScanState -> do
          let flags = Elf.phdrSegmentFlags phdr
          if flags `hasFlags` Elf.pf_x then do
            when (Elf.phdrFileSize phdr /= Elf.phdrMemSize phdr) $ do
              relinkFail $ printf "Binary code program header %s file size must match memory size." (show idx)
            pure $! SeenCodePhdr idx (Elf.phdrSegmentVirtAddr phdr + Elf.phdrMemSize phdr)
           else
            relinkFail "Expected first loaded segment to be code segment."
        SeenCodePhdr codeIdx _codeEndOff -> do
          let flags = Elf.phdrSegmentFlags phdr
          when (flags `hasFlags` Elf.pf_x) $ do
            relinkFail "Found multiple executable segments."
          let phdrFileStart = Elf.phdrFileStart phdr
          let phdrFileEnd   = phdrFileStart `Elf.incOffset` Elf.phdrFileSize phdr
          pure $! SeenDataPhdr codeIdx phdr phdrFileEnd
        SeenDataPhdr codeIdx dataFirstPhdr dataFileEnd -> do
          let flags = Elf.phdrSegmentFlags phdr
          when (flags `hasFlags` Elf.pf_x) $ do
            relinkFail "Found multiple executable segments."
          let phdrFileStart = Elf.phdrFileStart phdr
          when (dataFileEnd > phdrFileStart) $ do
            relinkFail "Found overlapping load segment."
          let phdrFileEnd   = phdrFileStart `Elf.incOffset` Elf.phdrFileSize phdr
          pure $! SeenDataPhdr codeIdx dataFirstPhdr phdrFileEnd
    _ -> do
      pure s

------------------------------------------------------------------------
-- Phase 3.  Collect Object information

-- | Section index in new binary
newtype NewSectionIndex = NewSectionIndex { fromNewSectionIndex :: Word16 }
  deriving (Eq, Num)

-- | Type synonym for addresses in new address space.
newtype NewAddr = NewAddr { fromNewAddr :: Word64 }

incNewAddr :: Integral a => NewAddr -> a -> NewAddr
incNewAddr (NewAddr base) o = NewAddr (base + fromIntegral o)

-- | @newAddrOff base off@ returns distance from @off@ to @base@
newAddrOff :: NewAddr -> NewAddr -> Word64
newAddrOff (NewAddr base) (NewAddr off)
  | off < base = error "Invalid offset"
  | otherwise = off - base

-- | Contents for collecting
data ObjInfoContext
  = ObjInfoContext { objctxHeader :: !(Elf.ElfHeader 64)
                   -- ^ Header for object file
                   , objctxContents :: !BS.ByteString
                   -- ^ Contents of object file.
                   , objctxShdrs :: !(V.Vector (Elf.Shdr BS.ByteString Word64))
                     -- ^ Object section headers
                   , objctxInfoOfObjDefinedFun :: !(BS.ByteString -> Maybe ObjFunDef)
                     -- ^ Maps function names in the object code to
                     -- the number of bytes available in the binary to
                     -- replace so that we can determine if we can
                     -- overwrite the address.
                   , objctxOverflowStartAddr :: !NewAddr
                     -- ^ Starting address of spill section.
                   , objctxOverflowCodeShdrIndex :: !Word16
                     -- ^ Index of section header for spill section.
                   , objctxShdrFromBinAddr :: !(Word64 -> Maybe (Word16, Word64))
                     -- ^ Maps code addresses in original binary to section index and offset
                     -- it it belongs to a section.
                   }

-- | Information extracted from pass over section headers in object file.
data ObjInfo =
  ObjInfo { objinfoSymtab :: !ObjectSectionIndex
            -- ^ Index of symbol table (or 0 if not found).
          , objinfoSymbolAddrMap :: !(Map BS.ByteString (NewSectionIndex, Word64))
            -- ^ Map symbol names in object file to new section index
            -- and offset within new section.
          , objinfoSectionAddrMap :: !(Map ObjectSectionIndex NewAddr)
            -- ^ Maps allocated object sections to their address they are
            -- loaded at in new binary.
          , objinfoBinMap :: !(Map Word64 ObjectSectionIndex)
            -- ^ Map from addresses of functions to be replaced in
            --  binary to the section index.
          , objinfoOverflowEndAddr :: !NewAddr
            -- ^ End address for code that spilled to new section.
          , objinfoOverflowCodeRev :: [ObjectSectionIndex]
            -- ^ Overflow code in reverse order of how it should be
            -- stored in program.
          }

-- | This is part of the pass over object section to collect
-- information.
collectObjShdrInfo :: ObjInfoContext
                      -- ^ Context
                   -> ObjInfo
                      -- ^ Object references
                   -> ObjectSectionIndex
                      -- ^ This section header index
                   -> Elf.Shdr BS.ByteString Word64
                      -- ^ Section header to process
                   -> RelinkM ObjInfo
collectObjShdrInfo ctx objInfo thisIdx shdr = do
  let flags = Elf.shdrFlags shdr
  case Elf.shdrType shdr of
    -- Skip initial null section.
    Elf.SHT_NULL -> do
      pure objInfo
    Elf.SHT_REL -> relinkFail "REL entries not supported."
    Elf.SHT_RELA -> do
      pure objInfo
    Elf.SHT_SYMTAB -> do
      when (objinfoSymtab objInfo /= 0) $ do
        relinkFail $ "Duplicate object file symbol tables."
      -- Record symbol table index.
      pure $! objInfo { objinfoSymtab = thisIdx }
    Elf.SHT_PROGBITS
      | flags `hasFlags` Elf.shf_alloc -> do
          when (flags `hasFlags` Elf.shf_write) $ do
            relinkFail "Do not support writable sections."
          unless (flags `hasFlags` Elf.shf_execinstr) $ do
            relinkFail $ printf "%s Unsupported: Must be executable." (BSC.unpack (Elf.shdrName shdr))
          when (flags `hasFlags` Elf.shf_merge) $ do
            relinkFail "Do not support mergable sections."
          when (flags `hasFlags` Elf.shf_tls) $ do
            relinkFail "Do not support TLS in object files."

          case Elf.shdrName shdr of
            shdrName
              | Just fnName <- BS.stripPrefix ".text." shdrName -> do
                  finfo <-
                    case objctxInfoOfObjDefinedFun ctx fnName of
                      Nothing -> relinkFail $ "Unexpected object function: " <> BSC.unpack fnName
                      Just i -> pure i
                  -- If this section header fits in original binary,
                  -- then we replace it.
                  if ofdBinSize finfo <= Elf.shdrSize shdr then do
                    -- Get address
                    let addr = ofdBinAddr finfo
                    newSymbolAddrMap <- do
                      let symbolAddrMap = objinfoSymbolAddrMap objInfo
                      case objctxShdrFromBinAddr ctx addr of
                        Nothing -> pure symbolAddrMap
                        Just (secIdx, secOff) -> do
                          pure $ Map.insert fnName (NewSectionIndex secIdx, secOff) symbolAddrMap
                    let newSectionAddrMap =
                          Map.insert thisIdx (NewAddr addr) (objinfoSectionAddrMap objInfo)
                    let newBinMap :: Map Word64 ObjectSectionIndex
                        newBinMap = Map.insert addr thisIdx (objinfoBinMap objInfo)
                    pure $! objInfo { objinfoSymbolAddrMap = newSymbolAddrMap
                                    , objinfoSectionAddrMap = newSectionAddrMap
                                    , objinfoBinMap = newBinMap
                                    }
                   else do
                    -- Otherwise we append it to end.
                    let secIdx = NewSectionIndex (objctxOverflowCodeShdrIndex ctx)
                    let addr = objinfoOverflowEndAddr objInfo
                    let secOff = newAddrOff (objctxOverflowStartAddr ctx) addr
                    let newSymbolAddrMap =
                          Map.insert fnName (secIdx, secOff) (objinfoSymbolAddrMap objInfo)
                    let newSectionAddrMap =
                          Map.insert thisIdx addr (objinfoSectionAddrMap objInfo)
                    let newEnd = incNewAddr (objinfoOverflowEndAddr objInfo) (Elf.shdrSize shdr)
                    pure $! objInfo
                      { objinfoSymbolAddrMap = newSymbolAddrMap
                      , objinfoSectionAddrMap = newSectionAddrMap
                      , objinfoOverflowCodeRev = thisIdx : objinfoOverflowCodeRev objInfo
                      , objinfoOverflowEndAddr = newEnd
                      }
            ".text" -> do
              when (Elf.shdrSize shdr /= 0) $ do
                relinkFail "Expect object file compiled with -ffunction-sections."
              pure objInfo
            shdrName -> do
              relinkFail $ "Unknown allocated section: " <> BSC.unpack shdrName
    -- Skip other sections
    _ -> pure $! objInfo

{-

TODO:
1. Recreate .gnu.hash"
2. Recreate .note.gnu-build-id"

0x0000000000000004 (HASH)               0x400308
0x000000006ffffef5 (GNU_HASH)           0x400320

-}

-- | Information collected during first pass over binary program header information.
data BinaryPhdrLayout
   = BinaryPhdrLayout { bplBinDataAddr :: !Word64
                        -- ^ Address at start of data segment.
                      , bplNewDataEndOffset :: !(Elf.FileOffset Word64)
                        -- ^ end offset for data
                      , bplNewDataContent :: !Bld.Builder
                      , bplBinCodePhdrIndex :: !Word16
                        -- ^ Index of program header table.
                      }

-- | Get bytes from a section header file.
getShdrContents :: Elf.Shdr nm Word64 -> Elf.ElfHeaderInfo 64 -> BS.ByteString
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
          padding = Bld.byteString $ BS.replicate (fromIntegral (o' - objCodeEndOffset)) 0
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
                             -> Elf.SymtabEntry BS.ByteString v
                             -> Elf.SymtabEntry Word32 v
finalizeSymtabEntryNameIndex strtabOffsetMap e =
  case Map.lookup (Elf.steName e) strtabOffsetMap of
    Nothing -> error $ "internal failure: Unexpected symbol."
    Just idx -> e { Elf.steName = idx }


-- | Replace section header name index with bytestring.
substituteShdrName :: BS.ByteString -> Elf.Shdr Word32 Word64 -> IO (Elf.Shdr BS.ByteString Word64)
substituteShdrName shstrtab shdr =
  case Elf.lookupString (Elf.shdrName shdr) shstrtab of
    Left _ -> relinkFail "Failed to find section header name."
    Right nm -> pure $ shdr { Elf.shdrName = nm }

-- | Get section header table.
getShdrTable :: Elf.ElfHeaderInfo 64 -> IO (V.Vector (Elf.Shdr BS.ByteString Word64))
getShdrTable binHeaderInfo = do
  -- Index of section header entry for section name table.
  let shstrtabShdrIndex :: Word16
      shstrtabShdrIndex = Elf.shstrtabIndex binHeaderInfo

  when (shstrtabShdrIndex == 0) $ do
    relinkFail "Require non-zero shstrtab index."
  -- Sections in binary
  let rawBinShdrs :: V.Vector (Elf.Shdr Word32 Word64)
      rawBinShdrs = Elf.headerShdrs binHeaderInfo
  when (fromIntegral shstrtabShdrIndex >= V.length rawBinShdrs) $ do
    relinkFail "Invalid binary section header table"
  let binShstrtab :: BS.ByteString
      binShstrtab = getShdrContents (rawBinShdrs V.! fromIntegral shstrtabShdrIndex) binHeaderInfo
  traverse (substituteShdrName binShstrtab) rawBinShdrs

$(pure [])

newCodeSectionName :: BS.ByteString
newCodeSectionName = ".text.reopt"

$(pure [])

--------------------------------------------------------------------------------
-- BinarySectionLayout

-- | Information needed to compute code layout.
data BinaryLayoutContext = BinaryLayoutContext { blcBinCodeEndOff :: !(Elf.FileOffset Word64)
                                                 -- ^ End offset of code section.
                                               }

$(pure [])

-- | Section information for new binary.
data NewSectionInfo
   = OldBinaryCodeSection !(Elf.Shdr BS.ByteString Word64)
   | NewBinaryCodeSection
   | OldBinaryDataSection !(Elf.Shdr BS.ByteString Word64)
   | NewSymtabSection     !(Elf.Shdr BS.ByteString Word64)
   | NewStrtabSection     !(Elf.Shdr BS.ByteString Word64)
   | NewShstrtabSection   !(Elf.Shdr BS.ByteString Word64)

$(pure [])

type AddrToSectionMap = Map Word64 (Word64, Word16)

-- | Information extracted from scan of sections in binary.
data BinarySectionLayout =
  BSL { bslSectionCount :: !Word16
        -- ^ Number of sections added
      , bslSectionNames :: ![BS.ByteString]
        -- ^ Names of setions
      , bslSectionHeaders :: [NewSectionInfo]
        --- ^ List of sections in reverse order.
      , bslOverflowCodeShdrIndex :: !Word16
        -- ^ Number of section headers in binary prior to overflow
        -- section.
      , bslSymtabIndex :: !Word16
        -- ^ Index of symbol table (or zero if not found)
      , bslStrtabIndex :: !Word16
        -- ^ Index of string table (or zero if not found)
      , bslAllocatedAddrMap :: !AddrToSectionMap
        -- ^ Maps address of section headers to size and index.
      }

  -- Convert from section index in binary to new section index.
mapBinSectionIndex :: BinarySectionLayout -> Word16 -> NewSectionIndex
mapBinSectionIndex bsl i
  -- If i is less than new code index then use it
  | i < bslOverflowCodeShdrIndex bsl = NewSectionIndex i
  | otherwise = NewSectionIndex (i+1)

addBinShdr :: BinarySectionLayout
           -> BS.ByteString
           -> NewSectionInfo
           -> BinarySectionLayout
addBinShdr bsl nm secInfo = seq nm $ seq secInfo $
  bsl { bslSectionCount = bslSectionCount bsl  + 1
      , bslSectionNames = nm : bslSectionNames bsl
      , bslSectionHeaders = secInfo:bslSectionHeaders bsl
      }

addOverflowCodeShdr :: BinarySectionLayout -> BinarySectionLayout
addOverflowCodeShdr bsl =
  bsl { bslSectionCount    = bslSectionCount bsl + 1
      , bslSectionNames    = newCodeSectionName : bslSectionNames bsl
      , bslSectionHeaders  = NewBinaryCodeSection : bslSectionHeaders bsl
      , bslOverflowCodeShdrIndex = bslSectionCount bsl
      }

-- | This is used in a fold over the section headers to infer layout
-- information.
layoutBinaryShdr :: BinaryLayoutContext
                 -> BinarySectionLayout
                 -> Word16 -- ^ Index of section header in binary
                 -> Elf.Shdr BS.ByteString Word64
                 -> IO BinarySectionLayout
layoutBinaryShdr nci bsl0 idx shdr = seq idx $ do
  let bsl1 | bslOverflowCodeShdrIndex bsl0 == 0
           , Elf.shdrOff shdr > blcBinCodeEndOff nci =
               addOverflowCodeShdr bsl0
           | otherwise = bsl0
  if Elf.shdrName shdr == ".symtab" then do
    let bsl2 = bsl1 { bslSymtabIndex = idx }
    pure $ addBinShdr bsl2 (Elf.shdrName shdr) $ NewSymtabSection shdr
   else if Elf.shdrName shdr == ".strtab" then do
    let bsl2 = bsl1 { bslStrtabIndex = idx }
    pure $ addBinShdr bsl2 (Elf.shdrName shdr) $ NewStrtabSection shdr
   else if Elf.shdrName shdr == ".shstrtab" then
    pure $ addBinShdr bsl1 (Elf.shdrName shdr) $ NewShstrtabSection shdr
   else if Elf.shdrOff shdr > blcBinCodeEndOff nci then
    pure $ addBinShdr bsl1 (Elf.shdrName shdr) $ OldBinaryDataSection shdr
   else do
    let bsl2
          | (Elf.shdrFlags shdr .&. Elf.shf_alloc) == Elf.shf_alloc =
              let addr = Elf.shdrAddr shdr
                  sz   = Elf.shdrSize shdr
                  m = bslAllocatedAddrMap bsl1
               in bsl1 { bslAllocatedAddrMap = Map.insert addr (sz, idx) m  }
          | otherwise =
              bsl1
    pure $ addBinShdr bsl2 (Elf.shdrName shdr) $ OldBinaryCodeSection shdr

mkBinarySectionLayout :: BinaryLayoutContext
                         -- ^ Context information
                      -> V.Vector (Elf.Shdr BS.ByteString Word64)
                         -- ^ Section header in original binary
                      -> IO BinarySectionLayout
mkBinarySectionLayout ctx binShdrs = do
  let bsl0 = BSL { bslSectionCount = 0
                 , bslSectionNames   = []
                 , bslSectionHeaders = []
                 , bslOverflowCodeShdrIndex = 0
                 , bslSymtabIndex = 0
                 , bslStrtabIndex = 0
                 , bslAllocatedAddrMap = Map.empty
                 }
  bsl <- ifoldlM' (layoutBinaryShdr ctx) bsl0 binShdrs
  when (V.length binShdrs == 0) $ do
    relinkFail "Do not support binaries missing section headers."
  when (bslOverflowCodeShdrIndex bsl == 0) $ do
    relinkFail "Unsupport binary; expected additional section headers after code."
  pure bsl

$(pure [])

--------------------------------------------------------------------------------
-- Perform relocations in code from object file.

performObjRelocs :: Elf.ElfData -- ^ Encoding
                 -> V.Vector (Elf.Shdr BS.ByteString Word64)
                    -- ^ Object file section headers
                 -> BS.ByteString
                    -- ^ Contents of object file
                 -> RelocInfo Word64
                    -- ^ Relocation info for object file.
                 -> NewAddr
                    -- ^ Address of section
                 -> Word16
                    -- ^ Index of section to perform relocations for.
                 -> IO BS.ByteString
performObjRelocs elfDta objShdrs contents objRelocInfo (NewAddr addr) secIdx = do
  let shdrCount :: Word32
      shdrCount = fromIntegral (V.length objShdrs)
  when (secIdx >= fromIntegral shdrCount) $ do
    error "Internal error: invalid shdr index"
  let shdr = objShdrs V.! fromIntegral secIdx
  let nm :: String
      nm = BSC.unpack (Elf.shdrName shdr)
  code <- checkedSlice (nm <> " section") (Elf.shdrFileRange shdr) contents
  let relaShdrIndex = Elf.shdrLink shdr
  when (relaShdrIndex >= shdrCount) $ do
    relinkFail "Invalid relocation index"
  let relaShdr = objShdrs V.! fromIntegral relaShdrIndex
  relaBytes <- checkedSlice (nm <> " relocations") (Elf.shdrFileRange relaShdr) contents
  relas <-
    case Elf.decodeRelaEntries elfDta relaBytes of
      Left msg -> relinkFail msg
      Right r -> pure r
  case performRelocs objRelocInfo addr code relas of
    Left msg -> relinkFail msg
    Right bytes -> pure bytes

$(pure [])

-- | Encoded program headers
mkNewPhdrTable :: Elf.ElfHeaderInfo 64 -- ^ Existing binary
                   -> BinaryPhdrLayout -- ^ Information about program headers
                   -> Word64 -- ^ Total size of overflow
                   -> Bld.Builder
mkNewPhdrTable binHeaderInfo binPhdrInfo overflowTotalSize = gen 0
  where hdr = Elf.header binHeaderInfo
        cl = Elf.headerClass hdr
        elfDta = Elf.headerData hdr

        mkPhdr :: Word16 -> Elf.Phdr 64
        mkPhdr idx = do
          let phdr = Elf.phdrByIndex binHeaderInfo idx
          -- Extend code phdr to include new data
          if idx == bplBinCodePhdrIndex binPhdrInfo then
            phdr { Elf.phdrFileSize = Elf.phdrFileSize phdr + overflowTotalSize
                 , Elf.phdrMemSize  = Elf.phdrMemSize  phdr + overflowTotalSize
                 }
            -- Adjust segments in data or later by delta
           else if Elf.phdrSegmentVirtAddr phdr >= bplBinDataAddr binPhdrInfo then
            phdr { Elf.phdrFileStart = Elf.incOffset (Elf.phdrFileStart phdr) overflowTotalSize
                 }
            -- Copy other program headers.
           else
            phdr

        gen i | i >= Elf.phdrCount binHeaderInfo = mempty
              | otherwise = Elf.encodePhdr cl elfDta (mkPhdr i) <> gen (i+1)

$(pure [])

--------------------------------------------------------------------------------
-- New Section header generation

-- | Information needed to render new section headers.
data NewShdrContext =
  NewShdrContext
  { nscBinShdrCount :: !Word16
  , nscBinSectionIndexMap :: !(Word16 -> NewSectionIndex)
    -- ^ Map section header indices in original binary to indices in
    -- new binary.
  , nscSectionNameMap :: !(BSC.ByteString -> Word32)
    -- ^ Maps section names to their offset.
  , nscOverflowOffset :: !(Elf.FileOffset Word64)
    -- ^ Starting file offset of new code.
  , nscOverflowAddr :: !NewAddr
    -- ^ Address of new code section (relative to base in dynamically linked files)
  , nscOverflowSize    :: !Word64
    -- ^ Code size
  , nscDataDelta :: !Word64    -- ^ Amount to increment data
  , nscSymtabOffset  :: !(Elf.FileOffset Word64)
  , nscSymtabSize :: !Word64
  , nscSymtabLocalCount :: !Word32
  , nscStrtabOffset  :: !(Elf.FileOffset Word64)
  , nscStrtabSize :: !Word64
  , nscShstabOffset  :: !(Elf.FileOffset Word64)
  , nscShstrtabSize :: !Word64
  }

-- | Function that maps old section header link to new section header link.
nscMapShdrLink :: NewShdrContext -> Word32 -> Word32
nscMapShdrLink nsc link =
  -- If link is a current section index, then we treat it as a section index and
  -- remap it.
  if 1 <= link && link < fromIntegral (nscBinShdrCount nsc) then
    let link' = fromIntegral link
     in fromIntegral $ fromNewSectionIndex $ nscBinSectionIndexMap nsc link'
   else
    link

mkNewShdr :: NewShdrContext
          -> NewSectionInfo
          -> Elf.Shdr BS.ByteString Word64
mkNewShdr nsc nsi =
  case nsi of
    OldBinaryCodeSection shdr ->
      let newInfo | Elf.shdrFlags shdr `hasFlags` Elf.shf_info_link =
                    nscMapShdrLink nsc (Elf.shdrInfo shdr)
                  | otherwise = Elf.shdrInfo shdr
       in shdr { Elf.shdrLink = nscMapShdrLink nsc (Elf.shdrLink shdr)
               , Elf.shdrInfo = newInfo
               }
    NewBinaryCodeSection ->
      Elf.Shdr
      { Elf.shdrName  = newCodeSectionName
      , Elf.shdrType  = Elf.SHT_PROGBITS
      , Elf.shdrFlags = Elf.shf_alloc .|. Elf.shf_execinstr
      , Elf.shdrAddr  = fromNewAddr (nscOverflowAddr nsc)
      , Elf.shdrOff   = nscOverflowOffset nsc
      , Elf.shdrSize  = nscOverflowSize nsc
      , Elf.shdrLink  = 0
      , Elf.shdrInfo  = 0
      , Elf.shdrAddrAlign = 16
      , Elf.shdrEntSize = 0
      }
    OldBinaryDataSection shdr ->
      shdr { Elf.shdrLink = nscMapShdrLink nsc (Elf.shdrLink shdr)
           , Elf.shdrOff = Elf.shdrOff shdr `Elf.incOffset` nscDataDelta nsc
           }
    NewSymtabSection shdr ->
      shdr { Elf.shdrOff  = nscSymtabOffset nsc
           , Elf.shdrSize = nscSymtabSize nsc
           , Elf.shdrLink = nscMapShdrLink nsc (Elf.shdrLink shdr)
           , Elf.shdrInfo = nscSymtabLocalCount nsc
           }
    NewStrtabSection shdr ->
      shdr { Elf.shdrOff  = nscStrtabOffset nsc
           , Elf.shdrSize = nscStrtabSize nsc
           }
    NewShstrtabSection shdr ->
      shdr { Elf.shdrOff  = nscShstabOffset nsc
           , Elf.shdrSize = nscShstrtabSize nsc
           }

mkNewShdrTable :: Elf.ElfHeader 64
               -> NewShdrContext
               -> [NewSectionInfo]
               -> Bld.Builder
mkNewShdrTable hdr nsc shdrs =
  let cl = Elf.headerClass hdr
      elfDta = Elf.headerData hdr
      renderShdr :: NewSectionInfo -> Bld.Builder
      renderShdr nsi =
        let shdr = mkNewShdr nsc nsi
            shdr' = shdr { Elf.shdrName = nscSectionNameMap nsc (Elf.shdrName shdr) }
        in Elf.encodeShdr cl elfDta shdr'
   in foldMap renderShdr shdrs


$(pure [])

-- | Information from constructing new symbol table.
data NewSymtabInfo = NewSymtabInfo { nsiStrtabContents :: !BSC.ByteString
                                   , nsiSymtabSize :: !Word64
                                   , nsiSymtabLocalCount :: !Word32
                                   , nsiSymtabContents :: !Bld.Builder
                                   }

mapFromFuns :: (Foldable f, Ord k) => (a -> k) -> (a -> v) -> f a -> Map k v
mapFromFuns kf vf l =
  let insAddr m f = Map.insert (kf f) (vf f) m
   in foldl' insAddr Map.empty l


-- | Create symbol table in new file.
mkNewSymtab :: MergeRelations
            -> Elf.ElfHeaderInfo 64
            -> BinarySectionLayout
            -> V.Vector (Elf.Shdr BS.ByteString Word64)
            -- ^ Section headers in original binary
            -> V.Vector (Elf.SymtabEntry BS.ByteString Word64)
               -- ^ Symbols in object file
            -> (BS.ByteString -> Maybe (NewSectionIndex, Word64))
               -- ^ Maps symbol names in object file to index and
               -- offset where they are stored.
            -> IO NewSymtabInfo
mkNewSymtab ctx binHeaderInfo binShdrInfo binShdrs objSymbols addrOfObjSymbol = do
  let hdr = Elf.header binHeaderInfo
      cl = Elf.headerClass hdr
      elfDta = Elf.headerData hdr

  -- Section header for symbol table in binary
  binSymtabShdr <-
    if bslSymtabIndex binShdrInfo == 0 then
      relinkFail "Could not find symbol table in binary."
     else
      pure $ binShdrs  V.! fromIntegral (bslSymtabIndex binShdrInfo)

  let binLocalSymCount :: Word32
      binLocalSymCount = Elf.shdrInfo binSymtabShdr

  let binSymtab :: BS.ByteString
      binSymtab = getShdrContents binSymtabShdr binHeaderInfo

  let binStrtabIdx :: Int
      binStrtabIdx = fromIntegral (Elf.shdrLink binSymtabShdr)
  when (binStrtabIdx >= V.length binShdrs) $ do
    relinkFail "Invalid binary string table index."

  let binStrtab :: BS.ByteString
      binStrtab = getShdrContents (binShdrs V.! binStrtabIdx) binHeaderInfo

  let addrObjectNameMap :: Map Word64 BS.ByteString
      addrObjectNameMap = mapFromFuns ofdBinAddr ofdObjName (mrObjectFuns ctx)

  -- Map symbol in the original binary to a new address.
  let mapBinSymbol :: Elf.SymtabEntry BS.ByteString Word64
                   -- ^ Symbol entry in the original binary
                   -> State (Set BS.ByteString) (Elf.SymtabEntry BS.ByteString Word64)
      mapBinSymbol e = do
        -- Look to see if global symbols have been moved.
        moff <-
          case Map.lookup (Elf.steValue e) addrObjectNameMap of
            Just nm -> do
              -- Record symbol already has been recorded so we do
              modify $ Set.insert nm
              pure $ addrOfObjSymbol nm
            Nothing -> do
              pure Nothing
        case moff of
          Nothing -> do
            let binIdx = Elf.fromElfSectionIndex (Elf.steIndex e)
                NewSectionIndex newIdx = mapBinSectionIndex binShdrInfo binIdx
            pure $! e { Elf.steIndex = Elf.ElfSectionIndex newIdx }
          Just (NewSectionIndex midx, off) ->
            pure $! e { Elf.steIndex = Elf.ElfSectionIndex midx
                      , Elf.steValue = off
                      }

  binSymbols <-
    case Elf.decodeSymtab cl elfDta binStrtab binSymtab of
      Left _e -> relinkFail "Could not parse binary symbol table."
      Right syms -> pure $ syms
  -- This maps
  let ((newLocalSyms, newGlobalSyms), usedOverflowSymbolSet) = flip runState Set.empty $ do
        let (localSyms, globalSyms) = V.splitAt (fromIntegral binLocalSymCount) binSymbols
        l <- V.mapM mapBinSymbol localSyms
        g <- V.mapM mapBinSymbol globalSyms
        pure (l,g)

  -- Get symbols from object file.
  -- Note these are considered local
  newObjSyms <- do

    -- Process object symbol at given index.
    let processObjSymbol :: Int
                         -> IO (Maybe (Elf.SymtabEntry BS.ByteString Word64, Int))
        processObjSymbol i
            | i >= V.length objSymbols = pure Nothing
            | e <- objSymbols V.! i
            , Set.notMember (Elf.steName e) usedOverflowSymbolSet
            , Elf.steType e == Elf.STT_FUNC
            , Elf.steBind e == Elf.STB_GLOBAL = do
                let nm = Elf.steName e
                (NewSectionIndex newIdx, newVal) <-
                  case addrOfObjSymbol nm of
                    Just p -> pure p
                    Nothing -> relinkFail $ "Could not find symbol " <> BSC.unpack nm
                when (Elf.steValue e /= 0) $ do
                  relinkFail $ "Expected symbol value to be zero."
                let e' = e { Elf.steBind   = Elf.STB_LOCAL
                           , Elf.steIndex  = Elf.ElfSectionIndex newIdx
                           , Elf.steValue  = newVal
                           }
                pure (Just (e', i+1))
            | otherwise = do
                processObjSymbol (i+1)
    V.unfoldrM processObjSymbol 0

  -- Get number of local symbols (used in section header table)
  let newLocalSymCount :: Word32
      newLocalSymCount = fromIntegral $ V.length newLocalSyms + V.length newObjSyms

  -- Symbols
  let newSymbols ::  V.Vector (Elf.SymtabEntry BS.ByteString Word64)
      newSymbols = newLocalSyms <> newObjSyms <> newGlobalSyms

  -- Get end offset of symbol table.
  let newSymtabSize :: Word64
      newSymtabSize =
        let cnt :: Word64
            cnt = fromIntegral (V.length newSymbols)
            sz :: Word64
            sz  = fromIntegral (Elf.symtabEntrySize cl)
         in (cnt * sz)

  -- Get symbol string table
  let newStrtabContents :: BS.ByteString
      strtabOffsetMap :: Map BS.ByteString Word32
      (newStrtabContents, strtabOffsetMap) = Elf.encodeStringTable $
        V.toList $ Elf.steName <$> newSymbols

  let newSymtabEntries :: V.Vector (Elf.SymtabEntry Word32 Word64)
      newSymtabEntries = finalizeSymtabEntryNameIndex strtabOffsetMap <$> newSymbols

  pure $! NewSymtabInfo { nsiStrtabContents = newStrtabContents
                        , nsiSymtabSize = newSymtabSize
                        , nsiSymtabLocalCount = newLocalSymCount
                        , nsiSymtabContents = foldMap (Elf.encodeSymtabEntry cl elfDta) newSymtabEntries
                        }


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
            -> MergeRelations
            -- ^ Mapping information for relating binary and object.
            -> IO BSL.ByteString
mergeObject binHeaderInfo objHeaderInfo ctx = do

  let binHdr = Elf.header binHeaderInfo
  let objHdr = Elf.header objHeaderInfo

  -- Step 1. Header validation
  let cl     = Elf.ELFCLASS64
  let elfDta = Elf.ELFDATA2LSB

  -- Check this is an executable
  case Elf.headerType binHdr of
    Elf.ET_EXEC -> pure ()
    Elf.ET_DYN -> pure ()
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

  -- Code program header in binary
  binCodePhdrIndex <- getCodeIndex binPhdrScanResults
  let binCodePhdr :: Elf.Phdr 64
      binCodePhdr = Elf.phdrByIndex binHeaderInfo binCodePhdrIndex

  -- Analyze layout of section headers in new binary by iterating over
  -- section headers in input binary.

  binShdrs <- getShdrTable binHeaderInfo
  binShdrIndexInfo <- do
    let codeEndOff = Elf.incOffset (Elf.phdrFileStart binCodePhdr) (Elf.phdrFileSize binCodePhdr)
    let lctx = BinaryLayoutContext { blcBinCodeEndOff = codeEndOff }
    mkBinarySectionLayout lctx binShdrs

  -- Step 2a. Get values referencing start of spill code.

  -- Get ehdr Size
  let ehdrSize :: Word64
      ehdrSize = fromIntegral (Elf.ehdrSize cl)

  when (Elf.phdrFileStart binCodePhdr /= 0) $ do
    relinkFail $ "Expected binary code program header to start at front of file."

  when (Elf.phdrTableFileOffset binHeaderInfo /= fromIntegral (Elf.ehdrSize cl)) $ do
    relinkFail "Expected program header at start of file."

  -- End of code section
  let binCodeEndOffset :: Elf.FileOffset Word64
      binCodeEndOffset =
        Elf.incOffset (Elf.phdrFileStart binCodePhdr)
                      (Elf.phdrFileSize binCodePhdr)

  -- Start of overflow section offset.
  let overflowOffset :: Elf.FileOffset Word64
      overflowOffset = Elf.alignFileOffset 16 binCodeEndOffset

  -- Number of bytes in padding between end of old code and overflow code section.
  let overflowPadding :: Word64
      overflowPadding = Elf.fromFileOffset overflowOffset - Elf.fromFileOffset binCodeEndOffset

  -- Address of start of overflow section.
  -- This is the of the binary code segment rounded up to nearest multiple of 16.
  let overflowAddr :: NewAddr
      overflowAddr = NewAddr $
        Elf.phdrSegmentVirtAddr binCodePhdr
        + Elf.phdrMemSize binCodePhdr
        + overflowPadding

  -- Get section headers for objects
  objShdrs <-
    case Elf.headerNamedShdrs objHeaderInfo of
      Left (idx, nmErr) -> do
        relinkFail $ "Bad section name at index " <> show idx <> ":\n  " <> show nmErr
      Right objShdrs ->
        pure objShdrs

  -- Phase 3. Collect object section header information.
  --
  -- As part of this step, we check to see if function to be inserted that
  -- replaces an existing function can fit inside the space of the function
  -- to be replaced.  If so, we mark that function has replacing the existing
  -- one.  If not, we add the function to the set of functions that we need
  -- to find space for.

  let newCodeIndexFromAddr :: Word64 -> Maybe (Word16, Word64)
      newCodeIndexFromAddr addr = do
        (base, (sz, idx)) <- Map.lookupLE addr (bslAllocatedAddrMap binShdrIndexInfo)
        when (addr - base >= sz) Nothing
        pure (idx, addr - base)

  let objectNameInfoMap :: Map BS.ByteString ObjFunDef
      objectNameInfoMap = mapFromFuns ofdObjName id (mrObjectFuns ctx)

  objInfo <- do
    -- Create information needed to parse object section headers
    let octx =
          ObjInfoContext
          { objctxHeader = objHdr
          , objctxContents = Elf.headerFileContents objHeaderInfo
          , objctxShdrs = objShdrs
          , objctxInfoOfObjDefinedFun = \nm -> Map.lookup nm objectNameInfoMap
          , objctxOverflowStartAddr = overflowAddr
          , objctxOverflowCodeShdrIndex = bslOverflowCodeShdrIndex binShdrIndexInfo
          , objctxShdrFromBinAddr = newCodeIndexFromAddr
          }
     -- Create initial object information.
    let initObjInfo = ObjInfo { objinfoSymtab = 0
                              , objinfoSymbolAddrMap = Map.empty
                              , objinfoSectionAddrMap = Map.empty
                              , objinfoBinMap = Map.empty
                              , objinfoOverflowEndAddr = overflowAddr
                              , objinfoOverflowCodeRev = []
                              }
    -- Process sections headers
    ifoldlM' (collectObjShdrInfo octx) initObjInfo objShdrs

  -- Step 5. Copy functions in object file that cannot overwrite previous
  -- definition into new code section at end of this section.
  -- When doing this, generate jumps that need to be added.

  -- Number of bytes in overflow section
  let overflowSize :: Word64
      overflowSize = newAddrOff overflowAddr (objinfoOverflowEndAddr objInfo)

  -- Size of object code plus any padding between end of old code and start of object code.
  let overflowTotalSize :: Word64
      overflowTotalSize = overflowPadding + overflowSize

  -- End of overflow
  let overflowEndOffset :: Elf.FileOffset Word64
      overflowEndOffset = Elf.incOffset overflowOffset overflowSize

  -- Get layout for binary data now that we know new code added.
  binPhdrLayout <-
    mkBinaryPhdrLayout binHeaderInfo overflowEndOffset binPhdrScanResults

  -- Get offst of symbol table
  let newSymtabOffset :: Elf.FileOffset Word64
      newSymtabOffset = fixAlignment (bplNewDataEndOffset binPhdrLayout) (Elf.symtabAlign cl)

  -- Section header for symbol table in object file
  (objSymtabShdr :: Elf.Shdr BS.ByteString Word64) <- do
    let idx = objinfoSymtab objInfo
    when (idx == 0) $
      relinkFail "Could not find object file symbol table."
    pure $ objShdrs V.! fromIntegral idx

  let objSymtab :: BS.ByteString
      objSymtab = getShdrContents objSymtabShdr objHeaderInfo

  -- Get object string table.
  let objStrtabIdx :: Int
      objStrtabIdx = fromIntegral (Elf.shdrLink objSymtabShdr)
  when (objStrtabIdx >= V.length objShdrs) $ do
    relinkFail "Invalid binary string table index."
  let objStrtab :: BS.ByteString
      objStrtab = getShdrContents (objShdrs V.! objStrtabIdx) objHeaderInfo

  -- Map symbol names in object file to new section index and offset
  -- within new section.
  let addrOfObjSymbol :: BS.ByteString -> Maybe (NewSectionIndex, Word64)
      addrOfObjSymbol nm = Map.lookup nm (objinfoSymbolAddrMap objInfo)

  objSymbols <-
    case Elf.decodeSymtab cl elfDta objStrtab objSymtab of
      Left _e -> relinkFail "Could not parse object file symbol table."
      Right syms -> pure $ syms

  newSymtab <- mkNewSymtab ctx binHeaderInfo binShdrIndexInfo binShdrs objSymbols addrOfObjSymbol

  -- Get section header stringtable size
  let newStrtabSize :: Word64
      newStrtabSize = fromIntegral (BS.length (nsiStrtabContents newSymtab))

  -- Get end offset of symbol table.
  let newStrtabOffset :: Elf.FileOffset Word64
      newStrtabOffset = Elf.incOffset newSymtabOffset (nsiSymtabSize newSymtab)

  -- Get string table end offset
  let newShstrtabOffset :: Elf.FileOffset Word64
      newShstrtabOffset = Elf.incOffset newStrtabOffset newStrtabSize

  -- .shstrtab contents
  let newShstrtabContents :: BS.ByteString
      shstrtabOffsetMap :: Map BS.ByteString Word32
      (newShstrtabContents, shstrtabOffsetMap) =
        Elf.encodeStringTable $ bslSectionNames binShdrIndexInfo

  -- Get section header stringtable size
  let newShstrtabSize :: Word64
      newShstrtabSize = fromIntegral (BS.length newShstrtabContents)

  -- Section header string table offset.
  let newShstrtabEndOffset :: Elf.FileOffset Word64
      newShstrtabEndOffset = Elf.incOffset newShstrtabOffset newShstrtabSize

  -- Get offset of section header
  let newShdrTableOffset :: Elf.FileOffset Word64
      newShdrTableOffset =
        Elf.alignFileOffset (Elf.shdrTableAlign cl) newShstrtabEndOffset

  -- Note. This has already checked all offsets are valid.
  let objBinCodeMap :: Map Word64 ObjectSectionIndex
      objBinCodeMap = objinfoBinMap objInfo

  let undefAddrMap =
        let ins m r = Map.insert (ofrObjName r) (ofrBinAddr r) m
         in foldl' ins Map.empty (mrUndefinedFuns ctx)

  -- Compute information needed to resolve relocations in object file.
  let objRelocInfo :: RelocInfo Word64
      objRelocInfo =
        let addrOfObjSec :: ObjectSectionIndex -> Maybe Word64
            addrOfObjSec idx = fromNewAddr <$> Map.lookup idx (objinfoSectionAddrMap objInfo)
         in mkRelocInfo (`Map.lookup` undefAddrMap) addrOfObjSec objSymbols

  -- Generate code in the overflow segment.
  objOverflowCode <- do
    let insObjFun :: ObjectSectionIndex
                  -> (NewAddr -> Bld.Builder -> IO Bld.Builder)
                     -- ^ Continuation that takes next address and
                     -- object code built so far.
                  -> NewAddr -- ^ Current address
                  -> Bld.Builder -- ^ Bytestring built so far.
                  -> IO Bld.Builder
        insObjFun secIdx cont addr prev = do
          let objContents = Elf.headerFileContents objHeaderInfo
          bytes <- performObjRelocs elfDta objShdrs objContents objRelocInfo addr secIdx
          let newAddr = incNewAddr addr (BS.length bytes)
          cont newAddr (prev <> Bld.byteString bytes)
    let overCode = reverse (objinfoOverflowCodeRev objInfo)
    foldr insObjFun (\_ p -> pure p) overCode overflowAddr mempty

  -- Contents of program header after elf header and program header table
  -- with overflow contents added
  newBinCodeContent <- do
    let baseCodeAddr = Elf.phdrSegmentVirtAddr binCodePhdr
    let (phdrOff, phdrSize) = Elf.phdrTableRange objHeaderInfo
    -- Contents of code segment in original binary.
    let contents = Elf.headerFileContents binHeaderInfo
    let codeFileRange = Elf.phdrFileRange binCodePhdr
    origBinCodeContents <- checkedSlice "Invalid code segment." codeFileRange contents
    let off = fromIntegral phdrOff + fromIntegral phdrSize
    -- Insert function for inserting object code
    let insObjBin :: Word64 -- ^ Address of next object
                  -> ObjectSectionIndex
                  -> (Word64 -> Bld.Builder -> IO a)
                  -> Word64
                  -> Bld.Builder
                  -> IO a
        insObjBin addr secIdx cont endAddr prev = do
          let endOff = fromIntegral $ endAddr - baseCodeAddr
          let objOff = fromIntegral $ addr - baseCodeAddr
          -- Get bytes in binary in front of object code.
          let binCopy = Bld.byteString (BS.drop endOff (BS.take objOff origBinCodeContents))
          -- Get relocation code
          let objContents = Elf.headerFileContents objHeaderInfo
          let objAddr = incNewAddr overflowAddr objOff
          bytes <-
            performObjRelocs elfDta objShdrs objContents objRelocInfo objAddr secIdx
          -- Go to next
          let nextAddr = addr + fromIntegral (BS.length bytes)
          cont nextAddr (prev <> binCopy <> Bld.byteString bytes)
    let finalObjBin :: Word64 -> Bld.Builder -> IO Bld.Builder
        finalObjBin addr prev = do
          let codeOff = fromIntegral $ addr - baseCodeAddr
          let r = Bld.byteString (BS.drop codeOff origBinCodeContents)
          pure $ prev <> r <> objOverflowCode
    Map.foldrWithKey insObjBin finalObjBin objBinCodeMap off mempty


  -- .shstrtab section index.
  newShstrtabIndex <- do
    let idx = Elf.shstrtabIndex binHeaderInfo
    when (idx >= Elf.shdrCount binHeaderInfo) $ do
      relinkFail "Could not find .shstrtab index."
    pure $! mapBinSectionIndex binShdrIndexInfo idx

  -- Elf header.
  let ehdr = Elf.Ehdr { Elf.ehdrHeader   = binHdr
                      , Elf.ehdrPhoff    = Elf.FileOffset ehdrSize
                      , Elf.ehdrShoff    = newShdrTableOffset
                      , Elf.ehdrPhnum    = binPhdrCount
                      , Elf.ehdrShnum    = Elf.shdrCount binHeaderInfo + 1
                      , Elf.ehdrShstrndx = fromNewSectionIndex newShstrtabIndex
                      }

  -- Section Header table
  let newShdrCtx :: NewShdrContext
      newShdrCtx =
        NewShdrContext
        { nscBinShdrCount     = Elf.shdrCount binHeaderInfo
        , nscBinSectionIndexMap = mapBinSectionIndex binShdrIndexInfo
        , nscSectionNameMap = \nm ->
            let msg = "internal failure: Missing section header name."
             in Map.findWithDefault (error msg) nm shstrtabOffsetMap
        , nscOverflowOffset   = overflowOffset
        , nscOverflowSize     = overflowSize
        , nscOverflowAddr     = overflowAddr
        , nscDataDelta        = overflowTotalSize
        , nscSymtabOffset     = newSymtabOffset
        , nscSymtabSize       = nsiSymtabSize newSymtab
        , nscSymtabLocalCount = nsiSymtabLocalCount newSymtab
        , nscStrtabOffset     = newStrtabOffset
        , nscStrtabSize       = newStrtabSize
        , nscShstabOffset     = newShstrtabOffset
        , nscShstrtabSize     = newShstrtabSize
        }
  let newShdrs = reverse (bslSectionHeaders binShdrIndexInfo)

  -- Create final elf image.
  return $ Bld.toLazyByteString $
    -- Main elf header
    Elf.encodeEhdr ehdr
    -- Program header table
    <> mkNewPhdrTable binHeaderInfo binPhdrLayout overflowTotalSize
    -- Existing code with overflow added.
    <> newBinCodeContent
    -- Padding and data content
    <> bplNewDataContent binPhdrLayout
    -- Symbol table
    <> nsiSymtabContents newSymtab
    -- string table
    <> Bld.byteString (nsiStrtabContents newSymtab)
    -- Section header name table
    <> Bld.byteString newShstrtabContents
    -- section header table padding
    <> Bld.byteString (BS.replicate (fromIntegral (newShdrTableOffset - newShstrtabEndOffset)) 0)
    -- Section header table
    <> mkNewShdrTable binHdr newShdrCtx newShdrs
