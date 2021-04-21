{-|
This module performs the relinking step that merges between the binary
and new object.
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
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import           Data.Word
import           GHC.Stack ( HasCallStack )
import           Numeric (showHex)
import           Text.Printf (printf)

import qualified Reopt.Relinker.Binary as Bin
import           Reopt.Relinker.Constants ( newCodeSectionName )
import qualified Reopt.Relinker.NewBinary as New
import qualified Reopt.Relinker.NewLayout as New
import           Reopt.Relinker.NewSymtab
import           Reopt.Relinker.Relations as Relations
import           Reopt.Relinker.Relocations
import           Reopt.Utils.Flags


------------------------------------------------------------------------
-- RelinkM and ObjRelocState

type PureRelinkM = ExceptT String (State [String])

runPureRelinkM :: PureRelinkM a -> ([String], Either String a)
runPureRelinkM m =
  let (r, warnings) = runState (runExceptT m) []
   in (reverse warnings, r)

relinkWarn :: String -> PureRelinkM ()
relinkWarn warn = modify $ (warn:)

-- | Return a part of a bytestring after checking range returned is in bounds.
pureCheckedSlice :: (MonadError String m,  Integral w)
                 => String -> Elf.FileRange w -> BS.ByteString -> m BS.ByteString
pureCheckedSlice msg (o,sz) f = do
  when (toInteger o + toInteger sz > toInteger (BS.length f)) $ do
    throwError msg
  pure $! BS.take (fromIntegral sz) (BS.drop (fromIntegral o) f)

------------------------------------------------------------------------
-- Merger

-- | Check original binary satisfies preconditions.
checkBinaryAssumptions :: Elf.ElfHeader w -> PureRelinkM  ()
checkBinaryAssumptions binaryHdr = do
  when (Elf.headerData binaryHdr /= Elf.ELFDATA2LSB) $ do
    throwError $ "Expected the original binary to be least-significant bit first."
  when (Elf.headerMachine binaryHdr /= Elf.EM_X86_64) $ do
    throwError $ "Only x86 64-bit object files are supported."
  when (Elf.headerFlags binaryHdr /= 0) $ do
    throwError $ "Expected elf flags in binary to be zero."

-- | Check object file satisfies preconditions.
checkObjectHeaderAssumptions :: Elf.ElfHeader w -> PureRelinkM ()
checkObjectHeaderAssumptions hdr = do
  when (Elf.headerData hdr /= Elf.ELFDATA2LSB) $ do
    throwError $ "Expected the new binary binary to be least-significant bit first."
  when (Elf.headerType hdr /= Elf.ET_REL) $ do
    throwError $ "Expected a relocatable file as input."
  when (Elf.headerMachine hdr /= Elf.EM_X86_64) $ do
    throwError $ "Only x86 64-bit executables are supported."
  when (Elf.headerFlags hdr /= 0) $ do
    throwError $ "Expected elf flags in new object to be zero."

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

------------------------------------------------------------------------
-- Phase 3.  Collect Object information

-- | Type synonym for addresses in new address space.
newtype NewAddr = NewAddr { _fromNewAddr :: Word64 }

instance Show NewAddr where
  show (NewAddr a) = showHex a ""

incNewAddr :: Integral a => NewAddr -> a -> NewAddr
incNewAddr (NewAddr base) o = NewAddr (base + fromIntegral o)

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
                   , objctxOverflowCodeShdrIndex :: !Word16
                     -- ^ Index of section header for code overflow section.
                   , objctxShdrFromBinAddr :: !(Word64 -> Maybe (Word16, Word64))
                     -- ^ Maps code addresses in original binary to section index and offset
                     -- it it belongs to a section.
                   }

-- | Address associated with where an object file should be overloaded.
data ObjAddr
   = BinAddr !Word64
     -- ^ Address in new bianry
   | OverflowAddr !Word64
   | RodataAddr !Word64

-- | Information extracted from pass over section headers in object file.
data ObjInfo =
  ObjInfo { objinfoSymtab :: !ObjectSectionIndex
            -- ^ Index of symbol table (or 0 if not found).
          , objinfoSymbolAddrMap :: !(Map BS.ByteString (New.NewSectionIndex, Word64))
            -- ^ Map symbol names in object file to new section index
            -- and offset within new section.
          , objSectionAddrMap :: !(Map ObjectSectionIndex ObjAddr)
            -- ^ Maps allocated object sections to their address they are
            -- loaded at in new binary.
          , replaceAddrSectionMap :: !(Map Word64 ObjectSectionIndex)
            -- ^ Map from addresses of functions to be replaced in
            -- binary to the section index of the code in the object
            -- file.
          , objOverflowCodeRev :: [ObjectSectionIndex]
            -- ^ Indices of sections with overflow code in reverse order of how it
            -- should be stored in program.
          , objOverflowSize :: !Word64
            -- ^ Overflow code section size.
          , objAdditionalRodataRev :: [ObjectSectionIndex]
            -- ^ Indices of sections with overflow code in reverse order of how it
            -- should be stored in program.
          , objRodataSize :: !Word64
            -- ^ Read only data size
          }

collectCodeSection :: ObjInfoContext
                      -- ^ Context
                   -> ObjInfo
                      -- ^ Object references
                   -> ObjectSectionIndex
                      -- ^ This section header index
                   -> Elf.Shdr BS.ByteString Word64
                      -- ^ Section header to process
                   -> BS.ByteString
                      -- ^ Name of function this represents
                   -> PureRelinkM ObjInfo
collectCodeSection ctx objInfo thisIdx shdr fnName = do
  finfo <-
    case objctxInfoOfObjDefinedFun ctx fnName of
      Nothing -> throwError $ "Unexpected object function: " <> BSC.unpack fnName
      Just i -> pure i
  -- If this section header fits in original binary,
  -- then we replace it.
  if Elf.shdrSize shdr <= ofdBinSize finfo then do
    -- Get address
    let addr = ofdBinAddr finfo
    newSymbolAddrMap <- do
      let symbolAddrMap = objinfoSymbolAddrMap objInfo
      case objctxShdrFromBinAddr ctx addr of
        Nothing -> pure symbolAddrMap
        Just (secIdx, secOff) -> do
          pure $ Map.insert fnName (New.NewSectionIndex secIdx, secOff) symbolAddrMap
    -- Check address does not overlap with previous function.
    case Map.lookupLE addr (replaceAddrSectionMap objInfo) of
      Nothing -> pure ()
      Just (prevAddr, prevIdx) -> do
        let prevCodeShdr = objctxShdrs ctx V.! fromIntegral prevIdx
        let prevEnd = prevAddr + Elf.shdrFileSize prevCodeShdr
        when (prevEnd > addr) $ do
          throwError $ printf "Function at address 0x%s extends past address 0x%s."
            (showHex prevAddr "") (showHex addr "")

    pure $!
      objInfo
      { objinfoSymbolAddrMap = newSymbolAddrMap
      , objSectionAddrMap = Map.insert thisIdx (BinAddr addr) (objSectionAddrMap objInfo)
        -- Record addr replaced by function at section thisIdx
      , replaceAddrSectionMap =
          Map.insert addr thisIdx (replaceAddrSectionMap objInfo)
      }
   else do
    -- Otherwise we append it to end.
    let secIdx = New.NewSectionIndex (objctxOverflowCodeShdrIndex ctx)
    let addr = OverflowAddr (objOverflowSize objInfo)
    let secOff =  objOverflowSize objInfo
    let newSymbolAddrMap =
          Map.insert fnName (secIdx, secOff) (objinfoSymbolAddrMap objInfo)
    pure $! objInfo
      { objinfoSymbolAddrMap = newSymbolAddrMap
      , objSectionAddrMap = Map.insert thisIdx addr (objSectionAddrMap objInfo)
      , objOverflowCodeRev = thisIdx : objOverflowCodeRev objInfo
      , objOverflowSize = objOverflowSize objInfo + Elf.shdrSize shdr
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
                   -> PureRelinkM ObjInfo
collectObjShdrInfo ctx objInfo thisIdx shdr = do
  let flags = Elf.shdrFlags shdr
  case Elf.shdrType shdr of
    -- Skip initial null section.
    Elf.SHT_NULL -> do
      pure objInfo
    Elf.SHT_REL -> do
      throwError "REL entries not supported."
    Elf.SHT_RELA -> do
      pure objInfo
    Elf.SHT_SYMTAB -> do
      when (objinfoSymtab objInfo /= 0) $ do
        throwError $ "Duplicate object file symbol tables."
      -- Record symbol table index.
      pure $! objInfo { objinfoSymtab = thisIdx }
    Elf.SHT_PROGBITS
      | flags `hasFlags` Elf.shf_alloc -> do
        when (flags `hasFlags` Elf.shf_tls) $ do
          throwError "Do not support TLS in object files."
        case Elf.shdrName shdr of
          shdrName
            | Just fnName <- BS.stripPrefix ".text." shdrName
            , flags `hasFlags` Elf.shf_execinstr
            , not (flags `hasFlags` Elf.shf_write)
            , not (flags `hasFlags` Elf.shf_merge) -> do
                collectCodeSection ctx objInfo thisIdx shdr fnName
          ".text" -> do
            when (Elf.shdrSize shdr /= 0) $ do
              throwError "Expect object file compiled with -ffunction-sections."
            pure objInfo
          ".rodata.cst16" -> do
            let off = objRodataSize objInfo
            pure objInfo
              { objSectionAddrMap = Map.insert thisIdx (RodataAddr off) (objSectionAddrMap objInfo)
              , objRodataSize = off + Elf.shdrSize shdr
              , objAdditionalRodataRev = thisIdx : objAdditionalRodataRev objInfo
              }
          shdrName -> do
            throwError $ "Unsupported allocated section: " <> BSC.unpack shdrName
    Elf.SHT_X86_64_UNWIND -> do
      relinkWarn $ printf "Dropping %s" (BSC.unpack (Elf.shdrName shdr))
      pure objInfo
    -- Skip other sections
    _ -> do
      pure objInfo

$(pure [])

-- | Replace section header name index with bytestring.
substituteShdrName :: BS.ByteString -> Elf.Shdr Word32 Word64 -> PureRelinkM (Elf.Shdr BS.ByteString Word64)
substituteShdrName shstrtab shdr =
  case Elf.lookupString (Elf.shdrName shdr) shstrtab of
    Left _ -> throwError "Failed to find section header name."
    Right nm -> pure $ shdr { Elf.shdrName = nm }

-- | Get section header table.
getShdrTable :: Elf.ElfHeaderInfo 64 -> PureRelinkM (V.Vector (Elf.Shdr BS.ByteString Word64))
getShdrTable binHeaderInfo = do
  -- Index of section header entry for section name table.
  let shstrtabShdrIndex :: Word16
      shstrtabShdrIndex = Elf.shstrtabIndex binHeaderInfo

  when (shstrtabShdrIndex == 0) $ do
    throwError "Require non-zero shstrtab index."
  -- Sections in binary
  let rawBinShdrs :: V.Vector (Elf.Shdr Word32 Word64)
      rawBinShdrs = Elf.headerShdrs binHeaderInfo
  when (fromIntegral shstrtabShdrIndex >= V.length rawBinShdrs) $ do
    throwError "Invalid binary section header table"
  let binShstrtab :: BS.ByteString
      binShstrtab = getShdrContents (rawBinShdrs V.! fromIntegral shstrtabShdrIndex) binHeaderInfo
  traverse (substituteShdrName binShstrtab) rawBinShdrs

$(pure [])

--------------------------------------------------------------------------------
-- AddrToSectionMap

-- | Map from addresses at allocated section to size and index of section.
type AddrToSectionMap = Map Word64 (Word64, Word16)

-- | Create map from section headers.
mkAddrToSectionMap  :: V.Vector (Elf.Shdr BS.ByteString Word64)
                       -- ^ Section headers in original binary
                    -> AddrToSectionMap
mkAddrToSectionMap = V.ifoldl' ins Map.empty
  where ins m idx shdr
          | (Elf.shdrFlags shdr .&. Elf.shf_alloc) == Elf.shf_alloc =
              let addr = Elf.shdrAddr shdr
                  sz   = Elf.shdrSize shdr
               in Map.insert addr (sz, fromIntegral idx) m
          | otherwise = m

$(pure [])

--------------------------------------------------------------------------------
-- BinarySectionLayout

-- | Information needed to compute code layout.
data BinaryLayoutContext = BinaryLayoutContext { blcBinCodeEndOff :: !(Elf.FileOffset Word64)
                                                 -- ^ End offset of code section.
                                               }

addBinShdr :: BinarySectionLayout
           -> BS.ByteString
           -> BinarySectionLayout
addBinShdr bsl nm = seq nm $
  bsl { bslSectionCount = bslSectionCount bsl  + 1
      , bslSectionNames = nm : bslSectionNames bsl
      }

addOverflowCodeShdr :: BinarySectionLayout -> BinarySectionLayout
addOverflowCodeShdr bsl =
  bsl { bslSectionCount    = bslSectionCount bsl + 1
      , bslSectionNames    = newCodeSectionName : bslSectionNames bsl
      , bslOverflowCodeShdrIndex = bslSectionCount bsl
      }

-- | This is used in a fold over the section headers to infer layout
-- information.
layoutBinaryShdr :: BinaryLayoutContext
                 -> BinarySectionLayout
                 -> Int -- ^ Index of section header in binary
                 -> Elf.Shdr BS.ByteString Word64
                 -> BinarySectionLayout
layoutBinaryShdr nci bsl0 idxInt shdr = do
  let idx = fromIntegral idxInt
  let bsl1 | bslOverflowCodeShdrIndex bsl0 == 0
           , Elf.shdrOff shdr > blcBinCodeEndOff nci =
               addOverflowCodeShdr bsl0
           | otherwise = bsl0
  let bsl2 | Elf.shdrName shdr == ".strtab" = bsl1 { bslStrtabIndex = idx }
           | Elf.shdrName shdr == ".symtab" = bsl1 { bslSymtabIndex = idx }
           | otherwise = bsl1
  addBinShdr bsl2 (Elf.shdrName shdr)

mkBinarySectionLayout :: BinaryLayoutContext
                         -- ^ Context information
                      -> V.Vector (Elf.Shdr BS.ByteString Word64)
                         -- ^ Section header in original binary
                      -> PureRelinkM BinarySectionLayout
mkBinarySectionLayout ctx binShdrs = do
  let bsl0 = BSL { bslSectionCount = 0
                 , bslSectionNames   = []
                 , bslOverflowCodeShdrIndex = 0
                 , bslSymtabIndex = 0
                 , bslStrtabIndex = 0
                 }
  when (V.length binShdrs == 0) $ do
    throwError "Do not support binaries missing section headers."
  let bsl = V.ifoldl' (layoutBinaryShdr ctx) bsl0 binShdrs
  when (bslOverflowCodeShdrIndex bsl == 0) $ do
    throwError "Unsupport binary; expected additional section headers after code."
  pure bsl

$(pure [])

--------------------------------------------------------------------------------
-- Perform relocations in code from object file.

-- | Apply relocations to object code.
performObjRelocs ::  Elf.ElfHeaderInfo w -- ^ Elf header
                -> V.Vector (Elf.Shdr BS.ByteString Word64)
                    -- ^ Object file section headers
                 -> RelocInfo Word64
                    -- ^ Relocation info for object file.
                 -> NewAddr
                    -- ^ Address of section
                 -> Word16
                    -- ^ Index of section to perform relocations for.
                 -> Except String BS.ByteString
performObjRelocs elfHdr objShdrs objRelocInfo (NewAddr addr) secIdx = do
  let elfDta = Elf.headerData (Elf.header elfHdr)
  let contents = Elf.headerFileContents elfHdr
  let shdrCount :: Word32
      shdrCount = fromIntegral (V.length objShdrs)
  when (secIdx >= fromIntegral shdrCount) $ do
    error "Internal error: invalid shdr index"
  let shdr = objShdrs V.! fromIntegral secIdx
  let nm :: String
      nm = BSC.unpack (Elf.shdrName shdr)
  code <- pureCheckedSlice (nm <> " section") (Elf.shdrFileRange shdr) contents
  case findRelaSection objRelocInfo secIdx of
    Nothing -> do
      pure code
    Just relaIdx -> do
      let relaShdr = objShdrs V.! fromIntegral relaIdx
      let relaName = Elf.shdrName relaShdr
      let relaData = BS.take (fromIntegral (Elf.shdrSize relaShdr))
                   $ BS.drop (fromIntegral (Elf.shdrOff  relaShdr))
                   $ contents
      relas <-
        case Elf.decodeRelaEntries elfDta relaData of
          Left msg -> throwError msg
          Right r -> pure r
      case performRelocs objRelocInfo addr code relas of
        Left (idx, msg) -> do
          throwError $ printf "Relinking error in %s (relocation index = %s):\n  %s" (BSC.unpack relaName) (show idx) msg
        Right bytes ->
          pure bytes

$(pure [])

-- | Apply relocations to sections in binary and return concatenated result.
generateOverflowSection :: Elf.ElfHeaderInfo 64
                    -- ^ Binary header info
                   -> V.Vector (Elf.Shdr BS.ByteString Word64)
                    -- ^ Object file section headers
                  -> RelocInfo Word64
                  -> NewAddr
                  -> [ObjectSectionIndex]
                  -> PureRelinkM Bld.Builder
generateOverflowSection objHeaderInfo objShdrs objRelocInfo overflowAddr overflowSections = do
  let insObjFun :: ObjectSectionIndex
                -> (NewAddr -> Bld.Builder -> PureRelinkM Bld.Builder)
                    -- ^ Continuation that takes next address and
                    -- object code built so far.
                -> NewAddr -- ^ Current address
                -> Bld.Builder -- ^ Bytestring built so far.
                -> PureRelinkM Bld.Builder
      insObjFun secIdx cont addr prev = do
        bytes <-
          case runExcept (performObjRelocs objHeaderInfo objShdrs objRelocInfo addr secIdx) of
            Left e -> throwError e
            Right r -> pure r
        let newAddr = incNewAddr addr (BS.length bytes)
        cont newAddr (prev <> Bld.byteString bytes)
  foldr insObjFun (\_ p -> pure p) overflowSections overflowAddr mempty

-- | Make the new code program header
mkBinCodeContent :: BS.ByteString -- ^ Contents of original binary
                 -> Elf.ElfHeaderInfo 64
                    -- ^ Object header info
                 -> V.Vector (Elf.Shdr BS.ByteString Word64)
                    -- ^ Object file section headers
                 -> RelocInfo Word64
                 -> ObjInfo
                    -- ^ Information inferred about how to layout object code derived from
                    -- parsing headers.
                 -> Word64 -- ^ Code address
                 -> Int -- ^ Code starting offset.
                 -> Int -- ^ Number of bytes in code.
                 -> Either String Bld.Builder
mkBinCodeContent binContents objHeaderInfo objShdrs objRelocInfo objInfo codeAddr codeOff codeSize = runExcept $ do
  let codeBytes = BS.take codeSize (BS.drop codeOff binContents)
  let codeEndAddr = codeAddr + fromIntegral codeSize
  -- Map from addresses of functions to be replaced in
  --  binary to the section index of that object
  let objBinCodeMap :: Map Word64 ObjectSectionIndex
      objBinCodeMap = Map.takeWhileAntitone (< codeEndAddr)
                    $ Map.dropWhileAntitone (< codeAddr)
                    $ replaceAddrSectionMap objInfo
  -- Infer bytes in regions
  let insObjBin :: Word64 -- ^ Address of next function in object file.
                -> ObjectSectionIndex -- ^ Section index in object file.
                -> ((Word64, BS.ByteString, Bld.Builder) -> Except String a)
                -> (Word64, BS.ByteString, Bld.Builder)
                -> Except String a
      insObjBin nextObjAddr secIdx cont (prevEndAddr, curBytes, prev) = do
        when (nextObjAddr < prevEndAddr) $ do
          throwError $
            printf "Expected next function %s after previous function ends %s."
              (showHex nextObjAddr "") (showHex prevEndAddr "")
        -- Get size of binary data to copy from last address to new one.
        let binSize = fromIntegral (nextObjAddr - prevEndAddr)
        -- Get bytes in binary in front of object code.
        let binCopy = Bld.byteString (BS.take binSize curBytes)
        -- Get relocation code
        bytes <- performObjRelocs objHeaderInfo objShdrs objRelocInfo (NewAddr nextObjAddr) secIdx
        -- Go to next
        let thisEndAddr = nextObjAddr + fromIntegral (BS.length bytes)
        let nextBytes = BS.drop (fromIntegral (thisEndAddr - prevEndAddr)) curBytes
        cont (thisEndAddr, nextBytes, prev <> binCopy <> Bld.byteString bytes)
  let finish (_, remaining, prev) = pure $ prev <> Bld.byteString remaining
  Map.foldrWithKey insObjBin finish objBinCodeMap (codeAddr, codeBytes, mempty)

getObjectSymbols :: Integral (Elf.ElfWordType  w)
                 => Elf.ElfHeaderInfo w -- ^ Object file to merge into existing binary.
                 -> V.Vector (Elf.Shdr nm (Elf.ElfWordType w))
                 -> ObjectSectionIndex
                 -> PureRelinkM (V.Vector (Elf.SymtabEntry BS.ByteString (Elf.ElfWordType w)))
getObjectSymbols objHeaderInfo objShdrs objSymtabIndex = do
  let hdr = Elf.header objHeaderInfo
  let cl = Elf.headerClass hdr
  let elfDta = Elf.headerData hdr
  -- Section header for symbol table in object file
  objSymtabShdr <- do
    when (objSymtabIndex == 0) $
      throwError "Could not find object file symbol table."
    pure $ objShdrs V.! fromIntegral objSymtabIndex

  let objSymtab :: BS.ByteString
      objSymtab = getShdrContents objSymtabShdr objHeaderInfo


  -- Get object string table.
  let objStrtabIdx :: Int
      objStrtabIdx = fromIntegral (Elf.shdrLink objSymtabShdr)
  when (objStrtabIdx >= V.length objShdrs) $ do
    throwError "Invalid binary string table index."

  let objStrtab :: BS.ByteString
      objStrtab = getShdrContents (objShdrs V.! objStrtabIdx) objHeaderInfo

  case Elf.decodeSymtab cl elfDta objStrtab objSymtab of
    Left _e -> throwError "Could not parse object file symbol table."
    Right syms -> pure $ syms

-- | This merges an existing elf binary and new object file to create a
-- combined binary.  The object file is expected to be passed with function
-- sections.
--
-- The algorithm for merging runs in multiple phases:
-- 1. Collect binary information.
-- 2. Collect object information.
-- 3. Compute file layout
-- 4. Generate contents
mergeObject :: HasCallStack
            => Elf.ElfHeaderInfo 64 -- ^ Existing binary
            -> Elf.ElfHeaderInfo 64 -- ^ Object file to merge into existing binary.
            -> MergeRelations
            -- ^ Mapping information for relating binary and object.
            -> ([String], Either String BSL.ByteString)
mergeObject binHeaderInfo objHeaderInfo ctx = runPureRelinkM $ do
  -----------------------------------------------------------------------------
  -- 1. Collect binary information.

  let binHdr = Elf.header binHeaderInfo

  -- Header validation
  let cl     = Elf.ELFCLASS64

  -- Check this is an executable
  case Elf.headerType binHdr of
    Elf.ET_EXEC -> pure ()
    Elf.ET_DYN -> pure ()
    _ -> throwError $ "Expected the original binary is an executable."
  -- Check original binary properties
  checkBinaryAssumptions binHdr

  -- Check OSABI compat
  --when (Elf.headerOSABI objHdr /= Elf.headerOSABI binHdr) $ do
  --  hPutStrLn stderr $ printf "Unexpected object ABI of %s (Expected %s)."

  binShdrs <- getShdrTable binHeaderInfo

  binLayout <-
    case Bin.inferBinaryLayout binHeaderInfo binShdrs of
      Left msg -> throwError msg
      Right r -> pure r

  -- Code program header in binary
  let binCodePhdrIndex = Bin.eclCodePhdrIndex binLayout

  let binPhdrs :: V.Vector (Elf.Phdr 64)
      binPhdrs =
        V.generate (fromIntegral (Elf.phdrCount binHeaderInfo))
                   (Elf.phdrByIndex  binHeaderInfo . fromIntegral)

  let binCodePhdr :: Elf.Phdr 64
      binCodePhdr = binPhdrs V.! fromIntegral binCodePhdrIndex

  binShdrIndexInfo <- do
    let codeEndOff = Elf.incOffset (Elf.phdrFileStart binCodePhdr) (Elf.phdrFileSize binCodePhdr)
    let lctx = BinaryLayoutContext { blcBinCodeEndOff = codeEndOff }
    mkBinarySectionLayout lctx binShdrs

  -- End of code section
  let binCodeEndOffset :: Elf.FileOffset Word64
      binCodeEndOffset =
        Elf.incOffset (Elf.phdrFileStart binCodePhdr)
                      (Elf.phdrFileSize binCodePhdr)

  -----------------------------------------------------------------------------
  -- 2. Collect object information.
  --
  -- As part of this step, we check to see if function to be inserted that
  -- replaces an existing function can fit inside the space of the function
  -- to be replaced.  If so, we mark that function has replacing the existing
  -- one.  If not, we add the function to the set of functions that we need
  -- to find space for.

  let objHdr = Elf.header objHeaderInfo
  -- Check object assumptions
  checkObjectHeaderAssumptions objHdr

  let addrToSec = mkAddrToSectionMap binShdrs

  let newCodeIndexFromAddr :: Word64 -> Maybe (Word16, Word64)
      newCodeIndexFromAddr addr = do
        (base, (sz, idx)) <- Map.lookupLE addr addrToSec
        when (addr - base >= sz) Nothing
        pure (idx, addr - base)

  let objectNameInfoMap :: Map BS.ByteString ObjFunDef
      objectNameInfoMap = mapFromFuns ofdObjName id (mrObjectFuns ctx)

  -- Get section headers for objects
  objShdrs <-
    case Elf.headerNamedShdrs objHeaderInfo of
      Left (idx, nmErr) -> do
        throwError $ "Bad section name at index " <> show idx <> ":\n  " <> show nmErr
      Right objShdrs ->
        pure objShdrs

  -- Determine which
  objInfo <- do
    -- Create information needed to parse object section headers
    let octx =
          ObjInfoContext
          { objctxHeader = objHdr
          , objctxContents = Elf.headerFileContents objHeaderInfo
          , objctxShdrs = objShdrs
          , objctxInfoOfObjDefinedFun = \nm -> Map.lookup nm objectNameInfoMap
          , objctxOverflowCodeShdrIndex = bslOverflowCodeShdrIndex binShdrIndexInfo
          , objctxShdrFromBinAddr = newCodeIndexFromAddr
          }
     -- Create initial object information.
    let initObjInfo = ObjInfo { objinfoSymtab = 0
                              , objinfoSymbolAddrMap = Map.empty
                              , objSectionAddrMap = Map.empty
                              , replaceAddrSectionMap = Map.empty
                              , objOverflowSize = 0
                              , objOverflowCodeRev = []
                              , objRodataSize   = 0
                              , objAdditionalRodataRev = []
                              }
    -- Process sections headers
    ifoldlM' (collectObjShdrInfo octx) initObjInfo objShdrs

  objSymbols <- getObjectSymbols objHeaderInfo objShdrs (objinfoSymtab objInfo)


  newSymtab <-
    if bslSymtabIndex binShdrIndexInfo == 0 then
      pure Nothing
     else do
      -- Map symbol names in object file to new section index and offset
      -- within new section.
      let addrOfObjSymbol :: BS.ByteString -> Maybe (New.NewSectionIndex, Word64)
          addrOfObjSymbol nm = Map.lookup nm (objinfoSymbolAddrMap objInfo)
      case runExcept $ mkNewSymtab ctx binHeaderInfo binShdrIndexInfo binShdrs objSymbols addrOfObjSymbol of
        Left e -> throwError e
        Right r -> pure (Just r)

  -- Number of bytes in overflow section
  let overflowSize :: Word64
      overflowSize = objOverflowSize objInfo

  -----------------------------------------------------------------------------
  -- 3. Compute file layout

  -- Start of overflow section offset.
  let overflowOffset :: Elf.FileOffset Word64
      overflowOffset = Elf.alignFileOffset 16 binCodeEndOffset
  let overflowEndOffset = Elf.fromFileOffset overflowOffset + overflowSize

  -- Number of bytes in padding between end of old code and overflow code section.
  let overflowPadding :: Word64
      overflowPadding = Elf.fromFileOffset overflowOffset - Elf.fromFileOffset binCodeEndOffset

  -- .shstrtab contents
  let newShstrtabContents :: BS.ByteString
      shstrtabOffsetMap :: Map BS.ByteString Word32
      (newShstrtabContents, shstrtabOffsetMap) =
        Elf.encodeStringTable $ bslSectionNames binShdrIndexInfo

  -- Create file indices where new data is added.
  let layoutSegmentAppends
        | objRodataSize objInfo == 0 = do
          let newCodeSize = overflowEndOffset - Elf.fromFileOffset binCodeEndOffset
          [(fromIntegral binCodeEndOffset, fromIntegral newCodeSize)]
        | otherwise =
          case Bin.eclRodataPhdrIndex binLayout of
            -- No rodata -- put rodata after overflow.
            Nothing -> do
              let newRodataOffset = Elf.fromFileOffset $ Elf.alignFileOffset 16 (Elf.FileOffset overflowEndOffset)
              let newRodataEndOffset = newRodataOffset + objRodataSize objInfo
              let sz = newRodataEndOffset - Elf.fromFileOffset binCodeEndOffset
              [(fromIntegral binCodeEndOffset, fromIntegral sz)]
            -- Separate rodata
            Just rodataIdx -> do
              let newCodeSize = overflowEndOffset - Elf.fromFileOffset binCodeEndOffset
              let rodataPhdr = Elf.phdrByIndex binHeaderInfo rodataIdx
              let rodataOffset :: Word64
                  rodataOffset = Elf.fromFileOffset (Elf.phdrFileStart rodataPhdr)
              let rodataEndOffset = rodataOffset + Elf.phdrFileSize rodataPhdr
              let newRodataOffset = Elf.fromFileOffset $ Elf.alignFileOffset 16 (Elf.FileOffset rodataEndOffset)
              let newRodataEndOffset = newRodataOffset + objRodataSize objInfo
              let newRodataSize = newRodataEndOffset - rodataEndOffset
              [ (fromIntegral binCodeEndOffset, fromIntegral newCodeSize)
                , (fromIntegral rodataEndOffset,  fromIntegral newRodataSize)
                ]

  -- Create new binary layout
  newBinLayout <- do
    let layoutCtx = New.LayoutCtx
          { New.lctxClass = cl
          , New.lctxPhdrCount = Elf.phdrCount binHeaderInfo
          , New.lctxShdrCount = Elf.shdrCount binHeaderInfo + 1
          , New.lctxShstrtabSize = fromIntegral (BS.length newShstrtabContents)
          , New.lctxStrtabSize   =
              maybe 0 (BS.length . newStrtabContents) newSymtab
          , New.lctxSymtabSize   =
              maybe 0 (fromIntegral . newSymtabSize) newSymtab
          , New.lctxAppendMap = Map.fromList layoutSegmentAppends
          }
    case New.layoutNewBinary layoutCtx binLayout of
      Left msg -> throwError msg
      Right l -> pure l

  -- .shstrtab section index.
  newShstrtabIndex <- do
    let idx = Elf.shstrtabIndex binHeaderInfo
    when (idx >= Elf.shdrCount binHeaderInfo) $ do
      throwError "Could not find .shstrtab index."
    pure $! mapBinSectionIndex binShdrIndexInfo idx

  -- Get offset of program header table
  newPhdrTableOffset <-
    case New.nblPhdrTableOffset newBinLayout of
      Nothing -> throwError "Missing program header table."
      Just o -> pure o

  -- Get offset of section header table
  newShdrTableOffset <-
    case New.nblShdrTableOffset newBinLayout of
      Nothing -> throwError "Missing section header table."
      Just o -> pure o

  -----------------------------------------------------------------------------
  -- 4. Generate contents

  let binContents = Elf.headerFileContents binHeaderInfo


  let codeEndAddr = Elf.phdrSegmentVirtAddr binCodePhdr + Elf.phdrMemSize binCodePhdr

  -- Address of start of overflow section.
  -- This is the of the binary code segment rounded up to nearest multiple of 16.
  let overflowAddr :: Word64
      overflowAddr = codeEndAddr + overflowPadding

  -- Compute address of rodata.
  let additionalRodataAddr
        | objRodataSize objInfo == 0 = Nothing
        | otherwise =
          case Bin.eclRodataPhdrIndex binLayout of
            -- No rodata -- put rodata after overflow.
            Nothing -> do
              let newRodataOffset = Elf.fromFileOffset $ Elf.alignFileOffset 16 (Elf.FileOffset overflowEndOffset)
              let padding = newRodataOffset - overflowEndOffset
              Just $ codeEndAddr + padding
            -- Separate rodata
            Just rodataIdx -> do
              let rodataPhdr = Elf.phdrByIndex binHeaderInfo rodataIdx
              let rodataAddr = Elf.phdrSegmentVirtAddr rodataPhdr
              let rodataOffset = Elf.fromFileOffset (Elf.phdrFileStart rodataPhdr)
              let rodataEndOffset = rodataOffset + Elf.phdrFileSize rodataPhdr
              let newRodataOffset = Elf.fromFileOffset (Elf.alignFileOffset 16 (Elf.FileOffset rodataEndOffset))
              let size = newRodataOffset - rodataOffset
              Just $ rodataAddr + size

  -- Compute information needed to resolve relocations in object file.
  objRelocInfo <- do
    let addrOfObjSec :: ObjectSectionIndex -> Maybe Word64
        addrOfObjSec idx =
          case Map.lookup idx (objSectionAddrMap objInfo) of
            Nothing -> Nothing
            Just (BinAddr a) -> Just a
            Just (OverflowAddr o) -> Just (overflowAddr + o)
            Just (RodataAddr o) -> (\a -> a + o) <$> additionalRodataAddr
    let undefAddrMap :: Map BS.ByteString Word64
        undefAddrMap =
          let ins m r = Map.insert (ofrObjName r) (ofrBinAddr r) m
           in foldl' ins Map.empty (mrUndefinedFuns ctx)
    case mkRelocInfo (`Map.lookup` undefAddrMap) addrOfObjSec objSymbols objShdrs of
      Left err -> throwError $ "Object file error: " ++ err
      Right r  -> pure r

  objOverflowCode <- do
    let overflowCodeRegions = reverse (objOverflowCodeRev objInfo)
    generateOverflowSection objHeaderInfo objShdrs objRelocInfo (NewAddr overflowAddr) overflowCodeRegions

  -- Size of object code plus any padding between end of old code and start of object code.
  let newCodePhdrSize  :: Word64
      newCodePhdrSize = Elf.phdrFileSize binCodePhdr + overflowPadding + overflowSize

  newRodataContents <- do
    case additionalRodataAddr of
      Nothing -> pure mempty
      Just addr -> do
        let overflowSections = reverse (objAdditionalRodataRev objInfo)
        generateOverflowSection objHeaderInfo objShdrs objRelocInfo (NewAddr addr) overflowSections

  let contentAppends
        | objRodataSize objInfo == 0 =
            [(binCodeEndOffset, [(overflowSize, objOverflowCode)])]
        | otherwise =
          case Bin.eclRodataPhdrIndex binLayout of
            -- No rodata -- put rodata after overflow.
            Nothing ->
              [ ( fromIntegral binCodeEndOffset
                , [(overflowSize, objOverflowCode), (objRodataSize objInfo, newRodataContents)]
                )
                ]
            -- Separate rodata
            Just rodataIdx -> do
              let rodataPhdr = Elf.phdrByIndex binHeaderInfo rodataIdx
              let rodataOffset = Elf.fromFileOffset (Elf.phdrFileStart rodataPhdr)
              let rodataEndOffset = rodataOffset + Elf.phdrFileSize rodataPhdr
              [ (fromIntegral binCodeEndOffset, [(overflowSize, objOverflowCode)])
                , (fromIntegral rodataEndOffset,  [(objRodataSize objInfo, newRodataContents)])
                ]

  -- Compute context for new binary layout.
  let newBuildCtx =
        New.BuildCtx
        { New.bctxHeaderInfo = binHeaderInfo
        , New.bctxExtendedSegmentMap =
            Map.singleton binCodePhdrIndex newCodePhdrSize
        , New.bctxPhdrTableOffset = newPhdrTableOffset

        , New.bctxBinShdrs = binShdrs
        , New.bctxSectionNameMap = \nm ->
            let msg = "internal failure: Missing section header name."
             in Map.findWithDefault (error msg) nm shstrtabOffsetMap
        , New.bctxBinSectionIndexMap = mapBinSectionIndex binShdrIndexInfo
        , New.bctxShdrTableOffset = newShdrTableOffset
        , New.bctxShdrStrndx      = New.fromNewSectionIndex newShstrtabIndex
        , New.bctxShstrtab  = newShstrtabContents

        , New.bctxCodeMap =
            mkBinCodeContent binContents objHeaderInfo objShdrs objRelocInfo objInfo
        , New.bctxOverflowCodeShdrIndex = bslOverflowCodeShdrIndex binShdrIndexInfo
        , New.bctxOverflowAddr          = overflowAddr
        , New.bctxOverflowInsertOffset  = binCodeEndOffset
        , New.bctxOverflowSize          = overflowSize
        , New.bctxAppendMap = Map.fromList contentAppends
        , New.bctxNewSymtab = newSymtab
        , New.bctxFileOffsetFn = New.nblFindNewOffset newBinLayout
        }
  -- Create final elf image.
  return $ Bld.toLazyByteString $ New.buildNewBinary newBuildCtx binLayout