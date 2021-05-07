{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn #-}
module Reopt.PltParser
  ( PltInfo(..)
  , PltMap
  , extractPLTEntries
  , ResolvedPltEntry(..)
  , resolvePltEntry
    -- * Exports
  , ppBuffer
  , hasPhdrType
  , phdrContents
  ) where

import           Control.Monad.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import qualified Data.Map as Map
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           Text.Printf ( printf )

-----------------------------------------------------------------------
-- Utility

ppByte :: Word8 -> String -> String
ppByte x s | x < 16 = '0' : showHex x s
           | otherwise = showHex x s

ppBuffer :: BS.ByteString  -> String
ppBuffer = go . BS.unpack
  where go [] = []
        go (h:r) = ppByte h (go1 r)
        go1 [] = []
        go1 (h:r) = ' ' : ppByte h (go1 r)

phdrContents :: Integral (Elf.ElfWordType w) => Elf.ElfHeaderInfo w -> Elf.Phdr w -> BS.ByteString
phdrContents elf p =
  let off = fromIntegral (Elf.phdrFileStart p)
      sz = fromIntegral (Elf.phdrFileSize p)
   in BS.take sz $ BS.drop off (Elf.headerFileContents elf)

word32LE :: Word32 -> BS.ByteString
word32LE x = BS.pack [ w 0, w 1, w 2, w 3]
  where w i = fromIntegral (x `shiftR` (8*i))

getWord32LE :: BS.ByteString -> Int -> Word32
getWord32LE s o = w 0 .|. w 1 .|. w 2 .|. w 3
  where w :: Int -> Word32
        w i = fromIntegral (BS.index s (o+i)) `shiftL` (8*i)


-----------------------------------------------------------------------
-- Instruction utilities

endBr64Insn :: BS.ByteString
endBr64Insn = BS.pack [ 0xf3, 0x0f, 0x1e, 0xfa ]

-- | bnd jmp
bndRelJmpInsnPrefix :: BS.ByteString
bndRelJmpInsnPrefix = BS.pack [0xf2, 0xe9]

jmpInsnPrefix :: BS.ByteString
jmpInsnPrefix = BS.pack [0xe9]

-- | Indirect jump instruction
indRelJmpInsnPrefix :: BS.ByteString
indRelJmpInsnPrefix = BS.pack [0xff, 0x25]

-- | Indirect jump instruction
indRelJmpInsn :: Word32 -- ^ Delta from next instruction to entry
              -> BS.ByteString
indRelJmpInsn ipDelta = indRelJmpInsnPrefix <> word32LE ipDelta

-- | 1 byte nop
nopInsn :: BS.ByteString
nopInsn = BS.pack [0x90]

nopl2Insn :: BS.ByteString
nopl2Insn = BS.pack [0x66, 0x90]

nopl4Insn :: BS.ByteString
nopl4Insn = BS.pack [0x0f, 0x1f, 0x04, 0x00]

nopl5Insn :: BSC.ByteString
nopl5Insn = BS.pack [0x0f, 0x1f, 0x44, 0x00, 0x00]

-- | Push addr insn
pushAddrInsn :: Word32 -> BS.ByteString
pushAddrInsn d = BS.pack [0xff, 0x35] <> word32LE d

pushLitInsnPrefix :: BS.ByteString
pushLitInsnPrefix = BS.pack [0x68]

pushLitInsn :: Word32 -> BS.ByteString
pushLitInsn w = pushLitInsnPrefix <> word32LE w

bndIndRelJmpInsnPrefix :: BS.ByteString
bndIndRelJmpInsnPrefix = BS.pack [0xf2] <> indRelJmpInsnPrefix

bndIndRelJmpInsn :: Word32 -- ^ Delta from next instruction to entry
              -> BS.ByteString
bndIndRelJmpInsn ipDelta = bndIndRelJmpInsnPrefix <> word32LE ipDelta

-----------------------------------------------------------------------
-- PLTEntry

-- | Information about the contents of a entry in the PLT table.
data PltEntry
  = PltResolutionFn
  -- ^ Function at start of .plt section that resolves entries.
  | PltStrictGotJmp !Word64
  -- ^ @PltStrictGotJmp a@ jump to address @a@.
  --
  -- It is used for jumps to global offset table.
  | PltLazyGotJmp !Word32
  -- ^ @JmpPushJmp idx@ jumps to `DE_PLTGOT address + 8 * idx`.
  | PltIndexedStartJmp !Word32
  -- ^ @PushAndJumpPlt v@ pushes @v@ to the stack and then jumps to the address
  -- at the start of the PLT entry.
  | PltJmp8 !Word64
  -- ^ Relative jump to given address.
  | PltPushZeroJmpNext
  -- ^ This corresponds to a PLT that pushes a 32bit 0 to stack and
  -- jumps to next table entry.
  --
  -- It does not appear to be valid code.
  | UnalignedGotJump !Word64
    -- ^ Recognized a jump to a GOT table at an unaligned address.
    --
    -- special case from /sbin/ldconfig.real in
    -- libc-bin 2.33-0ubuntu5.  See
    -- Should be found in libc-bin_2.33-0ubuntu5_amd64.deb
    --   http://mirrors.kernel.org/ubuntu/pool/main/g/glibc/libc-bin_2.33-0ubuntu5_amd64.deb
    --   SHA256 checksum: ba5db4f3a5a7b40d7c1c8d932926c6b5b7fdbd635859e82bdb58e1ef42ba6a87

data ResolvedPltEntry
   -- | A PLT stub that points to symbol at given index
   = PltStub !Word32
   -- | An address supporting something that should be callable.
   | PltNotCallable

type GotMap = Map.Map Word64 Word32
-- ^ Map from addresses in global address table to symbol map is for.

resolvePltEntry :: (Integral v, Show v)
                => GotMap
                   -- ^ Map from addresses
                -> v -- ^ Address of start fo PLT entry
                -> PltEntry
                -> Except String ResolvedPltEntry
resolvePltEntry gotMap _pltAddr = \case
  PltResolutionFn ->
    pure $ PltNotCallable
  PltStrictGotJmp gotAddr -> do
    case Map.lookup gotAddr gotMap of
      Nothing -> do
        pure PltNotCallable
      Just symIdx ->
        pure $ PltStub symIdx
  PltLazyGotJmp _idx ->
    pure $ PltNotCallable
  PltIndexedStartJmp _ ->
    pure $! PltNotCallable
  PltJmp8 _ ->
    pure $! PltNotCallable
  PltPushZeroJmpNext ->
    pure $! PltNotCallable
  UnalignedGotJump _->
    pure $ PltNotCallable

checkInsns :: BS.ByteString -> [BS.ByteString -> Either String a] -> Either [String] a
checkInsns actual = go []
  where go :: [String] -> [BS.ByteString -> Either String a] -> Either [String] a
        go p [] = Left (reverse p)
        go p (h:r) =
          case h actual of
            Left e -> go (e:p) r
            Right a -> Right a

-- | This is the stub used to resolve GOT entries lazily.
-- It expects the index of the GOT will be at the top of the stack
-- and is not a valid function entry point.
jmpPushJumpDispatch :: Integral v
                    => v -- ^ Address of start of PLT
                    -> v -- ^ Address for start of got
                    -> BS.ByteString
                    -> Either String PltEntry
jmpPushJumpDispatch pltAddr gotAddr actual = do
  let expectedInsns = pushAddrInsn  (fromIntegral ((gotAddr + 8)  - (pltAddr + 6)))
                   <> indRelJmpInsn (fromIntegral ((gotAddr + 16) - (pltAddr + 12)))

  let actualInsns = BS.take 12 actual
  when (actualInsns /= expectedInsns) $ do
    Left $ "pj0: " ++ ppBuffer expectedInsns
  -- Check the last four bytes are a no-op.
  let expectedNop4Insns = [BS.pack [0x0f, 0x1f, 0x40, 0x00], BS.replicate 4 0x90]
  unless (BS.drop 12 actual `elem` expectedNop4Insns) $ do
    Left $ "pj0: Bad nop " ++ ppBuffer (BS.drop 12 actual)
  pure $ PltResolutionFn

-- | PLT header with bound prefix
bndPltInitInsn :: Integral v
            => v -- ^ Address of start of PLT
            -> v -- ^ Address for start of got
            -> BS.ByteString
            -> Either String PltEntry
bndPltInitInsn pltAddr gotAddr actual = do
  let expectedNop = BS.pack [0x0f, 0x1f, 0x00]
  let expected = pushAddrInsn (fromIntegral $ (gotAddr + 8) - (pltAddr + 6))
              <> bndIndRelJmpInsn (fromIntegral $ (gotAddr + 16) - (pltAddr+13))
              <> expectedNop
  when (expected /= actual) $ do
    Left $ "bl0: " ++ ppBuffer expected
  pure $ PltResolutionFn

-- | Entry in plt with form:
-- jmp *gotEntry
-- push idx
-- jmp *plt
pltIdxInsns :: Integral v
            => v -- ^ Address of start of PLT
            -> v -- ^ Address specified in DT_PLTGOT
            -> Word32  -- Index of entry
            -> BS.ByteString
            -> Either String PltEntry
pltIdxInsns pltAddr pltgotAddr idx actual = do
  let tgtAddr = pltgotAddr + 8 * fromIntegral (idx+2)
  let insnAddr = pltAddr + 16 * fromIntegral idx + 6
  let expected = indRelJmpInsn (fromIntegral (tgtAddr - insnAddr))
                 <> pushLitInsn (idx-1)
                 <> jmpInsnPrefix <> word32LE (negate (16 * (idx+1)))
  when (expected /= actual) $ do
    Left $ "piX: " ++ ppBuffer expected
  pure $ PltLazyGotJmp (idx+2)

-- | PLT entry with endbr64 landing pad followed by bnd
-- indirect PC relative jump.
--   endbr64
--   bnd jmp *(ip + delta)
--   nop
endbrBndJmpInsns
  :: forall v
  .  (Integral v, Ord v, Show v)
  => v -- ^ Address of start of PLT
  -> Word32  -- Index of entry
  -> BS.ByteString
  -> Either String PltEntry
endbrBndJmpInsns pltBaseAddr idx actual = do
  let pltEntryAddr = pltBaseAddr + 16 * fromIntegral idx
  let expectedInsn = endBr64Insn <> bndIndRelJmpInsnPrefix
  unless (BS.take 7 actual == expectedInsn) $ do
    Left $ "ebj: " ++ ppBuffer expectedInsn
  -- Compute index we are jumping to.
  let actualDelta :: Word32
      actualDelta = getWord32LE actual 7
  let actualAddr :: Word64
      actualAddr = fromIntegral pltEntryAddr + 11 + fromIntegral actualDelta
  -- Check 5-byte no-op instruction at end.
  let actualNop = BS.drop 11 actual
  let expectedNop5 = [ nopl5Insn, nopl4Insn <> nopInsn]
  unless (actualNop `elem` expectedNop5) $ do
    Left $ "ebj: Unexpected no-op " ++ ppBuffer actualNop
  if actualAddr .&. 0x7 /= 0 then
    pure $ UnalignedGotJump actualAddr
   else
    pure $ PltStrictGotJmp actualAddr

-- | PLT header with bound prefix
--
-- Expected
--   endbr64
--   push val
--   jmp pltStart
pltBndIdxInsns :: Word32  -- ^ Index of PLT entry
               -> BS.ByteString -- Bytes in entry
               -> Either String PltEntry
pltBndIdxInsns idx actual = do
  -- Check for endBr64Insns
  unless (endBr64Insn == BS.take 4 actual) $ do
    Left $ "pbX: Expected endBr64Insn " ++ ppBuffer endBr64Insn

  unless (pushLitInsnPrefix == BS.take 1 (BS.drop 4 actual)) $ do
    Left $ "pbX: Expected push " ++ ppBuffer pushLitInsnPrefix

  -- Value pushed (index of instruction)
  let pushValue = getWord32LE actual 5

  unless (bndRelJmpInsnPrefix == BS.take 2 (BS.drop 9 actual)) $ do
    Left $ "pbX: " ++ replicate 27 ' ' <> ppBuffer bndRelJmpInsnPrefix

  let relJmp = getWord32LE actual 11

  unless (nopInsn == BS.drop 15 actual) $ do
    Left $ "pbX: Missing nop"

  if pushValue == 0 && relJmp == 0 then do
    -- Handle special case from /sbin/ldconfig.real in
    -- libc-bin 2.33-0ubuntu5.  See
    -- Should be found in libc-bin_2.33-0ubuntu5_amd64.deb
    --   http://mirrors.kernel.org/ubuntu/pool/main/g/glibc/libc-bin_2.33-0ubuntu5_amd64.deb
    --   SHA256 checksum: ba5db4f3a5a7b40d7c1c8d932926c6b5b7fdbd635859e82bdb58e1ef42ba6a87
    pure $ PltPushZeroJmpNext
   else do
    let expected1 = negate (16 * idx + 0xf)
    unless (relJmp == expected1) $ do
      Left $ printf "pbX: unexpected jump (push = %s, relJmp = 0x%s, expected = 0x%s)"
              (show pushValue) (showHex relJmp "") (showHex expected1 "")
    pure $! PltIndexedStartJmp pushValue

-- | PLT header with bound prefix
--
-- Expected
--   endbr64
--   push val
--   jmp pltStart
jmp8Insns :: (Integral v, Show v)
          => v -- ^ PLT address
          -> Word32 -- ^ Index
          -> BS.ByteString -- Bytes in entry
          -> Either String PltEntry
jmp8Insns pltAddr idx actual = do
  let pltEntryAddr = pltAddr + 8 * fromIntegral idx
  -- Check for endBr64Insns
  unless (BS.take 2 actual == indRelJmpInsnPrefix) $ do
    Left $ "jm8: Jmp " ++ ppBuffer indRelJmpInsnPrefix
  -- Indirect jump target
  let actualDelta :: Word32
      actualDelta = getWord32LE actual 2
  let actualAddr :: Word64
      actualAddr = fromIntegral pltEntryAddr
                 + fromIntegral idx + 4 + fromIntegral actualDelta
  unless (BS.take 2 (BS.drop 6 actual) == nopl2Insn) $ do
    Left $ "jm8: Nop " ++ ppBuffer nopl2Insn
  pure $! PltJmp8 actualAddr

-----------------------------------------------------------------------
-- Extraction

hasPlt8Size :: BS.ByteString -> Bool
hasPlt8Size s
  = BS.length s >= 8
  && BS.take 2 s == indRelJmpInsnPrefix
  && BS.take 2 (BS.drop 6 s) == BS.pack [0x66, 0x90]

checkPlt :: forall w
         . Elf.ElfHeaderInfo w
         -> Maybe (Elf.ElfWordType w)
         -> Elf.Shdr BS.ByteString (Elf.ElfWordType w)
         -> Except String (Int, V.Vector PltEntry)
checkPlt elf mpltgotAddr shdr = Elf.elfClassInstances (Elf.headerClass (Elf.header elf)) $ do
  let shdrName = BSC.unpack (Elf.shdrName shdr)
  let fileOff =  fromIntegral (Elf.shdrOff shdr)
  let fileSize = fromIntegral (Elf.shdrSize shdr)
    -- Get list of predicates that may recognize a PLT entry
  let pltContents = BS.take fileSize $ BS.drop fileOff $ Elf.headerFileContents elf
  let entrySize | hasPlt8Size pltContents = 8
                | otherwise = 16
  let (cnt,m) = fileSize `quotRem` entrySize
  when (m /= 0) $ do
    throwError $ printf "%s is not a multiple of expected entry size." shdrName
  let pltAddr :: Elf.ElfWordType w
      pltAddr = Elf.shdrAddr shdr

  -- Check entries
  v <- forM (V.generate cnt fromIntegral) $ \(idx :: Word32) -> do
    -- Recognizers
    let recognizers :: [BS.ByteString -> Either String PltEntry]
        recognizers
          | hasPlt8Size pltContents =
            [ jmp8Insns pltAddr idx ]
          | otherwise =
            case Elf.shdrName shdr of
              ".plt"  | Just pltgotAddr <- mpltgotAddr
                      , idx == 0  -> [ jmpPushJumpDispatch pltAddr pltgotAddr
                                     , bndPltInitInsn pltAddr pltgotAddr

                                     ]
                      | Just pltgotAddr <- mpltgotAddr ->
                        [ pltIdxInsns pltAddr pltgotAddr idx
                        , pltBndIdxInsns idx
                        , endbrBndJmpInsns pltAddr idx
                        ]
                      | otherwise -> []
              ".plt.got" -> [endbrBndJmpInsns pltAddr idx]
              ".plt.sec" -> [endbrBndJmpInsns pltAddr idx]
              _ -> error "Unexpected section header."
    -- Get entry contents
    let off = entrySize * fromIntegral idx
    let entryContents = BS.take entrySize $ BS.drop off pltContents
    case checkInsns entryContents recognizers of
      Left errors -> do
        throwError $ printf "%s address invalid %s (%s):\n  Actual:\n         %s\n  Expect:\n%s"
          shdrName
          (showHex (pltAddr + fromIntegral off) "")
          (show idx)
          (ppBuffer entryContents)
          (unlines (("    " ++) <$> errors))
      Right e -> pure e
  pure (entrySize, v)


hasPhdrType :: Elf.PhdrType -> Elf.Phdr w -> Bool
hasPhdrType tp p = Elf.phdrSegmentType p == tp

getDynamicWord :: Elf.DynamicSection w
               -> Elf.ElfDynamicTag
               -> Except String (Elf.ElfWordType w)
getDynamicWord dynSection tag = do
  case Map.findWithDefault [] tag (Elf.dynMap dynSection) of
    [] -> throwError $ printf "Missing %s." (show tag)
    _:_:_ -> throwError  $ printf "Multiple %s." (show tag)
    [a] -> pure a

tryGetDynamicWord :: Elf.DynamicSection w
               -> Elf.ElfDynamicTag
               -> Except String (Maybe (Elf.ElfWordType w))
tryGetDynamicWord dynSection tag = do
  case Map.findWithDefault [] tag (Elf.dynMap dynSection) of
    _:_:_ -> throwError  $ printf "Multiple %s." (show tag)
    [] -> pure Nothing
    [a] -> pure (Just a)

checkRela :: String -- ^ Name for error reporting
          -> Elf.ElfData -- ^ Endianess of Elf file
          -> BS.ByteString
          -> [Elf.X86_64_RelocationType]
          -> Word64 -- ^ Maximum
          -> Map.Map Word64 Word32 -- ^ Map from addresses to symbol index.
          -> Word64
          -> Except String (Map.Map Word64 Word32)
checkRela nm dta bs allowed cnt m idx = do
  if idx >= cnt then
    pure m
   else do
    let rela :: Elf.RelaEntry Elf.X86_64_RelocationType
        rela = Elf.decodeRelaEntry dta bs (fromIntegral idx)
    when (Elf.relaType rela `notElem` allowed) $ do
      throwError $ printf "%s: Unexpected relocation type %s." nm (show (Elf.relaType rela))
    case Elf.relaType rela of
      Elf.R_X86_64_64 -> pure ()
      Elf.R_X86_64_IRELATIVE -> pure ()
      Elf.R_X86_64_RELATIVE  -> pure ()
      Elf.R_X86_64_TPOFF64   -> pure ()
      tp -> do
        when (Elf.relaAddend rela /= 0) $ do
          throwError $ printf "%s: PLT stub relocation addened is non-zero (%s)." nm (show tp)
    let m' = Map.insert (Elf.relaAddr rela) (Elf.relaSym rela) m
    checkRela nm dta bs allowed cnt m' (idx+1)

-- | Maps offsets with PLT entries to target (if any) and size of entry.
type PltMap w = Map.Map (Elf.ElfWordType w) (ResolvedPltEntry, Elf.ElfWordType w)



-- | Information
data PltInfo w
  = PltInfo
  { pltVirtMap :: !(Elf.VirtAddrMap w)
  , pltStrtabOff :: !(Elf.ElfWordType w)
  , pltStrtabSize :: !(Elf.ElfWordType w)
  , pltSymtabOff :: !(Elf.ElfWordType w)
  , pltMap :: !(PltMap w)
  }

-- | Extract PLT entries from header.
extractPLTEntries :: forall w
                  .  (w ~ 64)
                  => Elf.ElfHeaderInfo w
                  -> V.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w))
                  -> Either String (Maybe (PltInfo w))
extractPLTEntries elf shdrs = runExcept $ do
  let dta = Elf.headerData  (Elf.header elf)
  let cl = Elf.headerClass (Elf.header elf)
  Elf.elfClassInstances cl $ do
    let d = Elf.headerData (Elf.header elf)
    let phdrs = Elf.headerPhdrs elf
    case filter (hasPhdrType Elf.PT_DYNAMIC) phdrs of
      [] -> pure Nothing
      _:_:_ -> do
        throwError "Multiple dynamic segments."
      [dynPhdr] -> do
        let dynContents = phdrContents elf dynPhdr
        -- Create dynamic section
        dynSection <-
          case Elf.dynamicEntries d cl dynContents of
            Left e  -> throwError $ printf "Failed to parse dynamic entries:\n  %s" (show e)
            Right r -> pure r

        strtabOff  <- getDynamicWord dynSection Elf.DT_STRTAB
        strtabSize <- getDynamicWord dynSection Elf.DT_STRSZ
        symtabOff  <- getDynamicWord dynSection Elf.DT_SYMTAB

        -- Create virtual map
        vmap <-
          case Elf.virtAddrMap (Elf.headerFileContents elf) phdrs of
            Nothing -> throwError $ "Memory contents are ambiguous."
            Just r -> pure r

        -- Check this is a rela section
        mpltRel <- tryGetDynamicWord dynSection Elf.DT_PLTREL
        case mpltRel of
          Nothing ->
            pure ()
          Just a -> do
            let relType = Elf.ElfDynamicTag (fromIntegral a)
            when (Elf.DT_RELA /= relType) $ do
              throwError $ "Unexpected rel value: " ++ show relType
        -- .rela.dyn parsing
        --[10] .rela.dyn         RELA             0000000000000c28  00000c28
        --00000000000009a8  0000000000000018   A       6     0     8
        -- DT_RELA = 0xc28
        -- DT_RELASZ = 0x2472
        -- DT_RELAENT  = 0x18
        -- DT_RELACOUNT = 96 (seems to be R_x86_64_RELATIVE entries followe by R_X86_64_GLOB_DAT, R_x86_64_COPY)
        mrelaDynAddr <- tryGetDynamicWord dynSection Elf.DT_RELA
        relaMap <-
          case mrelaDynAddr of
            Nothing ->
              pure Map.empty
            Just relaDynAddr -> do
              entSize <- getDynamicWord dynSection Elf.DT_RELAENT
              when (entSize /= 0x18) $ do
                throwError "Expected entrysize."
              relaSize <- getDynamicWord dynSection Elf.DT_RELASZ
              let (rcount, rmod) = relaSize `divMod` entSize
              when (rmod /= 0) $ do
                throwError "Relocation size not a multiple of entry size."

              contents <-
                case Elf.lookupVirtAddrContents relaDynAddr vmap of
                  Nothing -> throwError  $ printf "Invalid dynamic addr %s." (show relaDynAddr)
                  Just c -> do
                    when (BS.length c < fromIntegral relaSize) $ do
                      throwError $ "Relocations extend outside loadable segment."
                    pure (BS.take (fromIntegral relaSize) c)

              -- Relocations
              mcount <- tryGetDynamicWord dynSection Elf.DT_RELACOUNT
              case mcount of
                Nothing -> do
                  checkRela "rg" dta contents [Elf.R_X86_64_RELATIVE, Elf.R_X86_64_64, Elf.R_X86_64_GLOB_DAT, Elf.R_X86_64_COPY, Elf.R_X86_64_TPOFF64] rcount Map.empty 0
                Just count -> do
                  when (count > rcount) $ do
                    throwError $ printf "DT_RELACOUNT %s larger than maximum size %s." (show count) (show rcount)
                  m0 <- checkRela "rc" dta contents [Elf.R_X86_64_RELATIVE] count Map.empty 0
                  checkRela "rh" dta contents  [Elf.R_X86_64_64, Elf.R_X86_64_GLOB_DAT, Elf.R_X86_64_COPY, Elf.R_X86_64_TPOFF64, Elf.R_X86_64_IRELATIVE, Elf.R_X86_64_DTPOFF64, Elf.R_X86_64_DTPMOD64] rcount m0 count

        -- DT_JMPREL
        mjmpRelAddr <- tryGetDynamicWord dynSection Elf.DT_JMPREL
        jmprelMap <-
          case mjmpRelAddr of
            Nothing ->
              pure Map.empty
            Just jmpRelAddr -> do
              entSize <- getDynamicWord dynSection Elf.DT_RELAENT
              when (entSize /= 0x18) $ do
                throwError "Expected entrysize."
              relaSize <- getDynamicWord dynSection Elf.DT_PLTRELSZ
              let (rcount, rmod) = relaSize `divMod` entSize
              when (rmod /= 0) $ do
                throwError "Relocation size not a multiple of entry size."

              contents <-
                case Elf.lookupVirtAddrContents jmpRelAddr vmap of
                  Nothing -> throwError  $ printf "Invalid plt addr %s." (show jmpRelAddr)
                  Just c -> do
                    when (BS.length c < fromIntegral relaSize) $ do
                      throwError $ "PLT relocations extend outside loadable segment."
                    pure (BS.take (fromIntegral relaSize) c)
              checkRela "jg" dta contents [Elf.R_X86_64_JUMP_SLOT, Elf.R_X86_64_IRELATIVE] rcount Map.empty 0

        -- Address of global offset table
        -- Note. There are binaries with `.plt.got` sections but no DT_PLTGOT section.
        pltgotAddr <- tryGetDynamicWord dynSection Elf.DT_PLTGOT

        let gotAddrMap :: Map.Map Word64 Word32
            gotAddrMap = relaMap <> jmprelMap
        let ins :: PltMap w
                -> Elf.Shdr BS.ByteString (Elf.ElfWordType w)
                -> Except String (PltMap w)
            ins m0 shdr
              | Elf.shdrName shdr `elem` [".plt", ".plt.got", ".plt.sec"]
              , Elf.shdrType shdr /= Elf.SHT_NOBITS = do
                (entrySize, pltEntries) <- checkPlt elf pltgotAddr shdr
                let insEntry :: PltMap w
                             -> Int
                             -> Except String (PltMap w)
                    insEntry m i = do
                      let addr = Elf.shdrAddr shdr + fromIntegral (entrySize * i)
                      let entry = pltEntries V.! i
                      e <- resolvePltEntry gotAddrMap addr entry
                      pure $! Map.insert addr (e, fromIntegral entrySize) m
                foldlM insEntry m0 (V.generate (V.length pltEntries) id)
              | otherwise =
                pure m0
        m <- foldlM ins Map.empty shdrs
        pure $ Just
          $ PltInfo
          { pltVirtMap = vmap
          , pltStrtabOff = strtabOff
          , pltStrtabSize = strtabSize
          , pltSymtabOff  = symtabOff
          , pltMap = m
          }

-- 0x600 .rela.dyn
-- tiny-AES-c/test.elf: Unknown .got address 3ff8.
--   /usr/sbin/ldconfig.real