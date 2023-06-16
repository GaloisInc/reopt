module Reopt.Relinker.NewSymtab (
  NewSymtab (..),
  mkNewSymtab,

  -- * Utilities
  getShdrContents,
  mapFromFuns,
  NewSectionIndex (..),
) where

import Control.Monad.Except (
  Except,
  MonadError (throwError),
  when,
 )
import Control.Monad.State (State, modify, runState)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Bld
import Data.ByteString.Char8 qualified as BSC
import Data.ElfEdit qualified as Elf
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64)
import GHC.Stack (HasCallStack)

import Reopt.Relinker.Relations as Relations (
  MergeRelations (mrObjectFuns),
  ObjFunDef (ofdBinAddr, ofdObjName),
 )

-- | Section index in new binary
newtype NewSectionIndex = NewSectionIndex {fromNewSectionIndex :: Word16}
  deriving (Eq, Num)

mapFromFuns :: (Foldable f, Ord k) => (a -> k) -> (a -> v) -> f a -> Map k v
mapFromFuns kf vf l =
  let insAddr m f = Map.insert (kf f) (vf f) m
   in foldl' insAddr Map.empty l

-- | Get bytes from a section header file.
getShdrContents ::
  Integral (Elf.ElfWordType w) =>
  Elf.Shdr nm (Elf.ElfWordType w) ->
  Elf.ElfHeaderInfo w ->
  BS.ByteString
getShdrContents shdr hdrInfo =
  let o = fromIntegral $ Elf.shdrOff shdr
      sz = fromIntegral $ Elf.shdrSize shdr
   in BS.take sz $ BS.drop o $ Elf.headerFileContents hdrInfo

-----------------------------------------------------------------------
-- NewSymtab

-- | Information from constructing new symbol table.
data NewSymtab = NewSymtab
  { newStrtabContents :: !BSC.ByteString
  , newSymtabSize :: !Word64
  , newSymtabLocalCount :: !Word32
  , newSymtabContents :: !Bld.Builder
  }

-- |  Resolve symbol table entry into offset.
finalizeSymtabEntryNameIndex ::
  HasCallStack =>
  Map BS.ByteString Word32 ->
  Elf.SymtabEntry BS.ByteString v ->
  Elf.SymtabEntry Word32 v
finalizeSymtabEntryNameIndex strtabOffsetMap e =
  case Map.lookup (Elf.steName e) strtabOffsetMap of
    Nothing -> error "internal failure: Unexpected symbol."
    Just idx -> e{Elf.steName = idx}

-- | Create symbol table in new file.
mkNewSymtab ::
  MergeRelations ->
  Elf.ElfHeaderInfo 64 ->
  -- | Section headers in original binary
  V.Vector (Elf.Shdr BS.ByteString Word64) ->
  -- | Index of symbol table
  Word16 ->
  -- | Map section indices from original binary to new.
  (Word16 -> Word16) ->
  -- | Symbols in object file
  V.Vector (Elf.SymtabEntry BS.ByteString Word64) ->
  -- | Maps symbol names in object file to index and
  -- offset where they are stored.
  (BS.ByteString -> Maybe (NewSectionIndex, Word64)) ->
  Except String NewSymtab
mkNewSymtab ctx binHeaderInfo binShdrs binSymtabIndex binShdrIndexMap objSymbols addrOfObjSymbol = do
  let hdr = Elf.header binHeaderInfo
      cl = Elf.headerClass hdr
      elfDta = Elf.headerData hdr

  let binSymtabShdr = binShdrs V.! fromIntegral binSymtabIndex

  let binLocalSymCount :: Word32
      binLocalSymCount = Elf.shdrInfo binSymtabShdr

  let binSymtab :: BS.ByteString
      binSymtab = getShdrContents binSymtabShdr binHeaderInfo

  let binStrtabIdx :: Int
      binStrtabIdx = fromIntegral (Elf.shdrLink binSymtabShdr)
  when (binStrtabIdx >= V.length binShdrs) $ do
    throwError "Invalid binary string table index."

  let binStrtab :: BS.ByteString
      binStrtab = getShdrContents (binShdrs V.! binStrtabIdx) binHeaderInfo

  let addrObjectNameMap :: Map Word64 BS.ByteString
      addrObjectNameMap = mapFromFuns ofdBinAddr ofdObjName (mrObjectFuns ctx)

  -- Map symbol in the original binary to a new address.
  let mapBinSymbol ::
        Elf.SymtabEntry BS.ByteString Word64 ->
        -- \^ Symbol entry in the original binary
        State (Set BS.ByteString) (Elf.SymtabEntry BS.ByteString Word64)
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
                newIdx = binShdrIndexMap binIdx
            pure $! e{Elf.steIndex = Elf.ElfSectionIndex newIdx}
          Just (NewSectionIndex midx, off) ->
            pure $!
              e
                { Elf.steIndex = Elf.ElfSectionIndex midx
                , Elf.steValue = off
                }

  binSymbols <-
    case Elf.decodeSymtab cl elfDta binStrtab binSymtab of
      Left _e -> throwError "Could not parse binary symbol table."
      Right syms -> pure syms
  -- This maps
  let ((newLocalSyms, newGlobalSyms), usedOverflowSymbolSet) = flip runState Set.empty $ do
        let (localSyms, globalSyms) = V.splitAt (fromIntegral binLocalSymCount) binSymbols
        l <- V.mapM mapBinSymbol localSyms
        g <- V.mapM mapBinSymbol globalSyms
        pure (l, g)

  -- Get symbols from object file.
  -- Note these are conewdered local
  newObjSyms <- do
    -- Process object symbol at given index.
    let processObjSymbol ::
          Int ->
          Except String (Maybe (Elf.SymtabEntry BS.ByteString Word64, Int))
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
                  Nothing -> throwError $ "Could not find symbol " <> BSC.unpack nm

              when (Elf.steValue e /= 0) $ do
                throwError "Expected symbol value to be zero."
              let e' =
                    e
                      { Elf.steBind = Elf.STB_LOCAL
                      , Elf.steIndex = Elf.ElfSectionIndex newIdx
                      , Elf.steValue = newVal
                      }
              pure (Just (e', i + 1))
          | otherwise = do
              processObjSymbol (i + 1)
    V.unfoldrM processObjSymbol 0

  -- Get number of local symbols (used in section header table)
  let newLocalSymCount :: Word32
      newLocalSymCount = fromIntegral $ V.length newLocalSyms + V.length newObjSyms

  -- Symbols
  let newSymbols :: V.Vector (Elf.SymtabEntry BS.ByteString Word64)
      newSymbols = newLocalSyms <> newObjSyms <> newGlobalSyms

  -- Get end offset of symbol table.
  let symtabSize :: Word64
      symtabSize =
        let cnt :: Word64
            cnt = fromIntegral (V.length newSymbols)
            sz :: Word64
            sz = fromIntegral (Elf.symtabEntrySize cl)
         in (cnt * sz)

  -- Get symbol string table
  let strtabContents :: BS.ByteString
      strtabOffsetMap :: Map BS.ByteString Word32
      (strtabContents, strtabOffsetMap) =
        Elf.encodeStringTable $
          V.toList $
            Elf.steName <$> newSymbols

  let newSymtabEntries :: V.Vector (Elf.SymtabEntry Word32 Word64)
      newSymtabEntries = finalizeSymtabEntryNameIndex strtabOffsetMap <$> newSymbols

  pure $!
    NewSymtab
      { newStrtabContents = strtabContents
      , newSymtabSize = symtabSize
      , newSymtabLocalCount = newLocalSymCount
      , newSymtabContents = foldMap (Elf.encodeSymtabEntry cl elfDta) newSymtabEntries
      }
