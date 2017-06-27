-- dump_vtables
-- Takes an ELF file as input and attempts to dump the contents of the
-- vtables.

{-# LANGUAGE DataKinds #-}

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Either
import Data.ElfEdit
import Data.List
import Data.List.Split
import Data.Macaw.Memory
import Data.Macaw.Memory.ElfLoader
import Data.Maybe
import Data.Parameterized.Some
import Data.Word
import Numeric (showHex, showIntAtBase)
import Reopt
import System.Directory
import System.Environment
import System.IO

import qualified Data.ByteString as B
import qualified Data.Vector as V

trimLeadingZeros :: String -> String
trimLeadingZeros = dropWhile (=='0')

-- | Detect whether a symbol table entry refers to a vtable.
--
-- This is a guess based on my own inspection of the symbol tables of compiled C++
-- ELF binaries. Basically, the symbol name of every vtable I've encountered starts
-- with _ZTV; furthermore, vtables always reside in an executable segment.
isVTableEntry :: Memory 64 -> ElfSymbolTableEntry Word64 -> Bool
isVTableEntry m ste = (B.isPrefixOf (B.pack [95,90,84,86]) . steName) ste
                      && (isCodeAddr m . memWord . steValue) ste

-- | Get the vtable entries from the symbol table.
--
vTableEntries :: Memory 64 -> ElfSymbolTable Word64 -> [ElfSymbolTableEntry Word64]
vTableEntries mem =
  filter (isVTableEntry mem) . V.toList . elfSymbolTableEntries

-- | RTTI datatype.
data RTTI = RTTI { rttiAddr :: Word64
                 , rttiPtr1 :: Word64
                 , rttiMangledName :: B.ByteString
                 } deriving (Eq)

instance Show RTTI where
  show (RTTI rttiAddr rttiPtr1 rttiMangledName) =
    "addr = " ++ (trimLeadingZeros . showAddr64) rttiAddr ++ ", " ++
    "rttiPtr1 = " ++ (trimLeadingZeros . showAddr64) rttiPtr1 ++ ", " ++
    "name = " ++ show rttiMangledName

-- | Read a null-terminated byte string from memory given a 64-bit address.
readNTBSFromAddr :: Memory 64 -> Word64 -> B.ByteString
readNTBSFromAddr mem ptr =
  case b of
    0 -> B.empty
    _ -> B.cons b (readNTBSFromAddr mem (ptr + 1))
  where b = (B.head . (\(Right r) -> r) . readByteString segAddr) 1
        segAddr = (fromJust . absoluteAddrSegment mem . memWord) ptr

-- | Build an RTTI datatype from the memory and a 64-bit address.
rttiFromVTableAddr :: Memory 64 -> Word64 -> Maybe RTTI
rttiFromVTableAddr mem ptr = case maybeSegAddr of
  Nothing -> Nothing
  Just segAddr -> Just $ RTTI ptr rttiPtr1 rttiMangledName

--        segAddr = fromJust maybeSegAddr
    where contents = (\(Right r) -> r) $ readByteString segAddr 16
          rttiPtr1 = (bsWord64le . B.take 8) contents
          rttiMangledNamePtr = (bsWord64le . B.take 8 . B.drop 8) contents
          rttiMangledName = readNTBSFromAddr mem rttiMangledNamePtr
  where maybeSegAddr = (absoluteAddrSegment mem . memWord) ptr

-- | VTable datatype.
data VTable = VTable { vTableAddr :: SegmentedAddr 64
                     , vTableSize :: Word64
                     , vTableOffset :: Word64
                     , vTableRTTI :: Maybe RTTI
                     , vTableFPtrs :: [Word64]
                     , vTableContents :: B.ByteString
                     } deriving (Eq)

-- | Build a VTable datatype from the memory and a symbol table entry.
vTableFromSTE :: Memory 64 -> ElfSymbolTableEntry Word64 -> VTable
vTableFromSTE mem ste = VTable { vTableAddr = segAddr,
                                 vTableSize = size,
                                 vTableFPtrs = fptrs,
                                 vTableOffset = (bsWord64le . B.take 8) contents,
                                 vTableRTTI = rttiFromVTableAddr mem rttiPtr,
                                 vTableContents = contents }
  where segAddr = (getAddrSegSTE ste)
        getAddrSegSTE = fromJust . absoluteAddrSegment mem . memWord . steValue
        size = (steSize ste)
        contents = (\(Right r) -> r) $ readByteString segAddr size
        rttiPtr = (bsWord64le . B.take 8 . B.drop 8) contents
        fptrs = (map bsWord64le . map B.pack . chunksOf 8 . B.unpack . B.drop 16) contents

instance Show VTable where
  show (VTable segAddr size offset rtti fptrs contents) =
    "VTable:\n" ++
    "  Address: " ++ show segAddr ++ "\n" ++
    "  Size: " ++ show size ++ " bytes\n" ++
    "  Offset: " ++ show offset ++ "\n" ++
    "  RTTI: " ++ rttiString ++ "\n" ++
    "  Function addrs: " ++ intercalate "," (map (dropWhile (=='0') . showAddr64) fptrs)
    where rttiString = case rtti of
                         Nothing -> "not present"
                         Just x -> show x

-- | Get a list of all the VTables in an Elf 64 object.
vTablesFromElf64 :: Elf 64 -> Either String [VTable]
vTablesFromElf64 e =
  case memoryForElf (LoadOptions LoadBySegment False) e of
    Left s -> Left s
    Right (_,m) ->
      case elfSymtab e of
        []     -> Left "No symbol table; can't find vtables yet! (not implemented)"
        (s:[]) -> Right $ map (vTableFromSTE m) $ vTableEntries m s
        _      -> Left "Need exactly one symbol table in ELF file"

main = do
  args <- getArgs
  case args of
    (bFileName:_) -> do
      e <- readElf64 bFileName
      case vTablesFromElf64 e of
            Left s -> putStrLn s
            Right vtables -> mapM_ print vtables
    _ -> putStrLn "Please supply a file to dump."
