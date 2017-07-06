-- dump_vtables
-- Takes an ELF file as input and attempts to dump the contents of the
-- vtables.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Either
import Data.ElfEdit
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Macaw.Memory (bsWord64le)
import Data.Parameterized.Some
import Data.Word
import Numeric (showHex, showIntAtBase)
import Reopt
import System.Directory
import System.Environment
import System.IO

import qualified Data.ByteString as B
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

trimLeadingZeros :: String -> String
trimLeadingZeros = dropWhile (=='0')

-- | Detect whether a symbol table entry refers to a vtable.
--
-- This is a guess based on my own inspection of the symbol tables of compiled C++
-- ELF binaries. Basically, the symbol name of every vtable I've encountered starts
-- with _ZTV; furthermore, vtables always reside in an executable segment.
elfSection :: Elf 64 -> Word16 -> ElfSection Word64
elfSection e i
  | (section:[]) <- sections = section
  | (_:_) <- sections = error $ "Multiple sections with index " ++ show i
  | _     <- sections = error $ "No sections with index " ++ show i
  where sections = filter (hasSectionIndex i) (e^..elfSections)
        hasSectionIndex i section = elfSectionIndex section == i

-- TODO: Only accept entries that point to .rodata
isVTableEntry :: Elf 64 -> ElfSymbolTableEntry Word64 -> Bool
isVTableEntry e ste = B.isPrefixOf (B.pack [95,90,84,86]) (steName ste)
                      && elfSectionName section == rodata
  where section = elfSection e (fromElfSectionIndex (steIndex ste))
        rodata = B.pack [46,114,111,100,97,116,97]
-- isVTableEntry m ste = (B.isPrefixOf (B.pack [95,90,84,86]) . steName) ste
--                       && (isCodeAddr m . memWord . steValue) ste

-- | Get the vtable entries from the symbol table.
--
vTableEntries :: Elf 64 -> ElfSymbolTable Word64 -> [ElfSymbolTableEntry Word64]
vTableEntries e =
  filter (isVTableEntry e) . V.toList . elfSymbolTableEntries

-- | RTTI datatype.
data RTTI = RTTI { rttiAddr :: Word64
                 , rttiPtr1 :: Word64 -- not sure how I can use this yet, or what it really is
                 , rttiMangledName :: B.ByteString
                 , rttiMangledNameAddr :: Word64
                 , rttiParentRTTIAddr :: Maybe Word64
                 } deriving (Eq)

instance Show RTTI where
  show (RTTI rttiAddr _ rttiMangledName _ rttiParentRTTIAddr) =
    "addr = " ++ (trimLeadingZeros . showAddr64) rttiAddr ++ ", " ++
    "name = " ++ show rttiMangledName ++ ", " ++
    "parentRTTIAddr = " ++ 
    case rttiParentRTTIAddr of
      Just w -> (trimLeadingZeros . showAddr64) w
      Nothing -> "none"

-- | Read a null-terminated byte string from memory given a 64-bit address.
readNTBSFromAddr :: ElfSection Word64 -> Word64 -> B.ByteString
readNTBSFromAddr section ptr = B.takeWhile (/=0) $ readElfSection section ptr 10

-- | VTable datatype.
data VTable = VTable { vTableAddr :: Word64
                     , vTableSize :: Word64
                     , vTableOffset :: Word64
                     , vTableRTTI :: Maybe RTTI
                     , vTableFPtrs :: [Word64]
                     , vTableContents :: B.ByteString
                     } deriving (Eq)


readElfSection :: ElfSection Word64 -> Word64 -> Word64 -> B.ByteString
readElfSection section addr size =
  B.take (fromIntegral size) (B.drop adjustedAddr (elfSectionData section))
  where adjustedAddr = fromIntegral $ addr - (elfSectionAddr section)

-- | Build an RTTI datatype from the memory and a 64-bit address.
rttiFromPtr :: ElfSection Word64 -> Word64 -> Maybe RTTI
rttiFromPtr section ptr
  | B.null contents = Nothing
  | otherwise = Just (RTTI ptr rttiPtr1 rttiName rttiNameAddr rttiParentRTTIAddr)
  where contents = readElfSection section ptr 24
        rttiPtr1 = (bsWord64le . B.take 8) contents
        rttiNameAddr = (bsWord64le . B.take 8 . B.drop 8) contents
--        rttiName = B.pack []
        rttiName = readNTBSFromAddr section rttiNameAddr
        rttiParentRTTIAddr = case (rttiNameAddr - ptr) of
          16 -> Nothing
          24 -> Just $ (bsWord64le . B.take 8 . B.drop 16) contents
          _ -> Nothing

-- | Build a VTable datatype from the memory and a symbol table entry.
vTableFromSTE :: Elf 64 -> ElfSymbolTableEntry Word64 -> VTable
vTableFromSTE e ste = VTable { vTableAddr = addr
                             , vTableSize = size
                             , vTableFPtrs = fptrs
                             , vTableOffset = (bsWord64le . B.take 8) contents
                             , vTableRTTI = rtti
                             , vTableContents = contents }
  where addr = steValue ste
        size = steSize ste
        -- TODO: Why is there an ElfSectionIndex type? Why not just
        -- use Word16? Given the answer to that question, why does the
        -- ElfSection type use a Word16 as its index instead of
        -- ElfSectionIndex?
        idx = fromElfSectionIndex $ steIndex ste
        section = elfSection e idx
        contents = readElfSection section addr size
        rttiPtr = (bsWord64le . B.take 8 . B.drop 8) contents
        rtti = rttiFromPtr section rttiPtr
        fptrs = (map bsWord64le . map B.pack . chunksOf 8 . B.unpack . B.drop 16) contents

instance Show VTable where
  show (VTable addr size offset rtti fptrs contents) =
    "VTable:\n" ++
    "  Address: " ++ trimLeadingZeros (showAddr64 addr) ++ "\n" ++
    "  Size: " ++ show size ++ " bytes\n" ++
    "  Offset: " ++ show offset ++ "\n" ++
    "  RTTI: " ++ rttiString ++ "\n" ++
    "  Function addrs: " ++ intercalate "," (map (trimLeadingZeros . showAddr64) fptrs)
    where rttiString = case rtti of
                         Nothing -> "not present"
                         Just x -> show x

-- | Get a list of all the VTables in an Elf 64 object.
vTablesFromElf64 :: Elf 64 -> Either String [VTable]
vTablesFromElf64 e =
  case elfSymtab e of
    []     -> Left "No symbol table; can't find vtables yet! (not implemented)"
    (s:[]) -> Right $ map (vTableFromSTE e) $ vTableEntries e s
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
