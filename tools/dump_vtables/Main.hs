-- dump_vtables
-- Takes an ELF file as input and attempts to dump the contents of the
-- vtables.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Either
import Data.ElfEdit
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Macaw.Memory (bsWord32le, bsWord64le)
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
import qualified Data.ByteString.Char8 as C

------------------------------------------------------------------------
-- Utilities

-- | Trim the leading zeros
trimLeadingZeros :: String -> String
trimLeadingZeros = dropWhile (=='0')

-- Get a section by its section index.
elfSection :: Elf 64 -> ElfSectionIndex -> ElfSection Word64
elfSection e i
  | [section] <- sections = section
  | (_:_) <- sections = error $ "Multiple sections with index " ++ show idx
  | _     <- sections = error $ "No sections with index " ++ show idx
  where idx = fromElfSectionIndex i
        sections = filter (hasSectionIndex idx) (e^..elfSections)
        hasSectionIndex jdx section = elfSectionIndex section == idx

-- | Read a null-terminated byte string from memory given a 64-bit address.
readNTBSFromAddr :: Elf 64 -> Word64 -> B.ByteString
readNTBSFromAddr e ptr = B.takeWhile (/=0) $ readElfAddr e ptr 100

-- | Read a specified number of bytes from an elf section at a specific
-- address.
readElfSection :: ElfSection Word64 -> Word64 -> Word64 -> B.ByteString
readElfSection section addr size =
  B.take (fromIntegral size) (B.drop adjustedAddr (elfSectionData section))
  where adjustedAddr = fromIntegral $ addr - (elfSectionAddr section)

readElfAddr :: Elf 64 -> Word64 -> Word64 -> B.ByteString
readElfAddr e addr size = case sections of
                            (section:_) -> readElfSection section addr size
                            _           -> error "couldn't find section"
  where sections = filter (hasAddr addr) (e^..elfSections)
        hasAddr addr section
          = (elfSectionAddr section) <= addr &&
            addr < (elfSectionAddr section) + (elfSectionSize section)

------------------------------------------------------------------------
-- VTable datatype
-- TODO: add more information about the various fields

-- | RTTI datatype.
data RTTI = RTTI { rttiAddr :: Word64
                 , typeInfoAddr :: Word64 -- pointer to type_info vtable.
                 , rttiMangledName :: B.ByteString
                 , rttiMangledNameAddr :: Word64
                 , rttiParentRTTIAddrs :: [Word64]
                 } deriving (Eq)

instance Show RTTI where
  show (RTTI rttiAddr _ rttiMangledName _ rttiParentRTTIAddrs) =
    "addr = " ++ (trimLeadingZeros . showPaddedHex) rttiAddr ++ ", " ++
    "name = " ++ show rttiMangledName ++ ", " ++
    "parentRTTIAddrs = " ++ intercalate ", " (map (trimLeadingZeros . showPaddedHex) rttiParentRTTIAddrs)

-- | VTable datatype.
data VTable = VTable { vTableAddr :: Word64
                     , vTableSize :: Word64
                     , vTableOffset :: Word64
                     -- ^ offset to top (TODO)
                     , vTableRTTI :: Maybe RTTI
                     , vTableFPtrs :: [Word64]
                     , vTableContents :: B.ByteString
                     } deriving (Eq)

instance Show VTable where
  show (VTable addr size offset rtti fptrs contents) =
    "VTable:\n" ++
    "  Address: " ++ trimLeadingZeros (showPaddedHex addr) ++ "\n" ++
    "  Size: " ++ show size ++ " bytes\n" ++
    "  Offset: " ++ show offset ++ "\n" ++
    "  RTTI: " ++ rttiString ++ "\n"
    -- "  Function addrs: " ++ intercalate "," (map (trimLeadingZeros . showPaddedHex)
    -- fptrs)
    where rttiString = case rtti of
                         Nothing -> "not present"
                         Just x -> show x
------------------------------------------------------------------------
-- Extracting VTables

-- | Get the vtable symbol table entries from the symbol table.
vTableEntries :: Elf 64 -> ElfSymbolTable Word64 -> [ElfSymbolTableEntry Word64]
vTableEntries e =
  filter (isVTableEntry e) . V.toList . elfSymbolTableEntries
  where
    isVTableEntry e ste = B.isPrefixOf "_ZTV" (steName ste)
                          && elfSectionName (section ste) == ".rodata"
                          -- ^ This is a guess. So far they've all been in .rodata.
    section ste = elfSection e (steIndex ste)
    -- TODO: ByteString has a IsString instance, so use B.fromString
--    ztv = "_ZTV"
--    rodata = ".rodata"
--    rodata = B.pack [46,114,111,100,97,116,97]

data InheritanceType = BaseInheritance
                     | SingleInheritance
                     | MultipleInheritance
  deriving (Show, Eq)

-- | Determine what kind of inheritance, given the vtable of the corresponding
-- typeinfo object.
typeInfoInheritance :: Elf 64 -> Word64 -> InheritanceType
typeInfoInheritance e addr =
  let addrRttiAddr = addr - 8
      rttiAddr = bsWord64le $ readElfAddr e addrRttiAddr 8
      addrNameAddr = rttiAddr + 8
      nameAddr = bsWord64le $ readElfAddr e addrNameAddr 8
      name = C.unpack $ readNTBSFromAddr e nameAddr
      -- TODO: maybe find a better way to do this...
      single   = isInfixOf "si_class_type_info"  name
      multiple = isInfixOf "vmi_class_type_info" name
  in case (single, multiple) of
       (False, False) -> BaseInheritance
       (True, _) -> SingleInheritance
       _         -> MultipleInheritance

-- | Build an RTTI datatype from an address
rttiFromPtr :: Elf 64 -> Word64 -> Maybe RTTI
rttiFromPtr e ptr
  | B.null contents = Nothing
  | otherwise = Just (RTTI ptr typeInfoAddr rttiName rttiNameAddr rttiParentRTTIAddrs)
  where contents = readElfAddr e ptr 128
        typeInfoAddr = (bsWord64le . B.take 8) contents
        rttiNameAddr = (bsWord64le . B.take 8 . B.drop 8) contents
        rttiName = readNTBSFromAddr e rttiNameAddr
        inheritanceType = typeInfoInheritance e typeInfoAddr
        rttiParentRTTIAddrs = case inheritanceType of
          BaseInheritance -> []
          SingleInheritance -> [(bsWord64le . B.take 8 . B.drop 16) contents]
          MultipleInheritance ->
            let numParents = (fromIntegral . toInteger . bsWord32le . B.take 4 . B.drop 20) contents
                allParents = B.take (16*numParents) $ B.drop 24 contents
                parentChunks = map (take 8) $ chunksOf 16 $ B.unpack allParents
                parents = map bsWord64le $ map B.pack $ parentChunks
            in parents

-- | Build a VTable datatype from an elf object and a particular symbol table entry.
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
        section = elfSection e (steIndex ste)
        contents = readElfAddr e addr size
        rttiPtr = (bsWord64le . B.take 8 . B.drop 8) contents
        rtti = rttiFromPtr e rttiPtr
        fptrs = (map bsWord64le . map B.pack . chunksOf 8 . B.unpack . B.drop 16) contents
        -- TODO: fptrs is broken for multiple inheritance

-- | Get a list of all the VTables in an Elf 64 object.
vTablesFromElf64 :: Elf 64 -> Either String [VTable]
vTablesFromElf64 e =
  case elfSymtab e of
    []     -> Left "No symbol table; can't find vtables yet! (not implemented)"
    (s:[]) -> Right $ map (vTableFromSTE e) $ vTableEntries e s
    _      -> Left "Need exactly one symbol table in ELF file"

------------------------------------------------------------------------
-- | main

main = do
  args <- getArgs
  case args of
    (bFileName:_) -> do
      e <- readElf64 bFileName
      case vTablesFromElf64 e of
        Left s -> putStrLn s
        Right vtables -> mapM_ print vtables
    _ -> putStrLn "Please supply a file to dump."
