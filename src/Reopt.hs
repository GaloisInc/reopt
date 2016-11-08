{-# LANGUAGE DataKinds #-}
module Reopt
  ( printDisassemblyLine
  , isCodeSection
  , instructionNames
  , printSectionDisassembly
--  , printExecutableAddressesInGlobalData
  , showBytes
  , slice
    -- * File reading
  , parseElf64
  , readElf64
  , readStaticElf
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ElfEdit
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Flexdis86
import           Numeric
import           System.Exit
import           System.IO
import           System.IO.Error


-- | @stringToFixedBuffer n s@ returns a string with length @n@ containing
-- @s@ or a prefix of @s@.  If @n@ exceeds the length of @s@, then additional
-- whitespace is appended to @s@.
stringToFixedBuffer :: Int -> String -> String
stringToFixedBuffer g s | g == n = s
                        | g < n = take g s
                        | otherwise = s ++ replicate (g-n) ' '
  where n = length s

-- | Slice part of bytestring.
slice :: Int -> Int -> B.ByteString -> B.ByteString
slice i n b = B.take n (B.drop i b)

-- | Show a given hexideimal number with a fixed width, adding
-- zeros as needed.
showPaddedHex :: (Integral a, Show a) => Int -> a -> String
showPaddedHex l v = assert (l >= n) $ replicate (l-n) '0' ++ s
  where s = showHex v ""
        n = length s

word8AsHex :: Word8 -> String
word8AsHex = showPaddedHex 2

showAddr64 :: Word64 -> String
showAddr64 = showPaddedHex 16

-- | Convert ByteString to a string of hex digits.
showBytes :: B.ByteString -> String
showBytes b = unwords (word8AsHex <$> B.unpack b)

-- | Retun number of digits required to show a given unsigned number in hex.
hexDigitsReq :: Bits a => a -> Int
hexDigitsReq b = go 1 (b `shiftR` 4)
  where go r v | popCount v == 0 = r
               | otherwise = go (r+1) (v `shiftR` 4)

-- | Number of digits required to print a 64-bit word in hex.
digits64 :: Int
digits64 = 64 `div` 4

trimForWord64Buffer :: Word64 -> Int -> String -> String
trimForWord64Buffer base n = drop d
  where m = hexDigitsReq (max base (base + fromIntegral n))
        d = digits64 - m

-- | Print out disasembly for a specific line.
printDisassemblyLine :: Word64 -- ^ Base address for section or segment.
                     -> B.ByteString -- ^ Data region for code.
                     -> DisassembledAddr
                     -> IO ()
printDisassemblyLine base buffer (DAddr i n mi) = do
  let o = base + fromIntegral i
  let trimFn = trimForWord64Buffer base (B.length buffer)
  let ppAddr = trimFn . showAddr64
  let b = showBytes $ slice i n buffer
  let next = o + fromIntegral n
  let r = case mi of
            Nothing  -> take 20 b
            Just ins -> stringToFixedBuffer 21 b ++ "\t" ++ show (ppInstruction next ins)
  putStrLn $ "  " ++ ppAddr o ++ ":\t" ++ r
  if n > 7
    then printDisassemblyLine base buffer $ DAddr (i+7) (n-7) Nothing
    else return ()

isCodeSection :: (Bits w, Eq w, Num w) => ElfSection w -> Bool
isCodeSection s = elfSectionFlags s .&. shf_execinstr == shf_execinstr

-- | Return name of instructions in section.
instructionNames :: [ElfSection Word64] -> Set String
instructionNames l = Set.fromList $ concatMap resolve l
  where resolve s = iiOp <$> sectionInstructions s

-- | Return all instructions in the given section.
sectionInstructions :: ElfSection Word64 -> [InstructionInstance]
sectionInstructions s = [ i | DAddr _ _ (Just i) <- dta ]
  where buffer = elfSectionData s
        dta = disassembleBuffer buffer

-- | Elf sections
printSectionDisassembly :: ElfSection Word64 -> IO ()
printSectionDisassembly s = do
  let nm = elfSectionName s
  let addr = elfSectionAddr s
  putStrLn $ "Disassembly of section " ++ nm ++ ":"
  putStrLn ""
  putStrLn $ (showPaddedHex 16 addr) ++ " <" ++ nm ++ ">:"
  let buffer = elfSectionData s
  let dta = disassembleBuffer buffer
  let pp da = do
        let base = elfSectionAddr s
        printDisassemblyLine base buffer da
  mapM_ pp dta
  putStrLn ""

{-
-- | This function prints out executable addresses found in global data
-- that are not obtained by traversing the code segments.  Useful for
-- identifying possible entry points via global data.
printExecutableAddressesInGlobalData :: Elf Word64 -> IO ()
printExecutableAddressesInGlobalData e = do
  mem <- either fail return $ memoryForElfSegments (knownNat :: NatRepr 64) e
  let exec_words :: [Word64]
      exec_words = [ w | w <- segmentAsWord64le =<< memSegments mem
                   , mem & addrHasPermissions w pf_x
                   ]
  -- Get list of assembly
  let entry_points = Set.fromList exec_words

  let sections = filter isCodeSection $ e^..elfSections
  let found_points = [ addr + fromIntegral i
                     | s <- sections
                     , let addr = elfSectionAddr s
                     , let buffer = elfSectionData s
                     , let dta = disassembleBuffer buffer
                     , DAddr i _ Just{} <- dta
                     ]
  let found_set = Set.fromList found_points
  let missing_entries :: [Word64]
      missing_entries = Set.toList $ entry_points `Set.difference` found_set
  print missing_entries
  print (length missing_entries)
  let cnt = length missing_entries
  putStrLn $ show cnt  ++ " possible jump targets found via address search, but"
  putStrLn $ "not found during when disassembling sections."
  putStrLn ""
  forM_ missing_entries $ \o -> do
    case findSegment o mem of
      Just s -> do
        let base = memBase s
        let buffer = memBytes s
        let offset = fromIntegral (o - base)
        let (n,mi) = tryDisassemble (B.drop offset buffer)
        let da = DAddr offset n mi
        printDisassemblyLine base buffer da

      Nothing -> do
        putStrLn $ showHex o " not found in segment."
-}

--------------------------------------------------------------------------------
-- Reading elf utilities

parseElf64 :: String
              -- ^ Name of output for error messages
           -> B.ByteString
              -- ^ Data to read
           -> IO (Elf Word64)
parseElf64 nm bs = do
  case parseElf bs of
    ElfHeaderError _ msg -> do
      hPutStrLn stderr $ "Error reading " ++ nm ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Elf32Res{} -> do
      hPutStrLn stderr "32-bit elf files are not yet supported."
      exitFailure
    Elf64Res l e -> do
      when (not (null l)) $ do
        hPutStrLn stderr $ "Recoverable errors occurred in reading elf file:"
      forM_ l $ \emsg -> do
        hPutStrLn stderr (show emsg)
      return e

showUsage :: IO ()
showUsage = do
  putStrLn "For help on using reopt, run \"reopt --help\"."

readElf64 :: FilePath
             -- ^ Filepath to rad.
          -> IO (Elf Word64)
readElf64 path = do
  when (null path) $ do
    hPutStrLn stderr "Please specify a path."
    showUsage
    exitFailure
  let h e | isDoesNotExistError e = do
            hPutStrLn stderr $ path ++ " does not exist."
            showUsage
            exitFailure
          | otherwise = throwIO e
  bs <- B.readFile path `catch` h
  parseElf64 path bs


readStaticElf :: FilePath -> IO (Elf Word64)
readStaticElf path = do
  e <- readElf64 path
  mi <- elfInterpreter e
  case mi of
    Nothing ->
      return ()
    Just{} ->
      fail "reopt does not yet support generating CFGs from dynamically linked executables."
  return e
