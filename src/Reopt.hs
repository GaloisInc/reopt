{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reopt
  ( readSomeElf
  , readElf64
  , parseElf64
  , dumpDisassembly
  , showPaddedHex
  ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ElfEdit
import           Data.Parameterized.Some
import           Data.Word
import           Flexdis86
import           Numeric
import           System.Exit
import           System.IO
import           System.IO.Error

showUsage :: IO ()
showUsage = hPutStrLn stderr "For help on using reopt, run \"reopt --help\"."

------------------------------------------------------------------------
-- checkedRedRFile

-- | This reads a file as a strict bytestring.
--
-- This will exit the program if failures occur.
checkedReadFile :: FilePath -> IO BS.ByteString
checkedReadFile path = do
  when (null path) $ do
    hPutStrLn stderr "Please specify a path."
    showUsage
    exitFailure
  let h e | isDoesNotExistError e = do
            hPutStrLn stderr $ path ++ " does not exist."
            showUsage
            exitFailure
          | isUserError e = do
            hPutStrLn stderr (ioeGetErrorString e)
            exitFailure
          | otherwise = do
            hPutStrLn stderr (show e)
            hPutStrLn stderr (show (ioeGetErrorType e))
            exitFailure
  BS.readFile path `catch` h

------------------------------------------------------------------------
-- Read an elf file

-- | Print errors that occured when reading to @stderr@.
showElfParseErrors :: (Integral (ElfWordType w), Show (ElfWordType w)) -- Eq (ElfWordType w), Num (ElfWordType w)
                   => [ElfParseError w] -> IO ()
showElfParseErrors l = do
  when (not (null l)) $ do
    hPutStrLn stderr $ "Recoverable errors occurred in reading elf file:"
    forM_ l $ \emsg -> do
      hPutStrLn stderr (show emsg)

parseElf64 :: String
              -- ^ Name of output for error messages
           -> BS.ByteString
              -- ^ Data to read
           -> IO (Elf 64)
parseElf64 nm bs = do
  case parseElfHeaderInfo bs of
    Left (_, msg) -> do
      hPutStrLn stderr $ "Could not parse Elf file " ++ nm ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Right (Elf32 _) -> do
      hPutStrLn stderr "32-bit elf files are not yet supported."
      exitFailure
    Right (Elf64 hdr) -> do
      let (l, e) = getElf hdr
      showElfParseErrors l
      return e

-- | Read a 64-bit elf file.
readElf64 :: FilePath
             -- ^ Filepath to rad.
          -> IO (Elf 64)
readElf64 path = checkedReadFile path >>= parseElf64 path

-- | Read an elf file from the path and write errors to standard error.
--
-- This will exit if path cannot be read.
readSomeElf :: FilePath -> IO (Some Elf)
readSomeElf path = do
  bs <- checkedReadFile path
  case parseElf bs of
    ElfHeaderError _ msg -> do
      hPutStrLn stderr $ "Error reading " ++ path ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Elf32Res l e -> do
      showElfParseErrors l
      return (Some e)
    Elf64Res l e -> do
      showElfParseErrors l
      return (Some e)

------------------------------------------------------------------------
-- Print disassembly

-- | Return number of digits required to show a given unsigned number in hex.
hexDigitsReq :: Bits a => a -> Int
hexDigitsReq b = go 1 (b `shiftR` 4)
  where go r v | popCount v == 0 = r
               | otherwise = go (r+1) (v `shiftR` 4)


trimForWord64Buffer :: Word64 -> Int -> String -> String
trimForWord64Buffer base n s = drop d s
  where m = hexDigitsReq (max base (base + fromIntegral n))
        d = 16 - m

-- | Show a given hexideimal number with a fixed width, adding
-- zeros as needed.
showPaddedHex :: (FiniteBits a, Integral a, Show a) => a -> String
showPaddedHex v = assert (l >= n) $ replicate (l-n) '0' ++ s
  where l = finiteBitSize v `shiftR` 2
        s | v >= 0 = showHex v ""
          | otherwise = error "showPaddedHex given negtive number"
        n = length s

-- | Convert ByteString to a string of hex digits.
showBytes :: BS.ByteString -> String
showBytes b = unwords (showPaddedHex <$> BS.unpack b)

-- | Slice part of bytestring.
slice :: Int -> Int -> BS.ByteString -> BS.ByteString
slice i n b = BS.take n (BS.drop i b)

-- | @stringToFixedBuffer n s@ returns a string with length @n@ containing
-- @s@ or a prefix of @s@.  If @n@ exceeds the length of @s@, then additional
-- whitespace is appended to @s@.
stringToFixedBuffer :: Int -> String -> String
stringToFixedBuffer g s | g == n = s
                        | g < n = take g s
                        | otherwise = s ++ replicate (g-n) ' '
  where n = length s

-- | Print out disasembly for a specific line.
printX86DisassemblyLine :: Word64 -- ^ Base address for section or segment.
                     -> BS.ByteString -- ^ Data region for code.
                     -> DisassembledAddr -- ^ Output from flexdis
                     -> IO ()
printX86DisassemblyLine base buffer (DAddr i n mi) = do
  let o = base + fromIntegral i
  let ppAddr x = trimForWord64Buffer base (BS.length buffer) (showPaddedHex x)
  let b = showBytes $ slice i n buffer
  let next = o + fromIntegral n
  let r = case mi of
            Nothing  -> take 20 b
            Just ins -> stringToFixedBuffer 21 b ++ "\t" ++ show (ppInstruction next ins)
  putStrLn $ "  " ++ ppAddr o ++ ":\t" ++ r
  when (n > 7) $ do
    printX86DisassemblyLine base buffer $ DAddr (i+7) (n-7) Nothing

-- | Elf sections
printX86SectionDisassembly :: BSC.ByteString -> Word64 -> BS.ByteString -> IO ()
printX86SectionDisassembly nm addr buffer = do
  putStrLn $ "Disassembly of section " ++ BSC.unpack nm ++ ":"
  putStrLn ""
  putStrLn $ showPaddedHex addr ++ " <" ++ BSC.unpack nm ++ ">:"
  let dta = disassembleBuffer buffer
  mapM_ (printX86DisassemblyLine addr buffer) dta
  putStrLn ""

isCodeSection :: (Bits w, Num w) => ElfSection w -> Bool
isCodeSection s = elfSectionFlags s .&. shf_execinstr == shf_execinstr

dumpDisassembly :: FilePath -> IO ()
dumpDisassembly path = do
  bs <- checkedReadFile path
  e <- parseElf64 path bs
  let sections = filter isCodeSection $ e^..elfSections
  when (null sections) $ do
    putStrLn "Binary contains no executable sections."
  forM_ sections $ \s -> do
    printX86SectionDisassembly (elfSectionName s) (elfSectionAddr s) (elfSectionData s)
