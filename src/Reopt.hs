{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt
  ( readSomeElf
  , readElf64
  , parseElf64
  , dumpDisassembly
  , showPaddedHex
    -- * Architecture info
  , SomeArchitectureInfo(..)
    -- * Code discovery
  , discoverBinary
  , runCompleteDiscovery
  ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Either
import           Data.ElfEdit
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Word
import           Flexdis86
import           Numeric
import           System.Exit
import           System.IO
import           System.IO.Error

import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery hiding (AddrSymMap)
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86

import qualified Reopt.CFG.LLVM as LLVM


#ifdef SUPPORT_ARM
import qualified Data.VEX.FFI
import           Data.Macaw.ARM
#endif

showUsage :: IO ()
showUsage = hPutStrLn stderr "For help on using reopt, run \"reopt --help\"."

------------------------------------------------------------------------
-- Resolve which symbols to include

-- | Resolve a hex string or other string as a address of symbol name.
resolveSymName :: String -> Either Word64 String
resolveSymName ('0':'x': nm) | [(w,"")] <- readHex nm = Left w
resolveSymName ('0':'X': nm) | [(w,"")] <- readHex nm = Left w
resolveSymName nm = Right nm

-- | Attempt to find the address of a string identifying a symbol
-- name, and return either the string if it cannot be resolved or the
-- address.
resolveSymAddr :: Memory w
               -> Map BS.ByteString [MemSegmentOff w]
                 -- ^ Map from symbol names in binary to their address.
              -> String
                 -- ^ The name of a symbol as a string.
              -> Either String [MemSegmentOff w]
resolveSymAddr mem symMap nm0 = addrWidthClass (memAddrWidth mem) $
  case resolveSymName nm0 of
    Left w ->
      case resolveAbsoluteAddr mem (fromIntegral w) of
        Just off -> Right [off]
        Nothing -> Left nm0
    Right nm -> do
      case Map.lookup (fromString nm) symMap of
         Just addrs -> Right addrs
         Nothing -> Left nm

resolveIncludeFn :: Memory w
                 -> Map BS.ByteString [MemSegmentOff w]
                    -- ^ Map from symbol names to addresses with name
                 -> [String] -- ^ Addresses to include
                 -> [String]
                 -> IO ([MemSegmentOff w], (MemSegmentOff w -> Bool))
resolveIncludeFn mem symMap [] excludeNames = do
  let (bad, excludeAddrs) = partitionEithers $ resolveSymAddr mem symMap  <$> excludeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList (concat excludeAddrs)
  pure ([], (`Set.notMember` s))
resolveIncludeFn mem symMap includeNames [] = do
  let (bad, includeAddrs) = partitionEithers $ resolveSymAddr mem symMap  <$> includeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList (concat includeAddrs)
  pure $ (concat includeAddrs, (`Set.member` s))
resolveIncludeFn _ _ _ _ = do
  fail "Cannot both include and exclude specific addresses."

------------------------------------------------------------------------
-- Architecture info

data SomeArchitectureInfo w =
  forall arch
    . ( w ~ RegAddrWidth (ArchReg arch)
      )
    => SomeArch (ArchitectureInfo arch)


------------------------------------------------------------------------
-- checkedReadFile

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
showElfParseErrors :: [ElfParseError]
                   -> IO ()
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
-- This will print to stderr and exit if path cannot be read.
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
-- Get binary information

getElfArchInfo :: Elf w -> IO (SomeArchitectureInfo w)
getElfArchInfo e =
  case (elfClass e, elfMachine e, elfOSABI e) of
    (ELFCLASS64, EM_X86_64, ELFOSABI_LINUX)   -> pure (SomeArch x86_64_linux_info)
    (ELFCLASS64, EM_X86_64, ELFOSABI_SYSV)    -> pure (SomeArch x86_64_linux_info)
    (ELFCLASS64, EM_X86_64, ELFOSABI_FREEBSD) -> pure (SomeArch x86_64_freeBSD_info)
#ifdef SUPPORT_ARM
    (ELFCLASS32, EM_ARM, ELFOSABI_SYSV) -> do
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure (SomeArch armArch32le)
    (ELFCLASS64, EM_AARCH64, ELFOSABI_SYSV) -> do
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure (SomeArch armArch64le)
#endif
    (cl, arch, abi) -> do
     let archName = case Map.lookup arch elfMachineNameMap of
                      Just nm -> nm
                      Nothing -> "unknown-abi(" ++ showHex (fromElfMachine arch) ")"
     hPutStrLn stderr
        $ "Do not support " ++ show (elfClassBitWidth cl) ++ "-bit "
        ++ archName ++ " " ++ show abi ++ " binaries."
     exitFailure

------------------------------------------------------------------------
-- Explore a control flow graph.

-- | Discover functions in an elf file.
--
--  Note. This prints warnings to stderr
runCompleteDiscovery :: LoadOptions
                     -- ^ Options controlling loading
                     -> DiscoveryOptions
                     -- ^ Options controlling discovery
                     -> Elf (ArchAddrWidth arch)
                     -> ArchitectureInfo arch
                     -> [String] -- ^ Included addresses
                     -> [String] -- ^ Excluded addresses
                     -> IO ( DiscoveryState arch
                           , LLVM.AddrSymMap (ArchAddrWidth arch)
                           , Map BS.ByteString [ArchSegmentOff arch]
                           )
runCompleteDiscovery loadOpts disOpt e ainfo includeAddr excludeAddr = do
  (warnings, mem, entry, symbols) <- either fail pure $
    resolveElfContents loadOpts e
  mapM_ (hPutStrLn stderr) warnings

  let addrSymMap = Map.fromList [ (memSymbolStart msym, memSymbolName msym) | msym <- symbols ]
  let symAddrMap = Map.fromListWith (++) [ (memSymbolName msym, [memSymbolStart msym]) | msym <- symbols ]
  (entries, fnPred) <- resolveIncludeFn mem symAddrMap includeAddr excludeAddr
  let initEntries = maybeToList entry ++ entries
  let initState
        = emptyDiscoveryState mem addrSymMap ainfo
        & markAddrsAsFunction InitAddr initEntries
  s <- completeDiscoveryState initState disOpt fnPred
  pure (s, addrSymMap, symAddrMap)

-- | Discover code in the binary identified by the given path.
discoverBinary :: FilePath
               -> LoadOptions -- ^ Options controlling loading
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> [String] -- ^ Included addresses
               -> [String] -- ^ Excluded addresses
               -> IO (Some DiscoveryState)
discoverBinary path loadOpts disOpt includeAddr excludeAddr = do
  Some e <- readSomeElf path
  -- Get architecture information for elf
  SomeArch ainfo <- getElfArchInfo e
  (s, _,_) <- runCompleteDiscovery loadOpts disOpt e ainfo includeAddr excludeAddr
  pure (Some s)

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
  let r = case mi of
            Nothing  -> take 20 b
            Just ins -> stringToFixedBuffer 21 b ++ "\t" ++ show (ppInstruction ins)
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
