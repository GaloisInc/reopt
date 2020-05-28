{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reopt
  ( readElf64
  , parseElf64
  , showPaddedHex
  , checkedReadFile
  , isCodeSection
  , printX86SectionDisassembly
    -- * SymAddrMap
  , SymAddrMap
  , SymAddrMapLookupError(..)
  , symAddrMapLookup
    -- * Architecture info
  , SomeArchitectureInfo(..)
    -- * Code discovery
  , discoverBinary
    -- * Function recovery
  , Reopt.Header.Header
  , Reopt.Header.emptyHeader
  , Reopt.Header.parseHeader
  , RecoveredModule(..)
  , GetFnsLogEvent(..)
    -- * Redirections
  , ControlFlowTargetSet
  , discoveryControlFlowTargets
  , addrRedirection
  , mergeAndWrite
    -- * LLVM
  , LLVMVersion
  , versionOfString
  , LLVMConfig
  , llvmAssembly
  , latestLLVMConfig
  , getLLVMConfig
  , compileLLVM
      -- * X86 specific
  , X86OS(..)
  , osPersonality
  , osLinkName
  , discoverX86Elf
    -- * Utility
  , copyrightNotice
    -- * Re-exports
  , Data.Macaw.Memory.ElfLoader.LoadOptions(..)
  ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
--import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Either
import           Data.ElfEdit
import qualified Data.ElfEdit as Elf
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import qualified Data.Set as Set
import           Data.String
import qualified Data.Vector as V
import           Data.Word
import qualified Flexdis86 as F
import           Numeric
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import qualified Text.PrettyPrint.HughesPJ as HPJ
import           Text.Printf (printf)

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.SyscallInfo
import           Data.Macaw.X86.X86Reg

import           Reopt.CFG.FnRep
import           Reopt.CFG.FunctionCheck
import           Reopt.CFG.LLVM (LLVMArchSpecificOps, LLVMGenOptions, moduleForFunctions)
import           Reopt.CFG.Recovery
import qualified Reopt.ExternalTools as Ext
import           Reopt.Header
import           Reopt.Hints
import           Reopt.Relinker
import qualified Reopt.VCG.Annotations as Ann

#ifdef SUPPORT_ARM
import qualified Data.VEX.FFI
import           Data.Macaw.VEX.AArch32 (armArch32le)
import           Data.Macaw.VEX.AArch64 (armArch64le)
#endif

copyrightNotice :: String
copyrightNotice = "Copyright 2014-19 Galois, Inc."


showUsage :: Handle -> IO ()
showUsage h = hPutStrLn h "For help on using reopt, run \"reopt --help\"."

------------------------------------------------------------------------
-- Resolve which symbols to include

-- | Map from defined symbol names to the address of the symbol
newtype SymAddrMap w = SymAddrMap (Map BS.ByteString [MemSegmentOff w])

symAddrMapFromList :: [MemSymbol w] -> SymAddrMap w
symAddrMapFromList = SymAddrMap . foldl ins Map.empty
  where ins m msym =
          let nm = memSymbolName msym
              addr = memSymbolStart msym
           in seq addr $ Map.insertWith (\_ old -> addr:old) nm [addr] m

-- | Error code if @symAddrMapLookup@ fails.
data SymAddrMapLookupError
   = SymAddrMapNotFound
   | SymAddrMapAmbiguous

-- | Lookup entry in symbol to address map.
symAddrMapLookup :: SymAddrMap w -> BS.ByteString -> Either SymAddrMapLookupError (MemSegmentOff w)
symAddrMapLookup (SymAddrMap m) nm =
  case Map.lookup nm m of
    Nothing -> Left SymAddrMapNotFound
    Just [a] -> Right a
    Just _ -> Left SymAddrMapAmbiguous

-- | Attempt to find the address of a string identifying a symbol
-- name, and return either the string if it cannot be resolved or the
-- address.
resolveSymAddr :: Memory w
                  -- ^ Loaded memory object.
               -> RegionIndex
                  -- ^ Region index for resolving addresses.
               -> SymAddrMap w
                 -- ^ Map from symbol names in binary to associated addresses.
               -> String
                  -- ^ The name of a symbol as a string.
               -> Either String (MemSegmentOff w)
resolveSymAddr mem regIdx symMap nm0 = addrWidthClass (memAddrWidth mem) $
  case resolveSymName nm0 of
    AddrIdent w ->
      case resolveRegionOff mem regIdx (fromIntegral w) of
        Just off -> Right off
        Nothing -> Left nm0
    SymbolIdent nm ->
      case symAddrMapLookup symMap nm of
        Left _ -> Left nm0
        Right a -> Right a

resolveIncludeFn :: Memory w
                    -- ^ Memory for binary
                 -> RegionIndex
                    -- ^ Region index to use for resolving addresses as numbers.
                 -> SymAddrMap w
                    -- ^ Map from symbol names to addresses with name.
                    --
                    -- Note. This includes local and weak symbols.
                    -- Currently there is no way to disambiguate which
                    -- symbol is referred to, but that is likely ok.
                 -> [String] -- ^ Addresses to include
                 -> [String] -- ^ Addresses to exclude.
                 -> IO ([MemSegmentOff w], (MemSegmentOff w -> Bool))
resolveIncludeFn mem ridx symMap [] excludeNames = do
  let (bad, excludeAddrs) = partitionEithers $ resolveSymAddr mem ridx symMap <$> excludeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList excludeAddrs
  pure ([], (`Set.notMember` s))
resolveIncludeFn mem ridx symMap includeNames [] = do
  let (bad, includeAddrs) = partitionEithers $ resolveSymAddr mem ridx  symMap  <$> includeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList includeAddrs
  pure $ (includeAddrs, (`Set.member` s))
resolveIncludeFn _ _ _ _ _ = do
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
-- This will exit the program if failures occur, and also write to stderr.
checkedReadFile :: FilePath -> IO BS.ByteString
checkedReadFile path = do
  when (null path) $ do
    hPutStrLn stderr "Please specify a path."
    showUsage stderr
    exitFailure
  let h e | isDoesNotExistError e = do
            hPutStrLn stderr $ path ++ " does not exist."
            showUsage stderr
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
parseElf64 nm bs =
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
readSomeElf :: FilePath -> IO (Some ElfHeaderInfo)
readSomeElf path = do
  bs <- checkedReadFile path
  case parseElfHeaderInfo bs of
    Left (_, msg) -> do
      hPutStrLn stderr $ "Error reading " ++ path ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Right (Elf32 hdr) ->
      pure $! Some hdr
    Right (Elf64 hdr) ->
      pure $! Some hdr

------------------------------------------------------------------------
-- Get binary information

getElfArchInfo :: ElfClass w -> ElfMachine -> ElfOSABI -> IO (SomeArchitectureInfo w)
getElfArchInfo cl arch abi =
  case (cl, arch, abi) of
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
    _ -> do
     let archName = case Map.lookup arch elfMachineNameMap of
                      Just nm -> nm
                      Nothing -> "unknown-abi(" ++ showHex (fromElfMachine arch) ")"
     hPutStrLn stderr
        $ "Do not support " ++ show (elfClassBitWidth cl) ++ "-bit "
        ++ archName ++ " " ++ show abi ++ " binaries."
     exitFailure

------------------------------------------------------------------------
-- Explore a control flow graph.

elfInstances :: ElfHeaderInfo w
             -> ((MemWidth w, Integral (ElfWordType w), Show (ElfWordType w)) => a)
             -> a
elfInstances hdr x =
  case Elf.headerClass (Elf.header hdr) of
    ELFCLASS32 -> x
    ELFCLASS64 -> x

-- | Discover functions in an elf file.
--
--  Note. This prints warnings to stderr
runCompleteDiscovery :: LoadOptions
                     -- ^ Option to load the binary at the given address
                     -> DiscoveryOptions
                     -- ^ Options controlling discovery
                     -> ElfHeaderInfo (ArchAddrWidth arch)
                     -> ArchitectureInfo arch
                     -> [String] -- ^ Included addresses
                     -> [String] -- ^ Excluded addresses
                     -> IO ( Elf (ArchAddrWidth arch)
                           , DiscoveryState arch
                           , AddrSymMap (ArchAddrWidth arch)
                           , SymAddrMap (ArchAddrWidth arch)
                           )
runCompleteDiscovery loadOpts disOpt hdrInfo ainfo includeAddr excludeAddr = elfInstances hdrInfo $ do
  let hdr = Elf.header hdrInfo
  case Elf.headerType hdr of
    -- This is for object files.
    Elf.ET_REL -> do
      case loadOffset loadOpts of
        Nothing -> pure ()
        Just _ -> hPutStrLn stderr $ "Ignoring load offset for object file as there is no global base address."

      let (l, elfFile) = getElf hdrInfo
      showElfParseErrors l
      (mem, secMap, warnings) <-
        case memoryForElfSections elfFile of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings
      let isRelevant ste = Elf.steType ste == Elf.STT_FUNC
--      let secIdxMap  = Map.fromList [ (elfSectionIndex s, s) | s <- elfFile^..Elf.elfSections ]
--      let secNameMap = Map.fromList [ (elfSectionName s, s)  | s <- elfFile^..Elf.elfSections ]

      let (symErrs, symbols) = resolveElfFuncSymbols mem secMap isRelevant elfFile

      mapM_ (hPutStrLn stderr . show) symErrs

      let addrSymMap = Map.fromList [ (memSymbolStart msym, memSymbolName msym) | msym <- symbols ]
      let symAddrMap = symAddrMapFromList symbols
      -- Get initial entries and predicate for exploring
      -- TODO: Check this.
      let regIdx :: RegionIndex
          regIdx = 1

      (allEntries, fnPred) <- resolveIncludeFn mem regIdx symAddrMap includeAddr excludeAddr
      let initState
            = emptyDiscoveryState mem addrSymMap ainfo
            & markAddrsAsFunction InitAddr allEntries
      s <- completeDiscoveryState initState disOpt fnPred
      pure (elfFile, s, addrSymMap, symAddrMap)
    -- Static executable
    Elf.ET_EXEC -> do
      case loadOffset loadOpts of
        Nothing -> pure ()
        Just _ -> hPutStrLn stderr $ "Ignoring load offset for unrelocatable executable."
      let (l, elfFile) = getElf hdrInfo
      showElfParseErrors l
      (mem, secMap, warnings) <-
        case memoryForElfSegments defaultLoadOptions elfFile of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings
      let isRelevant ste = Elf.steType ste == Elf.STT_FUNC
      let (symErrs, symbols) = resolveElfFuncSymbols mem secMap isRelevant elfFile
      mapM_ (hPutStrLn stderr . show) symErrs
      let addrSymMap = Map.fromList [ (memSymbolStart msym, memSymbolName msym) | msym <- symbols ]
      let symAddrMap = symAddrMapFromList symbols
      -- Get initial entries and predicate for exploring
      (entries, fnPred) <- resolveIncludeFn mem 0 symAddrMap includeAddr excludeAddr
      let entry = Elf.headerEntry hdr
      allEntries <-
        case resolveRegionOff mem 0 (fromIntegral entry) of
          Nothing -> do
            hPutStrLn stderr $ "Could not resolve entry point 0x" ++ showHex entry ""
            pure entries
          Just v -> do
            pure (v:entries)
      let initState
            = emptyDiscoveryState mem addrSymMap ainfo
            & markAddrsAsFunction InitAddr allEntries
      s <- completeDiscoveryState initState disOpt fnPred
      pure (elfFile, s, addrSymMap, symAddrMap)
    -- This is a shared library or position-independent executable.
    Elf.ET_DYN -> do
      let (l, elfFile) = getElf hdrInfo
      showElfParseErrors l
      (mem, secMap, warnings) <-
        case memoryForElfSegments loadOpts elfFile of
          Left errMsg -> hPutStrLn stderr errMsg *> exitFailure
          Right r -> pure r
      mapM_ (hPutStrLn stderr . show) warnings
      let isRelevant ste = Elf.steType ste == Elf.STT_FUNC
      let (symErrs, symbols) = resolveElfFuncSymbols mem secMap isRelevant elfFile
      let addrSymMap = Map.fromList [ (memSymbolStart msym, memSymbolName msym) | msym <- symbols ]
      let symAddrMap = symAddrMapFromList symbols
      -- Get initial entries and predicate for exploring
      let regIdx :: RegionIndex
          regIdx = if isJust (loadOffset loadOpts) then 0 else 1
      (entries, fnPred) <- resolveIncludeFn mem regIdx symAddrMap includeAddr excludeAddr
      mapM_ (hPutStrLn stderr . show) symErrs
      let entry = Elf.headerEntry hdr
      let adjEntry =
            case loadOffset loadOpts of
              Nothing -> toInteger entry
              Just o -> toInteger o + toInteger entry
      allEntries <-
        case resolveRegionOff mem regIdx (fromInteger adjEntry) of
          Nothing -> do
            hPutStrLn stderr $ "Could not resolve entry point: " ++ showHex entry ""
            pure entries
          Just v -> do
            pure (v:entries)
      let initState
            = emptyDiscoveryState mem addrSymMap ainfo
            & markAddrsAsFunction InitAddr allEntries
      s <- completeDiscoveryState initState disOpt fnPred
      pure (elfFile, s, addrSymMap, symAddrMap)
    Elf.ET_CORE -> do
      hPutStrLn stderr "Reopt does not support loading core files."
      exitFailure
    tp -> do
      hPutStrLn stderr $ "Reopt does not support loading elf files with type " ++ show tp ++ "."
      exitFailure

-- | Discover code in the binary identified by the given path.
discoverBinary :: FilePath
               -> LoadOptions
                  -- ^ Option to load the binary at the given address
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> [String] -- ^ Included addresses
               -> [String] -- ^ Excluded addresses
               -> IO (Some DiscoveryState)
discoverBinary path loadOpts disOpt includeAddr excludeAddr = do
  Some hdrInfo <- readSomeElf path
  let hdr = header hdrInfo
  -- Get architecture information for elf
  SomeArch ainfo <- getElfArchInfo (headerClass hdr) (headerMachine hdr) (headerOSABI hdr)
  (_, s, _,_) <- runCompleteDiscovery loadOpts disOpt hdrInfo ainfo includeAddr excludeAddr
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
printX86DisassemblyLine :: Handle  -- ^ Handle to write to
                        -> Word64 -- ^ Base address for section or segment.
                        -> BS.ByteString -- ^ Data region for code.
                        -> F.DisassembledAddr -- ^ Output from flexdis
                        -> IO ()
printX86DisassemblyLine h base buffer (F.DAddr i n mi) = do
  let o = base + fromIntegral i
  let ppAddr x = trimForWord64Buffer base (BS.length buffer) (showPaddedHex x)
  let b = showBytes $ slice i n buffer
  let r = case mi of
            Nothing  -> take 20 b
            Just ins -> stringToFixedBuffer 21 b ++ "\t" ++ show (F.ppInstruction ins)
  hPutStrLn h $ "  " ++ ppAddr o ++ ":\t" ++ r
  when (n > 7) $ do
    printX86DisassemblyLine h base buffer $ F.DAddr (i+7) (n-7) Nothing

-- | Print all the disassembly for a buffer to stdout.
printX86SectionDisassembly :: Handle
                           -> BSC.ByteString
                           -> Word64
                           -> BS.ByteString
                           -> IO ()
printX86SectionDisassembly h nm addr buffer = do
  hPutStrLn h $ "Disassembly of section " ++ BSC.unpack nm ++ ":"
  hPutStrLn h ""
  hPutStrLn h $ showPaddedHex addr ++ " <" ++ BSC.unpack nm ++ ">:"
  let dta = F.disassembleBuffer buffer
  mapM_ (printX86DisassemblyLine h addr buffer) dta
  hPutStrLn h ""

isCodeSection :: (Bits w, Num w) => ElfSection w -> Bool
isCodeSection s = elfSectionFlags s .&. shf_execinstr == shf_execinstr

------------------------------------------------------------------------
-- X86-specific functions

-- | Returns information about the registers needed and modified by a
-- x86 terminal statement.
summarizeX86TermStmt :: SyscallPersonality
                     -> ComputeArchTermStmtEffects X86_64 ids
summarizeX86TermStmt _ Hlt _ =
  ArchTermStmtRegEffects { termRegDemands = []
                         , termRegTransfers = []
                         }
summarizeX86TermStmt _ UD2 _ =
  ArchTermStmtRegEffects { termRegDemands = []
                         , termRegTransfers = []
                         }
summarizeX86TermStmt sysp X86Syscall proc_state = do
  -- Compute the arguments needed by the function
  let argRegs
        | BVValue _ call_no <- proc_state^.boundValue syscall_num_reg
        , Just (_,_,argtypes) <- Map.lookup (fromIntegral call_no) (spTypeInfo sysp) =
            take (length argtypes) syscallArgumentRegs
        | otherwise =
            syscallArgumentRegs
  let callRegs = [Some sp_reg] ++ Set.toList x86CalleeSavedRegs
  ArchTermStmtRegEffects { termRegDemands = Some <$> argRegs
                         , termRegTransfers = callRegs
                         }

x86DemandInfo :: SyscallPersonality
              -> ArchDemandInfo X86_64
x86DemandInfo sysp =
  ArchDemandInfo { functionArgRegs = [Some RAX]
                                     ++ (Some <$> x86ArgumentRegs)
                                     ++ (Some <$> x86FloatArgumentRegs)
                 , functionRetRegs = ((Some <$> x86ResultRegs) ++ (Some <$> x86FloatResultRegs))
                 , calleeSavedRegs = x86CalleeSavedRegs
                 , computeArchTermStmtEffects = summarizeX86TermStmt sysp
                 , demandInfoCtx = x86DemandContext
                 }

------------------------------------------------------------------------
-- LLVMVersion

newtype LLVMVersion = LLVMVersion [Integer]
  deriving (Eq, Ord)

-- | Parse a string as a version
versionOfString :: String -> Maybe LLVMVersion
versionOfString s = do
  let (f,r) = span (/= '.') s
  case (reads f, r) of
    ([(v,"")], [])
      | v >= 0 ->
        if v == 0 then
          Just (LLVMVersion [])
        else
          Just (LLVMVersion [v])
    ([(v,"")], '.':rest)
      | v >= 0 -> do
        LLVMVersion l <- versionOfString rest
        if null l && v == 0 then
          pure (LLVMVersion [])
        else
          pure (LLVMVersion (v : l))
    _ -> Nothing


instance IsString LLVMVersion where
  fromString s =
    case versionOfString s of
      Just v -> v
      Nothing -> error $ "Could not interpret " ++ show s ++ " as a version."

type LLVMConfig = LPP.Config

-- | Configuration for LLVM 3.5 - 3.6
llvm35Config :: LLVMConfig
llvm35Config =
  LPP.Config { LPP.cfgLoadImplicitType = True
             , LPP.cfgGEPImplicitType  = True
             , LPP.cfgUseDILocation    = False
             }

-- | Configuration for LLVM 3.7 & 3.8
latestLLVMConfig :: LLVMConfig
latestLLVMConfig =
  LPP.Config { LPP.cfgLoadImplicitType = False
             , LPP.cfgGEPImplicitType  = False
             , LPP.cfgUseDILocation    = True
             }

llvmVersionMap :: Map LLVMVersion LLVMConfig
llvmVersionMap = Map.fromList
  [ (,) "3.5.0" llvm35Config
  , (,) "3.7.0" latestLLVMConfig
  ]

--  | Get the LLVM LLVM config associated with a version
getLLVMConfig :: LLVMVersion -> Maybe LLVMConfig
getLLVMConfig v = snd <$> Map.lookupLE v llvmVersionMap

-- | Pretty print an LLVM module using the format expected by the given LLVM version.
ppLLVM :: LPP.Config -> L.Module -> HPJ.Doc
ppLLVM c m = LPP.withConfig c $ LPP.ppModule m

------------------------------------------------------------------------
-- Execution

data X86OS = Linux | FreeBSD

instance Show X86OS where
  show Linux = "Linux"
  show FreeBSD = "FreeBSD"

osPersonality :: X86OS -> SyscallPersonality
osPersonality Linux = linux_syscallPersonality
osPersonality FreeBSD = freeBSD_syscallPersonality

osArchitectureInfo :: X86OS -> ArchitectureInfo X86_64
osArchitectureInfo Linux = x86_64_linux_info
osArchitectureInfo FreeBSD = x86_64_freeBSD_info

-- | Return the name to pass the linker for this architecture.
osLinkName :: X86OS -> String
osLinkName Linux = "x86_64-unknown-linux-gnu"
osLinkName FreeBSD = "x86_64-unknown-freebsd-elf"

getX86ElfArchInfo :: ElfMachine -> ElfOSABI -> IO X86OS
getX86ElfArchInfo EM_X86_64 ELFOSABI_LINUX   = pure Linux
getX86ElfArchInfo EM_X86_64 ELFOSABI_SYSV    = pure Linux
getX86ElfArchInfo EM_X86_64 ELFOSABI_FREEBSD = pure FreeBSD
getX86ElfArchInfo EM_X86_64 ELFOSABI_NETBSD  = pure FreeBSD
getX86ElfArchInfo m abi = do
  hPutStrLn stderr $ printf "Do not support %s %s binaries." (show m) (show abi)
  exitFailure

-- | Map memory addresses to the associated type for that address.
type FunctionTypeMap arch = Map BS.ByteString (FunctionType arch)

-- | This updates the map by adding the types for this function along
-- with the types for all directly referenced functions.
--
-- It calls `error` if any of the functions have inconsistent typing,
-- as this error should have been discovered during function recovery.
getReferencedFunctions :: forall arch
                       .  ( Eq (FunctionType arch)
                          , Show (FunctionType arch)
                          , MemWidth (ArchAddrWidth arch)
                          , FoldableFC (ArchFn arch)
                          , FoldableF (FnArchStmt arch)
                          )
                       => Set.Set BS.ByteString
                       -- ^ Functions with definitions that we do not
                       -- need declarations for.
                       -> FunctionTypeMap arch
                       -> Function arch
                       -> FunctionTypeMap arch
getReferencedFunctions excluded m0 f =
    foldl' (foldFnValue findReferencedFunctions) m0 (fnBlocks f)
  where findReferencedFunctions :: FunctionTypeMap arch
                                -> FnValue arch tp
                                -> FunctionTypeMap arch
        findReferencedFunctions m (FnFunctionEntryValue ft nm) =
          insertAddr nm ft m
        findReferencedFunctions m _ = m

        insertAddr :: BS.ByteString
                   -> FunctionType arch
                   -> FunctionTypeMap arch
                   -> FunctionTypeMap arch
        insertAddr nm ft m
          | Set.member nm excluded = m
          | otherwise =
            case Map.lookup nm m of
              Just ft' | ft /= ft' ->
                         error $ BSC.unpack nm ++ " has incompatible types:\n"
                              ++ show ft  ++ "\n"
                              ++ show ft' ++ "\n"
                       | otherwise -> m
              _ -> Map.insert nm ft m

-- | List of annotations
type AnnotatedFuns = [(BS.ByteString, X86FunTypeInfo)]

-- | An error from parsing the external header for arguments
data HeaderAnnError
   = OutOfGPRegs !String
     -- ^ @OutOfGPRegs The argument @nm
   | UnsupportedArgType !String !HdrType
   | UnsupportedReturnType !HdrType
   | VarArgsUnsupported

-- | Pretty print for header errors.
showHeaderAnnError :: BSC.ByteString -> HeaderAnnError -> String
showHeaderAnnError fnm (OutOfGPRegs _) =
  printf "Ignoring header type on %s: Only stack arguments supported and no registers available." (BSC.unpack fnm)
showHeaderAnnError fnm (UnsupportedArgType _ tp) =
  printf "Ignoring header type on %s: Do not support argument type %s." (BSC.unpack fnm) (ppHdrType tp)
showHeaderAnnError fnm (UnsupportedReturnType tp) =
  printf "Ignoring header type on %s: Do not support return argument type %s." (BSC.unpack fnm) (ppHdrType tp)
showHeaderAnnError fnm VarArgsUnsupported =
  printf "Ignoring header type on %s: Do not support vararg functions." (BSC.unpack fnm)

-- |  State monad for resolving arguments.
data ArgResolverState = ARS { arsPrev :: [X86ArgInfo]
                              -- ^ Arguments identified in reverse order.
                            , arsNextGPP :: [F.Reg64]
                              -- ^ General purpose registers still
                              -- available for arguments.
                            }

type ArgResolver = StateT ArgResolverState (Except HeaderAnnError)

-- | Attempt to resolve what register to associate with an argument.
resolveArgType :: String  -- ^ Argument name.
               -> HdrType -- ^ Initial type for display purposes.
               -> HdrType -- ^ Type Initial type for display purposes.
               -> ArgResolver ()
resolveArgType nm initType tp0 =
  case tp0 of
    UInt64HdrType -> do
      regs <- gets arsNextGPP
      case regs of
        [] ->
          throwError $ OutOfGPRegs nm
        (r:rest) -> do
          modify $ \s -> s { arsPrev = ArgBV64 r : arsPrev s
                           , arsNextGPP = rest
                           }
    TypedefHdrType _ tp ->
      resolveArgType nm initType tp
    _ -> do
      throwError $ UnsupportedArgType nm initType

-- | This parses the types extracted from header function argumnts to
-- the machine code registers that the function will expect.
argsToRegisters :: Int -- ^ Number of arguments processed so far.
                -> V.Vector HdrFunArg
                   -- ^ Remaining arguments to parse
                -> ArgResolver ()
argsToRegisters cnt args
  | cnt >= V.length args = pure ()
  | otherwise = do
      let arg = args V.! cnt
      let nm = fromMaybe ("arg" ++ show cnt) (funArgName arg)
      resolveArgType nm (funArgType arg) (funArgType arg)
      argsToRegisters (cnt+1) args

parseReturnType :: HdrType -> Either HeaderAnnError [X86RetInfo]
parseReturnType tp0 =
  case tp0 of
    VoidHdrType ->
      Right []
    UInt64HdrType ->
      Right [RetBV64 F.RAX]
    PtrHdrType _ ->
      Right [RetBV64 F.RAX]
    TypedefHdrType _ tp
      | Right r <- parseReturnType tp ->
        Right r
    _ -> Left $ UnsupportedReturnType tp0

-- | This checks whether any of the symbols in the map start with the given string as a prefix.
isUsedPrefix :: BSC.ByteString -> AddrSymMap w -> Bool
isUsedPrefix prefix m = any (\nm -> prefix `BSC.isPrefixOf` nm) (Map.elems m)

-- | Name of recovered function when no function exists.
nosymFunctionName :: BSC.ByteString -> MemSegmentOff w -> BSC.ByteString
nosymFunctionName prefix segOff =
  let addr = segoffAddr segOff
   in prefix <> "_" <> BSC.pack (show (addrBase addr)) <> "_" <> BSC.pack (show (addrOffset addr))

-- | Returns name of recovered function.
recoveredFunctionName :: MemWidth w
                      => AddrSymMap w
                      -- ^ Maps addresses of symbols to the associated symbol name.
                      -> BSC.ByteString
                      -- ^ Prefix to use for automatically generated symbols.
                      -- To be able to distinguish symbols, this should not be
                      -- a prefix for any of the symbols in the map.
                      -> MemSegmentOff w
                      -> BSC.ByteString
recoveredFunctionName m prefix segOff =
  case Map.lookup segOff m of
    Just sym -> sym
    Nothing -> nosymFunctionName prefix segOff

-- | Map x86 function type to known functon abi.
--
-- This is used for global function argument analysis which doesn't yet support vararg
-- functions such as printf.
toKnownFunABI :: X86FunTypeInfo -> [KnownFunABI X86Reg]
toKnownFunABI (X86NonvarargFun args rets) =
  [KnownFnABI { kfArguments = argReg <$> args
              , kfReturn = retReg <$> rets
              }
  ]
toKnownFunABI X86PrintfFun = []

-- | Resolve annotations on funbction types from C header, and return
-- warnings and the list of functions.
resolveHeaderFuns :: Header
                  -> ([(BSC.ByteString, HeaderAnnError)], AnnotatedFuns)
resolveHeaderFuns hdr =  Map.foldrWithKey resolveTypeRegs ([],[]) (hdrFunDecls hdr)
  where resolveTypeRegs :: BSC.ByteString -- ^ Name of function
                        -> HdrFunDecl -- ^ Function types
                        -> ([(BSC.ByteString, HeaderAnnError)], AnnotatedFuns)
                        -> ([(BSC.ByteString, HeaderAnnError)], AnnotatedFuns)
        resolveTypeRegs funName tp (prevWarnings,prev) =
          if hfdVarArg tp then
            ((funName,VarArgsUnsupported):prevWarnings,prev)
           else do
            let s0 = ARS { arsPrev = []
                         , arsNextGPP = [ F.RDI, F.RSI, F.RDX, F.RCX, F.R8, F.R9 ]
                         }
            case (,) <$> runExcept (execStateT (argsToRegisters 0 (hfdArgs tp)) s0)
                     <*> parseReturnType (hfdRet tp) of
              Left e -> do
                ((funName,e):prevWarnings,prev)
              Right (s, ret) ->
                let fti = X86NonvarargFun (reverse (arsPrev s)) ret
                 in (prevWarnings, (funName, fti):prev)

-- | Construct function type from demands.
inferFunctionTypeFromDemands :: Map (MemSegmentOff 64) (DemandSet X86Reg)
                             -> Map (MemSegmentOff 64) X86FunTypeInfo
inferFunctionTypeFromDemands dm =
  let go :: DemandSet X86Reg
         -> Map (MemSegmentOff 64) (RegisterSet X86Reg)
         -> Map (MemSegmentOff 64) (RegisterSet X86Reg)
      go ds m = Map.unionWith Set.union (functionResultDemands ds) m

      retDemands :: Map (MemSegmentOff 64) (RegisterSet X86Reg)
      retDemands = foldr go Map.empty dm

      -- drop the suffix which isn't a member of the arg set.  This
      -- allows e.g. arg0, arg2 to go to arg0, arg1, arg2.
      dropArgSuffix :: (a -> X86Reg tp)
                    -> [a]
                    -> RegisterSet X86Reg
                    -> [a]
      dropArgSuffix f regs rs =
        reverse $ dropWhile (not . (`Set.member` rs) . Some . f) $ reverse regs

      -- Turns a set of arguments into a prefix of x86 argument registers and friends
      orderPadArgs :: (RegisterSet X86Reg, RegisterSet X86Reg) -> X86FunTypeInfo
      orderPadArgs (argSet, retSet) =
        let args = fmap ArgBV64   (dropArgSuffix X86_GP     x86GPPArgumentRegs argSet)
                ++ fmap ArgMM512D (dropArgSuffix X86_ZMMReg [0..7] argSet)
            rets = fmap RetBV64   (dropArgSuffix X86_GP     [F.RAX, F.RDX] retSet)
                ++ fmap RetMM512D (dropArgSuffix X86_ZMMReg [0,1]          retSet)
         in X86NonvarargFun args rets
  in fmap orderPadArgs
     $ Map.mergeWithKey (\_ ds rets -> Just (registerDemands ds, rets))
                        (fmap (\ds ->  (registerDemands ds, mempty)))
                        (fmap (\s -> (mempty,s)))
                        dm
                        retDemands

-- | Event passed to logger when discovering functions
data GetFnsLogEvent
   = HeaderAnnError !BSC.ByteString !HeaderAnnError
   | StartFunRecovery !BSC.ByteString
     -- ^ Notify we are starting analysis of given function.
   | RecoveryFailed !BSC.ByteString !String
     -- ^ @RecoveryFailed fnm msg@ notes we failed to recover function due to given reason.
   | RecoveryPLTSkipped !BSC.ByteString
   | RecoveryWarning !BSC.ByteString !String
   | GetFnsError !String
     -- ^ A general error message

instance Show GetFnsLogEvent where
  show (HeaderAnnError fnm e) = showHeaderAnnError fnm e
  show (StartFunRecovery fnm) = "Recovering function " ++ BSC.unpack fnm
  show (RecoveryFailed _ msg)  = "  Failed:  " ++ msg
  show (RecoveryPLTSkipped _)  = "  Skipped PLT stub"
  show (RecoveryWarning _ msg) = "  Warning: " ++ msg
  show (GetFnsError msg) = msg

-- | Analyze an elf binary to extract information.
--
--  Note. This prints warnings to stderr and may exit if the Elf file cannot be parsed.
discoverX86Elf :: (GetFnsLogEvent -> IO ())
               -- ^ Logging function for errors
               -> FilePath
               -- ^ Path to binary for exploring CFG
               -> LoadOptions
               -- ^ Option to load the binary at the given address
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> [String] -- ^ Included addresses (if empty then all addresses included)
               -> [String] -- ^ Excluded addresses
               -> Header
               -- ^ Header with hints for assisting typing.
               -> BSC.ByteString -- ^ Prefix to use if we need to generate new function endpoints later.
               -> IO ( Elf 64
                     , X86OS
                     , DiscoveryState X86_64
                     , AddrSymMap 64
                     , RecoveredModule X86_64
                     )
discoverX86Elf logger path loadOpts disOpt includeAddr excludeAddr hdrAnn unnamedFunPrefix = do
  bs <- checkedReadFile path
  hdrInfo <-
    case parseElfHeaderInfo bs of
      Left (_, msg) -> do
        hPutStrLn stderr $ "Could not parse Elf file " ++ path ++ ":"
        hPutStrLn stderr $ "  " ++ msg
        exitFailure
      Right (Elf32 _) -> do
        hPutStrLn stderr "32-bit elf files are not yet supported."
        exitFailure
      Right (Elf64 hdrInfo) -> do
        pure hdrInfo
  let hdr = header hdrInfo
  os <- getX86ElfArchInfo (headerMachine hdr) (headerOSABI hdr)
  (e, discState, addrSymMap, symAddrMap) <-
    runCompleteDiscovery loadOpts disOpt hdrInfo (osArchitectureInfo os) includeAddr excludeAddr
  when (isUsedPrefix unnamedFunPrefix addrSymMap) $ do
    hPutStrLn stderr $ printf "No symbol in the binary may start with the prefix %d." (BSC.unpack unnamedFunPrefix)
    exitFailure
  let sysp = osPersonality os
  let mem = memory discState
  -- Resolve function type annotations from header, and report rwarnings.
  let (hdrWarnings, symTypeList) = resolveHeaderFuns hdrAnn
  mapM_ (\(nm,err) -> logger (HeaderAnnError nm err)) hdrWarnings

  -- Generate map from symbol names to known type.
  --
  -- This is used when we determine that a function jumps to an
  -- undefined symbol via a relocation.
  let symTypeMap :: Map BS.ByteString (KnownFunABI X86Reg)
      symTypeMap = Map.fromList $
        [ (nm, tp)
        | (nm, annTp) <- symTypeList
        , tp <- toKnownFunABI annTp
        ]

  -- Generate map from symbol names to known type.
  --
  -- This is used when we see a function jumps to a defined address.
  addrTypeMap <- do
    let insSymType :: Map (MemSegmentOff 64) (KnownFunABI X86Reg)
                   -> (BS.ByteString, X86FunTypeInfo)
                   -> IO (Map (MemSegmentOff 64) (KnownFunABI X86Reg))
        insSymType m (sym,annTp) = do
          case symAddrMapLookup symAddrMap sym of
            Left SymAddrMapNotFound -> do
              logger $ GetFnsError $ "Could not find symbol " ++ BSC.unpack sym ++ "."
              pure m
            Left SymAddrMapAmbiguous -> do
              logger $ GetFnsError $ "Ambiguous symbol " ++ BSC.unpack sym ++ "."
              pure m
            Right addr -> do
              pure $ foldl (\m' tp -> Map.insert addr tp m') m (toKnownFunABI annTp)
    foldlM insSymType Map.empty symTypeList

  -- Compute only those functions whose entries are not known.
  let notKnown (Some f) = not (Map.member (discoveredFunAddr f) addrTypeMap)

  -- Compute the function demands
  let fDems =
        functionDemands (x86DemandInfo sysp) addrTypeMap symTypeMap mem $
          filter notKnown $ exploredFunctions discState

  let funNameMap ::  Map (MemSegmentOff 64) BS.ByteString
      funNameMap = addrSymMap
                <> Map.fromList [ (addr, nosymFunctionName unnamedFunPrefix addr)
                                | Some finfo <- exploredFunctions discState
                                , let addr = discoveredFunAddr finfo
                                , Map.notMember addr addrSymMap
                                ]
  let funTypeMap ::  Map BS.ByteString X86FunTypeInfo
      funTypeMap = Map.fromList [ (recoveredFunctionName addrSymMap unnamedFunPrefix addr, tp)
                                | (addr,tp) <- Map.toList (inferFunctionTypeFromDemands fDems)
                                ]
                <> Map.singleton "printf" X86PrintfFun
                <> Map.fromList symTypeList
  fnDefs <- fmap catMaybes $
    forM (exploredFunctions discState) $ \(Some finfo) -> do
      let fnm = discoveredFunName finfo
      logger $ StartFunRecovery fnm
      case checkFunction finfo of
        FunctionOK -> do
          case recoverFunction sysp funNameMap funTypeMap mem finfo of
            Left msg -> do
              logger $ RecoveryFailed fnm msg
              pure Nothing
            Right (warnings, fn) -> do
              mapM_ (logger . RecoveryWarning fnm) warnings
              pure (Just fn)
        FunctionHasPLT -> do
          logger $ RecoveryPLTSkipped fnm
          -- Skip PLT functions with no error message.
          pure Nothing
        FunctionIncomplete -> do
          logger $ RecoveryFailed fnm $ "Incomplete discovery."
          pure Nothing
  -- Get list of addresses included in this set.
  let excludedSet = Set.fromList $ recoveredFunctionName addrSymMap unnamedFunPrefix . fnAddr <$> fnDefs

  -- Get all functions that are referenced, but not defined in the module.
  let declFunTypeMap :: FunctionTypeMap X86_64
      declFunTypeMap = foldl (getReferencedFunctions excludedSet) Map.empty fnDefs

  let recMod = RecoveredModule { recoveredDecls = [ FunctionDecl { funDeclName = nm
                                                                 , funDeclType = tp
                                                                 }
                                                  | (nm, tp) <- Map.toList declFunTypeMap
                                                  ]
                               , recoveredDefs = fnDefs
                               }
  seq recMod $ pure (e, os, discState, addrSymMap, recMod)

-- | Produce a LLVM textual rendering of the module for the LLVM version.
llvmAssembly :: LLVMArchSpecificOps X86_64
                -- ^ architecture specific functions
             -> LLVMGenOptions
                -- ^ Options for generating LLVM
             -> RecoveredModule X86_64
                -- ^ Module to generate
             -> LPP.Config
             -> (Builder.Builder, [Ann.FunctionAnn])
llvmAssembly archOps genOpts recMod cfg =
      -- Generate LLVM module
   let (m,ann) = moduleForFunctions archOps genOpts recMod
       -- Render into LLVM
       out = HPJ.fullRender HPJ.PageMode 10000 1 pp mempty (ppLLVM cfg m)
    in (out, ann)
  where pp :: HPJ.TextDetails -> Builder.Builder -> Builder.Builder
        pp (HPJ.Chr c)  b = Builder.charUtf8 c <> b
        pp (HPJ.Str s)  b = Builder.stringUtf8 s <> b
        pp (HPJ.PStr s) b = Builder.stringUtf8 s <> b

--------------------------------------------------------------------------------
-- Compile the LLVM

-- | Compile a bytestring containing LLVM assembly or bitcode into an object.
--
-- This writes to standard out and throws an error.
compileLLVM :: Int -- ^ Optimization level
            -> FilePath -- ^ Path to LLVM `opt` command
            -> FilePath -- ^ Path to llc
            -> FilePath -- ^ Path to llvm-mc
            -> String   -- ^ Architure triple to pass to LLC
            -> Builder.Builder -- ^ Representiation of LLVM to serialize
            -> IO BS.ByteString
compileLLVM optLevel optPath llcPath llvmMcPath arch llvm = do
  -- Run llvm on resulting binary
  mres <- runExceptT $ do
    -- Skip optimization if optLevel == 0
    llvm_opt <-
      if optLevel /= 0 then do
        Ext.run_opt optPath optLevel $ \inHandle -> do
          Builder.hPutBuilder inHandle llvm
       else
        pure $ BSL.toStrict $ Builder.toLazyByteString llvm
    let llc_opts = Ext.LLCOptions { Ext.llc_triple    = Just arch
                                  , Ext.llc_opt_level = optLevel
                                  }
    asm <- Ext.run_llc llcPath llc_opts llvm_opt
    Ext.runLlvmMc llvmMcPath asm arch

  case mres of
    Left f -> do
      hPutStrLn stderr (show f)
      exitFailure
    Right b -> return b

--------------------------------------------------------------------------------
-- ControlFlowTargetMap

-- | A map from all control flow targets in the program to the start address of
-- functions that may target them.
--
-- This is used to compute when it is safe to insert a redirection.  We want to
-- ensure that adding a redirection will not break unknow
newtype ControlFlowTargetSet w = CFTS { cfTargets :: Map (MemSegmentOff w) [MemSegmentOff w]
                                      }

-- | Return how many bytes of space there are to write after address without
-- ovewriting another control flow target.
lookupControlFlowTargetSpace :: forall w
                             .  MemWidth w
                             => MemSegmentOff w
                             -> ControlFlowTargetSet w
                             -> MemWord w
lookupControlFlowTargetSpace addr0 = go 0 addr0 addr0
  where seg = segoffSegment addr0
        go :: MemWord w -> MemSegmentOff w -> MemSegmentOff w -> ControlFlowTargetSet w -> MemWord w
        go inc base addr s =
          case Map.lookupGT addr (cfTargets s) of
            Just (next,fns)
              | segoffSegment addr == segoffSegment next ->
                let d = segoffOffset next - segoffOffset addr
                 in if null (filter (/= base) fns) then
                      go (inc+d) base next s
                     else
                      inc+d
            _ ->
              if segmentSize seg >= segoffOffset addr then
                segmentSize seg - segoffOffset addr
               else
                0

addControlFlowTarget :: ControlFlowTargetSet w
                     -> MemSegmentOff w
                     -> MemSegmentOff w -- ^ Function entry point
                     -> ControlFlowTargetSet w
addControlFlowTarget m a f = m { cfTargets = Map.insertWith (++) a [f] (cfTargets m) }

addFunctionEntryPoint :: ControlFlowTargetSet w
                      -> MemSegmentOff w
                      -> ControlFlowTargetSet w
addFunctionEntryPoint s a = addControlFlowTarget s a a


addFunDiscoveryControlFlowTargets :: ControlFlowTargetSet (ArchAddrWidth arch)
                                  -> Some (DiscoveryFunInfo arch)
                                  -> ControlFlowTargetSet (ArchAddrWidth arch)
addFunDiscoveryControlFlowTargets m0 (Some f) =
  foldl' (\m b -> addControlFlowTarget m b (discoveredFunAddr f)) m0 (Map.keys (f^.parsedBlocks))

discoveryControlFlowTargets :: DiscoveryState arch -> ControlFlowTargetSet (ArchAddrWidth arch)
discoveryControlFlowTargets info =
  let m0 = CFTS { cfTargets = Map.empty }
      m = foldl' addFunDiscoveryControlFlowTargets m0 (exploredFunctions info)
   in foldl' addFunctionEntryPoint m (Map.keys (symbolNames info))

-- | This creates a code redirection or returns the address as failing.
addrRedirection :: ControlFlowTargetSet 64
                -> AddrSymMap 64
                   -- ^ Map from address to symbol name
                -> BSC.ByteString
                   -- ^ Prefix to use for functions without a prefix.
                -> Function X86_64
                -> CodeRedirection Word64
addrRedirection tgts addrSymMap prefix f = do
  let a = fnAddr f
  let off :: Word64
      off = fromIntegral (segmentOffset (segoffSegment a)) + fromIntegral (segoffOffset a)
  CodeRedirection { redirSourceVAddr = off
                  , redirSourceSize =
                      fromIntegral (lookupControlFlowTargetSpace (fnAddr f) tgts)
                  , redirTarget =
                      case Map.lookup a addrSymMap of
                        Just nm -> nm
                        Nothing ->
                          let aa = segoffAddr a
                           in prefix <> "_" <> BSC.pack (show (addrBase aa)) <> "_" <> BSC.pack (show (addrOffset aa))
                  }


-- | Merge a binary and new object
mergeAndWrite :: FilePath
              -> Elf 64 -- ^ Original binary
              -> Elf 64 -- ^ New object
              -> [CodeRedirection Word64] -- ^ List of redirections from old binary to new.
              -> IO ()
mergeAndWrite output_path orig_binary new_obj redirs = do
  let mres = mergeObject orig_binary new_obj redirs x86_64_immediateJump
  case mres of
    Left e -> fail e
    Right new_binary -> do
      BSL.writeFile output_path $ renderElf new_binary
      -- Update the file mode
      do fs <- getFileStatus output_path
         let fm = ownerExecuteMode
               .|. groupExecuteMode
               .|. otherExecuteMode
         setFileMode output_path (fileMode fs `unionFileModes` fm)
