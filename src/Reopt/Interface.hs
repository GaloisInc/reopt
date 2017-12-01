{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reopt.Interface where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Either
import           Data.ElfEdit
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Vector as V
import           Data.Word
import           GHC.IO (ioToST, stToIO)
import           Numeric (readHex)
import           System.Exit (exitFailure)
import           System.IO
import           System.IO.Error

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import           Data.Macaw.Memory
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.SyscallInfo

import           Reopt
import           Reopt.CFG.FnRep (Function(..), FunctionType(..))
import           Reopt.CFG.FunctionCheck
import           Reopt.CFG.Recovery

------------------------------------------------------------------------
-- X86-specific functions

summarizeX86TermStmt :: SyscallPersonality
                     -> ComputeArchTermStmtEffects X86_64 ids
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
                 }

------------------------------------------------------------------------
-- Discovery options

-- | Options controlling discovery
data DiscoveryOptions
   = DiscoveryOptions { logAtAnalyzeFunction  :: !Bool
                        -- ^ Print a message each time we apply discovery analysis to
                        -- a new function.
                      , logAtAnalyzeBlock     :: !Bool
                        -- ^ Print a message each time we analyze a block within a
                        -- function.
                      , exploreFunctionSymbols :: !Bool
                        -- ^ Flag indicates we should explore function symbols
                      , exploreCodeAddrInMem :: !Bool
                        -- ^ Flag indicating we should explore all potential code
                        -- addresses in memory
                      }

defaultDiscoveryOptions :: DiscoveryOptions
defaultDiscoveryOptions =
  DiscoveryOptions { logAtAnalyzeFunction = True
                   , logAtAnalyzeBlock      = False
                   , exploreFunctionSymbols = True
                   , exploreCodeAddrInMem = False
                   }

------------------------------------------------------------------------
-- Execution

dumpDisassembly :: FilePath -> IO ()
dumpDisassembly path = do
  e <- readElf64 path
  let sections = filter isCodeSection $ e^..elfSections
  when (null sections) $ do
    putStrLn "Binary contains no executable sections."
  mapM_ printSectionDisassembly sections


ppSymbol :: MemWidth w => MemSegmentOff w -> SymbolAddrMap w -> String
ppSymbol addr sym_map =
  case symbolAtAddr addr sym_map of
    Just fnName -> show addr ++ " (" ++ UTF8.toString fnName ++ ")"
    Nothing  -> show addr

resolveFuns :: MemWidth (RegAddrWidth (ArchReg arch))
            => DiscoveryOptions -- ^ Options controlling discovery
            -> SymbolAddrMap (ArchAddrWidth arch)
            -> DiscoveryState arch
            -> IO (DiscoveryState arch)
resolveFuns disOpt sym_map info = seq info $
  case Map.lookupMin (info^.unexploredFunctions) of
    Nothing -> pure info
    Just (addr, rsn) -> do
      when (logAtAnalyzeFunction disOpt) $ do
        hPutStrLn stderr $ "Analyzing function: " ++ ppSymbol addr sym_map
      info' <- stToIO $ analyzeFunction (blockLogFn disOpt) addr rsn info
      resolveFuns disOpt sym_map (fst info')

-- | Return the segment offset of the elf file entry point or fail if undefined.
getElfEntry :: Monad m => Memory w -> Elf w -> m (MemSegmentOff w)
getElfEntry mem e =  addrWidthClass (memAddrWidth mem) $ do
  elfClassInstances (elfClass e) $ do
  case resolveAbsoluteAddr mem (fromIntegral (elfEntry e)) of
    Nothing -> fail "Could not resolve entry"
    Just v  -> pure v

getSymbolMap :: Monad m => Memory w -> Elf w -> m (SymbolAddrMap w)
getSymbolMap mem e = do
  entries <-
    case elfSymtab e of
      [] -> pure $ []
      [tbl] -> pure $ V.toList (elfSymbolTableEntries tbl)
      _ -> fail "Elf contains multiple symbol tables."

  let resolved = resolvedSegmentedElfFuncSymbols mem entries
  -- Check for unresolved symbols
  case symbolAddrMap (head <$> resolved) of
    Left msg -> fail msg
    Right m -> pure m

-- | Create a discovery state and symbol-address map
mkFinalCFGWithSyms :: forall arch
                   .  ArchitectureInfo arch
                   -> Memory (ArchAddrWidth arch) -- ^ Layout in memory of file
                   -> Elf (ArchAddrWidth arch) -- ^ Elf file to create CFG for.
                   -> DiscoveryOptions -- ^ Options controlling discovery
                   -> IO (DiscoveryState arch, SymbolAddrMap (ArchAddrWidth arch))
mkFinalCFGWithSyms ainfo mem e disOpt = withArchConstraints ainfo $ do
  -- Get meap from addresses to symbol names
  sym_map <- getSymbolMap mem e
  -- Create initial discovery state
  entry <- getElfEntry mem e
  let ds0 = emptyDiscoveryState mem sym_map ainfo
          & markAddrsAsFunction InitAddr [entry]
  -- Add symbol table entries to discovery state if requested
  let ds1 | exploreFunctionSymbols disOpt =
              ds0 & markAddrsAsFunction InitAddr (symbolAddrs sym_map)
          | otherwise = ds0
  -- Perform discovery
  ds2 <- resolveFuns disOpt sym_map ds1
  -- Discover functions from arbitrary mem pointers
  ds3 <-
    if exploreCodeAddrInMem disOpt then do
      let mem_contents = withArchConstraints ainfo $ memAsAddrPairs mem LittleEndian
      resolveFuns disOpt sym_map $ ds2 & exploreMemPointers mem_contents
     else
      return ds2
  -- Return
  pure (ds3, sym_map)

data SomeArchitectureInfo w =
  forall arch
    . ( w ~ RegAddrWidth (ArchReg arch)
      )
    => SomeArch (ArchitectureInfo arch)

getElfArchInfo :: Elf w -> IO (SomeArchitectureInfo w)
getElfArchInfo e =
  case (elfClass e, elfMachine e, elfOSABI e) of
    (ELFCLASS64, EM_X86_64, ELFOSABI_LINUX)   -> pure (SomeArch x86_64_linux_info)
    (ELFCLASS64, EM_X86_64, ELFOSABI_SYSV)    -> pure (SomeArch x86_64_linux_info)
    (ELFCLASS64, EM_X86_64, ELFOSABI_FREEBSD) -> pure (SomeArch x86_64_freeBSD_info)
#ifdef SUPPORT_ARM
    (ELFCLASS32, EM_ARM, ELFOSABI_SYSV) -> do
      Data.VEX.FFI.init Data.VEX.FFI.stdOptions
      pure (SomeArch armArchle)
#endif
    (cl, arch, abi) -> do
     fail $ "Do not support " ++ show (elfClassBitWidth cl) ++ "-bit "
            ++ show arch ++ "-" ++ show abi ++ "binaries."

getX86ElfArchInfo :: Elf 64 -> IO (ArchitectureInfo X86_64, SyscallPersonality, String)
getX86ElfArchInfo e =
  case elfOSABI e of
    ELFOSABI_LINUX   -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_SYSV    -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_FREEBSD -> pure (x86_64_freeBSD_info, freeBSD_syscallPersonality, "FreeBSD")
    abi              ->
     fail $ "Do not support " ++ show EM_X86_64 ++ "-" ++ show abi ++ "binaries."

showNonfatalErrors :: (Eq (ElfWordType w), Num (ElfWordType w), Show (ElfWordType w))
                   => [ElfParseError w] -> IO ()
showNonfatalErrors l = do
  when (not (null l)) $ do
    hPutStrLn stderr $ "Recoverable errors occurred in reading elf file:"
    forM_ l $ \emsg -> do
      hPutStrLn stderr (show emsg)

readSomeElf :: FilePath -> IO (Some Elf)
readSomeElf path = do
  when (null path) $ do
    hPutStrLn stderr "Please specify a path."
    hPutStrLn stderr "For help on using reopt, run \"reopt --help\"."
    exitFailure
  let h e | isDoesNotExistError e = do
            hPutStrLn stderr $ path ++ " does not exist."
            hPutStrLn stderr "For help on using reopt, run \"reopt --help\"."
            exitFailure
          | isUserError e = do
            hPutStrLn stderr (ioeGetErrorString e)
            exitFailure
          | otherwise = do
            hPutStrLn stderr (show e)
            hPutStrLn stderr (show (ioeGetErrorType e))
            exitFailure
  bs <- BS.readFile path `catch` h
  case parseElf bs of
    ElfHeaderError _ msg -> do
      hPutStrLn stderr $ "Error reading " ++ path ++ ":"
      hPutStrLn stderr $ "  " ++ msg
      exitFailure
    Elf32Res l e -> do
      showNonfatalErrors l
      return (Some e)
    Elf64Res l e -> do
      showNonfatalErrors l
      return (Some e)

resolveSymName :: String -> Either Word64 String
resolveSymName ('0':'x': nm) | [(w,"")] <- readHex nm = Left w
resolveSymName nm = Right nm

-- | Attempt to find the address of a string identifying a symbol name, and
-- return either the string if it cannot be resolved or the address.
resolveSymAddr :: Memory w
               -> Map BS.ByteString (MemSegmentOff w)
                 -- ^ Map from symbol names in binary to their address.
              -> String
                 -- ^ The name of a symbol as a string.
              -> Either String (MemSegmentOff w)
resolveSymAddr mem symMap nm0 = addrWidthClass (memAddrWidth mem) $
  case resolveSymName nm0 of
    Left w ->
      case resolveAbsoluteAddr mem (fromIntegral w) of
        Just off -> Right off
        Nothing -> Left nm0
    Right nm -> do
      case Map.lookup (fromString nm) symMap of
         Just addr -> Right addr
         Nothing -> Left nm

blockLogFn :: MemWidth w => DiscoveryOptions -> MemSegmentOff w -> ST RealWorld ()
blockLogFn disOpt
  | logAtAnalyzeBlock disOpt = \addr ->
     ioToST $ hPutStrLn stderr $ "  Analyzing block: " ++ show addr
  | otherwise = \_ -> pure ()

inferFunctionTypeFromDemands :: Map (MemSegmentOff 64) (DemandSet X86Reg)
                             -> Map (MemSegmentOff 64) FunctionType
inferFunctionTypeFromDemands dm =
  let go ds m = Map.unionWith Set.union (functionResultDemands ds) m
      retDemands :: Map (MemSegmentOff 64) (RegisterSet X86Reg)
      retDemands = foldr go Map.empty dm

      -- drop the suffix which isn't a member of the arg set.  This
      -- allows e.g. arg0, arg2 to go to arg0, arg1, arg2.
      maximumArgPrefix :: [X86Reg tp] -> RegisterSet X86Reg -> Int
      maximumArgPrefix regs rs =
        length $ dropWhile (not . (`Set.member` rs) . Some) $ reverse regs

      -- Turns a set of arguments into a prefix of x86ArgumentRegisters and friends
      orderPadArgs :: (RegisterSet X86Reg, RegisterSet X86Reg) -> FunctionType
      orderPadArgs (args, rets) =
        FunctionType { fnNIntArgs = maximumArgPrefix x86ArgumentRegs args
                     , fnNFloatArgs = maximumArgPrefix x86FloatArgumentRegs args
                     , fnNIntRets = maximumArgPrefix x86ResultRegs rets
                     , fnNFloatRets = maximumArgPrefix x86FloatResultRegs rets
                     }
  in fmap orderPadArgs
     $ Map.mergeWithKey (\_ ds rets -> Just (registerDemands ds, rets))
                        (fmap (\ds ->  (registerDemands ds, mempty)))
                        (fmap (\s -> (mempty,s)))
                        dm
                        retDemands

-- | Try to recover function information from the information
-- recovered during code discovery.
getFns :: SyscallPersonality
       -> Map BS.ByteString (MemSegmentOff 64)
          -- ^ Maps symbol names to addresses
       -> Set String
          -- ^ Name of symbols/addresses to exclude
       -> DiscoveryState X86_64
          -- ^ Information about original binary recovered from static analysis.
       -> IO [Function]
getFns sysp symMap excludedNames info = do

  let mem = memory info
  -- Compute which functions to compute by looking at the binary
  let nms = Set.toList excludedNames
  let (bad, excludedAddrs) = partitionEithers $ resolveSymAddr mem symMap  <$> nms
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad

  let excludeSet = Set.fromList excludedAddrs
  -- Check that the address of the function is not one that we are excluding.
  let include :: Some (DiscoveryFunInfo X86_64) -> Bool
      include (Some f) = Set.notMember (discoveredFunAddr f) excludeSet

  let entries = filter include $ exploredFunctions info

  let fDems :: Map (MemSegmentOff 64) (DemandSet X86Reg)
      fDems = functionDemands (x86DemandInfo sysp) info
  let fArgs :: AddrToX86FunctionTypeMap
      fArgs = inferFunctionTypeFromDemands fDems
  seq fArgs $ do
  fmap catMaybes $ forM entries $ \(Some finfo) -> do
    let entry = discoveredFunAddr finfo
    case () of
      _ | checkFunction finfo -> do
            case recoverFunction sysp fArgs mem finfo of
              Left msg -> do
                hPutStrLn stderr $ "Could not recover function " ++ show entry ++ ":\n  " ++ msg
                pure Nothing
              Right fn -> do
                pure (Just fn)
        | otherwise -> do
            hPutStrLn stderr $ "Invalid function at " ++ show entry
            pure Nothing
