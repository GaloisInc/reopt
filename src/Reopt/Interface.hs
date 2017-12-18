{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Reopt.Interface where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Either
import           Data.ElfEdit
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Tuple (swap)
import           Data.Type.Equality
import qualified Data.Vector as V
import           Data.Word
import           GHC.IO (ioToST, stToIO)
import           Numeric (readHex)
import           System.Directory (doesFileExist)
import           System.Exit (exitFailure)
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>), (<>))
import qualified Text.PrettyPrint.HughesPJ as HPJ

-- import           Paths_reopt (getLibDir, version)

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
import           Reopt.CFG.LLVM as LLVM
import           Reopt.CFG.Recovery
import qualified Reopt.ExternalTools as Ext
import           Reopt.Relinker

#ifdef SUPPORT_ARM
import qualified Data.VEX.FFI
import           Data.Macaw.ARM
#endif

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
                 , demandInfoCtx = x86DemandContext
                 }

------------------------------------------------------------------------
-- LLVMVersion

-- | Version of LLVM to generate
data LLVMVersion
   = LLVM35
   | LLVM36
   | LLVM37
   | LLVM38

-- | Convert a string to the LLVM version identifier.
asLLVMVersion :: String -> Maybe LLVMVersion
asLLVMVersion s =
  case s of
    "llvm35" -> Just LLVM35
    "llvm36" -> Just LLVM36
    "llvm37" -> Just LLVM37
    "llvm38" -> Just LLVM38
    _ -> Nothing


-- | Pretty print an LLVM module using the format expected by the given LLVM version.
ppLLVM :: LLVMVersion -> L.Module -> HPJ.Doc
ppLLVM LLVM35 m = LPP.ppLLVM35 $ LPP.ppModule m
ppLLVM LLVM36 m = LPP.ppLLVM36 $ LPP.ppModule m
ppLLVM LLVM37 m = LPP.ppLLVM37 $ LPP.ppModule m
ppLLVM LLVM38 m = LPP.ppLLVM38 $ LPP.ppModule m

------------------------------------------------------------------------
-- Discovery options

-- | Options controlling discovery
data DiscoveryOptions
   = DiscoveryOptions { forceMemLoadStyle :: !(Maybe LoadStyle)
                        -- ^ This describes whether we force a particular load style.
                      , logAtAnalyzeFunction  :: !Bool
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
  DiscoveryOptions { forceMemLoadStyle = Nothing
                   , logAtAnalyzeFunction = True
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
            => (String -> IO ()) -- ^ logging function
            -> DiscoveryOptions -- ^ Options controlling discovery
            -> SymbolAddrMap (ArchAddrWidth arch)
            -> DiscoveryState arch
            -> IO (DiscoveryState arch)
resolveFuns logger disOpt sym_map info = seq info $
  case Map.lookupMin (info^.unexploredFunctions) of
    Nothing -> pure info
    Just (addr, rsn) -> do
      when (logAtAnalyzeFunction disOpt) $ do
        logger $ "Analyzing function: " ++ ppSymbol addr sym_map
      info' <- stToIO $ analyzeFunction (blockLogFn logger disOpt) addr rsn info
      resolveFuns logger disOpt sym_map (fst info')

getSymbolMap :: (String -> IO ()) -- ^ logging function
             -> Memory w
             -> SectionIndexMap w
             -> Elf w
             -> IO (SymbolAddrMap w)
getSymbolMap logger mem indexMap e = do
  elfClassInstances (elfClass e) $ do
  entries <-
    case elfSymtab e of
      [] -> pure $ []
      [tbl] -> pure $ V.toList (elfSymbolTableEntries tbl)
      _ -> fail "Elf contains multiple symbol tables."
  let (unresolved, resolved) =
        resolvedSegmentedElfFuncSymbols mem indexMap entries
  forM_ unresolved $ \symbol -> do
    logger $ "Could not resolve " ++ show (steName symbol)
  -- Check for unresolved symbols
  case symbolAddrMap (head <$> resolved) of
    Left msg -> fail msg
    Right m -> pure m

-- | Return the segment offset of the elf file entry point or fail if undefined.
getElfEntry :: Monad m => Memory w -> Elf w -> m (MemSegmentOff w)
getElfEntry mem e =  addrWidthClass (memAddrWidth mem) $ do
  elfClassInstances (elfClass e) $ do
  case resolveAbsoluteAddr mem (fromIntegral (elfEntry e)) of
    Nothing -> fail "Could not resolve entry"
    Just v  -> pure v

isRelocatable :: Elf w -> Bool
isRelocatable e = any (hasSegmentType PT_DYNAMIC) (elfSegments e)

-- | Create a discovery state and symbol-address map
mkInitialCFGWithSyms :: forall arch
                   .  (String -> IO ())
                   -> ArchitectureInfo arch
                   -> Elf (ArchAddrWidth arch) -- ^ Elf file to create CFG for.
                   -> DiscoveryOptions -- ^ Options controlling discovery
                   -> IO ( SectionIndexMap (ArchAddrWidth arch)
                         , Memory (ArchAddrWidth arch)
                         , DiscoveryState arch
                         , SymbolAddrMap (ArchAddrWidth arch)
                         )
mkInitialCFGWithSyms logger ainfo e disOpt = withArchConstraints ainfo $ do
  -- Get meap from addresses to symbol names
  -- Initialize entry state with entry point if it is an executable/shared library.
  case elfType e of
    ET_REL -> do
      case forceMemLoadStyle disOpt of
        Just LoadBySegment -> do
          fail $ "Cannot load object files by segment."
        _ -> pure ()
      let loadOpts =
            LoadOptions { loadRegionIndex = 1
                        , loadStyle = LoadBySection
                        , includeBSS = False
                        }
      (secMap, mem) <- either fail return $ memoryForElf loadOpts e
      symMap <- getSymbolMap logger mem secMap e
      let initState
            = emptyDiscoveryState mem symMap ainfo
      pure (secMap, mem, initState, symMap)
    ET_EXEC -> do
      let loadSty = fromMaybe LoadBySegment (forceMemLoadStyle disOpt)
      let loadOpts =
            LoadOptions { loadRegionIndex =
                            if isRelocatable e then 1 else 0
                        , loadStyle = loadSty
                        , includeBSS = False
                        }
      (secMap, mem) <- either fail return $ memoryForElf loadOpts e
      symMap <- getSymbolMap logger mem secMap e
      entry <- getElfEntry mem e
      let initState
            = emptyDiscoveryState mem symMap ainfo
            & markAddrsAsFunction InitAddr [entry]
      -- Return
      pure (secMap, mem, initState, symMap)
    ET_DYN -> do
      let loadSty = fromMaybe LoadBySegment (forceMemLoadStyle disOpt)
      let loadOpts =
            LoadOptions { loadRegionIndex = 1
                        , loadStyle = loadSty
                        , includeBSS = False
                        }
      (secMap, mem) <- either fail return $ memoryForElf loadOpts e
      symMap <- getSymbolMap logger mem secMap e
      entry <- getElfEntry mem e
      let initState
            = emptyDiscoveryState mem symMap ainfo
            & markAddrsAsFunction InitAddr [entry]
      -- Discover functions from arbitrary mem pointers
      -- Return
      pure (secMap, mem, initState, symMap)
    ET_CORE -> do
      fail $ "Reopt does not support loading core files."
    tp -> do
      fail $ "Reopt does not support loading elf files with type " ++ show tp ++ "."


-- | Create a discovery state and symbol-address map
mkFinalCFGWithSyms :: forall arch
                   .  (String -> IO ())
                   -> ArchitectureInfo arch
                   -> Elf (ArchAddrWidth arch) -- ^ Elf file to create CFG for.
                   -> DiscoveryOptions -- ^ Options controlling discovery
                   -> IO ( SectionIndexMap (ArchAddrWidth arch)
                         , Memory (ArchAddrWidth arch)
                         , DiscoveryState arch
                         , SymbolAddrMap (ArchAddrWidth arch)
                         )
mkFinalCFGWithSyms logger ainfo e disOpt = withArchConstraints ainfo $ do
  (secMap, mem, initState, symMap) <- mkInitialCFGWithSyms logger ainfo e disOpt
  -- Add symbol table entries to discovery state if requested
  let postSymState
        | exploreFunctionSymbols disOpt =
            initState & markAddrsAsFunction InitAddr (symbolAddrs symMap)
        | otherwise = initState
  -- Discover functions
  postPhase1Discovery <- resolveFuns logger disOpt symMap postSymState
  finalState <-
    if exploreCodeAddrInMem disOpt then do
      let mem_contents = withArchConstraints ainfo $ memAsAddrPairs mem LittleEndian
      resolveFuns logger disOpt symMap $ postPhase1Discovery & exploreMemPointers mem_contents
     else
      return postPhase1Discovery
  -- Return results
  pure (secMap, mem, finalState, symMap)

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
                   => (String -> IO ()) -- ^ logging function
                   -> [ElfParseError w]
                   -> IO ()
showNonfatalErrors logger l = do
  when (not (null l)) $ do
    logger $ "Recoverable errors occurred in reading elf file:"
    forM_ l $ \emsg -> do
      hPutStrLn stderr (show emsg)

readSomeElf :: (String -> IO ()) -> FilePath -> IO (Some Elf)
readSomeElf logger path = do
  when (null path) $ do
    logger "Please specify a path."
    logger "For help on using reopt, run \"reopt --help\"."
    exitFailure
  let h e | isDoesNotExistError e = do
            logger $ path ++ " does not exist."
            logger "For help on using reopt, run \"reopt --help\"."
            exitFailure
          | isUserError e = do
            logger (ioeGetErrorString e)
            exitFailure
          | otherwise = do
            logger (show e)
            logger (show (ioeGetErrorType e))
            exitFailure
  bs <- BS.readFile path `catch` h
  case parseElf bs of
    ElfHeaderError _ msg -> do
      logger $ "Error reading " ++ path ++ ":"
      logger $ "  " ++ msg
      exitFailure
    Elf32Res l e -> do
      showNonfatalErrors logger l
      return (Some e)
    Elf64Res l e -> do
      showNonfatalErrors logger l
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

blockLogFn :: MemWidth w
           => (String -> IO ())
           -> DiscoveryOptions
           -> MemSegmentOff w
           -> ST RealWorld ()
blockLogFn logger disOpt
  | logAtAnalyzeBlock disOpt = \addr ->
     ioToST $ logger $ "  Analyzing block: " ++ show addr
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
getFns :: (String -> IO ())
          -- ^ logging function
       -> SyscallPersonality
       -> Map BS.ByteString (MemSegmentOff 64)
          -- ^ Maps symbol names to addresses
       -> Set String
          -- ^ Name of symbols/addresses to exclude
       -> DiscoveryState X86_64
          -- ^ Information about original binary recovered from static analysis.
       -> IO [Function]
getFns logger sysp symMap excludedNames info = do
  let mem = memory info
  -- Compute which functions to compute by looking at the binary
  let nms = Set.toList excludedNames
  let (bad, excludedAddrs) = partitionEithers $ resolveSymAddr mem symMap  <$> nms
  when (not (null bad)) $ do
    logger $ "Could not resolve symbols: " ++ unwords bad

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
                logger $ "Could not recover function " ++ show entry ++ ":\n  " ++ msg
                pure Nothing
              Right fn -> do
                pure (Just fn)
        | otherwise -> do
            -- FIXME
            logger $ "Invalid function at " ++ show entry
            pure Nothing

llvmAssembly :: LLVMVersion -> L.Module -> Builder.Builder
llvmAssembly v m = HPJ.fullRender HPJ.PageMode 10000 1 pp mempty (ppLLVM v m)
  where pp :: HPJ.TextDetails -> Builder.Builder -> Builder.Builder
        pp (HPJ.Chr c)  b = Builder.charUtf8 c <> b
        pp (HPJ.Str s)  b = Builder.stringUtf8 s <> b
        pp (HPJ.PStr s) b = Builder.stringUtf8 s <> b

writeFileBuilder :: FilePath -> Builder.Builder -> IO ()
writeFileBuilder nm b = bracket (openBinaryFile nm WriteMode) hClose (\h -> Builder.hPutBuilder h b)

--------------------------------------------------------------------------------
-- Redirections

-- | This creates a code redirection or returns the address as failing.
addrRedirection :: ControlFlowTargetSet 64
                -> LLVM.AddrSymMap 64
                -> ElfSegmentMap 64
                -> Function
                -> Either (MemSegmentOff 64) (CodeRedirection Word64)
addrRedirection tgts addrSymMap m f = do
  let a = fnAddr f
  let w = case msegAddr a of
            Just absAddr -> fromIntegral absAddr
            Nothing -> error "Redirection does not yet support relocatable binaries."
  case lookupElfOffset m w of
    Nothing -> Left (fnAddr f)
    Just (idx,off) -> Right redir
      where L.Symbol sym_name = LLVM.functionName addrSymMap (fnAddr f)
            redir = CodeRedirection { redirSourcePhdr   = idx
                                    , redirSourceOffset = off
                                    , redirSourceSize   = fromIntegral (lookupControlFlowTargetSpace (fnAddr f) tgts)
                                    , redirTarget       = UTF8.fromString sym_name
                                    }


targetArch :: ElfOSABI -> IO String
targetArch abi =
  case abi of
    ELFOSABI_SYSV    -> return "x86_64-unknown-linux-elf"
    ELFOSABI_NETBSD  -> return "x86_64-unknown-freebsd-elf"
    ELFOSABI_FREEBSD -> return "x86_64-unknown-linux-elf"
    _ -> fail $ "Do not support architecture " ++ show abi ++ "."

-- | Compile a bytestring containing LLVM assembly or bitcode into an object.
compile_llvm_to_obj :: Int -> FilePath -> FilePath -> FilePath -> String -> BS.ByteString -> FilePath -> IO ()
compile_llvm_to_obj optLevel optPath llcPath gasPath arch llvm obj_path = do
  -- Run llvm on resulting binary
  putStrLn "Compiling new code"
  mres <- runExceptT $ do
    -- Skip optimization if optLevel == 0
    llvm_opt <-
      if optLevel /= 0 then do
        Ext.run_opt optPath optLevel llvm
       else
        pure llvm
    let llc_opts = Ext.LLCOptions { Ext.llc_triple    = Just arch
                                  , Ext.llc_opt_level = optLevel
                                  }
    asm <- Ext.run_llc llcPath llc_opts llvm_opt
    Ext.run_gas gasPath asm obj_path

  case mres of
    Left f -> fail $ show f
    Right () -> return ()

-- | Link the object and libreopt path together and return new object.
link_with_libreopt :: FilePath -- ^ Path to directory to write temporary files to.
                   -> FilePath -- ^ Path to libreopt.
                   -> FilePath -- ^ LLVM link path.
--                   -> String -- ^ Name of architecture
                   -> Builder.Builder -- ^ Object file.
                   -> IO BS.ByteString
link_with_libreopt obj_dir libreopt_path link_path obj_llvm = do
  do exists <- doesFileExist libreopt_path
     when (not exists) $ do
       fail $ "Could not find path to libreopt.bc needed to link object; tried " ++ libreopt_path

  let obj_llvm_path = obj_dir </> "obj.ll"
  writeFileBuilder obj_llvm_path obj_llvm

  mllvm <- runExceptT $
    Ext.run_llvm_link link_path [ obj_llvm_path, libreopt_path ]
  either (fail . show) return mllvm

------------------------------------------------------------------------
-- Pattern match on stack pointer possibilities.

-- | Extract list containing symbol names and addresses.
elfAddrSymEntries :: SectionIndexMap w
                     -- ^ Map from elf section indices to base address for section
                     -- and section in Elf file
                  -> Elf w
                  -> [(BS.ByteString, MemSegmentOff w)]
elfAddrSymEntries m binary =
  elfClassInstances (elfClass binary) $
  addrWidthClass (elfAddrWidth (elfClass binary)) $
  [ (steName entry, val)
  | tbl <- elfSymtab binary
  , entry <- V.toList $ elfSymbolTableEntries tbl
    -- Check this is a function or NOTYPE
  , steType entry `elem` [ STT_FUNC, STT_NOTYPE ]
    -- Compute address of symbol from section
  , let idx = steIndex entry
  , idx /= SHN_UNDEF && idx <= SHN_LORESERVE
    -- Get base index of section
  , (base, sec) <- maybeToList $ Map.lookup idx m
  , let diff = toInteger (steValue entry) - toInteger (elfSectionAddr sec)
  , val <- maybeToList $ incSegmentOff base diff
  ]

-- | Create map from symbol names to address.
elfSymAddrMap  :: SectionIndexMap w
                  -- ^ Map from elf section indices to base address for section
                  -- and section in Elf file
               -> Elf w
               -> Map BS.ByteString (MemSegmentOff w)
elfSymAddrMap m binary = Map.fromList $ elfAddrSymEntries m binary

-- | Create map from addresses to symbol name.
--
-- Used for naming functions.
elfAddrSymMap :: SectionIndexMap w
              -> Elf w
              -> LLVM.AddrSymMap w
elfAddrSymMap m binary = Map.fromList $ swap <$> elfAddrSymEntries m binary

-- | Merge a binary and new object
mergeAndWrite :: FilePath
              -> Elf 64 -- ^ Original binary
              -> Elf 64 -- ^ New object
              -> SymbolNameToAddrMap Word64 -- ^ Extra rdictions
              -> [CodeRedirection Word64] -- ^ List of redirections from old binary to new.
              -> IO ()
mergeAndWrite output_path orig_binary new_obj extra_syms redirs = do
  putStrLn $ "Performing final relinking."
  let (mres, warnings) = mergeObject orig_binary new_obj extra_syms redirs
  when (hasRelinkWarnings warnings) $ do
    hPrint stderr (pretty warnings)
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
  where seg = msegSegment addr0
        go :: MemWord w -> MemSegmentOff w -> MemSegmentOff w -> ControlFlowTargetSet w -> MemWord w
        go inc base addr s =
          case Map.lookupGT addr (cfTargets s) of
            Just (next,fns)
              | msegSegment addr == msegSegment next ->
                let d = msegOffset next - msegOffset addr
                 in if null (filter (/= base) fns) then
                      go (inc+d) base next s
                     else
                      inc+d
            _ ->
              if segmentSize seg >= msegOffset addr then
                segmentSize seg - msegOffset addr
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
   in foldl' addFunctionEntryPoint m (symbolAddrs (symbolNames info))

elfIs64Bit :: ElfClass w -> Maybe (w :~: 64)
elfIs64Bit ELFCLASS32 = Nothing
elfIs64Bit ELFCLASS64 = Just Refl

-- | Maps virtual addresses to the phdr at them.
type ElfSegmentMap w = Map (ElfWordType w) (Phdr w)

-- | Create an elf segment map from a layout.
elfSegmentMap :: forall w . ElfLayout w -> ElfSegmentMap w
elfSegmentMap l = elfClassInstances (elfLayoutClass l) $ foldl' insertElfSegment Map.empty (allPhdrs l)
  where insertElfSegment ::  Ord (ElfWordType w) => ElfSegmentMap w -> Phdr w -> ElfSegmentMap w
        insertElfSegment m p
          | elfSegmentType seg == PT_LOAD = Map.insert a p m
          | otherwise = m
          where seg = phdrSegment p
                a = elfSegmentVirtAddr (phdrSegment p)

-- | Lookup an address in the segment map, returning the index of the phdr
-- and the offset.
lookupElfOffset :: ElfSegmentMap 64 -> Word64 -> Maybe (Word16, Word64)
lookupElfOffset m a =
  case Map.lookupLE a m of
    Just (base, phdr) | a < base + phdrFileSize phdr ->
        Just (elfSegmentIndex seg, a - base)
      where seg = phdrSegment phdr
    _ -> Nothing
