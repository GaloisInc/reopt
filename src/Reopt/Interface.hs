{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Reopt.Interface
  ( DiscoveryOptions(..)
  , defaultDiscoveryOptions
  , getFns
  , resolveSymName
  , readSomeElf
    -- * Architecture info
  , SomeArchitectureInfo(..)
  , getElfArchInfo
  , discoverBinary
  , discoverX86Binary
  , discoverX86Elf
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Either
import           Data.ElfEdit
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Vector as V
import           Data.Word
import           GHC.IO (ioToST, stToIO)
import           Numeric (readHex)
import           System.IO

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import           Data.Macaw.Memory
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.SyscallInfo

import           Reopt
import           Reopt.CFG.FnRep (Function(..), FunctionType(..))
import           Reopt.CFG.FunctionCheck
import           Reopt.CFG.Recovery

#ifdef SUPPORT_ARM
import qualified Data.VEX.FFI
import           Data.Macaw.ARM
#endif

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
-- Architecture info

data SomeArchitectureInfo w =
  forall arch
    . ( w ~ RegAddrWidth (ArchReg arch)
      )
    => SomeArch (ArchitectureInfo arch)

------------------------------------------------------------------------
-- Get binary information

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
mkInitialCFGWithSyms :: Elf w -- ^ Elf file to create CFG for.
                     -> DiscoveryOptions -- ^ Options controlling discovery
                     -> IO ( SectionIndexMap w -- Map from section indices to
                           , Memory w -- Initial memory
                           , [MemSegmentOff w]       -- Additional entry points
                           )
mkInitialCFGWithSyms e disOpt = do
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
      pure (secMap, mem, [])
    ET_EXEC -> do
      let loadSty = fromMaybe LoadBySegment (forceMemLoadStyle disOpt)
      let loadOpts =
            LoadOptions { loadRegionIndex =
                            if isRelocatable e then 1 else 0
                        , loadStyle = loadSty
                        , includeBSS = False
                        }
      (secMap, mem) <- either fail return $ memoryForElf loadOpts e
      entry <- getElfEntry mem e
      pure (secMap, mem, [entry])
    ET_DYN -> do
      let loadSty = fromMaybe LoadBySegment (forceMemLoadStyle disOpt)
      let loadOpts =
            LoadOptions { loadRegionIndex = 1
                        , loadStyle = loadSty
                        , includeBSS = False
                        }
      (secMap, mem) <- either fail return $ memoryForElf loadOpts e
      entry <- getElfEntry mem e
      pure (secMap, mem, [entry])
    ET_CORE -> do
      fail $ "Reopt does not support loading core files."
    tp -> do
      fail $ "Reopt does not support loading elf files with type " ++ show tp ++ "."

-- | Generate a map from addresses to any associated symbol names.
getSymbolMap :: Memory w
             -> SectionIndexMap w
             -> Elf w
             -> IO [(BS.ByteString, MemSegmentOff w)]
getSymbolMap mem indexMap e = elfClassInstances (elfClass e) $ do
  entries <-
    case elfSymtab e of
      [] -> pure $ []
      [tbl] -> pure $ V.toList (elfSymbolTableEntries tbl)
      _ -> fail "Elf contains multiple symbol tables."
  let (symErrs, resolvedEntries) = resolveElfFuncSymbols mem indexMap entries
  forM_ symErrs $ \err -> do
    hPutStrLn stderr $ show err

  -- Check for unresolved symbols
  pure $ resolvedEntries

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
------------------------------------------------------------------------
-- Explore a control flow graph.

ppSymbol :: MemWidth w => MemSegmentOff w -> AddrSymMap w -> String
ppSymbol addr sym_map =
  case Map.lookup addr sym_map of
    Just fnName -> show addr ++ " (" ++ UTF8.toString fnName ++ ")"
    Nothing  -> show addr

blockLogFn :: MemWidth w => DiscoveryOptions -> MemSegmentOff w -> ST RealWorld ()
blockLogFn disOpt
  | logAtAnalyzeBlock disOpt = \addr ->
     ioToST $ hPutStrLn stderr $ "  Analyzing block: " ++ show addr
  | otherwise = \_ -> pure ()

resolveFuns :: MemWidth (RegAddrWidth (ArchReg arch))
            => DiscoveryOptions -- ^ Options controlling discovery
            -> AddrSymMap (ArchAddrWidth arch)
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

-- | Explore until we have found all functions we can.
mkFinalCFGWithSyms :: forall arch
                   .  ArchitectureInfo arch
                   -> DiscoveryOptions -- ^ Options controlling discovery
                   -> Memory (ArchAddrWidth arch)
                   -> [MemSegmentOff (ArchAddrWidth arch)]       -- Additional entry points
                   -> AddrSymMap (ArchAddrWidth arch)
                   -> IO (DiscoveryState arch)
mkFinalCFGWithSyms ainfo disOpt mem initEntries symMap = withArchConstraints ainfo $ do
  let initState
        = emptyDiscoveryState mem symMap ainfo
        & markAddrsAsFunction InitAddr initEntries
  -- Add symbol table entries to discovery state if requested
  let postSymState
        | exploreFunctionSymbols disOpt =
            initState & markAddrsAsFunction InitAddr (Map.keys symMap)
        | otherwise = initState
  -- Discover functions
  postPhase1Discovery <- resolveFuns disOpt symMap postSymState
  -- Discovery functions from memory
  if exploreCodeAddrInMem disOpt then do
    let mem_contents = withArchConstraints ainfo $ memAsAddrPairs mem LittleEndian
    resolveFuns disOpt symMap $ postPhase1Discovery & exploreMemPointers mem_contents
   else
    return postPhase1Discovery

-- | Discover code in the binary identified by the given path.
discoverBinary :: FilePath
              -> DiscoveryOptions -- ^ Options controlling discovery
              -> IO (Some DiscoveryState)
discoverBinary path disOpt = do
  Some e <- readSomeElf path
  -- Get architecture information for elf
  SomeArch ainfo <- getElfArchInfo e
  -- Get
  (secMap, mem, initEntries) <- mkInitialCFGWithSyms e disOpt
  -- Get symbol table map
  entries <- getSymbolMap mem secMap e
  let symMap = Map.fromList [ (addr,nm) | (nm,addr) <- entries ]
  Some <$> mkFinalCFGWithSyms ainfo disOpt mem initEntries symMap

$(pure [])

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
-- Execution

getX86ElfArchInfo :: Elf 64 -> IO (ArchitectureInfo X86_64, SyscallPersonality, String)
getX86ElfArchInfo e =
  case elfOSABI e of
    ELFOSABI_LINUX   -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_SYSV    -> pure (x86_64_linux_info,   linux_syscallPersonality,   "Linux")
    ELFOSABI_FREEBSD -> pure (x86_64_freeBSD_info, freeBSD_syscallPersonality, "FreeBSD")
    abi              ->
     fail $ "Do not support " ++ show EM_X86_64 ++ "-" ++ show abi ++ "binaries."


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

-- | Create a discovery state and symbol-address map
discoverX86Binary :: FilePath -- ^ Path to binary for exploring CFG
                  -> DiscoveryOptions -- ^ Options controlling discovery
                  -> IO ( SyscallPersonality
                        , String
                        , DiscoveryState X86_64
                        , AddrSymMap 64
                        , Map BS.ByteString (MemSegmentOff 64)
                        )
discoverX86Binary path disOpt = do
  e <- readElf64 path
  (secMap, mem, initEntries) <- mkInitialCFGWithSyms e disOpt
  (ainfo, sysp, pers) <- getX86ElfArchInfo e
  entries <- getSymbolMap mem secMap e
  let addrSymMap = Map.fromList [ (addr,nm) | (nm,addr) <- entries ]
  finalState <- mkFinalCFGWithSyms ainfo disOpt mem initEntries addrSymMap
  let symAddrMap = Map.fromList entries
  pure (sysp, pers, finalState, addrSymMap, symAddrMap)

-- | Create a discovery state and symbol-address map
discoverX86Elf :: FilePath -- ^ Path to binary for exploring CFG
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> IO ( Elf 64
                     , SyscallPersonality
                     , String
                     , DiscoveryState X86_64
                     , AddrSymMap 64
                     , Map BS.ByteString (MemSegmentOff 64)
                     )
discoverX86Elf path disOpt = do
  e <- readElf64 path
  (secMap, mem, initEntries) <- mkInitialCFGWithSyms e disOpt
  (ainfo, sysp, pers) <- getX86ElfArchInfo e
  entries <- getSymbolMap mem secMap e
  let addrSymMap = Map.fromList [ (addr,nm) | (nm,addr) <- entries ]
  finalState <- mkFinalCFGWithSyms ainfo disOpt mem initEntries addrSymMap
  let symAddrMap = Map.fromList entries
  pure (e, sysp, pers, finalState, addrSymMap, symAddrMap)
