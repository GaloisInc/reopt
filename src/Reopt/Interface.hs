{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Interface
  ( -- * Binary discovery
    DiscoveryOptions(..)
  , defaultDiscoveryOptions
  , discoverBinary
    -- * Functions
  , getFns
    -- * Architecture info
  , SomeArchitectureInfo(..)
  , getElfArchInfo
    -- * X86 specific
  , X86OS(..)
  , osPersonality
  , osArchitectureInfo
  , osLinkName
  , discoverX86Binary
  , discoverX86Elf
    -- * LLVM
  , LLVMVersion(..)
  , asLLVMVersion
  , llvmAssembly
    -- * Redirections
  , ControlFlowTargetSet
  , discoveryControlFlowTargets
  , elfSegmentMap
  , addrRedirection
    -- * Utilities
  , resolveSymName
  , compileLLVM
  , writeFileBuilder
  ) where

import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Except
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
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Word
import           Numeric (readHex)
import           System.IO
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import qualified Text.PrettyPrint.HughesPJ as HPJ

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.SyscallInfo

import           Reopt
import           Reopt.CFG.FnRep (Function(..), FunctionType, FunctionTypeMap)
import           Reopt.CFG.FnRep.X86 (X86FunctionType(..))
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
-- Architecture info

data SomeArchitectureInfo w =
  forall arch
    . ( w ~ RegAddrWidth (ArchReg arch)
      )
    => SomeArch (ArchitectureInfo arch)

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
      pure (SomeArch armArchle)
#endif
    (cl, arch, abi) -> do
     fail $ "Do not support " ++ show (elfClassBitWidth cl) ++ "-bit "
            ++ show arch ++ "-" ++ show abi ++ "binaries."

------------------------------------------------------------------------
-- Resolve symbol addresses

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
                           , AddrSymMap (ArchAddrWidth arch)
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

------------------------------------------------------------------------
-- Explore a control flow graph.

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

getX86ElfArchInfo :: Elf 64 -> IO X86OS
getX86ElfArchInfo e =
  case elfOSABI e of
    ELFOSABI_LINUX   -> pure Linux
    ELFOSABI_SYSV    -> pure Linux
    ELFOSABI_FREEBSD -> pure FreeBSD
    ELFOSABI_NETBSD  -> pure FreeBSD
    abi              -> fail $ "Do not support " ++ show EM_X86_64 ++ "-" ++ show abi ++ "binaries."

inferFunctionTypeFromDemands :: Map (MemSegmentOff 64) (DemandSet X86Reg)
                             -> FunctionTypeMap X86_64
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
      orderPadArgs :: (RegisterSet X86Reg, RegisterSet X86Reg) -> FunctionType X86_64
      orderPadArgs (args, rets) =
        X86FunctionType { fnNIntArgs = maximumArgPrefix x86ArgumentRegs args
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
getFns :: Monad m
       => (String -> m ())
       -> SyscallPersonality
       -> DiscoveryState X86_64
          -- ^ Information about original binary recovered from static analysis.
       -> m [Function X86_64]
getFns logger sysp info = do

  let mem = memory info

  let entries = exploredFunctions info

  let fDems :: Map (MemSegmentOff 64) (DemandSet X86Reg)
      fDems = functionDemands (x86DemandInfo sysp) info
  let fArgs :: FunctionTypeMap X86_64
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

-- | Create a discovery state and symbol-address map
--
--  Note. This prints warnings to stderr
discoverX86Elf :: FilePath -- ^ Path to binary for exploring CFG
               -> LoadOptions -- ^ Options controling loading
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> [String] -- ^ Included addresses (if empty then all addresses included)
               -> [String] -- ^ Excluded addresses
               -> IO ( Elf 64
                     , X86OS
                     , DiscoveryState X86_64
                     , AddrSymMap 64
                     , Map BS.ByteString [MemSegmentOff 64]
                     )
discoverX86Elf path loadOpts disOpt includeAddr excludeAddr = do
  e <- readElf64 path
  os <- getX86ElfArchInfo e
  (discState, addrSymMap, symAddrMap) <-
    runCompleteDiscovery loadOpts disOpt e (osArchitectureInfo os) includeAddr excludeAddr
  pure (e, os, discState, addrSymMap, symAddrMap)

-- | Create a discovery state and symbol-address map
--
--  Note. This prints warnings to stderr
discoverX86Binary :: FilePath -- ^ Path to binary for exploring CFG
                  -> LoadOptions -- ^ Options controling loading
                  -> DiscoveryOptions -- ^ Options controlling discovery
                  -> [String] -- ^ Included addresses
                  -> [String] -- ^ Excluded addresses
                  -> IO ( X86OS
                        , DiscoveryState X86_64
                        , AddrSymMap 64
                        )
discoverX86Binary path loadOpts disOpt includeAddr excludeAddr = do
  (_,os,discState,addrSymMap,_) <-
    discoverX86Elf path loadOpts disOpt includeAddr excludeAddr
  pure (os, discState, addrSymMap)

llvmAssembly :: LLVMVersion -> L.Module -> Builder.Builder
llvmAssembly v m = HPJ.fullRender HPJ.PageMode 10000 1 pp mempty (ppLLVM v m)
  where pp :: HPJ.TextDetails -> Builder.Builder -> Builder.Builder
        pp (HPJ.Chr c)  b = Builder.charUtf8 c <> b
        pp (HPJ.Str s)  b = Builder.stringUtf8 s <> b
        pp (HPJ.PStr s) b = Builder.stringUtf8 s <> b

--------------------------------------------------------------------------------
-- Redirections

-- | Compile a bytestring containing LLVM assembly or bitcode into an object.
compileLLVM :: Int
            -> FilePath
            -> FilePath
            -> FilePath
            -> String
            -> Builder.Builder
            -> FilePath
            -> IO ()
compileLLVM optLevel optPath llcPath gasPath arch llvm obj_path = do
  -- Run llvm on resulting binary
  putStrLn "Compiling new code"
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
    Ext.run_gas gasPath asm obj_path

  case mres of
    Left f -> fail $ show f
    Right () -> return ()

writeFileBuilder :: FilePath -> Builder.Builder -> IO ()
writeFileBuilder nm b = bracket (openBinaryFile nm WriteMode) hClose (\h -> Builder.hPutBuilder h b)

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
   in foldl' addFunctionEntryPoint m (Map.keys (symbolNames info))

-- | Maps virtual addresses to the phdr at them.
type ElfSegmentMap w = Map (ElfWordType w) (Phdr w)

-- | Create an elf segment map from a layout.
elfSegmentMap :: forall w . ElfLayout w -> ElfSegmentMap w
elfSegmentMap l = elfClassInstances (elfLayoutClass l) $ foldl' insertElfSegment Map.empty (allPhdrs l)
  where insertElfSegment ::  Ord (ElfWordType w) => ElfSegmentMap w -> Phdr w -> ElfSegmentMap w
        insertElfSegment m p
          | phdrSegmentType p == PT_LOAD = Map.insert a p m
          | otherwise = m
          where a = phdrSegmentVirtAddr p

-- | Lookup an address in the segment map, returning the index of the phdr
-- and the offset.
lookupElfOffset :: ElfSegmentMap 64 -> Word64 -> Maybe (Word16, Word64)
lookupElfOffset m a =
  case Map.lookupLE a m of
    Just (base, phdr) | a < base + phdrFileSize phdr ->
        Just (phdrSegmentIndex phdr, a - base)
    _ -> Nothing

-- | This creates a code redirection or returns the address as failing.
addrRedirection :: ControlFlowTargetSet 64
                -> LLVM.AddrSymMap 64
                -> ElfSegmentMap 64
                -> Function X86_64
                -> Either (MemSegmentOff 64) (CodeRedirection Word64)
addrRedirection tgts addrSymMap m f = do
  let a = fnAddr f
  let w = case msegAddr a of
            Just absAddr -> fromIntegral absAddr
            Nothing -> error "Redirection does not yet support relocatable binaries."
  case lookupElfOffset m w of
    Nothing -> Left (fnAddr f)
    Just (idx,off)
        | idx /= 0 -> error "addrRedirection should have 0 index."
        | otherwise -> Right redir
      where Right nmFun = LLVM.llvmFunctionName addrSymMap "reopt"
            L.Symbol sym_name = nmFun (fnAddr f)
            redir = CodeRedirection { redirSourceOffset = off
                                    , redirSourceSize   = fromIntegral (lookupControlFlowTargetSpace (fnAddr f) tgts)
                                    , redirTarget       = UTF8.fromString sym_name
                                    }
