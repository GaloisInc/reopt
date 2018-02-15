{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
    -- * Utilities
  , resolveSymName
  ) where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Either
import           Data.ElfEdit
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Word
import           Numeric (readHex)
import           System.IO

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
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

$(pure [])

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

-- \
resolveIncludeFn :: Memory w
                 -> Map BS.ByteString (MemSegmentOff w)
                 -> [String] -- ^ Addresses to include
                 -> [String]
                 -> IO ([MemSegmentOff w], (MemSegmentOff w -> Bool)
resolveIncludeFn mem symMap [] excludeNames = do
  let (bad, excludeAddrs) = partitionEithers $ resolveSymAddr mem symMap  <$> excludeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList excludeAddrs
  pure ([], (`Set.notMember` s))
resolveIncludeFn mem symMap includeNames [] = do
  let (bad, includeAddrs) = partitionEithers $ resolveSymAddr mem symMap  <$> includeNames
  when (not (null bad)) $ do
    hPutStrLn stderr $ "Could not resolve symbols: " ++ unwords bad
  let s = Set.fromList includeAddrs
  pure $ (includeAddrs, (`Set.member` s))
resolveIncludeFn _ _ _ _ = do
  fail "Cannot both include and exclude specific addresses."

runCompleteDiscovery :: ArchitectureInfo arch
                     -> DiscoveryOptions
                     -- ^ Options controlling discovery
                     -> Memory (ArchAddrWidth arch)
                     -> Maybe (ArchSegmentOff arch)
                     -> [MemSymbol (ArchAddrWidth arch)]
                        -- ^ Symbols
                     -> [String] -- ^ Included addresses
                     -> [String] -- ^ Excluded addresses
                     -> IO ( DiscoveryState arch
                           , AddrSymMap (ArchAddrWidth arch)
                           , Map BS.ByteString (ArchSegmentOff arch)
                           )
runCompleteDiscovery ainfo disOpt mem entry symbols includeAddr excludeAddr = do
  let addrSymMap = Map.fromList [ (memSymbolStart msym, memSymbolName msym) | msym <- symbols ]
  let symAddrMap = Map.fromList [ (memSymbolName msym, memSymbolStart msym) | msym <- symbols ]
  fnPred <- resolveIncludeFn mem symAddrMap includeAddr excludeAddr
  let initEntries = maybeToList entry
  s <- completeDiscoveryState ainfo disOpt mem (maybeToList entry) addrSymMap fnPred
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
  (warnings, mem, entry, symbols) <- either fail pure $
    initElfDiscoveryInfo loadOpts e
  mapM_ (hPutStrLn stderr) warnings
  (s, _,_) <- runCompleteDiscovery ainfo disOpt mem entry symbols includeAddr excludeAddr
  pure (Some s)

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
osLinkName Linux = "x86_64-unknown-linux-elf"
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
       -> DiscoveryState X86_64
          -- ^ Information about original binary recovered from static analysis.
       -> IO [Function]
getFns sysp info = do

  let mem = memory info

  let entries = exploredFunctions info

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
                  -> LoadOptions -- ^ Options controling loading
                  -> DiscoveryOptions -- ^ Options controlling discovery
                  -> [String] -- ^ Included addresses
                  -> [String] -- ^ Excluded addresses
                  -> IO ( X86OS
                        , DiscoveryState X86_64
                        , AddrSymMap 64
                        )
discoverX86Binary path loadOpts disOpt includeAddr excludeAddr = do
  e <- readElf64 path
  (warnings, mem, entry, symbols) <- either fail pure $
    initElfDiscoveryInfo loadOpts e
  mapM_ (hPutStrLn stderr) warnings
  os <- getX86ElfArchInfo e
  (finalState, addrSymMap, _) <-
    runCompleteDiscovery (osArchitectureInfo os) disOpt mem entry symbols includeAddr excludeAddr
  pure (os, finalState, addrSymMap)

-- | Create a discovery state and symbol-address map
discoverX86Elf :: FilePath -- ^ Path to binary for exploring CFG
               -> LoadOptions -- ^ Options controling loading
               -> DiscoveryOptions -- ^ Options controlling discovery
               -> [String] -- ^ Included addresses
               -> [String] -- ^ Excluded addresses
               -> IO ( Elf 64
                     , X86OS
                     , DiscoveryState X86_64
                     , AddrSymMap 64
                     , Map BS.ByteString (MemSegmentOff 64)
                     )
discoverX86Elf path loadOpts disOpt includeAddr excludeAddr = do
  e <- readElf64 path
  (warnings, mem, entry, symbols) <- either fail pure $
    initElfDiscoveryInfo loadOpts e
  mapM_ (hPutStrLn stderr) warnings
  os <- getX86ElfArchInfo e
  (finalState, addrSymMap, symAddrMap) <-
    runCompleteDiscovery (osArchitectureInfo os) disOpt mem entry symbols includeAddr excludeAddr
  pure (e, os, finalState, addrSymMap, symAddrMap)
