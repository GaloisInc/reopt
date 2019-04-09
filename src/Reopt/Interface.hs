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
    -- * Functions
  , getFns
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
import           Data.ElfEdit
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Parameterized.Some
import qualified Data.Set as Set
import           Data.Word
import           System.IO
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as LPP
import qualified Text.PrettyPrint.HughesPJ as HPJ

import           Data.Macaw.Analysis.FunctionArgs
import           Data.Macaw.Architecture.Info (ArchitectureInfo(..))
import           Data.Macaw.CFG
import           Data.Macaw.Discovery hiding (AddrSymMap)
import           Data.Macaw.Memory.ElfLoader

import           Data.Macaw.X86
import           Data.Macaw.X86.SyscallInfo

import           Reopt
import           Reopt.CFG.FnRep (Function(..))
import           Reopt.CFG.FnRep.X86 (X86FunctionType(..))
import           Reopt.CFG.FunctionCheck
import           Reopt.CFG.LLVM as LLVM
import           Reopt.CFG.Recovery
import qualified Reopt.ExternalTools as Ext
import           Reopt.Relinker

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
                             -> Map (MemSegmentOff 64) X86FunctionType
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
      orderPadArgs :: (RegisterSet X86Reg, RegisterSet X86Reg) -> X86FunctionType
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
  let fArgs ::  Map (MemSegmentOff 64) X86FunctionType
      fArgs = inferFunctionTypeFromDemands fDems
  seq fArgs $ do
   fmap catMaybes $ forM entries $ \(Some finfo) -> do
    let entry = discoveredFunAddr finfo
    case checkFunction finfo of
      FunctionOK -> do
        case recoverFunction sysp fArgs mem finfo of
          Left msg -> do
            logger $ "Could not recover function " ++ show entry ++ ":\n  " ++ msg
            pure Nothing
          Right (warnings, fn) -> do
            mapM_ logger warnings
            pure (Just fn)
      FunctionHasPLT -> do
        -- Skip PLT functions with no error message.
        pure Nothing
      FunctionIncomplete -> do
        logger $ "Skipped incomplete function at " ++ show entry
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
  let w = case segoffAsAbsoluteAddr a of
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
