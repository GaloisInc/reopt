{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module ReoptTests (
  reoptTests
  ) where


import           Control.Exception
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           Data.Macaw.Discovery
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Data.Map.Strict as Map
import           System.FilePath.Posix
import           System.IO
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>), (<>))

import qualified Reopt.CFG.LLVM as LLVM
import qualified Reopt.CFG.LLVM.X86 as LLVM
import           Reopt

reoptTests :: [FilePath] -> T.TestTree
reoptTests = T.testGroup "reopt" . map mkTest

logger :: String -> IO ()
logger _ = pure ()

defaultLLVMGenOptions :: LLVM.LLVMGenOptions
defaultLLVMGenOptions =
  LLVM.LLVMGenOptions { LLVM.llvmExceptionIsUB = False }

-- | This just tests that we can successfully run discovery,
-- function recovery and LLVM generation on the given input Elf file.
mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ do
  let blocks_path = replaceFileName fp (takeBaseName fp ++ ".blocks")
  let fns_path    = replaceFileName fp (takeBaseName fp ++ ".fns")
  let llvmPath   = replaceFileName fp (takeBaseName fp ++ ".ll")

  let loadOpts = MM.defaultLoadOptions
  let discOpts = DiscoveryOptions { exploreFunctionSymbols = True
                                  , exploreCodeAddrInMem   = False
                                  , logAtAnalyzeFunction   = False
                                  , logAtAnalyzeBlock      = False
                                  }

  (os, discState, addrSymMap, symAddrMap) <-
    discoverX86Binary fp loadOpts discOpts [] []

  writeFile blocks_path $ show $ ppDiscoveryStateBlocks discState
  let symAddrHashMap :: HashMap BSC.ByteString (MemSegmentOff 64)
      symAddrHashMap = HMap.fromList [ (nm,addr) | (nm,addr) <- Map.toList symAddrMap ]
  let hdr = emptyHeader
  recMod <- getFns logger addrSymMap symAddrHashMap hdr "reopt" (osPersonality os) discState
  writeFile fns_path $ show (vcat (pretty <$> recoveredDefs recMod))

  let archOps = LLVM.x86LLVMArchOps (show os)
  bracket (openBinaryFile llvmPath WriteMode) hClose $ \h -> do
    let (llvmContents, _ann) =
          llvmAssembly archOps defaultLLVMGenOptions recMod latestLLVMConfig
    Builder.hPutBuilder h llvmContents
