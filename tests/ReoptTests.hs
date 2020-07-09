{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module ReoptTests (
  reoptTests
  ) where


import           Control.Exception
import qualified Data.ByteString.Builder as Builder
import           Data.Macaw.Discovery
import qualified Data.Macaw.Memory.ElfLoader as MM
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

-- | Function that accepts warnings/errors from macaw.
logger :: GetFnsLogEvent -> IO ()
logger _msg = pure () -- error $ "Test failed: " ++ msg

defaultLLVMGenOptions :: LLVM.LLVMGenOptions
defaultLLVMGenOptions =
  LLVM.LLVMGenOptions { LLVM.mcExceptionIsUB = False }

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
  let hdrAnn = emptyAnnDeclarations
  (_, os, discState, recMod) <-
    discoverX86Elf logger fp loadOpts discOpts [] [] hdrAnn "reopt"

  writeFile blocks_path $ show $ ppDiscoveryStateBlocks discState
  writeFile fns_path $ show (vcat (pretty <$> recoveredDefs recMod))

  let archOps = LLVM.x86LLVMArchOps (show os)
  bracket (openBinaryFile llvmPath WriteMode) hClose $ \h -> do
    let (llvmContents, _ann) =
          llvmAssembly archOps defaultLLVMGenOptions recMod latestLLVMConfig
    Builder.hPutBuilder h llvmContents
