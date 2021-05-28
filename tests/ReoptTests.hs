{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ReoptTests
  ( reoptTests,
  )
where

import Control.Exception
import qualified Data.ByteString.Builder as Builder
import Data.Macaw.Discovery
import qualified Data.Macaw.Memory.ElfLoader as MM
import Prettyprinter
import Reopt
import System.FilePath.Posix
import System.IO
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

reoptTests :: [FilePath] -> T.TestTree
reoptTests = T.testGroup "reopt" . map mkTest

-- | Function that accepts warnings/errors from macaw.
logger :: a -> IO ()
logger _msg = pure () -- error $ "Test failed: " ++ msg

-- | This just tests that we can successfully run discovery,
-- function recovery and LLVM generation on the given input Elf file.
mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ do
  let blocks_path = replaceFileName fp (takeBaseName fp ++ ".blocks")
  let fns_path = replaceFileName fp (takeBaseName fp ++ ".fns")
  let llvmPath = replaceFileName fp (takeBaseName fp ++ ".ll")

  let loadOpts = MM.defaultLoadOptions
  let discOpts =
        DiscoveryOptions
          { exploreFunctionSymbols = True,
            exploreCodeAddrInMem = False,
            logAtAnalyzeFunction = False,
            logAtAnalyzeBlock = False
          }
  let hdrAnn = emptyAnnDeclarations
  let reoptOpts = ReoptOptions {roIncluded = [], roExcluded = []}
  contents <- checkedReadFile fp
  (_, os, discState, recMod, _) <-
    recoverX86Elf logger fp contents loadOpts discOpts reoptOpts hdrAnn "reopt"

  writeFile blocks_path $ show $ ppDiscoveryStateBlocks discState
  writeFile fns_path $ show (vcat (pretty <$> recoveredDefs recMod))

  let archOps = x86LLVMArchOps (show os)
  bracket (openBinaryFile llvmPath WriteMode) hClose $ \h -> do
    let (llvmContents, _ann) =
          llvmAssembly archOps defaultLLVMGenOptions recMod latestLLVMConfig
    Builder.hPutBuilder h llvmContents