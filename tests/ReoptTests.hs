{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ReoptTests
  ( reoptTests,
  )
where

import qualified Data.ByteString.Builder as Builder
import Data.Macaw.Discovery
import qualified Data.Macaw.Memory.ElfLoader as MM
import Prettyprinter
import Reopt
import Reopt.Utils.Exit
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
  let reoptOpts = defaultReoptOptions
                  { roDiscoveryOptions =
                    DiscoveryOptions
                    { exploreFunctionSymbols = True,
                      exploreCodeAddrInMem = False,
                      logAtAnalyzeFunction = False,
                      logAtAnalyzeBlock = False
                    }
                  }
  let hdrAnn = emptyAnnDeclarations
  contents <- checkedReadFile fp

  hdrInfo <- either fail pure $ parseElfHeaderInfo64 fp contents
  mr <-
    runReoptM logger $ do
      recoverX86Elf loadOpts reoptOpts hdrAnn "reopt" hdrInfo
  (os, discState, recMod, _, _logEvents) <- either (fail . show) pure mr

  writeFile blocks_path $ show $ ppDiscoveryStateBlocks discState
  writeFile fns_path $ show (vcat (pretty <$> recoveredDefs recMod))

  withBinaryFile llvmPath WriteMode $ \h -> do
    let (llvmContents, _ann, _decl, _logEvents) =
          renderLLVMBitcode defaultLLVMGenOptions latestLLVMConfig os recMod
    Builder.hPutBuilder h llvmContents
