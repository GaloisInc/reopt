{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module ReoptTests (
  reoptTests
  ) where


import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.ElfEdit as E
import           Data.Macaw.Discovery
import qualified Data.Macaw.Memory.ElfLoader as MM
import           Data.Typeable ( Typeable )
import           System.FilePath.Posix
import           System.IO.Temp (withSystemTempDirectory)
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

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ withSystemTempDirectory "reopt." $ \_obj_dir -> do
  let blocks_path = replaceFileName fp (takeBaseName fp ++ ".blocks")
  let fns_path    = replaceFileName fp (takeBaseName fp ++ ".fns")
  let llvm_path   = replaceFileName fp (takeBaseName fp ++ ".ll")

  let loadOpts = MM.LoadOptions { MM.loadRegionIndex      = Just 0
                                , MM.loadRegionBaseOffset = 0
                                }
  let discOpts = DiscoveryOptions { exploreFunctionSymbols = False
                                  , exploreCodeAddrInMem   = False
                                  , logAtAnalyzeFunction   = False
                                  , logAtAnalyzeBlock      = False
                                  }

  (os, disc_info, addrSymMap) <-
    discoverX86Binary fp loadOpts discOpts [] []

  writeFile blocks_path $ show $ ppDiscoveryStateBlocks disc_info

  recMod <- getFns logger addrSymMap "reopt" (osPersonality os) disc_info
  writeFile fns_path $ show (vcat (pretty <$> recoveredDefs recMod))

  let archOps = LLVM.x86LLVMArchOps (show os)
  let obj_llvm = llvmAssembly latestLLVMConfig $ LLVM.moduleForFunctions archOps recMod
  writeFileBuilder llvm_path obj_llvm

  return ()

_withELF :: FilePath -> (E.Elf 64 -> IO ()) -> IO ()
_withELF fp k = do
  bytes <- B.readFile fp
  case E.parseElf bytes of
    E.ElfHeaderError off msg ->
      error ("Error parsing ELF header at offset " ++ show off ++ ": " ++ msg)
    E.Elf32Res [] _e32 -> error "ELF32 is unsupported in the test suite"
    E.Elf64Res [] e64 -> k e64
    E.Elf32Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)
    E.Elf64Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)

data ElfException = MemoryLoadError String
  deriving (Typeable, Show)

instance C.Exception ElfException
