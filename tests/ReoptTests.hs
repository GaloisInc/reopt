{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module ReoptTests (
  reoptTests
  ) where


import           Control.Exception
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as E
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           Data.Macaw.Discovery
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Data.Map.Strict as Map
import           Data.Typeable ( Typeable )
import           System.FilePath.Posix
import           System.IO
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
  let llvmPath   = replaceFileName fp (takeBaseName fp ++ ".ll")

  let loadOpts = MM.LoadOptions { MM.loadRegionIndex      = Just 0
                                , MM.loadRegionBaseOffset = 0
                                }
  let discOpts = DiscoveryOptions { exploreFunctionSymbols = False
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
    Builder.hPutBuilder h $
      llvmAssembly latestLLVMConfig $ LLVM.moduleForFunctions archOps recMod

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
