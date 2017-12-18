{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module ReoptTests (
  reoptTests
  ) where


import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.Set as Set
import           Data.Typeable ( Typeable )
import           System.FilePath.Posix
import           System.IO (hPutStrLn, stderr)
import           System.IO.Temp (withSystemTempDirectory)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>), (<>))

import Paths_reopt

import qualified Data.ElfEdit as E
import           Data.Macaw.Discovery
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MM

import           Reopt.Interface
import qualified Reopt.CFG.LLVM as LLVM

reoptTests :: [FilePath] -> T.TestTree
reoptTests = T.testGroup "reopt" . map mkTest

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ withELF fp $ \e -> withSystemTempDirectory "reopt." $ \obj_dir -> do
  -- (secMap, mem) <- either fail return $ MM.memoryForElf loadOpts e
  (ainfo, sysp, syscallPostfix) <- getX86ElfArchInfo e
  let blocks_path = replaceFileName fp (takeBaseName fp ++ ".blocks")
  let fns_path    = replaceFileName fp (takeBaseName fp ++ ".fns")
  let llvm_path   = replaceFileName fp (takeBaseName fp ++ ".ll")
  let obj_path    = replaceFileName fp (takeBaseName fp ++ ".o")

  (secMap,_mem,disc_info,_) <- mkFinalCFGWithSyms ainfo e discOpts
  let addrSymMap  = elfAddrSymMap secMap e
  writeFile blocks_path $ show $ ppDiscoveryStateBlocks disc_info

  fns <- getFns (hPutStrLn stderr) sysp (elfSymAddrMap secMap e) Set.empty disc_info
  writeFile fns_path $ show (vcat (pretty <$> fns))

  let llvmVer = LLVM38
  let obj_llvm = llvmAssembly llvmVer $ LLVM.moduleForFunctions syscallPostfix addrSymMap fns
  writeFileBuilder llvm_path obj_llvm

  arch <- targetArch (E.elfOSABI e)
  libreopt_path <- (</> arch </> "libreopt.bc") <$> getLibDir
  let llvm_link_path = "llvm-link"
  llvm <- link_with_libreopt obj_dir libreopt_path llvm_link_path obj_llvm
  return ()
  -- compile_llvm_to_obj args arch llvm output_path

  where loadOpts = MM.LoadOptions { MM.loadRegionIndex = 0
                                  , MM.loadStyle       = MM.LoadBySegment
                                  , MM.includeBSS      = False
                                  }
        discOpts = DiscoveryOptions { logAtAnalyzeFunction   = False
                                    , logAtAnalyzeBlock      = False
                                    , exploreFunctionSymbols = False
                                    , exploreCodeAddrInMem   = False
                                    , forceMemLoadStyle      = Nothing -- Just MM.LoadBySegment
                                    }

withELF :: FilePath -> (E.Elf 64 -> IO ()) -> IO ()
withELF fp k = do
  bytes <- B.readFile fp
  case E.parseElf bytes of
    E.ElfHeaderError off msg ->
      error ("Error parsing ELF header at offset " ++ show off ++ ": " ++ msg)
    E.Elf32Res [] _e32 -> error "ELF32 is unsupported in the test suite"
    E.Elf64Res [] e64 -> k e64
    E.Elf32Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)
    E.Elf64Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)

withMemory :: forall w m a
            . (C.MonadThrow m, MM.MemWidth w, Integral (E.ElfWordType w))
           => E.Elf w
           -> (MM.Memory w -> m a)
           -> m a
withMemory e k = do
  let opt = MM.LoadOptions { MM.loadRegionIndex = 0
                           , MM.loadStyle = MM.LoadBySegment
                           , MM.includeBSS = True }
  case MM.memoryForElf opt e of
    Left err -> C.throwM (MemoryLoadError err)
    Right (_sim, mem) -> k mem

data ElfException = MemoryLoadError String
  deriving (Typeable, Show)

instance C.Exception ElfException
