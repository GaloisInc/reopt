{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module ElfX64Linux (
  elfX64LinuxTests
  ) where

import Control.Lens ( (^.) )
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable ( Typeable )
import Data.Word ( Word64 )
import qualified System.Exit as IO
import System.FilePath ( (<.>), dropExtension, takeFileName )
import qualified System.IO as IO
import qualified System.IO.Temp as IO
import qualified System.Process as P
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Text.Printf ( printf )
import Text.Read ( readMaybe )

import qualified Data.ElfEdit as E

import qualified Data.Parameterized.Some as PU
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Discovery.Info as MD
import qualified Reopt.CFG.Implementation as RO

elfX64LinuxTests :: [FilePath] -> T.TestTree
elfX64LinuxTests = T.testGroup "ELF x64 Linux" . map mkTest

data ExpectedResult = R { funcs :: [(Word64, [Word64])]
                        , ignoreBlocks :: [Word64]
                        }
                    deriving (Read, Show, Eq)

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ withAssembledCode asmFilename (testDiscovery fp)
  where
    asmFilename = dropExtension fp

testDiscovery :: FilePath -> E.Elf Word64 -> IO ()
testDiscovery expectedFilename elf =
  withMemory MM.Addr64 elf $ \mem -> do
    let Just entryPoint = MM.absoluteAddrSegment mem (fromIntegral (E.elfEntry elf))
    case MD.cfgFromAddrs RO.x86_64_linux_info mem M.empty [entryPoint] [] of
      PU.Some di -> do
        expectedString <- readFile expectedFilename
        case readMaybe expectedString of
          Nothing -> T.assertFailure ("Invalid expected result: " ++ show expectedString)
          Just er -> do
            let expectedEntries = M.fromList [ (entry, S.fromList starts) | (entry, starts) <- funcs er ]
                ignoredBlocks = S.fromList (ignoreBlocks er)
            F.forM_ (M.elems (di ^. MD.funInfo)) $ \dfi -> do
              let actualEntry = fromIntegral (MM.addrValue (MD.discoveredFunAddr dfi))
                  actualBlockStarts = S.fromList [ fromIntegral (MM.addrValue (MD.regionAddr pbr))
                                                 | pbr <- M.elems (dfi ^. MD.parsedBlocks)
                                                 ]
              case (S.member actualEntry ignoredBlocks, M.lookup actualEntry expectedEntries) of
                (True, _) -> return ()
                (_, Nothing) -> T.assertFailure (printf "Unexpected entry point: 0x%x" actualEntry)
                (_, Just expectedBlockStarts) ->
                  T.assertEqual (printf "Block starts for 0x%x" actualEntry) expectedBlockStarts (actualBlockStarts `S.difference` ignoredBlocks)

withAssembledCode :: FilePath -> (E.Elf Word64 -> IO ()) -> IO ()
withAssembledCode asmFile k = do
  IO.withSystemTempFile (takeFileName asmFile <.> "exe") $ \outfile exeH -> do
    IO.hClose exeH
    let p = P.proc "gcc" ["-nostdlib", "-o", outfile, asmFile]
    (_, _, _, ph) <- P.createProcess p
    ec <- P.waitForProcess ph
    case ec of
      IO.ExitFailure code -> T.assertFailure ("Failed to assemble file " ++ asmFile ++ " with exit code " ++ show code)
      IO.ExitSuccess -> withELF outfile k

withELF :: FilePath -> (E.Elf Word64 -> IO ()) -> IO ()
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
           => MM.AddrWidthRepr w
           -> E.Elf (E.ElfWordType w)
           -> (MM.Memory w -> m a)
           -> m a
withMemory relaWidth e k =
  case MM.memoryForElfSegments relaWidth e of
    Left err -> C.throwM (MemoryLoadError err)
    Right (_sim, mem) -> k mem

data ElfException = MemoryLoadError String
  deriving (Typeable, Show)

instance C.Exception ElfException
