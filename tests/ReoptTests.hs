{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module ReoptTests (
  reoptTests
  ) where


import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import           Data.Typeable ( Typeable )
import Test.Tasty as T
import Test.Tasty.HUnit as T

import qualified Data.ElfEdit as E
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MM

reoptTests :: [FilePath] -> T.TestTree
reoptTests = T.testGroup "reopt" . map mkTest

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ putStrLn "dummy test"

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

_withMemory :: forall w m a
            . (C.MonadThrow m, MM.MemWidth w, Integral (E.ElfWordType w))
           => E.Elf w
           -> (MM.Memory w -> m a)
           -> m a
_withMemory e k = do
  let opt = MM.LoadOptions { MM.loadRegionIndex = Just 0
                           , MM.loadStyleOverride = Just MM.LoadBySegment
                           , MM.includeBSS = True
                           }
  case MM.memoryForElf opt e of
    Left err -> C.throwM (MemoryLoadError err)
    Right (_sim, mem) -> k mem

data ElfException = MemoryLoadError String
  deriving (Typeable, Show)

instance C.Exception ElfException
