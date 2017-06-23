-- dump_vtables
-- Takes an ELF file as input and attempts to dump the contents of the
-- vtables.

{-# LANGUAGE DataKinds #-}

import Control.Exception
import Control.Lens
import Control.Monad
import Data.ElfEdit
import Data.Macaw.Memory
import Data.Macaw.Memory.ElfLoader
import Data.Maybe
import Data.Parameterized.Some
import Data.Word
import Numeric (showHex, showIntAtBase)
import Reopt
import System.Directory
import System.Environment
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V

main = do
  args <- getArgs
  case args of
    (bFileName:_) -> do
      e <- readElf64 bFileName
      let Right (sim, m) =  memoryForElf (LoadOptions LoadBySegment False) e
      let pairs = memAsAddrPairs m LittleEndian
      let vTableEntries = case elfSymtab e of
            (s:[]) -> V.filter vte $ elfSymbolTableEntries s
            _ -> error "Need exactly one symbol table in ELF file"
      let mw = V.map (fromJust . absoluteAddrSegment m . memWord . steValue)
               $ vTableEntries
               :: V.Vector (SegmentedAddr 64)
      mapM_ print mw
      return ()
--      mapM_ (print . (\x -> (steName x, showHex (steValue x) "", steSize x))) $ vTableEntries
    _ -> putStrLn "Please supply a file to dump."
  where vte = B.isPrefixOf (B.pack [95,90,84,86]) . steName
