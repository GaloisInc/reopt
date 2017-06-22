-- dump_vtables
-- Takes an ELF file as input and attempts to dump the contents of the
-- vtables.

import Control.Exception
import Control.Lens
import Control.Monad
import Data.ElfEdit
import Data.Macaw.Memory
import Data.Parameterized.Some
import Reopt
import System.Directory
import System.Environment
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V

copy source dest = do
  contents <- B.readFile source
  bracketOnError
    (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
    (\(tempName, tempHandle) -> do
        B.hPutStr tempHandle contents
        hClose tempHandle
        renameFile tempName dest
        putStrLn $ "Copied file " ++ source ++ " to " ++ dest ++ ".")

regularChunks :: Int -> B.ByteString -> [B.ByteString]
regularChunks sz bs
  | B.length bs < sz = []
  | otherwise = B.take sz bs : regularChunks sz (B.drop sz bs)

isLoadSegment :: ElfSegment w -> Bool
isLoadSegment seg = case elfSegmentType seg of
  PT_LOAD -> True
  _       -> False

main = do
  args <- getArgs
  case args of
    (bFileName:_) -> do
      e <- readElf64 bFileName
      let vTableEntries = case elfSymtab e of
            (s:[]) -> V.filter vte $ elfSymbolTableEntries s
            _ -> error "Need exactly one symbol table in ELF file"
      mapM_ (print . steName) $ vTableEntries
    _ -> putStrLn "Please supply a file to dump."
  where vte = B.isPrefixOf (B.pack [95,90,84,86]) . steName
