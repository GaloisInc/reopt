module Main (main) where

import qualified Data.ByteString as BS
import           Data.ElfEdit
import           Data.Macaw.Dwarf
import           System.Environment
import           System.IO

run :: Elf v -> IO ()
run e = do
  case dwarfInfoFromElf e of
    (nonFatalErrors, cunits) -> do
      hPutStrLn stderr "Errors:"
      mapM_ (hPutStrLn stderr . ("  " ++)) nonFatalErrors
      mapM_ print cunits

main :: IO ()
main = do
  [path] <- getArgs
  bs <- BS.readFile path
  case parseElf bs of
    Elf64Res _ e -> do
      run e
    Elf32Res _ e -> do
      run e
    ElfHeaderError _ msg -> do
      hPutStrLn stderr $
        "Error parsing file:\n"
        ++ "  " ++ msg
