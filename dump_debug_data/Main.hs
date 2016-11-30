module Main where

import Control.Monad
import Data.ElfEdit
import Data.Macaw.Dwarf
import Data.Macaw.Memory.ElfLoader
import System.Environment
import System.IO

run :: Elf v -> IO ()
run e = do
  case loadDwarf e of
    Left msg -> do
      hPutStrLn stderr "Fatal error:"
      hPutStrLn stderr $ "  " ++ msg
    Right (nonFatalErrors, cunits) -> do
      hPutStrLn stderr "Non fatal errors:"
      mapM_ (hPutStrLn stderr . ("  " ++)) nonFatalErrors
      mapM_ print cunits

main :: IO ()
main = do
  [path] <- getArgs
  se <- readElf path
  case se of
    Elf64 e -> run e
    Elf32 e -> run e
