module Main ( main ) where

import System.FilePath.Glob ( namesMatching )
import qualified Test.Tasty as T

import qualified ElfX64Linux as T

main :: IO ()
main = do
  x64AsmTests <- namesMatching "tests/x64/*.s.expected"
  T.defaultMain $ T.testGroup "ReoptTests" [
    T.elfX64LinuxTests x64AsmTests
    ]

