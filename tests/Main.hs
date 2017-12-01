module Main ( main ) where

import System.FilePath.Glob ( namesMatching )
import qualified Test.Tasty as T

import qualified ReoptTests as T

main :: IO ()
main = do
  x64AsmTests <- namesMatching "tests/x64/*.exe"
  T.defaultMain $ T.testGroup "ReoptTests" [
    T.reoptTests x64AsmTests
    ]

