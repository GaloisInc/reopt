module Main ( main ) where

import System.FilePath.Glob ( namesMatching )
import qualified Test.Tasty as T

import qualified ReoptTests as T
import qualified TyConstraintTests as TC
import qualified Prettyprinter as PP

main :: IO ()
main = do

  x64AsmTests <- namesMatching "tests/x64/*.exe"
  T.defaultMain $ T.testGroup "ReoptTests" [
    TC.constraintTests,
    T.reoptTests x64AsmTests
    ]

