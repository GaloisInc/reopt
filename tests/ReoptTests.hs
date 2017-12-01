module ReoptTests (
  reoptTests
  ) where

import Test.Tasty as T
import Test.Tasty.HUnit as T

reoptTests :: [FilePath] -> T.TestTree
reoptTests = T.testGroup "reopt" . map mkTest

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ putStrLn "test not implemented"
