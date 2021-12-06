module Main ( main ) where

import System.FilePath.Glob ( namesMatching )
import qualified Test.Tasty as T

import Reopt.TypeInference.Constraints
  (TyVar(..),
   Ty(..),
   andTy,
   orTy,
   ptrTy,
   TyConstraint(..),
   andC,
   orC,
   initContext,
   processAtomicConstraints,
   int32Ty,
   int64Ty)
import qualified ReoptTests as T
import qualified Prettyprinter as PP

constraintTest :: String -> [TyConstraint] -> IO ()
constraintTest testName cs = do
  putStrLn $ "\nConstraint Test: " ++ testName
  let initCtx = initContext cs
  putStrLn $ "  Initial Context:"
  putStrLn (show $ PP.indent 4 $ PP.pretty initCtx)
  let resultCtx = processAtomicConstraints initCtx
  putStrLn $ "  Result Context:"
  putStrLn (show $ PP.indent 4 $ PP.pretty resultCtx)


x0,x1,x2,x3 :: TyVar
x0Ty,x1Ty,x2Ty,x3Ty :: Ty
x0   = TyVar 0
x0Ty = VarTy x0
x1   = TyVar 1
x1Ty = VarTy x1
x2   = TyVar 2
x2Ty = VarTy x2
x3   = TyVar 3
x3Ty = VarTy x3

main :: IO ()
main = do
  -- constraintTest "No Constraints" []
  -- constraintTest "One EqC" [ EqC x0Ty TopTy ]
  -- constraintTest "Two EqC" [ EqC x0Ty TopTy
  --                          , EqC x1Ty int32Ty]
  -- constraintTest "Alias EqC 1"
  --   [ EqC x0Ty x1Ty
  --   , EqC x1Ty int32Ty]
  -- constraintTest "Alias EqC 2"
  --   [ EqC x1Ty x0Ty
  --   , EqC x1Ty int32Ty]
  -- constraintTest "Alias EqC 2"
  --   [ EqC x0Ty x1Ty
  --   , SubC x1Ty int64Ty
  --   , SubC x2Ty x1Ty
  --   , SubC x2Ty int64Ty]

  x64AsmTests <- namesMatching "tests/x64/*.exe"
  T.defaultMain $ T.testGroup "ReoptTests" [
    T.reoptTests x64AsmTests
    ]

