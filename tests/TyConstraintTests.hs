{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TyConstraintTests
  ( constraintTests,
  )
where

import Data.List (sortBy)
import qualified Data.Map as Map
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Prettyprinter as PP

import Reopt.TypeInference.Constraints
  (TyVar(..),
   Ty(..),
   TyConstraint(..),
   unifyConstraints,
   orC,
   andC,
   andTy,
   orTy,
   ptrTy,
   int32Ty,
   int64Ty)

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

constraintTests :: T.TestTree
constraintTests = T.testGroup "Type Constraint Tests"
  [ eqCTests
  , subCTests
  , orCTests
  ]

-- Simple tests having to do with equality constraints
eqCTests :: T.TestTree
eqCTests = T.testGroup "Equality Constraint Tests"
  [ mkTest "Single EqC var left"  [EqC x0Ty int64Ty] [(x0, int64Ty)],
    mkTest "Single EqC var right" [EqC int64Ty x0Ty] [(x0, int64Ty)],
    mkTest "Multiple EqCs" [EqC x0Ty int64Ty, EqC x1Ty int32Ty]
                            [(x0, int64Ty), (x1, int32Ty)],
    mkTest "EqC Transitivity 1" [EqC x0Ty x1Ty, EqC x1Ty int32Ty]
                                 [(x0, int32Ty), (x1, int32Ty)],
    mkTest "EqC Transitivity 2" [EqC x1Ty int32Ty, EqC x0Ty x1Ty]
                                 [(x0, int32Ty), (x1, int32Ty)],
    mkTest "EqC Transitivity 2" [EqC x1Ty x2Ty, EqC x0Ty x1Ty, EqC x2Ty int64Ty]
                                 [(x0, int64Ty), (x1, int64Ty), (x2, int64Ty)]
  ]

-- Simple tests having to do with equality constraints.
subCTests :: T.TestTree
subCTests = T.testGroup "Equality Constraint Tests"
  [ mkTest "Single SubC upper bound"  [SubC x0Ty int64Ty] [(x0, int64Ty)],
    mkTest "Single SubC lower bound"  [SubC int64Ty x0Ty] [(x0, int64Ty)],
    mkTest "Single SubC lower+upper bounds" [SubC x0Ty int64Ty, SubC int32Ty x0Ty]
                                            [(x0, int32Ty)],
    mkTest "Multiple SubCs 1"  [SubC int64Ty x0Ty, SubC x0Ty int64Ty] [(x0, int64Ty)],
    mkTest "Multiple SubCs 2"  [SubC int64Ty x0Ty, SubC x0Ty int64Ty, SubC x1Ty int32Ty]
                               [(x0, int64Ty), (x1, int32Ty)],
    mkTest "Multiple SubCs 3"  [SubC int64Ty x0Ty, SubC x0Ty int64Ty, SubC x1Ty int32Ty, SubC x1Ty x0Ty]
                               [(x0, int64Ty), (x1, int32Ty)],
    mkTest "SubC Transitivity 1"
      [SubC x0Ty int64Ty, SubC x1Ty x0Ty]
      [(x0, int64Ty), (x1, int64Ty)],
    mkTest "SubC Transitivity 2"
      [SubC x0Ty int64Ty, SubC x1Ty x0Ty]
      [(x0, int64Ty), (x1, int64Ty)]
  ]

-- | Some (basic?) tests focusing on disjunctions.
orCTests :: T.TestTree
orCTests = T.testGroup "Disjunctive Constraint Tests"
  [ mkTest "disjunctive syllogism 1"
      [ orC [andC [EqC x0Ty (ptrTy 64 int64Ty), SubC x1Ty int64Ty],
             andC [EqC x1Ty (ptrTy 64 int64Ty), SubC x0Ty int64Ty]],
        SubC x0Ty int64Ty]
      [(x0, int64Ty), (x1, ptrTy 64 int64Ty)],
    mkTest "disjunctive syllogism 2"
      [ orC [andC [EqC x0Ty (ptrTy 64 int64Ty), SubC x1Ty int64Ty],
             andC [EqC x1Ty (ptrTy 64 int64Ty), SubC x0Ty int64Ty],
             andC [SubC x0Ty int64Ty, SubC x0Ty int64Ty]],
        SubC int32Ty x0Ty,
        SubC int32Ty x1Ty]
      [(x0, int32Ty), (x1, int32Ty)],
    mkTest "disjunctive syllogism 3"
      [ orC [andC [EqC x0Ty (ptrTy 64 int64Ty), SubC x1Ty int64Ty],
             andC [EqC x1Ty (ptrTy 64 int64Ty), SubC x0Ty int64Ty],
             andC [SubC x0Ty int64Ty, SubC x0Ty int64Ty]],
        SubC x2Ty x0Ty,
        SubC x3Ty x1Ty,
        SubC int32Ty x2Ty,
        SubC int32Ty x3Ty]
      [(x0, int32Ty), (x1, int32Ty), (x2, int32Ty), (x3, int32Ty)]
  ]

newtype TypeEnv = TypeEnv [(TyVar, Ty)]
  deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv xs) = show $ map (\(x,xTy) -> (PP.pretty x, PP.pretty xTy)) xs

tyEnv :: [(TyVar, Ty)] -> TypeEnv
tyEnv = let cmp (x,_) (y,_) = compare x y
          in TypeEnv . sortBy cmp

mkTest :: String -> [TyConstraint] -> [(TyVar, Ty)] -> T.TestTree
mkTest name cs expected =
  let actual = Map.toList $ unifyConstraints cs
    in T.testCase name $ (tyEnv actual) T.@?= (tyEnv expected)




