{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TyConstraintTests
  ( absurdTests,
    tyEnvTests,
  )
where

import Data.Bifunctor (bimap)
import Data.Function (on)
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
   ptrTy,
   int32Ty,
   int64Ty,
   initContext,
   reduceContext,
   absurdC)

x0,x1,x2,x3,x4 :: TyVar
x0Ty,x1Ty,x2Ty,x3Ty,x4Ty :: Ty
x0   = TyVar 0
x0Ty = VarTy x0
x1   = TyVar 1
x1Ty = VarTy x1
x2   = TyVar 2
x2Ty = VarTy x2
x3   = TyVar 3
x3Ty = VarTy x3
x4   = TyVar 4
x4Ty = VarTy x4

----------------------------------------------------------------------------
-- Constraint Satisfiability/Absurdity tests

-- | `mkAbsurdTests testName ctxCs tgtCs` constructs a context from `ctxCs` and
-- checks that each constraints in `tgtCs` is either absurd or not (depending on
-- the accompanying Bool).
mkAbsurdTests :: String -> [TyConstraint] -> [(TyConstraint, Bool)] -> T.TestTree
mkAbsurdTests name contextConstraints unsatisfiableConstraints =
  T.testGroup name $ map singleTest unsatisfiableConstraints
  where ctx = reduceContext $ initContext contextConstraints
        singleTest (c, expected) =
          T.testCase (show $ (PP.pretty c))
          $ T.assertEqual "absurdC"
                          expected
                          (absurdC ctx c)

absurdTests :: T.TestTree
absurdTests = T.testGroup "Constraint Satisfiability Tests"
  [ mkAbsurdTests
     "simple equality constraints"
     []
     [(EqC int32Ty int64Ty, True),
      (EqC (ptrTy 64 int32Ty) (ptrTy 64 int64Ty), True),
      (EqC int64Ty int64Ty, False),
      (EqC (ptrTy 64 int64Ty) (ptrTy 64 int64Ty), False)],
    mkAbsurdTests
     "simple equality constraints with type variable equalities"
     [(EqC x0Ty int32Ty), (EqC x1Ty int64Ty)]
     [(EqC x0Ty x1Ty, True),
      (EqC (ptrTy 64 x0Ty) (ptrTy 64 x1Ty), True),
      (EqC x1Ty x1Ty, False),
      (EqC (ptrTy 64 x1Ty) (ptrTy 64 x1Ty), False)],
    mkAbsurdTests
     "simple equality constraints with bounded type variables 1"
     [(SubC x0Ty int32Ty), (SubC x1Ty int64Ty)]
     [(EqC x0Ty x1Ty, False),
      (EqC (ptrTy 64 x0Ty) (ptrTy 64 x1Ty), False),
      (EqC x1Ty x1Ty, False),
      (EqC (ptrTy 64 x1Ty) (ptrTy 64 x1Ty), False)],
    mkAbsurdTests
     "simple equality constraints with bounded type variables 2"
     [(SubC x0Ty int32Ty), (SubC int32Ty x0Ty), (SubC x1Ty int64Ty), (SubC int64Ty x1Ty)]
     [(EqC x0Ty x1Ty, True),
      (EqC (ptrTy 64 x0Ty) (ptrTy 64 x1Ty), True),
      (EqC x1Ty x1Ty, False),
      (EqC (ptrTy 64 x1Ty) (ptrTy 64 x1Ty), False)],
    mkAbsurdTests
     "simple subtype constraints"
     []
     [(SubC int32Ty int64Ty, False),
      (SubC (ptrTy 64 int32Ty) (ptrTy 64 int64Ty), False),
      (SubC int64Ty int64Ty, False),
      (SubC int64Ty int32Ty, True),
      (SubC (ptrTy 64 int64Ty) (ptrTy 64 int32Ty), True)],
    mkAbsurdTests
     "simple subtype constraints with type variable equalities"
     [(EqC x0Ty int32Ty), (EqC x1Ty int64Ty)]
     [(SubC x0Ty x1Ty, False),
      (SubC (ptrTy 64 x0Ty) (ptrTy 64 x1Ty), False),
      (SubC x1Ty x1Ty, False),
      (SubC x1Ty x0Ty, True),
      (SubC (ptrTy 64 x1Ty) (ptrTy 64 x0Ty), True)]]

----------------------------------------------------------------------------
-- Type Environment tests


newtype TypeEnv = TypeEnv [(TyVar, Ty)]
  deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv xs) = show $ bimap PP.pretty PP.pretty <$> xs

tyEnv :: [(TyVar, Ty)] -> TypeEnv
tyEnv = TypeEnv . sortBy (compare `on` fst)


mkTyEnvTest :: String -> [TyConstraint] -> [(TyVar, Ty)] -> T.TestTree
mkTyEnvTest name cs expected = T.testCase name $ (tyEnv actual) T.@?= (tyEnv expected)
  where actual = Map.toList $ unifyConstraints cs

tyEnvTests :: T.TestTree
tyEnvTests = T.testGroup "Type Constraint Tests"
  [ eqCTests
  , subCTests
  , orCTests
  , complexCTests
  ]

-- Simple tests having to do with equality constraints
eqCTests :: T.TestTree
eqCTests = T.testGroup "Equality Constraint Tests"
  [ mkTyEnvTest "Single EqC var left"  [EqC x0Ty int64Ty] [(x0, int64Ty)],
    mkTyEnvTest "Single EqC var right" [EqC int64Ty x0Ty] [(x0, int64Ty)],
    mkTyEnvTest "Multiple EqCs" [EqC x0Ty int64Ty, EqC x1Ty int32Ty]
                            [(x0, int64Ty), (x1, int32Ty)],
    mkTyEnvTest "EqC Transitivity 1" [EqC x0Ty x1Ty, EqC x1Ty int32Ty]
                                 [(x0, int32Ty), (x1, int32Ty)],
    mkTyEnvTest "EqC Transitivity 2" [EqC x1Ty int32Ty, EqC x0Ty x1Ty]
                                 [(x0, int32Ty), (x1, int32Ty)],
    mkTyEnvTest "EqC Transitivity 2" [EqC x1Ty x2Ty, EqC x0Ty x1Ty, EqC x2Ty int64Ty]
                                 [(x0, int64Ty), (x1, int64Ty), (x2, int64Ty)]
  ]

-- Simple tests having to do with equality constraints.
subCTests :: T.TestTree
subCTests = T.testGroup "Equality Constraint Tests"
  [ mkTyEnvTest "Single SubC upper bound"  [SubC x0Ty int64Ty] [(x0, int64Ty)],
    mkTyEnvTest "Single SubC lower bound"  [SubC int64Ty x0Ty] [(x0, int64Ty)],
    mkTyEnvTest "Single SubC lower+upper bounds" [SubC x0Ty int64Ty, SubC int32Ty x0Ty]
                                            [(x0, int32Ty)],
    mkTyEnvTest "Multiple SubCs 1"  [SubC int64Ty x0Ty, SubC x0Ty int64Ty] [(x0, int64Ty)],
    mkTyEnvTest "Multiple SubCs 2"  [SubC int64Ty x0Ty, SubC x0Ty int64Ty, SubC x1Ty int32Ty]
                               [(x0, int64Ty), (x1, int32Ty)],
    mkTyEnvTest "Multiple SubCs 3"  [SubC int64Ty x0Ty, SubC x0Ty int64Ty, SubC x1Ty int32Ty, SubC x1Ty x0Ty]
                               [(x0, int64Ty), (x1, int32Ty)],
    mkTyEnvTest "SubC Transitivity 1"
      [SubC x0Ty int64Ty, SubC x1Ty x0Ty]
      [(x0, int64Ty), (x1, int64Ty)],
    mkTyEnvTest "SubC Transitivity 2"
      [SubC x0Ty int64Ty, SubC x1Ty x0Ty]
      [(x0, int64Ty), (x1, int64Ty)]
  ]

-- | Some (basic?) tests focusing on disjunctions.
orCTests :: T.TestTree
orCTests = T.testGroup "Disjunctive Constraint Tests"
  [ mkTyEnvTest "disjunctive syllogism 1"
      [ orC [andC [EqC x0Ty (ptrTy 64 int64Ty), SubC x1Ty int64Ty],
             andC [EqC x1Ty (ptrTy 64 int64Ty), SubC x0Ty int64Ty]],
        SubC x0Ty int64Ty]
      [(x0, int64Ty), (x1, ptrTy 64 int64Ty)],
    mkTyEnvTest "disjunctive syllogism 2"
      [ orC [andC [EqC x0Ty (ptrTy 64 int64Ty), SubC x1Ty int64Ty],
             andC [EqC x1Ty (ptrTy 64 int64Ty), SubC x0Ty int64Ty],
             andC [SubC x0Ty int64Ty, SubC x0Ty int64Ty]],
        SubC int32Ty x0Ty,
        SubC int32Ty x1Ty]
      [(x0, int32Ty), (x1, int32Ty)],
    mkTyEnvTest "disjunctive syllogism 3"
      [ orC [andC [EqC x0Ty (ptrTy 64 int64Ty), SubC x1Ty int64Ty],
             andC [EqC x1Ty (ptrTy 64 int64Ty), SubC x0Ty int64Ty],
             andC [SubC x0Ty int64Ty, SubC x0Ty int64Ty]],
        SubC x2Ty x0Ty,
        SubC x3Ty x1Ty,
        SubC int32Ty x2Ty,
        SubC int32Ty x3Ty]
      [(x0, int32Ty), (x1, int32Ty), (x2, int32Ty), (x3, int32Ty)]
  ]

-- | Non-basic (?) tests.
complexCTests :: T.TestTree
complexCTests = T.testGroup "Complex Constraint Tests"
  [ mkTyEnvTest "variables related by equalities 1"
      [ EqC x0Ty (ptrTy 64 x1Ty),
        EqC x1Ty x2Ty,
        EqC x2Ty x3Ty,
        EqC x3Ty int64Ty,
        orC [andC [EqC x0Ty (ptrTy 64 int32Ty), EqC x4Ty int64Ty],
             andC [EqC x0Ty (ptrTy 64 int64Ty), EqC x4Ty int32Ty]]
      ]
      [(x0, ptrTy 64 int64Ty),
       (x1, int64Ty),
       (x2, int64Ty),
       (x3, int64Ty),
       (x4, int32Ty)]
  ]






