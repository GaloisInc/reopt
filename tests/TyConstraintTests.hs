{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TyConstraintTests
  ( constraintTests,
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
   recTy',
   readAtTy,
   updateAtTy,
   int32Ty,
   int64Ty,
   num32Ty,
   num64Ty)


x0,x1,x2,x3  :: TyVar
x0Ty,x1Ty,x2Ty,x3Ty :: Ty
[(x0,x0Ty),
 (x1,x1Ty),
 (x2,x2Ty),
 (x3,x3Ty)] = map (\i -> (TyVar i, VarTy $ TyVar i))
                  [0..3]

constraintTests :: T.TestTree
constraintTests = T.testGroup "Type Constraint Tests"
  [ eqCTests
  , subCTests
  , orCTests
  , recTests
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
  [ mkTest "disjunctive syllogism 0"
      [ orC [andC [EqC x0Ty int64Ty, EqC x1Ty int32Ty],
             andC [SubC x0Ty (ptrTy 64 int64Ty), EqC x1Ty int64Ty]],
        SubC x0Ty (ptrTy 64 TopTy)]
      [(x0, (ptrTy 64 int64Ty)), (x1, int64Ty)],
    mkTest "disjunctive syllogism 1"
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
      [(x0, int32Ty), (x1, int32Ty), (x2, int32Ty), (x3, int32Ty)],
    mkTest "disjunctive syllogism 4"
      [ SubC x1Ty (ptrTy 64 TopTy),
        orC [andC [EqC x0Ty num32Ty, EqC x1Ty num64Ty],
             andC [EqC x0Ty num64Ty, SubC x1Ty (ptrTy 64 TopTy)]]
      ]
      [(x0, num64Ty),
       (x1, (ptrTy 64 TopTy))],
    mkTest "disjunctive syllogism 5"
      [ SubC x1Ty (ptrTy 64 TopTy),
        orC [andC [EqC x0Ty num64Ty, EqC x1Ty num64Ty],
             andC [SubC x0Ty (ptrTy 64 TopTy), SubC x1Ty (ptrTy 64 TopTy)]]
      ]
      [(x0, (ptrTy 64 TopTy)),
       (x1, (ptrTy 64 TopTy))],
    mkTest "disjunctive syllogism 6"
      [ EqC  x1Ty num64Ty,
        orC [andC [EqC x0Ty num32Ty, EqC x1Ty num64Ty],
             andC [EqC x0Ty num64Ty, SubC x1Ty (ptrTy 64 TopTy)]]
      ]
      [(x0, num32Ty),
       (x1, num64Ty)],
    mkTest "disjunctive syllogism 7"
      [ EqC  x1Ty num64Ty,
        SubC x2Ty (ptrTy 64 TopTy),
        orC [andC [EqC x0Ty num64Ty, EqC x1Ty num64Ty, EqC x2Ty num64Ty],
             andC [EqC x0Ty num64Ty, SubC x1Ty (ptrTy 64 TopTy), SubC x2Ty (ptrTy 64 TopTy)],
             andC [SubC x0Ty (ptrTy 64 TopTy), EqC x1Ty num64Ty,  SubC x2Ty (ptrTy 64 TopTy)]]
      ]
      [(x0, (ptrTy 64 TopTy)),
       (x1, num64Ty),
       (x2, (ptrTy 64 TopTy))]
  ]

recTests :: T.TestTree
recTests = T.testGroup "Record Type Tests"
  [ mkTest "Record type var in field 1"
      [ EqC x0Ty (recTy' [(0, x1Ty), (64, x2Ty)]),
        EqC x1Ty int32Ty,
        EqC x2Ty int64Ty]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, int64Ty)],
    mkTest "Record type var in field 2"
      [ EqC x1Ty int32Ty,
        EqC x0Ty (recTy' [(0, x1Ty), (64, x2Ty)]),
        EqC x2Ty int64Ty]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, int64Ty)],
    mkTest "Record type var in field 3"
      [ EqC x0Ty (recTy' [(0, x1Ty), (64, x2Ty)]),
        SubC x1Ty int32Ty,
        SubC x2Ty int64Ty]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, int64Ty)],
    mkTest "Record type var in field 4"
      [ SubC x1Ty int32Ty,
        EqC x0Ty (recTy' [(0, x1Ty), (64, x2Ty)]),
        SubC x2Ty int64Ty]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, int64Ty)],
    mkTest "Record type var in field 5"
      [ EqC x0Ty (recTy' [(0, x1Ty), (64, x2Ty)]),
        EqC x0Ty x3Ty,
        EqC x3Ty (recTy' [(0, int32Ty), (64, int64Ty)])]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, int64Ty),
       (x3, recTy' [(0, int32Ty), (64, int64Ty)])],
    mkTest "Record type with field ref 1"
      [ EqC x0Ty (recTy' [(0, int32Ty), (64, int64Ty)]),
        EqC x1Ty (readAtTy x0Ty 0),
        EqC x2Ty (readAtTy x0Ty 64)]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, int64Ty)],
    mkTest "Record type with field ref 2"
      [ EqC x1Ty (readAtTy x0Ty 0),
        EqC x0Ty (recTy' [(0, int32Ty), (64, int64Ty)]),
        EqC x2Ty (readAtTy x0Ty 64)]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, int64Ty)],
    mkTest "Record type with update 1"
      [ EqC x0Ty (recTy' [(0, int32Ty), (64, int64Ty)]),
        EqC x1Ty (readAtTy x0Ty 0),
        EqC x2Ty (updateAtTy x0Ty 64 (ptrTy 64 x1Ty)),
        EqC x3Ty (readAtTy x2Ty 64)]
      [(x0, recTy' [(0, int32Ty), (64, int64Ty)]),
       (x1, int32Ty),
       (x2, recTy' [(0, int32Ty), (64, (ptrTy 64 int32Ty))]),
       (x3, (ptrTy 64 int32Ty))]
  ]

newtype TypeEnv = TypeEnv [(TyVar, Ty)]
  deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv xs) = show $ bimap PP.pretty PP.pretty <$> xs

tyEnv :: [(TyVar, Ty)] -> TypeEnv
tyEnv = TypeEnv . sortBy (compare `on` fst)

mkTest :: String -> [TyConstraint] -> [(TyVar, Ty)] -> T.TestTree
mkTest name cs expected =
  let actual = Map.toList $ unifyConstraints cs
    in T.testCase name $ (tyEnv actual) T.@?= (tyEnv expected)




