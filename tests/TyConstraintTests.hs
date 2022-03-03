{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

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

import Reopt.TypeInference.Constraints.Solving
  (TyVar,
   --   RowVar(..),
   Ty(..),
   Ty, FTy,
   unifyConstraints,
   eqTC, ConstraintSolvingMonad, runConstraintSolvingMonad, freshTyVar, numTy, varTy
  , pattern FPtrTy, pattern FNumTy, ptrTy, pattern FUnknownTy, pattern FRecTy, structTy)

import Data.Foldable (traverse_)
import Reopt.TypeInference.Constraints.Solving.Monad (freshRowVar)
import Reopt.TypeInference.Constraints.Solving.RowVariables (Offset, RowVar)

-- x0,x1,x2,x3,x4,_x5  :: TyVar
-- x0Ty,x1Ty,x2Ty,x3Ty,x4Ty,_x5Ty  :: ITy
-- [(x0,x0Ty),
--  (x1,x1Ty),
--  (x2,x2Ty),
--  (x3,x3Ty),
--  (x4,x4Ty),
--  (_x5,_x5Ty)] = map mkTyVar [0..5]
--   where
--     mkTyVar i =
--       let tv = TyVar i Nothing in
--       (tv, UnknownTy tv)

-- usedRowVars :: [Int]
-- usedRowVars = [0..3]

-- r0,r1,_r2,_r3  :: RowVar
-- [r0,r1,_r2,_r3] = map RowVar usedRowVars

-- firstFreshRowVar :: Int
-- firstFreshRowVar = maximum usedRowVars + 1


constraintTests :: T.TestTree
constraintTests = T.testGroup "Type Constraint Tests"
  [ eqCTests
  ]

num64 :: Ty
num64 = numTy 64

fnum64 :: FTy
fnum64 = FNumTy 64

tv :: ConstraintSolvingMonad TyVar
tv = freshTyVar Nothing Nothing

tvEq :: TyVar -> TyVar -> ConstraintSolvingMonad ()
tvEq v v' = eqTC (varTy v) (varTy v')


tvEqs :: [(TyVar, TyVar)] -> ConstraintSolvingMonad ()
tvEqs = traverse_ (uncurry tvEq)

recTy :: [(Offset, Ty)] -> RowVar -> Ty
recTy os rv = structTy (Map.fromList os) rv

frecTy :: [(Offset, FTy)] -> FTy
frecTy os = FRecTy (Map.fromList os) 

-- Simple tests having to do with equality constraints
eqCTests :: T.TestTree
eqCTests = T.testGroup "Equality Constraint Tests"
  [ mkTest "Single eqTC var left"  (do { x0 <- tv; eqTC (varTy x0) num64; pure [(x0, fnum64)] })
  , mkTest "Single eqTC var right" (do { x0 <- tv; eqTC num64 (varTy x0); pure [(x0, fnum64)] })
  , mkTest "Multiple eqTCs" $ do
      x0 <- tv
      x1 <- tv
      eqTC (varTy x0) num64
      eqTC (varTy x1) (ptrTy num64)
      pure [(x0, fnum64), (x1, FPtrTy fnum64)]
  
  , mkTest "eqTC simple transitivity 1" $ do
      x0 <- tv
      x1 <- tv
      tvEq x0 x1
      eqTC (varTy x1) num64
      pure [(x0, fnum64), (x1, fnum64)]
      
  ,  mkTest "eqTC simple Transitivity 2" $ do
      x0 <- tv
      x1 <- tv
      eqTC (varTy x1) num64
      tvEq x0 x1
      pure [(x0, fnum64), (x1, fnum64)]

  , mkTest "eqTC simple transitivity 3" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      tvEqs [(x1, x2), (x0, x1)]
      eqTC (varTy x2) num64
      pure [(x0, fnum64), (x1, fnum64), (x2, fnum64)]
      
  , mkTest "eqTC with pointers 1" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      tvEqs [(x1, x2), (x0, x1) ]
      eqTC (varTy x2) num64
      eqTC (varTy x3) (ptrTy num64)
      pure [(x0, fnum64), (x1, fnum64), (x2, fnum64), (x3, FPtrTy fnum64)]
      
  , mkTest "eqTC with pointers 2" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv; x4 <- tv
      tvEqs [(x0, x2)]
      eqTC (varTy x1) (ptrTy (varTy x2))
      eqTC (varTy x2) num64
      eqTC (varTy x3) num64
      eqTC (varTy x4) (ptrTy (varTy x3))
      
      pure [(x0, fnum64), (x1, FPtrTy fnum64), (x2, fnum64), (x3, fnum64), (x4, FPtrTy fnum64)]

  , mkTest "eqTC with pointers 3" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv; x4 <- tv
      tvEqs [(x0, x2)]
      eqTC (varTy x1) (ptrTy (varTy x2))
      eqTC (varTy x3) num64
      eqTC (varTy x4) (ptrTy (varTy x3))

      pure [(x0, FUnknownTy), (x1, FPtrTy FUnknownTy), (x2, FUnknownTy), (x3, fnum64), (x4, FPtrTy fnum64)]

  , mkTest "eqTC with records 1" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      tvEqs [ (x1, x2), (x0, x1) ]
      eqTC (varTy x2) num64
      r0 <- freshRowVar
      eqTC (varTy x3) (structTy (Map.singleton 0 (varTy x0)) r0)
      pure [(x0, fnum64), (x1, fnum64), (x2, fnum64), (x3, frecTy [(0, fnum64)])]
  
  , mkTest "eqTC with records 2" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      tvEqs [ (x1, x2) ]
      eqTC (varTy x0) (ptrTy (varTy x1))
      eqTC (varTy x2) num64
      r0 <- freshRowVar
      eqTC (varTy x3) (recTy [(0, varTy x0), (8, varTy x1)] r0)
      
      pure [ (x0, FPtrTy fnum64), (x1, fnum64), (x2, fnum64)
           , (x3, frecTy [(0, FPtrTy fnum64), (8, fnum64)])
           ]
        
       -- These next tests check that record constraints are unified properly
       -- during constraint solving when there are possible unknown other fields
       -- (i.e., the row variables). This arises when we want to combine
       -- different atomic facts describing offsets from a single memory
       -- location. E.g., if we separately learn (1) at `p` there is a `num` and
       -- (2) at `p+8` there is an `ptr(num)`, these statements about `p` can be
       -- described via the following two atomic constraints: `p = {0 : num|ρ}`
       -- and `p = {8 : ptr(num)|ρ'}`. Our unification should then combine these
       -- constraints on `p` into `p = {0 : num, 8 : ptr(num)}`.
        
  , mkTest "eqTC with records+rows 1" $ do
      x0 <- tv
      let x0Ty = varTy x0
      r0 <- freshRowVar
      r1 <- freshRowVar
      eqTC x0Ty (recTy [(0, num64)] r0)     
      eqTC x0Ty (recTy [(8, ptrTy num64)] r1)
      pure [(x0, frecTy [(0, fnum64), (8, FPtrTy fnum64)])]
      
  , mkTest "eqTC with records+rows 2" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2
          
      r0 <- freshRowVar      
      r1 <- freshRowVar
    
      eqTC x0Ty (recTy [(0, num64)] r0)
      eqTC x0Ty (recTy [(8, ptrTy num64)] r1)
      eqTC x1Ty (recTy mempty r0)
      eqTC x2Ty (recTy mempty r1)
      pure [ (x0, frecTy [(0, fnum64), (8, FPtrTy fnum64)])
           , (x1, frecTy [(8, FPtrTy fnum64)])
           , (x2, frecTy [(0, fnum64)])
           ]
  , mkTest "eqTC with records+rows 3" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2
          x3Ty = varTy x3
          
      r0 <- freshRowVar      
      r1 <- freshRowVar
      
      eqTC x0Ty (recTy [(0, num64), (8, ptrTy x1Ty)] r0)
      eqTC x0Ty (recTy [(8, ptrTy num64), (16, num64)] r1)
      eqTC x2Ty (recTy mempty r0)
      eqTC x3Ty (recTy mempty r1)
      pure [ (x0, frecTy [(0, fnum64), (8, FPtrTy fnum64), (16, fnum64)])
            , (x1, fnum64)
            , (x2, frecTy [(16, fnum64)])
            , (x3, frecTy [(0, fnum64)])]

  ]


newtype TypeEnv = TypeEnv [(TyVar, FTy)]
  deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv xs) = show $ bimap PP.pretty PP.pretty <$> xs

tyEnv :: [(TyVar, FTy)] -> TypeEnv
tyEnv = TypeEnv . sortBy (compare `on` fst)

mkTest :: String -> ConstraintSolvingMonad [(TyVar, FTy)] -> T.TestTree
mkTest name m = T.testCase name (runConstraintSolvingMonad test)
  where
    test = do
      expected <- m
      res      <- unifyConstraints
      let actual = [ (k, Map.findWithDefault FUnknownTy k res)
                   | (k, _) <- expected ]
      pure (actual T.@?= expected)
