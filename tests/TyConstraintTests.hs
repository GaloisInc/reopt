{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module TyConstraintTests
  ( constraintTests
  -- utility
  , ghciTest
  )
where

import           Data.Bifunctor   (bimap)
import           Data.Foldable    (traverse_)
import qualified Data.Map         as Map
import qualified Prettyprinter    as PP
import qualified Test.Tasty       as T
import qualified Test.Tasty.HUnit as T

import Reopt.TypeInference.Solver
  (TyVar,
   --   RowVar(..),
   Ty(..),
   Ty, FTy,
   unifyConstraints,
   eqTC, SolverM, runSolverM, freshTyVar, numTy, varTy, ConstraintSolution (..)
  , pattern FPtrTy, pattern FNumTy, ptrTy, pattern FUnknownTy
  , pattern FStructTy, pattern FNamedStruct
  , pattern FConflictTy
  , ptrAddTC, ptrSubTC, OperandClass (OCOffset), ptrTC, ptrTy')

import Reopt.TypeInference.Solver.Constraints (ConstraintProvenance (..))
import Reopt.TypeInference.Solver.RowVariables (Offset, singletonFieldMap, FieldMap, fieldMapFromList)
import Reopt.TypeInference.Solver.Monad (withFresh)

constraintTests :: T.TestTree
constraintTests = T.testGroup "Type Constraint Tests"
  [ eqCTests
  , ptrCTests
  , recursiveTests
  , conflictTests
  ]

num8, num32, num64 :: Ty
num8 = numTy 8
num32 = numTy 32
num64 = numTy 64

fnum8, fnum32, fnum64 :: FTy
fnum8 = FNumTy 8
fnum32 = FNumTy 32
fnum64 = FNumTy 64

tv :: SolverM TyVar
tv = freshTyVar Nothing Nothing

tvEq :: TyVar -> TyVar -> SolverM ()
tvEq v v' = eqTC prov (varTy v) (varTy v')


tvEqs :: [(TyVar, TyVar)] -> SolverM ()
tvEqs = traverse_ (uncurry tvEq)

recTy :: [(Offset, Ty)] -> FieldMap Ty
recTy = fieldMapFromList

fptrTy' :: FTy -> FTy
fptrTy' = FPtrTy . FStructTy . singletonFieldMap 0

frecTy :: [(Offset, FTy)] -> FTy
frecTy = FStructTy . fieldMapFromList

-- Simple tests having to do with equality constraints
eqCTests :: T.TestTree
eqCTests = T.testGroup "Equality Constraint Tests"
  [ mkTest "Single eqTC var left"  (do { x0 <- tv; eqTC prov (varTy x0) num64; pure [(x0, fnum64)] })
  , mkTest "Single eqTC var right" (do { x0 <- tv; eqTC prov num64 (varTy x0); pure [(x0, fnum64)] })
  , mkTest "Multiple eqTCs" $ do
      x0 <- tv
      x1 <- tv
      eqTC prov (varTy x0) num64
      eqTC prov (varTy x1) (ptrTy' num64)
      pure [(x0, fnum64), (x1, fptrTy' fnum64)]

  , mkTest "eqTC simple transitivity 1" $ do
      x0 <- tv
      x1 <- tv
      tvEq x0 x1
      eqTC prov (varTy x1) num64
      pure [(x0, fnum64), (x1, fnum64)]

  ,  mkTest "eqTC simple Transitivity 2" $ do
      x0 <- tv
      x1 <- tv
      eqTC prov (varTy x1) num64
      tvEq x0 x1
      pure [(x0, fnum64), (x1, fnum64)]

  , mkTest "eqTC simple transitivity 3" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      tvEqs [(x1, x2), (x0, x1)]
      eqTC prov (varTy x2) num64
      pure [(x0, fnum64), (x1, fnum64), (x2, fnum64)]

  , mkTest "eqTC with pointers 1" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      tvEqs [(x1, x2), (x0, x1) ]
      eqTC prov (varTy x2) num64
      eqTC prov (varTy x3) (ptrTy' num64)
      pure [(x0, fnum64), (x1, fnum64), (x2, fnum64), (x3, fptrTy' fnum64)]

  , mkTest "eqTC with pointers 2" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv; x4 <- tv
      tvEqs [(x0, x2)]
      eqTC prov (varTy x1) (ptrTy' (varTy x2))
      eqTC prov (varTy x2) num64
      eqTC prov (varTy x3) num64
      eqTC prov (varTy x4) (ptrTy' (varTy x3))

      pure [(x0, fnum64), (x1, fptrTy' fnum64), (x2, fnum64), (x3, fnum64), (x4, fptrTy' fnum64)]

  , mkTest "eqTC with pointers 3" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv; x4 <- tv
      tvEqs [(x0, x2)]
      eqTC prov (varTy x1) (ptrTy' (varTy x2))
      eqTC prov (varTy x3) num64
      eqTC prov (varTy x4) (ptrTy' (varTy x3))

      pure [(x0, FUnknownTy), (x1, fptrTy' FUnknownTy), (x2, FUnknownTy), (x3, fnum64), (x4, fptrTy' fnum64)]

  , mkTest "eqTC with records 1" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      tvEqs [ (x1, x2), (x0, x1) ]
      eqTC prov (varTy x2) num64
      eqTC prov (varTy x3) (ptrTy' (varTy x0))
      pure [(x0, fnum64), (x1, fnum64), (x2, fnum64), (x3, FPtrTy $ frecTy [(0, fnum64)])]

  , mkTest "eqTC with records 2" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      tvEqs [ (x1, x2) ]
      eqTC prov (varTy x0) (ptrTy' (varTy x1))
      eqTC prov (varTy x2) num64
      eqTC prov (varTy x3) (ptrTy $ recTy [(0, varTy x0), (8, varTy x1)])
      pure [ (x0, fptrTy' fnum64), (x1, fnum64), (x2, fnum64)
           , (x3, FPtrTy $ frecTy [(0, fptrTy' fnum64), (8, fnum64)])
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
      eqTC prov x0Ty (ptrTy $ recTy [(0, num64)])
      eqTC prov x0Ty (ptrTy $ recTy [(8, ptrTy' num64)])
      pure [(x0, FPtrTy $ frecTy [(0, fnum64), (8, fptrTy' fnum64)])]

  -- , mkTest "eqTC with records+rows 2" $ do
  --     x0 <- tv; x1 <- tv; x2 <- tv
  --     let x0Ty = varTy x0
  --         x1Ty = varTy x1
  --         x2Ty = varTy x2

  --     r0 <- freshRowVar
  --     r1 <- freshRowVar

  --     eqTC x0Ty (recTy [(0, num64)] r0)
  --     eqTC x0Ty (recTy [(8, ptrTy num64)] r1)
  --     eqTC x1Ty (recTy mempty r0)
  --     eqTC x2Ty (recTy mempty r1)
  --     pure [ (x0, frecTy [(0, fnum64), (8, FPtrTy fnum64)])
  --          , (x1, frecTy [(8, FPtrTy fnum64)])
  --          , (x2, frecTy [(0, fnum64)])
  --          ]

  -- , mkTest "eqTC with records+rows 3" $ do
  --     x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
  --     let x0Ty = varTy x0
  --         x1Ty = varTy x1
  --         x2Ty = varTy x2
  --         x3Ty = varTy x3

  --     r0 <- freshRowVar
  --     r1 <- freshRowVar

  --     eqTC x0Ty (recTy [(0, num64), (8, ptrTy x1Ty)] r0)
  --     eqTC x0Ty (recTy [(8, ptrTy num64), (16, num64)] r1)
  --     eqTC x2Ty (recTy mempty r0)
  --     eqTC x3Ty (recTy mempty r1)
  --     pure [ (x0, frecTy [(0, fnum64), (8, FPtrTy fnum64), (16, fnum64)])
  --           , (x1, fnum64)
  --           , (x2, frecTy [(16, fnum64)])
  --           , (x3, frecTy [(0, fnum64)])]
  ]

ptrCTests :: T.TestTree
ptrCTests = T.testGroup "Pointer Arith Constraint Tests"
  [ mkTest "Constrained by arg" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2

      eqTC prov x1Ty (ptrTy $ recTy [(0, num64), (8, ptrTy' num64)])
      ptrAddTC prov x0Ty x1Ty x2Ty (OCOffset 8)
      pure [(x0, fptrTy' (fptrTy' fnum64))]

  , mkTest "Constrained by result 1" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2

      eqTC prov x0Ty (ptrTy $ recTy [(0, num64), (8, ptrTy' num64)])

      ptrAddTC prov x0Ty x1Ty x2Ty (OCOffset 8)
      pure [(x1, FPtrTy $ frecTy [(8, fnum64), (16, fptrTy' fnum64)])]
  , mkTest "Constrained by result 2" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2

      eqTC prov x0Ty (ptrTy $ recTy [(0, num64), (8, ptrTy' num64)])
      eqTC prov x1Ty (ptrTy $ recTy [(0, ptrTy' num64)])

      ptrAddTC prov x0Ty x1Ty x2Ty (OCOffset 8)
      pure [(x1, FPtrTy $ frecTy [(0, fptrTy' fnum64), (8, fnum64), (16, fptrTy' fnum64)])]

  , mkTest "Accessing pointer members from a struct pointer" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2
          x3Ty = varTy x3

      eqTC prov x0Ty (ptrTy (recTy [(0, num8)]))
      eqTC prov x1Ty (ptrTy (recTy [(0, num64)]))
      eqTC prov x2Ty (ptrTy (recTy [(0, num32)]))
      eqTC prov x3Ty (ptrTy (recTy []))
      ptrAddTC prov x0Ty x3Ty num64 (OCOffset 0)
      ptrAddTC prov x1Ty x3Ty num64 (OCOffset 8)
      ptrAddTC prov x2Ty x3Ty num64 (OCOffset 72)
      pure [ (x0, FPtrTy (frecTy [(0, fnum8), (8, fnum64), (72, fnum32)]))
           , (x1, FPtrTy (frecTy [(0, fnum64), (64, fnum32)]))
           , (x2, FPtrTy (frecTy [(0, fnum32)]))
           , (x3, FPtrTy (frecTy [(0, fnum8), (8, fnum64), (72, fnum32)]))]


  , mkTest "Simple cycle test (liveness)" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2

      -- x0 = ptr (recTy {0 -> x1} (freshRow))
      ptrTC prov x1Ty x0Ty

      -- x1 is a byte
      eqTC prov x1Ty (numTy 8)

      eqTC prov x2Ty num64
      ptrAddTC prov x0Ty x0Ty x2Ty (OCOffset 1)
      pure [] -- Liveness

  , mkTest "Nested Cycle test (liveness)" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2
          x3Ty = varTy x3

      -- x0 = ptr (recTy {0 -> x1} (freshRow))
      ptrTC prov x1Ty x0Ty
      ptrTC prov x1Ty x3Ty

      -- x1 is a byte
      eqTC prov x1Ty (numTy 8)

      eqTC prov x2Ty num64
      ptrAddTC prov x3Ty x0Ty x2Ty (OCOffset 1)
      ptrAddTC prov x0Ty x3Ty x2Ty (OCOffset 1)
      -- If we get here, then we have succeeded, although we may want
      -- to return a value once array stride detection produces a
      -- reasonable type.
      pure []

  , mkTest "Nested cycle test 2 (liveness)" $ do
      x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2
          x3Ty = varTy x3

      -- x0 = ptr (recTy {0 -> x1} (freshRow))
      ptrTC prov x1Ty x0Ty
      ptrTC prov x1Ty x3Ty

      -- x1 is a byte
      eqTC prov x1Ty (numTy 8)

      eqTC prov x2Ty num64
      -- Inverted constraint order from above
      ptrAddTC prov x0Ty x3Ty x2Ty (OCOffset 1)
      ptrAddTC prov x3Ty x0Ty x2Ty (OCOffset 1)
      -- If we get here, then we have succeeded, although we may want
      -- to return a value once array stride detection produces a
      -- reasonable type.
      pure []


  , mkTest "Pointer offset subtraction" $ do
      x0 <- tv; x1 <- tv; x2 <- tv
      let x0Ty = varTy x0
          x1Ty = varTy x1
          x2Ty = varTy x2

      eqTC prov x1Ty (ptrTy $ recTy [(0, num64), (8, ptrTy' num64)])
      ptrSubTC prov x0Ty x1Ty x2Ty (OCOffset 8)
      pure [(x0, FPtrTy $ frecTy [(8, fnum64), (16, fptrTy' fnum64)])]
  ]

-- t4 = do
--       x0 <- tv; x1 <- tv; x2 <- tv; x3 <- tv
--       let x0Ty = varTy x0
--           x1Ty = varTy x1
--           x2Ty = varTy x2
--           x3Ty = varTy x3

--       r0 <- freshRowVar
--       eqTC prov x0Ty (recTy [(0, ptrTy x0Ty)] r0)


recursiveTests :: T.TestTree
recursiveTests = T.testGroup "Recursive Type Tests"
  [ mkTest "Recursive linked list" $ do
      x0 <- tv
      let x0Ty = varTy x0
      eqTC prov x0Ty (ptrTy $ recTy [(0, ptrTy' x0Ty)])
      pure [(x0, FPtrTy $ FNamedStruct "struct.reopt.t1")]
  ]

conflictTests :: T.TestTree
conflictTests = T.testGroup "Conflict Type Tests"
  [ mkTest "Simple conflict" $ withFresh $ \x0 -> do
      let x0Ty = varTy x0
      eqTC prov x0Ty num64
      eqTC prov x0Ty (ptrTy' num64)
      -- eqTC x0Ty (ptrTy' num64)
      -- eqTC x0Ty (ptrTy' (ptrTy' num64))

      pure [(x0, FConflictTy 64)]
  ]

ghciTest :: Bool -> SolverM a -> PP.Doc d
ghciTest doTrace t = PP.pretty . runSolverM doTrace False 64 $ do
  t >> unifyConstraints

newtype TypeEnv = TypeEnv [(TyVar, FTy)]
  deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv xs) = show $ bimap PP.pretty PP.pretty <$> xs

-- tyEnv :: [(TyVar, FTy)] -> TypeEnv
-- tyEnv = TypeEnv . sortBy (compare `on` fst)

mkTest :: String -> SolverM [(TyVar, FTy)] -> T.TestTree
mkTest name m = T.testCase name (runSolverM False False 64 test)
  where
    test = do
      expected <- m
      res      <- unifyConstraints
      let actual = [ (k, Map.findWithDefault FUnknownTy k (csTyVars res))
                   | (k, _) <- expected ]
      pure (TypeEnv actual T.@?= TypeEnv expected)

prov :: ConstraintProvenance
prov = TestingProv
