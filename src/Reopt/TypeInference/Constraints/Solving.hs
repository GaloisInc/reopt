module Reopt.TypeInference.Constraints.Solving
  ( Ty (..), numTy, ptrTy, varTy, -- structTy,
    ConstraintSolvingMonad, runConstraintSolvingMonad,
    eqTC,
  ) where

import           Data.Map.Strict                                       (Map)
import qualified Data.Map.Strict                                       as Map

import Reopt.TypeInference.Constraints.Solving.RowVariables
  ( Offset (Offset),
    RowVar (RowVar),
    Offset (..), RowExpr
  )
import Reopt.TypeInference.Constraints.Solving.Solver
  ( unifyConstraints,
  )
import Reopt.TypeInference.Constraints.Solving.TypeVariables
  ( TyVar (..),
  )
import Reopt.TypeInference.Constraints.Solving.Types
  ( FTy,
    ITy(..),
    TyF(..),
  )
import Reopt.TypeInference.Constraints.Solving.Monad (ConstraintSolvingMonad, runConstraintSolvingMonad, freshTyVar, addTyVarEq)

-- This type is easier to work with, as it isn't normalised.
data Ty =
  Var TyVar
  | Ty  (TyF RowExpr Ty)

-- Smart constructors for Ty

numTy :: Int -> Ty
numTy = Ty . NumTy

ptrTy :: Ty -> Ty
ptrTy = Ty . PtrTy

varTy :: TyVar -> Ty
varTy = Var

-- structTy :: Map Offset Ty -> Ty
-- structTy = Ty . RecTy 

--------------------------------------------------------------------------------
-- Compilers from Ty into ITy

compileTy :: Ty -> ConstraintSolvingMonad ITy
compileTy (Var tv) = pure (VarTy tv)
compileTy (Ty ty)  = ITy <$> traverse nameTy ty
  where
    nameTy ty' = freshTyVar Nothing . Just =<< compileTy ty'

--------------------------------------------------------------------------------
-- Constraint constructors

eqTC :: Ty -> Ty -> ConstraintSolvingMonad ()
eqTC ty1 ty2 = do
  let mk = freshTyVar Nothing . Just
  tv1 <- mk =<< compileTy ty1
  tv2 <- mk =<< compileTy ty2
  addTyVarEq tv1 tv2

