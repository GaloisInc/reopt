module Reopt.TypeInference.Constraints.Solving
  ( Ty (..), FTy, TyVar, numTy, ptrTy, varTy, -- structTy,
    ConstraintSolvingMonad, runConstraintSolvingMonad,
    eqTC, ptrTC, freshTyVar,
    unifyConstraints,
    tyToLLVMType,
  ) where

import           Data.Map.Strict                                       (Map)
import qualified Data.Map.Strict                                       as Map

import Reopt.TypeInference.Constraints.Solving.RowVariables
  ( RowExpr (RowExprVar), RowVar, Offset
  )
import Reopt.TypeInference.Constraints.Solving.Solver
  ( unifyConstraints,
  )
import Reopt.TypeInference.Constraints.Solving.TypeVariables
  ( TyVar (..),
  )
import Reopt.TypeInference.Constraints.Solving.Types
  ( ITy(..),
    TyF(..),
    FTy, tyToLLVMType
  )
import Reopt.TypeInference.Constraints.Solving.Monad (ConstraintSolvingMonad, runConstraintSolvingMonad, freshTyVar, addTyVarEq, freshRowVar)

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

structTy :: Map Offset Ty -> RowVar -> Ty
structTy os r = Ty $ RecTy os (RowExprVar r)

--------------------------------------------------------------------------------
-- Compilers from Ty into ITy

nameTy :: Ty -> ConstraintSolvingMonad TyVar
nameTy ty = freshTyVar Nothing . Just =<< compileTy ty

compileTy :: Ty -> ConstraintSolvingMonad ITy
compileTy (Var tv) = pure (VarTy tv)
compileTy (Ty ty)  = ITy <$> traverse nameTy ty

--------------------------------------------------------------------------------
-- Constraint constructors

eqTC :: Ty -> Ty -> ConstraintSolvingMonad ()
eqTC ty1 ty2 = do
  tv1 <- nameTy ty1
  tv2 <- nameTy ty2
  addTyVarEq tv1 tv2

ptrTC :: Ty -> Ty -> ConstraintSolvingMonad ()
ptrTC target ptr = do
  rv <- freshRowVar
  -- emits ptr = { 0 -> target | rv }
  let pTy = ptrTy (structTy (Map.singleton 0 target) rv) 
  eqTC ptr pTy
  
  
  
