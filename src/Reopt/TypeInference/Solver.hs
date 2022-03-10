{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Reopt.TypeInference.Solver
  ( Ty (..), TyVar, numTy, ptrTy, varTy, structTy, resolveTyVar,
    SolverM, runSolverM,
    eqTC, ptrTC, freshTyVar, ptrAddTC,
    OperandClass (..),
    unifyConstraints, ConstraintSolution(..), StructName,
    tyToLLVMType,
    -- FTy stuff
    FTy, pattern FNumTy, pattern FPtrTy, pattern FRecTy, pattern FUnknownTy, pattern FNamedStruct, 
    -- Testing
  ) where

import           Data.Map.Strict                                       (Map)
import qualified Data.Map.Strict                                       as Map

import Reopt.TypeInference.Solver.RowVariables
  ( RowExpr (RowExprVar), RowVar(..), Offset, NoRow (NoRow)
  )
import Reopt.TypeInference.Solver.Solver
  ( unifyConstraints, ConstraintSolution(..)
  )
import Reopt.TypeInference.Solver.TypeVariables
  ( TyVar (..),
  )
import Reopt.TypeInference.Solver.Types
  ( ITy(..),
    TyF(..),
    FTy(..), tyToLLVMType, StructName, resolveTyVar
  )
import Reopt.TypeInference.Solver.Monad (SolverM, runSolverM, freshTyVar, addTyVarEq, freshRowVar, ptrWidthNumTy, addPtrAdd)
import Reopt.TypeInference.Solver.Constraints (OperandClass(..))
import Control.Lens (use)
import Data.Generics.Product (field)

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

tySize :: Int -> Ty -> Maybe Int
tySize _ (Var tv) = tyVarSize tv
tySize ptrSz (Ty ty) =
  case ty of
    NumTy n  -> Just n
    PtrTy {} -> Just ptrSz
    RecTy {} -> Nothing

nameTy :: Ty -> SolverM TyVar
nameTy ty = do
  psz <- use (field @"ptrWidth")
  let tysz = tySize psz ty
  freshTyVar Nothing tysz . Just =<< compileTy ty

compileTy :: Ty -> SolverM ITy
compileTy (Var tv) = pure (VarTy tv)
compileTy (Ty ty)  = ITy <$> traverse nameTy ty

--------------------------------------------------------------------------------
-- Constraint constructors

eqTC :: Ty -> Ty -> SolverM ()
eqTC ty1 ty2 = do
  tv1 <- nameTy ty1
  ity2 <- compileTy ty2
  addTyVarEq tv1 ity2

ptrTC :: Ty -> Ty -> SolverM ()
ptrTC target ptr = do
  rv <- freshRowVar
  -- emits ptr = { 0 -> target | rv }
  let pTy = ptrTy (structTy (Map.singleton 0 target) rv) 
  eqTC ptr pTy

ptrAddTC :: Ty -> Ty -> Ty -> OperandClass -> SolverM ()
ptrAddTC rty lhsty rhsty oc = do
  rv <- nameTy rty
  lhstv <- nameTy lhsty
  rhstv <- nameTy rhsty

  -- Constrain rhsty if it is an offset
  case oc of
    OCOffset _ -> addTyVarEq rhstv . ITy =<< ptrWidthNumTy
    _ -> pure ()

  addPtrAdd rv lhstv rhstv oc
  
--------------------------------------------------------------------------------
-- LLVM support (FTy patterns)

pattern FNumTy :: Int -> FTy
pattern FNumTy sz = FTy (NumTy sz)

pattern FRecTy :: Map Offset FTy -> FTy
pattern FRecTy ty = FTy (RecTy ty NoRow)

pattern FPtrTy :: FTy -> FTy
pattern FPtrTy ty = FTy (PtrTy ty)

pattern FUnknownTy :: Int -> FTy
pattern FUnknownTy n = UnknownTy n

pattern FNamedStruct :: StructName -> FTy
pattern FNamedStruct s = NamedStruct s
