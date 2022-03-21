{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Reopt.TypeInference.Solver
  ( Ty (..), TyVar, RowVar, numTy, ptrTy, ptrTy', varTy,
    SolverM, runSolverM,
    eqTC, ptrTC, maybeGlobalTC, isNumTC,
    freshTyVar, freshRowVar, ptrAddTC, subTypeTC,
    OperandClass (..),
    unifyConstraints, ConstraintSolution(..), StructName,
    tyToLLVMType,
   
    -- FTy stuff
    FTy, pattern FNumTy, pattern FPtrTy, pattern FUnknownTy
  , pattern FNamedStruct,  pattern FStructTy, pattern FConflictTy
    -- Testing
  ) where


import           Control.Monad                            (join)
import qualified Prettyprinter                            as PP

import           Reopt.TypeInference.Solver.Constraints   (EqC (EqC), EqRowC (..),
                                                           OperandClass (..))
import           Reopt.TypeInference.Solver.Finalise      (ConstraintSolution (..))
import           Reopt.TypeInference.Solver.Monad         (Conditional (..), Schematic (Schematic),
                                                           Pattern (Pattern),
                                                           PatternRHS (..),
                                                           Schematic (DontCare),
                                                           SolverM, addCondEq,
                                                           addSubType,
                                                           addTyVarEq,
                                                           freshRowVar,
                                                           freshRowVarFM,
                                                           freshTyVar,
                                                           ptrWidthNumTy,
                                                           runSolverM,
                                                           withFresh)
import           Reopt.TypeInference.Solver.RowVariables  (FieldMap,
                                                           RowExpr (..), RowVar,
                                                           singletonFieldMap, Offset)
import           Reopt.TypeInference.Solver.Solver        (unifyConstraints)
import           Reopt.TypeInference.Solver.TypeVariables (TyVar (..))
import           Reopt.TypeInference.Solver.Types         (FTy (..), ITy (..),
                                                           StructName, TyF (..),
                                                           tyToLLVMType)

-- This type is easier to work with, as it isn't normalised.
data Ty =
  Var TyVar
  | Ty  (TyF (FieldMap Ty) Ty)

-- Smart constructors for Ty

numTy :: Int -> Ty
numTy = Ty . NumTy

ptrTy :: FieldMap Ty -> Ty
ptrTy = Ty . PtrTy

ptrTy' :: Ty -> Ty
ptrTy' = Ty . PtrTy . singletonFieldMap 0

varTy :: TyVar -> Ty
varTy = Var

--------------------------------------------------------------------------------
-- Compilers from Ty into ITy

nameTy :: Ty -> SolverM TyVar
nameTy ty = freshTyVar Nothing . Just =<< compileTy ty

compileTy :: Ty -> SolverM ITy
compileTy (Var tv) = pure (VarTy tv)
compileTy (Ty ty)  = ITy <$>
  case ty of
    NumTy n  -> pure (NumTy n)
    PtrTy fm -> do
      fm' <- traverse nameTy fm
      PtrTy . RowExprVar <$> freshRowVarFM fm'
    ConflictTy n -> pure (ConflictTy n)
    TupleTy ts -> TupleTy <$> traverse nameTy ts
    VecTy n ty' -> VecTy n <$> nameTy ty'
    
--------------------------------------------------------------------------------
-- Constraint constructors

eqTC :: Ty -> Ty -> SolverM ()
eqTC ty1 ty2 = do
  tv1 <- nameTy ty1
  ity2 <- compileTy ty2
  addTyVarEq tv1 ity2

-- emits ptr :: PtrTy { 0 -> target }
ptrTC :: Ty -> Ty -> SolverM ()
ptrTC target ptr = eqTC ptr (ptrTy (singletonFieldMap 0 target))

subTypeTC :: Ty -> Ty -> SolverM ()
subTypeTC a b = join $ addSubType <$> nameTy a <*> nameTy b

isNumTC :: Ty -> Int -> SolverM ()
isNumTC tv n = join $ addTyVarEq <$> nameTy tv <*> pure (ITy $ NumTy n)

--------------------------------------------------------------------------------
-- Pointer-sized addition

ptrAddTC :: Ty -> Ty -> Ty -> OperandClass -> SolverM ()
ptrAddTC rty lhsty rhsty oc = do
  rv    <- nameTy rty
  lhstv <- nameTy lhsty
  rhstv <- nameTy rhsty
  ptrNumTy <- ITy <$> ptrWidthNumTy

  -- Numeric cases
  let name       = PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <>
                   PP.pretty rhstv
  addCondEq $ Conditional
    { cName       = show (name PP.<+> "(numeric case)")
    , cGuard       = [ [ isNum rv ], [ isNum lhstv, isNum rhstv ] ]
    , cConstraints = ( [EqC rv (VarTy lhstv), EqC rv (VarTy rhstv)]
                     , [])
    }

  case oc of
    OCOffset o -> do
      let name' = name PP.<+> PP.parens ("+ " <> PP.pretty o)
      addTyVarEq rhstv ptrNumTy

      withFresh $ \rowv ->
        addCondEq $ Conditional
        { cName  = show (name' PP.<+> "(pointer case)")
        , cGuard = [ [ isPtr rv ], [ isPtr lhstv ] ]
          -- Unify with new rowvar, and constrain.
        , cConstraints = ( [ EqC rv    (ITy (PtrTy (RowExprShift o rowv)))
                           , EqC lhstv (ITy (PtrTy (RowExprVar     rowv)))
                           ]
                         , [] )
        }

    OCSymbolic -> do
      withFresh $ \resrv lrv ->
        addCondEq $ Conditional
        { cName       = show (name PP.<+> "(symbolic pointer case 1)")
        , cGuard  = [ [ isPtr rv   , isNum rhstv ]
                    , [ isPtr lhstv, isNum rhstv ] ]
        , cConstraints = ( [ EqC rv    (ITy (PtrTy resrv))
                           , EqC lhstv (ITy (PtrTy lrv)) ]
                         , [] )
        }

      withFresh $ \resrv rrv ->
        addCondEq $ Conditional
        { cName       = show (name PP.<+> "(symbolic pointer case 2)")
        , cGuard  = [ [ isPtr rv   , isNum lhstv ], [ isPtr rhstv, isNum lhstv ] ]
        , cConstraints = ( [ EqC rv    (ITy (PtrTy resrv))
                           , EqC rhstv (ITy (PtrTy rrv)) ]
                         , [] )
        }

    OCPointer ->
      withFresh $ \rhsrowv ->
        addCondEq $ Conditional
        { cName       = show (name PP.<+> "(possible global pointer case)")
        , cGuard      = [[ isPtr rv ]]
        , cConstraints = ( [ EqC lhstv ptrNumTy
                           , EqC rhstv (ITy $ PtrTy rhsrowv) ]
                         , [] )
        }

      -- addCondEq $ Conditional
      --   { cName       = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty rhstv
      --                     PP.<+> "(possible global pointer case 2)")
      --   , cEnabled  = isNum lhstv
      --   , cAddConstraints = addTyVarEq rv (VarTy lhstv) >> addTyVarEq rv (VarTy rhstv)
      --   }


isNum :: TyVar -> Pattern
isNum v = Pattern v IsNum

isPtr :: TyVar -> Pattern
isPtr v = Pattern v (IsPtr DontCare)

--------------------------------------------------------------------------------
-- Global pointers

maybeGlobalTC :: Ty -> RowVar -> Offset -> SolverM ()
maybeGlobalTC ty rowv off = do
  tv <- nameTy ty
  withFresh $ \rowv' -> 
    addCondEq $ Conditional
    { cName       = show (PP.pretty tv <> " is " <> PP.pretty rowv <> " + " <> PP.pretty off )
    , cGuard      = [[ Pattern tv (IsPtr (Schematic rowv')) ]]
    , cConstraints = ( [], [EqRowC (RowExprVar rowv') (RowExprShift off rowv) ] )
    }
  
--------------------------------------------------------------------------------
-- LLVM support (FTy patterns)

pattern FNumTy :: Int -> FTy
pattern FNumTy sz = FTy (NumTy sz)

pattern FPtrTy :: FTy -> FTy
pattern FPtrTy ty = FTy (PtrTy ty)

pattern FUnknownTy :: FTy
pattern FUnknownTy = UnknownTy

pattern FStructTy :: FieldMap FTy -> FTy
pattern FStructTy fm = StructTy fm

pattern FNamedStruct :: StructName -> FTy
pattern FNamedStruct s = NamedStruct s

pattern FConflictTy :: Int -> FTy
pattern FConflictTy n = FTy (ConflictTy n)
