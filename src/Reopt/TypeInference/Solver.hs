{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Reopt.TypeInference.Solver
  ( Ty (..), TyVar, RowVar, numTy, ptrTy, ptrTy', varTy,
    SolverM, runSolverM,
    eqTC, ptrTC, maybeGlobalTC, isNumTC, ptrSubTC,
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
                                                           OperandClass (..),
                                                           ConstraintProvenance (..))
import           Reopt.TypeInference.Solver.Finalise      (ConstraintSolution (..))
import           Reopt.TypeInference.Solver.Monad         (Conditional (..),
                                                           Conjunction (Conjunction),
                                                           Disjunction (Disjunction),
                                                           Schematic (DontCare, Schematic),
                                                           Pattern (Pattern),
                                                           PatternRHS (..),
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

eqTC :: ConstraintProvenance -> Ty -> Ty -> SolverM ()
eqTC prov ty1 ty2 = do
  tv1 <- nameTy ty1
  ity2 <- compileTy ty2
  addTyVarEq prov tv1 ity2

-- emits ptr :: PtrTy { 0 -> target }
ptrTC :: ConstraintProvenance -> Ty -> Ty -> SolverM ()
ptrTC prov target ptr = eqTC prov ptr (ptrTy (singletonFieldMap 0 target))

subTypeTC :: Ty -> Ty -> SolverM ()
subTypeTC a b = join $ addSubType <$> nameTy a <*> nameTy b

isNumTC :: ConstraintProvenance -> Ty -> Int -> SolverM ()
isNumTC prov tv n = join $ addTyVarEq prov <$> nameTy tv <*> pure (ITy $ NumTy n)

--------------------------------------------------------------------------------
-- Pointer-sized addition

ptrAddTC :: ConstraintProvenance -> Ty -> Ty -> Ty -> OperandClass -> SolverM ()
ptrAddTC prov rty lhsty rhsty oc = do
  rv    <- nameTy rty
  lhstv <- nameTy lhsty
  rhstv <- nameTy rhsty
  ptrNumTy <- ITy <$> ptrWidthNumTy

  -- Numeric cases
  let name =
        PP.pretty rv
          <> " = "
          <> PP.pretty lhstv
          <> " + "
          <> PP.pretty rhstv
  addCondEq $
    Conditional
      { cName = show (name PP.<+> "(non-ptr case)")
      , cGuard =
          Disjunction
            [ Conjunction [isNum rv]
            , Conjunction [isNum lhstv, isNum rhstv]
            , -- add is strict wrt conflicts.
              Conjunction [isConflict rv]
            , Conjunction [isConflict lhstv]
            , Conjunction [isConflict rhstv]
            ]
      , cConstraints =
          ( [ EqC rv (VarTy lhstv) prov
            , EqC rv (VarTy rhstv) prov
            ]
          , []
          )
      }

  case oc of
    OCOffset o -> do
      let name' = name PP.<+> PP.parens ("+ " <> PP.pretty o)
      addTyVarEq prov rhstv ptrNumTy

      withFresh $ \rowv ->
        addCondEq $
          Conditional
            { cName = show (name' PP.<+> "(pointer case)")
            , cGuard = Disjunction [Conjunction [isPtr rv], Conjunction [isPtr lhstv]]
            , -- Unify with new rowvar, and constrain.
              cConstraints =
                (
                  [ EqC rv (ITy (PtrTy (RowExprShift o rowv))) prov
                  , EqC lhstv (ITy (PtrTy (RowExprVar rowv))) prov
                  ]
                , []
                )
            }
    OCSymbolic -> do
      withFresh $ \resrv lrv ->
        addCondEq $
          Conditional
            { cName = show (name PP.<+> "(symbolic pointer case 1)")
            , cGuard =
                Disjunction
                  [ Conjunction [isPtr rv, isNum rhstv]
                  , Conjunction [isPtr lhstv]
                  ]
            , cConstraints =
                (
                  [ EqC rv (ITy (PtrTy resrv)) prov
                  , EqC lhstv (ITy (PtrTy lrv)) prov
                  , EqC rhstv ptrNumTy prov
                  ]
                , []
                )
            }

      withFresh $ \resrv rrv ->
        addCondEq $
          Conditional
            { cName = show (name PP.<+> "(symbolic pointer case 2)")
            , cGuard = Disjunction [Conjunction [isPtr rv, isNum lhstv], Conjunction [isPtr rhstv]]
            , cConstraints =
                (
                  [ EqC rv (ITy (PtrTy resrv)) prov
                  , EqC rhstv (ITy (PtrTy rrv)) prov
                  , EqC lhstv ptrNumTy prov
                  ]
                , []
                )
            }

    -- FIXME: we are assuming that large numbers (in data seg.) cannot be offsets
    OCPointer ->
      withFresh $ \rhsrowv ->
        addCondEq $
          Conditional
            { cName = show (name PP.<+> "(possible global pointer case)")
            , cGuard = Disjunction [Conjunction [isPtr rv]]
            , cConstraints =
                (
                  [ EqC lhstv ptrNumTy prov
                  , EqC rhstv (ITy $ PtrTy rhsrowv) prov
                  ]
                , []
                )
            }

-- addCondEq $ Conditional
--   { cName       = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty rhstv
--                     PP.<+> "(possible global pointer case 2)")
--   , cEnabled  = isNum lhstv
--   , cAddConstraints = addTyVarEq rv (VarTy lhstv) >> addTyVarEq rv (VarTy rhstv)
--   }

{- | @ptrAddTC rty lhsty rhsty oc@ is emitted when we have a
subtraction, or addition with a negative offset.
-}
ptrSubTC :: ConstraintProvenance -> Ty -> Ty -> Ty -> OperandClass -> SolverM ()
ptrSubTC prov rty lhsty rhsty oc = do
  rv    <- nameTy rty
  lhstv <- nameTy lhsty
  rhstv <- nameTy rhsty
  ptrNumTy <- ITy <$> ptrWidthNumTy

  -- Numeric cases
  let name       = PP.pretty rv <> " = " <> PP.pretty lhstv <> " - " <>
                   PP.pretty rhstv

  -- If we have a numeric result and at least one numberic argument,
  -- or we have a numeric lhs, we can infer we are doing a numeric sub
  addCondEq $
    Conditional
      { cName = show (name PP.<+> "(non-ptr case)")
      , cGuard =
          Disjunction
            [ Conjunction [isNum rv, isNum rhstv]
            , Conjunction [isNum lhstv]
            , Conjunction [isConflict rv]
            , Conjunction [isConflict lhstv]
            , Conjunction [isConflict rhstv]
            ]
      , cConstraints =
          ( [ EqC rv (VarTy lhstv) prov
            , EqC rv (VarTy rhstv) prov
            ]
          , []
          )
      }

  case oc of
    OCOffset o -> do
      let name' = name PP.<+> PP.parens ("- " <> PP.pretty o)
      addTyVarEq prov rhstv ptrNumTy

      -- dual to the rv = ptr + off
      withFresh $ \rowv ->
        addCondEq $
          Conditional
            { cName = show (name' PP.<+> "(ptr - off)")
            , cGuard = Disjunction [Conjunction [isPtr rv], Conjunction [isPtr lhstv]]
            , -- Unify with new rowvar, and constrain.
              cConstraints =
                (
                  [ EqC lhstv (ITy (PtrTy (RowExprShift o rowv))) prov
                  , EqC rv (ITy (PtrTy (RowExprVar rowv))) prov
                  ]
                , []
                )
            }
    _ -> do
      withFresh $ \resrv lrv ->
        addCondEq $
          Conditional
            { cName = show (name PP.<+> "(ptr - num)")
            , cGuard =
                Disjunction
                  [ Conjunction [isPtr rv]
                  , Conjunction [isPtr lhstv, isNum rhstv]
                  ]
            , cConstraints =
                (
                  [ EqC rv (ITy (PtrTy resrv)) prov
                  , EqC lhstv (ITy (PtrTy lrv)) prov
                  , EqC rhstv ptrNumTy prov
                  ]
                , []
                )
            }

      -- If the lhs and rhs are both pointer, the result is a number
      addCondEq $
        Conditional
          { cName = show (name PP.<+> "(ptr - ptr)")
          , cGuard = Disjunction [Conjunction [isPtr lhstv, isPtr rhstv]]
          , cConstraints =
              ( [EqC rv ptrNumTy prov]
              , []
              )
          }

isConflict :: TyVar -> Pattern
isConflict v = Pattern v IsConflict

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
    addCondEq $
      Conditional
        { cName = show (PP.pretty tv <> " is " <> PP.pretty rowv <> " + " <> PP.pretty off)
        , cGuard = Disjunction [Conjunction [Pattern tv (IsPtr (Schematic rowv'))]]
        , cConstraints = ([], [EqRowC (RowExprVar rowv') (RowExprShift off rowv)])
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
