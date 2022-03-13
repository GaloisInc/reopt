{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Solver
  ( Ty (..), TyVar, numTy, ptrTy, ptrTy', varTy,
    SolverM, runSolverM,
    eqTC, ptrTC, freshTyVar, ptrAddTC,
    OperandClass (..),
    unifyConstraints, ConstraintSolution(..), StructName,
    tyToLLVMType,
    -- FTy stuff
    FTy, pattern FNumTy, pattern FPtrTy, pattern FUnknownTy, pattern FNamedStruct,  pattern FStructTy,
    -- Testing
  ) where


import qualified Prettyprinter                            as PP
import           Reopt.TypeInference.Solver.Constraints   (OperandClass (..), EqC (EqC))
import           Reopt.TypeInference.Solver.Finalise      (ConstraintSolution (..))
import           Reopt.TypeInference.Solver.Monad         (Conditional (..),
                                                           PatternRHS (..),
                                                           Schematic (DontCare),
                                                           SolverM, addCondEq,
                                                           addTyVarEq,
                                                           freshRowVarFM,
                                                           freshTyVar,
                                                           ptrWidthNumTy,
                                                           runSolverM,
                                                           withFresh, Pattern (Pattern))
import           Reopt.TypeInference.Solver.RowVariables  (FieldMap,
                                                           RowExpr (..),
                                                           singletonFieldMap)
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

  where
    isNum v = Pattern v IsNum
    isPtr v = Pattern v (IsPtr DontCare)

-- -- | If this detects a looping add (i.e., from while () *p++;) then we
-- -- disable the constraint.  This will could return Just True over
-- -- Nothing more often, but doens't need to (and is a hack).
-- cycleFilter :: TyVar -> TyVar -> Offset -> SolverM (Maybe Bool)
-- cycleFilter rv0 lhsv0 off = do
--   (_rv, m_rty) <- lookupTyVar rv0
--   (_lhsv, m_lhsTy) <- lookupTyVar lhsv0

--   -- The *p++ case, where we are looping through via a pointer, and
--   -- we thus have a constant offset.  This boils down to x = x +
--   -- off, although there may be a bit of work to get here, and we
--   -- won't hav ethe same type variables, only the same row variable.

--   -- Handles the recursive while () *p++ case.  If we try to unify
--   --
--   -- shift k r = shift j r
--   --
--   -- k + off =/= r
--   --
--   -- The we are in the array stride case and can set the result to
--   -- the first operand (more or less?)
--   case (m_rty, m_lhsTy) of
--     (Just (PtrTy rre), Just (PtrTy lre)) -> do
--       lre' <- lookupRowExprRep lre
--       rre' <- lookupRowExprRep rre
--       pure $ Just . not $
--         rowExprVar rre' == rowExprVar lre'
--         && rowExprShift rre' /= rowExprShift lre' + off
--     _ -> pure Nothing

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
