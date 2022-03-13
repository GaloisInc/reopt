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


import Reopt.TypeInference.Solver.RowVariables
  ( RowExpr (..), Offset, rowExprVar, rowExprShift, FieldMap, singletonFieldMap
  )
import Reopt.TypeInference.Solver.Solver
  ( unifyConstraints
  )
import Reopt.TypeInference.Solver.Finalise
  ( ConstraintSolution(..)
  )
import Reopt.TypeInference.Solver.TypeVariables
  ( TyVar (..),
  )
import Reopt.TypeInference.Solver.Types
  ( ITy(..), ITy',
    TyF(..),
    FTy(..), tyToLLVMType, StructName
  )
import Reopt.TypeInference.Solver.Monad
  (SolverM, runSolverM, freshTyVar, addTyVarEq
  , ptrWidthNumTy, Conditional(..), addCondEq, withFresh, lookupTyVar, freshRowVarFM, lookupRowExprRep)
import Reopt.TypeInference.Solver.Constraints (OperandClass(..))
import qualified Prettyprinter as PP

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

--------------------------------------------------------------------------------
-- Pointer-sized addition

-- These combinators are a little tricky (we say defined when the FV have defs):
--  - x .&. y will PASS   when BOTH   x and y are defined and true
--            will REJECT when EITHER x or  y are defined and false
--            will DELAY  when BOTH   x and y are delayed
--
--  - x .*. y will PASS   when EITHER x and y are defined and true
--            will REJECT when EITHER x and y are defined and false
--            will DELAY  when BOTH   x and y are delayed

(.*.), (.&.) :: SolverM (Maybe Bool) -> SolverM (Maybe Bool) ->
                SolverM (Maybe Bool)
x .&. y = go <$> x <*> y
  where
    go (Just False) _           = Just False
    go _           (Just False) = Just False
    go (Just True) (Just True)  = Just True
    go _           _            = Nothing

x .*. y = go <$> x <*> y
  where
    go (Just False) _           = Just False
    go _           (Just False) = Just False
    go (Just True) _            = Just True
    go _           (Just True)  = Just True
    go _           _            = Nothing

ptrAddTC :: Ty -> Ty -> Ty -> OperandClass -> SolverM ()
ptrAddTC rty lhsty rhsty oc = do
  rv    <- nameTy rty
  lhstv <- nameTy lhsty
  rhstv <- nameTy rhsty
  addCondEq $ Conditional
    { cName       = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty rhstv
                          PP.<+> "(numeric case)")
    , cEnabled  = isNum rv .*. (isNum lhstv .&. isNum rhstv)
    , cAddConstraints = addTyVarEq rv (VarTy lhstv) >> addTyVarEq rv (VarTy rhstv)
    }

  case oc of
    OCOffset o -> do
      addTyVarEq rhstv . ITy =<< ptrWidthNumTy
      addCondEq $ Conditional
        { cName = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty o
                       PP.<+> "(pointer case)")
        , cEnabled = cycleFilter rv lhstv o .*. (isPtr rv .*. isPtr lhstv)
        , cAddConstraints = withFresh $ \rowv -> do
            -- this is a bit gross, but should work.
            addTyVarEq rv    (ITy (PtrTy (RowExprShift o rowv)))
            addTyVarEq lhstv (ITy (PtrTy (RowExprVar     rowv)))
        }

    OCSymbolic -> do
      addCondEq $ Conditional
        { cName       = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty rhstv
                          PP.<+> "(symbolic pointer case 1)")
        , cEnabled  = (isPtr rv .*. isPtr lhstv) .&. isNum rhstv
        , cAddConstraints = withFresh $ \rt lt -> do
            addTyVarEq rv    (ITy $ PtrTy rt)
            addTyVarEq lhstv (ITy $ PtrTy lt)
        }
        
      addCondEq $ Conditional
        { cName       = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty rhstv
                          PP.<+> "(symbolic pointer case 2)")
        , cEnabled  = (isPtr rv .*. isPtr rhstv) .&. isNum lhstv
        , cAddConstraints = withFresh $ \rt rhst -> do
            -- Force both to be pointers, although we don't know to what.
            addTyVarEq rv    (ITy $ PtrTy rt)
            addTyVarEq rhstv (ITy $ PtrTy rhst)
        }

    OCPointer -> do
      addCondEq $ Conditional
        { cName       = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty rhstv
                          PP.<+> "(possible global pointer case 1)")
        , cEnabled  = isPtr rv .*. isPtr rhstv -- probably the rhs is never a pointer.
        , cAddConstraints = withFresh $ \rt rhst -> do
            -- Force the rhs to be a pointer, lhs a number
            addTyVarEq lhstv . ITy =<< ptrWidthNumTy
            addTyVarEq rv    (ITy $ PtrTy rt)            
            addTyVarEq rhstv (ITy $ PtrTy rhst)
        }

      -- addCondEq $ Conditional
      --   { cName       = show (PP.pretty rv <> " = " <> PP.pretty lhstv <> " + " <> PP.pretty rhstv
      --                     PP.<+> "(possible global pointer case 2)")
      --   , cEnabled  = isNum lhstv
      --   , cAddConstraints = addTyVarEq rv (VarTy lhstv) >> addTyVarEq rv (VarTy rhstv)
      --   }
  where
    isPtr' PtrTy {} = True
    isPtr' _         = False

    isNum' NumTy {} = True
    isNum' _        = False

    isThing :: (ITy' -> Bool) -> TyVar -> SolverM (Maybe Bool)
    isThing f t = do
      (_, m_r) <- lookupTyVar t
      pure (f <$> m_r)

    isPtr, isNum :: TyVar -> SolverM (Maybe Bool)
    isPtr = isThing isPtr'
    isNum = isThing isNum'

-- | If this detects a looping add (i.e., from while () *p++;) then we
-- disable the constraint.  This will could return Just True over
-- Nothing more often, but doens't need to (and is a hack).
cycleFilter :: TyVar -> TyVar -> Offset -> SolverM (Maybe Bool)
cycleFilter rv0 lhsv0 off = do
  (_rv, m_rty) <- lookupTyVar rv0
  (_lhsv, m_lhsTy) <- lookupTyVar lhsv0

  -- The *p++ case, where we are looping through via a pointer, and
  -- we thus have a constant offset.  This boils down to x = x +
  -- off, although there may be a bit of work to get here, and we
  -- won't hav ethe same type variables, only the same row variable.

  -- Handles the recursive while () *p++ case.  If we try to unify
  --
  -- shift k r = shift j r
  --
  -- k + off =/= r
  --
  -- The we are in the array stride case and can set the result to
  -- the first operand (more or less?)
  case (m_rty, m_lhsTy) of
    (Just (PtrTy rre), Just (PtrTy lre)) -> do
      lre' <- lookupRowExprRep lre
      rre' <- lookupRowExprRep rre
      pure $ Just . not $
        rowExprVar rre' == rowExprVar lre'
        && rowExprShift rre' /= rowExprShift lre' + off
    _ -> pure Nothing

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
