{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Solver.RowVariableSubstitution where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Reopt.TypeInference.Solver.Constraints
  ( EqRowC (EqRowC),
  )
import Reopt.TypeInference.Solver.Monad
  ( SolverM,
    shiftOffsets, addTyVarEq, addRowVarEq, freshRowVar, addRowExprEq
  )
import Reopt.TypeInference.Solver.RowVariables
  ( RowExpr,
    RowVar,
    rowExprVar, rowExprShift, Offset, rowVar, shiftRowExpr
  )
import Reopt.TypeInference.Solver.Types
  ( TyF (..), ITy'
  )
import Reopt.TypeInference.Solver.TypeVariables (TyVar)
import Data.Foldable (sequenceA_)

substRowVarInITy :: RowVar -> Map Offset TyVar -> RowExpr -> ITy' ->
                    SolverM ITy'
substRowVarInITy r1 os r2 = \case
  RecTy os' r3 | rowExprVar r3 == r1 -> do
    -- FIXME: copied from below.                   
    let os_shifted = shiftOffsets (rowExprShift r3) os
        r2'        = shiftRowExpr (rowExprShift r3) r2
      
    -- Assert common fields are the same
    sequenceA_ $ Map.intersectionWith addTyVarEq os_shifted os'
    pure (RecTy (Map.union os_shifted os') r2')
         
  ty -> pure ty

-- FIXME: move?
unifyRecTy :: Map Offset TyVar -> RowExpr ->
              Map Offset TyVar -> RowExpr -> 
              SolverM ()
unifyRecTy fs1 r1 fs2 r2 = do    
  let onlyIn1 = fs1 `Map.difference` fs2
      onlyIn2 = fs2 `Map.difference` fs1

  -- Unify overlapping vars.
  sequenceA_ $ Map.intersectionWith addTyVarEq fs1 fs2
      
  case (Map.null onlyIn1, Map.null onlyIn2) of
    -- fs1 is a superset of fs2, so we can set
    --    r2 == { onlyIn1 | r1 }
    -- We make ty1 the new defn. of tv2 as there is no more info
    -- to add at that type.
    (_, True) -> addRowExprEq r2 onlyIn1 r1
    -- Dual of the above
    (True, _) -> addRowExprEq r1 onlyIn2 r2
    
    -- We need a fresh row variable for the "remainder" of the row variables
    -- on each side when you factor out the fields mentioned across.
    --
    -- E.g. when you start with
    --
    --   RecTy { 0 : i8 } r1 = RecTy { 8 : i8 } r2
    --
    -- you should conclude that there exists a r3 s.t.
    --
    --   r1 = { 8 : i8 | r3 }   and   r2 = { 0 : i8 | r3 }
    (False, False) -> do
      r3 <- freshRowVar
      addRowExprEq r1 onlyIn2 (rowVar r3)
      addRowExprEq r2 onlyIn1 (rowVar r3)
      
substRowVarInEqRowC :: RowVar ->  Map Offset TyVar -> RowExpr -> EqRowC ->
                       SolverM ()
substRowVarInEqRowC r1 os r2 (EqRowC r3 os' r4)
  -- { os | r2} = {os' | r4 }
  | r1 == r3        = unifyRecTy os r2 os' r4

  -- r3 = { os' | { os | r2 } }
  | rowExprVar r4 == r1 = do
      let os_shifted = shiftOffsets (rowExprShift r4) os
          r2'  = shiftRowExpr (rowExprShift r4) r2
      
      -- Assert common fields are the same
      sequenceA_ $ Map.intersectionWith addTyVarEq os_shifted os'
      addRowVarEq r3 (Map.union os_shifted os') r2'

  -- Just re-emit row eq.
  | otherwise = addRowVarEq r3 os' r4

