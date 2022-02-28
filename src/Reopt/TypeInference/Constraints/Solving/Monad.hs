{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Constraints.Solving.Monad where

import Control.Lens ((<<+=))
import Control.Monad.State (MonadState, State, execState)
import Data.Bifunctor (first)
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import Reopt.TypeInference.Constraints.Solving.Constraints
  ( EqC,
    EqRowC,
    InRowC,
    OrC,
  )
import Reopt.TypeInference.Constraints.Solving.RowVariables (Offset, RowVar (RowVar))
import Reopt.TypeInference.Constraints.Solving.TypeVariableSubstitution (SubstTyVar (substTyVar))
import Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar)
import Reopt.TypeInference.Constraints.Solving.Types (ITy, prettyMap)

class Monad m => CanFreshRowVar m where
  freshRowVar :: m RowVar

data ConstraintSolvingState = ConstraintSolvingState
  { ctxEqCs :: [EqC],
    ctxInRowCs :: [InRowC],
    ctxOrCs :: [OrC],
    ctxEqRowCs :: [EqRowC],
    -- | Equality constraints that were dropped due to being absurd.
    ctxAbsurdEqCs :: [EqC],
    -- | Occurs check failures.
    ctxOccursCheckFailures :: [EqC],
    -- | Known type for type variables.
    ctxTyVarMap :: Map TyVar ITy,
    nextTraceId :: Int,
    nextRowVar :: Int
  }
  deriving (Eq, Generic, Ord, Show)

instance PP.Pretty ConstraintSolvingState where
  pretty ctx =
    let row title entries = title PP.<+> PP.list entries
     in PP.vsep
          [ row "EqCs" $ map PP.pretty $ ctxEqCs ctx,
            row "InRowCs" $ map PP.pretty $ ctxInRowCs ctx,
            row "OrCs" $ map PP.pretty $ ctxOrCs ctx,
            row "EqRowCs" $ map PP.pretty $ ctxEqRowCs ctx,
            row "Absurd EqCs" $ map PP.pretty $ ctxAbsurdEqCs ctx,
            row "Occurs check failures" $ map PP.pretty $ ctxOccursCheckFailures ctx,
            row "Type Var Map" $ prettyMap PP.pretty PP.pretty $ ctxTyVarMap ctx
          ]

instance SubstTyVar ConstraintSolvingState where
  -- N.B., we only substitute in unprocessed constraints
  substTyVar xt ctx =
    ConstraintSolvingState
      { ctxEqCs = fmap (substTyVar xt) (ctxEqCs ctx),
        ctxInRowCs = fmap (substTyVar xt) (ctxInRowCs ctx),
        ctxOrCs = fmap (substTyVar xt) (ctxOrCs ctx),
        ctxEqRowCs = ctxEqRowCs ctx, -- no TyVars in EqRowCs
        ctxTyVarMap = fmap (substTyVar xt) (ctxTyVarMap ctx),
        -- Not modified because we only substitute in unprocessed constraints:
        ctxAbsurdEqCs = ctxAbsurdEqCs ctx,
        ctxOccursCheckFailures = ctxOccursCheckFailures ctx,
        nextTraceId = nextTraceId ctx,
        nextRowVar = nextRowVar ctx
      }

newtype ConstraintSolvingMonad a = ConstraintSolvingMonad
  { getConstraintSolvingMonad :: State ConstraintSolvingState a
  }
  deriving (Applicative, Functor, Monad, MonadState ConstraintSolvingState)

instance CanFreshRowVar ConstraintSolvingMonad where
  freshRowVar = ConstraintSolvingMonad $ RowVar <$> (field @"nextRowVar" <<+= 1)

runConstraintSolvingMonad :: ConstraintSolvingState -> ConstraintSolvingMonad () -> ConstraintSolvingState
runConstraintSolvingMonad s = flip execState s . getConstraintSolvingMonad

shiftOffsets :: Offset -> Map Offset ITy -> Map Offset ITy
shiftOffsets o =
  Map.fromList . map (first (+ o)) . Map.toList
