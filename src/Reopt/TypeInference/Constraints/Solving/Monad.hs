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
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import Reopt.TypeInference.Constraints.Solving.Constraints
  ( EqC (EqC),
    EqRowC,
    InRowC,
    OrC,
    RowShiftC (RowShiftC),
  )
import Reopt.TypeInference.Constraints.Solving.RowVariables (RowVar (RowVar))
import Reopt.TypeInference.Constraints.Solving.TypeVariableSubstitution (SubstTyVar (substTyVar))
import Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar)
import Reopt.TypeInference.Constraints.Solving.Types (ITy, Offset (Offset), Ty (RecTy), prettyMap)

class Monad m => CanFreshRowVar m where
  freshRowVar :: m RowVar

data ConstraintSolvingState = ConstraintSolvingState
  { ctxEqCs :: [EqC],
    ctxInRowCs :: [InRowC],
    ctxRowShiftCs :: [RowShiftC],
    ctxOrCs :: [OrC],
    ctxEqRowCs :: [EqRowC],
    -- | Equality constraints that were dropped due to being absurd.
    ctxAbsurdEqCs :: [EqC],
    -- | Occurs check failures.
    ctxOccursCheckFailures :: [EqC],
    -- | Known type for type variables.
    ctxTyVarMap :: Map TyVar ITy,
    -- | Known row offset relationships, i.e. @r -> (o,r')@
    --   says @r@ with offset @o@ applied corresponds to
    -- row @r'@. .
    ctxRowShiftMap :: Map RowVar (Set (Offset, RowVar)),
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
            row "RowShiftCs" $ map PP.pretty $ ctxRowShiftCs ctx,
            row "OrCs" $ map PP.pretty $ ctxOrCs ctx,
            row "EqRowCs" $ map PP.pretty $ ctxEqRowCs ctx,
            row "Absurd EqCs" $ map PP.pretty $ ctxAbsurdEqCs ctx,
            row "Occurs check failures" $ map PP.pretty $ ctxOccursCheckFailures ctx,
            row "Type Var Map" $ prettyMap PP.pretty PP.pretty $ ctxTyVarMap ctx,
            row "Row Shift Map" $
              let prettySMap (r, sSet) = map (\(o, r') -> PP.pretty (RowShiftC r o r')) $ Set.toList sSet
               in concatMap prettySMap $ Map.toList $ ctxRowShiftMap ctx
          ]

instance SubstTyVar ConstraintSolvingState where
  -- N.B., we only substitute in unprocessed constraints
  substTyVar xt ctx =
    ConstraintSolvingState
      { ctxEqCs = fmap (substTyVar xt) (ctxEqCs ctx),
        ctxInRowCs = fmap (substTyVar xt) (ctxInRowCs ctx),
        ctxRowShiftCs = fmap (substTyVar xt) (ctxRowShiftCs ctx),
        ctxOrCs = fmap (substTyVar xt) (ctxOrCs ctx),
        ctxEqRowCs = ctxEqRowCs ctx, -- no TyVars in EqRowCs
        ctxTyVarMap = fmap (substTyVar xt) (ctxTyVarMap ctx),
        -- Not modified because we only substitute in unprocessed constraints:
        ctxAbsurdEqCs = ctxAbsurdEqCs ctx,
        ctxOccursCheckFailures = ctxOccursCheckFailures ctx,
        ctxRowShiftMap = ctxRowShiftMap ctx,
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

shiftStructuralInformationBy :: Integer -> Map Offset ITy -> Map Offset ITy
shiftStructuralInformationBy o =
  Map.fromList . concatMap retainPositiveOffsets . Map.toList
  where
    retainPositiveOffsets (Offset a, ty) =
      let newOffset = fromIntegral a + o
       in [(Offset (fromIntegral newOffset), ty) | newOffset >= 0]

substRowVarInRowShiftC ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  RowShiftC ->
  ConstraintSolvingMonad (RowShiftC, Maybe EqC)
substRowVarInRowShiftC r1 r2 os (RowShiftC r3 o@(Offset n) r4)
  | r3 == r1 = do
    -- Here we want to replace r1 with {os | r2} in a constraint meaning:
    -- shift o r1 = r4   -->   shift o {os | r2} = r4
    -- Which means we want to introduce a fresh r such that:
    -- shift o {| r2} = r   and   r4 = {shift o os | r}
    r <- freshRowVar
    return
      ( RowShiftC r2 o r,
        Just (EqC (RecTy mempty r4) (RecTy (shiftStructuralInformationBy (fromIntegral n) os) r))
      )
  | r4 == r1 = do
    -- Here we want to replace r1 with {os | r2} in a constraint meaning:
    -- shift o r3 = r1   -->   shift o r3 {os | r2}
    -- Which means we want to introduce a fresh r such that:
    -- r3 = {shift (-o) os | r}   and   shift o {|r} = {|r2}
    r <- freshRowVar
    return
      ( RowShiftC r o r2,
        Just (EqC (RecTy mempty r3) (RecTy (shiftStructuralInformationBy (- (fromIntegral n)) os) r))
      )
  | otherwise = return (RowShiftC r3 o r4, Nothing)
