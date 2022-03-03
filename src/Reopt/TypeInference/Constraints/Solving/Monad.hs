{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Reopt.TypeInference.Constraints.Solving.Monad where

import           Control.Lens                                          (Lens',
                                                                        uses,
                                                                        (%%=),
                                                                        (%=),
                                                                        (<<+=))
import           Control.Monad.State                                   (MonadState, State, evalState)
import           Data.Generics.Product                                 (field)
import           Data.Map.Strict                                       (Map)
import qualified Data.Map.Strict                                       as Map
import           GHC.Generics                                          (Generic)
import qualified Prettyprinter                                         as PP
import           Reopt.TypeInference.Constraints.Solving.Constraints   (EqC (EqC),
                                                                        EqRowC (EqRowC),)
import           Reopt.TypeInference.Constraints.Solving.RowVariables  (RowVar (RowVar), Offset (Offset), RowExpr (RowExprVar, RowExprShift), rowVar)
import           Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar (TyVar))
import           Reopt.TypeInference.Constraints.Solving.Types         (ITy (..),
                                                                        ITy', prettyMap)

import Reopt.TypeInference.Constraints.Solving.UnionFindMap as UM

import Data.Bifunctor (first)

data ConstraintSolvingState = ConstraintSolvingState
  { ctxEqCs    :: [EqC],
    ctxEqRowCs :: [EqRowC],

    nextTraceId :: Int,
    nextRowVar :: Int,
    nextTyVar  :: Int,

    -- | The union-find data-structure mapping each tyvar onto its
    -- representative tv.  If no mapping exists, it is a self-mapping.

    ctxTyVars :: UnionFindMap TyVar ITy'
  }
  deriving (Eq, Generic, Ord, Show)

emptyContext :: ConstraintSolvingState
emptyContext = ConstraintSolvingState
  { ctxEqCs    = []
  , ctxEqRowCs    = []
  , nextTraceId    = 0
  , nextRowVar     = 0
  , nextTyVar      = 0
  , ctxTyVars      = empty
  }

newtype SolverM a = SolverM
  { getSolverM :: State ConstraintSolvingState a
  }
  deriving (Applicative, Functor, Monad, MonadState ConstraintSolvingState)

runSolverM :: SolverM a -> a
runSolverM = flip evalState emptyContext . getSolverM

--------------------------------------------------------------------------------
-- Adding constraints

addTyVarEq :: TyVar -> TyVar -> SolverM ()
addTyVarEq tv1 tv2 = field @"ctxEqCs" %= (EqC tv1 tv2 :)

addRowVarEq :: RowVar -> Map Offset TyVar -> RowExpr ->
               SolverM ()
addRowVarEq r1 os r2 = field @"ctxEqRowCs" %= (EqRowC r1 os r2 :)

addRowExprEq :: RowExpr -> Map Offset TyVar -> RowExpr ->
               SolverM ()
addRowExprEq (RowExprVar r1) os r2 = addRowVarEq r1 os r2
addRowExprEq (RowExprShift o r1) os r2 = do
  r3 <- freshRowVar
  -- This terminates as eventually we will hit the above with r3
  addRowExprEq r2 mempty (rowVar r3)
  addRowVarEq r1 (shiftOffsets (- o) os) (rowVar r3)

--------------------------------------------------------------------------------
-- Getting constraints

popField :: Lens' ConstraintSolvingState [a] -> SolverM (Maybe a)
popField fld =
  fld %%= \case
    [] -> (Nothing, [])
    (c : cs) -> (Just c, cs)

dequeueEqC :: SolverM (Maybe EqC)
dequeueEqC = popField (field @"ctxEqCs")

dequeueEqRowC :: SolverM (Maybe EqRowC)
dequeueEqRowC = popField (field @"ctxEqRowCs")

--------------------------------------------------------------------------------
-- Operations over type variable state

freshRowVar :: SolverM RowVar
freshRowVar = RowVar <$> (field @"nextRowVar" <<+= 1)

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class.  This also updates the eqv. map to
-- amortise lookups.

lookupTyVarRep :: TyVar -> SolverM TyVar
lookupTyVarRep tv0 = field @"ctxTyVars" %%= UM.lookupRep tv0

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class, and the definition for that type
-- var, if any.

lookupTyVar :: TyVar -> SolverM (TyVar, Maybe ITy')
lookupTyVar tv = field @"ctxTyVars" %%= UM.lookup tv

-- | Always return a new type variable.
freshTyVar' :: Maybe String -> SolverM TyVar
freshTyVar' orig = flip TyVar orig <$> (field @"nextTyVar" <<+= 1)

freshTyVar :: Maybe String -> Maybe ITy -> SolverM TyVar
freshTyVar orig Nothing = freshTyVar' orig
freshTyVar _orig (Just (VarTy v)) = pure v -- Don't allocate, just return the equiv. var.
freshTyVar orig  (Just (ITy ty)) = do
  tyv <- freshTyVar' orig
  defineTyVar tyv ty
  pure tyv

-- | Always define a type variable, even if it has a def.
defineTyVar :: TyVar -> ITy' -> SolverM ()
defineTyVar tyv ty = field @"ctxTyVars" %= UM.insert tyv ty

undefineTyVar :: TyVar -> SolverM ()
undefineTyVar ty = field @"ctxTyVars" %= UM.delete ty

-- | @unsafeUnifyTyVars root leaf@ will make @root@ the new equiv. rep
-- for @leaf@.  Note that both root and leaf should be the reps. of
-- their corresponding equivalence classes. 
unsafeUnifyTyVars :: TyVar -> TyVar -> SolverM ()
unsafeUnifyTyVars root leaf = field @"ctxTyVars" %= UM.unify root leaf

--------------------------------------------------------------------------------
-- Other stuff

shiftStructuralInformationBy :: Integer -> Map Offset v -> Map Offset v
shiftStructuralInformationBy o =
  Map.fromList . concatMap retainPositiveOffsets . Map.toList
  where
    retainPositiveOffsets (Offset a, ty) =
      let newOffset = fromIntegral a + o
       in [(Offset (fromIntegral newOffset), ty) | newOffset >= 0]

-- substRowVarInRowShiftC ::
--   RowVar ->
--   RowVar ->
--   Map Offset TyVar ->
--   RowShiftC ->
--   SolverM (RowShiftC, Maybe EqC)
-- substRowVarInRowShiftC r1 r2 os (RowShiftC r3 o@(Offset n) r4)
--   | r3 == r1 = do
--     -- Here we want to replace r1 with {os | r2} in a constraint meaning:
--     -- shift o r1 = r4   -->   shift o {os | r2} = r4
--     -- Which means we want to introduce a fresh r such that:
--     -- shift o {| r2} = r   and   r4 = {shift o os | r}
--     r <- freshRowVar
--     let eqC = EqC (ITy $ RecTy mempty r4)
--                   (ITy $ RecTy (shiftStructuralInformationBy (fromIntegral n) os) r)
--     return ( RowShiftC r2 o r, Just eqC )
--   | r4 == r1 = do
--     -- Here we want to replace r1 with {os | r2} in a constraint meaning:
--     -- shift o r3 = r1   -->   shift o r3 {os | r2}
--     -- Which means we want to introduce a fresh r such that:
--     -- r3 = {shift (-o) os | r}   and   shift o {|r} = {|r2}
--     r <- freshRowVar
--     let eqC = EqC (ITy $ RecTy mempty r3)
--                   (ITy $ RecTy (shiftStructuralInformationBy (- (fromIntegral n)) os) r)
--     return ( RowShiftC r o r2, Just eqC)
--   | otherwise = return (RowShiftC r3 o r4, Nothing)



--------------------------------------------------------------------------------
-- Instances

instance PP.Pretty ConstraintSolvingState where
  pretty ctx =
    let row title entries = title PP.<+> PP.list entries
     in PP.vsep
          [ row "EqCs" $ map PP.pretty $ ctxEqCs ctx,
            row "EqRowCs" $ map PP.pretty $ ctxEqRowCs ctx,
            row "Type Var Map" [PP.pretty (ctxTyVars ctx)]
          ]

shiftOffsets :: Offset -> Map Offset v -> Map Offset v
shiftOffsets 0 = id
shiftOffsets o =
  Map.fromList . map (first (+ o)) . Map.toList
