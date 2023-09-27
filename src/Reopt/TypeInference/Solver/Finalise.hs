{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Solver.Finalise (
  finalizeTypeDefs,
  ConstraintSolution (..),
) where

import Control.Lens (over, _3)
import Control.Monad.State (gets)
import Data.Either (lefts, partitionEithers)
import Data.Generics.Labels ()
import Data.Graph (SCC (..), flattenSCCs)
import Data.Graph.SCC (stronglyConnCompR)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Prettyprinter qualified as PP

import Reopt.TypeInference.Solver.Monad (
  ConstraintSolvingState (..),
  SolverM,
  freshRowVarFM,
  lookupRowExpr,
  lookupTyVarRep,
 )
import Reopt.TypeInference.Solver.RowVariables (
  FieldMap,
  RowVar,
  dropFieldMap,
  emptyFieldMap,
  rowExprShift,
  rowExprVar,
 )
import Reopt.TypeInference.Solver.TypeVariables (TyVar)
import Reopt.TypeInference.Solver.Types (
  FTy (..),
  FreeRowVars,
  FreeTyVars (freeTyVars),
  ITy',
  StructName,
  TyF (..),
  freeRowVars,
  prettyMap,
  rowVarToStructName,
 )
import Reopt.TypeInference.Solver.UnionFindMap qualified as UM

data ConstraintSolution = ConstraintSolution
  { csTyVars :: Map TyVar FTy
  , csRowVars :: Map RowVar FTy
  , csNamedStructs :: [(StructName, FTy)]
  }
  deriving (Eq, Show, Generic)

-- csRowExpr :: ConstraintSolution -> RowVar -> FTy
-- csRowExpr cs rv =
--   Map.findWithDefault (FTy (PtrTy emptyFieldMap)) rv (csRowVars cs)

-- FIXME: this breaks the abstraction of the UnionFindMap.
finalizeTypeDefs :: SolverM ConstraintSolution
finalizeTypeDefs = do
  -- Might introduce new row vars so has to be first.
  tm <- traverse normTyF =<< gets ctxTyVars

  rm <- traverse normFieldMap =<< gets ctxRowVars
  pure (finalizeTypeDefs' tm rm)
 where
  -- This will copy FieldMaps such that everything is 0 based.
  normTyF :: ITy' -> SolverM (TyF RowVar TyVar)
  normTyF = \case
    PtrTy re -> do
      (re', m_fm) <- lookupRowExpr re
      let fm = fromMaybe emptyFieldMap m_fm
      let off = rowExprShift re'
      if off == 0
        then pure (PtrTy (rowExprVar re'))
        else PtrTy <$> freshRowVarFM (dropFieldMap off fm)
    UnknownFunPtrTy -> pure UnknownFunPtrTy
    FunPtrTy args ret -> FunPtrTy <$> mapM lookupTyVarRep args <*> lookupTyVarRep ret
    NumTy n -> pure (NumTy n)
    ConflictTy n -> pure (ConflictTy n)
    TupleTy ts -> TupleTy <$> traverse lookupTyVarRep ts
    VecTy n ty -> VecTy n <$> lookupTyVarRep ty

  normFieldMap :: FieldMap TyVar -> SolverM (FieldMap TyVar)
  normFieldMap = traverse lookupTyVarRep

-- FIXME: breaks abstractions
finalizeTypeDefs' ::
  UM.UnionFindMap TyVar TyVar (TyF RowVar TyVar) ->
  UM.UnionFindMap RowVar ki (FieldMap TyVar) ->
  ConstraintSolution
finalizeTypeDefs' um@(UM.UnionFindMap teqvs tdefs) (UM.UnionFindMap _reqvs rdefs) =
  over #csTyVars (Map.union eqvRes) preSoln
 where
  -- Include equivalences.
  eqvRes = Map.fromSet mkOneEqv (Map.keysSet teqvs)

  mkOneEqv k =
    let (k', _) = UM.lookupRep k um
     in resolveOne (csTyVars preSoln) k'

  preSoln = foldl goSCC (ConstraintSolution mempty mempty mempty) sccs

  -- duplicated from below.
  resolveOne m t = Map.findWithDefault UnknownTy t m

  goSCC r (AcyclicSCC (Left n)) = finaliseTyF n r
  goSCC r (AcyclicSCC (Right n)) = finaliseFieldMap n r
  goSCC r (CyclicSCC cs) = finaliseCyclic cs r

  sccs :: [SCC Node]
  sccs = map (fmap mkNode) (stronglyConnCompR nodes)

  mkNode (Left ty, Left tv, es) = Left (ty, tv, es)
  mkNode (Right fm, Right rv, es) = Right (fm, rv, es)
  mkNode _ = error "Impossible"

  nodes =
    [ (Left ty, Left tv, edges ty)
    | (tv, ty) <- Map.toList tdefs -- tv is a rep. var.
    ]
      ++ [ (Right fm, Right rv, edges fm)
         | (rv, fm) <- Map.toList rdefs -- tv is a rep. var.
         ]

  edges ::
    forall a.
    (FreeTyVars a, FreeRowVars a) =>
    a ->
    [Either TyVar RowVar]
  edges v =
    (Left <$> Set.toList (freeTyVars v))
      ++ (Right <$> Set.toList (freeRowVars v))

type Node =
  Either
    (TyF RowVar TyVar, TyVar, [NodeName])
    (FieldMap TyVar, RowVar, [NodeName])
type NodeName = Either TyVar RowVar

finaliseTyF ::
  (TyF RowVar TyVar, TyVar, a) ->
  ConstraintSolution ->
  ConstraintSolution
finaliseTyF (ty, tv, _) r =
  over #csTyVars (Map.insert tv (norm ty)) r
 where
  norm = \case
    PtrTy rv -> FTy (PtrTy (Map.findWithDefault (StructTy emptyFieldMap) rv (csRowVars r)))
    UnknownFunPtrTy -> FTy UnknownFunPtrTy
    FunPtrTy args ret -> FTy (FunPtrTy (map normTy args) (normTy ret))
    NumTy n -> FTy (NumTy n)
    ConflictTy n -> FTy (ConflictTy n)
    TupleTy ts -> FTy (TupleTy (map normTy ts))
    VecTy n t -> FTy (VecTy n (normTy t))
  normTy t = Map.findWithDefault UnknownTy t (csTyVars r)

finaliseFieldMap ::
  (FieldMap TyVar, RowVar, a) ->
  ConstraintSolution ->
  ConstraintSolution
finaliseFieldMap (fm, rv, _) r =
  over #csRowVars (Map.insert rv (StructTy (norm <$> fm))) r
 where
  norm t = Map.findWithDefault UnknownTy t (csTyVars r)

finaliseCyclic :: [Node] -> ConstraintSolution -> ConstraintSolution
finaliseCyclic cs s
  -- We have cycles even after removing records, perhaps through
  -- pointers, so we just drop the types (i.e., make Unknown)
  | any isCyclic sccs = s
  -- trace ("Ignoring too cyclic types" ++
  --        show (PP.list
  --              [ PP.pretty n PP.<+> "=" PP.<+> PP.pretty ty
  --              | (ty, n, _) <- cs
  --              ])) s
  | otherwise =
      -- trace ("Keeping cyclic types " ++
      --        show (PP.list
      --              [ PP.pretty n PP.<+> "=" PP.<+> PP.pretty ty
      --              | (ty, n, _) <- cs
      --              ])) $
      over #csNamedStructs (namedDefs ++) sWithTys
 where
  namedDefs =
    [ (rowVarToStructName rv, StructTy $ resolveOne <$> fm)
    | (fm, rv, _) <- fms
    ]

  -- Perform this topologically to ensure we have defs before we see
  -- them.
  sWithTys = foldl (flip finaliseTyF) sWithNamed tys'

  sWithNamed = over #csRowVars (Map.union named) s

  named =
    Map.fromList
      [ (rv, NamedStruct $ rowVarToStructName rv)
      | (_, rv, _) <- fms
      ]

  (tys, fms) = partitionEithers cs

  sccs = stronglyConnCompR (over _3 lefts <$> tys)
  tys' = flattenSCCs sccs

  resolveOne t = Map.findWithDefault UnknownTy t (csTyVars sWithTys)

  isCyclic CyclicSCC{} = True
  isCyclic _ = False

--------------------------------------------------------------------------------
-- Instances

instance PP.Pretty ConstraintSolution where
  pretty soln =
    PP.vsep $
      prettyDefs (csNamedStructs soln)
        ++ prettyMap PP.pretty PP.pretty (csTyVars soln)
   where
    prettyDefs ds =
      [ PP.pretty n PP.<+> "=" PP.<+> PP.pretty ty
      | (n, ty) <- ds
      ]
