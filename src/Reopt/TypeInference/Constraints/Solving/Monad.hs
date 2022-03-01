{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Reopt.TypeInference.Constraints.Solving.Monad where

import           Control.Lens                                          (Lens',
                                                                        at, use,
                                                                        uses,
                                                                        (%%=),
                                                                        (%=),
                                                                        (.=),
                                                                        (<<+=))
import           Control.Monad.State                                   (MonadState (get, put),
                                                                        State,
                                                                        execState,
                                                                        gets,
                                                                        modify)
import           Data.Generics.Product                                 (field)
import           Data.Map.Strict                                       (Map)
import qualified Data.Map.Strict                                       as Map
import           Data.Set                                              (Set)
import qualified Data.Set                                              as Set
import           GHC.Generics                                          (Generic)
import qualified Prettyprinter                                         as PP
import           Reopt.TypeInference.Constraints.Solving.Constraints   (EqC (EqC),
                                                                        EqRowC (EqRowC),
                                                                        InRowC,
                                                                        OrC,
                                                                        RowShiftC (RowShiftC))
import           Reopt.TypeInference.Constraints.Solving.RowVariables  (RowVar (RowVar))
import           Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar (TyVar))
import           Reopt.TypeInference.Constraints.Solving.Types         (ITy (..),
                                                                        ITy',
                                                                        Offset (Offset),
                                                                        TyF (RecTy),
                                                                        prettyMap)

class Monad m => CanFreshRowVar m where
  freshRowVar :: m RowVar

data ConstraintSolvingState = ConstraintSolvingState
  { ctxEqCs :: [EqC],
    ctxInRowCs :: [InRowC],
    ctxEqRowCs :: [EqRowC],
    -- | Known row offset relationships, i.e. @r -> (o,r')@
    --   says @r@ with offset @o@ applied corresponds to
    -- row @r'@. .
    ctxRowShiftMap :: Map RowVar (Set (Offset, RowVar)),
    
    nextTraceId :: Int,
    nextRowVar :: Int,
    nextTyVar  :: Int,
    
    -- | The union-find data-structure mapping each tyvar onto its
    -- representative tv.  If no mapping exists, it is a self-mapping.

    -- FIXME (perf): replace by a mutable array of unboxed ints, or at
    -- least IntMap
    ctxTyVarEqv :: Map TyVar TyVar,
    -- | Known type for type variables.  Note that a variable is never
    -- defined as another variable, that is what ctxTyVarEqv is for.
    -- Invariant: every root in @ctxTyVarEqv@ has a mapping here, and
    -- just those roots.
    ctxTyVarDefs :: Map TyVar ITy'
  }
  deriving (Eq, Generic, Ord, Show)

emptyContext :: Int -> ConstraintSolvingState
emptyContext nextRow = ConstraintSolvingState
  { ctxEqCs    = []
  , ctxInRowCs = []
  , ctxRowShiftCs = []
  , ctxEqRowCs    = []
  , ctxRowShiftMap = mempty
  , nextTraceId    = 0
  , nextRowVar     = nextRow
  , nextTyVar      = 0
  , ctxTyVarEqv    = mempty
  , ctxTyVarDefs   = mempty
  }

newtype ConstraintSolvingMonad a = ConstraintSolvingMonad
  { getConstraintSolvingMonad :: State ConstraintSolvingState a
  }
  deriving (Applicative, Functor, Monad, MonadState ConstraintSolvingState)

instance CanFreshRowVar ConstraintSolvingMonad where
  freshRowVar = ConstraintSolvingMonad $ RowVar <$> (field @"nextRowVar" <<+= 1)

runConstraintSolvingMonad :: ConstraintSolvingState -> ConstraintSolvingMonad () -> ConstraintSolvingState
runConstraintSolvingMonad s = flip execState s . getConstraintSolvingMonad

--------------------------------------------------------------------------------
-- Adding constraints

addTyVarEq :: TyVar -> TyVar -> ConstraintSolvingMonad ()
addTyVarEq tv1 tv2 = field @"ctxEqCs" %= (EqC tv1 tv2 :)

addRowVarEq :: RowVar -> Map Offset TyVar -> RowVar -> ConstraintSolvingMonad ()
addRowVarEq lhs offs rhs = field @"ctxEqRowCs" %= (EqRowC lhs offs rhs :)

--------------------------------------------------------------------------------
-- Getting constraints

popField :: Lens' ConstraintSolvingState [a] -> ConstraintSolvingMonad (Maybe a)
popField fld =
  fld %%= \case
    [] -> (Nothing, [])
    (c : cs) -> (Just c, cs)

dequeueEqC :: ConstraintSolvingMonad (Maybe EqC)
dequeueEqC = popField (field @"ctxEqCs")

dequeueInRowC :: ConstraintSolvingMonad (Maybe InRowC)
dequeueInRowC = popField (field @"ctxInRowCs")

dequeueRowShiftC :: ConstraintSolvingMonad (Maybe RowShiftC)
dequeueRowShiftC = popField (field @"ctxRowShiftCs")

dequeueEqRowC :: ConstraintSolvingMonad (Maybe EqRowC)
dequeueEqRowC = popField (field @"ctxEqRowCs")

--------------------------------------------------------------------------------
-- Operations over type variable state

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class.  This also updates the eqv. map to
-- amortise lookups.

lookupTyVarRep :: TyVar -> ConstraintSolvingMonad TyVar
lookupTyVarRep tv0 = field @"ctxTyVarEqv" %%= go tv0
  where
    go :: TyVar -> Map TyVar TyVar -> (TyVar, Map TyVar TyVar)
    go tv m =
      case Map.lookup tv m of
        Nothing  -> (tv, m)
        Just tv' ->
          let (tv'', m') = go tv' m
          in (tv'', Map.insert tv tv'' m') -- short circuit next lookup.

-- | Lookup a type variable, returns the representative of the
-- corresponding equivalence class, and the definition for that type
-- var, if any.

lookupTyVar :: TyVar -> ConstraintSolvingMonad (TyVar, Maybe ITy')
lookupTyVar tv = do
  tv' <- lookupTyVarRep tv
  def <- uses (field @"ctxTyVarDefs") (Map.lookup tv')
  pure (tv', def)

-- | Always return a new type variable.
freshTyVar' :: Maybe String -> ConstraintSolvingMonad TyVar
freshTyVar' orig = flip TyVar orig <$> (field @"nextTyVar" <<+= 1)
  
freshTyVar :: Maybe String -> Maybe ITy -> ConstraintSolvingMonad TyVar
freshTyVar orig Nothing = freshTyVar' orig
freshTyVar _orig (Just (VarTy v)) = pure v -- Don't allocate, just return the equiv. var.
freshTyVar orig  (Just (ITy ty)) = do
  tyv <- freshTyVar' orig
  defineTyVar tyv ty
  pure tyv

-- | Always define a type variable, even if it has a def.
defineTyVar :: TyVar -> ITy' -> ConstraintSolvingMonad ()
defineTyVar tyv ty = field @"ctxTyVarDefs" %= Map.insert tyv ty

undefineTyVar :: TyVar -> ConstraintSolvingMonad ()
undefineTyVar ty = field @"ctxTyVarDefs" %= Map.delete ty

-- | @unsafeUnifyTyVars root leaf@ will make @root@ the new equiv. rep
-- for @leaf@.  Note that both root and leaf should be the reps. of
-- their corresponding equivalence classes. 
unsafeUnifyTyVars :: TyVar -> TyVar -> ConstraintSolvingMonad ()
unsafeUnifyTyVars root leaf = field @"ctxTyVarEqv" %= Map.insert leaf root

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
--   ConstraintSolvingMonad (RowShiftC, Maybe EqC)
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
  pretty _ctx = "FIXME"
    -- let row title entries = title PP.<+> PP.list entries
    --  in PP.vsep
    --       [ row "EqCs" $ map PP.pretty $ ctxEqCs ctx,
    --         row "InRowCs" $ map PP.pretty $ ctxInRowCs ctx,
    --         row "RowShiftCs" $ map PP.pretty $ ctxRowShiftCs ctx,
    --         row "OrCs" $ map PP.pretty $ ctxOrCs ctx,
    --         row "EqRowCs" $ map PP.pretty $ ctxEqRowCs ctx,
    --         row "Absurd EqCs" $ map PP.pretty $ ctxAbsurdEqCs ctx,
    --         row "Occurs check failures" $ map PP.pretty $ ctxOccursCheckFailures ctx,
    --         row "Type Var Map" $ prettyMap PP.pretty PP.pretty $ ctxTyVarMap ctx,
    --         row "Row Shift Map" $
    --           let prettySMap (r, sSet) = map (\(o, r') -> PP.pretty (RowShiftC r o r')) $ Set.toList sSet
    --            in concatMap prettySMap $ Map.toList $ ctxRowShiftMap ctx,
    --         row "TyVar Equivalences" $
    --           let pp (ty, ty') = PP.pretty ty PP.<+> "=" PP.<+> PP.pretty ty'
    --           in map pp $ Map.toList $ ctxTyVarEqv ctx
    --       ]

shiftOffsets :: Offset -> Map Offset ITy -> Map Offset ITy
shiftOffsets o =
  Map.fromList . map (first (+ o)) . Map.toList
