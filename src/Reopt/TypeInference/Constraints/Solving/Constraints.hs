{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Reopt.TypeInference.Constraints.Solving.Constraints where

import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Prettyprinter as PP
import Reopt.TypeInference.Constraints.Solving.RowVariables (RowExpr, Offset, RowVar)
import Reopt.TypeInference.Constraints.Solving.Types
  ( FreeRowVars (..),
    FreeTyVars (..),
    prettyRow,
  )
import Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar)

-- | @EqC t1 t2@ means @t1@ and @t2@ are literally the same type.
data EqC = EqC {eqLhs :: TyVar, eqRhs :: TyVar}
  deriving (Eq, Ord, Show)

prettySExp :: [PP.Doc ann] -> PP.Doc ann
prettySExp docs = PP.group $ PP.encloseSep "(" ")" " " docs

instance PP.Pretty EqC where
  pretty (EqC l r) = prettySExp [PP.pretty l, "=", PP.pretty r]

instance FreeTyVars EqC where
  freeTyVars (EqC t1 t2) = Set.union (freeTyVars t1) (freeTyVars t2)

instance FreeRowVars EqC where
  freeRowVars (EqC t1 t2) = Set.union (freeRowVars t1) (freeRowVars t2)

-- | Stands for: lhs = { offsets | rhs }
data EqRowC = EqRowC
  { eqRowLHS :: RowVar,
    eqRowOffsets :: Map Offset TyVar,
    eqRowRHS :: RowExpr
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty EqRowC where
  pretty (EqRowC r1 os r2) = prettySExp [PP.pretty r1, "=", prettyRow os r2]

instance FreeTyVars EqRowC where
  freeTyVars (EqRowC _ os _) = foldMap Set.singleton os

instance FreeRowVars EqRowC where
  freeRowVars (EqRowC r1 os r2) =
    Set.singleton r1
      `Set.union` foldr (Set.union . freeRowVars) Set.empty os
      `Set.union` freeRowVars r2

-- | @InRowC o t r@ means in row @r@ offset @o@ must contain a @t@.
data InRowC = InRowC
  { inRowRowExpr :: RowExpr,
    inRowOffset :: Offset,
    inRowTypeAtOffset :: TyVar
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty InRowC where
  pretty (InRowC o t r) = prettySExp [PP.pretty o, ":", PP.pretty t, "∈", PP.pretty r]

instance FreeTyVars InRowC where
  freeTyVars (InRowC _ _ t) = freeTyVars t

instance FreeRowVars InRowC where
  freeRowVars (InRowC r _ t) = freeRowVars r `Set.union` freeRowVars t

-- eqRowTC :: RowVar -> Map Offset ITy -> RowVar -> TyConstraint
-- eqRowTC r1 os r2 = EqRowTC $ EqRowC r1 os r2

-- isNumTC :: Int -> ITy -> TyConstraint
-- isNumTC sz t = EqTC (EqC t (ITy $ NumTy sz))

-- isPtrTC :: ITy -> ITy -> TyConstraint
-- isPtrTC pointer pointee = EqTC (EqC pointer (ITy $ PtrTy pointee))

-- isOffsetTC :: ITy -> Natural -> ITy -> RowVar -> TyConstraint
-- isOffsetTC base offset typ row =
--   EqTC (EqC base (PtrTy (RecTy (Map.singleton (Offset offset) typ) row)))

-- isPointerWithOffsetTC :: (ITy, RowVar) -> (ITy, RowVar) -> Offset -> TyConstraint
-- isPointerWithOffsetTC (base, baseRow) (result, resultRow) offset =
--   andTC
--     [ eqTC base (PtrTy (RecTy Map.empty baseRow)),
--       eqTC result (PtrTy (RecTy Map.empty resultRow)),
--       -- Make no mistake, here, since we have:
--       -- result = base + offset
--       -- Then in terms of rows, it's more like:
--       -- resultRow + offset = base
--       -- e.g.
--       -- { 0 : T } + 16 = { 16 : T }
--       RowShiftTC (RowShiftC resultRow offset baseRow)
--     ]
