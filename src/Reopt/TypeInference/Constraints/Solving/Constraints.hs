{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Constraints.Solving.Constraints where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Prettyprinter as PP
import Reopt.TypeInference.Constraints.Solving.RowVariables (RowVar)
import Reopt.TypeInference.Constraints.Solving.Types
  ( FreeRowVars (..),
    FreeTyVars (..),
    ITy,
    Offset (Offset),
    Ty (..),
    prettyRow,
  )

-- | @EqC t1 t2@ means @t1@ and @t2@ are literally the same type.
data EqC = EqC {eqLhs :: ITy, eqRhs :: ITy}
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
    eqRowOffsets :: Map Offset ITy,
    eqRowRHS :: RowVar
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty EqRowC where
  pretty (EqRowC r1 os r2) = prettySExp [PP.pretty r1, "=", prettyRow os r2]

instance FreeTyVars EqRowC where
  freeTyVars (EqRowC _ os _) = foldr (Set.union . freeTyVars) Set.empty os

instance FreeRowVars EqRowC where
  freeRowVars (EqRowC r1 os r2) = Set.fromList [r1, r2] `Set.union` foldr (Set.union . freeRowVars) Set.empty os

-- | @InRowC o t r@ means in row @r@ offset @o@ must contain a @t@.
data InRowC = InRowC
  { inRowRowVar :: RowVar,
    inRowOffset :: Offset,
    inRowTypeAtOffset :: ITy
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty InRowC where
  pretty (InRowC o t r) = prettySExp [PP.pretty o, ":", PP.pretty t, "∈", PP.pretty r]

instance FreeTyVars InRowC where
  freeTyVars (InRowC _ _ t) = freeTyVars t

instance FreeRowVars InRowC where
  freeRowVars (InRowC r _ t) = Set.insert r (freeRowVars t)

-- | @RowShiftC r1 o r2@ means that @r1@ with offsets shifted by @o@ corresponds to @r2@.
-- I.e., if from other information we could derive @r1@ corresponded
-- to @{0:NumTy}@ and @o@ = 42 then @RowShiftC r1 o r2@ would imply that in @r2@
-- @{42:NumTy}@ holds.
data RowShiftC = RowShiftC
  { rowShiftBase :: RowVar,
    rowShiftBy :: Offset,
    rowShiftShifted :: RowVar
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty RowShiftC where
  pretty (RowShiftC r1 o r2) = prettySExp ["shift", PP.pretty r1, PP.pretty o, "=", PP.pretty r2]

instance FreeTyVars RowShiftC where
  freeTyVars RowShiftC {} = Set.empty

instance FreeRowVars RowShiftC where
  freeRowVars (RowShiftC r1 _ r2) = Set.fromList [r1, r2]

-- | Logical disjunction.
newtype OrC = OrC [TyConstraint]
  deriving (Eq, Ord, Show)

instance PP.Pretty OrC where
  pretty (OrC tcs) = PP.group ("(or" PP.<+> PP.hang 0 (PP.vsep (PP.pretty <$> tcs)) PP.<> ")")

instance FreeTyVars OrC where
  freeTyVars (OrC cs) = foldr (Set.union . freeTyVars) Set.empty cs

instance FreeRowVars OrC where
  freeRowVars (OrC cs) = foldr (Set.union . freeRowVars) Set.empty cs

-- | Logical conjunction.
newtype AndC = AndC [TyConstraint]
  deriving (Eq, Ord, Show)

instance PP.Pretty AndC where
  pretty (AndC tcs) = PP.group ("(and" PP.<+> PP.hang 0 (PP.vsep (PP.pretty <$> tcs)) PP.<> ")")

instance FreeTyVars AndC where
  freeTyVars (AndC cs) = foldr (Set.union . freeTyVars) Set.empty cs

instance FreeRowVars AndC where
  freeRowVars (AndC cs) = foldr (Set.union . freeRowVars) Set.empty cs

data TyConstraint
  = -- | @EqC t1 t2@ means @t1@ and @t2@ are literally the same type.
    EqTC EqC
  | -- | @InRowC o t r@ means in row @r@ offset @t@ must contain a @t@.
    InRowTC InRowC
  | -- | @RowOffsetC r1 o r2@ means that @r1@ with offsets shifted by @o@ corresponds to @r2@.
    RowShiftTC RowShiftC
  | EqRowTC EqRowC
  | -- | Logical disjunction. Use @orTC@ smart constructor instead to perform NEEDED simplifications.
    OrTC OrC
  | -- | Logical conjunction. Use @andTC@ smart constructor instead to perform NEEDED simplifications.
    AndTC AndC
  deriving (Eq, Ord, Show)

instance PP.Pretty TyConstraint where
  pretty = \case
    EqTC c -> PP.pretty c
    InRowTC c -> PP.pretty c
    RowShiftTC c -> PP.pretty c
    OrTC c -> PP.pretty c
    AndTC c -> PP.pretty c
    EqRowTC c -> PP.pretty c

instance FreeTyVars TyConstraint where
  freeTyVars = \case
    EqTC c -> freeTyVars c
    InRowTC c -> freeTyVars c
    RowShiftTC c -> freeTyVars c
    OrTC c -> freeTyVars c
    AndTC c -> freeTyVars c
    EqRowTC c -> freeTyVars c

instance FreeRowVars TyConstraint where
  freeRowVars = \case
    EqTC c -> freeRowVars c
    InRowTC c -> freeRowVars c
    RowShiftTC c -> freeRowVars c
    OrTC c -> freeRowVars c
    AndTC c -> freeRowVars c
    EqRowTC c -> freeRowVars c

-- | The trivial constraint.
trivialTC :: TyConstraint
trivialTC = AndTC $ AndC []

-- | The absurd constraint.
absurdTC :: TyConstraint
absurdTC = OrTC $ OrC []

eqTC :: ITy -> ITy -> TyConstraint
eqTC t1 t2 = EqTC $ EqC t1 t2

eqRowTC :: RowVar -> Map Offset ITy -> RowVar -> TyConstraint
eqRowTC r1 os r2 = EqRowTC $ EqRowC r1 os r2

inRowTC :: RowVar -> Offset -> ITy -> TyConstraint
inRowTC r o t = InRowTC $ InRowC r o t

-- | Disjunction smart constructor that performs NEEDED simplifications.
orTC :: [TyConstraint] -> TyConstraint
orTC = go Set.empty
  where
    go :: Set TyConstraint -> [TyConstraint] -> TyConstraint
    go acc [] = case Set.toList acc of
      [c] -> c
      cs -> OrTC $ OrC cs
    go _ (AndTC (AndC []) : _) = trivialTC
    go acc (OrTC (OrC cs) : cs') = go acc (cs ++ cs')
    go acc (c : cs) = go (Set.insert c acc) cs

-- | Conjunction smart constructor that performs NEEDED simplifications.
andTC :: [TyConstraint] -> TyConstraint
andTC = go Set.empty
  where
    go :: Set TyConstraint -> [TyConstraint] -> TyConstraint
    go acc [] = case Set.toList acc of
      [c] -> c
      cs -> AndTC $ AndC cs
    go _ (OrTC (OrC []) : _) = absurdTC
    go acc (AndTC (AndC cs) : cs') = go acc (cs ++ cs')
    go acc (c : cs) = go (Set.insert c acc) cs

isNumTC :: Int -> ITy -> TyConstraint
isNumTC sz t = EqTC (EqC t (NumTy sz))

isPtrTC :: ITy -> ITy -> TyConstraint
isPtrTC pointer pointee = EqTC (EqC pointer (PtrTy pointee))

isOffsetTC :: ITy -> Natural -> ITy -> RowVar -> TyConstraint
isOffsetTC base offset typ row =
  EqTC (EqC base (PtrTy (RecTy (Map.singleton (Offset offset) typ) row)))

isPointerWithOffsetTC :: (ITy, RowVar) -> (ITy, RowVar) -> Offset -> TyConstraint
isPointerWithOffsetTC (base, baseRow) (result, resultRow) offset =
  andTC
    [ eqTC base (PtrTy (RecTy Map.empty baseRow)),
      eqTC result (PtrTy (RecTy Map.empty resultRow)),
      -- Make no mistake, here, since we have:
      -- result = base + offset
      -- Then in terms of rows, it's more like:
      -- resultRow + offset = base
      -- e.g.
      -- { 0 : T } + 16 = { 16 : T }
      RowShiftTC (RowShiftC resultRow offset baseRow)
    ]
