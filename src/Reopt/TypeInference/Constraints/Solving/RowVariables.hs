{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Solver.RowVariables where

import qualified Prettyprinter as PP

newtype RowVar = RowVar {rowVarInt :: Int}
  deriving (Eq, Ord, Show)

instance PP.Pretty RowVar where
  pretty (RowVar n) = "ρ" <> PP.pretty n

-- | Used to denote a record type has no row variable.
data NoRow = NoRow
  deriving (Eq, Ord, Show)

instance PP.Pretty NoRow where
  pretty _ = "∅"

-- | Byte offset.
newtype Offset = Offset {getOffset :: Integer}
  deriving (Eq, Ord, Show)
  deriving (Enum) via Integer
  deriving (Integral) via Integer
  deriving (Num) via Integer
  deriving (Real) via Integer

instance PP.Pretty Offset where
  pretty (Offset n) = PP.pretty n

data RowExpr
  = RowExprVar RowVar
  | RowExprShift Offset RowVar
  deriving (Eq, Ord, Show)

instance PP.Pretty RowExpr where
  pretty (RowExprVar v) = PP.pretty v
  pretty (RowExprShift o v) = PP.hsep ["shift", PP.pretty o, PP.pretty v]

rowVar :: RowVar -> RowExpr
rowVar = RowExprVar

rowExprVar :: RowExpr -> RowVar
rowExprVar (RowExprVar v) = v
rowExprVar (RowExprShift _ v) = v

replaceRowExprPreservingShift :: RowExpr -> RowExpr -> RowExpr
replaceRowExprPreservingShift (RowExprVar _) e = e
replaceRowExprPreservingShift (RowExprShift o _) e = shiftRowExpr o e

shiftRowExpr :: Offset -> RowExpr -> RowExpr
shiftRowExpr s (RowExprVar v) = RowExprShift s v
shiftRowExpr s (RowExprShift s' v) = RowExprShift (s + s') v

rowExprShift :: RowExpr -> Offset
rowExprShift (RowExprVar _) = 0
rowExprShift (RowExprShift o _) = o
