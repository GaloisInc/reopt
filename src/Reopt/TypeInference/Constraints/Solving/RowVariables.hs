{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Constraints.Solving.RowVariables where

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
