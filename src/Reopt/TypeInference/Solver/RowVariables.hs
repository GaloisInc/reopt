{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}

module Reopt.TypeInference.Solver.RowVariables where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Prettyprinter   as PP

import Reopt.TypeInference.Solver.UnionFindMap  (UFMKeyInfo(..))

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
shiftRowExpr 0 re = re
shiftRowExpr s (RowExprVar v) = RowExprShift s v
shiftRowExpr s (RowExprShift s' v) = RowExprShift (s + s') v

rowExprShift :: RowExpr -> Offset
rowExprShift (RowExprVar _) = 0
rowExprShift (RowExprShift o _) = o

--------------------------------------------------------------------------------
-- Mappings

newtype FieldMap t = FieldMap { getFieldMap :: Map Offset t }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance PP.Pretty t => PP.Pretty (FieldMap t) where
  pretty (FieldMap m) = 
    PP.group $ PP.align $
      PP.encloseSep (PP.flatAlt "{ " "{") (PP.flatAlt " }" "}") mempty
        [ PP.encloseSep mempty mempty ", " $
            map (\(off, t) -> PP.pretty off PP.<+> ":" PP.<+> PP.pretty t) $
            Map.toAscList m
        ]

emptyFieldMap :: FieldMap t
emptyFieldMap = FieldMap mempty

shiftFieldMap :: Offset -> FieldMap t -> FieldMap t
shiftFieldMap off m = FieldMap $ Map.mapKeysMonotonic (+ off) (getFieldMap m)

dropFieldMap :: Offset -> FieldMap t -> FieldMap t
dropFieldMap off (FieldMap m) = FieldMap $
  Map.filterWithKey (\off' _ -> off' >= 0) $
  Map.mapKeysMonotonic (\off' -> off' - off) m

unifyFieldMaps :: FieldMap t -> FieldMap t -> (FieldMap t, [(t, t)])
unifyFieldMaps (FieldMap m1) (FieldMap m2) =
  ( FieldMap $ Map.union m1 m2
  , Map.elems (Map.intersectionWith (,) m1 m2))

singletonFieldMap :: Offset -> t -> FieldMap t
singletonFieldMap off v = FieldMap $ Map.singleton off v

fieldMapFromList :: [(Offset, t)] -> FieldMap t
fieldMapFromList = FieldMap . Map.fromList

--------------------------------------------------------------------------------
-- Union find helpers

data RowInfo = RowInfo
  { riRowVar :: RowVar
  , riShift  :: Offset
  }
  deriving (Eq, Ord, Show)

instance UFMKeyInfo RowVar RowInfo where
  compact old new = new { riShift = riShift old + riShift new }
  projectKey     = riRowVar
  injectKey k    = RowInfo { riRowVar = k, riShift = 0 }
  invertKey k ki = ki { riRowVar = k }

instance PP.Pretty RowInfo where
  pretty (RowInfo rv off)
    | off == 0  = PP.pretty rv
    | otherwise = PP.pretty rv <> " + " <> PP.pretty off
    
