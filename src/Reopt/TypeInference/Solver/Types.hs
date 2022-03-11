{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}

module Reopt.TypeInference.Solver.Types where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Prettyprinter   as PP
import qualified Text.LLVM                                as L

import           Reopt.TypeInference.Solver.RowVariables  (FieldMap (..),
                                                           Offset,
                                                           RowExpr (RowExprShift, RowExprVar),
                                                           RowVar, FieldMap, rowVarInt)
import           Reopt.TypeInference.Solver.TypeVariables (TyVar)

-- FIXME: f will be used when we add e.g. tuples/vectors
data TyF rvar f
  = -- | A scalar numeric value (i.e., a signed/unsigned integer, but _not_ a pointer).
    NumTy Int
  | -- | A pointer to a value.
    PtrTy rvar
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | An unrolled ITy
type ITy' = TyF RowExpr TyVar

-- | Type used during inference (i.e., can have free type/row variables)
data ITy
  = VarTy TyVar
  -- | Only TyVars are allowed in recursive positions.
  | ITy ITy'
  deriving (Eq, Ord, Show)

-- | Final types resulting from inference (i.e., no free type variables).

type StructName = String

data FTy =
  UnknownTy
  | NamedStruct StructName
  | FTy (TyF (FieldMap FTy) FTy)
  deriving (Eq, Ord, Show)

rowVarToStructName :: RowVar -> StructName
rowVarToStructName tv = "struct.reopt.t" ++ show (rowVarInt tv)

prettyMap :: (k -> PP.Doc d) -> (v -> PP.Doc d) -> Map k v -> [PP.Doc d]
prettyMap ppKey ppValue =
  PP.punctuate PP.comma . map prettyEntry . Map.toAscList
  where
    prettyEntry (k, v) = PP.group (PP.hsep [ppKey k, "→", ppValue v])

prettyRow :: PP.Pretty v => Map Offset v -> RowExpr -> PP.Doc d
prettyRow os r = PP.hsep ["{", PP.hsep (prettyMap PP.pretty PP.pretty os), "|", PP.pretty r, "}"]

recTyByteWidth :: Int -> [(Offset, FTy)] -> Integer
recTyByteWidth ptrSz = offsetAfterLast . last
  where
    offsetAfterLast (o, ty) = fromIntegral o + tyByteWidth ptrSz ty

-- | This shoold only be called on types which can occur within a
-- RecTy, i.e., not records.
tyByteWidth :: Int -> FTy -> Integer
tyByteWidth ptrSz UnknownTy = fromIntegral ptrSz `div` 8
tyByteWidth _ptrSz NamedStruct {} = error "Saw a named struct in tyByteWidth"
tyByteWidth ptrSz (FTy ty) =
  case ty of
    NumTy n -> fromIntegral n `div` 8
    PtrTy _ -> fromIntegral ptrSz `div` 8

recTyToLLVMType :: Int -> [(Offset, FTy)] -> L.Type
recTyToLLVMType ptrSz [(0, ty)] = tyToLLVMType ptrSz ty
recTyToLLVMType ptrSz fields = L.Struct (go 0 fields)
  where
    go :: Offset -> [(Offset, FTy)] -> [L.Type]
    go _ [] = []
    go nextOffset flds@((o, ty) : rest)
      | o == nextOffset = tyToLLVMType ptrSz ty : go (o + fromIntegral (tyByteWidth ptrSz ty)) rest
      | otherwise = L.PrimType (L.Integer (8 * (fromIntegral o - fromIntegral nextOffset))) : go o flds

tyToLLVMType :: Int -> FTy -> L.Type
tyToLLVMType ptrSz UnknownTy =
  L.PrimType (L.Integer (fromIntegral ptrSz))
tyToLLVMType _ptrSz (NamedStruct s) = L.Alias (L.Ident s)
tyToLLVMType ptrSz (FTy ty) =
  case ty of
    NumTy n -> L.PrimType (L.Integer (fromIntegral n))
    PtrTy flds -> L.PtrTo $ recTyToLLVMType ptrSz (Map.assocs (getFieldMap flds))

--------------------------------------------------------------------------------
-- Instances

-- Pretty

instance PP.Pretty ITy where
  pretty = \case
    VarTy v -> PP.pretty v
    ITy ty  -> PP.pretty ty

instance (PP.Pretty f, PP.Pretty rv) => PP.Pretty (TyF rv f) where
  pretty = \case
    NumTy sz -> "i" <> PP.pretty sz
    PtrTy t -> "ptr " <> PP.pretty t


instance PP.Pretty FTy where
  pretty = \case
    UnknownTy -> "?"
    NamedStruct n -> PP.pretty n
    FTy ty  -> PP.pretty ty

-- FreeTyVars

class FreeTyVars a where
  freeTyVars :: a -> Set TyVar

instance FreeTyVars TyVar where
  freeTyVars = Set.singleton

instance FreeTyVars f => FreeTyVars (TyF rvar f) where
  freeTyVars = foldMap freeTyVars

instance FreeTyVars ITy where
  freeTyVars = \case
    VarTy v  -> Set.singleton v
    ITy   ty -> freeTyVars ty

instance FreeTyVars t => FreeTyVars (FieldMap t) where
  freeTyVars = foldMap freeTyVars

-- FreeRowVars

class FreeRowVars a where
  freeRowVars :: a -> Set RowVar

instance FreeRowVars RowVar where
  freeRowVars = Set.singleton

instance FreeRowVars RowExpr where
  freeRowVars (RowExprVar v) = Set.singleton v
  freeRowVars (RowExprShift _ v) = Set.singleton v

instance (FreeRowVars r, FreeRowVars f) => FreeRowVars (TyF r f) where
  freeRowVars = \case
    NumTy _ -> Set.empty
    PtrTy t -> freeRowVars t

instance FreeRowVars TyVar where
  freeRowVars _ = Set.empty

instance FreeRowVars ITy where
  freeRowVars = \case
    VarTy {}  -> Set.empty
    ITy   ty  -> freeRowVars ty

instance FreeRowVars t => FreeRowVars (FieldMap t) where
  freeRowVars = foldMap freeRowVars
