{-# LANGUAGE OverloadedStrings #-}

module Reopt.TypeInference.Solver.Types where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter qualified as PP
import Text.LLVM qualified as L

import Reopt.TypeInference.Solver.RowVariables (
  FieldMap (..),
  Offset,
  RowExpr (RowExprShift, RowExprVar),
  RowVar,
  rowVarInt,
 )
import Reopt.TypeInference.Solver.TypeVariables (TyVar)

-- FIXME: f will be used when we add e.g. tuples/vectors
data TyF rvar f
  = -- | A scalar numeric value (i.e., a signed/unsigned integer, but _not_ a pointer).
    NumTy Int
  | -- | A pointer to a value.
    PtrTy rvar
  | -- | A type which is used differently in different contexts
    ConflictTy Int
  | -- | A n-ary tuple
    TupleTy [f]
  | -- | A vector
    VecTy Int f
  | -- | An unknown function pointer type
    UnknownFunPtrTy
  | -- | A known function pointer type
    FunPtrTy [f] f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | An unrolled ITy
type ITy' = TyF RowExpr TyVar

-- | Type used during inference (i.e., can have free type/row variables)
data ITy
  = VarTy TyVar
  | -- | Only TyVars are allowed in recursive positions.
    ITy ITy'
  deriving (Eq, Ord, Show)

-- | Final types resulting from inference (i.e., no free type variables).
type StructName = String

data FTy
  = UnknownTy
  | NamedStruct StructName
  | StructTy (FieldMap FTy)
  | FTy (TyF FTy FTy)
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

-- | This should only be called on types which can occur within a RecTy, i.e.,
-- not records.
tyByteWidth :: Int -> FTy -> Integer
tyByteWidth ptrSz UnknownTy = fromIntegral ptrSz `div` 8
tyByteWidth _ptrSz StructTy{} = error "Saw a StructTy in tyByteWidth"
tyByteWidth _ptrSz (NamedStruct n) = error ("Saw a named struct in tyByteWidth " ++ n)
tyByteWidth ptrSz (FTy ty) =
  case ty of
    NumTy n -> fromIntegral n `div` 8
    PtrTy _ -> fromIntegral ptrSz `div` 8
    UnknownFunPtrTy -> fromIntegral ptrSz `div` 8
    FunPtrTy{} -> fromIntegral ptrSz `div` 8
    ConflictTy n -> fromIntegral n `div` 8
    TupleTy{} -> error "Saw a TupleTy in tyByteWidth"
    VecTy{} -> error "Saw a VecTy in tyByteWidth"

recTyToLLVMType :: Int -> [(Offset, FTy)] -> L.Type
-- This breaks recursive types.
-- recTyToLLVMType ptrSz [(0, ty)] = tyToLLVMType ptrSz ty
recTyToLLVMType ptrSz fields = L.Struct (go 0 fields)
 where
  go :: Offset -> [(Offset, FTy)] -> [L.Type]
  go _ [] = []
  go nextOffset flds@((o, ty) : rest)
    | o == nextOffset = tyToLLVMType ptrSz ty : go (o + fromIntegral (tyByteWidth ptrSz ty)) rest
    | otherwise =
        let pad = L.Vector (fromIntegral o - fromIntegral nextOffset) (L.PrimType (L.Integer 8))
         in pad : go o flds

-- c.f. typeToLLVMType
tyToLLVMType :: Int -> FTy -> L.Type
tyToLLVMType ptrSz = go
 where
  go :: FTy -> L.Type
  go UnknownTy = L.PrimType (L.Integer (fromIntegral ptrSz))
  go (NamedStruct s) = L.Alias (L.Ident s)
  go (StructTy fm) = recTyToLLVMType ptrSz (Map.assocs (getFieldMap fm))
  go (FTy ty) =
    case ty of
      NumTy n -> L.PrimType (L.Integer (fromIntegral n))
      PtrTy ty' -> L.PtrTo $ tyToLLVMType ptrSz ty'
      UnknownFunPtrTy -> L.PtrTo L.Opaque
      FunPtrTy args ret -> L.PtrTo $ L.FunTy (go ret) (map go args) False
      ConflictTy n -> L.PrimType (L.Integer (fromIntegral n))
      TupleTy ts -> L.Struct (map go ts)
      VecTy n ty' -> L.Vector (fromIntegral n) (go ty')

--------------------------------------------------------------------------------
-- Instances

-- Pretty

instance PP.Pretty ITy where
  pretty = \case
    VarTy v -> PP.pretty v
    ITy ty -> PP.pretty ty

instance (PP.Pretty f, PP.Pretty rv) => PP.Pretty (TyF rv f) where
  pretty = \case
    NumTy sz -> "i" <> PP.pretty sz
    PtrTy t -> "ptr " <> PP.pretty t
    UnknownFunPtrTy -> "? (???)*"
    FunPtrTy args ret -> PP.pretty ret <> " (" <> PP.hcat (PP.punctuate PP.comma (map PP.pretty args)) <> ")*"
    ConflictTy n -> "![" <> PP.pretty n <> "]"
    TupleTy ts -> PP.tupled (map PP.pretty ts)
    VecTy n ty -> "< " <> PP.pretty n <> " x " <> PP.pretty ty <> " >"

instance PP.Pretty FTy where
  pretty = \case
    UnknownTy -> "?"
    NamedStruct n -> PP.pretty n
    StructTy tm -> PP.pretty tm
    FTy ty -> PP.pretty ty

-- FreeTyVars

class FreeTyVars a where
  freeTyVars :: a -> Set TyVar

instance FreeTyVars TyVar where
  freeTyVars = Set.singleton

instance FreeTyVars RowVar where
  freeTyVars _ = Set.empty

instance (FreeTyVars rvar, FreeTyVars f) => FreeTyVars (TyF rvar f) where
  freeTyVars = \case
    NumTy _ -> Set.empty
    PtrTy t -> freeTyVars t
    UnknownFunPtrTy -> Set.empty
    FunPtrTy args ret -> freeTyVars ret `Set.union` Set.unions (map freeTyVars args)
    ConflictTy{} -> Set.empty
    TupleTy ts -> foldMap freeTyVars ts
    VecTy _ ty -> freeTyVars ty

instance FreeTyVars ITy where
  freeTyVars = \case
    VarTy v -> Set.singleton v
    ITy ty -> freeTyVars ty

instance FreeTyVars t => FreeTyVars (FieldMap t) where
  freeTyVars = foldMap freeTyVars

instance FreeTyVars RowExpr where
  freeTyVars _ = Set.empty

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
    UnknownFunPtrTy -> Set.empty
    FunPtrTy args ret -> freeRowVars ret `Set.union` Set.unions (map freeRowVars args)
    ConflictTy{} -> Set.empty
    TupleTy ts -> foldMap freeRowVars ts
    VecTy _ ty -> freeRowVars ty

instance FreeRowVars TyVar where
  freeRowVars _ = Set.empty

instance FreeRowVars ITy where
  freeRowVars = \case
    VarTy{} -> Set.empty
    ITy ty -> freeRowVars ty

instance FreeRowVars t => FreeRowVars (FieldMap t) where
  freeRowVars = foldMap freeRowVars
