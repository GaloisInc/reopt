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

import           Reopt.TypeInference.Solver.RowVariables  (NoRow (NoRow),
                                                           Offset,
                                                           RowExpr (RowExprShift, RowExprVar),
                                                           RowVar)
import           Reopt.TypeInference.Solver.TypeVariables (TyVar)


data TyF rvar f
  = -- | A scalar numeric value (i.e., a signed/unsigned integer, but _not_ a pointer).
    NumTy Int
  | -- | A pointer to a value.
    PtrTy f
  | -- | Record type, mapping byte offsets to types and with a row variable
    -- for describing constraints on the existence/type of additional fields.
    RecTy (Map Offset f) rvar
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (PP.Pretty f, PP.Pretty rv) => PP.Pretty (TyF rv f) where
  pretty = \case
    NumTy sz -> "i" <> PP.pretty sz
    PtrTy t -> "ptr" <> PP.parens (PP.pretty t)
    RecTy flds row ->
      PP.group $
        PP.braces $
          PP.cat $
            (++ ["|" PP.<> PP.pretty row]) $
              PP.punctuate (PP.comma <> PP.space) $
                map (\(off, t) -> PP.pretty off PP.<+> ":" PP.<+> PP.pretty t) $
                  Map.toAscList flds

class FreeTyVars a where
  freeTyVars :: a -> Set TyVar

class FreeRowVars a where
  freeRowVars :: a -> Set RowVar

instance FreeTyVars f => FreeTyVars (TyF rvar f) where
  freeTyVars = foldMap freeTyVars

instance FreeRowVars RowExpr where
  freeRowVars (RowExprVar v) = Set.singleton v
  freeRowVars (RowExprShift _ v) = Set.singleton v

instance FreeRowVars f => FreeRowVars (TyF RowExpr f) where
  freeRowVars = \case
    NumTy _ -> Set.empty
    PtrTy t -> freeRowVars t
    RecTy flds row -> foldr (Set.union . freeRowVars) (freeRowVars row) flds

-- | An unrolled ITy
type ITy' = TyF RowExpr TyVar

-- | Type used during inference (i.e., can have free type/row variables)
data ITy
  = VarTy TyVar
  -- | Only TyVars are allowed in recursive positions.
  | ITy ITy'
  deriving (Eq, Ord, Show)

instance PP.Pretty ITy where
  pretty = \case
    VarTy v -> PP.pretty v
    ITy ty  -> PP.pretty ty

instance FreeTyVars TyVar where
  freeTyVars = Set.singleton

instance FreeRowVars TyVar where
  freeRowVars _ = Set.empty

instance FreeTyVars ITy where
  freeTyVars = \case
    VarTy v  -> Set.singleton v
    ITy   ty -> freeTyVars ty

instance FreeRowVars ITy where
  freeRowVars = \case
    VarTy {}  -> Set.empty
    ITy   ty  -> freeRowVars ty

-- | Final types resulting from inference (i.e., no free type variables).
data FTy = UnknownTy | FTy (TyF NoRow FTy)
  deriving (Eq, Ord, Show)
  
instance PP.Pretty FTy where
  pretty = \case
    UnknownTy -> "?"
    FTy ty  -> PP.pretty ty

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

tyByteWidth :: Int -> FTy -> Integer
tyByteWidth ptrSz UnknownTy = fromIntegral ptrSz `div` 8
tyByteWidth ptrSz (FTy ty) =
  case ty of
    NumTy n -> fromIntegral n `div` 8
    PtrTy _ -> fromIntegral ptrSz `div` 8
    RecTy flds NoRow -> recTyByteWidth ptrSz (Map.assocs flds)

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
tyToLLVMType ptrSz (FTy ty) =
  case ty of
    NumTy n -> L.PrimType (L.Integer (fromIntegral n))
    PtrTy typ -> L.PtrTo (tyToLLVMType ptrSz typ)
    RecTy flds NoRow -> recTyToLLVMType ptrSz (Map.assocs flds)

-- iRecTy :: [(Natural, ITy)] -> RowVar -> ITy
-- iRecTy flds = ITy $ RecTy (Map.fromList (map (first Offset) flds))

-- fRecTy :: [(Natural, FTy)] -> FTy
-- fRecTy flds = FTy $ RecTy (Map.fromList (map (first Offset) flds)) NoRow

-- | Checks if two types are inconsistent. Note that @inconsistent t1 t2 = False@
-- does not imply @t1@ and @t2@ are equivalent. In particular, records need only
-- agree on common fields, type variables aren't necessarily inconsistent, etc.

-- inconsistent :: forall x r. TyF r f -> TyF x f -> Bool
-- inconsistent = check
--   where
--     check :: Ty x r -> Ty x r -> Bool
--     check type1 type2 =
--       case (type1, type2) of
--         (UnknownTy {}, _) -> False
--         (_, UnknownTy {}) -> False
--         (NumTy {}, NumTy {}) -> False
--         (PtrTy {}, NumTy {}) -> True
--         (NumTy {}, PtrTy {}) -> True
--         (PtrTy t1, PtrTy t2) -> check t1 t2
--         (RecTy fs1 _r1, RecTy fs2 _r2) -> checkCommonFields fs1 fs2
--         (RecTy fs1 _r1, t2) -> case Map.lookup (Offset 0) fs1 of
--           Nothing -> False
--           Just ty0 -> check ty0 t2
--         (t1, RecTy fs2 _r2) -> case Map.lookup (Offset 0) fs2 of
--           Nothing -> False
--           Just ty0 -> check t1 ty0
--     checkCommonFields :: Map Offset (Ty x r) -> Map Offset (Ty x r) -> Bool
--     checkCommonFields fs1 fs2 =
--       any
--         ( \(f, fTy) -> case Map.lookup f fs2 of
--             Nothing -> False
--             Just fTy' -> check fTy fTy'
--         )
--         $ Map.toList fs1
