{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Reopt.TypeInference.Constraints.Solving.Types where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Prettyprinter as PP
import Reopt.TypeInference.Constraints.Solving.RowVariables
  ( NoRow (NoRow),
    Offset (Offset),
    RowExpr (RowExprShift, RowExprVar),
    RowVar,
    rowVar,
  )
import Reopt.TypeInference.Constraints.Solving.TypeVariables
  ( TyVar,
  )
import qualified Text.LLVM as L

data Ty tvar rvar
  = -- | An unknown type (e.g., type variable).
    UnknownTy tvar
  | -- | A scalar numeric value (i.e., a signed/unsigned integer, but _not_ a pointer).
    NumTy Int
  | -- | A pointer to a value.
    PtrTy (Ty tvar rvar)
  | -- | Record type, mapping byte offsets to types and with a row variable
    -- for describing constraints on the existence/type of additional fields.
    RecTy (Map Offset (Ty tvar rvar)) rvar
  deriving (Eq, Ord, Show)

instance (PP.Pretty tv, PP.Pretty rv) => PP.Pretty (Ty tv rv) where
  pretty = \case
    UnknownTy x -> PP.pretty x
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

-- | Type used during inference (i.e., can have free type/row variables)
type ITy = Ty TyVar RowExpr

-- | Used to denote a type could not be resolved to a known type.
data Unknown = Unknown
  deriving (Eq, Ord, Show)

instance PP.Pretty Unknown where
  pretty _ = "?"

unknownTy :: Ty Unknown rvar
unknownTy = UnknownTy Unknown

class FreeTyVars a where
  freeTyVars :: a -> Set TyVar

class FreeRowVars a where
  freeRowVars :: a -> Set RowVar

instance FreeRowVars RowExpr where
  freeRowVars (RowExprVar v) = Set.singleton v
  freeRowVars (RowExprShift _ v) = Set.singleton v

-- | Final types resulting from inference (i.e., no free type variables).
type FTy = Ty Unknown NoRow

instance FreeTyVars (Ty TyVar rvar) where
  freeTyVars = \case
    UnknownTy x -> Set.singleton x
    NumTy _ -> Set.empty
    PtrTy t -> freeTyVars t
    RecTy flds _row -> foldr (Set.union . freeTyVars) Set.empty flds

instance FreeRowVars (Ty tvar RowExpr) where
  freeRowVars = \case
    UnknownTy _ -> Set.empty
    NumTy _ -> Set.empty
    PtrTy t -> freeRowVars t
    RecTy flds row -> foldr (Set.union . freeRowVars) (freeRowVars row) flds

prettyMap :: (k -> PP.Doc d) -> (v -> PP.Doc d) -> Map k v -> [PP.Doc d]
prettyMap ppKey ppValue =
  PP.punctuate PP.comma . map prettyEntry . Map.toAscList
  where
    prettyEntry (k, v) = PP.group (PP.hsep [ppKey k, "→", ppValue v])

prettyRow ::
  PP.Pretty t =>
  PP.Pretty r =>
  Map Offset (Ty t r) ->
  RowExpr ->
  PP.Doc d
prettyRow os r = PP.hsep ["{", PP.hsep (prettyMap PP.pretty PP.pretty os), "|", PP.pretty r, "}"]

recTyByteWidth :: Int -> [(Offset, FTy)] -> Integer
recTyByteWidth ptrSz = offsetAfterLast . last
  where
    offsetAfterLast (o, ty) = fromIntegral o + tyByteWidth ptrSz ty

tyByteWidth :: Int -> FTy -> Integer
tyByteWidth _ (NumTy n) = fromIntegral n `div` 8
tyByteWidth ptrSz (PtrTy _) = fromIntegral ptrSz `div` 8
tyByteWidth ptrSz (RecTy flds NoRow) = recTyByteWidth ptrSz (Map.assocs flds)
tyByteWidth ptrSz (UnknownTy Unknown) = fromIntegral ptrSz `div` 8

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
tyToLLVMType _ (NumTy n) = L.PrimType (L.Integer (fromIntegral n))
tyToLLVMType ptrSz (PtrTy typ) = L.PtrTo (tyToLLVMType ptrSz typ)
tyToLLVMType ptrSz (RecTy flds NoRow) = recTyToLLVMType ptrSz (Map.assocs flds)
tyToLLVMType ptrSz (UnknownTy Unknown) = L.PrimType (L.Integer (fromIntegral ptrSz))

iRecTy :: [(Integer, ITy)] -> RowVar -> ITy
iRecTy flds = RecTy (Map.fromList (map (first Offset) flds)) . rowVar

fRecTy :: [(Integer, FTy)] -> FTy
fRecTy flds = RecTy (Map.fromList (map (first Offset) flds)) NoRow

-- | Checks if two types are inconsistent. Note that @inconsistent t1 t2 = False@
-- does not imply @t1@ and @t2@ are equivalent. In particular, records need only
-- agree on common fields, type variables aren't necessarily inconsistent, etc.
inconsistent :: forall x r. Ty x r -> Ty x r -> Bool
inconsistent = check
  where
    check :: Ty x r -> Ty x r -> Bool
    check type1 type2 =
      case (type1, type2) of
        (UnknownTy {}, _) -> False
        (_, UnknownTy {}) -> False
        (NumTy {}, NumTy {}) -> False
        (PtrTy {}, NumTy {}) -> True
        (NumTy {}, PtrTy {}) -> True
        (PtrTy t1, PtrTy t2) -> check t1 t2
        (RecTy fs1 _r1, RecTy fs2 _r2) -> checkCommonFields fs1 fs2
        (RecTy fs1 _r1, t2) -> case Map.lookup (Offset 0) fs1 of
          Nothing -> False
          Just ty0 -> check ty0 t2
        (t1, RecTy fs2 _r2) -> case Map.lookup (Offset 0) fs2 of
          Nothing -> False
          Just ty0 -> check t1 ty0
    checkCommonFields :: Map Offset (Ty x r) -> Map Offset (Ty x r) -> Bool
    checkCommonFields fs1 fs2 =
      any
        ( \(f, fTy) -> case Map.lookup f fs2 of
            Nothing -> False
            Just fTy' -> check fTy fTy'
        )
        $ Map.toList fs1
