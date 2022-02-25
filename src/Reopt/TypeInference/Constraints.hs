{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Constraints where

import Control.Lens (Lens', over, set, use, (<<+=))
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.State (MonadState (get, put), State, StateT (StateT), execState, gets, modify)
import Data.Bifunctor (Bifunctor (second), first)
import Data.Either (partitionEithers)
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import qualified Prettyprinter as PP
import qualified Text.LLVM as L
import Data.Maybe (catMaybes)

-- | Set to @True@ to enable tracing in unification
traceUnification :: Bool
traceUnification = True

prettySExp :: [PP.Doc ann] -> PP.Doc ann
prettySExp docs = PP.group $ PP.encloseSep "(" ")" " " docs

data TyVar = TyVar
  { tyVarInt :: Int,
    -- | If you want to record the origin of this type variable, it will show
    -- out when you pretty-print it.  Recommended, except in the test suite
    -- where there is not much point in marking test type variables.
    tyVarOrigin :: Maybe String
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty TyVar where
  pretty tyv = PP.hcat ["α", PP.pretty (tyVarInt tyv), maybeOrigin (tyVarOrigin tyv)]
    where
      maybeOrigin Nothing = mempty
      maybeOrigin (Just origin) = PP.space <> PP.parens (PP.pretty origin)

class FreeTyVars a where
  freeTyVars :: a -> Set TyVar

newtype RowVar = RowVar {rowVarInt :: Int}
  deriving (Eq, Ord, Show)

instance PP.Pretty RowVar where
  pretty (RowVar n) = "ρ" <> PP.pretty n

class FreeRowVars a where
  freeRowVars :: a -> Set RowVar

-- | Byte offset.
newtype Offset = Offset {getOffset :: Natural}
  deriving (Eq, Ord, Show)
  deriving (Num) via Natural

instance PP.Pretty Offset where
  pretty (Offset n) = PP.pretty n

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

-- | Type used during inference (i.e., can have free type/row variables)
type ITy = Ty TyVar RowVar

-- | Used to denote a type could not be resolved to a known type.
data Unknown = Unknown
  deriving (Eq, Ord, Show)

instance PP.Pretty Unknown where
  pretty _ = "?"

unknownTy :: Ty Unknown rvar
unknownTy = UnknownTy Unknown

-- | Used to denote a record type has no row variable.
data NoRow = NoRow
  deriving (Eq, Ord, Show)

instance PP.Pretty NoRow where
  pretty _ = "∅"

-- | Final types resulting from inference (i.e., no free type variables).
type FTy = Ty Unknown NoRow

instance FreeTyVars (Ty TyVar rvar) where
  freeTyVars = \case
    UnknownTy x -> Set.singleton x
    NumTy _ -> Set.empty
    PtrTy t -> freeTyVars t
    RecTy flds _row -> foldr (Set.union . freeTyVars) Set.empty flds

instance FreeRowVars (Ty tvar RowVar) where
  freeRowVars = \case
    UnknownTy _ -> Set.empty
    NumTy _ -> Set.empty
    PtrTy t -> freeRowVars t
    RecTy flds row -> foldr (Set.union . freeRowVars) (Set.singleton row) flds

recTyByteWidth :: Int -> [(Offset, FTy)] -> Integer
recTyByteWidth ptrSz = offsetAfterLast . last
  where
    offsetAfterLast (Offset o, ty) = fromIntegral o + tyByteWidth ptrSz ty

tyByteWidth :: Int -> FTy -> Integer
tyByteWidth _ (NumTy n) = fromIntegral n `div` 8
tyByteWidth ptrSz (PtrTy _) = fromIntegral ptrSz `div` 8
tyByteWidth ptrSz (RecTy flds NoRow) = recTyByteWidth ptrSz (Map.assocs flds)
tyByteWidth ptrSz (UnknownTy Unknown) = fromIntegral ptrSz `div` 8

bumpOffsetBy :: Int -> Offset -> Offset
bumpOffsetBy n (Offset o)
  | n < 0 = error "bumpOffsetBy negative number not allowed"
  | otherwise = Offset (o + fromIntegral n)

recTyToLLVMType :: Int -> [(Offset, FTy)] -> L.Type
recTyToLLVMType ptrSz [(Offset 0, ty)] = tyToLLVMType ptrSz ty
recTyToLLVMType ptrSz fields = L.Struct (go 0 fields)
  where
    go :: Natural -> [(Offset, FTy)] -> [L.Type]
    go _ [] = []
    go nextOffset flds@((Offset o, ty) : rest)
      | o == nextOffset = tyToLLVMType ptrSz ty : go (o + fromIntegral (tyByteWidth ptrSz ty)) rest
      | otherwise = L.PrimType (L.Integer (8 * (fromIntegral o - fromIntegral nextOffset))) : go o flds

tyToLLVMType :: Int -> FTy -> L.Type
tyToLLVMType _ (NumTy n) = L.PrimType (L.Integer (fromIntegral n))
tyToLLVMType ptrSz (PtrTy typ) = L.PtrTo (tyToLLVMType ptrSz typ)
tyToLLVMType ptrSz (RecTy flds NoRow) = recTyToLLVMType ptrSz (Map.assocs flds)
tyToLLVMType ptrSz (UnknownTy Unknown) = L.PrimType (L.Integer (fromIntegral ptrSz))

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

iRecTy :: [(Natural, ITy)] -> RowVar -> ITy
iRecTy flds = RecTy (Map.fromList (map (first Offset) flds))

fRecTy :: [(Natural, FTy)] -> FTy
fRecTy flds = RecTy (Map.fromList (map (first Offset) flds)) NoRow

-- | @EqC t1 t2@ means @t1@ and @t2@ are literally the same type.
data EqC = EqC {eqLhs :: ITy, eqRhs :: ITy}
  deriving (Eq, Ord, Show)

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

prettyMap :: (k -> PP.Doc d) -> (v -> PP.Doc d) -> Map k v -> [PP.Doc d]
prettyMap ppKey ppValue =
  PP.punctuate PP.comma . map prettyEntry . Map.toAscList
  where
    prettyEntry (k, v) = PP.group (PP.hsep [ppKey k, "→", ppValue v])

prettyRow :: PP.Pretty t => PP.Pretty r => Map Offset (Ty t r) -> RowVar -> PP.Doc d
prettyRow os r = PP.hsep ["{", PP.hsep (prettyMap PP.pretty PP.pretty os), "|", PP.pretty r, "}"]

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

class SubstRowVar a where
  substRowVar :: RowVar -> RowVar -> Map Offset ITy -> a -> a

instance SubstRowVar ITy where
  substRowVar r1 r2 os = \case
    UnknownTy v -> UnknownTy v
    NumTy sz -> NumTy sz
    PtrTy t -> PtrTy (substRowVar r1 r2 os t)
    RecTy flds rvar
      -- TODO: check for fields conflict
      | rvar == r1 -> RecTy (Map.union (substRowVar r1 r2 os <$> flds) os) r2
      | otherwise -> RecTy (substRowVar r1 r2 os <$> flds) rvar

instance SubstRowVar EqC where
  substRowVar r1 r2 os (EqC l r) = EqC (substRowVar r1 r2 os l) (substRowVar r1 r2 os r)

instance SubstRowVar InRowC where
  substRowVar r1 r2 os (InRowC r o t) =
    InRowC
      (if r == r1 && o `notElem` Map.keys os then r2 else r)
      o
      (substRowVar r1 r2 os t)

substRowVarInRowShiftC ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  RowShiftC ->
  ConstraintSolvingMonad (RowShiftC, Maybe EqC)
substRowVarInRowShiftC r1 r2 os (RowShiftC r3 o@(Offset n) r4)
  | r3 == r1 = do
    -- Here we want to replace r1 with {os | r2} in a constraint meaning:
    -- shift o r1 = r4   -->   shift o {os | r2} = r4
    -- Which means we want to introduce a fresh r such that:
    -- shift o {| r2} = r   and   r4 = {shift o os | r}
    r <- freshRowVar
    return
      ( RowShiftC r2 o r,
        Just (EqC (RecTy mempty r4) (RecTy (shiftStructuralInformationBy (fromIntegral n) os) r))
      )
  | r4 == r1 = do
    -- Here we want to replace r1 with {os | r2} in a constraint meaning:
    -- shift o r3 = r1   -->   shift o r3 {os | r2}
    -- Which means we want to introduce a fresh r such that:
    -- r3 = {shift (-o) os | r}   and   shift o {|r} = {|r2}
    r <- freshRowVar
    return
      ( RowShiftC r o r2,
        Just (EqC (RecTy mempty r3) (RecTy (shiftStructuralInformationBy (- (fromIntegral n)) os) r))
      )
  | otherwise = return (RowShiftC r3 o r4, Nothing)

-- instance SubstRowVar OrC where
--   substRowVar r1 r2 os (OrC cs) = OrC (substRowVar r1 r2 os <$> cs)

-- instance SubstRowVar AndC where
--   substRowVar r1 r2 os (AndC cs) = AndC (substRowVar r1 r2 os <$> cs)

substRowVarInTyConstraint ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  TyConstraint ->
  ConstraintSolvingMonad TyConstraint
substRowVarInTyConstraint r1 r2 os = \case
  EqTC c -> pure $ EqTC $ substRowVar r1 r2 os c
  InRowTC c -> pure $ InRowTC $ substRowVar r1 r2 os c
  RowShiftTC c -> do
    (rsC, meqC) <- substRowVarInRowShiftC r1 r2 os c
    return $ case meqC of
      Nothing -> RowShiftTC rsC
      Just eqC -> AndTC (AndC [RowShiftTC rsC, EqTC eqC])
  EqRowTC c -> pure $ either EqTC EqRowTC (substRowVarInEqRowC r1 r2 os c)
  OrTC orC -> OrTC <$> substRowVarInOrC r1 r2 os orC
  AndTC andC -> AndTC <$> substRowVarInAndC r1 r2 os andC

substRowVarInAndC ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  AndC ->
  ConstraintSolvingMonad AndC
substRowVarInAndC r1 r2 os (AndC cs) =
  AndC <$> mapM (substRowVarInTyConstraint r1 r2 os) cs

substRowVarInOrC ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  OrC ->
  ConstraintSolvingMonad OrC
substRowVarInOrC r1 r2 os (OrC cs) =
  OrC <$> mapM (substRowVarInTyConstraint r1 r2 os) cs

----------------------------------------------------------------------------------------
-- Type Operations

-- FIXME using an explicit substitution function is simple to reason about...
-- but if we're doing it _a lot_ that could be slow, so we might want to switch
-- to using an explicit substitution data structure that is kept in the context
-- and used resolve things on an as needed basis.
class SubstTyVar a where
  substTyVar :: (TyVar, ITy) -> a -> a

instance SubstTyVar ITy where
  substTyVar xt@(x, xTy) = \case
    UnknownTy y -> if x == y then xTy else UnknownTy y
    NumTy sz -> NumTy sz
    PtrTy t -> PtrTy $ substTyVar xt t
    RecTy flds rvar -> RecTy (fmap (substTyVar xt) flds) rvar

instance SubstTyVar EqC where
  substTyVar xt (EqC l r) = EqC (substTyVar xt l) (substTyVar xt r)

instance SubstTyVar InRowC where
  substTyVar xt (InRowC r o t) = InRowC r o (substTyVar xt t)

instance SubstTyVar RowShiftC where
  substTyVar _ c@RowShiftC {} = c

instance SubstTyVar AndC where
  substTyVar xt (AndC cs) = AndC $ map (substTyVar xt) cs

instance SubstTyVar OrC where
  substTyVar xt (OrC cs) = OrC $ map (substTyVar xt) cs

instance SubstTyVar TyConstraint where
  substTyVar xt = \case
    EqTC c -> EqTC $ substTyVar xt c
    InRowTC c -> InRowTC $ substTyVar xt c
    RowShiftTC c -> RowShiftTC $ substTyVar xt c
    OrTC c -> OrTC $ substTyVar xt c
    AndTC c -> AndTC $ substTyVar xt c
    EqRowTC c -> EqRowTC c -- no TyVars in Rows

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

-- | @decomposeEqC t1 t2@ returns constraints implied from @EqP t1 t2@.
decomposeEqC :: EqC -> ConstraintSolvingMonad [TyConstraint]
decomposeEqC (EqC lhs rhs) = go lhs rhs
  where
    go :: ITy -> ITy -> ConstraintSolvingMonad [TyConstraint]
    go UnknownTy {} _ = pure []
    go _ UnknownTy {} = pure []
    go (PtrTy t1) (PtrTy t2) = pure [eqTC t1 t2]
    go (RecTy fs1 r1) (RecTy fs2 r2)
      | Map.null fs1 && Map.null fs2 = pure [eqRowTC r1 mempty r2]
      | Map.null fs1 = pure [eqRowTC r1 fs2 r2]
      | Map.null fs2 = pure [eqRowTC r2 fs1 r1]
      | otherwise = do
        let -- shared fields must be equal
            fldPs =
              map (uncurry eqTC) $
                Map.elems $
                  Map.intersectionWith (,) fs1 fs2
            -- fields from fs2 not in fs1 must appear in r1
            r1Ps =
              map (uncurry (inRowTC r1)) $
                Map.toList $ Map.difference fs2 fs1
            -- fields from fs1 not in fs2 must appear in r2
            r2Ps =
              map (uncurry (inRowTC r2)) $
                Map.toList $ Map.difference fs1 fs2
        -- We need a fresh row variable for the "remainder" of the row variables
        -- on each side when you factor out the fields mentioned across.
        --
        -- E.g. when you start with RecTy { 0 : i8 } r1 = RecTy { 8 : i8 } r2, you
        -- should conclude that there exists a r3 s. t.
        --
        -- r1 = { 8 : i8 | r3 }   and   r2 = { 0 : i8 | r3 }
        r3 <- freshRowVar
        let eqr1 = eqTC (RecTy mempty r1) (RecTy (Map.difference fs2 fs1) r3)
        let eqr2 = eqTC (RecTy mempty r2) (RecTy (Map.difference fs1 fs2) r3)
        pure $ eqr1 : eqr2 : fldPs ++ r1Ps ++ r2Ps
    go (RecTy fs r) t = pure $ goRecNonRec fs r t
    go t (RecTy fs r) = pure $ goRecNonRec fs r t
    go _ _ = pure []
    -- Handle the case when a record and a non-record are equal,
    -- i.e., treat the non-record as a record with one field `0`.
    goRecNonRec :: Map Offset ITy -> RowVar -> ITy -> [TyConstraint]
    goRecNonRec flds row nonRecTy = case Map.lookup (Offset 0) flds of
      Nothing -> [inRowTC row (Offset 0) nonRecTy]
      Just ty0 -> [eqTC nonRecTy ty0]

instance PP.Pretty ConstraintSolvingState where
  pretty ctx =
    let row title entries = title PP.<+> PP.list entries
     in PP.vsep
          [ row "EqCs" $ map PP.pretty $ ctxEqCs ctx,
            row "InRowCs" $ map PP.pretty $ ctxInRowCs ctx,
            row "RowShiftCs" $ map PP.pretty $ ctxRowShiftCs ctx,
            row "OrCs" $ map PP.pretty $ ctxOrCs ctx,
            row "EqRowCs" $ map PP.pretty $ ctxEqRowCs ctx,
            row "Absurd EqCs" $ map PP.pretty $ ctxAbsurdEqCs ctx,
            row "Occurs check failures" $ map PP.pretty $ ctxOccursCheckFailures ctx,
            row "Type Var Map" $
              map (\(x, xTy) -> prettySExp [PP.pretty x, "↦", PP.pretty xTy]) $
                Map.toList $ ctxTyVarMap ctx,
            row "Row Shift Map" $
              let prettySMap (r, sSet) = map (\(o, r') -> PP.pretty (RowShiftC r o r')) $ Set.toList sSet
               in concatMap prettySMap $ Map.toList $ ctxRowShiftMap ctx
          ]

instance SubstTyVar ConstraintSolvingState where
  -- N.B., we only substitute in unprocessed constraints
  substTyVar xt ctx =
    ConstraintSolvingState
      { ctxEqCs = fmap (substTyVar xt) (ctxEqCs ctx),
        ctxInRowCs = fmap (substTyVar xt) (ctxInRowCs ctx),
        ctxRowShiftCs = fmap (substTyVar xt) (ctxRowShiftCs ctx),
        ctxOrCs = fmap (substTyVar xt) (ctxOrCs ctx),
        ctxEqRowCs = ctxEqRowCs ctx, -- no TyVars in EqRowCs
        ctxTyVarMap = fmap (substTyVar xt) (ctxTyVarMap ctx),
        -- Not modified because we only substitute in unprocessed constraints:
        ctxAbsurdEqCs = ctxAbsurdEqCs ctx,
        ctxOccursCheckFailures = ctxOccursCheckFailures ctx,
        ctxRowShiftMap = ctxRowShiftMap ctx,
        nextTraceId = nextTraceId ctx,
        nextRowVar = nextRowVar ctx
      }

-- Until we had a row-shift term, substituting a row in an EqRowC can not
-- necessarily be represented as another EqRowC.
substRowVarInEqRowC :: RowVar -> RowVar -> Map Offset ITy -> EqRowC -> Either EqC EqRowC
-- substRowVarInEqRowC :: RowVar -> RowVar -> Map Offset ITy -> EqRowC -> (EqRowC, [InRowC])
substRowVarInEqRowC r1 r2 os (EqRowC r3 os' r4)
  -- In this case, we intend to replace r1 with {os | r2} in a constraint of the form:
  -- r1 = {os' | r4}
  -- This ought to mean the constraint:
  -- {os | r2} = {os' | r4}
  -- Now os and os' may overlap some.  For those offsets that overlap, we should
  -- check for conflicts, and otherwise omit them entirely from the resulting
  -- constraints.
  -- The remaining constraint being:
  -- {unique_os | r2} = {unique_os' | r4}
  -- which we can represent either as an EqC, or as a combination of an EqRowC
  -- and some InRowCs.
  -- Let's try an EqC for now as it makes the type simpler (no need for lists).
  | r3 == r1 =
    let uniqOs = Map.filterWithKey (\k _ -> k `notElem` Map.keys os') os
     in let uniqOs' = Map.filterWithKey (\k _ -> k `notElem` Map.keys os) os'
         in Left $ EqC (RecTy uniqOs r2) (RecTy uniqOs' r4)
  -- (EqRowC r2 os' r4, uncurry (InRowC r4) <$> filter ((`notElem` Map.keys os') . fst) (Map.assocs os))
  -- TODO: check for conflicts?
  | r4 == r1 = Right $ EqRowC r3 (Map.union os os') r4
  -- It should never be the case that both r3 and r4 are equal to r1, so this
  -- should only trigger when both are **not** r1.
  | otherwise = Right $ EqRowC r3 os' r4

-- TODO: check that this is used correctly!
replaceRowVar :: RowVar -> RowVar -> RowVar -> RowVar
replaceRowVar r1 r2 r = if r == r1 then r2 else r

substRowVarInConstraintSolvingState ::
  RowVar ->
  RowVar ->
  Map Offset ITy ->
  ConstraintSolvingState ->
  ConstraintSolvingMonad ConstraintSolvingState
substRowVarInConstraintSolvingState r1 r2 os ctx = do
  (rowShiftCs, eqCsFromRowShiftCs) <-
    second catMaybes
    . unzip
    <$> mapM (substRowVarInRowShiftC r1 r2 os) (ctxRowShiftCs ctx)
  let (eqCsFromEqRowCs, newEqRowCs) = partitionEithers $ substRowVarInEqRowC r1 r2 os <$> ctxEqRowCs ctx
  orCs <- mapM (substRowVarInOrC r1 r2 os) (ctxOrCs ctx)
  return $ ConstraintSolvingState
    { ctxEqCs = (substRowVar r1 r2 os <$> ctxEqCs ctx) ++ eqCsFromEqRowCs ++ eqCsFromRowShiftCs,
      ctxInRowCs = substRowVar r1 r2 os <$> ctxInRowCs ctx,
      ctxRowShiftCs = rowShiftCs, -- substRowVarInRowShiftC r1 r2 os <$> ctxRowShiftCs ctx,
      ctxOrCs = orCs,
      ctxEqRowCs = newEqRowCs,
      ctxTyVarMap = substRowVar r1 r2 os <$> ctxTyVarMap ctx,
      ctxRowShiftMap = unifyRowVarMap r1 r2 (Set.map (second (replaceRowVar r1 r2))) (unifyRowShiftMap r1 r2) (ctxRowShiftMap ctx),
      -- Not modified because we only substitute in unprocessed constraints:
      ctxAbsurdEqCs = ctxAbsurdEqCs ctx,
      ctxOccursCheckFailures = ctxOccursCheckFailures ctx,
      nextTraceId = nextTraceId ctx,
      nextRowVar = nextRowVar ctx
    }

unifyRowShiftMap ::
  RowVar ->
  RowVar ->
  Set (Offset, RowVar) ->
  Set (Offset, RowVar) ->
  Set (Offset, RowVar)
unifyRowShiftMap r1 r2 s1 s2 = Set.map (second (replaceRowVar r1 r2)) (Set.union s1 s2)

unifyRowVarMap :: RowVar -> RowVar -> (v -> v) -> (v -> v -> v) -> Map RowVar v -> Map RowVar v
unifyRowVarMap r1 r2 unify union m = unify <$> Map.delete r1 (Map.alter insertOrAlter r2 m)
  where
    insertOrAlter Nothing = Map.lookup r1 m
    insertOrAlter (Just v) = Just (maybe v (union v) (Map.lookup r1 m))

-- | @traceContext description ctx ctx'@ reports how the context changed via @trace@.
traceContext :: PP.Doc () -> ConstraintSolvingMonad () -> ConstraintSolvingMonad ()
traceContext description action = do
  tId <- field @"nextTraceId" <<+= 1
  when traceUnification $ do
    stateBefore <- get
    let msg =
          PP.vsep
            [ PP.hsep [">>>", PP.parens (PP.pretty tId), description],
              PP.indent 4 $ PP.pretty stateBefore
            ]
    trace (show msg) (pure ())
  action
  when traceUnification $ do
    stateAfter <- get
    let msg =
          PP.vsep
            [ PP.hsep ["<<< ", PP.parens (PP.pretty tId), description],
              PP.indent 4 $ PP.pretty stateAfter
            ]
    trace (show msg) (return ())

-- | Does the context have any equality or subtype constraints left to process?
hasAtomicConstraints :: ConstraintSolvingState -> Bool
hasAtomicConstraints ctx =
  not
    ( and
        [ null $ ctxEqCs ctx,
          null $ ctxInRowCs ctx,
          null $ ctxRowShiftCs ctx,
          null $ ctxEqRowCs ctx
        ]
    )

popField :: Lens' ConstraintSolvingState [a] -> ConstraintSolvingMonad (Maybe a)
popField fld =
  use fld >>= \case
    [] -> return Nothing
    (c : cs) -> do
      modify $ set fld cs
      return (Just c)

dequeueEqC :: ConstraintSolvingMonad (Maybe EqC)
dequeueEqC = popField (field @"ctxEqCs")

dequeueInRowC :: ConstraintSolvingMonad (Maybe InRowC)
dequeueInRowC = popField (field @"ctxInRowCs")

dequeueRowShiftC :: ConstraintSolvingMonad (Maybe RowShiftC)
dequeueRowShiftC = popField (field @"ctxRowShiftCs")

dequeueEqRowC :: ConstraintSolvingMonad (Maybe EqRowC)
dequeueEqRowC = popField (field @"ctxEqRowCs")

addConstraints :: [TyConstraint] -> ConstraintSolvingMonad ()
addConstraints = mapM_ go
  where
    go :: TyConstraint -> ConstraintSolvingMonad ()
    go (EqTC c) = modify $ over (field @"ctxEqCs") (c :)
    go (InRowTC c) = modify $ over (field @"ctxInRowCs") (c :)
    go (RowShiftTC c) = modify $ over (field @"ctxRowShiftCs") (c :)
    go (OrTC c) = modify $ over (field @"ctxOrCs") (c :)
    go (AndTC (AndC cs)) = addConstraints cs
    go (EqRowTC c) = modify $ over (field @"ctxEqRowCs") (c :)

emptyContext :: Int -> ConstraintSolvingState
emptyContext = ConstraintSolvingState [] [] [] [] [] [] [] Map.empty Map.empty 0

-- | Is an equality constraint absurd?
absurdEqC :: EqC -> Bool
absurdEqC (EqC l r) = inconsistent l r

-- | Is an equality constraint trivial?
trivialEqC :: EqC -> Bool
trivialEqC (EqC l r) = l == r

-- | Updates the context with the given equality. This action is "atomic" in the
-- sense that it may generate some additional implied constraints, but it does
-- not also solve those as well. It handles exactly one constraint.
solveEqC :: EqC -> ConstraintSolvingMonad ()
solveEqC c
  | trivialEqC c = pure ()
  | absurdEqC c = modify $ over (field @"ctxAbsurdEqCs") (c :)
  | otherwise = do
    addConstraints =<< decomposeEqC c
    case (eqLhs c, eqRhs c) of
      (UnknownTy x, t) -> solveVarEq x t
      (t, UnknownTy x) -> solveVarEq x t
      (_, _) -> pure ()
  where
    solveVarEq :: TyVar -> ITy -> ConstraintSolvingMonad ()
    solveVarEq x t =
      if Set.member x (freeTyVars t)
        then -- if we fail the occurs check, remember that for debugging and move on
          modify $ over (field @"ctxOccursCheckFailures") (c :)
        else -- Otherwise, record x -> t in the type map
        -- and perform that substutition everywhere else.
        do
          modify $ over (field @"ctxTyVarMap") (Map.insert x t)
          modify $ substTyVar (x, t)

-- | Updates the context with an @InRowC@ constraint. Like @solveEqC@ this is an
-- "atomic" update.
solveInRowC :: InRowC -> ConstraintSolvingMonad ()
solveInRowC (InRowC r o t) = do
  r' <- freshRowVar
  addConstraints [eqRowTC r (Map.singleton o t) r']

-- | @shiftFields o m@ adds offset @o@ to the offsets in @m@.
shiftFields :: Offset -> Map Offset ITy -> Map Offset ITy
shiftFields n = Map.mapKeys (+ n)

-- | Updates the context with a @RowShiftC@ constraint. Like @solveEqC@ this is
-- an "atomic" update.
solveRowShiftC :: RowShiftC -> ConstraintSolvingMonad ()
solveRowShiftC (RowShiftC r1 n r2) =
  modify $ over (field @"ctxRowShiftMap") (Map.alter alter r1)
  where
    alter Nothing = Just (Set.singleton (n, r2))
    alter (Just s) = Just (Set.union s (Set.singleton (n, r2)))

-- | TODO
solveEqRowC :: EqRowC -> ConstraintSolvingMonad ()
solveEqRowC (EqRowC r1 os r2) = do
  -- TODO (val) There's gotta be a combinator for doing this?
  s <- get
  s' <- substRowVarInConstraintSolvingState r1 r2 os s
  put s'

-- | Process all atomic (i.e., non-disjunctive) constraints, updating the
-- context with each.
processAtomicConstraints :: ConstraintSolvingMonad ()
processAtomicConstraints = traceContext "processAtomicConstraints" $ do
  dequeueEqC >>= \case
    Just c -> solveEqC c >> processAtomicConstraints
    Nothing ->
      dequeueInRowC >>= \case
        Just c -> solveInRowC c >> processAtomicConstraints
        Nothing ->
          dequeueRowShiftC >>= \case
            Just c -> solveRowShiftC c >> processAtomicConstraints
            Nothing ->
              dequeueEqRowC >>= \case
                Just c -> solveEqRowC c >> processAtomicConstraints
                Nothing -> return ()

-- | Reduce/simplify a constraint in a given context.
reduceC :: TyConstraint -> ConstraintSolvingMonad TyConstraint
reduceC tc =
  case tc of
    EqTC c
      | absurdEqC c -> return absurdTC
      | trivialEqC c -> return trivialTC
      | otherwise -> return tc
    AndTC (AndC cs) -> andTC <$> mapM reduceC cs
    OrTC (OrC cs) -> orTC <$> mapM reduceC cs
    EqRowTC {} -> return tc
    InRowTC {} -> return tc
    RowShiftTC {} -> return tc

-- | Attempt to reduce and eliminate disjunctions in the given context. Any
-- resulting non-disjuncts are added to their respective field in the context.
reduceDisjuncts :: ConstraintSolvingMonad ()
reduceDisjuncts = traceContext "reduceDisjuncts" $ do
  disjs <- gets ctxOrCs
  modify $ set (field @"ctxOrCs") []
  mapM_ elimOr disjs
  where
    elimOr :: OrC -> ConstraintSolvingMonad ()
    elimOr (OrC ds) = addConstraints . (: []) . orTC =<< mapM reduceC ds

-- | Reduce a context by processing its atomic constraints and then attempting
-- to reduce disjucts to atomic constraints. Halts once no more atomic constraints
-- exist or can be inferred from the disjunctions.
reduceContext :: ConstraintSolvingMonad ()
reduceContext = traceContext "reduceContext" $ do
  processAtomicConstraints
  reduceDisjuncts
  whenM (gets hasAtomicConstraints) reduceContext

-- ctx2 <- if hasAtomicConstraints ctx1 then reduceContext ctx1 else pure ctx1

-- | Remove any remaining @TyVar@s, replacing them with @UnknownTy ()@.
removeTyVars :: ITy -> Ty Unknown RowVar
removeTyVars = \case
  UnknownTy _ -> unknownTy
  NumTy sz -> NumTy sz
  PtrTy ty -> PtrTy $ removeTyVars ty
  RecTy flds rv -> RecTy (fmap removeTyVars flds) rv

-- | Remove any remaining @RowVar@s.
removeRowVars :: Ty Unknown RowVar -> FTy
removeRowVars = \case
  UnknownTy _ -> unknownTy
  NumTy sz -> NumTy sz
  PtrTy ty -> PtrTy $ removeRowVars ty
  RecTy flds _ -> RecTy (fmap removeRowVars flds) NoRow

class Monad m => CanFreshRowVar m where
  freshRowVar :: m RowVar

-- | Substitute out a row variable in a record type for the corresponding offset
-- mappings.  If this results in a single offset with conflicting types, replace
-- the type with an @Unknown@.
-- substFinalRowVar :: Map RowVar (Map Offset FTy) -> Ty Unknown RowVar -> FTy
-- substFinalRowVar rowMaps = \case
--   UnknownTy {} -> unknownTy
--   NumTy sz -> NumTy sz
--   PtrTy t -> PtrTy $ substFinalRowVar rowMaps t
--   RecTy flds r ->
--     let flds' = fmap (substFinalRowVar rowMaps) flds
--      in case Map.lookup r rowMaps of
--           Nothing -> RecTy flds' NoRow
--           Just rMap ->
--             let combineTypes t1 t2 = if t1 == t2 then t1 else unknownTy
--                 mergedFlds = Map.unionWith combineTypes flds' rMap
--              in RecTy mergedFlds NoRow
finalizeRowVar :: Ty Unknown RowVar -> FTy
finalizeRowVar = \case
  UnknownTy {} -> unknownTy
  NumTy sz -> NumTy sz
  PtrTy t -> PtrTy (finalizeRowVar t)
  RecTy flds _ -> RecTy (finalizeRowVar <$> flds) NoRow

shiftStructuralInformationBy :: Integer -> Map Offset ITy -> Map Offset ITy
shiftStructuralInformationBy o =
  Map.fromList . concatMap retainPositiveOffsets . Map.toList
  where
    retainPositiveOffsets (Offset a, ty) =
      let newOffset = fromIntegral a + o
       in [(Offset (fromIntegral newOffset), ty) | newOffset >= 0]

expandInRowMapWithOneShift :: RowVar -> Map RowVar (Map Offset ITy) -> (Offset, RowVar) -> Map RowVar (Map Offset ITy)
expandInRowMapWithOneShift r1 inRowMap (Offset o, r2) =
  inRowMap
    -- This is a little weird, we can probably do something smarter eventually
    `Map.union` maybe Map.empty (Map.singleton r2 . shiftStructuralInformationBy (fromIntegral o)) (Map.lookup r1 inRowMap)
    `Map.union` maybe Map.empty (Map.singleton r1 . shiftStructuralInformationBy (- fromIntegral o)) (Map.lookup r2 inRowMap)

expandInRowMapWithSetOfShifts ::
  Map RowVar (Map Offset ITy) ->
  RowVar ->
  Set (Offset, RowVar) ->
  Map RowVar (Map Offset ITy)
expandInRowMapWithSetOfShifts inRowMap r1 =
  Set.foldl (expandInRowMapWithOneShift r1) inRowMap

expandInRowMapWithShiftMap ::
  Map RowVar (Map Offset ITy) ->
  Map RowVar (Set (Offset, RowVar)) ->
  Map RowVar (Map Offset ITy)
expandInRowMapWithShiftMap = Map.foldlWithKey expandInRowMapWithSetOfShifts

data ConstraintSolvingState = ConstraintSolvingState
  { ctxEqCs :: [EqC],
    ctxInRowCs :: [InRowC],
    ctxRowShiftCs :: [RowShiftC],
    ctxOrCs :: [OrC],
    ctxEqRowCs :: [EqRowC],
    -- | Equality constraints that were dropped due to being absurd.
    ctxAbsurdEqCs :: [EqC],
    -- | Occurs check failures.
    ctxOccursCheckFailures :: [EqC],
    -- | Known type for type variables.
    ctxTyVarMap :: Map TyVar ITy,
    -- | Known row offset relationships, i.e. @r -> (o,r')@
    --   says @r@ with offset @o@ applied corresponds to
    -- row @r'@. .
    ctxRowShiftMap :: Map RowVar (Set (Offset, RowVar)),
    nextTraceId :: Int,
    nextRowVar :: Int
  }
  deriving (Eq, Generic, Ord, Show)

newtype ConstraintSolvingMonad a = ConstraintSolvingMonad
  { getConstraintSolvingMonad :: State ConstraintSolvingState a
  }
  deriving (Applicative, Functor, Monad, MonadState ConstraintSolvingState)

instance CanFreshRowVar ConstraintSolvingMonad where
  freshRowVar = ConstraintSolvingMonad $ RowVar <$> (field @"nextRowVar" <<+= 1)

runConstraintSolvingMonad :: ConstraintSolvingState -> ConstraintSolvingMonad () -> ConstraintSolvingState
runConstraintSolvingMonad s = flip execState s . getConstraintSolvingMonad

-- | Unify the given constraints, returning a conservative type map for all type
-- variables.
unifyConstraints :: Int -> [TyConstraint] -> Map TyVar FTy
unifyConstraints nextRV initialConstraints =
  let fvs = foldr (Set.union . freeTyVars) Set.empty initialConstraints
      finalCtx = runConstraintSolvingMonad (emptyContext nextRV) (addConstraints initialConstraints >> reduceContext)
      -- finalInRowMap = expandInRowMapWithShiftMap (ctxInRowMap finalCtx) (ctxRowShiftMap finalCtx)
      -- finalRowMap = fmap (removeRowVars . removeTyVars) <$> finalInRowMap
      -- finalTyMap = fmap (substFinalRowVar finalRowMap . removeTyVars) (ctxTyVarMap finalCtx)
      finalTyMap = fmap (finalizeRowVar . removeTyVars) (ctxTyVarMap finalCtx)
      unknownTyMap = Map.fromSet (const unknownTy) fvs
   in Map.union finalTyMap unknownTyMap
