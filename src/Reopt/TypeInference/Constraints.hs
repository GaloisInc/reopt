{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Reopt.TypeInference.Constraints where

import Control.Lens ( over )
import Data.Bifunctor (first, Bifunctor (second))
import Data.Generics.Product ( field )
import Data.Map.Strict (Map)
import Data.Set (Set)
import Debug.Trace ( trace )
import GHC.Generics ( Generic )
import Numeric.Natural (Natural)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Prettyprinter as PP
import qualified Text.LLVM as L

-- | Set to @True@ to enable tracing in unification
traceUnification :: Bool
traceUnification = False


prettySExp :: [PP.Doc ann] -> PP.Doc ann
prettySExp docs = PP.group $ PP.encloseSep "(" ")" " " docs

data TyVar = TyVar
  { tyVarInt :: Int
  , tyVarOrigin :: String
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty TyVar where
  pretty tyv = "α" <> PP.pretty (tyVarInt tyv) <> " (" <> PP.pretty (tyVarOrigin tyv) <> ")"

class FreeTyVars a where
  freeTyVars :: a -> Set TyVar

newtype RowVar = RowVar {rowVarInt :: Int }
  deriving (Eq, Ord, Show)

instance PP.Pretty RowVar where
  pretty (RowVar n) = "ρ" <> PP.pretty n

class FreeRowVars a where
  freeRowVars :: a -> Set RowVar

-- | Byte offset.
newtype Offset = Offset { getOffset :: Natural }
  deriving (Eq, Ord, Show)
  deriving Num via Natural


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
  deriving (Eq,Ord,Show)

instance PP.Pretty Unknown where
  pretty _ = "?"

unknownTy :: Ty Unknown rvar
unknownTy = UnknownTy Unknown

-- | Used to denote a record type has no row variable.
data NoRow = NoRow
  deriving (Eq,Ord,Show)

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
    RecTy flds row -> PP.group $ PP.braces $ PP.cat
                      $ (++ ["|" PP.<> PP.pretty row])
                      $ PP.punctuate (PP.comma <> PP.space)
                      $ map (\(off,t) -> PP.pretty off PP.<+> ":" PP.<+> PP.pretty t)
                      $ Map.toAscList flds

iRecTy :: [(Natural, ITy)] -> RowVar -> ITy
iRecTy flds = RecTy (Map.fromList (map (first Offset) flds))

fRecTy :: [(Natural, FTy)] -> FTy
fRecTy flds = RecTy (Map.fromList (map (first Offset) flds)) NoRow


-- | @EqC t1 t2@ means @t1@ and @t2@ are literally the same type.
data EqC = EqC {eqLhs :: ITy, eqRhs :: ITy}
  deriving (Eq, Ord, Show)

instance PP.Pretty EqC where
  pretty (EqC l r) = prettySExp [PP.pretty l,"=",PP.pretty r]
instance FreeTyVars EqC where
  freeTyVars (EqC t1 t2) = Set.union (freeTyVars t1) (freeTyVars t2)
instance FreeRowVars EqC where
  freeRowVars (EqC t1 t2) = Set.union (freeRowVars t1) (freeRowVars t2)


-- | TODO
data EqRowC = EqRowC { eqRowLHS :: RowVar, eqRowRHS :: RowVar }
  deriving (Eq, Ord, Show)

instance PP.Pretty EqRowC where
  pretty (EqRowC r1 r2) = prettySExp [PP.pretty r1, "=", PP.pretty r2]
instance FreeTyVars EqRowC where
  freeTyVars (EqRowC _ _) = Set.empty
instance FreeRowVars EqRowC where
  freeRowVars (EqRowC r1 r2) = Set.fromList [r1, r2]

-- | @InRowC o t r@ means in row @r@ offset @o@ must contain a @t@.
data InRowC = InRowC Offset ITy RowVar
  deriving (Eq, Ord, Show)

instance PP.Pretty InRowC where
  pretty (InRowC o t r) = prettySExp [PP.pretty o,":",PP.pretty t,"∈",PP.pretty r]
instance FreeTyVars InRowC where
  freeTyVars (InRowC _ t _) = freeTyVars t
instance FreeRowVars InRowC where
  freeRowVars (InRowC _ t r) = Set.insert r (freeRowVars t)

-- | @RowShiftC r1 o r2@ means that @r1@ with offsets shifted by @o@ corresponds to @r2@.
-- I.e., if from other information we could derive @r1@ corresponded
-- to @{0:NumTy}@ and @o@ = 42 then @RowShiftC r1 o r2@ would imply that in @r2@
-- @{42:NumTy}@ holds.
data RowShiftC = RowShiftC RowVar Offset RowVar
  deriving (Eq, Ord, Show)

instance PP.Pretty RowShiftC where
  pretty (RowShiftC r1 o r2) = prettySExp ["shift", PP.pretty r1,PP.pretty o,"=",PP.pretty r2]
instance FreeTyVars RowShiftC where
  freeTyVars RowShiftC{} = Set.empty
instance FreeRowVars RowShiftC where
  freeRowVars (RowShiftC r1 _ r2) = Set.fromList [r1,r2]

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

inRowTC :: Offset -> ITy -> RowVar -> TyConstraint
inRowTC o t r = InRowTC $ InRowC o t r

-- | Disjunction smart constructor that performs NEEDED simplifications.
orTC :: [TyConstraint] -> TyConstraint
orTC = go Set.empty
  where go :: Set TyConstraint -> [TyConstraint] -> TyConstraint
        go acc [] = case Set.toList acc of
                      [c] -> c
                      cs -> OrTC $ OrC cs
        go _   (AndTC (AndC []):_) = trivialTC
        go acc (OrTC (OrC cs):cs') = go acc (cs++cs')
        go acc (c:cs) = go (Set.insert c acc) cs

-- | Conjunction smart constructor that performs NEEDED simplifications.
andTC :: [TyConstraint] -> TyConstraint
andTC = go Set.empty
  where go :: Set TyConstraint -> [TyConstraint] -> TyConstraint
        go acc [] = case Set.toList acc of
                    [c] -> c
                    cs -> AndTC $ AndC cs
        go _   (OrTC (OrC []):_) = absurdTC
        go acc (AndTC (AndC cs):cs') = go acc (cs++cs')
        go acc (c:cs) = go (Set.insert c acc) cs

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
    [ eqTC base (PtrTy (RecTy Map.empty baseRow))
    , eqTC result (PtrTy (RecTy Map.empty resultRow))
    -- Make no mistake, here, since we have:
    -- result = base + offset
    -- Then in terms of rows, it's more like:
    -- resultRow + offset = base
    -- e.g.
    -- { 0 : T } + 16 = { 16 : T }
    , RowShiftTC (RowShiftC resultRow offset baseRow)
    ]


$(pure [])

class UnifyRowVars a where
  unifyRowVars :: RowVar -> RowVar -> a -> a

instance UnifyRowVars ITy where
  unifyRowVars r1 r2 = \case
    UnknownTy v -> UnknownTy v
    NumTy sz -> NumTy sz
    PtrTy t -> PtrTy (unifyRowVars r1 r2 t)
    RecTy flds rvar -> RecTy (unifyRowVars r1 r2 <$> flds) (unifyRowVars r1 r2 rvar)

instance UnifyRowVars RowVar where
  unifyRowVars r1 r2 r
    | r1 == r   = r2
    | otherwise = r

instance UnifyRowVars EqC where
  unifyRowVars r1 r2 (EqC l r) = EqC (unifyRowVars r1 r2 l) (unifyRowVars r1 r2 r)

instance UnifyRowVars InRowC where
  unifyRowVars r1 r2 (InRowC o t r) = InRowC o (unifyRowVars r1 r2 t) (unifyRowVars r1 r2 r)

instance UnifyRowVars RowShiftC where
  unifyRowVars r1 r2 (RowShiftC r3 o r4) = RowShiftC (unifyRowVars r1 r2 r3) o (unifyRowVars r1 r2 r4)

instance UnifyRowVars OrC where
  unifyRowVars r1 r2 (OrC cs) = OrC (unifyRowVars r1 r2 <$> cs)

instance UnifyRowVars AndC where
  unifyRowVars r1 r2 (AndC cs) = AndC (unifyRowVars r1 r2 <$> cs)

instance UnifyRowVars EqRowC where
  unifyRowVars r1 r2 (EqRowC r3 r4) = EqRowC (unifyRowVars r1 r2 r3) (unifyRowVars r1 r2 r4)

instance UnifyRowVars TyConstraint where
  unifyRowVars r1 r2 = \case
    EqTC c -> EqTC (unifyRowVars r1 r2 c)
    InRowTC c -> InRowTC (unifyRowVars r1 r2 c)
    RowShiftTC c -> RowShiftTC (unifyRowVars r1 r2 c)
    OrTC c -> OrTC (unifyRowVars r1 r2 c)
    AndTC c -> AndTC (unifyRowVars r1 r2 c)
    EqRowTC c -> EqRowTC (unifyRowVars r1 r2 c)

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
  substTyVar xt (InRowC o t r) = InRowC o (substTyVar xt t) r

instance SubstTyVar RowShiftC where
  substTyVar _ c@RowShiftC{} = c

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
        (UnknownTy{}, _) -> False
        (_, UnknownTy{}) -> False
        (NumTy{}, NumTy{}) -> False
        (PtrTy{}, NumTy{}) -> True
        (NumTy{}, PtrTy{}) -> True
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
      any (\(f,fTy) -> case Map.lookup f fs2 of
                         Nothing -> False
                         Just fTy' -> check fTy fTy')
       $ Map.toList fs1


-- | @decomposeEqC t1 t2@ returns constraints implied from @EqP t1 t2@.
decomposeEqC :: EqC -> [TyConstraint]
decomposeEqC (EqC lhs rhs) = go lhs rhs
  where
    go :: ITy -> ITy -> [TyConstraint]
    go UnknownTy{} _ = []
    go _ UnknownTy{} = []
    go (PtrTy t1) (PtrTy t2) = [eqTC t1 t2]
    go (RecTy fs1 r1) (RecTy fs2 r2) =
      let -- shared fields must be equal
          fldPs = map (uncurry eqTC)
                  $ Map.elems
                  $ Map.intersectionWith (,) fs1 fs2
          -- fields from fs2 not in fs1 must appear in r1
          r1Ps = map (\(o, tTy) -> inRowTC o tTy r1)
                  $ Map.toList $ Map.difference fs2 fs1
          -- fields from fs1 not in fs2 must appear in r2
          r2Ps = map (\(o, tTy) -> inRowTC o tTy r2)
                  $ Map.toList $ Map.difference fs1 fs2
      in fldPs ++ r1Ps ++ r2Ps ++ [EqRowTC (EqRowC r1 r2)]
    go (RecTy fs r) t = goRecNonRec fs r t
    go t (RecTy fs r) = goRecNonRec fs r t
    go _ _ = []
    -- Handle the case when a record and a non-record are equal,
    -- i.e., treat the non-record as a record with one field `0`.
    goRecNonRec :: Map Offset ITy -> RowVar -> ITy -> [TyConstraint]
    goRecNonRec flds row nonRecTy = case Map.lookup (Offset 0) flds of
      Nothing -> [inRowTC (Offset 0) nonRecTy row]
      Just ty0 -> [eqTC nonRecTy ty0]


-- | A context for unification.
data Context =
  Context
    { ctxEqCs :: [EqC],
      ctxInRowCs :: [InRowC],
      ctxRowShiftCs :: [RowShiftC],
      ctxOrCs  :: [OrC],
      ctxEqRowCs :: [EqRowC],
      -- | Equality constraints that were dropped due to being absurd.
      ctxAbsurdEqCs :: [EqC],
      -- | Occurs check failures.
      ctxOccursCheckFailures :: [EqC],
      -- | Known type for type variables.
      ctxTyVarMap :: Map TyVar ITy,
      -- | Known row membership constraints, i.e. @r -> o -> t@
      --   says `r` contains at offset `o` type `t`.
      ctxInRowMap :: Map RowVar (Map Offset ITy),
      -- | Known row offset relationships, i.e. @r -> (o,r')@
      --   says @r@ with offset @o@ applied corresponds to
      -- row @r'@. .
      ctxRowShiftMap :: Map RowVar (Set (Offset, RowVar))
    }
  deriving (Eq, Generic, Ord, Show)

instance PP.Pretty Context where
  pretty ctx =
    let row title entries = title PP.<+> PP.list entries in
      PP.vsep
      [ row "EqCs" $ map PP.pretty $ ctxEqCs ctx,
        row "InRowCs" $ map PP.pretty $ ctxInRowCs ctx,
        row "RowShiftCs" $ map PP.pretty $ ctxRowShiftCs ctx,
        row "OrCs" $ map PP.pretty $ ctxOrCs ctx,
        row "EqRowCs" $ map PP.pretty $ ctxEqRowCs ctx,
        row "Absurd EqCs" $ map PP.pretty $ ctxAbsurdEqCs ctx,
        row "Occurs check failures" $ map PP.pretty $ ctxOccursCheckFailures ctx,
        row "Type Var Map" $ map (\(x,xTy) -> prettySExp [PP.pretty x,"↦",PP.pretty xTy])
                           $ Map.toList $ ctxTyVarMap ctx,
        row "In Row Map" $
          let prettyRMap (r,rMap) = map (\(o,t) -> PP.pretty (InRowC o t r)) $ Map.toList rMap
          in concatMap prettyRMap $ Map.toList $ ctxInRowMap ctx,
        row "Row Shift Map" $
          let prettySMap (r,sSet) = map (\(o,r') -> PP.pretty (RowShiftC r o r')) $ Set.toList sSet
          in concatMap prettySMap $ Map.toList $ ctxRowShiftMap ctx
      ]

instance SubstTyVar Context where
  -- N.B., we only substitute in unprocessed constraints
  substTyVar xt ctx =
    Context
      { ctxEqCs       = fmap (substTyVar xt) (ctxEqCs ctx)
      , ctxInRowCs    = fmap (substTyVar xt) (ctxInRowCs ctx)
      , ctxRowShiftCs = fmap (substTyVar xt) (ctxRowShiftCs ctx)
      , ctxOrCs       = fmap (substTyVar xt) (ctxOrCs ctx)
      , ctxEqRowCs    = ctxEqRowCs ctx -- no TyVars in EqRowCs
      , ctxTyVarMap   = fmap (substTyVar xt) (ctxTyVarMap ctx)
      , ctxInRowMap   = fmap (fmap (substTyVar xt)) (ctxInRowMap ctx)
      -- Not modified because we only substitute in unprocessed constraints:
      , ctxAbsurdEqCs          = ctxAbsurdEqCs ctx
      , ctxOccursCheckFailures = ctxOccursCheckFailures ctx
      , ctxRowShiftMap         = ctxRowShiftMap ctx
      }

instance UnifyRowVars Context where
  unifyRowVars r1 r2 ctx =
    Context
      { ctxEqCs        = unifyRowVars r1 r2 <$> ctxEqCs ctx
      , ctxInRowCs     = unifyRowVars r1 r2 <$> ctxInRowCs ctx
      , ctxRowShiftCs  = unifyRowVars r1 r2 <$> ctxRowShiftCs ctx
      , ctxOrCs        = unifyRowVars r1 r2 <$> ctxOrCs ctx
      , ctxEqRowCs     = unifyRowVars r1 r2 <$> ctxEqRowCs ctx
      , ctxTyVarMap    = unifyRowVars r1 r2 <$> ctxTyVarMap ctx
      , ctxInRowMap    = unifyRowVarMap r1 r2 (unifyRowVars r1 r2 <$>) (unifyInRowMap r1 r2) (ctxInRowMap ctx)
      , ctxRowShiftMap = unifyRowVarMap r1 r2 (Set.map (second (unifyRowVars r1 r2))) (unifyRowShiftMap r1 r2) (ctxRowShiftMap ctx)
      -- Not modified because we only substitute in unprocessed constraints:
      , ctxAbsurdEqCs          = ctxAbsurdEqCs ctx
      , ctxOccursCheckFailures = ctxOccursCheckFailures ctx
      }

unifyInRowMap ::
  UnifyRowVars b =>
  RowVar -> RowVar -> Map Offset b -> Map Offset b -> Map Offset b
unifyInRowMap r1 r2 m1 m2 = unifyRowVars r1 r2 <$> Map.union m1 m2

unifyRowShiftMap ::
  RowVar -> RowVar ->
  Set (Offset, RowVar) -> Set (Offset, RowVar) -> Set (Offset, RowVar)
unifyRowShiftMap r1 r2 s1 s2 = Set.map (second (unifyRowVars r1 r2)) (Set.union s1 s2)

unifyRowVarMap :: RowVar -> RowVar -> (v -> v) -> (v -> v -> v) -> Map RowVar v -> Map RowVar v
unifyRowVarMap r1 r2 unify union m = unify <$> Map.delete r1 (Map.alter insertOrAlter r2 m)
  where
    insertOrAlter Nothing = Map.lookup r1 m
    insertOrAlter (Just v) = Just (maybe v (union v) (Map.lookup r1 m))

-- | @traceContext description ctx ctx'@ reports how the context changed via @trace@.
traceContext :: PP.Doc () -> Context -> Context -> Context
traceContext description preCtx postCtx =
  if not traceUnification then postCtx else
    let msg = PP.vsep [PP.hsep [">>> ", description]
                      , PP.indent 4 $ PP.pretty preCtx
                      , PP.hsep ["<<< ", description]
                      , PP.indent 4 $ PP.pretty postCtx]
      in trace (show msg) postCtx


-- | Does the context have any equality or subtype constraints left to process?
hasAtomicConstraints :: Context -> Bool
hasAtomicConstraints ctx =
  not (and [ null $ ctxEqCs ctx
           , null $ ctxInRowCs ctx
           , null $ ctxRowShiftCs ctx
           , null $ ctxEqRowCs ctx
           ])

dequeueEqC :: Context -> Maybe (Context, EqC)
dequeueEqC ctx = case ctxEqCs ctx of
  [] -> Nothing
  c:cs -> Just (ctx{ctxEqCs=cs},c)

dequeueInRowC :: Context -> Maybe (Context, InRowC)
dequeueInRowC ctx = case ctxInRowCs ctx of
  [] -> Nothing
  c:cs -> Just (ctx{ctxInRowCs=cs},c)

dequeueRowShiftC :: Context -> Maybe (Context, RowShiftC)
dequeueRowShiftC ctx = case ctxRowShiftCs ctx of
  [] -> Nothing
  c:cs -> Just (ctx{ctxRowShiftCs=cs},c)

dequeueEqRowC :: Context -> Maybe (Context, EqRowC)
dequeueEqRowC ctx = case ctxEqRowCs ctx of
  [] -> Nothing
  c:cs -> Just (ctx{ctxEqRowCs=cs},c)

addConstraints :: [TyConstraint] -> Context -> Context
addConstraints = flip addConstraints'

addConstraints' :: Context -> [TyConstraint] -> Context
addConstraints' = foldr go
  where
    go (EqTC c) = over (field @"ctxEqCs") (c:)
    go (InRowTC c) = over (field @"ctxInRowCs") (c:)
    go (RowShiftTC c) = over (field @"ctxRowShiftCs") (c:)
    go (OrTC c) = over (field @"ctxOrCs") (c:)
    go (AndTC (AndC cs)) = addConstraints cs
    go (EqRowTC c) = over (field @"ctxEqRowCs") (c :)

emptyContext :: Context
emptyContext = Context [] [] [] [] [] [] [] Map.empty Map.empty Map.empty

-- | Partition the constraints into the @Context@, which we use to order
-- which are handled when during unification.
initContext :: [TyConstraint] -> Context
initContext = addConstraints' emptyContext


-- | Is an equality constraint absurd?
absurdEqC :: EqC -> Bool
absurdEqC (EqC l r) = inconsistent l r

-- | Is an equality constraint trivial?
trivialEqC ::  EqC -> Bool
trivialEqC (EqC l r) = l == r

-- | Updates the context with the given equality. This action is "atomic" in the
-- sense that it may generate some additional implied constraints, but it does
-- not also solve those as well. It handles exactly one constraint.
solveEqC :: Context -> EqC -> Context
solveEqC ctx0 c
  | trivialEqC c = ctx0
  | absurdEqC c = over (field @"ctxAbsurdEqCs") (c:) ctx0
  | otherwise =
    let ctx1 = addConstraints (decomposeEqC c) ctx0 in
    case (eqLhs c, eqRhs c) of
      (UnknownTy x, t) -> solveVarEq ctx1 x t
      (t, UnknownTy x) -> solveVarEq ctx1 x t
      (_,_) -> ctx1
  where solveVarEq :: Context -> TyVar -> ITy -> Context
        solveVarEq ctx x t =
          if Set.member x (freeTyVars t)
            -- if we fail the occurs check, remember that for debugging and move on
            then over (field @"ctxOccursCheckFailures") (c:) ctx
            -- Otherwise, record x -> t in the type map
            -- and perform that substutition everywhere else.
            else substTyVar (x,t) $ over (field @"ctxTyVarMap") (Map.insert x t) ctx

-- | Updates the context with an @InRowC@ constraint. Like @solveEqC@ this is an
-- "atomic" update.
solveInRowC :: Context -> InRowC -> Context
solveInRowC ctx0 (InRowC o t r) =
  case Map.lookup r (ctxInRowMap ctx0) of
    Nothing -> over (field @"ctxInRowMap") (Map.insert r (Map.singleton o t)) ctx0
    Just rMap -> case Map.lookup o rMap of
      Nothing -> over (field @"ctxInRowMap") (Map.insert r (Map.insert o t rMap)) ctx0
      Just t' -> addConstraints [eqTC t t'] ctx0


-- | @shiftFields o m@ adds offset @o@ to the offsets in @m@.
shiftFields :: Offset -> Map Offset ITy -> Map Offset ITy
shiftFields n = Map.mapKeys (+n)

-- | Updates the context with a @RowShiftC@ constraint. Like @solveEqC@ this is
-- an "atomic" update.
solveRowShiftC :: Context -> RowShiftC -> Context
solveRowShiftC ctx0 (RowShiftC r1 n r2) =
  let ctx1 =
       over (field @"ctxRowShiftMap")
            (Map.insert
              r1
              (Set.insert (n,r2)
               (Map.findWithDefault Set.empty r1 (ctxRowShiftMap ctx0))))
            ctx0
    in case Map.lookup r1 (ctxInRowMap ctx0) of
        Nothing -> ctx1
        Just r1Entries ->
          let newCs = map (\(m, t) -> inRowTC (n + m) t r2)
                      $ Map.toList r1Entries
            in addConstraints newCs ctx1


-- | We solve an @EqRowC r1 r2@ by substituting all instances of @r1@ into @r2@.
-- This is mostly trivial, except in places where @r1@ is used as a key in a
-- map.
solveEqRowC :: Context -> EqRowC -> Context
solveEqRowC ctx (EqRowC r1 r2) = unifyRowVars r1 r2 ctx


-- | Process all atomic (i.e., non-disjunctive) constraints, updating the
-- context with each.
processAtomicConstraints :: Context -> Context
processAtomicConstraints ctx =  traceContext "processAtomicConstraints" ctx $
  case dequeueEqC ctx of
    Just (ctx', c) -> processAtomicConstraints $ solveEqC ctx' c
    Nothing ->
      case dequeueInRowC ctx of
        Just (ctx', c) -> processAtomicConstraints $ solveInRowC ctx' c
        Nothing ->
          case dequeueRowShiftC ctx of
            Just (ctx', c) -> processAtomicConstraints $ solveRowShiftC ctx' c
            Nothing ->
              case dequeueEqRowC ctx of
                Just (ctx', c) -> processAtomicConstraints $ solveEqRowC ctx' c
                Nothing -> ctx

-- | Is an @InRowC@ constraint absurd, given the other information present in the @Context@.
absurdInRowC :: Context -> InRowC -> Bool
absurdInRowC ctx (InRowC o t r) = case Map.lookup r (ctxInRowMap ctx) >>= Map.lookup o of
  Nothing -> False
  Just t' -> absurdEqC (EqC t t')

-- | Is an @InRowC@ constraint trivial, given the other information present in the @Context@.
trivialInRowC :: Context -> InRowC -> Bool
trivialInRowC ctx (InRowC o t r) = case Map.lookup r (ctxInRowMap ctx) >>= Map.lookup o of
  Nothing -> False
  Just t' -> trivialEqC (EqC t t')

-- | Is a @RowShiftC@ constraint absurd, given the other information present in the @Context@.
absurdRowShiftC :: Context -> RowShiftC -> Bool
absurdRowShiftC ctx (RowShiftC r1 n r2) =
  case (Map.lookup r1 rMap, Map.lookup r2 rMap) of
    (Just flds1, Just flds2) ->
      -- Compute the implied common field types and check if equality between any such pair
      -- is absurd.
      any absurdEqC $ Map.elems $ Map.intersectionWith EqC (Map.mapKeys (+n) flds1) flds2
    (_,_) -> False
  where rMap = ctxInRowMap ctx

-- | Reduce/simplify a constraint in a given context.
reduceC :: Context -> TyConstraint -> TyConstraint
reduceC ctx = go
  where
    go :: TyConstraint -> TyConstraint
    go =
      \case
        tc@(EqTC c) -> if absurdEqC c then absurdTC
                       else if trivialEqC c then trivialTC
                       else tc
        tc@(InRowTC c) -> if absurdInRowC ctx c then absurdTC
                          else if trivialInRowC ctx c then trivialTC
                          else tc
        tc@(RowShiftTC c) -> if absurdRowShiftC ctx c then absurdTC
                             else tc
        OrTC (OrC cs) -> orTC $ map go cs
        AndTC (AndC cs) -> andTC $ map go cs
        tc@(EqRowTC _) -> tc -- TODO: (val) is this correct?

-- | Attempt to reduce and eliminate disjunctions in the given context. Any
-- resulting non-disjuncts are added to their respective field in the context.
reduceDisjuncts :: Context -> Context
reduceDisjuncts initialContext = traceContext "reduceDisjuncts" initialContext $
  let disjs = ctxOrCs initialContext
      ctx0 = initialContext{ctxOrCs = []}
   in foldr elimOr ctx0 disjs
  where elimOr ::  OrC -> Context -> Context
        elimOr (OrC ds) ctx = addConstraints [orTC (map (reduceC ctx) ds)] ctx


-- | Reduce a context by processing its atomic constraints and then attempting
-- to reduce disjucts to atomic constraints. Halts once no more atomic constraints
-- exist or can be inferred from the disjunctions.
reduceContext :: Context -> Context
reduceContext ctx0 = traceContext "reduceContext" ctx0 $
  let ctx1 = reduceDisjuncts $ processAtomicConstraints ctx0
    in if hasAtomicConstraints ctx1 then reduceContext ctx1
       else ctx1


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

-- | Substitute out a row variable in a record type for the
-- corresponding offset mappings. If this results in a single
-- offset with conflicting types, replace the type
-- with an @Unknown@.
substRowVar :: Map RowVar (Map Offset FTy) -> Ty Unknown RowVar -> FTy
substRowVar rowMaps = \case
  UnknownTy{} -> unknownTy
  NumTy sz -> NumTy sz
  PtrTy t -> PtrTy $ substRowVar rowMaps t
  RecTy flds r -> let flds' = fmap (substRowVar rowMaps) flds in
    case Map.lookup r rowMaps of
      Nothing -> RecTy flds' NoRow
      Just rMap ->
        let combineTypes t1 t2 = if t1 == t2 then t1 else unknownTy
            mergedFlds = Map.unionWith combineTypes flds' rMap
        in RecTy mergedFlds NoRow


shiftStructuralInformationBy :: Integer -> Map Offset ITy -> Map Offset ITy
shiftStructuralInformationBy o =
  Map.fromList . concatMap retainPositiveOffsets . Map.toList
  where
    retainPositiveOffsets (Offset a, ty) =
      let newOffset = fromIntegral a + o in
      [(Offset (fromIntegral newOffset), ty) | newOffset >= 0]


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


-- | Unify the given constraints, returning a conservative type map for all type
-- variables.
unifyConstraints :: [TyConstraint] -> Map TyVar FTy
unifyConstraints initialConstraints =
  let fvs = foldr (Set.union . freeTyVars) Set.empty initialConstraints
      finalCtx = reduceContext $ initContext initialConstraints
      finalInRowMap = expandInRowMapWithShiftMap (ctxInRowMap finalCtx) (ctxRowShiftMap finalCtx)
      finalRowMap = fmap (removeRowVars . removeTyVars) <$> trace (show finalInRowMap) finalInRowMap
      finalTyMap = fmap (substRowVar finalRowMap . removeTyVars) (ctxTyVarMap finalCtx)
      unknownTyMap = Map.fromSet (const unknownTy) fvs
    in Map.union finalTyMap unknownTyMap
