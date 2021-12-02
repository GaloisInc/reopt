{-# LANGUAGE LambdaCase #-}
-- Loosely based on 'TIE: Principled Reverse Engineering of Types in Binary
-- Programs' (NDSS 2011) by Jonghyup Lee, Thanassis Avgerinos, and David Brumley

-- Not clear yet how much will directly be applicable etc.
module Reopt.TypeInference.Constraints where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data NumSize
  = NumSize8
  | NumSize16
  | NumSize32
  | NumSize64
  deriving (Eq, Ord)

numSizeInt :: NumSize -> Int
numSizeInt =
  \case
    NumSize8 -> 8
    NumSize16 -> 16
    NumSize32 -> 32
    NumSize64 -> 64

newtype TyVar = TyVar {tyVarInt :: Int }
  deriving (Eq, Ord)

-- | Types of values in x86 machine code (missing reg types, records/memory, functions)
data X86Ty
  = TopTy
  | BotTy
  | VarTy TyVar -- a type unification variable
  | PtrTy X86Ty
  | NumTy NumSize
  | ScalarFloatTy
  | VecFloatTy -- FIXME does this overlap with ScalarFloatTy in it's memory representation?
  | AndTy X86Ty X86Ty
  | OrTy X86Ty X86Ty
  deriving (Eq, Ord)

isBaseTy :: X86Ty -> Bool
isBaseTy t = case t of
  TopTy -> False
  BotTy -> False
  VarTy{} -> False
  PtrTy{} -> True
  NumTy{} -> True
  ScalarFloatTy -> True
  VecFloatTy -> True
  AndTy{} -> False
  OrTy{} -> False


num8Ty, num16Ty, num32Ty, num64Ty :: X86Ty
num8Ty = NumTy NumSize8
num16Ty = NumTy NumSize16
num32Ty = NumTy NumSize32
num64Ty = NumTy NumSize64

-- Constructs a pointer type, simplifying some cases.
ptrTy :: X86Ty -> X86Ty
ptrTy BotTy = BotTy
ptrTy t = PtrTy t

-- Constructs an intersection type, simplifying some cases.
andTy :: X86Ty -> X86Ty -> X86Ty
andTy BotTy _ = BotTy
andTy _ BotTy = BotTy
andTy TopTy t = t
andTy t TopTy = t
andTy (PtrTy t1) (PtrTy t2) = PtrTy (andTy t1 t2)
andTy t1 t2 = if t1 == t2 then t1 else AndTy t1 t2

-- Constructs a union type, simplifying some cases.
orTy :: X86Ty -> X86Ty -> X86Ty
orTy BotTy t = t
orTy t BotTy = t
orTy TopTy _ = TopTy
orTy _ TopTy = TopTy
orTy t1 t2 = if t1 == t2 then t1 else OrTy t1 t2


-- | Return the given type with all type variables replaced via the lookup function.
concretize :: X86Ty -> (TyVar -> X86Ty) -> X86Ty
concretize initialTy lookupVar = go initialTy
  where
    go :: X86Ty -> X86Ty
    go TopTy = TopTy
    go BotTy = BotTy
    go (VarTy x) = lookupVar x
    go (PtrTy t) = ptrTy (go t)
    go t@(NumTy _) = t
    go ScalarFloatTy = ScalarFloatTy
    go VecFloatTy = VecFloatTy
    go (AndTy t1 t2) = andTy (go t1) (go t2)
    go (OrTy t1 t2) = orTy (go t1) (go t2)

-- | @subst x t1 t2@ says substitute appearances of @x@ out for @t1@ in @t2@.
subst :: TyVar -> X86Ty -> X86Ty -> X86Ty
subst x xTy ty = concretize ty (\y -> if x == y then xTy else (VarTy y))

subtype :: X86Ty -> X86Ty -> Bool
subtype type1 type2 =
  case (type1,type2) of
    (t1,t2) | t1 == t2 -> True
    (_, TopTy) -> True
    (BotTy, _) -> True
    (PtrTy t1, PtrTy t2) -> subtype t1 t2
    (NumTy n1, NumTy n2) -> (numSizeInt n1) <= (numSizeInt n2)
    (AndTy t1 t2, t3) -> subtype t1 t3 || subtype t2 t3
    (OrTy t1 t2, t3)  -> subtype t1 t3 && subtype t2 t3
    (t1, AndTy t2 t3) -> subtype t1 t2 && subtype t1 t3
    (t1, OrTy t2 t3)  -> subtype t1 t2 || subtype t1 t3
    (_, _) -> False


-- | Calculates the (least) upper bound of two types (denoted by the “join”
-- operator ⊔).
upperBound :: X86Ty -> X86Ty -> X86Ty
upperBound type1 type2 =
    case (type1,type2) of
    (t1,t2) | t1 == t2 -> t1
    -- J-Subtype
    (t1, t2) | isBaseTy t1 && isBaseTy t2 && subtype t1 t2 -> t2
    (t1, t2) | isBaseTy t1 && isBaseTy t2 && subtype t2 t1 -> t1
    -- J-TypeVar
    (t1@(VarTy _), t2) -> orTy t1 t2
    (t1, t2@(VarTy _)) -> orTy t1 t2
    -- J-Ptr (N.B., TIE uses ⊓ in this rule, but I think that's a typo)
    ((PtrTy t1), (PtrTy t2)) -> ptrTy (upperBound t1 t2)
    -- J-NoRel
    (_,_) -> TopTy

-- | Calculates the (greatest) lower bound of two types (denoted by the meet”
-- operator ⊓).
lowerBound :: X86Ty -> X86Ty -> X86Ty
lowerBound type1 type2 =
  case (type1,type2) of
    (t1,t2) | t1 == t2 -> t1
    -- M-Subtype
    (t1, t2) | isBaseTy t1 && isBaseTy t2 && subtype t1 t2 -> t1
    (t1, t2) | isBaseTy t1 && isBaseTy t2 && subtype t2 t1 -> t2
    -- M-TypeVar
    (t1@(VarTy _), t2) -> andTy t1 t2
    (t1, t2@(VarTy _)) -> andTy t1 t2
    -- M-Ptr
    ((PtrTy t1), (PtrTy t2)) -> ptrTy (lowerBound t1 t2)
    -- M-NoRel
    (_,_) -> BotTy

tyFreeVars :: X86Ty -> Set TyVar
tyFreeVars =
    \case
       TopTy -> Set.empty
       BotTy -> Set.empty
       (VarTy x) -> Set.singleton x
       (PtrTy t) -> tyFreeVars t
       (NumTy _) -> Set.empty
       ScalarFloatTy -> Set.empty
       VecFloatTy -> Set.empty
       (AndTy t1 t2) -> Set.union (tyFreeVars t1) (tyFreeVars t2)
       (OrTy t1 t2) -> Set.union (tyFreeVars t1) (tyFreeVars t2)

-- | @occursIn x t@, does `x` appear in `t`?
occursIn :: TyVar -> X86Ty -> Bool
occursIn x ty = Set.member x $ tyFreeVars ty


-- | A type which has not been subjected to any substitutions currently being
-- considered.
-- newtype RawX86Ty = RawX86Ty {rawX86Ty :: X86Ty}



-- | x86 type constraints
data X86TyConstraint
  = -- | An equality constraint.
    EqC X86Ty X86Ty
  | -- | A subtype constraint.
    SubC X86Ty X86Ty
  | OrC [X86TyConstraint]
  | AndC [X86TyConstraint]

-- eqConstraint :: X86Ty -> X86Ty -> X86TyConstraint
-- eqConstraint


-- cSubst :: TyVar -> X86Ty -> X86TyConstraint -> X86TyConstraint
-- cSubst x xTy constraint = go constraint
--   where go :: X86TyConstraint -> X86TyConstraint
--         go (EqC t1 t2)  = EqC (subst x xTy t1) (subst x xTy t2)
--         go (SubC t1 t2) = SubC (subst x xTy t1) (subst x xTy t2)
--         go (OrC cs)     = OrC (map go cs)
--         go (AndC cs)    = AndC (map go cs)


-- | @decompSubC t1 t2@ decomposes `t1 <: t2` into any implied constraints. Cf.
-- TIE's `Υ` operator from § 6.3.2.
decomposeSubC :: X86Ty -> X86Ty -> [X86TyConstraint]
decomposeSubC (PtrTy t1) (PtrTy t2) = [SubC t1 t2]
decomposeSubC t1 (AndTy t2 t3) = [SubC t1 t2, SubC t1 t3]
decomposeSubC (OrTy t1 t2) t3 = [SubC t1 t3, SubC t2 t3]
decomposeSubC _ _ = []

-- | An equality constraint before relevant substitutions have been applied.
data RawEqC = RawEqC X86Ty X86Ty
-- | A subtype constraint before relevant substitutions have been applied.
data RawSubC = RawSubC X86Ty X86Ty
-- | A disjunction of constraints before relevant substitutions have been applied.
data RawOrC = RawOrC [X86TyConstraint]

-- | Constraints, organized by type. Cf. `C` in TIE § 6.3. N.B., in TIE,
-- substutition is used on this entire set frequently. We prefer lazily
-- performing these substitutions _as we handle constraints_, via @substEqs@
-- and the relevant @ConstraintInfo@.
data ConstraintSet
  = ConstraintSet
    { csRawEqConstraints  :: [RawEqC]
    , csEqConstraints  :: [(X86Ty, X86Ty)]
    , csRawSubConstraints :: [RawSubC]
    , csSubConstraints :: [(X86Ty, X86Ty)]
    , csRawOrConstraints  :: [RawOrC]
    }

emptyConstraintSet :: ConstraintSet
emptyConstraintSet = ConstraintSet [] [] [] [] []


data ConstraintInfo
  = ConstraintInfo
    { -- | Map from type variables to their known subtypes.
      -- One half of `S_{<:}` from TIE § 6.3.
      ciSubtypeCache :: Map TyVar (Set X86Ty)
      -- | Map from type variables to their known supertypes.
      -- One half of `S_{<:}` from TIE § 6.3.
    , ciSupertypeCache :: Map TyVar (Set X86Ty)
    -- | Known type for type variables. `S_{=}` from TIE § 6.3. N.B., this map
    -- is used in conjunction with `substEqs` to apply substitutions lazily.
    , ciVarTypes :: Map TyVar X86Ty
    -- | Upper bounds for type variables. `B^{↑}` from TIE § 6.3.
    , ciVarUpperBounds :: Map TyVar X86Ty
    -- | Lower bounds for type variables. `B^{↓}` from TIE § 6.3.
    , ciVarLowerBounds :: Map TyVar X86Ty
    }

-- | Perform the substitutions implied thus far by the @ConstraintInfo@'s
-- @ciVarTypes@ map. (This is to avoid performing repeated substitutions on the
-- entire constraint set.)
substEqs :: X86Ty -> ConstraintInfo -> X86Ty
substEqs t cinfo = concretize t lookupVar
  where lookupVar x = Map.findWithDefault (VarTy x) x (ciVarTypes cinfo)

-- | Concretize the type using any available upper bounds for type variables.
currentUpperBound :: X86Ty -> ConstraintInfo -> X86Ty
currentUpperBound t cinfo = concretize t lookupVar
  where lookupVar x = Map.findWithDefault TopTy x (ciVarUpperBounds cinfo)

-- | Concretize the type using any available lower bounds for type variables.
currentLowerBound :: X86Ty -> ConstraintInfo -> X86Ty
currentLowerBound t cinfo = concretize t lookupVar
  where lookupVar x = Map.findWithDefault BotTy x (ciVarLowerBounds cinfo)

emptyConstraintInfo :: ConstraintInfo
emptyConstraintInfo = ConstraintInfo Map.empty Map.empty Map.empty Map.empty Map.empty

addConstraints :: [X86TyConstraint] -> ConstraintSet -> ConstraintSet
addConstraints [] cset = cset
addConstraints ((EqC  t1 t2):cs) cset =
    addConstraints cs (cset {csRawEqConstraints = (RawEqC t1 t2):csRawEqConstraints cset})
addConstraints ((SubC t1 t2):cs) cset =
  addConstraints cs (cset {csRawSubConstraints = (RawSubC t1 t2):csRawSubConstraints cset})
addConstraints ((OrC cs'):cs) cset =
    addConstraints cs (cset {csRawOrConstraints = (RawOrC cs'):csRawOrConstraints cset})
addConstraints ((AndC cs'):cs) cset =
    addConstraints (cs'++cs) cset

-- | Partition the constraints into the @ConstraintSet@, which we use to order
-- which are handled when during unification.
initConstraintSet :: [X86TyConstraint] -> ConstraintSet
initConstraintSet constraints = addConstraints constraints emptyConstraintSet

-- FIXME use S_{=} to not actually do full substitutions all the time and just
-- do them _as_ we consider constraints...? Use a newtype?

-- | @solveEqC (s,t) cset cinfo@ updates @cset@ and @cinfo@ with the equality
-- `s == t`. Cf. TIE Algorithm 1. If @Nothing@ is returned, the equality
-- failed the occurs check.
solveEqC :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> Maybe (ConstraintSet, ConstraintInfo)
solveEqC (s, t) (cset, cinfo) = go s t
  where go :: X86Ty -> X86Ty -> Maybe (ConstraintSet, ConstraintInfo)
        go (VarTy x) t2 =
          if occursIn x t2
            then Nothing
            else Just (cset, cinfo{ciVarTypes = Map.insert x t2 (ciVarTypes cinfo)})
        go t1 t2@(VarTy _) = go t2 t1
        go (PtrTy t1) (PtrTy t2) = Just (cset{csEqConstraints = (t1,t2):csEqConstraints cset} , cinfo)
        go _ _ = Just (cset, cinfo)


-- | Given `s <: t`, for all type variables `α` where `α <: s`, we add `α <: t`,
-- update the upper bound of `α`, and record any implied information from `α <:
-- s`. Cf. rule (2) in the Decomposition Rules of § 6.3.2. N.B., we interpret
-- the `S <: T` constraint described by TIE to include any `S` and `T`, but for
-- `∀(α <: S)` to quantify only over known subtype constraints where `α` is
-- _explicitly a type variable_.
updateUpperBounds :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
updateUpperBounds (s, t) initial =
  -- Find all `α` where `α <: s` and perform updates to propogate `α <: t`.
  Map.foldrWithKey update initial (ciSupertypeCache $ snd initial)
  where
    update :: TyVar -> Set X86Ty -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
    update a ts (cset, cinfo) =
      -- If `α </: s` or if we _already_ know `α <: t` then simply move on.
      if not (Set.member s ts) || (Set.member t ts) then (cset, cinfo)
      -- otherwise we need to compute the closure
      else
        -- Add `Υ(α <: T)` to `C`. (N.B., TIE suggests adding `Υ(α <: S)`
        -- instead, but if we previously recorded `α <: S` already, then we
        -- would have already recorded `Υ(α <: S)` at that time as well).
        let cset' = addConstraints (decomposeSubC (VarTy a) t) cset
            -- `S_{<:} = S_{<:} ∪ {a <: t}`
            supCache' = let updateSupSet mTys =
                              case mTys of Nothing  -> Just $ Set.singleton t
                                           Just tys -> Just $ Set.insert t tys
                        in Map.alter updateSupSet a $ ciSupertypeCache cinfo
            -- `B↑(α) ← B↑(α) ⊓ B↑(T)`, `
            upperBounds' = let aTy = lowerBound (currentUpperBound (VarTy a) cinfo) (currentUpperBound t cinfo)
                             in Map.insert a aTy $ ciVarUpperBounds cinfo
            cinfo' = cinfo{ciSupertypeCache = supCache', ciVarUpperBounds = upperBounds'}
          in (cset', cinfo')

-- | Given `t1 <: t2`, if `t2` is a type variable `x`, update the lower bounds
-- of any supertypes. I.e., ∀ `y` s.t.`x <: y`, we want to record `t1 <: y`. Cf.
-- rule (3) in the Decomposition Rules of § 6.3.2.
updateLowerBounds :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
updateLowerBounds (t1, VarTy x) (cset, cinfo) = undefined -- FIXME
updateLowerBounds (_, _)  (cset, cinfo) = (cset, cinfo)

-- FIXME use `decomposeSubC` as a "preprocess" pass over the subtype constraint...???

-- | @solveSubC (t1,t2) cset cinfo@ updates @cset@ and @cinfo@ with the subtype constraint
-- `t1 <: t2`. Cf. TIE § 6.3.2.
solveSubC :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
solveSubC (t1,t2) (cset, cinfo) = -- FIXME substEqs here
    -- If `t1 <: t2` the contraint is trivial and should be discarded.
    if subtype t1 t2 then (cset, cinfo)  -- FIXME occurs check
    else let (cset', cinfo') = updateLowerBounds (t1,t2)
                               $ updateUpperBounds (t1,t2)
                               $ (cset, cinfo)
           in (addConstraints (decomposeSubC t1 t2) cset', cinfo')


newtype X86TyUnificationError = X86TyUnificationError String


-- | Unify the given constraints, returning a conservative type map for all type
-- variables.
unifyConstraints :: [X86TyConstraint] -> Either X86TyUnificationError (Map TyVar X86Ty)
unifyConstraints initialConstraints = undefined -- FIXME
