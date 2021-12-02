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

-- | Types of values in x86 machine code
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
isBaseTy t =
  case t of
    PtrTy _ -> True
    NumTy _ -> True
    ScalarFloatTy -> True
    VecFloatTy -> True
    _ -> False


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
    (t1@(VarTy _), t2) -> OrTy t1 t2
    (t1, t2@(VarTy _)) -> OrTy t1 t2
    -- J-Ptr
    ((PtrTy t1), (PtrTy t2)) -> PtrTy (upperBound t1 t2)
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
    (t1@(VarTy _), t2) -> AndTy t1 t2
    (t1, t2@(VarTy _)) -> AndTy t1 t2
    -- M-Ptr
    ((PtrTy t1), (PtrTy t2)) -> PtrTy (lowerBound t1 t2)
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


-- | x86 type constraints
data X86TyConstraint
  = -- | An equality constraint.
    EqC X86Ty X86Ty
  | -- | A subtype constraint.
    SubC X86Ty X86Ty
  | OrC [X86TyConstraint]
  | AndC [X86TyConstraint]


cSubst :: TyVar -> X86Ty -> X86TyConstraint -> X86TyConstraint
cSubst x xTy constraint = go constraint
  where go :: X86TyConstraint -> X86TyConstraint
        go (EqC t1 t2)  = EqC (subst x xTy t1) (subst x xTy t2)
        go (SubC t1 t2) = SubC (subst x xTy t1) (subst x xTy t2)
        go (OrC cs)     = OrC (map go cs)
        go (AndC cs)    = AndC (map go cs)


-- | @decompSubC t1 t2@ decomposes `t1 <: t2` into any implied constraints. Cf.
-- TIE's `Υ` operator from § 6.3.2.
decomposeSubC :: X86Ty -> X86Ty -> [X86TyConstraint]
decomposeSubC (PtrTy t1) (PtrTy t2) = [SubC t1 t2]
decomposeSubC t1 (AndTy t2 t3) = [SubC t1 t2, SubC t1 t3]
decomposeSubC (OrTy t1 t2) t3 = [SubC t1 t3, SubC t2 t3]
decomposeSubC _ _ = []

data ConstraintSet
  = ConstraintSet
    { csEqConstraints  :: [(X86Ty, X86Ty)]
    , csSubConstraints :: [(X86Ty, X86Ty)]
    , csOrConstraints  :: [[X86TyConstraint]]
    }

emptyConstraintSet :: ConstraintSet
emptyConstraintSet = ConstraintSet [] [] []

csetSubst :: TyVar -> X86Ty -> ConstraintSet -> ConstraintSet
csetSubst x xTy (ConstraintSet eqs subs ors) = ConstraintSet eqs' subs' ors'
  where pSubst (t1,t2) = (subst x xTy t1, subst x xTy t2)
        eqs'  = map pSubst eqs
        subs' = map pSubst subs
        ors'  = map (map (cSubst x xTy)) ors


data ConstraintInfo
  = ConstraintInfo
    { -- | Map from type variables to their known subtypes.
      -- One half of `S_{<:}` from TIE § 6.3.
      ciSubtypeCache :: Map TyVar (Set X86Ty)
      -- | Map from type variables to their known supertypes.
      -- One half of `S_{<:}` from TIE § 6.3.
    , ciSupertypeCache :: Map TyVar (Set X86Ty)
    -- | Known type for type variables. `S_{=}` from TIE § 6.3.
    , ciVarTypes :: Map TyVar X86Ty
    -- | Upper bounds for type variables. `B^{↑}` from TIE § 6.3.
    , ciVarUpperBounds :: Map TyVar X86Ty
    -- | Lower bounds for type variables. `B^{↓}` from TIE § 6.3.
    , ciVarLowerBounds :: Map TyVar X86Ty
    }

-- | Concretize the type using any available upper bounds for type variables.
currentUpperBound :: X86Ty -> ConstraintInfo -> X86Ty
currentUpperBound t cinfo = concretize t lookup
  where lookup x = Map.findWithDefault TopTy x (ciVarUpperBounds cinfo)

-- | Concretize the type using any available lower bounds for type variables.
currentLowerBound :: X86Ty -> ConstraintInfo -> X86Ty
currentLowerBound t cinfo = concretize t lookup
  where lookup x = Map.findWithDefault BotTy x (ciVarLowerBounds cinfo)

emptyConstraintInfo :: ConstraintInfo
emptyConstraintInfo = ConstraintInfo Map.empty Map.empty Map.empty Map.empty Map.empty

addConstraints :: [X86TyConstraint] -> ConstraintSet -> ConstraintSet
addConstraints [] cset = cset
addConstraints ((EqC  t1 t2):cs) cset = addConstraints cs (cset {csEqConstraints = (t1,t2):csEqConstraints cset})
addConstraints ((SubC t1 t2):cs) cset = addConstraints cs (cset {csSubConstraints = (t1,t2):csSubConstraints cset})
addConstraints ((OrC cs'):cs) cset = addConstraints cs (cset {csOrConstraints = cs':csOrConstraints cset})
addConstraints ((AndC cs'):cs) cset = addConstraints (cs'++cs) cset

-- | Partition the constraints into the @ConstraintSet@, which we use to order
-- which are handled when during unification.
initConstraintSet :: [X86TyConstraint] -> ConstraintSet
initConstraintSet constraints = addConstraints constraints emptyConstraintSet

-- | @solveEqC (t1,t2) cset cinfo@ updates @cset@ and @cinfo@ with the equality
-- `t1 == t2`. Cf. TIE Algorithm 1. If @Nothing@ is returned, the equality
-- failed the occurs check.
solveEqC :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> Maybe (ConstraintSet, ConstraintInfo)
solveEqC (VarTy x, t2) (cset, cinfo) =
  let varTypes = ciVarTypes cinfo in
    if occursIn x t2 then
      Nothing
    else
      case Map.lookup x varTypes of
        Just xTy -> solveEqC (xTy, t2) (cset, cinfo)
        Nothing ->
          let varTypes' = Map.insert x t2 varTypes in
            Just (csetSubst x t2 cset, cinfo{ciVarTypes = varTypes'})
solveEqC (t1, t2@(VarTy _)) (cset, cinfo) =
  solveEqC (t2, t1) (cset, cinfo)
solveEqC (PtrTy t1, PtrTy t2) (cset, cinfo) =
  Just (cset{csEqConstraints = (t1,t2):csEqConstraints cset} , cinfo)
solveEqC _ (cset, cinfo) = Just (cset, cinfo)


-- | Given `t1 <: t2`, if `t1` is a type variable `x`, update the upper bounds
-- of any subtypes. I.e., ∀ `y` s.t.`y <: x`, we now know `y <: t2`. Cf. rule
-- (2) in the Decomposition Rules of § 6.3.2.
updateUpperBounds :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
updateUpperBounds (xTy@(VarTy x),t2) initial = Map.foldrWithKey update initial (ciSupertypeCache $ snd initial)
  where
    update :: TyVar -> Set X86Ty -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
    update y ySuperTySet (cset, cinfo) =
      if not $ Set.member xTy ySuperTySet
        then -- `y </: x`, so there's nothing to update
          (cset, cinfo)
      else -- `y <: x` implying `y <: t2`
        -- AMK: TIE suggests updating cset with `Υ(α <: S)` but also
        -- seems to suggest doing so would be a no-op when they describe `Υ`:
        -- "For constraints on basic type variables, where
        -- further decomposition is not possible, the
        -- decomposition operator returns an empty set (∅)."
        let cset' = cset
            -- add `y <: t2` to our known supertypes
            cinfo' = cinfo{ciSupertypeCache = Map.insert y (Set.insert t2 ySuperTySet) (ciSupertypeCache cinfo)}
            -- update y upper bound to it's current upper bound ⊓ t2's current upper bound
            yUB = upperBound (currentUpperBound (VarTy y) cinfo') (currentUpperBound (VarTy y) cinfo')
          in (cset', cinfo'{ciVarUpperBounds = Map.insert y yUB (ciVarUpperBounds cinfo')})
updateUpperBounds (_,_) (cset, cinfo) = (cset, cinfo)

-- | Given `t1 <: t2`, if `t2` is a type variable `x`, update the lower bounds
-- of any supertypes. I.e., ∀ `y` s.t.`x <: y`, we want to record `t1 <: y`. Cf.
-- rule (3) in the Decomposition Rules of § 6.3.2.
updateLowerBounds :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
updateLowerBounds (t1,VarTy x) (cset, cinfo) = undefined -- FIXME
updateLowerBounds (_, _)  (cset, cinfo) = (cset, cinfo)

-- | @solveSubC (t1,t2) cset cinfo@ updates @cset@ and @cinfo@ with the subtype constraint
-- `t1 <: t2`. Cf. TIE § 6.3.2.
solveSubC :: (X86Ty,X86Ty) -> (ConstraintSet, ConstraintInfo) -> (ConstraintSet, ConstraintInfo)
solveSubC (t1,t2) (cset, cinfo) =
    -- If `t1 <: t2` the contraint is trivial and should be discarded.
    if subtype t1 t2 then (cset, cinfo)
    else let (cset', cinfo') = updateLowerBounds (t1,t2)
                               $ updateUpperBounds (t1,t2)
                               $ (cset, cinfo)
           in (addConstraints (decomposeSubC t1 t2) cset', cinfo')


newtype X86TyUnificationError = X86TyUnificationError String


-- | Unify the given constraints, returning a conservative type map for all type
-- variables.
unifyConstraints :: [X86TyConstraint] -> Either X86TyUnificationError (Map TyVar X86Ty)
unifyConstraints initialConstraints = undefined -- FIXME


-- Questions
-- 1. When does unification fail assuming we have ironed out all the wrinkles in
--    the algorithm? Is that just when it can't decide on a precise type?
-- 2. Is the current `X86Ty` the right type to return to other parts of Reopt?
--    Or are too many parts of it artifacts of the TIE unification algorithm?
--    I'm leaning towards the latter, and we should have a `X86ConcreteTy` data
--    type basically which does not include top/botton/union/etc.
