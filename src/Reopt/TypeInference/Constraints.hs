{-# LANGUAGE LambdaCase #-}
-- Loosely based on 'TIE: Principled Reverse Engineering of Types in Binary
-- Programs' (NDSS 2011) by Jonghyup Lee, Thanassis Avgerinos, and David Brumley

-- Not clear yet how much will directly be applicable etc.
module Reopt.TypeInference.Constraints where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.List.NonEmpty (NonEmpty)
-- import qualified Data.List.NonEmpty as NonEmpty


newtype TyVar = TyVar {tyVarInt :: Int }
  deriving (Eq, Ord)

-- | Types of values in x86 machine code (missing reg types, records/memory, functions)
data Ty
  = -- | Top type (any kind of value).
    TopTy
  | -- | Bottom type (no possible values).
    BotTy
  | -- Type unification variable.
    VarTy TyVar
  | -- | A pointer of specified bit width pointing to some type of value (i.e., on X86_64 bit width would be 64).
    PtrTy Int Ty
  | -- | Code of specified bit width (i.e., on X86_64 bit width would be 64).
    CodeTy Int
  | -- | Register type of specified bit width (i.e., any value that can fit in a register of said size).
    RegTy Int
  | -- | Integer type of specified bit width (sign or unsigned).
    NumTy Int
  | -- | Unsigned integer type of specified bit width.
    UIntTy Int
  | -- | Signed integer type of specified bit width.
    IntTy Int
  | -- | Float type of specified bit width (e.g., float32, float64).
    FloatTy Int
  -- | -- | Record type, mapping bit offsets to types
  --   RecordTy [(Int, Ty)]
  | -- | Intersection of two or more types. Should not contain duplicates or nested intersections.
    AndTy Ty Ty [Ty]
  | -- | Union of two or more types. Should not contain duplicates or nested unions.
    OrTy Ty Ty [Ty]
  deriving (Eq, Ord)

isBaseTy :: Ty -> Bool
isBaseTy t = case t of
  TopTy -> False
  BotTy -> False
  VarTy{} -> False
  PtrTy{} -> True
  CodeTy{} -> True
  RegTy{} -> True
  NumTy{} -> True
  UIntTy{} -> True
  IntTy{} -> True
  FloatTy{} -> True
  AndTy{} -> False
  OrTy{} -> False

reg1Ty, reg8Ty, reg16Ty, reg32Ty, reg64Ty :: Ty
reg1Ty  = RegTy 1
reg8Ty  = RegTy 8
reg16Ty = RegTy 16
reg32Ty = RegTy 32
reg64Ty = RegTy 64

uint8Ty, uint16Ty, uint32Ty, uint64Ty :: Ty
uint8Ty  = UIntTy 8
uint16Ty = UIntTy 16
uint32Ty = UIntTy 32
uint64Ty = UIntTy 64

int8Ty, int16Ty, int32Ty, int64Ty :: Ty
int8Ty  = IntTy 8
int16Ty = IntTy 16
int32Ty = IntTy 32
int64Ty = IntTy 64

num8Ty, num16Ty, num32Ty, num64Ty :: Ty
num8Ty  = NumTy 8
num16Ty = NumTy 16
num32Ty = NumTy 32
num64Ty = NumTy 64

float32Ty, float64Ty :: Ty
float32Ty = FloatTy 32
float64Ty = FloatTy 64

-- Constructs a pointer type, simplifying some cases.
ptrTy :: Int -> Ty -> Ty
ptrTy _ BotTy = BotTy
ptrTy w t = PtrTy w t

-- Constructs an intersection type, simplifying some basic cases.
andTy :: [Ty] -> Ty
andTy = go Set.empty
  where go :: Set Ty -> [Ty] -> Ty
        go acc [] = case Set.toList acc of
                      [] -> TopTy
                      [t] -> t
                      t1:t2:ts -> AndTy t1 t2 ts
        go acc (TopTy:ts) = go acc ts
        go _   (BotTy:_) = BotTy
        go acc (AndTy s1 s2 ss:ts) = go acc (s1:s2:(ss++ts))
        go acc (t:ts) = go (Set.insert t acc) ts


-- Constructs a union type, simplifying some basic cases.
orTy :: [Ty] -> Ty
orTy = go Set.empty
  where go :: Set Ty -> [Ty] -> Ty
        go acc [] = case Set.toList acc of
                      [] -> BotTy
                      [t] -> t
                      t1:t2:ts -> OrTy t1 t2 ts
        go acc (BotTy:ts) = go acc ts
        go _   (TopTy:_) = TopTy
        go acc (OrTy s1 s2 ss:ts) = go acc (s1:s2:(ss++ts))
        go acc (t:ts) = go (Set.insert t acc) ts


-- | Return the given type with all type variables replaced via the lookup function.
concretize :: Ty -> (TyVar -> Ty) -> Ty
concretize initialTy lookupVar = go initialTy
  where
    go :: Ty -> Ty
    go TopTy = TopTy
    go BotTy = BotTy
    go (VarTy x) = lookupVar x
    go (PtrTy w t) = ptrTy w (go t)
    go t@CodeTy{} = t
    go t@RegTy{} = t
    go t@NumTy{} = t
    go t@IntTy{} = t
    go t@UIntTy{} = t
    go t@FloatTy{} = t
    go (AndTy t1 t2 ts) = andTy $ (go t1):(go t2):(map go ts)
    go (OrTy t1 t2 ts) = orTy $ (go t1):(go t2):(map go ts)

-- | @subst x t1 t2@ says substitute appearances of @x@ out for @t1@ in @t2@.
subst :: TyVar -> Ty -> Ty -> Ty
subst x xTy ty = concretize ty (\y -> if x == y then xTy else (VarTy y))

subtype :: Ty -> Ty -> Bool
subtype type1 type2 =
  case (type1,type2) of
    -- Reflexivity
    (s,t) | s == t -> True
    -- Top/Bot
    (_, TopTy) -> True
    (BotTy, _) -> True
    -- Pointer subtypes
    (PtrTy w1 s, PtrTy w2 t) -> w1 <= w2 && subtype s t
    -- Register subtypes
    (RegTy w1,   RegTy w2) -> w1 <= w2
    (PtrTy w1 n, RegTy w2) -> w1 <= w2
    (CodeTy w1,  RegTy w2) -> w1 <= w2
    (NumTy w1,   RegTy w2) -> w1 <= w2
    (IntTy w1,   RegTy w2) -> w1 <= w2
    (UIntTy w1,  RegTy w2) -> w1 <= w2
    -- Numeric (signed/unsigned integer) subtypes
    (NumTy w1,  NumTy w2) -> w1 <= w2
    (IntTy w1,  NumTy w2) -> w1 <= w2
    (UIntTy w1, NumTy w2) -> w1 <= w2
    -- Signed/Unsigned integer subtypes
    (IntTy w1,  IntTy w2)  -> w1 <= w2
    (UIntTy w1, UIntTy w2) -> w1 <= w2
    -- Set-theoretic subtypes
    (AndTy s1 s2 ss, t) -> any (\s -> subtype s t) (s1:s2:ss)
    (OrTy s1 s2 ss, t)  -> all (\s -> subtype s t) (s1:s2:ss)
    (s, AndTy t1 t2 ts) -> all (subtype s) (t1:t2:ts)
    (s, OrTy t1 t2 ts)  -> any (subtype s) (t1:t2:ts)
    -- Conservative base case
    (_, _) -> False


-- | Calculates the (least) upper bound of two types (denoted by the “join”
-- operator ⊔).
upperBound :: Ty -> Ty -> Ty
upperBound type1 type2 =
    case (type1,type2) of
    (s,t) | s == t -> s
    -- J-Subtype
    (s, t) | isBaseTy s && isBaseTy t && subtype s t -> t
    (s, t) | isBaseTy s && isBaseTy t && subtype t s -> s
    -- J-TypeVar
    (s@(VarTy _), t) -> orTy [s,t]
    (s, t@(VarTy _)) -> orTy [s,t]
    -- J-Ptr (N.B., TIE uses ⊓ in this rule, but I think that's a typo)
    ((PtrTy w1 s), (PtrTy w2 t)) | w1 == w2 -> ptrTy w1 (upperBound s t)
    -- J-NoRel
    (_,_) -> TopTy

-- | Calculates the (greatest) lower bound of two types (denoted by the meet”
-- operator ⊓).
lowerBound :: Ty -> Ty -> Ty
lowerBound type1 type2 =
  case (type1,type2) of
    (s,t) | s == t -> s
    -- M-Subtype
    (s, t) | isBaseTy s && isBaseTy t && subtype s t -> s
    (s, t) | isBaseTy s && isBaseTy t && subtype t s -> t
    -- M-TypeVar
    (s@(VarTy _), t) -> andTy [s,t]
    (s, t@(VarTy _)) -> andTy [s,t]
    -- M-Ptr
    ((PtrTy w1 s), (PtrTy w2 t)) | w1 == w2 -> ptrTy w1 (lowerBound s t)
    -- M-NoRel
    (_,_) -> BotTy

tyFreeVars :: Ty -> Set TyVar
tyFreeVars =
    \case
       TopTy -> Set.empty
       BotTy -> Set.empty
       VarTy x -> Set.singleton x
       PtrTy _ t -> tyFreeVars t
       CodeTy{} -> Set.empty
       RegTy{} -> Set.empty
       NumTy{} -> Set.empty
       IntTy{} -> Set.empty
       UIntTy{} -> Set.empty
       FloatTy{} -> Set.empty
       (AndTy t1 t2 ts) -> foldl Set.union (tyFreeVars t1) (map tyFreeVars $ t2:ts)
       (OrTy t1 t2 ts)  -> foldl Set.union (tyFreeVars t1) (map tyFreeVars $ t2:ts)

-- | @occursIn x t@, does `x` appear in `t`?
occursIn :: TyVar -> Ty -> Bool
occursIn x ty = Set.member x $ tyFreeVars ty

-- | x86 type constraints
data TyConstraint
  = -- | An equality constraint.
    EqC Ty Ty
  | -- | A subtype constraint.
    SubC Ty Ty
  | OrC [TyConstraint]
  | AndC [TyConstraint]

-- eqConstraint :: Ty -> Ty -> TyConstraint
-- eqConstraint


-- cSubst :: TyVar -> Ty -> TyConstraint -> TyConstraint
-- cSubst x xTy constraint = go constraint
--   where go :: TyConstraint -> TyConstraint
--         go (EqC t1 t2)  = EqC (subst x xTy t1) (subst x xTy t2)
--         go (SubC t1 t2) = SubC (subst x xTy t1) (subst x xTy t2)
--         go (OrC cs)     = OrC (map go cs)
--         go (AndC cs)    = AndC (map go cs)


-- | @decompSubC t1 t2@ decomposes `t1 <: t2` into any implied constraints. Cf.
-- TIE's `Υ` operator from § 6.3.2.
decomposeSubC :: Ty -> Ty -> [TyConstraint]
decomposeSubC type1 type2 =
  case (type1,type2) of
    ((PtrTy w1 s), (PtrTy w2 t)) | w1 == w2 ->  [SubC s t]
    (s, (AndTy t1 t2 ts)) -> map (SubC s) (t1:t2:ts)
    ((OrTy s1 s2 ss), t) -> map (\s -> SubC s t) (s1:s2:ss)
    _ -> []

-- | Constraints and working sets. Cf. `C` and related sets/maps in TIE § 6.3. N.B., in TIE,
-- substutition is used on the entire `C` set frequently. We prefer lazily
-- performing these substitutions _as we handle constraints_, via @substEqs@
-- and the relevant @Context@.
data Context
  = Context
    { -- | Equality constraints, i.e. each `(s,t)` means `s == t`. Cf. `C` from TIE § 6.3.
      ctxEqConstraints  :: [(Ty, Ty)]
    , -- | Subtype constraints, i.e. each `(s,t)` means `s <: t`. Cf. `C` from TIE § 6.3.
      ctxSubConstraints :: [(Ty, Ty)]
    , -- | Disjunctive constraints, i.e. each `[c1,c2,...]` means `c1 ∨ c2 ∨ ...`. Cf. `C` from TIE § 6.3.
      ctxOrConstraints  :: [[TyConstraint]]
      -- | Map from type variables to their known subtypes.
      -- One half of `S_{<:}` from TIE § 6.3.
    , ctxSubtypeMap :: Map TyVar (Set Ty)
      -- | Map from type variables to their known supertypes.
      -- One half of `S_{<:}` from TIE § 6.3.
    , ctxSupertypeMap :: Map TyVar (Set Ty)
    -- | Known type for type variables. `S_{=}` from TIE § 6.3. N.B., this map
    -- is used in conjunction with `substEqs` to apply substitutions lazily.
    , ctxVarTypeMap :: Map TyVar Ty
    -- | Upper bounds for type variables. `B^{↑}` from TIE § 6.3.
    , ctxVarUpperBoundMap :: Map TyVar Ty
    -- | Lower bounds for type variables. `B^{↓}` from TIE § 6.3.
    , ctxVarLowerBoundMap :: Map TyVar Ty
    }

addConstraints :: [TyConstraint] -> Context -> Context
addConstraints [] ctx = ctx
addConstraints ((EqC  t1 t2):cs) ctx =
    addConstraints cs (ctx {ctxEqConstraints = (t1,t2):ctxEqConstraints ctx})
addConstraints ((SubC t1 t2):cs) ctx =
  addConstraints cs (ctx {ctxSubConstraints = (t1,t2):ctxSubConstraints ctx})
addConstraints ((OrC cs'):cs) ctx =
    addConstraints cs (ctx {ctxOrConstraints = cs':ctxOrConstraints ctx})
addConstraints ((AndC cs'):cs) ctx =
    addConstraints (cs'++cs) ctx


-- | Perform the substitutions implied thus far by the @Context@'s
-- @ctxVarTypes@ map. (This is to avoid performing repeated substitutions on the
-- entire constraint set.)
substEqs :: Ty -> Context -> Ty
substEqs t cinfo = concretize t lookupVar
  where lookupVar x = Map.findWithDefault (VarTy x) x (ctxVarTypeMap cinfo)

-- | Concretize the type using any available upper bounds for type variables.
currentUpperBound :: Ty -> Context -> Ty
currentUpperBound t cinfo = concretize t lookupVar
  where lookupVar x = Map.findWithDefault TopTy x (ctxVarUpperBoundMap cinfo)

-- | Concretize the type using any available lower bounds for type variables.
currentLowerBound :: Ty -> Context -> Ty
currentLowerBound t cinfo = concretize t lookupVar
  where lookupVar x = Map.findWithDefault BotTy x (ctxVarLowerBoundMap cinfo)

emptyContext :: Context
emptyContext = Context [] [] [] Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Partition the constraints into the @Context@, which we use to order
-- which are handled when during unification.
initContext :: [TyConstraint] -> Context
initContext constraints = addConstraints constraints emptyContext

-- FIXME use S_{=} to not actually do full substitutions all the time and just
-- do them _as_ we consider constraints...? Use a newtype?

-- | @solveEqC (s,t) cset cinfo@ updates @cset@ and @cinfo@ with the equality
-- `s == t`. Cf. TIE Algorithm 1. If @Nothing@ is returned, the equality
-- failed the occurs check. FIXME should we have a `decomposeEqC` similar to `decomposeSubC`?
solveEqC :: (Ty,Ty) -> Context -> Maybe Context
solveEqC (type1, type2) ctx = go (substEqs type1 ctx) (substEqs type2 ctx)
  where go :: Ty -> Ty -> Maybe Context
        go (VarTy x) t =
          if occursIn x t
            then Nothing
            else Just $ ctx{ctxVarTypeMap = Map.insert x t (ctxVarTypeMap ctx)}
        go s t@(VarTy _) = go t s
        go (PtrTy _w1 s) (PtrTy _w2 t) = Just $ ctx {ctxEqConstraints = (s,t):ctxEqConstraints ctx}
        go _ _ = Just ctx


-- | Given `s <: t`, for all type variables `α` where `α <: s`, we add `α <: t`,
-- update the upper bound of `α`, and record any implied information from `α <:
-- s`. Cf. rule (2) in the Decomposition Rules of § 6.3.2. N.B., we interpret
-- the `S <: T` constraint described by TIE to include any `S` and `T`, but for
-- `∀(α <: S)` to quantify only over known subtype constraints where `α` is
-- _explicitly a type variable_.
updateUpperBounds :: (Ty,Ty) -> Context -> Context
updateUpperBounds (s, t) initial =
  -- Find all `α` where `α <: s` and perform updates to propogate `α <: t`.
  Map.foldrWithKey update initial (ctxSupertypeMap initial)
  where
    update :: TyVar -> Set Ty -> Context -> Context
    update a ts ctx =
      -- If `α </: s` or if we _already_ know `α <: t` then simply move on.
      if not (Set.member s ts) || (Set.member t ts) then ctx
      -- otherwise we need to compute the closure
      else
        let -- `S_{<:} = S_{<:} ∪ {a <: t}`
            supMap' = let updateSupSet mTys =
                              case mTys of Nothing  -> Just $ Set.singleton t
                                           Just tys -> Just $ Set.insert t tys
                        in Map.alter updateSupSet a $ ctxSupertypeMap ctx
            -- `B↑(α) ← B↑(α) ⊓ B↑(T)`
            upperMap' = let aTy = lowerBound (currentUpperBound (VarTy a) ctx) (currentUpperBound t ctx)
                          in Map.insert a aTy $ ctxVarUpperBoundMap ctx
          in -- Add `Υ(α <: T)` to `C`. (N.B., TIE suggests adding `Υ(α <: S)`
             -- instead, but if we previously recorded `α <: S` already, then we
             -- would have already recorded `Υ(α <: S)` at that time as well).
             addConstraints (decomposeSubC (VarTy a) t)
             $ ctx { ctxSupertypeMap = supMap',
                     ctxVarUpperBoundMap = upperMap'}

-- | Given `s <: t`, for all type variables `β` where `t <: β`, we add `s <: β`,
-- update the lower bound of `β`, and record any implied information from `s <:
-- β`. Cf. rule (3) in the Decomposition Rules of § 6.3.2. N.B., we interpret
-- the `S <: T` constraint described by TIE to include any `S` and `T`, but for
-- `∀(T <: α)` to quantify only over known subtype constraints where `β` is
-- _explicitly a type variable_.
updateLowerBounds :: (Ty,Ty) -> Context -> Context
updateLowerBounds (s, t) initial =
  -- Find all `β` where `t <: β` and perform updates to propogate `s <: β`.
  Map.foldrWithKey update initial (ctxSubtypeMap initial)
  where
    update :: TyVar -> Set Ty -> Context -> Context
    update b ts ctx =
      -- If `t </: β` or if we _already_ know `s <: β` then simply move on.
      if not (Set.member t ts) || (Set.member s ts) then ctx
      -- otherwise we need to compute the closure
      else
        -- Add `Υ(S <: β)` to `C`. (N.B., TIE suggests adding `Υ(T <: β)`
        -- instead, but if we previously recorded `T <: β` already, then we
        -- would have already recorded `Υ(T <: β)` at that time as well).
        let -- `S_{<:} = S_{<:} ∪ {s <: β}`
            subMap' = let updateSubSet mTys =
                              case mTys of Nothing  -> Just $ Set.singleton s
                                           Just tys -> Just $ Set.insert s tys
                        in Map.alter updateSubSet b $ ctxSubtypeMap ctx
            -- `B↓(α) ← B↓(β) ⊔ B↓(S)`
            lowerMap' = let bTy = upperBound (currentLowerBound (VarTy b) ctx) (currentLowerBound s ctx)
                          in Map.insert b bTy $ ctxVarLowerBoundMap ctx
          in addConstraints (decomposeSubC s (VarTy b))
             $ ctx { ctxSubtypeMap = subMap',
                     ctxVarUpperBoundMap = lowerMap'}

-- FIXME use `decomposeSubC` as a "preprocess" pass over the subtype constraint...???

-- | @solveSubC (t1,t2) cset cinfo@ updates @cset@ and @cinfo@ with the subtype constraint
-- `t1 <: t2`. Cf. TIE § 6.3.2.
solveSubC :: (Ty,Ty) -> Context -> Context
solveSubC (type1,type2) ctx =
  let t1 = substEqs type1 ctx
      t2 = substEqs type2 ctx
  in -- Occurs check
     if not $ Set.disjoint (tyFreeVars t1) (tyFreeVars t2) then ctx
     -- If `t1 <: t2` the contraint is trivial and should be discarded.
     else if subtype t1 t2 then ctx
     else addConstraints (decomposeSubC t1 t2)
           $ updateLowerBounds (t1,t2)
           $ updateUpperBounds (t1,t2)
           $ ctx


newtype TyUnificationError = TyUnificationError String


-- | Unify the given constraints, returning a conservative type map for all type
-- variables.
unifyConstraints :: [TyConstraint] -> Either TyUnificationError (Map TyVar Ty)
unifyConstraints initialConstraints = undefined -- FIXME
