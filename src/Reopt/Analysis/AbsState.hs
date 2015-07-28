{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.Analysis.AbsState
  ( AbsBlockState
  , mkAbsBlockState
  , absX86State
  , absBlockDiff
  , shiftSpecificOffset
  , setAbsIP
  , AbsBlockStack
  , StackEntry(..)
  , AbsValue(..)
  , emptyAbsValue
  , joinAbsValue
  , ppAbsValue
  , abstractSingleton
  , subValue
  , concreteStackOffset
  , concretize
  , asConcreteSingleton
  , meet
  , size
  , codePointerSet
  , AbsDomain(..)
  , AbsProcessorState
  , absInitialRegs
  , startAbsStack
  , initAbsProcessorState
  , absAssignments
  , assignLens
  , stridedInterval
  , finalAbsBlockState
  , addAssignment
  , addMemWrite
  , transferValue
  , transferRHS
  , abstractULt
  , abstractULeq
  , isBottom
  ) where

import           Control.Applicative ( (<$>) )
import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad (guard)
import           Control.Monad.State.Strict
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Reopt.Analysis.Domains.StridedInterval as SI
import           Reopt.Object.Memory
import           Reopt.CFG.Representation
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types

import           Debug.Trace

------------------------------------------------------------------------
-- Abstract states

class Eq d => AbsDomain d where
  -- | The top element
  top :: d

  -- | A partial ordering over d.  forall x. x `leq` top
  leq :: d -> d -> Bool
  leq x y =
    case joinD y x of
      Nothing -> True
      Just _ -> False

  -- | Least upper bound (always defined, as we have top)
  lub :: d -> d -> d
  lub x y = case joinD x y of
              Nothing -> x
              Just r -> r

  -- | Join the old and new states and return the updated state iff
  -- the result is larger than the old state.
  joinD :: d -> d -> Maybe d
  joinD old new
    | new `leq` old = Nothing
    | otherwise     = Just $ lub old new

  {-# MINIMAL (top, ((leq,lub) | joinD)) #-}

type ValueSet = Set Integer

------------------------------------------------------------------------
-- AbsValue

data AbsValue (tp :: Type) where
  -- | An absolute value.
  FinSet :: !ValueSet -> AbsValue (BVType n)
  -- A possibly empty set of values that either point to a code segment or 0.
  CodePointers :: !(Set Word64) -> AbsValue (BVType 64)

  -- Offset of stack from the beginning of the block at the given address.
  -- First argument is address of block.
  StackOffset :: !(Set Integer) -> AbsValue (BVType 64)
  -- An offset to the stack at some offset.
  SomeStackOffset :: AbsValue (BVType 64)
  -- | A strided interval
  StridedInterval :: !(SI.StridedInterval (BVType n)) -> AbsValue (BVType n)

  -- | A sub-value about which we know only some bits.
  -- (e.g., we know that the lower 8 bits are < 10)
  SubValue :: ((n + 1) <= n')
           => !(NatRepr n)
           -> !(AbsValue (BVType n))
           ->  AbsValue (BVType n')
  -- | Any value
  TopV :: AbsValue tp

-- | Denotes that we do not know of any value that could be in set.
emptyAbsValue :: AbsValue (BVType 64)
emptyAbsValue = CodePointers Set.empty

-- | Returns a finite set of values with some width.
data SomeFinSet tp where
  IsFin :: ValueSet -> SomeFinSet (BVType n)
  NotFin :: SomeFinSet tp

asFinSet :: String -> AbsValue tp -> SomeFinSet tp
asFinSet _ (FinSet s) = IsFin s
asFinSet nm (CodePointers s)
  | s == Set.singleton 0 = IsFin (Set.singleton 0)
  | otherwise = trace ("dropping Codeptr " ++ nm) $
      IsFin (Set.mapMonotonic toInteger s)
asFinSet _ _ = NotFin

asFinSet64 :: String -> AbsValue (BVType 64) -> Maybe (Set Word64)
asFinSet64 _ (FinSet s) = Just $! (Set.mapMonotonic fromInteger s)
asFinSet64 nm (CodePointers s)
  | isZeroPtr s = Just s
  | otherwise = trace ("dropping Codeptr " ++ nm) $ Just s
asFinSet64 _ _ = Nothing

codePointerSet :: AbsValue tp -> Set Word64
codePointerSet (CodePointers s) = s
codePointerSet _ = Set.empty

-- prop_SubValue :: AbsValue tp -> Bool
-- prop_SubValue (SubValue n v v') =
--   case (concretize v, concretize v') of
--    (_, Nothing)         -> True
--    -- FIXME: maybe? this just says all lower bits are set.  We could
--    -- expand out the Top, but that might be expensive for large n.
--    (Nothing, _)         -> False
--    (Just vs, Just vs')  -> vs `Set.isSubsetOf` (Set.map (toUnsigned n) vs')
-- prop_SubValue _ = True

-- | The maximum number of values we hold in a ValueSet, after which
-- we move to intervals
maxSetSize :: Int
maxSetSize = 5

-- Note that this is syntactic equality only.
instance Eq (AbsValue tp) where
  FinSet x    == FinSet y      = x == y
  CodePointers x == CodePointers y = x == y
  StackOffset ox  == StackOffset oy   = ox == oy
  SomeStackOffset == SomeStackOffset  = True
  StridedInterval si1 == StridedInterval si2 = si1 == si2
  SubValue n v == SubValue n' v'
    | Just Refl <- testEquality n n' = v == v'
    | otherwise = False
  TopV == TopV = True
  _    == _    = False

instance EqF AbsValue where
  eqF = (==)

instance Show (AbsValue tp) where
  show = show . pretty

instance Pretty (AbsValue tp) where
  pretty (FinSet s) = ppIntegerSet s
  pretty (CodePointers s) = text "code" <+> ppIntegerSet s
  pretty (StridedInterval s) = pretty s
  pretty (SubValue n av) = (pretty av) <> brackets (integer (natValue n))
  pretty (StackOffset     s) = text "rsp_0 +" <+> ppIntegerSet s
  pretty SomeStackOffset = text "rsp_0 + ?"
  pretty TopV = text "top"

ppIntegerSet :: (Show w, Integral w) => Set w -> Doc
ppIntegerSet vs = encloseSep lbrace rbrace comma (map ppv (Set.toList vs))
  where ppv v' = assert (v' >= 0) $ text ("0x" ++ showHex v' "")


-- | Returns a set of concrete integers that this value may be.
-- This function will neither return the complete set or an
-- known under-approximation.
concretize :: AbsValue tp -> Maybe (Set Integer)
concretize (FinSet s) = Just s
concretize (CodePointers s) = Just (Set.mapMonotonic toInteger s)
concretize (SubValue _ _) = Nothing -- we know nothing about _all_ values
concretize (StridedInterval s) =
  trace ("Concretizing " ++ show (pretty s)) $
  Just (Set.fromList (SI.toList s))
concretize _ = Nothing

-- FIXME: make total, we would need to carry around tp
size :: AbsValue tp -> Maybe Integer
size (FinSet s) = Just $ fromIntegral (Set.size s)
size (CodePointers s) = Just $ fromIntegral (Set.size s)
size (StridedInterval s) = Just $ SI.size s
size (StackOffset s) = Just $ fromIntegral (Set.size s)
size _ = Nothing

-- | Return single value is the abstract value can only take on one value.
asConcreteSingleton :: AbsValue tp -> Maybe Integer
asConcreteSingleton v = do
  sz <- size v
  guard (sz == 1)
  [i] <- Set.toList <$> concretize v
  return i

-- -----------------------------------------------------------------------------
-- Smart constructors

-- | Smart constructor for strided intervals which takes care of top
stridedInterval :: SI.StridedInterval (BVType tp) -> AbsValue (BVType tp)
stridedInterval si
  | SI.isTop si = TopV
  | otherwise   = StridedInterval si

-- | Smart constructor for sub-values.  This ensures that the
-- subvalues are sorted on size.
subValue :: ((n + 1) <= n') =>
            NatRepr n -> AbsValue (BVType n)
            -> AbsValue (BVType n')
subValue n v
  | TopV <- v = TopV
  | otherwise = SubValue n v

isEmpty :: AbsValue tp -> Bool
isEmpty (CodePointers s) = Set.null s
isEmpty (FinSet s) = Set.null s
isEmpty _ = False

-- -----------------------------------------------------------------------------
-- Instances

-- | Returns true if set just contains 0.
isZeroPtr :: Set Word64 -> Bool
isZeroPtr s = Set.size s == 1 && Set.findMin s == 0

-- | Join the old and new states and return the updated state iff
-- the result is larger than the old state.
-- This also returns any addresses that are discarded during joining.
joinAbsValue :: AbsValue tp -> AbsValue tp -> Maybe (AbsValue tp)
joinAbsValue x y
    | Set.null s = r
    | otherwise = trace ("dropping " ++ show (ppIntegerSet s) ++ "\n" ++ show x ++ "\n" ++ show y ++ "\n") r
  where (r,s) = runState (joinAbsValue' x y) Set.empty

addWords :: Set Word64 -> State (Set Word64) ()
addWords s = modify $ Set.union (Set.delete 0 s)

-- | Join the old and new states and return the updated state iff
-- the result is larger than the old state.
-- This also returns any addresses that are discarded during joining.
joinAbsValue' :: AbsValue tp -> AbsValue tp -> State (Set Word64) (Maybe (AbsValue tp))

joinAbsValue' TopV x = do
  addWords (codePointerSet x)
  return $! Nothing
joinAbsValue' x y | isEmpty y = return $ Nothing
                  | isEmpty x = return $ (Just $! y)
joinAbsValue' (CodePointers old) (CodePointers new)
    | new `Set.isSubsetOf` old = return $ Nothing
    | otherwise = return $ (Just $! CodePointers r)
  where r = Set.union old new
joinAbsValue' (FinSet old) (CodePointers new0)
    | isZeroPtr new0 =
      if Set.member 0 old then
        return $ Nothing
      else
        return $ (Just $! FinSet $ Set.insert 0 old)
    | new `Set.isSubsetOf` old = do
      addWords new0
      return $ Nothing
    | Set.size r > maxSetSize = do
      addWords new0
      return $ Just TopV
    | otherwise = do
      addWords new0
      return $ Just (FinSet r)

  where r = Set.union old new
        new = Set.mapMonotonic toInteger new0

joinAbsValue' (CodePointers old) (FinSet new)
    | isZeroPtr old = do
      return $ (Just $! FinSet (Set.insert 0 new))
    | Set.size r > maxSetSize = do
      addWords old
      return $ Just TopV
    | otherwise = do
      addWords old
      return $ Just (FinSet r)
  where r = Set.union (Set.mapMonotonic toInteger old) new
joinAbsValue' (FinSet old) (FinSet new)
    | new `Set.isSubsetOf` old = return $ Nothing
    | Set.size r > maxSetSize = return $ Just TopV
    | otherwise = return $ Just (FinSet r)
  where r = Set.union old new
joinAbsValue' (StackOffset old) (StackOffset new)
    | new `Set.isSubsetOf` old = return $ Nothing
    | Set.size r > maxSetSize = return $ Just TopV
    | otherwise = return $ Just (StackOffset r)
  where r = Set.union old new

-- Intervals
joinAbsValue' v v'
    | StridedInterval si_old <- v, StridedInterval si_new <- v'
    , si_new `SI.isSubsetOf` si_old =
      return $ Nothing
    | StridedInterval si_old <- v, StridedInterval si_new <- v' =
      return $ go si_old si_new
    | StridedInterval si <- v,  FinSet s <- v' =
      return $ go si (SI.fromFoldable (type_width (SI.typ si)) s)
    | StridedInterval si <- v,  CodePointers s <- v' = do
      addWords s
      return $ go si (SI.fromFoldable (type_width (SI.typ si)) (Set.mapMonotonic toInteger s))
    | StridedInterval si <- v', FinSet s <- v =
      return $ go si (SI.fromFoldable (type_width (SI.typ si)) s)
    | StridedInterval si <- v', CodePointers s <- v = do
      addWords s
      return $ go si (SI.fromFoldable (type_width (SI.typ si)) (Set.mapMonotonic toInteger s))
  where go si1 si2 = Just $ stridedInterval (SI.lub si1 si2)

-- Sub values
joinAbsValue' (SubValue n av) (SubValue n' av') =
  case testNatCases n n' of
    NatCaseLT LeqProof -> fmap (subValue n) <$> joinAbsValue' av (trunc av' n)
    NatCaseEQ          -> fmap (subValue n) <$> joinAbsValue' av av'
    NatCaseGT LeqProof -> do
      let new_av = trunc av n'
      mv <- joinAbsValue' new_av av'
      return $ Just $! subValue n' (fromMaybe new_av mv)
-- Join addresses
joinAbsValue' SomeStackOffset{} StackOffset{} = return $ Nothing
joinAbsValue' StackOffset{} new@SomeStackOffset = return $ Just new
joinAbsValue' SomeStackOffset{} SomeStackOffset{} = return $ Nothing

joinAbsValue' x y = do
  addWords (codePointerSet x)
  addWords (codePointerSet y)
  return $! Just TopV

member :: Integer -> AbsValue tp -> Bool
member n TopV = True
member n (FinSet s) = Set.member n s
member n (CodePointers s) | 0 <= n && n <= toInteger (maxBound :: Word64) =
  Set.member (fromInteger n) s
member n (StridedInterval si) = SI.member n si
member n (SubValue _n' v) = member n v
member _n _v = False


isBottom :: AbsValue tp -> Bool
isBottom (FinSet v)       = Set.null v
isBottom (CodePointers v) = Set.null v
isBottom (StackOffset v)  = Set.null v
isBottom SomeStackOffset = False
isBottom (StridedInterval v) = SI.size v == 0
isBottom (SubValue _ v) = isBottom v
isBottom TopV = False

-- meet is probably the wrong word here --- we are really refining the
-- abstract value based upon some new information.  Thus, we want to
-- return an overapproximation rather than an underapproximation of
-- the value.
-- Currently the only case we care about is where v' is an interval

-- @meet x y@ returns an over-approximation of the values in @x@ and @y@.
meet :: AbsValue tp -> AbsValue tp -> AbsValue tp
meet x y
  | isBottom m, not (isBottom x), not (isBottom y) =
      trace ("Got empty: " ++ show (pretty x) ++ " " ++ show (pretty y)) $ m
  | otherwise = m  
  where m = meet' x y
   
meet' :: AbsValue tp -> AbsValue tp -> AbsValue tp
meet' TopV x = x
meet' x TopV = x
-- FIXME: reuse an old value if possible?
meet' (CodePointers old) (CodePointers new) = CodePointers $ Set.intersection old new
--TODO: Fix below
meet' (asFinSet "meet" -> IsFin old) (asFinSet "meet" -> IsFin new) =
  FinSet $ Set.intersection old new
meet' (StackOffset old) (StackOffset new) =
  StackOffset $ Set.intersection old new

-- Intervals
meet' v v'
  | StridedInterval si_old <- v, StridedInterval si_new <- v'
    = stridedInterval $ si_old `SI.glb` si_new
  | StridedInterval si <- v,  IsFin s <- asFinSet "meet" v'
    = FinSet $ Set.filter (`SI.member` si) s
  | StridedInterval si <- v', IsFin s <- asFinSet "meet" v
    = FinSet $ Set.filter (`SI.member` si) s

-- These cases are currently sub-optimal: really we want to remove all
-- those from the larger set which don't have a prefix in the smaller
-- set.
meet' v v'
  | SubValue n av <- v, SubValue n' av' <- v' =
      case testNatCases n n' of
        NatCaseLT LeqProof -> subValue n av -- FIXME
        NatCaseEQ          -> subValue n (meet av av')
        NatCaseGT LeqProof -> subValue n' av' -- FIXME
  | SubValue n av <- v, IsFin s <- asFinSet "meet" v' =
      FinSet $ Set.filter (\x -> (toUnsigned n x) `member` av) s
  | SubValue n av <- v', IsFin s <- asFinSet "meet" v =
      FinSet $ Set.filter (\x -> (toUnsigned n x) `member` av) s
  | SubValue n av <- v, StridedInterval si <- v' = v' -- FIXME: ?
  | StridedInterval si <- v, SubValue n av <- v' = v -- FIXME: ?

-- Join addresses
meet' SomeStackOffset{} s@StackOffset{} = s
meet' s@StackOffset{} SomeStackOffset{} = s
meet' SomeStackOffset SomeStackOffset{} = SomeStackOffset
meet' x _ = x -- Arbitrarily pick one.
-- meet x y = error $ "meet: impossible" ++ show (x,y)

trunc :: (v+1 <= u)
      => AbsValue (BVType u)
      -> NatRepr v
      -> AbsValue (BVType v)
trunc (FinSet s) w       = FinSet (Set.map (toUnsigned w) s)
trunc (CodePointers s) w = FinSet (Set.map (toUnsigned w . toInteger) s)
trunc (StridedInterval s) w = stridedInterval (SI.trunc s w)
trunc (SubValue n av) w =
  case testNatCases n w of
   NatCaseLT LeqProof -> SubValue n av
   NatCaseEQ          -> av
   NatCaseGT LeqProof -> trunc av w
trunc (StackOffset _) _ = TopV
trunc SomeStackOffset _ = TopV
trunc TopV _ = TopV

uext :: forall u v.
        (u+1 <= v) => AbsValue (BVType u) -> NatRepr v -> AbsValue (BVType v)
uext (FinSet s) _ = FinSet s
uext (CodePointers s) _ = FinSet (Set.mapMonotonic toInteger s)
uext (StridedInterval si) w =
  StridedInterval $ si { SI.typ = BVTypeRepr w }
uext (SubValue (n :: NatRepr n) av) w =
  -- u + 1 <= v, n + 1 <= u, need n + 1 <= v
  -- proof: n + 1 <= u + 1 by addIsLeq
  --        u + 1 <= v     by assumption
  --        n + 1 <= v     by transitivity
  case proof of
    LeqProof -> SubValue n av
  where
    proof :: LeqProof (n + 1) v
    proof = leqTrans (leqAdd (LeqProof :: LeqProof (n+1) u) n1) (LeqProof :: LeqProof (u + 1) v)
uext (StackOffset _) _ = TopV
uext SomeStackOffset _ = TopV
uext TopV _ = TopV

bvadd :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
-- Stacks
bvadd w (StackOffset s) (FinSet t) | [o] <- Set.toList t = do
  StackOffset $ Set.map (addOff w o) s
bvadd w (FinSet t) (StackOffset s) | [o] <- Set.toList t = do
  StackOffset $ Set.map (addOff w o) s
-- Strided intervals
bvadd w v v'
  | StridedInterval si1 <- v, StridedInterval si2 <- v' = go si1 si2
  | StridedInterval si <- v,  IsFin s <- asFinSet "bvadd" v' = go si (SI.fromFoldable w s)
  | StridedInterval si <- v', IsFin s <- asFinSet "bvadd" v  = go si (SI.fromFoldable w s)
  where
    go si1 si2 = stridedInterval $ SI.bvadd w si1 si2

-- subvalues
-- bvadd w (SubValue _n _av c) v' = bvadd w c v'
-- bvadd w v (SubValue _n _av c)  = bvadd w v c

-- the rest
bvadd _ StackOffset{} _ = SomeStackOffset
bvadd _ _ StackOffset{} = SomeStackOffset
bvadd _ SomeStackOffset _ = SomeStackOffset
bvadd _ _ SomeStackOffset = SomeStackOffset
bvadd _ _ _ = TopV

setL :: ([Integer] -> AbsValue (BVType n))
     -> (Set Integer -> AbsValue (BVType n))
     -> [Integer]
     -> AbsValue (BVType n)
setL def c l | length l > maxSetSize = def l
             | otherwise = c (Set.fromList l)

bvsub :: Memory Word64
      -> NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
bvsub mem w (CodePointers s) (asFinSet "bvsub2" -> IsFin t)
    | all (isCodeAddrOrNull mem) vals = CodePointers (Set.fromList vals)
    | isZeroPtr s = FinSet (Set.map (toUnsigned w . negate) t)
    | otherwise = error "Losing code pointers due to bvsub."
      -- TODO: Fix this.
--      trace ("drooping " ++ show (ppIntegerSet s) ++ " " ++ show (ppIntegerSet t)) $
--        setL (stridedInterval . SI.fromFoldable w) FinSet (toInteger <$> vals)
  where vals :: [Word64]
        vals = do
          x <- Set.toList s
          y <- Set.toList t
          return $! x - fromInteger y
bvsub _ w (FinSet s) (asFinSet "bvsub3" -> IsFin t) =
  setL (stridedInterval . SI.fromFoldable w) FinSet $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x - y))
bvsub _ w v v'
  | StridedInterval si1 <- v, StridedInterval si2 <- v' = go si1 si2
  | StridedInterval si <- v,  IsFin s <- asFinSet "bvsub4" v' = go si (SI.fromFoldable w s)
  | StridedInterval si <- v', IsFin s <- asFinSet "bvsub5" v  = go si (SI.fromFoldable w s)
  where
    go _si1 _si2 = TopV -- FIXME
bvsub _ w (StackOffset s) (asFinSet "bvsub6" -> IsFin t) = setL (const SomeStackOffset) StackOffset $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x - y))

-- subvalues
-- bvsub w (SubValue _n _av c) v' = bvsub w c v'
-- bvsub w v (SubValue _n _av c)  = bvsub w v c

bvsub _ _ StackOffset{} _ = SomeStackOffset
bvsub _ _ _ StackOffset{} = TopV
bvsub _ _ SomeStackOffset _ = SomeStackOffset
bvsub _ _ _ SomeStackOffset = TopV
bvsub _ _ _ _ = TopV -- Keep the pattern checker happy
-- bvsub _ TopV _ = TopV
-- bvsub _ _ TopV = TopV

bvmul :: NatRepr u
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
      -> AbsValue (BVType u)
bvmul w (asFinSet "bvmul" -> IsFin s) (asFinSet "bvmul" -> IsFin t) =
  setL (stridedInterval . SI.fromFoldable w) FinSet $ do
  x <- Set.toList s
  y <- Set.toList t
  return (toUnsigned w (x * y))
bvmul w v v'
  | StridedInterval si1 <- v, StridedInterval si2 <- v' = go si1 si2
  | StridedInterval si <- v,  IsFin s <- asFinSet "bvmul" v' = go si (SI.fromFoldable w s)
  | StridedInterval si <- v', IsFin s <- asFinSet "bvmul" v  = go si (SI.fromFoldable w s)
  where
    go si1 si2 = stridedInterval $ SI.bvmul w si1 si2

-- bvmul w (SubValue _n _av c) v' = bvmul w c v'
-- bvmul w v (SubValue _n _av c)  = bvmul w v c

bvmul _ _ _ = TopV

ppAbsValue :: AbsValue tp -> Maybe Doc
ppAbsValue TopV = Nothing
ppAbsValue v = Just (pretty v)

-- | Print a list of Docs vertically separated.
instance PrettyRegValue AbsValue where
  ppValueEq _ TopV = Nothing
  ppValueEq r v = Just (text (show r) <+> text "=" <+> pretty v)

-- | Indicates if address is an address in code segment or null.
isCodeAddrOrNull :: Memory Word64 -> Word64 -> Bool
isCodeAddrOrNull _ 0 = True
isCodeAddrOrNull mem a = isCodeAddr mem a

abstractSingleton :: Memory Word64 -> NatRepr n -> Integer -> AbsValue (BVType n)
abstractSingleton mem w i
  | Just Refl <- testEquality w n64
  , 0 <= i && i <= maxUnsigned w
  , isCodeAddrOrNull mem (fromIntegral i) =
    CodePointers (Set.singleton (fromIntegral i))
  | 0 <= i && i <= maxUnsigned w = FinSet (Set.singleton i)
  | otherwise = error $ "abstractSingleton given bad value: " ++ show i ++ " " ++ show w

concreteStackOffset :: Integer -> AbsValue (BVType 64)
concreteStackOffset o = StackOffset (Set.singleton o)

------------------------------------------------------------------------
-- Restrictions

hasMaximum :: TypeRepr tp -> AbsValue tp -> Maybe Integer
hasMaximum tp v =
  case v of
   FinSet s | Set.null s -> Nothing
            | otherwise  -> Just $! Set.findMax s
   CodePointers s | Set.null s -> Nothing
                  | otherwise  -> Just $! (toInteger (Set.findMax s))
   StridedInterval si -> Just (SI.intervalEnd si)
   TopV               -> Just $ case tp of BVTypeRepr n -> maxUnsigned n
   _                  -> Nothing


hasMinimum :: TypeRepr tp -> AbsValue tp -> Maybe Integer
hasMinimum _tp v =
  case v of
   FinSet s       | Set.null s -> Nothing
                  | otherwise  -> Just $! Set.findMin s
   CodePointers s | Set.null s -> Nothing
                  | otherwise  -> Just $! toInteger (Set.findMin s)
   StridedInterval si -> Just $! SI.base si
   _                  -> Just 0

-- | @abstractULt x y@ refines x and y with the knowledge that @x < y@
-- is unsigned.
-- For example, given {2, 3} and {2, 3, 4}, we know (only) that
-- {2, 3} and {3, 4} because we may pick any element from either set.

abstractULt :: TypeRepr tp
              -> AbsValue tp -> AbsValue tp
              -> (AbsValue tp, AbsValue tp)
abstractULt _tp TopV TopV = (TopV, TopV)
abstractULt tp x y
  | Just u_y <- hasMaximum tp y
  , Just l_x <- hasMinimum tp x
  , BVTypeRepr n <- tp =
    trace' ("abstractLt " ++ show (pretty x) ++ " " ++ show (pretty y) ++ " -> ")
    ( meet x (stridedInterval $ SI.mkStridedInterval tp False 0 (u_y - 1) 1)
    , meet y (stridedInterval $ SI.mkStridedInterval tp False (l_x + 1)
                                                     (maxUnsigned n) 1))
    where trace' m x = trace (m ++ show x) x
          
abstractULt _tp x y = (x, y)

-- | @abstractULeq x y@ refines x and y with the knowledge that @x <= y@
abstractULeq :: TypeRepr tp
               -> AbsValue tp -> AbsValue tp
               -> (AbsValue tp, AbsValue tp)
abstractULeq _tp TopV TopV = (TopV, TopV)
abstractULeq tp x y
  | Just u_y <- hasMaximum tp y
  , Just l_x <- hasMinimum tp x
  , BVTypeRepr n <- tp =
    trace' ("abstractLeq " ++ show (pretty x) ++ " " ++ show (pretty y) ++ " -> ")
    ( meet x (stridedInterval $ SI.mkStridedInterval tp False 0 u_y 1)
    , meet y (stridedInterval $ SI.mkStridedInterval tp False l_x
                                                     (maxUnsigned n) 1))
    where trace' m x = trace (m ++ show x) x
    
abstractULeq _tp x y = (x, y)

------------------------------------------------------------------------
-- AbsBlockState

data StackEntry where
  StackEntry :: TypeRepr tp -> AbsValue tp -> StackEntry

instance Eq StackEntry where
  StackEntry x_tp x_v == StackEntry y_tp y_v
    | Just Refl <- testEquality x_tp y_tp = x_v == y_v
    | otherwise = False

-- | The AbsBlockStack describes offsets of the stack.
-- Values that are not in the map may denote any values.
-- The stack grows down, so nonegative keys are those within
-- rsp.
type AbsBlockStack = Map Integer StackEntry

absStackLeq :: AbsBlockStack -> AbsBlockStack -> Bool
absStackLeq x y = all entryLeq (Map.toList y)
  where entryLeq (o, StackEntry y_tp y_v) =
          case Map.lookup o x of
            Just (StackEntry x_tp x_v) | Just Refl <- testEquality x_tp y_tp ->
              isNothing (joinAbsValue y_v x_v)
            _ -> False

-- | @absStackJoinD x y@ returns the stack containing the union @z@ of the
-- values in @x@ and @y@.  It sets the first state parameter to true if @z@
-- is different from @x@ and adds and escaped code pointers to the second
-- parameter.
absStackJoinD :: AbsBlockStack -> AbsBlockStack -> State (Bool,Set Word64) AbsBlockStack
absStackJoinD y x = do
  let entryLeq (o, StackEntry y_tp y_v) =
        case Map.lookup o x of
          Just (StackEntry x_tp x_v) | Just Refl <- testEquality x_tp y_tp -> do
            s <- use _2
            case runState (joinAbsValue' y_v x_v) s of
              (Nothing,  s') -> do
                _2 .= s'
                return $ Just (o, StackEntry y_tp y_v)
              (Just z_v, s') -> do
                _1 .= True
                _2 .= s'
                return $ Just (o, StackEntry y_tp z_v)
          _ -> do
            _1 .= True
            _2 %= Set.union (Set.delete 0 (codePointerSet y_v))
            return Nothing
  z <- mapM entryLeq (Map.toList y)
  return $ Map.fromList (catMaybes z)


absStackLub :: AbsBlockStack -> AbsBlockStack -> AbsBlockStack
absStackLub = Map.mergeWithKey merge (\_ -> Map.empty) (\_ -> Map.empty)
  where merge :: Integer -> StackEntry -> StackEntry -> Maybe StackEntry
        merge _ (StackEntry x_tp x_v) (StackEntry y_tp y_v) =
          case testEquality x_tp y_tp of
            Just Refl ->
              case joinAbsValue x_v y_v of
                Nothing | x_v == TopV -> Nothing

                        | otherwise -> Just (StackEntry x_tp x_v)
                Just TopV -> Nothing
                Just v -> Just (StackEntry x_tp v)
            Nothing -> Nothing

ppAbsStack :: AbsBlockStack -> Doc
ppAbsStack m = vcat (pp <$> Map.toList m)
  where pp (o,StackEntry _ v) = text (show o) <+> text ":=" <+> pretty v

-- | State at beginning of a block.
data AbsBlockState
      = AbsBlockState { _absX86State :: !(X86State AbsValue)
                      , _startAbsStack :: !AbsBlockStack
                      }
  deriving Eq


absX86State :: Simple Lens AbsBlockState (X86State AbsValue)
absX86State = lens _absX86State (\s v -> s { _absX86State = v })

startAbsStack :: Simple Lens AbsBlockState AbsBlockStack
startAbsStack = lens _startAbsStack (\s v -> s { _startAbsStack = v })

traceUnless :: Bool -> String -> a -> a
traceUnless True _ = id
traceUnless False msg = trace msg

instance AbsDomain AbsBlockState where
  top = AbsBlockState { _absX86State = mkX86State (\_ -> TopV)
                      , _startAbsStack = Map.empty
                      }

  joinD x y | regs_changed = Just $! z
            | otherwise = Nothing
    where xs = x^.absX86State
          ys = y^.absX86State

          x_stk = x^.startAbsStack
          y_stk = y^.startAbsStack

          (zs,(regs_changed,dropped)) = flip runState (False, Set.empty) $ do
            z_regs <- mkX86StateM $ \r -> do
              let xr = xs^.register r
              (c,s) <- get
              case runState (joinAbsValue' xr (ys^.register r)) s of
                (Nothing,s') -> do
                  seq s' $ put $ (c,s')
                  return $! xr
                (Just zr,s') -> do
                  seq s' $ put $ (True,s')
                  return $! zr
            z_stk <- absStackJoinD x_stk y_stk
            return $ AbsBlockState { _absX86State   = z_regs
                                   , _startAbsStack = z_stk
                                   }

          z = traceUnless (Set.null dropped)
                ("dropped abs " ++ show (ppIntegerSet dropped) ++ "\n"
                                ++ show x ++ "\n" ++ show y) $
              zs

instance Pretty AbsBlockState where
  pretty s =
      text "registers:" <$$>
      indent 2 (pretty (s^.absX86State)) <$$>
      stack_d
    where stack = s^.startAbsStack
          stack_d | Map.null stack = empty
                  | otherwise = text "stack:" <$$>
                                indent 2 (ppAbsStack (s^.startAbsStack))

instance Show AbsBlockState where
  show s = show (pretty s)


absBlockDiff :: AbsBlockState -> AbsBlockState -> [Some N.RegisterName]
absBlockDiff x y = filter isDifferent x86StateRegisters
  where isDifferent (Some n) = x^.absX86State^.register n /= y^.absX86State^.register n

------------------------------------------------------------------------
-- AbsProcessorState

-- | this1 stores the abstract state of the system at a given point in time.
data AbsProcessorState = AbsProcessorState { absMem :: !(Memory Word64)
                       , _absInitialRegs :: !(X86State AbsValue)
                       , _absAssignments :: !(MapF Assignment AbsValue)
                       , _curAbsStack :: !AbsBlockStack
                       }

-- FIXME
instance Pretty AbsProcessorState where
  pretty regs = pretty (AbsBlockState { _absX86State   = regs ^. absInitialRegs
                                      , _startAbsStack = regs ^. curAbsStack })



initAbsProcessorState :: Memory Word64 -> AbsBlockState -> AbsProcessorState
initAbsProcessorState mem s = AbsProcessorState { absMem = mem
                            , _absInitialRegs = s^.absX86State
                            , _absAssignments = MapF.empty
                            , _curAbsStack = s^.startAbsStack
                            }


absInitialRegs :: Simple Lens AbsProcessorState (X86State AbsValue)
absInitialRegs = lens _absInitialRegs (\s v -> s { _absInitialRegs = v })

absAssignments :: Simple Lens AbsProcessorState (MapF Assignment AbsValue)
absAssignments = lens _absAssignments (\s v -> s { _absAssignments = v })

curAbsStack :: Simple Lens AbsProcessorState AbsBlockStack
curAbsStack = lens _curAbsStack (\s v -> s { _curAbsStack = v })

-- | A lens that allows one to lookup and update the value of an assignment in
-- map from assignments to abstract values.
assignLens :: Assignment tp
           -> Simple Lens (MapF Assignment AbsValue) (AbsValue tp)
assignLens ass = lens (fromMaybe TopV . MapF.lookup ass)
                      (\s v -> MapF.insert ass v s)

-- | Merge in the value of the assignment.  If we have already seen a
-- value, this will combine with meet.
addAssignment :: Assignment tp -> AbsProcessorState -> AbsProcessorState
addAssignment a c =
  c & (absAssignments . assignLens a) %~ flip meet (transferRHS c (assignRhs a))

deleteRange :: Integer -> Integer -> Map Integer v -> Map Integer v
deleteRange l h m
  | h < l = m
  | otherwise =
    case Map.lookupGE l m of
      Just (k,_) | k <= h -> deleteRange (k+1) h (Map.delete k m)
      _ -> m

someValueWidth :: Value tp -> Integer
someValueWidth v =
  case valueType v of
    BVTypeRepr w -> natValue w

addMemWrite :: Value (BVType 64) -> Value tp -> AbsProcessorState -> AbsProcessorState
addMemWrite a v r =
  case (transferValue r a, transferValue r v) of
    -- (_,TopV) -> r
    -- We overwrite _some_ stack location.  An alternative would be to
    -- update everything with v.
    (SomeStackOffset, _) ->
      drop' " in SomeStackOffset case"
    (st@(StackOffset s), _) | Set.size s > 1 ->
      let w = someValueWidth v
      in  r & curAbsStack %~ flip (Set.fold (\o m -> deleteRange o (o+w-1) m)) s
    (StackOffset s, TopV) | [o] <- Set.toList s -> do
      let w = someValueWidth v
       in r & curAbsStack %~ deleteRange o (o+w-1)           
    (StackOffset s, v_abs) | [o] <- Set.toList s -> do
      let w = someValueWidth v
          e = StackEntry (valueType v) v_abs
       in r & curAbsStack %~ Map.insert o e . deleteRange o (o+w-1)
    -- FIXME: nuke stack on an unknown address or Top?
    _ -> r
  where drop' msg = 
          trace ("addMemWrite: dropping stack at "
                 ++ show (pretty $ r ^. absInitialRegs ^. curIP)
                 ++ " via " ++ show (pretty a)
                 ++ msg) $
          r & curAbsStack .~ Map.empty


addOff :: NatRepr w -> Integer -> Integer -> Integer
addOff w o v = toUnsigned w (o + v)

subOff :: NatRepr w -> Integer -> Integer -> Integer
subOff w o v = toUnsigned w (o - v)

resetRSP :: (N.RegisterName cl -> AbsValue (N.RegisterType cl))
         -> (N.RegisterName cl -> AbsValue (N.RegisterType cl))
resetRSP otherFn r
  | Just Refl <- testEquality r N.rsp = concreteStackOffset 0
  | otherwise = otherFn r

mkAbsBlockState :: (forall cl . N.RegisterName cl -> AbsValue (N.RegisterType cl))
                -> AbsBlockStack
                -> AbsBlockState
mkAbsBlockState trans newStack =
  AbsBlockState { _absX86State = mkX86State (resetRSP trans)
                , _startAbsStack = newStack
                }

-- | Return state for after value has run.
finalAbsBlockState :: AbsProcessorState -> X86State Value -> AbsBlockState
finalAbsBlockState c s = do
  case transferValue c (s^.register N.rsp) of
    --
    StackOffset offsets | [0] <- Set.toList offsets ->
      let transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
          transferReg r = transferValue c (s^.register r)
       in mkAbsBlockState transferReg (c^.curAbsStack)
    StackOffset offsets
      | [o] <- Set.toList offsets ->
        shiftSpecificOffset (\r -> transferValue c (s^.register r)) (c^.curAbsStack) o
      | otherwise ->
        shiftSomeOffset (\r -> transferValue c (s^.register r))
    SomeStackOffset -> shiftSomeOffset (\r -> transferValue c (s^.register r))
    _ ->
      let transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
          transferReg r =
            case transferValue c (s^.register r) of
              StackOffset _ -> TopV
              SomeStackOffset -> TopV
              v -> v
       in mkAbsBlockState transferReg Map.empty


shiftSpecificOffset :: (forall cl . N.RegisterName cl -> AbsValue (N.RegisterType cl))
                       -- ^ Function for getting register values.
                    -> AbsBlockStack
                       -- ^ Stack before shift.
                    -> Integer
                       -- ^ Amount to shift offset by
                    -> AbsBlockState
shiftSpecificOffset f stk o = mkAbsBlockState transferReg newStack
  where transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
        transferReg r =
          case f r of
            StackOffset t -> StackOffset (Set.map (\v -> subOff n64 v o) t)
            v -> v
        newStack = Map.fromList $
            [ (subOff n64 a o, v) | (a,v) <- Map.toList stk ]

shiftSomeOffset :: (forall cl . N.RegisterName cl -> AbsValue (N.RegisterType cl))
                -> AbsBlockState
shiftSomeOffset f = mkAbsBlockState transferReg Map.empty
  where transferReg :: N.RegisterName cl -> AbsValue (N.RegisterType cl)
        transferReg r =
          case f r of
            StackOffset _ -> SomeStackOffset
            v -> v

-- | Update the block state to point to a specific IP address.
setAbsIP :: Memory Word64 -> CodeAddr -> AbsBlockState -> AbsBlockState
setAbsIP mem a b
  | not (isCodeAddrOrNull mem a) =
    error "setAbsIP given address that is not a code pointer."
    -- Check to avoid reassigning next IP if it is not needed.
  | CodePointers s <- b^.absX86State^.curIP
  , Set.size s == 1
  , Set.member a s =
    b
  | otherwise =
    b & absX86State . curIP .~ CodePointers (Set.singleton a)

------------------------------------------------------------------------
-- Transfer functions

-- | Compute abstract value from value and current registers.
transferValue :: AbsProcessorState
              -> Value tp
              -> AbsValue tp
transferValue c v =
  case v of
   BVValue w i
     | 0 <= i && i <= maxUnsigned w -> abstractSingleton (absMem c) w i
     | otherwise -> error $ "transferValue given illegal value " ++ show (pretty v)
   -- Invariant: v is in m
   AssignedValue a ->
     fromMaybe (error $ "Missing assignment for " ++ show (assignId a))
               (MapF.lookup a (c^.absAssignments))
   Initial r
--     | Just Refl <- testEquality r N.rsp -> do
--       StackOffset (Set.singleton 0)
     | otherwise -> c ^. absInitialRegs ^. register r

transferApp :: AbsProcessorState
            -> App Value tp
            -> AbsValue tp
transferApp r a =
  case a of
    Trunc v w -> trunc (transferValue r v) w
    UExt  v w -> uext  (transferValue r v) w
    BVAdd w x y -> bvadd w (transferValue r x) (transferValue r y)
    BVSub w x y -> bvsub (absMem r) w (transferValue r x) (transferValue r y)
    BVMul w x y -> bvmul w (transferValue r x) (transferValue r y)
    _ -> TopV

transferRHS :: forall tp
            .  AbsProcessorState
            -> AssignRhs tp
            -> AbsValue tp
transferRHS r rhs =
  case rhs of
    EvalApp app    -> transferApp r app
    SetUndefined _ -> TopV
    Read (MemLoc a tp)
      | StackOffset s <- transferValue r a
      , [o] <- Set.toList s
      , Just (StackEntry v_tp v) <- Map.lookup o (r^.curAbsStack)
      , Just Refl <- testEquality tp v_tp ->
         v
    Read _ -> TopV
    -- We know only that it will return up to (and including(?)) cnt
    MemCmp _sz cnt _src _dest _rev
      | Just upper <- hasMaximum knownType (transferValue r cnt)
        -> stridedInterval $ SI.mkStridedInterval knownType False 0 upper 1
      | otherwise -> TopV
