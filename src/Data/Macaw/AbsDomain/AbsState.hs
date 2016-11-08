{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Werror #-}
module Data.Macaw.AbsDomain.AbsState
  ( AbsBlockState
  , setAbsIP
  , mkAbsBlockState
  , absRegState
  , absStackHasReturnAddr
  , AbsBlockStack
  , StackEntry(..)
  , ArchAbsValue
  , AbsValue(..)
  , bvadd
  , emptyAbsValue
  , joinAbsValue
  , ppAbsValue
  , absTrue
  , absFalse
  , subValue
  , concreteStackOffset
  , concretize
  , asConcreteSingleton
  , meet
  , absValueSize
  , codePointerSet
  , AbsDomain(..)
  , AbsProcessorState
  , curAbsStack
  , absInitialRegs
  , startAbsStack
  , initAbsProcessorState
  , absAssignments
  , assignLens
  , stridedInterval
  , finalAbsBlockState
  , addMemWrite
  , transferValue
  , transferValue'
  , abstractULt
  , abstractULeq
  , isBottom
  , transferApp
    -- * Utilities
  , hasMaximum
  ) where

import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.Foldable
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Classes (EqF(..), OrdF(..), ShowF(..))
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.Macaw.AbsDomain.StridedInterval as SI
import           Data.Macaw.CFG
import           Data.Macaw.DebugLogging
import           Data.Macaw.Memory
import qualified Data.Macaw.Memory.Permissions as Perm
import           Data.Macaw.Types

------------------------------------------------------------------------
-- Utilities

addOff :: NatRepr w -> Integer -> Integer -> Integer
addOff w o v = toUnsigned w (o + v)

------------------------------------------------------------------------
-- AbsDomain

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

------------------------------------------------------------------------
-- AbsValue

-- | The abstract information that is associated with values of a given type.
--
-- The first parameter is the width of pointers on the value.  It is expected
-- to be at most 64 bits.
data AbsValue w (tp :: Type)
  = forall n . (tp ~ BVType n) => FinSet !(Set Integer)
    -- ^ Denotes that this value can take any one of the fixed set.
  | (tp ~ BVType w) => CodePointers !(Set (SegmentedAddr w)) !Bool
     -- ^ A possibly empty set of values that either point to a code segment.
     -- This Boolean indicates whether this set contains the address 0.
  | (tp ~ BVType w) => StackOffset !(SegmentedAddr w) !(Set Int64)
    -- ^ Offset of stack from the beginning of the block at the given address.
    --  First argument is address of block.
  | (tp ~ BVType w) => SomeStackOffset !(SegmentedAddr w)
    -- ^ An offset to the stack at some offset.
  | forall n . (tp ~ BVType n) => StridedInterval !(SI.StridedInterval n)
    -- ^ A strided interval
  | forall n n'
    . ((n + 1) <= n', tp ~ BVType n')
    => SubValue !(NatRepr n) !(AbsValue w (BVType n))
    -- ^ A sub-value about which we know only some bits.
    -- (e.g., we know that the lower 8 bits are < 10)
  | TopV
    -- ^ Any value
  | (tp ~ BVType w) => ReturnAddr
    -- ^ Denotes a return address in the body of a function.

-- | Denotes that we do not know of any value that could be in set.
emptyAbsValue :: AbsValue w (BVType w)
emptyAbsValue = CodePointers Set.empty False

-- | Returns a finite set of values with some width.
data SomeFinSet tp where
  IsFin :: !(Set Integer) -> SomeFinSet (BVType n)
  NotFin :: SomeFinSet tp

-- | Given a segmented addr and flag indicating if zero is contained return the underlying
-- integer set and the set of addresses that had no base.
partitionAbsoluteAddrs :: Integral (MemWord w)
                        => Set (SegmentedAddr w)
                        -> Bool
                        -> (Set Integer, Set (SegmentedAddr w))
partitionAbsoluteAddrs addrSet b = foldl' f (s0, Set.empty) addrSet
   where s0 = if b then Set.singleton 0 else Set.empty
         f (intSet,badSet) addr =
           case segmentBase (addrSegment addr) of
             Just base -> seq intSet' $ (intSet', badSet)
               where intSet' = Set.insert w intSet
                     w = toInteger base + toInteger (addr^.addrOffset)
             Nothing -> seq badSet' $ (intSet, badSet')
               where badSet' = Set.insert addr badSet

asFinSet :: forall w tp
         .  Integral (MemWord w)
         => String
         -> AbsValue w tp
         -> SomeFinSet tp
asFinSet _ (FinSet s) = IsFin s
asFinSet _ (CodePointers s False)
  | Set.null s = IsFin Set.empty
asFinSet _ (CodePointers s True)
  | Set.null s = IsFin (Set.singleton 0)
asFinSet nm (CodePointers addrSet b) = go (Set.toList addrSet) $! s0
  where s0 = if b then Set.singleton 0 else Set.empty
        go :: [SegmentedAddr w] -> Set Integer -> SomeFinSet (BVType w)
        go [] s = debug DAbsInt ("dropping Codeptr " ++ nm) $ IsFin s
        go (a:r) s =
          case segmentBase (addrSegment a) of
            Just base -> go r $! s'
              where v = toInteger base + toInteger (a^.addrOffset)
                    s' = Set.insert v s
            Nothing -> NotFin
asFinSet _ _ = NotFin

-- asFinSet64 :: String -> AbsValue (BVType 64) -> Maybe (Set Word64)
-- asFinSet64 _ (FinSet s) = Just $! (Set.mapMonotonic fromInteger s)
-- asFinSet64 nm (CodePointers s)
--   | isZeroPtr s = Just s
--   | otherwise = debug DAbsInt ("dropping Codeptr " ++ nm) $ Just s
-- asFinSet64 _ _ = Nothing

codePointerSet :: AbsValue w tp -> Set (SegmentedAddr w)
codePointerSet (CodePointers s _) = s
codePointerSet _ = Set.empty

-- | The maximum number of values we hold in a value set, after which we move to
-- intervals
maxSetSize :: Int
maxSetSize = 5

-- Note that this is syntactic equality only.
instance Eq (AbsValue w tp) where
  FinSet x    == FinSet y      = x == y
  CodePointers x xb == CodePointers y yb = x == y && xb == yb
  StackOffset ax ox  == StackOffset ay oy   = (ax,ox) == (ay,oy)
  SomeStackOffset ax == SomeStackOffset ay = ax == ay
  StridedInterval si1 == StridedInterval si2 = si1 == si2
  SubValue n v == SubValue n' v'
    | Just Refl <- testEquality n n' = v == v'
    | otherwise = False
  TopV == TopV = True
  ReturnAddr == ReturnAddr = True
  _    == _    = False

instance EqF (AbsValue w) where
  eqF = (==)

instance Show (AbsValue w tp) where
  show = show . pretty

instance Pretty (AbsValue w tp) where
  pretty (FinSet s) = text "finset" <+> ppIntegerSet s
  pretty (CodePointers s b) = text "code" <+> ppSet (s0 ++ sd)
    where s0 = if b then [text "0"] else []
          sd = f <$> Set.toList s
          f segAddr = text (show segAddr)

  pretty (StridedInterval s) =
    text "strided" <> parens (pretty s)
  pretty (SubValue n av) =
    text "sub" <> parens (integer (natValue n) <> comma <+> pretty av)
  pretty (StackOffset a s) = ppSet (ppv <$> Set.toList s)
    where ppv v' | v' >= 0   = text $ "rsp_" ++ show a ++ " + " ++ showHex v' ""
                 | otherwise = text $ "rsp_" ++ show a ++ " - " ++ showHex (negate v') ""

  pretty (SomeStackOffset a) = text $ "rsp_" ++ show a ++ " + ?"
  pretty TopV = text "top"
  pretty ReturnAddr = text "return_addr"

ppSet :: [Doc] -> Doc
ppSet = encloseSep lbrace rbrace comma

ppIntegerSet :: (Integral w, Show w) => Set w -> Doc
ppIntegerSet s = ppSet (ppv <$> Set.toList s)
  where ppv v' = assert (v' >= 0) $ text (showHex v' "")

-- | Returns a set of concrete integers that this value may be.
-- This function will neither return the complete set or an
-- known under-approximation.
concretize :: Integral (MemWord w) => AbsValue w tp -> Maybe (Set Integer)
concretize (FinSet s) = Just s
concretize (CodePointers s b) = Just $ Set.fromList $
  [ toInteger base + toInteger (addr^.addrOffset)
  | addr <- Set.toList s
  , base <- maybeToList (segmentBase (addrSegment addr))
  ]
  ++ (if b then [0] else [])
concretize (SubValue _ _) = Nothing -- we know nothing about _all_ values
concretize (StridedInterval s) =
  debug DAbsInt ("Concretizing " ++ show (pretty s)) $
  Just (Set.fromList (SI.toList s))
concretize _ = Nothing

-- FIXME: make total, we would need to carry around tp
absValueSize :: AbsValue w tp -> Maybe Integer
absValueSize (FinSet s) = Just $ fromIntegral (Set.size s)
absValueSize (CodePointers s b) = Just $ fromIntegral (Set.size s) + (if b then 1 else 0)
absValueSize (StridedInterval s) = Just $ SI.size s
absValueSize (StackOffset _ s) = Just $ fromIntegral (Set.size s)
absValueSize _ = Nothing

-- | Return single value is the abstract value can only take on one value.
asConcreteSingleton :: Integral (MemWord w) => AbsValue w tp -> Maybe Integer
asConcreteSingleton v = do
  sz <- absValueSize v
  guard (sz == 1)
  [i] <- Set.toList <$> concretize v
  return i

-- -----------------------------------------------------------------------------
-- Smart constructors

-- | Smart constructor for strided intervals which takes care of top
stridedInterval :: SI.StridedInterval n -> AbsValue w (BVType n)
stridedInterval si
  | SI.isTop si = TopV
  | otherwise   = StridedInterval si

-- | Smart constructor for sub-values.  This ensures that the
-- subvalues are sorted on size.
subValue :: ((n + 1) <= n')
         => NatRepr n
         -> AbsValue w (BVType n)
         -> AbsValue w (BVType n')
subValue n v
  | TopV <- v = TopV
  | otherwise = SubValue n v

isEmpty :: AbsValue w tp -> Bool
isEmpty (CodePointers s b) = Set.null s && not b
isEmpty (FinSet s) = Set.null s
isEmpty _ = False

-- -----------------------------------------------------------------------------
-- Instances

{-
-- | Returns true if set just contains 0.
isZeroPtr :: Set Word64 -> Bool
isZeroPtr s = Set.size s == 1 && Set.findMin s == 0
-}

-------------------------------------------------------------------------------
-- Joining abstract values

ppSegAddrSet :: Set (SegmentedAddr w) -> Doc
ppSegAddrSet s =  ppSet (text . show <$> Set.toList s)

-- | Join the old and new states and return the updated state iff
-- the result is larger than the old state.
-- This also returns any addresses that are discarded during joining.
joinAbsValue :: Integral (MemWord w)
             => AbsValue w tp
             -> AbsValue w tp
             -> Maybe (AbsValue w tp)
joinAbsValue x y
    | Set.null s = r
    | otherwise = debug DAbsInt ("dropping " ++ show dropped ++ "\n" ++ show x ++ "\n" ++ show y ++ "\n") r
  where (r,s) = runState (joinAbsValue' x y) Set.empty
        dropped = ppSegAddrSet s

addWords :: Set (SegmentedAddr w) -> State (Set (SegmentedAddr w)) ()
addWords s = modify $ Set.union s

-- | Join the old and new states and return the updated state iff
-- the result is larger than the old state.
-- This also returns any addresses that are discarded during joining.
joinAbsValue' :: Integral (MemWord w)
              => AbsValue w tp
              -> AbsValue w tp
              -> State (Set (SegmentedAddr w)) (Maybe (AbsValue w tp))
joinAbsValue' TopV x = do
  addWords (codePointerSet x)
  return $! Nothing
joinAbsValue' x y | isEmpty y = return $ Nothing
                  | isEmpty x = return $ (Just $! y)
joinAbsValue' (CodePointers old old_b) (CodePointers new new_b)
    | new `Set.isSubsetOf` old && (not new_b || old_b) = return $ Nothing
    | otherwise = return $ (Just $! CodePointers r (old_b || new_b))
  where r = Set.union old new
joinAbsValue' (FinSet old) (CodePointers new_set new_zero)
    | wordSet `Set.isSubsetOf` old = do
      addWords new_set
      return $ Nothing
    | Set.size r > maxSetSize = do
      addWords new_set
      return $ Just TopV
    | otherwise = do
      addWords new_set
      return $ Just (FinSet r)

  where (wordSet,_) = partitionAbsoluteAddrs new_set new_zero
        r = Set.union old wordSet

joinAbsValue' (CodePointers old old_zero) (FinSet new)
    | Set.size r > maxSetSize = do
      addWords old
      return $ Just TopV
    | otherwise = do
      addWords old
      return $ Just (FinSet r)
  where (wordSet,_) = partitionAbsoluteAddrs old old_zero
        r = Set.union wordSet new
joinAbsValue' (FinSet old) (FinSet new)
    | new `Set.isSubsetOf` old = return $ Nothing
    | Set.size r > maxSetSize = return $ Just TopV
    | otherwise = return $ Just (FinSet r)
  where r = Set.union old new
joinAbsValue' (StackOffset a_old old) (StackOffset b_old new)
    | a_old /= b_old = return (Just TopV)
    | new `Set.isSubsetOf` old = return $ Nothing
    | Set.size r > maxSetSize = return $ Just TopV
    | otherwise = return $ Just (StackOffset a_old r)
  where r = Set.union old new

-- Intervals
joinAbsValue' v v'
    | StridedInterval si_old <- v, StridedInterval si_new <- v'
    , si_new `SI.isSubsetOf` si_old =
      return $ Nothing
    | StridedInterval si_old <- v, StridedInterval si_new <- v' =
      return $ go si_old si_new
    | StridedInterval si <- v,  FinSet s <- v' =
      return $ go si (SI.fromFoldable (SI.typ si) s)
    | StridedInterval si <- v,  CodePointers s b <- v' = do
      addWords s
      let (wordSet, _) = partitionAbsoluteAddrs s b
      return $ go si (SI.fromFoldable (SI.typ si) wordSet)
    | StridedInterval si <- v', FinSet s <- v =
      return $ go si (SI.fromFoldable (SI.typ si) s)
    | StridedInterval si <- v', CodePointers s b <- v = do
      addWords s
      let (wordSet, _) = partitionAbsoluteAddrs s b
      return $ go si (SI.fromFoldable (SI.typ si) wordSet)
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
joinAbsValue' (SomeStackOffset ax) (StackOffset ay _) | ax == ay = return $ Nothing
joinAbsValue' (StackOffset ax _) (SomeStackOffset ay)
  | ax == ay = return $ Just (SomeStackOffset ax)
joinAbsValue' (SomeStackOffset ax) (SomeStackOffset ay) | ax == ay = return $ Nothing


joinAbsValue' ReturnAddr ReturnAddr = return Nothing


joinAbsValue' x y = do
  addWords (codePointerSet x)
  addWords (codePointerSet y)
  return $! Just TopV

-------------------------------------------------------------------------------
-- Abstract value operations

member :: Integer -> AbsValue w tp -> Bool
member = undefined
{-
member _ TopV = True
member n (FinSet s) = Set.member n s
member n (CodePointers s b) | 0 <= n && n <= toInteger (maxBound :: Word64) =
  b && (n == 0) || Set.member (fromInteger n) s
member n (StridedInterval si) = SI.member n si
member n (SubValue _n' v) = member n v
member _n _v = False
-}

-- | Returns true if this value represents the empty set.
isBottom :: AbsValue w tp -> Bool
isBottom (FinSet v)       = Set.null v
isBottom (CodePointers v b) = Set.null v && not b
isBottom (StackOffset _ v) = Set.null v
isBottom (SomeStackOffset _) = False
isBottom (StridedInterval v) = SI.size v == 0
isBottom (SubValue _ v) = isBottom v
isBottom TopV = False
isBottom ReturnAddr = False

-------------------------------------------------------------------------------
-- Intersection abstract values

-- meet is probably the wrong word here --- we are really refining the
-- abstract value based upon some new information.  Thus, we want to
-- return an overapproximation rather than an underapproximation of
-- the value.
-- Currently the only case we care about is where v' is an interval

-- @meet x y@ returns an over-approximation of the values in @x@ and @y@.
meet :: Integral (MemWord w)
     => AbsValue w tp
     -> AbsValue w tp
     -> AbsValue w tp
meet x y
  | isBottom m
  , not (isBottom x)
  , not (isBottom y) =
      debug DAbsInt ("Got empty: " ++ show (pretty x) ++ " " ++ show (pretty y)) $ m
  | otherwise = m
  where m = meet' x y

meet' :: Integral (MemWord w) => AbsValue w tp -> AbsValue w tp -> AbsValue w tp
meet' TopV x = x
meet' x TopV = x
-- FIXME: reuse an old value if possible?
meet' (CodePointers old old_zero) (CodePointers new new_zero) =
  CodePointers (Set.intersection old new) (old_zero && new_zero)
--TODO: Fix below
meet' (asFinSet "meet" -> IsFin old) (asFinSet "meet" -> IsFin new) =
  FinSet $ Set.intersection old new
meet' (StackOffset ax old) (StackOffset ay new) | ax == ay =
  StackOffset ax $ Set.intersection old new

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
  | SubValue _ _ <- v, StridedInterval _ <- v' = v' -- FIXME: ?
  | StridedInterval _ <- v, SubValue _ _ <- v' = v -- FIXME: ?

-- Join addresses
meet' (SomeStackOffset ax) s@(StackOffset ay _) = assert (ax == ay) s
meet' s@(StackOffset ax _) (SomeStackOffset ay) | ax == ay = s
meet' (SomeStackOffset ax) (SomeStackOffset ay) = assert (ax == ay) $ SomeStackOffset ax
meet' x _ = x -- Arbitrarily pick one.
-- meet x y = error $ "meet: impossible" ++ show (x,y)

-------------------------------------------------------------------------------
-- Operations

trunc :: (Integral (MemWord w), v+1 <= u)
      => AbsValue w (BVType u)
      -> NatRepr v
      -> AbsValue w (BVType v)
trunc (FinSet s) w       = FinSet (Set.map (toUnsigned w) s)
trunc (CodePointers s b) _ = FinSet sf
  where (sf,_) = partitionAbsoluteAddrs s b
trunc (StridedInterval s) w = stridedInterval (SI.trunc s w)
trunc (SubValue n av) w =
  case testNatCases n w of
   NatCaseLT LeqProof -> SubValue n av
   NatCaseEQ          -> av
   NatCaseGT LeqProof -> trunc av w
trunc (StackOffset _ _)   _ = TopV
trunc (SomeStackOffset _) _ = TopV
trunc ReturnAddr _ = TopV
trunc TopV _ = TopV

uext :: forall u v w
     .  (u+1 <= v, Integral (MemWord w))
     => AbsValue w (BVType u) -> NatRepr v -> AbsValue w (BVType v)
uext (FinSet s) _ = FinSet s
uext (CodePointers s b) _ = FinSet wordSet
  where (wordSet, _) = partitionAbsoluteAddrs s b
uext (StridedInterval si) w =
  StridedInterval $ si { SI.typ = w }
uext (SubValue (n :: NatRepr n) av) _ =
  -- u + 1 <= v, n + 1 <= u, need n + 1 <= v
  -- proof: n + 1 <= u + 1 by addIsLeq
  --        u + 1 <= v     by assumption
  --        n + 1 <= v     by transitivity
  case proof of
    LeqProof -> SubValue n av
  where
    proof :: LeqProof (n + 1) v
    proof = leqTrans (leqAdd (LeqProof :: LeqProof (n+1) u) n1) (LeqProof :: LeqProof (u + 1) v)
uext (StackOffset _ _) _ = TopV
uext (SomeStackOffset _) _ = TopV
uext ReturnAddr _ = TopV
uext TopV _ = TopV

bvadd :: forall w u
      .  Integral (MemWord w)
      => NatRepr u
      -> AbsValue w (BVType u)
      -> AbsValue w (BVType u)
      -> AbsValue w (BVType u)
-- Stacks
bvadd w (StackOffset a s) (FinSet t) | [o] <- Set.toList t = do
  StackOffset a $ Set.map (fromInteger . addOff w o . toInteger) s
bvadd w (FinSet t) (StackOffset a s) | [o] <- Set.toList t = do
  StackOffset a $ Set.map (fromInteger . addOff w o . toInteger) s
-- Strided intervals
bvadd w v v'
  | StridedInterval si1 <- v, StridedInterval si2 <- v' = go si1 si2
  | StridedInterval si <- v,  IsFin s <- asFinSet "bvadd" v' = go si (SI.fromFoldable w s)
  | StridedInterval si <- v', IsFin s <- asFinSet "bvadd" v  = go si (SI.fromFoldable w s)
  where
    go :: SI.StridedInterval u -> SI.StridedInterval u -> AbsValue w (BVType u)
    go si1 si2 = stridedInterval $ SI.bvadd w si1 si2

-- subvalues
-- bvadd w (SubValue _n _av c) v' = bvadd w c v'
-- bvadd w v (SubValue _n _av c)  = bvadd w v c

-- the rest
bvadd _ (StackOffset ax _) _ = SomeStackOffset ax
bvadd _ _ (StackOffset ax _) = SomeStackOffset ax
bvadd _ (SomeStackOffset ax) _ = SomeStackOffset ax
bvadd _ _ (SomeStackOffset ax) = SomeStackOffset ax
bvadd _ _ _ = TopV

setL :: Ord a
     => ([a] -> AbsValue w (BVType n))
     -> (Set a -> AbsValue w (BVType n))
     -> [a]
     -> AbsValue w (BVType n)
setL def c l | length l > maxSetSize = def l
             | otherwise = c (Set.fromList l)

-- | Subtracting
bvsub :: forall w u
      .  MemWidth w
      => Memory w
         -- ^ Memory used for checking code pointers.
      -> NatRepr u
      -> AbsValue w (BVType u)
      -> AbsValue w (BVType u)
      -> AbsValue w (BVType u)
bvsub _mem w (CodePointers s b) (FinSet t)
      -- If we just have zero.
    | Set.null s && b = FinSet (Set.map (toUnsigned w . negate) t)
    | all isJust vals && (not b || Set.singleton 0 == t) =
      CodePointers (Set.fromList (catMaybes vals)) b
    | otherwise = error "Losing code pointers due to bvsub."
      -- TODO: Fix this.
--      debug DAbsInt ("drooping " ++ show (ppIntegerSet s) ++ " " ++ show (ppIntegerSet t)) $
--        setL (stridedInterval . SI.fromFoldable w) FinSet (toInteger <$> vals)
  where vals :: [Maybe (SegmentedAddr w)]
        vals = do
          x <- Set.toList s
          y <- Set.toList t
          if toInteger (x^.addrOffset) >= y then
            return $ Just $ x & addrOffset -~ fromInteger y
           else
            return $ Nothing

bvsub _ _ xv@(CodePointers xs xb) (CodePointers ys yb)
      -- If we just have zero.
    | Set.null ys && yb = xv
      --
    | all isJust vals && xb == yb =
        if xb then
          FinSet (Set.insert 0 (Set.fromList (catMaybes vals)))
         else
          FinSet (Set.fromList (catMaybes vals))
    | otherwise = error "Losing code pointers due to bvsub."
      -- TODO: Fix this.
--      debug DAbsInt ("drooping " ++ show (ppIntegerSet s) ++ " " ++ show (ppIntegerSet t)) $
--        setL (stridedInterval . SI.fromFoldable w) FinSet (toInteger <$> vals)
  where vals :: [Maybe Integer]
        vals = do
          x <- Set.toList xs
          y <- Set.toList ys
          if segmentIndex (addrSegment x) == segmentIndex (addrSegment y) then
            pure $ Just $ toInteger (x^.addrOffset) - toInteger (y^.addrOffset)
           else do
            pure $ Nothing
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
bvsub _ w (StackOffset ax s) (asFinSet "bvsub6" -> IsFin t) =
  setL (\_ -> SomeStackOffset ax) (StackOffset ax) $ do
    x <- toInteger <$> Set.toList s
    y <- Set.toList t
    return $! fromInteger (toUnsigned w (x - y))
bvsub _ _ (StackOffset ax _) _ = SomeStackOffset ax
bvsub _ _ _ (StackOffset _ _) = TopV
bvsub _ _ (SomeStackOffset ax) _ = SomeStackOffset ax
bvsub _ _ _ (SomeStackOffset _) = TopV
bvsub _ _ _ _ = TopV -- Keep the pattern checker happy

bvmul :: forall w u
      .  Integral (MemWord w)
      => NatRepr u
      -> AbsValue w (BVType u)
      -> AbsValue w (BVType u)
      -> AbsValue w (BVType u)
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
    go :: SI.StridedInterval u -> SI.StridedInterval u -> AbsValue w (BVType u)
    go si1 si2 = stridedInterval $ SI.bvmul w si1 si2

-- bvmul w (SubValue _n _av c) v' = bvmul w c v'
-- bvmul w v (SubValue _n _av c)  = bvmul w v c

bvmul _ _ _ = TopV

-- FIXME: generalise
bitop :: Integral (MemWord w)
      => (Integer -> Integer -> Integer)
      -> NatRepr u
      -> AbsValue w (BVType u)
      -> AbsValue w  (BVType u)
      -> AbsValue w (BVType u)
bitop doOp _w (asFinSet "bvand" -> IsFin s) (asConcreteSingleton -> Just v)
  = FinSet (Set.map (flip doOp v) s)
bitop doOp _w (asConcreteSingleton -> Just v) (asFinSet "bvand" -> IsFin s)
  = FinSet (Set.map (doOp v) s)
bitop _ _ _ _ = TopV

ppAbsValue :: AbsValue w tp -> Maybe Doc
ppAbsValue TopV = Nothing
ppAbsValue v = Just (pretty v)

-- | Print a list of Docs vertically separated.
instance ShowF r => PrettyRegValue r (AbsValue w) where
  ppValueEq _ TopV = Nothing
  ppValueEq r v = Just (text (showF r) <+> text "=" <+> pretty v)


absTrue :: AbsValue w BoolType
absTrue = FinSet (Set.singleton 1)

absFalse :: AbsValue w BoolType
absFalse = FinSet (Set.singleton 0)

-- | This returns the smallest abstract value that contains the
-- given unsigned integer.
abstractSingleton :: (Num (MemWord w))
                  => NatRepr w
                     -- ^ Width of code pointer
                  -> (MemWord w -> Maybe (SegmentedAddr w))
                     -- ^ Predicate that recognizes if the given value is a code
                     -- pointer.
                  -> NatRepr n
                  -> Integer
                  -> AbsValue w (BVType n)
abstractSingleton code_w is_code w i
  | Just Refl <- testEquality w code_w
  , 0 <= i && i <= maxUnsigned w
  , Just sa <- is_code (fromInteger i) =
    CodePointers (Set.singleton sa) False
  | 0 <= i && i <= maxUnsigned w = FinSet (Set.singleton i)
  | otherwise = error $ "abstractSingleton given bad value: " ++ show i ++ " " ++ show w

concreteStackOffset :: SegmentedAddr w -> Integer -> AbsValue w (BVType w)
concreteStackOffset a o = StackOffset a (Set.singleton (fromInteger o))

------------------------------------------------------------------------
-- Restrictions

hasMaximum :: TypeRepr tp -> AbsValue w tp -> Maybe Integer
hasMaximum tp v =
  case v of
   FinSet s | Set.null s -> Nothing
            | otherwise  -> Just $! Set.findMax s
   CodePointers s b | Set.null s -> if b then Just 0 else Nothing
                    | otherwise  -> Just $ case tp of BVTypeRepr n -> maxUnsigned n
   StridedInterval si -> Just (SI.intervalEnd si)
   TopV               -> Just $ case tp of BVTypeRepr n -> maxUnsigned n
   _                  -> Nothing


hasMinimum :: TypeRepr tp -> AbsValue w tp -> Maybe Integer
hasMinimum _tp v =
  case v of
   FinSet s       | Set.null s -> Nothing
                  | otherwise  -> Just $! Set.findMin s
   CodePointers s b | Set.null s -> if b then Just 0 else Nothing
   StridedInterval si -> Just $! SI.base si
   _                  -> Just 0

-- | @abstractULt x y@ refines x and y with the knowledge that @x < y@
-- is unsigned.
-- For example, given {2, 3} and {2, 3, 4}, we know (only) that
-- {2, 3} and {3, 4} because we may pick any element from either set.

abstractULt :: Integral (MemWord w)
            => TypeRepr tp
            -> AbsValue w tp
            -> AbsValue w tp
            -> (AbsValue w tp, AbsValue w tp)
abstractULt _tp TopV TopV = (TopV, TopV)
abstractULt tp x y
  | Just u_y <- hasMaximum tp y
  , Just l_x <- hasMinimum tp x
  , BVTypeRepr n <- tp =
    -- debug DAbsInt' ("abstractLt " ++ show (pretty x) ++ " " ++ show (pretty y) ++ " -> ")
    ( meet x (stridedInterval $ SI.mkStridedInterval n False 0 (u_y - 1) 1)
    , meet y (stridedInterval $ SI.mkStridedInterval n False (l_x + 1)
                                                     (maxUnsigned n) 1))

abstractULt _tp x y = (x, y)

-- | @abstractULeq x y@ refines x and y with the knowledge that @x <= y@
abstractULeq :: Integral (MemWord w)
             => TypeRepr tp
               -> AbsValue w tp
               -> AbsValue w tp
               -> (AbsValue w tp, AbsValue w tp)
abstractULeq _tp TopV TopV = (TopV, TopV)
abstractULeq tp x y
  | Just u_y <- hasMaximum tp y
  , Just l_x <- hasMinimum tp x
  , BVTypeRepr n <- tp =
    -- trace' ("abstractLeq " ++ show (pretty x) ++ " " ++ show (pretty y) ++ " -> ")
    ( meet x (stridedInterval $ SI.mkStridedInterval n False 0 u_y 1)
    , meet y (stridedInterval $ SI.mkStridedInterval n False l_x
                                                     (maxUnsigned n) 1))

abstractULeq _tp x y = (x, y)

------------------------------------------------------------------------
-- AbsBlockStack

data StackEntry w
   = forall tp . StackEntry !(TypeRepr tp) !(AbsValue w tp)

instance Eq (StackEntry w) where
  StackEntry x_tp x_v == StackEntry y_tp y_v
    | Just Refl <- testEquality x_tp y_tp = x_v == y_v
    | otherwise = False

-- | The AbsBlockStack describes offsets of the stack.
-- Values that are not in the map may denote any values.
-- The stack grows down, so nonegative keys are those within
-- rsp.
type AbsBlockStack w = Map Int64 (StackEntry w)

-- absStackLeq :: AbsBlockStack -> AbsBlockStack -> Bool
-- absStackLeq x y = all entryLeq (Map.toList y)
--   where entryLeq (o, StackEntry y_tp y_v) =
--           case Map.lookup o x of
--             Just (StackEntry x_tp x_v) | Just Refl <- testEquality x_tp y_tp ->
--               isNothing (joinAbsValue y_v x_v)
--             _ -> False

-- | @absStackJoinD y x@ returns the stack containing the union @z@ of the
-- values in @y@ and @x@.  It sets the first state parameter to true if @z@
-- is different from @y@ and adds and escaped code pointers to the second
-- parameter.
absStackJoinD :: Integral (MemWord w)
              => AbsBlockStack w
              -> AbsBlockStack w
              -> State (Bool,Set (SegmentedAddr w)) (AbsBlockStack w)
absStackJoinD y x = do
  -- This attempts to merge information from the new state into the old state.
  let entryLeq (o, StackEntry y_tp y_v) =
        case Map.lookup o x of
          -- The new state contains a valuewith the same type.
          Just (StackEntry x_tp x_v) | Just Refl <- testEquality x_tp y_tp -> do
            s <- use _2
            -- Attempt to merge values
            case runState (joinAbsValue' y_v x_v) s of
              -- If merging returns the value y_v, then keep it.
              (Nothing,  s') -> do
                _2 .= s'
                return $ Just (o, StackEntry y_tp y_v)
              -- Otherwise merging returned a new value.
              (Just z_v, s') -> do
                case y_v of
                  ReturnAddr -> debug DAbsInt ("absStackJoinD dropping return value:\n"
                                    ++ "Old state: " ++ show (ppAbsStack y)
                                    ++ "New state: " ++ show (ppAbsStack x)) $
                    return ()
                  _ -> return ()
                _1 .= True
                _2 .= s'
                return $ Just (o, StackEntry y_tp z_v)
          _ -> do
            case y_v of
              ReturnAddr -> debug DAbsInt ("absStackJoinD dropping return value:\nOld state: " ++ show (ppAbsStack y) ++ "\nNew state: " ++ show (ppAbsStack x)) $ return ()
              _ -> return ()
            _1 .= True
            _2 %= Set.union (codePointerSet y_v)
            return Nothing
  z <- mapM entryLeq (Map.toList y)
  return $! Map.fromList (catMaybes z)

ppAbsStack :: AbsBlockStack w -> Doc
ppAbsStack m = vcat (pp <$> Map.toDescList m)
  where pp (o,StackEntry _ v) = text (showHex o " :=") <+> pretty v

------------------------------------------------------------------------
-- AbsBlockState

-- | State at beginning of a block.
data AbsBlockState r
   = AbsBlockState { _absRegState :: !(RegState r (AbsValue (RegAddrWidth r)))
                   , _startAbsStack :: !(AbsBlockStack (RegAddrWidth r))
                   }

deriving instance MapF.OrdF r => Eq (AbsBlockState r)


absRegState :: Simple Lens (AbsBlockState r)
                           (RegState r (AbsValue (RegAddrWidth r)))
absRegState = lens _absRegState (\s v -> s { _absRegState = v })

startAbsStack :: Simple Lens (AbsBlockState r) (AbsBlockStack (RegAddrWidth r))
startAbsStack = lens _startAbsStack (\s v -> s { _startAbsStack = v })

traceUnless :: Bool -> String -> a -> a
traceUnless True _ = id
traceUnless False msg = debug DAbsInt msg


instance ( RegisterInfo r
         )
      => AbsDomain (AbsBlockState r) where

  top = AbsBlockState { _absRegState = mkRegState (\_ -> TopV)
                      , _startAbsStack = Map.empty
                      }

  joinD x y | regs_changed = Just $! z
            | otherwise = Nothing
    where xs = x^.absRegState
          ys = y^.absRegState

          x_stk = x^.startAbsStack
          y_stk = y^.startAbsStack

          (zs,(regs_changed,dropped)) = flip runState (False, Set.empty) $ do
            z_regs <- mkRegStateM $ \r -> do
              let xr = xs^.boundValue r
              (c,s) <- get
              case runState (joinAbsValue' xr (ys^.boundValue r)) s of
                (Nothing,s') -> do
                  seq s' $ put $ (c,s')
                  return $! xr
                (Just zr,s') -> do
                  seq s' $ put $ (True,s')
                  return $! zr
            z_stk <- absStackJoinD x_stk y_stk
            return $ AbsBlockState { _absRegState   = z_regs
                                   , _startAbsStack = z_stk
                                   }

          z = traceUnless (Set.null dropped)
                ("dropped abs " ++ show (ppSegAddrSet dropped) ++ "\n"
                                ++ show x ++ "\n" ++ show y) $
              zs

instance ( OrdF r
         , ShowF r
         ) => Pretty (AbsBlockState r) where
  pretty s =
      text "registers:" <$$>
      indent 2 (pretty (s^.absRegState)) <$$>
      stack_d
    where stack = s^.startAbsStack
          stack_d | Map.null stack = empty
                  | otherwise = text "stack:" <$$>
                                indent 2 (ppAbsStack stack)

instance (OrdF r, ShowF r) => Show (AbsBlockState r) where
  show s = show (pretty s)

-- | Update the block state to point to a specific IP address.
setAbsIP :: RegisterInfo r
         => SegmentedAddr (RegAddrWidth r)
            -- ^ The address to set.
         -> AbsBlockState r
         -> AbsBlockState r
setAbsIP a b
    -- Check to avoid reassigning next IP if it is not needed.
  | CodePointers s False <- b^.absRegState^.curIP
  , Set.size s == 1
  , Set.member a s =
    b
  | otherwise =
    b & absRegState . curIP .~ CodePointers (Set.singleton a) False

------------------------------------------------------------------------
-- AbsProcessorState

-- | The absolute value associated with a given architecture.
--
-- This is only a function of the address width.
type ArchAbsValue arch = AbsValue (RegAddrWidth (ArchReg arch))

-- | This stores the abstract state of the system at a given point in time.
data AbsProcessorState r ids
   = AbsProcessorState { absCodeWidth    :: !(NatRepr (RegAddrWidth r))
                         -- ^ The width of a code pointer; the 'NatRepr' type
                         -- connects the type-level nat with the value
                       , absMem       :: !(Memory (RegAddrWidth r))
                         -- ^ Recognizer for code addresses.
                       , _absInitialRegs
                         :: !(RegState r (AbsValue (RegAddrWidth r)))
                         -- ^ Default values of registers
                       , _absAssignments :: !(MapF (AssignId ids) (AbsValue (RegAddrWidth r)))
                         -- ^ The assignments that have been seen, and the
                         -- symbolic values associated with them
                       , _curAbsStack    :: !(AbsBlockStack (RegAddrWidth r))
                         -- ^ The current symbolic state of the stack
                       }

absInitialRegs :: Simple Lens (AbsProcessorState r ids)
                              (RegState r (AbsValue (RegAddrWidth r)))
absInitialRegs = lens _absInitialRegs (\s v -> s { _absInitialRegs = v })

absAssignments :: Simple Lens (AbsProcessorState r ids)
                              (MapF (AssignId ids) (AbsValue (RegAddrWidth r)))
absAssignments = lens _absAssignments (\s v -> s { _absAssignments = v })

curAbsStack :: Simple Lens (AbsProcessorState r ids) (AbsBlockStack (RegAddrWidth r))
curAbsStack = lens _curAbsStack (\s v -> s { _curAbsStack = v })

instance (OrdF r, ShowF r)
      => Show (AbsProcessorState r ids) where
  show = show . pretty

-- FIXME
instance (OrdF r, ShowF r)
      => Pretty (AbsProcessorState r ids) where
  pretty regs = pretty (AbsBlockState { _absRegState   = regs ^. absInitialRegs
                                      , _startAbsStack = regs ^. curAbsStack })

initAbsProcessorState :: (Num (RegAddr r), Ord (RegAddr r))
                      => NatRepr (RegAddrWidth r)
                      -> Memory (RegAddrWidth r)
                         -- ^ Current state of memory in the processor.
                         --
                         -- Used for checking code segment status.
                      -> AbsBlockState r
                      -> AbsProcessorState r ids
initAbsProcessorState code_width mem s =
  AbsProcessorState { absCodeWidth = code_width
                    , absMem = mem
                    , _absInitialRegs = s^.absRegState
                    , _absAssignments = MapF.empty
                    , _curAbsStack = s^.startAbsStack
                    }

-- | A lens that allows one to lookup and update the value of an assignment in
-- map from assignments to abstract values.
assignLens :: AssignId ids tp
           -> Simple Lens (MapF (AssignId ids) (AbsValue w)) (AbsValue w tp)
assignLens ass = lens (fromMaybe TopV . MapF.lookup ass)
                      (\s v -> MapF.insert ass v s)

deleteRange :: Int64 -> Int64 -> AbsBlockStack w -> AbsBlockStack w
deleteRange l h m
  | h < l = m
  | otherwise =
    case Map.lookupGE l m of
      Just (k,v)
        | k <= h
        , StackEntry _ ReturnAddr <- v ->
          debug DAbsInt ("Deleting return address at offset " ++ show (k,l,h))
                (deleteRange (k+1) h (Map.delete k m))
        | k <= h ->
          deleteRange (k+1) h (Map.delete k m)
      _ -> m

-- Return the width of a value.
someValueWidth :: ( HasRepr (ArchReg arch) TypeRepr
                  )
               => Value arch ids tp
               -> Integer
someValueWidth v =
  case typeRepr v of
    BVTypeRepr w -> natValue w

valueByteSize :: ( HasRepr (ArchReg arch) TypeRepr
                 )
              => Value arch ids tp
              -> Int64
valueByteSize v = fromInteger $ (someValueWidth v + 7) `div` 8

-- | Prune stack based on write that may modify stack.
pruneStack :: AbsBlockStack w -> AbsBlockStack w
pruneStack = Map.filter f
  where f (StackEntry _ ReturnAddr) = True
        f _ = False

------------------------------------------------------------------------
-- Transfer Value

transferValue' :: ( OrdF (ArchReg a)
                  , ShowF (ArchReg a)
                  , Num (ArchAddr a)
                  )
               => NatRepr (ArchAddrWidth a)
                  -- ^ Width of a code pointer
               -> (MemWord (ArchAddrWidth a) -> Maybe (SegmentedAddr (ArchAddrWidth a)))
                  -- ^ Predicate that recognizes if address is code addreess.
               -> MapF (AssignId ids) (ArchAbsValue a)
               -> RegState (ArchReg a) (ArchAbsValue a)
               -> Value a ids tp
               -> ArchAbsValue a tp
transferValue' code_width is_code amap aregs v =
  case v of
   BVValue w i
     | 0 <= i && i <= maxUnsigned w -> abstractSingleton code_width is_code w i
     | otherwise -> error $ "transferValue given illegal value " ++ show (pretty v)
   RelocatableValue{} -> error $ "Relocatable values are not supported."
   -- Invariant: v is in m
   AssignedValue a ->
     fromMaybe (error $ "Missing assignment for " ++ show (assignId a))
               (MapF.lookup (assignId a) amap)
   Initial r
--     | Just Refl <- testEquality r N.rsp -> do
--       StackOffset (Set.singleton 0)
     | otherwise -> aregs ^. boundValue r

-- | Compute abstract value from value and current registers.
transferValue :: ( OrdF (ArchReg a)
                 , ShowF (ArchReg a)
                 , MemWidth (ArchAddrWidth a)
                 )
              => AbsProcessorState (ArchReg a) ids
              -> Value a ids tp
              -> ArchAbsValue a tp
transferValue c v =
  let is_code addr = do
        sa <- absoluteAddrSegment (absMem c) addr
        if segmentFlags (addrSegment sa) `Perm.hasPerm` Perm.execute then
          Just $! sa
         else
          Nothing
   in transferValue' (absCodeWidth c) is_code (c^.absAssignments) (c^.absInitialRegs) v

------------------------------------------------------------------------
-- Operations

addMemWrite :: ( HasRepr (ArchReg arch) TypeRepr
               , OrdF    (ArchReg arch)
               , ShowF   (ArchReg arch)
               , MemWidth (ArchAddrWidth arch)
               )
            => ArchAbsValue arch (BVType (ArchAddrWidth arch))
               -- ^ Current instruction pointer
               --
               -- Used for pretty printing
            -> BVValue arch ids (ArchAddrWidth arch)
            -> Value arch ids tp
            -> AbsProcessorState (ArchReg arch) ids
            -> AbsProcessorState (ArchReg arch) ids
addMemWrite cur_ip a v r =
  case (transferValue r a, transferValue r v) of
    -- (_,TopV) -> r
    -- We overwrite _some_ stack location.  An alternative would be to
    -- update everything with v.
    (SomeStackOffset _, _) ->
      debug DAbsInt ("addMemWrite: dropping stack at "
             ++ show (pretty cur_ip)
             ++ " via " ++ show (pretty a)
             ++" in SomeStackOffset case") $
      r & curAbsStack %~ pruneStack
    (StackOffset _ s, _) | Set.size s > 1 ->
      let w = valueByteSize v
      in  r & curAbsStack %~ flip (Set.fold (\o m -> deleteRange o (o+w-1) m)) s
    (StackOffset _ s, TopV) | [o] <- Set.toList s ->
      let w = valueByteSize v
       in r & curAbsStack %~ deleteRange o (o+w-1)
    (StackOffset _ s, v_abs) | [o] <- Set.toList s ->
      let w = valueByteSize v
          e = StackEntry (typeRepr v) v_abs
       in r & curAbsStack %~ Map.insert o e . deleteRange o (o+w-1)
    -- FIXME: nuke stack on an unknown address or Top?
    _ -> r

-- subOff :: NatRepr w -> Integer -> Integer -> Integer
-- subOff w o v = toUnsigned w (o - v)

mkAbsBlockState :: RegisterInfo r
                => (forall tp . r tp -> AbsValue (RegAddrWidth r) tp)
                -> AbsBlockStack (RegAddrWidth r)
                -> AbsBlockState r
mkAbsBlockState trans newStack =
  AbsBlockState { _absRegState = mkRegState trans
                , _startAbsStack = newStack
                }

absStackHasReturnAddr :: AbsBlockState r -> Bool
absStackHasReturnAddr s = isJust $ find isReturnAddr (Map.elems (s^.startAbsStack))
  where isReturnAddr (StackEntry _ ReturnAddr) = True
        isReturnAddr _ = False


-- | Return state for after value has run.
finalAbsBlockState :: forall a ids
                   .  ( RegisterInfo (ArchReg a)
                      , MemWidth (ArchAddrWidth a)
                      )
                   => AbsProcessorState (ArchReg a) ids
                   -> RegState (ArchReg a) (Value a ids)
                   -> AbsBlockState (ArchReg a)
finalAbsBlockState c s =
  let transferReg :: ArchReg a tp -> ArchAbsValue a tp
      transferReg r = transferValue c (s^.boundValue r)
   in mkAbsBlockState transferReg (c^.curAbsStack)

------------------------------------------------------------------------
-- Transfer functions

transferApp :: ( OrdF (ArchReg a)
               , ShowF (ArchReg a)
               , MemWidth (ArchAddrWidth a)
               )
            => AbsProcessorState (ArchReg a) ids
            -> App (Value a ids) tp
            -> ArchAbsValue a tp
transferApp r a =
  case a of
    Trunc v w -> trunc (transferValue r v) w
    UExt  v w -> uext  (transferValue r v) w
    BVAdd w x y -> bvadd w (transferValue r x) (transferValue r y)
    BVSub w x y -> bvsub (absMem r) w (transferValue r x) (transferValue r y)
    BVMul w x y -> bvmul w (transferValue r x) (transferValue r y)
    BVAnd w x y -> bitop (.&.) w (transferValue r x) (transferValue r y)
    BVOr w x y  -> bitop (.|.) w (transferValue r x) (transferValue r y)
    _ -> TopV
