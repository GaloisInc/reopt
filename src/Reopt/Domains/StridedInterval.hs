------------------------------------------------------------------------
-- |
-- Module           : Reopt.Domains.StridedInterval
-- Description      : A strided interval domain x + [a .. b] * c
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Simon Winwood <sjw@galois.com>
-- Stability        : provisional
--
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- FIXME: take rounding/number of bits/etc. into consideration
-- FIXME: only really useful for unsigned?
module Reopt.Domains.StridedInterval
       ( StridedInterval(..)
         -- Constructors 
       , singleton, mkStridedInterval, fromFoldable
         -- Predicates
       , isSingleton, isTop, member, isSubsetOf
         -- Destructors
       , toList, intervalEnd
         -- Domain operations
       , lub, lubSingleton, glb
         -- Operations
       , bvadd, bvmul, trunc
         -- Debugging
       ) where

import           Debug.Trace

import           Control.Applicative ( (<$>), (<*>) )
import qualified Data.Foldable as Fold
import           Data.Maybe (isNothing)
import qualified Data.Set as S
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Reopt.Semantics.Types

import           Test.QuickCheck


-- -----------------------------------------------------------------------------
-- Data type decl and instances

-- This is a canonical (and more compact) representation, basically we
-- turn x + [a .. b] * c into (x + a * c) + [0 .. b - a] * c

-- FIXME: we might not need EmptyInterval (we could also just say that
-- stride == 0 is empty)
data StridedInterval (tp :: Type) =
  StridedInterval { typ :: TypeRepr tp -- maybe not needed?
                  , base :: Integer
                  , range :: Integer
                  , stride :: Integer }
  | EmptyInterval

instance Eq (StridedInterval tp) where
  EmptyInterval == EmptyInterval = True
  si1@StridedInterval{} == si2@StridedInterval{} =
    base si1 == base si2 && range si1 == range si2 && stride si1 == stride si2
  _ == _ = False

instance Show (StridedInterval tp) where
  show = show . pretty

intervalEnd :: StridedInterval tp -> Integer
intervalEnd EmptyInterval = error "intervalEnd"
intervalEnd si = base si + range si * stride si


-- -----------------------------------------------------------------------------
-- Constructors

-- | Construct a singleton value
singleton :: TypeRepr tp -> Integer -> StridedInterval tp
singleton tp v = StridedInterval { typ = tp
                                 , base = v
                                 , range = 0
                                 , stride = 1 }

-- | Make an interval given the start, end, and stride. Note that this
-- will round up if (start - end) is not a multiple of the stride,
-- i.e., @mkStr
mkStridedInterval :: TypeRepr tp -> Bool 
                  -> Integer -> Integer -> Integer
                  -> StridedInterval tp
mkStridedInterval tp roundUp start end s
  | end < start     = EmptyInterval
  | s == 0          = singleton tp start
  | r == 0          = singleton tp start
  | otherwise       =
      StridedInterval { typ = tp
                      , base = start
                      , range = r
                      , stride = s }
  where
    r = ((end - start) `div` s)
        + (if roundUp && (end - start) `mod` s /= 0 then 1 else 0)

fromFoldable :: Fold.Foldable t =>
                NatRepr n -> t Integer -> StridedInterval (BVType n)
fromFoldable sz vs
  | isEmpty vs  = EmptyInterval
  | otherwise  = mkStridedInterval tp True start end s
  where
    tp      = BVTypeRepr sz
    isEmpty = not . Fold.any (const True)
    start   = Fold.minimum vs
    end     = Fold.maximum vs
    -- This is a bit of a hack, relying on the fact that gcd 0 v == v
    s       = Fold.foldl' (\g v -> gcd g (v - start)) 0 vs


-- -----------------------------------------------------------------------------
-- Predicates

isSingleton :: StridedInterval tp -> Maybe Integer
isSingleton StridedInterval { base = b, range = 0 } = Just b
isSingleton _  = Nothing

isTop :: StridedInterval tp -> Bool
isTop si@StridedInterval{} = case typ si of BVTypeRepr sz -> si == top sz
isTop _  = False

member :: Integer -> StridedInterval tp -> Bool
member _ EmptyInterval = False
member n si = base si <= n
              && (n - base si) `mod` stride si == 0
              && (n - base si) `div` stride si <= range si 

-- is the set represented by si1 contained in si2?
isSubsetOf :: StridedInterval (BVType n)
       -> StridedInterval (BVType n)
       -> Bool
isSubsetOf EmptyInterval _ = True
isSubsetOf _ EmptyInterval = False
isSubsetOf si1 si2
  | Just s <- isSingleton si1 = member s si2
  | otherwise = member (base si1) si2
                && member (intervalEnd si1) si2
                && stride si2 <= gcd (stride si1) (stride si2)


-- -----------------------------------------------------------------------------
-- Domain operations

lub :: StridedInterval (BVType n)
       -> StridedInterval (BVType n)
       -> StridedInterval (BVType n)
lub EmptyInterval{} si = si
lub si EmptyInterval{} = si
-- FIXME: make more precise?
lub si1 si2
  | Just s <- isSingleton si1 = lubSingleton s si2 
  | Just s <- isSingleton si2 = lubSingleton s si1
  | otherwise =
      mkStridedInterval (typ si1) True lower upper
                        (gcd (gcd (stride si1) (stride si2))
                             ((max (base si1) (base si2)) - lower))
  where
    lower = min (base si1) (base si2)
    upper = max (intervalEnd si1) (intervalEnd si2)

prop_lub :: StridedInterval (BVType 64)
            -> StridedInterval (BVType 64)
            -> Bool
prop_lub x y = x `isSubsetOf` (x `lub` y)
               && y `isSubsetOf` (x `lub` y)

lubSingleton :: Integer
                -> StridedInterval (BVType n)
                -> StridedInterval (BVType n)
lubSingleton s si
  | member s si  = si
  | Just s' <- isSingleton si =
      let l = (min s s')
          u = (max s s') 
      in mkStridedInterval (typ si) True l u (u - l)
  | s < base si  = go s si_upper (base si) 
  -- | si_upper < s = go (base si) s si_upper
  | otherwise    = go (base si) (max s si_upper) s
  where
    si_upper = intervalEnd si
    go lower upper to_contain =
      mkStridedInterval (typ si) True lower upper
                        (gcd (stride si) (to_contain - lower))

prop_glb :: StridedInterval (BVType 64)
            -> StridedInterval (BVType 64)
            -> Bool
prop_glb x y = (x `glb` y) `isSubsetOf` x
               && (x `glb` y) `isSubsetOf` y

-- | Greatest lower bound.  @glb si1 si2@ contains only those values
-- which are in @si1@ and @si2@.
glb :: StridedInterval (BVType n)
       -> StridedInterval (BVType n)
       -> StridedInterval (BVType n)
glb EmptyInterval _ = EmptyInterval
glb _ EmptyInterval = EmptyInterval
glb si1 si2
  | Just s <- isSingleton si1 =
      if s `member` si2 then si1 else EmptyInterval
  | Just s <- isSingleton si2 =
      if s `member` si1 then si2 else EmptyInterval
  | base si1 == base si2 =
      mkStridedInterval (typ si1) False (base si1) upper
                        (lcm (stride si1) (stride si2))
   -- lower is the least value that is greater than both bases, less
   -- than both ends, and in both intervals.  That is,
   --
   -- base1 + n * stride1 = base2 + m * stride2
   --
   -- or
   -- 
   --    n * stride1 - m * stride2 = base2 - base1
   --
   -- where n, m are integers s.t. the above holds, we want also that
   -- the n, m are in range1, range2, resp.
  | Just (n, _) <- solveLinearDiophantine (stride si1) (stride si2)
                                          (base si2 - base si1)
                                          (range si1) (range si2) =
      mkStridedInterval (typ si1) False (base si1 + n * stride si1) upper s
  | otherwise = EmptyInterval
  where
    upper = min (intervalEnd si1) (intervalEnd si2)
    s     = lcm (stride si1) (stride si2)

-- solves ax - by = c, (NOTE - sign) for x and y with 0 <= x, y <=
-- a_max, b_max resp.  Assumes a > 0, b > 0, c /= 0.
--
-- In this restricted case, we have
--
-- a * n - b * m = gcd (a, -b) (> 0)
--
-- so we want least t s.t.
--
-- ceiling (max (n * c / - a, m * c / - b)) <= t
-- and
-- t <= floor (min ((a_max * gcd - n * c) / b, b_max * gcd - m * c) / a)
    
solveLinearDiophantine :: Integer -> Integer -> Integer
                          -> Integer -> Integer 
                          -> Maybe (Integer, Integer)
solveLinearDiophantine a b c a_max b_max
  | c `rem` g /= 0 = Nothing
  | t <= t_upper = Just ( n * (c `quot` g) + (b `quot` g) * t
                        , m * (c `quot` g) + (a `quot` g) * t )
  | otherwise  = Nothing
  where
    (g, n, m) = eGCD a (-b)

    t = max (ceil_quot (n * c) (- a)) (ceil_quot (m * c) (- b))
    t_upper = min (floor_quot (a_max * g - n * c) b)
                  (floor_quot (b_max * g - m * c) a)

-- calculates ceil(x/y)
ceil_quot :: Integral a => a -> a -> a    
ceil_quot x y = x `quot` y + (if x `rem` y == 0 then 0 else 1)

floor_quot :: Integral a => a -> a -> a
floor_quot x y = x `div` y

prop_sld :: Positive Integer -> Positive Integer
            -> NonZero Integer -> Positive Integer -> Positive Integer
            -> Property
prop_sld a b c d e = not (isNothing v) ==> p
  where
    p = case v of
         Just (x, y) -> x >= 0 && y >= 0
                        && x <= getPositive d
                        && y <= getPositive e
                        && (getPositive a) * x - (getPositive b) * y == (getNonZero c)
         _ -> True
    v = solveLinearDiophantine (getPositive a) (getPositive b) (getNonZero c)
                               (getPositive d) (getPositive e)
        
      
-- | Returns the gcd, and n and m s.t. n * a + m * b = g
-- clagged, fixed, from http://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- this is presumably going to be slower than the gmp version :(
eGCD :: Integral a => a -> a -> (a,a,a)
eGCD a0 b0 = let (g, m, n) = go a0 b0
           in if g < 0 then (-g, -m, -n) else (g, m, n)
  where
    go a 0 = (a, 1, 0)
    go a b = let (g, x, y) = go b $ rem a b
             in (g, y, x - (a `quot` b) * y)

prop_eGCD :: Integer -> Integer -> Bool
prop_eGCD x y = let (g, a, b) = eGCD x y in x * a + y * b == g

-- -----------------------------------------------------------------------------
-- Operations

-- These operations probably only really make sense for constants or
-- constant ranges.  We can always just make the stride 1, but this
-- loses information.

-- We have x + [0..a] * b + y + [0 .. d] * e
-- = (x + y) + { 0 * b, 0 * e, 1 * b, 1 * e, ..., a * b, d * e }
-- `subsetOf` let m = gcd(b, e)
--            in (x + y) + [0 .. (a * b / m) + (d * e / m) ] * m

top :: NatRepr u -> StridedInterval (BVType u)
top sz = StridedInterval { typ = BVTypeRepr sz
                         , base = 0
                         , range = maxUnsigned sz
                         , stride = 1 }

clamp :: NatRepr u -> StridedInterval (BVType u) -> StridedInterval (BVType u)
clamp sz v = trunc v sz 

bvadd :: NatRepr u
      -> StridedInterval (BVType u)
      -> StridedInterval (BVType u)
      -> StridedInterval (BVType u)
bvadd _ EmptyInterval{} _ = EmptyInterval
bvadd _ _ EmptyInterval{} = EmptyInterval
bvadd sz si1 si2
  | Just s <- isSingleton si1 =
      clamp sz $ si2 { base = base si2 + s}
bvadd sz si1 si2
  | Just s <- isSingleton si2 =
      clamp sz $ si1 { base = base si1 + s }
bvadd sz si1 si2 =
  clamp sz $ StridedInterval { typ = typ si1
                             , base = base si1 + base si2
                             , range = r
                             , stride = m }
  where
    m = gcd (stride si1) (stride si2)
    r = (range si1 * (stride si1 `div` m)) + (range si2 * (stride si2 `div` m)) 

prop_bvadd ::  StridedInterval (BVType 64)
            -> StridedInterval (BVType 64)
            -> Bool
prop_bvadd = mk_prop (+) bvadd

-- We have (x + [0..a] * b) * (y + [0 .. d] * e)
-- = (x * y) + [0..a] * b * y + [0 .. d] * e * x + ([0..a] * b * [0..d] * e)
-- `subsetOf`  (x * y) + [0..a] * (b * y)
--           + 0       + [0 .. d] * (e * x)
--           + 0       + ([0..a*d] * (b * e)

bvmul :: NatRepr u
      -> StridedInterval (BVType u)
      -> StridedInterval (BVType u)
      -> StridedInterval (BVType u)
bvmul _ EmptyInterval{} _ = EmptyInterval
bvmul _ _ EmptyInterval{} = EmptyInterval
bvmul sz si1 si2 =
  bvadd sz
        (bvadd sz
               (mk (base si1 * base si2) (range si1) (stride si1 * base si2))
               (mk 0 (range si2) (stride si2 * base si1)))
        (mk 0 (range si1 * range si2) (stride si1 * stride si2))
  where
    mk b r s
      | s == 0    = singleton (typ si1) b
      | otherwise = StridedInterval { typ = typ si1, base = b, range = r, stride = s }

prop_bvmul ::  StridedInterval (BVType 64)
            -> StridedInterval (BVType 64)
            -> Bool
prop_bvmul = mk_prop (*) bvmul

-- filterLeq :: TypeRepr tp -> StridedInterval tp -> Integer -> StridedInterval tp
-- filterLeq tp@(BVTypeRepr _) si x = glb si (mkStridedInterval tp False 0 x 1)

-- filterGeq :: TypeRepr tp -> StridedInterval tp -> Integer -> StridedInterval tp
-- filterGeq tp@(BVTypeRepr _) si x = glb si (mkStridedInterval tp False x u 1)
--   where
--     u = case tp of BVTypeRepr n -> maxUnsigned n

-- | Truncate an interval.

-- OPT: this could be made much more efficient I think.
trunc :: -- (v+1 <= u) =>
  StridedInterval (BVType u)
  -> NatRepr v
  -> StridedInterval (BVType v)
trunc EmptyInterval _ = EmptyInterval
trunc si sz
  -- No change/complete wrap case --- happens when we add
  -- (unsigned int) -1, for example.
  | si' `isSubsetOf` top' = si'
  | otherwise     = go pfx (next_g pfx) (range si - (range pfx + 1))
  where
    mk_range b r =
      let max_range = (maxUnsigned sz - b) `div` stride si
      in if r <= max_range then r else max_range

    pfx = si' { range = mk_range (base si') (range si') }

    next_g new_si = toUnsigned sz (intervalEnd new_si + stride si)
    -- FIXME: we should stop when we see repeated elements, but it
    -- might be faster to do it this way.
    go acc g n
      -- no more range left
      | n < 0 = acc
      -- we hit a cycle (maybe via lub)
      -- | g `member` acc = acc
      | otherwise      =
          let new_range = mk_range g n
              new_si = StridedInterval { typ = typ top'
                                       , base = g
                                       , range = new_range
                                       , stride = stride si }
          in -- trace ("new_si at " ++ show n ++ " " ++ show (pretty new_si)
             --        ++ " acc " ++ show (pretty acc)) $
             go (lub acc new_si) (next_g new_si) (n - (range new_si + 1))

    si'  = si { typ = typ top'
              , base = toUnsigned sz (base si) }
    top' = top sz      

prop_trunc :: StridedInterval (BVType 64)
              -> Positive (Small Integer)
              -> Property
prop_trunc si sz
  | Just (Some n) <- someNat sz' = sz' < 64 ==> p n
  | otherwise = True ==> True
  where
    p :: NatRepr n -> Bool
    p n = S.fromList (map (toUnsigned n) (toList si))
          `S.isSubsetOf`
          S.fromList (toList (trunc si n))
    sz' = getSmall (getPositive sz)

-- -----------------------------------------------------------------------------
-- Testing

mk_prop :: (Integer -> Integer -> Integer)
           -> (NatRepr 64
               -> StridedInterval (BVType 64)
               -> StridedInterval (BVType 64)
               -> StridedInterval (BVType 64))
           -> StridedInterval (BVType 64)
           -> StridedInterval (BVType 64)
           -> Bool
mk_prop int_f si_f x y = and [ (toUnsigned n64 (int_f v v'))
                               `member`
                               (si_f n64 x y)
                             | v  <- toList x
                             , v' <- toList y ]

toList :: StridedInterval (BVType sz) -> [Integer]
toList EmptyInterval        = []
toList si@StridedInterval{} = map (\v -> base si + stride si * v) [0 .. range si]

instance Pretty (StridedInterval tp) where
  pretty EmptyInterval        = brackets empty
  pretty si | Just s <- isSingleton si = brackets (integer s)
  pretty si@StridedInterval{} = brackets (integer (base si) <> comma
                                          <+> integer (base si + stride si)
                                          <+> text ".."
                                          <+> integer (base si + range si * stride si))

instance Arbitrary (StridedInterval (BVType 64)) where
  arbitrary = frequency [ (1, return EmptyInterval)
                        , (9, si) ]
    where
      si = do lower <- arbitrarySizedNatural
              upper <- suchThat arbitrarySizedNatural (>= lower)
              s     <- suchThat arbitrarySizedNatural (> 0)
              return $ mkStridedInterval (BVTypeRepr n64) True lower upper s
