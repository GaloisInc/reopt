{-|
Copyright        : (c) Galois, Inc 2015
Maintainer       : Simon Winwood <sjw@galois.com>

A strided interval domain x + [a .. b] * c
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- FIXME: take rounding/number of bits/etc. into consideration
-- FIXME: only really useful for unsigned?
module Data.Macaw.AbsDomain.StridedInterval
       ( StridedInterval(..)
         -- Constructors
       , singleton, mkStridedInterval, fromFoldable
         -- Predicates
       , isSingleton, isTop, member, isSubsetOf
         -- Destructors
       , toList, intervalEnd, size
         -- Domain operations
       , lub, lubSingleton, glb
         -- Operations
       , bvadd, bvmul, trunc
         -- Debugging
       ) where


import           Control.Exception (assert)
import qualified Data.Foldable as Fold
import           Data.Parameterized.NatRepr
import           GHC.TypeLits (Nat)
import           Test.QuickCheck
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty)

-- import           Data.Macaw.DebugLogging

-- -----------------------------------------------------------------------------
-- Data type decl and instances

-- This is a canonical (and more compact) representation, basically we
-- turn x + [a .. b] * c into (x + a * c) + [0 .. b - a] * c

data StridedInterval (w :: Nat) =
  StridedInterval { typ    :: !(NatRepr w) -- ^ number of bits in type.
                  , base   :: !Integer
                  , range  :: !Integer -- ^ This is the number of elements in the interval + 1
                  , stride :: !Integer
                  }

instance Eq (StridedInterval tp) where
  si1@StridedInterval{} == si2@StridedInterval{} =
    base si1 == base si2 && range si1 == range si2 && stride si1 == stride si2

instance Show (StridedInterval tp) where
  show = show . pretty

intervalEnd :: StridedInterval tp -> Integer
--intervalEnd EmptyInterval = error "intervalEnd"
intervalEnd si = base si + range si * stride si

size :: StridedInterval tp -> Integer
--size EmptyInterval = 0
size si = range si + 1

-- -----------------------------------------------------------------------------
-- Constructors

-- | Construct a singleton value
singleton :: NatRepr w -> Integer -> StridedInterval w
singleton tp v = StridedInterval { typ = tp
                                 , base = v
                                 , range = 0
                                 , stride = 1
                                 }

empty :: NatRepr w -> StridedInterval w
empty tp =
  StridedInterval { typ = tp
                  , base = 0
                  , range = -1
                  , stride = 1
                  }

-- | Make an interval given the start, end, and stride. Note that this
-- will round up if (start - end) is not a multiple of the stride,
-- i.e., @mkStr
mkStridedInterval :: NatRepr w -> Bool
                  -> Integer -> Integer -> Integer
                  -> StridedInterval w
mkStridedInterval tp roundUp start end s
  | end < start = empty tp
  | s == 0          = singleton tp start
  | r == 0          = singleton tp start
  | otherwise       =
      StridedInterval { typ = tp
                      , base = start
                      , range = r
                      , stride = s }
  where
    r | roundUp = ((end - start) + (s - 1)) `div` s
      | otherwise = (end - start) `div` s

fromFoldable :: Fold.Foldable t =>
                NatRepr n -> t Integer -> StridedInterval n
fromFoldable sz vs
  | isEmptyV vs  = empty sz
  | otherwise    = mkStridedInterval sz True start end s
  where
    isEmptyV = not . Fold.any (const True)
    start    = Fold.minimum vs
    end      = Fold.maximum vs
    -- This is a bit of a hack, relying on the fact that gcd 0 v == v
    s       = Fold.foldl' (\g v -> gcd g (v - start)) 0 vs


-- -----------------------------------------------------------------------------
-- Predicates

isEmpty :: StridedInterval w -> Bool
isEmpty s = range s < 0

isSingleton :: StridedInterval w -> Maybe Integer
isSingleton StridedInterval { base = b, range = 0 } = Just b
isSingleton _  = Nothing

isTop :: StridedInterval w -> Bool
isTop si = si == top (typ si)

member :: Integer -> StridedInterval w -> Bool
member _ si | isEmpty si = False
member n si = assert (stride si /= 0) $
              base si <= n
              && (n - base si) `mod` stride si == 0
              && n <= base si + stride si * range si

-- is the set represented by si1 contained in si2?
isSubsetOf :: StridedInterval w
       -> StridedInterval w
       -> Bool
isSubsetOf si1 _ | isEmpty si1 = True
isSubsetOf si1 si2
  | Just s <- isSingleton si1 = member s si2
  | otherwise = member (base si1) si2
                && member (intervalEnd si1) si2
                && stride si2 <= gcd (stride si1) (stride si2)


-- -----------------------------------------------------------------------------
-- Domain operations

lub :: StridedInterval w
       -> StridedInterval w
       -> StridedInterval w
lub s t | isEmpty s = t
lub s t | isEmpty t = s
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

-- prop_lub :: StridedInterval (BVType 64)
--             -> StridedInterval (BVType 64)
--             -> Bool
-- prop_lub x y = x `isSubsetOf` (x `lub` y)
--                && y `isSubsetOf` (x `lub` y)

lubSingleton :: Integer
                -> StridedInterval w
                -> StridedInterval w
lubSingleton s si
  | member s si  = si
  | Just s' <- isSingleton si =
      let l = (min s s')
          u = (max s s')
      in mkStridedInterval (typ si) True l u (u - l)
  | s < base si  = go s si_upper (base si)
  | otherwise    = go (base si) (max s si_upper) s
  where
    si_upper = intervalEnd si
    go lower upper to_contain =
      mkStridedInterval (typ si) True lower upper
                        (gcd (stride si) (to_contain - lower))

-- | Greatest lower bound.  @glb si1 si2@ contains only those values
-- which are in @si1@ and @si2@.
glb :: StridedInterval w
       -> StridedInterval w
       -> StridedInterval w
--glb EmptyInterval _ = EmptyInterval
--glb _ EmptyInterval = EmptyInterval
glb si1 si2
  | Just s' <- isSingleton si1 =
      if s' `member` si2 then si1 else empty (typ si1)
  | Just s' <- isSingleton si2 =
      if s' `member` si1 then si2 else empty (typ si1)
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
  | otherwise = empty (typ si1)
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
floor_quot _ 0 = error "floor_quot div by 0"
floor_quot x y = x `div` y

-- prop_sld :: Positive Integer -> Positive Integer
--             -> NonZero Integer -> Positive Integer -> Positive Integer
--             -> Property
-- prop_sld a b c d e = not (isNothing v) ==> p
--   where
--     p = case v of
--          Just (x, y) -> x >= 0 && y >= 0
--                         && x <= getPositive d
--                         && y <= getPositive e
--                         && (getPositive a) * x - (getPositive b) * y == (getNonZero c)
--          _ -> True
--     v = solveLinearDiophantine (getPositive a) (getPositive b) (getNonZero c)
--                                (getPositive d) (getPositive e)


-- | Returns the gcd, and n and m s.t. n * a + m * b = g
-- clagged, fixed, from
--    http://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- this is presumably going to be slower than the gmp version :(
eGCD :: Integral a => a -> a -> (a,a,a)
eGCD a0 b0 = let (g, m, n) = go a0 b0
           in if g < 0 then (-g, -m, -n) else (g, m, n)
  where
    go a 0 = (a, 1, 0)
    go a b = let (g, x, y) = go b $ rem a b
             in (g, y, x - (a `quot` b) * y)

-- prop_eGCD :: Integer -> Integer -> Bool
-- prop_eGCD x y = let (g, a, b) = eGCD x y in x * a + y * b == g

-- -----------------------------------------------------------------------------
-- Operations

-- These operations probably only really make sense for constants or
-- constant ranges.  We can always just make the stride 1, but this
-- loses information.

-- We have x + [0..a] * b + y + [0 .. d] * e
-- = (x + y) + { 0 * b, 0 * e, 1 * b, 1 * e, ..., a * b, d * e }
-- `subsetOf` let m = gcd(b, e)
--            in (x + y) + [0 .. (a * b / m) + (d * e / m) ] * m

top :: NatRepr u -> StridedInterval u
top sz = StridedInterval { typ = sz
                         , base = 0
                         , range = maxUnsigned sz
                         , stride = 1 }

clamp :: NatRepr u -> StridedInterval u -> StridedInterval u
clamp sz v = trunc v sz

bvadd :: NatRepr u
      -> StridedInterval u
      -> StridedInterval u
      -> StridedInterval u
--bvadd _ EmptyInterval{} _ = EmptyInterval
--bvadd _ _ EmptyInterval{} = EmptyInterval
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
    r | m == 0 = error "bvadd given 0 stride"
      | otherwise = (range si1 * (stride si1 `div` m)) + (range si2 * (stride si2 `div` m))

-- prop_bvadd ::  StridedInterval (BVType 64)
--             -> StridedInterval (BVType 64)
--             -> Bool
-- prop_bvadd = mk_prop (+) bvadd

-- We have (x + [0..a] * b) * (y + [0 .. d] * e)
-- = (x * y) + [0..a] * b * y + [0 .. d] * e * x + ([0..a] * b * [0..d] * e)
-- `subsetOf`  (x * y) + [0..a] * (b * y)
--           + 0       + [0 .. d] * (e * x)
--           + 0       + ([0..a*d] * (b * e)

bvmul :: NatRepr u
      -> StridedInterval u
      -> StridedInterval u
      -> StridedInterval u
--bvmul _ EmptyInterval{} _ = EmptyInterval
--bvmul _ _ EmptyInterval{} = EmptyInterval
-- bvmul sz si1 si2 = top sz -- FIXME: this blows up with the trunc, unfortunately
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

-- prop_bvmul ::  StridedInterval (BVType 64)
--             -> StridedInterval (BVType 64)
--             -> Bool
-- prop_bvmul = mk_prop (*) bvmul

-- filterLeq :: NatRepr w -> StridedInterval w -> Integer -> StridedInterval w
-- filterLeq tp@(BVTypeRepr _) si x = glb si (mkStridedInterval tp False 0 x 1)

-- filterGeq :: NatRepr w -> StridedInterval w -> Integer -> StridedInterval w
-- filterGeq tp@(BVTypeRepr _) si x = glb si (mkStridedInterval tp False x u 1)
--   where
--     u = case tp of BVTypeRepr n -> maxUnsigned n

-- | Returns the least b' s.t. exists i < n. b' = (b + i * q) mod m
-- This is a little tricky.  We want to find { b + i * q | i <- {0 .. n} } mod M
-- Now, if we know the values in {0..q} we can figure out the whole sequence.
-- In particular, we are looking for the new b --- the stride is gcd q M (assuming wrap)
--
-- Let q_w be k * q mod M s.t. 0 <= q_w < q.  Alternately, q_w = q - M mod q
--
-- Assume wlog b0 = b < q.  Take
-- b1 = b0 + q_w
-- b2 = b1 + q_w
-- b3 = ...
--
-- i.e. bs = { b + i * q_w | i <- {0 .. n `div` ceil(m / q)} }
--
-- Now, we are interested in these value mod q, so take
-- bs = { b0 + i * q_w } mod q
--
-- Thus, we get a recursion of (M, q) (q, q_w) (q_w, ...)
--
-- = (M, q) (q, q - M mod q) (q - M mod q, (q - M mod q) - q mod (q - M mod q))
--
-- Base cases:
--  Firstly, when q divides M, we can stop, or n == 0
--  Otherwise, for small M we can search for some i, that is
--  the least 0 <= k < M s.t.
--
--  EX i. (b + i * q) mod M = k
--

-- Assumes b < q (?)
-- currently broken :(
-- leastMod :: Integer -> Integer -> Integer -> Integer -> Integer
-- leastMod _ 0 _ _ = error "leastMod given m = 0"
-- leastMod _ _ 0 _ = error "leastMod given q = 0"
-- leastMod b m q n
--   | b + n * q < m  = b -- no wrap
--   | m `mod` q == 0 = b -- assumes q <= m
--   | otherwise =
--       debug DAbsInt (show ((b, m, q, n), (next_b, m', q', next_n, next_n `div` m_div_q))) $
--       leastMod next_b m' q'
--                 -- FIXME: we sometimes miss a +1 here, we do this to
--                 -- be conservative (overapprox.)
--       (next_n `div` m_div_q)
--   where
--     m_div_q | q == 0 = error "leastMod given q == 0"
--             | r == 0 = error "leastMod given m `div` q == 0"
--             | otherwise = r
--       where r = m `div` q
--     m' = q
--     q' | q == 0 = error "leastMod given q == 0"
--        | otherwise = q - m `mod` q
--     (next_b, next_n)
--       | b < q'    = (b, n)
--       | otherwise = let i = m_div_q + 1
--                     in (((b + i * q) `mod` m) `mod` q, n - i)

-- | Truncate an interval.

-- OPT: this could be made much more efficient I think.
trunc :: StridedInterval u
      -> NatRepr v
      -> StridedInterval v
trunc si sz
  | isTop si              = top'
  -- No change/complete wrap case --- happens when we add
  -- (unsigned int) -1, for example.
  | si' `isSubsetOf` top' = si'
  -- where stride is a power of 2 (well, divides 2 ^ sz), we easily
  -- figure out the new base and just over-approximate by all the values
  | modulus `mod` stride si == 0 =
      let base' = (base_mod_sz
                  + (stride si * ((modulus - base_mod_sz) `ceilDiv` stride si)))
                  `mod` modulus
      in si' { base = base', range = (modulus `ceilDiv` stride si) - 1 }
   -- We wrap at least once
  | otherwise     = top'
  where
    modulus = 2 ^ (natValue sz)
    si'  = si { typ = typ top'
              , base = toUnsigned sz (base si) }
    top' = top sz
    base_mod_sz = base si'
    -- positive only
    ceilDiv _ 0 = error "SI.trunc given 0 stride."
    ceilDiv x y = (x + y - 1) `div` y

-- -----------------------------------------------------------------------------
-- Testing

toList :: StridedInterval w -> [Integer]
toList si@StridedInterval{} = map (\v -> base si + stride si * v) [0 .. range si]

instance Pretty (StridedInterval w) where
  pretty si | isEmpty si = text "[]"
  pretty si | Just s <- isSingleton si = brackets (integer s)
  pretty si@StridedInterval{} = brackets (integer (base si) <> comma
                                          <+> integer (base si + stride si)
                                          <+> text ".."
                                          <+> integer (base si + range si * stride si))

instance Arbitrary (StridedInterval 64) where
  arbitrary = frequency [ (1, return (empty knownNat))
                        , (9, si) ]
    where
      si = do lower <- sized $ \n -> choose (0, toInteger n)
              upper <- sized $ \n -> choose (lower, toInteger n)
              s     <- sized $ \n -> choose (1, toInteger n)
              return $ mkStridedInterval knownNat True lower upper s
