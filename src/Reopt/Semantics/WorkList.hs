------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.WorkList
-- Description      : Control Flow Graph discovery support
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Simon Winwood <sjw@galois.com>
-- Stability        : provisional
--
-- An implementation of a worklist algorithm
------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}

-- FIXME: move somewhere else 
module Reopt.Semantics.WorkList
       ( WorkList
       , null
       , split
       , empty
       , singleton
       , fromSet
       , insert
       , insertFold
       , iterate
       ) where

import           Control.Applicative ( (<$>) )
import           Control.Lens
import qualified Data.Foldable as Fold
import           Data.Set (Set)
import qualified Data.Set as S
import           Prelude hiding (null, iterate)

------------------------------------------------------------------------
-- Worklists
--

-- | A worklist is a datastructure that is more or less set-like.  The
-- implementation is hidden to allow us to replace the set by a list,
-- for example.
--
-- In particular, double insertion is idempotent

newtype WorkList a = WL { _workList :: Set a }
                              
workList :: Simple Lens (WorkList a) (Set a)
workList = lens _workList (\s v -> s { _workList = v })

null :: WorkList a -> Bool
null = S.null . _workList

split :: Ord a => WorkList a -> Maybe (a, WorkList a)
split wl = (_2 %~ WL) <$> S.minView (wl^.workList)

empty :: WorkList a
empty = WL S.empty

singleton :: a -> WorkList a
singleton = WL . S.singleton

fromSet :: Set a -> WorkList a
fromSet = WL


-- | Inserts a new element into the worklist, ignoring duplicates.
insert :: Ord a => a -> WorkList a -> WorkList a
insert v = workList %~ S.insert v

-- | Inserts a collection of elements into the worklist, ignoring duplicates.
insertFold :: (Ord a, Fold.Foldable t) => t a -> WorkList a -> WorkList a
insertFold vs wl = Fold.foldl' (\b a -> b & workList %~ S.insert a) wl vs

iterate :: (Ord a, Monad m) => (a -> m [a]) -> WorkList a -> m ()
iterate f = go
  where
    go wl 
      | Just (v, wl') <- split wl =
          do nexts <- f v
             go (insertFold nexts wl')
      | otherwise = return ()
