------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.DeadRegisterElimination
-- Description      : A CFG pass to remove registers which are not used
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- Finds all values which look like a code pointer.
-- 
------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}

module Reopt.Semantics.CodePointerDiscovery where

-- import           Control.Applicative ((<$>), (<*>))
-- import           Control.Lens
-- import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Foldable (foldMap)
import           Data.List (find)
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.IntervalMap.FingerTree as IMap

import           Data.Parameterized.NatRepr (widthVal)
import           Reopt.Memory
import           Reopt.Semantics.Representation

type ElfCodeSections = [(CodeAddr, CodeAddr)] -- start, end

-- FIXME: secs = \cfg root -> ... so ghc will inline properly?
discoverCodePointers :: Memory CodeAddr -> CFG -> BlockLabel -> Set CodeAddr
discoverCodePointers mem cfg root = traverseBlocks cfg root (scanBlock mem) merge
  where
    merge l v r = l `S.union` v `S.union` r

scanBlock :: Memory CodeAddr -> Block -> Set CodeAddr
scanBlock mem = foldMap go . blockStmts
  where

    go :: Stmt -> Set CodeAddr
    -- FIXME: remove hard coded size value here.
    -- At the moment, our criteria is simply that the pointer is 64 bits
    -- and points somewhere in the code section(s).
    go (Write _ (BVValue sz val))
      | widthVal sz == 64
      , isCodePointer val          = S.singleton (fromIntegral val)
    go _                           = S.empty
    
    isCodePointer :: Integer -> Bool
    isCodePointer val = addrHasPermissions (fromIntegral val) pf_x mem
