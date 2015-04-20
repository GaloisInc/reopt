------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.CodePointerDiscovery
-- Description      : This discovers potential code pointers that are
--                    written to memory.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- Finds all values written to memory that look like a code pointer.
------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
module Reopt.Semantics.CodePointerDiscovery
  ( discoverCodePointers
  ) where

import           Data.Foldable (foldMap)
import           Data.Set (Set)
import qualified Data.Set as S

import           Data.Parameterized.NatRepr (widthVal)
import           Reopt.Memory
import           Reopt.Semantics.Representation

-- | This looks for any places where a 64-bit value pointing to the code
-- segment is written into
discoverCodePointers :: Memory CodeAddr -> CFG -> BlockLabel -> Set CodeAddr
discoverCodePointers mem cfg root = traverseBlocks cfg root (scanBlock mem) merge
  where merge l v r = l `S.union` v `S.union` r

scanBlock :: Memory CodeAddr -> Block -> Set CodeAddr
scanBlock mem = foldMap go . blockStmts
  where

    go :: Stmt -> Set CodeAddr
    -- FIXME: remove hard coded size value here.
    -- At the moment, our criteria is simply that the pointer is 64 bits
    -- and points somewhere in the code section(s).
    go (Write _ (BVValue sz val))
      | widthVal sz == 64
      , isCodePointer val          = S.singleton (fromInteger val)
    go _                           = S.empty

    isCodePointer :: Integer -> Bool
    isCodePointer val = addrHasPermissions (fromInteger val) pf_x mem
