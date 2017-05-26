{-|
Copyright        : (c) Galois, Inc 2015-2017
Maintainer       : Simon Winwood <sjw@galois.com>

This contains a function "execInstruction" that steps a single Flexdis86
instruction.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Reopt.Semantics.FlexdisMatcher
  ( execInstruction
  , semanticsMap
  ) where

import           Data.Foldable
import           Data.Macaw.Types
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Flexdis86 as F

import           Reopt.Semantics.InstructionDef
import           Reopt.Semantics.Monad
import           Reopt.Semantics.Semantics

------------------------------------------------------------------------
-- Instruction semantics

mapNoDupFromList :: (Ord k, Show k) => [(k,v)] -> Either k (Map k v)
mapNoDupFromList = foldlM ins M.empty
  where ins m (k,v) =
          case M.lookup k m of
            Just _ -> Left k
            Nothing -> Right (M.insert k v m)

semanticsMap :: Map String InstructionSemantics
semanticsMap =
  case mapNoDupFromList all_instructions of
    Right m -> m
    Left k -> error $ "semanticsMap contains duplicate entries for " ++ show k ++ "."

-- | This function executes a single instruction.
--
-- We divide instructions into
--   * regular:   those which take arguments of the same, polymorphic, width
--   * irrugular: those which have particular parsing requirements
--   * fixed:     those which have exact sizes known

-- FIXME: do something more interesting here than 'Maybe'
execInstruction :: (FullSemantics m)
                => Value m (BVType 64)
                   -- ^ Next ip address
                -> F.InstructionInstance
                -> Maybe (m ())
execInstruction next ii =
  case M.lookup (F.iiOp ii) semanticsMap of
    Just (InstructionSemantics f) -> Just $ do
      rip .= next
      f ii -- (F.iiLockPrefix ii) (F.iiAddrSize ii) (F.iiArgs ii)
    Nothing -> Nothing
