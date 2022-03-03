{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reopt.TypeInference.Solver.TypeVariables
  ( TyVar (TyVar),
  )
where

import           Data.Function (on)
import qualified Prettyprinter as PP

data TyVar = TyVar
  { tyVarInt :: Int,
    -- | If you want to record the origin of this type variable, it will show
    -- out when you pretty-print it.  Recommended, except in the test suite
    -- where there is not much point in marking test type variables.
    tyVarOrigin :: Maybe String
  }
  deriving (Show)

instance Eq TyVar where
  (==) = (==) `on` tyVarInt

instance Ord TyVar where
  compare = compare `on` tyVarInt

instance PP.Pretty TyVar where
  pretty tyv = PP.hcat ["α", PP.pretty (tyVarInt tyv), maybeOrigin (tyVarOrigin tyv)]
    where
      maybeOrigin Nothing = mempty
      maybeOrigin (Just origin) = PP.space <> PP.parens (PP.pretty origin)
