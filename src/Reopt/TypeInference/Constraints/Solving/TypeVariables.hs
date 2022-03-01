{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reopt.TypeInference.Constraints.Solving.TypeVariables
  ( TyVar (TyVar),
  )
where

import qualified Prettyprinter as PP
import Data.Function (on)

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
