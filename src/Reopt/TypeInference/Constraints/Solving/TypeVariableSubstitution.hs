{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Reopt.TypeInference.Constraints.Solving.TypeVariableSubstitution
  ( SubstTyVar (substTyVar),
  )
where

import Reopt.TypeInference.Constraints.Solving.Constraints
  ( AndC (AndC),
    EqC (EqC),
    InRowC (InRowC),
    OrC (OrC),
    TyConstraint (AndTC, EqRowTC, EqTC, InRowTC, OrTC),
  )
import Reopt.TypeInference.Constraints.Solving.TypeVariables (TyVar)
import Reopt.TypeInference.Constraints.Solving.Types (ITy, Ty (NumTy, PtrTy, RecTy, UnknownTy))

-- FIXME using an explicit substitution function is simple to reason about...
-- but if we're doing it _a lot_ that could be slow, so we might want to switch
-- to using an explicit substitution data structure that is kept in the context
-- and used resolve things on an as needed basis.
class SubstTyVar a where
  substTyVar :: (TyVar, ITy) -> a -> a

instance SubstTyVar ITy where
  substTyVar xt@(x, xTy) = \case
    UnknownTy y -> if x == y then xTy else UnknownTy y
    NumTy sz -> NumTy sz
    PtrTy t -> PtrTy $ substTyVar xt t
    RecTy flds rvar -> RecTy (fmap (substTyVar xt) flds) rvar

instance SubstTyVar EqC where
  substTyVar xt (EqC l r) = EqC (substTyVar xt l) (substTyVar xt r)

instance SubstTyVar InRowC where
  substTyVar xt (InRowC r o t) = InRowC r o (substTyVar xt t)

instance SubstTyVar AndC where
  substTyVar xt (AndC cs) = AndC $ map (substTyVar xt) cs

instance SubstTyVar OrC where
  substTyVar xt (OrC cs) = OrC $ map (substTyVar xt) cs

instance SubstTyVar TyConstraint where
  substTyVar xt = \case
    EqTC c -> EqTC $ substTyVar xt c
    InRowTC c -> InRowTC $ substTyVar xt c
    OrTC c -> OrTC $ substTyVar xt c
    AndTC c -> AndTC $ substTyVar xt c
    EqRowTC c -> EqRowTC c -- no TyVars in Rows
