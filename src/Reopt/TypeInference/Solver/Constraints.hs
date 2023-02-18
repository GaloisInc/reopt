{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Reopt.TypeInference.Solver.Constraints where

import qualified Data.Set                                 as Set
import           GHC.Generics                             (Generic)
import qualified Prettyprinter                            as PP
import           Reopt.TypeInference.Solver.RowVariables  (RowExpr, Offset)
import           Reopt.TypeInference.Solver.TypeVariables (TyVar)
import           Reopt.TypeInference.Solver.Types         (FreeRowVars (..),
                                                           FreeTyVars (..), ITy)

-- | @EqC t1 t2@ means @t1@ and @t2@ are literally the same type.
data EqC = EqC {eqLhs :: !TyVar, eqRhs :: !ITy }
  deriving (Eq, Ord, Show, Generic)

prettySExp :: [PP.Doc ann] -> PP.Doc ann
prettySExp docs = PP.group $ PP.encloseSep "(" ")" " " docs

instance PP.Pretty EqC where
  pretty (EqC l r) = prettySExp [PP.pretty l, "=", PP.pretty r]

instance FreeTyVars EqC where
  freeTyVars (EqC t1 t2) = Set.union (freeTyVars t1) (freeTyVars t2)

instance FreeRowVars EqC where
  freeRowVars (EqC t1 t2) = Set.union (freeRowVars t1) (freeRowVars t2)

-- | Stands for: lhs = { offsets | rhs }
data EqRowC = EqRowC
  { eqRowLHS :: !RowExpr,
    eqRowRHS :: !RowExpr
  }
  deriving (Eq, Ord, Show)

instance PP.Pretty EqRowC where
  pretty (EqRowC r1 r2) = prettySExp [PP.pretty r1, "=", PP.pretty r2]

instance FreeTyVars EqRowC where
  freeTyVars (EqRowC _ _) = mempty

instance FreeRowVars EqRowC where
  freeRowVars (EqRowC r1 r2) = freeRowVars r1 `Set.union` freeRowVars r2

-- --------------------------------------------------------------------------------
-- -- Pointer addition (maybe, could be number add)

-- | What sort of constant operand we are dealing with for an
-- addition.
data OperandClass =
  OCSymbolic   -- ^ The second operand is not a constant
  | OCOffset Offset -- ^ The second operand is a small constant, we
                     -- already know it is a number (from constraint
                     -- generation.)
  | OCPointer -- ^ The second operand could be in an ELF segment.
  deriving (Eq, Ord, Show)

instance PP.Pretty OperandClass where
  pretty c =
    case c of
      OCSymbolic      -> mempty
      OCOffset offset -> prettySExp ["+", PP.pretty offset]
      OCPointer       -> "?"

instance FreeTyVars OperandClass where
  freeTyVars _ = mempty

-- data PtrAddC = PtrAddC
--   { ptrAddResult :: TyVar
--   , ptrAddLHS    :: TyVar
--   -- ^ The symbolic operand.
--   , ptrAddRHS    :: TyVar
--   , ptrAddClass  :: OperandClass
--   -- ^ If an operand is a constant then this is that constant.
--   }
--   deriving (Eq, Ord, Show)

-- instance PP.Pretty PtrAddC where
--   pretty (PtrAddC resTy lTy rTy oc) =
--     prettySExp [ PP.pretty resTy, "=", PP.pretty lTy, PP.pretty rTy, PP.pretty oc]

-- instance FreeTyVars PtrAddC where
--   freeTyVars (PtrAddC resTy lTy rTy oc) = Set.fromList [resTy, lTy, rTy] <> freeTyVars oc

-- instance FreeRowVars PtrAddC where
--   freeRowVars PtrAddC {} = mempty

--------------------------------------------------------------------------------
-- SubType

data SubC t = SubC !t !t
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

infix 5 :<:
pattern (:<:) :: a -> a -> SubC a
pattern a :<: b = SubC a b
{-# COMPLETE (:<:) #-}

type SubTypeC = SubC TyVar
type SubRowC  = SubC RowExpr

instance PP.Pretty a => PP.Pretty (SubC a) where
  pretty (a :<: b) =
    prettySExp [ PP.pretty a, "<:", PP.pretty b]

instance FreeTyVars a => FreeTyVars (SubC a) where
  freeTyVars (a :<: b) = freeTyVars a `Set.union` freeTyVars b

instance FreeRowVars a => FreeRowVars (SubC a) where
  freeRowVars (a :<: b) = freeRowVars a `Set.union` freeRowVars b
