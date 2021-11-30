{-# LANGUAGE LambdaCase #-}
-- Loosely based on 'TIE: Principled Reverse Engineering of Types in Binary
-- Programs' (NDSS 2011) by Jonghyup Lee, Thanassis Avgerinos, and David Brumley

-- Not clear yet how much will directly be applicable etc.
module Reopt.TypeInference.Constraints where


data NumSize
  = NumSize8
  | NumSize16
  | NumSize32
  | NumSize64
  deriving (Eq, Ord)


newtype TyVar = TyVar {tyVarInt :: Int }
  deriving (Eq, Ord)

-- | Types of values in x86 machine code
data X86Ty
  = UnknownTy -- the top type
  | AbsurdTy -- the bottom type
  | VarTy TyVar -- a type unification variable
  | PtrTy
  | NumTy NumSize
  | ScalarFloatTy
  | VecFloatTy
  deriving (Eq, Ord)

-- | x86 type constraints
data X86TyConstraint
  = -- | These types are known to be equal.
    EqC X86Ty X86Ty
  -- | These types were used in an addition operation of width @NumSize@.
  | AddC NumSize X86Ty X86Ty

numSizeInt :: NumSize -> Int
numSizeInt =
  \case
    NumSize8 -> 8
    NumSize16 -> 16
    NumSize32 -> 32
    NumSize64 -> 64

-- | @subtype t1 t2@ returns @True@ iff @t1@ is a subtype of @t2@.
subtype :: X86Ty -> X86Ty -> Bool
subtype t1 t2 =
  case (t1,t2) of
    (_,_) | t1 == t2 -> True
    (_, UnknownTy) -> True
    (AbsurdTy, _) -> True
    (NumTy n1, NumTy n2) -> (numSizeInt n1) <= (numSizeInt n2)
    (_,_) -> False




-- -- | Calculates the least upper bound of two types (denoted by the “join”
-- operator ⊔).
-- join :: Ty -> Ty -> Ty
-- join  = undefined -- FIXME

-- -- | Calculates the greatest lower bound of two types (denoted by the meet”
-- -- operator ⊓).
-- meet :: Ty -> Ty -> Ty
-- meet  = undefined -- FIXME
