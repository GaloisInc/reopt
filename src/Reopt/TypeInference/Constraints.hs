-- Loosely based on 'TIE: Principled Reverse Engineering of Types in Binary
-- Programs' (NDSS 2011) by Jonghyup Lee, Thanassis Avgerinos, and David Brumley

-- Not clear yet how much will directly be applicable etc.
module Reopt.TypeInference.Constraints where



data Ty
  = TyData DataTy
  | TyFun
  | TyTop
  | TyBot
  | TyAnd Ty Ty
  | TyOr Ty Ty
  | TyArrow Ty Ty

data DataTy
  = DTyBase BaseTy
  | DTyMem MemTy


data BaseTy
  = BTyBase RegTy
  | BTyRefined RefinedTy

-- | Maps each memory cell address to the type stored at that address.
data MemTy = MemTy -- ??? {∀ addresses i|li : Ti } wut is that? a set of address/type mappings?



-- | Register types, Cf. τ_reg
data RegTy
  = Reg1Ty
  | Reg8Ty
  | Reg16Ty
  | Reg32Ty
  | Reg64Ty


-- | Refined types, Cf. τ_refined
data RefinedTy
  = Num8RTy
  | Num16RTy
  | Num32RTy
  | Num64RTy
  | UInt8RTy
  | UInt16RTy
  | UInt32RTy
  | UInt64RTy
  | Int8RTy
  | Int16RTy
  | Int32RTy
  | Int64RTy
  | PtrRTy Ty
  | CodeRTy
