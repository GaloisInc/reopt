{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Reopt.Semantics.Monad
 ( module Reopt.Semantics.Monad 
 , Bits.Bits
 , (Bits..&.)
 ) where

import Control.Applicative
import Data.Bits as Bits
import Data.Word

------------------------------------------------------------------------
-- InterruptNo

newtype InterruptNo = InterruptNo { interruptNoIdx :: Word8 }

de :: InterruptNo
de = InterruptNo 0

db :: InterruptNo
db = InterruptNo 1

------------------------------------------------------------------------
-- SemanticsMonad

data family Value (m :: * -> *) tp
data family Assignable (m :: * -> *) tp

type Pred m = Value m Bool

class (Applicative m, Monad m) => ProcessorMonad m where
  af_flag :: Assignable m Bool
  cf_flag :: Assignable m Bool
  of_flag :: Assignable m Bool
  pf_flag :: Assignable m Bool
  sf_flag :: Assignable m Bool
  zf_flag :: Assignable m Bool

  false :: Value m Bool
  true  :: Value m Bool

  undef :: Value m Bool

  even_parity :: Value m Word8 -> Pred m
  uadd4_overflows :: Value m Word8 -> Value m Word8 -> Pred m

{-
  mux :: Pred m -> m () -> m () -> m ()
-}

{-
  getPC :: m (ValueOf m Word64)
  setPC :: ValueOf m Word64 -> m ()

  incPC :: SemanticsMonad m => Int64 -> m ()


  getMem :: TypeOf tp -> ValueOf m Word64 -> m (ValueOf m tp)
  setMem :: TypeOf tp -> ValueOf m Word64 -> ValueOf m tp -> m ()

  getReg8 :: Reg8 -> m (ValueOf m Word8)
  setReg8 :: Reg8 -> ValueOf m Word8 -> m ()

  getReg16 :: Reg16 -> m (ValueOf m Word16)
  setReg16 :: Reg16 -> ValueOf m Word16 -> m ()

  getReg32 :: Reg32 -> m (ValueOf m Word32)
  setReg32 :: Reg32 -> ValueOf m Word32 -> m ()

  getReg64 :: Reg64 -> m (ValueOf m Word64)
  setReg64 :: Reg64 -> ValueOf m Word64 -> m ()

  getFlag :: RFLAGS -> m (Value1 m)   
  setFlag :: RFLAGS -> Value1 m -> m ()

  raiseInterrupt :: InterruptNo -> m ()
  exec_syscall :: m ()
-}

class ProcessorMonad m => IsAssignable m tp where
  get :: Assignable m tp -> m (Value m tp)
  (.=) :: Assignable m tp -> Value m tp -> m ()

{-
class IsBV m tp where
  msb :: Value m tp -> Pred m
  is_zero :: Value m tp -> Pred m
  sadd_overflows :: Value m tp -> Value m tp -> Pred m
  uadd_overflows :: Value m tp -> Value m tp -> Pred m
  least_byte :: Value m tp -> Value m Word8

  -- | bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsf :: Value m tp -> Value m tp
  -- | bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsr :: Value m tp -> Value m tp
-}

class IsBV v tp where
  msb :: v tp -> v Bool
  is_zero :: v tp -> v Bool
  sadd_overflows :: v tp -> v tp -> v Bool
  uadd_overflows :: v tp -> v tp -> v Bool
  least_byte :: v tp -> v Word8

  -- | bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsf :: v tp -> v tp
  -- | bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  bsr :: v tp -> v tp

class ( IsAssignable m Bool
      , IsAssignable m Word8
      , IsAssignable m tp
      , IsBV (Value m) tp
      , Num (Value m tp)
      , Bits (Value m tp)
      )
   => IsAssignableBV m tp where

