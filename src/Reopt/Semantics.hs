------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics
-- Description      : Defines the semantics of x86 instructions.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This is the top-level module containing the definitions for x86
-- instructions.
------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reopt.Semantics
  ( exec_adc
  , exec_add
  , exec_and
  , exec_bsf
  , exec_bsr
  , exec_bswap
  ) where

import Reopt.Semantics.Monad

uadd4_overflows :: ( IsLeq 4 n
                   , IsValue v
                   )
                => v (BVType n) -> v (BVType n) -> v BoolType
uadd4_overflows x y = uadd_overflows (least_nibble x) (least_nibble y)

uadc4_overflows :: ( IsLeq 4 n
                   , IsValue v
                   )
                => v (BVType n) -> v (BVType n) -> v BoolType -> v BoolType
uadc4_overflows x y c = uadc_overflows (least_nibble x) (least_nibble y) c

-- | Update flags with given result value.
set_result_flags :: IsLocationBV m n => Value m (BVType n) -> m ()
set_result_flags res = do
  sf_flag .= msb res
  zf_flag .= is_zero res
  pf_flag .= even_parity (least_byte res)

-- | Assign value to location and update corresponding flags.
set_result_value :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
set_result_value dst res = do
  set_result_flags res
  dst .= res

-- | Set bitwise flags.
set_bitwise_flags :: IsLocationBV m n => Value m (BVType n) -> m ()
set_bitwise_flags res = do
  of_flag .= false
  cf_flag .= false
  set_undefined af_flag
  set_result_flags res

exec_adc :: IsLocationBV m n
         => MLocation m (BVType n)
         -> Value m (BVType n)
         -> m ()
exec_adc dst y = do
  -- Get current value stored in destination.
  dst_val <- get dst
  -- Get current value of carry bit
  c <- get cf_flag
  -- Set overflow and arithmetic flags
  of_flag .= sadc_overflows  dst_val y c
  af_flag .= uadc4_overflows dst_val y c
  cf_flag .= uadc_overflows  dst_val y c
  -- Set result value.
  set_result_value dst (dst_val .+ y)

-- | @add@
exec_add :: IsLocationBV m n
         => MLocation m (BVType n)
         -> Value m (BVType n)
         -> m ()
exec_add dst y = do
  -- Get current value stored in destination.
  dst_val <- get dst
  -- Set overflow and arithmetic flags
  of_flag .= sadd_overflows  dst_val y
  af_flag .= uadd4_overflows dst_val y
  cf_flag .= uadd_overflows  dst_val y
  -- Set result value.
  set_result_value dst (dst_val .+ y)

-- | Add sign double
exec_addsd :: Semantics m => MLocation m DoubleType -> Value m DoubleType -> m ()
exec_addsd r y = modify r (+y)

-- | And two values together.
exec_and :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_and r y = do
  x <- get r
  let z = x .&. y
  set_bitwise_flags z
  r .= z

exec_bsf :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_bsf r y = do
  zf_flag .= is_zero y
  set_undefined cf_flag
  set_undefined of_flag
  set_undefined sf_flag
  set_undefined af_flag
  set_undefined pf_flag
  r .= bsf y

exec_bsr :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_bsr r y = do
  zf_flag .= is_zero y
  set_undefined cf_flag
  set_undefined of_flag
  set_undefined sf_flag
  set_undefined af_flag
  set_undefined pf_flag
  r .= bsr y

-- | Run bswap instruction.
exec_bswap :: IsLocationBV m n => MLocation m (BVType n) -> m ()
exec_bswap r = modify r reverse_bytes

-- | Run clc instruction.
exec_clc :: Semantics m => m ()
exec_clc = cf_flag .= false

-- | Run cld instruction.
exec_cld :: Semantics m => m ()
exec_cld = df_flag .= false

is_above :: Semantics m => m (Value m BoolType)
is_above = do
  cf <- get cf_flag
  zf <- get zf_flag
  return $ complement cf .&. complement zf

exec_cmova :: Semantics m => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_cmova r y = do
  a <- is_above
  when_ a (r .= y)

exec_cmova_ia64_32 :: (Semantics m, Num  (Value m (BVType 32)))
                   => Reg64 -> Value m (BVType 32) -> m ()
exec_cmova_ia64_32 r y = do
  a <- is_above
  ifte_ a
        (low_dword  r .= y)
        (high_dword r .= 0)

-- TODO: Get list of complete number of instructions.