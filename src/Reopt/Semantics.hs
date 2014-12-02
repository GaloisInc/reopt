{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reopt.Semantics
  ( exec_adc
  , exec_add
  , exec_and
  , exec_bsf
  , exec_bsr
  ) where

import Reopt.Semantics.Monad

uadd4_overflows :: ( IsLeq 4 n
                   , IsValue v
                   )
                => v (BV n) -> v (BV n) -> v Bool
uadd4_overflows x y = uadd_overflows (least_nibble x) (least_nibble y)

uadc4_overflows :: ( IsLeq 4 n
                   , IsValue v
                   )
                => v (BV n) -> v (BV n) -> v Bool -> v Bool
uadc4_overflows x y c = uadc_overflows (least_nibble x) (least_nibble y) c

-- | Update flags with given result value.
set_result_flags :: IsAssignableBV m n => Value m (BV n) -> m ()
set_result_flags res = do
  sf_flag .= msb res
  zf_flag .= is_zero res
  pf_flag .= even_parity (least_byte res)

-- | Assign value to location and update corresponding flags.
set_result_value :: IsAssignableBV m n => Assignable m (BV n) -> Value m (BV n) -> m ()
set_result_value dst res = do
  set_result_flags res
  dst .= res

-- | Set bitwise flags.
set_bitwise_flags :: IsAssignableBV m n => Value m (BV n) -> m ()
set_bitwise_flags res = do
  of_flag .= false
  cf_flag .= false
  set_undefined af_flag
  set_result_flags res

exec_adc :: IsAssignableBV m nn
         => Assignable m (BV n)
         -> Value m (BV n)
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
  set_result_value dst (dst_val + y)

exec_add :: IsAssignableBV m n
         => Assignable m (BV n)
         -> Value m (BV n)
         -> m ()
exec_add dst y = do
  -- Get current value stored in destination.
  dst_val <- get dst
  -- Set overflow and arithmetic flags
  of_flag .= sadd_overflows  dst_val y
  af_flag .= uadd4_overflows dst_val y
  cf_flag .= uadd_overflows  dst_val y
  -- Set result value.
  set_result_value dst (dst_val + y)

exec_and :: IsAssignableBV m n => Assignable m (BV n) -> Value m (BV n) -> m ()
exec_and r y = do
  x <- get r
  let z = x .&. y
  set_bitwise_flags z
  r .= z

exec_bsf :: IsAssignableBV m n => Assignable m (BV n) -> Value m (BV n) -> m ()
exec_bsf r y = do
  zf_flag .= is_zero y
  set_undefined cf_flag
  set_undefined of_flag
  set_undefined sf_flag
  set_undefined af_flag
  set_undefined pf_flag
  r .= bsf y

exec_bsr :: IsAssignableBV m n => Assignable m (BV n) -> Value m (BV n) -> m ()
exec_bsr r y = do
  zf_flag .= is_zero y
  set_undefined cf_flag
  set_undefined of_flag
  set_undefined sf_flag
  set_undefined af_flag
  set_undefined pf_flag
  r .= bsr y