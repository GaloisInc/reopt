module Reopt.Semantics where 
import Reopt.Semantics.Monad

-- | Update flags with given result value.
set_result_flags :: IsAssignableBV m tp => Value m tp -> m ()
set_result_flags r = do
  sf_flag .= msb r
  zf_flag .= is_zero r
  pf_flag .= even_parity (least_byte r)

-- | Assign value to location and update corresponding flags.
set_result_value :: IsAssignableBV m tp => Assignable m tp -> Value m tp -> m ()
set_result_value lhs val = do
  set_result_flags val
  lhs .= val

-- | Set bitwise flags.
set_bitwise_flags :: IsAssignableBV m tp => Value m tp -> m ()
set_bitwise_flags r = do
  of_flag .= false
  cf_flag .= false
  af_flag .= undef
  set_result_flags r

exec_add :: IsAssignableBV m tp
         => Assignable m tp -> Value m tp -> m ()
exec_add dst y = do
  dst_val <- get dst
  of_flag .= sadd_overflows dst_val y
  af_flag .= uadd4_overflows (least_byte dst_val) (least_byte y)
  cf_flag .= uadd_overflows dst_val y
  set_result_value dst (dst_val + y)

exec_and :: IsAssignableBV m tp => Assignable m tp -> Value m tp -> m ()
exec_and r y = do
  x <- get r
  let z = x .&. y
  set_bitwise_flags z
  r .= z

exec_bsf :: IsAssignableBV m tp => Assignable m tp -> Value m tp -> m ()
exec_bsf r y = do
  zf_flag .= is_zero y
  cf_flag .= undef
  of_flag .= undef
  sf_flag .= undef
  af_flag .= undef
  pf_flag .= undef
  r .= bsf y

exec_bsr :: IsAssignableBV m tp => Assignable m tp -> Value m tp -> m ()
exec_bsr r y = do
  zf_flag .= is_zero y
  cf_flag .= undef
  of_flag .= undef
  sf_flag .= undef
  af_flag .= undef
  pf_flag .= undef
  r .= bsr y