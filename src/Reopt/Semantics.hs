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
module Reopt.Semantics where


import Control.Applicative ( (<$>), (<*>) )

import Data.Parameterized.NatRepr (widthVal, NatRepr)
import Reopt.Semantics.Monad

uadd4_overflows :: ( IsLeq 4 n
                   , IsValue v
                   )
                => v (BVType n) -> v (BVType n) -> v BoolType
uadd4_overflows x y = uadd_overflows (least_nibble x) (least_nibble y)

usub4_overflows :: ( IsLeq 4 n
                       , IsValue v
                       )
                    => v (BVType n) -> v (BVType n) -> v BoolType
usub4_overflows x y = usub_overflows (least_nibble x) (least_nibble y)

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

push :: IsLocationBV m n => Value m (BVType n) -> m ()
push v = do old_sp <- get rsp
            let delta   = bvLit n64 $ widthVal sz `div` 8 -- delta in bytes
                new_sp  = old_sp `bvSub` delta
                sp_addr = mkBVAddr sz new_sp
            sp_addr .= v
            rsp     .= new_sp
     where
       sz = bv_width v

pop :: IsLocationBV m n => NatRepr n -> m (Value m (BVType n))
pop sz = do old_sp <- get rsp
            let delta   = bvLit n64 $ widthVal sz `div` 8 -- delta in bytes
                new_sp  = old_sp `bvAdd` delta
                sp_addr = mkBVAddr sz old_sp
            v   <- get sp_addr
            rsp .= new_sp
            return v
            
-- * Implementation of the semantics

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
  set_result_value dst (dst_val `bvAdd` y `bvAdd` uext c)

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
  set_result_value dst (dst_val `bvAdd` y)

-- | Add sign double
exec_addsd :: Semantics m => MLocation m XMMType -> Value m DoubleType -> m ()
exec_addsd r y = modify (xmm_low64 r) (`doubleAdd` y)

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

really_exec_call :: IsLocationBV m 64 => Value m (BVType 64) -> m ()
really_exec_call next_pc = do
  old_pc <- get IPReg
  push old_pc -- push value of next instruction
  IPReg .= next_pc
  
exec_call_relative :: IsLocationBV m 64 => Value m (BVType 64) -> m ()
exec_call_relative off = do
  old_pc <- get IPReg
  let next_pc = old_pc `bvAdd` off
  really_exec_call next_pc

exec_call_absolute :: IsLocationBV m 64 => Value m (BVType 64) -> m ()
exec_call_absolute = really_exec_call

-- | Sign extend al -> ax, ax -> eax, eax -> rax, resp.
exec_cbw, exec_cwde, exec_cdqe :: Semantics m => m ()
exec_cbw = do v <- get (reg_low n8 r_rax)
              reg_low n16 r_rax .= sext n16 v

exec_cwde = do v <- get (reg_low n16 r_rax)
               reg_low n32 r_rax .= sext n32 v

exec_cdqe = do v <- get (reg_low n32 r_rax)
               rax .= sext n64 v

-- | Run clc instruction.
exec_clc :: Semantics m => m ()
exec_clc = cf_flag .= false

-- | Run cld instruction.
exec_cld :: Semantics m => m ()
exec_cld = df_flag .= false

-- * Conditions

cond_a, cond_ae, cond_b, cond_be, cond_g, cond_ge, cond_l, cond_le, cond_o, cond_p, cond_s, cond_z,
  cond_no, cond_np, cond_ns, cond_nz :: Semantics m => m (Value m BoolType)
cond_a = (\c z -> complement c .&. complement z) <$> get cf_flag <*> get zf_flag
cond_ae  = complement <$> get cf_flag
cond_b   = get cf_flag
cond_be  = (.|.) <$> get cf_flag <*> get zf_flag
cond_g   = (\z s o -> complement z .&. (s `bvXor` o)) <$> get zf_flag <*> get sf_flag <*> get of_flag
cond_ge  = (\s o   -> s `bvXor` o) <$> get sf_flag <*> get of_flag
cond_l   = complement <$> cond_ge
cond_le  = complement <$> cond_g
cond_o   = get of_flag
cond_p   = get pf_flag
cond_s   = get sf_flag
cond_z   = get zf_flag
cond_no  = complement <$> cond_o
cond_np  = complement <$> cond_p
cond_ns  = complement <$> cond_s
cond_nz  = complement <$> cond_z

exec_cmovcc :: Semantics m => m (Value m BoolType) -> MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_cmovcc cc r y = do
  a <- cc
  when_ a (r .= y)

-- FIXME: we don't need a location, just a value.
exec_cmp :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_cmp dst y = do dst_val <- get dst
                    -- Set overflow and arithmetic flags
                    of_flag .= ssub_overflows  dst_val y
                    af_flag .= usub4_overflows dst_val y
                    cf_flag .= usub_overflows  dst_val y
                    -- Set result value.
                    set_result_flags (dst_val `bvSub` y)

exec_dec :: IsLocationBV m n => MLocation m (BVType n) -> m ()
exec_dec dst = do dst_val <- get dst
                  let v1 = bvLit (bv_width dst_val) 1
                  -- Set overflow and arithmetic flags
                  of_flag .= ssub_overflows  dst_val v1
                  af_flag .= usub4_overflows dst_val v1
                  -- no carry flag
                  -- Set result value.
                  set_result_value dst (dst_val `bvSub` v1)

-- exec_div :: Int -> Value m (BVType 8) -> m ()
-- exec_div os v = do 

exec_inc :: IsLocationBV m n => MLocation m (BVType n) -> m ()
exec_inc dst = do
  -- Get current value stored in destination.
  dst_val <- get dst
  let y  = bvLit (bv_width dst_val) 1
  -- Set overflow and arithmetic flags
  of_flag .= sadd_overflows  dst_val y
  af_flag .= uadd4_overflows dst_val y
  -- no cf_flag
  -- Set result value.
  set_result_value dst (dst_val `bvAdd` y)

exec_jcc :: Semantics m => m (Value m BoolType) -> Value m (BVType 64) -> m ()
exec_jcc cc off = do
  a <- cc
  when_ a $ do old_pc <- get IPReg
               let next_pc = old_pc `bvAdd` off
               IPReg .= next_pc

exec_jmp_absolute :: Semantics m => Value m (BVType 64) -> m ()
exec_jmp_absolute v = IPReg .= v

exec_jmp_relative :: Semantics m => Value m (BVType 64) -> m ()
exec_jmp_relative off = do old_pc <- get IPReg
                           IPReg .= (old_pc `bvAdd` off)

-- FIXME: ensure that v is the address, not the contents
exec_lea :: Semantics m =>  MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_lea l v = l .= v

exec_leave :: Semantics m => m ()
exec_leave = do bp_v <- get rbp
                rsp .= bp_v
                bp_v' <- pop n64
                rbp .= bp_v'

-- FIXME: special segment stuff?
-- FIXME: CR and debug regs?
exec_mov :: Semantics m =>  MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_mov l v = l .= v

exec_movapd :: Semantics m =>  MLocation m (BVType 128) -> Value m (BVType 128) -> m ()
exec_movapd l v = l .= v

exec_movaps :: Semantics m =>  MLocation m (BVType 128) -> Value m (BVType 128) -> m ()
exec_movaps l v = l .= v

exec_movasd :: Semantics m =>  MLocation m (BVType 64) -> Value m DoubleType -> m ()
exec_movasd l v = l .= v

exec_movass :: Semantics m =>  MLocation m (BVType 32) -> Value m FloatType -> m ()
exec_movass l v = l .= v

-- And exec_movsxd
exec_movsx_d :: (Semantics m, IsLeq n' n) =>  MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_movsx_d l v = l .= sext (loc_width l) v



-- FIXME: duplicates subtraction term by calling exec_cmp
exec_sub :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_sub l v = do v0 <- get l
                  l .= (v0 `bvSub` v)
                  exec_cmp l v -- set flags 


-- exec_cmova_ia64_32 :: (Semantics m, Num  (Value m (BVType 32)))
--                    => Reg64 -> Value m (BVType 32) -> m ()
-- exec_cmova_ia64_32 r y = do
--   a <- is_above
--   ifte_ a
--         (low_dword  r .= y)
--         (high_dword r .= 0)

-- TODO: Get list of complete number of instructions.
