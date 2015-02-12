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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-} -- for Binop/Unop type synonyms

module Reopt.Semantics where

import Control.Applicative ( (<$>), (<*>) )
import Data.Type.Equality
import Data.Word
import GHC.TypeLits

import Data.Parameterized.NatRepr (widthVal, NatRepr, addNat, addIsLeq, withAddLeq, testLeq, LeqProof(..))
import Reopt.Semantics.Monad

type Binop = IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
type Unop  = IsLocationBV m n => MLocation m (BVType n) -> m ()
type UnopV = Semantics m => Value m (BVType n) -> m ()

uadd4_overflows :: ( 4 <= n, IsValue v)
                => v (BVType n) -> v (BVType n) -> v BoolType
uadd4_overflows x y = uadd_overflows (least_nibble x) (least_nibble y)

usub4_overflows :: (4 <= n, IsValue v)
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

push :: Semantics m => Value m (BVType n) -> m ()
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
  set_result_value dst (dst_val `bvAdd` y `bvAdd` uext (loc_width dst) c)

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
exec_addsd r y = modify (`doubleAdd` y) (xmm_low64 r)

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
exec_bswap = modify reverse_bytes

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
exec_cbw = do v <- get (reg_low8 r_rax)
              reg_low16 r_rax .= sext n16 v

exec_cwde = do v <- get (reg_low16 r_rax)
               reg_low32 r_rax .= sext n32 v

exec_cdqe = do v <- get (reg_low32 r_rax)
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
                  let v1 = bvLit (bv_width dst_val) (1 :: Int)
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
  let y  = bvLit (bv_width dst_val) (1 :: Int)
  -- Set overflow and arithmetic flags
  of_flag .= sadd_overflows  dst_val y
  af_flag .= uadd4_overflows dst_val y
  -- no cf_flag
  -- Set result value.
  set_result_value dst (dst_val `bvAdd` y)

exec_jcc :: Semantics m => m (Value m BoolType) -> Value m (BVType 64) -> m ()
exec_jcc cc off = do
  a <- cc
  when_ a $ jump_off off

jump_off :: Semantics m => Value m (BVType 64) -> m ()
jump_off off = do
  old_pc <- get IPReg
  let next_pc = old_pc `bvAdd` off
  IPReg .= next_pc

exec_jmp_absolute :: Semantics m => Value m (BVType 64) -> m ()
exec_jmp_absolute v = IPReg .= v

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

exec_movsd :: Semantics m =>  MLocation m (BVType 64) -> Value m DoubleType -> m ()
exec_movsd l v = l .= v

exec_movss :: Semantics m =>  MLocation m (BVType 32) -> Value m FloatType -> m ()
exec_movss l v = l .= v

-- And exec_movsxd
exec_movsx_d :: (Semantics m, 1 <= n', n' <= n)
             =>  MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_movsx_d l v = l .= sext (loc_width l) v

exec_movzx :: (Semantics m, IsLeq n' n) =>  MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_movzx l v = l .= uext (loc_width l) v

set_reg_pair :: Semantics m => MLocation m (BVType n) -> MLocation m (BVType n) -> Value m (BVType (n + n)) -> m ()
set_reg_pair upperL lowerL v = do lowerL .= lower
                                  upperL .= upper
  where
    (upper, lower) = bvSplit v
    
-- FIXME: is this the right way around?
exec_mul :: forall m n. IsLocationBV m n => Value m (BVType n) -> m ()
exec_mul v
  | Just Refl <- testEquality (bv_width v) n8  = go (\v' -> reg_low16 r_rax .= v') (reg_low8 r_rax)                                                 
  | Just Refl <- testEquality (bv_width v) n16 = go (set_reg_pair (reg_low16 r_rdx) (reg_low16 r_rax)) (reg_low16 r_rax)
  | Just Refl <- testEquality (bv_width v) n32 = go (set_reg_pair (reg_low32 r_rdx) (reg_low32 r_rax)) (reg_low32 r_rax)
  | Just Refl <- testEquality (bv_width v) n64 = go (set_reg_pair rdx rax) rax
  | otherwise                                  = fail "mul: Unknown bit width"
  where
    _ = addIsLeq (bv_width v) (bv_width v) -- hack to get n <= n + n
    go :: (n <= n + n) => (Value m (BVType (n + n)) -> m ()) -> MLocation m (BVType n) -> m ()
    go f l = do v' <- get l 
                let sz = addNat (bv_width v) (bv_width v)                    
                    r  = uext sz v' `bvMul` uext sz v -- FIXME: uext here is OK?
                    upper_r = fst (bvSplit r) :: Value m (BVType n)
                set_undefined sf_flag
                set_undefined af_flag
                set_undefined pf_flag
                set_undefined zf_flag
                ifte_ (is_zero upper_r)
                  (do of_flag .= false
                      cf_flag .= false)                  
                  (do of_flag .= true
                      cf_flag .= true)
                f r

really_exec_imul :: forall m n. IsLocationBV m n => Value m (BVType n) -> Value m (BVType n) -> (Value m (BVType (n + n)) -> m ()) -> m ()
really_exec_imul v v' f = withAddLeq (bv_width v) (bv_width v') $ \sz -> do
   let r  = sext sz v' `bvMul` sext sz v
       (_, lower_r :: Value m (BVType n)) = bvSplit r
   set_undefined af_flag
   set_undefined pf_flag
   set_undefined zf_flag                    
   sf_flag .= msb lower_r
   ifte_ (r .=. sext sz lower_r)
     (do of_flag .= false
         cf_flag .= false)                  
     (do of_flag .= true
         cf_flag .= true)
   f r

exec_imul1 :: forall m n. IsLocationBV m n => Value m (BVType n) -> m ()
exec_imul1 v
  | Just Refl <- testEquality (bv_width v) n8  = go (\v' -> reg_low16 r_rax .= v') (reg_low8 r_rax)                                                 
  | Just Refl <- testEquality (bv_width v) n16 = go (set_reg_pair (reg_low16 r_rdx) (reg_low16 r_rax)) (reg_low16 r_rax)
  | Just Refl <- testEquality (bv_width v) n32 = go (set_reg_pair (reg_low32 r_rdx) (reg_low32 r_rax)) (reg_low32 r_rax)
  | Just Refl <- testEquality (bv_width v) n64 = go (set_reg_pair rdx rax) rax
  | otherwise                                  = fail "imul: Unknown bit width"
  where
    _ = addIsLeq (bv_width v) (bv_width v) -- hack to get n <= n + n
    go :: (n <= n + n) => (Value m (BVType (n + n)) -> m ()) -> MLocation m (BVType n) -> m ()
    go f l = do v' <- get l 
                really_exec_imul v v' f

-- FIXME: clag from exec_mul, exec_imul
exec_imul2_3 :: forall m n. IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> Value m (BVType n) -> m ()
exec_imul2_3 l v v' = really_exec_imul v v' $ \r -> l .= snd (bvSplit r)
 
-- | Should be equiv to 0 - *l
exec_neg :: (IsLocationBV m n) =>  MLocation m (BVType n) -> m ()
exec_neg l = do
  v <- get l
  ifte_ (is_zero v) (cf_flag .= false) (cf_flag .= true)
  let r = bvNeg v
      zero = bvLit (bv_width v) (0 :: Int)
  of_flag .= ssub_overflows  zero v
  af_flag .= usub4_overflows zero v
  set_result_value l r

exec_not :: (Semantics m) => MLocation m (BVType n) -> m ()
exec_not = modify complement

exec_or :: Binop
exec_or l v = do
  v' <- get l
  set_undefined af_flag
  of_flag .= false
  cf_flag .= false
  set_result_value l (v' .|. v)

exec_pop :: Unop
exec_pop l = do v <- pop (loc_width l)
                l .= v

exec_push :: UnopV
exec_push v = push v

exec_ret :: Semantics m => Maybe Word16 -> m ()
exec_ret m_off = do
  next_ip <- pop n64
  case m_off of
    Nothing  -> return ()
    Just off -> modify (bvAdd (bvLit n64 off)) rsp
  IPReg .= next_ip

-- exec_sar ::

really_exec_shift :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n')
                     -> (Value m (BVType n) -> Value m (BVType n') -> Value m (BVType n))
                     -> (Value m (BVType n) -> Value m (BVType n') -> Value m (BVType n') -> Value m BoolType)
                     -> (Value m (BVType n) -> Value m (BVType n)  -> Value m BoolType    -> Value m BoolType)                     
                     -> m ()
really_exec_shift l count do_shift mk_cf mk_of = do
  v    <- get l
  -- The intel manual says that the count is masked to give an upper
  -- bound on the time the shift takes, with a mask of 63 in the case
  -- of a 64 bit operand, and 31 in the other cases.
  let nbits :: Int = case testLeq (bv_width v) n32 of
                       Just LeqProof -> 32
                       _             -> 64             
      countMASK = bvLit (bv_width count) (nbits - 1)
      tempCOUNT = count .&. countMASK  -- FIXME: prefer mod?
      r = do_shift v tempCOUNT
          
  -- When the count is zero, nothing happens, in particular, no flags change
  when_ (complement $ is_zero tempCOUNT) $ do
    let dest_width = bvLit (bv_width tempCOUNT) (widthVal (bv_width v))
    let new_cf = mk_cf v dest_width tempCOUNT
        
    ifte_ (tempCOUNT `bvLt` dest_width)
      (cf_flag .= new_cf)
      (set_undefined cf_flag)
    
    ifte_ (tempCOUNT .=. bvLit (bv_width tempCOUNT) (1 :: Int))
      (of_flag .= mk_of v r new_cf)
      (set_undefined of_flag)

    set_undefined af_flag
    set_result_value l r

-- FIXME: could be 8 instead of n' here ...
exec_shl :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_shl l count = really_exec_shift l count bvShl
                   (\v dest_width tempCOUNT -> bvBit v (dest_width `bvSub` tempCOUNT))
                   (\_ r new_cf -> msb r `bvXor` new_cf)

exec_shr :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_shr l count = really_exec_shift l count bvShl
                   (\v _ tempCOUNT -> bvBit v (tempCOUNT `bvSub` bvLit (bv_width tempCOUNT) (1 :: Int)))
                   (\v _ _         -> msb v)

-- FIXME: we can factor this out as above, but we need to check the CF
-- for SAR (intel manual says it is only undefined for shl/shr when
-- the shift is >= the bit width.
exec_sar :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_sar l count = do
  v    <- get l
  -- The intel manual says that the count is masked to give an upper
  -- bound on the time the shift takes, with a mask of 63 in the case
  -- of a 64 bit operand, and 31 in the other cases.
  let nbits :: Int = case testLeq (bv_width v) n32 of
                       Just LeqProof -> 32
                       _             -> 64                     
      countMASK = bvLit (bv_width count) (nbits - 1)
      tempCOUNT = count .&. countMASK  -- FIXME: prefer mod?
      r = bvSar v tempCOUNT
          
  -- When the count is zero, nothing happens, in particular, no flags change
  when_ (complement $ is_zero tempCOUNT) $ do
    let dest_width = bvLit (bv_width tempCOUNT) (widthVal (bv_width v))
    let new_cf = bvBit v (tempCOUNT `bvSub` bvLit (bv_width tempCOUNT) (1 :: Int))
    
    ifte_ (tempCOUNT `bvLt` dest_width)
      (cf_flag .= new_cf)
      (cf_flag .= msb v) -- FIXME: correct?  we assume here that we will get the sign bit ...
    
    ifte_ (tempCOUNT .=. bvLit (bv_width tempCOUNT) (1 :: Int))
      (of_flag .= false)
      (set_undefined of_flag)

    set_undefined af_flag
    set_result_value l r

-- FIXME: use really_exec_shift above?
exec_rol :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_rol l count = do
  v    <- get l
  -- The intel manual says that the count is masked to give an upper
  -- bound on the time the shift takes, with a mask of 63 in the case
  -- of a 64 bit operand, and 31 in the other cases.
  let nbits :: Int = case testLeq (bv_width v) n32 of
                       Just LeqProof -> 32
                       _             -> 64             
      countMASK = bvLit (bv_width count) (nbits - 1)
      -- FIXME: this is from the manual, but I think the masking is overridden by the mod?
      tempCOUNT = (count .&. countMASK) `bvMod` (bvLit (bv_width count) (widthVal (bv_width v)))
      r = bvRol v tempCOUNT
          
  -- When the count is zero, nothing happens, in particular, no flags change
  when_ (complement $ is_zero tempCOUNT) $ do
    let new_cf = bvBit r (bvLit (bv_width r) (0 :: Int))
        
    cf_flag .= new_cf
    
    ifte_ (tempCOUNT .=. bvLit (bv_width tempCOUNT) (1 :: Int))
      (of_flag .= (msb r `bvXor` new_cf))
      (set_undefined of_flag)

    l .= r
  

exec_setcc :: Semantics m => m (Value m BoolType) -> MLocation m (BVType 8) -> m ()
exec_setcc cc l = do
  a <- cc
  ifte_ a (l .= bvLit n8 (1 :: Int)) (l .= bvLit n8 (0 :: Int))

exec_sbb :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_sbb l v = do cf <- get cf_flag
                  exec_sub l (v `bvAdd` uext (bv_width v) cf)

-- FIXME: duplicates subtraction term by calling exec_cmp
exec_sub :: Binop
exec_sub l v = do v0 <- get l
                  l .= (v0 `bvSub` v)
                  exec_cmp l v -- set flags

exec_test :: Binop
exec_test l v = do
  v' <- get l
  let r = v' .&. v
  set_bitwise_flags r

exec_xchg :: IsLocationBV m n => MLocation m (BVType n) -> MLocation m (BVType n) -> m ()
exec_xchg l l' = do v  <- get l
                    v' <- get l'
                    l  .= v'
                    l' .= v

exec_xor :: Binop
exec_xor l v = do
  v0 <- get l
  let r = v0 `bvXor` v
  set_bitwise_flags r
  l .= r

-- exec_cmova_ia64_32 :: (Semantics m, Num  (Value m (BVType 32)))
--                    => Reg64 -> Value m (BVType 32) -> m ()
-- exec_cmova_ia64_32 r y = do
--   a <- is_above
--   ifte_ a
--         (low_dword  r .= y)
--         (high_dword r .= 0)

-- TODO: Get list of complete number of instructions.
