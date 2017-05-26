{-
Copyright        : (c) Galois, Inc 2015-2017
Maintainer       : Joe Hendrix <jhendrix@galois.com>

This module provides definitions for x86 instructions.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Semantics.Semantics
  ( all_instructions
  ) where

import           Prelude hiding (isNaN)
import           Data.Bits (shiftL)
import           Data.Int
import           Data.Macaw.CFG (MemRepr(..), memReprBytes)
import           Data.Macaw.Memory (Endianness (LittleEndian))
import           Data.Macaw.Types
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Proxy
import qualified Flexdis86 as F

import qualified Reopt.Machine.StateNames as N
import           Reopt.Semantics.Getters
import           Reopt.Semantics.InstructionDef
import           Reopt.Semantics.Monad

-- * Preliminaries

-- The representation for a address
addrRepr :: MemRepr (BVType 64)
addrRepr = BVMemRepr n8 LittleEndian

type Binop = forall m n.
  IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()

uadd4_overflows :: ( 4 <= n, IsValue v)
                => v (BVType n) -> v (BVType n) -> v BoolType
uadd4_overflows x y = uadd_overflows (least_nibble x) (least_nibble y)

usub4_overflows :: (4 <= n, IsValue v)
                => v (BVType n) -> v (BVType n) -> v BoolType
usub4_overflows x y = usub_overflows (least_nibble x) (least_nibble y)

uadc4_overflows :: ( 4 <= n
                   , IsValue v
                   )
                => v (BVType n) -> v (BVType n) -> v BoolType -> v BoolType
uadc4_overflows x y c = uadc_overflows (least_nibble x) (least_nibble y) c

fmap_loc :: Semantics m => MLocation m (BVType n) -> (Value m (BVType n) -> Value m (BVType n)) -> m ()
fmap_loc l f = do
  lv <- get l
  l .= f lv

-- | Update flags with given result value.
set_result_flags :: IsLocationBV m n => Value m (BVType n) -> m ()
set_result_flags res = do
  sf_loc .= msb res
  zf_loc .= is_zero res
  pf_loc .= even_parity (least_byte res)

-- | Assign value to location and update corresponding flags.
set_result_value :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
set_result_value dst res = do
  set_result_flags res
  dst .= res

-- | Set bitwise flags.
set_bitwise_flags :: IsLocationBV m n => Value m (BVType n) -> m ()
set_bitwise_flags res = do
  of_loc .= false
  cf_loc .= false
  set_undefined af_loc
  set_result_flags res

push :: Semantics m => MemRepr tp -> Value m tp -> m ()
push repr v = do
  old_sp <- get rsp
  let delta   = bvLit n64 $ memReprBytes repr -- delta in bytes
      new_sp  = old_sp `bvSub` delta
  MemoryAddr new_sp repr .= v
  rsp     .= new_sp

pop :: Semantics m => MemRepr tp -> m (Value m tp)
pop repr = do
  -- Get current stack pointer value.
  old_sp <- get rsp
  -- Get value at stack pointer.
  v   <- get (MemoryAddr old_sp repr)
  -- Increment stack pointer
  rsp .= bvAdd old_sp (bvLit n64 (memReprBytes repr))
  -- Return value
  return v

modify_low ::
  ( Semantics m
  , 1 <= n1
  , 1 <= n2
  , n1 <= n2 -- The only constraint that matters mathematically.
  , 1 <= n2 - n1
  , n2 - n1 <= n2
  , n2 ~ ((n2 - n1) + n1)
  )
  => NatRepr n1
  -> MLocation m (BVType n2)
  -> (Value m (BVType n1) -> Value m (BVType n1))
  -> m ()
modify_low w l f = do
  v0 <- get l
  let n = loc_width l
  let low_mask  = bvLit n (maxUnsigned w)
  let high_mask = complement low_mask
  let v0high = v0 .&. high_mask
  let v1Low = uext n (f (bvTrunc w v0))
  l .= v0high .|. v1Low

set_low ::
  ( Semantics m
  , ((n2 - n1) + n1) ~ n2
  , 1 <= n1
  , 1 <= n2
  , n1 <= n2
  , 1 <= n2 - n1
  , n2 - n1 <= n2
  )
  => MLocation m (BVType n2)
  -> Value m (BVType n1)
  -> m ()
set_low r c = modify_low (bv_width c) r (\_ -> c)

set_reg_pair :: (Semantics m, 1 <= n)
             => (N.RegisterName cl -> MLocation m (BVType n))
             -> N.RegisterName cl
             -> N.RegisterName cl
             -> Value m (BVType (n + n))
             -> m ()
set_reg_pair f upperL lowerL v = do
  let (upper, lower) = bvSplit v
  f lowerL .= lower
  f upperL .= upper

-- ** Condition codes

-- * General Purpose Instructions
-- ** Data Transfer Instructions

-- FIXME: has the side effect of reading r, but this should be safe because r can only be a register.

def_cmov_list :: [InstructionDef]
def_cmov_list =
  defConditionals "cmov" $ \mnem cc ->
    defBinaryLV mnem $ \r y -> do
      c <- cc
      r_v <- get r
      r .= mux c y r_v

-- | Run bswap instruction.
exec_bswap :: IsLocationBV m n => MLocation m (BVType n) -> m ()
exec_bswap l = do
  v0 <- get l
  l .= (bvUnvectorize (loc_width l) $ reverse $ bvVectorize n8 v0)

def_xadd :: InstructionDef
def_xadd =
  defBinaryLL "xadd" $ \_ d s -> do
    d0 <- get d
    s0 <- get s
    s .= d0
    exec_add d s0 -- sets flags

-- | Sign extend al -> ax, ax -> eax, eax -> rax, resp.
exec_cbw, exec_cwde, exec_cdqe :: Semantics m => m ()
exec_cbw = do v <- get (reg_low8 N.rax)
              reg_low16 N.rax .= sext n16 v

exec_cwde = do v <- get (reg_low16 N.rax)
               reg_low32 N.rax .= sext n32 v

exec_cdqe = do v <- get (reg_low32 N.rax)
               rax .= sext n64 v

def_pop :: InstructionDef
def_pop =
  defUnary "pop" $ \_ fval -> do
    Some (HasRepSize rep l) <- getAddrRegOrSegment fval
    val <- pop (repValSizeMemRepr rep)
    l .= val

def_push :: InstructionDef
def_push =
  defUnary "push" $ \_ val -> do
    Some (HasRepSize rep v) <- getAddrRegSegmentOrImm val
    push (repValSizeMemRepr rep) v


-- | Sign extend ax -> dx:ax, eax -> edx:eax, rax -> rdx:rax, resp.
def_cwd :: InstructionDef
def_cwd = defNullary "cwd" $ do
  v <- get (reg_low16 N.rax)
  set_reg_pair reg_low16 N.rdx N.rax (sext knownNat v)

def_cdq :: InstructionDef
def_cdq = defNullary "cdq" $ do
  v <- get (reg_low32 N.rax)
  set_reg_pair reg_low32 N.rdx N.rax (sext knownNat v)

def_cqo :: InstructionDef
def_cqo = defNullary "cqo" $ do
  v <- get rax
  set_reg_pair fullRegister N.rdx N.rax (sext knownNat v)

-- FIXME: special segment stuff?
-- FIXME: CR and debug regs?
exec_mov :: Semantics m =>  MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_mov l v = l .= v

exec_cmpxchg :: forall m n
              . (IsLocationBV m n)
             => MLocation m (BVType n)
             -> Value m (BVType n)
             -> m ()
exec_cmpxchg dest src = go dest src $ regLocation (bv_width src) N.rax
  where
    go :: MLocation m (BVType n)
       -> Value m (BVType n)
       -> MLocation m (BVType n) -- AL/AX/EAX/RAX depending on operand size
       -> m ()
    go d s acc = do
      temp <- get d
      a  <- get acc
      exec_cmp acc temp -- set flags
      ifte_ (a .=. temp)
        (do zf_loc .= true
            d .= s
        )
        (do zf_loc .= false
            acc .= temp
            d   .= temp -- FIXME: this store is redundant, but it is in the ISA, so we do it.
        )

get_reg_pair :: (Semantics m, 1 <= n)
             => (N.RegisterName cl -> MLocation m (BVType n))
             -> N.RegisterName cl
             -> N.RegisterName cl
             -> m (Value m (BVType (n+n)))
get_reg_pair f upperL lowerL = bvCat <$> get (f upperL) <*> get (f lowerL)

exec_cmpxchg8b :: Semantics m => MLocation m (BVType 64) -> m ()
exec_cmpxchg8b loc = do
  temp64 <- get loc
  edx_eax <- get_reg_pair reg_low32 N.rdx N.rax
  ifte_ (edx_eax .=. temp64)
    (do zf_loc .= true
        ecx_ebx <- get_reg_pair reg_low32 N.rcx N.rbx
        loc .= ecx_ebx
    )
    (do zf_loc .= false
        set_reg_pair reg_low32 N.rdx N.rax temp64
        loc .= edx_eax -- FIXME: this store is redundant, but it is in the ISA, so we do it.
    )

def_movsx :: InstructionDef
def_movsx = defBinaryLVge "movsxd" $ \l v -> l .= sext (loc_width l) v

def_movsxd :: InstructionDef
def_movsxd = defBinaryLVge "movsxd" $ \l v -> l .= sext (loc_width l) v

def_movzx :: InstructionDef
def_movzx = defBinaryLVge "movzx" $ \l v -> do
  l .= uext (loc_width l) v

-- The xchng instruction
def_xchg :: InstructionDef
def_xchg = defBinary "xchg" $ \_ f_loc f_loc' -> do
  SomeBV l <- getSomeBVLocation f_loc
  l' <- getBVLocation f_loc' (loc_width l)
  v  <- get l
  v' <- get l'
  l  .= v'
  l' .= v

-- ** Binary Arithmetic Instructions

exec_adc :: IsLocationBV m n
         => MLocation m (BVType n)
         -> Value m (BVType n)
         -> m ()
exec_adc dst y = do
  -- Get current value stored in destination.
  dst_val <- get dst
  -- Get current value of carry bit
  c <- get cf_loc
  -- Set overflow and arithmetic flags
  of_loc .= sadc_overflows  dst_val y c
  af_loc .= uadc4_overflows dst_val y c
  cf_loc .= uadc_overflows  dst_val y c
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
  of_loc .= sadd_overflows  dst_val y
  af_loc .= uadd4_overflows dst_val y
  cf_loc .= uadd_overflows  dst_val y
  -- Set result value.
  set_result_value dst (dst_val `bvAdd` y)

-- FIXME: we don't need a location, just a value.
exec_cmp :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_cmp dst y = do
  dst_val <- get dst
  -- Set overflow and arithmetic flags
  of_loc .= ssub_overflows  dst_val y
  af_loc .= usub4_overflows dst_val y
  cf_loc .= usub_overflows  dst_val y
  -- Set result value.
  set_result_flags (dst_val `bvSub` y)

exec_dec :: IsLocationBV m n => MLocation m (BVType n) -> m ()
exec_dec dst = do
  dst_val <- get dst
  let v1 = bvLit (bv_width dst_val) 1
  -- Set overflow and arithmetic flags
  of_loc .= ssub_overflows  dst_val v1
  af_loc .= usub4_overflows dst_val v1
  -- no carry flag
  -- Set result value.
  set_result_value dst (dst_val `bvSub` v1)

  -- Steps:
  --
  -- - check that denom is non-zero and raise exception if it is.
  --   - read about x86 exceptions to learn right behavior of
  --     predicate and flags.
  --
  -- - extend denominator to (n + n) bits and do division and
  --   remainder.
  --
  -- - raise exception if quotient can't be truncated without
  -- information loss.
  --
  -- - truncate the quotient and the remainder (it's always safe
  -- to truncate the remainder, since the original fit in 'n'
  -- bits. (does this still make sense for *signed* division when
  -- the remainder is negative?)).

  -- Making these ops parameters leads to weird type checking
  -- problems: GHC 7.10.2 complains that it can't conclude that @t ~ t@
  -- for @t = (4 <=? (n + n))@. Type equality should be reflexive, yeah?

-- | Helper function for @div@ and @idiv@ instructions.
--
-- The difference between @div@ and @idiv@ is whether the primitive
-- operations are signed or not.
--
-- The x86 division instructions are peculiar. A @2n@-bit numerator is
-- read from fixed registers and an @n@-bit quotient and @n@-bit
-- remainder are written to those fixed registers. An exception is
-- raised if the denominator is zero or if the quotient does not fit
-- in @n@ bits.
--
-- Also, results should be rounded towards zero. These operations are
-- called @quot@ and @rem@ in Haskell, whereas @div@ and @mod@ in
-- Haskell round towards negative infinity.
--
-- Source: the x86 documentation for @idiv@, Intel x86 manual volume
-- 2A, page 3-393.
exec_div_helper :: forall m n
   . IsLocationBV m n
  => Bool -- ^ Signed ('True') or unsigned ('False').
  -> Value m (BVType n) -- ^ Denominator; numerator is read from regs.
  -> m ()
exec_div_helper signed denominator
  | Just Refl <- testEquality n n8  =
    go (reg_low8 N.rax) (reg_high8 N.rax)
  | Just Refl <- testEquality n n16 =
    go (reg_low16 N.rax) (reg_low16 N.rdx)
  | Just Refl <- testEquality n n32 =
    go (reg_low32 N.rax) (reg_low32 N.rdx)
  | Just Refl <- testEquality n n64 =
    go rax rdx
  | otherwise =
    fail "div: Unknown bit width"
  where
    n :: NatRepr n
    n = bv_width denominator

    go :: (1 <= n + n, n <= n + n)
       => MLocation m (BVType n) -- ^ Location of lower half of numerator.
       -> MLocation m (BVType n) -- ^ Location of upper half of numerator.
       -> m ()
    go ax dx = do
      exception false (is_zero denominator) DivideError

      let (quotOp, remOp, extOp) =
            if signed
            then (bvSignedQuot, bvSignedRem, sext)
            else (bvQuot, bvRem, uext)

      -- Numerator.
      axv <- get ax
      dxv <- get dx
      let numerator = bvCat dxv axv

      -- Quotient and remainder.
      --
      -- VERY SUBTLE BUG: for signed quotient, it's also an error if
      -- the @n+n@-bit quotient overflows -- below we check that the
      -- @n+n@-bit quotient fits in @n@ bits -- which happens when you
      -- divide the largest @n@-bit negative number (@-2^(n+n-1)@) by
      -- @-1@. Depending on how the 'quotOp' is defined, or check
      -- below may or may not catch this overflow:
      --
      -- - in LLVM the 'quotOp' (@sdiv@) is undefined in this case, so
      --   anything could happen.
      --
      -- - in an implementation that returns the @n+n@-bit 2's
      --   complement representation of @2^(n+n-1)@ we detect the
      --   error, since this number is equal to @-2^(n+n-1)@ in 2's
      --   complement, which does not fit in @n@ bits.
      let nn = addNat n n
      let denominator' = extOp nn denominator
      q' <- quotOp numerator denominator'
      r' <- remOp numerator denominator'
      let q = bvTrunc n q'
      let r = bvTrunc n r'

      -- Check that quotient fits in @n@ bits.
      let q'' = extOp nn q
      exception false (q' .=/=. q'') DivideError

      set_undefined cf_loc
      set_undefined of_loc
      set_undefined sf_loc
      set_undefined af_loc
      set_undefined pf_loc
      set_undefined zf_loc

      ax .= q
      dx .= r

-- | Unsigned (@div@ instruction) and signed (@idiv@ instruction) division.
def_div :: InstructionDef
def_div  = defUnaryV "div"  $ exec_div_helper False

def_idiv :: InstructionDef
def_idiv = defUnaryV "idiv" $ exec_div_helper True

--  | Execute the halt instruction
--
-- This code assumes that we are not running in kernel mode.
def_hlt :: InstructionDef
def_hlt = defNullary "hlt" $ exception false true (GeneralProtectionException 0)

exec_inc :: IsLocationBV m n => MLocation m (BVType n) -> m ()
exec_inc dst = do
  -- Get current value stored in destination.
  dst_val <- get dst
  let y  = bvLit (bv_width dst_val) 1
  -- Set overflow and arithmetic flags
  of_loc .= sadd_overflows  dst_val y
  af_loc .= uadd4_overflows dst_val y
  -- no cf_loc
  -- Set result value.
  set_result_value dst (dst_val `bvAdd` y)

-- FIXME: is this the right way around?
exec_mul :: forall m n
          . (IsLocationBV m n)
         => Value m (BVType n)
         -> m ()
exec_mul v
  | Just Refl <- testEquality (bv_width v) n8  =
    go (\v' -> reg_low16 N.rax .= v') (reg_low8 N.rax)
  | Just Refl <- testEquality (bv_width v) n16 =
    go (set_reg_pair reg_low16 N.rdx N.rax) (reg_low16 N.rax)
  | Just Refl <- testEquality (bv_width v) n32 =
    go (set_reg_pair reg_low32 N.rdx N.rax) (reg_low32 N.rax)
  | Just Refl <- testEquality (bv_width v) n64 =
    go (set_reg_pair fullRegister N.rdx N.rax) rax
  | otherwise =
    fail "mul: Unknown bit width"
  where
    go :: (1 <= n+n, n <= n+n)
       => (Value m (BVType (n + n)) -> m ()) -> MLocation m (BVType n) -> m ()
    go f l = do
      v' <- get l
      let sz = addNat (bv_width v) (bv_width v)
          r  = uext sz v' `bvMul` uext sz v -- FIXME: uext here is OK?
          upper_r = fst (bvSplit r) :: Value m (BVType n)
      set_undefined sf_loc
      set_undefined af_loc
      set_undefined pf_loc
      set_undefined zf_loc
      let does_overflow = complement (is_zero upper_r)
      of_loc .= does_overflow
      cf_loc .= does_overflow
      f r

really_exec_imul :: forall m n
                  . (IsLocationBV m n)
                 => Value m (BVType n)
                 -> Value m (BVType n)
                 -> m (Value m (BVType (n + n)))
really_exec_imul v v' = do
  let w = bv_width v
  let sz = addNat w w
  let w_is_pos :: LeqProof 1 n
      w_is_pos = LeqProof
  withLeqProof (leqAdd w_is_pos w) $ do
  withLeqProof (addIsLeq w w) $ do
  let r :: Value m (BVType (n + n))
      r  = sext sz v' .* sext sz v
      (_, lower_r :: Value m (BVType n)) = bvSplit r
  set_undefined af_loc
  set_undefined pf_loc
  set_undefined zf_loc
  sf_loc .= msb lower_r
  let does_overflow = (r .=/=. sext sz lower_r)
  of_loc .= does_overflow
  cf_loc .= does_overflow
  pure r

exec_imul1 :: forall m n. IsLocationBV m n => Value m (BVType n) -> m ()
exec_imul1 v
  | Just Refl <- testEquality (bv_width v) n8  = do
      v' <- get $ reg_low8 N.rax
      r <- really_exec_imul v v'
      reg_low16 N.rax .= r
  | Just Refl <- testEquality (bv_width v) n16 = do
      v' <- get $ reg_low16 N.rax
      r <- really_exec_imul v v'
      set_reg_pair reg_low16 N.rdx N.rax r
  | Just Refl <- testEquality (bv_width v) n32 = do
      v' <- get $ reg_low32 N.rax
      r <- really_exec_imul v v'
      set_reg_pair reg_low32 N.rdx N.rax r
  | Just Refl <- testEquality (bv_width v) n64 = do
      v' <- get rax
      r <- really_exec_imul v v'
      set_reg_pair fullRegister N.rdx N.rax r
  | otherwise =
      fail "imul: Unknown bit width"

-- FIXME: clag from exec_mul, exec_imul
exec_imul2_3 :: forall m n n'
              . (IsLocationBV m n, 1 <= n', n' <= n)
             => MLocation m (BVType n) -> Value m (BVType n) -> Value m (BVType n') -> m ()
exec_imul2_3 l v v' = do
  withLeqProof (dblPosIsPos (LeqProof :: LeqProof 1 n)) $ do
  r <- really_exec_imul v (sext (bv_width v) v')
  l .= snd (bvSplit r)

def_imul :: InstructionDef
def_imul = defVariadic "imul"   $ \_ vs ->
  case vs of
    [val] -> do
      SomeBV v <- getSomeBVValue val
      exec_imul1 v
    [loc, val] -> do
      SomeBV l <- getSomeBVLocation loc
      v' <- getSignExtendedValue val (loc_width l)
      v <- get l
      exec_imul2_3 l v v'
    [loc, val, val'] -> do
      SomeBV l <- getSomeBVLocation loc
      v  <- getBVValue val (loc_width l)
      SomeBV v' <- getSomeBVValue val'
      Just LeqProof <- return $ testLeq (bv_width v') (bv_width v)
      exec_imul2_3 l v v'
    _ ->
      fail "Impossible number of argument in imul"

-- | Should be equiv to 0 - *l
exec_neg :: (IsLocationBV m n) =>  MLocation m (BVType n) -> m ()
exec_neg l = do
  v <- get l
  cf_loc .= mux (is_zero v) false true
  let r = bvNeg v
      zero = bvLit (bv_width v) 0
  of_loc .= ssub_overflows  zero v
  af_loc .= usub4_overflows zero v
  set_result_value l r

exec_sahf :: Semantics m => m ()
exec_sahf =
  do v <- get (reg_high8 N.rax)
     let mk n = bvBit v (bvLit n8 n)
     cf_loc .= mk 0
     pf_loc .= mk 2
     af_loc .= mk 4
     zf_loc .= mk 6
     sf_loc .= mk 7

exec_sbb :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_sbb l v = do cf <- get cf_loc
                  v0 <- get l
                  let v' = v `bvAdd` (uext (loc_width l) cf)
                  -- Set overflow and arithmetic flags
                  of_loc .= ssbb_overflows v0 v cf
                  af_loc .= uadd4_overflows v (uext knownNat cf) .|.
                    usub4_overflows v0 v'
                  cf_loc .= uadd_overflows v (uext (loc_width l) cf) .|.
                    (usub_overflows  v0 v')
                  -- Set result value.
                  let res = v0 `bvSub` v'
                  set_result_flags res
                  l .= res

-- FIXME: duplicates subtraction term by calling exec_cmp
exec_sub :: Binop
exec_sub l v = do
  v0 <- get l
  exec_cmp l v -- set flags
  l .= (v0 `bvSub` v)

-- ** Decimal Arithmetic Instructions
-- ** Logical Instructions

-- | And two values together.
exec_and :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_and r y = do
  x <- get r
  let z = x .&. y
  set_bitwise_flags z
  r .= z

exec_not :: (Semantics m, 1 <= n) => MLocation m (BVType n) -> m ()
exec_not = modify complement

exec_or :: Binop
exec_or l v = do
  v' <- get l
  set_undefined af_loc
  of_loc .= false
  cf_loc .= false
  set_result_value l (v' .|. v)

exec_xor :: Binop
exec_xor l v = do
  v0 <- get l
  let r = v0 `bvXor` v
  set_bitwise_flags r
  l .= r

-- ** Shift and Rotate Instructions


really_exec_shift :: (1 <= n', n' <= n, IsLocationBV m n)
                  => MLocation m (BVType n)
                  -> Value m (BVType n')
                     -- Operation for performing the shift.
                     -- Takes value as first argument and shift amount as second arg.
                  -> (Value m (BVType n) -> Value m (BVType n) -> Value m (BVType n))
                     -- Operation for constructing new carry flag "cf" value.
                  -> (Value m (BVType n) -> Value m (BVType n')
                                         -> Value m (BVType n')
                                         -> Value m BoolType)
                     -- Operation for constructing new overflow flag "of" value.
                  -> (Value m (BVType n) -> Value m (BVType n)
                                         -> Value m BoolType
                                         -> Value m BoolType)
                  -> m ()
really_exec_shift l count do_shift mk_cf mk_of = do
  v    <- get l
  -- The intel manual says that the count is masked to give an upper
  -- bound on the time the shift takes, with a mask of 63 in the case
  -- of a 64 bit operand, and 31 in the other cases.
  let nbits =
        case testLeq (bv_width v) n32 of
          Just LeqProof -> 32
          _             -> 64
      count_mask = bvLit (bv_width count) (nbits - 1)
      --
      low_count = count .&. count_mask  -- FIXME: prefer mod?
      r = do_shift v (uext (bv_width v) low_count)

  -- When the count is zero, nothing happens, in particular, no flags change
  unless_ (is_zero low_count) $ do
    let dest_width = bvLit (bv_width low_count) (natValue (bv_width v))

    let new_cf = mk_cf v dest_width low_count
    cf_undef <- make_undefined knownType
    cf_loc .= mux (low_count `bvUlt` dest_width) new_cf cf_undef

    let low1 = bvLit (bv_width low_count) 1

    of_undef <- make_undefined knownType
    of_loc .= mux (low_count .=. low1) (mk_of v r new_cf) of_undef

    set_undefined af_loc
    set_result_value l r

-- FIXME: could be 8 instead of n' here ...
exec_shl :: (1 <= n', n' <= n, IsLocationBV m n)
         => MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_shl l count = really_exec_shift l count bvShl mk_cf mk_of
  where mk_cf v dest_width low_count = bvBit v (dest_width `bvSub` low_count)
        mk_of _ r new_cf = msb r `bvXor` new_cf

exec_shr :: (1 <= n', n' <= n, IsLocationBV m n)
         => MLocation m (BVType n)
         -> Value m (BVType n') -> m ()
exec_shr l count = really_exec_shift l count bvShr mk_cf mk_of
  where mk_cf v _ low_count = bvBit v (low_count `bvSub` bvLit (bv_width low_count) 1)
        mk_of v _ _         = msb v

-- FIXME: we can factor this out as above, but we need to check the CF
-- for SAR (intel manual says it is only undefined for shl/shr when
-- the shift is >= the bit width.
exec_sar :: (1 <= n', n' <= n, IsLocationBV m n)
         => MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_sar l count = do
  v    <- get l
  -- The intel manual says that the count is masked to give an upper
  -- bound on the time the shift takes, with a mask of 63 in the case
  -- of a 64 bit operand, and 31 in the other cases.
  let nbits = case testLeq (bv_width v) n32 of
                Just LeqProof -> 32
                Nothing       -> 64
      countMASK = bvLit (bv_width v) (nbits - 1)
      low_count = uext (bv_width v) count .&. countMASK  -- FIXME: prefer mod?
      r = bvSar v low_count

  -- When the count is zero, nothing happens, in particular, no flags change
  when_ (complement $ is_zero low_count) $ do
    let dest_width = bvLit (bv_width low_count) (natValue (bv_width v))
    let new_cf = bvBit v (low_count `bvSub` bvLit (bv_width low_count) 1)

    -- FIXME: correct?  we assume here that we will get the sign bit ...
    cf_loc .= mux (low_count `bvUlt` dest_width) new_cf (msb v)

    ifte_ (low_count .=. bvLit (bv_width low_count) 1)
      (of_loc .= false)
      (set_undefined of_loc)

    set_undefined af_loc
    set_result_value l r

-- FIXME: use really_exec_shift above?
exec_rol :: (1 <= n', n' <= n, IsLocationBV m n)
         => MLocation m (BVType n)
         -> Value m (BVType n')
         -> m ()
exec_rol l count = do
  v    <- get l
  -- The intel manual says that the count is masked to give an upper
  -- bound on the time the shift takes, with a mask of 63 in the case
  -- of a 64 bit operand, and 31 in the other cases.
  let nbits = case testLeq (bv_width v) n32 of
                Just LeqProof -> 32
                _             -> 64
      countMASK = bvLit (bv_width v) (nbits - 1)
      low_count = uext (bv_width v) count .&. countMASK
      -- countMASK is sufficient for 32 and 64 bit operand sizes, but not 16 or
      -- 8, so we need to mask those off again...
      effectiveMASK = bvLit (bv_width v) (natValue (bv_width v) - 1)
      effective = uext (bv_width v) count .&. effectiveMASK
      r = bvRol v effective

  l .= r

  -- When the count is zero only the assignment happens (cf is not changed)
  when_ (complement $ is_zero low_count) $ do
    let new_cf = bvBit r (bvLit (bv_width r) 0)
    cf_loc .= new_cf

    ifte_ (low_count .=. bvLit (bv_width low_count) 1)
          (of_loc .= (msb r `bvXor` new_cf))
          (set_undefined of_loc)

-- FIXME: use really_exec_shift above?
exec_ror :: (1 <= n', n' <= n, IsLocationBV m n)
         => MLocation m (BVType n)
         -> Value m (BVType n')
         -> m ()
exec_ror l count = do
  v    <- get l
  -- The intel manual says that the count is masked to give an upper
  -- bound on the time the shift takes, with a mask of 63 in the case
  -- of a 64 bit operand, and 31 in the other cases.
  let nbits = case testLeq (bv_width v) n32 of
                Just LeqProof -> 32
                Nothing       -> 64
      countMASK = bvLit (bv_width v) (nbits - 1)
      low_count = uext (bv_width v) count .&. countMASK
      -- countMASK is sufficient for 32 and 64 bit operand sizes, but not 16 or
      -- 8, so we need to mask those off again...
      effectiveMASK = bvLit (bv_width v) (natValue (bv_width v) - 1)
      effective = uext (bv_width v) count .&. effectiveMASK
      r = bvRor v effective

  l .= r

  when_ (complement $ is_zero low_count) $ do
    let new_cf = bvBit r (bvLit (bv_width r) (natValue (bv_width r) - 1))
    cf_loc .= new_cf

    ifte_ (low_count .=. bvLit (bv_width low_count) 1)
          (of_loc .= (msb r `bvXor` bvBit r (bvLit (bv_width r) (natValue (bv_width v) - 2))))
          (set_undefined of_loc)

-- ** Bit and Byte Instructions

isRegister :: Location addr tp -> Bool
isRegister (Register _)      = True
isRegister (MemoryAddr {})   = False
isRegister (ControlReg _)    = True
isRegister (DebugReg _)      = True
isRegister (SegmentReg _)    = True
isRegister (X87ControlReg _) = True
isRegister (X87StackRegister {}) = False

-- return val modulo the size of the register at loc iff loc is a register, otherwise return val
moduloRegSize :: (IsValue v, 1 <= n) => Location addr (BVType n') -> v (BVType n) -> v (BVType n)
moduloRegSize loc val
  | Just Refl <- testEquality (loc_width loc) n8  = go loc val  7
  | Just Refl <- testEquality (loc_width loc) n16 = go loc val 15
  | Just Refl <- testEquality (loc_width loc) n32 = go loc val 31
  | Just Refl <- testEquality (loc_width loc) n64 = go loc val 63
  | otherwise = val -- doesn't match any of the register sizes
  where go l v maskVal | isRegister l = v .&. bvLit (bv_width v) maskVal -- v mod maskVal
                       | otherwise = v

-- make a bitmask of size 'width' with only the bit at bitPosition set
singleBitMask :: (IsValue v, 1 <= n, 1 <= log_n, log_n <= n) => NatRepr n -> v (BVType log_n) -> v (BVType n)
singleBitMask width bitPosition = bvShl (bvLit width 1) (uext width bitPosition)

exec_bt :: (IsLocationBV m n, 1 <= log_n) => MLocation m (BVType n) -> Value m (BVType log_n) -> m ()
exec_bt base offset = do
  b <- get base
  -- if base is register, take offset modulo 16/32/64 based on reg width
  cf_loc .= bvBit b (moduloRegSize base offset)
  set_undefined of_loc
  set_undefined sf_loc
  set_undefined af_loc
  set_undefined pf_loc

-- for all BT* instructions that modify the checked bit
exec_bt_chg :: (IsLocationBV m n, 1 <= log_n, log_n <= n)
            => (Value m (BVType n) -> Value m (BVType n) -> Value m (BVType n))
            -> MLocation m (BVType n)
            -> Value m (BVType log_n) -> m ()
exec_bt_chg op base offset = do
  exec_bt base offset
  b <- get base
  base .= b `op` singleBitMask (loc_width base) (moduloRegSize base offset)

exec_btc, exec_btr, exec_bts :: (IsLocationBV m n, 1 <= log_n, log_n <= n) => MLocation m (BVType n) -> Value m (BVType log_n) -> m ()
exec_btc = exec_bt_chg bvXor
exec_btr = exec_bt_chg $ \l r -> l .&. (complement r)
exec_bts = exec_bt_chg (.|.)


exec_bsf :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_bsf r y = do
  zf_loc .= is_zero y
  set_undefined cf_loc
  set_undefined of_loc
  set_undefined sf_loc
  set_undefined af_loc
  set_undefined pf_loc
  r .= bsf y

exec_bsr :: IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()
exec_bsr r y = do
  zf_loc .= is_zero y
  set_undefined cf_loc
  set_undefined of_loc
  set_undefined sf_loc
  set_undefined af_loc
  set_undefined pf_loc
  r .= bsr y

exec_test :: Binop
exec_test l v = do
  v' <- get l
  let r = v' .&. v
  set_bitwise_flags r

def_set_list :: [InstructionDef]
def_set_list =
  defConditionals "set" $ \mnem cc ->
    defUnary mnem $ \_ v -> do
      l <- getBVLocation v n8
      c <- cc
      l .= mux c (bvLit n8 1) (bvLit n8 0)

-- ** Control Transfer Instructions

def_call :: InstructionDef
def_call = defUnary "call" $ \_ v -> do
  -- Push value of next instruction
  old_pc <- get rip
  push addrRepr old_pc
  -- Set IP
  tgt <- getJumpTarget v
  rip .= tgt

-- | Conditional jumps
def_jcc_list :: [InstructionDef]
def_jcc_list =
  defConditionals "j" $ \mnem cc ->
    defUnary mnem $ \_ v -> do
      a <- cc
      when_ a $ do
        old_pc <- get rip
        off <- getBVValue v knownNat
        rip .= old_pc `bvAdd` off

def_jmp :: InstructionDef
def_jmp = defUnary "jmp" $ \_ v -> do
  tgt <- getJumpTarget v
  rip .= tgt

def_ret :: InstructionDef
def_ret = defVariadic "ret"    $ \_ vs ->
  case vs of
    [] -> do
      -- Pop IP and jump to it.
      next_ip <- pop addrRepr
      rip .= next_ip
    [F.WordImm off] -> do
      -- Pop IP and adjust stack pointer.
      next_ip <- pop addrRepr
      modify (bvAdd (bvLit n64 (toInteger off))) rsp
      -- Set IP
      rip .= next_ip
    _ ->
      fail "Unexpected number of args to ret"

-- ** String Instructions

-- | MOVS/MOVSB Move string/Move byte string
-- MOVS/MOVSW Move string/Move word string
-- MOVS/MOVSD Move string/Move doubleword string

regLocation :: NatRepr n -> N.RegisterName N.GP -> Location addr (BVType n)
regLocation sz
  | Just Refl <- testEquality sz n8  = reg_low8
  | Just Refl <- testEquality sz n16 = reg_low16
  | Just Refl <- testEquality sz n32 = reg_low32
  | Just Refl <- testEquality sz n64 = fullRegister
  | otherwise = fail "regLocation: Unknown bit width"

-- FIXME: probably doesn't work for 32 bit address sizes
-- arguments are only for the size, they are fixed at rsi/rdi
exec_movs :: Semantics m
          => Bool -- Flag indicating if RepPrefix appeared before instruction
          -> NatRepr w -- Number of bytes to move at a time.
          -> m ()
exec_movs False w = do
  let repr = BVMemRepr w LittleEndian
  let bytesPerOp = bvLit n64 (memReprBytes repr)

  -- The direction flag indicates post decrement or post increment.
  df <- get df_loc
  src  <- get rsi
  dest <- get rdi
  v' <- get $ MemoryAddr src repr
  MemoryAddr dest repr .= v'

  rsi .= mux df (src  .- bytesPerOp) (src  .+ bytesPerOp)
  rdi .= mux df (dest .- bytesPerOp) (dest .+ bytesPerOp)
exec_movs True w = do
  let repr = BVMemRepr w LittleEndian

    -- FIXME: aso modifies this
  let count_reg = regLocation n64 N.rcx
      bytesPerOp = memReprBytes repr
      bytesPerOpv = bvLit n64 bytesPerOp
  -- The direction flag indicates post decrement or post increment.
  df <- get df_loc
  src   <- get rsi
  dest  <- get rdi
  count <- get count_reg
  let total_bytes = count .* bytesPerOpv
  -- FIXME: we might need direction for overlapping regions
  count_reg .= bvLit n64 (0::Integer)
  memcopy bytesPerOp count src dest df
  rsi .= mux df (src   .- total_bytes) (src   .+ total_bytes)
  rdi .= mux df (dest  .- total_bytes) (dest  .+ total_bytes)

def_movs :: InstructionDef
def_movs = defBinary "movs" $ \pfx loc loc' -> do
  case (loc, loc') of
    (F.ByteReg F.DIL,  F.ByteReg F.SIL) ->
      exec_movs (pfx == F.RepPrefix) (knownNat :: NatRepr 1)
    (F.WordReg F.DI,   F.WordReg F.SI) ->
      exec_movs (pfx == F.RepPrefix) (knownNat :: NatRepr 2)
    (F.DWordReg F.EDI, F.DWordReg F.ESI) ->
      exec_movs (pfx == F.RepPrefix) (knownNat :: NatRepr 4)
    (F.QWordReg F.RDI, F.QWordReg F.RSI) ->
      exec_movs (pfx == F.RepPrefix) (knownNat :: NatRepr 8)
    _ -> fail "Bad argument to movs"

-- FIXME: can also take rep prefix
-- FIXME: we ignore the aso here.
-- | CMPS/CMPSB Compare string/Compare byte string
-- CMPS/CMPSW Compare string/Compare word string
-- CMPS/CMPSD Compare string/Compare doubleword string

exec_cmps :: Semantics m
          => Bool
          -> RepValSize w
          -> m ()
exec_cmps repz_pfx rval = repValHasSupportedWidth rval $ do
  let repr = repValSizeMemRepr rval
  -- The direction flag indicates post decrement or post increment.
  df <- get df_loc
  v_rsi <- get rsi
  v_rdi <- get rdi
    -- FIXME: aso modifies this
  let bytesPerOp = memReprBytes repr
  let bytesPerOp' = bvLit n64 bytesPerOp
  if repz_pfx then do
    count <- get (regLocation n64 N.rcx)
    unless_ (count .=. bvKLit 0) $ do
      nsame <- memcmp bytesPerOp count v_rsi v_rdi df
      let equal = (nsame .=. count)
          nwordsSeen = mux equal count (count `bvSub` (nsame `bvAdd` bvKLit 1))

      -- we need to set the flags as if the last comparison was done, hence this.
      let lastWordBytes = (nwordsSeen `bvSub` bvKLit 1) `bvMul` bytesPerOp'
          lastSrc  = mux df (v_rsi `bvSub` lastWordBytes) (v_rsi `bvAdd` lastWordBytes)
          lastDest = mux df (v_rdi `bvSub` lastWordBytes) (v_rdi `bvAdd` lastWordBytes)

      v' <- get $ MemoryAddr lastDest repr
      exec_cmp (MemoryAddr lastSrc repr) v' -- FIXME: right way around?

      -- we do this to make it obvious so repz cmpsb ; jz ... is clear
      zf_loc .= equal
      let nbytesSeen = nwordsSeen `bvMul` bytesPerOp'

      rsi .= mux df (v_rsi `bvSub` nbytesSeen) (v_rsi `bvAdd` nbytesSeen)
      rdi .= mux df (v_rdi `bvSub` nbytesSeen) (v_rdi `bvAdd` nbytesSeen)
      rcx .= (count .- nwordsSeen)
   else do
     v' <- get $ MemoryAddr v_rdi repr
     exec_cmp (MemoryAddr   v_rsi repr) v' -- FIXME: right way around?
     rsi .= mux df (v_rsi  `bvSub` bytesPerOp') (v_rsi `bvAdd` bytesPerOp')
     rdi .= mux df (v_rdi  `bvSub` bytesPerOp') (v_rdi `bvAdd` bytesPerOp')


def_cmps :: InstructionDef
def_cmps = defBinary "cmps" $ \pfx loc loc' -> do
  Some rval <-
    case (loc, loc') of
      (F.ByteReg F.SIL,  F.ByteReg F.DIL) -> do
        pure $ Some ByteRepVal
      (F.WordReg F.SI,   F.WordReg F.DI) -> do
        pure $ Some WordRepVal
      (F.DWordReg F.ESI, F.DWordReg F.EDI) -> do
        pure $ Some DWordRepVal
      (F.QWordReg F.RSI, F.QWordReg F.RDI) -> do
        pure $ Some QWordRepVal
      _ -> fail "Bad argument to cmps"
  exec_cmps (pfx == F.RepZPrefix) rval

-- SCAS/SCASB Scan string/Scan byte string
-- SCAS/SCASW Scan string/Scan word string
-- SCAS/SCASD Scan string/Scan doubleword string

xaxValLoc :: RepValSize w -> Location a (BVType w)
xaxValLoc ByteRepVal  = reg_low8  (N.X86_GP 0)
xaxValLoc WordRepVal  = reg_low16 (N.X86_GP 0)
xaxValLoc DWordRepVal = reg_low32 (N.X86_GP 0)
xaxValLoc QWordRepVal = fullRegister (N.X86_GP 0)

-- The arguments to this are always rax/QWORD PTR es:[rdi], so we only
-- need the args for the size.
exec_scas :: Semantics m
          => Bool -- Flag indicating if RepZPrefix appeared before instruction
          -> Bool -- Flag indicating if RepNZPrefix appeared before instruction
          -> RepValSize n
          -> m ()
exec_scas True True _val_loc = error "Can't have both Z and NZ prefix"
-- single operation case
exec_scas False False rep = repValHasSupportedWidth rep $ do
  df <- get df_loc
  v_rdi <- get rdi
  v_rax <- get (xaxValLoc rep)
  let memRepr = repValSizeMemRepr rep
  exec_cmp (MemoryAddr v_rdi memRepr) v_rax  -- FIXME: right way around?
  let bytesPerOp = mux df (bvLit n64 (negate (memReprBytes memRepr)))
                          (bvLit n64 (memReprBytes memRepr))
  rdi   .= v_rdi `bvAdd` bytesPerOp
-- repz or repnz prefix set
exec_scas _repz_pfx repnz_pfx rep = repValHasSupportedWidth rep $ do
  let mrepr = repValSizeMemRepr rep
  let val_loc = xaxValLoc rep
  -- Get the direction flag -- it will be used to determine whether to add or subtract at each step.
  -- If the flag is zero, then the register is incremented, otherwise it is incremented.
  df    <- get df_loc

  -- Get value that we are using in comparison
  v_rax <- get val_loc

  --  Get the starting address for the comparsions
  v_rdi <- get rdi
  -- Get maximum number of times to execute instruction
  count <- get rcx
  unless_ (count .=. bvKLit 0) $ do

    count' <- rep_scas repnz_pfx df rep v_rax v_rdi count

    -- Get number of bytes each comparison will use
    let bytesPerOp = memReprBytes mrepr
    -- Get multiple of each element (negated for direction flag
    let bytePerOpLit = mux df (bvKLit (negate bytesPerOp)) (bvKLit bytesPerOp)

    -- Count the number of bytes seen.
    let nBytesSeen    = (count `bvSub` count') `bvMul` bytePerOpLit

    let lastWordBytes = nBytesSeen `bvSub` bytePerOpLit

    exec_cmp (MemoryAddr (v_rdi `bvAdd` lastWordBytes) mrepr) v_rax

    rdi .= v_rdi `bvAdd` nBytesSeen
    rcx .= count'

def_scas :: InstructionDef
def_scas = defBinary "scas" $ \pfx loc loc' -> do
  Some rval <-
    case (loc, loc') of
      (F.ByteReg  F.AL,  F.Mem8  (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
        pure $ Some ByteRepVal
      (F.WordReg  F.AX,  F.Mem16 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
        pure $ Some WordRepVal
      (F.DWordReg F.EAX, F.Mem32 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
        pure $ Some DWordRepVal
      (F.QWordReg F.RAX, F.Mem64 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
        pure $ Some QWordRepVal
      _ -> error $ "scas given bad addrs " ++ show (loc, loc')
  exec_scas (pfx == F.RepZPrefix) (pfx == F.RepNZPrefix) rval

-- LODS/LODSB Load string/Load byte string
-- LODS/LODSW Load string/Load word string
-- LODS/LODSD Load string/Load doubleword string

-- | STOS/STOSB Store string/Store byte string
-- STOS/STOSW Store string/Store word string
-- STOS/STOSD Store string/Store doubleword string
exec_stos :: Semantics m
          => Bool -- Flag indicating if RepPrefix appeared before instruction
          -> RepValSize w
          -> m ()
exec_stos False rep = do
  let mrepr = repValSizeMemRepr rep
  -- The direction flag indicates post decrement or post increment.
  df   <- get df_loc
  dest <- get rdi
  v    <- get (xaxValLoc rep)
  let neg_szv = bvLit n64 (negate (memReprBytes mrepr))
  let szv     = bvLit n64 (memReprBytes mrepr)
  MemoryAddr dest mrepr .= v
  rdi .= dest .+ mux df neg_szv szv
exec_stos True rep = do
  let mrepr = repValSizeMemRepr rep
  -- The direction flag indicates post decrement or post increment.
  df   <- get df_loc
  dest <- get rdi
  v    <- get (xaxValLoc rep)
  let szv = bvLit n64 (memReprBytes mrepr)
  count <- get (regLocation n64 N.rcx)
  let nbytes     = count `bvMul` szv
  memset count v dest df
  rdi .= mux df (dest .- nbytes) (dest .+ nbytes)
  rcx .= bvKLit 0

def_stos :: InstructionDef
def_stos = defBinary "stos" $ \pfx loc loc' -> do
  case (loc, loc') of
    (F.Mem8  (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement), F.ByteReg  F.AL) -> do
      exec_stos (pfx == F.RepPrefix) ByteRepVal
    (F.Mem16 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement), F.WordReg  F.AX) -> do
      exec_stos (pfx == F.RepPrefix) WordRepVal
    (F.Mem32 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement), F.DWordReg F.EAX) -> do
      exec_stos (pfx == F.RepPrefix) DWordRepVal
    (F.Mem64 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement), F.QWordReg F.RAX) -> do
      exec_stos (pfx == F.RepPrefix) QWordRepVal
    _ -> error $ "stos given bad arguments " ++ show (loc, loc')

-- REP        Repeat while ECX not zero
-- REPE/REPZ  Repeat while equal/Repeat while zero
-- REPNE/REPNZ Repeat while not equal/Repeat while not zero

-- ** I/O Instructions
-- ** Enter and Leave Instructions

exec_leave :: Semantics m => m ()
exec_leave = do
  bp_v <- get rbp
  rsp .= bp_v
  bp_v' <- pop addrRepr
  rbp .= bp_v'

-- ** Flag Control (EFLAG) Instructions

-- | Run clc instruction.
exec_clc :: Semantics m => m ()
exec_clc = cf_loc .= false

-- | Run cld instruction.
exec_cld :: Semantics m => m ()
exec_cld = df_loc .= false

-- ** Segment Register Instructions
-- ** Miscellaneous Instructions

def_lea :: InstructionDef
def_lea = defBinary "lea" $ \_ loc (F.VoidMem ar) -> do
  SomeBV l <- getSomeBVLocation loc
  -- ensure that the location is at most 64 bits
  Just LeqProof <- return $ testLeq (loc_width l) n64
  v <- getBVAddress ar
  l .= bvTrunc (loc_width l) v

-- ** Random Number Generator Instructions
-- ** BMI1, BMI2

-- * X86 FPU instructions

type FPUnop  = forall flt m.
  Semantics m => FloatInfoRepr flt -> MLocation m (FloatType flt) -> m ()
type FPUnopV = forall flt m.
  Semantics m => FloatInfoRepr flt -> Value m (FloatType flt) -> m ()
type FPBinop = forall flt_d flt_s m.
  Semantics m => FloatInfoRepr flt_d -> MLocation m (FloatType flt_d) ->
                 FloatInfoRepr flt_s -> Value m (FloatType flt_s) -> m ()

-- ** Data transfer instructions

-- | FLD Load floating-point value
exec_fld :: FPUnopV
exec_fld fir v = x87Push (fpCvt fir X86_80FloatRepr v)

-- | FST Store floating-point value
exec_fst :: FPUnop
exec_fst fir l = do
  v <- get (X87StackRegister 0)
  set_undefined c0_loc
  set_undefined c2_loc
  set_undefined c3_loc
  -- TODO: The value assigned to c1_loc seems wrong
  -- The bit is only set if the floating-point inexact exception is thrown.
  -- It should be set to 0 is if a stack underflow occurred.
  c1_loc .= fpCvtRoundsUp X86_80FloatRepr fir v

  l .= fpCvt X86_80FloatRepr fir v

-- | FSTP Store floating-point value
exec_fstp :: FPUnop
exec_fstp fir l = exec_fst fir l >> x87Pop

-- FILD Load integer
-- FIST Store integer
-- FISTP1 Store integer and pop
-- FBLD Load BCD
-- FBSTP Store BCD and pop
-- FXCH Exchange registers
-- FCMOVE Floating-point conditional   move if equal
-- FCMOVNE Floating-point conditional  move if not equal
-- FCMOVB Floating-point conditional   move if below
-- FCMOVBE Floating-point conditional  move if below or equal
-- FCMOVNB Floating-point conditional  move if not below
-- FCMOVNBE Floating-point conditional move if not below or equal
-- FCMOVU Floating-point conditional   move if unordered
-- FCMOVNU Floating-point conditional  move if not unordered

-- ** Basic arithmetic instructions

fparith :: Semantics m =>
           (forall flt
            . FloatInfoRepr flt
            -> Value m (FloatType flt)
            -> Value m (FloatType flt)
            -> Value m (FloatType flt))
           -> (forall flt
               . FloatInfoRepr flt
               -> Value m (FloatType flt)
               -> Value m (FloatType flt)
               -> Value m BoolType)
           -> FloatInfoRepr flt_d
           -> MLocation m (FloatType flt_d)
           -> FloatInfoRepr flt_s
           -> Value m (FloatType flt_s)
           -> m ()
fparith op opRoundedUp fir_d l fir_s v = do
  let up_v = fpCvt fir_s fir_d v
  v' <- get l
  set_undefined c0_loc
  c1_loc .= opRoundedUp fir_d v' up_v
  set_undefined c2_loc
  set_undefined c3_loc
  l .= op fir_d v' up_v

-- | FADD Add floating-point
exec_fadd :: FPBinop
exec_fadd = fparith fpAdd fpAddRoundedUp

-- FADDP Add floating-point and pop
-- FIADD Add integer

-- | FSUB Subtract floating-point
exec_fsub :: FPBinop
exec_fsub = fparith fpSub fpSubRoundedUp

-- | FSUBP Subtract floating-point and pop
exec_fsubp :: FPBinop
exec_fsubp fir_d l fir_s v = exec_fsub fir_d l fir_s v >> x87Pop

-- FISUB Subtract integer

-- | FSUBR Subtract floating-point reverse
exec_fsubr :: FPBinop
exec_fsubr = fparith (reverseOp fpSub) (reverseOp fpSubRoundedUp)
  where
    reverseOp f = \fir x y -> f fir y x

-- | FSUBRP Subtract floating-point reverse and pop
exec_fsubrp :: FPBinop
exec_fsubrp fir_d l fir_s v = exec_fsubr fir_d l fir_s v >> x87Pop

-- FISUBR Subtract integer reverse

-- FIXME: we could factor out commonalities between this and fadd
-- | FMUL Multiply floating-point
exec_fmul :: FPBinop
exec_fmul = fparith fpMul fpMulRoundedUp

-- FMULP Multiply floating-point and pop
-- FIMUL Multiply integer
-- FDIV Divide floating-point
-- FDIVP Divide floating-point and pop
-- FIDIV Divide integer
-- FDIVR Divide floating-point reverse
-- FDIVRP Divide floating-point reverse and pop
-- FIDIVR Divide integer reverse
-- FPREM Partial remainder
-- FPREM1 IEEE Partial remainder
-- FABS Absolute value
-- FCHS Change sign
-- FRNDINT Round to integer
-- FSCALE Scale by power of two
-- FSQRT Square root
-- FXTRACT Extract exponent and significand

-- ** Comparison instructions

-- FCOM Compare floating-point
-- FCOMP Compare floating-point and pop
-- FCOMPP Compare floating-point and pop twice
-- FUCOM Unordered compare floating-point
-- FUCOMP Unordered compare floating-point and pop
-- FUCOMPP Unordered compare floating-point and pop twice
-- FICOM Compare integer
-- FICOMP Compare integer and pop
-- FCOMI Compare floating-point and set EFLAGS
-- FUCOMI Unordered compare floating-point and set EFLAGS
-- FCOMIP Compare floating-point, set EFLAGS, and pop
-- FUCOMIP Unordered compare floating-point, set EFLAGS, and pop
-- FTST Test floating-point (compare with 0.0)
-- FXAM Examine floating-point

-- ** Transcendental instructions

-- FSIN Sine
-- FCOS Cosine
-- FSINCOS Sine and cosine
-- FPTAN Partial tangent
-- FPATAN Partial arctangent
-- F2XM1 2x − 1
-- FYL2X y∗log2x
-- FYL2XP1 y∗log2(x+1)

-- ** Load constant instructions

-- FLD1 Load +1.0
-- FLDZ Load +0.0
-- FLDPI Load π
-- FLDL2E Load log2e
-- FLDLN2 Load loge2
-- FLDL2T Load log210
-- FLDLG2 Load log102


-- ** x87 FPU control instructions

-- FINCSTP Increment FPU register stack pointer
-- FDECSTP Decrement FPU register stack pointer
-- FFREE Free floating-point register
-- FINIT Initialize FPU after checking error conditions
-- FNINIT Initialize FPU without checking error conditions
-- FCLEX Clear floating-point exception flags after checking for error conditions
-- FNCLEX Clear floating-point exception flags without checking for error conditions
-- FSTCW Store FPU control word after checking error conditions

-- | FNSTCW Store FPU control word without checking error conditions
def_fnstcw :: InstructionDef
def_fnstcw = defUnary "fnstcw" $ \_ loc -> do
  case loc of
    F.Mem16 f_addr -> do
      addr <- getBVAddress f_addr
      set_undefined c0_loc
      set_undefined c1_loc
      set_undefined c2_loc
      set_undefined c3_loc
      fnstcw addr
    _ -> fail $ "fnstcw given bad argument " ++ show loc

-- FLDCW Load FPU control word
-- FSTENV Store FPU environment after checking error conditions
-- FNSTENV Store FPU environment without checking error conditions
-- FLDENV Load FPU environment
-- FSAVE Save FPU state after checking error conditions
-- FNSAVE Save FPU state without checking error conditions
-- FRSTOR Restore FPU state
-- FSTSW Store FPU status word after checking error conditions
-- FNSTSW Store FPU status word without checking error conditions
-- WAIT/FWAIT Wait for FPU
-- FNOP FPU no operation

-- * X87 FPU and SIMD State Management Instructions

-- FXSAVE Save x87 FPU and SIMD state
-- FXRSTOR Restore x87 FPU and SIMD state


-- * MMX Instructions

-- ** MMX Data Transfer Instructions

exec_movd, exec_movq :: (IsLocationBV m n, 1 <= n')
                     => MLocation m (BVType n)
                     -> Value m (BVType n')
                     -> m ()
exec_movd l v
  | Just LeqProof <- testLeq  (loc_width l) (bv_width v) = l .= bvTrunc (loc_width l) v
  | Just LeqProof <- testLeq  (bv_width v) (loc_width l) = l .=    uext (loc_width l) v
  | otherwise = fail "movd: Unknown bit width"
exec_movq = exec_movd


-- ** MMX Conversion Instructions

-- PACKSSWB Pack words into bytes with signed saturation
-- PACKSSDW Pack doublewords into words with signed saturation
-- PACKUSWB Pack words into bytes with unsigned saturation

punpck :: (IsLocationBV m n, 1 <= o)
       => (([Value m (BVType o)], [Value m (BVType o)]) -> [Value m (BVType o)])
       -> NatRepr o -> MLocation m (BVType n) -> Value m (BVType n) -> m ()
punpck f pieceSize l v = do
  v0 <- get l
  let dSplit = f $ splitHalf $ bvVectorize pieceSize v0
      sSplit = f $ splitHalf $ bvVectorize pieceSize v
      r = bvUnvectorize (loc_width l) $ concat $ zipWith (\a b -> [b, a]) dSplit sSplit
  l .= r
  where splitHalf :: [a] -> ([a], [a])
        splitHalf xs = splitAt ((length xs + 1) `div` 2) xs

punpckh, punpckl :: (IsLocationBV m n, 1 <= o) => NatRepr o -> MLocation m (BVType n) -> Value m (BVType n) -> m ()
punpckh = punpck fst
punpckl = punpck snd

exec_punpckhbw, exec_punpckhwd, exec_punpckhdq, exec_punpckhqdq :: Binop
exec_punpckhbw  = punpckh n8
exec_punpckhwd  = punpckh n16
exec_punpckhdq  = punpckh n32
exec_punpckhqdq = punpckh n64

exec_punpcklbw, exec_punpcklwd, exec_punpckldq, exec_punpcklqdq :: Binop
exec_punpcklbw  = punpckl n8
exec_punpcklwd  = punpckl n16
exec_punpckldq  = punpckl n32
exec_punpcklqdq = punpckl n64


-- ** MMX Packed Arithmetic Instructions

exec_paddb :: Binop
exec_paddb l v = do
  v0 <- get l
  l .= vectorize2 n8 bvAdd v0 v

exec_paddw :: Binop
exec_paddw l v = do
  v0 <- get l
  l .= vectorize2 n16 bvAdd v0 v

exec_paddd :: Binop
exec_paddd l v = do
  v0 <- get l
  l .= vectorize2 n32 bvAdd v0 v

-- PADDSB Add packed signed byte integers with signed saturation
-- PADDSW Add packed signed word integers with signed saturation
-- PADDUSB Add packed unsigned byte integers with unsigned saturation
-- PADDUSW Add packed unsigned word integers with unsigned saturation

exec_psubb :: Binop
exec_psubb l v = do
  v0 <- get l
  l .= vectorize2 n8 bvSub v0 v

exec_psubw :: Binop
exec_psubw l v = do
  v0 <- get l
  l .= vectorize2 n16 bvSub v0 v

exec_psubd :: Binop
exec_psubd l v = do
  v0 <- get l
  l .= vectorize2 n32 bvSub v0 v

-- PSUBSB Subtract packed signed byte integers with signed saturation
-- PSUBSW Subtract packed signed word integers with signed saturation
-- PSUBUSB Subtract packed unsigned byte integers with unsigned saturation
-- PSUBUSW Subtract packed unsigned word integers with unsigned saturation
-- PMULHW Multiply packed signed word integers and store high result
-- PMULLW Multiply packed signed word integers and store low result
-- PMADDWD Multiply and add packed word integers


-- ** MMX Comparison Instructions

-- replace pairs with 0xF..F if `op` returns true, otherwise 0x0..0
pcmp :: (IsLocationBV m n, 1 <= o)
     => (Value m (BVType o) -> Value m (BVType o) -> Value m BoolType)
     -> NatRepr o
     -> MLocation m (BVType n) -> Value m (BVType n) -> m ()
pcmp op sz l v = do
  v0 <- get l
  l .= vectorize2 sz chkHighLow v0 v
  where chkHighLow d s = mux (d `op` s)
                             (bvLit (bv_width d) (negate 1))
                             (bvLit (bv_width d) 0)

exec_pcmpeqb, exec_pcmpeqw, exec_pcmpeqd  :: Binop
exec_pcmpeqb = pcmp (.=.) n8
exec_pcmpeqw = pcmp (.=.) n16
exec_pcmpeqd = pcmp (.=.) n32

exec_pcmpgtb, exec_pcmpgtw, exec_pcmpgtd  :: Binop
exec_pcmpgtb = pcmp (flip bvSlt) n8
exec_pcmpgtw = pcmp (flip bvSlt) n16
exec_pcmpgtd = pcmp (flip bvSlt) n32


-- ** MMX Logical Instructions

exec_pand :: Binop
exec_pand l v = do
  v0 <- get l
  l .= v0 .&. v

exec_pandn :: Binop
exec_pandn l v = do
  v0 <- get l
  l .= complement v0 .&. v

exec_por :: Binop
exec_por l v = do
  v0 <- get l
  l .= v0 .|. v

exec_pxor :: Binop
exec_pxor l v = do
  v0 <- get l
  l .= v0 `bvXor` v


-- ** MMX Shift and Rotate Instructions

-- | PSLLW Shift packed words left logical
-- PSLLD Shift packed doublewords left logical
-- PSLLQ Shift packed quadword left logical

def_psllx :: (1 <= elsz) => String -> NatRepr elsz -> InstructionDef
def_psllx mnem elsz = defBinaryLVpoly mnem $ \l count -> do
  lv <- get l
  let ls  = bvVectorize elsz lv
      -- This is somewhat tedious: we want to make sure that we don't
      -- truncate e.g. 2^31 to 0, so we saturate if the size is over
      -- the number of bits we want to shift.  We can always fit the
      -- width into count bits (assuming we are passed 16, 32, or 64).
      nbits   = bvLit (bv_width count) (natValue elsz)
      countsz = case testNatCases (bv_width count) elsz of
                  NatCaseLT LeqProof -> uext' elsz count
                  NatCaseEQ          -> count
                  NatCaseGT LeqProof -> bvTrunc' elsz count

      ls' = map (\y -> mux (bvUlt count nbits) (bvShl y countsz) (bvLit elsz 0)) ls

  l .= bvUnvectorize (loc_width l) ls'

-- PSRLW Shift packed words right logical
-- PSRLD Shift packed doublewords right logical
-- PSRLQ Shift packed quadword right logical
-- PSRAW Shift packed words right arithmetic
-- PSRAD Shift packed doublewords right arithmetic


-- ** MMX State Management Instructions

-- EMMS Empty MMX state


-- * SSE Instructions
-- ** SSE SIMD Single-Precision Floating-Point Instructions
-- *** SSE Data Transfer Instructions

-- Semantics for SSE movsb instruction
def_movsd :: InstructionDef
def_movsd = defBinary "movsd" $ \_ v1 v2 -> do
  case (v1, v2) of
    -- If source is an XMM register then we will leave high order bits alone.
    (_, F.XMMReg src) -> do
      dest <-
        case v1 of
          F.XMMReg r      -> pure $ subRegister n0 n64 (N.xmmFromFlexdis r)
          F.Mem128 f_addr -> (`MemoryAddr` doubleMemRepr) <$> getBVAddress f_addr
          _ -> fail $ "Unexpected dest argument to movsd"
      vLow <- get (subRegister n0 n64 (N.xmmFromFlexdis src))
      dest .= vLow
    -- If destination is an XMM register and source is memory, then zero out
    -- high order bits.
    (F.XMMReg dest, F.Mem128 src_addr) -> do
      addr <- getBVAddress src_addr
      v' <- get (MemoryAddr addr doubleMemRepr)
      fullRegister (N.xmmFromFlexdis dest) .= uext n128 v'
    _ ->
      fail $ "Unexpected arguments in FlexdisMatcher.movsd: " ++ show v1 ++ ", " ++ show v2

-- Semantics for SSE movss instruction
def_movss :: InstructionDef
def_movss = defBinary "movss" $ \_ v1 v2 -> do
  case (v1, v2) of
    -- If source is an XMM register then we will leave high order bits alone.
    (_, F.XMMReg src) -> do
      dest <-
        case v1 of
          F.XMMReg r      -> pure $ subRegister n0 n32 (N.xmmFromFlexdis r)
          F.Mem128 f_addr -> (`MemoryAddr` floatMemRepr) <$> getBVAddress f_addr
          _ -> fail $ "Unexpected dest argument to movss"
      vLow <- get (subRegister n0 n32 (N.xmmFromFlexdis src))
      dest .= vLow
    -- If destination is an XMM register and source is memory, then zero out
    -- high order bits.
    (F.XMMReg dest, F.Mem128 src_addr) -> do
      addr <- getBVAddress src_addr
      v' <- get (MemoryAddr addr floatMemRepr)
      fullRegister (N.xmmFromFlexdis dest) .= uext n128 v'
    _ ->
      fail $ "Unexpected arguments in FlexdisMatcher.movss: " ++ show v1 ++ ", " ++ show v2

def_pshufb :: InstructionDef
def_pshufb = defBinary "pshufb" $ \_ f_d f_s2 -> do
  case (f_d, f_s2) of
    (F.XMMReg d, F.XMMReg s2) -> do
      let d_loc = fullRegister (N.xmmFromFlexdis d)
      d_val  <- get d_loc
      s2_val <- get (fullRegister (N.xmmFromFlexdis s2))
      (d_loc .=) =<< pshufb SIMD_128 d_val s2_val
    _ -> do
      fail $ "pshufb only supports 2 XMM registers as arguments."

-- MOVAPS Move four aligned packed single-precision floating-point values between XMM registers or between and XMM register and memory
def_movaps :: InstructionDef
def_movaps = defBinaryXMMV "movaps" $ \l v -> l .= v

def_movups :: InstructionDef
def_movups = defBinaryXMMV "movups" $ \l v -> l .= v

-- MOVHPS Move two packed single-precision floating-point values to an from the high quadword of an XMM register and memory

def_movhlps :: InstructionDef
def_movhlps = defBinaryKnown "movhlps" exec
  where exec :: Semantics m => MLocation m (BVType 128) -> Value m (BVType 128) -> m ()
        exec l v = do
          v0 <- get l

          let lit64  = bvLit n128 64
          let high64 = bvLit n128 $ (2^(64::Int) - 1) `shiftL` 64
          let low64  = bvLit n128 $  2^(64::Int) - 1

          let upper = v0 .&. high64
          let lower = (v `bvShr` lit64)  .&. low64
          l .= upper .|. lower

-- MOVLPS Move two packed single-precision floating-point values to an from the low quadword of an XMM register and memory

def_movlhps :: InstructionDef
def_movlhps = defBinaryKnown "movlhps" exec
  where exec :: Semantics m => MLocation m (BVType 128) -> Value m (BVType 128) -> m ()
        exec l v = do
          v0 <- get l
          let lit64 = bvLit n128 64
          let low64 = bvLit n128 (2^(64 :: Int) - 1)
          let upper = (v  .&. low64) `bvShl` lit64
          let lower = v0 .&. low64
          l .= upper .|. lower

-- *** SSE Packed Arithmetic Instructions

-- | ADDPS Add packed single-precision floating-point values
def_addps :: InstructionDef
def_addps = defBinaryXMMV "addps" $ \l v -> do
  fmap_loc l $ \lv -> vectorize2 n32 (fpAdd SingleFloatRepr) lv v

-- ADDSS Add scalar single-precision floating-point values
def_addss :: InstructionDef
def_addss = defBinaryXMMV "addss" $ \r y -> do
  modify_low knownNat r (\x -> fpAdd SingleFloatRepr x y)

-- SUBPS Subtract packed single-precision floating-point values
def_subps :: InstructionDef
def_subps = defBinaryXMMV "subps" $ \l v -> do
  fmap_loc l $ \lv -> vectorize2 n32 (fpSub SingleFloatRepr) lv v

-- SUBSS Subtract scalar single-precision floating-point values
def_subss :: InstructionDef
def_subss = defBinaryXMMV "subss" $ \r y -> do
  modify_low knownNat r (\x -> fpSub SingleFloatRepr x y)

-- | MULPS Multiply packed single-precision floating-point values
def_mulps :: InstructionDef
def_mulps = defBinaryXMMV "mulps" $ \l v -> do
  fmap_loc l $ \lv -> vectorize2 n64 (fpMul DoubleFloatRepr) lv v

-- MULSS Multiply scalar single-precision floating-point values
def_mulss :: InstructionDef
def_mulss = defBinaryXMMV "mulss" $ \r y -> do
  modify_low knownNat r (\x -> fpMul SingleFloatRepr x y)

-- DIVPS Divide packed single-precision floating-point values
-- DIVSS Divide scalar single-precision floating-point values
def_divss :: InstructionDef
def_divss = defBinaryXMMV "divss" $ \r y -> do
  modify_low knownNat r (\x -> fpDiv SingleFloatRepr x y)

-- RCPPS Compute reciprocals of packed single-precision floating-point values
-- RCPSS Compute reciprocal of scalar single-precision floating-point values
-- SQRTPS Compute square roots of packed single-precision floating-point values
-- SQRTSS Compute square root of scalar single-precision floating-point values
-- RSQRTPS Compute reciprocals of square roots of packed single-precision floating-point values
-- RSQRTSS Compute reciprocal of square root of scalar single-precision floating-point values
-- MAXPS Return maximum packed single-precision floating-point values
-- MAXSS Return maximum scalar single-precision floating-point values
-- MINPS Return minimum packed single-precision floating-point values
-- MINSS Return minimum scalar single-precision floating-point values

-- *** SSE Comparison Instructions

-- CMPPS Compare packed single-precision floating-point values
-- CMPSS Compare scalar single-precision floating-point values
-- COMISS Perform ordered comparison of scalar single-precision floating-point values and set flags in EFLAGS register
-- | UCOMISS Perform unordered comparison of scalar single-precision floating-point values and set flags in EFLAGS register
def_ucomiss :: InstructionDef
-- Invalid (if SNaN operands), Denormal.
def_ucomiss = defBinaryXMMV "ucomiss" $ \l v -> do
  v' <- bvTrunc knownNat <$> get l
  let fir = SingleFloatRepr
  let unordered = (isNaN fir v .|. isNaN fir v')
      lt        = fpLt fir v' v
      eq        = fpEq fir v' v

  zf_loc .= (unordered .|. eq)
  pf_loc .= unordered
  cf_loc .= (unordered .|. lt)

  of_loc .= false
  af_loc .= false
  sf_loc .= false

-- *** SSE Logical Instructions

exec_andpx :: (Semantics m, 1 <= elsz) => NatRepr elsz -> MLocation m XMMType -> Value m XMMType -> m ()
exec_andpx elsz l v = fmap_loc l $ \lv -> vectorize2 elsz (.&.) lv v

-- | ANDPS Perform bitwise logical AND of packed single-precision floating-point values
def_andps :: InstructionDef
def_andps = defBinaryKnown "andps" $ exec_andpx n32

-- ANDNPS Perform bitwise logical AND NOT of packed single-precision floating-point values

exec_orpx :: (Semantics m, 1 <= elsz) => NatRepr elsz -> MLocation m XMMType -> Value m XMMType -> m ()
exec_orpx elsz l v = fmap_loc l $ \lv -> vectorize2 elsz (.|.) lv v

-- | ORPS Perform bitwise logical OR of packed single-precision floating-point values
def_orps :: InstructionDef
def_orps = defBinaryKnown "orps" $ exec_orpx n32

-- XORPS Perform bitwise logical XOR of packed single-precision floating-point values

def_xorps :: InstructionDef
def_xorps =
  defBinary "xorps" $ \_ loc val -> do
    l <- getBVLocation loc n128
    v <- readXMMValue val
    modify (`bvXor` v) l

-- *** SSE Shuffle and Unpack Instructions

-- SHUFPS Shuffles values in packed single-precision floating-point operands
-- UNPCKHPS Unpacks and interleaves the two high-order values from two single-precision floating-point operands
-- | UNPCKLPS Unpacks and interleaves the two low-order values from two single-precision floating-point operands

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x, y]) xs ys)

def_unpcklps :: InstructionDef
def_unpcklps = defBinaryKnown "unpcklps" exec
  where exec :: Semantics m => MLocation m XMMType -> Value m XMMType -> m ()
        exec l v = fmap_loc l $ \lv -> do
          let lsd = drop 2 $ bvVectorize n32 lv
              lss = drop 2 $ bvVectorize n32 v
          bvUnvectorize (loc_width l) (interleave lss lsd)

-- *** SSE Conversion Instructions

-- CVTPI2PS Convert packed doubleword integers to packed single-precision floating-point values
-- CVTSI2SS Convert doubleword integer to scalar single-precision floating-point value
def_cvtsi2ss :: InstructionDef
def_cvtsi2ss =
  defBinary "cvtsi2ss" $ \_ loc val -> do
    xmm <- getBVLocation loc n128
    SomeBV v <- getSomeBVValue val
    set_low xmm (fpFromBV SingleFloatRepr v)

-- CVTPS2PI Convert packed single-precision floating-point values to packed doubleword integers
-- CVTTPS2PI Convert with truncation packed single-precision floating-point values to packed doubleword integers
-- CVTSS2SI Convert a scalar single-precision floating-point value to a doubleword integer

def_cvtss2si :: InstructionDef
def_cvtss2si =
  defBinary "cvtss2si" $ \_ loc val -> do
    SomeBV l  <- getSomeBVLocation loc
    v <- truncateBVValue knownNat =<< getSomeBVValue val
    l .= truncFPToSignedBV (loc_width l) DoubleFloatRepr v

-- | CVTTSS2SI Convert with truncation a scalar single-precision floating-point value to a scalar doubleword integer
def_cvttss2si :: InstructionDef
-- Invalid, Precision.  Returns 80000000 if exception is masked
def_cvttss2si =
  defBinary "cvtss2si" $ \_ loc val -> do
    SomeBV l  <- getSomeBVLocation loc
    v <- truncateBVValue knownNat =<< getSomeBVValue val
    l .= truncFPToSignedBV (loc_width l) SingleFloatRepr v

-- ** SSE MXCSR State Management Instructions

-- LDMXCSR Load MXCSR register
-- STMXCSR Save MXCSR register state

-- ** SSE 64-Bit SIMD Integer Instructions

-- replace pairs with the left operand if `op` is true (e.g., bvUlt for min)
pselect :: (IsLocationBV m n, 1 <= o)
        => (Value m (BVType o) -> Value m (BVType o) -> Value m BoolType)
        -> NatRepr o
        -> MLocation m (BVType n) -> Value m (BVType n) -> m ()
pselect op sz l v = do
  v0 <- get l
  l .= vectorize2 sz chkPair v0 v
  where chkPair d s = mux (d `op` s) d s

-- PAVGB Compute average of packed unsigned byte integers
-- PAVGW Compute average of packed unsigned word integers
-- PEXTRW Extract word

-- | PINSRW Insert word
exec_pinsrw :: Semantics m => MLocation m XMMType -> Value m (BVType 16) -> Int8 -> m ()
exec_pinsrw l v off = do
  lv <- get l
  -- FIXME: is this the right way around?
  let ls = bvVectorize n16 lv
      (lower, _ : upper) = splitAt (fromIntegral off - 1) ls
      ls' = lower ++ [v] ++ upper
  l .= bvUnvectorize knownNat ls'

def_pinsrw :: InstructionDef
def_pinsrw = defTernary "pinsrw" $ \_ loc val imm -> do
  l  <- getBVLocation loc knownNat
  v  <- truncateBVValue knownNat =<< getSomeBVValue val
  case imm of
    F.ByteImm off -> exec_pinsrw l v off
    _ -> fail "Bad offset to pinsrw"

-- PMAXUB Maximum of packed unsigned byte integers
-- PMAXSW Maximum of packed signed word integers
-- PMINUB Minimum of packed unsigned byte integers
-- PMINSW Minimum of packed signed word integers
def_pmaxu :: (1 <= w) => String -> NatRepr w -> InstructionDef
def_pmaxu mnem w = defBinaryLV mnem $ pselect (flip bvUlt) w

def_pmaxs :: (1 <= w) => String -> NatRepr w -> InstructionDef
def_pmaxs mnem w = defBinaryLV mnem $ pselect (flip bvSlt) w

def_pminu :: (1 <= w) => String -> NatRepr w -> InstructionDef
def_pminu mnem w = defBinaryLV mnem $ pselect bvUlt w

def_pmins :: (1 <= w) => String -> NatRepr w -> InstructionDef
def_pmins mnem w = defBinaryLV mnem $ pselect bvSlt w

exec_pmovmskb :: forall m n n'. (IsLocationBV m n)
              => MLocation m (BVType n)
              -> Value m (BVType n')
              -> m ()
exec_pmovmskb l v
  | Just Refl <- testEquality (bv_width v) n64 = do
      l .= uext (loc_width l) (mkMask n8 v)
  | Just LeqProof <- testLeq n32 (loc_width l)
  , Just Refl <- testEquality (bv_width v) n128 = do
      let prf = withLeqProof (leqTrans (LeqProof :: LeqProof 16 32)
                                       (LeqProof :: LeqProof 32 n))
      l .= prf (uext (loc_width l) (mkMask n16 v))
  | otherwise = fail "pmovmskb: Unknown bit width"
  where mkMask sz src = bvUnvectorize sz $ map msb $ bvVectorize n8 src

-- PMULHUW Multiply packed unsigned integers and store high result
-- PSADBW Compute sum of absolute differences
-- PSHUFW Shuffle packed integer word in MMX register

-- ** SSE Cacheability Control, Prefetch, and Instruction Ordering Instructions

-- MASKMOVQ Non-temporal store of selected bytes from an MMX register into memory
-- MOVNTQ  Non-temporal store of quadword from an MMX register into memory
-- MOVNTPS Non-temporal store of four packed single-precision floating-point
--   values from an XMM register into memory
-- PREFETCHh Load 32 or more of bytes from memory to a selected level of the
--   processor's cache hierarchy
-- SFENCE Serializes store operations

-- * SSE2 Instructions
-- ** SSE2 Packed and Scalar Double-Precision Floating-Point Instructions
-- *** SSE2 Data Movement Instructions

-- | MOVAPD Move two aligned packed double-precision floating-point values
-- between XMM registers or between and XMM register and memory
def_movapd :: InstructionDef
def_movapd = defBinaryXMMV "movapd" $ \l v -> l .= v

-- | MOVUPD Move two unaligned packed double-precision floating-point values
--   between XMM registers or between and XMM register and memory
def_movupd :: InstructionDef
def_movupd = defBinaryXMMV "movupd" $ \l v -> l .= v

exec_movhpd, exec_movlpd :: forall m n n'. (IsLocationBV m n, 1 <= n')
                         => MLocation m (BVType n)
                         -> Value m (BVType n')
                         -> m ()
exec_movhpd l v = do
  v0 <- get l
  let dstPieces = bvVectorize n64 v0
      srcPieces = bvVectorize n64 v
      rPieces = [head srcPieces] ++ (drop 1 dstPieces)
  l .= bvUnvectorize (loc_width l) rPieces
exec_movlpd l v = do
  v0 <- get l
  let dstPieces = bvVectorize n64 v0
      srcPieces = bvVectorize n64 v
      rPieces =  (init dstPieces) ++ [last srcPieces]
  l .= bvUnvectorize (loc_width l) rPieces

-- MOVMSKPD Extract sign mask from two packed double-precision floating-point values

-- *** SSE2 Packed Arithmetic Instructions

-- ADDPD Add packed double-precision floating-point values
-- | ADDSD Add scalar double precision floating-point values
def_addsd :: InstructionDef
-- FIXME: Overflow, Underflow, Invalid, Precision, Denormal.
def_addsd = defBinaryXMMV "addsd" $ \r y ->
  modify_low knownNat r (\x -> fpAdd DoubleFloatRepr x y)

-- SUBPD Subtract scalar double-precision floating-point values

-- | SUBSD Subtract scalar double-precision floating-point values
def_subsd :: InstructionDef
def_subsd = defBinaryXMMV "subsd" $ \r y ->
  modify_low knownNat r (\x -> fpSub DoubleFloatRepr x y)

-- MULPD Multiply packed double-precision floating-point values

-- | MULSD Multiply scalar double-precision floating-point values
def_mulsd :: InstructionDef
def_mulsd = defBinaryXMMV "mulsd" $ \r y -> do
  modify_low knownNat r (\x -> fpMul DoubleFloatRepr x y)

-- DIVPD Divide packed double-precision floating-point values

-- | DIVSD Divide scalar double-precision floating-point values
def_divsd :: InstructionDef
def_divsd = defBinaryXMMV "divsd" $ \r y ->
  modify_low knownNat r (\x -> fpDiv DoubleFloatRepr x y)

-- SQRTPD Compute packed square roots of packed double-precision floating-point values
-- SQRTSD Compute scalar square root of scalar double-precision floating-point values
-- MAXPD Return maximum packed double-precision floating-point values
-- MAXSD Return maximum scalar double-precision floating-point values
-- MINPD Return minimum packed double-precision floating-point values
-- MINSD Return minimum scalar double-precision floating-point values

-- *** SSE2 Logical Instructions

-- | ANDPD  Perform bitwise logical AND of packed double-precision floating-point values
def_andpd :: InstructionDef
def_andpd = defBinaryKnown "andpd" $ exec_andpx n64

-- ANDNPD Perform bitwise logical AND NOT of packed double-precision floating-point values
-- | ORPD   Perform bitwise logical OR of packed double-precision floating-point values
def_orpd :: InstructionDef
def_orpd = defBinaryKnown "orpd" $ exec_orpx n64

-- XORPD  Perform bitwise logical XOR of packed double-precision floating-point values

def_xorpd :: InstructionDef
def_xorpd =
  defBinary "xorpd" $ \_ loc val -> do
    l <- getBVLocation loc n128
    v <- readXMMValue val
    modify (`bvXor` v) l

-- *** SSE2 Compare Instructions

-- CMPPD Compare packed double-precision floating-point values
-- | CMPSD Compare scalar double-precision floating-point values
def_cmpsd :: InstructionDef
def_cmpsd =
  defTernary "cmpsd" $ \_ loc val imm -> do
    l  <- getBVLocation loc n128
    v  <- truncateBVValue n64 =<< getSomeBVValue val
    f <- case imm of
           F.ByteImm opcode ->
             case opcode of
               0 -> return $ fpEq DoubleFloatRepr
               1 -> return $ fpLt DoubleFloatRepr
               2 -> fail "cmpsd: CMPLESD case unimplemented" -- FIXME
               3 -> fail "cmpsd: CMPUNORDSD case unimplemented" -- FIXME
               4 -> fail "cmpsd: CMPNEWSD case unimplemented" -- FIXME
               5 -> return $ \x y -> bvNeg (fpLt DoubleFloatRepr x y)
               6 -> fail "cmpsd: CMPNLESD case unimplemented" -- FIXME
               7 -> fail "cmpsd: CMPORDSD case unimplemented" -- FIXME
               _ -> fail ("cmpsd: unexpected opcode " ++ show opcode)
           _ -> fail "Impossible argument in cmpsd"
    modify_low knownNat l $ \lv -> do
      let res = f lv v
          allOnes  = bvLit knownNat (-1)
          allZeros = bvLit knownNat 0
      mux res allOnes allZeros

-- COMISD Perform ordered comparison of scalar double-precision floating-point values and set flags in EFLAGS register

-- | UCOMISD Perform unordered comparison of scalar double-precision
-- floating-point values and set flags in EFLAGS register.
def_ucomisd :: InstructionDef
-- Invalid (if SNaN operands), Denormal.
def_ucomisd = defBinaryXMMV "ucomisd" $ \l v -> do
  let fir = DoubleFloatRepr
  v' <- bvTrunc knownNat <$> get l
  let unordered = (isNaN fir v .|. isNaN fir v')
      lt        = fpLt fir v' v
      eq        = fpEq fir v' v

  zf_loc .= (unordered .|. eq)
  pf_loc .= unordered
  cf_loc .= (unordered .|. lt)

  of_loc .= false
  af_loc .= false
  sf_loc .= false

-- *** SSE2 Shuffle and Unpack Instructions

-- CMPPD Compare packed double-precision floating-point values
-- CMPSD Compare scalar double-precision floating-point values
-- COMISD Perform ordered comparison of scalar double-precision floating-point values and set flags in EFLAGS register
-- UCOMISD Perform unordered comparison of scalar double-precision floating-point values and set flags in EFLAGS register.

-- *** SSE2 Conversion Instructions

-- CVTPD2PI  Convert packed double-precision floating-point values to packed doubleword integers.
-- CVTTPD2PI Convert with truncation packed double-precision floating-point values to packed doubleword integers
-- CVTPI2PD  Convert packed doubleword integers to packed double-precision floating-point values
-- CVTPD2DQ  Convert packed double-precision floating-point values to packed doubleword integers
-- CVTTPD2DQ Convert with truncation packed double-precision floating-point values to packed doubleword integers
-- CVTDQ2PD  Convert packed doubleword integers to packed double-precision floating-point values
-- CVTPS2PD  Convert packed single-precision floating-point values to packed double-precision floating- point values
-- CVTPD2PS  Convert packed double-precision floating-point values to packed single-precision floating- point values

-- | CVTSS2SD  Convert scalar single-precision floating-point values to
-- scalar double-precision floating-point values
def_cvtss2sd :: InstructionDef
def_cvtss2sd = defBinaryXMMV "cvtss2sd" $ \l v ->
  set_low l (fpCvt SingleFloatRepr DoubleFloatRepr v)

-- | CVTSD2SS  Convert scalar double-precision floating-point values to scalar single-precision floating-point values
def_cvtsd2ss :: InstructionDef
def_cvtsd2ss = defBinaryXMMV "cvtss2ss" $ \l v ->
  set_low l (fpCvt DoubleFloatRepr SingleFloatRepr v)

-- CVTSD2SI  Convert scalar double-precision floating-point values to a doubleword integer

-- | CVTTSD2SI Convert with truncation scalar double-precision floating-point values to scalar doubleword integers
def_cvttsd2si :: InstructionDef
-- Invalid, Precision.  Returns 80000000 if exception is masked
def_cvttsd2si =
  defBinary "cvttsd2si" $ \_ loc val -> do
    SomeBV l  <- getSomeBVLocation loc
    v <- truncateBVValue knownNat =<< getSomeBVValue val
    l .= truncFPToSignedBV (loc_width l) DoubleFloatRepr v

-- | CVTSI2SD  Convert doubleword integer to scalar double-precision floating-point value
def_cvtsi2sd :: InstructionDef
def_cvtsi2sd =
  defBinary "cvtsi2sd" $ \_ loc val -> do
    l <- getBVLocation loc n128
    SomeBV v <- getSomeBVValue val
    set_low l (fpFromBV DoubleFloatRepr v)

-- ** SSE2 Packed Single-Precision Floating-Point Instructions

-- CVTDQ2PS  Convert packed doubleword integers to packed single-precision floating-point values
-- CVTPS2DQ  Convert packed single-precision floating-point values to packed doubleword integers
-- CVTTPS2DQ Convert with truncation packed single-precision floating-point values to packed doubleword integers

-- ** SSE2 128-Bit SIMD Integer Instructions

-- | MOVDQA Move aligned double quadword.

-- FIXME: exception on unaligned loads
def_movdqa :: InstructionDef
def_movdqa = defBinaryXMMV "movdqa" $ \l v -> l .= v

-- | MOVDQU Move unaligned double quadword

-- FIXME: no exception on unaligned loads
def_movdqu :: InstructionDef
def_movdqu = defBinaryXMMV "movdqu" $ \l v -> l .= v

-- MOVQ2DQ Move quadword integer from MMX to XMM registers
-- MOVDQ2Q Move quadword integer from XMM to MMX registers
-- PMULUDQ Multiply packed unsigned doubleword integers
-- | PADDQ Add packed quadword integers
-- FIXME: this can also take 64 bit values?
exec_paddq :: Binop
exec_paddq l v = do
  v0 <- get l
  l .= vectorize2 n64 bvAdd v0 v

-- PSUBQ Subtract packed quadword integers
-- PSHUFLW Shuffle packed low words
-- PSHUFHW Shuffle packed high words

exec_pshufd :: forall m n k. (IsLocationBV m n, 1 <= k)
            => MLocation m (BVType n)
            -> Value m (BVType n)
            -> Value m (BVType k)
            -> m ()
exec_pshufd l v imm
  | Just Refl <- testEquality (bv_width imm) n8 = do
      let order = bvVectorize (addNat n1 n1) imm
          dstPieces = concatMap (\src128 -> map (getPiece src128) order) $ bvVectorize n128 v
      l .= bvUnvectorize (loc_width l) dstPieces
  | otherwise = fail "pshufd: Unknown bit width"
  where shiftAmt :: Value m (BVType 2) -> Value m (BVType 128)
        shiftAmt pieceID = bvMul (uext n128 pieceID) $ bvLit n128 32

        getPiece :: Value m (BVType 128) -> Value m (BVType 2) -> Value m (BVType 32)
        getPiece src pieceID = bvTrunc n32 $ src `bvShr` (shiftAmt pieceID)

exec_pslldq :: (IsLocationBV m n, 1 <= n', n' <= n)
            => MLocation m (BVType n) -> Value m (BVType n') -> m ()
exec_pslldq l v = do
  v0 <- get l
  -- temp is 16 if v is greater than 15, otherwise v
  let not15 = complement $ bvLit (bv_width v) 15
      temp = mux (is_zero $ not15 .&. v)
                 (uext (bv_width v0) v)
                 (bvLit (bv_width v0) 16)
  l .= v0 `bvShl` (temp .* bvLit (bv_width v0) 8)

-- PSRLDQ Shift double quadword right logical
-- PUNPCKHQDQ Unpack high quadwords
-- PUNPCKLQDQ Unpack low quadwords

-- ** SSE2 Cacheability Control and Ordering Instructions


-- CLFLUSH Flushes and invalidates a memory operand and its associated cache line from all levels of the processor’s cache hierarchy
-- LFENCE Serializes load operations
-- MFENCE Serializes load and store operations
-- PAUSE      Improves the performance of “spin-wait loops”
-- MASKMOVDQU Non-temporal store of selected bytes from an XMM register into memory
-- MOVNTPD    Non-temporal store of two packed double-precision floating-point values from an XMM register into memory
-- MOVNTDQ    Non-temporal store of double quadword from an XMM register into memory
-- MOVNTI     Non-temporal store of a doubleword from a general-purpose register into memory

-- * SSE3 Instructions
-- ** SSE3 x87-FP Integer Conversion Instruction

-- FISTTP Behaves like the FISTP instruction but uses truncation, irrespective of the rounding mode specified in the floating-point control word (FCW)

-- ** SSE3 Specialized 128-bit Unaligned Data Load Instruction

def_lddqu :: InstructionDef
def_lddqu = defBinary "lddqu"  $ \_ loc val -> do
  l <- getBVLocation loc n128
  v <- case val of
    F.VoidMem a -> readBVAddress a xmmMemRepr
    _ -> fail "readVoidMem given bad address."
  l .= v

-- ** SSE3 SIMD Floating-Point Packed ADD/SUB Instructions

-- ADDSUBPS Performs single-precision addition on the second and fourth pairs of 32-bit data elements within the operands; single-precision subtraction on the first and third pairs
-- ADDSUBPD Performs double-precision addition on the second pair of quadwords, and double-precision subtraction on the first pair

-- ** SSE3 SIMD Floating-Point Horizontal ADD/SUB Instructions

-- HADDPS Performs a single-precision addition on contiguous data elements. The first data element of the result is obtained by adding the first and second elements of the first operand; the second element by adding the third and fourth elements of the first operand; the third by adding the first and second elements of the second operand; and the fourth by adding the third and fourth elements of the second operand.
-- HSUBPS Performs a single-precision subtraction on contiguous data elements. The first data element of the result is obtained by subtracting the second element of the first operand from the first element of the first operand; the second element by subtracting the fourth element of the first operand from the third element of the first operand; the third by subtracting the second element of the second operand from the first element of the second operand; and the fourth by subtracting the fourth element of the second operand from the third element of the second operand.
-- HADDPD Performs a double-precision addition on contiguous data elements. The first data element of the result is obtained by adding the first and second elements of the first operand; the second element by adding the first and second elements of the second operand.
-- HSUBPD Performs a double-precision subtraction on contiguous data elements. The first data element of the result is obtained by subtracting the second element of the first operand from the first element of the first operand; the second element by subtracting the second element of the second operand from the first element of the second operand.


-- ** SSE3 SIMD Floating-Point LOAD/MOVE/DUPLICATE Instructions

-- MOVSHDUP Loads/moves 128 bits; duplicating the second and fourth 32-bit data elements
-- MOVSLDUP Loads/moves 128 bits; duplicating the first and third 32-bit data elements
-- MOVDDUP Loads/moves 64 bits (bits[63:0] if the source is a register) and returns the same 64 bits in both the lower and upper halves of the 128-bit result register; duplicates the 64 bits from the source

-- ** SSE3 Agent Synchronization Instructions

-- MONITOR Sets up an address range used to monitor write-back stores
-- MWAIT Enables a logical processor to enter into an optimized state while waiting for a write-back store to the address range set up by the MONITOR instruction


-- * Supplemental Streaming SIMD Extensions 3 (SSSE3) Instructions
-- ** Horizontal Addition/Subtraction

-- PHADDW Adds two adjacent, signed 16-bit integers horizontally from the source and destination operands and packs the signed 16-bit results to the destination operand.
-- PHADDSW Adds two adjacent, signed 16-bit integers horizontally from the source and destination operands and packs the signed, saturated 16-bit results to the destination operand.
-- PHADDD Adds two adjacent, signed 32-bit integers horizontally from the source and destination operands and packs the signed 32-bit results to the destination operand.
-- PHSUBW Performs horizontal subtraction on each adjacent pair of 16-bit signed integers by subtracting the most significant word from the least significant word of each pair in the source and destination operands. The signed 16-bit results are packed and written to the destination operand.
-- PHSUBSW Performs horizontal subtraction on each adjacent pair of 16-bit signed integers by subtracting the most significant word from the least significant word of each pair in the source and destination operands. The signed, saturated 16-bit results are packed and written to the destination operand.
-- PHSUBD Performs horizontal subtraction on each adjacent pair of 32-bit signed integers by subtracting the most significant doubleword from the least significant double word of each pair in the source and destination operands. The signed 32-bit results are packed and written to the destination operand.

-- ** Packed Absolute Values

-- PABSB Computes the absolute value of each signed byte data element.
-- PABSW Computes the absolute value of each signed 16-bit data element.
-- PABSD Computes the absolute value of each signed 32-bit data element.

-- ** Multiply and Add Packed Signed and Unsigned Bytes

-- PMADDUBSW Multiplies each unsigned byte value with the corresponding signed byte value to produce an intermediate, 16-bit signed integer. Each adjacent pair of 16-bit signed values are added horizontally. The signed, saturated 16-bit results are packed to the destination operand.

-- ** Packed Multiply High with Round and Scale

-- PMULHRSW Multiplies vertically each signed 16-bit integer from the destination operand with the corresponding signed 16-bit integer of the source operand, producing intermediate, signed 32-bit integers. Each intermediate 32-bit integer is truncated to the 18 most significant bits. Rounding is always performed by adding 1 to the least significant bit of the 18-bit intermediate result. The final result is obtained by selecting the 16 bits immediately to the right of the most significant bit of each 18-bit intermediate result and packed to the destination operand.

-- ** Packed Shuffle Bytes

-- PSHUFB Permutes each byte in place, according to a shuffle control mask. The least significant three or four bits of each shuffle control byte of the control mask form the shuffle index. The shuffle mask is unaffected. If the most significant bit (bit 7) of a shuffle control byte is set, the constant zero is written in the result byte.


-- ** Packed Sign

-- PSIGNB/W/D Negates each signed integer element of the destination operand if the sign of the corresponding data element in the source operand is less than zero.

-- ** Packed Align Right

exec_palignr :: forall m n k. (IsLocationBV m n, 1 <= k, k <= n)
             => MLocation m (BVType n)
             -> Value m (BVType n)
             -> Value m (BVType k)
             -> m ()
exec_palignr l v imm = do
  v0 <- get l

  -- 1 <= n+n, given 1 <= n
  withLeqProof (dblPosIsPos (LeqProof :: LeqProof 1 n)) $ do
  -- k <= (n+n), given k <= n and n <= n+n
  withLeqProof (leqTrans k_leq_n (leqAdd (leqRefl n) n)) $ do

  -- imm is # of bytes to shift, so multiply by 8 for bits to shift
  let n_plus_n = addNat (bv_width v) (bv_width v)
      shiftAmt = bvMul (uext n_plus_n imm) $ bvLit n_plus_n 8

  let (_, lower) = bvSplit $ (v0 `bvCat` v) `bvShr` shiftAmt
  l .= lower

  where n :: Proxy n
        n = Proxy
        k_leq_n :: LeqProof k n
        k_leq_n = LeqProof

------------------------------------------------------------------------
-- Instruction list


all_instructions :: [InstructionDef]
all_instructions =
  [ def_lea
  , def_call
  , def_jmp
  , def_ret
  , def_cwd
  , def_cdq
  , def_cqo
  , def_movsx
  , def_movsxd
  , def_movzx
  , def_xchg
  , def_cmps
  , def_movs
  , def_stos
  , def_scas

    -- fixed size instructions.  We truncate in the case of
    -- an xmm register, for example
  , def_addsd
  , def_subsd
  , def_movsd
  , def_movapd
  , def_movaps
  , def_movups
  , def_movupd
  , def_movdqa
  , def_movdqu
  , def_movss
  , def_mulsd
  , def_divsd
  , def_psllx "psllw" n16
  , def_psllx "pslld" n32
  , def_psllx "psllq" n64
  , def_ucomisd
  , def_xorpd
  , def_xorps

  , def_cvtss2si
  , def_cvtsi2ss
  , def_cvtsd2ss
  , def_cvtsi2sd
  , def_cvtss2sd
  , def_cvttsd2si
  , def_cvttss2si
  , def_pinsrw
  , def_cmpsd
  , def_andpd
  , def_orpd
  , def_unpcklps

    -- regular instructions
  , defBinaryLV "add" exec_add
  , defBinaryLV "adc" exec_adc
  , defBinaryLV "and" exec_and
  , defBinaryLVge "bt"  exec_bt
  , defBinaryLVge "btc" exec_btc
  , defBinaryLVge "btr" exec_btr
  , defBinaryLVge "bts" exec_bts
  , defBinaryLV "bsf" exec_bsf
  , defBinaryLV "bsr" exec_bsr
  , defUnaryLoc "bswap" $ exec_bswap
  , defNullary  "cbw"   $ exec_cbw
  , defNullary  "cwde"  $ exec_cwde
  , defNullary  "cdqe"  $ exec_cdqe
  , defNullary  "clc"   $ exec_clc
  , defNullary  "cld"   $ exec_cld
  , defBinaryLV "cmp" exec_cmp
  , defUnaryLoc  "dec"  $ exec_dec
  , def_div
  , def_hlt
  , def_idiv
  , def_imul

  , defUnaryLoc "inc"   $ exec_inc
  , defNullary  "leave" $ exec_leave
  , defBinaryLV "mov"   $ exec_mov
  , defUnaryV   "mul"   $ exec_mul
  , defUnaryLoc "neg"   $ exec_neg
  , defNullary  "nop"   $ return ()
  , defUnaryLoc "not"   $ exec_not
  , defBinaryLV "or" exec_or
  , defNullary "pause" $ return ()
  , def_pop

  , defBinaryLV   "cmpxchg"   exec_cmpxchg
  , defUnaryKnown "cmpxchg8b" exec_cmpxchg8b
  , def_push
  , defBinaryLVge "rol"  exec_rol
  , defBinaryLVge "ror"  exec_ror
  , defNullary    "sahf" exec_sahf
  , defBinaryLV   "sbb"  exec_sbb
  , defBinaryLVge "sar"  exec_sar
  , defBinaryLVge "shl"  exec_shl
  , defBinaryLVge "shr"  exec_shr
  , defNullary    "std" $ df_loc .= true
  , defBinaryLV   "sub" exec_sub
  , defBinaryLV   "test" exec_test
  , def_xadd
  , defBinaryLV "xor" exec_xor

  , defNullary "ud2"     $ exception false true UndefinedInstructionError

    -- Primitive instructions
  , defNullary "syscall" $ primitive Syscall
  , defNullary "cpuid"   $ primitive CPUID
  , defNullary "rdtsc"   $ primitive RDTSC
  , defNullary "xgetbv"  $ primitive XGetBV

    -- MMX instructions
  , defBinaryLVpoly "movd" exec_movd
  , defBinaryLVpoly "movq" exec_movq
  , defBinaryLV "punpckhbw"  $ exec_punpckhbw
  , defBinaryLV "punpckhwd"  $ exec_punpckhwd
  , defBinaryLV "punpckhdq"  $ exec_punpckhdq
  , defBinaryLV "punpckhqdq" $ exec_punpckhqdq
  , defBinaryLV "punpcklbw"  $ exec_punpcklbw
  , defBinaryLV "punpcklwd"  $ exec_punpcklwd
  , defBinaryLV "punpckldq"  $ exec_punpckldq
  , defBinaryLV "punpcklqdq" $ exec_punpcklqdq
  , defBinaryLV "paddb"      $ exec_paddb
  , defBinaryLV "paddw"      $ exec_paddw
  , defBinaryLV "paddq"      $ exec_paddq
  , defBinaryLV "paddd"      $ exec_paddd
  , defBinaryLV "psubb"      $ exec_psubb
  , defBinaryLV "psubw"      $ exec_psubw
  , defBinaryLV "psubd"      $ exec_psubd
  , defBinaryLV "pcmpeqb"    $ exec_pcmpeqb
  , defBinaryLV "pcmpeqw"    $ exec_pcmpeqw
  , defBinaryLV "pcmpeqd"    $ exec_pcmpeqd
  , defBinaryLV "pcmpgtb"    $ exec_pcmpgtb
  , defBinaryLV "pcmpgtw"    $ exec_pcmpgtw
  , defBinaryLV "pcmpgtd"    $ exec_pcmpgtd
  , defBinaryLV "pand"       $ exec_pand
  , defBinaryLV "pandn"      $ exec_pandn
  , defBinaryLV "por"        $ exec_por
  , defBinaryLV "pxor"       $ exec_pxor

    -- SSE instructions
  , def_movhlps
  , def_movlhps
    -- SSE Packed
  , def_addps
  , def_addss
  , def_subps
  , def_subss
  , def_mulps
  , def_mulss
  , def_divss
    -- SSE Comparison
  , def_ucomiss
    -- SSE Logical
  , def_andps
  , def_orps

  , def_pmaxu "pmaxub"  n8
  , def_pmaxu "pmaxuw" n16
  , def_pmaxu "pmaxud" n32

  , def_pmaxs "pmaxsb"  n8
  , def_pmaxs "pmaxsw" n16
  , def_pmaxs "pmaxsd" n32

  , def_pminu "pminub"  n8
  , def_pminu "pminuw" n16
  , def_pminu "pminud" n32

  , def_pmins "pminsb"  n8
  , def_pmins "pminsw" n16
  , def_pmins "pminsd" n32

  , defBinaryLVpoly "pmovmskb" exec_pmovmskb
  , defBinaryLVpoly "movhpd"   exec_movhpd
  , defBinaryLVpoly "movlpd"   exec_movlpd
  , def_pshufb

  , defTernaryLVV  "pshufd" exec_pshufd
  , defBinaryLVge "pslldq" exec_pslldq
  , def_lddqu
  , defTernaryLVV "palignr" exec_palignr
    -- X87 FP instructions
  , defFPBinaryImplicit "fadd"   $ exec_fadd
  , defUnaryFPV      "fld"   $ exec_fld
  , defFPBinaryImplicit "fmul"   $ exec_fmul
  , def_fnstcw -- stores to bv memory (i.e., not FP)
  , defUnaryFPL     "fst"    $ exec_fst
  , defUnaryFPL     "fstp"   $ exec_fstp
  , defFPBinaryImplicit "fsub"   $ exec_fsub
  , defFPBinaryImplicit "fsubp"  $ exec_fsubp
  , defFPBinaryImplicit "fsubr"  $ exec_fsubr
  , defFPBinaryImplicit "fsubrp" $ exec_fsubrp
  ]
  ++ def_cmov_list
  ++ def_jcc_list
  ++ def_set_list
