------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.FlexdisMatcher
-- Description      : Pattern matches against a Flexdis86 InstructionInstance.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Simon Winwood <sjw@galois.com>
-- Stability        : provisional
--
-- This contains a function "execInstruction" that steps a single Flexdis86
-- instruction.
------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Reopt.Semantics.FlexdisMatcher
  ( execInstruction
  , semanticsMap
  ) where

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Parameterized.NatRepr
import           Data.Word
import           Debug.Trace
import qualified Flexdis86 as F
import           GHC.TypeLits (KnownNat)
import           Numeric (showHex)

import qualified Reopt.Machine.StateNames as N
import           Reopt.Semantics.Monad
import           Reopt.Semantics.Semantics

data SomeBV v where
  SomeBV :: SupportedBVWidth n => v (BVType n) -> SomeBV v

-- | Get a value with the given width, sign extending as necessary.
getSignExtendedValue :: forall m w
                     .  (FullSemantics m, 1 <= w)
                     => F.Value
                     -> NatRepr w
                     -> m (Value m (BVType w))
getSignExtendedValue v out_w =
  case v of
    -- If an instruction can take a VoidMem, it needs to get it explicitly
    F.VoidMem _ar       -> fail "VoidMem"
    F.Mem8  ar           -> mk . mkBVAddr n8   =<< getBVAddress ar
    F.Mem16 ar           -> mk . mkBVAddr n16  =<< getBVAddress ar
    F.Mem32 ar           -> mk . mkBVAddr n32  =<< getBVAddress ar
    F.Mem64 ar           -> mk . mkBVAddr n64  =<< getBVAddress ar
    F.Mem128 ar          -> mk . mkBVAddr n128 =<< getBVAddress ar

    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> mk (reg_low8  $ N.gpFromFlexdis r64)
      | Just r64 <- F.is_high_reg r -> mk (reg_high8 $ N.gpFromFlexdis r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r                    -> mk (reg_low16 (N.gpFromFlexdis $ F.reg16_reg r))
    F.DWordReg r                    -> mk (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
    F.QWordReg r                    -> mk (fullRegister $ N.gpFromFlexdis r)    
    F.XMMReg r                      -> mk (fullRegister $ N.xmmFromFlexdis r)
    
    F.ByteImm  i                    -> return $! bvLit out_w i
    F.WordImm  i                    -> return $! bvLit out_w i
    F.DWordImm i                    -> return $! bvLit out_w i
    F.QWordImm i                    -> return $! bvLit out_w i
    
    _ -> fail $ "getSignExtendedValue given unexpected width: " ++ show v
  where
    -- FIXME: what happens with signs etc?
    mk :: Location (Value m (BVType 64)) (BVType u)
       -> m (Value m (BVType w))
    mk l
      | Just LeqProof <- testLeq n1 (loc_width l)
      , Just LeqProof <- testLeq (loc_width l) out_w =
        sext out_w <$> get l
      | otherwise =
        fail $ "getSignExtendedValue given bad value."

-- | Extracts the value, truncating as required
getSomeBVValue :: FullSemantics m => F.Value -> m (SomeBV (Value m))
getSomeBVValue v =
  case v of
    F.ControlReg cr     -> mk (fullRegister $ N.controlFromFlexdis cr)
    F.DebugReg dr       -> mk (fullRegister $ N.debugFromFlexdis dr)
    F.MMXReg mmx        -> mk (x87reg_mmx $ N.mmxFromFlexdis mmx)
    F.XMMReg xmm        -> mk (fullRegister $ N.xmmFromFlexdis xmm)
    F.SegmentValue s    -> mk (fullRegister $ N.segmentFromFlexdis s)
    F.X87Register n     -> mk (X87StackRegister n)
    F.FarPointer _      -> fail "FarPointer"
    -- If an instruction can take a VoidMem, it needs to get it explicitly
    F.VoidMem _ar       -> fail "VoidMem"
    F.Mem8  ar           -> getBVAddress ar >>= mk . mkBVAddr n8 -- FIXME: what size here?
    F.Mem16 ar           -> getBVAddress ar >>= mk . mkBVAddr n16
    F.Mem32 ar           -> getBVAddress ar >>= mk . mkBVAddr n32
    F.Mem64 ar           -> getBVAddress ar >>= mk . mkBVAddr n64
    F.Mem128 ar          -> getBVAddress ar >>= mk . mkBVAddr n128
    -- Floating point memory
    F.FPMem32 ar          -> getBVAddress ar >>= mk . mkFPAddr SingleFloatRepr
    F.FPMem64 ar          -> getBVAddress ar >>= mk . mkFPAddr DoubleFloatRepr
    F.FPMem80 ar          -> getBVAddress ar >>= mk . mkFPAddr X86_80FloatRepr

    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> mk (reg_low8 $ N.gpFromFlexdis r64)
      | Just r64 <- F.is_high_reg r -> mk (reg_high8 $ N.gpFromFlexdis r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r                    -> mk (reg_low16 (N.gpFromFlexdis $ F.reg16_reg r))
    F.DWordReg r                    -> mk (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
    F.QWordReg r                    -> mk (fullRegister $ N.gpFromFlexdis r)
    F.ByteImm  w                    -> return (SomeBV $ bvLit n8  w) -- FIXME: should we cast here?
    F.WordImm  w                    -> return $ SomeBV $ bvLit n16 w
    F.DWordImm w                    -> return $ SomeBV $ bvLit n32 w
    F.QWordImm w                    -> return $ SomeBV $ bvLit n64 w
    F.JumpOffset _ off              -> return $ SomeBV $ bvLit n64 off
  where
    -- FIXME: what happens with signs etc?
    mk :: forall m n'
       . (Semantics m, SupportedBVWidth n')
       => MLocation m (BVType n')
       -> m (SomeBV (Value m))
    mk l = SomeBV <$> get l

-- | Calculates the address corresponding to an AddrRef
getBVAddress :: forall m. FullSemantics m => F.AddrRef -> m (Value m (BVType 64))
getBVAddress ar =
  case ar of
   -- FIXME: It seems that there is no sign extension here ...
    F.Addr_32 seg m_r32 m_int_r32 i32 -> do
      base <- case m_r32 of
                Nothing -> return $! bvKLit 0
                Just r  -> get (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
      scale <- case m_int_r32 of
                 Nothing     -> return $! bvKLit 0
                 Just (i, r) -> bvTrunc n32 . bvMul (bvLit n32 i)
                                <$> get (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
      let offset = uext n64 (base `bvAdd` scale `bvAdd` bvLit n32 (F.displacementInt i32))
      mk_absolute seg offset
    F.IP_Offset_32 _seg _i32                 -> fail "IP_Offset_32"
    F.Offset_32    _seg _w32                 -> fail "Offset_32"
    F.Offset_64    seg w64 -> do
      mk_absolute seg (bvLit n64 w64)
    F.Addr_64      seg m_r64 m_int_r64 i32 -> do
      base <- case m_r64 of
                Nothing -> return v0_64
                Just r  -> get (fullRegister $ N.gpFromFlexdis r)
      scale <- case m_int_r64 of
                 Nothing     -> return v0_64
                 Just (i, r) -> bvTrunc n64 . bvMul (bvLit n64 i)
                                 <$> get (fullRegister $ N.gpFromFlexdis r)
      let offset = base `bvAdd` scale `bvAdd` bvLit n64 i32
      mk_absolute seg offset
    F.IP_Offset_64 seg i32 -> do
      ip_val <- get (fullRegister N.rip)
      mk_absolute seg (bvAdd (bvLit n64 i32) ip_val)
  where
    v0_64 = bvLit n64 (0 :: Int)
    -- | Add the segment base to compute an absolute address.
    mk_absolute :: F.Segment -> Value m (BVType 64) -> m (Value m (BVType 64))
    mk_absolute seg offset
      -- In 64-bit mode the CS, DS, ES, and SS segment registers
      -- are forced to zero, and so segmentation is a nop.
      --
      -- We could nevertheless call 'getSegmentBase' in all cases
      -- here, but that adds a lot of noise to the AST in the common
      -- case of segments other than FS or GS.
      | seg == F.CS || seg == F.DS || seg == F.ES || seg == F.SS = return offset
      -- The FS and GS segments can be non-zero based in 64-bit mode.
      | otherwise = do
        base <- getSegmentBase seg
        return $ base `bvAdd` offset

-- | Extract the _location_ of a value, not the value contained.
getSomeBVLocation :: FullSemantics m => F.Value -> m (SomeBV (MLocation m))
getSomeBVLocation v =
  case v of
    F.ControlReg cr     -> mk (fullRegister $ N.controlFromFlexdis cr)
    F.DebugReg dr       -> mk (fullRegister $ N.debugFromFlexdis dr)
    F.MMXReg mmx        -> mk (x87reg_mmx $ N.mmxFromFlexdis mmx)
    F.XMMReg xmm        -> mk (fullRegister $ N.xmmFromFlexdis xmm)
    F.SegmentValue s    -> mk (fullRegister $ N.segmentFromFlexdis s)
    F.FarPointer _      -> fail "FarPointer"
    F.VoidMem ar        -> getBVAddress ar >>= mk . mkBVAddr n8 -- FIXME: what size here?
    F.Mem8  ar          -> getBVAddress ar >>= mk . mkBVAddr n8
    F.Mem16 ar          -> getBVAddress ar >>= mk . mkBVAddr n16
    F.Mem32 ar          -> getBVAddress ar >>= mk . mkBVAddr n32
    F.Mem64 ar          -> getBVAddress ar >>= mk . mkBVAddr n64
    F.Mem128 ar         -> getBVAddress ar >>= mk . mkBVAddr n128
    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> mk (reg_low8  $ N.gpFromFlexdis r64)
      | Just r64 <- F.is_high_reg r -> mk (reg_high8 $ N.gpFromFlexdis r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r                    -> mk (reg_low16 (N.gpFromFlexdis $ F.reg16_reg r))
    F.DWordReg r                    -> mk (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
    F.QWordReg r                    -> mk (fullRegister $ N.gpFromFlexdis r)
    F.ByteImm  _ -> noImm
    F.WordImm  _ -> noImm
    F.DWordImm _ -> noImm
    F.QWordImm _ -> noImm
    F.JumpOffset{} -> error "Jump Offset is not a location."
    F.X87Register i -> mk (X87StackRegister i)
    F.FPMem32 ar -> getBVAddress ar >>= mk . mkBVAddr n32
    F.FPMem64 ar -> getBVAddress ar >>= mk . mkBVAddr n64
    F.FPMem80 ar -> getBVAddress ar >>= mk . mkBVAddr n80
  where
    noImm :: Monad m => m a
    noImm = fail "Immediate is not a location"
    mk :: forall m n. (FullSemantics m, SupportedBVWidth n)
       => MLocation m (BVType n)
       -> m (SomeBV (MLocation m))
    mk = return . SomeBV

castSomeLocToWidth :: Monad m
                   => NatRepr w
                   -> SomeBV (Location addr)
                   -> m (Location addr (BVType w))
castSomeLocToWidth expected (SomeBV v)
  | Just Refl <- testEquality (loc_width v) expected = do
      return v
  | otherwise = traceStack "LOC_WIDTH_ERROR" $
     fail $ "Widths aren't equal: " ++ show (loc_width v) ++ " and " ++ show expected

-- | Given a bitvector value with some width this attempts to interpret it
-- at a specific width, and calls 'fail' if it cannot.
castSomeBVToWidth :: (Monad m, IsValue f)
                  => NatRepr w
                  -> SomeBV f
                  -> m (f (BVType w))
castSomeBVToWidth expected (SomeBV v)
  | Just Refl <- testEquality (bv_width v) expected = return v
  | otherwise = traceStack "VALUE_WIDTH_ERROR" $
     fail $ "Widths aren't equal: " ++ show (bv_width v) ++ " and " ++ show expected


readBVAddress :: FullSemantics m => F.AddrRef -> NatRepr n -> m (Value m (BVType n))
readBVAddress ar w =
  get . mkBVAddr w =<< getBVAddress ar

-- | Get a XMM value
readXMMValue :: FullSemantics m => F.Value -> m (Value m (BVType 128))
readXMMValue (F.XMMReg r) = get $ fullRegister $ N.xmmFromFlexdis r
readXMMValue (F.Mem128 a) = readBVAddress a n128
readXMMValue _ = fail "XMM Instruction given unexpected value."

-- | Read a value expected to be a void memory value.
readVoidMemValue :: FullSemantics m => F.Value -> NatRepr n -> m (Value m (BVType n))
readVoidMemValue (F.VoidMem a) w = readBVAddress a w
readVoidMemValue _ _ = fail "readVoidMem given bad address."

getBVLocation :: FullSemantics m => F.Value -> NatRepr n -> m (MLocation m (BVType n))
getBVLocation l w = do
  castSomeLocToWidth w =<< getSomeBVLocation l

truncateBVValue :: (Monad m, IsValue v, 1 <= n)
                => NatRepr n -> SomeBV v -> m (v (BVType n))
truncateBVValue n (SomeBV v)
  | Just LeqProof <- testLeq n (bv_width v) = do
      return (bvTrunc n v)
  | otherwise =
    fail $ "Widths isn't >=: " ++ show (bv_width v) ++ " and " ++ show n

newtype SemanticsOp
      = SemanticsOp { _unSemanticsOp :: forall m. Semantics m
                                    => F.InstructionInstance
                                    -> m () }

mapNoDupFromList :: (Ord k, Show k) => String -> [(k,v)] -> Map k v
mapNoDupFromList nm = foldl' ins M.empty
  where ins m (k,v) = M.insertWith (\_ _ -> error (e_msg k)) k v m
        e_msg k = nm ++ " contains duplicate entries for " ++ show k ++ "."

semanticsMap :: Map String SemanticsOp
semanticsMap = mapNoDupFromList "semanticsMap" instrs
  where
    mk :: String
       -> (forall m. Semantics m => (F.LockPrefix, [F.Value]) -> m ())
       -> (String, SemanticsOp)
    mk s f = (s, semanticsOp f)

    instrs :: [(String, SemanticsOp)]
    instrs = [ mk "lea"  $ mkBinop $ \loc (F.VoidMem ar) -> do
                 SomeBV l <- getSomeBVLocation loc
                 -- ensure that the location is at most 64 bits
                 Just LeqProof <- return $ testLeq (loc_width l) n64
                 v <- getBVAddress ar
                 exec_lea l (bvTrunc (loc_width l) v)
              , mk "call"   $ maybe_ip_relative really_exec_call
              , mk "imul"   $ \arg@(_, vs) ->
                 case vs of
                   [_] ->
                     unopV exec_imul1 arg
                   [_, _] ->
                     binop (\l v' -> do { v <- get l; exec_imul2_3 l v v' }) arg
                   [loc, val, val'] -> do
                     SomeBV l <- getSomeBVLocation loc
                     v  <- castSomeBVToWidth (loc_width l) =<< getSomeBVValue val
                     SomeBV v' <- getSomeBVValue val'
                     Just LeqProof <- return $ testLeq (bv_width v') (bv_width v)
                     exec_imul2_3 l v v'
                   _ ->
                     fail "Impossible number of argument in imul"
              , mk "jmp"    $ maybe_ip_relative exec_jmp_absolute
              , mk "cqo"    $ \_ -> exec_cqo
              , mk "movsx"  $ geBinop exec_movsx_d
              , mk "movsxd" $ geBinop exec_movsx_d
              , mk "movzx"  $ geBinop exec_movzx
              , mk "xchg"   $ mkBinop $ \v v' -> do
                  SomeBV l <- getSomeBVLocation v
                  l' <- getSomeBVLocation v' >>= castSomeLocToWidth (loc_width l)
                  exec_xchg l l'

              , mk "ret"    $ \(_, vs) ->
                  case vs of
                    []              -> exec_ret Nothing
                    [F.WordImm imm] -> exec_ret (Just (fromIntegral imm))
                    _               -> error "Unexpected number of args to ret"
              , mk "cmps"   $ mkBinopPfxLL $ \pfx -> exec_cmps (pfx == F.RepZPrefix)

              , mk "movs"  $ mkBinopPfxLL
                $ \pfx dest_loc src_loc ->
                   case testLeq (loc_width dest_loc) n64 of
                    Just LeqProof -> exec_movs (pfx == F.RepPrefix) dest_loc src_loc
                    Nothing       -> fail "Argument to movs is too large"

              , mk "stos" $ mkBinopPfxLL
                $ \pfx dest_loc src_loc ->
                   case testLeq (loc_width dest_loc) n64 of
                    Just LeqProof -> exec_stos (pfx == F.RepPrefix) dest_loc src_loc
                    Nothing       -> fail "Argument to stos is too large"

              , mk "scas" $ mkBinopPfxLL
                $ \pfx val_loc buf_loc ->
                   case testLeq (loc_width val_loc) n64 of
                    Just LeqProof -> exec_scas (pfx == F.RepZPrefix ) (pfx == F.RepNZPrefix) val_loc buf_loc
                    Nothing       -> fail "Argument to scass is too large"

              -- fixed size instructions.  We truncate in the case of
              -- an xmm register, for example
              , mk "addsd"   $ truncateKnownBinop exec_addsd
              , mk "addss"   $ truncateKnownBinop exec_addss              
              , mk "subsd"   $ truncateKnownBinop exec_subsd
              , mk "movsd"   $ mkBinop (movsX n64)
              , mk "movapd"  $ truncateKnownBinop exec_movapd
              , mk "movaps"  $ truncateKnownBinop exec_movaps
              , mk "movups"  $ truncateKnownBinop exec_movups
              , mk "movdqa"  $ truncateKnownBinop exec_movdqa
              , mk "movdqu"  $ truncateKnownBinop exec_movdqa
              , mk "movsd_sse" $ mkBinop (movsX n64)
              , mk "movss"   $ mkBinop (movsX n32)
              , mk "mulsd"   $ truncateKnownBinop exec_mulsd
              , mk "divsd"   $ truncateKnownBinop exec_divsd
              , mk "divss"   $ truncateKnownBinop exec_divss
              , mk "ucomisd" $ truncateKnownBinop exec_ucomisd
              , mk "xorpd"   $ mkBinop $ \loc val -> do
                  l <- getBVLocation loc n128
                  v <- readXMMValue val
                  modify (`bvXor` v) l
              , mk "xorps"   $ mkBinop $ \loc val -> do
                  l <- getBVLocation loc n128
                  v <- readXMMValue val
                  modify (`bvXor` v) l

              , mk "cvtsi2ss" $ mkBinop $ \loc val -> do
                l <- getBVLocation loc n128
                SomeBV v <- getSomeBVValue val
                exec_cvtsi2ss l v

              , mk "cvttsd2si" $ mkBinop $ \loc val -> do
                  SomeBV l  <- getSomeBVLocation loc
                  v <- truncateBVValue knownNat =<< getSomeBVValue val
                  exec_cvttsd2si l v

              , mk "cvtsi2sd" $ mkBinop $ \loc val -> do
                l <- getBVLocation loc n128
                SomeBV v <- getSomeBVValue val
                exec_cvtsi2sd l v

              , mk "cvtss2sd" $ truncateKnownBinop exec_cvtss2sd

              -- regular instructions
              , mk "add"     $ binop exec_add
              , mk "adc"     $ binop exec_adc
              , mk "and"     $ binop exec_and
              , mk "bt"      $ geBinop exec_bt
              , mk "btc"     $ geBinop exec_btc
              , mk "btr"     $ geBinop exec_btr
              , mk "bts"     $ geBinop exec_bts
              , mk "bsf"     $ binop exec_bsf
              , mk "bsr"     $ binop exec_bsr
              , mk "bswap"   $ unop  exec_bswap
              , mk "cbw"     $ const exec_cbw
              , mk "cwde"    $ const exec_cwde
              , mk "cdqe"    $ const exec_cdqe
              , mk "clc"     $ const exec_clc
              , mk "cld"     $ \_ -> exec_cld
              , mk "cmp"     $ binop exec_cmp
              , mk "dec"     $ unop exec_dec
              , mk "div"     $ unopV exec_div
              , mk "hlt"     $ \_ -> exec_hlt
              , mk "idiv"    $ unopV exec_idiv
              , mk "inc"     $ unop exec_inc
              , mk "leave"   $ const exec_leave
              , mk "mov"     $ binop exec_mov
              , mk "mul"     $ unopV exec_mul
              , mk "neg"     $ unop exec_neg
              , mk "nop"     $ const (return ())
              , mk "not"     $ unop exec_not
              , mk "or"      $ binop exec_or
              , mk "pause"   $ const (return ())
              , mk "pop"     $ unop exec_pop

              , mk "cmpxchg" $ binop exec_cmpxchg
              , mk "cmpxchg8b" $ knownUnop exec_cmpxchg8b
              , mk "push"    $ unopV exec_push
              , mk "rol"     $ geBinop exec_rol
              , mk "ror"     $ geBinop exec_ror
              , mk "sahf"    $ const exec_sahf
              , mk "sbb"     $ binop exec_sbb
              , mk "sar"     $ geBinop exec_sar
              , mk "shl"     $ geBinop exec_shl
              , mk "shr"     $ geBinop exec_shr
              , mk "std"     $ const (df_loc .= true)
              , mk "sub"     $ binop exec_sub
              , mk "test"    $ binop exec_test
              , mk "xadd"    $ mkBinopPfxLL (\_ -> exec_xadd)
              , mk "xor"     $ binop exec_xor

              -- Primitive instructions
              , mk "syscall" $ const (primitive Syscall)
              , mk "cpuid"   $ const (primitive CPUID)
              , mk "rdtsc"   $ const (primitive RDTSC)
              , mk "xgetbv"  $ const (primitive XGetBV)

              -- MMX instructions
              , mk "movd"    $ mkBinopLV exec_movd
              , mk "movq"    $ mkBinopLV exec_movq
              , mk "punpckhbw"  $ binop exec_punpckhbw
              , mk "punpckhwd"  $ binop exec_punpckhwd
              , mk "punpckhdq"  $ binop exec_punpckhdq
              , mk "punpckhqdq" $ binop exec_punpckhqdq
              , mk "punpcklbw"  $ binop exec_punpcklbw
              , mk "punpcklwd"  $ binop exec_punpcklwd
              , mk "punpckldq"  $ binop exec_punpckldq
              , mk "punpcklqdq" $ binop exec_punpcklqdq
              , mk "paddb"   $ binop exec_paddb
              , mk "paddw"   $ binop exec_paddw
              , mk "paddd"   $ binop exec_paddd
              , mk "psubb"   $ binop exec_psubb
              , mk "psubw"   $ binop exec_psubw
              , mk "psubd"   $ binop exec_psubd
              , mk "pcmpeqb" $ binop exec_pcmpeqb
              , mk "pcmpeqw" $ binop exec_pcmpeqw
              , mk "pcmpeqd" $ binop exec_pcmpeqd
              , mk "pcmpgtb" $ binop exec_pcmpgtb
              , mk "pcmpgtw" $ binop exec_pcmpgtw
              , mk "pcmpgtd" $ binop exec_pcmpgtd
              , mk "pand"    $ binop exec_pand
              , mk "pandn"   $ binop exec_pandn
              , mk "por"     $ binop exec_por
              , mk "pxor"    $ binop exec_pxor

              -- SSE instructions
              , mk "movhlps" $ knownBinop exec_movhlps
              , mk "movlhps" $ knownBinop exec_movlhps
              , mk "pmaxub"  $ binop exec_pmaxub
              , mk "pmaxuw"  $ binop exec_pmaxuw
              , mk "pmaxud"  $ binop exec_pmaxud
              , mk "pmaxsb"  $ binop exec_pmaxsb
              , mk "pmaxsw"  $ binop exec_pmaxsw
              , mk "pmaxsd"  $ binop exec_pmaxsd
              , mk "pminub"  $ binop exec_pminub
              , mk "pminuw"  $ binop exec_pminuw
              , mk "pminud"  $ binop exec_pminud
              , mk "pminsb"  $ binop exec_pminsb
              , mk "pminsw"  $ binop exec_pminsw
              , mk "pminsd"  $ binop exec_pminsd
              , mk "pmovmskb" $ mkBinopLV exec_pmovmskb
              , mk "movhpd"  $ mkBinopLV exec_movhpd
              , mk "movlpd"  $ mkBinopLV exec_movlpd
              , mk "pshufd"  $ ternop exec_pshufd
              , mk "pslldq"  $ geBinop exec_pslldq
              , mk "lddqu"   $ mkBinop $ \loc val -> do
                  l <- getBVLocation loc n128
                  v <- readVoidMemValue val n128
                  exec_lddqu l v
              , mk "palignr" $ ternop exec_palignr

              -- X87 FP instructions
              , mk "fadd"    $ fpUnopOrRegBinop exec_fadd
              , mk "fld"     $ fpUnopV exec_fld
              , mk "fmul"    $ fpUnopOrRegBinop exec_fmul
              , mk "fnstcw"  $ knownUnop exec_fnstcw -- stores to bv memory (i.e., not FP)
              , mk "fst"     $ fpUnop exec_fst
              , mk "fstp"    $ fpUnop exec_fstp
              , mk "fsub"    $ fpUnopOrRegBinop exec_fsub
              , mk "fsubp"   $ fpUnopOrRegBinop exec_fsubp
              , mk "fsubr"   $ fpUnopOrRegBinop exec_fsubr
              , mk "fsubrp"  $ fpUnopOrRegBinop exec_fsubrp
             ] ++ mkConditionals "cmov" (\f -> binop (exec_cmovcc f))
               ++ mkConditionals "j"    (\f ->
                    mkUnop $ \v ->
                      exec_jcc f =<< castSomeBVToWidth knownNat =<< getSomeBVValue v)
               ++ mkConditionals "set"  (\f ->
                    mkUnop $ \v ->
                      exec_setcc f =<< castSomeLocToWidth knownNat =<< getSomeBVLocation v)

-- Helpers
x87fir :: FloatInfoRepr X86_80Float
x87fir = X86_80FloatRepr

-- | Call the appropriate @movss@ or @movsd@ variant.
--
-- The @movss@ and @movsd@ instructions have different semantics
-- depending on the argument type.
movsX ::
  ( Semantics m
  , 1 <= n
  , n <= 128
  , ((128 - n) + n) ~ 128
  , 1 <= 128 - n
  , 128 - n <= 128
  ) =>
  NatRepr n -> F.Value -> F.Value -> m ()
movsX n v1 v2 = do
  -- The memory addresses below are supposed to point to 64 or 32
  -- bits, but flexdis always produces 128 ??? This appears to be a
  -- bug, but Simon did not feel that it was worth fixing; the
  -- workaround is easy: just adjust the number of bits pointed to.
  case (v1, v2) of
    (F.XMMReg {}, F.XMMReg {}) -> do
      l1 <- getBVLocation v1 n128
      l2 <- getBVLocation v2 n128
      exec_movsX_xmm_xmm n l1 l2
    (F.Mem128 {},  F.XMMReg {}) -> do
      l1 <- getBVLocation v1 n128
      l2 <- getBVLocation v2 n128
      let l1' = adjustMemoryAddr l1 n
      exec_movsX_mem_xmm l1' l2
    (F.XMMReg {}, F.Mem128 {}) -> do
      l1 <- getBVLocation v1 n128
      l2 <- getBVLocation v2 n128
      let l2' = adjustMemoryAddr l2 n
      exec_movsX_xmm_mem l1 l2'
    _ -> fail $ "Unexpected arguments in FlexdisMatcher.movsX: " ++
                show n ++ ", " ++ show v1 ++ ", " ++ show v2
  where
    -- | Change the number of bits pointed to by an address.
    adjustMemoryAddr ::
      Location addr (BVType src) -> NatRepr tgt -> Location addr (BVType tgt)
    adjustMemoryAddr (MemoryAddr a _) tgt_width =
      MemoryAddr a (BVTypeRepr tgt_width)
    adjustMemoryAddr _ _ =
      error "FlexdisMatcher.movsX.adjustMemoryAddr: non addr argument!"

semanticsOp :: (forall m. Semantics m => (F.LockPrefix, [F.Value]) -> m ())
            -> SemanticsOp
semanticsOp f = SemanticsOp (\ii -> f (F.iiLockPrefix ii, fmap fst (F.iiArgs ii)))

mkConditionals :: String
               -> (forall m. Semantics m
                   => m (Value m BoolType)
                   -> (F.LockPrefix, [F.Value])
                   -> m ())
               -> [(String, SemanticsOp)]
mkConditionals pfx mkop = map (\(sfx, f) -> (pfx ++ sfx, f)) conditionals
  where
    -- conditional instruction support (cmovcc, jcc)
    conditionals :: [(String, SemanticsOp)]
    conditionals = [ (,) "a"  $ semanticsOp $ mkop cond_a
                   , (,) "ae" $ semanticsOp $ mkop cond_ae
                   , (,) "b"  $ semanticsOp $ mkop cond_b
                   , (,) "be" $ semanticsOp $ mkop cond_be
                   , (,) "g"  $ semanticsOp $ mkop cond_g
                   , (,) "ge" $ semanticsOp $ mkop cond_ge
                   , (,) "l"  $ semanticsOp $ mkop cond_l
                   , (,) "le" $ semanticsOp $ mkop cond_le
                   , (,) "o"  $ semanticsOp $ mkop cond_o
                   , (,) "p"  $ semanticsOp $ mkop cond_p
                   , (,) "s"  $ semanticsOp $ mkop cond_s
                   , (,) "z"  $ semanticsOp $ mkop cond_z
                   , (,) "e"  $ semanticsOp $ mkop cond_z
                   , (,) "ne" $ semanticsOp $ mkop cond_nz
                   , (,) "no" $ semanticsOp $ mkop cond_no
                   , (,) "np" $ semanticsOp $ mkop cond_np
                   , (,) "ns" $ semanticsOp $ mkop cond_ns
                   , (,) "nz" $ semanticsOp $ mkop cond_nz ]

maybe_ip_relative :: Semantics m =>
                     (Value m (BVType 64) -> m b)
                     -> (t, [F.Value]) -> m b
maybe_ip_relative f (_, vs)
  | [F.JumpOffset _ off] <- vs = do
      next_ip <- bvAdd (bvLit n64 off) <$> get (fullRegister N.rip)
      f next_ip
  | [v] <- vs =
     f =<< castSomeBVToWidth knownNat =<< getSomeBVValue v

  | otherwise  = fail "wrong number of operands"

mkTernop :: FullSemantics m
        => (F.Value -> F.Value -> F.Value -> m a)
        -> (F.LockPrefix, [F.Value])
        -> m a
mkTernop f = mkTernopPfx (\_ -> f)

mkTernopPfx :: FullSemantics m
              => (F.LockPrefix -> F.Value -> F.Value -> F.Value -> m a)
              -> (F.LockPrefix, [F.Value])
              -> m a
mkTernopPfx f (pfx, vs) =
  case vs of
    [v, v', v''] -> f pfx v v' v''
    _            -> fail $ "expecting 3 arguments, got " ++ show (length vs)

mkBinop :: FullSemantics m
        => (F.Value -> F.Value -> m a)
        -> (F.LockPrefix, [F.Value])
        -> m a
mkBinop f = mkBinopPfx (\_ -> f)

mkBinopPfx :: FullSemantics m
              => (F.LockPrefix -> F.Value -> F.Value -> m a)
              -> (F.LockPrefix, [F.Value])
              -> m a
mkBinopPfx f (pfx, vs) =
  case vs of
    [v, v']   -> f pfx v v'
    _         -> fail $ "expecting 2 arguments, got " ++ show (length vs)

mkUnop :: FullSemantics m
          => (F.Value -> m a)
          -> (F.LockPrefix, [F.Value])
          -> m a
mkUnop f = mkUnopPfx (\_ -> f)

mkUnopPfx :: FullSemantics m
          => (F.LockPrefix -> F.Value -> m a)
          -> (F.LockPrefix, [F.Value])
          -> m a
mkUnopPfx f (pfx, vs) = case vs of
                       [v]   -> f pfx v
                       _     -> fail $ "expecting 1 arguments, got " ++ show (length vs)

_mkUnopPfxL ::  Semantics m
           => (forall n. (IsLocationBV m n, 1 <= n) =>
               F.LockPrefix -> MLocation m (BVType n) -> m a)
           -> (F.LockPrefix, [F.Value]) -> m a
_mkUnopPfxL f = mkUnopPfx $ \pfx loc -> do
  SomeBV l <- getSomeBVLocation loc
  f pfx l

mkBinopLV ::  Semantics m
        => (forall n n'. (IsLocationBV m n, 1 <= n')
            => MLocation m (BVType n) -> Value m (BVType n') -> m a)
        -> (F.LockPrefix, [F.Value])
        -> m a
mkBinopLV f = mkBinop $ \loc val -> do
  SomeBV l <- getSomeBVLocation loc
  SomeBV v <- getSomeBVValue val
  f l v

mkBinopPfxLL ::  Semantics m
        => (forall n. (IsLocationBV m n, 1 <= n) =>
            F.LockPrefix ->  MLocation m (BVType n) -> MLocation m (BVType n) -> m a)
        -> (F.LockPrefix, [F.Value]) -> m a
mkBinopPfxLL f = mkBinopPfx $ \pfx loc loc' -> do
  SomeBV l <- getSomeBVLocation loc
  l'       <- getSomeBVLocation loc' >>= castSomeLocToWidth (loc_width l)
  f pfx l l'

-- The location size must be >= the value size.
geBinop :: FullSemantics m
        => (forall n n'. (IsLocationBV m n, 1 <= n', n' <= n)
                       => MLocation m (BVType n) -> Value m (BVType n') -> m ())
        -> (F.LockPrefix, [F.Value])
        -> m ()
geBinop f = mkBinopLV $ \l v -> do
              Just LeqProof <- return $ testLeq (bv_width v) (loc_width l)
              f l v

truncateKnownBinop :: ( KnownNat n'
                      , 1 <= n'
                      , FullSemantics m
                      )
                   => (MLocation m XMMType -> Value m (BVType n') -> m ())
                   -> (F.LockPrefix, [F.Value]) -> m ()
truncateKnownBinop f = mkBinop $ \loc val -> do
  l <- getBVLocation loc n128
  v <- truncateBVValue knownNat =<< getSomeBVValue val
  f l v

knownBinop :: (KnownNat n, KnownNat n', FullSemantics m)
           => (MLocation m (BVType n) -> Value m (BVType n') -> m ())
           -> (F.LockPrefix, [F.Value])
           -> m ()
knownBinop f = mkBinop $ \loc val -> do
  l  <- getSomeBVLocation loc >>= castSomeLocToWidth knownNat
  v  <- castSomeBVToWidth knownNat =<< getSomeBVValue val
  f l v

knownUnop :: (KnownNat n, FullSemantics m) => (MLocation m (BVType n) -> m ())
             -> (F.LockPrefix, [F.Value]) -> m ()
knownUnop f = mkUnop $ \loc -> do
  l  <- getSomeBVLocation loc >>= castSomeLocToWidth knownNat
  f l

unopV :: FullSemantics m => (forall n. IsLocationBV m n => Value m (BVType n) -> m ())
         -> (F.LockPrefix, [F.Value]) -> m ()
unopV f = mkUnop $ \val -> do SomeBV v <- getSomeBVValue val
                              f v

unop :: FullSemantics m => (forall n. IsLocationBV m n => MLocation m (BVType n) -> m ())
        -> (F.LockPrefix, [F.Value]) -> m ()
unop f = mkUnop $ \val -> do SomeBV v <- getSomeBVLocation val
                             f v

binop :: FullSemantics m
      => (forall n. IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ())
      -> (F.LockPrefix, [F.Value]) -> m ()
binop f = mkBinop $ \loc val -> do
  SomeBV l <- getSomeBVLocation loc
  v <- getSignExtendedValue val (loc_width l)
  f l v

ternop :: FullSemantics m
       => (forall k n. (IsLocationBV m n, 1 <= k, k <= n) => MLocation m (BVType n)
                                                          -> Value m (BVType n)
                                                          -> Value m (BVType k)
                                                          -> m ())
       -> (F.LockPrefix, [F.Value]) -> m ()
ternop f = mkTernop $ \loc val1 val2 -> do
  SomeBV l <- getSomeBVLocation loc
  v1 <- castSomeBVToWidth (loc_width l) =<< getSomeBVValue val1
  SomeBV v2 <- getSomeBVValue val2
  Just LeqProof <- return $ testLeq (bv_width v2) (bv_width v1)
  f l v1 v2

fpUnopV :: forall m
        . Semantics m
        => (forall flt. FloatInfoRepr flt -> Value m (FloatType flt) -> m ())
        -> (F.LockPrefix, [F.Value])
        -> m ()
fpUnopV f (_, vs)
  | [F.FPMem32 ar]     <- vs = go SingleFloatRepr ar
  | [F.FPMem64 ar]     <- vs = go DoubleFloatRepr ar
  | [F.FPMem80 ar]     <- vs = go X86_80FloatRepr ar
  | [F.X87Register n]  <- vs = get (X87StackRegister n) >>= f x87fir
  | otherwise                = fail $ "fpUnop: expecting 1 FP argument, got: " ++ show vs
  where
    go :: forall flt. FloatInfoRepr flt -> F.AddrRef -> m ()
    go sz ar = do v <- getBVAddress ar >>= get . mkFPAddr sz
                  f sz v

fpUnop :: forall m. Semantics m
       => (forall flt. FloatInfoRepr flt -> MLocation m (FloatType flt) -> m ())
       -> (F.LockPrefix, [F.Value])
       -> m ()
fpUnop f (_, vs)
  | [F.FPMem32 ar]     <- vs = go SingleFloatRepr ar
  | [F.FPMem64 ar]     <- vs = go DoubleFloatRepr ar
  | [F.FPMem80 ar]     <- vs = go X86_80FloatRepr ar
  | [F.X87Register n]  <- vs = f x87fir (X87StackRegister n)
  | otherwise                = fail $ "fpUnop: expecting 1 FP argument, got: " ++ show vs
  where
    go :: forall flt. FloatInfoRepr flt -> F.AddrRef -> m ()
    go sz ar = do l <- mkFPAddr sz <$> getBVAddress ar
                  f sz l

fpUnopOrRegBinop :: forall m. Semantics m
                 => (forall flt_d flt_s
                     . FloatInfoRepr flt_d
                     -> MLocation m (FloatType flt_d)
                     -> FloatInfoRepr flt_s
                     -> Value m (FloatType flt_s)
                     -> m ())
                    -> (F.LockPrefix, [F.Value])
                 -> m ()
fpUnopOrRegBinop f args@(_, vs)
  | length vs == 1     = fpUnopV (f x87fir (X87StackRegister 0)) args
  | otherwise          = knownBinop (\r r' -> f x87fir r x87fir r') args

-- | This function executes a single instruction.
--
-- We divide instructions into
--   * regular:   those which take arguments of the same, polymorphic, width
--   * irrugular: those which have particular parsing requirements
--   * fixed:     those which have exact sizes known

-- FIXME: do something more interesting here than 'Maybe'
execInstruction :: FullSemantics m => Word64 -> F.InstructionInstance -> Maybe (m ())
execInstruction next ii =
  case M.lookup (F.iiOp ii) semanticsMap of
    Just (SemanticsOp f) -> Just $ do
      rip .= bvLit knownNat next
      f ii -- (F.iiLockPrefix ii) (F.iiAddrSize ii) (F.iiArgs ii)
    Nothing -> trace ("Unsupported instruction (" ++ showHex next "): " ++ show ii) Nothing
