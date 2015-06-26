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
  ) where

import           Control.Applicative ( (<$>) )
import           Data.List (foldl', stripPrefix)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Type.Equality -- (testEquality, castWith, :~:(..) )
import           GHC.TypeLits (KnownNat)
import           Debug.Trace

import           Data.Parameterized.NatRepr
import qualified Flexdis86 as F
import           Reopt.Semantics.Semantics
import           Reopt.Semantics.Monad
import qualified Reopt.Machine.StateNames as N

data SomeBV v where
  SomeBV :: SupportedBVWidth n => v (BVType n) -> SomeBV v

-- | Extracts the value, truncating as required
getSomeBVValue :: FullSemantics m => F.Value -> m (SomeBV (Value m))
getSomeBVValue v =
  case v of
    F.ControlReg cr     -> mk (Register $ N.controlFromFlexdis cr)
    F.DebugReg dr       -> mk (Register $ N.debugFromFlexdis dr)
    F.MMXReg mmx        -> mk (Register $ N.mmxFromFlexdis mmx)
    F.XMMReg xmm        -> mk (Register $ N.xmmFromFlexdis xmm)
    F.SegmentValue s    -> mk (Register $ N.segmentFromFlexdis s)
    F.X87Register n     -> mk (X87StackRegister n)
    F.FarPointer _      -> fail "FarPointer"
    -- If an instruction can take a VoidMem, it needs to get it explicitly
    F.VoidMem _ar       -> fail "VoidMem"
    F.Mem8  ar          -> getBVAddress ar >>= mk . mkBVAddr n8 -- FIXME: what size here?
    F.Mem16 ar          -> getBVAddress ar >>= mk . mkBVAddr n16
    F.Mem32 ar          -> getBVAddress ar >>= mk . mkBVAddr n32
    F.Mem64 ar          -> getBVAddress ar >>= mk . mkBVAddr n64
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
    F.QWordReg r                    -> mk (Register $ N.gpFromFlexdis r)
    F.ByteImm  w                    -> return (SomeBV $ bvLit n8  w) -- FIXME: should we cast here?
    F.WordImm  w                    -> return (SomeBV $ bvLit n16 w)
    F.DWordImm w                    -> return (SomeBV $ bvLit n32 w)
    F.QWordImm w                    -> return (SomeBV $ bvLit n64 w)
    F.JumpOffset off                -> return (SomeBV $ bvLit n64 off)
  where
    -- FIXME: what happens with signs etc?
    mk :: forall m n'. (Semantics m, SupportedBVWidth n') => MLocation m (BVType n') -> m (SomeBV (Value m))
    mk l = SomeBV <$> get l

-- | Calculates the address corresponding to an AddrRef
getBVAddress :: FullSemantics m => F.AddrRef -> m (Value m (BVType 64))
getBVAddress ar =
  case ar of
   -- FIXME: It seems that there is no sign extension here ...
    F.Addr_32 seg m_r32 m_int_r32 i32 -> do
      check_seg_value seg
      base <- case m_r32 of
                Nothing -> return $! bvKLit 0
                Just r  -> get (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
      scale <- case m_int_r32 of
                 Nothing     -> return $! bvKLit 0
                 Just (i, r) -> bvTrunc n32 . bvMul (bvLit n32 i)
                                <$> get (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
      return $ uext n64 (base `bvAdd` scale `bvAdd` bvLit n32 i32)

    F.IP_Offset_32 _seg _i32                 -> fail "IP_Offset_32"
    F.Offset_32    _seg _w32                 -> fail "Offset_32"
    F.Offset_64    seg w64                 -> do check_seg_value seg
                                                 return (bvLit n64 w64)
    F.Addr_64      seg m_r64 m_int_r64 i32 -> do check_seg_value seg
                                                 base <- case m_r64 of
                                                           Nothing -> return v0_64
                                                           Just r  -> get (Register $ N.gpFromFlexdis r)
                                                 scale <- case m_int_r64 of
                                                            Nothing     -> return v0_64
                                                            Just (i, r) -> bvTrunc n64 . bvMul (bvLit n64 i)
                                                                           <$> get (Register $ N.gpFromFlexdis r)
                                                 return (base `bvAdd` scale `bvAdd` bvLit n64 i32)
    F.IP_Offset_64 seg i32                 -> do check_seg_value seg
                                                 bvAdd (bvLit n64 i32) <$> get (Register N.rip)
  where
    v0_64 = bvLit n64 (0 :: Int)
    check_seg_value seg
            | seg == F.cs || seg == F.ds || seg == F.es || seg == F.ss = return ()
            | otherwise                                                = fail "Segmentation is not supported"

-- | Extract the _location_ of a value, not the value contained.
getSomeBVLocation :: FullSemantics m => F.Value -> m (SomeBV (MLocation m))
getSomeBVLocation v =
  case v of
    F.ControlReg cr     -> mk (Register $ N.controlFromFlexdis cr)
    F.DebugReg dr       -> mk (Register $ N.debugFromFlexdis dr)
    F.MMXReg mmx        -> mk (Register $ N.mmxFromFlexdis mmx)
    F.XMMReg xmm        -> mk (Register $ N.xmmFromFlexdis xmm)
    F.SegmentValue s    -> mk (Register $ N.segmentFromFlexdis s)
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
    F.QWordReg r                    -> mk (Register $ N.gpFromFlexdis r)
    F.ByteImm  _ -> noImm
    F.WordImm  _ -> noImm
    F.DWordImm _ -> noImm
    F.QWordImm _ -> noImm
    F.JumpOffset _ -> error "Jump Offset is not a location."
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

checkEqBV :: Monad m  => (forall n'. f (BVType n') -> NatRepr n') -> NatRepr n -> f (BVType p) -> m (f (BVType n))
checkEqBV getW n v
  | Just Refl <- testEquality (getW v) n = return v
  | otherwise = traceStack "WIDTH_ERROR" $ fail $ "Widths aren't equal: " ++ show (getW v) ++ " and " ++ show n

checkSomeBV :: Monad m
            => (forall n'. f (BVType n') -> NatRepr n')
            -> NatRepr n
            -> SomeBV f
            -> m (f (BVType n))
checkSomeBV getW n (SomeBV v) = checkEqBV getW n v

readBVAddress :: FullSemantics m => F.AddrRef -> NatRepr n -> m (Value m (BVType n))
readBVAddress ar w =
  get . mkBVAddr w =<< getBVAddress ar

getBVValue :: FullSemantics m => F.Value -> NatRepr n -> m (Value m (BVType n))
getBVValue (F.VoidMem ar) w = readBVAddress ar w
getBVValue (F.Mem8    ar) w = readBVAddress ar w
getBVValue (F.Mem16   ar) w = readBVAddress ar w
getBVValue (F.Mem32   ar) w = readBVAddress ar w
getBVValue (F.Mem64   ar) w = readBVAddress ar w
getBVValue (F.Mem128  ar) w = readBVAddress ar w
getBVValue v w = checkSomeBV bv_width w =<< getSomeBVValue v

getBVLocation :: FullSemantics m => F.Value -> NatRepr n -> m (MLocation m (BVType n))
getBVLocation l w = do
  checkSomeBV loc_width w =<< getSomeBVLocation l

truncateBVValue :: (Monad m, IsValue v, 1 <= n)
                => NatRepr n -> SomeBV v -> m (v (BVType n))
truncateBVValue n (SomeBV v)
  | Just LeqProof <- testLeq n (bv_width v) = do
      return (bvTrunc n v)
  | otherwise                               = fail $ "Widths isn't >=: " ++ show (bv_width v) ++ " and " ++ show n


truncateBVLocation :: (1 <= n) => SomeBV (Location addr) -> NatRepr n -> Location addr (BVType n)
truncateBVLocation (SomeBV l) tgt
  | Just Refl <- testEquality tgt (loc_width l) = l
  | Just LeqProof <- testLeq (incNat tgt) (loc_width l) =
    TruncLoc l tgt
  | otherwise = error "truncateBVLocation given bad width."

unimplemented :: Monad m => m ()
unimplemented = fail "UNIMPLEMENTED"

newtype SemanticsOp
      = SemanticsOp { unSemanticsOp :: forall m. Semantics m
                                    => F.InstructionInstance
                                    -> m () }


mapNoDupFromList :: (Ord k, Show k) => String -> [(k,v)] -> Map k v
mapNoDupFromList nm = foldl' ins M.empty
  where ins m (k,v) = M.insertWith (\_ _ -> error (e_msg k)) k v m
        e_msg k = nm ++ " contains duplicate entries for " ++ show k ++ "."

-- semanticsMap :: forall m. Semantics m => Map String ((F.LockPrefix, [F.Value]) -> m ())
semanticsMap :: Map String SemanticsOp
semanticsMap = mapNoDupFromList "semanticsMap" instrs
  where
    mk :: String
       -> (forall m. Semantics m => (F.LockPrefix, [F.Value]) -> m ())
       -> (String, SemanticsOp)
    mk s f = (s, SemanticsOp $ \ii -> f (F.iiLockPrefix ii, F.iiArgs ii))
    mk' :: String
       -> (forall m. Semantics m => F.InstructionInstance -> m ())
       -> (String, SemanticsOp)
    mk' s f = (s, SemanticsOp f)

    instrs :: [(String, SemanticsOp)]
    instrs = [ mk "lea"  $ mkBinop $ \loc (F.VoidMem ar) ->
                                       do SomeBV l <- getSomeBVLocation loc
                                          -- ensure that the location is at most 64 bits
                                          Just LeqProof <- return $ testLeq (loc_width l) n64
                                          v <- getBVAddress ar
                                          exec_lea l (bvTrunc (loc_width l) v)
              , mk "call"   $ maybe_ip_relative really_exec_call
              , mk "imul"   $ \arg@(_, vs) -> case vs of
                                            [_]              -> unopV exec_imul1 arg
                                            [_, _]           -> binop (\l v' -> do { v <- get l; exec_imul2_3 l v v' }) arg
                                            [loc, val, val'] -> do SomeBV l <- getSomeBVLocation loc
                                                                   v  <- getSomeBVValue val  >>= checkSomeBV bv_width (loc_width l)
                                                                   SomeBV v' <- getSomeBVValue val'
                                                                   Just LeqProof <- return $ testLeq (bv_width v') (bv_width v)
                                                                   exec_imul2_3 l v v'
                                            _                 -> fail "Impossible number of argument in imul"
              , mk "jmp"    $ maybe_ip_relative exec_jmp_absolute
              , mk "cqo"    $ \_ -> exec_cqo
              , mk "movsx"  $ geBinop exec_movsx_d
              , mk "movsxd" $ geBinop exec_movsx_d
              , mk "movzx"  $ geBinop exec_movzx
              , mk "xchg"   $ mkBinop $ \v v' -> do
                  SomeBV l <- getSomeBVLocation v
                  l' <- getSomeBVLocation v' >>= checkSomeBV loc_width (loc_width l)
                  exec_xchg l l'

              , mk "ret"    $ \(_, vs) -> case vs of
                                            []              -> exec_ret Nothing
                                            [F.WordImm imm] -> exec_ret (Just imm)

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
                    Nothing       -> fail "Argument to movs is too large"

              -- fixed size instructions.  We truncate in the case of
              -- an xmm register, for example
              , mk "addsd"   $ truncateKnownBinop exec_addsd
              , mk "subsd"   $ truncateKnownBinop exec_subsd
              , mk "movsd"   $ truncate64Op exec_movsd
              , mk "movapd"  $ truncateKnownBinop exec_movapd
              , mk "movaps"  $ truncateKnownBinop exec_movaps
              , mk "movdqa"  $ truncateKnownBinop exec_movdqa
              , mk "movdqu"  $ truncateKnownBinop exec_movdqa                
              , mk "movsd_sse" $ truncate64Op exec_movsd
              , mk "movss"   $ truncate32Op exec_movss
              , mk "mulsd"   $ truncateKnownBinop exec_mulsd
              , mk "divsd"   $ truncateKnownBinop exec_divsd
              , mk "ucomisd" $ truncateKnownBinop exec_ucomisd
              , mk "xorpd"   $ mkBinop $ \loc val -> do
                  l <- getBVLocation loc n128
                  v <- getBVValue val n128
                  modify (`bvXor` v) l
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
              , mk "bsf"     $ binop exec_bsf
              , mk "bsr"     $ binop exec_bsr
              , mk "bswap"   $ unop  exec_bswap
              , mk "cbw"     $ const exec_cbw
              , mk "cwde"    $ const exec_cwde
              , mk "cdqe"    $ const exec_cdqe
              , mk "clc"     $ const exec_clc
              , mk "cld"     $ const exec_cld
              , mk "cmp"     $ binop exec_cmp
              , mk "dec"     $ unop exec_dec
              , mk "div"     $ unopV exec_div
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
              , mk "sbb"     $ binop exec_sbb
              , mk "sar"     $ geBinop exec_sar
              , mk "shl"     $ geBinop exec_shl
              , mk "shr"     $ geBinop exec_shr
              , mk "std"     $ const (df_loc .= true)
              , mk "sub"     $ binop exec_sub
              , mk "syscall" $ const syscall
              , mk "test"    $ binop exec_test
              , mk "xor"     $ binop exec_xor
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
               ++ mkConditionals "j"    (\f -> mkUnop $ \v -> getSomeBVValue v >>= checkSomeBV bv_width knownNat >>= exec_jcc f)
               ++ mkConditionals "set"  (\f -> mkUnop $ \v -> getSomeBVLocation v >>= checkSomeBV loc_width knownNat >>= exec_setcc f)


-- Helpers
x87fir :: FloatInfoRepr X86_80Float
x87fir = X86_80FloatRepr

semanticsOp :: (forall m. Semantics m => (F.LockPrefix, [F.Value]) -> m ())
            -> SemanticsOp
semanticsOp f = SemanticsOp (\ii -> f (F.iiLockPrefix ii, F.iiArgs ii))


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
  | [F.JumpOffset off] <- vs
       = do next_ip <- bvAdd (bvLit n64 off) <$> get (Register N.rip)
            f next_ip
  | [v]                <- vs
       = getSomeBVValue v >>= checkSomeBV bv_width knownNat >>= f

  | otherwise  = fail "wrong number of operands"

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
    vs        -> fail $ "expecting 2 arguments, got " ++ show (length vs)

mkUnop :: FullSemantics m
          => (F.Value -> m a)
          -> (F.LockPrefix, [F.Value])
          -> m a
mkUnop f (_, vs) = case vs of
                     [v]   -> f v
                     vs    -> fail $ "expecting 1 arguments, got " ++ show (length vs)

mkBinopLV ::  Semantics m
        => (forall n n'. (IsLocationBV m n, 1 <= n') => MLocation m (BVType n) -> Value m (BVType n') -> m a)
        -> (F.LockPrefix, [F.Value]) -> m a
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
  l'       <- getSomeBVLocation loc' >>= checkSomeBV loc_width (loc_width l)
  f pfx l l'

-- The location size must be >= the value size.
geBinop :: FullSemantics m
        => (forall n n'. (IsLocationBV m n, 1 <= n', n' <= n)
                       => MLocation m (BVType n) -> Value m (BVType n') -> m ())
        -> (F.LockPrefix, [F.Value]) -> m ()
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

truncate32Op :: (FullSemantics m)
             => (MLocation m (BVType 32) -> Value m (BVType 32) -> m ())
             -> (F.LockPrefix, [F.Value]) -> m ()
truncate32Op f = mkBinop $ \loc val -> do
  l <- (`truncateBVLocation` n32) <$> getSomeBVLocation loc
  v <- truncateBVValue n32 =<< getSomeBVValue val
  f l v

truncate64Op :: (FullSemantics m)
             => (MLocation m (BVType 64) -> Value m (BVType 64) -> m ())
             -> (F.LockPrefix, [F.Value]) -> m ()
truncate64Op f = mkBinop $ \loc val -> do
  l <- (`truncateBVLocation` n64) <$> getSomeBVLocation loc
  v <- truncateBVValue n64 =<< getSomeBVValue val
  f l v


knownBinop :: (KnownNat n, KnownNat n', FullSemantics m) => (MLocation m (BVType n) -> Value m (BVType n') -> m ())
              -> (F.LockPrefix, [F.Value]) -> m ()
knownBinop f = mkBinop $ \loc val -> do l  <- getSomeBVLocation loc >>= checkSomeBV loc_width knownNat
                                        v  <- getSomeBVValue val >>= checkSomeBV bv_width knownNat
                                        f l v

knownUnop :: (KnownNat n, FullSemantics m) => (MLocation m (BVType n) -> m ())
             -> (F.LockPrefix, [F.Value]) -> m ()
knownUnop f = mkUnop $ \loc -> do l  <- getSomeBVLocation loc >>= checkSomeBV loc_width knownNat
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
  v <- checkSomeBV bv_width (loc_width l) =<< getSomeBVValue val
  f l v

fpUnopV :: forall m. Semantics m => (forall flt. FloatInfoRepr flt -> Value m (FloatType flt) -> m ())
           -> (F.LockPrefix, [F.Value]) -> m ()
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

fpUnopOrRegBinop :: forall m. Semantics m =>
                    (forall flt_d flt_s. FloatInfoRepr flt_d -> MLocation m (FloatType flt_d) -> FloatInfoRepr flt_s -> Value m (FloatType flt_s) -> m ())
                    -> (F.LockPrefix, [F.Value]) -> m ()
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
execInstruction :: FullSemantics m => Int -> F.InstructionInstance -> Maybe (m ())
execInstruction w ii =
  case M.lookup (F.iiOp ii) semanticsMap of
    Just (SemanticsOp f) -> Just $ do
      old_pc <- get rip
      rip .= old_pc `bvAdd` (bvLit n64 w)
      f ii -- (F.iiLockPrefix ii) (F.iiAddrSize ii) (F.iiArgs ii)
    _                    -> trace ("Unsupported instruction: " ++ show ii) Nothing
