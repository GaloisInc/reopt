{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.FlexdisMatcher
-- Description      : Pattern matches against a Flexdis86 InstructionInstance.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Reopt.Semantics.FlexdisMatcher
  ( execInstruction
  ) where

import Control.Monad (when)
import Control.Applicative ( (<$>) )

import qualified Flexdis86 as F
import Reopt.Semantics
import Reopt.Semantics.Monad

import Data.Parameterized.NatRepr


data SomeBV v where
  SomeBV :: SupportedBVWidth n => v (BVType n) -> SomeBV v

-- | Extracts the value, truncating as required
getBVValue :: (FullSemantics m) => NatRepr n -> F.Value -> m (Value m (BVType n))
getBVValue (sz :: NatRepr n) v = 
  case v of 
    F.ControlReg cr     -> mk (CReg cr)
    F.DebugReg dr       -> mk (DReg dr)
    F.MMXReg mmx        -> mk (MMXReg mmx)
    F.XMMReg xmm        -> mk (XMMReg xmm)
    F.SegmentValue s    -> mk (SegmentReg s)
    F.FarPointer _      -> fail "FarPointer"    
    F.VoidMem _ar       -> fail "VoidMem"
    F.Mem8  ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n8 -- FIXME: what size here?
    F.Mem16 ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n16
    F.Mem32 ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n32
    F.Mem64 ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n64
    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> mk (reg_low n8 r64)
      | Just r64 <- F.is_high_reg r -> mk (reg_high n8 r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r                    -> mk (reg_low n16 (F.reg16_reg r))
    F.DWordReg r                    -> mk (reg_low n16 (F.reg32_reg r))
    F.QWordReg r                    -> mk (GPReg r)
    F.ByteImm  w                    -> return (bvLit sz $ fromIntegral w) -- FIXME: should we cast here?
    F.WordImm  w                    -> return (bvLit sz $ fromIntegral w)
    F.DWordImm w                    -> return (bvLit sz $ fromIntegral w)
    F.QWordImm w                    -> return (bvLit sz $ fromIntegral w) 
    F.JumpOffset off                -> return (bvLit sz $ fromIntegral off)
  where
    -- FIXME: what happens with signs etc?
    mk :: forall m n'. (FullSemantics m, SupportedBVWidth n') => MLocation m (BVType n') -> m (Value m (BVType n))
    mk l = do when (widthVal (loc_width l) < widthVal sz) $ fail "Extending in getBVValue"
              get (BVSlice l 0 (BVTypeRepr sz))

-- | Calculates the address corresponding to an expression.
getBVEffectiveAddress :: FullSemantics m => F.AddrRef -> m (Value m (BVType 64))
getBVEffectiveAddress ar =
  case ar of
    F.Addr_32      _seg _m_r32 _m_int_r32 _i32 -> fail "Addr_32"                                                 
    F.IP_Offset_32 _seg _i32                 -> fail "IP_Offset_32"
    F.Offset_32    _seg _w32                 -> fail "Offset_32"
    F.Offset_64    seg w64                 -> do check_seg_value seg
                                                 return (bvLit n64 (fromIntegral w64))
    F.Addr_64      seg m_r64 m_int_r64 i32 -> do check_seg_value seg
                                                 base <- case m_r64 of
                                                           Nothing -> return v0_64
                                                           Just r  -> get (GPReg r)
                                                 scale <- case m_int_r64 of
                                                            Nothing     -> return v0_64
                                                            Just (i, r) -> bvTrunc n64 . bvMul (bvLit n64 i) <$> get (GPReg r)
                                                 return (base `bvAdd` scale `bvAdd` bvLit n64 (fromIntegral i32))
    F.IP_Offset_64 seg i32                 -> do check_seg_value seg
                                                 bvAdd (bvLit n64 (fromIntegral i32)) <$> get IPReg
  where
    v0_64 = bvLit n64 0
    check_seg_value seg
            | seg == F.cs || seg == F.ds || seg == F.es || seg == F.ss = return ()
            | otherwise                                                = fail "Segmentation is not supported"
    -- seg_value seg
    --   | seg == F.cs || seg == F.ds || seg == F.es || seg == F.ss = return (bvLit n64 0)
    --   | otherwise                                                = fail "Segmentation is not supported"
-- | Extract the _location_ of a value, not the value contained.
getBVLocation :: FullSemantics m => F.Value -> m (SomeBV (MLocation m))
getBVLocation v =
  case v of 
    F.ControlReg cr     -> mk (CReg cr)
    F.DebugReg dr       -> mk (DReg dr)
    F.MMXReg mmx        -> mk (MMXReg mmx)
    F.XMMReg xmm        -> mk (XMMReg xmm)
    F.SegmentValue s    -> mk (SegmentReg s)
    F.FarPointer _      -> fail "FarPointer"    
    F.VoidMem ar        -> getBVEffectiveAddress ar >>= mk . mkBVAddr n8 -- FIXME: what size here?
    F.Mem8  ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n8 -- FIXME: what size here?
    F.Mem16 ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n16
    F.Mem32 ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n32
    F.Mem64 ar          -> getBVEffectiveAddress ar >>= mk . mkBVAddr n64
    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> mk (reg_low n8 r64)
      | Just r64 <- F.is_high_reg r -> mk (reg_high n8 r64)
      | otherwise                 -> fail "unknown r8"
    F.WordReg  r                    -> mk (reg_low n16 (F.reg16_reg r))
    F.DWordReg r                    -> mk (reg_low n16 (F.reg32_reg r))
    F.QWordReg r                    -> mk (GPReg r)
    -- ByteImm  Word8    
    -- WordImm  Word16
    -- DWordImm Word32
    -- QWordImm Word64
    -- JumpOffset Int64
    _                 -> fail "Immediate is not a location"
  where
    mk :: forall m n. (FullSemantics m, SupportedBVWidth n) => MLocation m (BVType n) -> m (SomeBV (MLocation m))
    mk = return . SomeBV

execInstruction :: FullSemantics m => F.InstructionInstance -> m ()
execInstruction ii =
  case F.iiOp ii of
    -- irregular instructions
    "lea" | [loc, F.VoidMem ar] <- F.iiArgs ii
            -> do SomeBV l <- getBVLocation loc
                  v <- getBVEffectiveAddress ar
                  exec_lea l (bvTrunc (loc_width l) v)
    "addsd" -> fail ""

    -- regular instructions
    "add"   -> binop exec_add
    "adc"   -> binop exec_adc
    "and"   -> binop exec_and
    "bsf"   -> binop exec_bsf
    "bsr"   -> binop exec_bsr
    "bswap" -> unop  exec_bswap
    "call" | [F.JumpOffset off] <- F.iiArgs ii
            -> do next_ip <- bvAdd (bvLit n64 $ fromIntegral off) <$> get IPReg
                  really_exec_call next_ip
    "call" | [v] <- F.iiArgs ii
            -> really_exec_call =<< getBVValue n64 v
    _       -> fail $ "Unsupported instruction: " ++ show ii
  where
    unop :: FullSemantics m => (forall n. IsLocationBV m n => MLocation m (BVType n) -> m ()) -> m ()
    unop f = case F.iiArgs ii of
               [loc] -> do SomeBV l <- getBVLocation loc
                           f l
    binop :: FullSemantics m => (forall n. IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ()) -> m ()
    binop f = case F.iiArgs ii of
                [loc, val] -> do SomeBV l <- getBVLocation loc
                                 v <- getBVValue (loc_width l) val
                                 f l v
                vs         -> error $ "binop: expecting 2 arguments, got " ++ show (length vs)


  -- case (F.iiOp ii, F.iiArgs ii) of
  --   ("add", vs) -> binop exec_add vs
    
    
  --   _ -> fail $ "Unsupported instruction: " ++ show ii
