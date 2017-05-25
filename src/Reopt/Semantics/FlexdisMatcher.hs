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
import           Data.Macaw.CFG (MemRepr(..))
import           Data.Macaw.Memory (Endianness(..))
import           Data.Macaw.Types (n0, n1, n8, n16, n32, n64, n128)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import qualified Flexdis86 as F
import           GHC.TypeLits (KnownNat)

import qualified Reopt.Machine.StateNames as N
import           Reopt.Semantics.Conditions
import           Reopt.Semantics.Monad
import           Reopt.Semantics.Semantics

data SomeBV v where
  SomeBV :: SupportedBVWidth n => v (BVType n) -> SomeBV v

byteMemRepr :: MemRepr (BVType 8)
byteMemRepr = BVMemRepr (knownNat :: NatRepr 1) LittleEndian

wordMemRepr :: MemRepr (BVType 16)
wordMemRepr = BVMemRepr (knownNat :: NatRepr 2) LittleEndian

dwordMemRepr :: MemRepr (BVType 32)
dwordMemRepr = BVMemRepr (knownNat :: NatRepr 4) LittleEndian

qwordMemRepr :: MemRepr (BVType 64)
qwordMemRepr = BVMemRepr (knownNat :: NatRepr 8) LittleEndian

qqwordMemRepr :: MemRepr (BVType 128)
qqwordMemRepr = BVMemRepr (knownNat :: NatRepr 16) LittleEndian

floatMemRepr :: MemRepr (BVType 32)
floatMemRepr = BVMemRepr (knownNat :: NatRepr 4) LittleEndian

doubleMemRepr :: MemRepr (BVType 64)
doubleMemRepr = BVMemRepr (knownNat :: NatRepr 8) LittleEndian

x87MemRepr :: MemRepr (BVType 80)
x87MemRepr = BVMemRepr (knownNat :: NatRepr 10) LittleEndian

xmmMemRepr :: MemRepr (BVType 128)
xmmMemRepr = BVMemRepr (knownNat :: NatRepr 16) LittleEndian


-- | Get a value with the given width, sign extending as necessary.
getSignExtendedValue :: forall m w
                     .  (FullSemantics m, 1 <= w)
                     => F.Value
                     -> NatRepr w
                     -> m (Value m (BVType w))
getSignExtendedValue v out_w =
  case v of
    -- If an instruction can take a VoidMem, it needs to get it explicitly
    F.VoidMem _ar -> fail "VoidMem"
    F.Mem8   ar   -> mk . (`MemoryAddr` byteMemRepr)   =<< getBVAddress ar
    F.Mem16  ar   -> mk . (`MemoryAddr` wordMemRepr)   =<< getBVAddress ar
    F.Mem32  ar   -> mk . (`MemoryAddr` dwordMemRepr)  =<< getBVAddress ar
    F.Mem64  ar   -> mk . (`MemoryAddr` qwordMemRepr)  =<< getBVAddress ar
    F.Mem128 ar   -> mk . (`MemoryAddr` qqwordMemRepr) =<< getBVAddress ar

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
    F.ControlReg cr     -> locValue (fullRegister $ N.controlFromFlexdis cr)
    F.DebugReg dr       -> locValue (fullRegister $ N.debugFromFlexdis dr)
    F.MMXReg mmx        -> locValue (x87reg_mmx $ N.mmxFromFlexdis mmx)
    F.XMMReg xmm        -> locValue (fullRegister $ N.xmmFromFlexdis xmm)
    F.SegmentValue s    -> locValue (fullRegister $ N.segmentFromFlexdis s)
    F.X87Register n     -> locValue (X87StackRegister n)
    F.FarPointer _      -> fail "FarPointer"
    -- If an instruction can take a VoidMem, it needs to get it explicitly
    F.VoidMem _ar       -> fail "VoidMem"
    F.Mem8  ar           -> getBVAddress ar >>= locValue . (`MemoryAddr`  byteMemRepr)
    F.Mem16 ar           -> getBVAddress ar >>= locValue . (`MemoryAddr`  wordMemRepr)
    F.Mem32 ar           -> getBVAddress ar >>= locValue . (`MemoryAddr` dwordMemRepr)
    F.Mem64 ar           -> getBVAddress ar >>= locValue . (`MemoryAddr` qwordMemRepr)
    F.Mem128 ar          -> getBVAddress ar >>= locValue . (`MemoryAddr`   xmmMemRepr)
    -- Floating point memory
    F.FPMem32 ar          -> getBVAddress ar >>= locValue . mkFPAddr SingleFloatRepr
    F.FPMem64 ar          -> getBVAddress ar >>= locValue . mkFPAddr DoubleFloatRepr
    F.FPMem80 ar          -> getBVAddress ar >>= locValue . mkFPAddr X86_80FloatRepr

    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> locValue (reg_low8 $ N.gpFromFlexdis r64)
      | Just r64 <- F.is_high_reg r -> locValue (reg_high8 $ N.gpFromFlexdis r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r                    -> locValue (reg_low16 (N.gpFromFlexdis $ F.reg16_reg r))
    F.DWordReg r                    -> locValue (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
    F.QWordReg r                    -> locValue (fullRegister $ N.gpFromFlexdis r)
    F.ByteImm  w                    -> return $ SomeBV $ bvLit n8  w
    F.WordImm  w                    -> return $ SomeBV $ bvLit n16 w
    F.DWordImm w                    -> return $ SomeBV $ bvLit n32 w
    F.QWordImm w                    -> return $ SomeBV $ bvLit n64 w
    F.JumpOffset _ off              -> return $ SomeBV $ bvLit n64 off
  where
    locValue :: forall m n'
             .  (Semantics m, SupportedBVWidth n')
             => MLocation m (BVType n')
             -> m (SomeBV (Value m))
    locValue l = SomeBV <$> get l

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

data HasRepSize f w = HasRepSize { _ppvWidth :: !(RepValSize w)
                                 , _ppvValue :: !(f (BVType w))
                                 }

-- | Gets the location to store the value poped from.
-- These functions only support general purpose registers/addresses and segments.
getPopLocation :: forall m . FullSemantics m => F.Value -> m (Some (HasRepSize (MLocation m)))
getPopLocation v =
  case v of
    F.SegmentValue s -> pure $ Some $ HasRepSize WordRepVal (fullRegister $ N.segmentFromFlexdis s)
    F.Mem8  ar -> Some . HasRepSize  ByteRepVal . (`MemoryAddr`  byteMemRepr) <$> getBVAddress ar
    F.Mem16 ar -> Some . HasRepSize  WordRepVal . (`MemoryAddr`  wordMemRepr) <$> getBVAddress ar
    F.Mem32 ar -> Some . HasRepSize DWordRepVal . (`MemoryAddr` dwordMemRepr) <$> getBVAddress ar
    F.Mem64 ar -> Some . HasRepSize QWordRepVal . (`MemoryAddr` qwordMemRepr) <$> getBVAddress ar

    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> pure $ Some $ HasRepSize  ByteRepVal (reg_low8 $ N.gpFromFlexdis r64)
      | Just r64 <- F.is_high_reg r -> pure $ Some $ HasRepSize  ByteRepVal (reg_high8 $ N.gpFromFlexdis r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r                    -> pure $ Some $ HasRepSize  WordRepVal (reg_low16 (N.gpFromFlexdis $ F.reg16_reg r))
    F.DWordReg r                    -> pure $ Some $ HasRepSize DWordRepVal (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
    F.QWordReg r                    -> pure $ Some $ HasRepSize QWordRepVal (fullRegister $ N.gpFromFlexdis r)
    _  -> fail $ "Argument " ++ show v ++ " not supported."

-- | Gets a value that can be pushed.
-- These functions only support general purpose registers/addresses and segments.
getPushValue :: forall m . FullSemantics m => F.Value -> m (Some (HasRepSize (Value m)))
getPushValue v =
  case v of
    F.ByteImm  w -> return $ Some $ HasRepSize ByteRepVal  $ bvLit n8  w
    F.WordImm  w -> return $ Some $ HasRepSize WordRepVal  $ bvLit n16 w
    F.DWordImm w -> return $ Some $ HasRepSize DWordRepVal $ bvLit n32 w
    F.QWordImm w -> return $ Some $ HasRepSize QWordRepVal $ bvLit n64 w
    _ -> do
      Some (HasRepSize rep l) <- getPopLocation v
      Some . HasRepSize rep <$> get l

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
    F.VoidMem ar        -> SomeBV . (`MemoryAddr`   byteMemRepr) <$> getBVAddress ar -- FIXME: what size here?
    F.Mem8  ar          -> SomeBV . (`MemoryAddr`   byteMemRepr) <$> getBVAddress ar
    F.Mem16 ar          -> SomeBV . (`MemoryAddr`   wordMemRepr) <$> getBVAddress ar
    F.Mem32 ar          -> SomeBV . (`MemoryAddr`  dwordMemRepr) <$> getBVAddress ar
    F.Mem64 ar          -> SomeBV . (`MemoryAddr`  qwordMemRepr) <$> getBVAddress ar
    F.Mem128 ar         -> SomeBV . (`MemoryAddr` qqwordMemRepr) <$> getBVAddress ar
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
    F.JumpOffset{}  -> fail "Jump Offset is not a location."
    F.X87Register i -> mk (X87StackRegister i)
    F.FPMem32 ar -> getBVAddress ar >>= mk . (`MemoryAddr` floatMemRepr)
    F.FPMem64 ar -> getBVAddress ar >>= mk . (`MemoryAddr` doubleMemRepr)
    F.FPMem80 ar -> getBVAddress ar >>= mk . (`MemoryAddr` x87MemRepr)
  where
    noImm :: Monad m => m a
    noImm = fail "Immediate is not a location"
    mk :: forall m n. (FullSemantics m, SupportedBVWidth n)
       => MLocation m (BVType n)
       -> m (SomeBV (MLocation m))
    mk = return . SomeBV

readBVAddress :: FullSemantics m => F.AddrRef -> MemRepr tp -> m (Value m tp)
readBVAddress ar repr =
  get . (`MemoryAddr` repr) =<< getBVAddress ar

-- | Get a XMM value
readXMMValue :: FullSemantics m => F.Value -> m (Value m (BVType 128))
readXMMValue (F.XMMReg r) = get $ fullRegister $ N.xmmFromFlexdis r
readXMMValue (F.Mem128 a) = readBVAddress a xmmMemRepr
readXMMValue _ = fail "XMM Instruction given unexpected value."

-- | Translate a flexdis value to a location with a particular width.
getBVLocation :: FullSemantics m => F.Value -> NatRepr n -> m (MLocation m (BVType n))
getBVLocation l expected = do
  SomeBV v <- getSomeBVLocation l
  case testEquality (loc_width v) expected of
    Just Refl ->
      return v
    Nothing ->
      fail $ "Widths aren't equal: " ++ show (loc_width v) ++ " and " ++ show expected

-- | Translate a flexdis value to a value with a particular width.
getBVValue :: FullSemantics m => F.Value -> NatRepr n -> m (Value m (BVType n))
getBVValue val expected = do
  SomeBV v <- getSomeBVValue val
  case testEquality (bv_width v) expected of
    Just Refl -> return v
    Nothing ->
      fail $ "Widths aren't equal: " ++ show (bv_width v) ++ " and " ++ show expected

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

-- Semantics for SSE movsb instruction
exec_movsd :: FullSemantics m => F.Value -> F.Value -> m ()
exec_movsd v1 v2 = do
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
exec_movss :: FullSemantics m => F.Value -> F.Value -> m ()
exec_movss v1 v2 = do
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

type SemanticsArgs = (F.LockPrefix, [F.Value], String)

semanticsOp :: String
            -> (forall m. Semantics m => SemanticsArgs -> m ())
            -> (String, SemanticsOp)
semanticsOp mnemonic f = (mnemonic, SemanticsOp (\ii -> f (F.iiLockPrefix ii, fmap fst (F.iiArgs ii), mnemonic)))

mkConditionals :: String
               -> (String
                   -> (forall m. Semantics m => m (Value m BoolType))
                   -> (String, SemanticsOp))
               -> [(String, SemanticsOp)]
mkConditionals pfx mkop = mk <$> conditionalDefs
  where
    mk :: (String, ConditionalDef) -> (String, SemanticsOp)
    mk (suffix, ConditionalDef sop) = mkop (pfx ++ suffix) sop

mkUnop :: FullSemantics m
          => (F.LockPrefix -> F.Value -> m a)
          -> SemanticsArgs
          -> m a
mkUnop f (pfx, vs, mnemonic) =
  case vs of
    [v]   -> f pfx v
    _     -> fail $ "mkUnop: " ++ mnemonic ++ " expecting 1 arguments, got " ++ show (length vs)

unop :: String
     -> (forall m n. FullSemantics m => IsLocationBV m n => MLocation m (BVType n) -> m ())
     -> (String, SemanticsOp)
unop s f = semanticsOp s $ mkUnop $ \_ val -> do
  SomeBV v <- getSomeBVLocation val
  f v

knownUnop :: KnownNat n
          => String
          -> (forall m . FullSemantics m => MLocation m (BVType n) -> m ())
          -> (String, SemanticsOp)
knownUnop s f =
  semanticsOp s $ mkUnop $ \_ loc -> do
    l  <- getBVLocation loc knownNat
    f l

unopV :: String
      -> (forall m n. (FullSemantics m, IsLocationBV m n) => Value m (BVType n) -> m ())
      -> (String, SemanticsOp)
unopV s f =
  semanticsOp s $ mkUnop $ \_ val -> do
    SomeBV v <- getSomeBVValue val
    f v

mkBinopPfx :: FullSemantics m
           => (F.LockPrefix -> F.Value -> F.Value -> m a)
           -> SemanticsArgs
           -> m a
mkBinopPfx f (pfx, vs, mnemonic) =
  case vs of
    [v, v']   -> f pfx v v'
    _         -> fail $ "mkBinopPfx: " ++ mnemonic ++ ": expecting 2 arguments, got " ++ show (length vs)

mkBinop :: FullSemantics m
        => (F.Value -> F.Value -> m a)
        -> SemanticsArgs
        -> m a
mkBinop f = mkBinopPfx (\_ -> f)

binop :: FullSemantics m
      => (forall n. IsLocationBV m n => MLocation m (BVType n) -> Value m (BVType n) -> m ())
      -> SemanticsArgs -> m ()
binop f = mkBinop $ \loc val -> do
  SomeBV l <- getSomeBVLocation loc
  v <- getSignExtendedValue val (loc_width l)
  f l v

fpUnop :: forall m. Semantics m
       => (forall flt. FloatInfoRepr flt -> MLocation m (FloatType flt) -> m ())
       -> SemanticsArgs
       -> m ()
fpUnop f (_, vs, mnemonic)
  | [F.FPMem32 ar]     <- vs = go SingleFloatRepr ar
  | [F.FPMem64 ar]     <- vs = go DoubleFloatRepr ar
  | [F.FPMem80 ar]     <- vs = go X86_80FloatRepr ar
  | [F.X87Register n]  <- vs = f x87fir (X87StackRegister n)
  | otherwise                = fail $ "fpUnop: " ++ mnemonic ++ " expecting 1 FP argument, got: " ++ show vs
  where
    go :: forall flt. FloatInfoRepr flt -> F.AddrRef -> m ()
    go sz ar = do l <- mkFPAddr sz <$> getBVAddress ar
                  f sz l

semanticsMap :: Map String SemanticsOp
semanticsMap = mapNoDupFromList "semanticsMap" instrs
  where
    mk :: String
       -> (forall m. Semantics m => (F.LockPrefix, [F.Value], String) -> m ())
       -> (String, SemanticsOp)
    mk = semanticsOp

    instrs :: [(String, SemanticsOp)]
    instrs = [ mk "lea"  $ mkBinop $ \loc (F.VoidMem ar) -> do
                 SomeBV l <- getSomeBVLocation loc
                 -- ensure that the location is at most 64 bits
                 Just LeqProof <- return $ testLeq (loc_width l) n64
                 v <- getBVAddress ar
                 exec_lea l (bvTrunc (loc_width l) v)

              , mk "call" $ mkUnop $ \_ v -> do
                  -- Push value of next instruction
                  old_pc <- get rip
                  push addrRepr old_pc
                  -- Set IP
                  tgt <- getJumpTarget v
                  rip .= tgt

              , mk "imul"   $ \(_, vs, _) ->
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
              , mk "jmp"    $ mkUnop $ \_ v -> do
                  tgt <- getJumpTarget v
                  rip .= tgt
              , mk "cwd"    $ \_ -> exec_cwd
              , mk "cdq"    $ \_ -> exec_cdq
              , mk "cqo"    $ \_ -> exec_cqo
              , mk "movsx"  $ geBinop exec_movsx_d
              , mk "movsxd" $ geBinop exec_movsx_d
              , mk "movzx"  $ geBinop exec_movzx
              , mk "xchg"   $ mkBinop $ \v v' -> do
                  SomeBV l <- getSomeBVLocation v
                  l' <- getBVLocation v' (loc_width l)
                  exec_xchg l l'

              , mk "ret"    $ \(_, vs, _) ->
                  case vs of
                    [] -> do
                      -- Pop IP and jump to it.
                      next_ip <- pop addrRepr
                      rip .= next_ip
                    [F.WordImm off] -> do
                      -- Pop IP and adjust stack pointer.
                      next_ip <- pop addrRepr
                      modify (bvAdd (bvLit n64 off)) rsp
                      -- Set IP
                      rip .= next_ip
                    _ ->
                      fail "Unexpected number of args to ret"
              , mk "cmps"   $ mkBinopPfx $ \pfx loc loc' -> do
                  case (loc, loc') of
                    (F.ByteReg F.SIL,  F.ByteReg F.DIL) -> do
                      exec_cmps (pfx == F.RepZPrefix)   ByteRepVal
                    (F.WordReg F.SI,   F.WordReg F.DI) -> do
                      exec_cmps (pfx == F.RepZPrefix)   WordRepVal
                    (F.DWordReg F.ESI, F.DWordReg F.EDI) -> do
                      exec_cmps (pfx == F.RepZPrefix)  DWordRepVal
                    (F.QWordReg F.RSI, F.QWordReg F.RDI) -> do
                      exec_cmps (pfx == F.RepZPrefix)  QWordRepVal
                    _ -> fail "Bad argument to cmps"

              , mk "movs"  $ mkBinopPfx $ \pfx loc loc' -> do
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

              , mk "stos" $ mkBinopPfx $ \pfx loc loc' -> do
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

              , mk "scas" $ mkBinopPfx $ \pfx loc loc' -> do
                  case (loc, loc') of
                    (F.ByteReg  F.AL,  F.Mem8  (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
                      exec_scas (pfx == F.RepZPrefix) (pfx == F.RepNZPrefix) ByteRepVal
                    (F.WordReg  F.AX,  F.Mem16 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
                      exec_scas (pfx == F.RepZPrefix) (pfx == F.RepNZPrefix) WordRepVal
                    (F.DWordReg F.EAX, F.Mem32 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
                      exec_scas (pfx == F.RepZPrefix) (pfx == F.RepNZPrefix) DWordRepVal
                    (F.QWordReg F.RAX, F.Mem64 (F.Addr_64 F.ES (Just F.RDI) Nothing F.NoDisplacement)) -> do
                      exec_scas (pfx == F.RepZPrefix) (pfx == F.RepNZPrefix) QWordRepVal
                    _ -> error $ "scas given bad addrs " ++ show (loc, loc')

              -- fixed size instructions.  We truncate in the case of
              -- an xmm register, for example
              , mk "addsd"     $ truncateKnownBinop exec_addsd
              , mk "addps"     $ truncateKnownBinop exec_addps
              , mk "addss"     $ truncateKnownBinop exec_addss
              , mk "subss"     $ truncateKnownBinop exec_subss
              , mk "subsd"     $ truncateKnownBinop exec_subsd
              , mk "movsd"     $ mkBinop $ exec_movsd
              , mk "movapd"    $ truncateKnownBinop exec_movapd
              , mk "movaps"    $ truncateKnownBinop exec_movaps
              , mk "movups"    $ truncateKnownBinop exec_movups
              , mk "movupd"    $ truncateKnownBinop exec_movupd
              , mk "movdqa"    $ truncateKnownBinop exec_movdqa
              , mk "movdqu"    $ truncateKnownBinop exec_movdqu
              , mk "movss"     $ mkBinop $ exec_movss
              , mk "mulsd"     $ truncateKnownBinop exec_mulsd
              , mk "mulss"     $ truncateKnownBinop exec_mulss
              , mk "divsd"   $ truncateKnownBinop exec_divsd
              , mk "divss"   $ truncateKnownBinop exec_divss
              , mk "psllw"   $ mkBinopLV (exec_psllx n16)
              , mk "pslld"   $ mkBinopLV (exec_psllx n32)
              , mk "psllq"   $ mkBinopLV (exec_psllx n64)
              , mk "ucomisd" $ truncateKnownBinop exec_ucomisd
              , mk "ucomiss" $ truncateKnownBinop exec_ucomiss
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

              , mk "cvttss2si" $ mkBinop $ \loc val -> do
                  SomeBV l  <- getSomeBVLocation loc
                  v <- truncateBVValue knownNat =<< getSomeBVValue val
                  exec_cvttsd2si l v

              , mk "cvtsi2sd" $ mkBinop $ \loc val -> do
                l <- getBVLocation loc n128
                SomeBV v <- getSomeBVValue val
                exec_cvtsi2sd l v

              , mk "cvtss2sd" $ truncateKnownBinop exec_cvtss2sd
              , mk "cvtsd2ss" $ truncateKnownBinop exec_cvtsd2ss

              , mk "pinsrw"   $ \(_, vs, _) ->
                 case vs of
                   [loc, val, F.ByteImm off] -> do
                     l  <- getBVLocation loc knownNat
                     v  <- truncateBVValue knownNat =<< getSomeBVValue val
                     exec_pinsrw l v off
                   _ ->
                     fail "Impossible number of argument in pinsrw"

              , mk "cmpsd" $ \(_, vs, _) ->
                 case vs of
                   [loc, val, F.ByteImm opcode] -> do
                     l  <- getBVLocation loc knownNat
                     v  <- truncateBVValue knownNat =<< getSomeBVValue val
                     exec_cmpsd l v opcode
                   _ -> fail "Impossible number of argument in cmpsd"

              , mk "andps"   $ knownBinop exec_andps
              , mk "andpd"   $ knownBinop exec_andpd
              , mk "orps"    $ knownBinop exec_orps
              , mk "orpd"    $ knownBinop exec_orpd
              , mk "mulps"   $ knownBinop exec_mulps

              , mk "subps"     $ knownBinop exec_subps
              , mk "unpcklps"  $ knownBinop exec_unpcklps

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
              , unop "bswap" exec_bswap
              , mk "cbw"     $ \_ -> exec_cbw
              , mk "cwde"    $ \_ -> exec_cwde
              , mk "cdqe"    $ \_ -> exec_cdqe
              , mk "clc"     $ \_ -> exec_clc
              , mk "cld"     $ \_ -> exec_cld
              , mk "cmp"     $ binop exec_cmp
              , unop  "dec"  $ exec_dec
              , unopV "div"  $ exec_div
              , mk "hlt"     $ \_ -> exec_hlt
              , unopV "idiv" $ exec_idiv
              , unop  "inc"  $ exec_inc
              , mk "leave"   $ const exec_leave
              , mk "mov"     $ binop exec_mov
              , unopV "mul"  $ exec_mul
              , unop "neg"   $ exec_neg
              , mk "nop"     $ \_ -> return ()
              , unop "not"   $ exec_not
              , mk "or"      $ binop exec_or
              , mk "pause"   $ \_ -> return ()
              , mk "pop"     $ mkUnop $ \_ fval -> do
                  Some (HasRepSize rep l) <- getPopLocation fval
                  val <- pop (repValSizeMemRepr rep)
                  l .= val
              , mk "cmpxchg" $ binop exec_cmpxchg
              , knownUnop "cmpxchg8b" exec_cmpxchg8b
              , mk "push"    $ mkUnop $ \_ val -> do
                  Some (HasRepSize rep v) <- getPushValue val
                  push (repValSizeMemRepr rep) v
              , mk "rol"     $ geBinop exec_rol
              , mk "ror"     $ geBinop exec_ror
              , mk "sahf"    $ \_ -> exec_sahf
              , mk "sbb"     $ binop exec_sbb
              , mk "sar"     $ geBinop exec_sar
              , mk "shl"     $ geBinop exec_shl
              , mk "shr"     $ geBinop exec_shr
              , mk "std"     $ \_ -> df_loc .= true
              , mk "sub"     $ binop exec_sub
              , mk "test"    $ binop exec_test
              , mk "xadd"    $ mkBinopPfxLL (\_ -> exec_xadd)
              , mk "xor"     $ binop exec_xor

              , mk "ud2"     $ \_ -> exception false true UndefinedInstructionError

              -- Primitive instructions
              , mk "syscall" $ const (primitive Syscall)
              , mk "cpuid"   $ \_ -> primitive CPUID
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
              , mk "paddq"   $ binop exec_paddq
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
              , (,) "pshufb" $ SemanticsOp $ \ii -> do
                  case fmap fst (F.iiArgs ii) of
                    [F.XMMReg d, F.XMMReg s2] -> do
                      let d_loc = fullRegister (N.xmmFromFlexdis d)
                      d_val  <- get d_loc
                      s2_val <- get (fullRegister (N.xmmFromFlexdis s2))
                      (d_loc .=) =<< pshufb SIMD_128 d_val s2_val
                    _ -> do
                      fail $ "pshufb only supports 2 XMM registers as arguments.  Found:\n"
                             ++ show ii
              , mk "pshufd"  $ ternop exec_pshufd
              , mk "pslldq"  $ geBinop exec_pslldq
              , mk "lddqu"   $ mkBinop $ \loc val -> do
                  l <- getBVLocation loc n128
                  v <- case val of
                         F.VoidMem a -> readBVAddress a xmmMemRepr
                         _ -> fail "readVoidMem given bad address."
                  exec_lddqu l v
              , mk "palignr" $ ternop exec_palignr

              -- X87 FP instructions
              , mk "fadd"    $ fpUnopOrRegBinop exec_fadd
              , mk "fld"     $ fpUnopV exec_fld
              , mk "fmul"    $ fpUnopOrRegBinop exec_fmul
              , knownUnop "fnstcw" $ exec_fnstcw -- stores to bv memory (i.e., not FP)
              , mk "fst"     $ fpUnop exec_fst
              , mk "fstp"    $ fpUnop exec_fstp
              , mk "fsub"    $ fpUnopOrRegBinop exec_fsub
              , mk "fsubp"   $ fpUnopOrRegBinop exec_fsubp
              , mk "fsubr"   $ fpUnopOrRegBinop exec_fsubr
              , mk "fsubrp"  $ fpUnopOrRegBinop exec_fsubrp
             ] ++ mkConditionals "cmov" (\s f -> semanticsOp s $ binop (exec_cmovcc f))
               ++ mkConditionals "j"    (\s f ->
                    semanticsOp s $ mkUnop $ \_ v ->
                      exec_jcc f =<< getBVValue v knownNat)
               ++ mkConditionals "set"  (\s f ->
                    semanticsOp s $ mkUnop $ \_ v ->
                      exec_setcc f =<< getBVLocation v knownNat)

-- Helpers
x87fir :: FloatInfoRepr X86_80Float
x87fir = X86_80FloatRepr

-- | Return the target of a call or jump instruction.
getJumpTarget :: Semantics m
              => F.Value
              -> m (Value m (BVType 64))
getJumpTarget v =
  case v of
    F.Mem64 ar -> do
      a <- getBVAddress ar
      get (MemoryAddr a qwordMemRepr)
    F.QWordReg r -> get (fullRegister $ N.gpFromFlexdis r)
    F.JumpOffset _ off -> bvAdd (bvLit n64 off) <$> get (fullRegister N.rip)
    _ -> fail "Unexpected argument"

mkTernop :: FullSemantics m
        => (F.Value -> F.Value -> F.Value -> m a)
        -> SemanticsArgs
        -> m a
mkTernop f = mkTernopPfx (\_ -> f)

mkTernopPfx :: FullSemantics m
              => (F.LockPrefix -> F.Value -> F.Value -> F.Value -> m a)
              -> SemanticsArgs
              -> m a
mkTernopPfx f (pfx, vs, mnemonic) =
  case vs of
    [v, v', v''] -> f pfx v v' v''
    _            -> fail $ "mkTernopPfx: " ++ mnemonic ++ ": expecting 3 arguments, got " ++ show (length vs)

mkBinopLV ::  Semantics m
        => (forall n n'. (IsLocationBV m n, 1 <= n')
            => MLocation m (BVType n) -> Value m (BVType n') -> m a)
        -> SemanticsArgs
        -> m a
mkBinopLV f = mkBinop $ \loc val -> do
  SomeBV l <- getSomeBVLocation loc
  SomeBV v <- getSomeBVValue val
  f l v

mkBinopPfxLL ::  Semantics m
        => (forall n. (IsLocationBV m n, 1 <= n) =>
            F.LockPrefix ->  MLocation m (BVType n) -> MLocation m (BVType n) -> m a)
        -> SemanticsArgs -> m a
mkBinopPfxLL f = mkBinopPfx $ \pfx loc loc' -> do
  SomeBV l <- getSomeBVLocation loc
  l'       <- getBVLocation loc' (loc_width l)
  f pfx l l'

-- The location size must be >= the value size.
geBinop :: FullSemantics m
        => (forall n n'. (IsLocationBV m n, 1 <= n', n' <= n)
                       => MLocation m (BVType n) -> Value m (BVType n') -> m ())
        -> SemanticsArgs
        -> m ()
geBinop f = mkBinopLV $ \l v -> do
              Just LeqProof <- return $ testLeq (bv_width v) (loc_width l)
              f l v

truncateKnownBinop :: ( KnownNat n'
                      , 1 <= n'
                      , FullSemantics m
                      )
                   => (MLocation m XMMType -> Value m (BVType n') -> m ())
                   -> SemanticsArgs -> m ()
truncateKnownBinop f = mkBinop $ \loc val -> do
  l <- getBVLocation loc n128
  v <- truncateBVValue knownNat =<< getSomeBVValue val
  f l v

knownBinop :: (KnownNat n, KnownNat n', FullSemantics m)
           => (MLocation m (BVType n) -> Value m (BVType n') -> m ())
           -> SemanticsArgs
           -> m ()
knownBinop f = mkBinop $ \loc val -> do
  l  <- getBVLocation loc knownNat
  v  <- getBVValue val knownNat
  f l v

ternop :: FullSemantics m
       => (forall k n
           .  (IsLocationBV m n, 1 <= k, k <= n)
           => MLocation m (BVType n)
           -> Value m (BVType n)
           -> Value m (BVType k)
           -> m ())
       -> SemanticsArgs -> m ()
ternop f = mkTernop $ \loc val1 val2 -> do
  SomeBV l <- getSomeBVLocation loc
  v1 <- getBVValue val1 (loc_width l)
  SomeBV v2 <- getSomeBVValue val2
  Just LeqProof <- return $ testLeq (bv_width v2) (bv_width v1)
  f l v1 v2

fpUnopV :: forall m
        . Semantics m
        => (forall flt. FloatInfoRepr flt -> Value m (FloatType flt) -> m ())
        -> SemanticsArgs
        -> m ()
fpUnopV f (_, vs, mnemonic)
  | [F.FPMem32 ar]     <- vs = go SingleFloatRepr ar
  | [F.FPMem64 ar]     <- vs = go DoubleFloatRepr ar
  | [F.FPMem80 ar]     <- vs = go X86_80FloatRepr ar
  | [F.X87Register n]  <- vs = get (X87StackRegister n) >>= f x87fir
  | otherwise                = fail $ "fpUnopV: " ++ mnemonic ++ " expecting 1 FP argument, got: " ++ show vs
  where
    go :: forall flt. FloatInfoRepr flt -> F.AddrRef -> m ()
    go sz ar = do v <- getBVAddress ar >>= get . mkFPAddr sz
                  f sz v

fpUnopOrRegBinop :: forall m. Semantics m
                 => (forall flt_d flt_s
                     . FloatInfoRepr flt_d
                     -> MLocation m (FloatType flt_d)
                     -> FloatInfoRepr flt_s
                     -> Value m (FloatType flt_s)
                     -> m ())
                    -> SemanticsArgs
                 -> m ()
fpUnopOrRegBinop f args@(_, vs, _mnemonic)
  | length vs == 1     = fpUnopV (f x87fir (X87StackRegister 0)) args
  | otherwise          = knownBinop (\r r' -> f x87fir r x87fir r') args

-- | This function executes a single instruction.
--
-- We divide instructions into
--   * regular:   those which take arguments of the same, polymorphic, width
--   * irrugular: those which have particular parsing requirements
--   * fixed:     those which have exact sizes known

-- FIXME: do something more interesting here than 'Maybe'
execInstruction :: (FullSemantics m)
                => Value m (BVType 64)
                   -- ^ Next ip address
                -> F.InstructionInstance
                -> Maybe (m ())
execInstruction next ii =
  case M.lookup (F.iiOp ii) semanticsMap of
    Just (SemanticsOp f) -> Just $ do
      rip .= next
      f ii -- (F.iiLockPrefix ii) (F.iiAddrSize ii) (F.iiArgs ii)
    Nothing -> Nothing
