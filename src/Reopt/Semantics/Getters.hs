{-|
Copyright        : (c) Galois, Inc 2015-2017
Maintainer       : Joe Hendrix <jhendrix@galois.com>

This defines operations for mapping flexdis values to Macaw values.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Semantics.Getters
  ( SomeBV(..)
  , getBVAddress
  , readBVAddress
  , getSomeBVLocation
  , getBVLocation
  , getSomeBVValue
  , getBVValue
  , getSignExtendedValue
  , truncateBVValue
  , getJumpTarget
  , FPLocation(..)
  , getFPLocation
  , FPValue(..)
  , getFPValue
  , HasRepSize(..)
  , getAddrRegOrSegment
  , getAddrRegSegmentOrImm
  , readXMMValue
    -- * Reprs
  , byteMemRepr
  , wordMemRepr
  , dwordMemRepr
  , qwordMemRepr
  , xmmMemRepr
  , floatMemRepr
  , doubleMemRepr
  ) where

import           Data.Macaw.CFG (MemRepr(..))
import           Data.Macaw.Memory (Endianness(..))
import           Data.Macaw.Types (FloatType, BVType, n8, n16, n32, n64)
import qualified Data.Macaw.X86.X86Reg as N
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import qualified Flexdis86 as F
import           GHC.TypeLits (KnownNat)

import           Reopt.Semantics.Monad

byteMemRepr :: MemRepr (BVType 8)
byteMemRepr = BVMemRepr (knownNat :: NatRepr 1) LittleEndian

wordMemRepr :: MemRepr (BVType 16)
wordMemRepr = BVMemRepr (knownNat :: NatRepr 2) LittleEndian

dwordMemRepr :: MemRepr (BVType 32)
dwordMemRepr = BVMemRepr (knownNat :: NatRepr 4) LittleEndian

qwordMemRepr :: MemRepr (BVType 64)
qwordMemRepr = BVMemRepr (knownNat :: NatRepr 8) LittleEndian

floatMemRepr :: MemRepr (BVType 32)
floatMemRepr = BVMemRepr (knownNat :: NatRepr 4) LittleEndian

doubleMemRepr :: MemRepr (BVType 64)
doubleMemRepr = BVMemRepr (knownNat :: NatRepr 8) LittleEndian

x87MemRepr :: MemRepr (BVType 80)
x87MemRepr = BVMemRepr (knownNat :: NatRepr 10) LittleEndian

xmmMemRepr :: MemRepr (BVType 128)
xmmMemRepr = BVMemRepr (knownNat :: NatRepr 16) LittleEndian

------------------------------------------------------------------------
-- Getters

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
                 Just (i, r) -> bvTrunc n32 . bvMul (bvLit n32 (toInteger i))
                                <$> get (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
      let offset = uext n64 (base `bvAdd` scale `bvAdd` bvLit n32 (toInteger (F.displacementInt i32)))
      mk_absolute seg offset
    F.IP_Offset_32 _seg _i32                 -> fail "IP_Offset_32"
    F.Offset_32    _seg _w32                 -> fail "Offset_32"
    F.Offset_64    seg w64 -> do
      mk_absolute seg (bvLit n64 (toInteger w64))
    F.Addr_64      seg m_r64 m_int_r64 i32 -> do
      base <- case m_r64 of
                Nothing -> return v0_64
                Just r  -> get (fullRegister $ N.gpFromFlexdis r)
      scale <- case m_int_r64 of
                 Nothing     -> return v0_64
                 Just (i, r) -> bvTrunc n64 . bvMul (bvLit n64 (toInteger i))
                                 <$> get (fullRegister $ N.gpFromFlexdis r)
      let offset = base `bvAdd` scale `bvAdd` bvLit n64 (toInteger i32)
      mk_absolute seg offset
    F.IP_Offset_64 seg i32 -> do
      ip_val <- get (fullRegister N.rip)
      mk_absolute seg (bvAdd (bvLit n64 (toInteger i32)) ip_val)
  where
    v0_64 = bvLit n64 0
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

readBVAddress :: FullSemantics m => F.AddrRef -> MemRepr tp -> m (Value m tp)
readBVAddress ar repr =
  get . (`MemoryAddr` repr) =<< getBVAddress ar

-- | A bitvector value with a width that satisfies `SupportedBVWidth`.
data SomeBV v where
  SomeBV :: SupportedBVWidth n => v (BVType n) -> SomeBV v


-- | Extract the location of a bitvector value.
getSomeBVLocation :: FullSemantics m => F.Value -> m (SomeBV (MLocation m))
getSomeBVLocation v =
  case v of
    F.ControlReg cr  -> pure $ SomeBV $ ControlReg cr
    F.DebugReg dr    -> pure $ SomeBV $ DebugReg dr
    F.MMXReg mmx     -> pure $ SomeBV $ x87reg_mmx $ N.X87_FPUReg mmx
    F.XMMReg xmm     -> pure $ SomeBV $ fullRegister $ N.xmmFromFlexdis xmm
    F.SegmentValue s -> pure $ SomeBV $ SegmentReg s
    F.X87Register i -> mk (X87StackRegister i)
    F.FarPointer _      -> fail "FarPointer"
    -- SomeBV . (`MemoryAddr`   byteMemRepr) <$> getBVAddress ar -- FIXME: what size here?
    F.VoidMem _  -> fail "VoidMem"
    F.Mem8   ar  -> SomeBV . (`MemoryAddr`   byteMemRepr) <$> getBVAddress ar
    F.Mem16  ar  -> SomeBV . (`MemoryAddr`   wordMemRepr) <$> getBVAddress ar
    F.Mem32  ar  -> SomeBV . (`MemoryAddr`  dwordMemRepr) <$> getBVAddress ar
    F.Mem64  ar  -> SomeBV . (`MemoryAddr`  qwordMemRepr) <$> getBVAddress ar
    F.Mem128 ar  -> SomeBV . (`MemoryAddr`    xmmMemRepr) <$> getBVAddress ar
    F.FPMem32 ar -> getBVAddress ar >>= mk . (`MemoryAddr` floatMemRepr)
    F.FPMem64 ar -> getBVAddress ar >>= mk . (`MemoryAddr` doubleMemRepr)
    F.FPMem80 ar -> getBVAddress ar >>= mk . (`MemoryAddr` x87MemRepr)
    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> mk (reg_low8  $ N.gpFromFlexdis r64)
      | Just r64 <- F.is_high_reg r -> mk (reg_high8 $ N.gpFromFlexdis r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r -> mk (reg_low16 (N.gpFromFlexdis $ F.reg16_reg r))
    F.DWordReg r -> mk (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
    F.QWordReg r -> mk (fullRegister $ N.gpFromFlexdis r)
    F.ByteImm  _ -> noImm
    F.WordImm  _ -> noImm
    F.DWordImm _ -> noImm
    F.QWordImm _ -> noImm
    F.JumpOffset{}  -> fail "Jump Offset is not a location."
  where
    noImm :: Monad m => m a
    noImm = fail "Immediate is not a location"
    mk :: (Applicative m, SupportedBVWidth n) => f (BVType n) -> m (SomeBV f)
    mk = pure . SomeBV

-- | Translate a flexdis value to a location with a particular width.
getBVLocation :: FullSemantics m => F.Value -> NatRepr n -> m (MLocation m (BVType n))
getBVLocation l expected = do
  SomeBV v <- getSomeBVLocation l
  case testEquality (loc_width v) expected of
    Just Refl ->
      return v
    Nothing ->
      fail $ "Widths aren't equal: " ++ show (loc_width v) ++ " and " ++ show expected

-- | Return a bitvector value.
getSomeBVValue :: FullSemantics m => F.Value -> m (SomeBV (Value m))
getSomeBVValue v =
  case v of
    F.ByteImm  w        -> return $ SomeBV $ bvLit n8  $ toInteger w
    F.WordImm  w        -> return $ SomeBV $ bvLit n16 $ toInteger w
    F.DWordImm w        -> return $ SomeBV $ bvLit n32 $ toInteger w
    F.QWordImm w        -> return $ SomeBV $ bvLit n64 $ toInteger w
    F.JumpOffset _ off  -> return $ SomeBV $ bvLit n64 $ toInteger off
    _ -> do
      SomeBV l <- getSomeBVLocation v
      SomeBV <$> get l

-- | Translate a flexdis value to a value with a particular width.
getBVValue :: FullSemantics m
           => F.Value
           -> NatRepr n
           -> m (Value m (BVType n))
getBVValue val expected = do
  SomeBV v <- getSomeBVValue val
  case testEquality (bv_width v) expected of
    Just Refl -> return v
    Nothing ->
      fail $ "Widths aren't equal: " ++ show (bv_width v) ++ " and " ++ show expected

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
    F.Mem8   ar   -> mk . (`MemoryAddr`  byteMemRepr) =<< getBVAddress ar
    F.Mem16  ar   -> mk . (`MemoryAddr`  wordMemRepr) =<< getBVAddress ar
    F.Mem32  ar   -> mk . (`MemoryAddr` dwordMemRepr) =<< getBVAddress ar
    F.Mem64  ar   -> mk . (`MemoryAddr` qwordMemRepr) =<< getBVAddress ar
    F.Mem128 ar   -> mk . (`MemoryAddr`   xmmMemRepr) =<< getBVAddress ar

    F.ByteReg  r
      | Just r64 <- F.is_low_reg r  -> mk (reg_low8  $ N.gpFromFlexdis r64)
      | Just r64 <- F.is_high_reg r -> mk (reg_high8 $ N.gpFromFlexdis r64)
      | otherwise                   -> fail "unknown r8"
    F.WordReg  r                    -> mk (reg_low16 (N.gpFromFlexdis $ F.reg16_reg r))
    F.DWordReg r                    -> mk (reg_low32 (N.gpFromFlexdis $ F.reg32_reg r))
    F.QWordReg r                    -> mk (fullRegister $ N.gpFromFlexdis r)
    F.XMMReg r                      -> mk (fullRegister $ N.xmmFromFlexdis r)

    F.ByteImm  i                    -> return $! bvLit out_w (toInteger i)
    F.WordImm  i                    -> return $! bvLit out_w (toInteger i)
    F.DWordImm i                    -> return $! bvLit out_w (toInteger i)
    F.QWordImm i                    -> return $! bvLit out_w (toInteger i)

    _ -> fail $ "getSignExtendedValue given unexpected width: " ++ show v
  where
    -- FIXME: what happens with signs etc?
    mk :: forall u
       .  (1 <= u, KnownNat u)
       => Location (Value m (BVType 64)) (BVType u)
       -> m (Value m (BVType w))
    mk l
      | Just LeqProof <- testLeq (knownNat :: NatRepr u) out_w =
        sext out_w <$> get l
      | otherwise =
        fail $ "getSignExtendedValue given bad value."

truncateBVValue :: (Monad m, IsValue v, 1 <= n)
                => NatRepr n
                -> SomeBV v
                -> m (v (BVType n))
truncateBVValue n (SomeBV v)
  | Just LeqProof <- testLeq n (bv_width v) = do
      return (bvTrunc n v)
  | otherwise =
    fail $ "Widths isn't >=: " ++ show (bv_width v) ++ " and " ++ show n

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
    F.JumpOffset _ off -> bvAdd (bvLit n64 (toInteger off)) <$> get (fullRegister N.rip)
    _ -> fail "Unexpected argument"

------------------------------------------------------------------------
-- Floating point

-- | This describes a floating point value including the type.
data FPLocation m flt = FPLocation (FloatInfoRepr flt) (MLocation m (FloatType flt))

-- | This describes a floating point value including the type.
data FPValue m flt = FPValue (FloatInfoRepr flt) (Value m (FloatType flt))

readFPLocation :: Semantics m => FPLocation m flt -> m (FPValue m flt)
readFPLocation (FPLocation repr l) = FPValue repr <$>  get l

-- | Read an address as a floating point vlaue
getFPAddrLoc :: Semantics m => FloatInfoRepr flt -> F.AddrRef -> m (FPLocation m flt)
getFPAddrLoc repr f_addr = do
  addr <- getBVAddress f_addr
  pure $ FPLocation repr (mkFPAddr repr addr)

-- | Get a floating point value from the argument.
getFPLocation :: Semantics m => F.Value -> m (Some (FPLocation m))
getFPLocation v =
  case v of
    F.FPMem32 ar -> Some <$> getFPAddrLoc SingleFloatRepr ar
    F.FPMem64 ar -> Some <$> getFPAddrLoc DoubleFloatRepr ar
    F.FPMem80 ar -> Some <$> getFPAddrLoc X86_80FloatRepr ar
    F.X87Register n -> pure $ Some $ FPLocation X86_80FloatRepr (X87StackRegister n)
    _ -> fail $ "Bad floating point argument."

-- | Get a floating point value from the argument.
getFPValue :: Semantics m => F.Value -> m (Some (FPValue m))
getFPValue v = getFPLocation v >>= \(Some l) -> Some <$> readFPLocation l

------------------------------------------------------------------------
-- Standard memory values

data HasRepSize f w = HasRepSize { _ppvWidth :: !(RepValSize w)
                                 , _ppvValue :: !(f (BVType w))
                                 }

-- | Gets the location to store the value poped from.
-- These functions only support general purpose registers/addresses and segments.
getAddrRegOrSegment :: forall m . FullSemantics m => F.Value -> m (Some (HasRepSize (MLocation m)))
getAddrRegOrSegment v =
  case v of
    F.SegmentValue s -> pure $ Some $ HasRepSize WordRepVal (SegmentReg s)
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
getAddrRegSegmentOrImm :: forall m . FullSemantics m => F.Value -> m (Some (HasRepSize (Value m)))
getAddrRegSegmentOrImm v =
  case v of
    F.ByteImm  w -> return $ Some $ HasRepSize ByteRepVal  $ bvLit n8  (toInteger w)
    F.WordImm  w -> return $ Some $ HasRepSize WordRepVal  $ bvLit n16 (toInteger w)
    F.DWordImm w -> return $ Some $ HasRepSize DWordRepVal $ bvLit n32 (toInteger w)
    F.QWordImm w -> return $ Some $ HasRepSize QWordRepVal $ bvLit n64 (toInteger w)
    _ -> do
      Some (HasRepSize rep l) <- getAddrRegOrSegment v
      Some . HasRepSize rep <$> get l

------------------------------------------------------------------------
-- SSE

-- | Get a XMM value
readXMMValue :: FullSemantics m => F.Value -> m (Value m (BVType 128))
readXMMValue (F.XMMReg r) = get $ fullRegister $ N.xmmFromFlexdis r
readXMMValue (F.Mem128 a) = readBVAddress a xmmMemRepr
readXMMValue _ = fail "XMM Instruction given unexpected value."
