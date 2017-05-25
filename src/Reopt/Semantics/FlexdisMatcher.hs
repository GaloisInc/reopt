{-|
Copyright        : (c) Galois, Inc 2015-2017
Maintainer       : Simon Winwood <sjw@galois.com>

This contains a function "execInstruction" that steps a single Flexdis86
instruction.
-}
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
import           Reopt.Semantics.InstructionDef
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
    F.X87Register n -> pure $ Some $ FPLocation x87fir (X87StackRegister n)
    _ -> fail $ "Bad floating point argument."

-- | Get a floating point value from the argument.
getFPValue :: Semantics m => F.Value -> m (Some (FPValue m))
getFPValue v = getFPLocation v >>= \(Some l) -> Some <$> readFPLocation l

------------------------------------------------------------------------
-- SemanticsOp


mkConditionals :: String
               -> (String
                   -> (forall m. Semantics m => m (Value m BoolType))
                   -> InstructionDef)
               -> [InstructionDef]
mkConditionals pfx mkop = mk <$> conditionalDefs
  where
    mk :: (String, ConditionalDef) -> InstructionDef
    mk (suffix, ConditionalDef sop) = mkop (pfx ++ suffix) sop

defNullary :: String
           -> (forall m . FullSemantics m => m ())
           -> InstructionDef
defNullary mnem f = defVariadic mnem (\_ _ -> f)

-- | Define an instruction that expects a single argument
defUnary :: String
            -- ^ Instruction mnemonic
         -> (forall m . FullSemantics m => F.LockPrefix -> F.Value -> m ())
             -- ^ Sementic definition
         -> InstructionDef
defUnary mnem f = defVariadic mnem $ \pfx vs ->
  case vs of
    [v]   -> f pfx v
    _     -> fail $ "defUnary: " ++ mnem ++ " expecting 1 arguments, got " ++ show (length vs)

-- | This defines an instruction that expects a single bitvec location as an argument.
defUnaryLoc :: String
            -> (forall m n. FullSemantics m => IsLocationBV m n => MLocation m (BVType n) -> m ())
            -> InstructionDef
defUnaryLoc s f = defUnary s $ \_ val -> do
  SomeBV v <- getSomeBVLocation val
  f v

defUnaryKnown :: KnownNat n
          => String
          -> (forall m . FullSemantics m => MLocation m (BVType n) -> m ())
          -> InstructionDef
defUnaryKnown s f = defUnary s $ \_ loc -> f =<< getBVLocation loc knownNat

defUnaryV :: String
      -> (forall m n. (FullSemantics m, IsLocationBV m n) => Value m (BVType n) -> m ())
      -> InstructionDef
defUnaryV s f =  defUnary s $ \_ val -> do
  SomeBV v <- getSomeBVValue val
  f v

defBinary :: String
          -> (forall m . FullSemantics m => F.LockPrefix -> F.Value -> F.Value -> m ())
          -> InstructionDef
defBinary mnem f = defVariadic mnem $ \pfx vs ->
  case vs of
    [v, v']   -> f pfx v v'
    _         -> fail $ "defBinary: " ++ mnem ++ ": expecting 2 arguments, got " ++ show (length vs)

defBinaryLV :: String
      -> (forall m n. (FullSemantics m, IsLocationBV m n) => MLocation m (BVType n) -> Value m (BVType n) -> m ())
      -> InstructionDef
defBinaryLV mnem f = defBinary mnem $ \_ loc val -> do
  SomeBV l <- getSomeBVLocation loc
  v <- getSignExtendedValue val (loc_width l)
  f l v

-- | This defines a instruction that expects a location and a value that may have
-- differing widths
defBinaryLVpoly :: String
                 -> (forall m n n'
                    . (Semantics m, IsLocationBV m n, 1 <= n')
                    => MLocation m (BVType n) -> Value m (BVType n') -> m ())
                 -> InstructionDef
defBinaryLVpoly mnem f = defBinary mnem $ \_ loc val -> do
  SomeBV l <- getSomeBVLocation loc
  SomeBV v <- getSomeBVValue val
  f l v

-- | This defines a instruction that expects a location and a value that may have
-- differing widths, but the location must be larger than the value.
defBinaryLVge :: String
        -> (forall m n n'. (FullSemantics m, IsLocationBV m n, 1 <= n', n' <= n)
                       => MLocation m (BVType n) -> Value m (BVType n') -> m ())
        -> InstructionDef
defBinaryLVge mnem f = defBinaryLVpoly mnem $ \l v -> do
  Just LeqProof <- return $ testLeq (bv_width v) (loc_width l)
  f l v

defBinaryKnown :: (KnownNat n, KnownNat n')
           => String
           -> (forall m . FullSemantics m => MLocation m (BVType n) -> Value m (BVType n') -> m ())
           -> InstructionDef
defBinaryKnown mnem f = defBinary mnem $ \_ loc val -> do
  l  <- getBVLocation loc knownNat
  v  <- getBVValue val knownNat
  f l v

defBinaryXMMV :: ( KnownNat n
                    , 1 <= n
                    )
                 => String
                 -> (forall m . FullSemantics m => MLocation m XMMType -> Value m (BVType n) -> m ())
                 -> InstructionDef
defBinaryXMMV mnem f = defBinary mnem $ \_ loc val -> do
  l <- getBVLocation loc n128
  v <- truncateBVValue knownNat =<< getSomeBVValue val
  f l v

defBinaryLL :: String
          -> (forall m n. (Semantics m, IsLocationBV m n, 1 <= n)
             => F.LockPrefix
             ->  MLocation m (BVType n) -> MLocation m (BVType n) -> m ())
          -> InstructionDef
defBinaryLL mnem f = defBinary mnem $ \pfx loc loc' -> do
  SomeBV l <- getSomeBVLocation loc
  l'       <- getBVLocation loc' (loc_width l)
  f pfx l l'

defTernary :: String
           -> (forall m . FullSemantics m => F.LockPrefix -> F.Value -> F.Value -> F.Value -> m ())
           -> InstructionDef
defTernary mnem f = defVariadic mnem $ \pfx vs -> do
  case vs of
    [v, v', v''] -> f pfx v v' v''
    _ ->
      fail $ "defTernary: " ++ mnem ++ ": expecting 3 arguments, got " ++ show (length vs)

defTernaryLVV :: String
              -> (forall m k n
                  . (FullSemantics m, IsLocationBV m n, 1 <= k, k <= n)
                  => MLocation m (BVType n)
                  -> Value m (BVType n)
                  -> Value m (BVType k)
                  -> m ())
              -> InstructionDef
defTernaryLVV mnem f = defTernary mnem $ \_ loc val1 val2 -> do
  SomeBV l <- getSomeBVLocation loc
  v1 <- getBVValue val1 (loc_width l)
  SomeBV v2 <- getSomeBVValue val2
  Just LeqProof <- return $ testLeq (bv_width v2) (bv_width v1)
  f l v1 v2

defUnaryFPL :: String
            -> (forall m flt. Semantics m => FloatInfoRepr flt -> MLocation m (FloatType flt) -> m ())
            -> InstructionDef
defUnaryFPL mnem f = defUnary mnem $ \_ v -> do
  Some (FPLocation repr loc) <- getFPLocation v
  f repr loc

defUnaryFPV :: String
        -> (forall m flt. Semantics m => FloatInfoRepr flt -> Value m (FloatType flt) -> m ())
        -> InstructionDef
defUnaryFPV mnem f = defUnary mnem $ \_ v -> do
  Some (FPValue repr val) <- getFPValue v
  f repr val

-- | Define a function that takes either two floating point arguements.
--
-- If only one argument is provided, it is used as the second argument,
-- and the first argument is implicitly the top of the floating point stack.
deFPBinImplicit :: String
                 -> (forall m flt_d flt_s
                     .  Semantics m
                     => FloatInfoRepr flt_d
                     -> MLocation m (FloatType flt_d)
                     -> FloatInfoRepr flt_s
                     -> Value m (FloatType flt_s)
                     -> m ())
                 -> InstructionDef
deFPBinImplicit mnem f = defVariadic mnem $ \_ vs -> do
  case vs of
    [v] -> do
      Some (FPValue repr val) <- getFPValue v
      f x87fir (X87StackRegister 0) repr val
    [loc, val] -> do
      l  <- getBVLocation loc knownNat
      v  <- getBVValue val knownNat
      f x87fir l x87fir v
    _ -> do
      fail $ "deFPBinImplicit " ++ mnem ++ ": expecting 2 arguments, got " ++ show (length vs)

------------------------------------------------------------------------
-- Instruction semantics

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


mapNoDupFromList :: (Ord k, Show k) => String -> [(k,v)] -> Map k v
mapNoDupFromList nm = foldl' ins M.empty
  where ins m (k,v) = M.insertWith (\_ _ -> error (e_msg k)) k v m
        e_msg k = nm ++ " contains duplicate entries for " ++ show k ++ "."

semanticsMap :: Map String InstructionSemantics
semanticsMap = mapNoDupFromList "semanticsMap" instrs
  where

    instrs :: [InstructionDef]
    instrs = [ defBinary "lea" $ \_ loc (F.VoidMem ar) -> do
                 SomeBV l <- getSomeBVLocation loc
                 -- ensure that the location is at most 64 bits
                 Just LeqProof <- return $ testLeq (loc_width l) n64
                 v <- getBVAddress ar
                 exec_lea l (bvTrunc (loc_width l) v)

              , defUnary "call" $ \_ v -> do
                  -- Push value of next instruction
                  old_pc <- get rip
                  push addrRepr old_pc
                  -- Set IP
                  tgt <- getJumpTarget v
                  rip .= tgt

              , defVariadic "imul"   $ \_ vs ->
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
              , defUnary   "jmp" $ \_ v -> do
                  tgt <- getJumpTarget v
                  rip .= tgt
              , defNullary "cwd" exec_cwd
              , defNullary "cdq" exec_cdq
              , defNullary "cqo" exec_cqo
              , defBinaryLVge "movsx"  exec_movsx_d
              , defBinaryLVge "movsxd" exec_movsx_d
              , defBinaryLVge "movzx"  exec_movzx
              , defBinary "xchg" $ \_ v v' -> do
                  SomeBV l <- getSomeBVLocation v
                  l' <- getBVLocation v' (loc_width l)
                  exec_xchg l l'
              , defVariadic "ret"    $ \_ vs ->
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
              , defBinary "cmps" $ \pfx loc loc' -> do
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

              , defBinary "movs" $ \pfx loc loc' -> do
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

              , defBinary "stos" $ \pfx loc loc' -> do
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

              , defBinary "scas" $ \pfx loc loc' -> do
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
              , defBinaryXMMV "addsd" exec_addsd
              , defBinaryXMMV "addps" exec_addps
              , defBinaryXMMV "addss" exec_addss
              , defBinaryXMMV "subss" exec_subss
              , defBinaryXMMV "subsd" exec_subsd
              , def_movsd
              , defBinaryXMMV "movapd" exec_movapd
              , defBinaryXMMV "movaps" exec_movaps
              , defBinaryXMMV "movups" exec_movups
              , defBinaryXMMV "movupd" exec_movupd
              , defBinaryXMMV "movdqa" exec_movdqa
              , defBinaryXMMV "movdqu" exec_movdqu
              , def_movss
              , defBinaryXMMV "mulsd" exec_mulsd
              , defBinaryXMMV "mulss" exec_mulss
              , defBinaryXMMV "divsd" exec_divsd
              , defBinaryXMMV "divss" exec_divss
              , defBinaryLVpoly "psllw" $ exec_psllx n16
              , defBinaryLVpoly "pslld" $ exec_psllx n32
              , defBinaryLVpoly "psllq" $ exec_psllx n64
              , defBinaryXMMV "ucomisd" exec_ucomisd
              , defBinaryXMMV "ucomiss" exec_ucomiss
              , defBinary "xorpd" $ \_ loc val -> do
                  l <- getBVLocation loc n128
                  v <- readXMMValue val
                  modify (`bvXor` v) l
              , defBinary "xorps" $ \_ loc val -> do
                  l <- getBVLocation loc n128
                  v <- readXMMValue val
                  modify (`bvXor` v) l

              , defBinary "cvtsi2ss" $ \_ loc val -> do
                  l <- getBVLocation loc n128
                  SomeBV v <- getSomeBVValue val
                  exec_cvtsi2ss l v

              , defBinary "cvttsd2si" $ \_ loc val -> do
                  SomeBV l  <- getSomeBVLocation loc
                  v <- truncateBVValue knownNat =<< getSomeBVValue val
                  exec_cvttsd2si l v

              , defBinary "cvtss2si" $ \_ loc val -> do
                  SomeBV l  <- getSomeBVLocation loc
                  v <- truncateBVValue knownNat =<< getSomeBVValue val
                  exec_cvttsd2si l v

              , defBinary "cvtsi2sd" $ \_ loc val -> do
                  l <- getBVLocation loc n128
                  SomeBV v <- getSomeBVValue val
                  exec_cvtsi2sd l v

              , defBinaryXMMV "cvtss2sd" exec_cvtss2sd
              , defBinaryXMMV "cvtss2ss" exec_cvtsd2ss

              , defTernary "pinsrw" $ \_ loc val imm -> do
                  l  <- getBVLocation loc knownNat
                  v  <- truncateBVValue knownNat =<< getSomeBVValue val
                  case imm of
                    F.ByteImm off -> exec_pinsrw l v off
                    _ -> fail "Bad offset to pinsrw"

              , defTernary "cmpsd" $ \_ loc val imm -> do
                  l  <- getBVLocation loc knownNat
                  v  <- truncateBVValue knownNat =<< getSomeBVValue val
                  case imm of
                    F.ByteImm opcode -> exec_cmpsd l v opcode
                    _ -> fail "Impossible argument in cmpsd"

              , defBinaryKnown "andps" exec_andps
              , defBinaryKnown "andpd" exec_andpd
              , defBinaryKnown "orps"  exec_orps
              , defBinaryKnown "orpd"  exec_orpd
              , defBinaryKnown "mulps" exec_mulps

              , defBinaryKnown "subps"    exec_subps
              , defBinaryKnown "unpcklps" exec_unpcklps

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
              , defUnaryV "div"  $ exec_div
              , defNullary "hlt"    $ exec_hlt
              , defUnaryV "idiv" $ exec_idiv
              , defUnaryLoc "inc"   $ exec_inc
              , defNullary  "leave" $ exec_leave
              , defBinaryLV "mov"  $ exec_mov
              , defUnaryV "mul"  $ exec_mul
              , defUnaryLoc "neg"   $ exec_neg
              , defNullary  "nop"   $ return ()
              , defUnaryLoc "not"   $ exec_not
              , defBinaryLV "or" exec_or
              , defNullary "pause" $ return ()
              , defUnary "pop" $ \_ fval -> do
                  Some (HasRepSize rep l) <- getPopLocation fval
                  val <- pop (repValSizeMemRepr rep)
                  l .= val
              , defBinaryLV "cmpxchg" exec_cmpxchg
              , defUnaryKnown "cmpxchg8b" exec_cmpxchg8b
              , defUnary "push" $ \_ val -> do
                  Some (HasRepSize rep v) <- getPushValue val
                  push (repValSizeMemRepr rep) v
              , defBinaryLVge "rol" exec_rol
              , defBinaryLVge "ror" exec_ror
              , defNullary "sahf" $ exec_sahf
              , defBinaryLV "sbb" exec_sbb
              , defBinaryLVge "sar"  exec_sar
              , defBinaryLVge "shl"  exec_shl
              , defBinaryLVge "shr"  exec_shr
              , defNullary "std" $ df_loc .= true
              , defBinaryLV "sub" exec_sub
              , defBinaryLV "test" exec_test
              , defBinaryLL "xadd" $ \_ x -> exec_xadd x
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
              , defBinaryKnown "movhlps" exec_movhlps
              , defBinaryKnown "movlhps" exec_movlhps
              , defBinaryLV "pmaxub" exec_pmaxub
              , defBinaryLV "pmaxuw" exec_pmaxuw
              , defBinaryLV "pmaxud" exec_pmaxud
              , defBinaryLV "pmaxsb" exec_pmaxsb
              , defBinaryLV "pmaxsw" exec_pmaxsw
              , defBinaryLV "pmaxsd" exec_pmaxsd
              , defBinaryLV "pminub" exec_pminub
              , defBinaryLV "pminuw" exec_pminuw
              , defBinaryLV "pminud" exec_pminud
              , defBinaryLV "pminsb" exec_pminsb
              , defBinaryLV "pminsw" exec_pminsw
              , defBinaryLV "pminsd" exec_pminsd
              , defBinaryLVpoly "pmovmskb" exec_pmovmskb
              , defBinaryLVpoly "movhpd"   exec_movhpd
              , defBinaryLVpoly "movlpd"   exec_movlpd
              , (,) "pshufb" $ InstructionSemantics $ \ii -> do
                  case fmap fst (F.iiArgs ii) of
                    [F.XMMReg d, F.XMMReg s2] -> do
                      let d_loc = fullRegister (N.xmmFromFlexdis d)
                      d_val  <- get d_loc
                      s2_val <- get (fullRegister (N.xmmFromFlexdis s2))
                      (d_loc .=) =<< pshufb SIMD_128 d_val s2_val
                    _ -> do
                      fail $ "pshufb only supports 2 XMM registers as arguments.  Found:\n"
                             ++ show ii
              , defTernaryLVV  "pshufd" exec_pshufd
              , defBinaryLVge "pslldq" exec_pslldq
              , defBinary "lddqu"  $ \_ loc val -> do
                  l <- getBVLocation loc n128
                  v <- case val of
                         F.VoidMem a -> readBVAddress a xmmMemRepr
                         _ -> fail "readVoidMem given bad address."
                  exec_lddqu l v
              , defTernaryLVV "palignr" exec_palignr
              -- X87 FP instructions
              , deFPBinImplicit "fadd"   $ exec_fadd
              , defUnaryFPV      "fld"   $ exec_fld
              , deFPBinImplicit "fmul"   $ exec_fmul
              , defUnaryKnown   "fnstcw" $ exec_fnstcw -- stores to bv memory (i.e., not FP)
              , defUnaryFPL     "fst"    $ exec_fst
              , defUnaryFPL     "fstp"   $ exec_fstp
              , deFPBinImplicit "fsub"   $ exec_fsub
              , deFPBinImplicit "fsubp"  $ exec_fsubp
              , deFPBinImplicit "fsubr"  $ exec_fsubr
              , deFPBinImplicit "fsubrp" $ exec_fsubrp
             ] ++ mkConditionals "cmov" (\s f -> defBinaryLV s (exec_cmovcc f))
               ++ mkConditionals "j"    (\s f ->
                    defUnary s $ \_ v ->
                      exec_jcc f =<< getBVValue v knownNat)
               ++ mkConditionals "set"  (\s f ->
                    defUnary s $ \_ v ->
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
    Just (InstructionSemantics f) -> Just $ do
      rip .= next
      f ii -- (F.iiLockPrefix ii) (F.iiAddrSize ii) (F.iiArgs ii)
    Nothing -> Nothing
