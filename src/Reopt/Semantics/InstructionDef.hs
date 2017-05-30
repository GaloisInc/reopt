{-|
Copyright        : (c) Galois, Inc 2015-2017
Maintainer       : Joe Hendrix <jhendrix@galois.com>

This defines the instruction def type, which contains the
semantic definition of a function.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Semantics.InstructionDef
  ( InstructionDef
  , InstructionSemantics(..)
  , defVariadic
  , defConditionals
    -- * Nullary function helper
  , defNullary
    -- * Unary instruction helpers
  , defUnary
  , defUnaryLoc
  , defUnaryKnown
  , defUnaryV
  , defUnaryFPL
  , defUnaryFPV
    -- * Binary instruction helpers
  , defBinary
  , defBinaryLV
  , defBinaryLVpoly
  , defBinaryLVge
  , defBinaryKnown
  , defBinaryXMMV
  , defBinaryLL
  , defFPBinaryImplicit
    -- * Ternary instruction helpers
  , defTernary
  , defTernaryLVV
  ) where

import qualified Flexdis86 as F
import           Data.Macaw.Types
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           GHC.TypeLits (KnownNat)

import           Reopt.Semantics.Conditions
import           Reopt.Semantics.Getters
import           Data.Macaw.X86.Monad

-- This is a wrapper around the semantics of an instruction.
newtype InstructionSemantics
      = InstructionSemantics { _unInstructionSemantics
                               :: forall m. Semantics m
                               => F.InstructionInstance
                               -> m ()
                             }

-- | The information needed to define an instruction semantics.
type InstructionDef = (String, InstructionSemantics)

-- | Create a instruction that potentially takes any number of arguments.
defVariadic :: String
            -> (forall m. Semantics m => F.LockPrefix -> [F.Value] -> m ())
            -> InstructionDef
defVariadic mnemonic f =
  (mnemonic, InstructionSemantics (\ii -> f (F.iiLockPrefix ii) (fst <$> F.iiArgs ii)))

-- | Defien an instruction that expects no arguments.
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

-- | Defines an instruction that expects a single bitvec location as an argument.
defUnaryLoc :: String
            -> (forall m n. FullSemantics m => IsLocationBV m n => MLocation m (BVType n) -> m ())
            -> InstructionDef
defUnaryLoc s f = defUnary s $ \_ val -> do
  SomeBV v <- getSomeBVLocation val
  f v

-- | Defines an instruction that expects a bitvector location with a known fixed width.
defUnaryKnown :: KnownNat n
              => String
              -> (forall m . FullSemantics m => MLocation m (BVType n) -> m ())
              -> InstructionDef
defUnaryKnown s f = defUnary s $ \_ loc -> f =<< getBVLocation loc knownNat

-- | Defines an instruction that expects a single bitvec value as an argument.
defUnaryV :: String
          -> (forall m n. (FullSemantics m, IsLocationBV m n) => Value m (BVType n) -> m ())
          -> InstructionDef
defUnaryV s f =  defUnary s $ \_ val -> do
  SomeBV v <- getSomeBVValue val
  f v

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

-- | Define an instruction that expects two arguments.
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

-- | Define an instruction from a function with fixed widths kmown at compile time/.
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

-- | Define a function that takes either two floating point arguments.
--
-- If only one argument is provided, it is used as the second argument,
-- and the first argument is implicitly the top of the floating point stack.
defFPBinaryImplicit :: String
                   -> (forall m flt_d flt_s
                       .  Semantics m
                       => FloatInfoRepr flt_d
                       -> MLocation m (FloatType flt_d)
                       -> FloatInfoRepr flt_s
                       -> Value m (FloatType flt_s)
                       -> m ())
                   -> InstructionDef
defFPBinaryImplicit mnem f = defVariadic mnem $ \_ vs -> do
  case vs of
    [v] -> do
      Some (FPValue repr val) <- getFPValue v
      f X86_80FloatRepr (X87StackRegister 0) repr val
    [loc, val] -> do
      l  <- getBVLocation loc knownNat
      v  <- getBVValue val knownNat
      f X86_80FloatRepr l X86_80FloatRepr v
    _ -> do
      fail $ "deFPBinImplicit " ++ mnem ++ ": expecting 2 arguments, got " ++ show (length vs)

-- | Define an instruction that expects three arguments.
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

-- | This generates a list of instruction definitinos -- one for each conditional predicate.
defConditionals :: String
                -> (String
                    -> (forall m. Semantics m => m (Value m BoolType))
                    -> InstructionDef)
                -> [InstructionDef]
defConditionals pfx mkop = mk <$> conditionalDefs
  where
    mk :: (String, ConditionalDef) -> InstructionDef
    mk (suffix, ConditionalDef sop) = mkop (pfx ++ suffix) sop
