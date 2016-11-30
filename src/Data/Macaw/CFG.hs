{-|
Module           : Data.Macaw.CFG
Copyright        : (c) Galois, Inc 2015-2016
Maintainer       : Joe Hendrix <jhendrix@galois.com>

Defines data types needed to represent control flow graphs from
machine code.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Macaw.CFG
  ( CFG
  , emptyCFG
  , cfgBlocks
--  , cfgBlockEnds
  , insertBlocksForCode
  , traverseBlocks
  , traverseBlockAndChildren
  , findBlock
  , PrettyCFGConstraints
    -- * Block level declarations
  , BlockLabel(..)
  , isRootBlockLabel
  , rootBlockLabel
  , mkRootBlockLabel
  , Block(..)
    -- * Stmt level declarations
  , Stmt(..)
  , TermStmt(..)
  , Assignment(..)
  , AssignId(..)
  , AssignRhs(..)
    -- * Value
  , Value(..)
  , BVValue
  , valueAsApp
  , valueWidth
  , asBaseOffset
  , asInt64Constant
  , mkLit
  , bvValue
  , ppValueAssignments
  , ppValueAssignmentList
  , App(..)
  -- * App
  , appType
  , appWidth
  , mapApp
  , foldApp
  , traverseApp
  -- * RegState
  , RegState(..)
  , boundValue
  , cmpRegState
  , curIP
  , mkRegState
  , mkRegStateM
  , traverseRegsWith
  , zipWithRegState
  -- * Pretty printing
  , ppApp
  , ppAssignId
  , ppLit
  , ppNat
  , ppValue
  , sexpr
  , sexprA
  , PrettyF(..)
  , PrettyFF(..)
  , PrettyArch(..)
  , PrettyRegValue(..)
    -- * Architecture type families
  , ArchAddr
  , ArchSegmentedAddr
  , ArchFn
  , ArchReg
  , ArchStmt
  , RegAddr
  , RegAddrWidth
    -- ** Classes
  , RegisterInfo(..)
    -- ** Synonyms
  , ArchAddrWidth
  , ArchLabel
  , Data.Macaw.Memory.SegmentedAddr
  ) where

import           Control.Applicative
import           Control.Exception (assert)
import           Control.Lens
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.Int (Int64)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isNothing, catMaybes)
import           Data.Monoid as Monoid
import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some
import           Data.Parameterized.TH.GADT
import           Data.Parameterized.TraversableF
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           GHC.TypeLits
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Macaw.Memory (MemWord, SegmentedAddr(..))
import           Data.Macaw.Types

-- Note:
-- The declarations in this file follow a top-down order, so the top-level
-- definitions should be first.

type Prec = Int

colonPrec :: Prec
colonPrec = 7

plusPrec :: Prec
plusPrec = 6


-- | Class for pretty printing with a precedence field.
class PrettyPrec v where
  prettyPrec :: Int -> v -> Doc

-- | Pretty print over all instances of a type.
class PrettyF (f :: k -> *) where
  prettyF :: f tp -> Doc

-- | Pretty print over a pair of instances
class PrettyFF (f :: j -> k -> *) where
  prettyFF :: f a b -> Doc

-- | Pretty print a document with parens if condition is true
parenIf :: Bool -> Doc -> Doc
parenIf True d = parens d
parenIf False d = d

bracketsep :: [Doc] -> Doc
bracketsep [] = text "{}"
bracketsep (h:l) = vcat $
  [text "{" <+> h]
  ++ fmap (text "," <+>) l
  ++ [text "}"]

------------------------------------------------------------------------
-- BlockLabel

-- | A label used to identify a block.
--
-- The field is the address width.
data BlockLabel w
     -- | A block that came from an address in the code.
   = GeneratedBlock { labelAddr   :: !(SegmentedAddr w)
                    , labelIndex  :: {-# UNPACK #-} !Word64
                    -- ^ A unique identifier for a generated block.
                    }
  deriving Eq

isRootBlockLabel :: BlockLabel w -> Bool
isRootBlockLabel (GeneratedBlock _ w) = w == 0

rootBlockLabel :: BlockLabel w -> BlockLabel w
rootBlockLabel (GeneratedBlock p _) = GeneratedBlock p 0

mkRootBlockLabel :: SegmentedAddr w -> BlockLabel w
mkRootBlockLabel p = GeneratedBlock p 0

instance Ord (BlockLabel w) where
  compare (GeneratedBlock p v) (GeneratedBlock p' v') =
    compare p p' Monoid.<> compare v v'

instance Show (BlockLabel w) where
  showsPrec _ (GeneratedBlock a 0) s = "block_" ++ shows a s
  showsPrec _ (GeneratedBlock a w) s = "subblock_" ++ shows a ("_" ++ shows w s)
  {-# INLINABLE showsPrec #-}

instance Pretty (BlockLabel w) where
  pretty l = text (show l)

-----------------------------------------------------------------------
-- App

-- | App defines builtin operations on values.
data App (f :: Type -> *) (tp :: Type) where

  Mux :: !(NatRepr n)
      -> !(f BoolType)
      -> !(f (BVType n))
      -> !(f (BVType n))
      -> App f (BVType n)

  ----------------------------------------------------------------------
  -- Operations related to concatenating and extending bitvectors.

  -- This returns a 80-bit value where the high 16-bits are all 1s,
  -- and the low 64-bits are the given register.
  MMXExtend :: !(f (BVType 64))
            -> App f (BVType 80)

  -- Concatenate two bitvectors together (low-bits are first)
  ConcatV :: !(NatRepr n)
          -> !(NatRepr m)
          -> !(f (BVType n))
          -> !(f (BVType m))
          -> App f (BVType (n+m))

  -- Get upper half of bitvector
  UpperHalf :: !(NatRepr n)
            -> !(f (BVType (n+n)))
            -> App f (BVType n)

  -- Truncate a bitvector value.
  Trunc :: (1 <= n, n+1 <= m) => !(f (BVType m)) -> !(NatRepr n) -> App f (BVType n)
  -- Signed extension.
  SExt :: (1 <= m, m+1 <= n) => f (BVType m) -> NatRepr n -> App f (BVType n)
  -- Unsigned extension.
  UExt :: (1 <= m, m+1 <= n) => f (BVType m) -> NatRepr n -> App f (BVType n)

  ----------------------------------------------------------------------
  -- Boolean operations

  AndApp :: !(f BoolType) -> !(f BoolType) -> App f BoolType
  OrApp  :: !(f BoolType) -> !(f BoolType) -> App f BoolType
  NotApp :: !(f BoolType) -> App f BoolType

  ----------------------------------------------------------------------
  -- Bitvector operations

  BVAdd :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  BVSub :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Multiply two numbers
  BVMul :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Unsigned division (rounds fractions to zero).
  --
  -- This operation is not defined when the denominator is zero. The
  -- caller should raise a #de exception in this case (see
  -- 'Reopt.Semantics.Implementation.exec_div').
  BVQuot :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Unsigned modulo (rounds fractional results to zero)
  --
  -- See 'BVQuot' for usage.
  BVRem :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Signed division (rounds fractional results to zero).
  --
  -- See 'BVQuot' for usage.
  BVSignedQuot :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Signed modulo (rounds fractional results to zero).
  --
  -- The resulting modulus has the same sign as the quotient and satisfies
  -- the constraint that for all x y where y != 0:
  --   x = (y * BVSignedQuot x y) + BVSignedRem x y
  --
  -- See 'BVQuot' for usage.
  BVSignedRem :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Unsigned less than.
  BVUnsignedLt :: !(f (BVType n)) -> !(f (BVType n)) -> App f BoolType

  -- Unsigned less than or equal.
  BVUnsignedLe :: !(f (BVType n)) -> !(f (BVType n)) -> App f BoolType

  -- Signed less than
  BVSignedLt :: !(f (BVType n)) -> !(f (BVType n)) -> App f BoolType

  -- Signed less than or equal.
  BVSignedLe :: !(f (BVType n)) -> !(f (BVType n)) -> App f BoolType

  -- @BVTestBit x i@ returns true iff bit @i@ of @x@ is true.
  -- 0 is the index of the least-significant bit.
  BVTestBit :: !(f (BVType n)) -> !(f (BVType log_n)) -> App f BoolType

  -- Bitwise complement
  BVComplement :: !(NatRepr n) -> !(f (BVType n)) -> App f (BVType n)
  -- Bitwise and
  BVAnd :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  -- Bitwise or
  BVOr  :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  -- Exclusive or
  BVXor :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Logical left shift (x * 2 ^ n)
  BVShl :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  -- Logical right shift (x / 2 ^ n)
  BVShr :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  -- Arithmetic right shift (x / 2 ^ n)
  BVSar :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Compare for equality.
  BVEq :: !(f (BVType n)) -> !(f (BVType n)) -> App f BoolType

  -- Return true if value contains even number of true bits.
  EvenParity :: !(f (BVType 8)) -> App f BoolType

  -- Reverse the bytes in a bitvector expression.
  ReverseBytes :: !(NatRepr n) -> !(f (BVType n)) -> App f (BVType n)

  -- Add two values and a carry bit to determine if they have an unsigned
  -- overflow.
  UadcOverflows :: !(NatRepr n)
                -> !(f (BVType n))
                -> !(f (BVType n))
                -> !(f BoolType)
                -> App f BoolType
  -- Add two values and a carry bit to determine if they have a signed
  -- overflow.
  SadcOverflows :: !(NatRepr n)
                -> !(f (BVType n))
                -> !(f (BVType n))
                -> !(f BoolType)
                -> App f BoolType

  -- Unsigned subtract with borrow overflow
  UsbbOverflows :: !(NatRepr n)
                -> !(f (BVType n))
                -> !(f (BVType n))
                -> !(f BoolType)
                -> App f BoolType

  -- Signed subtract with borrow overflow
  SsbbOverflows :: !(NatRepr n)
                -> !(f (BVType n))
                -> !(f (BVType n))
                -> !(f BoolType)
                -> App f BoolType


  -- bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  Bsf :: !(NatRepr n) -> !(f (BVType n)) -> App f (BVType n)

  -- bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices greater than return value must be unset.
  Bsr :: !(NatRepr n) -> !(f (BVType n)) -> App f (BVType n)

  ----------------------------------------------------------------------
  -- Floating point operations

  FPIsQNaN :: !(FloatInfoRepr flt)
           -> !(f (FloatType flt))
           -> App f BoolType

  FPIsSNaN :: !(FloatInfoRepr flt)
           -> !(f (FloatType flt))
           -> App f BoolType

  FPAdd :: !(FloatInfoRepr flt)
        -> !(f (FloatType flt))
        -> !(f (FloatType flt))
        -> App f (FloatType flt)

  FPAddRoundedUp :: !(FloatInfoRepr flt)
                 -> !(f (FloatType flt))
                 -> !(f (FloatType flt))
                 -> App f BoolType

  FPSub :: !(FloatInfoRepr flt)
        -> !(f (FloatType flt))
        -> !(f (FloatType flt))
        -> App f (FloatType flt)

  FPSubRoundedUp
    :: !(FloatInfoRepr flt)
    -> !(f (FloatType flt))
    -> !(f (FloatType flt))
    -> App f BoolType

  FPMul :: !(FloatInfoRepr flt)
        -> !(f (FloatType flt))
        -> !(f (FloatType flt))
        -> App f (FloatType flt)

  FPMulRoundedUp :: !(FloatInfoRepr flt)
                 -> !(f (FloatType flt))
                 -> !(f (FloatType flt))
                 -> App f BoolType

  -- Divides two floating point numbers.
  FPDiv :: !(FloatInfoRepr flt)
        -> !(f (FloatType flt))
        -> !(f (FloatType flt))
        -> App f (FloatType flt)

  -- Compare if one floating is strictly less than another.
  FPLt :: !(FloatInfoRepr flt)
       -> !(f (FloatType flt))
       -> !(f (FloatType flt))
       -> App f BoolType

  -- Floating point equality (equates -0 and 0)
  FPEq :: !(FloatInfoRepr flt)
       -> !(f (FloatType flt))
       -> !(f (FloatType flt))
       -> App f BoolType

  FPCvt :: !(FloatInfoRepr flt)
        -> !(f (FloatType flt))
        -> !(FloatInfoRepr flt')
        -> App f (FloatType flt')

  FPCvtRoundsUp :: !(FloatInfoRepr flt)
                -> !(f (FloatType flt))
                -> !(FloatInfoRepr flt')
                -> App f BoolType

  FPFromBV :: !(f (BVType n))
           -> !(FloatInfoRepr flt)
           -> App f (FloatType flt)

  -- Convert a floating point value to a signed integer.
  -- If the conversion is inexact, then the value is truncated to zero.
  -- If the conversion is out of the range of the bitvector, then a floating
  -- point exception should be raised.
  -- If that exception is masked, then this returns -1 (as a signed bitvector).
  TruncFPToSignedBV :: FloatInfoRepr flt
                    -> f (FloatType flt)
                    -> NatRepr n
                    -> App f (BVType n)

-----------------------------------------------------------------------
-- App utilities

-- Force app to be in template-haskell context.
$(pure [])

instance TestEquality f => Eq (App f tp) where
  (==) = \x y -> isJust (testEquality x y)

instance TestEquality f => TestEquality (App f) where
  testEquality = $(structuralTypeEquality [t|App|]
                   [ (DataArg 0                  `TypeApp` AnyType, [|testEquality|])
                   , (ConType [t|NatRepr|]       `TypeApp` AnyType, [|testEquality|])
                   , (ConType [t|FloatInfoRepr|] `TypeApp` AnyType, [|testEquality|])
                   ]
                  )

instance OrdF f => OrdF (App f) where
  compareF = $(structuralTypeOrd [t|App|]
                   [ (DataArg 0                  `TypeApp` AnyType, [|compareF|])
                   , (ConType [t|NatRepr|]       `TypeApp` AnyType, [|compareF|])
                   , (ConType [t|FloatInfoRepr|] `TypeApp` AnyType, [|compareF|])
                   ]
              )

instance OrdF f => Ord (App f tp) where
  compare a b = case compareF a b of LTF -> LT
                                     EQF -> EQ
                                     GTF -> GT

traverseApp :: Applicative m
            => (forall u . f u -> m (g u))
            -> App f tp
            -> m (App g tp)
traverseApp = $(structuralTraversal [t|App|] [])

mapApp :: (forall u . f u -> g u)
       -> App f tp
       -> App g tp
mapApp f m = runIdentity $ traverseApp (return . f) m

foldApp :: Monoid m => (forall u. f u -> m) -> App f tp -> m
foldApp f m = getConst (traverseApp (\f_u -> Const $ f f_u) m)

------------------------------------------------------------------------
-- App pretty printing

sexpr :: String -> [Doc] -> Doc
sexpr nm d = parens (hsep (text nm : d))

sexprA :: Applicative m => String -> [m Doc] -> m Doc
sexprA nm d = sexpr nm <$> sequenceA d

ppNat :: Applicative m => NatRepr n -> m Doc
ppNat n = pure (text (show n))

prettyPure :: (Applicative m, Pretty v) => v -> m Doc
prettyPure = pure . pretty

ppApp :: (forall u . f u -> Doc)
      -> App f tp
      -> Doc
ppApp pp a0 = runIdentity $ ppAppA (Identity . pp) a0

ppAppA :: Applicative m
      => (forall u . f u -> m Doc)
      -> App f tp
      -> m Doc
ppAppA pp a0 =
  case a0 of
    Mux _ c x y -> sexprA "mux" [ pp c, pp x, pp y ]
    MMXExtend e -> sexprA "mmx_extend" [ pp e ]
    ConcatV _ _ x y -> sexprA "concat" [ pp x, pp y ]
    UpperHalf _ x -> sexprA "upper_half" [ pp x ]
    Trunc x w -> sexprA "trunc" [ pp x, ppNat w ]
    SExt x w -> sexprA "sext" [ pp x, ppNat w ]
    UExt x w -> sexprA "uext" [ pp x, ppNat w ]
    AndApp x y -> sexprA "and" [ pp x, pp y ]
    OrApp  x y -> sexprA "or"  [ pp x, pp y ]
    NotApp x   -> sexprA "not" [ pp x ]
    BVAdd _ x y -> sexprA "bv_add" [ pp x, pp y ]
    BVSub _ x y -> sexprA "bv_sub" [ pp x, pp y ]
    BVMul _ x y -> sexprA "bv_mul" [ pp x, pp y ]
    BVQuot _ x y      -> sexprA "bv_uquot" [ pp x, pp y ]
    BVSignedQuot _ x y -> sexprA "bv_squot" [ pp x, pp y ]
    BVRem _ x y       -> sexprA "bv_urem" [ pp x, pp y ]
    BVSignedRem _ x y -> sexprA "bv_srem" [ pp x, pp y ]
    BVUnsignedLt x y  -> sexprA "bv_ult"  [ pp x, pp y ]
    BVUnsignedLe x y  -> sexprA "bv_ule"  [ pp x, pp y ]
    BVSignedLt x y    -> sexprA "bv_slt"  [ pp x, pp y ]
    BVSignedLe x y    -> sexprA "bv_sle"  [ pp x, pp y ]
    BVTestBit x i -> sexprA "bv_testbit" [ pp x, pp i]
    BVComplement _ x -> sexprA "bv_complement" [ pp x ]
    BVAnd _ x y -> sexprA "bv_and" [ pp x, pp y ]
    BVOr  _ x y -> sexprA "bv_or"  [ pp x, pp y ]
    BVXor _ x y -> sexprA "bv_xor" [ pp x, pp y ]
    BVShl _ x y -> sexprA "bv_shl" [ pp x, pp y ]
    BVShr _ x y -> sexprA "bv_shr" [ pp x, pp y ]
    BVSar _ x y -> sexprA "bv_sar" [ pp x, pp y ]
    BVEq x y    -> sexprA "bv_eq" [ pp x, pp y ]
    EvenParity x -> sexprA "even_parity" [ pp x ]
    ReverseBytes _ x -> sexprA "reverse_bytes" [ pp x ]
    UadcOverflows _ x y c -> sexprA "uadc_overflows" [ pp x, pp y, pp c ]
    SadcOverflows _ x y c -> sexprA "sadc_overflows" [ pp x, pp y, pp c ]
    UsbbOverflows _ x y c -> sexprA "usbb_overflows" [ pp x, pp y, pp c ]
    SsbbOverflows _ x y c -> sexprA "ssbb_overflows" [ pp x, pp y, pp c ]
    Bsf _ x -> sexprA "bsf" [ pp x ]
    Bsr _ x -> sexprA "bsr" [ pp x ]

    -- Floating point
    FPIsQNaN rep x          -> sexprA "fpIsQNaN" [ prettyPure rep, pp x ]
    FPIsSNaN rep x          -> sexprA "fpIsSNaN" [ prettyPure rep, pp x ]
    FPAdd rep x y           -> sexprA "fpAdd" [ prettyPure rep, pp x, pp y ]
    FPAddRoundedUp rep x y  -> sexprA "fpAddRoundedUp" [ prettyPure rep, pp x, pp y ]
    FPSub rep x y           -> sexprA "fpSub" [ prettyPure rep, pp x, pp y ]
    FPSubRoundedUp rep x y  -> sexprA "fpSubRoundedUp" [ prettyPure rep, pp x, pp y ]
    FPMul rep x y           -> sexprA "fpMul" [ prettyPure rep, pp x, pp y ]
    FPMulRoundedUp rep x y  -> sexprA "fpMulRoundedUp" [ prettyPure rep, pp x, pp y ]
    FPDiv rep x y           -> sexprA "fpDiv" [ prettyPure rep, pp x, pp y ]
    FPLt rep x y            -> sexprA "fpLt" [ prettyPure rep, pp x, pp y ]
    FPEq rep x y            -> sexprA "fpEq" [ prettyPure rep, pp x, pp y ]
    FPCvt src x tgt         -> sexprA "fpCvt" [ prettyPure src, pp x, prettyPure tgt ]
    FPCvtRoundsUp src x tgt -> sexprA "fpCvtRoundsUp" [ prettyPure src, pp x, prettyPure tgt ]
    FPFromBV x tgt          -> sexprA "fpFromBV" [ pp x, prettyPure tgt ]
    TruncFPToSignedBV _ x w -> sexprA "truncFP_sbv" [ pp x, ppNat w]

------------------------------------------------------------------------
-- appType

appType :: App f tp -> TypeRepr tp
appType a =
  case a of
    Mux w _ _ _ -> BVTypeRepr w
    MMXExtend{} -> knownType
    ConcatV w w' _ _ -> BVTypeRepr (addNat w w')
    UpperHalf w _ -> BVTypeRepr w
    Trunc _ w -> BVTypeRepr w
    SExt _ w -> BVTypeRepr w
    UExt _ w -> BVTypeRepr w

    AndApp{} -> knownType
    OrApp{}  -> knownType
    NotApp{} -> knownType

    BVAdd w _ _ -> BVTypeRepr w
    BVSub w _ _ -> BVTypeRepr w
    BVMul w _ _ -> BVTypeRepr w
    BVQuot w _ _ -> BVTypeRepr w
    BVSignedQuot w _ _ -> BVTypeRepr w
    BVRem w _ _ -> BVTypeRepr w
    BVSignedRem w _ _ -> BVTypeRepr w

    BVUnsignedLt{} -> knownType
    BVUnsignedLe{} -> knownType
    BVSignedLt{} -> knownType
    BVSignedLe{} -> knownType
    BVTestBit{} -> knownType

    BVComplement w _ -> BVTypeRepr w
    BVAnd w _ _ -> BVTypeRepr w
    BVOr  w _ _ -> BVTypeRepr w
    BVXor w _ _ -> BVTypeRepr w
    BVShl w _ _ -> BVTypeRepr w
    BVShr w _ _ -> BVTypeRepr w
    BVSar w _ _ -> BVTypeRepr w
    BVEq _ _ -> knownType
    EvenParity _ -> knownType
    ReverseBytes w _ -> BVTypeRepr w

    UadcOverflows{}  -> knownType
    SadcOverflows{} -> knownType
    UsbbOverflows{} -> knownType
    SsbbOverflows{} -> knownType

    Bsf w _ -> BVTypeRepr w
    Bsr w _ -> BVTypeRepr w

    -- Floating point
    FPIsQNaN _ _ -> knownType
    FPIsSNaN _ _ -> knownType
    FPAdd rep _ _ -> floatTypeRepr rep
    FPAddRoundedUp{} -> knownType
    FPSub rep _ _ -> floatTypeRepr rep
    FPSubRoundedUp{} -> knownType
    FPMul rep _ _ -> floatTypeRepr rep
    FPMulRoundedUp{} -> knownType
    FPDiv rep _ _ -> floatTypeRepr rep
    FPLt{} -> knownType
    FPEq{} -> knownType
    FPCvt _ _ tgt   -> floatTypeRepr tgt
    FPCvtRoundsUp{} -> knownType
    FPFromBV _ tgt  -> floatTypeRepr tgt
    TruncFPToSignedBV _ _ w -> BVTypeRepr w

appWidth :: App f (BVType n) -> NatRepr n
appWidth a =
  case appType a of
    BVTypeRepr n -> n

$(pure [])
------------------------------------------------------------------------
-- Type families for architecture specific components.

-- | Width of register used to store addresses.
type family RegAddrWidth (r :: Type -> *) :: Nat

-- | The value used to store
type RegAddr r = MemWord (RegAddrWidth r)

-- | A type family for architecture specific functions.
--
-- These function may return a value.  They may depend on the current state of
-- the heap, but should not affect the processor state.
--
-- The function may depend on the set of registers defined so far, and the type
-- of the result.
type family ArchFn (arch :: *) :: * -> Type -> *

-- | A type family for defining architecture specific registers that are
-- assigned as part of the language.
type family ArchReg (arch :: *) :: Type -> *

-- | A type family for defining architecture-specific statements that are
-- part of the language.
type family ArchStmt (arch :: *) :: * -> *

-- | The type to use for addresses on the architecutre.
type ArchAddr arch = RegAddr (ArchReg arch)

-- | A segmented addr for a given architecture.
type ArchSegmentedAddr arch = SegmentedAddr (ArchAddrWidth arch)

-- | Number of bits in addreses for architecture.
type ArchAddrWidth arch = RegAddrWidth (ArchReg arch)

type ArchLabel arch = BlockLabel (ArchAddrWidth arch)

-- | Operations on a Arch
class ShowF (ArchReg arch) => PrettyArch arch where
  -- | A function for pretty printing an archFn of a given type.
  ppArchFn :: Applicative m
           => (forall u . Value arch ids u -> m Doc)
              -- ^ Function for pretty printing vlaue.
           -> ArchFn arch ids tp
           -> m Doc

-- | This class provides access to information about registers.
class ( OrdF r
      , ShowF r
      , Integral (MemWord (RegAddrWidth r))
      ) => RegisterInfo r where

  -- | List of all arch registers.
  archRegs :: [Some r]

  -- | The stack pointer register
  sp_reg :: r (BVType (RegAddrWidth r))

  -- | The instruction pointer register
  ip_reg :: r (BVType (RegAddrWidth r))

  -- | The register used to store system call numbers.
  syscall_num_reg :: r (BVType (RegAddrWidth r))

  -- | Registers used for passing system call arguments
  syscallArgumentRegs :: [r (BVType (RegAddrWidth r))]

------------------------------------------------------------------------
-- AssignId

-- | An 'AssignId' is a unique identifier used to identify assignment
-- statements, in a manner similar to SSA (single static assignment)
-- form. 'AssignId's are typed, and also include a type variable @ids@ that
-- intuitively denotes the set of identifiers from which they are drawn.
newtype AssignId (ids :: *) (tp :: Type) = AssignId (Nonce ids tp)

ppAssignId :: AssignId ids tp -> Doc
ppAssignId (AssignId w) = text ("r" ++ show w)

instance TestEquality (AssignId ids) where
  testEquality (AssignId id1) (AssignId id2) = testEquality id1 id2

instance OrdF (AssignId ids) where
  compareF (AssignId id1) (AssignId id2) = compareF id1 id2

instance ShowF (AssignId ids) where
  showF (AssignId n) = show n

instance Show (AssignId ids tp) where
  show (AssignId n) = show n

------------------------------------------------------------------------
-- Value, Assignment, AssignRhs declarations.

-- | A value at runtime.
data Value arch ids tp
   = forall n
   .  (tp ~ BVType n)
   => BVValue !(NatRepr n) !Integer
     -- ^ A constant bitvector
   | ( tp ~ BVType (ArchAddrWidth arch))
   => RelocatableValue !(NatRepr (ArchAddrWidth arch)) !(ArchSegmentedAddr arch)
     -- ^ A value that can be relocated.
   | AssignedValue !(Assignment arch ids tp)
     -- ^ Value from an assignment statement.
   | Initial !(ArchReg arch tp)
     -- ^ Represents the value assigned to the register when the block started.

type BVValue arch ids w = Value arch ids (BVType w)

-- | An assignment consists of a unique location identifier and a right-
-- hand side that returns a value.
data Assignment arch ids tp =
  Assignment { assignId :: !(AssignId ids tp)
             , assignRhs :: !(AssignRhs arch ids tp)
             }

-- | The right hand side of an assignment is an expression that
-- returns a value.
data AssignRhs (arch :: *) ids tp where
  -- An expression that is computed from evaluating subexpressions.
  EvalApp :: !(App (Value arch ids) tp)
          -> AssignRhs arch ids tp

  -- An expression with an undefined value.
  SetUndefined :: (tp ~ BVType n)
               => !(NatRepr n) -- Width of undefined value.
               -> AssignRhs arch ids tp

  -- Read memory at given location.
  ReadMem :: !(Value arch ids (BVType (ArchAddrWidth arch)))
          -> !(TypeRepr tp)
          -> AssignRhs arch ids tp

  -- Call an architecture specific function that returns some result.
  EvalArchFn :: !(ArchFn arch ids tp)
             -> !(TypeRepr tp)
             -> AssignRhs arch ids tp

------------------------------------------------------------------------
-- Type operations on assignment AssignRhs, and Value

instance HasRepr (AssignRhs arch ids) TypeRepr where
  typeRepr rhs =
    case rhs of
      EvalApp a -> appType a
      SetUndefined w -> BVTypeRepr w
      ReadMem _ tp -> tp
      EvalArchFn _ rtp -> rtp

instance ( HasRepr (ArchReg arch) TypeRepr
         )
      => HasRepr (Value arch ids) TypeRepr where

  typeRepr (BVValue w _) = BVTypeRepr w
  typeRepr (RelocatableValue w _) = BVTypeRepr w
  typeRepr (AssignedValue a) = typeRepr (assignRhs a)
  typeRepr (Initial r) = typeRepr r

valueWidth :: ( HasRepr (ArchReg arch) TypeRepr
              )
           => Value arch ids (BVType n) -> NatRepr n
valueWidth v =
  case typeRepr v of
    BVTypeRepr n -> n

$(pure [])

------------------------------------------------------------------------
-- Pretty print Assign, AssignRhs, Value operations

ppLit :: NatRepr n -> Integer -> Doc
ppLit w i =
  text ("0x" ++ showHex i "") <+> text "::" <+> brackets (text (show w))

-- | Pretty print a value.
ppValue :: ShowF (ArchReg arch) => Prec -> Value arch ids tp -> Doc
ppValue p (BVValue w i)     = assert (i >= 0) $ parenIf (p > colonPrec) $ ppLit w i
ppValue p (RelocatableValue _ a) = parenIf (p > plusPrec) $ text (show a)
ppValue _ (AssignedValue a) = ppAssignId (assignId a)
ppValue _ (Initial r)       = text (showF r) PP.<> text "_0"

instance ShowF (ArchReg arch) => PrettyPrec (Value arch ids tp) where
  prettyPrec = ppValue

instance ShowF (ArchReg arch) => Pretty (Value arch ids tp) where
  pretty = ppValue 0

instance ShowF (ArchReg arch) => Show (Value arch ids tp) where
  show = show . pretty

-- | Pretty print an assignment right-hand side using operations parameterized
-- over an application to allow side effects.
ppAssignRhs :: Applicative m
            => (forall u . Value arch ids u -> m Doc)
               -- ^ Function for pretty printing value.
            -> (forall u . ArchFn arch ids u -> m Doc)
               -- ^ Function for pretty printing an architecture-specific function
            -> AssignRhs arch ids tp
            -> m Doc
ppAssignRhs pp _ (EvalApp a) = ppAppA pp a
ppAssignRhs _  _ (SetUndefined w) = pure $ text "undef ::" <+> brackets (text (show w))
ppAssignRhs pp _ (ReadMem a _) = (\d -> text "*" PP.<> d) <$> pp a
ppAssignRhs _ pp (EvalArchFn f _) = pp f

instance ( ShowF (ArchReg arch)
         , PrettyFF (ArchFn arch)
         ) =>
         Pretty (AssignRhs arch ids tp) where
  pretty v = runIdentity $ ppAssignRhs (Identity . ppValue 10) (Identity . prettyFF) v

instance ( ShowF    (ArchReg arch)
         , PrettyFF (ArchFn arch)
         ) =>
         Pretty (Assignment arch ids tp) where
  pretty (Assignment lhs rhs) = ppAssignId lhs <+> text ":=" <+> pretty rhs

$(pure [])

------------------------------------------------------------------------
-- Pretty print a value assignment

-- | Helper type to wrap up a 'Doc' with a dummy type argument; used to put
-- 'Doc's into heterogenous maps in the below
newtype DocF (a :: Type) = DocF Doc

-- | This pretty prints a value's representation while saving the pretty
-- printed repreentation of subvalues in a map.
collectValueRep :: forall arch ids tp
                .  PrettyArch arch
                => Prec
                -> Value arch ids tp
                -> State (MapF (AssignId ids) DocF) Doc
collectValueRep _ (AssignedValue a :: Value arch ids tp) = do
  let lhs = assignId a
  mr <- gets $ MapF.lookup lhs
  when (isNothing mr) $ do
    let ppVal :: forall u . Value arch ids u ->
                 State (MapF (AssignId ids) DocF) Doc
        ppVal = collectValueRep 10
    rhs <- ppAssignRhs ppVal (ppArchFn ppVal) (assignRhs a)
    let d = ppAssignId lhs <+> text ":=" <+> rhs
    modify $ MapF.insert lhs (DocF d)
    return ()
  return $! ppAssignId lhs
collectValueRep p v = return $ ppValue p v

-- | This pretty prints all the history used to create a value.
ppValueAssignments' :: State (MapF (AssignId ids) DocF) Doc -> Doc
ppValueAssignments' m =
  case MapF.elems bindings of
    [] -> rhs
    (Some (DocF h):r) ->
      let first               = text "let" PP.<+> h
          f (Some (DocF b))   = text "    " PP.<> b
       in vcat (first:fmap f r) <$$>
          text " in" PP.<+> rhs
  where (rhs, bindings) = runState m MapF.empty

-- | This pretty prints all the history used to create a value.
ppValueAssignments :: PrettyArch arch => Value arch ids tp -> Doc
ppValueAssignments v = ppValueAssignments' (collectValueRep 0 v)

ppValueAssignmentList :: PrettyArch arch => [Value arch ids tp] -> Doc
ppValueAssignmentList vals =
  ppValueAssignments' $ do
    docs <- mapM (collectValueRep 0) vals
    return $ brackets $ hcat (punctuate comma docs)

$(pure [])

------------------------------------------------------------------------
-- Value equality

instance OrdF (ArchReg arch)
      => OrdF (Value arch ids) where
  compareF (BVValue wx vx) (BVValue wy vy) =
    case compareF wx wy of
      LTF -> LTF
      EQF -> fromOrdering (compare vx vy)
      GTF -> GTF
  compareF BVValue{} _ = LTF
  compareF _ BVValue{} = GTF

  compareF (RelocatableValue _ x) (RelocatableValue _ y) =
    fromOrdering (compare x y)
  compareF RelocatableValue{} _ = LTF
  compareF _ RelocatableValue{} = GTF

  compareF (AssignedValue x) (AssignedValue y) =
    compareF (assignId x) (assignId y)
  compareF AssignedValue{} _ = LTF
  compareF _ AssignedValue{} = GTF

  compareF (Initial r) (Initial r') =
    case compareF r r' of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

instance OrdF (ArchReg arch)
      => TestEquality (Value arch ids) where
  testEquality x y = orderingF_refl (compareF x y)

instance OrdF (ArchReg arch)
      => Eq  (Value arch ids tp) where
  x == y = isJust (testEquality x y)

instance OrdF (ArchReg arch)
      => Ord (Value arch ids tp) where
  compare x y = toOrdering (compareF x y)

instance OrdF (ArchReg arch)
      => EqF (Value arch ids) where
  eqF = (==)

------------------------------------------------------------------------
-- Value operations

mkLit :: NatRepr n -> Integer -> Value arch ids (BVType n)
mkLit n v = BVValue n (v .&. mask)
  where mask = maxUnsigned n

bvValue :: KnownNat n => Integer -> Value arch ids (BVType n)
bvValue i = mkLit knownNat i

valueAsApp :: Value arch ids tp -> Maybe (App (Value arch ids) tp)
valueAsApp (AssignedValue (Assignment _ (EvalApp a))) = Just a
valueAsApp _ = Nothing

asInt64Constant :: Value arch ids (BVType 64) -> Maybe Int64
asInt64Constant (BVValue _ o) = Just (fromInteger o)
asInt64Constant _ = Nothing

asBaseOffset :: Value arch ids (BVType w) -> (Value arch ids (BVType w), Integer)
asBaseOffset x
  | Just (BVAdd _ x_base (BVValue _  x_off)) <- valueAsApp x = (x_base, x_off)
  | otherwise = (x,0)

------------------------------------------------------------------------
-- RegState

-- | This represents the state of the processor registers after some
-- execution.
newtype RegState (r :: k -> *) (f :: k -> *)   = RegState (MapF.MapF r f)
--  deriving (FunctorF, FoldableF)

deriving instance FunctorF (RegState r)
deriving instance FoldableF (RegState r)
instance TraversableF (RegState r) where
  traverseF f (RegState m) = RegState <$> traverseF f m

-- | Traverse the register state with the name of each register and value.
traverseRegsWith :: Applicative m
                 => (forall tp. r tp -> f tp -> m (g tp))
                 -> RegState r f
                 -> m (RegState r g)
traverseRegsWith f (RegState m) = RegState <$> MapF.traverseWithKey f m

-- | Get a register out of the state.
boundValue :: forall r f tp
           .  OrdF r
           => r tp
           -> Simple Lens (RegState r f) (f tp)
boundValue r = lens getter setter
  where getter (RegState m) =
          case MapF.lookup r m of
            Just v -> v
            Nothing -> error "internal error in boundValue given unexpected reg"
        setter (RegState m) v = RegState (MapF.insert r v m)

instance (OrdF r, EqF f) => Eq (RegState r f) where
  s == s' = cmpRegState eqF s s'

cmpRegState :: OrdF r
            => (forall u. f u -> g u -> Bool)
            -> RegState r f
            -> RegState r g
            -> Bool
cmpRegState p (RegState x) (RegState y) = go (MapF.toList x) (MapF.toList y)
  where go [] [] = True
        go [] (_:_) = False
        go (_:_) [] = False
        go (MapF.Pair xk xv:xr) (MapF.Pair yk yv:yr) =
          case testEquality xk yk of
            Nothing -> False
            Just Refl -> p xv yv && go xr yr

--  The value of the current instruction pointer.
curIP :: RegisterInfo r
      => Simple Lens (RegState r f) (f (BVType (RegAddrWidth r)))
curIP = boundValue ip_reg

mkRegStateM :: (RegisterInfo r, Applicative m)
            => (forall tp . r tp -> m (f tp))
            -> m (RegState r f)
mkRegStateM f = RegState . MapF.fromList <$> traverse g archRegs
  where g (Some r) = MapF.Pair r <$> f r

-- Create a pure register state
mkRegState :: RegisterInfo r -- AbsRegState r
           => (forall tp . r tp -> f tp)
           -> RegState r f
mkRegState f = runIdentity (mkRegStateM (return . f))

zipWithRegState :: RegisterInfo r
                => (forall u. f u -> g u -> h u)
                -> RegState r f
                -> RegState r g
                -> RegState r h
zipWithRegState f x y = mkRegState (\r -> f (x ^. boundValue r) (y ^. boundValue r))

------------------------------------------------------------------------
-- Pretty printing RegState

-- | This class provides a way of optionally pretty printing the contents
-- of a register or omitting them.
class PrettyRegValue r (f :: Type -> *) where
  -- | ppValueEq should return a doc if the contents of the given register
  -- should be printed, and Nothing if the contents should be ignored.
  ppValueEq :: r tp -> f tp -> Maybe Doc

instance ( OrdF r
         , PrettyRegValue r f
         )
      => Pretty (RegState r f) where
  pretty (RegState m) = bracketsep $ catMaybes (f <$> MapF.toList m)
    where f :: MapF.Pair r f -> Maybe Doc
          f (MapF.Pair r v) = ppValueEq r v

instance ( OrdF r
         , PrettyRegValue r f
         )
      => Show (RegState r f) where
  show s = show (pretty s)

instance ( OrdF r
         , ShowF r
         , r ~ ArchReg arch
         )
      => PrettyRegValue r (Value arch ids) where
  ppValueEq r (Initial r')
    | Just _ <- testEquality r r' = Nothing
  ppValueEq r v
    | otherwise   = Just $ text (showF r) <+> text "=" <+> pretty v

------------------------------------------------------------------------
-- Stmt

-- | A statement in our control flow graph language.
data Stmt arch ids
   = forall tp . AssignStmt !(Assignment arch ids tp)
   | forall tp . WriteMem !(BVValue arch ids (ArchAddrWidth arch)) !(Value arch ids tp)
    -- ^ Write to memory at the given location
   | PlaceHolderStmt !([Some (Value arch ids)]) !String
   | Comment !Text
   | ExecArchStmt (ArchStmt arch ids)
     -- ^ Execute an architecture specific statement

instance ( ShowF    (ArchReg arch)
         , PrettyF  (ArchStmt arch)
         , PrettyFF (ArchFn arch)
         )
         => Pretty (Stmt arch ids) where
  pretty (AssignStmt a) = pretty a
  pretty (WriteMem a rhs) = text "*" PP.<> prettyPrec 11 a <+> text ":=" <+> ppValue 0 rhs
  pretty (PlaceHolderStmt vals name) = text ("PLACEHOLDER: " ++ name)
                                       <+> parens (hcat $ punctuate comma
                                                   $ map (viewSome (ppValue 0)) vals)
  pretty (Comment s) = text $ "# " ++ Text.unpack s
  pretty (ExecArchStmt s) = prettyF s


instance ( ShowF (ArchReg arch)
         , PrettyF (ArchStmt arch)
         , PrettyFF (ArchFn arch)
         )
         => Show (Stmt arch ids) where
  show = show . pretty

------------------------------------------------------------------------
-- TermStmt

-- A terminal statement in a block
data TermStmt arch ids
     -- | Fetch and execute the next instruction from the given processor state.
  = FetchAndExecute !(RegState (ArchReg arch) (Value arch ids))
    -- | Branch and execute one block or another.
  | Branch !(Value arch ids BoolType) !(ArchLabel arch) !(ArchLabel arch)
    -- | The syscall instruction.
    -- We model system calls as terminal instructions because from the
    -- application perspective, the semantics will depend on the operating
    -- system.
  | Syscall !(RegState (ArchReg arch) (Value arch ids))

instance ( OrdF (ArchReg arch)
         , ShowF (ArchReg arch)
         , Integral (ArchAddr arch)
         , Show (ArchAddr arch)
         )
      => Pretty (TermStmt arch ids) where
  pretty (FetchAndExecute s) =
    text "fetch_and_execute" <$$>
    indent 2 (pretty s)
  pretty (Branch c x y) =
    text "branch" <+> ppValue 0 c <+> pretty x <+> pretty y
  pretty (Syscall s) =
    text "syscall" <$$>
    indent 2 (pretty s)

------------------------------------------------------------------------
-- Block

-- | A basic block in a control flow graph.
-- Consists of:
-- 1. A label that should uniquely identify the block, equence of
data Block arch ids =
  Block { blockLabel :: !(ArchLabel arch)
        , blockStmts :: !([Stmt arch ids])
          -- ^ List of statements in the block.
--        , blockCache :: !(MapF (App (Value arch ids)) (Assignment arch ids))
          -- ^ This maps applications to the associated assignment.
        , blockTerm :: !(TermStmt arch ids)
          -- ^ The last statement in the block.
        }

instance ( OrdF  (ArchReg arch)
         , ShowF (ArchReg arch)
         , Integral (ArchAddr arch)
         , Show     (ArchAddr arch)
         , PrettyFF  (ArchFn arch)
         , PrettyF   (ArchStmt arch)
         )
      => Pretty (Block arch ids) where
  pretty b = do
    pretty (blockLabel b) PP.<> text ":" <$$>
      indent 2 (vcat (pretty <$> blockStmts b) <$$> pretty (blockTerm b))

------------------------------------------------------------------------
-- CFG

-- | A CFG is a map from all reachable code locations
-- to the block for that code location.
data CFG arch ids
   = CFG { _cfgBlocks :: !(Map (ArchLabel arch) (Block arch ids))
         , _cfgBlockRanges :: !(Map (ArchSegmentedAddr arch) (ArchAddr arch))
           -- ^ Maps each address that is the start of a block
           -- to the size of the block.
         }

-- | Create empty CFG
emptyCFG :: CFG addr ids
emptyCFG = CFG { _cfgBlocks = Map.empty
               , _cfgBlockRanges = Map.empty
               }

cfgBlocks :: Simple Lens (CFG arch ids) (Map (ArchLabel arch) (Block arch ids))
cfgBlocks = lens _cfgBlocks (\s v -> s { _cfgBlocks = v })

cfgBlockRanges :: Simple Lens (CFG arch ids) (Map (ArchSegmentedAddr arch) (ArchAddr arch))
cfgBlockRanges = lens _cfgBlockRanges (\s v -> s { _cfgBlockRanges = v })

{-
cfgBlockEnds :: CFG arch ids -> Set (ArchAddr arch)
cfgBlockEnds g = Set.fromList (Map.elems (_cfgBlockRanges g))
-}

-- | Return block with given label.
findBlock :: Ord (ArchAddr arch) => CFG arch ids -> ArchLabel arch ->
             Maybe (Block arch ids)
findBlock g l = _cfgBlocks g ^. at l

insertBlock :: ( Integral (ArchAddr arch)
               , Ord (ArchAddr arch)
               , Show (ArchAddr arch)
               )
            => Block arch ids
            -> CFG arch ids
            -> CFG arch ids
insertBlock b c = do
  let lbl = blockLabel b
  case findBlock c lbl of
    Just{} -> error $ "Block with label " ++ show (pretty lbl) ++ " already defined."
    Nothing -> c { _cfgBlocks = Map.insert (blockLabel b) b (_cfgBlocks c) }

-- | Inserts blocks for a contiguous region of code.
insertBlocksForCode :: ( Ord (ArchAddr arch)
                       , Integral (ArchAddr arch)
                       , Show     (ArchAddr arch)
                       )
                    => ArchSegmentedAddr arch -- ^ Start of block
                    -> ArchAddr arch -- ^ Size of block
                    -> [Block arch ids]
                    -> CFG arch ids
                    -> CFG arch ids
insertBlocksForCode start size bs cfg =
  let cfg' = cfg & cfgBlockRanges %~ Map.insert start size
   in foldl' (flip insertBlock) cfg' bs

-- | Constraints for pretty printing a 'CFG'.
type PrettyCFGConstraints arch
   = ( OrdF (ArchReg arch)
     , ShowF (ArchReg arch)
     , Integral (ArchAddr arch)
     , Show     (ArchAddr arch)
     , PrettyF  (ArchStmt arch)
     , PrettyFF (ArchFn arch)
     )

instance PrettyCFGConstraints arch
      => Pretty (CFG arch ids) where
  pretty g = vcat (pretty <$> Map.elems (_cfgBlocks g))

-- FIXME: refactor to be more efficient
-- FIXME: not a Traversal, more like a map+fold
traverseBlocks :: Ord (ArchAddr arch)
               => CFG arch ids
               -> ArchLabel arch
               -> (Block arch ids -> a)
               -> (a -> a -> a -> a)
               -> a
traverseBlocks cfg root f merge = go root
  where
    go l = case findBlock cfg l of
             Nothing -> error $ "label not found"
             Just b  ->
               let v = f b
                in case blockTerm b of
                     Branch _ lb rb -> merge (go lb) v (go rb)
                     _              -> v

-- | As for traverseBlocks but starting from a block in the cfg, not
-- an address
traverseBlockAndChildren :: Ord (ArchAddr arch)
                         => CFG arch ids
                         -> Block arch ids
                         -> (Block arch ids -> a)
                            -- Maps a block to a value
                         -> (Block arch ids -> a -> a -> a)
                            -- Maps results from to sub-blocks together.
                         -> a
traverseBlockAndChildren cfg b0 f merge = goBlock b0
  where
    goBlock b = case blockTerm b of
                 Branch _ lb rb -> merge b (go lb) (go rb)
                 _              -> f b

    go l = case _cfgBlocks cfg ^. at l of
            Nothing -> error $ "label not found"
            Just b  -> goBlock b
