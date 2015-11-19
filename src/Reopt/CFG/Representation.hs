------------------------------------------------------------------------
-- |
-- Module           : Reopt.CFG.Representation
-- Description      : Defines basic data types used for representing Reopt CFG.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This defines the data types needed to represent X86 control flow graps.
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.CFG.Representation
  ( CFG
  , emptyCFG
  , cfgBlocks
  , cfgBlockEnds
  , insertBlocksForCode
  , traverseBlocks
  , traverseBlockAndChildren
  , findBlock
    -- * Block level declarations
  , BlockLabel(..)
  , isRootBlockLabel
  , rootBlockLabel
  , mkRootBlockLabel
  , Block(..)
  , CodeAddr
  , hasCallComment
  , hasRetComment
    -- * Stmt level declarations
  , Stmt(..)
  , StmtLoc(..)
  , TermStmt(..)
  , Assignment(..)
  , assignmentType
  , AssignId
  , AssignRhs(..)
    -- * Value
  , Value(..)
  , BVValue
  , valueAsApp
  , valueType
  , valueWidth
  , asBaseOffset
  , asInt64Constant
  , asStackAddrOffset
  , mkLit
  , bvValue
  , ppValueAssignments
  , App(..)
  -- * App
  , appType
  , appWidth
  , mapApp
  , foldApp
  , traverseApp
  -- * Pretty printing
  , ppApp
  , ppAssignId
  , ppLit
  , ppNat
  , ppValue
  , sexpr
    -- * X86State
  , module Reopt.Machine.X86State
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
import           Data.Maybe (isNothing)
import           Data.Monoid as Monoid
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           GHC.TypeLits
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import           Data.Parameterized.Some
import qualified Reopt.Machine.StateNames as N
import           Reopt.Machine.Types
import           Reopt.Machine.X86State
import           Reopt.Utils.PrettyPrint

-- Note:
-- The declarations in this file follow a top-down order, so the top-level
-- definitions should be first.

type Prec = Int

colonPrec :: Prec
colonPrec = 5

-- | An address of a code location.
--
-- This is currently just a Word64, but we may need to switch to a
-- structured representation once dynamic libraries are supported.
type CodeAddr = Word64

------------------------------------------------------------------------
-- BlockLabel

-- | A label used to identify a block.
data BlockLabel
     -- | A block that came from an address in the code.
   = GeneratedBlock { labelAddr   :: {-# UNPACK #-} !CodeAddr
                    , labelIndex  :: {-# UNPACK #-} !Word64
                    -- ^ A unique identifier for a generated block.
                    }
  deriving Eq

isRootBlockLabel :: BlockLabel -> Bool
isRootBlockLabel (GeneratedBlock _ w) = w == 0

rootBlockLabel :: BlockLabel -> BlockLabel
rootBlockLabel (GeneratedBlock p _) = GeneratedBlock p 0

mkRootBlockLabel :: Word64 -> BlockLabel
mkRootBlockLabel p = GeneratedBlock p 0

instance Ord BlockLabel where
  compare (GeneratedBlock p v) (GeneratedBlock p' v') =
    compare p p' Monoid.<> compare v v'

instance Show BlockLabel where
  showsPrec _ (GeneratedBlock a 0) s = "block_" ++ showHex a s
  showsPrec _ (GeneratedBlock a w) s = "subblock_" ++ showHex a ("_" ++ shows w s)

instance Pretty BlockLabel where
  pretty l = text (show l)

------------------------------------------------------------------------
-- Loctions a statement may need to read or write to.

data StmtLoc a tp where
  MemLoc     :: !a -> TypeRepr tp -> StmtLoc a tp
  ControlLoc :: !(N.RegisterName 'N.Control) -> StmtLoc a (BVType 64)
  DebugLoc   :: !(N.RegisterName 'N.Debug)   -> StmtLoc a (BVType 64)
  FS :: StmtLoc a (BVType 16)
  GS :: StmtLoc a (BVType 16)

  -- X87 precision control field.  Values are:
  -- 00 Single Precision (24 bits)
  -- 01 Reserved
  -- 10 Double Precision (53 bits)
  -- 11 Double Extended Precision (64 bits)
  X87_PC :: StmtLoc a (BVType 2)

  -- X87 rounding control field.  Values are:
  --
  -- 00 Round to nearest (even)
  -- Rounded result is the closest to the infinitely precise result. If two
  -- values are equally close, the result is the even value (that is, the one
  -- with the least-significant bit of zero). Default
  --
  -- 01 Round down (toward −∞)
  -- Rounded result is closest to but no greater than the infinitely precise result.
  --
  -- 10 Round up (toward +∞)
  -- Rounded result is closest to but no less than the infinitely precise result.
  --
  -- 11 Round toward zero (Truncate)
  -- Rounded result is closest to but no greater in absolute value than the
  -- infinitely precise result.
  X87_RC :: StmtLoc a (BVType 2)


stmtLocType :: StmtLoc a tp -> TypeRepr tp
stmtLocType (MemLoc _ tp) = tp
stmtLocType ControlLoc{} = knownType
stmtLocType DebugLoc{}   = knownType
stmtLocType FS = knownType
stmtLocType GS = knownType
stmtLocType X87_PC = knownType
stmtLocType X87_RC = knownType

class PrettyPrec v where
  prettyPrec :: Int -> v -> Doc

instance PrettyPrec a => Pretty (StmtLoc a tp) where
  pretty (MemLoc a _) = text "*" PP.<> prettyPrec 11 a
  pretty (ControlLoc r) = text (show r)
  pretty (DebugLoc r) = text (show r)
  pretty FS = text "fs"
  pretty GS = text "gs"
  pretty X87_PC = text "x87_pc"
  pretty X87_RC = text "x87_rc"

------------------------------------------------------------------------
-- Stmt

type BVValue n = Value (BVType n)

data Stmt where
  AssignStmt :: !(Assignment tp) -> Stmt
  Write :: !(StmtLoc (BVValue 64) tp) -> Value tp -> Stmt
  MemCopy :: !Integer
             -- ^ Number of bytes to copy at a time (1,2,4,8)
          -> !(BVValue 64)
             -- ^ Number of values to move.
          -> !(BVValue 64)
             -- ^ Start of source buffer.
          -> !(BVValue 64)
             -- ^ Start of destination buffer.
          -> !(BVValue 1)
             -- ^ Flag indicates whether direction of move:
             -- True means we should decrement buffer pointers after each copy.
             -- False means we should increment the buffer pointers after each copy.
          -> Stmt

  MemSet :: BVValue 64
            -- ^ Number of values to assign
         -> BVValue n
            -- ^ Value to assign
         -> BVValue 64
            -- ^ Address to start assigning from.
         -> Stmt

  PlaceHolderStmt :: [Some Value] -> String -> Stmt
  Comment :: !Text -> Stmt

instance Show Stmt where
  show = show . pretty

instance Pretty Stmt where
  pretty (AssignStmt a) = pretty a
  pretty (Write loc rhs) = pretty loc <+> text ":=" <+> ppValue 0 rhs
  pretty (MemCopy sz cnt src dest rev) =
      text "memcopy" <+> parens (hcat $ punctuate comma args)
    where args = [pretty sz, pretty cnt, pretty src, pretty dest, pretty rev]
  pretty (MemSet cnt val dest) =
      text "memset" <+> parens (hcat $ punctuate comma args)
    where args = [pretty cnt, pretty val, pretty dest]
  pretty (PlaceHolderStmt vals name) = text ("PLACEHOLDER: " ++ name)
                                       <+> parens (hcat $ punctuate comma
                                                   $ map (viewSome (ppValue 0)) vals)
  pretty (Comment s) = text $ "# " ++ Text.unpack s

------------------------------------------------------------------------
-- TermStmt

-- A terminal statement in a block
data TermStmt
     -- | Fetch and execute the next instruction from the given processor state.
  = FetchAndExecute !(X86State Value)
    -- | Branch and execute one block or another.
  | Branch !(Value BoolType) !BlockLabel !BlockLabel
    -- | The syscall instruction.
    -- We model system calls as terminal instructions because from the
    -- application perspective, the semantics will depend on the operating
    -- system.
  | Syscall !(X86State Value)


instance Pretty TermStmt where
  pretty (FetchAndExecute s) =
    text "fetch_and_execute" <$$>
    indent 2 (pretty s)
  pretty (Branch c x y) =
    text "branch" <+> ppValue 0 c <+> pretty x <+> pretty y
  pretty (Syscall s) =
    text "syscall" <$$>
    indent 2 (pretty s)

------------------------------------------------------------------------
-- Assignment and AssignRhs declarations.

-- | This should be an identifier that can be used to identify the
-- assignment statement uniquely within the CFG.
type AssignId = Word64

-- | An assignment consists of a unique location identifier and a right-
-- hand side that returns a value.
data Assignment tp = Assignment { assignId :: !AssignId
                                , assignRhs :: !(AssignRhs tp)
                                }

-- | The right hand side of an assignment is an expression that
-- returns a value.
data AssignRhs tp where
  -- An expression that is computed from evaluating subexpressions.
  EvalApp :: !(App Value tp)
          -> AssignRhs tp

  -- An expression with an undefined value.
  SetUndefined :: !(NatRepr n) -- Width of undefined value.
               -> AssignRhs (BVType n)

  Read :: !(StmtLoc (Value (BVType 64)) tp)
       -> AssignRhs tp

  -- Compares to memory regions
  MemCmp :: !Integer
         -- ^ Number of bytes per value.
         -> !(BVValue 64)
         -- ^ Number of values to compare
         -> !(BVValue 64)
         -- ^ Pointer to first buffer.
         -> !(BVValue 64)
         -- ^ Pointer to second buffer.
         -> !(BVValue 1)
         -- ^ Direction flag, False means increasing
         -> AssignRhs (BVType 64)

------------------------------------------------------------------------
-- Assignment

ppAssignId :: AssignId -> Doc
ppAssignId w = text ("r" ++ show w)

assignmentType :: Assignment tp -> TypeRepr tp
assignmentType (Assignment _ rhs) = assignRhsType rhs

instance TestEquality Assignment where
  testEquality x y = orderingF_refl (compareF x y)

instance OrdF Assignment where
  compareF x y =
    case compare (assignId x) (assignId y) of
      LT -> LTF
      GT -> GTF
      EQ ->
        case testEquality (assignRhsType (assignRhs x)) (assignRhsType (assignRhs y)) of
          Just Refl -> EQF
          Nothing -> error "mismatched types"

instance Pretty (Assignment tp) where
  pretty (Assignment lhs rhs) = ppAssignId lhs <+> text ":=" <+> pretty rhs

------------------------------------------------------------------------
-- AssignRhs

ppAssignRhs :: Applicative m
            => (forall u . Value u -> m Doc)
            -> AssignRhs tp
            -> m Doc
ppAssignRhs pp (EvalApp a) = ppAppA pp a
ppAssignRhs _  (SetUndefined w) = pure $ text "undef ::" <+> brackets (text (show w))
ppAssignRhs _  (Read loc) = pure $ pretty loc
ppAssignRhs pp (MemCmp sz cnt src dest rev) = sexprA "memcmp" args
  where args = [pure (pretty sz), pp cnt, pp src, pp dest, pp rev]

instance Pretty (AssignRhs tp) where
  pretty v = runIdentity $ ppAssignRhs (Identity . ppValue 10) v

-- | Returns the type of an assignment rhs.
assignRhsType :: AssignRhs tp -> TypeRepr tp
assignRhsType rhs =
  case rhs of
    EvalApp a -> appType a
    SetUndefined w -> BVTypeRepr w
    Read loc  -> stmtLocType loc
    MemCmp _sz _cnt _src _dest _rev -> knownType

------------------------------------------------------------------------
-- Value

-- | A value at runtime.
data Value tp where
  -- Bitvector value.
  BVValue :: !(NatRepr n) -> Integer -> Value (BVType n)

  -- Value from an assignment statement.
  AssignedValue :: !(Assignment tp) -> Value tp

  Initial :: !(N.RegisterName cl) -> Value (N.RegisterType cl)

instance Eq (Value tp) where
  x == y = isJust (testEquality x y)

instance Ord (Value tp) where
  compare x y = toOrdering (compareF x y)

instance TestEquality Value where
  testEquality x y = orderingF_refl (compareF x y)

instance EqF Value where
  eqF = (==)

instance OrdF Value where
  compareF (BVValue wx vx) (BVValue wy vy) =
    case compareF wx wy of
      LTF -> LTF
      EQF -> fromOrdering (compare vx vy)
      GTF -> GTF
  compareF BVValue{} _ = LTF
  compareF _ BVValue{} = GTF

  compareF (AssignedValue a1) (AssignedValue a2) = compareF a1 a2
  compareF AssignedValue{} _ = LTF
  compareF _ AssignedValue{} = GTF

  compareF (Initial r) (Initial r') =
    case compareF r r' of
      LTF -> LTF
      EQF -> EQF
      GTF -> GTF

valueType :: Value tp -> TypeRepr tp
valueType (BVValue n _) = BVTypeRepr n
valueType (AssignedValue a) = assignmentType a
valueType (Initial r)       = N.registerType r

valueWidth :: Value (BVType n) -> NatRepr n
valueWidth v =
  case valueType v of
    BVTypeRepr n -> n

valueAsApp :: Value tp -> Maybe (App Value tp)
valueAsApp (AssignedValue (Assignment _ (EvalApp a))) = Just a
valueAsApp _ = Nothing

asInt64Constant :: Value (BVType 64) -> Maybe Int64
asInt64Constant (BVValue _ o) = Just (fromInteger o)
asInt64Constant _ = Nothing

asStackAddrOffset :: Value tp -> Maybe (Value (BVType 64))
asStackAddrOffset addr
  | Just (BVAdd _ (Initial base) offset) <- valueAsApp addr
  , Just Refl <- testEquality base N.rsp = do
    Just offset
  | Initial base <- addr
  , Just Refl <- testEquality base N.rsp = do
    Just (BVValue knownNat 0)
  | otherwise =
    Nothing

asBaseOffset :: Value (BVType 64) -> (Value (BVType 64), Integer)
asBaseOffset x
  | Just (BVAdd _ x_base (BVValue _  x_off)) <- valueAsApp x = (x_base, x_off)
  | otherwise = (x,0)

mkLit :: NatRepr n -> Integer -> Value (BVType n)
mkLit n v = BVValue n (v .&. mask)
  where mask = maxUnsigned n

bvValue :: KnownNat n => Integer -> Value (BVType n)
bvValue i = mkLit knownNat i

instance PrettyPrec (Value tp) where
  prettyPrec = ppValue

-- | Pretty print a value.
ppValue :: Prec -> Value tp -> Doc
ppValue p (BVValue w i) = assert (i >= 0) $ parenIf (p > colonPrec) $ ppLit w i
ppValue _ (AssignedValue a) = ppAssignId (assignId a)
ppValue _ (Initial r)       = text (show r) PP.<> text "_0"

ppLit :: NatRepr n -> Integer -> Doc
ppLit w i =
  text ("0x" ++ showHex i "") <+> text "::" <+> brackets (text (show w))

instance Show (Value tp) where
  show = show . pretty

instance Pretty (Value tp) where
  pretty = ppValue 0

instance PrettyRegValue Value where
  ppValueEq r v
    | Just _ <- testEquality v (Initial r) = Nothing
    | otherwise   = Just $ text (show r) <+> text "=" <+> pretty v


collectValueRep :: Prec -> Value tp -> State (Map AssignId Doc) Doc
collectValueRep _ (AssignedValue a) = do
  let lhs = assignId a
  mr <- gets $ Map.lookup lhs
  when (isNothing mr) $ do
    rhs <- ppAssignRhs (collectValueRep 10) (assignRhs a)
    let d = ppAssignId lhs <+> text ":=" <+> rhs
    modify $ Map.insert lhs d
  return $! ppAssignId lhs
collectValueRep p v = return $ ppValue p v

-- | This pretty prints all the history used to create a value.
ppValueAssignments :: Value tp -> Doc
ppValueAssignments v
   | Map.null bindings = rhs
   | otherwise =
     text "let" PP.<+> vcat (Map.elems bindings) <$$>
     text " in" PP.<+> rhs

  where (rhs, bindings) = flip runState Map.empty $
          collectValueRep 0 v

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

appWidth :: App f (BVType n) -> NatRepr n
appWidth a =
  case appType a of
    BVTypeRepr n -> n

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

-----------------------------------------------------------------------
-- App utilities

-- Force app to be in template-haskell context.
$(return [])

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
-- Block

-- | A basic block in a control flow graph.
-- Consists of:
-- 1. A label that should uniquely identify the block, equence of
data Block = Block { blockLabel :: !BlockLabel
                     -- | List of statements in the block.
                   , blockStmts :: !([Stmt])
                     -- | This maps applications to the associated assignment.
                   , blockCache :: !(MapF (App Value) Assignment)
                     -- | The last statement in the block.
                   , blockTerm :: !TermStmt
                   }

instance Pretty Block where
  pretty b = do
    pretty (blockLabel b) PP.<> text ":" <$$>
      indent 2 (vcat (pretty <$> blockStmts b) <$$> pretty (blockTerm b))

-- | Returns true if block has a call comment.
hasCallComment :: Block -> Bool
hasCallComment b = any isCallComment (blockStmts b)
  where isCallComment (Comment s) = "call" `Text.isInfixOf` s
        isCallComment _ = False

-- | Returns true if block has a ret comment.
hasRetComment :: Block -> Bool
hasRetComment b = any isRetComment (blockStmts b)
  where isRetComment (Comment s) = "ret" `Text.isSuffixOf` s
        isRetComment _ = False

------------------------------------------------------------------------
-- CFG

-- | A CFG is a map from all reachable code locations
-- to the block for that code location.
data CFG = CFG { _cfgBlocks :: !(Map BlockLabel Block)
                 -- | Maps each address that is the start of a block
                 -- to the address just past the end of that block.
                 -- Blocks are expected to be contiguous.
               , _cfgBlockRanges :: !(Map CodeAddr CodeAddr)
               }

-- | Create empty CFG
emptyCFG :: CFG
emptyCFG = CFG { _cfgBlocks = Map.empty
               , _cfgBlockRanges = Map.empty
               }

cfgBlocks :: Simple Lens CFG (Map BlockLabel Block)
cfgBlocks = lens _cfgBlocks (\s v -> s { _cfgBlocks = v })

cfgBlockRanges :: Simple Lens CFG (Map CodeAddr CodeAddr)
cfgBlockRanges = lens _cfgBlockRanges (\s v -> s { _cfgBlockRanges = v })

cfgBlockEnds :: CFG -> Set CodeAddr
cfgBlockEnds g = Set.fromList (Map.elems (g^.cfgBlockRanges))

insertBlock :: Block -> CFG -> CFG
insertBlock b c = do
  let lbl = blockLabel b
  case Map.lookup lbl (c^.cfgBlocks) of
    Just{} -> error $ "Block with label " ++ show (pretty lbl) ++ " already defined."
    Nothing -> c & cfgBlocks %~ Map.insert (blockLabel b) b

-- | Inserts blocks for a contiguous region of code.
insertBlocksForCode :: CodeAddr -> CodeAddr -> [Block] -> CFG -> CFG
insertBlocksForCode start end bs = execState $ do
  modify $ \cfg -> foldl' (flip insertBlock) cfg bs
  cfgBlockRanges %= Map.insert start end

-- | Return block with given label.
findBlock :: CFG -> BlockLabel -> Maybe Block
findBlock g l = Map.lookup l (g^.cfgBlocks)

instance Pretty CFG where
  pretty g = vcat (pretty <$> Map.elems (g^.cfgBlocks))

-- FIXME: refactor to be more efficient
-- FIXME: not a Traversal, more like a map+fold
traverseBlocks :: CFG
               -> BlockLabel
               -> (Block -> a)
               -> (a -> a -> a -> a)
               -> a
traverseBlocks cfg root f merge = go root
  where
    go l = case cfg ^. cfgBlocks . at l of
            Nothing -> error $ "label not found"
            Just b  -> let v = f b in
                        case blockTerm b of
                         Branch _ lb rb -> merge (go lb) v (go rb)
                         _              -> v

-- | As for traverseBlocks but starting from a block in the cfg, not
-- an address
traverseBlockAndChildren :: CFG
                         -> Block
                         -> (Block -> a)
                            -- Maps a block to a value
                         -> (Block -> a -> a -> a)
                            -- Maps results from to sub-blocks together.
                         -> a
traverseBlockAndChildren cfg b0 f merge = goBlock b0
  where
    goBlock b = case blockTerm b of
                 Branch _ lb rb -> merge b (go lb) (go rb)
                 _              -> f b

    go l = case cfg ^. cfgBlocks . at l of
            Nothing -> error $ "label not found"
            Just b  -> goBlock b
