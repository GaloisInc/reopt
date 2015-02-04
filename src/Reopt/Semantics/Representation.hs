------------------------------------------------------------------------
-- |
-- Module           : Reopt.Semantics.Representation
-- Description      : Defines basic data types used for representing Reopt CFG.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This defines the data types needed to represent X86 control flow graps.
------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Semantics.Representation
  ( CFG
  , emptyCFG
  , insertBlock
    -- * Block level declarations
  , BlockLabel(..)
  , Block(..)
  , CodeAddr
    -- * Stmt level declarations
  , Stmt(..)
  , TermStmt(..)
  , Assignment(..)
  , assignmentWidth
  , AssignId
  , AssignRhs(..)
  , assignRhsWidth
    -- * Value
  , Value(..)
  , valueWidth
  , bvValue
  , App(..)
  , appWidth
  , mapApp
  , traverseApp
    -- * X86State
  , X86State(..)
  , curIP
  , reg64Regs
  , mmxRegs
  , xmmRegs
  , flagRegs
  ) where

import Control.Applicative
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Parameterized.NatRepr
import qualified Data.Vector as V
import Data.Word
import GHC.TypeLits

import qualified Flexdis86.InstructionSet as Flexdis86
import Reopt.Semantics.Monad
  ( BoolType
  , DoubleType
  , Type(..)
  , TypeRepr(..)
  )

-- Note:
-- The declarations in this file follow a top-down order, so the top-level
-- definitions should be first.

------------------------------------------------------------------------
-- CFG

-- | A CFG is a map from all reachable code locations
-- to the block for that code location.
newtype CFG = CFG { _cfgBlocks :: Map BlockLabel Block }

-- | Create empty CFG
emptyCFG :: CFG
emptyCFG = CFG { _cfgBlocks = Map.empty }

cfgBlocks :: Simple Lens CFG (Map BlockLabel Block)
cfgBlocks = lens _cfgBlocks (\s v -> s { _cfgBlocks = v })

insertBlock :: Block -> CFG -> CFG
insertBlock b c =
  case Map.lookup (blockLabel b) (c^.cfgBlocks) of
    Just{} -> error "Block with given label already defined."
    Nothing -> c & cfgBlocks %~ Map.insert (blockLabel b) b

------------------------------------------------------------------------
-- BlockLabel

-- | A label used to identify a block.
data BlockLabel
   = DecompiledBlock CodeAddr
     -- ^ A block that came from an address in the code.
   | GeneratedBlock Word64
     -- ^ A unique identifier for a generated block.
  deriving (Eq, Ord)

-- | An address of a code location.
--
-- This is currently just a Word64, but we may need to switch to a
-- structured representation once dynamic libraries are supported.
type CodeAddr = Word64

------------------------------------------------------------------------
-- Block

-- | A basic block in a control flow graph.
-- Consists of:
-- 1. A label that should uniquely identify the block, equence of
data Block = Block { blockLabel :: BlockLabel
                   , blockStmts :: [Stmt]
                     -- | The last statement in the block.
                   , blockTerm :: TermStmt
                   }

------------------------------------------------------------------------
-- Stmt

data Stmt where
  AssignStmt :: !(Assignment tp) -> Stmt
  -- Write to memory contains address followed by value to write.
  WriteMem :: Value (BVType 64) -> Value tp -> Stmt
  -- Write to control register.
  WriteControlReg :: !Flexdis86.ControlReg -> Value (BVType 64) -> Stmt
  -- Write to debug register.
  WriteDebugReg   :: !Flexdis86.DebugReg -> Value (BVType 64) -> Stmt
  -- Read the FS segment register.
  WriteFS :: Value (BVType 64) -> Stmt
  -- Read the GS segment register.
  WriteGS :: Value (BVType 64) -> Stmt

------------------------------------------------------------------------
-- TermStmt

-- A terminal statement in a block
data TermStmt where
  -- Fetch and execute the next instruction from the given processor state.
  FetchAndExecute :: !X86State -> TermStmt

  Branch :: !(Value BoolType)
         -> !BlockLabel
         -> !BlockLabel
         -> TermStmt

------------------------------------------------------------------------
-- X86State

-- | This represents the state of the processor registers after some
-- execution.
data X86State = X86State
     { _curIP  :: !(Value (BVType 64))
     , _reg64Regs :: !(V.Vector (Value (BVType  64)))
     , _mmxRegs   :: !(V.Vector (Value (BVType  64)))
     , _xmmRegs   :: !(V.Vector (Value (BVType 128)))
     , _flagRegs  :: !(V.Vector (Value BoolType))
     }

-- | The value of the current instruction pointer.
curIP :: Simple Lens X86State (Value (BVType 64))
curIP = lens _curIP (\s v -> s { _curIP = v })

-- | Assignments to the 16 general-purpose registers.
reg64Regs :: Simple Lens X86State (V.Vector (Value (BVType 64)))
reg64Regs = lens _reg64Regs (\s v -> s { _reg64Regs = v })

-- | Assignments to the 8 64-bit MMX registers.
mmxRegs :: Simple Lens X86State (V.Vector (Value (BVType 64)))
mmxRegs = lens _mmxRegs (\s v -> s { _mmxRegs = v })

-- | Assignments to the 16 128-bit XMM registers.
xmmRegs :: Simple Lens X86State (V.Vector (Value (BVType 128)))
xmmRegs = lens _xmmRegs (\s v -> s { _xmmRegs = v })

-- | Assignments to the flag registers.
flagRegs :: Simple Lens X86State (V.Vector (Value BoolType))
flagRegs = lens _flagRegs (\s v -> s { _flagRegs = v })

------------------------------------------------------------------------
-- Assignment

-- | This should be an identifier that can be used to identify the
-- assignment statement uniquely within the CFG.
type AssignId = Word64

-- | An assignment consists of a unique location identifier and a right-
-- hand side that returns a value.
data Assignment tp = Assignment !AssignId (AssignRhs tp)

assignmentWidth :: Assignment (BVType n) -> NatRepr n
assignmentWidth (Assignment _ rhs) = assignRhsWidth rhs

-- | The right hand side of an assignment is an expression that
-- returns a value.
data AssignRhs tp where
  -- An expression that is computed from evaluating subexpressions.
  EvalApp :: !(App Value tp)
          -> AssignRhs tp

  -- An expression with an undefined value.
  SetUndefined :: !(NatRepr n) -- Width of undefined value.
               -> AssignRhs (BVType n)

  -- Read a value out of memory.
  ReadAddr :: !(Value (BVType 64)) -> TypeRepr tp -> AssignRhs tp

  -- Read a control register
  ReadControlReg :: !Flexdis86.ControlReg -> AssignRhs (BVType 64)

  -- Read a debug register
  ReadDebugReg :: !Flexdis86.DebugReg -> AssignRhs (BVType 64)

  -- Read the FS segment register.
  ReadFS :: AssignRhs (BVType 64)

  -- Read the GS segment register.
  ReadGS :: AssignRhs (BVType 64)

assignRhsWidth :: AssignRhs (BVType n) -> NatRepr n
assignRhsWidth = undefined -- TODO: implement this.

-----------------------------------------------------------------------
-- App

-- | App defines builtin operations on values.
data App f tp where

  ----------------------------------------------------------------------
  -- Operations related to concatenating and extending bitvectors.

  -- Concatenate two bitvectors together (low-bits are first)
  ConcatV :: {-#UNPACK #-} !(NatRepr n)
          -> !(f (BVType n))
          -> !(f (BVType n))
          -> App f (BVType (n+n))

  -- Get upper half of bitvector
  UpperHalf :: {-# UNPACK #-} !(NatRepr n)
            -> !(f (BVType (n+n)))
            -> App f (BVType n)

  -- Truncate a bitvector value.
  Trunc :: (n <= m) => !(f (BVType m)) -> !(NatRepr n) -> App f (BVType n)
  -- Signed extension.
  SExt :: (1 <= m, m <= n) => f (BVType m) -> NatRepr n -> App f (BVType n)
  -- Unsigned extension.
  UExt :: (m <= n) => f (BVType m) -> NatRepr n -> App f (BVType n)

  ----------------------------------------------------------------------
  -- Boolean operations

  AndApp :: !(f BoolType) -> !(f BoolType) -> App f BoolType
  OrApp  :: !(f BoolType) -> !(f BoolType) -> App f BoolType
  NotApp :: !(f BoolType) -> App f BoolType

  ----------------------------------------------------------------------
  -- Bitvector operations

  BVAdd :: NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVSub :: NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)

  -- Multiply two numbers
  BVMul :: NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)

  -- Bitwise complement
  BVComplement :: NatRepr n -> f (BVType n) -> App f (BVType n)
  -- Bitwise and
  BVAnd :: NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  -- Bitwise or
  BVOr  :: NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  -- Exclusive or
  BVXor :: NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)

  -- Compare for equality.
  BVEq :: f (BVType n) -> f (BVType n) -> App f BoolType

  -- Return true if value contains even number of true bits.
  EvenParity :: f (BVType 8) -> App f BoolType

  -- Reverse the bytes in a bitvector expression.
  ReverseBytes :: NatRepr n -> f (BVType n) -> App f (BVType n)

  -- Add two values and a carry bit to determine if they have a signed
  -- overflow.
  UadcOverflows :: f (BVType n)
                -> f (BVType n)
                -> f BoolType
                -> App f BoolType
  -- Add two values and a carry bit to determine if they have a signed
  -- overflow.
  SadcOverflows :: f (BVType n)
                -> f (BVType n)
                -> f BoolType
                -> App f BoolType

  -- Unsigned subtract with borrow overflow
  UsbbOverflows :: f (BVType n)
                -> f (BVType n)
                -> f BoolType
                -> App f BoolType

  -- Signed subtract with borrow overflow
  SsbbOverflows :: f (BVType n)
                -> f (BVType n)
                -> f BoolType
                -> App f BoolType


  -- bsf "bit scan forward" returns the index of the least-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices less than return value must be unset.
  Bsf :: NatRepr n -> f (BVType n) -> App f (BVType n)

  -- bsr "bit scan reverse" returns the index of the most-significant
  -- bit that is 1.  Undefined if value is zero.
  -- All bits at indices greater than return value must be unset.
  Bsr :: NatRepr n -> f (BVType n) -> App f (BVType n)

  ----------------------------------------------------------------------
  -- Floating point operations

  -- Double precision addition.
  DoubleAdd :: f DoubleType -> f DoubleType -> App f DoubleType


appWidth :: App f (BVType n) -> NatRepr n
appWidth a =
  case a of
    ConcatV n _ _ -> addNat n n
--    LowerHalf n _ -> n
--    UpperHalf n _ -> n
    SExt _ n -> n
    UExt _ n -> n

    AndApp{} -> knownNat
    OrApp{}  -> knownNat
    NotApp{} -> knownNat

    BVAdd w _ _ -> w
    BVSub w _ _ -> w
    BVMul w _ _ -> w

    BVComplement w _ -> w
    BVAnd w _ _ -> w
    BVOr  w _ _ -> w
    BVXor w _ _ -> w
    BVEq _ _ -> knownNat
    EvenParity _ -> knownNat
    ReverseBytes w _ -> w

    UadcOverflows{}  -> knownNat
    SadcOverflows{} -> knownNat
    UsbbOverflows{} -> knownNat
    SsbbOverflows{} -> knownNat

    Bsf w _ -> w
    Bsr w _ -> w
    DoubleAdd _ _ -> knownNat


mapApp :: (forall u . f u -> g u)
       -> App f tp
       -> App g tp
-- TODO: Look at TemplateHaskell code in Crucible to autogenerate this.
mapApp = undefined

traverseApp :: Applicative m
            => (forall u . f u -> m (g u))
            -> App f tp
            -> m (App g tp)
-- TODO: Look at TemplateHaskell code in Crucible to autogenerate this.
traverseApp = undefined

------------------------------------------------------------------------
-- Value

-- | A value at runtime.
data Value tp where
  -- | Bitvector value.
  BVValue :: !(NatRepr n) -> Integer -> Value (BVType n)

  -- | Value from an assignment statement.
  AssignedValue :: !(Assignment tp) -> Value tp

valueWidth :: Value (BVType n) -> NatRepr n
valueWidth (BVValue n _) = n
valueWidth (AssignedValue a) = assignmentWidth a

bvValue :: KnownNat n => Integer -> Value (BVType n)
bvValue i = BVValue knownNat i

{-
-- | Return portion of a bitvector
bvSlice :: Value (BVType n) -- ^ Expression to evaluate
        -> Int             -- ^ Offset in bits
        -> NatRepr m       -- ^ Type representation
        -> Value (BVType m)
bvSlice = undefined


-- | Update a bitvector array at a given slice.
updateBVSlice :: Value (BVType n) -- ^ Value to update slice of.
              -> Int              -- ^ Offset in bits.
              -> Value (BVType m) -- ^ Value to update
              -> Value (BVType n)
updateBVSlice = undefined
-}
