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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Reopt.Semantics.Representation
  ( CFG
  , emptyCFG
  , cfgBlocks
  , insertBlock
    -- * Block level declarations
  , BlockLabel(..)
  , Block(..)
  , CodeAddr
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
  , valueType
  , valueWidth
  , bvValue
  , App(..)
  , appType
  , mapApp
  , foldApp
  , traverseApp
    -- * X86State
  , X86State(..)
  , curIP
  , reg64Regs
  , x87ControlWord
  , x87StatusWord
  , x87TagWords
  , x87Regs
  , xmmRegs
  , flagRegs
  , foldX86StateValue
    -- * X87StatusWord
  , X87StatusWord(..)
  , x87_ie
  , x87_de
  , x87_ze
  , x87_oe
  , x87_ue
  , x87_pe
  , x87_ef
  , x87_es
  , x87_c0
  , x87_c1
  , x87_c2
  , x87_c3
  , x87_top
  , x87_busy
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable (foldMap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid (Monoid, mappend)
import           Data.Parameterized.NatRepr
import           Data.Parameterized.TH.GADT
import qualified Data.Vector as V
import           Data.Word
import           GHC.TypeLits
import           Numeric (showHex)
import           Text.PrettyPrint.Leijen as PP hiding ((<$>))

import           Data.Parameterized.Classes
import qualified Flexdis86.InstructionSet as Flexdis86
import           Reopt.Semantics.Monad
  ( BoolType
  , Type(..)
  , TypeRepr(..)
  , knownType
  )

-- Note:
-- The declarations in this file follow a top-down order, so the top-level
-- definitions should be first.


type Prec = Int

colonPrec :: Prec
colonPrec = 5

------------------------------------------------------------------------
-- Pretty printing utilities

orderingIsEqual :: OrderingF (x :: k) (y :: k) -> Maybe (x :~: y)
orderingIsEqual o =
  case o of
    LTF -> Nothing
    EQF -> Just Refl
    GTF -> Nothing

bracketsep :: [Doc] -> Doc
bracketsep [] = text "{}"
bracketsep (h:l) =
  vcat ([text "{" <+> h] ++ fmap (text "," <+>) l ++ [text "}"])

-- FIXME: move
isInitial :: Value tp -> Bool
isInitial (BVValue {})       = False
isInitial (AssignedValue {}) = False
isInitial _                  = True

ppValueEq :: Doc -> Value tp -> Maybe Doc
ppValueEq nm v
  | isInitial v = Nothing
  | otherwise   = Just $ nm <+> text "=" <+> ppValue 0 v

rec :: String -> Value tp -> Maybe Doc
rec nm v = ppValueEq (text nm) v

recv :: String -> V.Vector (Value tp) -> [Maybe Doc]
recv nm v = f <$> [0..V.length v - 1]
  where f i = ppValueEq (text nm <> parens (int i)) (v V.! i)

parenIf :: Bool -> Doc -> Doc
parenIf True d = parens d
parenIf False d = d

------------------------------------------------------------------------
-- CFG

-- | A CFG is a map from all reachable code locations
-- to the block for that code location.
newtype CFG = CFG { _cfgBlocks :: (Map BlockLabel Block) }

-- | Create empty CFG
emptyCFG :: CFG
emptyCFG = CFG { _cfgBlocks = Map.empty }

cfgBlocks :: Simple Lens CFG (Map BlockLabel Block)
cfgBlocks = lens _cfgBlocks (\s v -> s { _cfgBlocks = v })

insertBlock :: Block -> CFG -> CFG
insertBlock b c = do
  let lbl = blockLabel b
  case Map.lookup lbl (c^.cfgBlocks) of
    Just{} -> error $ "Block with label " ++ show (pretty lbl) ++ " already defined."
    Nothing -> c & cfgBlocks %~ Map.insert (blockLabel b) b

instance Pretty CFG where
  pretty g = vcat (pretty <$> Map.elems (g^.cfgBlocks))

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

instance Pretty BlockLabel where
  pretty (DecompiledBlock a) = text ("addr_" ++ showHex a "")
  pretty (GeneratedBlock w)  = text ("label_" ++ show w)

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

instance Pretty Block where
  pretty b =
     pretty (blockLabel b) <> text ":" <$$>
     indent 2 (vcat (pretty <$> blockStmts b) <$$> pretty (blockTerm b))

------------------------------------------------------------------------
-- Loctions a statement may need to read or write to.

data StmtLoc tp where
  MemLoc     :: !(Value (BVType 64)) -> TypeRepr tp -> StmtLoc tp
  ControlLoc :: !Flexdis86.ControlReg -> StmtLoc (BVType 64)
  DebugLoc   :: !Flexdis86.DebugReg   -> StmtLoc (BVType 64)
  FS :: StmtLoc (BVType 16)
  GS :: StmtLoc (BVType 16)

stmtLocType :: StmtLoc tp -> TypeRepr tp
stmtLocType (MemLoc _ tp) = tp
stmtLocType ControlLoc{} = knownType
stmtLocType DebugLoc{}   = knownType
stmtLocType FS = knownType
stmtLocType GS = knownType


instance Pretty (StmtLoc tp) where
  pretty (MemLoc a _) = text "*" <> ppValue 11 a
  pretty (ControlLoc r) = text (show r)
  pretty (DebugLoc r) = text (show r)
  pretty FS = text "fs"
  pretty GS = text "gs"

------------------------------------------------------------------------
-- Stmt

data Stmt where
  AssignStmt :: !(Assignment tp) -> Stmt
  Write :: !(StmtLoc tp) -> Value tp -> Stmt

instance Pretty Stmt where
  pretty (AssignStmt a) = pretty a
  pretty (Write loc rhs) = pretty loc <+> text ":=" <+> ppValue 0 rhs

------------------------------------------------------------------------
-- TermStmt

-- A terminal statement in a block
data TermStmt where
  -- Fetch and execute the next instruction from the given processor state.
  FetchAndExecute :: !X86State -> TermStmt

  -- Branch and execute one block or another.
  Branch :: !(Value BoolType)
         -> !BlockLabel
         -> !BlockLabel
         -> TermStmt

instance Pretty TermStmt where
  pretty (FetchAndExecute s) =
    text "fetch_and_execute" <$$>
    indent 2 (pretty s)
  pretty (Branch c x y) =
    text "branch" <+> ppValue 0 c <+> pretty x <+> pretty y

------------------------------------------------------------------------
-- X87StatusWord

-- The X87 status word.
data X87StatusWord
   = X87StatusWord
   { _x87_ie   :: !(Value BoolType) -- Bit 0
   , _x87_de   :: !(Value BoolType) -- Bit 1
   , _x87_ze   :: !(Value BoolType) -- Bit 2
   , _x87_oe   :: !(Value BoolType) -- Bit 3
   , _x87_ue   :: !(Value BoolType) -- Bit 4
   , _x87_pe   :: !(Value BoolType) -- Bit 5
   , _x87_ef   :: !(Value BoolType) -- Bit 6
   , _x87_es   :: !(Value BoolType) -- Bit 7
   , _x87_c0   :: !(Value BoolType) -- Bit 8
   , _x87_c1   :: !(Value BoolType) -- Bit 9
   , _x87_c2   :: !(Value BoolType) -- Bit 10
   , _x87_c3   :: !(Value BoolType) -- Bit 14
     -- Top of the stack pointer.
     -- This contains a value 0-7 indicating the top of the stack.
   , _x87_top  :: !(Value (BVType 3)) -- Bits 11-13
   , _x87_busy :: !(Value BoolType)  -- Bit 15
   }

x87_ie :: Simple Lens X87StatusWord (Value BoolType)
x87_ie = lens _x87_ie (\s v -> s { _x87_ie = v })

x87_de :: Simple Lens X87StatusWord (Value BoolType)
x87_de = lens _x87_de (\s v -> s { _x87_de = v })

x87_ze :: Simple Lens X87StatusWord (Value BoolType)
x87_ze = lens _x87_ze (\s v -> s { _x87_ze = v })

x87_oe :: Simple Lens X87StatusWord (Value BoolType)
x87_oe = lens _x87_oe (\s v -> s { _x87_oe = v })

x87_ue :: Simple Lens X87StatusWord (Value BoolType)
x87_ue = lens _x87_ue (\s v -> s { _x87_ue = v })

x87_pe :: Simple Lens X87StatusWord (Value BoolType)
x87_pe = lens _x87_pe (\s v -> s { _x87_pe = v })

x87_ef :: Simple Lens X87StatusWord (Value BoolType)
x87_ef = lens _x87_ef (\s v -> s { _x87_ef = v })

x87_es :: Simple Lens X87StatusWord (Value BoolType)
x87_es = lens _x87_es (\s v -> s { _x87_es = v })

x87_c0 :: Simple Lens X87StatusWord (Value BoolType)
x87_c0 = lens _x87_c0 (\s v -> s { _x87_c0 = v })

x87_c1 :: Simple Lens X87StatusWord (Value BoolType)
x87_c1 = lens _x87_c1 (\s v -> s { _x87_c1 = v })

x87_c2 :: Simple Lens X87StatusWord (Value BoolType)
x87_c2 = lens _x87_c2 (\s v -> s { _x87_c2 = v })

x87_c3 :: Simple Lens X87StatusWord (Value BoolType)
x87_c3 = lens _x87_c3 (\s v -> s { _x87_c3 = v })

x87_top :: Simple Lens X87StatusWord (Value (BVType 3))
x87_top = lens _x87_top (\s v -> s { _x87_top = v })

x87_busy :: Simple Lens X87StatusWord (Value BoolType)
x87_busy = lens _x87_busy (\s v -> s { _x87_busy = v })

------------------------------------------------------------------------
-- X86State

-- | This represents the state of the processor registers after some
-- execution.
data X86State = X86State
     { _curIP  :: !(Value (BVType 64))
       -- 16 general purposes registers.
     , _reg64Regs :: !(V.Vector (Value (BVType  64)))
       -- 32 individual bits in the flags register.
     , _flagRegs  :: !(V.Vector (Value BoolType))
       -- The 16-bit x87 control word.
       -- See:
       --   Intel® 64 and IA-32 Architectures Software Developer’s Manual
       --   Volume 1:
       --   Section 8.1.5
     , _x87ControlWord :: !(V.Vector (Value BoolType))
       -- The x86 status word.
     , _x87StatusWord :: !X87StatusWord
       -- The 8 x87 tag words
       -- (used to indicate the status of different FPU registers).
     , _x87TagWords :: !(V.Vector (Value (BVType 2)))
       -- The 8 x87 FPU regs (MMX registers alias these).
       -- MMX registers alias these with (for example).
       -- MM3 maps to R3.
     , _x87Regs   :: !(V.Vector (Value (BVType  80)))
       -- One of 8 128-bit XMM registers
     , _xmmRegs   :: !(V.Vector (Value (BVType 128)))
     }

foldX86StateValue :: Monoid a => (forall u. Value u -> a) -> X86State -> a
foldX86StateValue f s = f (s^.curIP)
                        `mappend` foldMap f (s^.reg64Regs)
                        `mappend` foldMap f (s^.flagRegs)
                        `mappend` foldMap f (s^.x87ControlWord)
                        `mappend` foldSW  (s^.x87StatusWord)
                        `mappend` foldMap f (s^.x87TagWords)
                        `mappend` foldMap f (s^.x87Regs)
                        `mappend` foldMap f (s^.xmmRegs)
  where
    foldSW s' = f (s'^.x87_ie)
                `mappend` f (s'^.x87_de)
                `mappend` f (s'^.x87_ze)
                `mappend` f (s'^.x87_oe)
                `mappend` f (s'^.x87_ue)
                `mappend` f (s'^.x87_pe)
                `mappend` f (s'^.x87_ef)
                `mappend` f (s'^.x87_es)
                `mappend` f (s'^.x87_c0)
                `mappend` f (s'^.x87_c1)
                `mappend` f (s'^.x87_c2)
                `mappend` f (s'^.x87_c3)
                `mappend` f (s'^.x87_top)
                `mappend` f (s'^.x87_busy)




-- | The value of the current instruction pointer.
curIP :: Simple Lens X86State (Value (BVType 64))
curIP = lens _curIP (\s v -> s { _curIP = v })

-- | Assignments to the 16 general-purpose registers.
reg64Regs :: Simple Lens X86State (V.Vector (Value (BVType 64)))
reg64Regs = lens _reg64Regs (\s v -> s { _reg64Regs = v })

-- | 32 individual bits in the flags register.
flagRegs :: Simple Lens X86State (V.Vector (Value BoolType))
flagRegs = lens _flagRegs (\s v -> s { _flagRegs = v })

-- | The current x87 control word.
x87ControlWord :: Simple Lens X86State (V.Vector (Value BoolType))
x87ControlWord = lens _x87ControlWord (\s v -> s { _x87ControlWord = v })

-- | The current x87 status word.
x87StatusWord :: Simple Lens X86State X87StatusWord
x87StatusWord = lens _x87StatusWord (\s v -> s { _x87StatusWord = v })

-- | The 8 x87 tag words
-- used to indicate the status of different FPU registers.
x87TagWords :: Simple Lens X86State (V.Vector (Value (BVType 2)))
x87TagWords = lens _x87TagWords (\s v -> s { _x87TagWords = v })

-- | Assignments to the 8 80-bit FPU registers.
x87Regs :: Simple Lens X86State (V.Vector (Value (BVType 80)))
x87Regs = lens _x87Regs (\s v -> s { _x87Regs = v })

-- | Assignments to the 16 128-bit XMM registers.
xmmRegs :: Simple Lens X86State (V.Vector (Value (BVType 128)))
xmmRegs = lens _xmmRegs (\s v -> s { _xmmRegs = v })

ppStatus :: X87StatusWord -> [Maybe Doc]
ppStatus s = 
  [ rec "x87_ie" (s^.x87_ie)
  , rec "x87_de" (s^.x87_de)
  , rec "x87_ze" (s^.x87_ze)
  , rec "x87_oe" (s^.x87_oe)
  , rec "x87_ue" (s^.x87_ue)
  , rec "x87_pe" (s^.x87_pe)
  , rec "x87_ef" (s^.x87_ef)
  , rec "x87_es" (s^.x87_es)
  , rec "x87_c0" (s^.x87_c0)
  , rec "x87_c1" (s^.x87_c1)
  , rec "x87_c2" (s^.x87_c2)
  , rec "x87_c3" (s^.x87_c3)
  , rec "x87_top"  (s^.x87_top)
  , rec "x87_busy" (s^.x87_busy)
  ]

instance Pretty X86State where
  pretty s =
    bracketsep $ catMaybes ([ rec "ip" (s^.curIP)]
                            ++ recv "reg" (s^.reg64Regs)
                            ++ recv "flag" (s^.flagRegs)
                            ++ recv "x87_control" (s^.x87ControlWord)
                            ++ ppStatus (s^.x87StatusWord)
                            ++ recv "x87_tag" (s^.x87TagWords)
                            ++ recv "r" (s^.x87Regs)
                            ++ recv "xmm" (s^.xmmRegs))

------------------------------------------------------------------------
-- Assignment

-- | This should be an identifier that can be used to identify the
-- assignment statement uniquely within the CFG.
type AssignId = Word64

ppAssignId :: AssignId -> Doc
ppAssignId w = text ("r" ++ show w)

-- | An assignment consists of a unique location identifier and a right-
-- hand side that returns a value.
data Assignment tp = Assignment { assignId :: !AssignId
                                , assignRhs :: !(AssignRhs tp)
                                }

assignmentType :: Assignment tp -> TypeRepr tp
assignmentType (Assignment _ rhs) = assignRhsType rhs

instance TestEquality Assignment where
  testEquality x y = orderingIsEqual (compareF x y)

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

-- | The right hand side of an assignment is an expression that
-- returns a value.
data AssignRhs tp where
  -- An expression that is computed from evaluating subexpressions.
  EvalApp :: !(App Value tp)
          -> AssignRhs tp

  -- An expression with an undefined value.
  SetUndefined :: !(NatRepr n) -- Width of undefined value.
               -> AssignRhs (BVType n)

  Read :: !(StmtLoc tp) -> AssignRhs tp

instance Pretty (AssignRhs tp) where
  pretty (EvalApp a) = ppApp (ppValue 10) a
  pretty (SetUndefined w) = text "undef ::" <+> brackets (text (show w))
  pretty (Read loc) = pretty loc

-- | Returns the type of an assignment rhs.
assignRhsType :: AssignRhs tp -> TypeRepr tp
assignRhsType rhs =
  case rhs of
    EvalApp a -> appType a
    SetUndefined w -> BVTypeRepr w
    Read loc  -> stmtLocType loc

------------------------------------------------------------------------
-- Value

-- | A value at runtime.
data Value tp where
  -- Bitvector value.
  BVValue :: !(NatRepr n) -> Integer -> Value (BVType n)

  -- Value from an assignment statement.
  AssignedValue :: !(Assignment tp) -> Value tp

  Initial_X87_IE :: Value BoolType
  Initial_X87_DE :: Value BoolType
  Initial_X87_ZE :: Value BoolType
  Initial_X87_OE :: Value BoolType
  Initial_X87_UE :: Value BoolType
  Initial_X87_PE :: Value BoolType
  Initial_X87_EF :: Value BoolType
  Initial_X87_ES :: Value BoolType
  Initial_X87_C0 :: Value BoolType
  Initial_X87_C1 :: Value BoolType
  Initial_X87_C2 :: Value BoolType
  Initial_X87_C3 :: Value BoolType

  -- Initial state of one of 16 general-purpose registers.
  InitialGenReg :: Int -> Value (BVType 64)

  -- One of 32 initial flag registers.
  InitialFlag :: Int -> Value BoolType

  -- Control bit
  InitialX87ControlBit :: Int -> Value BoolType

  -- X87 tag register.
  InitialTagWord :: Int -> Value (BVType 2)

  -- One of 8 fpu/mmx registers.
  InitialFPUReg :: Int -> Value (BVType 80)

  -- One of 8 XMM registers
  InitialXMMReg :: Int -> Value (BVType 128)

instance TestEquality Value where
  testEquality x y = orderingIsEqual (compareF x y)

instance OrdF Value where
  compareF (BVValue wx vx) (BVValue wy vy) =
    case compareF wx wy of
      LTF -> LTF
      EQF -> fromOrdering (compare vx vy)
      GTF -> GTF

  compareF BVValue{} _ = LTF
  compareF _ BVValue{} = GTF

valueType :: Value tp -> TypeRepr tp
valueType (BVValue n _) = BVTypeRepr n
valueType (AssignedValue a) = assignmentType a
valueType Initial_X87_IE = knownType
valueType Initial_X87_DE = knownType
valueType Initial_X87_ZE = knownType
valueType Initial_X87_OE = knownType
valueType Initial_X87_UE = knownType
valueType Initial_X87_PE = knownType
valueType Initial_X87_EF = knownType
valueType Initial_X87_ES = knownType
valueType Initial_X87_C0 = knownType
valueType Initial_X87_C1 = knownType
valueType Initial_X87_C2 = knownType
valueType Initial_X87_C3 = knownType
valueType InitialGenReg{} = knownType
valueType InitialFlag{}   = knownType
valueType InitialX87ControlBit{} = knownType
valueType InitialTagWord{}  = knownType
valueType InitialFPUReg{}   = knownType
valueType InitialXMMReg{}   = knownType

valueWidth :: Value (BVType n) -> NatRepr n
valueWidth v =
  case valueType v of
    BVTypeRepr n -> n

bvValue :: KnownNat n => Integer -> Value (BVType n)
bvValue i = BVValue knownNat i

ppValue :: Prec -> Value tp -> Doc
ppValue p (BVValue w i) = parenIf (p > colonPrec) $
  text (show i) <+> text "::" <+> brackets (text (show w))
ppValue _ (AssignedValue a) = ppAssignId (assignId a)
ppValue _ _ = text "initial" -- TODO: Print out the actual initial value.

-----------------------------------------------------------------------
-- App

-- | App defines builtin operations on values.
data App f tp where

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

  BVAdd :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  BVSub :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Multiply two numbers
  BVMul :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Unsigned division (rounds fractions to zero).
  BVDiv :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Signed division (rounds fractional results to zero).
  BVSignedDiv :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Unsigned modulo
  BVMod :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Signed modulo.
  -- The resulting modulus has the same sign as the quotient and satisfies
  -- the constraint that for all x y where y != 0:
  --   x = (y * BVSignedDiv x y) + BVSignedMod x y
  BVSignedMod :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Unsigned less than.
  BVUnsignedLt :: !(f (BVType n)) -> !(f (BVType n)) -> App f BoolType

  -- @BVBit x i@ returns true iff bit @i@ of @x@ is true.
  -- 0 is the index of the least-significant bit.
  BVBit :: !(f (BVType n)) -> !(f (BVType log_n)) -> App f BoolType

  -- Bitwise complement
  BVComplement :: !(NatRepr n) -> !(f (BVType n)) -> App f (BVType n)
  -- Bitwise and
  BVAnd :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  -- Bitwise or
  BVOr  :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)
  -- Exclusive or
  BVXor :: !(NatRepr n) -> !(f (BVType n)) -> !(f (BVType n)) -> App f (BVType n)

  -- Compare for equality.
  BVEq :: !(f (BVType n)) -> !(f (BVType n)) -> App f BoolType

  -- Return true if value contains even number of true bits.
  EvenParity :: !(f (BVType 8)) -> App f BoolType

  -- Reverse the bytes in a bitvector expression.
  ReverseBytes :: !(NatRepr n) -> !(f (BVType n)) -> App f (BVType n)

  -- Add two values and a carry bit to determine if they have a signed
  -- overflow.
  UadcOverflows :: !(f (BVType n))
                -> !(f (BVType n))
                -> !(f BoolType)
                -> App f BoolType
  -- Add two values and a carry bit to determine if they have a signed
  -- overflow.
  SadcOverflows :: !(f (BVType n))
                -> !(f (BVType n))
                -> !(f BoolType)
                -> App f BoolType

  -- Unsigned subtract with borrow overflow
  UsbbOverflows :: !(f (BVType n))
                -> !(f (BVType n))
                -> !(f BoolType)
                -> App f BoolType

  -- Signed subtract with borrow overflow
  SsbbOverflows :: !(f (BVType n))
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

  -----------------------dat -----------------------------------------------
  -- Floating point operations

  -- Double precision addition.
  -- DoubleAdd :: f DoubleType -> f DoubleType -> App f DoubleType

sexpr :: String -> [Doc] -> Doc
sexpr nm d = parens (hsep (text nm : d))

ppNat :: NatRepr n -> Doc
ppNat n = text (show n)

ppApp :: (forall u . f u -> Doc)
      -> App f tp
      -> Doc
ppApp pp a0 =
  case a0 of
    Mux _ c x y -> sexpr "mux" [ pp c, pp x, pp y ]
    MMXExtend e -> sexpr "mmx_extend" [ pp e ]
    ConcatV _ x y -> sexpr "concat" [ pp x, pp y ]
    UpperHalf _ x -> sexpr "upper_half" [ pp x ]
    Trunc x w -> sexpr "trunc" [ pp x, ppNat w ]
    SExt x w -> sexpr "sext" [ pp x, ppNat w ]
    UExt x w -> sexpr "uext" [ pp x, ppNat w ]
    AndApp x y -> sexpr "and" [ pp x, pp y ]
    OrApp  x y -> sexpr "or"  [ pp x, pp y ]
    NotApp x   -> sexpr "not" [ pp x ]
    BVAdd _ x y -> sexpr "bv_add" [ pp x, pp y ]
    BVSub _ x y -> sexpr "bv_sub" [ pp x, pp y ]
    BVMul _ x y -> sexpr "bv_mul" [ pp x, pp y ]
    BVDiv _ x y       -> sexpr "bv_udiv" [ pp x, pp y ]
    BVSignedDiv _ x y -> sexpr "bv_sdiv" [ pp x, pp y ]
    BVMod _ x y       -> sexpr "bv_umod" [ pp x, pp y ]
    BVSignedMod _ x y -> sexpr "bv_smod" [ pp x, pp y ]
    BVUnsignedLt x y  -> sexpr "bv_ult"  [ pp x, pp y ]
    BVBit x i -> sexpr "bv_bitset" [ pp x, pp i]
    BVComplement _ x -> sexpr "bv_complement" [ pp x ]
    BVAnd _ x y -> sexpr "bv_and" [ pp x, pp y ]
    BVOr  _ x y -> sexpr "bv_or"  [ pp x, pp y ]
    BVXor _ x y -> sexpr "bv_xor" [ pp x, pp y ]
    BVEq x y    -> sexpr "bv_eq" [ pp x, pp y ]
    EvenParity x -> sexpr "even_parity" [ pp x ]
    ReverseBytes _ x -> sexpr "reverse_bytes" [ pp x ]
    UadcOverflows x y c -> sexpr "uadc_overflows" [ pp x, pp y, pp c ]
    SadcOverflows x y c -> sexpr "sadc_overflows" [ pp x, pp y, pp c ]
    UsbbOverflows x y c -> sexpr "usbb_overflows" [ pp x, pp y, pp c ]
    SsbbOverflows x y c -> sexpr "ssbb_overflows" [ pp x, pp y, pp c ]
    Bsf _ x -> sexpr "bsf" [ pp x ]
    Bsr _ x -> sexpr "bsr" [ pp x ]

appType :: App f tp -> TypeRepr tp
appType a =
  case a of
    Mux w _ _ _ -> BVTypeRepr w
    MMXExtend{} -> knownType
    ConcatV w _ _ -> BVTypeRepr (addNat w w)
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
    BVDiv w _ _ -> BVTypeRepr w
    BVSignedDiv w _ _ -> BVTypeRepr w
    BVMod w _ _ -> BVTypeRepr w
    BVSignedMod w _ _ -> BVTypeRepr w

    BVUnsignedLt{} -> knownType
    BVBit{} -> knownType

    BVComplement w _ -> BVTypeRepr w
    BVAnd w _ _ -> BVTypeRepr w
    BVOr  w _ _ -> BVTypeRepr w
    BVXor w _ _ -> BVTypeRepr w
    BVEq _ _ -> knownType
    EvenParity _ -> knownType
    ReverseBytes w _ -> BVTypeRepr w

    UadcOverflows{}  -> knownType
    SadcOverflows{} -> knownType
    UsbbOverflows{} -> knownType
    SsbbOverflows{} -> knownType

    Bsf w _ -> BVTypeRepr w
    Bsr w _ -> BVTypeRepr w
    -- DoubleAdd _ _ -> knownType

-----------------------------------------------------------------------
-- App utilities

-- Force app to be in template-haskell context.
$(return [])

traverseApp :: Applicative m
            => (forall u . f u -> m (g u))
            -> App f tp
            -> m (App g tp)
traverseApp = $(structuralTraversal $[t|App|])

mapApp :: (forall u . f u -> g u)
       -> App f tp
       -> App g tp
mapApp f m = runIdentity $ traverseApp (return . f) m

foldApp :: Monoid m => (forall u. f u -> (b -> m)) -> b -> App f tp -> m
foldApp f v m = getConst (traverseApp (\f_u -> Const $ \b -> f f_u b) m) v


    
