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
{-# LANGUAGE PatternGuards #-}

module Reopt.Semantics.Representation
  ( CFG
  , emptyCFG
  , cfgBlocks
  , insertBlock
  , traverseBlocks
    -- * Block level declarations
  , BlockLabel(..)
  , blockParent
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
  , register
  , curIP
  , reg64Regs
  , x87ControlWord
  , x87StatusWord
  , x87TagWords
  , x87TopReg
  , x87Regs
  , xmmRegs
  , flagRegs
  , foldX86StateValue
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

import           Data.Parameterized.Some
import           Data.Parameterized.Classes
import qualified Flexdis86.InstructionSet as Flexdis86
import           Reopt.Semantics.Monad
  ( BoolType
  , Type(..)
  , TypeRepr(..)
  , knownType
  )
import qualified Reopt.Semantics.StateNames as N


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

ppValueEq :: N.RegisterName cl -> Value (N.RegisterType cl) -> Maybe Doc
ppValueEq r v
  | Just _ <- testEquality v (Initial r) = Nothing
  | otherwise   = Just $ text (show r) <+> text "=" <+> ppValue 0 v

rec :: N.RegisterName cl -> Value (N.RegisterType cl) -> Maybe Doc
rec init v = ppValueEq init v

recv :: (Int -> N.RegisterName cl)
        -> V.Vector (Value (N.RegisterType cl)) -> [Maybe Doc]
recv mkR v = f <$> [0..V.length v - 1]
  where
    f i = ppValueEq (mkR i) (v V.! i)

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

------------------------------------------------------------------------
-- BlockLabel

-- | A label used to identify a block.
data BlockLabel
   = DecompiledBlock CodeAddr
     -- ^ A block that came from an address in the code.
   | GeneratedBlock CodeAddr Word64
     -- ^ A unique identifier for a generated block.
  deriving Eq

instance Ord BlockLabel where
  compare (DecompiledBlock v) (DecompiledBlock v') = compare v v'
  compare (DecompiledBlock v) (GeneratedBlock p _)
    | p == v = LT
    | otherwise = compare v p
  compare (GeneratedBlock p _) (DecompiledBlock v) 
    | p == v = GT
    | otherwise = compare p v
  compare (GeneratedBlock p v) (GeneratedBlock p' v')
    | p == p' = compare v v'
    | otherwise = compare p p'
    
-- | A label always has a parent, i.e., the DecompiledBlock that
-- generated it
blockParent :: BlockLabel -> CodeAddr
blockParent (DecompiledBlock v)  = v
blockParent (GeneratedBlock v _) = v

-- | An address of a code location.
--
-- This is currently just a Word64, but we may need to switch to a
-- structured representation once dynamic libraries are supported.
type CodeAddr = Word64

instance Pretty BlockLabel where
  pretty (DecompiledBlock a)   = text ("addr_" ++ showHex a "")
  pretty (GeneratedBlock p w)  = text ("label_" ++ showHex p "_" ++ show w)

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
  ControlLoc :: !(N.RegisterName N.Control) -> StmtLoc (BVType 64)
  DebugLoc   :: !(N.RegisterName N.Debug)   -> StmtLoc (BVType 64)
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
  PlaceHolderStmt :: [Some Value] -> String -> Stmt
  Comment :: String -> Stmt

instance Pretty Stmt where
  pretty (AssignStmt a) = pretty a
  pretty (Write loc rhs) = pretty loc <+> text ":=" <+> ppValue 0 rhs
  pretty (PlaceHolderStmt vals name) = text ("PLACEHOLDER: " ++ name)
                                       <+> parens (hcat $ punctuate comma
                                                   $ map (viewSome (ppValue 0)) vals)
  pretty (Comment s) = text $ "# " ++ s

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

{-
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
-}

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
     , _x87StatusWord :: !(V.Vector (Value BoolType))
     , _x87TopReg     :: !(Value (BVType 3))
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
                        `mappend` foldMap f (s^.x87StatusWord)
                        `mappend` foldMap f (s^.x87TagWords)
                        `mappend` foldMap f (s^.x87Regs)
                        `mappend` foldMap f (s^.xmmRegs)

register :: N.RegisterName cl -> Simple Lens X86State (Value (N.RegisterType cl))
register reg = case reg of
                N.IPReg           -> curIP
                N.GPReg n         -> reg64Regs . idx n
                N.FlagReg n       -> flagRegs . idx n                
                N.X87ControlReg n -> x87ControlWord . idx n
                N.X87StatusReg n  -> x87StatusWord . idx n
                N.X87TopReg       -> x87TopReg
                N.X87TagReg n     -> x87TagWords . idx n
                N.X87FPUReg n     -> x87Regs . idx n
                N.XMMReg n        -> xmmRegs . idx n
                -- X87PC
                -- X87RC
                -- SegmentReg
                -- DebugReg
                -- ControlReg 
                _               -> error $ "Unexpected reg: " ++ show reg
  where
    idx :: forall tp. Int -> Lens' (V.Vector (Value tp)) (Value tp)
    idx n = lens (V.! n) (\s v -> (ix n .~ v) s)
  
-- | the value oDebugReg{}  f the current instruction pointer.
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
x87StatusWord :: Simple Lens X86State (V.Vector (Value BoolType))
x87StatusWord = lens _x87StatusWord (\s v -> s { _x87StatusWord = v })

-- | The 8 x87 tag words
-- used to indicate the status of different FPU registers.
x87TagWords :: Simple Lens X86State (V.Vector (Value (BVType 2)))
x87TagWords = lens _x87TagWords (\s v -> s { _x87TagWords = v })

-- | the value oDebugReg{}  f the current instruction pointer.
x87TopReg :: Simple Lens X86State (Value (BVType 3))
x87TopReg = lens _x87TopReg (\s v -> s { _x87TopReg = v })

-- | Assignments to the 8 80-bit FPU registers.
x87Regs :: Simple Lens X86State (V.Vector (Value (BVType 80)))
x87Regs = lens _x87Regs (\s v -> s { _x87Regs = v })

-- | Assignments to the 16 128-bit XMM registers.
xmmRegs :: Simple Lens X86State (V.Vector (Value (BVType 128)))
xmmRegs = lens _xmmRegs (\s v -> s { _xmmRegs = v })

instance Pretty X86State where
  pretty s =
    bracketsep $ catMaybes ([ rec   N.rip (s^.curIP)]
                            ++ recv N.GPReg (s^.reg64Regs)
                            ++ recv N.FlagReg (s^.flagRegs)
                            ++ recv N.X87ControlReg (s^.x87ControlWord)
                            ++ recv N.X87StatusReg (s^.x87StatusWord)
                            ++ recv N.X87TagReg (s^.x87TagWords)
                            ++ recv N.X87FPUReg (s^.x87Regs)
                            ++ recv N.XMMReg (s^.xmmRegs))

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

  Initial :: !(N.RegisterName cl) -> Value (N.RegisterType cl)

instance TestEquality Value where
  testEquality x y = orderingIsEqual (compareF x y)

instance OrdF Value where
  compareF (BVValue wx vx) (BVValue wy vy) =
    case compareF wx wy of
      LTF -> LTF
      EQF -> fromOrdering (compare vx vy)
      GTF -> GTF

  compareF (AssignedValue a1) (AssignedValue a2) = compareF a1 a2

  compareF (Initial r) (Initial r') =
    case compareF r r' of
     LTF -> LTF
     EQF -> EQF
     GTF -> GTF
  
  compareF _ Initial{} = LTF
  compareF Initial{} _ = GTF
  
  compareF BVValue{} _ = LTF
  compareF _ BVValue{} = GTF

valueType :: Value tp -> TypeRepr tp
valueType (BVValue n _) = BVTypeRepr n
valueType (AssignedValue a) = assignmentType a
valueType (Initial r)       = N.registerType r

valueWidth :: Value (BVType n) -> NatRepr n
valueWidth v =
  case valueType v of
    BVTypeRepr n -> n

bvValue :: KnownNat n => Integer -> Value (BVType n)
bvValue i = BVValue knownNat i

ppValue :: Prec -> Value tp -> Doc
ppValue p (BVValue w i) = parenIf (p > colonPrec) $
  text ("0x" ++ showHex i "") <+> text "::" <+> brackets (text (show w))
ppValue _ (AssignedValue a) = ppAssignId (assignId a)
ppValue _ (Initial r)       = text (show r) <> text "_0"

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
    BVShl _ x y -> sexpr "bv_shl" [ pp x, pp y ]
    BVShr _ x y -> sexpr "bv_shr" [ pp x, pp y ]
    BVSar _ x y -> sexpr "bv_sar" [ pp x, pp y ]    
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


    
