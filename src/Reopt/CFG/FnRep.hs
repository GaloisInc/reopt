{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

-- | Defines a program representation with machine registers replaced by values.
module Reopt.CFG.FnRep (
  RecoveredModule (..),
  FunctionDecl (..),
  Function (..),
  fnBlocks,
  FnAssignId (..),
  FnAssignment (..),
  FnAssignRhs (..),
  FnValue (..),
  FunctionType (..),
  FnBlock (..),
  FnBlockInvariant (..),
  FnMemAccessType (..),
  FnBlockLabel,
  fnBlockLabelFromAddr,
  fnBlockLabelAddr,
  fnBlockLabelString,
  FnStmt (..),
  FnTermStmt (..),
  FnJumpTarget (..),
  FnPhiVar (..),
  FnReturnVar (..),
  FnArchStmt,
  FoldFnValue (..),
  FnArchConstraints,
  -- ArchBlockPrecond
) where

import Control.Monad.Identity (Identity (runIdentity))
import Data.ByteString.Char8 qualified as BSC
import Data.Kind
import Data.Parameterized.Classes
import Data.Parameterized.NatRepr (type (<=))
import Data.Parameterized.Some (Some (..), viewSome)
import Data.Parameterized.TraversableF (FoldableF (foldlF'))
import Data.Parameterized.TraversableFC (
  FoldableFC (foldlFC, foldrFC),
 )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Word (Word64)
import Numeric (showHex)
import Prettyprinter qualified as PP

import Data.Macaw.Analysis.RegisterUse (BoundLoc)
import Data.Macaw.CFG (
  App (..),
  ArchAddrWidth,
  ArchBlockPrecond,
  ArchFn,
  ArchReg,
  IsArchFn (..),
  IsArchStmt (..),
  MemRepr (..),
  ppApp,
  sexpr,
 )
import Data.Macaw.Memory
import Data.Macaw.Types (
  BVType,
  BoolType,
  HasRepr (..),
  NatRepr (..),
  TypeRepr (BVTypeRepr, BoolTypeRepr),
 )
import Data.Macaw.Types qualified as M (Type)

-- | Utility to pretty print with commas separating arguments.
commas :: [PP.Doc a] -> PP.Doc a
commas = PP.hsep . PP.punctuate PP.comma

------------------------------------------------------------------------
-- FnAssignId

-- | A unique identifier for an assignment, phi variable, or return.
newtype FnAssignId = FnAssignId Word64
  deriving (Eq, Ord)

instance Show FnAssignId where
  showsPrec _ p = shows (PP.pretty p)

instance PP.Pretty FnAssignId where
  pretty (FnAssignId w) = "r" <> PP.pretty w

------------------------------------------------------------------------
-- FnPhiVar

-- | A phi variable
data FnPhiVar arch (tp :: M.Type) = FnPhiVar
  { unFnPhiVar :: !FnAssignId
  , fnPhiVarType :: !(TypeRepr tp)
  , fnPhiVarRep :: !(BoundLoc (ArchReg arch) tp)
  -- ^ Class representative for locations that lead to this phi
  -- variable.
  , fnPhiVarLocations :: ![BoundLoc (ArchReg arch) tp]
  -- ^ Locations read after this phi variable is introduced that are
  -- equivalent to this phi variable.
  }

instance HasRepr (FnPhiVar arch) TypeRepr where
  typeRepr = fnPhiVarType

instance TestEquality (FnPhiVar arch) where
  testEquality x y = orderingF_refl (compareF x y)

instance OrdF (FnPhiVar arch) where
  compareF x y =
    case compare (unFnPhiVar x) (unFnPhiVar y) of
      LT -> LTF
      GT -> GTF
      EQ ->
        case testEquality (fnPhiVarType x) (fnPhiVarType y) of
          Just Refl -> EQF
          Nothing -> error "mismatched types"

------------------------------------------------------------------------
-- FnReturnVar

data FnReturnVar tp = FnReturnVar
  { frAssignId :: !FnAssignId
  , frReturnType :: !(TypeRepr tp)
  }

instance PP.Pretty (FnReturnVar tp) where
  pretty = PP.pretty . frAssignId

------------------------------------------------------------------------
-- FunctionType

-- | Describes the type of a function.
data FunctionType (arch :: Type) = FunctionType
  { fnArgTypes :: ![Some TypeRepr]
  , fnVarArgs :: !Bool
  , fnReturnType :: !(Maybe (Some TypeRepr))
  }
  deriving (Ord, Eq, Show)

------------------------------------------------------------------------
-- FnAssignRhs mutually recursive constructors

-- | The right-hand side of a function assignment statement.
data
  FnAssignRhs
    -- Architecture
    (arch :: Type)
    -- Type constructor for the arguments of operations (in open recursion style)
    (f :: M.Type -> Type)
    -- Type of the value returned
    (tp :: M.Type)
  where
  -- | An expression with an undefined value.
  --
  -- NB. Undefined values in this representation are values with a
  -- non-deterministically chosen value.
  FnSetUndefined ::
    -- | Type of undefined value.
    !(TypeRepr tp) ->
    FnAssignRhs arch f tp
  -- | A unconditional memory read.
  FnReadMem ::
    !(f (BVType (ArchAddrWidth arch))) ->
    !(TypeRepr tp) ->
    FnAssignRhs arch f tp
  -- | A conditional memory read.
  FnCondReadMem ::
    !(MemRepr tp) ->
    !(f BoolType) ->
    !(f (BVType (ArchAddrWidth arch))) ->
    !(f tp) ->
    FnAssignRhs arch f tp
  -- | Evaluate the pure function given by the App.
  FnEvalApp ::
    !(App f tp) ->
    FnAssignRhs arch f tp
  -- | Evaluate an architeture-specific function.
  FnEvalArchFn ::
    !(ArchFn arch f tp) ->
    FnAssignRhs arch f tp
  -- | Arch-addr-width constant that may be a pointer.  Used to enable
  -- easier type assignment when translating to LLVM.
  FnAddrWidthConstant ::
    Integer ->
    FnAssignRhs arch f (BVType (ArchAddrWidth arch))

instance FoldableFC (ArchFn arch) => FoldableFC (FnAssignRhs arch) where
  foldrFC _ s (FnSetUndefined{}) = s
  foldrFC f s (FnReadMem loc _) = f loc s
  foldrFC f s (FnCondReadMem _ c a d) = f c (f a (f d s))
  foldrFC f s (FnEvalApp a) = foldrFC f s a
  foldrFC f s (FnEvalArchFn fn) = foldrFC f s fn
  foldrFC _ s (FnAddrWidthConstant{}) = s

------------------------------------------------------------------------
-- FnAssignment/FnValue mutually recursive constructors

data FnAssignment arch tp = FnAssignment
  { fnAssignId :: !FnAssignId
  , fnAssignRhs :: !(FnAssignRhs arch (FnValue arch) tp)
  }

-- | A value within a function execution.
data FnValue (arch :: Type) (tp :: M.Type) where
  -- | A value that is actually undefined, like a non-argument
  -- register at the start of a function.
  FnUndefined :: !(TypeRepr tp) -> FnValue arch tp
  -- | A particular Bool value
  FnConstantBool :: !Bool -> FnValue arch BoolType
  -- | A particular integer value
  FnConstantValue :: (1 <= n) => !(NatRepr n) -> !Integer -> FnValue arch (BVType n)
  -- | Value from an assignment statement.
  FnAssignedValue :: !(FnAssignment arch tp) -> FnValue arch tp
  -- | Value from a phi node
  FnPhiValue :: !(FnPhiVar arch tp) -> FnValue arch tp
  -- | A value returned by a function call
  FnReturn :: !(FnReturnVar tp) -> FnValue arch tp
  -- | The pointer to a function.
  FnFunctionEntryValue ::
    -- | Type of symbol
    !(FunctionType arch) ->
    -- | Symbol name of this function.
    BSC.ByteString ->
    FnValue arch (BVType (ArchAddrWidth arch))
  -- | Value is a function.
  --
  -- The int should be in the range @[0..argCount)@, and the type repr
  -- is the type of the argument.
  FnArg :: !Int -> !(TypeRepr tp) -> FnValue arch tp

------------------------------------------------------------------------
-- FoldFnValue

class FoldFnValue (v :: Type -> Type) where
  foldFnValue ::
    forall arch s.
    ( FoldableF (FnArchStmt arch)
    , FoldableFC (ArchFn arch)
    ) =>
    (forall u. s -> FnValue arch u -> s) ->
    s ->
    v arch ->
    s

------------------------------------------------------------------------
-- FnAssignment/FnValue/FnAssignRhs basic operations

type FnArchConstraints arch =
  ( IsArchFn (ArchFn arch)
  , IsArchStmt (FnArchStmt arch)
  , MemWidth (ArchAddrWidth arch)
  , HasRepr (ArchFn arch (FnValue arch)) TypeRepr
  , HasRepr (ArchReg arch) TypeRepr
  )

instance MemWidth (ArchAddrWidth arch) => PP.Pretty (FnValue arch tp) where
  pretty (FnUndefined{}) = "undef"
  pretty (FnConstantBool b) = if b then "true" else "false"
  pretty (FnConstantValue w i)
    | i >= 0 = PP.parens ("0x" <> PP.pretty (showHex i "") <> " : " <> "bv" PP.<+> PP.pretty (natValue w))
    | otherwise = error ("FnConstantBool given negative value: " ++ show i)
  pretty (FnAssignedValue ass) = PP.pretty (fnAssignId ass)
  pretty (FnPhiValue phi) = PP.pretty (unFnPhiVar phi)
  pretty (FnReturn var) = PP.pretty var
  pretty (FnFunctionEntryValue _ n) = PP.pretty (BSC.unpack n)
  pretty (FnArg i _) = "arg" <> PP.pretty i

instance FnArchConstraints arch => PP.Pretty (FnAssignRhs arch (FnValue arch) tp) where
  pretty rhs =
    case rhs of
      FnSetUndefined w -> "undef :: " <> PP.brackets (PP.pretty w)
      FnReadMem a tp -> sexpr "read" [PP.pretty a, PP.pretty tp]
      FnCondReadMem _ c a d -> sexpr "cond_read" [PP.pretty c, PP.pretty a, PP.pretty d]
      FnEvalApp a -> ppApp PP.pretty a
      FnEvalArchFn f -> runIdentity (ppArchFn (pure . PP.pretty) f)
      FnAddrWidthConstant i
        | i >= 0 ->
            PP.parens
              ( "0x" <> PP.pretty (showHex i "") <> " : " <> "bv"
                  PP.<+> PP.pretty (8 * addrSize (Proxy :: Proxy (ArchAddrWidth arch)))
              )
        | otherwise -> error ("FnAddrWidthConstant given negative value: " ++ show i)

instance FnArchConstraints arch => PP.Pretty (FnAssignment arch tp) where
  pretty (FnAssignment lhs rhs) = PP.pretty lhs <> " := " <> PP.pretty rhs

instance FnArchConstraints arch => Show (FnValue arch tp) where
  show = show . PP.pretty
instance FnArchConstraints arch => ShowF (FnValue arch)

instance FnArchConstraints arch => Show (FnAssignment arch tp) where
  show = show . PP.pretty

instance FnArchConstraints arch => ShowF (FnAssignment arch)

archWidthTypeRepr ::
  forall p arch.
  MemWidth (ArchAddrWidth arch) =>
  p arch ->
  TypeRepr (BVType (ArchAddrWidth arch))
archWidthTypeRepr _ = BVTypeRepr (addrWidthNatRepr (addrWidthRepr (Proxy :: Proxy (ArchAddrWidth arch))))

instance
  (MemWidth (ArchAddrWidth arch), HasRepr (ArchFn arch f) TypeRepr) =>
  HasRepr (FnAssignRhs arch f) TypeRepr
  where
  typeRepr rhs =
    case rhs of
      FnSetUndefined tp -> tp
      FnReadMem _ tp -> tp
      FnCondReadMem tp _ _ _ -> typeRepr tp
      FnEvalApp a -> typeRepr a
      FnEvalArchFn f -> typeRepr f
      FnAddrWidthConstant{} -> archWidthTypeRepr (Proxy :: Proxy arch)

instance FnArchConstraints arch => HasRepr (FnValue arch) TypeRepr where
  typeRepr v =
    case v of
      FnUndefined tp -> tp
      FnConstantBool _ -> BoolTypeRepr
      FnConstantValue sz _ -> BVTypeRepr sz
      FnAssignedValue (FnAssignment _ rhs) -> typeRepr rhs
      FnPhiValue phi -> fnPhiVarType phi
      FnReturn ret -> frReturnType ret
      FnFunctionEntryValue{} -> archWidthTypeRepr (Proxy :: Proxy arch)
      FnArg _ tp -> tp

------------------------------------------------------------------------
-- FnStmt

type family FnArchStmt (arch :: Type) :: (M.Type -> Type) -> Type

data FnStmt arch where
  -- | A comment
  FnComment :: !Text -> FnStmt arch
  -- | An assignment statement
  FnAssignStmt ::
    !(FnAssignment arch tp) ->
    FnStmt arch
  -- | Writes to memory
  FnWriteMem ::
    !(FnValue arch (BVType (ArchAddrWidth arch))) ->
    !(FnValue arch tp) ->
    FnStmt arch
  -- | A conditional write to memory
  FnCondWriteMem ::
    !(FnValue arch BoolType) ->
    !(FnValue arch (BVType (ArchAddrWidth arch))) ->
    !(FnValue arch tp) ->
    !(MemRepr tp) ->
    FnStmt arch
  -- | A call to a function with some arguments and return values.
  FnCall ::
    !(FnValue arch (BVType (ArchAddrWidth arch))) ->
    [Some (FnValue arch)] ->
    -- Return values
    !(Maybe (Some FnReturnVar)) ->
    FnStmt arch
  FnArchStmt ::
    (FnArchStmt arch (FnValue arch)) ->
    FnStmt arch

instance
  ( FnArchConstraints arch
  , IsArchStmt (FnArchStmt arch)
  ) =>
  PP.Pretty (FnStmt arch)
  where
  pretty s =
    case s of
      FnComment msg -> mconcat $ (\l -> "# " <> PP.pretty l <> PP.hardline) <$> Text.lines msg
      FnAssignStmt assign -> PP.pretty assign
      FnWriteMem addr val -> "*" <> PP.pretty addr PP.<+> ":=" PP.<+> PP.pretty val
      FnCondWriteMem cond addr val _repr -> "cond_write" PP.<+> PP.pretty cond PP.<+> PP.pretty addr PP.<+> PP.pretty val
      FnCall f args mret ->
        let argDocs = (\(Some v) -> PP.pretty v) <$> args
            retDoc = case mret of
              Just (Some r) -> PP.pretty r <> " := "
              Nothing -> mempty
         in retDoc <> "call" PP.<+> PP.pretty f <> PP.parens (commas argDocs)
      FnArchStmt stmt -> ppArchStmt PP.pretty stmt

instance FoldFnValue FnStmt where
  foldFnValue f s (FnWriteMem addr v) = s `f` addr `f` v
  foldFnValue f s (FnCondWriteMem c a v _) = s `f` c `f` a `f` v
  foldFnValue _ s (FnComment{}) = s
  foldFnValue f s (FnAssignStmt (FnAssignment _ rhs)) = foldlFC f s rhs
  foldFnValue f s (FnCall fn args _) = foldl (\s' (Some v) -> f s' v) (f s fn) args
  foldFnValue f s (FnArchStmt stmt) = foldlF' f s stmt

------------------------------------------------------------------------
-- FnBlockLabel

-- | A block label
newtype FnBlockLabel w = FnBlockLabel {fnBlockLabelAddr :: MemSegmentOff w}
  deriving (Eq, Ord)

-- | Render block label from segment offset.
fnBlockLabelFromAddr :: MemSegmentOff w -> FnBlockLabel w
fnBlockLabelFromAddr = FnBlockLabel

instance PP.Pretty (FnBlockLabel w) where
  pretty (FnBlockLabel s) =
    let a = segoffAddr s
        o = memWordToUnsigned (addrOffset a)
     in "block_" <> PP.pretty (addrBase a) <> "_" <> PP.pretty (showHex o "")

-- | Render block label as a string
fnBlockLabelString :: FnBlockLabel w -> String
fnBlockLabelString = show . PP.pretty

------------------------------------------------------------------------
-- FnJumpTarget

-- | A jump target along with values to assign the phi variables when
-- jumping.
data FnJumpTarget arch = FnJumpTarget
  { fnJumpLabel :: !(FnBlockLabel (ArchAddrWidth arch))
  -- ^ Label of block we are jumping to.
  , fnJumpPhiValues :: !(V.Vector (Some (FnValue arch)))
  -- ^ Values to assign to phi variables for block.
  --
  -- These must match the type of the jump target.
  }

instance MemWidth (ArchAddrWidth arch) => PP.Pretty (FnJumpTarget arch) where
  pretty tgt = PP.pretty (fnJumpLabel tgt) PP.<+> PP.encloseSep PP.lbracket PP.rbracket " " phiVals
   where
    phiVals = V.toList $ viewSome PP.pretty <$> fnJumpPhiValues tgt

------------------------------------------------------------------------
-- FnTermStmt

-- | The terminal statement from a block.
data FnTermStmt arch
  = FnJump !(FnJumpTarget arch)
  | FnBranch
      !(FnValue arch BoolType)
      !(FnJumpTarget arch)
      !(FnJumpTarget arch)
  | FnLookupTable
      !(FnValue arch (BVType (ArchAddrWidth arch)))
      !(V.Vector (FnJumpTarget arch))
  | -- | A return from the function with associated values
    FnRet !(Maybe (Some (FnValue arch)))
  | -- | A call statement to the given location with the arguments
    -- listed that does not return.
    FnTailCall -- Address of function to jump to.
      !(FnValue arch (BVType (ArchAddrWidth arch)))
      -- Arguments (must match function type)
      [Some (FnValue arch)]

instance
  FnArchConstraints arch =>
  PP.Pretty (FnTermStmt arch)
  where
  pretty s =
    case s of
      FnJump a -> "jump " <> PP.pretty a
      FnBranch c x y -> "branch" PP.<+> PP.pretty c PP.<+> PP.pretty x PP.<+> PP.pretty y
      FnLookupTable idx vec ->
        let entries = commas $ V.toList $ PP.pretty <$> vec
         in "lookup" PP.<+> PP.pretty idx <> " in " <> PP.parens entries
      FnRet Nothing -> "return void"
      FnRet (Just (Some ret)) -> "return " <> PP.pretty ret
      FnTailCall f args ->
        let argDocs = (\(Some v) -> PP.pretty v) <$> args
         in "tail_call" PP.<+> PP.pretty f <> PP.parens (commas argDocs)

instance FoldFnValue FnTermStmt where
  foldFnValue _ s (FnJump{}) = s
  foldFnValue f s (FnBranch c _ _) = f s c
  foldFnValue f s (FnLookupTable idx _) = s `f` idx
  foldFnValue f s (FnRet rets) = foldl (\t (Some v) -> f t v) s rets
  foldFnValue f s (FnTailCall fn args) =
    foldl (\s' (Some v) -> f s' v) (f s fn) args

------------------------------------------------------------------------
-- FnBlockInvariant

-- | Invariants about a block location.
data FnBlockInvariant arch where
  -- | @FnCalleeSavedReg r x@ indices that @r@ is a callee saved
  -- register, and @x:@ is a location that store the value @r@ had
  -- when the function started execution.
  FnCalleeSavedReg ::
    ArchReg arch tp ->
    BoundLoc (ArchReg arch) tp ->
    FnBlockInvariant arch
  -- | @FnEqualLocs x y@ indicares that the value stored in the
  -- locations @x@ and @y@ are all equal.
  FnEqualLocs ::
    BoundLoc (ArchReg arch) tp ->
    BoundLoc (ArchReg arch) tp ->
    FnBlockInvariant arch
  -- | @FnStackOffset o x@ indices that @x@ stores the value in the
  -- frame pointer plus @o@.
  --
  -- @o@ is typically negative on processors whose stacks grow down.
  FnStackOff ::
    !(MemInt (ArchAddrWidth arch)) ->
    !(BoundLoc (ArchReg arch) (BVType (ArchAddrWidth arch))) ->
    FnBlockInvariant arch

------------------------------------------------------------------------
-- FnBlock

-- | Indicates the type of access and whether it accessed heap or stack.
data FnMemAccessType
  = HeapAccess
  | StackAccess

-- | A block in the function.
--
-- This representation is designed to be both easy to generate LLVM and generate
-- block annotations for `reopt-vcg`.
data FnBlock arch = FnBlock
  { fbLabel :: !(FnBlockLabel (ArchAddrWidth arch))
  -- ^ Label for identifying block.
  , fbPrecond :: !(ArchBlockPrecond arch)
  -- ^ Architecture-specifici information assumed to be
  -- true when jumping to this block.
  , fbSize :: !Word64
  -- ^ Number of bytes in the machine code for this block.
  , fbPrevBlocks :: ![FnBlockLabel (ArchAddrWidth arch)]
  -- ^ Labels of blocks that jump to this one.
  , fbInvariants :: ![FnBlockInvariant arch]
  -- ^ Invariants inferred about machine code relevant to
  -- this translation.
  , fbPhiVars :: !(V.Vector (Some (FnPhiVar arch)))
  -- ^ Vector of phi variables that block expects to be assigned.
  , fbStmts :: ![FnStmt arch]
  -- ^ List of non-terminal statements in block.
  , fbTerm :: !(FnTermStmt arch)
  -- ^ Final terminal statement in block.
  , fbMemInsnAddrs :: !(V.Vector (Word64, FnMemAccessType))
  -- ^ Vector contains a pair @(off, atp)@ for each
  -- machine code instruction that accessed memory.  The
  -- offset @off@ is the offset of the start address of
  -- the instruction, and @atp@ indicates properties
  -- inferred about the acccess.
  }

instance
  ( FnArchConstraints arch
  , ShowF (ArchReg arch)
  , IsArchStmt (FnArchStmt arch)
  ) =>
  PP.Pretty (FnBlock arch)
  where
  pretty b =
    PP.pretty (fbLabel b)
      PP.<+> PP.encloseSep PP.lbracket PP.rbracket " " phiVars
        <> PP.hardline
        <> PP.indent 2 (phiBindings <> stmts <> tstmt)
   where
    ppPhiName v = PP.parens (PP.pretty (unFnPhiVar v) PP.<+> PP.pretty (fnPhiVarType v))
    phiVars = V.toList $ viewSome ppPhiName <$> fbPhiVars b
    ppBinding v l = PP.parens ("mc_binding " <> v PP.<+> PP.pretty l) <> PP.hardline

    ppPhiBindings :: Some (FnPhiVar arch) -> PP.Doc a
    ppPhiBindings (Some v) = foldMap (ppBinding (PP.pretty (unFnPhiVar v))) locs
     where
      locs = fnPhiVarRep v : fnPhiVarLocations v

    phiBindings = foldMap ppPhiBindings (fbPhiVars b)
    stmts = foldMap (\s -> PP.pretty s <> PP.hardline) (fbStmts b)
    tstmt = PP.pretty (fbTerm b)

instance FoldFnValue FnBlock where
  foldFnValue f s0 b = foldFnValue f (foldl (foldFnValue f) s0 (fbStmts b)) (fbTerm b)

------------------------------------------------------------------------
-- Function definitions

-- | A representation of machine-code function after stack allocation and
-- registers have been removed.
--
-- This currently isn't the case, as Phi nodes still use `ArchReg` to index the
-- nodes.  However, this will be changed.
data Function arch = Function
  { fnAddr :: !(MemSegmentOff (ArchAddrWidth arch))
  -- ^ The address for this function
  , fnType :: !(FunctionType arch)
  -- ^ Type of this  function
  , fnName :: !BSC.ByteString
  -- ^ Name of this function
  --
  -- This is required to be unique for the executable.
  , fnEntryBlock :: !(FnBlock arch)
  , -- Initial entry block.
    fnRestBlocks :: ![FnBlock arch]
  -- ^ A list of function blocks after entry.
  }

-- | Return blocks in function with entry block first.
fnBlocks :: Function arch -> [FnBlock arch]
fnBlocks f = fnEntryBlock f : fnRestBlocks f

instance
  ( FnArchConstraints arch
  , ShowF (ArchReg arch)
  , IsArchStmt (FnArchStmt arch)
  ) =>
  PP.Pretty (Function arch)
  where
  pretty fn =
    let nm = PP.pretty (BSC.unpack (fnName fn))
        addr = PP.pretty (fnAddr fn)
        ftp = fnType fn
        ppArg :: Integer -> Some TypeRepr -> PP.Doc a
        ppArg i (Some tp) = "arg" <> PP.pretty i <> " : " <> PP.pretty tp
        atp = PP.parens (commas (zipWith ppArg [0 ..] (fnArgTypes ftp)))
        rtp = case fnReturnType ftp of
          Nothing -> "void"
          Just (Some tp) -> PP.pretty tp
     in PP.vcat
          [ "function " <> nm <> " @ " <> addr <> atp <> " : " <> rtp
          , PP.lbrace
          , PP.indent 4 $ PP.vcat (PP.pretty <$> fnBlocks fn)
          , PP.rbrace
          ]

-- | A function declaration that has type information, but no recovered definition.
data FunctionDecl arch = FunctionDecl
  { funDeclAddr :: !(MemSegmentOff (ArchAddrWidth arch))
  -- ^ Address of function in binary.
  , funDeclName :: !BSC.ByteString
  -- ^ Symbol name for function.
  , funDeclType :: !(FunctionType arch)
  -- ^ Type of function
  , funDeclNoReturn :: !Bool
  -- ^ Whether function is marked as no-return.
  }

-- | A set of function declarations and definitions needed to construct an LLVM
-- module.
data RecoveredModule arch = RecoveredModule
  { recoveredDecls :: ![FunctionDecl arch]
  , recoveredDefs :: ![Function arch]
  }
