{-|
Defines a program representation with machine registers replaced by
values.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reopt.CFG.FnRep
   ( RecoveredModule(..)
   , FunctionDecl(..)
   , Function(..)
   , fnBlocks
   , FnAssignId(..)
   , FnAssignment(..)
   , FnAssignRhs(..)
   , FnValue(..)
   , FunctionType(..)
   , FnBlock(..)
   , FnBlockInvariant(..)
   , FnMemAccessType(..)
   , FnBlockLabel
   , fnBlockLabelFromAddr
   , fnBlockLabelAddr
   , fnBlockLabelString
   , FnStmt(..)
   , FnTermStmt(..)
   , FnJumpTarget(..)
   , FnPhiVar(..)
   , FnReturnVar(..)
   , FnArchStmt
   , FoldFnValue(..)
   , FnArchConstraints
     -- ArchBlockPrecond
   ) where

import           Control.Monad.Identity
import qualified Data.ByteString.Char8 as BSC
import           Data.Kind
import           Data.Parameterized.Classes
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           Prettyprinter

import           Data.Macaw.CFG
   ( App(..)
   , ppApp
   , sexpr
   , ArchFn
   , ArchAddrWidth
   , ArchBlockPrecond
   , ArchReg
   , IsArchFn(..)
   , IsArchStmt(..)
   , MemRepr(..)
   )
import           Data.Macaw.Analysis.RegisterUse (BoundLoc)
import           Data.Macaw.Memory
import qualified Data.Macaw.Types as M (Type)
import           Data.Macaw.Types hiding (Type)


-- | Utility to pretty print with commas separating arguments.
commas :: [Doc a] -> Doc a
commas = hsep . punctuate comma

------------------------------------------------------------------------
-- FnAssignId

-- | A unique identifier for an assignment, phi variable, or return.
newtype FnAssignId = FnAssignId Word64
  deriving (Eq, Ord)

instance Show FnAssignId where
  showsPrec _ p = shows (pretty p)

instance Pretty FnAssignId where
  pretty (FnAssignId w) = "r" <> pretty w

------------------------------------------------------------------------
-- FnPhiVar

-- | A phi variable
data FnPhiVar arch (tp :: M.Type) =
  FnPhiVar { unFnPhiVar :: !FnAssignId
           , fnPhiVarType :: !(TypeRepr tp)
           , fnPhiVarRep :: !(BoundLoc (ArchReg arch) tp)
             -- ^ Class representative for locations that lead to this
             -- phi variable.
           , fnPhiVarLocations :: ![BoundLoc (ArchReg arch) tp]
             -- ^ Locations read after this phi variable is introduced that
             -- are equivalent to this phi variable.
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

data FnReturnVar tp = FnReturnVar { frAssignId :: !FnAssignId
                                  , frReturnType :: !(TypeRepr tp)
                                  }

instance Pretty (FnReturnVar tp) where
  pretty = pretty . frAssignId

instance HasRepr FnReturnVar TypeRepr where
  typeRepr = frReturnType

------------------------------------------------------------------------
-- FunctionType

-- | Describes the type of a function.
data FunctionType (arch :: Type) =
  FunctionType { fnArgTypes :: ![Some TypeRepr]
               , fnVarArgs :: !Bool
               , fnReturnType :: !(Maybe (Some TypeRepr))
               }
  deriving (Ord, Eq, Show)

------------------------------------------------------------------------
-- FnAssignRhs mutually recursive constructors

-- | The right-hand side of a function assignment statement.
--
-- The first type paramter is the architecture.
-- The second type parameter is used for arguments to the operations described
-- by this.
-- The third type parameter is for the type of value returned.
data FnAssignRhs (arch :: Type) (f :: M.Type -> Type) (tp :: M.Type) where
  -- | An expression with an undefined value.
  --
  -- NB. Undefined values in this representation are values with a
  -- non-deterministically chosen value.
  FnSetUndefined :: !(TypeRepr tp) -- ^ Type of undefined value.
                 -> FnAssignRhs arch f tp
  -- | A unconditional memory read.
  FnReadMem :: !(f (BVType (ArchAddrWidth arch)))
            -> !(TypeRepr tp)
            -> FnAssignRhs arch f tp
  -- | A conditional memory read.
  FnCondReadMem :: !(MemRepr tp)
                -> !(f BoolType)
                -> !(f (BVType (ArchAddrWidth arch)))
                -> !(f tp)
                -> FnAssignRhs arch f tp
  -- | Evaluate the pure function given by the App.
  FnEvalApp :: !(App f tp)
            -> FnAssignRhs arch f tp
  -- | Evaluate an architeture-specific function.
  FnEvalArchFn :: !(ArchFn arch f tp)
               -> FnAssignRhs arch f tp

  -- | Arch-addr-width constant that may be a pointer.  Used to enable
  -- easier type assignment when translating to LLVM.
  FnAddrWidthConstant :: Integer
                      -> FnAssignRhs arch f (BVType (ArchAddrWidth arch))

instance FoldableFC (ArchFn arch) => FoldableFC (FnAssignRhs arch) where
  foldrFC _ s (FnSetUndefined {}) = s
  foldrFC f s (FnReadMem loc _)   = f loc s
  foldrFC f s (FnCondReadMem _ c a d)   = f c (f a (f d s))
  foldrFC f s (FnEvalApp a)       = foldrFC f s a
  foldrFC f s (FnEvalArchFn fn)   = foldrFC f s fn
  foldrFC _ s (FnAddrWidthConstant {}) = s

------------------------------------------------------------------------
-- FnAssignment/FnValue mutually recursive constructors

data FnAssignment arch tp
   = FnAssignment { fnAssignId :: !FnAssignId
                  , fnAssignRhs :: !(FnAssignRhs arch (FnValue arch) tp)
                  }

-- | A function value.
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
  FnFunctionEntryValue :: !(FunctionType arch)
                          -- ^ Type of symbol
                       -> BSC.ByteString
                          -- ^ Symbol name of this function.
                       -> FnValue arch (BVType (ArchAddrWidth arch))
  -- | Value is a function.
  --
  -- The int should be in the range @[0..argCount)@, and the type repr
  -- is the type.
  FnArg :: !Int -> !(TypeRepr tp) -> FnValue arch tp

------------------------------------------------------------------------
-- FoldFnValue

class FoldFnValue (v :: Type -> Type)  where
  foldFnValue :: forall arch s
              .  ( FoldableF (FnArchStmt arch)
                 , FoldableFC (ArchFn arch)
                 )
              => (forall u . s -> FnValue arch u -> s)
              -> s
              -> v arch
              -> s

------------------------------------------------------------------------
-- FnAssignment/FnValue/FnAssignRhs basic operations

type FnArchConstraints arch =
     ( IsArchFn (ArchFn arch)
     , MemWidth (ArchAddrWidth arch)
     , HasRepr (ArchFn arch (FnValue arch)) TypeRepr
     , HasRepr (ArchReg arch) TypeRepr
     )

instance MemWidth (ArchAddrWidth arch) => Pretty (FnValue arch tp) where
  pretty (FnUndefined {})         = "undef"
  pretty (FnConstantBool b)       = if b then "true" else "false"
  pretty (FnConstantValue w i)
    | i >= 0 = parens ("0x" <> pretty (showHex i "") <> " : " <> "bv" <+> pretty (natValue w))
    | otherwise = error ("FnConstantBool given negative value: " ++ show i)
  pretty (FnAssignedValue ass)    = pretty (fnAssignId ass)
  pretty (FnPhiValue phi)         = pretty (unFnPhiVar phi)
  pretty (FnReturn var)           = pretty var
  pretty (FnFunctionEntryValue _ n) = pretty (BSC.unpack n)
  pretty (FnArg i _)              = "arg" <> pretty i

instance FnArchConstraints arch => Pretty (FnAssignRhs arch (FnValue arch) tp) where
  pretty rhs =
    case rhs of
      FnSetUndefined w -> "undef :: " <> brackets (pretty w)
      FnReadMem a tp -> sexpr "read" [ pretty a, pretty tp]
      FnCondReadMem _ c a d -> sexpr "cond_read" [ pretty c, pretty a, pretty d ]
      FnEvalApp a -> ppApp pretty a
      FnEvalArchFn f -> runIdentity (ppArchFn (pure . pretty) f)
      FnAddrWidthConstant i
        | i >= 0 -> parens ("0x" <> pretty (showHex i "") <> " : " <> "bv"
                             <+> pretty (8 * addrSize (Proxy :: Proxy (ArchAddrWidth arch))))
        | otherwise -> error ("FnAddrWidthConstant given negative value: " ++ show i)

instance FnArchConstraints arch => Pretty (FnAssignment arch tp) where
  pretty (FnAssignment lhs rhs) = pretty lhs <> " := " <> pretty rhs

instance FnArchConstraints arch => Show (FnValue arch tp) where
  show = show . pretty
instance FnArchConstraints arch => ShowF (FnValue arch)

instance FnArchConstraints arch => Show (FnAssignment arch tp) where
  show = show . pretty

instance FnArchConstraints arch => ShowF (FnAssignment arch)

archWidthTypeRepr :: forall p arch
                  .  MemWidth (ArchAddrWidth arch)
                  => p arch
                  -> TypeRepr (BVType (ArchAddrWidth arch))
archWidthTypeRepr _ = BVTypeRepr (addrWidthNatRepr (addrWidthRepr (Proxy :: Proxy (ArchAddrWidth arch))))

instance (MemWidth (ArchAddrWidth arch), HasRepr (ArchFn arch f) TypeRepr)
      => HasRepr (FnAssignRhs arch f) TypeRepr where
  typeRepr rhs =
    case rhs of
      FnSetUndefined tp -> tp
      FnReadMem _ tp -> tp
      FnCondReadMem tp _ _ _ -> typeRepr tp
      FnEvalApp a    -> typeRepr a
      FnEvalArchFn f -> typeRepr f
      FnAddrWidthConstant {} -> archWidthTypeRepr (Proxy :: Proxy arch)

instance FnArchConstraints arch => HasRepr (FnValue arch) TypeRepr where
  typeRepr v =
    case v of
      FnUndefined tp -> tp
      FnConstantBool _ -> BoolTypeRepr
      FnConstantValue sz _ -> BVTypeRepr sz
      FnAssignedValue (FnAssignment _ rhs) -> typeRepr rhs
      FnPhiValue phi -> fnPhiVarType phi
      FnReturn ret   -> frReturnType ret
      FnFunctionEntryValue {} -> archWidthTypeRepr (Proxy :: Proxy arch)
      FnArg _ tp -> tp

instance (MemWidth (ArchAddrWidth arch), HasRepr (ArchFn arch (FnValue arch)) TypeRepr)
      => HasRepr (FnAssignment arch) TypeRepr where
  typeRepr a = typeRepr (fnAssignRhs a)

------------------------------------------------------------------------
-- FnStmt

type family FnArchStmt (arch :: Type) :: (M.Type -> Type) -> Type

data FnStmt arch where
  -- | A comment
  FnComment :: !Text -> FnStmt arch
  -- | An assignment statement
  FnAssignStmt :: !(FnAssignment arch tp)
               -> FnStmt arch
  -- | Writes to memory
  FnWriteMem :: !(FnValue arch (BVType (ArchAddrWidth arch)))
             -> !(FnValue arch tp)

             -> FnStmt arch
  -- | A conditional write to memory
  FnCondWriteMem :: !(FnValue arch BoolType)
                 -> !(FnValue arch (BVType (ArchAddrWidth arch)))
                 -> !(FnValue arch tp)
                 -> !(MemRepr tp)
                 -> FnStmt arch
  -- | A call to a function with some arguments and return values.
  FnCall :: !(FnValue arch (BVType (ArchAddrWidth arch)))
         -> [Some (FnValue arch)]
            -- Return values
         -> !(Maybe (Some FnReturnVar))
         -> FnStmt arch
  FnArchStmt :: (FnArchStmt arch (FnValue arch))
             -> FnStmt arch

instance ( FnArchConstraints arch
         , IsArchStmt (FnArchStmt arch)
         )
      => Pretty (FnStmt arch) where
  pretty s =
    case s of
      FnComment msg -> mconcat $ (\l -> "# " <> pretty l <> hardline) <$> Text.lines msg
      FnAssignStmt assign -> pretty assign
      FnWriteMem addr val -> "*" <> pretty addr <+> ":=" <+> pretty val
      FnCondWriteMem cond addr val _repr -> "cond_write" <+> pretty cond <+> pretty addr <+> pretty val
      FnCall f args mret ->
        let argDocs = (\(Some v) -> pretty v) <$> args
            retDoc = case mret of
                       Just (Some r) -> pretty r <> " := "
                       Nothing -> mempty
         in retDoc <> "call" <+> pretty f <> parens (commas argDocs)
      FnArchStmt stmt -> ppArchStmt pretty stmt

instance FoldFnValue FnStmt where
  foldFnValue f s (FnWriteMem addr v)                 = s `f` addr `f` v
  foldFnValue f s (FnCondWriteMem c a v _)            = s `f` c `f` a `f` v
  foldFnValue _ s (FnComment {})                      = s
  foldFnValue f s (FnAssignStmt (FnAssignment _ rhs)) = foldlFC f s rhs
  foldFnValue f s (FnCall fn args _) = foldl (\s' (Some v) -> f s' v) (f s fn) args
  foldFnValue f s (FnArchStmt stmt) = foldlF' f s stmt

------------------------------------------------------------------------
-- FnBlockLabel

-- | A block label
newtype FnBlockLabel w = FnBlockLabel { fnBlockLabelAddr :: MemSegmentOff w }
  deriving (Eq, Ord)

-- | Render block label from segment offset.
fnBlockLabelFromAddr ::  MemSegmentOff w -> FnBlockLabel w
fnBlockLabelFromAddr = FnBlockLabel

instance Pretty (FnBlockLabel w) where
  pretty (FnBlockLabel s) =
    let a = segoffAddr s
        o = memWordToUnsigned (addrOffset a)
     in "block_" <> pretty (addrBase a) <> "_" <> pretty (showHex o "")

-- | Render block label as a string
fnBlockLabelString :: FnBlockLabel w -> String
fnBlockLabelString = show . pretty

------------------------------------------------------------------------
-- FnJumpTarget

-- | A jump target along with values to assign the phi variables when
-- jumping.
data FnJumpTarget arch =
  FnJumpTarget { fnJumpLabel :: !(FnBlockLabel (ArchAddrWidth arch))
                 -- ^ Label of block we are jumping to.
               , fnJumpPhiValues :: !(V.Vector (Some (FnValue arch)))
                 -- ^ Values to assign to phi variables for block.
                 --
                 -- These must match the type of the jump target.
               }

instance MemWidth (ArchAddrWidth arch) => Pretty (FnJumpTarget arch) where
  pretty tgt = pretty (fnJumpLabel tgt) <+> encloseSep lbracket rbracket " " phiVals
    where phiVals = V.toList $ viewSome pretty <$> fnJumpPhiValues tgt

------------------------------------------------------------------------
-- FnTermStmt

-- | The terminal statement from a block.
data FnTermStmt arch
   = FnJump !(FnJumpTarget arch)
   | FnBranch !(FnValue arch BoolType)
              !(FnJumpTarget arch)
              !(FnJumpTarget arch)
   | FnLookupTable !(FnValue arch (BVType (ArchAddrWidth arch)))
                   !(V.Vector (FnJumpTarget arch))
   | FnRet !(Maybe (Some (FnValue arch)))
     -- ^ A return from the function with associated values
   | FnTailCall -- Address of function to jump to.
                !(FnValue arch (BVType (ArchAddrWidth arch)))
                -- Arguments (must match function type)
                [Some (FnValue arch)]
     -- ^ A call statement to the given location with the arguments
     -- listed that does not return.

instance FnArchConstraints arch
      => Pretty (FnTermStmt arch) where
  pretty s =
    case s of
      FnJump a -> "jump " <> pretty a
      FnBranch c x y -> "branch" <+> pretty c <+> pretty x <+> pretty y
      FnLookupTable idx vec ->
        let entries = commas $ V.toList $ pretty <$> vec
         in "lookup" <+> pretty idx <> " in " <> parens entries
      FnRet Nothing           -> "return void"
      FnRet (Just (Some ret)) -> "return " <> pretty ret
      FnTailCall f args ->
        let argDocs = (\(Some v) -> pretty v) <$> args
         in "tail_call" <+> pretty f <> parens (commas argDocs)

instance FoldFnValue FnTermStmt where
  foldFnValue _ s (FnJump {})          = s
  foldFnValue f s (FnBranch c _ _)     = f s c
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
  FnCalleeSavedReg :: ArchReg arch tp
                   -> BoundLoc (ArchReg arch) tp
                   -> FnBlockInvariant arch
  -- | @FnEqualLocs x y@ indicares that the value stored in the
  -- locations @x@ and @y@ are all equal.
  FnEqualLocs :: BoundLoc (ArchReg arch) tp
              -> BoundLoc (ArchReg arch) tp
              -> FnBlockInvariant arch
  -- | @FnStackOffset o x@ indices that @x@ stores the value in the
  -- frame pointer plus @o@.
  --
  -- @o@ is typically negative on processors whose stacks grow down.
  FnStackOff :: !(MemInt (ArchAddrWidth arch))
             -> !(BoundLoc (ArchReg arch) (BVType (ArchAddrWidth arch)))
             -> FnBlockInvariant arch

------------------------------------------------------------------------
-- FnBlock

-- | Indicates the type of access and whether it accessed heap or stack.
data FnMemAccessType
   = HeapAccess
   | StackAccess

-- | A block in the function.
--
-- This representation is designed to be both easy to generate LLVM
-- and generate block annotations for `reopt-vcg`.
data FnBlock arch
   = FnBlock { fbLabel :: !(FnBlockLabel (ArchAddrWidth arch))
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
             , fbStmts :: ![FnStmt     arch]
               -- ^ List of non-terminal statements in block.
             , fbTerm  :: !(FnTermStmt arch)
               -- ^ Final terminal statement in block.
             , fbMemInsnAddrs :: !(V.Vector (Word64, FnMemAccessType))
               -- ^ Vector contains a pair @(off, atp)@ for each
               -- machine code instruction that accessed memory.  The
               -- offset @off@ is the offset of the start address of
               -- the instruction, and @atp@ indicates properties
               -- inferred about the acccess.
             }

instance (FnArchConstraints arch
         , ShowF (ArchReg arch)
         , IsArchStmt (FnArchStmt arch)
         )
      => Pretty (FnBlock arch) where
  pretty b =
      pretty (fbLabel b) <+> encloseSep lbracket rbracket " " phiVars <> hardline
      <> indent 2 (phiBindings <> stmts <> tstmt)
    where
      ppPhiName v = parens (pretty (unFnPhiVar v) <+> pretty (fnPhiVarType v))
      phiVars = V.toList $ viewSome ppPhiName <$> fbPhiVars b
      ppBinding v l = parens ("mc_binding " <> v <+> pretty l) <> hardline

      ppPhiBindings :: Some (FnPhiVar arch) -> Doc a
      ppPhiBindings (Some v) = foldMap (ppBinding (pretty (unFnPhiVar v))) locs
        where locs = fnPhiVarRep v : fnPhiVarLocations v

      phiBindings = foldMap ppPhiBindings (fbPhiVars b)
      stmts = foldMap (\s -> pretty s <> hardline) (fbStmts b)
      tstmt = pretty (fbTerm b)
instance FoldFnValue FnBlock where
  foldFnValue f s0 b = foldFnValue f (foldl (foldFnValue f) s0 (fbStmts b)) (fbTerm b)

------------------------------------------------------------------------
-- Function definitions

-- | A representation of machine-code function after stack alocation
-- and registers have been removed.
--
-- This currently isn't the case, as Phi nodes still use `ArchReg` to
-- index the nodes.  However, this will be changed.
data Function arch
   = Function { fnAddr :: !(MemSegmentOff (ArchAddrWidth arch))
                -- ^ The address for this function
              , fnType :: !(FunctionType arch)
                -- ^ Type of this  function
              , fnName :: !BSC.ByteString
                -- ^ Name of this function
                --
                -- This is required to be unique for the executable.
              , fnEntryBlock :: !(FnBlock arch)
                -- Initial entry block.
              , fnRestBlocks :: ![FnBlock arch]
                -- ^ A list of function blocks after entry.
              }

-- | Return blocks in function with entry block first.
fnBlocks :: Function arch -> [FnBlock arch]
fnBlocks f = fnEntryBlock f : fnRestBlocks f

instance (FnArchConstraints arch
         , ShowF (ArchReg arch)
         , IsArchStmt (FnArchStmt arch)
         )
      => Pretty (Function arch) where
  pretty fn =
    let nm = pretty (BSC.unpack (fnName fn))
        addr = pretty (fnAddr fn)
        ftp = fnType fn
        ppArg :: Integer -> Some TypeRepr -> Doc a
        ppArg i (Some tp) = "arg" <> pretty i <> " : " <> pretty tp
        atp = parens (commas (zipWith ppArg [0..] (fnArgTypes ftp)))
        rtp = case fnReturnType ftp of
                Nothing -> "void"
                Just (Some tp) -> pretty tp
     in vcat [ "function " <> nm <> " @ " <> addr <> atp <> " : " <> rtp
             , lbrace
             , nest 4 $ vcat (pretty <$> fnBlocks fn)
             , rbrace
             ]

-- | A function declaration that has type information, but no recovered definition.
data FunctionDecl arch =
  FunctionDecl { -- | Address of function in binary.
                 funDeclAddr :: !(MemSegmentOff (ArchAddrWidth arch)),
                 -- | Symbol name for function.
                 funDeclName :: !BSC.ByteString,
                 -- | Type of function
                 funDeclType :: !(FunctionType arch),
                 -- | Whether function is marked as no-return.
                 funDeclNoReturn :: !Bool
                }

-- | A set of function declarations and definitions needed to
-- construct an LLVM module.
data RecoveredModule arch
   = RecoveredModule { recoveredDecls :: ![FunctionDecl arch]
                     , recoveredDefs  :: ![Function arch]
                     }
