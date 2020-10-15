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
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.AbsDomain.StackAnalysis (BoundLoc)
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
import           Data.Macaw.Memory
import qualified Data.Macaw.Types as M (Type)
import           Data.Macaw.Types hiding (Type)


-- | Utility to pretty print with commas separating arguments.
commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')

------------------------------------------------------------------------
-- FnAssignId

-- | A unique identifier for an assignment, phi variable, or return.
newtype FnAssignId = FnAssignId Word64
  deriving (Eq, Ord)

instance Show FnAssignId where
  showsPrec _ p = shows (pretty p)

instance Pretty FnAssignId where
  pretty (FnAssignId w) = text ('r' : show w)

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

instance FoldableFC (ArchFn arch) => FoldableFC (FnAssignRhs arch) where
  foldrFC _ s (FnSetUndefined {}) = s
  foldrFC f s (FnReadMem loc _)   = f loc s
  foldrFC f s (FnCondReadMem _ c a d)   = f c (f a (f d s))
  foldrFC f s (FnEvalApp a)       = foldrFC f s a
  foldrFC f s (FnEvalArchFn fn)   = foldrFC f s fn

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
                          -- ^ Symbol name to associate this this address.
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
  pretty (FnUndefined {})         = text "undef"
  pretty (FnConstantBool b)       = text $ if b then "true" else "false"
  pretty (FnConstantValue w i)
    | i >= 0 = parens (text ("0x" ++ showHex i "")
                        <+> text ":" <+> text "bv" <+> text (show w))
    | otherwise = error ("FnConstantBool given negative value: " ++ show i)
  pretty (FnAssignedValue ass)    = pretty (fnAssignId ass)
  pretty (FnPhiValue phi)         = pretty (unFnPhiVar phi)
  pretty (FnReturn var)           = pretty var
  pretty (FnFunctionEntryValue _ n) = text (BSC.unpack n)
  pretty (FnArg i _)              = text "arg" <> int i

instance FnArchConstraints arch => Pretty (FnAssignRhs arch (FnValue arch) tp) where
  pretty rhs =
    case rhs of
      FnSetUndefined w -> text "undef ::" <+> brackets (text (show w))
      FnReadMem a tp -> sexpr "read" [ pretty a, pretty tp]
      FnCondReadMem _ c a d -> sexpr "cond_read" [ pretty c, pretty a, pretty d ]
      FnEvalApp a -> ppApp pretty a
      FnEvalArchFn f -> runIdentity (ppArchFn (pure . pretty) f)

instance FnArchConstraints arch => Pretty (FnAssignment arch tp) where
  pretty (FnAssignment lhs rhs) = pretty lhs <+> text ":=" <+> pretty rhs

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
      FnComment msg -> text "#" <+> text (Text.unpack msg)
      FnAssignStmt assign -> pretty assign
      FnWriteMem addr val -> text "write" <+> pretty addr <+> pretty val
      FnCondWriteMem cond addr val _repr -> text "cond_write" <+> pretty cond <+> pretty addr <+> pretty val
      FnCall f args mret ->
        let argDocs = (\(Some v) -> pretty v) <$> args
            retDoc = case mret of
                       Just (Some r) -> pretty r <+> text ":= "
                       Nothing -> mempty
         in retDoc <> text "call" <+> pretty f <> parens (commas argDocs)
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

-- | Render block label as a string
fnBlockLabelString :: FnBlockLabel w -> String
fnBlockLabelString (FnBlockLabel s) =
  let a = segoffAddr s
   in "block_" ++ show (addrBase a) ++ "_" ++ showHex (memWordToUnsigned (addrOffset a)) ""

instance Pretty (FnBlockLabel w) where
  pretty = text . fnBlockLabelString

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
  pretty tgt = pretty (fnJumpLabel tgt) <+> encloseSep lbracket rbracket (text " ") phiVals
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
      FnJump a -> text "jump" <+> pretty a
      FnBranch c x y -> text "branch" <+> pretty c <+> pretty x <+> pretty y
      FnLookupTable idx vec -> text "lookup" <+> pretty idx <+> text "in"
                               <+> parens (commas $ V.toList $ pretty <$> vec)
      FnRet Nothing           -> text "return void"
      FnRet (Just (Some ret)) -> text "return" <+> pretty ret
      FnTailCall f args ->
        let argDocs = (\(Some v) -> pretty v) <$> args
         in text "tail_call" <+> pretty f <> parens (commas argDocs)

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
    pretty (fbLabel b) <+> encloseSep lbracket rbracket (text " ") phiVars <$$>
      indent 2 (vcat phiBindings <$$> vcat stmts <$$> tstmt)
    where
      ppPhiName v = parens (pretty (unFnPhiVar v) <+> pretty (fnPhiVarType v))
      phiVars = V.toList $ viewSome ppPhiName <$> fbPhiVars b
      ppBinding v l = parens (text "mc_binding" <+> v <+> pretty l)
      ppPhiBindings v = vcat $ ppBinding (pretty (unFnPhiVar v)) <$> locs
        where locs = fnPhiVarRep v : fnPhiVarLocations v
      phiBindings = V.toList $ viewSome ppPhiBindings <$> fbPhiVars b
      stmts = pretty <$> fbStmts b
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
        addr = pretty (show (fnAddr fn))
        ftp = fnType fn
        ppArg :: Integer -> Some TypeRepr -> Doc
        ppArg i (Some tp) = text "arg" <> text (show i) <+> text ":" <+> pretty tp
        atp = parens (commas (zipWith ppArg [0..] (fnArgTypes ftp)))
        rtp = case fnReturnType ftp of
                Nothing -> text "void"
                Just (Some tp) -> pretty tp
     in text "function" <+> nm <+> text "@" <+> addr <> atp <+> text ":" <+> rtp
        <$$> lbrace
        <$$> (nest 4 $ vcat (pretty <$> fnBlocks fn))
        <$$> rbrace

-- | A function declaration that has type information, but no recovered definition.
data FunctionDecl arch =
  FunctionDecl { funDeclName :: !BSC.ByteString
                 -- ^ Symbol name for function.
               , funDeclType :: !(FunctionType arch)
               }

-- | A set of function declarations and definitions needed to
-- construct an LLVM module.
data RecoveredModule arch
   = RecoveredModule { recoveredDecls :: ![FunctionDecl arch]
                     , recoveredDefs  :: ![Function arch]
                     }
