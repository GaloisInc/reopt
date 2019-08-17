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
   , FnBlockLabel
   , fnBlockLabelUnpack
   , FnStmt(..)
   , FnTermStmt(..)
   , FnRegValue(..)
   , FnPhiVar(..)
   , FnReturnVar(..)
   , FnArchStmt
   , FoldFnValue(..)
   , FnArchConstraints
   ) where

import           Control.Monad.Identity
import qualified Data.ByteString.Char8 as BSC
import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import           Data.Proxy
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word
import           Numeric (showHex)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.CFG
   ( App(..)
   , ppApp
   , sexpr
   , prettyF
   , ArchFn
   , ArchAddrWidth
   , ArchReg
   , IsArchFn(..)
   , IsArchStmt(..)
   , MemRepr(..)
   , PrettyF(..)
   )
import           Data.Macaw.Memory
import           Data.Macaw.Types

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

data FnPhiVar (tp :: Type) =
  FnPhiVar { unFnPhiVar :: !FnAssignId
           , fnPhiVarType :: !(TypeRepr tp)
           }

instance TestEquality FnPhiVar where
  testEquality x y = orderingF_refl (compareF x y)

instance OrdF FnPhiVar where
  compareF x y =
    case compare (unFnPhiVar x) (unFnPhiVar y) of
      LT -> LTF
      GT -> GTF
      EQ ->
        case testEquality (fnPhiVarType x) (fnPhiVarType y) of
          Just Refl -> EQF
          Nothing -> error "mismatched types"

instance Pretty (FnPhiVar tp) where
  pretty = pretty . unFnPhiVar

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
data FunctionType (arch :: *) =
  FunctionType { fnArgTypes :: [Some TypeRepr]
               , fnReturnTypes :: [Some TypeRepr]
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
data FnAssignRhs (arch :: *) (f :: Type -> *) (tp :: Type) where
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
  -- | Allocate space on the stack that will be automatically
  -- freed when the function returns.
  FnAlloca :: !(f (BVType (ArchAddrWidth arch)))
           -> FnAssignRhs arch f (BVType (ArchAddrWidth arch))
  -- | Evaluate an architeture-specific function.
  FnEvalArchFn :: !(ArchFn arch f tp)
               -> FnAssignRhs arch f tp

instance FoldableFC (ArchFn arch) => FoldableFC (FnAssignRhs arch) where
  foldrFC _ s (FnSetUndefined {}) = s
  foldrFC f s (FnReadMem loc _)   = f loc s
  foldrFC f s (FnCondReadMem _ c a d)   = f c (f a (f d s))
  foldrFC f s (FnEvalApp a)       = foldrFC f s a
  foldrFC f s (FnAlloca sz)       = f sz s
  foldrFC f s (FnEvalArchFn fn)   = foldrFC f s fn

------------------------------------------------------------------------
-- FnAssignment/FnValue mutually recursive constructors


data FnAssignment arch tp
   = FnAssignment { fnAssignId :: !FnAssignId
                  , fnAssignRhs :: !(FnAssignRhs arch (FnValue arch) tp)
                  }


-- | A function value.
data FnValue (arch :: *) (tp :: Type) where
  -- | A value from the raw type that we do not yet support at the function level.
  FnValueUnsupported :: !String -> !(TypeRepr tp) -> FnValue arch tp
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
  FnPhiValue :: !(FnPhiVar tp) -> FnValue arch tp
  -- | A value returned by a function call (rax/rdx/xmm0)
  FnReturn :: !(FnReturnVar tp) -> FnValue arch tp
  -- | The pointer to a function.
  FnFunctionEntryValue :: !(FunctionType arch)
                          -- ^ Type of symbol
                       -> BSC.ByteString
                          -- ^ Symbol name to associate this this address.
                       -> FnValue arch (BVType (ArchAddrWidth arch))

  -- | A pointer to an internal block at the given address.
  FnBlockValue :: !(MemSegmentOff (ArchAddrWidth arch))
               -> FnValue arch (BVType (ArchAddrWidth arch))

  -- | Value is a argument passed via a register.
  FnRegArg :: !(ArchReg arch tp)
           -> !Int
           -> FnValue arch tp

  -- | A global address
  FnGlobalDataAddr :: !(MemSegmentOff (ArchAddrWidth arch))
                   -> FnValue arch (BVType (ArchAddrWidth arch))

------------------------------------------------------------------------
-- FoldFnValue

class FoldFnValue (v :: * -> *)  where
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
  pretty (FnValueUnsupported reason _)
                                  = text $ "unsupported (" ++ reason ++ ")"
  pretty (FnUndefined {})         = text "undef"
  pretty (FnConstantBool b)       = text $ if b then "true" else "false"
  pretty (FnConstantValue w i)
    | i >= 0 = parens (text ("0x" ++ showHex i "") <+> text ":" <+> text "bv" <+> text (show w))
    | otherwise = error ("FnConstantBool given negative value: " ++ show i)
  pretty (FnAssignedValue ass)    = pretty (fnAssignId ass)
  pretty (FnPhiValue phi)         = pretty (unFnPhiVar phi)
  pretty (FnReturn var)           = pretty var
  pretty (FnFunctionEntryValue _ n) = text "FunctionEntry" <> text (BSC.unpack n)
  pretty (FnBlockValue addr)   = text "BlockValue" <> parens (pretty addr)
  pretty (FnRegArg _ n)           = text "arg" <> int n
  pretty (FnGlobalDataAddr addr)  = text "data@" <> parens (pretty addr)

instance FnArchConstraints arch => Pretty (FnAssignRhs arch (FnValue arch) tp) where
  pretty rhs =
    case rhs of
      FnSetUndefined w -> text "undef ::" <+> brackets (text (show w))
      FnReadMem a tp -> sexpr "read" [ pretty a, pretty tp]
      FnCondReadMem _ c a d -> sexpr "cond_read" [ pretty c, pretty a, pretty d ]
      FnEvalApp a -> ppApp pretty a
      FnAlloca sz -> sexpr "alloca" [pretty sz]
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
      FnAlloca _ -> archWidthTypeRepr (Proxy :: Proxy arch)
      FnEvalArchFn f -> typeRepr f

instance FnArchConstraints arch => HasRepr (FnValue arch) TypeRepr where
  typeRepr v =
    case v of
      FnValueUnsupported _ tp -> tp
      FnUndefined tp -> tp
      FnConstantBool _ -> BoolTypeRepr
      FnConstantValue sz _ -> BVTypeRepr sz
      FnAssignedValue (FnAssignment _ rhs) -> typeRepr rhs
      FnPhiValue phi -> fnPhiVarType phi
      FnReturn ret   -> frReturnType ret
      FnFunctionEntryValue {} -> archWidthTypeRepr (Proxy :: Proxy arch)
      FnBlockValue{} -> archWidthTypeRepr (Proxy :: Proxy arch)
      FnRegArg r _ -> typeRepr r
      FnGlobalDataAddr _ -> archWidthTypeRepr (Proxy :: Proxy arch)

------------------------------------------------------------------------
-- FnRegValue

data FnRegValue arch tp
   = CalleeSaved !(ArchReg arch tp)
     -- ^ This is a callee saved register.
   | FnRegValue !(FnValue arch tp)
     -- ^ A value assigned to a register

instance (ShowF (ArchReg arch), MemWidth (ArchAddrWidth arch)) => Pretty (FnRegValue arch tp) where
  pretty (CalleeSaved r)     = text "calleeSaved" <> parens (text $ showF r)
  pretty (FnRegValue v)      = pretty v

------------------------------------------------------------------------
-- FnStmt

type family FnArchStmt (arch :: *) :: (Type -> *) -> *

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
         -> !(FunctionType arch)
            -- Arguments
         -> [Some (FnValue arch)]
            -- Return values
         -> ![Some FnReturnVar]
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
      FnCall f _ args rets ->
        let arg_docs = (\(Some v) -> pretty v) <$> args
            ppRet :: forall tp . FnReturnVar tp -> Doc
            ppRet = pretty
         in parens (commas (viewSome ppRet <$> rets))
            <+> text ":=" <+> text "call"
            <+> pretty f <> parens (commas arg_docs)
      FnArchStmt stmt -> ppArchStmt pretty stmt

instance FoldFnValue FnStmt where
  foldFnValue f s (FnWriteMem addr v)                 = s `f` addr `f` v
  foldFnValue f s (FnCondWriteMem c a v _)            = s `f` c `f` a `f` v
  foldFnValue _ s (FnComment {})                      = s
  foldFnValue f s (FnAssignStmt (FnAssignment _ rhs)) = foldlFC f s rhs
  foldFnValue f s (FnCall fn _ args _) = foldl (\s' (Some v) -> f s' v) (f s fn) args
  foldFnValue f s (FnArchStmt stmt) = foldlF' f s stmt

------------------------------------------------------------------------
-- FnTermStmt

-- | A block label
newtype FnBlockLabel = FnBlockLabel Text
  deriving (Eq)

instance Pretty FnBlockLabel where
  pretty (FnBlockLabel t) = text (Text.unpack t)

charInRange :: Char -> (Char, Char) -> Bool
charInRange c (l,h) = l <= c && c <= h

isBlockStart :: Char -> Bool
isBlockStart c
  =  c `charInRange` ('a', 'z')
  || c `charInRange` ('A', 'Z')

isBlockId :: Char -> Bool
isBlockId c
  =  c `charInRange` ('a', 'z')
  || c `charInRange` ('A', 'Z')
  || c `charInRange` ('0', '9')
  || c == '_'

fnBlockLabelUnpack :: FnBlockLabel -> String
fnBlockLabelUnpack (FnBlockLabel t) = Text.unpack t

instance IsString FnBlockLabel where
  fromString [] = error "Block label must be non-empty."
  fromString (c:r)
    | isBlockStart c, all isBlockId r =
        FnBlockLabel $ Text.pack $ c:r
    | otherwise =
        error $ "Invalid block label: " ++ (c:r)


-- | The terminal statement from a block.
data FnTermStmt arch
   = FnJump !FnBlockLabel
   | FnBranch !(FnValue arch BoolType) !FnBlockLabel !FnBlockLabel
   | FnLookupTable !(FnValue arch (BVType (ArchAddrWidth arch)))
                   !(V.Vector FnBlockLabel)
   | FnRet ![Some (FnValue arch)]
     -- ^ A return from the function with associated values
   | FnTailCall !(FnValue arch (BVType (ArchAddrWidth arch)))
                -- Type
                !(FunctionType arch)
                -- Arguments
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
      FnRet rets ->
        text "return" <+> parens (hsep (viewSome pretty <$> rets))
      FnTailCall f _ args ->
        let arg_docs = (\(Some v) -> pretty v) <$> args
         in text "tail_call" <+> pretty f <> parens (commas arg_docs)

instance FoldFnValue FnTermStmt where
  foldFnValue _ s (FnJump {})          = s
  foldFnValue f s (FnBranch c _ _)     = f s c
  foldFnValue f s (FnLookupTable idx _) = s `f` idx
  foldFnValue f s (FnRet rets) = foldl (\t (Some v) -> f t v) s rets
  foldFnValue f s (FnTailCall fn _ args) = foldl (\s' (Some v) -> f s' v) (f s fn) args

------------------------------------------------------------------------
-- FnBlock

-- | A block in the function
data FnBlock arch
   = FnBlock { fbLabel :: !FnBlockLabel
               -- ^ Label for identifying block.
             , fbPrevBlocks :: ![FnBlockLabel]
               -- ^ Labels of blocks that jump to this one.
             , fbPhiMap :: !(MapF (ArchReg arch) FnPhiVar)
               -- ^ Maps registers to phi variables
             , fbStmts :: ![FnStmt     arch]
               -- ^ List of non-terminal statements in block.
             , fbTerm  :: !(FnTermStmt arch)
               -- ^ Final terminal statement in block.
             , fbRegMap :: !(MapF (ArchReg arch) (FnRegValue arch))
               -- ^ Map from registers to values supplied by this
               -- block to successors.
               --
               -- Used to resolve Phi nodes, and could be omitted if
               -- block has no successors.
             }

instance (FnArchConstraints arch
         , PrettyF (ArchReg arch)
         , IsArchStmt (FnArchStmt arch)
         )
      => Pretty (FnBlock arch) where
  pretty b =
    pretty (fbLabel b) <$$>
    indent 2 (ppPhis
              <$$> vcat (pretty <$> fbStmts b)
              <$$> pretty (fbTerm b))
    where
      go :: MapF.Pair (ArchReg arch) FnPhiVar -> Doc
      go (MapF.Pair r v) =
         pretty v <+> text ":= phi "
         <+> hsep (punctuate comma $ map (ppBinding r) (fbPrevBlocks b))

      ppPhis = vcat (go <$> MapF.toAscList (fbPhiMap b))

      ppBinding :: ArchReg arch tp -> FnBlockLabel -> Doc
      ppBinding r a = parens (pretty a <> comma <+> prettyF r)

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
              , fnEntryBlock :: !(FnBlock arch)
              , fnRestBlocks :: ![FnBlock arch]
                -- ^ A list of all function blocks here.
              }

-- | Return blocks in function with entry block first.
fnBlocks :: Function arch -> [FnBlock arch]
fnBlocks f = fnEntryBlock f : fnRestBlocks f

instance (FnArchConstraints arch
         , PrettyF (ArchReg arch)
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
        rtp = parens (hsep (viewSome pretty <$> fnReturnTypes ftp))
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
