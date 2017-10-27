{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reopt.CFG.FnRep
   ( FnAssignId(..)
   , FnAssignment(..)
   , FnAssignRhs(..)
   , FnValue(..)
   , Function(..)
   , FunctionType(..)
   , FnBlock(..)
   , FnStmt(..)
   , FnTermStmt(..)
   , FnRegValue(..)
   , FnPhiVar(..)
   , FnReturnVar(..)
   , FoldFnValue(..)
   , PhiBinding(..)
   , ftMaximumFunctionType
   , ftMinimumFunctionType
   , ftArgRegs
   , ftIntRetRegs
   , ftFloatRetRegs
   ) where

import           Control.Monad.Identity
import           Data.Foldable
import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Parameterized.TraversableFC
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import qualified Data.Vector as V
import           GHC.TypeLits
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Data.Macaw.CFG
   ( App(..)
   , ppApp
   , ppLit
   , sexpr
   , prettyF
   , ArchFn
   , ArchAddrWidth
   , RegAddrWidth
   , ArchReg
   , IsArchFn(..)
   , IsArchStmt(..)
   )
import           Data.Macaw.Memory
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes (X86_64, X86Stmt(..))
import           Data.Macaw.X86.Monad (XMMType)
import           Data.Macaw.X86.X86Reg
  ( X86Reg
  , x86ArgumentRegs
  , x86ResultRegs
  , x86FloatArgumentRegs
  , x86FloatResultRegs
  )

import           Reopt.CFG.BlockLabel

commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')

------------------------------------------------------------------------
-- FnAssignId

newtype FnAssignId = FnAssignId Word64
                   deriving (Eq, Ord)

instance Show FnAssignId where
  show (FnAssignId w) = show w

ppFnAssignId :: FnAssignId -> Doc
ppFnAssignId (FnAssignId w) = text ("r" ++ show w)

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
  pretty = ppFnAssignId . unFnPhiVar

------------------------------------------------------------------------
-- FnReturnVar

data FnReturnVar tp = FnReturnVar { frAssignId :: !FnAssignId
                                  , frReturnType :: !(TypeRepr tp)
                                  }

instance Pretty (FnReturnVar tp) where
  pretty = ppFnAssignId . frAssignId

------------------------------------------------------------------------
-- FunctionType

-- | This describes the expected arguments and return types of the function.
--
-- This is X86_64 specific.
data FunctionType =
  FunctionType { fnNIntArgs   :: !Int
               , fnNFloatArgs :: !Int
               , fnNIntRets   :: !Int
               , fnNFloatRets :: !Int
               }
  deriving (Ord, Eq, Show)

instance Pretty FunctionType where
  pretty f = parens (int (fnNIntArgs f) <> comma <+> int (fnNFloatArgs f))
             <+> text "->"
             <+> parens (int (fnNIntRets f) <> comma <+> int (fnNFloatRets f))

-- Convenience functions
ftMaximumFunctionType :: FunctionType
ftMaximumFunctionType = FunctionType (length x86ArgumentRegs)
                                     (length x86FloatArgumentRegs)
                                     (length x86ResultRegs)
                                     (length x86FloatResultRegs)

ftMinimumFunctionType :: FunctionType
ftMinimumFunctionType = FunctionType 0 0 0 0

-- | Return the registers used to pass arguments.
ftArgRegs :: FunctionType -> [Some X86Reg]
ftArgRegs ft = fmap Some (ftIntArgRegs ft) ++ fmap Some (ftFloatArgRegs ft)

ftIntArgRegs :: FunctionType -> [X86Reg (BVType 64)]
ftIntArgRegs ft = take (fnNIntArgs ft) x86ArgumentRegs

ftFloatArgRegs :: FunctionType -> [X86Reg (BVType 128)]
ftFloatArgRegs ft = take (fnNFloatArgs ft) x86FloatArgumentRegs

ftIntRetRegs :: FunctionType -> [X86Reg (BVType 64)]
ftIntRetRegs ft = take (fnNIntRets ft) x86ResultRegs

ftFloatRetRegs :: FunctionType -> [X86Reg (BVType 128)]
ftFloatRetRegs ft = take (fnNFloatRets ft) x86FloatResultRegs

------------------------------------------------------------------------
-- FnAssignment/FnValue/FnAssignRhs mutually recursive constructors

-- | The right-hand side of a function assingment statement.
data FnAssignRhs (arch :: *) (tp :: Type) where
  -- An expression with an undefined value.
  FnSetUndefined :: !(TypeRepr tp) -- Width of undefined value.
                 -> FnAssignRhs arch tp
  FnReadMem :: !(FnValue arch (BVType (ArchAddrWidth arch)))
            -> !(TypeRepr tp)
            -> FnAssignRhs arch tp
  FnEvalApp :: !(App (FnValue arch) tp)
            -> FnAssignRhs arch tp
  FnAlloca :: !(FnValue arch (BVType (ArchAddrWidth arch)))
           -> FnAssignRhs arch (BVType (ArchAddrWidth arch))
  FnEvalArchFn :: !(ArchFn arch (FnValue arch) tp) -> FnAssignRhs arch tp

data FnAssignment arch tp
   = FnAssignment { fnAssignId :: !FnAssignId
                  , fnAssignRhs :: !(FnAssignRhs arch tp)
                  }


-- | A function value.
data FnValue (arch :: *) (tp :: Type)
   =  FnValueUnsupported !String !(TypeRepr tp)
      -- | A value that is actually undefined, like a non-argument register at
      -- the start of a function.
   | FnUndefined !(TypeRepr tp)
     -- | A particular Bool value
   | (tp ~ BoolType) => FnConstantBool !Bool
     -- | A particular integer value
   | forall n . (tp ~ BVType n, 1 <= n) => FnConstantValue !(NatRepr n) !Integer
     -- | Value from an assignment statement.
   | FnAssignedValue !(FnAssignment arch tp)
     -- | Value from a phi node
   | FnPhiValue !(FnPhiVar tp)
     -- | A value returned by a function call (rax/rdx/xmm0)
   | FnReturn !(FnReturnVar tp)
     -- | The pointer to a function.
   | (arch ~ X86_64, tp ~ BVType (ArchAddrWidth arch))
     => FnFunctionEntryValue !FunctionType !(MemSegmentOff (ArchAddrWidth arch))
     -- | A pointer to an internal block at the given address.
   | (tp ~ BVType (ArchAddrWidth arch)) => FnBlockValue !(MemSegmentOff (ArchAddrWidth arch))
      -- | Value is a argument passed via a register.
   | FnRegArg !(ArchReg arch tp) !Int
     -- | A global address
   | (tp ~ BVType (ArchAddrWidth arch))
     => FnGlobalDataAddr !(MemSegmentOff (ArchAddrWidth arch))

------------------------------------------------------------------------
-- FoldFnValue

class FoldFnValue a where
  foldFnValue :: (forall u . s -> FnValue X86_64 u -> s) -> s -> a -> s

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
  pretty (FnConstantValue sz n)   = ppLit sz n
  pretty (FnAssignedValue ass)    = ppFnAssignId (fnAssignId ass)
  pretty (FnPhiValue phi)         = ppFnAssignId (unFnPhiVar phi)
  pretty (FnReturn var)           = pretty var
  pretty (FnFunctionEntryValue _ n) = text "FunctionEntry"
                                    <> parens (pretty $ show n)
  pretty (FnBlockValue addr)   = text "BlockValue" <> parens (pretty addr)
  pretty (FnRegArg _ n)           = text "arg" <> int n
  pretty (FnGlobalDataAddr addr)  = text "data@" <> parens (pretty addr)

ppFnAssignRhs :: FnArchConstraints arch
              => (forall u . FnValue arch u -> Doc)
              -> FnAssignRhs arch tp
              -> Doc
ppFnAssignRhs _  (FnSetUndefined w) = text "undef ::" <+> brackets (text (show w))
ppFnAssignRhs _  (FnReadMem loc _)  = text "*" <> pretty loc
ppFnAssignRhs pp (FnEvalApp a) = ppApp pp a
ppFnAssignRhs pp (FnAlloca sz) = sexpr "alloca" [pp sz]
ppFnAssignRhs pp (FnEvalArchFn f) = runIdentity (ppArchFn (pure . pp) f)

instance FnArchConstraints arch => Pretty (FnAssignRhs arch tp) where
  pretty = ppFnAssignRhs pretty

instance FnArchConstraints arch => Pretty (FnAssignment arch tp) where
  pretty (FnAssignment lhs rhs) = ppFnAssignId lhs <+> text ":=" <+> pretty rhs


instance FnArchConstraints arch => Show (FnAssignment arch tp) where
  show = show . pretty

instance FnArchConstraints arch => ShowF (FnAssignment arch)

archWidthTypeRepr :: forall p arch
                  .  MemWidth (ArchAddrWidth arch)
                  => p arch
                  -> TypeRepr (BVType (ArchAddrWidth arch))
archWidthTypeRepr _ = BVTypeRepr (addrWidthNatRepr (addrWidthRepr (Proxy :: Proxy (ArchAddrWidth arch))))

instance FnArchConstraints arch => HasRepr (FnAssignRhs arch) TypeRepr where
  typeRepr rhs =
    case rhs of
      FnSetUndefined tp -> tp
      FnReadMem _ tp -> tp
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
      FnFunctionEntryValue {} -> knownType
      FnBlockValue{} -> archWidthTypeRepr (Proxy :: Proxy arch)
      FnRegArg r _ -> typeRepr r
      FnGlobalDataAddr _ -> archWidthTypeRepr (Proxy :: Proxy arch)

instance FoldFnValue (FnAssignRhs X86_64 tp) where
  foldFnValue _ s (FnSetUndefined {}) = s
  foldFnValue f s (FnReadMem loc _)   = f s loc
  foldFnValue f s (FnEvalApp a)       = foldlFC f s a
  foldFnValue f s (FnAlloca sz)       = s `f` sz
  foldFnValue f s (FnEvalArchFn fn) = foldlFC f s fn

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
-- PhiBinding

-- | A Phi binding maps a phi variable to a list that contains a label
-- for each predecessor block, and the register to rad from at the end
-- of that blcok.
data PhiBinding r tp
   = PhiBinding (FnPhiVar tp) [(BlockLabel (RegAddrWidth r), r tp)]

------------------------------------------------------------------------
-- FnStmt

data FnStmt
  = forall tp . FnWriteMem !(FnValue X86_64 (BVType 64)) !(FnValue X86_64 tp)
    -- | A comment
  | FnComment !Text
    -- | An assignment statement
  | forall tp . FnAssignStmt !(FnAssignment X86_64 tp)
  | FnArchStmt (X86Stmt (FnValue X86_64))

instance Pretty FnStmt where
  pretty s =
    case s of
      FnWriteMem addr val -> text "*" <> parens (pretty addr) <+> text "=" <+> pretty val
      FnComment msg -> text "#" <+> text (Text.unpack msg)
      FnAssignStmt assign -> pretty assign
      FnArchStmt stmt -> ppArchStmt pretty stmt

instance FoldFnValue FnStmt where
  foldFnValue f s (FnWriteMem addr v)                 = s `f` addr `f` v
  foldFnValue _ s (FnComment {})                      = s
  foldFnValue f s (FnAssignStmt (FnAssignment _ rhs)) = foldFnValue f s rhs
  foldFnValue f s (FnArchStmt stmt) = foldlF' f s stmt

------------------------------------------------------------------------
-- FnTermStmt

data FnTermStmt
   = FnJump !(BlockLabel 64)
   | FnRet ![FnValue X86_64 (BVType 64)] ![FnValue X86_64 XMMType]
   | FnBranch !(FnValue X86_64 BoolType) !(BlockLabel 64) !(BlockLabel 64)
     -- ^ A branch to a block within the function, along with the return vars.
   | FnCall !(FnValue X86_64 (BVType 64))
            !FunctionType
            -- Arguments
            [Some (FnValue X86_64)]
            !([FnReturnVar (BVType 64)], [FnReturnVar XMMType])
            !(Maybe (BlockLabel 64))
     -- ^ A call statement to the given location with the arguments listed that
     -- returns to the label.
   | FnSystemCall !(FnValue  X86_64 (BVType 64))
                  ![(FnValue X86_64 (BVType 64))]
                  ![ Some FnReturnVar ] (BlockLabel 64)
   | FnLookupTable !(FnValue X86_64 (BVType 64)) !(V.Vector (MemSegmentOff 64))
   | FnTermStmtUndefined

instance Pretty FnTermStmt where
  pretty s =
    case s of
      FnBranch c x y -> text "branch" <+> pretty c <+> pretty x <+> pretty y
      FnJump lbl -> text "jump" <+> pretty lbl
      FnRet grets frets -> text "return" <+> parens (commas $ (pretty <$> grets) ++ (pretty <$> frets))
      FnCall f _ args (grets, frets) lbl ->
        let arg_docs = (\(Some v) -> pretty v) <$> args
            ret_docs = (pretty <$> grets) ++ (pretty <$> frets)
         in parens (commas ret_docs)
            <+> text ":=" <+> text "call"
            <+> pretty f <> parens (commas arg_docs) <+> pretty lbl
      FnSystemCall call_no args rets lbl ->
        let arg_docs = (pretty <$> args)
            ret_docs = viewSome pretty <$> rets
         in parens (commas ret_docs)
            <+> text ":=" <+> text "syscall"
            <+> pretty call_no <> parens (commas arg_docs) <+> pretty lbl
      FnLookupTable idx vec -> text "lookup" <+> pretty idx <+> text "in"
                               <+> parens (commas $ map (pretty . relativeSegmentAddr)
                                                        (V.toList vec))
      FnTermStmtUndefined -> text "undefined term"

instance FoldFnValue FnTermStmt where
  foldFnValue _ s (FnJump {})          = s
  foldFnValue f s (FnBranch c _ _)     = f s c
  foldFnValue f s (FnRet grets frets) = foldl f (foldl f s grets) frets
  foldFnValue f s (FnCall fn _ args _ _) = foldl (\s' (Some v) -> f s' v) (f s fn) args
  foldFnValue f s (FnSystemCall call_no args _rets _lbl) =
    foldl f (f s call_no) args
  foldFnValue f s (FnLookupTable idx _) = s `f` idx
  foldFnValue _ s (FnTermStmtUndefined {}) = s

------------------------------------------------------------------------
-- FnBlock

-- | A block in the function
data FnBlock
   = FnBlock { fbLabel :: !(BlockLabel 64)
               -- | List of phi bindings.
               --
               -- Only initial blocks -- not subblocks should have phi bindings.
             , fbPhiNodes  :: ![Some (PhiBinding X86Reg)]
             , fbStmts :: ![FnStmt]
             , fbTerm  :: !(FnTermStmt)
             , fbRegMap :: !(MapF X86Reg (FnRegValue X86_64))
             }

instance Pretty FnBlock where
  pretty b =
    pretty (fbLabel b) <$$>
    indent 2 (ppPhis
              <$$> vcat (pretty <$> fbStmts b)
              <$$> pretty (fbTerm b))
    where
      ppPhis = vcat (go <$> fbPhiNodes b)
      go :: Some (PhiBinding X86Reg) -> Doc
      go (Some (PhiBinding aid vs)) =
         pretty aid <+> text ":= phi " <+> hsep (punctuate comma $ map goLbl vs)
      goLbl :: (BlockLabel 64, X86Reg tp) -> Doc
      goLbl (lbl, node) = parens (pretty lbl <> comma <+> prettyF node)

instance FoldFnValue FnBlock where
  foldFnValue f s0 b = foldFnValue f (foldl (foldFnValue f) s0 (fbStmts b)) (fbTerm b)

------------------------------------------------------------------------
-- Function definitions

data Function = Function { fnAddr :: !(MemSegmentOff 64)
                           -- ^ The address for this function
                         , fnType :: !FunctionType
                           -- ^ Type of this  function
                         , fnBlocks :: [FnBlock]
                           -- ^ A list of all function blocks here.
                         }

instance Pretty Function where
  pretty fn =
    text "function " <+> pretty (show (fnAddr fn))
    <$$> lbrace
    <$$> (nest 4 $ vcat (pretty <$> fnBlocks fn))
    <$$> rbrace

instance FoldFnValue Function where
  foldFnValue f s0 fn = foldl' (foldFnValue f) s0 (fnBlocks fn)
