{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

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
   , FnPhiNodeInfo(..)
   , FnReturnVar(..)
   , FoldFnValue(..)
   , fnAssignRHSType
   , fnValueType
   , ftMaximumFunctionType
   , ftMinimumFunctionType
   , ftIntArgRegs
   , ftFloatArgRegs
   , ftIntRetRegs
   , ftFloatRetRegs
   ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Data.Vector as V

import           Data.Parameterized.Classes
import           Data.Parameterized.Map (MapF)
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF

import           Data.Macaw.CFG
   ( App(..)
   , BlockLabel
   , ppApp
   , ppLit
   , sexpr
   , appType
   , foldApp
   )
import           Data.Macaw.Memory (MemWord, SegmentedAddr)
import           Data.Macaw.Types

import           Reopt.Machine.X86State
  ( X86Reg
  , x86ArgumentRegs
  , x86ResultRegs
  , x86FloatArgumentRegs
  , x86FloatResultRegs
  )

commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')

newtype FnAssignId = FnAssignId Word64
                   deriving (Eq, Ord)

instance Show FnAssignId where
  show (FnAssignId w) = show w

ppFnAssignId :: FnAssignId -> Doc
ppFnAssignId (FnAssignId w) = text ("r" ++ show w)

data FnAssignment tp
   = FnAssignment { fnAssignId :: !FnAssignId
                  , fnAssignRhs :: !(FnAssignRhs tp)
                  }

instance Pretty (FnAssignment tp) where
  pretty (FnAssignment lhs rhs) = ppFnAssignId lhs <+> text ":=" <+> pretty rhs

-- | This describes the expected arguments and return types of the function.
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

ftIntArgRegs :: FunctionType -> [X86Reg (BVType 64)]
ftIntArgRegs ft = take (fnNIntArgs ft) x86ArgumentRegs

ftFloatArgRegs :: FunctionType -> [X86Reg (BVType 128)]
ftFloatArgRegs ft = take (fnNFloatArgs ft) x86FloatArgumentRegs

ftIntRetRegs :: FunctionType -> [X86Reg (BVType 64)]
ftIntRetRegs ft = take (fnNIntRets ft) x86ResultRegs

ftFloatRetRegs :: FunctionType -> [X86Reg (BVType 128)]
ftFloatRetRegs ft = take (fnNFloatRets ft) x86FloatResultRegs

-- FIXME: this is in the same namespace as assignments, maybe it shouldn't be?

data FnPhiVar (tp :: Type) =
  FnPhiVar { unFnPhiVar :: !FnAssignId
           , fnPhiVarType :: !(TypeRepr tp) }

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

-- | The right-hand side of a function assingment statement.
data FnAssignRhs (tp :: Type) where
  -- An expression with an undefined value.
  FnSetUndefined :: !(NatRepr n) -- Width of undefined value.
                 -> FnAssignRhs (BVType n)
  FnReadMem :: !(FnValue (BVType 64))
            -> !(TypeRepr tp)
            -> FnAssignRhs tp
  FnEvalApp :: !(App FnValue tp)
            -> FnAssignRhs tp
  FnAlloca :: !(FnValue (BVType 64))
           -> FnAssignRhs (BVType 64)

ppFnAssignRhs :: (forall u . FnValue u -> Doc)
                 -> FnAssignRhs tp
                 -> Doc
ppFnAssignRhs _  (FnSetUndefined w) = text "undef ::" <+> brackets (text (show w))
ppFnAssignRhs _  (FnReadMem loc _)  = text "*" <> pretty loc
ppFnAssignRhs pp (FnEvalApp a) = ppApp pp a
ppFnAssignRhs _pp (FnAlloca sz) = sexpr "alloca" [pretty sz]

instance Pretty (FnAssignRhs tp) where
  pretty = ppFnAssignRhs pretty

fnAssignRHSType :: FnAssignRhs tp -> TypeRepr tp
fnAssignRHSType rhs =
  case rhs of
    FnSetUndefined sz -> BVTypeRepr sz
    FnReadMem _ tp -> tp
    FnEvalApp a    -> appType a
    FnAlloca _ -> knownType


instance FoldFnValue (FnAssignRhs tp) where
  foldFnValue _f (FnSetUndefined {}) = mempty
  foldFnValue f (FnReadMem loc _)   = f loc
  foldFnValue f (FnEvalApp a)       = foldApp f a
  foldFnValue f (FnAlloca sz)       = f sz

-- tp <- {BVType 64, BVType 128}
data FnReturnVar tp = FnReturnVar { frAssignId :: !FnAssignId
                                  , frReturnType :: !(TypeRepr tp) }

instance Pretty (FnReturnVar tp) where
  pretty = ppFnAssignId . frAssignId

-- | A function value.
data FnValue (tp :: Type)
   =  FnValueUnsupported !String !(TypeRepr tp)
      -- | A value that is actually undefined, like a non-argument register at
      -- the start of a function.
   | FnUndefined !(TypeRepr tp)
   | forall n . (tp ~ BVType n) => FnConstantValue !(NatRepr n) !Integer
     -- | Value from an assignment statement.
   | FnAssignedValue !(FnAssignment tp)
     -- | Value from a phi node
   | FnPhiValue !(FnPhiVar tp)
     -- | A value returned by a function call (rax/rdx/xmm0)
   | FnReturn !(FnReturnVar tp)
     -- | The entry pointer to a function.
   | (tp ~ BVType 64) => FnFunctionEntryValue !FunctionType !(SegmentedAddr 64)
     -- | A pointer to an internal block at the given address.
   | (tp ~ BVType 64) => FnBlockValue !(SegmentedAddr 64)
     -- | Value is an interget argument passed via a register.
   | (tp ~ BVType 64) => FnIntArg !Int
     -- | Value is a function argument passed via a floating point XMM
     -- register.
   | (tp ~ BVType 128) => FnFloatArg !Int
     -- | A global address
   | (tp ~ BVType 64) => FnGlobalDataAddr !(SegmentedAddr 64)

instance Pretty (FnValue tp) where
  pretty (FnValueUnsupported reason _)
                                  = text $ "unsupported (" ++ reason ++ ")"
  pretty (FnUndefined {})         = text "undef"
  pretty (FnConstantValue sz n)   = ppLit sz n
  pretty (FnAssignedValue ass)    = ppFnAssignId (fnAssignId ass)
  pretty (FnPhiValue phi)         = ppFnAssignId (unFnPhiVar phi)
  pretty (FnReturn var)           = pretty var
  pretty (FnFunctionEntryValue _ n) = text "FunctionEntry"
                                    <> parens (pretty $ show n)
  pretty (FnBlockValue n)         = text "BlockValue"
                                    <> parens (pretty $ show n)
  pretty (FnIntArg n)             = text "arg" <> int n
  pretty (FnFloatArg n)           = text "fparg" <> int n
  pretty (FnGlobalDataAddr addr)  = text "data@"
                                    <> parens (pretty $ show addr)
class FoldFnValue a where
  foldFnValue :: Monoid m => (forall u . FnValue u -> m) -> a -> m

fnValueType :: FnValue tp -> TypeRepr tp
fnValueType v =
  case v of
    FnValueUnsupported _ tp -> tp
    FnUndefined tp -> tp
    FnConstantValue sz _ -> BVTypeRepr sz
    FnAssignedValue (FnAssignment _ rhs) -> fnAssignRHSType rhs
    FnPhiValue phi -> fnPhiVarType phi
    FnReturn ret   -> frReturnType ret
    FnFunctionEntryValue {} -> knownType
    FnBlockValue _ -> knownType
    FnIntArg _ -> knownType
    FnFloatArg _ -> knownType
    FnGlobalDataAddr _ -> knownType

------------------------------------------------------------------------
-- Function definitions

data Function = Function { fnAddr :: !(SegmentedAddr 64)
                           -- ^ In memory address of function
                         , fnSize :: !(MemWord 64)
                           -- ^ Number of bytes that function takes up.
                         , fnType :: !FunctionType
                         , fnBlocks :: [FnBlock]
                         }

instance Pretty Function where
  pretty fn =
    text "function " <+> pretty (show (fnAddr fn))
    <$$>
    lbrace
    <$$>
    (nest 4 $ vcat (pretty <$> fnBlocks fn))
    <$$>
    rbrace

instance FoldFnValue Function where
  foldFnValue f fn = mconcat (map (foldFnValue f) (fnBlocks fn))

data FnRegValue tp
   = CalleeSaved !(X86Reg tp)
     -- ^ This is a callee saved register.
   | FnRegValue !(FnValue tp)
     -- ^ A value assigned to a register

instance Pretty (FnRegValue tp) where
  pretty (CalleeSaved r)     = text "calleeSaved" <> parens (text $ show r)
  pretty (FnRegValue v)      = pretty v

newtype FnPhiNodeInfo tp
      = FnPhiNodeInfo { unFnPhiNodeInfo :: [(BlockLabel 64, FnValue tp)] }

instance FoldFnValue (FnPhiNodeInfo tp) where
  foldFnValue f (FnPhiNodeInfo vs) = mconcat (map (f . snd) vs)

data FnBlock
   = FnBlock { fbLabel :: !(BlockLabel 64)
               -- Maps predecessor label onto the reg value at that
               -- block
             , fbPhiNodes  :: !(MapF FnPhiVar FnPhiNodeInfo)
             , fbStmts :: ![FnStmt]
             , fbTerm  :: !(FnTermStmt)
             }

instance Pretty FnBlock where
  pretty b =
    pretty (fbLabel b) <$$>
    indent 2 (ppPhis
              <$$> vcat (pretty <$> fbStmts b)
              <$$> pretty (fbTerm b))
    where
      ppPhis = vcat $ MapF.foldrWithKey go mempty (fbPhiNodes b)
      go :: FnPhiVar tp -> FnPhiNodeInfo tp -> [Doc] -> [Doc]
      go aid vs d =
        (pretty aid <+> text ":= phi " <+> hsep (punctuate comma $ map goLbl (unFnPhiNodeInfo vs))) : d
      goLbl :: (BlockLabel 64, FnValue tp) -> Doc
      goLbl (lbl, node) = parens (pretty lbl <> comma <+> pretty node)

instance FoldFnValue FnBlock where
  foldFnValue f b =
    mconcat (toListF (foldFnValue f) (fbPhiNodes b)
             ++ map (foldFnValue f) (fbStmts b)
             ++ [ foldFnValue f (fbTerm b) ])

data FnStmt
  = forall tp . FnWriteMem !(FnValue (BVType 64)) !(FnValue tp)
    -- | A comment
  | FnComment !Text
    -- | An assignment statement
  | forall tp . FnAssignStmt !(FnAssignment tp)
    -- FIXME: can we share these with the front-end?
  | FnMemCopy !Integer
             -- /\ Number of bytes to copy at a time (1,2,4,8)
             !(FnValue (BVType 64))
              -- /\ Number of values to move.
             !(FnValue (BVType 64))
              -- /\ Start of source buffer.
             !(FnValue (BVType 64))
              -- /\ Start of destination buffer.
             !(FnValue (BVType 1))
              -- /\ Flag indicates whether direction of move:
              -- True means we should decrement buffer pointers after each copy.
              -- False means we should increment the buffer pointers after each copy.
  | forall n. FnMemSet (FnValue (BVType 64))
                        -- /\ Number of values to assign
                       (FnValue (BVType n))
                        -- /\ Value to assign
                       (FnValue (BVType 64))
                        -- /\ Address to start assigning from.
                       (FnValue (BVType 1))
                        -- /\ Direction flag

instance Pretty FnStmt where
  pretty s =
    case s of
      FnWriteMem addr val -> text "*" <> parens (pretty addr) <+> text "=" <+> pretty val
      FnComment msg -> text "#" <+> text (Text.unpack msg)
      FnAssignStmt assign -> pretty assign
      FnMemCopy  sz cnt src dest rev ->
        text "memcopy" <+> parens (hcat $ punctuate comma args)
        where args = [pretty sz, pretty cnt, pretty src, pretty dest, pretty rev]
      FnMemSet cnt val dest df ->
        text "memset" <+> parens (hcat $ punctuate comma args)
        where args = [pretty cnt, pretty val, pretty dest, pretty df]

instance FoldFnValue FnStmt where
  foldFnValue f (FnWriteMem addr v) = f addr `mappend` f v
  foldFnValue _f (FnComment {})     = mempty
  foldFnValue f (FnAssignStmt (FnAssignment _ rhs)) = foldFnValue f rhs
  foldFnValue f (FnMemCopy _sz cnt src dest rev) =
    f cnt `mappend` f src `mappend` f dest `mappend` f rev
  foldFnValue f (FnMemSet cnt v ptr df) =
    f cnt `mappend` f v `mappend` f ptr `mappend` f df

data FnTermStmt
   = FnJump !(BlockLabel 64)
   | FnRet !([FnValue (BVType 64)], [FnValue XMMType])
   | FnBranch !(FnValue BoolType) !(BlockLabel 64) !(BlockLabel 64)
     -- ^ A branch to a block within the function, along with the return vars.
     -- FIXME: need to add extra stack arg.
   | FnCall !(FnValue (BVType 64))
            !([FnValue (BVType 64)], [FnValue XMMType])
            !([FnReturnVar (BVType 64)], [FnReturnVar XMMType])
            !(Maybe (BlockLabel 64))
     -- ^ A call statement to the given location with the arguments listed that
     -- returns to the label.

     -- FIXME: specialized to BSD's (broken) calling convention
   | FnSystemCall !(MemWord 64) !String !String [(FnValue (BVType 64))] ![ Some FnReturnVar ]
        (BlockLabel 64)
   | FnLookupTable !(FnValue (BVType 64)) !(V.Vector (SegmentedAddr 64))
   | FnTermStmtUndefined

instance Pretty FnTermStmt where
  pretty s =
    case s of
      FnBranch c x y -> text "branch" <+> pretty c <+> pretty x <+> pretty y
      FnJump lbl -> text "jump" <+> pretty lbl
      FnRet (grets, frets) -> text "return" <+> parens (commas $ (pretty <$> grets) ++ (pretty <$> frets))
      FnCall f (gargs, fargs) (grets, frets) lbl ->
        let arg_docs = (pretty <$> gargs) ++ (pretty <$> fargs)
            ret_docs = (pretty <$> grets) ++ (pretty <$> frets)
         in parens (commas ret_docs)
            <+> text ":=" <+> text "call"
            <+> pretty f <> parens (commas arg_docs) <+> pretty lbl
      FnSystemCall _call_no _pname name args rets lbl ->
        let arg_docs = (pretty <$> args)
            ret_docs = viewSome pretty <$> rets
         in parens (commas ret_docs)
            <+> text ":=" <+> text "syscall"
            <+> text name <> parens (commas arg_docs) <+> pretty lbl
      FnLookupTable idx vec -> text "lookup" <+> pretty idx <+> text "in"
                               <+> parens (commas $ map (text . show) (V.toList vec))
      FnTermStmtUndefined -> text "undefined term"

instance FoldFnValue FnTermStmt where
  foldFnValue _f (FnJump {})           = mempty
  foldFnValue f (FnBranch c _ _)       = f c
  foldFnValue f (FnRet (grets, frets)) = mconcat (map f grets ++ map f frets)
  foldFnValue f (FnCall fn (gargs, fargs) _ _) =
    f fn `mappend` mconcat (map f gargs ++ map f fargs)
  foldFnValue f (FnSystemCall _call_no _pname _name args _rets _lbl) =
    mconcat (map f args)
  foldFnValue f (FnLookupTable idx _) = f idx
  foldFnValue _f (FnTermStmtUndefined {}) = mempty
