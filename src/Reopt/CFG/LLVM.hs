{-|
Copyright        : (c) Galois, Inc 2015-2018

This provides functionality for converting the
architecture-independent components of @FnReop@ functions into LLVM.

It exports a fairly large set of capabilities so that all
architecture-specific functionality can be implemented on top of this
layer.

-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Reopt.CFG.LLVM
  ( AddrSymMap
  , moduleForFunctions
  , llvmFunctionName
    -- * Internals for implement architecture specific functions
  , LLVMArchSpecificOps(..)
  , LLVMArchConstraints
  , HasValue
  , functionTypeToLLVM
  , Intrinsic
  , intrinsic
  , FunLLVMContext(funSymbolCallback, funAddrTypeMap)
  , FunctionSymbolCallback
  , BBLLVM
  , BBLLVMState(archFns, bbBlock, funContext)
  , typeToLLVMType
  , setAssignIdValue
  , padUndef
  , arithop
  , convop
  , bitcast
  , band
  , icmpop
  , llvmAsPtr
  , extractValue
  , insertValue
  , call
  , call_
  , mkLLVMValue
  , ret
  , effect
  , AsmAttrs
  , noSideEffect
  , sideEffect
  , callAsm
  , callAsm_
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Int
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
import           Data.Parameterized.TraversableF
import           Data.Proxy
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified GHC.Err.Located as Loc
import           GHC.TypeLits
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as L (ppType)
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Data.Macaw.CFG
import           Data.Macaw.CFG.BlockLabel
import           Data.Macaw.Types

import           Reopt.CFG.FnRep

------------------------------------------------------------------------
-- HasValue

class HasValue v where
  valueOf :: v -> L.Typed L.Value

instance HasValue (L.Typed L.Value) where
  valueOf = id

------------------------------------------------------------------------
-- Intrinsic

-- | An LLVM intrinsic
data Intrinsic = Intrinsic { intrinsicName :: !L.Symbol
                           , intrinsicRes  :: L.Type
                           , intrinsicArgs  :: [L.Type]
                           , intrinsicAttrs :: [L.FunAttr]
                           }

instance HasValue Intrinsic where
  valueOf i = L.Typed tp (L.ValSymbol (intrinsicName i))
    where tp = L.ptrT $ L.FunTy (intrinsicRes i) (intrinsicArgs i) False

-- | Define an intrinsic that has no special attributes
intrinsic :: String -> L.Type -> [L.Type] -> Intrinsic
intrinsic name res args = intrinsic' name res args []

-- | Define an intrinsic that has no special attributes
intrinsic' :: String -> L.Type -> [L.Type] -> [L.FunAttr] -> Intrinsic
intrinsic' name res args attrs = Intrinsic { intrinsicName = L.Symbol name
                                           , intrinsicRes  = res
                                           , intrinsicArgs = args
                                           , intrinsicAttrs = attrs
                                           }

--------------------------------------------------------------------------------
-- LLVM intrinsics
--------------------------------------------------------------------------------

-- | LLVM arithmetic with overflow intrinsic.
overflowOp :: String -> L.Type -> Intrinsic
overflowOp bop in_typ =
  intrinsic ("llvm." ++ bop ++ ".with.overflow." ++ show (L.ppType in_typ))
            (L.Struct [in_typ, L.iT 1])
            [in_typ, in_typ]

-- | @llvm.masked.load.*@ intrinsic
llvmMaskedLoad :: Int32 -- ^ Number of vector elements
                -> String -- ^ Type name (e.g. i32)
                -> L.Type -- ^ Element type (should match string)
                -> Intrinsic
llvmMaskedLoad n tp tpv = do
 let vstr = "v" ++ show n ++ tp
     mnem = "llvm.masked.load." ++ vstr ++ ".p0" ++ vstr
     args = [ L.PtrTo (L.Vector n tpv), L.iT 32, L.Vector n (L.iT 1), L.Vector n tpv ]
  in intrinsic mnem (L.Vector n tpv) args

llvmIntrinsics :: [Intrinsic]
llvmIntrinsics = [ overflowOp bop in_typ
                   | bop <- [ "uadd", "sadd", "usub", "ssub" ]
                   , in_typ <- map L.iT [4, 8, 16, 32, 64] ]
                 ++
                 [ intrinsic ("llvm." ++ uop ++ "." ++ show (L.ppType typ)) typ [typ, L.iT 1]
                 | uop  <- ["cttz", "ctlz"]
                 , typ <- map L.iT [8, 16, 32, 64] ]

--------------------------------------------------------------------------------
-- conversion to LLVM
--------------------------------------------------------------------------------

-- | Implements 'llvmFunctionName' after soundness checks complete.
--
-- See the documentation for 'moduleForFunctions' for the convention of how
-- symbols are generated.
llvmFunctionName' :: MemWidth w
                 => Map (MemAddr w) L.Symbol
                 -- ^ Maps addresses of symbols to the associated symbol name.
                 -> String
                    -- ^ Prefix to use for automatically generated symbols.
                    -- To be able to distinguish symbols, this should not be
                    -- a prefix for any of the symbols in the map.
                 -> (MemSegmentOff w -> L.Symbol)
llvmFunctionName' m prefix segOff =
    case Map.lookup addr m of
      Just sym -> sym
      Nothing ->
        L.Symbol $ prefix ++ "_" ++ show (addrBase addr) ++ "_" ++ show (addrOffset addr)
  where addr = segoffAddr segOff

type AddrSymMap w = Map.Map (MemSegmentOff w) BSC.ByteString

-- | Creates a function for generating LLVM names.
--
-- This See the documentation for 'moduleForFunctions' for the convention of how
-- symbols are generated.
llvmFunctionName :: MemWidth w
                 => AddrSymMap w
                 -- ^ Maps addresses of symbols to the associated symbol name.
                 -> String
                    -- ^ Prefix to use for automatically generated symbols.
                    -- To be able to distinguish symbols, this should not be
                    -- a prefix for any of the symbols in the map.
                 -> Either String (MemSegmentOff w -> L.Symbol)
llvmFunctionName m prefix
  | any (\nm -> prefix `isPrefixOf` BSC.unpack nm) (Map.elems m) =
    Left $ "Symbols in binary must not start with " ++ prefix
  | otherwise =
    let m' = Map.fromList [ (segoffAddr o, L.Symbol (BSC.unpack s)) | (o,s) <- Map.toList m ]
     in Right $ llvmFunctionName' m' prefix

blockWordName :: MemWidth w => MemSegmentOff w  -> L.Ident
blockWordName o = L.Ident $ show $ GeneratedBlock o 0

blockName :: MemWidth w => BlockLabel w -> L.BlockLabel
blockName l = L.Named (L.Ident (show l))

-- The type of FP arguments and results.  We actually want fp128, but
-- it looks like llvm (at least as of version 3.6.2) doesn't put fp128
-- into xmm0 on a return, whereas it does for <2 x double>

natReprToLLVMType :: NatRepr n -> L.Type
natReprToLLVMType = L.iT . fromIntegral . natValue

llvmFloatType :: FloatInfoRepr flt -> L.FloatType
llvmFloatType flt =
  case flt of
    HalfFloatRepr -> L.Half
    SingleFloatRepr -> L.Float
    DoubleFloatRepr -> L.Double
    QuadFloatRepr -> L.Fp128
    X86_80FloatRepr -> L.X86_fp80

typeToLLVMType :: TypeRepr tp -> L.Type
typeToLLVMType BoolTypeRepr   = L.iT 1
typeToLLVMType (BVTypeRepr n) = natReprToLLVMType n
typeToLLVMType (FloatTypeRepr flt) = L.PrimType $ L.FloatType $ llvmFloatType flt
typeToLLVMType (TupleTypeRepr s) = L.Struct (toListFC typeToLLVMType s)
typeToLLVMType (VecTypeRepr w tp)
   | toInteger (natValue w) <= toInteger (maxBound :: Int32) =
     L.Vector (fromIntegral (natValue w)) (typeToLLVMType tp)
   | otherwise = error $ "Vector width of " ++ show w ++ " is too large."


-- | This is a special label used for e.g. table lookup defaults (where we should never reach).
-- For now it will just loop.
failLabel :: L.Ident
failLabel = L.Ident "failure"

functionTypeToLLVM :: FunctionType arch -> L.Type
functionTypeToLLVM ft = L.ptrT $
  L.FunTy (L.Struct (viewSome typeToLLVMType <$> fnReturnTypes ft))
          (viewSome typeToLLVMType <$> fnArgTypes ft)
          False

declareFunction :: L.Symbol -> FunctionType arch -> L.Declare
declareFunction nm ftp =
  L.Declare { L.decRetType = L.Struct (viewSome typeToLLVMType <$> fnReturnTypes ftp)
            , L.decName    = nm
            , L.decArgs    = viewSome typeToLLVMType <$> fnArgTypes ftp
            , L.decVarArgs = False
            , L.decAttrs   = []
            , L.decComdat  = Nothing
            }



failBlock :: L.BasicBlock
failBlock = L.BasicBlock { L.bbLabel = Just (L.Named failLabel)
                         , L.bbStmts = [L.Effect L.Unreachable []]
                         }

getReferencedFunctions :: forall arch
                       .  ( Eq (FunctionType arch)
                          , Show (FunctionType arch)
                          , MemWidth (ArchAddrWidth arch)
                          , FoldableFC (ArchFn arch)
                          , FoldableF (FnArchStmt arch)
                          )
                       => FunctionTypeMap arch
                       -> Function arch
                       -> FunctionTypeMap arch
getReferencedFunctions m0 f =
    foldl' (foldFnValue findReferencedFunctions) m1 (fnBlocks f)
  where m1 = insertAddr (Proxy :: Proxy arch) (fnAddr f) (fnType f) m0

        findReferencedFunctions :: FunctionTypeMap arch
                                -> FnValue arch tp
                                -> FunctionTypeMap arch
        findReferencedFunctions m (FnFunctionEntryValue ft addr) =
          insertAddr (Proxy :: Proxy arch) addr ft m
        findReferencedFunctions m _ = m

        insertAddr :: Proxy arch
                   -> MemSegmentOff (ArchAddrWidth arch)
                   -> FunctionType arch
                   -> FunctionTypeMap arch
                   -> FunctionTypeMap arch
        insertAddr _ addr ft m =
          case Map.lookup addr m of
            Just ft' | ft /= ft' ->
                         Loc.error $ show (segoffAddr addr) ++ " has incompatible types:\n"
                              ++ show ft  ++ "\n"
                              ++ show ft' ++ "\n"
                     | otherwise -> m
            _ -> Map.insert addr ft m

-- Pads the given list of values to be the target lenght using undefs
padUndef :: L.Type -> Int -> [L.Typed L.Value] -> [L.Typed L.Value]
padUndef typ len xs = xs ++ (replicate (len - length xs) (L.Typed typ L.ValUndef))

-- | Information about a phi node
data PendingPhiNode arch tp = PendingPhiNode !L.Ident !L.Type [(ArchLabel arch, ArchReg arch tp)]

-- | Result obtained by printing a block to LLVM
data LLVMBlockResult arch = LLVMBlockResult { regBlock         :: !(FnBlock arch)
                                            , llvmBlockStmts   :: ![L.Stmt]
                                            , llvmBlockPhiVars :: ![Some (PendingPhiNode arch)]
                                            }

-- | Map needed to resolve phi references
type ResolvePhiMap arch = Map (ArchLabel arch) (LLVMBlockResult arch)

------------------------------------------------------------------------
-- IntrinsicMap

-- | Map from intrinsic name to intrinsic seen so far
--
-- Globally maintained when translating model so that we know which
-- definitions to include.
type IntrinsicMap = Map L.Symbol Intrinsic

------------------------------------------------------------------------
-- FunState

-- | State relative to a function.
data FunState arch =
  FunState { nmCounter :: !Int
             -- ^ Counter for generating new identifiers
           , funAssignValMap :: !(AssignValMap (ArchAddrWidth arch))
             -- ^ Map from function ids already assigned to label
             -- where it was assigned and associated value.
           , funIntrinsicMap :: !IntrinsicMap
           }

funIntrinsicMapLens :: Simple Lens (FunState arch) IntrinsicMap
funIntrinsicMapLens = lens funIntrinsicMap (\s v -> s { funIntrinsicMap = v })

------------------------------------------------------------------------
-- BBLLVM

-- | Architecture-specific operations for generating LLVM
data LLVMArchSpecificOps arch = LLVMArchSpecificOps
  { archEndianness :: !Endianness
    -- ^ Endiannes of architecture
   , mkInitBlock :: FunctionType arch
                -> L.BlockLabel
                -> ([L.Typed L.Ident], L.BasicBlock, V.Vector (L.Typed L.Value))
    -- ^ This is responsible for mapping between function arguments and
    -- the block format used by FnRep.
    --
    -- Essentially it is used so that we can map between the types needed
    -- to generate an ABI compliant function and the types expected by the
    -- FnRep.  Eventually, we hope to extend the FnRep representation
    -- enough so that this is not necessary.
    --
    -- The arguments are the type of the function and the label of the
    -- first block.
    --
    -- The return type consists of a triple containing the identifiers
    -- used in the function definition, a block to run before jumping to
    -- the first FnRep block, and a vector identifiying the arguments to
    -- actually use as the start of the function.
  , archFnCallback :: !(forall tp . ArchFn arch (FnValue arch) tp -> BBLLVM arch (L.Typed L.Value))
    -- ^ Callback for architecture-specific functions
  , archStmtCallback :: !(FnArchStmt arch (FnValue arch) -> BBLLVM arch ())
    -- ^ Callback for architecture-specific statements
  , popCountCallback :: !(forall w . NatRepr w -> FnValue arch (BVType w) -> BBLLVM arch (L.Typed L.Value))
    -- ^ Callback for emitting a popcount instruction
    --
    -- This is architecture-specific so that we can choose machine code.
  }

-- | Computes the LLVM symbol of a function at the given address.
type FunctionSymbolCallback w = MemSegmentOff w -> L.Symbol

-- | Information used to generate LLVM for a specific function.
--
-- This information is the same for all blocks within the function.
data FunLLVMContext arch = FunLLVMContext
  { funSymbolCallback :: !(FunctionSymbolCallback (ArchAddrWidth arch))
    -- ^ This is a callback for naming functions at specific addresses.
  , funAddrTypeMap :: !(FunctionTypeMap arch)
  , funArgs      :: !(V.Vector (L.Typed L.Value))
     -- ^ Arguments to this function.
  , funReturnType :: !L.Type
    -- ^ LLVM return type for this function.
  }


-- | State specific to translating a single basic block.
data BBLLVMState arch = BBLLVMState
  { archFns  :: !(LLVMArchSpecificOps arch)
    -- ^ Architecture-specific functions
  , funContext :: !(FunLLVMContext arch)
    -- ^ Context for function level declarations.
  , funState :: !(FunState arch)
   -- ^ State local to function rather than block
  , bbBlock :: !(FnBlock arch)
    -- ^ Basic block that we are generating the LLVM for.
  , bbStmts :: ![L.Stmt]
    -- ^ Statements in reverse order
  , bbBoundPhiVars :: ![Some (PendingPhiNode arch)]
    -- ^ Identifiers that we need to generate the phi node information for.
  }

funStateLens :: Simple Lens (BBLLVMState arch) (FunState arch)
funStateLens = lens funState (\s v -> s { funState = v })

newtype BBLLVM arch a = BBLLVM { unBBLLVM :: State (BBLLVMState arch) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (BBLLVMState arch)
           )

-- | Generate a fresh identifier with the name 'rX'
freshName :: BBLLVM arch L.Ident
freshName = do
  s <- get
  let fs = funState s
  let fs' = fs { nmCounter = nmCounter fs + 1 }
  put $! s { funState = fs' }
  pure $! L.Ident ('r' : show (nmCounter fs))

-- | Append a statement to the list of statements
emitStmt :: L.Stmt -> BBLLVM arch ()
emitStmt stmt = seq stmt $ do
  s <- get
  put $! s { bbStmts = stmt : bbStmts s }

-- | Evaluate an instruction and return the result.
evalInstr :: L.Instr -> BBLLVM arch L.Value
evalInstr i = do
  nm <- freshName
  emitStmt $! L.Result nm i []
  pure $! L.ValIdent nm

-- | Emit an instruction as an effect
effect :: L.Instr -> BBLLVM arch ()
effect i = emitStmt $ L.Effect i []

-- | Add a comment instruction
comment :: String -> BBLLVM arch ()
comment msg = effect (L.Comment msg)

ret :: L.Typed L.Value -> BBLLVM arch ()
ret = effect . L.Ret

unimplementedInstr' :: L.Type -> String -> BBLLVM arch (L.Typed L.Value)
unimplementedInstr' typ reason = do
  comment ("UNIMPLEMENTED: " ++ reason)
  return (L.Typed typ L.ValUndef)

-- | Map from assign ID to value.
type AssignValMap w = Map FnAssignId (BlockLabel w, L.Typed L.Value)

setAssignIdValue :: Loc.HasCallStack
                 => FnAssignId
                 -> ArchLabel arch
                 -> L.Typed L.Value
                 -> BBLLVM arch ()
setAssignIdValue fid blk v = do
  s <- get
  let fs = funState s
  case Map.lookup fid (funAssignValMap fs) of
    Just{} -> Loc.error $ "internal: Assign id " ++ show (pretty fid) ++ " already assigned."
    Nothing -> do
      let fs' = fs { funAssignValMap = Map.insert fid (blk,v) (funAssignValMap fs) }
      put $! s { funState = fs' }

addBoundPhiVar :: L.Ident
               -> L.Type
               -> [(ArchLabel arch, ArchReg arch tp)]
               -> BBLLVM arch ()
addBoundPhiVar nm tp info = do
  s <- get
  let pair = Some $ PendingPhiNode nm tp info
  seq pair $ put $! s { bbBoundPhiVars = pair : bbBoundPhiVars s }

------------------------------------------------------------------------
-- Convert a value to LLVM

type LLVMArchConstraints arch
   = ( FnArchConstraints arch
     , PrettyF (ArchReg arch)
     , OrdF (ArchReg arch)
     , IsArchStmt (FnArchStmt arch)
     , Eq (FunctionType arch)
     )

-- | Map a function value to a LLVM value with no change.
valueToLLVM :: forall arch tp
            .  ( LLVMArchConstraints arch
               , Loc.HasCallStack
               )
            => FunLLVMContext arch
            -> FnBlock arch
               -- ^ Block we are generating LLVM for.
            -> AssignValMap (ArchAddrWidth arch)
            -> FnValue arch tp
            -> L.Typed L.Value
valueToLLVM ctx blk m val = do
  let ptrWidth = addrWidthNatRepr (addrWidthRepr (Proxy :: Proxy (ArchAddrWidth arch)))
  case val of
    FnValueUnsupported _reason typ -> L.Typed (typeToLLVMType typ) L.ValUndef
    -- A value that is actually undefined, like a non-argument register at
    -- the start of a function.
    FnUndefined typ -> L.Typed (typeToLLVMType typ) L.ValUndef
    FnConstantBool b -> L.Typed (L.iT 1) $ L.integer (if b then 1 else 0)
    FnConstantValue sz n -> L.Typed (natReprToLLVMType sz) $ L.integer n
    -- Value from an assignment statement.
    FnAssignedValue (FnAssignment lhs _rhs) ->
      case Map.lookup lhs m of
        Just (_,v) -> v
        Nothing ->
          Loc.error $ "Could not find assignment value " ++ show (pretty lhs) ++ "\n"
                      ++ show (pretty blk)
    -- Value from a phi node
    FnPhiValue (FnPhiVar lhs _tp)  -> do
      case Map.lookup lhs m of
        Just (_,v) -> v
        Nothing ->
          Loc.error $ "Could not find phi value " ++ show (pretty lhs) ++ "\n"
                      ++ show (pretty blk)
    -- A value returned by a function call (rax/xmm0)
    FnReturn (FnReturnVar lhs _tp) ->
      case Map.lookup lhs m of
        Just (_,v) -> v
        Nothing ->
          Loc.error $ "Could not find return variable " ++ show (pretty lhs) ++ "\n"
                      ++ show (pretty blk) ++ "\n"
                      ++ show m
    -- The entry pointer to a function.  We do the cast as a const
    -- expr as function addresses appear as constants in e.g. phi
    -- nodes
    FnFunctionEntryValue ft addr -> do
      case Map.lookup addr (funAddrTypeMap ctx) of
        Just tp
          | ft /= tp -> Loc.error "Mismatch function type"
          | otherwise -> do
            let sym = funSymbolCallback ctx addr
            seq sym $ do
              let typ = natReprToLLVMType ptrWidth
              let fptr :: L.Typed L.Value
                  fptr = L.Typed (functionTypeToLLVM ft) (L.ValSymbol sym)
              L.Typed typ $ L.ValConstExpr (L.ConstConv L.PtrToInt fptr typ)
        Nothing -> do
          Loc.error $ "Could not identify " ++ show addr
    -- A pointer to an internal block at the given address.
    FnBlockValue _addr -> do
      Loc.error $ "Cannot create references to local blocks."
    -- Value is an argument passed via a register.
    FnRegArg _ i -> r
      where Just r = funArgs ctx V.!? i
    -- A global address
    FnGlobalDataAddr addr ->
      case segoffAsAbsoluteAddr addr of
        Nothing -> Loc.error $ "FnGlobalDataAddr only supports global values."
        Just fixedAddr -> L.Typed (natReprToLLVMType ptrWidth) $ L.integer $ fromIntegral fixedAddr

-- | Return number of bits and LLVM float type should take
floatTypeWidth :: L.FloatType -> Int32
floatTypeWidth l =
  case l of
    L.Half -> 16
    L.Float -> 32
    L.Double -> 64
    L.Fp128 -> 128
    L.X86_fp80 -> 80
    L.PPC_fp128 -> Loc.error "PPC floating point types not supported."

-- | Map a function value to a LLVM value with no change.
valueToLLVMBitvec :: (LLVMArchConstraints arch, Loc.HasCallStack)
                  => FunLLVMContext arch
                  -> FnBlock arch
                     -- ^ Block that we are generating LLVM for.
                  -> AssignValMap (ArchAddrWidth arch)
                  -> FnValue arch tp
                  -> L.Typed L.Value
valueToLLVMBitvec ctx blk m val = do
  let llvm_val = valueToLLVM ctx blk m val
  case L.typedType llvm_val of
    L.PrimType (L.Integer _) -> llvm_val
    L.PrimType (L.FloatType tp) ->
      let itp = L.iT (floatTypeWidth tp)
       in L.Typed itp $ L.ValConstExpr $ L.ConstConv L.BitCast llvm_val itp
    _ -> Loc.error $ "valueToLLVMBitvec given unsupported type: " ++ show (L.typedType llvm_val)

mkLLVMValue :: (LLVMArchConstraints arch, Loc.HasCallStack)
            => FnValue arch tp
            -> BBLLVM arch (L.Typed L.Value)
mkLLVMValue val = do
  s <- get
  let fs = funState s
  pure $! valueToLLVMBitvec (funContext s) (bbBlock s) (funAssignValMap fs) val

arithop :: L.ArithOp -> L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
arithop f val s = L.Typed (L.typedType val) <$> evalInstr (L.Arith f val s)

bitop :: L.BitOp -> L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
bitop f val s = L.Typed (L.typedType val) <$> evalInstr (L.Bit f val s)

-- | Conversion operation
convop :: L.ConvOp -> L.Typed L.Value -> L.Type -> BBLLVM arch (L.Typed L.Value)
convop f val tp = L.Typed tp <$> evalInstr (L.Conv f val tp)

-- | Compare two LLVM values using the given operator.
icmpop :: L.ICmpOp -> L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
icmpop f val s = do
  L.Typed (L.iT 1) <$> evalInstr (L.ICmp f val s)

-- | Extract a value from a struct
extractValue :: L.Typed L.Value -> Int32 -> BBLLVM arch (L.Typed L.Value)
extractValue ta i = do
  let etp = case L.typedType ta of
              L.Struct fl -> fl !! fromIntegral i
              L.Array _l etp' -> etp'
              _ -> Loc.error "extractValue not given a struct or array."
  L.Typed etp <$> evalInstr (L.ExtractValue ta [i])

insertValue :: L.Typed L.Value -> L.Typed L.Value -> Int32 -> BBLLVM arch (L.Typed L.Value)
insertValue ta tv i =
  L.Typed (L.typedType ta) <$> evalInstr (L.InsertValue ta tv [i])

-- | Do a bitcast
bitcast :: L.Typed L.Value -> L.Type -> BBLLVM arch (L.Typed L.Value)
bitcast = convop L.BitCast

-- | Unsigned extension
zext :: L.Typed L.Value -> L.Type -> BBLLVM arch (L.Typed L.Value)
zext = convop L.ZExt

band :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
band = bitop L.And

bor :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
bor = bitop L.Or

bxor :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
bxor = bitop L.Xor

shl :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
shl = bitop (L.Shl False False)

ashr :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
ashr = bitop (L.Ashr False)

lshr :: L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value)
lshr = bitop (L.Lshr False)

alloca :: L.Type -> Maybe (L.Typed L.Value) -> Maybe Int -> BBLLVM arch (L.Typed L.Value)
alloca tp cnt align = fmap (L.Typed (L.PtrTo tp)) $ evalInstr $ L.Alloca tp cnt align

-- | Generate a non-tail call that returns a value
call :: HasValue v => v -> [L.Typed L.Value] -> BBLLVM arch (L.Typed L.Value)
call (valueOf -> f) args =
  case L.typedType f of
    L.PtrTo (L.FunTy res argTypes varArgs) -> do
      when varArgs $ do
        Loc.error $ "Varargs not yet supported."
      let actualTypes = fmap L.typedType args
      when (argTypes /= actualTypes) $ do
        Loc.error $ "Unexpected arguments to " ++ show f ++ "\n"
                 ++ "Expected: " ++ show argTypes ++ "\n"
                 ++ "Actual:   " ++ show actualTypes
      fmap (L.Typed res) $ evalInstr $ L.Call False (L.typedType f) (L.typedValue f) args
    _ -> Loc.error $ "Call given non-function pointer argument:\n" ++ show f

-- | Generate a non-tail call that does not return a value
call_ :: HasValue v => v -> [L.Typed L.Value] -> BBLLVM arch ()
call_ (valueOf -> f) args =
  case L.typedType f of
    L.PtrTo (L.FunTy (L.PrimType L.Void) argTypes varArgs) -> do
      when varArgs $ do
        Loc.error $ "Varargs not yet supported."
      when (argTypes /= fmap L.typedType args) $ do
        Loc.error $ "Unexpected arguments to " ++ show f
      effect $ L.Call False (L.typedType f) (L.typedValue f) args
    _ -> Loc.error $ "call_ given non-function pointer argument\n" ++ show f

-- | Sign extend a boolean value to the given width.
carryValue :: (LLVMArchConstraints arch, Loc.HasCallStack, 1 <= w)
           => NatRepr w
           -> FnValue arch BoolType
           -> BBLLVM arch (L.Typed L.Value)
carryValue w x =
  (`zext` natReprToLLVMType w) =<< mkLLVMValue x


-- | Handle an intrinsic overflows
intrinsicOverflows :: (LLVMArchConstraints arch, Loc.HasCallStack, 1 <= w)
                    => String
                    -> FnValue arch (BVType w)
                    -> FnValue arch (BVType w)
                    -> FnValue arch BoolType
                    -> BBLLVM arch (L.Typed L.Value)
-- Special case where carry/borrow flag is 0.
intrinsicOverflows bop x y (FnConstantBool False) = do
  x' <- mkLLVMValue x
  y' <- mkLLVMValue y
  let in_typ = L.typedType x'
  r_tuple    <- call (overflowOp bop in_typ) [x', y']
  extractValue r_tuple 1
-- General case involves two calls
intrinsicOverflows bop x y c = do
  x' <- mkLLVMValue x
  y' <- mkLLVMValue y
  let in_typ = L.typedType x'
  r_tuple    <- call (overflowOp bop in_typ) [x', y']
  r          <- extractValue r_tuple 0
  overflows  <- extractValue r_tuple 1
  -- Check for overflow in carry flag
  c' <- carryValue (typeWidth x) c
  r_tuple'   <- call (overflowOp bop in_typ) [r, c']
  overflows' <- extractValue r_tuple' 1
  bor overflows (L.typedValue overflows')


appToLLVM :: forall arch tp
          .  (LLVMArchConstraints arch, Loc.HasCallStack)
          => App (FnValue arch) tp
          -> BBLLVM arch (L.Typed L.Value)
appToLLVM app = do
  let typ = typeToLLVMType $ typeRepr app
  let binop :: (L.Typed L.Value -> L.Value -> BBLLVM arch (L.Typed L.Value))
            -> FnValue arch utp
            -> FnValue arch utp
            -> BBLLVM arch (L.Typed L.Value)
      binop f x y = do
        x' <- mkLLVMValue x
        y' <- mkLLVMValue y
        f x' (L.typedValue y')
  case app of
    Eq x y ->  binop (icmpop L.Ieq) x y
    Mux _tp c t f -> do
      l_c <- mkLLVMValue c
      l_t <- mkLLVMValue t
      l_f <- mkLLVMValue f
      fmap (L.Typed (L.typedType l_t)) $ evalInstr $ L.Select l_c l_t (L.typedValue l_f)
    Trunc v sz -> mkLLVMValue v >>= \u -> convop L.Trunc u (natReprToLLVMType sz)
    SExt v sz  -> flip (convop L.SExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    UExt v sz  -> flip (convop L.ZExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    Bitcast x tp -> do
      llvmVal <- mkLLVMValue x
      convop L.BitCast llvmVal (typeToLLVMType (widthEqTarget tp))
    AndApp x y -> binop band x y
    OrApp  x y -> binop bor x y
    NotApp x   -> do
      -- x = 0 == complement x, according to LLVM manual.
      llvm_x <- mkLLVMValue x
      icmpop L.Ieq llvm_x (L.ValInteger 0)
    XorApp x y    -> binop bxor x y
    BVAdd _sz x y -> binop (arithop (L.Add False False)) x y
    BVAdc _sz x y (FnConstantBool False) -> do
      binop (arithop (L.Add False False)) x y
    BVAdc _sz x y c -> do
      r <- binop (arithop (L.Add False False)) x y
      arithop (L.Add False False) r . L.typedValue =<< carryValue (typeWidth x) c

    BVSub _sz x y -> binop (arithop (L.Sub False False)) x y
    BVSbb _sz x y (FnConstantBool False) -> do
      binop (arithop (L.Sub False False)) x y
    BVSbb _sz x y b -> do
      d <- binop (arithop (L.Sub False False)) x y
      arithop (L.Sub False False) d . L.typedValue =<< carryValue (typeWidth x) b

    BVMul _sz x y -> binop (arithop (L.Mul False False)) x y
    BVUnsignedLt x y     -> binop (icmpop L.Iult) x y
    BVUnsignedLe x y     -> binop (icmpop L.Iule) x y
    BVSignedLt x y       -> binop (icmpop L.Islt) x y
    BVSignedLe x y       -> binop (icmpop L.Isle) x y
    BVTestBit v n     -> do -- FIXME
      llvm_v <- mkLLVMValue v
      let in_typ = L.typedType llvm_v
      n' <- mkLLVMValue n
      n_ext <-
        case compareF (typeWidth v) (typeWidth n) of
          LTF -> Loc.error "BVTestBit expected second argument to be at least first"
          EQF -> pure n'
          GTF -> zext n' in_typ
      mask <- shl (L.Typed in_typ (L.ValInteger 1)) (L.typedValue n_ext)
      r <- bitop L.And llvm_v (L.typedValue mask)
      icmpop L.Ine r (L.ValInteger 0)
    BVComplement _sz v -> do
      -- xor x -1 == complement x, according to LLVM manual.
      llvm_v <- mkLLVMValue v
      bitop L.Xor llvm_v (L.ValInteger (-1))
    BVAnd _sz x y -> binop band x y
    BVOr _sz x y  -> binop bor  x y
    BVXor _sz x y -> binop bxor x y
    BVShl _sz x y -> binop shl  x y
    BVSar _sz x y -> binop ashr x y
    BVShr _sz x y -> binop lshr x y
    PopCount w v  -> do
      fn <- gets $ popCountCallback  . archFns
      fn w v
    ReverseBytes{} -> unimplementedInstr' typ "ReverseBytes"
    -- FIXME: do something more efficient?
    -- Basically does let (r, over)  = llvm.add.with.overflow(x,y)
    --                    (_, over') = llvm.add.with.overflow(r,c)
    --                in over'
    -- and we rely on llvm optimisations to throw away identical adds
    -- and adds of 0
    UadcOverflows x y c -> intrinsicOverflows "uadd" x y c
    SadcOverflows x y c -> intrinsicOverflows "sadd" x y c
    UsbbOverflows x y c -> intrinsicOverflows "usub" x y c
    SsbbOverflows x y c -> intrinsicOverflows "ssub" x y c

    Bsf _sz v -> do
      let cttz = intrinsic ("llvm.cttz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
      v' <- mkLLVMValue v
      call cttz [v', L.iT 1 L.-: L.int 1]
    Bsr _sz v -> do
      let ctlz = intrinsic ("llvm.ctlz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
      v' <- mkLLVMValue v
      call ctlz [v', L.iT 1 L.-: L.int 1]
    TupleField{} -> unimplementedInstr' typ (show (ppApp pretty app))

-- | Evaluate a value as a pointer
llvmAsPtr :: (LLVMArchConstraints arch, Loc.HasCallStack)
          => FnValue arch (BVType (ArchAddrWidth arch))
             -- ^ Value to evaluate
          -> L.Type
             -- ^ Type of value pointed to
          -> BBLLVM arch (L.Typed L.Value)
llvmAsPtr ptr tp = do
  llvmPtrAsBV  <- mkLLVMValue ptr
  convop L.IntToPtr llvmPtrAsBV (L.PtrTo tp)

addIntrinsic :: Intrinsic -> BBLLVM arch ()
addIntrinsic i = do
  m <- use $ funStateLens . funIntrinsicMapLens
  when (Map.notMember (intrinsicName i) m) $ do
    funStateLens . funIntrinsicMapLens .= Map.insert (intrinsicName i) i m

-- | Create a singleton vector from a value.
singletonVector :: L.Typed L.Value -> BBLLVM arch (L.Typed L.Value)
singletonVector val = do
  let eltType = L.typedType val
  let arrayType = L.Vector 1 eltType
  let vec0 = L.Typed arrayType L.ValUndef
  L.Typed arrayType
    <$> evalInstr (L.InsertElt vec0 val (L.ValInteger 0))

checkEndian :: Endianness -> BBLLVM arch ()
checkEndian end = do
  req <- gets $ archEndianness . archFns
  when (end /= req) $
    fail $ "Memory accesses must have type " ++ show req

llvmFloatName :: FloatInfoRepr flt -> String
llvmFloatName flt =
  case flt of
    HalfFloatRepr -> "f16"
    SingleFloatRepr -> "f32"
    DoubleFloatRepr -> "f64"
    QuadFloatRepr -> "f128"
    X86_80FloatRepr -> "f80"


-- | Given a `MemRepr`, return the name of the type for annotating
-- intrinsics and the type itself.
resolveLoadNameAndType :: MemRepr tp -> BBLLVM arch (String, L.Type)
resolveLoadNameAndType memRep =
  case memRep of
    BVMemRepr w end -> do
      checkEndian end
      let bvw = 8 * fromIntegral (natValue w)
      pure ("i" ++ show bvw, L.iT bvw)
    FloatMemRepr f end -> do
      checkEndian end
      pure (llvmFloatName f, L.PrimType (L.FloatType (llvmFloatType f)))
    PackedVecMemRepr n eltRep -> do
      (eltName, eltType) <- resolveLoadNameAndType eltRep
      unless (toInteger (natValue n) <= toInteger (maxBound :: Int32)) $ do
        error $ "Vector width of " ++ show n ++ " is too large."
      pure ("v" ++ show n ++ eltName, L.Vector (fromIntegral (natValue n)) eltType)

rhsToLLVM :: (LLVMArchConstraints arch, Loc.HasCallStack)
          => FnAssignRhs arch (FnValue arch) tp
          -> BBLLVM arch (L.Typed L.Value)
rhsToLLVM rhs = do
  case rhs of
    FnEvalApp app ->
      appToLLVM app
    FnSetUndefined tp -> do
      let typ = typeToLLVMType tp
      return (L.Typed typ L.ValUndef)
    FnReadMem ptr typ -> do
      let llvm_typ = typeToLLVMType typ
      p <- llvmAsPtr ptr llvm_typ
      fmap (L.Typed llvm_typ) $ evalInstr (L.Load p Nothing Nothing)
    FnCondReadMem memRepr cond addr passthru -> do
      (loadName, eltType) <- resolveLoadNameAndType memRepr
      let intr = llvmMaskedLoad 1 loadName eltType
      addIntrinsic intr
      llvmAddr     <- llvmAsPtr addr (L.Vector 1 eltType)
      let llvmAlign = L.Typed (L.iT 32) (L.ValInteger 0)
      llvmCond     <- singletonVector =<< mkLLVMValue cond
      llvmPassthru <- singletonVector =<< mkLLVMValue passthru
      rv <- call intr [ llvmAddr, llvmAlign, llvmCond, llvmPassthru ]
      r <- evalInstr $ L.ExtractElt rv (L.ValInteger 0)
      pure (L.Typed eltType r)
      --  FloatMemRepr :: !(FloatInfoRepr f) -> !Endianness -> MemRepr (FloatType f)
      --  PackedVecMemRepr :: !(NatRepr n) -> !(MemRepr tp) -> MemRepr (VecType n tp)

    FnAlloca v -> do
      v' <- mkLLVMValue v
      alloc_ptr <- alloca (L.iT 8) (Just v') Nothing
      convop L.PtrToInt alloc_ptr (L.iT 64)
    FnEvalArchFn f -> do
      fn <- gets $ archFnCallback  . archFns
      fn f


resolveFunctionEntry :: LLVMArchConstraints arch
                     => FnValue arch (BVType (ArchAddrWidth arch))
                     -> FunctionType arch
                     -> BBLLVM arch (L.Typed L.Value)
resolveFunctionEntry dest ft = do
  let fun_ty  = functionTypeToLLVM ft
  addrTypeMap <- gets $ funAddrTypeMap . funContext
  case dest of
    -- FIXME: use ft here instead?
    FnFunctionEntryValue dest_ftp addr
      | Just tp <- Map.lookup addr addrTypeMap -> do
          fnCallback  <- gets $ funSymbolCallback  . funContext
          let sym = fnCallback addr
          when (functionTypeToLLVM tp /= fun_ty) $ do
            Loc.error $ "Mismatch function type with " ++ show sym ++ "\n"
              ++ "Declared: " ++ show (functionTypeToLLVM tp) ++ "\n"
              ++ "Provided: " ++ show fun_ty
          when (ft /= dest_ftp) $ do
            Loc.error $ "Mismatch function type in call with " ++ show sym
          when (tp /= dest_ftp) $ do
            Loc.error $ "Mismatch function type in call with " ++ show sym
          return $ L.Typed fun_ty (L.ValSymbol sym)
    _ -> do
      dest' <- mkLLVMValue dest
      convop L.IntToPtr dest' fun_ty

stmtToLLVM :: forall arch
           .  (LLVMArchConstraints arch, Loc.HasCallStack)
           => FnStmt arch
           -> BBLLVM arch ()
stmtToLLVM stmt = do
  comment (show $ pretty stmt)
  case stmt of
   FnComment _ -> return ()
   FnAssignStmt (FnAssignment lhs rhs) -> do
     lbl <- gets $ fbLabel . bbBlock
     llvm_rhs <- rhsToLLVM rhs
     setAssignIdValue lhs lbl llvm_rhs
   FnWriteMem ptr v -> do
     llvm_v <- mkLLVMValue v
     llvm_ptr <- llvmAsPtr ptr (L.typedType llvm_v)
     -- Cast LLVM point to appropriate type
     effect $ L.Store llvm_v llvm_ptr Nothing Nothing
   FnCall dest ft args retvs -> do
     dest_f <- resolveFunctionEntry dest ft
     args' <- mapM (\(Some v) -> mkLLVMValue v) args
     retv <- call dest_f args'
     this_lbl <- gets $ fbLabel . bbBlock
     -- Assign all return variables to the extracted result
     let assignReturn :: Int -> Some FnReturnVar -> BBLLVM arch ()
         assignReturn i (Some fr) = do
           val <- extractValue retv (fromIntegral i)
           setAssignIdValue (frAssignId fr) this_lbl val
     itraverse_ assignReturn retvs
   FnArchStmt astmt -> do
     fn <- gets $ archStmtCallback . archFns
     fn astmt

termStmtToLLVM :: (LLVMArchConstraints arch, Loc.HasCallStack)
               => FnTermStmt arch
               -> BBLLVM arch ()
termStmtToLLVM tm =
  case tm of
    FnJump lbl -> do
      effect $ L.Jump (blockName lbl)
    FnRet rets -> do
      retType <- gets $ funReturnType . funContext
      retVals <- mapM (viewSome mkLLVMValue) rets
      -- construct the return result struct
      let initUndef = L.Typed retType L.ValUndef
      v <- ifoldlM (\n acc fld -> insertValue acc fld (fromIntegral n)) initUndef retVals
      effect (L.Ret v)
    FnBranch cond tlbl flbl -> do
      cond' <- mkLLVMValue cond
      effect $ L.Br cond' (blockName tlbl) (blockName flbl)
    FnTailCall dest ft args -> do
      dest_f <- resolveFunctionEntry dest ft
      args' <- mapM (viewSome mkLLVMValue) args
      retv <- call dest_f args'
      ret retv
    FnLookupTable idx vec -> do
      idx' <- mkLLVMValue idx
      let dests = map (L.Named . blockWordName) $ V.toList vec
      effect $ L.Switch idx' (L.Named failLabel) (zip [0..] dests)

-- | Convert a Phi node assignment to the right value
resolvePhiNodeReg :: (LLVMArchConstraints arch, Loc.HasCallStack)
                  => FunLLVMContext arch
                  -> AssignValMap (ArchAddrWidth arch)
                  -> ResolvePhiMap arch
                  -> (ArchLabel arch, ArchReg arch tp)
                  -> (L.Value, L.BlockLabel)
resolvePhiNodeReg ctx avmap m (lbl, reg) =
  case Map.lookup lbl m of
    Nothing -> Loc.error $ "Could not resolve block " ++ show lbl
    Just br -> do
      let b = regBlock br
      case MapF.lookup reg (fbRegMap b) of
        Nothing -> Loc.error $ "Could not resolve register " ++ show (prettyF reg) ++ " in block " ++ show lbl
        Just (CalleeSaved _) -> Loc.error $ "Resolve callee saved register"
        Just (FnRegValue v) -> (L.typedValue (valueToLLVMBitvec ctx b avmap v), blockName lbl)

resolvePhiStmt :: (LLVMArchConstraints arch, Loc.HasCallStack)
               => FunLLVMContext arch
               -> AssignValMap (ArchAddrWidth arch)
               -> ResolvePhiMap arch
               -> Some (PendingPhiNode arch)
               -> L.Stmt
resolvePhiStmt ctx avmap m (Some (PendingPhiNode nm tp info)) = L.Result nm i []
  where i = L.Phi tp (resolvePhiNodeReg ctx avmap m <$> info)

toBasicBlock :: (LLVMArchConstraints arch, Loc.HasCallStack)
             => FunLLVMContext arch
             -> AssignValMap (ArchAddrWidth arch)
             -> ResolvePhiMap arch
             -> LLVMBlockResult arch
             -> L.BasicBlock
toBasicBlock ctx avmap m res = L.BasicBlock { L.bbLabel = Just (blockName (fbLabel (regBlock res)))
                                            , L.bbStmts = phiStmts ++ llvmBlockStmts res
                                            }
  where phiStmts = resolvePhiStmt ctx avmap m <$> llvmBlockPhiVars res


-- | Add a phi var with the node info so that we have a ident to reference
-- it by and queue up work to assign the value later.
addPhiBinding :: Loc.HasCallStack => Some (PhiBinding (ArchReg arch)) -> BBLLVM arch ()
addPhiBinding (Some (PhiBinding (FnPhiVar fid tp) info)) = do
  lbl <- gets $ fbLabel . bbBlock
  nm <- freshName
  let llvm_tp = typeToLLVMType tp
  setAssignIdValue fid lbl (L.Typed llvm_tp (L.ValIdent nm))
  addBoundPhiVar nm llvm_tp info


blockToLLVM :: forall arch
            .  LLVMArchConstraints arch
            => LLVMArchSpecificOps arch
            -> FunLLVMContext arch
               -- ^ Context for block
            -> FunState arch
            -> FnBlock arch
               -- ^ Block to generate
            -> (LLVMBlockResult arch, FunState arch)
blockToLLVM fns ctx fs b = (res, funState s)
  where s0 = BBLLVMState { archFns = fns
                         , funContext     = ctx
                         , bbBlock        = b
                         , bbStmts        = []
                         , funState       = fs
                         , bbBoundPhiVars = []
                         }
        go :: BBLLVM arch ()
        go = do
          -- Add statements for Phi nodes
          mapM_ addPhiBinding (fbPhiNodes b)
          -- Add statements
          mapM_ stmtToLLVM (fbStmts b)
          -- Add term statement
          termStmtToLLVM (fbTerm b)
        s = execState (unBBLLVM go) s0

        res = LLVMBlockResult { regBlock = b
                              , llvmBlockStmts = reverse (bbStmts s)
                              , llvmBlockPhiVars = reverse (bbBoundPhiVars s)
                              }

------------------------------------------------------------------------
-- Inline assembly

-- | Attributes to annote inline assembly.
data AsmAttrs = AsmAttrs { asmSideeffect :: !Bool }

-- | Indicates all side effects  of inline assembly are captured in the constraint list.
noSideEffect :: AsmAttrs
noSideEffect = AsmAttrs { asmSideeffect = False }

-- | Indicates asm may have side effects that are not captured in the constraint list.
sideEffect :: AsmAttrs
sideEffect = AsmAttrs { asmSideeffect = True }

-- | Call some inline assembly
callAsm :: AsmAttrs -- ^ Asm attrs
        -> L.Type   -- ^ Return type
        -> String   -- ^ Code
        -> String   -- ^ Args code
        -> [L.Typed L.Value] -- ^ Arguments
        -> BBLLVM arch (L.Typed L.Value)
callAsm attrs resType asmCode asmArgs args = do
  let argTypes = L.typedType <$> args
      ftp = L.PtrTo (L.FunTy resType argTypes False)
      f = L.ValAsm (asmSideeffect attrs) False asmCode asmArgs
  L.Typed resType <$> evalInstr (L.Call False ftp f args)

-- | Call some inline assembly that does not return a value.
callAsm_ :: AsmAttrs -- ^ Asm attrs
         -> String   -- ^ Code
         -> String   -- ^ Args code
         -> [L.Typed L.Value] -- ^ Arguments
         -> BBLLVM arch ()
callAsm_ attrs asmCode asmArgs args = do
  let argTypes = L.typedType <$> args
  let ftp = L.PtrTo (L.FunTy (L.PrimType L.Void) argTypes False)
  let f :: L.Value
      f = L.ValAsm (asmSideeffect attrs) False asmCode asmArgs
  call_ (L.Typed ftp f) args

------------------------------------------------------------------------
-- Translating functions

-- | The LLVM translate monad records all the intrinsics detected when adding LLVM.
newtype LLVMTrans a = LLVMTrans (State IntrinsicMap a)
  deriving (Functor, Applicative, Monad, MonadState IntrinsicMap)

-- | Run the translation returning result and intrinsics.
runLLVMTrans :: LLVMTrans a -> ([Intrinsic], a)
runLLVMTrans (LLVMTrans action) =
  let (r, m) = runState action Map.empty
   in (Map.elems m, r)

-- | This translates the function to LLVM and returns the define.
--
-- We have each function return all possible results, although only the ones that are actually
-- used (we use undef for the others).  This makes the LLVM conversion slightly simpler.
--
-- Users should declare intrinsics via 'declareIntrinsics' before using this function.
-- They should also add any referenced functions.
defineFunction :: forall arch
               .  LLVMArchConstraints arch
               => LLVMArchSpecificOps arch
                  -- ^ Architecture specific operations
               -> FunctionSymbolCallback (ArchAddrWidth arch)
                  -- ^ Callback for naming functions
               -> FunctionTypeMap arch
               -> Function arch
               -> LLVMTrans L.Define
defineFunction archOps symFun funTypeMap f = do
  let symbol = symFun (fnAddr f)

  -- Create initial block
  let inputArgs :: [L.Typed L.Ident]
      initBlock :: L.BasicBlock
      postInitArgs :: V.Vector (L.Typed L.Value)
      (inputArgs, initBlock, postInitArgs) =
        mkInitBlock archOps (fnType f) (blockName (mkRootBlockLabel (fnAddr f)))

  let ctx :: FunLLVMContext arch
      ctx = FunLLVMContext { funSymbolCallback = symFun
                           , funAddrTypeMap = funTypeMap
                           , funArgs        = postInitArgs
                           , funReturnType  =
                             L.Struct (viewSome typeToLLVMType <$> fnReturnTypes (fnType f))
                           }

  -- Create ordinary blocks
  m0 <- get
  let init_fun_state :: FunState arch
      init_fun_state = FunState { nmCounter = 0
                                , funAssignValMap = Map.empty
                                , funIntrinsicMap = m0
                                }
  let mkBlockRes :: (FunState arch, [LLVMBlockResult arch])
                 -> FnBlock arch
                 -> (FunState arch, [LLVMBlockResult arch])
      mkBlockRes (fs, prev) b =
        let (r, fs') = blockToLLVM archOps ctx fs b
         in (fs', r:prev)

  let block_results :: [LLVMBlockResult arch]
      (fin_fs, block_results) = foldl mkBlockRes (init_fun_state, []) (fnBlocks f)

  -- Update intrins map
  put (funIntrinsicMap fin_fs)

  let resolvePhiMap :: ResolvePhiMap arch
      resolvePhiMap = Map.fromList
        [ (fbLabel (regBlock b), b)
        | b <- block_results
        ]

  let blocks :: [L.BasicBlock]
      blocks = toBasicBlock ctx (funAssignValMap fin_fs) resolvePhiMap <$> reverse block_results
  pure $
    L.Define { L.defLinkage  = Nothing
             , L.defRetType  =
                 L.Struct (viewSome typeToLLVMType <$> fnReturnTypes (fnType f))
             , L.defName     = symbol
             , L.defArgs     = inputArgs
             , L.defVarArgs  = False
             , L.defAttrs    = []
             , L.defSection  = Nothing
             , L.defGC       = Nothing
             , L.defBody     = [initBlock] ++ blocks ++ [failBlock]
             , L.defMetadata = Map.empty
             , L.defComdat   = Nothing
             }

------------------------------------------------------------------------
-- Other

declareIntrinsic :: Intrinsic -> L.Declare
declareIntrinsic i =
  L.Declare { L.decRetType = intrinsicRes i
            , L.decName    = intrinsicName i
            , L.decArgs    = intrinsicArgs i
            , L.decVarArgs = False
            , L.decAttrs   = intrinsicAttrs i
            , L.decComdat  = Nothing
            }

-- | Generate LLVM module from a list of functions describing the
-- behavior.
moduleForFunctions :: forall arch
                   .  (LLVMArchConstraints arch
                      , Show (FunctionType arch)
                      , FoldableFC (ArchFn arch)
                      , FoldableF (FnArchStmt arch))
                   => LLVMArchSpecificOps arch
                      -- ^ architecture specific functions
                   -> FunctionSymbolCallback (ArchAddrWidth arch)
                      -- ^ Function for getting name of functions
                   -> [Function arch]
                   -> L.Module
moduleForFunctions archOps symFun fns =
    L.Module { L.modSourceName = Nothing
             , L.modDataLayout = []
             , L.modTypes      = []
             , L.modNamedMd    = []
             , L.modUnnamedMd  = []
             , L.modGlobals    = []
             , L.modDeclares   = fmap declareIntrinsic llvmIntrinsics
                              ++ fmap declareIntrinsic dynIntrinsics
                              ++ fnDecls
             , L.modDefines    = defines
             , L.modInlineAsm  = []
             , L.modAliases    = []
             , L.modComdat     = Map.empty
             }
  where -- Get all function references
        funTypeMap :: FunctionTypeMap arch
        funTypeMap = foldl getReferencedFunctions Map.empty fns

        excludedSet = Set.fromList $ fnAddr <$> fns

        declFunMap =
          [ (addr, tp)
          | (addr, tp) <- Map.toList funTypeMap
          , Set.notMember addr excludedSet
          ]

        fnDecls = (\(addr, ftp) -> declareFunction (symFun addr) ftp)
          <$> declFunMap

        (dynIntrinsics, defines) = runLLVMTrans $
          traverse (defineFunction archOps symFun funTypeMap) fns
