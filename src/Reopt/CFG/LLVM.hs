{-|
Copyright        : (c) Galois, Inc 2015-2018

Functions which convert the types in Representaiton to their analogues
in LLVM
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Reopt.CFG.LLVM
  ( functionName
  , AddrSymMap
  , moduleForFunctions
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BSC
import           Data.Int
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Classes
import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some
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

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.Monad (RepValSize(..), repValSizeByteCount)
import           Data.Macaw.X86.X86Reg

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

------------------------------------------------------------------------
-- libreopt functions

iRead_X87_RC :: Intrinsic
iRead_X87_RC = intrinsic' "reopt.Read_X87_RC" (L.iT 2) [] [L.Readonly]

iWrite_X87_RC :: Intrinsic
iWrite_X87_RC = intrinsic "reopt.Write_X87_RC" L.voidT [L.iT 2]

iRead_X87_PC :: Intrinsic
iRead_X87_PC = intrinsic' "reopt.Read_X87_PC" (L.iT 2) [] [L.Readonly]

iWrite_X87_PC :: Intrinsic
iWrite_X87_PC = intrinsic "reopt.Write_X87_PC" L.voidT [L.iT 2]

iRead_FS :: Intrinsic
iRead_FS = intrinsic' "reopt.Read_FS" (L.iT 16) [] [L.Readonly]

iWrite_FS :: Intrinsic
iWrite_FS = intrinsic "reopt.Write_FS" L.voidT [L.iT 16]

iRead_GS :: Intrinsic
iRead_GS = intrinsic' "reopt.Read_GS" (L.iT 16) [] [L.Readonly]

iWrite_GS :: Intrinsic
iWrite_GS = intrinsic "reopt.Write_GS" L.voidT [L.iT 16]

iMemCopy :: Integer -> Intrinsic
iMemCopy n = intrinsic ("reopt.MemCopy.i" ++ show n) L.voidT [ L.iT 64, L.iT 64, L.iT 64, L.iT 1]

iMemSet :: L.Type -> Intrinsic
iMemSet typ = intrinsic ("reopt.MemSet." ++ show (L.ppType typ)) L.voidT args
  where args = [L.ptrT typ, typ, L.iT 64, L.iT 1]

iMemCmp :: Intrinsic
iMemCmp = intrinsic' "reopt.MemCmp" (L.iT 64) [L.iT 64, L.iT 64, L.iT 64, L.iT 64, L.iT 1] [L.Readonly]


reoptIntrinsics :: [Intrinsic]
reoptIntrinsics = [ iRead_X87_RC
                  , iWrite_X87_RC
                  , iRead_X87_PC
                  , iWrite_X87_PC
                  , iRead_FS
                  , iWrite_FS
                  , iRead_GS
                  , iWrite_GS
                  , iMemCmp
                  ]
                  ++ [ iMemCopy n       | n <- [8, 16, 32, 64] ]
                  ++ [ iMemSet (L.iT n) | n <- [8, 16, 32, 64] ]

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
     args = [ L.Vector n (L.PtrTo tpv), L.iT 32, L.Vector n (L.iT 1), L.Vector n tpv ]
  in intrinsic mnem tpv args

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

-- | Return the LLVM symbol associated with the given name
functionName :: AddrSymMap 64
                -- ^ Maps addresses of symbols to the associated symbol name.
             -> MemSegmentOff 64
             -> L.Symbol
functionName m addr
    | Just nm <- Map.lookup addr m =
        L.Symbol $ "reopt_gen_" ++ BSC.unpack nm
    | otherwise = L.Symbol $ "reopt_gen_" ++ show (segmentBase seg) ++ "_" ++ show off
      where seg = msegSegment addr
            off = segmentOffset seg + msegOffset addr

blockWordName :: MemSegmentOff 64  -> L.Ident
blockWordName addr = L.Ident ("block_" ++ show (segmentBase seg) ++ "_" ++ show off)
  where seg = msegSegment addr
        off = msegOffset addr

blockName :: MemWidth w => BlockLabel w -> L.BlockLabel
blockName l = L.Named (L.Ident (show l))

-- The type of FP arguments and results.  We actually want fp128, but
-- it looks like llvm (at least as of version 3.6.2) doesn't put fp128
-- into xmm0 on a return, whereas it does for <2 x double>

natReprToLLVMType :: NatRepr n -> L.Type
natReprToLLVMType = L.PrimType . L.Integer . fromIntegral . natValue

typeToLLVMType :: TypeRepr tp -> L.Type
typeToLLVMType BoolTypeRepr   = L.PrimType (L.Integer 1)
typeToLLVMType (BVTypeRepr n) = natReprToLLVMType n
typeToLLVMType (TupleTypeRepr s) = L.Struct (toListFC typeToLLVMType s)

functionFloatType :: L.Type
functionFloatType = L.Vector 2 (L.PrimType $ L.FloatType L.Double)

functionArgType :: Some X86Reg -> L.Type
functionArgType (Some r) =
  case r of
    X86_GP{} -> L.iT 64
    X86_YMMReg{} -> functionFloatType
    _ -> Loc.error "Unsupported function type registers"

functionTypeArgTypes :: FunctionType -> [L.Type]
functionTypeArgTypes ft = functionArgType <$> ftArgRegs ft

functionTypeToLLVM :: FunctionType -> L.Type
functionTypeToLLVM ft = L.ptrT (L.FunTy funReturnType (functionTypeArgTypes ft) False)

funReturnType :: L.Type
funReturnType = L.Struct $ (typeToLLVMType . typeRepr <$> x86ResultRegs)
                            ++ (replicate (length x86FloatResultRegs) functionFloatType)

-- | This is a special label used for e.g. table lookup defaults (where we should never reach).
-- For now it will just loop.
failLabel :: L.Ident
failLabel = L.Ident "failure"

argIdent :: Int -> L.Ident
argIdent i = L.Ident ("arg" ++ show i)

fltbvArg :: Int -> L.Ident
fltbvArg i = L.Ident ("fargbv" ++ show i)

-- | Create init block and post init args
--
-- This function is needed so that we coerce floating point input arguments to the expected types.
mkInitBlock :: FunctionType -- ^ Type of function
            -> L.BlockLabel -- ^ Label of first block
            -> ([L.Typed L.Ident], L.BasicBlock, V.Vector (L.Typed L.Value))
mkInitBlock ft lbl = (inputArgs, blk, postInitArgs)
  where mkInputReg :: L.Type -> Int -> L.Typed L.Ident
        mkInputReg tp i = L.Typed tp (argIdent i)

        inputArgs :: [L.Typed L.Ident]
        inputArgs = zipWith mkInputReg (functionTypeArgTypes ft) [0..]

        -- Block to generate
        blk = L.BasicBlock { L.bbLabel = Just (L.Named (L.Ident "init"))
                           , L.bbStmts = fltStmts ++ [L.Effect (L.Jump lbl) []]
                           }

        intArgs :: V.Vector (L.Typed L.Value)
        intArgs = V.generate (fnNIntArgs ft)   $ \i -> L.Typed (L.iT 64) (L.ValIdent (argIdent i))

        fltbvArgs :: V.Vector (L.Typed L.Value)
        fltbvArgs = V.generate (fnNFloatArgs ft) $ \i -> L.Typed (L.iT 128) (L.ValIdent (fltbvArg i))

        postInitArgs :: V.Vector (L.Typed L.Value)
        postInitArgs = intArgs V.++ fltbvArgs

        fltStmts :: [L.Stmt]
        fltStmts = fltStmt <$> [0..fnNFloatArgs ft-1]
          where fltStmt :: Int -> L.Stmt
                fltStmt i = L.Result (fltbvArg i) (L.Conv L.BitCast arg (L.iT 128)) []
                  where arg = L.Typed functionFloatType (L.ValIdent (argIdent (fnNIntArgs ft + i)))



failBlock :: L.BasicBlock
failBlock = L.BasicBlock { L.bbLabel = Just (L.Named failLabel)
                         , L.bbStmts = [L.Effect L.Unreachable []]
                         }

type FunctionTypeMap = Map (MemSegmentOff 64) FunctionType

getReferencedFunctions :: FunctionTypeMap
                       -> Function
                       -> FunctionTypeMap
getReferencedFunctions m0 f =
    foldFnValue findReferencedFunctions (insertAddr (fnAddr f) (fnType f) m0) f
  where findReferencedFunctions :: FunctionTypeMap
                                -> FnValue X86_64 tp
                                -> FunctionTypeMap
        findReferencedFunctions m (FnFunctionEntryValue ft addr) =
          insertAddr addr ft m
        findReferencedFunctions m _ = m

        insertAddr :: MemSegmentOff 64
                   -> FunctionType
                   -> FunctionTypeMap
                   -> FunctionTypeMap
        insertAddr addr ft m =
          case Map.lookup addr m of
            Just ft' | ft /= ft' ->
                         Loc.error $ show (relativeSegmentAddr addr) ++ " has incompatible types:\n"
                              ++ show ft  ++ "\n"
                              ++ show ft' ++ "\n"
                     | otherwise -> m
            _ -> Map.insert addr ft m

-- Pads the given list of values to be the target lenght using undefs
padUndef :: L.Type -> Int -> [L.Typed L.Value] -> [L.Typed L.Value]
padUndef typ len xs = xs ++ (replicate (len - length xs) (L.Typed typ L.ValUndef))

data FunLLVMContext = FunLLVMContext
  { funSyscallIntrinsicPostfix :: !String
  , funAddrSymMap :: !(AddrSymMap 64)
  , funAddrTypeMap :: !FunctionTypeMap
  , funArgs      :: !(V.Vector (L.Typed L.Value))
  }

-- | Information about a phi node
data PendingPhiNode tp = PendingPhiNode !L.Ident !L.Type [(BlockLabel 64, X86Reg tp)]

-- | Result obtained by printing a block to LLVM
data LLVMBlockResult = LLVMBlockResult { regBlock   :: !FnBlock
                                       , llvmBlockStmts  :: ![L.Stmt]
                                       , llvmBlockPhiVars :: ![Some PendingPhiNode]
                                       }

-- | Map needed to resolve phi references
type ResolvePhiMap = Map (BlockLabel 64) LLVMBlockResult

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
data FunState =
  FunState { nmCounter :: !Int
             -- ^ Counter for generating new identifiers
           , funAssignValMap :: !AssignValMap
             -- ^ Map from function ids already assigned to label
             -- where it was assigned and associated value.
           , funIntrinsicMap :: !IntrinsicMap
           }

funIntrinsicMapLens :: Simple Lens FunState IntrinsicMap
funIntrinsicMapLens = lens funIntrinsicMap (\s v -> s { funIntrinsicMap = v })

------------------------------------------------------------------------
-- BBLLVM

-- | State specific to translating a single basic block.
data BBLLVMState = BBLLVMState
  { funContext :: !FunLLVMContext
    -- ^ Context for function level declarations.
  , bbBlock :: !FnBlock
    -- ^ Basic block that we are generating the LLVM for.
  , bbStmts :: ![L.Stmt]
    -- ^ Statements in reverse order
  , funState :: !FunState
   -- ^ State local to function rather than block
  , bbBoundPhiVars :: ![Some PendingPhiNode]
    -- ^ Identifiers that we need to generate the phi node information for.
  }

funStateLens :: Simple Lens BBLLVMState FunState
funStateLens = lens funState (\s v -> s { funState = v })

newtype BBLLVM a = BBLLVM { unBBLLVM :: State BBLLVMState a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState BBLLVMState
           )

-- | Generate a fresh identifier with the name 'rX'
freshName :: BBLLVM L.Ident
freshName = do
  s <- get
  let fs = funState s
  let fs' = fs { nmCounter = nmCounter fs + 1 }
  put $! s { funState = fs' }
  pure $! L.Ident ('r' : show (nmCounter fs))

-- | Append a statement to the list of statements
emitStmt :: L.Stmt -> BBLLVM ()
emitStmt stmt = seq stmt $ do
  s <- get
  put $! s { bbStmts = stmt : bbStmts s }

-- | Evaluate an instruction and return the result.
evalInstr :: L.Instr -> BBLLVM L.Value
evalInstr i = do
  nm <- freshName
  emitStmt $! L.Result nm i []
  pure $! (L.ValIdent nm)

-- | Emit an instruction as an effect
effect :: L.Instr -> BBLLVM ()
effect i = emitStmt $ L.Effect i []

-- | Add a comment instruction
comment :: String -> BBLLVM ()
comment msg = effect (L.Comment msg)

ret :: L.Typed L.Value -> BBLLVM ()
ret = effect . L.Ret

jump :: L.BlockLabel -> BBLLVM ()
jump = effect . L.Jump

unimplementedInstr' :: L.Type -> String -> BBLLVM (L.Typed L.Value)
unimplementedInstr' typ reason = do
  comment ("UNIMPLEMENTED: " ++ reason)
  return (L.Typed typ L.ValUndef)

-- | Map from assign ID to value.
type AssignValMap = Map FnAssignId (BlockLabel 64, L.Typed L.Value)

setAssignIdValue :: FnAssignId -> BlockLabel 64 -> L.Typed L.Value -> BBLLVM ()
setAssignIdValue fid blk v = do
  s <- get
  let fs = funState s
  case Map.lookup fid (funAssignValMap fs) of
    Just{} -> Loc.error $ "internal: Assign id " ++ show fid ++ " already assigned."
    Nothing -> do
      let fs' = fs { funAssignValMap = Map.insert fid (blk,v) (funAssignValMap fs) }
      put $! s { funState = fs' }

addBoundPhiVar :: L.Ident -> L.Type -> [(BlockLabel 64, X86Reg tp)] -> BBLLVM ()
addBoundPhiVar nm tp info = do
  s <- get
  let pair = Some $ PendingPhiNode nm tp info
  seq pair $ put $! s { bbBoundPhiVars = pair : bbBoundPhiVars s }

------------------------------------------------------------------------
-- Convert a value to LLVM

-- | Map a function value to a LLVM value with no change.
valueToLLVM :: Loc.HasCallStack
            => FunLLVMContext
            -> FnBlock
               -- ^ Block we are generating LLVM for.
            -> AssignValMap
            -> FnValue X86_64 tp
            -> L.Typed L.Value
valueToLLVM ctx blk m val = do
  let typ = typeToLLVMType $ typeRepr val
  let  mk :: L.Value -> L.Typed L.Value
       mk  = L.Typed typ
  case val of
    FnValueUnsupported _reason _ -> mk L.ValUndef
    -- A value that is actually undefined, like a non-argument register at
    -- the start of a function.
    FnUndefined _ -> mk L.ValUndef
    FnConstantBool b -> mk $ L.integer (if b then 1 else 0)
    FnConstantValue _sz n -> mk $ L.integer n
    -- Value from an assignment statement.
    FnAssignedValue (FnAssignment lhs _rhs) ->
      case Map.lookup lhs m of
        Just (_,v) -> v
        Nothing ->
          Loc.error $ "Could not find assignment value " ++ show lhs ++ "\n"
                      ++ show (pretty blk)
    -- Value from a phi node
    FnPhiValue (FnPhiVar lhs _tp)  -> do
      case Map.lookup lhs m of
        Just (_,v) -> v
        Nothing ->
          Loc.error $ "Could not find phi value " ++ show lhs ++ "\n"
                      ++ show (pretty blk)
    -- A value returned by a function call (rax/xmm0)
    FnReturn (FnReturnVar lhs _tp) ->
      case Map.lookup lhs m of
        Just (_,v) -> v
        Nothing ->
          Loc.error $ "Could not find return variable " ++ show lhs ++ "\n"
                      ++ show (pretty blk)
    -- The entry pointer to a function.  We do the cast as a const
    -- expr as function addresses appear as constants in e.g. phi
    -- nodes
    FnFunctionEntryValue ft addr -> do
      case Map.lookup addr (funAddrTypeMap ctx) of
        Just tp
          | ft /= tp -> Loc.error "Mismatch function type"
          | otherwise -> do
            let sym = functionName (funAddrSymMap ctx) addr
            let fptr :: L.Typed L.Value
                fptr = L.Typed (functionTypeToLLVM ft) (L.ValSymbol sym)
            mk $ L.ValConstExpr (L.ConstConv L.PtrToInt fptr typ)
        Nothing -> do
          Loc.error $ "Could not identify " ++ show addr
    -- A pointer to an internal block at the given address.
    FnBlockValue addr ->
      mk $ L.ValLabel $ L.Named $ blockWordName addr
    -- Value is an argument passed via a register.
    FnRegArg _ i -> r
      where Just r = funArgs ctx V.!? i
    -- A global address
    FnGlobalDataAddr addr ->
      case msegAddr addr of
        Nothing -> Loc.error $ "FnGlobalDataAddr only supports global values."
        Just fixedAddr -> mk $ L.integer $ fromIntegral fixedAddr

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
valueToLLVMBitvec :: Loc.HasCallStack
                  => FunLLVMContext
                  -> FnBlock
                     -- ^ Block that we are generating LLVM for.
                  -> AssignValMap
                  -> FnValue X86_64 tp
                  -> L.Typed L.Value
valueToLLVMBitvec ctx blk m val = do
  let llvm_val = valueToLLVM ctx blk m val
  case L.typedType llvm_val of
    L.PrimType (L.Integer _) -> llvm_val
    L.PrimType (L.FloatType tp) ->
      let itp = L.iT (floatTypeWidth tp)
       in L.Typed itp $ L.ValConstExpr $ L.ConstConv L.BitCast llvm_val itp
    _ -> Loc.error $ "valueToLLVMBitvec given unsupported type."

mkLLVMValue :: Loc.HasCallStack => FnValue X86_64 tp -> BBLLVM (L.Typed L.Value)
mkLLVMValue val = do
  s <- get
  let fs = funState s
  pure $! valueToLLVMBitvec (funContext s) (bbBlock s) (funAssignValMap fs) val

arithop :: L.ArithOp -> L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
arithop f val s = do
  L.Typed (L.typedType val) <$> evalInstr (L.Arith f val s)

bitop :: L.BitOp -> L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
bitop f val s = do
  L.Typed (L.typedType val) <$> evalInstr (L.Bit f val s)

-- | Conversion operation
convop :: L.ConvOp -> L.Typed L.Value -> L.Type -> BBLLVM (L.Typed L.Value)
convop f val tp = do
  L.Typed tp <$> evalInstr (L.Conv f val tp)

{-
fcmp :: L.FCmpOp -> L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
fcmp f val s = do
  L.Typed (L.typedType val) <$> evalInstr (L.FCmp f val s)
-}

-- | Compare two LLVM values using the given operator.
icmpop :: L.ICmpOp -> L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
icmpop f val s = do
  L.Typed (L.iT 1) <$> evalInstr (L.ICmp f val s)

-- | Extract a value from a struct
extractValue :: L.Typed L.Value -> Int32 -> BBLLVM (L.Typed L.Value)
extractValue ta i = do
  let etp = case L.typedType ta of
              L.Struct fl -> fl !! fromIntegral i
              L.Array _l etp' -> etp'
              _ -> Loc.error "extractValue not given a struct or array."
  L.Typed etp <$> evalInstr (L.ExtractValue ta [i])

insertValue :: L.Typed L.Value -> L.Typed L.Value -> Int32 -> BBLLVM (L.Typed L.Value)
insertValue ta tv i =
  L.Typed (L.typedType ta) <$> evalInstr (L.InsertValue ta tv [i])

-- | Do a bitcast
bitcast :: L.Typed L.Value -> L.Type -> BBLLVM (L.Typed L.Value)
bitcast = convop L.BitCast

-- | Unsigned extension
zext :: L.Typed L.Value -> L.Type -> BBLLVM (L.Typed L.Value)
zext = convop L.ZExt


band :: L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
band = bitop L.And

bor :: L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
bor = bitop L.Or

bxor :: L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
bxor = bitop L.Xor

shl :: L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
shl = bitop (L.Shl False False)

ashr :: L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
ashr = bitop (L.Ashr False)

lshr :: L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
lshr = bitop (L.Lshr False)

alloca :: L.Type -> Maybe (L.Typed L.Value) -> Maybe Int -> BBLLVM (L.Typed L.Value)
alloca tp cnt align = fmap (L.Typed (L.PtrTo tp)) $ evalInstr $ L.Alloca tp cnt align

-- | Generate a non-tail call that returns a value
call :: HasValue v => v -> [L.Typed L.Value] -> BBLLVM (L.Typed L.Value)
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
call_ :: HasValue v => v -> [L.Typed L.Value] -> BBLLVM ()
call_ (valueOf -> f) args =
  case L.typedType f of
    L.PtrTo (L.FunTy (L.PrimType L.Void) argTypes varArgs) -> do
      when varArgs $ do
        Loc.error $ "Varargs not yet supported."
      when (argTypes /= fmap L.typedType args) $ do
        Loc.error $ "Unexpected arguments to " ++ show f
      effect $ L.Call False (L.typedType f) (L.typedValue f) args
    _ -> Loc.error $ "call_ given non-function pointer argument\n" ++ show f

-- | Handle an intrinsic overflows
intrinsicOverflows' :: (1 <= w)
                    => String
                    -> FnValue X86_64 (BVType w)
                    -> FnValue X86_64 (BVType w)
                    -> FnValue X86_64 BoolType
                    -> BBLLVM (L.Typed L.Value)
-- Special case where carry/borrow flag is 0.
intrinsicOverflows' bop x y (FnConstantBool False) = do
  x' <- mkLLVMValue x
  y' <- mkLLVMValue y
  let in_typ = L.typedType x'
  r_tuple    <- call (overflowOp bop in_typ) [x', y']
  extractValue r_tuple 1
-- General case involves two calls
intrinsicOverflows' bop x y c = do
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

carryValue :: (1 <= w) => NatRepr w -> FnValue X86_64 BoolType -> BBLLVM (L.Typed L.Value)
carryValue w x =
  (`zext` natReprToLLVMType w) =<< mkLLVMValue x

data AsmAttrs = AsmAttrs { asmSideeffect :: !Bool }

noSideEffect :: AsmAttrs
noSideEffect = AsmAttrs { asmSideeffect = False }

sideeffect :: AsmAttrs
sideeffect = AsmAttrs { asmSideeffect = True }

-- | Call some inline assembly
callAsm :: AsmAttrs -- ^ Asm attrs
        -> L.Type   -- ^ Return type
        -> String   -- ^ Code
        -> String   -- ^ Args code
        -> [L.Typed L.Value] -- ^ Arguments
        -> BBLLVM (L.Typed L.Value)
callAsm attrs resType asmCode asmArgs args = do
  let argTypes = L.typedType <$> args
      ftp = L.PtrTo (L.FunTy resType argTypes False)
      f = L.ValAsm (asmSideeffect attrs) False asmCode asmArgs
  L.Typed resType <$> evalInstr (L.Call False ftp f args)

-- | Generate a system call on Linux
-- TODO: Check registers also work for FreeBSD
emitSyscall :: L.Typed L.Value
            -> [L.Typed L.Value] -- ^ Arguments
            -> BBLLVM (L.Typed L.Value)
emitSyscall callNum args =
  callAsm sideeffect
              (L.iT 64)
              "syscall"
              -- This string marks rax as an output.
              -- It also marks rax, rdi, rsi, rdx, rcx, r8, r9 as inputs.
              -- It indicates that the function can make arbitrary
              -- modifications to memory, flags, rcx, and r11.
              "={rax},{rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},~{memory},~{flags},~{rcx},~{r11}"
              (callNum : padUndef (L.iT 64) 6 args)

-- | Generate the LLVM for checking parity of an 8-bit value is even.
evenParity :: L.Typed L.Value -> BBLLVM (L.Typed L.Value)
evenParity v = do
  -- This code calls takes the disjunction of the value with itself to update flags,
  -- then pushes 16-bit flags register to the stack, then pops it to a register.
  res <- callAsm noSideEffect (L.iT 16) "orb $1, $1\0Apushfw\0Apopw $0\0A" "=r,r" [v]
  -- Check parity flag
  parity_val <- band res (L.ValInteger 4)
  -- Check result is nonzero
  icmpop L.Ine parity_val (L.ValInteger 0)

appToLLVM' :: App (FnValue X86_64) tp -> BBLLVM (L.Typed L.Value)
appToLLVM' app = do
  let typ = typeToLLVMType $ typeRepr app
  let binop :: (L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value))
            -> FnValue X86_64 tp
            -> FnValue X86_64 tp
            -> BBLLVM (L.Typed L.Value)
      binop f x y = do
        x' <- mkLLVMValue x
        y' <- mkLLVMValue y
        f x' (L.typedValue y')
{-
    -- A Value that expects to FP bitvectors
  let fpbinop :: (L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value))
              -> FloatInfoRepr flt
              -> FnValue X86_64 (FloatType flt)
              -> FnValue X86_64 (FloatType flt)
              -> BBLLVM (L.Typed L.Value)
      fpbinop f frepr x y = do
        flt_x <- mkFloatLLVMValue x frepr
        flt_y <- mkFloatLLVMValue y frepr
        flt_z <- f flt_x (L.typedValue flt_y)
        bitcast flt_z (natReprToLLVMType (floatInfoBits frepr))
-}
  case app of
    Mux _tp c t f -> do
      l_c <- mkLLVMValue c
      l_t <- mkLLVMValue t
      l_f <- mkLLVMValue f
      fmap (L.Typed (L.typedType l_t)) $ evalInstr $ L.Select l_c l_t (L.typedValue l_f)
    Trunc v sz -> flip (convop L.Trunc) (natReprToLLVMType sz) =<< mkLLVMValue v
    SExt v sz  -> flip (convop L.SExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    UExt v sz  -> flip (convop L.ZExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    AndApp{}     -> unimplementedInstr' typ "AndApp"
    OrApp{}      -> unimplementedInstr' typ "OrApp"
    NotApp{}     -> unimplementedInstr' typ "NotApp"
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
    XorApp x y    -> binop bxor x y
    BVXor _sz x y -> binop bxor x y
    BVShl _sz x y -> binop shl  x y
    BVSar _sz x y -> binop ashr x y
    BVShr _sz x y -> binop lshr x y
    Eq x y ->  binop (icmpop L.Ieq) x y
    PopCount w v  -> do
      v' <- mkLLVMValue v
      let wv = natValue w
      when (wv `notElem` [16, 32, 64]) $ do
        fail $ "Only support popcount of 16, 32, or 64 bits"
      callAsm noSideEffect (L.iT (fromInteger wv)) "popcnt $0, $1" "=r,r" [v']

    ReverseBytes{} -> unimplementedInstr' typ "ReverseBytes"
    -- FIXME: do something more efficient?
    -- Basically does let (r, over)  = llvm.add.with.overflow(x,y)
    --                    (_, over') = llvm.add.with.overflow(r,c)
    --                in over'
    -- and we rely on llvm optimisations to throw away identical adds
    -- and adds of 0
    UadcOverflows x y c -> intrinsicOverflows' "uadd" x y c
    SadcOverflows x y c -> intrinsicOverflows' "sadd" x y c
    UsbbOverflows x y c -> intrinsicOverflows' "usub" x y c
    SsbbOverflows x y c -> intrinsicOverflows' "ssub" x y c

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
llvmAsPtr :: FnValue X86_64 (BVType 64)
             -- ^ Value to evaluate
          -> L.Type
             -- ^ Type of value pointed to
          -> BBLLVM (L.Typed L.Value)
llvmAsPtr ptr tp = do
  llvmPtrAsBV  <- mkLLVMValue ptr
  convop L.IntToPtr llvmPtrAsBV (L.PtrTo tp)

archFnToLLVM :: ArchFn X86_64 (FnValue X86_64) tp -> BBLLVM (L.Typed L.Value)
archFnToLLVM f =
  case f of
   EvenParity v -> do
     evenParity =<< mkLLVMValue v
   -- The x86 documentation for @idiv@ (Intel x86 manual volume 2A,
   -- page 3-393) says that results should be rounded towards
   -- zero. These operations are called @quot@ and @rem@ in Haskell,
   -- whereas @div@ and @mod@ in Haskell round towards negative
   -- infinity. The LLVM @srem@ and @sdiv@ also round towards negative
   -- infinity, and so are the correct operations to use here.  The
   -- LLVM documentation
   -- (http://llvm.org/releases/2.5/docs/LangRef.html) describes the
   -- semantics of @srem@ with "the result has the same sign as the
   -- dividend", which is equivalent to rounding towards zero.
   X86Div repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = L.PrimType $ L.Integer $ fromInteger $ 16 * repValSizeByteCount repr
     llvm_d_ext <- L.typedValue <$> convop L.ZExt llvm_d tp
     arithop (L.UDiv False) llvm_n llvm_d_ext
   X86Rem repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = L.PrimType $ L.Integer $ fromInteger $ 16 * repValSizeByteCount repr
     llvm_d_ext <- L.typedValue <$> convop L.ZExt llvm_d tp
     arithop L.URem llvm_n llvm_d_ext
   X86IDiv repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = L.PrimType $ L.Integer $ fromInteger $ 16 * repValSizeByteCount repr
     llvm_d_ext <- L.typedValue <$> convop L.SExt llvm_d tp
     arithop (L.SDiv False) llvm_n llvm_d_ext
   X86IRem repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = L.PrimType $ L.Integer $ fromInteger $ 16 * repValSizeByteCount repr
     llvm_d_ext <- L.typedValue <$> convop L.SExt llvm_d tp
     arithop L.SRem llvm_n llvm_d_ext
   RepnzScas sz val base cnt -> do
     -- Value to search for.
     llvm_val <- mkLLVMValue val
     -- Convert buffer to LLVM
     let w = L.iT (8 * fromInteger (repValSizeByteCount sz))
     llvm_ptr <- llvmAsPtr base w
     -- Get count
     llvm_cnt <- mkLLVMValue cnt
     let reg = case sz of
                 ByteRepVal  -> "%al"
                 WordRepVal  -> "%ax"
                 DWordRepVal -> "%eax"
                 QWordRepVal -> "%rax"
     -- Call asm
     res <- callAsm noSideEffect
                    (L.Struct [L.iT 64, L.PtrTo w])
                    ("repnz scas %es:(%rdi)," ++ reg)
                    "={cx},={di},{ax},{cx},1,~{flags}"
                    [llvm_val, llvm_cnt, llvm_ptr]
     -- Get first result
     extractValue res 0
   _ -> do
     error $ "LLVM backend does not yet support: " ++ show (runIdentity (ppArchFn (pure . pretty) f))

addIntrinsic :: Intrinsic -> BBLLVM ()
addIntrinsic i = do
  m <- use $ funStateLens . funIntrinsicMapLens
  when (Map.notMember (intrinsicName i) m) $ do
    funStateLens . funIntrinsicMapLens .= Map.insert (intrinsicName i) i m

-- | Create a singleton vector from a value.
singletonVector :: L.Typed L.Value -> BBLLVM (L.Typed L.Value)
singletonVector val = do
  let eltType = L.typedType val
  let arrayType = L.Vector 1 eltType
  let vec0 = L.Typed arrayType L.ValUndef
  L.Typed arrayType
    <$> evalInstr (L.InsertElt vec0 val (L.ValInteger 0))

rhsToLLVM :: FnAssignRhs X86_64 tp -> BBLLVM (L.Typed L.Value)
rhsToLLVM rhs = do
  case rhs of
    FnEvalApp app ->
      appToLLVM' app
    FnSetUndefined tp -> do
      let typ = typeToLLVMType tp
      return (L.Typed typ L.ValUndef)
    FnReadMem ptr typ -> do
      let llvm_typ = typeToLLVMType typ
      p <- llvmAsPtr ptr llvm_typ
      fmap (L.Typed llvm_typ) $ evalInstr (L.Load p Nothing Nothing)
    FnCondReadMem (BVMemRepr w LittleEndian) cond addr passthru -> do
      let bvw = 8 * fromIntegral (natValue w)
      let eltType = L.iT bvw
      let intr = llvmMaskedLoad 1 ("i" ++ show bvw) eltType
      addIntrinsic intr
      llvmAddr     <- singletonVector =<< llvmAsPtr addr eltType
      let llvmAlign = L.Typed (L.iT 32) (L.ValInteger 0)
      llvmCond     <- singletonVector =<< mkLLVMValue cond
      llvmPassthru <- singletonVector =<< mkLLVMValue passthru
      call intr [ llvmAddr, llvmAlign, llvmCond, llvmPassthru ]
    FnCondReadMem (BVMemRepr _ BigEndian) _cond _addr _passthru -> do
      error "LLVM backend does not yet support big endian memory reads."
    FnAlloca v -> do
      v' <- mkLLVMValue v
      alloc_ptr <- alloca (L.iT 8) (Just v') Nothing
      convop L.PtrToInt alloc_ptr (L.iT 64)
    FnEvalArchFn f -> archFnToLLVM f

archStmtToLLVM :: X86Stmt (FnValue X86_64) -> BBLLVM ()
archStmtToLLVM stmt =
  case stmt of
    MemCopy bytesPerCopy nValues src dest direction -> do
      nValues' <- mkLLVMValue nValues
      src'     <- mkLLVMValue src
      dest'    <- mkLLVMValue dest
      direction' <- mkLLVMValue direction
      call_ (iMemCopy (bytesPerCopy * 8)) [dest', src', nValues', direction']
    MemSet count v ptr dir -> do
      let typ = typeToLLVMType (typeRepr v)
      ptr'   <- llvmAsPtr ptr typ
      v'     <- mkLLVMValue v
      count' <- mkLLVMValue count
      df'    <- mkLLVMValue dir
      call_ (iMemSet typ) [ptr', v', count', df']
    _ -> error $ "LLVM generation: Unsupported architecture statement."

stmtToLLVM :: FnStmt -> BBLLVM ()
stmtToLLVM stmt = do
  comment (show $ pretty stmt)
  case stmt of
   FnAssignStmt (FnAssignment lhs rhs) -> do
     lbl <- gets $ fbLabel . bbBlock
     llvm_rhs <- rhsToLLVM rhs
     setAssignIdValue lhs lbl llvm_rhs
   FnWriteMem ptr v -> do
     llvm_v <- mkLLVMValue v
     llvm_ptr <- llvmAsPtr ptr (L.typedType llvm_v)
     -- Cast LLVM point to appropriate type
     effect $ L.Store llvm_v llvm_ptr Nothing
   FnComment _ -> return ()
   FnArchStmt astmt -> do
     archStmtToLLVM astmt

makeRet' :: [ L.Typed L.Value ] -> [ L.Typed L.Value ] -> BBLLVM ()
makeRet' grets frets = do
  -- clang constructs something like
  -- %3 = insertvalue { i64, i64 } undef, i64 %1, 0
  -- %4 = insertvalue { i64, i64 } %3, i64 %2, 1
  -- ret { i64, i64 } %4
  -- which we will duplicate, with undef padding where required.

  -- cast fp results to the required type
  cfrets <- mapM (`bitcast` functionFloatType) frets
  let frets' = padUndef functionFloatType (length x86FloatResultRegs) cfrets
  -- construct the return result struct
  let initUndef = L.Typed funReturnType L.ValUndef
  let grets'    = padUndef (L.iT 64) (length x86ResultRegs) grets
  v <- ifoldlM (\n acc fld -> insertValue acc fld (fromIntegral n)) initUndef (grets' ++ frets')
  ret v

termStmtToLLVM :: FnTermStmt -> BBLLVM ()
termStmtToLLVM tm =
  case tm of
     FnJump lbl -> do
       effect $ L.Jump (blockName lbl)
     FnRet grets frets -> do
       grets' <- mapM mkLLVMValue grets
       frets' <- mapM mkLLVMValue frets
       makeRet' grets' frets'
     FnBranch cond tlbl flbl -> do
       cond' <- mkLLVMValue cond
       effect $ L.Br cond' (blockName tlbl) (blockName flbl)
     FnCall dest ft args (gretvs, fretvs) contlbl -> do
       let arg_tys = functionTypeArgTypes ft
           ret_tys = funReturnType
           fun_ty  = L.ptrT (L.FunTy ret_tys arg_tys False)

       addrSymMap  <- gets $ funAddrSymMap  . funContext
       addrTypeMap <- gets $ funAddrTypeMap . funContext
       dest_f <-
         case dest of
           -- FIXME: use ft here instead?
           FnFunctionEntryValue dest_ftp addr
             | Just tp <- Map.lookup addr addrTypeMap -> do
               let sym = functionName addrSymMap addr
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

       let evalArg :: Some X86Reg -> Some (FnValue X86_64) -> BBLLVM (L.Typed L.Value)
           evalArg (Some (X86_GP _)) (Some v) = mkLLVMValue v
           evalArg (Some (X86_YMMReg _)) (Some v) = (`bitcast` functionFloatType) =<< mkLLVMValue v
           evalArg _ _ = Loc.error "Unsupported register arg"

       args' <- zipWithM evalArg (ftArgRegs ft) args



       retv <- call dest_f args'

       case contlbl of
         Nothing -> do
           ret retv
         Just target_lbl -> do
           this_lbl <- gets $ fbLabel . bbBlock
           -- Assign all return variables to the extracted result
           let assignIntReturn :: Int -> FnReturnVar (BVType 64) -> BBLLVM ()
               assignIntReturn i fr = do
                 val <- extractValue retv (fromIntegral i)
                 setAssignIdValue (frAssignId fr) this_lbl val
           itraverse_ assignIntReturn gretvs
           let fpBase = 2 -- length (gretvs)
           -- Assign floating point results
           let assignFltReturn :: Int -> FnReturnVar (BVType 128) -> BBLLVM ()
               assignFltReturn i fr = do
                 val_fp <- extractValue retv (fromIntegral $ fpBase + i)
                 val    <- bitcast val_fp (L.iT 128)
                 setAssignIdValue (frAssignId fr) this_lbl val
           itraverse_ assignFltReturn fretvs
           jump (blockName target_lbl)
     FnSystemCall call_num args rets next_lbl -> do
       pname <- gets $ funSyscallIntrinsicPostfix . funContext
       llvm_call_num <- mkLLVMValue call_num
       llvm_args  <- mapM mkLLVMValue args
       case pname of
         "Linux" -> do
           rvar <- emitSyscall llvm_call_num llvm_args
           case rets of
             [Some fr] -> do
               -- Assign all return variables to the extracted result
                 lbl <- gets $ fbLabel . bbBlock
                 setAssignIdValue (frAssignId fr) lbl rvar
                 jump (blockName next_lbl)
             _ -> error "Unexpected return values"
         "FreeBSD" -> do
           rvar <- emitSyscall llvm_call_num llvm_args
           case rets of
             [Some fr] -> do
               -- Assign all return variables to the extracted result
                 lbl <- gets $ fbLabel . bbBlock
                 setAssignIdValue (frAssignId fr) lbl rvar
                 jump (blockName next_lbl)
             _ -> error "Unexpected return values"
         _ -> error $ "Unsupported operating system: " ++ show pname

     FnLookupTable idx vec -> do
       idx' <- mkLLVMValue idx
       let dests = map (L.Named . blockWordName) $ V.toList vec
       effect $ L.Switch idx' (L.Named failLabel) (zip [0..] dests)

     FnTermStmtUndefined ->
       void $ unimplementedInstr' L.voidT "FnTermStmtUndefined"

-- | Convert a Phi node assignment to the right value
resolvePhiNodeReg :: Loc.HasCallStack
                  => FunLLVMContext
                  -> AssignValMap
                  -> ResolvePhiMap
                  -> (BlockLabel 64, X86Reg tp)
                  -> (L.Value, L.BlockLabel)
resolvePhiNodeReg ctx avmap m (lbl, reg) =
  case Map.lookup lbl m of
    Nothing -> Loc.error $ "Could not resolve block " ++ show lbl
    Just br -> do
      let b = regBlock br
      case MapF.lookup reg (fbRegMap b) of
        Nothing -> Loc.error $ "Could not resolve register " ++ show reg ++ " in block " ++ show lbl
        Just (CalleeSaved _) -> Loc.error $ "Resolve callee saved register"
        Just (FnRegValue v) -> (L.typedValue (valueToLLVMBitvec ctx b avmap v), blockName lbl)

resolvePhiStmt :: Loc.HasCallStack
               => FunLLVMContext
               -> AssignValMap
               -> ResolvePhiMap
               -> Some PendingPhiNode
               -> L.Stmt
resolvePhiStmt ctx avmap m (Some (PendingPhiNode nm tp info)) = L.Result nm i []
  where i = L.Phi tp (resolvePhiNodeReg ctx avmap m <$> info)

toBasicBlock :: Loc.HasCallStack
             => FunLLVMContext
             -> AssignValMap
             -> ResolvePhiMap
             -> LLVMBlockResult
             -> L.BasicBlock
toBasicBlock ctx avmap m res = L.BasicBlock { L.bbLabel = Just (blockName (fbLabel (regBlock res)))
                                            , L.bbStmts = phiStmts ++ llvmBlockStmts res
                                            }
  where phiStmts = resolvePhiStmt ctx avmap m <$> llvmBlockPhiVars res


-- | Add a phi var with the node info so that we have a ident to reference
-- it by and queue up work to assign the value later.
addPhiBinding :: Some (PhiBinding X86Reg) -> BBLLVM ()
addPhiBinding (Some (PhiBinding (FnPhiVar fid tp) info)) = do
  lbl <- gets $ fbLabel . bbBlock
  nm <- freshName
  let llvm_tp = typeToLLVMType tp
  setAssignIdValue fid lbl (L.Typed llvm_tp (L.ValIdent nm))
  addBoundPhiVar nm llvm_tp info

blockToLLVM :: FunLLVMContext
               -- ^ Context for block
            -> FunState
            -> FnBlock
               -- ^ Block to generate
            -> (LLVMBlockResult, FunState)
blockToLLVM ctx fs b = (res, funState s)
  where s0 = BBLLVMState { funContext     = ctx
                         , bbBlock        = b
                         , bbStmts        = []
                         , funState       = fs
                         , bbBoundPhiVars = []
                         }
        go :: BBLLVM ()
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
defineFunction :: String
                  -- ^ Name to append to system call
               -> AddrSymMap 64
               -> FunctionTypeMap
               -> Function
               -> LLVMTrans L.Define
defineFunction syscallPostfix addrSymMap funTypeMap f = do
  let symbol = functionName addrSymMap (fnAddr f)

  -- Create initial block
  let inputArgs :: [L.Typed L.Ident]
      initBlock :: L.BasicBlock
      postInitArgs :: V.Vector (L.Typed L.Value)
      (inputArgs, initBlock, postInitArgs) =
        mkInitBlock (fnType f) (blockName (mkRootBlockLabel (fnAddr f)))

  let ctx :: FunLLVMContext
      ctx = FunLLVMContext { funSyscallIntrinsicPostfix = syscallPostfix
                           , funAddrSymMap  = addrSymMap
                           , funAddrTypeMap = funTypeMap
                           , funArgs        = postInitArgs
                           }

  -- Create ordinary blocks
  m0 <- get
  let init_fun_state :: FunState
      init_fun_state = FunState { nmCounter = 0
                                , funAssignValMap = Map.empty
                                , funIntrinsicMap = m0
                                }
  let mkBlockRes :: (FunState, [LLVMBlockResult])
                 -> FnBlock
                 -> (FunState, [LLVMBlockResult])
      mkBlockRes (fs, prev) b =
        let (r, fs') = blockToLLVM ctx fs b
         in (fs', r:prev)

  let block_results :: [LLVMBlockResult]
      (fin_fs, block_results) = foldl mkBlockRes (init_fun_state, []) (fnBlocks f)

  -- Update intrins map
  put (funIntrinsicMap fin_fs)

  let resolvePhiMap :: ResolvePhiMap
      resolvePhiMap = Map.fromList
        [ (fbLabel (regBlock b), b)
        | b <- block_results
        ]

  let blocks :: [L.BasicBlock]
      blocks = toBasicBlock ctx (funAssignValMap fin_fs) resolvePhiMap <$> reverse block_results
  pure $
    L.Define { L.defLinkage  = Nothing
             , L.defRetType  = funReturnType
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

declareIntrinsic :: Intrinsic -> L.Declare
declareIntrinsic i =
  L.Declare { L.decRetType = intrinsicRes i
            , L.decName    = intrinsicName i
            , L.decArgs    = intrinsicArgs i
            , L.decVarArgs = False
            , L.decAttrs   = intrinsicAttrs i
            , L.decComdat  = Nothing
            }

-- | Declare all LLVM and reopt-specific intrinsics
intrinsicDecls :: [L.Declare]
intrinsicDecls = declareIntrinsic <$> (reoptIntrinsics ++ llvmIntrinsics)

declareFunction' :: AddrSymMap 64 -> (MemSegmentOff 64, FunctionType) -> L.Declare
declareFunction' addrSymMap (addr, ftp) =
  L.Declare { L.decRetType = funReturnType
            , L.decName    = functionName addrSymMap addr
            , L.decArgs    = functionTypeArgTypes ftp
            , L.decVarArgs = False
            , L.decAttrs   = []
            , L.decComdat  = Nothing
            }

-- | Get module for functions
moduleForFunctions :: String
                      -- ^ Name to append to system calls
                   -> AddrSymMap 64
                   -> [Function]
                   -> L.Module
moduleForFunctions syscallPostfix addrSymMap fns =
    L.Module { L.modSourceName = Nothing
             , L.modDataLayout = []
             , L.modTypes      = []
             , L.modNamedMd    = []
             , L.modUnnamedMd  = []
             , L.modGlobals    = []
             , L.modDeclares   = intrinsicDecls
                              ++ fmap declareIntrinsic dynIntrinsics
                              ++ fnDecls
             , L.modDefines    = defines
             , L.modInlineAsm  = []
             , L.modAliases    = []
             , L.modComdat     = Map.empty
             }
  where -- Get all function references
        funTypeMap :: FunctionTypeMap
        funTypeMap = foldl getReferencedFunctions Map.empty fns

        excludedSet = Set.fromList $ fnAddr <$> fns

        declFunMap =
          [ (addr, tp)
          | (addr, tp) <- Map.toList funTypeMap
          , Set.notMember addr excludedSet
          ]

        fnDecls = declareFunction' addrSymMap <$> declFunMap

        (dynIntrinsics, defines) = runLLVMTrans $
          traverse (defineFunction syscallPostfix addrSymMap funTypeMap) fns
