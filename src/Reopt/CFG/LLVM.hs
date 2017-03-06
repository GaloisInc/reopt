{-|

Module           : Reopt.CFG.LLVM
Copyright        : (c) Galois, Inc 2015-2016
Maintainer       : Simon Winwood <sjw@galois.com>

Functions which convert the types in Representaiton to their
analogues in LLVM
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reopt.CFG.LLVM
  ( functionName
  , AddrSymMap
  , moduleForFunctions
  ) where

import           Control.Lens
import           Control.Monad
--import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BSC
import           Data.Int
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as L (ppType)
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import qualified Data.Parameterized.Map as MapF
import           Data.Parameterized.Some

import           Data.Macaw.CFG
import           Data.Macaw.Memory
import           Data.Macaw.Types

import           Reopt.CFG.FnRep
import           Reopt.Machine.X86State

import           GHC.Stack

--------------------------------------------------------------------------------
-- reopt runtime
--------------------------------------------------------------------------------

-- FIXME: is False ok here??
intrinsic :: String -> L.Type -> [L.Type] -> L.Typed L.Value
intrinsic name res args =
  (L.ptrT $ L.FunTy res args False) L.-: L.Symbol name

iEvenParity :: L.Typed L.Value
iEvenParity = intrinsic "reopt.EvenParity" (L.iT 1) [L.iT 8]

iRead_X87_RC :: L.Typed L.Value
iRead_X87_RC = intrinsic "reopt.Read_X87_RC" (L.iT 2) []

iWrite_X87_RC :: L.Typed L.Value
iWrite_X87_RC = intrinsic "reopt.Write_X87_RC" L.voidT [L.iT 2]

iRead_X87_PC :: L.Typed L.Value
iRead_X87_PC = intrinsic "reopt.Read_X87_PC" (L.iT 2) []

iWrite_X87_PC :: L.Typed L.Value
iWrite_X87_PC = intrinsic "reopt.Write_X87_PC" L.voidT [L.iT 2]

iRead_FS :: L.Typed L.Value
iRead_FS = intrinsic "reopt.Read_FS" (L.iT 16) []

iWrite_FS :: L.Typed L.Value
iWrite_FS = intrinsic "reopt.Write_FS" L.voidT [L.iT 16]

iRead_GS :: L.Typed L.Value
iRead_GS = intrinsic "reopt.Read_GS" (L.iT 16) []

iWrite_GS :: L.Typed L.Value
iWrite_GS = intrinsic "reopt.Write_GS" L.voidT [L.iT 16]

iMemCopy :: Integer -> L.Typed L.Value
iMemCopy n = intrinsic ("reopt.MemCopy.i" ++ show n) L.voidT [ L.iT 64, L.iT 64, L.iT 64, L.iT 1]

iMemSet :: L.Type -> L.Typed L.Value
iMemSet typ = intrinsic ("reopt.MemSet." ++ show (L.ppType typ)) L.voidT
                         [L.ptrT typ, typ, L.iT 64, L.iT 1]

iMemCmp :: L.Typed L.Value
iMemCmp = intrinsic "reopt.MemCmp" (L.iT 64) [L.iT 64, L.iT 64
                                             , L.iT 64, L.iT 64
                                             , L.iT 1]

-- FIXME: use personalities
iSystemCall :: String -> L.Typed L.Value
iSystemCall pname
     | null pname = error "empty string given to iSystemCall"
     | otherwise = intrinsic ("reopt.SystemCall." ++ pname) (L.Struct [L.iT 64, L.iT 1]) argTypes
  where
    -- the +1 is for the additional syscall no. register, which is
    -- passed via the stack.
    argTypes = replicate (length x86SyscallArgumentRegs + 1) (L.iT 64)

reoptIntrinsics :: [L.Typed L.Value]
reoptIntrinsics = [ iEvenParity
                  , iRead_X87_RC
                  , iWrite_X87_RC
                  , iRead_X87_PC
                  , iWrite_X87_PC
                  , iRead_FS
                  , iWrite_FS
                  , iRead_GS
                  , iWrite_GS
                  , iMemCmp
                  , iSystemCall "Linux"
                  , iSystemCall "FreeBSD"
                  ]
                  ++ [ iMemCopy n       | n <- [8, 16, 32, 64] ]
                  ++ [ iMemSet (L.iT n) | n <- [8, 16, 32, 64] ]


--------------------------------------------------------------------------------
-- LLVM intrinsics
--------------------------------------------------------------------------------

llvmIntrinsics :: [L.Typed L.Value]
llvmIntrinsics = [ intrinsic ("llvm." ++ bop ++ ".with.overflow." ++ show (L.ppType in_typ))
                   (L.Struct [in_typ, L.iT 1]) [in_typ, in_typ]
                   | bop <- [ "uadd", "sadd", "usub", "ssub" ]
                   , in_typ <- map L.iT [4, 8, 16, 32, 64] ]
                 ++
                 [ intrinsic ("llvm." ++ uop ++ "." ++ show (L.ppType typ)) typ [typ, L.iT 1]
                 | uop  <- ["cttz", "ctlz"]
                 , typ <- map L.iT [8, 16, 32, 64] ]

--------------------------------------------------------------------------------
-- conversion to LLVM
--------------------------------------------------------------------------------

-- | Maps code addresses in the LLVM state to the associated symbol name if any.
type AddrSymMap = Map (SegmentedAddr 64) BSC.ByteString

type AddrFunMap = Map (SegmentedAddr 64) (L.Symbol, FunctionType)

-- | Return the LLVM symbol associated with the given name
functionName :: AddrSymMap
                -- ^ Maps addresses of symbols to the associated symbol name.
             -> SegmentedAddr 64
             -> L.Symbol
functionName m addr
    | Just nm <- Map.lookup addr m =
      L.Symbol $ "reopt_gen_" ++ BSC.unpack nm
    | Just base <- segmentBase seg =
      L.Symbol $ "reopt_gen_" ++ show (base + addr^.addrOffset)
    | otherwise =
      L.Symbol $ "reopt_gen_" ++ show (segmentIndex seg) ++ "_" ++ show (addr^.addrOffset)
  where seg = addrSegment addr

blockWordName :: SegmentedAddr 64 -> L.Ident
blockWordName p = L.Ident ("block_" ++ nm)
  where seg = addrSegment p
        offset = p^.addrOffset
        nm = case segmentBase seg of
               Just base -> show (base + offset)
               Nothing -> show (segmentIndex seg) ++ "_" ++ show offset

blockName :: BlockLabel w -> L.BlockLabel
blockName l = L.Named (L.Ident (show l))

-- The type of FP arguments and results.  We actually want fp128, but
-- it looks like llvm (at least as of version 3.6.2) doesn't put fp128
-- into xmm0 on a return, whereas it does for <2 x double>

functionFloatType :: L.Type
functionFloatType = L.Vector 2 (L.PrimType $ L.FloatType L.Double)

functionTypeArgTypes :: FunctionType -> [L.Type]
functionTypeArgTypes ft = replicate (fnNIntArgs ft) (L.iT 64)
                          ++ replicate (fnNFloatArgs ft) functionFloatType

functionTypeToLLVM :: FunctionType -> L.Type
functionTypeToLLVM ft = L.ptrT (L.FunTy funReturnType (functionTypeArgTypes ft) False)

funReturnType :: L.Type
funReturnType = L.Struct $ (map (typeToLLVMType . typeRepr) x86ResultRegs)
                            ++ (replicate (length x86FloatResultRegs) functionFloatType)

floatReprToLLVMFloatType :: FloatInfoRepr flt -> L.FloatType
floatReprToLLVMFloatType fir =
  case fir of
    HalfFloatRepr         -> L.Half
    SingleFloatRepr       -> L.Float
    DoubleFloatRepr       -> L.Double
    QuadFloatRepr         -> L.Fp128
    X86_80FloatRepr       -> L.X86_fp80

floatReprToLLVMType :: FloatInfoRepr flt -> L.Type
floatReprToLLVMType = L.PrimType  . L.FloatType . floatReprToLLVMFloatType

-- | This is a special label used for e.g. table lookup defaults (where we should never reach).
-- For now it will just loop.
failLabel :: L.Ident
failLabel = L.Ident "failure"

fltArg :: Int -> L.Ident
fltArg i = L.Ident ("farg" ++ show i)

fltbvArg :: Int -> L.Ident
fltbvArg i = L.Ident ("fargbv" ++ show i)


mkInitBlock :: L.BlockLabel
            -> Int
            -> L.BasicBlock
mkInitBlock lbl fltCnt =
    L.BasicBlock { L.bbLabel = Just (L.Named (L.Ident "init"))
                 , L.bbStmts = fltStmts
                            ++ [L.Effect (L.Jump lbl) []]
                 }
  where fltStmts = fltStmt <$> [0..fltCnt-1]
        fltStmt :: Int -> L.Stmt
        fltStmt i = L.Result (fltbvArg i) (L.Conv L.BitCast arg (L.iT 128)) []
          where arg = L.Typed functionFloatType (L.ValIdent (fltArg i))

failBlock :: L.BasicBlock
failBlock = L.BasicBlock { L.bbLabel = Just (L.Named failLabel)
                         , L.bbStmts = [L.Effect L.Unreachable []]
                         }

getReferencedFunctions :: Map (SegmentedAddr 64) FunctionType
                       -> Function
                       -> Map (SegmentedAddr 64) FunctionType
getReferencedFunctions m0 f = foldFnValue findReferencedFunctions (insertAddr (fnAddr f) (fnType f) m0) f
  where findReferencedFunctions :: Map (SegmentedAddr 64) FunctionType
                                -> FnValue tp
                                -> Map (SegmentedAddr 64) FunctionType
        findReferencedFunctions m (FnFunctionEntryValue ft addr) = insertAddr addr ft m
        findReferencedFunctions m _ = m

        insertAddr :: SegmentedAddr 64
                   -> FunctionType
                   -> Map (SegmentedAddr 64) FunctionType
                   -> Map (SegmentedAddr 64) FunctionType
        insertAddr addr ft m =
          case Map.lookup addr m of
            Just ft' | ft /= ft' ->
                         error $ show addr ++ " has incompatible types:\n"
                              ++ show ft  ++ "\n"
                              ++ show ft' ++ "\n"
                     | otherwise -> m
            _ -> Map.insert addr ft m

-- Pads the given list of values to be the target lenght using undefs
padUndef :: L.Type -> Int -> [L.Typed L.Value] -> [L.Typed L.Value]
padUndef typ len xs = xs ++ (replicate (len - length xs) (L.Typed typ L.ValUndef))

data FunLLVMContext = FunLLVMContext
  { funSyscallIntrinsicPostfix :: !String
  , funAddrFunMap :: !AddrFunMap
  , funIntArgs   :: V.Vector (L.Typed L.Value)
  , funFloatArgs :: V.Vector (L.Typed L.Value)
  }

-- | Information about a phi node
data PendingPhiNode tp = PendingPhiNode !L.Ident !L.Type [(BlockLabel 64, X86Reg tp)]

-- | Function relevative LLVM State
data FunState = FunState { nmCounter :: !Int
                         -- ^ Counter for generating new identifiers
                         , funAssignValMap :: !AssignValMap
                         -- ^ Map from function ids already assigned to label where it was assigned and associated value.
                         }


data BBLLVMState = BBLLVMState
  { funContext :: !FunLLVMContext
    -- ^ Context for function level declarations.
  , bbBlock :: !FnBlock
    -- ^ Basic block for error reporting purposes.
  , bbStmts :: ![L.Stmt]
    -- ^ Statements in reverse order
  , funState :: !FunState
   -- ^ State local to function rather than block
  , bbBoundPhiVars :: ![Some PendingPhiNode]
    -- ^ Identifiers that we need to generate the phi node information for.
  }


type BBLLVM = State BBLLVMState

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

getAssignIdValue :: HasCallStack
                 => FnBlock
                 -> AssignValMap
                    -- ^ Function id map for block
                 -> FnAssignId
                 -> L.Typed L.Value
getAssignIdValue b m fid = do
  case Map.lookup fid m of
    Nothing ->
      error $ "Could not find value " ++ show fid ++ "\n"
           ++ show m ++ "\n"
           ++ show (pretty b) ++ "\n"
           ++ prettyCallStack callStack
    Just (_,v) -> v

setAssignIdValue :: FnAssignId -> BlockLabel 64 -> L.Typed L.Value -> BBLLVM ()
setAssignIdValue fid blk v = do
  s <- get
  let fs = funState s
  case Map.lookup fid (funAssignValMap fs) of
    Just{} -> error $ "internal: Assign id " ++ show fid ++ " already assigned."
    Nothing -> do
      let fs' = fs { funAssignValMap = Map.insert fid (blk,v) (funAssignValMap fs) }
      put $! s { funState = fs' }

addBoundPhiVar :: L.Ident -> L.Type -> [(BlockLabel 64, X86Reg tp)] -> BBLLVM ()
addBoundPhiVar nm tp info = do
  s <- get
  let pair = Some $ PendingPhiNode nm tp info
  seq pair $ do
  put $! s { bbBoundPhiVars = pair : bbBoundPhiVars s }

-- | Map a function value to a LLVM value with no change.
valueToLLVM :: HasCallStack
            => FunLLVMContext
            -> FnBlock
            -> AssignValMap
            -> FnValue tp
            -> L.Typed L.Value
valueToLLVM ctx blk m val = do
  let typ = typeToLLVMType $ fnValueType val
  let  mk :: L.Value -> L.Typed L.Value
       mk  = L.Typed typ
  case val of
    FnValueUnsupported _reason _ -> mk L.ValUndef
    -- A value that is actually undefined, like a non-argument register at
    -- the start of a function.
    FnUndefined _ -> mk L.ValUndef
    FnConstantValue _sz n -> mk $ L.integer n
    -- Value from an assignment statement.
    FnAssignedValue (FnAssignment lhs _rhs) -> getAssignIdValue blk m lhs
    -- Value from a phi node
    FnPhiValue (FnPhiVar lhs _tp)  -> getAssignIdValue blk m lhs
    -- A value returned by a function call (rax/xmm0)
    FnReturn (FnReturnVar lhs _tp) -> getAssignIdValue blk m lhs
    -- The entry pointer to a function.  We do the cast as a const
    -- expr as function addresses appear as constants in e.g. phi
    -- nodes
    FnFunctionEntryValue ft addr -> do
      case Map.lookup addr (funAddrFunMap ctx) of
        Just (sym,tp)
          | ft /= tp -> error "Mismatch function type"
          | otherwise ->
            let fptr :: L.Typed L.Value
                fptr = L.Typed (functionTypeToLLVM ft) (L.ValSymbol sym)
             in mk $ L.ValConstExpr (L.ConstConv L.PtrToInt fptr typ)
        Nothing -> do
          error $ "Could not identify " ++ show addr
    -- A pointer to an internal block at the given address.
    FnBlockValue addr ->
      mk $ L.ValLabel $ L.Named $ blockWordName addr
    -- Value is an integer argument passed via a register.
    FnIntArg n -> r
      where Just r = funIntArgs ctx V.!? n

    -- Value is a function argument passed via a floating point XMM
    -- register.
    FnFloatArg n -> r
      where Just r = funFloatArgs ctx V.!? n
    -- A global address
    FnGlobalDataAddr addr ->
      case segmentBase (addrSegment addr) of
        Just base -> mk $ L.integer $ fromIntegral $ base + addr^.addrOffset
        Nothing -> error $ "FnGlobalDataAddr only supports global values."

-- | Return number of bits and LLVM float type should take
floatTypeWidth :: L.FloatType -> Int32
floatTypeWidth l =
  case l of
    L.Half -> 16
    L.Float -> 32
    L.Double -> 64
    L.Fp128 -> 128
    L.X86_fp80 -> 80
    L.PPC_fp128 -> error "PPC floating point types not supported."

-- | Map a function value to a LLVM value with no change.
valueToLLVMBitvec :: HasCallStack
                  => FunLLVMContext
                  -> FnBlock
                  -> AssignValMap
                  -> FnValue tp
                  -> L.Typed L.Value
valueToLLVMBitvec ctx blk m val = do
  let llvm_val = valueToLLVM ctx blk m val
  case L.typedType llvm_val of
    L.PrimType (L.Integer _) -> llvm_val
    L.PrimType (L.FloatType tp) ->
      let itp = L.iT (floatTypeWidth tp)
       in L.Typed itp $ L.ValConstExpr $ L.ConstConv L.BitCast llvm_val itp
    _ -> error $ "valueToLLVMBitvec given unsupported type."

mkLLVMValue :: HasCallStack => FnValue tp -> BBLLVM (L.Typed L.Value)
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

fcmp :: L.FCmpOp -> L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value)
fcmp f val s = do
  L.Typed (L.typedType val) <$> evalInstr (L.FCmp f val s)

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
              _ -> error "extractValue not given a struct or array."
  L.Typed etp <$> evalInstr (L.ExtractValue ta [i])

insertValue :: L.Typed L.Value -> L.Typed L.Value -> Int32 -> BBLLVM (L.Typed L.Value)
insertValue ta tv i =
  L.Typed (L.typedType ta) <$> evalInstr (L.InsertValue ta tv [i])

-- | Do a bitcast
bitcast :: L.Typed L.Value -> L.Type -> BBLLVM (L.Typed L.Value)
bitcast = convop L.BitCast

-- | Truncation
trunc :: L.Typed L.Value -> L.Type -> BBLLVM (L.Typed L.Value)
trunc = convop L.Trunc

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
call :: L.Typed L.Value -> [L.Typed L.Value] -> BBLLVM (L.Typed L.Value)
call f args =
  case L.typedType f of
    L.PtrTo (L.FunTy res argTypes varArgs) -> do
      when varArgs $ do
        error $ "Varargs not yet supported."
      when (argTypes /= fmap L.typedType args) $ do
        error $ "Unexpected arguments to " ++ show f
      fmap (L.Typed res) $ evalInstr $ L.Call False (L.typedType f) (L.typedValue f) args
    _ -> error $ "Call given non-function pointer argument:\n" ++ show f

-- | Generate a non-tail call that does not return a value
call_ :: L.Typed L.Value -> [L.Typed L.Value] -> BBLLVM ()
call_ f args =
  case L.typedType f of
    L.FunTy _ argTypes varArgs -> do
      when varArgs $ do
        error $ "Varargs not yet supported."
      when (argTypes /= fmap L.typedType args) $ do
        error $ "Unexpected arguments to " ++ show f
      effect $ L.Call False (L.typedType f) (L.typedValue f) args
    _ -> error "call_ given non-function pointer argument"

mkFloatLLVMValue :: HasCallStack
                 => FnValue (BVType (FloatInfoBits flt))
                 -> FloatInfoRepr flt
                 -> BBLLVM (L.Typed L.Value)
mkFloatLLVMValue val frepr = do
  s <- get
  let llvm_val = valueToLLVM (funContext s) (bbBlock s) (funAssignValMap (funState s)) val
  case L.typedType llvm_val of
    L.PrimType (L.FloatType tp) | tp == floatReprToLLVMFloatType frepr -> pure $ llvm_val
    L.PrimType (L.Integer _) -> bitcast llvm_val (floatReprToLLVMType frepr)
    _ -> error $ "internal: mkFloatLLVMValue given unsupported type."



intrinsicOverflows' :: String -> FnValue tp -> FnValue tp -> FnValue (BVType 1) -> BBLLVM (L.Typed L.Value)
intrinsicOverflows' bop x y c = do
  x' <- mkLLVMValue x
  y' <- mkLLVMValue y
  let in_typ = L.typedType x'
      op_with_overflow =
        intrinsic ("llvm." ++ bop ++ ".with.overflow." ++ show (L.ppType in_typ))
                  (L.Struct [in_typ, L.iT 1]) [in_typ, in_typ]
  c' <- (`zext` in_typ) =<< mkLLVMValue c

  r_tuple    <- call op_with_overflow [x', y']
  r          <- extractValue r_tuple 0
  overflows  <- extractValue r_tuple 1
  r_tuple'   <- call op_with_overflow [r, c']
  overflows' <- extractValue r_tuple' 1
  bor overflows (L.typedValue overflows')

appToLLVM' :: App FnValue tp -> BBLLVM (L.Typed L.Value)
appToLLVM' app = do
  let typ = typeToLLVMType $ appType app
  let binop :: (L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value))
            -> FnValue (BVType n)
            -> FnValue (BVType n)
            -> BBLLVM (L.Typed L.Value)
      binop f x y = do
        x' <- mkLLVMValue x
        y' <- mkLLVMValue y
        f x' (L.typedValue y')
    -- A Value that expects to FP bitvectors
  let fpbinop :: (L.Typed L.Value -> L.Value -> BBLLVM (L.Typed L.Value))
              -> FloatInfoRepr flt
              -> FnValue (FloatType flt)
              -> FnValue (FloatType flt)
              -> BBLLVM (L.Typed L.Value)
      fpbinop f frepr x y = do
        flt_x <- mkFloatLLVMValue x frepr
        flt_y <- mkFloatLLVMValue y frepr
        flt_z <- f flt_x (L.typedValue flt_y)
        bitcast flt_z (natReprToLLVMType (floatInfoBits frepr))
  case app of
    Mux _sz c t f -> do
      l_c <- mkLLVMValue c
      l_t <- mkLLVMValue t
      l_f <- mkLLVMValue f
      when (L.typedType l_t /= L.typedType l_f) $ do
        lbl <- gets $ fbLabel . bbBlock
        error $ "Internal: At " ++ show lbl ++ " expected compatible types to mux:\n"
           ++ "Type1: " ++ show (L.typedType l_t) ++ "Value: " ++ show (pretty t) ++ "\n"
           ++ "Type2: " ++ show (L.typedType l_f) ++ "Value: " ++ show (pretty f) ++ "\n"

      fmap (L.Typed (L.typedType l_t)) $ evalInstr $ L.Select l_c l_t (L.typedValue l_f)
    MMXExtend _v -> unimplementedInstr' typ "MMXExtend"
    ConcatV sz _sz' low high -> do
      llvm_low  <- mkLLVMValue low
      llvm_high <- mkLLVMValue high
      low'  <- zext llvm_low  typ
      high' <- zext llvm_high typ
      s_high <- shl high' (L.ValInteger (natValue sz))
      bitop L.Or s_high (L.typedValue low')
    UpperHalf sz v -> do
      llvm_v <- mkLLVMValue v
      v' <- lshr llvm_v (L.ValInteger (natValue sz))
      trunc v' (natReprToLLVMType sz)
    Trunc v sz -> flip (convop L.Trunc) (natReprToLLVMType sz) =<< mkLLVMValue v
    SExt v sz  -> flip (convop L.SExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    UExt v sz  -> flip (convop L.ZExt)  (natReprToLLVMType sz) =<< mkLLVMValue v
    AndApp{}     -> unimplementedInstr' typ "AndApp"
    OrApp{}      -> unimplementedInstr' typ "OrApp"
    NotApp{}     -> unimplementedInstr' typ "NotApp"
    BVAdd _sz x y -> binop (arithop (L.Add False False)) x y
    BVSub _sz x y -> binop (arithop (L.Sub False False)) x y
    BVMul _sz x y -> binop (arithop (L.Mul False False)) x y

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
    BVQuot _sz x y       -> binop (arithop (L.UDiv False)) x y
    BVRem _sz x y        -> binop (arithop L.URem) x y
    BVSignedQuot _sz x y -> binop (arithop (L.SDiv False)) x y
    BVSignedRem _sz x y  -> binop (arithop L.SRem) x y
    BVUnsignedLt x y     -> binop (icmpop L.Iult) x y
    BVUnsignedLe x y     -> binop (icmpop L.Iule) x y
    BVSignedLt x y       -> binop (icmpop L.Islt) x y
    BVSignedLe x y       -> binop (icmpop L.Isle) x y
    BVTestBit v n     -> do -- FIXME
      llvm_v <- mkLLVMValue v
      let in_typ = L.typedType llvm_v
      n' <- mkLLVMValue n
      n_ext <-
        case compare (natValue (fnValueWidth v)) (natValue (fnValueWidth n)) of
          LT -> error "BVTestBit expected second argument to be at least first"
          EQ -> pure n'
          GT -> zext n' in_typ
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
    BVEq x y      -> binop (icmpop L.Ieq) x y
    EvenParity v  -> do
      v' <- mkLLVMValue v
      call iEvenParity [v']
    ReverseBytes{} -> unimplementedInstr' typ "ReverseBytes"
    -- FIXME: do something more efficient?
    -- Basically does let (r, over)  = llvm.add.with.overflow(x,y)
    --                    (_, over') = llvm.add.with.overflow(r,c)
    --                in over'
    -- and we rely on llvm optimisations to throw away identical adds
    -- and adds of 0
    UadcOverflows _sz x y c -> intrinsicOverflows' "uadd" x y c
    SadcOverflows _sz x y c -> intrinsicOverflows' "sadd" x y c
    UsbbOverflows _sz x y c -> intrinsicOverflows' "usub" x y c
    SsbbOverflows _sz x y c -> intrinsicOverflows' "ssub" x y c

    Bsf _sz v -> do
      let cttz = intrinsic ("llvm.cttz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
      v' <- mkLLVMValue v
      call cttz [v', L.iT 1 L.-: L.int 1]
    Bsr _sz v -> do
      let ctlz = intrinsic ("llvm.ctlz." ++ show (L.ppType typ)) typ [typ, L.iT 1]
      v' <- mkLLVMValue v
      call ctlz [v', L.iT 1 L.-: L.int 1]

    FPIsQNaN frep v -> do
      let isQNaN = intrinsic ("reopt.isQNaN." ++ show (pretty frep)) (L.iT 1) [typ]
      v' <- mkLLVMValue v
      call isQNaN [v']

    FPIsSNaN frep v -> do
      let isSNaN = intrinsic ("reopt.isSNaN." ++ show (pretty frep)) (L.iT 1) [typ]
      v' <- mkLLVMValue v
      call isSNaN [v']

    FPAdd frep x y -> fpbinop (arithop L.FAdd) frep x y
    FPAddRoundedUp _frep _x _y -> unimplementedInstr' typ "FPAddRoundedUp"
    FPSub frep x y -> fpbinop (arithop L.FSub) frep x y
    FPSubRoundedUp _frep _x _y -> unimplementedInstr' typ "FPSubRoundedUp"
    FPMul frep x y -> fpbinop (arithop L.FMul) frep x y
    FPMulRoundedUp _frep _x _y -> unimplementedInstr' typ "FPMulRoundedUp"
    FPDiv frep x y -> fpbinop (arithop L.FDiv) frep x y
    -- FIXME: do we want ordered or unordered here?  The differ in how
    -- they treat QNaN
    FPLt  frep x y -> fpbinop (fcmp L.Fult) frep x y
    -- FIXME: similarly, we probably want oeq here (maybe?)
    FPEq  frep x y -> fpbinop (fcmp L.Foeq) frep x y
    FPCvt from_rep x to_rep -> do
      fp_x <- mkFloatLLVMValue x from_rep
      let to_typ    = floatReprToLLVMType to_rep
          from_bits = natValue $ floatInfoBits from_rep
          to_bits   = natValue $ floatInfoBits to_rep
      case compare from_bits to_bits of
        LT -> do
          fp_z <- convop L.FpExt fp_x to_typ
          bitcast fp_z (natReprToLLVMType (floatInfoBits to_rep))
        EQ -> do
          when (isNothing (MapF.testEquality from_rep to_rep)) $ do
            error $ "Incompatible floating point conversion "
                    ++ show from_rep ++ " and " ++ show to_rep
          bitcast fp_x (natReprToLLVMType (floatInfoBits to_rep))
        GT -> do
          fp_z <- convop L.FpTrunc fp_x to_typ
          bitcast fp_z (natReprToLLVMType (floatInfoBits to_rep))
    -- FIXME
    FPCvtRoundsUp _from_rep _x _to_rep -> unimplementedInstr' typ "FPCvtRoundsUp"
    FPFromBV v frepr -> do
      v' <- mkLLVMValue v
      flt_r <- convop L.SiToFp v' (floatReprToLLVMType frepr)
      bitcast flt_r (natReprToLLVMType (floatInfoBits frepr))
    -- FIXME: side-conditions here
    TruncFPToSignedBV frepr v sz -> do
      flt_v <- mkFloatLLVMValue v frepr
      convop L.FpToSi flt_v (natReprToLLVMType sz)

rhsToLLVM' :: FnAssignRhs tp -> BBLLVM (L.Typed L.Value)
rhsToLLVM' rhs =
  case rhs of
   FnEvalApp app ->
     appToLLVM' app
   FnSetUndefined sz -> do
     let typ = natReprToLLVMType sz
     return (L.Typed typ L.ValUndef)
   FnReadMem ptr typ -> do
     p <- mkLLVMValue ptr
     let llvm_typ = typeToLLVMType typ
     p' <- convop L.IntToPtr p (L.PtrTo llvm_typ)
     fmap (L.Typed llvm_typ) $ evalInstr (L.Load p' Nothing)
   FnAlloca v -> do
     v' <- mkLLVMValue v
     alloc_ptr <- alloca (L.iT 8) (Just v') Nothing
     convop L.PtrToInt alloc_ptr (L.iT 64)
   --     FS     -> L.call iRead_FS []
   --     GS     -> L.call iRead_GS []
   --     X87_PC -> L.call iRead_X87_PC []
   --     X87_RC -> L.call iRead_X87_RC []
   --     _      -> unimplementedInstr
   -- -- there doesn't seem to be a llvm.memcmp.* intrinsic
   -- MemCmp bytesPerCopy nValues src dest direction -> do
   --   nValues'   <- valueToLLVM nValues
   --   src'       <- valueToLLVM src
   --   dest'      <- valueToLLVM dest
   --   direction' <- valueToLLVM direction
   --   L.call iMemCmp [ L.iT 64 L.-: L.integer bytesPerCopy
   --                  , nValues', src', dest', direction' ]

stmtToLLVM' :: FnStmt -> BBLLVM ()
stmtToLLVM' stmt = do
  comment (show $ pretty stmt)
  case stmt of
   FnAssignStmt (FnAssignment lhs rhs) -> do
     lbl <- gets $ fbLabel . bbBlock
     llvm_rhs <- rhsToLLVM' rhs
     setAssignIdValue lhs lbl llvm_rhs
   FnWriteMem ptr v -> do
     llvm_ptr_as_bv  <- mkLLVMValue ptr
     llvm_v <- mkLLVMValue v
     -- Cast LLVM point to appropriate type
     llvm_ptr <- convop L.IntToPtr llvm_ptr_as_bv (L.PtrTo (L.typedType llvm_v))
     effect $ L.Store llvm_v llvm_ptr Nothing

   -- FS     -> L.call_ iWrite_FS [v']
   -- GS     -> L.call_ iWrite_GS [v']
   -- X87_PC -> L.call_ iWrite_X87_PC [v']
   -- X87_RC -> L.call_ iWrite_X87_RC [v']
   -- ControlLoc {} -> void $ unimplementedInstr
   -- DebugLoc {}   -> void $ unimplementedInstr
   FnComment _str -> return () -- L.comment $ Text.unpack str
   FnMemCopy bytesPerCopy nValues src dest direction -> do
     nValues' <- mkLLVMValue nValues
     src'     <- mkLLVMValue src
     dest'    <- mkLLVMValue dest
     direction' <- mkLLVMValue direction
     call_ (iMemCopy (bytesPerCopy * 8)) [dest', src', nValues', direction']

     -- case direction of
     --   FnConstantValue _ 0 -> liftBB $ do
     --    let typ = L.iT (fromIntegral $ 8 * bytesPerCopy)
     --        fn = intrinsic ("llvm.memcpy.p0"
     --                        ++ show (L.ppType typ)
     --                        ++ ".p0" ++ show (L.ppType typ)
     --                        ++ ".i64") L.voidT
     --             [L.ptrT typ, L.ptrT typ, L.iT 64, L.iT 32, L.iT 1]
     --    src_ptr  <- L.bitcast src'  (L.ptrT typ)
     --    dest_ptr <- L.bitcast dest' (L.ptrT typ)
     --    L.call_ fn [dest_ptr, src_ptr, nValues'
     --               , L.iT 32 L.-: L.int 0
     --               , L.iT 1  L.-: L.int 0 ]
     --   _ -> do

   FnMemSet count v ptr df -> do
     count' <- mkLLVMValue count
     v'     <- mkLLVMValue v
     ptr'   <- mkLLVMValue ptr
     df'    <- mkLLVMValue df
     let typ = typeToLLVMType $ fnValueType v
     ptr_ptr <- convop L.IntToPtr ptr' (L.PtrTo typ)
     call_ (iMemSet typ) [ptr_ptr, v', count', df']

   -- PlaceHolderStmt {} -> void $ unimplementedInstr
   -- _           -> void $ unimplementedInstr

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

termStmtToLLVM' :: FnTermStmt -> BBLLVM ()
termStmtToLLVM' tm =
  case tm of
     FnJump lbl -> do
       effect $ L.Jump (blockName lbl)
     FnRet (grets, frets) -> do
       grets' <- mapM mkLLVMValue grets
       frets' <- mapM mkLLVMValue frets
       makeRet' grets' frets'
     FnBranch cond tlbl flbl -> do
       cond' <- mkLLVMValue cond
       effect $ L.Br cond' (blockName tlbl) (blockName flbl)
     FnCall dest (gargs, fargs) (gretvs, fretvs) contlbl -> do
       let arg_tys = replicate (length gargs) (L.iT 64)
                     ++ replicate (length fargs) functionFloatType
           ret_tys = funReturnType
           fun_ty  = L.ptrT (L.FunTy ret_tys arg_tys False)

       addrFunMap <- gets $ funAddrFunMap . funContext
       dest_f <-
         case dest of
           -- FIXME: use ft here instead?
           FnFunctionEntryValue ft addr
             | Just (sym, tp) <- Map.lookup addr addrFunMap -> do
               when (functionTypeToLLVM tp /= fun_ty) $ do
                 error $ "Mismatch function type with " ++ show sym ++ "\n"
                   ++ "Declared: " ++ show (functionTypeToLLVM tp) ++ "\n"
                   ++ "Provided: " ++ show fun_ty
               when (tp /= ft) $ do
                 error $ "Mismatch function type in call with " ++ show sym
               return $ L.Typed fun_ty (L.ValSymbol sym)
           _ -> do
             dest' <- mkLLVMValue dest
             convop L.IntToPtr dest' fun_ty

       gargs' <- mapM mkLLVMValue gargs
       fargs' <- mapM ((`bitcast` functionFloatType) <=< mkLLVMValue) fargs
       let args' = gargs' ++ fargs'

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
     FnSystemCall call_no name args rets next_lbl -> do
       pname <- gets $ funSyscallIntrinsicPostfix . funContext
       args'  <- mapM mkLLVMValue args
       -- We put the call no at the end (on the stack) so we don't need to shuffle all the args.
       let allArgs = padUndef (L.iT 64) (length x86SyscallArgumentRegs) args'
                     ++ [ L.Typed (L.iT 64) (L.integer $ fromIntegral call_no) ]

       comment name
       rvar <- call (iSystemCall pname) allArgs
       -- Assign all return variables to the extracted result
       let assignReturnVar :: Int -> Some FnReturnVar -> BBLLVM ()
           assignReturnVar i (Some fr) = do
             lbl <- gets $ fbLabel . bbBlock
             val <- extractValue rvar (fromIntegral i)
             setAssignIdValue (frAssignId fr) lbl val
       itraverse_ assignReturnVar rets
       jump (blockName next_lbl)

     FnLookupTable idx vec -> do
       idx' <- mkLLVMValue idx
       let dests = map (L.Named . blockWordName) $ V.toList vec
       effect $ L.Switch idx' (L.Named failLabel) (zip [0..] dests)

     FnTermStmtUndefined ->
       void $ unimplementedInstr' L.voidT "FnTermStmtUndefined"

natReprToLLVMType :: NatRepr n -> L.Type
natReprToLLVMType = L.PrimType . L.Integer . fromIntegral . natValue

typeToLLVMType :: TypeRepr tp -> L.Type
typeToLLVMType (BVTypeRepr n) = natReprToLLVMType n

-- | Result obtained by printing a block to LLVM
data LLVMBlockResult = LLVMBlockResult { regBlock   :: !FnBlock
                                       , llvmBlockStmts  :: ![L.Stmt]
                                       , llvmBlockPhiVars :: ![Some PendingPhiNode]
                                       }

-- Map needed to resolve phi references
type ResolvePhiMap = Map (BlockLabel 64) LLVMBlockResult

-- | Convert a Phi node assignment to the right value
resolvePhiNodeReg :: FunLLVMContext
                  -> AssignValMap
                  -> ResolvePhiMap
                  -> (BlockLabel 64, X86Reg tp)
                  -> (L.Value, L.BlockLabel)
resolvePhiNodeReg ctx avmap m (lbl, reg) =
  case Map.lookup lbl m of
    Nothing -> error $ "Could not resolve block " ++ show lbl
    Just br -> do
      let b = regBlock br
      case MapF.lookup reg (fbRegMap b) of
        Nothing -> error $ "Could not resolve register " ++ show reg
        Just (CalleeSaved _) -> error $ "Resolve callee saved register"
        Just (FnRegValue v) -> (L.typedValue (valueToLLVMBitvec ctx b avmap v), blockName lbl)

resolvePhiStmt :: FunLLVMContext
               -> AssignValMap
               -> ResolvePhiMap
               -> Some PendingPhiNode
               -> L.Stmt
resolvePhiStmt ctx avmap m (Some (PendingPhiNode nm tp info)) = L.Result nm i []
  where i = L.Phi tp (resolvePhiNodeReg ctx avmap m <$> info)

toBasicBlock :: FunLLVMContext -> AssignValMap -> ResolvePhiMap -> LLVMBlockResult -> L.BasicBlock
toBasicBlock ctx avmap m res = L.BasicBlock { L.bbLabel = Just (blockName (fbLabel (regBlock res)))
                                            , L.bbStmts = phiStmts ++ llvmBlockStmts res
                                            }
  where phiStmts = resolvePhiStmt ctx avmap m <$> llvmBlockPhiVars res


-- | Add a phi var with the node info so that we have a ident to reference
-- it by and queue up work to assign the value later.
addPhiBinding :: Some PhiBinding -> BBLLVM ()
addPhiBinding (Some (PhiBinding (FnPhiVar fid tp) info)) = do
  lbl <- gets $ fbLabel . bbBlock
  nm <- freshName
  let llvm_tp = typeToLLVMType tp
  setAssignIdValue fid lbl (L.Typed llvm_tp (L.ValIdent nm))
  addBoundPhiVar nm llvm_tp info

-- | This converts
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
          mapM_ stmtToLLVM' $ fbStmts b
          -- Add term statement
          termStmtToLLVM' (fbTerm b)
        s = execState go s0

        res = LLVMBlockResult { regBlock = b
                              , llvmBlockStmts = reverse (bbStmts s)
                              , llvmBlockPhiVars = reverse (bbBoundPhiVars s)
                              }


-- | This writes a function to LLVM, and returns the value corresponding to the function.
--
-- We have each function return all possible results, although only the ones that are actually
-- used (we use undef for the others).  This makes the LLVM conversion slightly simpler.
--
-- Users should declare intrinsics via 'declareIntrinsics' before using this function.
-- They should also add any referenced functions.
defineFunction' :: String
                  -- ^ Name to append to system call
               -> AddrSymMap
               -> AddrFunMap
               -> Function
               -> L.Define
defineFunction' syscallPostfix addrSymMap addrFunMap f =
    L.Define { L.defAttrs = L.emptyFunAttrs
             , L.defRetType = funReturnType
             , L.defName = symbol
             , L.defArgs = args
             , L.defVarArgs = False
             , L.defSection = Nothing
             , L.defBody    = [initBlock] ++ blocks ++ [failBlock]
             , L.defMetadata = Map.empty
             }
  where
    symbol = functionName addrSymMap (fnAddr f)

    ft :: FunctionType
    ft = fnType f

    intArgs :: V.Vector (L.Typed L.Ident)
    intArgs = V.generate (fnNIntArgs ft)   $ \i -> L.Typed (L.iT 64) (L.Ident ("iarg" ++ show i))

    fltArgs :: V.Vector (L.Typed L.Ident)
    fltArgs = V.generate (fnNFloatArgs ft) $ \i -> L.Typed functionFloatType (fltArg i)

    fltbvArgs :: V.Vector (L.Typed L.Ident)
    fltbvArgs = V.generate (fnNFloatArgs ft) $ \i -> L.Typed (L.iT 128) (fltbvArg i)

    initBlock :: L.BasicBlock
    initBlock = mkInitBlock (blockName (mkRootBlockLabel (fnAddr f))) (fnNFloatArgs ft)

    args :: [L.Typed L.Ident]
    args = V.toList intArgs ++ V.toList fltArgs

    ctx :: FunLLVMContext
    ctx = FunLLVMContext { funSyscallIntrinsicPostfix = syscallPostfix
                         , funAddrFunMap  = addrFunMap
                         , funIntArgs     = fmap (fmap L.ValIdent) intArgs
                         , funFloatArgs   = fmap (fmap L.ValIdent) fltbvArgs
                         }

    mkBlockRes :: (FunState, [LLVMBlockResult])
               -> FnBlock
               -> (FunState, [LLVMBlockResult])
    mkBlockRes (fs, prev) b =
      let (r, fs') = blockToLLVM ctx fs b
       in (fs', r:prev)

    init_fun_state :: FunState
    init_fun_state = FunState { nmCounter = 0
                              , funAssignValMap = Map.empty
                              }

    block_results :: [LLVMBlockResult]
    (fin_fs, block_results) = foldl mkBlockRes (init_fun_state, []) (fnBlocks f)

    resolvePhiMap :: ResolvePhiMap
    resolvePhiMap = Map.fromList
      [ (fbLabel (regBlock b), b)
      | b <- block_results
      ]

    blocks :: [L.BasicBlock]
    blocks = toBasicBlock ctx (funAssignValMap fin_fs) resolvePhiMap <$> reverse block_results

declareIntrinsic :: L.Typed L.Value -> L.Declare
declareIntrinsic (L.Typed (L.PtrTo (L.FunTy rty argtys _)) (L.ValSymbol sym)) =
  L.Declare { L.decRetType = rty
            , L.decName    = sym
            , L.decArgs    = argtys
            , L.decVarArgs = False
            }
declareIntrinsic _ = error "Not an intrinsic"


-- | Declare all LLVM and reopt-specific intrinsics
intrinsicDecls :: [L.Declare]
intrinsicDecls = declareIntrinsic <$> (reoptIntrinsics ++ llvmIntrinsics)

declareFunction' :: (L.Symbol, FunctionType) -> L.Declare
declareFunction' (sym, ftp) =
  L.Declare { L.decRetType = funReturnType
            , L.decName    = sym
            , L.decArgs    = functionTypeArgTypes ftp
            , L.decVarArgs = False
            }

-- | Get module for functions
moduleForFunctions :: String
                      -- ^ Name to append to system calls
                   -> AddrSymMap
                   -> [Function]
                   -> L.Module
moduleForFunctions syscallPostfix addrSymMap fns =
    L.Module { L.modSourceName = Nothing
             , L.modDataLayout = []
             , L.modTypes      = []
             , L.modNamedMd    = []
             , L.modUnnamedMd  = []
             , L.modGlobals    = []
             , L.modDeclares   = intrinsicDecls ++ fnDecls
             , L.modDefines    = defines
             , L.modInlineAsm  = []
             , L.modAliases    = []
             }
         -- Get all function references
  where  all_refs :: Map (SegmentedAddr 64) FunctionType
         all_refs = foldl getReferencedFunctions Map.empty fns

         addrFunMap :: AddrFunMap
         addrFunMap = Map.fromList
           [ (addr, (functionName addrSymMap addr, tp))
           | (addr, tp) <- Map.toList all_refs
           ]

         declFunMap = addrFunMap `Map.withoutKeys` (Set.fromList (fnAddr <$> fns))

         fnDecls = declareFunction' <$> Map.elems declFunMap

         defines = defineFunction' syscallPostfix addrSymMap addrFunMap <$> fns
