{-|
Copyright        : (c) Galois, Inc 2015-2018

X86-specific operations for FunRep
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Reopt.CFG.LLVM.X86
  ( x86LLVMArchOps
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Some
import qualified Data.Vector as V
import qualified GHC.Err.Located as Loc
import qualified Text.LLVM as L
import qualified Text.LLVM.PP as L (ppType)
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Data.Macaw.CFG
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.Monad (RepValSize(..), repValSizeByteCount)
import           Data.Macaw.X86.X86Reg

import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86
import           Reopt.CFG.LLVM


x86LLVMRetType :: L.Type
x86LLVMRetType =
  L.Struct $ (typeToLLVMType . typeRepr <$> x86ResultRegs)
      ++ (replicate (length x86FloatResultRegs) functionFloatType)

argIdent :: Int -> L.Ident
argIdent i = L.Ident ("arg" ++ show i)

-- | Create a unique name for temporaries used in creating a float.
fltbvTempArg :: Int -> Int -> L.Ident
fltbvTempArg i j = L.Ident ("fargbv" ++ show i ++ "_" ++ show j)

-- | Create a unique name for the float argument.
fltbvArg :: Int -> L.Ident
fltbvArg i = L.Ident ("fargbv" ++ show i)

functionFloatType :: L.Type
functionFloatType = L.Vector 2 (L.PrimType $ L.FloatType L.Double)

functionArgType :: Some X86Reg -> L.Type
functionArgType (Some r) =
  case r of
    X86_GP{} -> L.iT 64
    X86_YMMReg{} -> functionFloatType
    _ -> Loc.error "Unsupported function type registers"


mkX86InitBlock :: FunctionType X86_64 -- ^ Type of function
            -> L.BlockLabel -- ^ Label of first block
            -> ([L.Typed L.Ident], L.BasicBlock, V.Vector (L.Typed L.Value))
mkX86InitBlock ft lbl = (inputArgs, blk, postInitArgs)
    where mkInputReg :: L.Type -> Int -> L.Typed L.Ident
          mkInputReg tp i = L.Typed tp (argIdent i)

          inputArgs :: [L.Typed L.Ident]
          inputArgs = zipWith mkInputReg (functionArgType <$> ftArgRegs ft) [0..]

          -- Block to generate
          blk = L.BasicBlock { L.bbLabel = Just (L.Named (L.Ident "init"))
                             , L.bbStmts = fltStmts ++ [L.Effect (L.Jump lbl) []]
                             }

          intArgs :: V.Vector (L.Typed L.Value)
          intArgs = V.generate (fnNIntArgs ft)   $ \i -> L.Typed (L.iT 64) (L.ValIdent (argIdent i))

          fltbvArgs :: V.Vector (L.Typed L.Value)
          fltbvArgs = V.generate (fnNFloatArgs ft) $ \i -> L.Typed (L.iT 256) (L.ValIdent (fltbvArg i))

          postInitArgs :: V.Vector (L.Typed L.Value)
          postInitArgs = intArgs V.++ fltbvArgs

          fltStmts :: [L.Stmt]
          fltStmts = concatMap fltStmt [0..fnNFloatArgs ft-1]
            where fltStmt :: Int -> [L.Stmt]
                  fltStmt i = do
                    -- Get typed arg for input
                    let arg = L.Typed functionFloatType (L.ValIdent (argIdent (fnNIntArgs ft + i)))
                    -- Get typed argument after bitcast from float to bv128.
                    let bv128Arg = L.Typed (L.iT 128) (L.ValIdent (fltbvTempArg i 0))
                    -- Return
                    [   L.Result (fltbvTempArg i 0) (L.Conv L.BitCast arg   (L.iT 128)) []
                      , L.Result (fltbvArg i)       (L.Conv L.ZExt bv128Arg (L.iT 256)) []
                      ]

------------------------------------------------------------------------
-- emitX86ArchFn

-- | Generate the LLVM for checking parity of an 8-bit value is even.
evenParity :: L.Typed L.Value -> BBLLVM X86_64 (L.Typed L.Value)
evenParity v = do
  -- This code calls takes the disjunction of the value with itself to update flags,
  -- then pushes 16-bit flags register to the stack, then pops it to a register.
  res <- callAsm noSideEffect (L.iT 16) "orb $1, $1\0Apushfw\0Apopw $0\0A" "=r,r" [v]
  -- Check parity flag
  parity_val <- band res (L.ValInteger 4)
  -- Check result is nonzero
  icmpop L.Ine parity_val (L.ValInteger 0)

emitX86ArchFn :: Loc.HasCallStack
              => ArchFn X86_64 (FnValue X86_64) tp
              -> BBLLVM X86_64 (L.Typed L.Value)
emitX86ArchFn f =
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
     let tp = L.iT $ fromInteger $ 16 * repValSizeByteCount repr
     llvm_d_ext <- L.typedValue <$> convop L.ZExt llvm_d tp
     arithop (L.UDiv False) llvm_n llvm_d_ext
   X86Rem repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = L.iT $ fromInteger $ 16 * repValSizeByteCount repr
     llvm_d_ext <- L.typedValue <$> convop L.ZExt llvm_d tp
     arithop L.URem llvm_n llvm_d_ext
   X86IDiv repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = L.iT $ fromInteger $ 16 * repValSizeByteCount repr
     llvm_d_ext <- L.typedValue <$> convop L.SExt llvm_d tp
     arithop (L.SDiv False) llvm_n llvm_d_ext
   X86IRem repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = L.iT $ fromInteger $ 16 * repValSizeByteCount repr
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
     -- Get rge cx result
     extractValue res 0
   _ -> do
     error $ "LLVM backend does not yet support: " ++ show (runIdentity (ppArchFn (pure . pretty) f))

------------------------------------------------------------------------
-- emitX86ArchStmt

-- | Generate a system call on Linux
-- TODO: Check registers also work for FreeBSD
emitSyscall :: L.Typed L.Value
            -> [L.Typed L.Value] -- ^ Arguments
            -> BBLLVM X86_64 (L.Typed L.Value)
emitSyscall callNum args =
  callAsm sideEffect
          (L.iT 64)
          "syscall"
          -- This string marks rax as an output.
          -- It also marks rax, rdi, rsi, rdx, r10, r8, r9 as inputs.
          -- It indicates that the function can make arbitrary
          -- modifications to memory, flags, rcx, and r11.
          "={rax},{rax},{rdi},{rsi},{rdx},{r10},{r8},{r9},~{memory},~{flags},~{rcx},~{r11}"
          (callNum : padUndef (L.iT 64) 6 args)

emitX86ArchStmt :: Loc.HasCallStack
               => String -- ^ Prefix for system calls
               -> X86FnStmt (FnValue X86_64)
               -> BBLLVM X86_64 ()
emitX86ArchStmt pname (X86FnSystemCall call_num args rets) = do
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
        _ -> error "Unexpected return values"
    "FreeBSD" -> do
      rvar <- emitSyscall llvm_call_num llvm_args
      case rets of
        [Some fr] -> do
          -- Assign all return variables to the extracted result
          lbl <- gets $ fbLabel . bbBlock
          setAssignIdValue (frAssignId fr) lbl rvar
        _ -> error "Unexpected return values"
    _ -> error $ "Unsupported operating system: " ++ show pname
emitX86ArchStmt _ (X86FnStmt stmt) =
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

------------------------------------------------------------------------
-- PopCount

emitPopCount :: NatRepr w -> FnValue X86_64 (BVType w) -> BBLLVM X86_64 (L.Typed L.Value)
emitPopCount w v = do
  v' <- mkLLVMValue v
  let wv = natValue w
  when (wv `notElem` [16, 32, 64]) $ do
    fail $ "Only support popcount of 16, 32, or 64 bits"
  callAsm noSideEffect (L.iT (fromInteger wv)) "popcnt $0, $1" "=r,r" [v']

------------------------------------------------------------------------
-- X86 call

emitX86Call :: FnValue X86_64 (BVType 64)
            -> FunctionType X86_64
               -- Arguments
            -> [Some (FnValue X86_64)]
               -- Return values
            -> FnReturnInfo X86_64 FnReturnVar
            -> BBLLVM X86_64 ()
emitX86Call dest ft args retvs = do
  aops <- gets $ archFns
  let fun_ty  = functionTypeToLLVM aops ft

  addrSymMap  <- gets $ funAddrSymMap  . funContext
  addrTypeMap <- gets $ funAddrTypeMap . funContext
  dest_f <-
    case dest of
      -- FIXME: use ft here instead?
      FnFunctionEntryValue dest_ftp addr
        | Just tp <- Map.lookup addr addrTypeMap -> do
            let sym = functionName addrSymMap addr
            when (functionTypeToLLVM aops tp /= fun_ty) $ do
              Loc.error $ "Mismatch function type with " ++ show sym ++ "\n"
                ++ "Declared: " ++ show (functionTypeToLLVM aops tp) ++ "\n"
                ++ "Provided: " ++ show fun_ty
            when (ft /= dest_ftp) $ do
              Loc.error $ "Mismatch function type in call with " ++ show sym
            when (tp /= dest_ftp) $ do
              Loc.error $ "Mismatch function type in call with " ++ show sym
            return $ L.Typed fun_ty (L.ValSymbol sym)
      _ -> do
        dest' <- mkLLVMValue dest
        convop L.IntToPtr dest' fun_ty

  let evalArg :: Some X86Reg -> Some (FnValue X86_64) -> BBLLVM X86_64 (L.Typed L.Value)
      evalArg (Some (X86_GP _)) (Some v) = mkLLVMValue v
      evalArg (Some (X86_YMMReg _)) (Some v) = do
        llvmVal <- mkLLVMValue v
        bitcast llvmVal functionFloatType
      evalArg _ _ = Loc.error "Unsupported register arg"

  args' <- zipWithM evalArg (ftArgRegs ft) args

  retv <- call dest_f args'

  this_lbl <- gets $ fbLabel . bbBlock
  -- Assign all return variables to the extracted result
  let assignIntReturn :: Int -> FnReturnVar (BVType 64) -> BBLLVM X86_64 ()
      assignIntReturn i fr = do
        val <- extractValue retv (fromIntegral i)
        setAssignIdValue (frAssignId fr) this_lbl val
  itraverse_ assignIntReturn (x86IntReturn retvs)
  let fpBase = 2
  -- Assign floating point results
  let assignFltReturn :: Int -> FnReturnVar (BVType 128) -> BBLLVM X86_64 ()
      assignFltReturn i fr = do
        val_fp <- extractValue retv (fromIntegral $ fpBase + i)
        val    <- bitcast val_fp (L.iT 128)
        setAssignIdValue (frAssignId fr) this_lbl val
  itraverse_ assignFltReturn (x86XMMReturn retvs)

------------------------------------------------------------------------
-- Tailcall

emitX86Tailcall :: FnValue X86_64 (BVType 64)
                -> FunctionType X86_64
                -> [Some (FnValue X86_64)]
                -> BBLLVM X86_64 ()
emitX86Tailcall dest ft args = do
  aops <- gets $ archFns
  let fun_ty  = functionTypeToLLVM aops ft

  addrSymMap  <- gets $ funAddrSymMap  . funContext
  addrTypeMap <- gets $ funAddrTypeMap . funContext
  dest_f <-
    case dest of
           -- FIXME: use ft here instead?
           FnFunctionEntryValue dest_ftp addr
             | Just tp <- Map.lookup addr addrTypeMap -> do
               let sym = functionName addrSymMap addr
               when (functionTypeToLLVM aops tp /= fun_ty) $ do
                 Loc.error $ "Mismatch function type with " ++ show sym ++ "\n"
                   ++ "Declared: " ++ show (functionTypeToLLVM aops tp) ++ "\n"
                   ++ "Provided: " ++ show fun_ty
               when (ft /= dest_ftp) $ do
                 Loc.error $ "Mismatch function type in call with " ++ show sym
               when (tp /= dest_ftp) $ do
                 Loc.error $ "Mismatch function type in call with " ++ show sym
               return $ L.Typed fun_ty (L.ValSymbol sym)
           _ -> do
             dest' <- mkLLVMValue dest
             convop L.IntToPtr dest' fun_ty

  let evalArg :: Some X86Reg -> Some (FnValue X86_64) -> BBLLVM X86_64 (L.Typed L.Value)
      evalArg (Some (X86_GP _)) (Some v) = mkLLVMValue v
      evalArg (Some (X86_YMMReg _)) (Some v) = (`bitcast` functionFloatType) =<< mkLLVMValue v
      evalArg _ _ = Loc.error "Unsupported register arg"

  args' <- zipWithM evalArg (ftArgRegs ft) args
  retv <- call dest_f args'
  ret retv

------------------------------------------------------------------------
-- Return

emitX86Return :: FnReturnInfo X86_64 (FnValue X86_64) -> BBLLVM X86_64  ()
emitX86Return rets = do
  grets <- mapM mkLLVMValue (x86IntReturn rets)
  frets <- mapM mkLLVMValue (x86XMMReturn rets)

  -- clang constructs something like
  -- %3 = insertvalue { i64, i64 } undef, i64 %1, 0
  -- %4 = insertvalue { i64, i64 } %3, i64 %2, 1
  -- ret { i64, i64 } %4
  -- which we will duplicate, with undef padding where required.

  -- cast fp results to the required type
  cfrets <- forM frets $ \v256 -> do
    v128 <- convop L.Trunc v256 (L.PrimType (L.Integer 128))
    bitcast v128 functionFloatType

  let frets' = padUndef functionFloatType (length x86FloatResultRegs) cfrets
  -- construct the return result struct
  let initUndef = L.Typed x86LLVMRetType L.ValUndef
  let grets'    = padUndef (L.iT 64) (length x86ResultRegs) grets
  v <- ifoldlM (\n acc fld -> insertValue acc fld (fromIntegral n)) initUndef (grets' ++ frets')
  ret v

------------------------------------------------------------------------
-- X86 specific intrinsics

iMemCopy :: Integer -> Intrinsic
iMemCopy n = intrinsic ("reopt.MemCopy.i" ++ show n) L.voidT [ L.iT 64, L.iT 64, L.iT 64, L.iT 1]

iMemSet :: L.Type -> Intrinsic
iMemSet typ = intrinsic ("reopt.MemSet." ++ show (L.ppType typ)) L.voidT args
  where args = [L.ptrT typ, typ, L.iT 64, L.iT 1]

x86Intrinsics :: [Intrinsic]
x86Intrinsics = [ iMemCopy n       | n <- [8, 16, 32, 64] ]
                  ++ [ iMemSet (L.iT n) | n <- [8, 16, 32, 64] ]

------------------------------------------------------------------------
-- ArchOps

x86LLVMArchOps :: String -- ^ Prefix for system call intrinsic
               -> LLVMArchSpecificOps X86_64
x86LLVMArchOps pname = LLVMArchSpecificOps
  { archIntrinsics = x86Intrinsics
  , archFnArgTypes = \ft -> functionArgType <$> ftArgRegs ft
  , archFnReturnType = \_ -> x86LLVMRetType
  , mkInitBlock = mkX86InitBlock
  , archFnCallback = emitX86ArchFn
  , archStmtCallback = emitX86ArchStmt pname
  , popCountCallback = emitPopCount
  , callCallback =  emitX86Call
  , tailcallCallback = emitX86Tailcall
  , returnCallback = emitX86Return
  }
