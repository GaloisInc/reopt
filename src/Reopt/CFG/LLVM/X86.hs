{-|
X86-specific operations for LLVM generation.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Reopt.CFG.LLVM.X86
  ( x86LLVMArchOps
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Parameterized.Some
import           GHC.Stack
import           Numeric.Natural
import qualified Text.LLVM as L
import           Text.PrettyPrint.ANSI.Leijen (pretty)

import           Data.Macaw.CFG
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes
import           Data.Macaw.X86.Monad (RepValSize(..), repValSizeByteCount)

import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86
import           Reopt.CFG.LLVM

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

itNat :: Natural -> L.Type
itNat w = L.iT (fromIntegral w)

emitX86ArchFn :: HasCallStack
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
     let tp = itNat (16 * repValSizeByteCount repr)
     llvm_d_ext <- L.typedValue <$> convop L.ZExt llvm_d tp
     arithop (L.UDiv False) llvm_n llvm_d_ext
   X86Rem repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = itNat (16 * repValSizeByteCount repr)
     llvm_d_ext <- L.typedValue <$> convop L.ZExt llvm_d tp
     arithop L.URem llvm_n llvm_d_ext
   X86IDiv repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = itNat (16 * repValSizeByteCount repr)
     llvm_d_ext <- L.typedValue <$> convop L.SExt llvm_d tp
     arithop (L.SDiv False) llvm_n llvm_d_ext
   X86IRem repr n d -> do
     llvm_n <- mkLLVMValue n
     llvm_d <- mkLLVMValue d
     let tp = itNat (16 * repValSizeByteCount repr)
     llvm_d_ext <- L.typedValue <$> convop L.SExt llvm_d tp
     arithop L.SRem llvm_n llvm_d_ext
   RepnzScas sz val base cnt -> do
     -- Value to search for.
     llvm_val <- mkLLVMValue val
     -- Convert buffer to LLVM
     let w = itNat (8 * repValSizeByteCount sz)
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

emitX86ArchStmt :: HasCallStack
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
          setAssignIdValue (frAssignId fr) rvar
        _ -> error "Unexpected return values"
    "FreeBSD" -> do
      rvar <- emitSyscall llvm_call_num llvm_args
      case rets of
        [Some fr] -> do
          -- Assign all return variables to the extracted result
          setAssignIdValue (frAssignId fr) rvar
        _ -> error "Unexpected return values"
    _ -> error $ "Unsupported operating system: " ++ show pname
emitX86ArchStmt _ (X86FnStmt stmt) =
  case stmt of
    RepMovs bytesPerCopy destExpr srcExpr cntExpr dirExpr -> do
      dest    <- mkLLVMValue destExpr
      src     <- mkLLVMValue srcExpr
      cnt     <- mkLLVMValue cntExpr
      df <-
        case dirExpr of
          FnConstantBool b -> pure b
          _ -> fail "LLVM generator only supports rep movs with constant df"
      let dfAsm = case df of
                    True  -> "std"
                    False -> "cld"
      let movsAsm = case bytesPerCopy of
                      ByteRepVal  -> "rep movsb"
                      WordRepVal  -> "rep movsw"
                      DWordRepVal -> "rep movsd"
                      QWordRepVal -> "rep movsq"
      callAsm_ noSideEffect
               (dfAsm ++ "\n" ++ movsAsm)
               "={cx},={si},={di},~{dirflag},~{memory}"
               [cnt, src, dest]

    RepStos bytesPerCopy destExpr srcExpr cntExpr dirExpr -> do
      dest    <- mkLLVMValue destExpr
      src     <- mkLLVMValue srcExpr
      cnt     <- mkLLVMValue cntExpr
      df <-
        case dirExpr of
          FnConstantBool b -> pure b
          _ -> fail "LLVM generator only supports rep stos with constant df"
      let dfAsm = case df of
                    True  -> "std"
                    False -> "cld"
      let stosAsm = case bytesPerCopy of
                      ByteRepVal  -> "rep stosb"
                      WordRepVal  -> "rep stosw"
                      DWordRepVal -> "rep stosd"
                      QWordRepVal -> "rep stosq"
      callAsm_ noSideEffect
               (dfAsm ++ "\n" ++stosAsm)
               "={cx},={si},={di},~{dirflag},~{memory}"
               [cnt, src, dest]

    _ -> error $ "LLVM generation: Unsupported architecture statement."

------------------------------------------------------------------------
-- PopCount

emitPopCount :: NatRepr w -> FnValue X86_64 (BVType w) -> BBLLVM X86_64 (L.Typed L.Value)
emitPopCount w v = do
  v' <- mkLLVMValue v
  let wv = natValue w
  when (wv `notElem` [16, 32, 64]) $ do
    fail $ "Only support popcount of 16, 32, or 64 bits"
  callAsm noSideEffect (L.iT (fromIntegral wv)) "popcnt $0, $1" "=r,r" [v']

------------------------------------------------------------------------
-- ArchOps

x86LLVMArchOps :: String -- ^ Prefix for system call intrinsic
               -> LLVMArchSpecificOps X86_64
x86LLVMArchOps pname = LLVMArchSpecificOps
  { archEndianness = LittleEndian
  , archFnCallback = emitX86ArchFn
  , archStmtCallback = emitX86ArchStmt pname
  , popCountCallback = emitPopCount
  }
