{-|
X86-specific operations for LLVM generation.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Reopt.CFG.LLVM.X86
  ( x86LLVMArchOps
  , x86ArchFnToLLVM
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Parameterized.Some
import           GHC.Stack
import           Numeric.Natural
import           Prettyprinter
import qualified Text.LLVM as L

import           Data.Macaw.CFG
import           Data.Macaw.Types

import           Data.Macaw.X86.ArchTypes

import           Reopt.CFG.FnRep
import           Reopt.CFG.FnRep.X86
import           Reopt.CFG.LLVM

------------------------------------------------------------------------
-- x86ArchFnToLLVM

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

mkPair :: L.Typed L.Value
       -> L.Typed L.Value
       -> BBLLVM arch (L.Typed L.Value)
mkPair x y = do
  let tp = L.Struct [L.typedType x, L.typedType y]
  let s0 = L.Typed tp L.ValUndef
  s1 <- insertValue s0 x 0
  insertValue s1 y 1

llvmDivNumerator :: Natural -- ^ Number of bits in w.
                 -> FnValue X86_64 (BVType w)
                 -> FnValue X86_64 (BVType w)
                 -> BBLLVM X86_64 (L.Typed L.Value)
llvmDivNumerator bitCount num1 num2 = do
  let extTp = llvmITypeNat (2 * bitCount)
  llvmNumH <- mkLLVMValue num1
  llvmNumHExt <- convop L.ZExt llvmNumH extTp
  let shiftAmt = L.ValInteger  (toInteger bitCount)
  llvmNumHShift <- shl llvmNumHExt shiftAmt
  llvmNumL <- mkLLVMValue num2
  llvmNumLExt <- convop L.ZExt llvmNumL extTp
  bor llvmNumHShift (L.typedValue llvmNumLExt)

x86ArchFnToLLVM :: ArchFn X86_64 (FnValue X86_64) tp
              -> Maybe (BBLLVM X86_64 (L.Typed L.Value))
x86ArchFnToLLVM f = do
  case f of
    EvenParity v -> Just $ do
      evenParity =<< mkLLVMValue v

    -- Unsigned division and remainder
    X86DivRem repr num1 num2 d -> Just $ do
      genOpts <- asks $ funLLVMGenOptions
      if mcExceptionIsUB genOpts then do
        -- If potentially undefined LLVM is allowed, then we
        -- translate to it.
        -- Get wide type
        let bitCount = 8 * repValSizeByteCount repr
        -- Get type of inputs and result
        let resTp = llvmITypeNat bitCount
        -- Division occurs at double bitwidth
        let extTp = llvmITypeNat (2 * bitCount)
        -- Compute numerator as (num1 << bitCount | num2)
        llvmNumExt <- llvmDivNumerator bitCount num1 num2
        -- Compute denominator
        llvmDen <- mkLLVMValue d
        llvmDenExt <- L.typedValue <$> convop L.ZExt llvmDen extTp
        -- Perform divison and remainder
        qext <- arithop (L.UDiv False) llvmNumExt llvmDenExt
        rext <- arithop L.URem llvmNumExt llvmDenExt
        -- Get low  order bits of quotient and remainder
        --
        -- Note.  This will compute the wrong answer rather than #DE
        -- when the division result overflows, but that is allowed by
        -- mcExceptionIsUB
        q <- convop L.Trunc qext resTp
        r <- convop L.Trunc rext resTp
        -- Compute pair
        mkPair q r
       else do
        -- Otherwise we switch to assembly
        let tp = llvmITypeNat (repValSizeBitCount repr)
        llvmNumH <- mkLLVMValue num1
        llvmNumL <- mkLLVMValue num2
        llvmDen <- mkLLVMValue d
        callAsm sideEffect
                (L.Struct [tp, tp])
                "div $4"
                "={ax},={dx},{dx},{ax},r,~{flags}"
                [llvmNumH, llvmNumL, llvmDen]

   -- Signed division and remainder
    X86IDivRem repr num1 num2 d -> Just $ do
      genOpts <- asks $ funLLVMGenOptions
      if mcExceptionIsUB genOpts then do
        -- Get bitwidth
        let bitCount = 8 * repValSizeByteCount repr
        -- Get type of inputs and result
        let resTp = llvmITypeNat bitCount
        -- Division occurs at double bitwidth
        let extTp = llvmITypeNat (2 * bitCount)
        -- Compute numerator as (num1 << bitCount | num2)
        llvmNumExt <- llvmDivNumerator bitCount num1 num2
        -- Compute denominator
        llvmDen <- mkLLVMValue d
        llvmDenExt <- L.typedValue <$> convop L.SExt llvmDen extTp
        -- Perform divison and remainder
        qext <- arithop (L.SDiv False) llvmNumExt llvmDenExt
        rext <- arithop L.SRem llvmNumExt llvmDenExt
        -- Get low  order bits of quotient and remainder
        --
        -- Note.  This will compute the wrong answer rather than #DE
        -- when the division result overflows, but that is allowed by
        -- mcExceptionIsUB
        q <- convop L.Trunc qext resTp
        r <- convop L.Trunc rext resTp
        -- Compute pair
        mkPair q r
       else do
        let tp = llvmITypeNat (repValSizeBitCount repr)
        llvmNumH <- mkLLVMValue num1
        llvmNumL <- mkLLVMValue num2
        llvmDen <- mkLLVMValue d
        callAsm sideEffect
                (L.Struct [tp, tp])
                ("idiv $4")
                "={ax},={dx},{dx},{ax},r,~{flags}"
                [llvmNumH, llvmNumL, llvmDen]

    RepnzScas sz val base cnt -> Just $ do
     -- Value to search for.
     llvm_val <- mkLLVMValue val
     -- Convert buffer to LLVM
     let w = llvmITypeNat (8 * repValSizeByteCount sz)
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


    -- Convert floating point to signed integer.
    SSE_CVTTSX2SI outW _floatTp x -> Just $ do
      llvmX <- mkLLVMValue x
      convop L.FpToSi llvmX (llvmITypeNat (natValue outW))

    -- Convert signed integer to floating point.
    SSE_CVTSI2SX outTp _inW x -> Just $ do
     llvmX <- mkLLVMValue x
     let llvmFloatType = case outTp of
                           SSE_Single -> L.Float
                           SSE_Double -> L.Double
     convop L.SiToFp llvmX (L.PrimType (L.FloatType llvmFloatType))

    _ -> Nothing

--   _ -> do
--     error $ "LLVM backend does not yet support: "
--       ++ show (runIdentity (ppArchFn (pure . pretty) f))

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
          _ -> error "LLVM generator only supports rep movs with constant df"
      let movsAsm = case bytesPerCopy of
                      ByteRepVal  -> "rep movsb"
                      WordRepVal  -> "rep movsw"
                      DWordRepVal -> "rep movsd"
                      QWordRepVal -> "rep movsq"
      let dfAsm = case df of
                    True  -> "std\n" ++ movsAsm ++ "\ncld"
                    False -> movsAsm
      callAsm_ noSideEffect
               (dfAsm ++ "\n" ++ movsAsm)
               "{cx},{si},{di},~{dirflag},~{flags},~{memory}"
               [cnt, src, dest]

    RepStos bytesPerCopy destExpr valExpr cntExpr dirExpr -> do
      dest    <- mkLLVMValue destExpr
      val     <- mkLLVMValue valExpr
      cnt     <- mkLLVMValue cntExpr
      df <-
        case dirExpr of
          FnConstantBool b -> pure b
          _ -> error "LLVM generator only supports rep stos with constant df"
      let stosAsm = case bytesPerCopy of
                      ByteRepVal  -> "rep stosb"
                      WordRepVal  -> "rep stosw"
                      DWordRepVal -> "rep stosd"
                      QWordRepVal -> "rep stosq"
      let dfAsm = case df of
                    True  -> "std\n" ++ stosAsm ++ "\ncld"
                    False -> stosAsm
      callAsm_ noSideEffect
               dfAsm
               "{cx},{di},{ax},~{memory},~{flags}"
               [cnt, dest, val]

    _ -> error $ "LLVM generation: Unsupported architecture statement."

------------------------------------------------------------------------
-- PopCount

emitPopCount :: NatRepr w -> FnValue X86_64 (BVType w) -> BBLLVM X86_64 (L.Typed L.Value)
emitPopCount w v = do
  v' <- mkLLVMValue v
  let wv = natValue w
  when (wv `notElem` [16, 32, 64]) $ do
    error $ "Only support popcount of 16, 32, or 64 bits"
  callAsm noSideEffect (llvmITypeNat wv) "popcnt $0, $1" "=r,r" [v']

------------------------------------------------------------------------
-- ArchOps

x86FnCallback :: HasCallStack
              => ArchFn X86_64 (FnValue X86_64) tp
              -> BBLLVM X86_64 (L.Typed L.Value)
x86FnCallback f = do
  case x86ArchFnToLLVM f of
    Just act -> act
    Nothing -> do
      error $ "LLVM backend does not yet support: "
         ++ show (runIdentity (ppArchFn (pure . pretty) f))

x86LLVMArchOps :: String -- ^ Prefix for system call intrinsic
               -> LLVMArchSpecificOps X86_64
x86LLVMArchOps pname =
  LLVMArchSpecificOps
  { archEndianness = LittleEndian
  , archFnCallback = x86FnCallback
  , archStmtCallback = emitX86ArchStmt pname
  , popCountCallback = emitPopCount
  }
