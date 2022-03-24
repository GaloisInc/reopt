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

mkFlags :: [Bool] -> BBLLVM arch (L.Typed L.Value)
mkFlags flags = do
  let tp = L.Struct $ map (const (L.iT 1)) flags
  let s0 = L.Typed tp L.ValUndef
  let insertFlag s (f, idx) = insertValue s (L.Typed (L.iT 1) $ L.ValBool f) idx
  foldM insertFlag s0 (zip flags [0..])


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
x86ArchFnToLLVM f =
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
        q <- llvmTrunc "X86DivRem" qext resTp
        r <- llvmTrunc "X86DivRem" rext resTp
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
        q <- llvmTrunc "X86IDivRem" qext resTp
        r <- llvmTrunc "X86IDivRem" rext resTp
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
     llvm_ptr <- llvmAsPtr "RepnzScas" w =<< mkLLVMValue base
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

    SSE_UnaryOp unop tp x y -> Just $ do
      genOpts <- asks $ funLLVMGenOptions
      llvmX <- mkLLVMValue x
      llvmY <- mkLLVMValue y
      if mcExceptionIsUB genOpts then do
        let (llvmFloatType, fXX) = case tp of
                SSE_Single -> (L.PrimType $ L.FloatType L.Float, "f32")
                SSE_Double -> (L.PrimType $ L.FloatType L.Double, "f64")
        case unop of
          SSE_Add -> arithop L.FAdd llvmX (L.typedValue llvmY)
          SSE_Sub -> arithop L.FSub llvmX (L.typedValue llvmY)
          SSE_Mul -> arithop L.FMul llvmX (L.typedValue llvmY)
          SSE_Div -> arithop L.FDiv llvmX (L.typedValue llvmY)
          SSE_Min ->
            let fmin = intrinsic ("llvm.minnum."++fXX) llvmFloatType [llvmFloatType, llvmFloatType]
             in call fmin [llvmX, llvmY]
          SSE_Max ->
            let fmax = intrinsic ("llvm.maxnum."++fXX) llvmFloatType [llvmFloatType, llvmFloatType]
             in call fmax [llvmX, llvmY]

      else do
        let (llvmFloatType, opSuffix) = case tp of
                SSE_Single -> (L.PrimType $ L.FloatType L.Float, "s")
                SSE_Double -> (L.PrimType $ L.FloatType L.Double, "d")
        let mnemonic = (ppMnem $ sseOpName unop) ++ "s" ++ opSuffix
        callAsm sideEffect
                llvmFloatType
               (mnemonic ++ " $2,$1")
               "=x|x,x|x,x|m,~{dirflag},~{fpsr},~{flags}"
               [llvmX, llvmY]

    SSE_UCOMIS tp x y -> Just $ do
      llvmX <- mkLLVMValue x
      llvmY <- mkLLVMValue y
      genOpts <- asks $ funLLVMGenOptions
      if mcExceptionIsUB genOpts then do
        -- According to intex x86 instruction set reference, UCOMISS/UCOMISD use
        -- the following values for flags ZF,PF,CF based on the result: unordered
        -- (111), greater than (000) less than (011), and equal (100)
        isLt <- fcmpop L.Folt llvmX (L.typedValue llvmY)
        ltRes <- mkFlags [False,False,True]
        isGt <- fcmpop L.Folt llvmY (L.typedValue llvmX)
        gtRes <- mkFlags [False,False,False]
        isEq <- fcmpop L.Foeq llvmX (L.typedValue llvmY)
        eqRes <- mkFlags [True,False,False]
        unordRes <- mkFlags [True,True,True]
        let resTp = L.Struct [(L.iT 1),(L.iT 1),(L.iT 1)]
        selectVal resTp [(isLt,ltRes),(isGt,gtRes),(isEq,eqRes)] unordRes
      else do
        res <- case tp of
                 SSE_Single ->
                   callAsm noSideEffect
                           (L.iT 16)
                           "ucomiss $1,$2\0A\09pushfw\0A\09popw $0\0A\09"
                           "=r|r,x|x,x|M,~{dirflag},~{fpsr},~{flags}"
                           [llvmX, llvmY]
                 SSE_Double ->
                   callAsm noSideEffect
                           (L.iT 16)
                           "ucomisd $1,$2\0A\09pushfw\0A\09popw $0\0A\09"
                           "=r|r,x|x,x|N,~{dirflag},~{fpsr},~{flags}"
                           [llvmX, llvmY]
        -- Eflags are returned in a 16bit integer so we
        -- extract flags and return in struct.
        cf <- band res (L.ValInteger 1) -- 0b1
        cfSet <- icmpop L.Ine cf (L.ValInteger 0)
        pf <- band res (L.ValInteger 4) -- 0b100
        pfSet <- icmpop L.Ine pf (L.ValInteger 0)
        zf <- band res (L.ValInteger 64) -- 0b1000000
        zfSet <- icmpop L.Ine zf (L.ValInteger 0)
        let retTp = L.Struct [(L.iT 1),(L.iT 1),(L.iT 1)]
        let s0 = L.Typed retTp L.ValUndef
        s1 <- insertValue s0 zfSet 0
        s2 <- insertValue s1 pfSet 1
        insertValue s2 cfSet 2
    MemCmp bytesPerVal valCnt ptr1 ptr2 dir ->
      case (cmpsxFromSize bytesPerVal, dir) of
        (Just cmpsx, FnConstantBool dfVal) -> Just $ do
          llvmCnt <- mkLLVMValue valCnt
          llvmPtr1 <- mkLLVMValue ptr1
          llvmPtr2 <- mkLLVMValue ptr2
          -- If the MemCmp `dir` flag is set to true, enable decrementing order
          -- for buffer pointers by setting the machine's DF.
          when dfVal $ callAsm_ noSideEffect "std" "~{flags},~{dirflag},~{fpsr},~{flags}" []
          llvmAsmRes <-
            callAsm sideEffect
                    (L.Struct [L.iT 64, L.iT 16])
                    ("mov $2,%rcx\0A\09"++ -- Load count into rcx register for cmpsx op
                     "mov $3,%rdi\0A\09"++ -- Load in ptr1
                     "mov $4,%rsi\0A\09"++ -- Load in ptr2
                     "repz "++cmpsx++"\0A\09"++ -- repeat cmpsx until zero (i.e., non-match)
                     "mov %rcx,$0\0A\09"++ -- move unvisited mem loc count into ret val 1
                     "pushfw\0A\09"++ -- push flags onto the stack (we want ZF, which was set by cmpsx)
                     "popw $1\0A\09") -- pop flags into ret val 2
                    "=r,=r,r,r,r,~{flags},~{rcx},~{rdi},~{rsi},~{dirflag},~{fpsr},~{flags}"
                    [llvmCnt, llvmPtr1, llvmPtr2]
          -- Restore the DF in needed.
          when dfVal $ callAsm_ noSideEffect "cld" "~{flags},~{dirflag},~{fpsr},~{flags}" []
          llvmUnvisitedCnt <- extractValue llvmAsmRes 0
          llvmFlags <- extractValue llvmAsmRes 1
          llvmZF <- band llvmFlags (L.ValInteger 64) -- 0b1000000
          llvmZFSet <- icmpop L.Ine llvmZF (L.ValInteger 0)
          -- sameCnt = (cnt - unvisitedCnt) - (zf ? 0 : 1);
          -- i.e., the number that are the same is the number visited minus 1 _if_ the ZF is clear
          -- and thus the last cmpsx indicated the final values were unequal.
          llvmVisitedCnt <- arithop (L.Sub False False) llvmCnt (L.typedValue llvmUnvisitedCnt)
          llvmLastValueSame <-
            selectVal (L.iT 64)
                      [(llvmZFSet, (L.Typed (L.iT 64) $ L.ValInteger 0))]
                      (L.Typed (L.iT 64) $ L.ValInteger 1)
          llvmSameCnt <- arithop (L.Sub False False) llvmVisitedCnt (L.typedValue llvmLastValueSame)
          llvmCntNonZero <- icmpop L.Ine llvmCnt (L.ValInteger 0)
          -- If there was a non-zero count we use the calculated count of the
          -- number of same values, but if the count was zero it's a degenerate
          -- case (you get an underflow) for how we calculated that number and
          -- so we need to check for that.
          selectVal (L.iT 64)
            [(llvmCntNonZero, llvmSameCnt)]
            (L.Typed (L.iT 64) $ L.ValInteger 0)
        _ -> Nothing -- no support for val size outside of [1,2,4,8] bytes or non-constant direction flag values
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
