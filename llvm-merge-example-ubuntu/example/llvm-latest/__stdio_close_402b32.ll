declare i1 @reopt.EvenParity(i8)
declare i2 @reopt.Read_X87_RC()
declare void @reopt.Write_X87_RC(i2)
declare i2 @reopt.Read_X87_PC()
declare void @reopt.Write_X87_PC(i2)
declare i16 @reopt.Read_FS()
declare void @reopt.Write_FS(i16)
declare i16 @reopt.Read_GS()
declare void @reopt.Write_GS(i16)
declare i64 @reopt.MemCmp(i64, i64, i64, i64, i1)
declare { i64, i1 } @reopt.SystemCall.Linux(i64, i64, i64, i64, i64, i64, i64)
declare { i64, i1 } @reopt.SystemCall.FreeBSD(i64, i64, i64, i64, i64, i64, i64)
declare void @reopt.MemCopy.i8(i64, i64, i64, i1)
declare void @reopt.MemCopy.i16(i64, i64, i64, i1)
declare void @reopt.MemCopy.i32(i64, i64, i64, i1)
declare void @reopt.MemCopy.i64(i64, i64, i64, i1)
declare void @reopt.MemSet.i8(i8*, i8, i64, i1)
declare void @reopt.MemSet.i16(i16*, i16, i64, i1)
declare void @reopt.MemSet.i32(i32*, i32, i64, i1)
declare void @reopt.MemSet.i64(i64*, i64, i64, i1)
declare { i8, i1 } @llvm.uadd.with.overflow.i8(i8, i8)
declare { i16, i1 } @llvm.uadd.with.overflow.i16(i16, i16)
declare { i32, i1 } @llvm.uadd.with.overflow.i32(i32, i32)
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64)
declare { i8, i1 } @llvm.sadd.with.overflow.i8(i8, i8)
declare { i16, i1 } @llvm.sadd.with.overflow.i16(i16, i16)
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64)
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8)
declare { i16, i1 } @llvm.usub.with.overflow.i16(i16, i16)
declare { i32, i1 } @llvm.usub.with.overflow.i32(i32, i32)
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64)
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8)
declare { i16, i1 } @llvm.ssub.with.overflow.i16(i16, i16)
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32)
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64)
declare i8 @llvm.cttz.i8(i8, i1)
declare i16 @llvm.cttz.i16(i16, i1)
declare i32 @llvm.cttz.i32(i32, i1)
declare i64 @llvm.cttz.i64(i64, i1)
declare i8 @llvm.ctlz.i8(i8, i1)
declare i16 @llvm.ctlz.i16(i16, i1)
declare i32 @llvm.ctlz.i32(i32, i1)
declare i64 @llvm.ctlz.i64(i64, i1)
declare { i64, i64, <2 x double> } @F4026b0(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402b2f(i64)
define { i64, i64, <2 x double> } @F402b32(i64 %a0) {
entry:
  br label %block_402b32
block_402b32:
  ; r0 := (alloca 0x10 :: [64])
  %r0 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 402b32: push   rax
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; *(r2) = unsupported (Initial register rax)
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %r4 = inttoptr i64 %R2 to i64*
  store i64 undef, i64* %r4
  ; # 402b33: mov    edi,DWORD PTR [rdi+0x78]
  ; r3 := (bv_add arg0 0x78 :: [64])
  %R3 = add i64 %a0, 120
  ; r4 := *r3
  %r6 = inttoptr i64 %R3 to i32*
  %R4 = load i32* %r6
  ; r5 := (uext r4 64)
  %R5 = zext i32 %R4 to i64
  ; # 402b36: call   402b2f
  %r9 = call { i64, i64, <2 x double> } @F402b2f(i64 %R5)
  %R6 = extractvalue { i64, i64, <2 x double> } %r9, 0
  br label %block_402b3b
block_402b3b:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R7 = phi i64 [ undef, %block_402b32 ]
  ; # 402b3b: movsxd rdi,eax
  ; r8 := (trunc r7 32)
  %R8 = trunc i64 %R7 to i32
  ; r9 := (sext r8 64)
  %R9 = sext i32 %R8 to i64
  ; # 402b3e: mov    eax,0x3
  ; # 402b43: syscall
  ; sys_close
  %r14 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R9, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 3)
  %R10 = extractvalue { i64, i1 } %r14, 0
  br label %block_402b45
block_402b45:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R14 = phi i128 [ undef, %block_402b3b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R13 = phi i64 [ undef, %block_402b3b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R12 = phi i64 [ undef, %block_402b3b ]
  %R11 = phi i64 [ %R10, %block_402b3b ]
  ; # 402b45: mov    rdi,rax
  ; # 402b48: call   4026b0
  %r20 = bitcast i128 %R14 to <2 x double>
  %r21 = call { i64, i64, <2 x double> } @F4026b0(i64 %R11, i64 %R13, i64 %R12, <2 x double> %r20)
  %R15 = extractvalue { i64, i64, <2 x double> } %r21, 0
  %R16 = extractvalue { i64, i64, <2 x double> } %r21, 1
  %r24 = extractvalue { i64, i64, <2 x double> } %r21, 2
  %R17 = bitcast <2 x double> %r24 to i128
  br label %block_402b4d
block_402b4d:
  ; # 402b4d: pop    rdx
  ; # 402b4e: ret
  %r26 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r27 = insertvalue { i64, i64, <2 x double> } %r26, i64 undef, 1
  %r28 = insertvalue { i64, i64, <2 x double> } %r27, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r28
failure:
  br label %failure
}