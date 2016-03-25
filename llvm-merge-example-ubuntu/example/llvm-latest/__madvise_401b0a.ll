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
define { i64, i64, <2 x double> } @F401b0a(i64 %a0, i64 %a1, i64 %a2) {
entry:
  br label %block_401b0a
block_401b0a:
  ; r0 := (alloca 0x10 :: [64])
  %r0 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 401b0a: movsxd rdx,edx
  ; r2 := (trunc arg2 32)
  %R2 = trunc i64 %a2 to i32
  ; r3 := (sext r2 64)
  %R3 = sext i32 %R2 to i64
  ; # 401b0d: push   rax
  ; r4 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R4 = add i64 %R1, 18446744073709551608
  ; *(r4) = unsupported (Initial register rax)
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %r6 = inttoptr i64 %R4 to i64*
  store i64 undef, i64* %r6
  ; # 401b0e: mov    eax,0x1c
  ; # 401b13: syscall
  ; sys_madvise
  %r7 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %a0, i64 %a1, i64 %R3, i64 undef, i64 undef, i64 undef, i64 28)
  %R5 = extractvalue { i64, i1 } %r7, 0
  br label %block_401b15
block_401b15:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R10 = phi i128 [ undef, %block_401b0a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R9 = phi i64 [ undef, %block_401b0a ]
  %R8 = phi i64 [ %R4, %block_401b0a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R7 = phi i64 [ undef, %block_401b0a ]
  %R6 = phi i64 [ %R5, %block_401b0a ]
  ; # 401b15: mov    rdi,rax
  ; # 401b18: call   4026b0
  ; r11 := (bv_add r8 0xfffffffffffffff8 :: [64])
  %R11 = add i64 %R8, 18446744073709551608
  ; r15 := (bv_add r11 0x8 :: [64])
  %R15 = add i64 %R11, 8
  %r16 = bitcast i128 %R10 to <2 x double>
  %r17 = call { i64, i64, <2 x double> } @F4026b0(i64 %R6, i64 %R9, i64 %R7, <2 x double> %r16)
  %R12 = extractvalue { i64, i64, <2 x double> } %r17, 0
  %R13 = extractvalue { i64, i64, <2 x double> } %r17, 1
  %r20 = extractvalue { i64, i64, <2 x double> } %r17, 2
  %R14 = bitcast <2 x double> %r20 to i128
  br label %block_401b1d
block_401b1d:
  %R18 = phi i128 [ %R14, %block_401b15 ]
  %R17 = phi i64 [ %R15, %block_401b15 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R16 = phi i64 [ undef, %block_401b15 ]
  ; # 401b1d: pop    rdx
  ; r19 := *r17
  %r25 = inttoptr i64 %R17 to i64*
  %R19 = load i64* %r25
  ; # 401b1e: ret
  %r27 = bitcast i128 %R18 to <2 x double>
  %r28 = insertvalue { i64, i64, <2 x double> } undef, i64 %R16, 0
  %r29 = insertvalue { i64, i64, <2 x double> } %r28, i64 %R19, 1
  %r30 = insertvalue { i64, i64, <2 x double> } %r29, <2 x double> %r27, 2
  ret { i64, i64, <2 x double> } %r30
failure:
  br label %failure
}