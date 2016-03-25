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
declare { i64, i64, <2 x double> } @F401b1f(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F4026b0(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F401c4a(i64 %a0, i64 %a1) {
entry:
  br label %block_401c4a
block_401c4a:
  ; r0 := (alloca 0x20 :: [64])
  %r0 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 401c4a: sub    rsp,0x18
  ; r2 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R2 = add i64 %R1, 18446744073709551592
  ; # 401c4e: mov    QWORD PTR [rsp+0x8],rdi
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; *(r3) = arg0
  %r5 = inttoptr i64 %R3 to i64*
  store i64 %a0, i64* %r5
  ; # 401c53: mov    QWORD PTR [rsp],rsi
  ; *(r2) = arg1
  %r6 = inttoptr i64 %R2 to i64*
  store i64 %a1, i64* %r6
  ; # 401c57: call   401b1f
  ; r4 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R4 = add i64 %R1, 18446744073709551584
  ; r8 := (bv_add r4 0x8 :: [64])
  %R8 = add i64 %R4, 8
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rdx
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rcx
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r8
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r9
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm0
  %r9 = bitcast i128 undef to <2 x double>
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm1
  %r10 = bitcast i128 undef to <2 x double>
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm2
  %r11 = bitcast i128 undef to <2 x double>
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm3
  %r12 = bitcast i128 undef to <2 x double>
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm4
  %r13 = bitcast i128 undef to <2 x double>
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm5
  %r14 = bitcast i128 undef to <2 x double>
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm6
  %r15 = bitcast i128 undef to <2 x double>
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm7
  %r16 = bitcast i128 undef to <2 x double>
  %r17 = call { i64, i64, <2 x double> } @F401b1f(i64 %a0, i64 %a1, i64 undef, i64 undef, i64 undef, i64 undef, <2 x double> %r9, <2 x double> %r10, <2 x double> %r11, <2 x double> %r12, <2 x double> %r13, <2 x double> %r14, <2 x double> %r15, <2 x double> %r16)
  %R5 = extractvalue { i64, i64, <2 x double> } %r17, 0
  %R6 = extractvalue { i64, i64, <2 x double> } %r17, 1
  %r20 = extractvalue { i64, i64, <2 x double> } %r17, 2
  %R7 = bitcast <2 x double> %r20 to i128
  br label %block_401c5c
block_401c5c:
  %R9 = phi i64 [ %R8, %block_401c4a ]
  ; # 401c5c: mov    eax,0xb
  ; # 401c61: mov    rsi,QWORD PTR [rsp]
  ; r10 := *r9
  %r23 = inttoptr i64 %R9 to i64*
  %R10 = load i64* %r23
  ; # 401c65: mov    rdi,QWORD PTR [rsp+0x8]
  ; r11 := (bv_add r9 0x8 :: [64])
  %R11 = add i64 %R9, 8
  ; r12 := *r11
  %r26 = inttoptr i64 %R11 to i64*
  %R12 = load i64* %r26
  ; # 401c6a: syscall
  ; sys_munmap
  %r28 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R12, i64 %R10, i64 undef, i64 undef, i64 undef, i64 undef, i64 11)
  %R13 = extractvalue { i64, i1 } %r28, 0
  br label %block_401c6c
block_401c6c:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R17 = phi i128 [ undef, %block_401c5c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R16 = phi i64 [ undef, %block_401c5c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R15 = phi i64 [ undef, %block_401c5c ]
  %R14 = phi i64 [ %R13, %block_401c5c ]
  ; # 401c6c: mov    rdi,rax
  ; # 401c6f: call   4026b0
  %r34 = bitcast i128 %R17 to <2 x double>
  %r35 = call { i64, i64, <2 x double> } @F4026b0(i64 %R14, i64 %R16, i64 %R15, <2 x double> %r34)
  %R18 = extractvalue { i64, i64, <2 x double> } %r35, 0
  %R19 = extractvalue { i64, i64, <2 x double> } %r35, 1
  %r38 = extractvalue { i64, i64, <2 x double> } %r35, 2
  %R20 = bitcast <2 x double> %r38 to i128
  br label %block_401c74
block_401c74:
  %R23 = phi i128 [ %R20, %block_401c6c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R22 = phi i64 [ undef, %block_401c6c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R21 = phi i64 [ undef, %block_401c6c ]
  ; # 401c74: add    rsp,0x18
  ; # 401c78: ret
  %r43 = bitcast i128 %R23 to <2 x double>
  %r44 = insertvalue { i64, i64, <2 x double> } undef, i64 %R21, 0
  %r45 = insertvalue { i64, i64, <2 x double> } %r44, i64 %R22, 1
  %r46 = insertvalue { i64, i64, <2 x double> } %r45, <2 x double> %r43, 2
  ret { i64, i64, <2 x double> } %r46
failure:
  br label %failure
}