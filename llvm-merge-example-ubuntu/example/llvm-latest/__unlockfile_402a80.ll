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
define { i64, i64, <2 x double> } @F402a80(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_402a80
block_402a80:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 402a80: mov    rax,rdi
  ; # 402a83: xor    edx,edx
  ; # 402a85: lea    rdi,[rdi+0x8c]
  ; r2 := (bv_add arg0 0x8c :: [64])
  %R2 = add i64 %a0, 140
  ; # 402a8c: mov    DWORD PTR [rax+0x8c],edx
  ; *(r2) = 0x0 :: [32]
  %r5 = inttoptr i64 %R2 to i32*
  store i32 0, i32* %r5
  ; # 402a92: lock or DWORD PTR [rsp],0x0
  ; r3 := *r1
  %r6 = inttoptr i64 %R1 to i32*
  %R3 = load i32* %r6
  ; *(r1) = r3
  %r8 = inttoptr i64 %R1 to i32*
  store i32 %R3, i32* %r8
  ; # 402a97: mov    eax,DWORD PTR [rax+0x90]
  ; r4 := (bv_add arg0 0x90 :: [64])
  %R4 = add i64 %a0, 144
  ; r5 := *r4
  %r10 = inttoptr i64 %R4 to i32*
  %R5 = load i32* %r10
  ; r6 := (uext r5 64)
  %R6 = zext i32 %R5 to i64
  ; # 402a9d: test   eax,eax
  ; r7 := (bv_eq r5 0x0 :: [32])
  %R7 = icmp eq i32 %R5, 0
  ; # 402a9f: je     402ac4
  br i1 %R7, label %subblock_402a80_1, label %subblock_402a80_2
subblock_402a80_1:
  br label %block_402ac4
subblock_402a80_2:
  br label %block_402aa1
block_402aa1:
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r10
  %R10 = phi i64 [ undef, %subblock_402a80_2 ]
  %R9 = phi i64 [ %a5, %subblock_402a80_2 ]
  %R8 = phi i64 [ %R2, %subblock_402a80_2 ]
  ; # 402aa1: mov    r8d,0xca
  ; # 402aa7: mov    edx,0x1
  ; # 402aac: mov    esi,0x81
  ; # 402ab1: mov    rax,r8
  ; # 402ab4: syscall
  ; sys_futex
  %r17 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R8, i64 129, i64 1, i64 %R10, i64 202, i64 %R9, i64 202)
  %R11 = extractvalue { i64, i1 } %r17, 0
  br label %block_402ab6
block_402ab6:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R18 = phi i128 [ undef, %block_402aa1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R17 = phi i64 [ undef, %block_402aa1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R16 = phi i64 [ undef, %block_402aa1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R15 = phi i64 [ undef, %block_402aa1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R14 = phi i64 [ undef, %block_402aa1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R13 = phi i64 [ undef, %block_402aa1 ]
  %R12 = phi i64 [ %R11, %block_402aa1 ]
  ; # 402ab6: cmp    rax,0xffffffffffffffda
  ; r19 := (bv_eq r12 0xffffffffffffffda :: [64])
  %R19 = icmp eq i64 %R12, 18446744073709551578
  ; # 402aba: jne    402ac4
  ; r20 := (bv_complement r19)
  %R20 = xor i1 %R19, -1
  br i1 %R20, label %subblock_402ab6_1, label %subblock_402ab6_2
subblock_402ab6_1:
  br label %block_402ac4
subblock_402ab6_2:
  br label %block_402abc
block_402abc:
  %R25 = phi i64 [ %R17, %subblock_402ab6_2 ]
  %R24 = phi i64 [ %R16, %subblock_402ab6_2 ]
  %R23 = phi i64 [ %R15, %subblock_402ab6_2 ]
  %R22 = phi i64 [ %R14, %subblock_402ab6_2 ]
  %R21 = phi i64 [ %R13, %subblock_402ab6_2 ]
  ; # 402abc: mov    rax,r8
  ; # 402abf: mov    rsi,rdx
  ; # 402ac2: syscall
  ; sys_futex
  %r33 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R22, i64 %R21, i64 %R21, i64 %R25, i64 %R23, i64 %R24, i64 202)
  %R26 = extractvalue { i64, i1 } %r33, 0
  br label %block_402ac4
block_402ac4:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R29 = phi i128 [ undef, %block_402abc ], [ %R18, %subblock_402ab6_1 ], [ %r0, %subblock_402a80_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R28 = phi i64 [ undef, %block_402abc ], [ %R13, %subblock_402ab6_1 ], [ 0, %subblock_402a80_1 ]
  %R27 = phi i64 [ %R26, %block_402abc ], [ %R12, %subblock_402ab6_1 ], [ %R6, %subblock_402a80_1 ]
  ; # 402ac4: ret
  %r38 = bitcast i128 %R29 to <2 x double>
  %r39 = insertvalue { i64, i64, <2 x double> } undef, i64 %R27, 0
  %r40 = insertvalue { i64, i64, <2 x double> } %r39, i64 %R28, 1
  %r41 = insertvalue { i64, i64, <2 x double> } %r40, <2 x double> %r38, 2
  ret { i64, i64, <2 x double> } %r41
failure:
  br label %failure
}