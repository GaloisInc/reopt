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
define { i64, i64, <2 x double> } @F4023e2(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_4023e2
block_4023e2:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 4023e2: mov    eax,DWORD PTR [rdi]
  ; r2 := *arg0
  %r4 = inttoptr i64 %a0 to i32*
  %R2 = load i32* %r4
  ; r3 := (uext r2 64)
  %R3 = zext i32 %R2 to i64
  ; # 4023e4: test   eax,eax
  ; r4 := (bv_eq r2 0x0 :: [32])
  %R4 = icmp eq i32 %R2, 0
  ; # 4023e6: je     40241b
  br i1 %R4, label %subblock_4023e2_1, label %subblock_4023e2_2
subblock_4023e2_1:
  br label %block_40241b
subblock_4023e2_2:
  br label %block_4023e8
block_4023e8:
  %R10 = phi i128 [ %r0, %subblock_4023e2_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r10
  %R9 = phi i64 [ undef, %subblock_4023e2_2 ]
  %R8 = phi i64 [ %a5, %subblock_4023e2_2 ]
  %R7 = phi i64 [ %a0, %subblock_4023e2_2 ]
  %R6 = phi i64 [ %R1, %subblock_4023e2_2 ]
  %R5 = phi i64 [ %a2, %subblock_4023e2_2 ]
  ; # 4023e8: xor    eax,eax
  ; # 4023ea: mov    DWORD PTR [rdi],eax
  ; *(r7) = 0x0 :: [32]
  %r14 = inttoptr i64 %R7 to i32*
  store i32 0, i32* %r14
  ; # 4023ec: lock or DWORD PTR [rsp],0x0
  ; r11 := *r6
  %r15 = inttoptr i64 %R6 to i32*
  %R11 = load i32* %r15
  ; *(r6) = r11
  %r17 = inttoptr i64 %R6 to i32*
  store i32 %R11, i32* %r17
  ; # 4023f1: mov    eax,DWORD PTR [rdi+0x4]
  ; r12 := (bv_add r7 0x4 :: [64])
  %R12 = add i64 %R7, 4
  ; r13 := *r12
  %r19 = inttoptr i64 %R12 to i32*
  %R13 = load i32* %r19
  ; r14 := (uext r13 64)
  %R14 = zext i32 %R13 to i64
  ; # 4023f4: test   eax,eax
  ; r15 := (bv_eq r13 0x0 :: [32])
  %R15 = icmp eq i32 %R13, 0
  ; # 4023f6: je     40241b
  br i1 %R15, label %subblock_4023e8_1, label %subblock_4023e8_2
subblock_4023e8_1:
  br label %block_40241b
subblock_4023e8_2:
  br label %block_4023f8
block_4023f8:
  %R18 = phi i64 [ %R9, %subblock_4023e8_2 ]
  %R17 = phi i64 [ %R8, %subblock_4023e8_2 ]
  %R16 = phi i64 [ %R7, %subblock_4023e8_2 ]
  ; # 4023f8: mov    r8d,0xca
  ; # 4023fe: mov    edx,0x1
  ; # 402403: mov    esi,0x81
  ; # 402408: mov    rax,r8
  ; # 40240b: syscall
  ; sys_futex
  %r26 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R16, i64 129, i64 1, i64 %R18, i64 202, i64 %R17, i64 202)
  %R19 = extractvalue { i64, i1 } %r26, 0
  br label %block_40240d
block_40240d:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R26 = phi i128 [ undef, %block_4023f8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R25 = phi i64 [ undef, %block_4023f8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R24 = phi i64 [ undef, %block_4023f8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R23 = phi i64 [ undef, %block_4023f8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R22 = phi i64 [ undef, %block_4023f8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R21 = phi i64 [ undef, %block_4023f8 ]
  %R20 = phi i64 [ %R19, %block_4023f8 ]
  ; # 40240d: cmp    rax,0xffffffffffffffda
  ; r27 := (bv_eq r20 0xffffffffffffffda :: [64])
  %R27 = icmp eq i64 %R20, 18446744073709551578
  ; # 402411: jne    40241b
  ; r28 := (bv_complement r27)
  %R28 = xor i1 %R27, -1
  br i1 %R28, label %subblock_40240d_1, label %subblock_40240d_2
subblock_40240d_1:
  br label %block_40241b
subblock_40240d_2:
  br label %block_402413
block_402413:
  %R33 = phi i64 [ %R25, %subblock_40240d_2 ]
  %R32 = phi i64 [ %R24, %subblock_40240d_2 ]
  %R31 = phi i64 [ %R23, %subblock_40240d_2 ]
  %R30 = phi i64 [ %R22, %subblock_40240d_2 ]
  %R29 = phi i64 [ %R21, %subblock_40240d_2 ]
  ; # 402413: mov    rax,r8
  ; # 402416: mov    rsi,rdx
  ; # 402419: syscall
  ; sys_futex
  %r42 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R30, i64 %R29, i64 %R29, i64 %R33, i64 %R31, i64 %R32, i64 202)
  %R34 = extractvalue { i64, i1 } %r42, 0
  br label %block_40241b
block_40241b:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R37 = phi i128 [ undef, %block_402413 ], [ %R26, %subblock_40240d_1 ], [ %R10, %subblock_4023e8_1 ], [ %r0, %subblock_4023e2_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R36 = phi i64 [ undef, %block_402413 ], [ %R21, %subblock_40240d_1 ], [ %R5, %subblock_4023e8_1 ], [ %a2, %subblock_4023e2_1 ]
  %R35 = phi i64 [ %R34, %block_402413 ], [ %R20, %subblock_40240d_1 ], [ %R14, %subblock_4023e8_1 ], [ %R3, %subblock_4023e2_1 ]
  ; # 40241b: ret
  %r47 = bitcast i128 %R37 to <2 x double>
  %r48 = insertvalue { i64, i64, <2 x double> } undef, i64 %R35, 0
  %r49 = insertvalue { i64, i64, <2 x double> } %r48, i64 %R36, 1
  %r50 = insertvalue { i64, i64, <2 x double> } %r49, <2 x double> %r47, 2
  ret { i64, i64, <2 x double> } %r50
failure:
  br label %failure
}