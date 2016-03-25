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
declare { i64, i64, <2 x double> } @F40217e(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402190(i64, i64, i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F402d6a(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_402d6a
block_402d6a:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 402d6a: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402d6b: mov    rbx,rdi
  ; # 402d6e: call   40217e
  %r5 = bitcast i128 %r0 to <2 x double>
  %r6 = call { i64, i64, <2 x double> } @F40217e(i64 %a0, i64 %a1, i64 %a2, <2 x double> %r5)
  %R3 = extractvalue { i64, i64, <2 x double> } %r6, 0
  %R4 = extractvalue { i64, i64, <2 x double> } %r6, 1
  %r9 = extractvalue { i64, i64, <2 x double> } %r6, 2
  %R5 = bitcast <2 x double> %r9 to i128
  br label %block_402d73
block_402d73:
  %R13 = phi i128 [ %R5, %block_402d6a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R12 = phi i64 [ undef, %block_402d6a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R11 = phi i64 [ undef, %block_402d6a ]
  %R10 = phi i64 [ %R3, %block_402d6a ]
  %R9 = phi i64 [ %R4, %block_402d6a ]
  %R8 = phi i64 [ %a0, %block_402d6a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R7 = phi i64 [ undef, %block_402d6a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R6 = phi i64 [ undef, %block_402d6a ]
  ; # 402d73: mov    rdx,QWORD PTR [rax]
  ; r14 := *r6
  %r19 = inttoptr i64 %R6 to i64*
  %R14 = load i64* %r19
  ; # 402d76: mov    QWORD PTR [rbx+0x70],rdx
  ; r15 := (bv_add r8 0x70 :: [64])
  %R15 = add i64 %R8, 112
  ; *(r15) = r14
  %r22 = inttoptr i64 %R15 to i64*
  store i64 %R14, i64* %r22
  ; # 402d7a: mov    rdx,QWORD PTR [rax]
  ; r16 := *r6
  %r23 = inttoptr i64 %R6 to i64*
  %R16 = load i64* %r23
  ; # 402d7d: test   rdx,rdx
  ; r17 := (bv_eq r16 0x0 :: [64])
  %R17 = icmp eq i64 %R16, 0
  ; # 402d80: je     402d86
  br i1 %R17, label %subblock_402d73_1, label %subblock_402d73_2
subblock_402d73_1:
  br label %block_402d86
subblock_402d73_2:
  br label %block_402d82
block_402d82:
  %R26 = phi i128 [ %R13, %subblock_402d73_2 ]
  %R25 = phi i64 [ %R12, %subblock_402d73_2 ]
  %R24 = phi i64 [ %R11, %subblock_402d73_2 ]
  %R23 = phi i64 [ %R10, %subblock_402d73_2 ]
  %R22 = phi i64 [ %R9, %subblock_402d73_2 ]
  %R21 = phi i64 [ %R8, %subblock_402d73_2 ]
  %R20 = phi i64 [ %R16, %subblock_402d73_2 ]
  %R19 = phi i64 [ %R7, %subblock_402d73_2 ]
  %R18 = phi i64 [ %R6, %subblock_402d73_2 ]
  ; # 402d82: mov    QWORD PTR [rdx+0x68],rbx
  ; r27 := (bv_add r20 0x68 :: [64])
  %R27 = add i64 %R20, 104
  ; *(r27) = r21
  %r36 = inttoptr i64 %R27 to i64*
  store i64 %R21, i64* %r36
  br label %block_402d86
block_402d86:
  %R36 = phi i128 [ %R26, %block_402d82 ], [ %R13, %subblock_402d73_1 ]
  %R35 = phi i64 [ %R25, %block_402d82 ], [ %R12, %subblock_402d73_1 ]
  %R34 = phi i64 [ %R24, %block_402d82 ], [ %R11, %subblock_402d73_1 ]
  %R33 = phi i64 [ %R23, %block_402d82 ], [ %R10, %subblock_402d73_1 ]
  %R32 = phi i64 [ %R22, %block_402d82 ], [ %R9, %subblock_402d73_1 ]
  %R31 = phi i64 [ %R21, %block_402d82 ], [ %R8, %subblock_402d73_1 ]
  %R30 = phi i64 [ %R20, %block_402d82 ], [ %R16, %subblock_402d73_1 ]
  %R29 = phi i64 [ %R19, %block_402d82 ], [ %R7, %subblock_402d73_1 ]
  %R28 = phi i64 [ %R18, %block_402d82 ], [ %R6, %subblock_402d73_1 ]
  ; # 402d86: mov    QWORD PTR [rax],rbx
  ; *(r28) = r31
  %r46 = inttoptr i64 %R28 to i64*
  store i64 %R31, i64* %r46
  ; # 402d89: call   402190
  %r47 = bitcast i128 %R36 to <2 x double>
  %r48 = call { i64, i64, <2 x double> } @F402190(i64 %R33, i64 %R32, i64 %R30, i64 %R29, i64 %R34, i64 %R35, <2 x double> %r47)
  %R37 = extractvalue { i64, i64, <2 x double> } %r48, 0
  %R38 = extractvalue { i64, i64, <2 x double> } %r48, 1
  %r51 = extractvalue { i64, i64, <2 x double> } %r48, 2
  %R39 = bitcast <2 x double> %r51 to i128
  br label %block_402d8e
block_402d8e:
  %R41 = phi i128 [ %R39, %block_402d86 ]
  %R40 = phi i64 [ %R31, %block_402d86 ]
  ; # 402d8e: mov    rax,rbx
  ; # 402d91: pop    rbx
  ; # 402d92: ret
  %r55 = bitcast i128 %R41 to <2 x double>
  %r56 = insertvalue { i64, i64, <2 x double> } undef, i64 %R40, 0
  %r57 = insertvalue { i64, i64, <2 x double> } %r56, i64 undef, 1
  %r58 = insertvalue { i64, i64, <2 x double> } %r57, <2 x double> %r55, 2
  ret { i64, i64, <2 x double> } %r58
failure:
  br label %failure
}