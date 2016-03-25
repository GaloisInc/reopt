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
declare { i64, i64, <2 x double> } @F402a2c(i64, <2 x double>)
define { i64, i64, <2 x double> } @F402e67(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_402e67
block_402e67:
  ; r0 := (alloca 0x10 :: [64])
  %r8 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 402e67: test   rdi,rdi
  ; r2 := (bv_eq arg0 0x0 :: [64])
  %R2 = icmp eq i64 %a0, 0
  ; # 402e6a: je     402eb3
  br i1 %R2, label %subblock_402e67_1, label %subblock_402e67_2
subblock_402e67_1:
  br label %block_402eb3
subblock_402e67_2:
  br label %block_402e6c
block_402e6c:
  %R16 = phi i128 [ %r7, %subblock_402e67_2 ]
  %R15 = phi i128 [ %r6, %subblock_402e67_2 ]
  %R14 = phi i128 [ %r5, %subblock_402e67_2 ]
  %R13 = phi i128 [ %r4, %subblock_402e67_2 ]
  %R12 = phi i128 [ %r3, %subblock_402e67_2 ]
  %R11 = phi i128 [ %r2, %subblock_402e67_2 ]
  %R10 = phi i128 [ %r1, %subblock_402e67_2 ]
  %R9 = phi i128 [ %r0, %subblock_402e67_2 ]
  %R8 = phi i64 [ %a5, %subblock_402e67_2 ]
  %R7 = phi i64 [ %a4, %subblock_402e67_2 ]
  %R6 = phi i64 [ %a0, %subblock_402e67_2 ]
  %R5 = phi i64 [ %R1, %subblock_402e67_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbx
  %R4 = phi i64 [ undef, %subblock_402e67_2 ]
  %R3 = phi i64 [ %a3, %subblock_402e67_2 ]
  ; # 402e6c: push   rbx
  ; r17 := (bv_add r5 0xfffffffffffffff8 :: [64])
  %R17 = add i64 %R5, 18446744073709551608
  ; *(r17) = r4
  %r27 = inttoptr i64 %R17 to i64*
  store i64 %R4, i64* %r27
  ; # 402e6d: mov    eax,DWORD PTR [rdi+0x8c]
  ; r18 := (bv_add r6 0x8c :: [64])
  %R18 = add i64 %R6, 140
  ; r19 := *r18
  %r29 = inttoptr i64 %R18 to i32*
  %R19 = load i32* %r29
  ; # 402e73: mov    rbx,rdi
  ; # 402e76: test   eax,eax
  ; r20 := (bv_slt r19 0x0 :: [32])
  %R20 = icmp slt i32 %R19, 0
  ; # 402e78: js     402e7f
  br i1 %R20, label %subblock_402e6c_1, label %subblock_402e6c_2
subblock_402e6c_1:
  br label %block_402e7f
subblock_402e6c_2:
  br label %block_402e7a
block_402e7a:
  %R23 = phi i128 [ %R9, %subblock_402e6c_2 ]
  %R22 = phi i64 [ %R6, %subblock_402e6c_2 ]
  %R21 = phi i64 [ %R6, %subblock_402e6c_2 ]
  ; # 402e7a: call   402a2c
  %r35 = bitcast i128 %R23 to <2 x double>
  %r36 = call { i64, i64, <2 x double> } @F402a2c(i64 %R22, <2 x double> %r35)
  %R24 = extractvalue { i64, i64, <2 x double> } %r36, 0
  %R25 = extractvalue { i64, i64, <2 x double> } %r36, 1
  %r39 = extractvalue { i64, i64, <2 x double> } %r36, 2
  %R26 = bitcast <2 x double> %r39 to i128
  br label %block_402e7f
block_402e7f:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R38 = phi i128 [ undef, %block_402e7a ], [ %R16, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R37 = phi i128 [ undef, %block_402e7a ], [ %R15, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R36 = phi i128 [ undef, %block_402e7a ], [ %R14, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R35 = phi i128 [ undef, %block_402e7a ], [ %R13, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R34 = phi i128 [ undef, %block_402e7a ], [ %R12, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R33 = phi i128 [ undef, %block_402e7a ], [ %R11, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R32 = phi i128 [ undef, %block_402e7a ], [ %R10, %subblock_402e6c_1 ]
  %R31 = phi i128 [ %R26, %block_402e7a ], [ %R9, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R30 = phi i64 [ undef, %block_402e7a ], [ %R8, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R29 = phi i64 [ undef, %block_402e7a ], [ %R7, %subblock_402e6c_1 ]
  %R28 = phi i64 [ %R21, %block_402e7a ], [ %R6, %subblock_402e6c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R27 = phi i64 [ undef, %block_402e7a ], [ %R3, %subblock_402e6c_1 ]
  ; # 402e7f: mov    rax,QWORD PTR [rbx+0x38]
  ; r39 := (bv_add r28 0x38 :: [64])
  %R39 = add i64 %R28, 56
  ; r40 := *r39
  %r54 = inttoptr i64 %R39 to i64*
  %R40 = load i64* %r54
  ; # 402e83: cmp    QWORD PTR [rbx+0x28],rax
  ; r41 := (bv_add r28 0x28 :: [64])
  %R41 = add i64 %R28, 40
  ; r42 := *r41
  %r57 = inttoptr i64 %R41 to i64*
  %R42 = load i64* %r57
  ; # 402e87: jbe    402e93
  ; r43 := (bv_ule r42 r40)
  %R43 = icmp ule i64 %R42, %R40
  br i1 %R43, label %subblock_402e7f_1, label %subblock_402e7f_2
subblock_402e7f_1:
  br label %block_402e93
subblock_402e7f_2:
  br label %block_402e89
block_402e89:
  %R55 = phi i128 [ %R38, %subblock_402e7f_2 ]
  %R54 = phi i128 [ %R37, %subblock_402e7f_2 ]
  %R53 = phi i128 [ %R36, %subblock_402e7f_2 ]
  %R52 = phi i128 [ %R35, %subblock_402e7f_2 ]
  %R51 = phi i128 [ %R34, %subblock_402e7f_2 ]
  %R50 = phi i128 [ %R33, %subblock_402e7f_2 ]
  %R49 = phi i128 [ %R32, %subblock_402e7f_2 ]
  %R48 = phi i128 [ %R31, %subblock_402e7f_2 ]
  %R47 = phi i64 [ %R30, %subblock_402e7f_2 ]
  %R46 = phi i64 [ %R29, %subblock_402e7f_2 ]
  %R45 = phi i64 [ %R28, %subblock_402e7f_2 ]
  %R44 = phi i64 [ %R27, %subblock_402e7f_2 ]
  ; # 402e89: xor    edx,edx
  ; # 402e8b: xor    esi,esi
  ; # 402e8d: mov    rdi,rbx
  ; # 402e90: call   QWORD PTR [rbx+0x48]
  ; r56 := (bv_add r45 0x48 :: [64])
  %R56 = add i64 %R45, 72
  ; r57 := *r56
  %r73 = inttoptr i64 %R56 to i64*
  %R57 = load i64* %r73
  %r75 = inttoptr i64 %R57 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r76 = bitcast i128 %R48 to <2 x double>
  %r77 = bitcast i128 %R49 to <2 x double>
  %r78 = bitcast i128 %R50 to <2 x double>
  %r79 = bitcast i128 %R51 to <2 x double>
  %r80 = bitcast i128 %R52 to <2 x double>
  %r81 = bitcast i128 %R53 to <2 x double>
  %r82 = bitcast i128 %R54 to <2 x double>
  %r83 = bitcast i128 %R55 to <2 x double>
  %r84 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r75(i64 %R45, i64 0, i64 0, i64 %R44, i64 %R46, i64 %R47, <2 x double> %r76, <2 x double> %r77, <2 x double> %r78, <2 x double> %r79, <2 x double> %r80, <2 x double> %r81, <2 x double> %r82, <2 x double> %r83)
  %R58 = extractvalue { i64, i64, <2 x double> } %r84, 0
  %R59 = extractvalue { i64, i64, <2 x double> } %r84, 1
  %r87 = extractvalue { i64, i64, <2 x double> } %r84, 2
  %R60 = bitcast <2 x double> %r87 to i128
  br label %block_402e93
block_402e93:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R72 = phi i128 [ undef, %block_402e89 ], [ %R38, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R71 = phi i128 [ undef, %block_402e89 ], [ %R37, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R70 = phi i128 [ undef, %block_402e89 ], [ %R36, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R69 = phi i128 [ undef, %block_402e89 ], [ %R35, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R68 = phi i128 [ undef, %block_402e89 ], [ %R34, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R67 = phi i128 [ undef, %block_402e89 ], [ %R33, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R66 = phi i128 [ undef, %block_402e89 ], [ %R32, %subblock_402e7f_1 ]
  %R65 = phi i128 [ %R60, %block_402e89 ], [ %R31, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R64 = phi i64 [ undef, %block_402e89 ], [ %R30, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R63 = phi i64 [ undef, %block_402e89 ], [ %R29, %subblock_402e7f_1 ]
  %R62 = phi i64 [ %R45, %block_402e89 ], [ %R28, %subblock_402e7f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R61 = phi i64 [ undef, %block_402e89 ], [ %R27, %subblock_402e7f_1 ]
  ; # 402e93: mov    rsi,QWORD PTR [rbx+0x8]
  ; r73 := (bv_add r62 0x8 :: [64])
  %R73 = add i64 %R62, 8
  ; r74 := *r73
  %r102 = inttoptr i64 %R73 to i64*
  %R74 = load i64* %r102
  ; # 402e97: mov    rax,QWORD PTR [rbx+0x10]
  ; r75 := (bv_add r62 0x10 :: [64])
  %R75 = add i64 %R62, 16
  ; r76 := *r75
  %r105 = inttoptr i64 %R75 to i64*
  %R76 = load i64* %r105
  ; # 402e9b: cmp    rsi,rax
  ; # 402e9e: jae    402eb2
  ; r77 := (bv_ule r76 r74)
  %R77 = icmp ule i64 %R76, %R74
  br i1 %R77, label %subblock_402e93_1, label %subblock_402e93_2
subblock_402e93_1:
  br label %block_402eb2
subblock_402e93_2:
  br label %block_402ea0
block_402ea0:
  %R91 = phi i128 [ %R72, %subblock_402e93_2 ]
  %R90 = phi i128 [ %R71, %subblock_402e93_2 ]
  %R89 = phi i128 [ %R70, %subblock_402e93_2 ]
  %R88 = phi i128 [ %R69, %subblock_402e93_2 ]
  %R87 = phi i128 [ %R68, %subblock_402e93_2 ]
  %R86 = phi i128 [ %R67, %subblock_402e93_2 ]
  %R85 = phi i128 [ %R66, %subblock_402e93_2 ]
  %R84 = phi i128 [ %R65, %subblock_402e93_2 ]
  %R83 = phi i64 [ %R64, %subblock_402e93_2 ]
  %R82 = phi i64 [ %R63, %subblock_402e93_2 ]
  %R81 = phi i64 [ %R74, %subblock_402e93_2 ]
  %R80 = phi i64 [ %R62, %subblock_402e93_2 ]
  %R79 = phi i64 [ %R61, %subblock_402e93_2 ]
  %R78 = phi i64 [ %R76, %subblock_402e93_2 ]
  ; # 402ea0: sub    rsi,rax
  ; r92 := (bv_sub r81 r78)
  %R92 = sub i64 %R81, %R78
  ; # 402ea3: mov    rdi,rbx
  ; # 402ea6: mov    rax,QWORD PTR [rbx+0x50]
  ; r93 := (bv_add r80 0x50 :: [64])
  %R93 = add i64 %R80, 80
  ; r94 := *r93
  %r124 = inttoptr i64 %R93 to i64*
  %R94 = load i64* %r124
  ; # 402eaa: pop    rbx
  ; # 402eab: mov    edx,0x1
  ; # 402eb0: jmp    rax
  %r126 = inttoptr i64 %R94 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r127 = bitcast i128 %R84 to <2 x double>
  %r128 = bitcast i128 %R85 to <2 x double>
  %r129 = bitcast i128 %R86 to <2 x double>
  %r130 = bitcast i128 %R87 to <2 x double>
  %r131 = bitcast i128 %R88 to <2 x double>
  %r132 = bitcast i128 %R89 to <2 x double>
  %r133 = bitcast i128 %R90 to <2 x double>
  %r134 = bitcast i128 %R91 to <2 x double>
  %r135 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r126(i64 %R80, i64 %R92, i64 1, i64 %R79, i64 %R82, i64 %R83, <2 x double> %r127, <2 x double> %r128, <2 x double> %r129, <2 x double> %r130, <2 x double> %r131, <2 x double> %r132, <2 x double> %r133, <2 x double> %r134)
  ret { i64, i64, <2 x double> } %r135
block_402eb2:
  %R98 = phi i128 [ %R65, %subblock_402e93_1 ]
  ; # 402eb2: pop    rbx
  br label %block_402eb3
block_402eb3:
  %R99 = phi i128 [ %R98, %block_402eb2 ], [ %r0, %subblock_402e67_1 ]
  ; # 402eb3: ret
  %r138 = bitcast i128 %R99 to <2 x double>
  %r139 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r140 = insertvalue { i64, i64, <2 x double> } %r139, i64 undef, 1
  %r141 = insertvalue { i64, i64, <2 x double> } %r140, <2 x double> %r138, 2
  ret { i64, i64, <2 x double> } %r141
failure:
  br label %failure
}