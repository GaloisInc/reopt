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
declare { i64, i64, <2 x double> } @F401f8a(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F402023(i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400335(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_400335
block_400335:
  ; r0 := (alloca 0x20 :: [64])
  %r8 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 400335: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 400336: mov    rbp,rsp
  ; # 400339: sub    rsp,0x10
  ; r3 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R3 = add i64 %R1, 18446744073709551592
  ; # 40033d: mov    QWORD PTR [rbp-0x8],rdi
  ; r4 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R4 = add i64 %R1, 18446744073709551600
  ; *(r4) = arg0
  %r14 = inttoptr i64 %R4 to i64*
  store i64 %a0, i64* %r14
  ; # 400341: mov    QWORD PTR [rbp-0x10],rsi
  ; *(r3) = arg1
  %r15 = inttoptr i64 %R3 to i64*
  store i64 %a1, i64* %r15
  ; # 400345: cmp    QWORD PTR [rbp-0x10],0x0
  ; r5 := *r3
  %r16 = inttoptr i64 %R3 to i64*
  %R5 = load i64* %r16
  ; r6 := (bv_eq r5 0x0 :: [64])
  %R6 = icmp eq i64 %R5, 0
  ; # 40034a: je     4003a8
  br i1 %R6, label %subblock_400335_1, label %subblock_400335_2
subblock_400335_1:
  br label %block_4003a8
subblock_400335_2:
  br label %block_40034c
block_40034c:
  %R8 = phi i128 [ %r0, %subblock_400335_2 ]
  %R7 = phi i64 [ %R2, %subblock_400335_2 ]
  ; # 40034c: mov    rax,QWORD PTR [rbp-0x10]
  ; r9 := (bv_add r7 0xfffffffffffffff0 :: [64])
  %R9 = add i64 %R7, 18446744073709551600
  ; r10 := *r9
  %r22 = inttoptr i64 %R9 to i64*
  %R10 = load i64* %r22
  ; # 400350: mov    rax,QWORD PTR [rax+0x10]
  ; r11 := (bv_add r10 0x10 :: [64])
  %R11 = add i64 %R10, 16
  ; r12 := *r11
  %r25 = inttoptr i64 %R11 to i64*
  %R12 = load i64* %r25
  ; # 400354: mov    rdx,QWORD PTR [rbp-0x8]
  ; r13 := (bv_add r7 0xfffffffffffffff8 :: [64])
  %R13 = add i64 %R7, 18446744073709551608
  ; r14 := *r13
  %r28 = inttoptr i64 %R13 to i64*
  %R14 = load i64* %r28
  ; # 400358: mov    rsi,rdx
  ; # 40035b: mov    rdi,rax
  ; # 40035e: call   402023
  %r30 = bitcast i128 %R8 to <2 x double>
  %r31 = call { i64, i64, <2 x double> } @F402023(i64 %R12, i64 %R14, <2 x double> %r30)
  %r32 = extractvalue { i64, i64, <2 x double> } %r31, 2
  %R15 = bitcast <2 x double> %r32 to i128
  br label %block_400363
block_400363:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R27 = phi i128 [ undef, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R26 = phi i128 [ undef, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R25 = phi i128 [ undef, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R24 = phi i128 [ undef, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R23 = phi i128 [ undef, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R22 = phi i128 [ undef, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R21 = phi i128 [ undef, %block_40034c ]
  %R20 = phi i128 [ %R15, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R19 = phi i64 [ undef, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R18 = phi i64 [ undef, %block_40034c ]
  %R17 = phi i64 [ %R7, %block_40034c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R16 = phi i64 [ undef, %block_40034c ]
  ; # 400363: mov    eax,0x2c
  ; # 400368: movsx  eax,al
  ; r28 := (sext 0x2c :: [8] 32)
  %R28 = sext i8 44 to i32
  ; r29 := (uext r28 64)
  %R29 = zext i32 %R28 to i64
  ; # 40036b: mov    rdx,QWORD PTR [rbp-0x8]
  ; r30 := (bv_add r17 0xfffffffffffffff8 :: [64])
  %R30 = add i64 %R17, 18446744073709551608
  ; r31 := *r30
  %r49 = inttoptr i64 %R30 to i64*
  %R31 = load i64* %r49
  ; # 40036f: mov    rsi,rdx
  ; # 400372: mov    edi,eax
  ; # 400374: call   401f8a
  %r51 = bitcast i128 %R20 to <2 x double>
  %r52 = bitcast i128 %R21 to <2 x double>
  %r53 = bitcast i128 %R22 to <2 x double>
  %r54 = bitcast i128 %R23 to <2 x double>
  %r55 = bitcast i128 %R24 to <2 x double>
  %r56 = bitcast i128 %R25 to <2 x double>
  %r57 = bitcast i128 %R26 to <2 x double>
  %r58 = bitcast i128 %R27 to <2 x double>
  %r59 = call { i64, i64, <2 x double> } @F401f8a(i64 %R29, i64 %R31, i64 %R31, i64 %R16, i64 %R18, i64 %R19, <2 x double> %r51, <2 x double> %r52, <2 x double> %r53, <2 x double> %r54, <2 x double> %r55, <2 x double> %r56, <2 x double> %r57, <2 x double> %r58)
  %R32 = extractvalue { i64, i64, <2 x double> } %r59, 0
  %R33 = extractvalue { i64, i64, <2 x double> } %r59, 1
  %r62 = extractvalue { i64, i64, <2 x double> } %r59, 2
  %R34 = bitcast <2 x double> %r62 to i128
  br label %block_400379
block_400379:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R46 = phi i128 [ undef, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R45 = phi i128 [ undef, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R44 = phi i128 [ undef, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R43 = phi i128 [ undef, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R42 = phi i128 [ undef, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R41 = phi i128 [ undef, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R40 = phi i128 [ undef, %block_400363 ]
  %R39 = phi i128 [ %R34, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R38 = phi i64 [ undef, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R37 = phi i64 [ undef, %block_400363 ]
  %R36 = phi i64 [ %R17, %block_400363 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R35 = phi i64 [ undef, %block_400363 ]
  ; # 400379: mov    rax,QWORD PTR [rbp-0x10]
  ; r47 := (bv_add r36 0xfffffffffffffff0 :: [64])
  %R47 = add i64 %R36, 18446744073709551600
  ; r48 := *r47
  %r77 = inttoptr i64 %R47 to i64*
  %R48 = load i64* %r77
  ; # 40037d: mov    rdx,QWORD PTR [rax]
  ; r49 := *r48
  %r79 = inttoptr i64 %R48 to i64*
  %R49 = load i64* %r79
  ; # 400380: mov    rax,QWORD PTR [rbp-0x8]
  ; r50 := (bv_add r36 0xfffffffffffffff8 :: [64])
  %R50 = add i64 %R36, 18446744073709551608
  ; r51 := *r50
  %r82 = inttoptr i64 %R50 to i64*
  %R51 = load i64* %r82
  ; # 400384: mov    rsi,rdx
  ; # 400387: mov    rdi,rax
  ; # 40038a: call   400335
  %r84 = bitcast i128 %R39 to <2 x double>
  %r85 = bitcast i128 %R40 to <2 x double>
  %r86 = bitcast i128 %R41 to <2 x double>
  %r87 = bitcast i128 %R42 to <2 x double>
  %r88 = bitcast i128 %R43 to <2 x double>
  %r89 = bitcast i128 %R44 to <2 x double>
  %r90 = bitcast i128 %R45 to <2 x double>
  %r91 = bitcast i128 %R46 to <2 x double>
  %r92 = call { i64, i64, <2 x double> } @F400335(i64 %R51, i64 %R49, i64 %R49, i64 %R35, i64 %R37, i64 %R38, <2 x double> %r84, <2 x double> %r85, <2 x double> %r86, <2 x double> %r87, <2 x double> %r88, <2 x double> %r89, <2 x double> %r90, <2 x double> %r91)
  %R52 = extractvalue { i64, i64, <2 x double> } %r92, 0
  %R53 = extractvalue { i64, i64, <2 x double> } %r92, 1
  %r95 = extractvalue { i64, i64, <2 x double> } %r92, 2
  %R54 = bitcast <2 x double> %r95 to i128
  br label %block_40038f
block_40038f:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R66 = phi i128 [ undef, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R65 = phi i128 [ undef, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R64 = phi i128 [ undef, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R63 = phi i128 [ undef, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R62 = phi i128 [ undef, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R61 = phi i128 [ undef, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R60 = phi i128 [ undef, %block_400379 ]
  %R59 = phi i128 [ %R54, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R58 = phi i64 [ undef, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R57 = phi i64 [ undef, %block_400379 ]
  %R56 = phi i64 [ %R36, %block_400379 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R55 = phi i64 [ undef, %block_400379 ]
  ; # 40038f: mov    rax,QWORD PTR [rbp-0x10]
  ; r67 := (bv_add r56 0xfffffffffffffff0 :: [64])
  %R67 = add i64 %R56, 18446744073709551600
  ; r68 := *r67
  %r110 = inttoptr i64 %R67 to i64*
  %R68 = load i64* %r110
  ; # 400393: mov    rdx,QWORD PTR [rax+0x8]
  ; r69 := (bv_add r68 0x8 :: [64])
  %R69 = add i64 %R68, 8
  ; r70 := *r69
  %r113 = inttoptr i64 %R69 to i64*
  %R70 = load i64* %r113
  ; # 400397: mov    rax,QWORD PTR [rbp-0x8]
  ; r71 := (bv_add r56 0xfffffffffffffff8 :: [64])
  %R71 = add i64 %R56, 18446744073709551608
  ; r72 := *r71
  %r116 = inttoptr i64 %R71 to i64*
  %R72 = load i64* %r116
  ; # 40039b: mov    rsi,rdx
  ; # 40039e: mov    rdi,rax
  ; # 4003a1: call   400335
  %r118 = bitcast i128 %R59 to <2 x double>
  %r119 = bitcast i128 %R60 to <2 x double>
  %r120 = bitcast i128 %R61 to <2 x double>
  %r121 = bitcast i128 %R62 to <2 x double>
  %r122 = bitcast i128 %R63 to <2 x double>
  %r123 = bitcast i128 %R64 to <2 x double>
  %r124 = bitcast i128 %R65 to <2 x double>
  %r125 = bitcast i128 %R66 to <2 x double>
  %r126 = call { i64, i64, <2 x double> } @F400335(i64 %R72, i64 %R70, i64 %R70, i64 %R55, i64 %R57, i64 %R58, <2 x double> %r118, <2 x double> %r119, <2 x double> %r120, <2 x double> %r121, <2 x double> %r122, <2 x double> %r123, <2 x double> %r124, <2 x double> %r125)
  %R73 = extractvalue { i64, i64, <2 x double> } %r126, 0
  %R74 = extractvalue { i64, i64, <2 x double> } %r126, 1
  %r129 = extractvalue { i64, i64, <2 x double> } %r126, 2
  %R75 = bitcast <2 x double> %r129 to i128
  br label %block_4003a6
block_4003a6:
  %R78 = phi i128 [ %R75, %block_40038f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R77 = phi i64 [ undef, %block_40038f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R76 = phi i64 [ undef, %block_40038f ]
  ; # 4003a6: jmp    4003be
  br label %block_4003be
block_4003a8:
  %R90 = phi i128 [ %r7, %subblock_400335_1 ]
  %R89 = phi i128 [ %r6, %subblock_400335_1 ]
  %R88 = phi i128 [ %r5, %subblock_400335_1 ]
  %R87 = phi i128 [ %r4, %subblock_400335_1 ]
  %R86 = phi i128 [ %r3, %subblock_400335_1 ]
  %R85 = phi i128 [ %r2, %subblock_400335_1 ]
  %R84 = phi i128 [ %r1, %subblock_400335_1 ]
  %R83 = phi i128 [ %r0, %subblock_400335_1 ]
  %R82 = phi i64 [ %a5, %subblock_400335_1 ]
  %R81 = phi i64 [ %a4, %subblock_400335_1 ]
  %R80 = phi i64 [ %R2, %subblock_400335_1 ]
  %R79 = phi i64 [ %a3, %subblock_400335_1 ]
  ; # 4003a8: mov    eax,0x2c
  ; # 4003ad: movsx  eax,al
  ; r91 := (sext 0x2c :: [8] 32)
  %R91 = sext i8 44 to i32
  ; r92 := (uext r91 64)
  %R92 = zext i32 %R91 to i64
  ; # 4003b0: mov    rdx,QWORD PTR [rbp-0x8]
  ; r93 := (bv_add r80 0xfffffffffffffff8 :: [64])
  %R93 = add i64 %R80, 18446744073709551608
  ; r94 := *r93
  %r149 = inttoptr i64 %R93 to i64*
  %R94 = load i64* %r149
  ; # 4003b4: mov    rsi,rdx
  ; # 4003b7: mov    edi,eax
  ; # 4003b9: call   401f8a
  %r151 = bitcast i128 %R83 to <2 x double>
  %r152 = bitcast i128 %R84 to <2 x double>
  %r153 = bitcast i128 %R85 to <2 x double>
  %r154 = bitcast i128 %R86 to <2 x double>
  %r155 = bitcast i128 %R87 to <2 x double>
  %r156 = bitcast i128 %R88 to <2 x double>
  %r157 = bitcast i128 %R89 to <2 x double>
  %r158 = bitcast i128 %R90 to <2 x double>
  %r159 = call { i64, i64, <2 x double> } @F401f8a(i64 %R92, i64 %R94, i64 %R94, i64 %R79, i64 %R81, i64 %R82, <2 x double> %r151, <2 x double> %r152, <2 x double> %r153, <2 x double> %r154, <2 x double> %r155, <2 x double> %r156, <2 x double> %r157, <2 x double> %r158)
  %R95 = extractvalue { i64, i64, <2 x double> } %r159, 0
  %R96 = extractvalue { i64, i64, <2 x double> } %r159, 1
  %r162 = extractvalue { i64, i64, <2 x double> } %r159, 2
  %R97 = bitcast <2 x double> %r162 to i128
  br label %block_4003be
block_4003be:
  %R100 = phi i128 [ %R78, %block_4003a6 ], [ %R97, %block_4003a8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R99 = phi i64 [ %R77, %block_4003a6 ], [ undef, %block_4003a8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R98 = phi i64 [ %R76, %block_4003a6 ], [ undef, %block_4003a8 ]
  ; # 4003be: nop
  ; # 4003bf: leave
  ; # 4003c0: ret
  %r167 = bitcast i128 %R100 to <2 x double>
  %r168 = insertvalue { i64, i64, <2 x double> } undef, i64 %R98, 0
  %r169 = insertvalue { i64, i64, <2 x double> } %r168, i64 %R99, 1
  %r170 = insertvalue { i64, i64, <2 x double> } %r169, <2 x double> %r167, 2
  ret { i64, i64, <2 x double> } %r170
failure:
  br label %failure
}