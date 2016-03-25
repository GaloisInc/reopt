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
declare { i64, i64, <2 x double> } @F402375(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402ce8(i64, <2 x double>)
define { i64, i64, <2 x double> } @F402058(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_402058
block_402058:
  ; r0 := (alloca 0x30 :: [64])
  %r8 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 402058: push   r13
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 40205a: push   r12
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 40205c: mov    r13,rdi
  ; # 40205f: push   rbp
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 402060: push   rbx
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 402061: mov    r12,rsi
  ; # 402064: push   r8
  ; r6 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R6 = add i64 %R1, 18446744073709551576
  ; *(r6) = arg4
  %r16 = inttoptr i64 %R6 to i64*
  store i64 %a4, i64* %r16
  ; # 402066: cmp    QWORD PTR [rdx+0x20],0x0
  ; r7 := (bv_add arg2 0x20 :: [64])
  %R7 = add i64 %a2, 32
  ; r8 := *r7
  %r18 = inttoptr i64 %R7 to i64*
  %R8 = load i64* %r18
  ; r9 := (bv_eq r8 0x0 :: [64])
  %R9 = icmp eq i64 %R8, 0
  ; # 40206b: mov    rbx,rdx
  ; # 40206e: je     402093
  br i1 %R9, label %subblock_402058_1, label %subblock_402058_2
subblock_402058_1:
  br label %block_402093
subblock_402058_2:
  br label %block_402070
block_402070:
  %R24 = phi i128 [ %R77, %subblock_40209b_1 ], [ %r7, %subblock_402058_2 ]
  %R23 = phi i128 [ %R76, %subblock_40209b_1 ], [ %r6, %subblock_402058_2 ]
  %R22 = phi i128 [ %R75, %subblock_40209b_1 ], [ %r5, %subblock_402058_2 ]
  %R21 = phi i128 [ %R74, %subblock_40209b_1 ], [ %r4, %subblock_402058_2 ]
  %R20 = phi i128 [ %R73, %subblock_40209b_1 ], [ %r3, %subblock_402058_2 ]
  %R19 = phi i128 [ %R72, %subblock_40209b_1 ], [ %r2, %subblock_402058_2 ]
  %R18 = phi i128 [ %R71, %subblock_40209b_1 ], [ %r1, %subblock_402058_2 ]
  %R17 = phi i128 [ %R70, %subblock_40209b_1 ], [ %r0, %subblock_402058_2 ]
  %R16 = phi i64 [ %R69, %subblock_40209b_1 ], [ %a0, %subblock_402058_2 ]
  %R15 = phi i64 [ %R68, %subblock_40209b_1 ], [ %a1, %subblock_402058_2 ]
  %R14 = phi i64 [ %R67, %subblock_40209b_1 ], [ %a5, %subblock_402058_2 ]
  %R13 = phi i64 [ %R66, %subblock_40209b_1 ], [ %a4, %subblock_402058_2 ]
  %R12 = phi i64 [ %R65, %subblock_40209b_1 ], [ %R6, %subblock_402058_2 ]
  %R11 = phi i64 [ %R64, %subblock_40209b_1 ], [ %a2, %subblock_402058_2 ]
  %R10 = phi i64 [ %R63, %subblock_40209b_1 ], [ %a3, %subblock_402058_2 ]
  ; # 402070: mov    rax,QWORD PTR [rbx+0x20]
  ; r25 := (bv_add r11 0x20 :: [64])
  %R25 = add i64 %R11, 32
  ; r26 := *r25
  %r37 = inttoptr i64 %R25 to i64*
  %R26 = load i64* %r37
  ; # 402074: sub    rax,QWORD PTR [rbx+0x28]
  ; r27 := (bv_add r11 0x28 :: [64])
  %R27 = add i64 %R11, 40
  ; r28 := *r27
  %r40 = inttoptr i64 %R27 to i64*
  %R28 = load i64* %r40
  ; r29 := (bv_sub r26 r28)
  %R29 = sub i64 %R26, %R28
  ; # 402078: cmp    rax,r12
  ; # 40207b: jae    4020a3
  ; r30 := (bv_ule r15 r29)
  %R30 = icmp ule i64 %R15, %R29
  br i1 %R30, label %subblock_402070_1, label %subblock_402070_2
subblock_402070_1:
  br label %block_4020a3
subblock_402070_2:
  br label %block_40207d
block_40207d:
  %R44 = phi i128 [ %R24, %subblock_402070_2 ]
  %R43 = phi i128 [ %R23, %subblock_402070_2 ]
  %R42 = phi i128 [ %R22, %subblock_402070_2 ]
  %R41 = phi i128 [ %R21, %subblock_402070_2 ]
  %R40 = phi i128 [ %R20, %subblock_402070_2 ]
  %R39 = phi i128 [ %R19, %subblock_402070_2 ]
  %R38 = phi i128 [ %R18, %subblock_402070_2 ]
  %R37 = phi i128 [ %R17, %subblock_402070_2 ]
  %R36 = phi i64 [ %R16, %subblock_402070_2 ]
  %R35 = phi i64 [ %R15, %subblock_402070_2 ]
  %R34 = phi i64 [ %R14, %subblock_402070_2 ]
  %R33 = phi i64 [ %R13, %subblock_402070_2 ]
  %R32 = phi i64 [ %R12, %subblock_402070_2 ]
  %R31 = phi i64 [ %R11, %subblock_402070_2 ]
  ; # 40207d: mov    rax,QWORD PTR [rbx+0x48]
  ; r45 := (bv_add r31 0x48 :: [64])
  %R45 = add i64 %R31, 72
  ; r46 := *r45
  %r59 = inttoptr i64 %R45 to i64*
  %R46 = load i64* %r59
  ; # 402081: mov    rdx,r12
  ; # 402084: mov    rsi,r13
  ; # 402087: pop    rcx
  ; r47 := *r32
  %r61 = inttoptr i64 %R32 to i64*
  %R47 = load i64* %r61
  ; # 402088: mov    rdi,rbx
  ; # 40208b: pop    rbx
  ; # 40208c: pop    rbp
  ; # 40208d: pop    r12
  ; # 40208f: pop    r13
  ; # 402091: jmp    rax
  %r63 = inttoptr i64 %R46 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r64 = bitcast i128 %R37 to <2 x double>
  %r65 = bitcast i128 %R38 to <2 x double>
  %r66 = bitcast i128 %R39 to <2 x double>
  %r67 = bitcast i128 %R40 to <2 x double>
  %r68 = bitcast i128 %R41 to <2 x double>
  %r69 = bitcast i128 %R42 to <2 x double>
  %r70 = bitcast i128 %R43 to <2 x double>
  %r71 = bitcast i128 %R44 to <2 x double>
  %r72 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r63(i64 %R31, i64 %R36, i64 %R35, i64 %R47, i64 %R33, i64 %R34, <2 x double> %r64, <2 x double> %r65, <2 x double> %r66, <2 x double> %r67, <2 x double> %r68, <2 x double> %r69, <2 x double> %r70, <2 x double> %r71)
  ret { i64, i64, <2 x double> } %r72
block_402093:
  %R56 = phi i128 [ %r0, %subblock_402058_1 ]
  %R55 = phi i64 [ %a0, %subblock_402058_1 ]
  %R54 = phi i64 [ %a1, %subblock_402058_1 ]
  %R53 = phi i64 [ %R6, %subblock_402058_1 ]
  %R52 = phi i64 [ %a2, %subblock_402058_1 ]
  %R51 = phi i64 [ %a2, %subblock_402058_1 ]
  ; # 402093: mov    rdi,rdx
  ; # 402096: call   402ce8
  ; r57 := (bv_add r53 0xfffffffffffffff8 :: [64])
  %R57 = add i64 %R53, 18446744073709551608
  ; r61 := (bv_add r57 0x8 :: [64])
  %R61 = add i64 %R57, 8
  %r81 = bitcast i128 %R56 to <2 x double>
  %r82 = call { i64, i64, <2 x double> } @F402ce8(i64 %R51, <2 x double> %r81)
  %R58 = extractvalue { i64, i64, <2 x double> } %r82, 0
  %R59 = extractvalue { i64, i64, <2 x double> } %r82, 1
  %r85 = extractvalue { i64, i64, <2 x double> } %r82, 2
  %R60 = bitcast <2 x double> %r85 to i128
  br label %block_40209b
block_40209b:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R77 = phi i128 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R76 = phi i128 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R75 = phi i128 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R74 = phi i128 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R73 = phi i128 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R72 = phi i128 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R71 = phi i128 [ undef, %block_402093 ]
  %R70 = phi i128 [ %R60, %block_402093 ]
  %R69 = phi i64 [ %R55, %block_402093 ]
  %R68 = phi i64 [ %R54, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R67 = phi i64 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R66 = phi i64 [ undef, %block_402093 ]
  %R65 = phi i64 [ %R61, %block_402093 ]
  %R64 = phi i64 [ %R52, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R63 = phi i64 [ undef, %block_402093 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R62 = phi i64 [ undef, %block_402093 ]
  ; # 40209b: test   eax,eax
  ; r78 := (trunc r62 32)
  %R78 = trunc i64 %R62 to i32
  ; r79 := (bv_eq r78 0x0 :: [32])
  %R79 = icmp eq i32 %R78, 0
  ; # 40209d: je     402070
  br i1 %R79, label %subblock_40209b_1, label %subblock_40209b_2
subblock_40209b_1:
  br label %block_402070
subblock_40209b_2:
  br label %block_40209f
block_40209f:
  %R80 = phi i128 [ %R70, %subblock_40209b_2 ]
  ; # 40209f: xor    eax,eax
  ; # 4020a1: jmp    4020f9
  br label %block_4020f9
block_4020a3:
  %R94 = phi i128 [ %R24, %subblock_402070_1 ]
  %R93 = phi i128 [ %R23, %subblock_402070_1 ]
  %R92 = phi i128 [ %R22, %subblock_402070_1 ]
  %R91 = phi i128 [ %R21, %subblock_402070_1 ]
  %R90 = phi i128 [ %R20, %subblock_402070_1 ]
  %R89 = phi i128 [ %R19, %subblock_402070_1 ]
  %R88 = phi i128 [ %R18, %subblock_402070_1 ]
  %R87 = phi i128 [ %R17, %subblock_402070_1 ]
  %R86 = phi i64 [ %R16, %subblock_402070_1 ]
  %R85 = phi i64 [ %R15, %subblock_402070_1 ]
  %R84 = phi i64 [ %R14, %subblock_402070_1 ]
  %R83 = phi i64 [ %R13, %subblock_402070_1 ]
  %R82 = phi i64 [ %R11, %subblock_402070_1 ]
  %R81 = phi i64 [ %R10, %subblock_402070_1 ]
  ; # 4020a3: cmp    BYTE PTR [rbx+0x8b],0x0
  ; r95 := (bv_add r82 0x8b :: [64])
  %R95 = add i64 %R82, 139
  ; r96 := *r95
  %r121 = inttoptr i64 %R95 to i8*
  %R96 = load i8* %r121
  ; r97 := (bv_slt r96 0x0 :: [8])
  %R97 = icmp slt i8 %R96, 0
  ; # 4020aa: mov    rbp,r12
  ; # 4020ad: js     4020c5
  br i1 %R97, label %subblock_4020a3_1, label %subblock_4020a3_2
subblock_4020a3_1:
  br label %block_4020c5
subblock_4020a3_2:
  br label %block_4020af
block_4020af:
  %R112 = phi i128 [ %R148, %block_4020c0 ], [ %R94, %subblock_4020a3_2 ]
  %R111 = phi i128 [ %R147, %block_4020c0 ], [ %R93, %subblock_4020a3_2 ]
  %R110 = phi i128 [ %R146, %block_4020c0 ], [ %R92, %subblock_4020a3_2 ]
  %R109 = phi i128 [ %R145, %block_4020c0 ], [ %R91, %subblock_4020a3_2 ]
  %R108 = phi i128 [ %R144, %block_4020c0 ], [ %R90, %subblock_4020a3_2 ]
  %R107 = phi i128 [ %R143, %block_4020c0 ], [ %R89, %subblock_4020a3_2 ]
  %R106 = phi i128 [ %R142, %block_4020c0 ], [ %R88, %subblock_4020a3_2 ]
  %R105 = phi i128 [ %R141, %block_4020c0 ], [ %R87, %subblock_4020a3_2 ]
  %R104 = phi i64 [ %R140, %block_4020c0 ], [ %R86, %subblock_4020a3_2 ]
  %R103 = phi i64 [ %R139, %block_4020c0 ], [ %R85, %subblock_4020a3_2 ]
  %R102 = phi i64 [ %R138, %block_4020c0 ], [ %R84, %subblock_4020a3_2 ]
  %R101 = phi i64 [ %R137, %block_4020c0 ], [ %R83, %subblock_4020a3_2 ]
  %R100 = phi i64 [ %R134, %block_4020c0 ], [ %R85, %subblock_4020a3_2 ]
  %R99 = phi i64 [ %R136, %block_4020c0 ], [ %R82, %subblock_4020a3_2 ]
  %R98 = phi i64 [ %R135, %block_4020c0 ], [ %R81, %subblock_4020a3_2 ]
  ; # 4020af: test   rbp,rbp
  ; r113 := (bv_eq r100 0x0 :: [64])
  %R113 = icmp eq i64 %R100, 0
  ; # 4020b2: je     4020c7
  br i1 %R113, label %subblock_4020af_1, label %subblock_4020af_2
subblock_4020af_1:
  br label %block_4020c7
subblock_4020af_2:
  br label %block_4020b4
block_4020b4:
  %R128 = phi i128 [ %R112, %subblock_4020af_2 ]
  %R127 = phi i128 [ %R111, %subblock_4020af_2 ]
  %R126 = phi i128 [ %R110, %subblock_4020af_2 ]
  %R125 = phi i128 [ %R109, %subblock_4020af_2 ]
  %R124 = phi i128 [ %R108, %subblock_4020af_2 ]
  %R123 = phi i128 [ %R107, %subblock_4020af_2 ]
  %R122 = phi i128 [ %R106, %subblock_4020af_2 ]
  %R121 = phi i128 [ %R105, %subblock_4020af_2 ]
  %R120 = phi i64 [ %R104, %subblock_4020af_2 ]
  %R119 = phi i64 [ %R103, %subblock_4020af_2 ]
  %R118 = phi i64 [ %R102, %subblock_4020af_2 ]
  %R117 = phi i64 [ %R101, %subblock_4020af_2 ]
  %R116 = phi i64 [ %R100, %subblock_4020af_2 ]
  %R115 = phi i64 [ %R99, %subblock_4020af_2 ]
  %R114 = phi i64 [ %R98, %subblock_4020af_2 ]
  ; # 4020b4: cmp    BYTE PTR [r13+rbp*1-0x1],0xa
  ; r129 := (bv_add r120 r116)
  %R129 = add i64 %R120, %R116
  ; r130 := (bv_add r129 0xffffffffffffffff :: [64])
  %R130 = add i64 %R129, 18446744073709551615
  ; r131 := *r130
  %r157 = inttoptr i64 %R130 to i8*
  %R131 = load i8* %r157
  ; r132 := (bv_eq r131 0xa :: [8])
  %R132 = icmp eq i8 %R131, 10
  ; # 4020ba: lea    rax,[rbp-0x1]
  ; r133 := (bv_add r116 0xffffffffffffffff :: [64])
  %R133 = add i64 %R116, 18446744073709551615
  ; # 4020be: je     4020e0
  br i1 %R132, label %subblock_4020b4_1, label %subblock_4020b4_2
subblock_4020b4_1:
  br label %block_4020e0
subblock_4020b4_2:
  br label %block_4020c0
block_4020c0:
  %R148 = phi i128 [ %R128, %subblock_4020b4_2 ]
  %R147 = phi i128 [ %R127, %subblock_4020b4_2 ]
  %R146 = phi i128 [ %R126, %subblock_4020b4_2 ]
  %R145 = phi i128 [ %R125, %subblock_4020b4_2 ]
  %R144 = phi i128 [ %R124, %subblock_4020b4_2 ]
  %R143 = phi i128 [ %R123, %subblock_4020b4_2 ]
  %R142 = phi i128 [ %R122, %subblock_4020b4_2 ]
  %R141 = phi i128 [ %R121, %subblock_4020b4_2 ]
  %R140 = phi i64 [ %R120, %subblock_4020b4_2 ]
  %R139 = phi i64 [ %R119, %subblock_4020b4_2 ]
  %R138 = phi i64 [ %R118, %subblock_4020b4_2 ]
  %R137 = phi i64 [ %R117, %subblock_4020b4_2 ]
  %R136 = phi i64 [ %R115, %subblock_4020b4_2 ]
  %R135 = phi i64 [ %R114, %subblock_4020b4_2 ]
  %R134 = phi i64 [ %R133, %subblock_4020b4_2 ]
  ; # 4020c0: mov    rbp,rax
  ; # 4020c3: jmp    4020af
  br label %block_4020af
block_4020c5:
  %R152 = phi i128 [ %R87, %subblock_4020a3_1 ]
  %R151 = phi i64 [ %R86, %subblock_4020a3_1 ]
  %R150 = phi i64 [ %R85, %subblock_4020a3_1 ]
  %R149 = phi i64 [ %R82, %subblock_4020a3_1 ]
  ; # 4020c5: xor    ebp,ebp
  br label %block_4020c7
block_4020c7:
  %R157 = phi i128 [ %R202, %block_4020f1 ], [ %R105, %subblock_4020af_1 ], [ %R152, %block_4020c5 ]
  %R156 = phi i64 [ %R203, %block_4020f1 ], [ %R104, %subblock_4020af_1 ], [ %R151, %block_4020c5 ]
  %R155 = phi i64 [ %R204, %block_4020f1 ], [ %R103, %subblock_4020af_1 ], [ %R150, %block_4020c5 ]
  %R154 = phi i64 [ %R199, %block_4020f1 ], [ %R100, %subblock_4020af_1 ], [ 0, %block_4020c5 ]
  %R153 = phi i64 [ %R198, %block_4020f1 ], [ %R99, %subblock_4020af_1 ], [ %R149, %block_4020c5 ]
  ; # 4020c7: mov    rdi,QWORD PTR [rbx+0x28]
  ; r158 := (bv_add r153 0x28 :: [64])
  %R158 = add i64 %R153, 40
  ; r159 := *r158
  %r186 = inttoptr i64 %R158 to i64*
  %R159 = load i64* %r186
  ; # 4020cb: mov    rdx,r12
  ; # 4020ce: mov    rsi,r13
  ; # 4020d1: call   402375
  %r188 = bitcast i128 %R157 to <2 x double>
  %r189 = call { i64, i64, <2 x double> } @F402375(i64 %R159, i64 %R156, i64 %R155, <2 x double> %r188)
  %R160 = extractvalue { i64, i64, <2 x double> } %r189, 0
  %R161 = extractvalue { i64, i64, <2 x double> } %r189, 1
  %r192 = extractvalue { i64, i64, <2 x double> } %r189, 2
  %R162 = bitcast <2 x double> %r192 to i128
  br label %block_4020d6
block_4020d6:
  %R166 = phi i128 [ %R162, %block_4020c7 ]
  %R165 = phi i64 [ %R155, %block_4020c7 ]
  %R164 = phi i64 [ %R154, %block_4020c7 ]
  %R163 = phi i64 [ %R153, %block_4020c7 ]
  ; # 4020d6: add    QWORD PTR [rbx+0x28],r12
  ; r167 := (bv_add r163 0x28 :: [64])
  %R167 = add i64 %R163, 40
  ; r168 := *r167
  %r199 = inttoptr i64 %R167 to i64*
  %R168 = load i64* %r199
  ; r169 := (bv_add r168 r165)
  %R169 = add i64 %R168, %R165
  ; *(r167) = r169
  %r202 = inttoptr i64 %R167 to i64*
  store i64 %R169, i64* %r202
  ; # 4020da: lea    rax,[r12+rbp*1]
  ; r170 := (bv_add r165 r164)
  %R170 = add i64 %R165, %R164
  ; # 4020de: jmp    4020f9
  br label %block_4020f9
block_4020e0:
  %R185 = phi i128 [ %R128, %subblock_4020b4_1 ]
  %R184 = phi i128 [ %R127, %subblock_4020b4_1 ]
  %R183 = phi i128 [ %R126, %subblock_4020b4_1 ]
  %R182 = phi i128 [ %R125, %subblock_4020b4_1 ]
  %R181 = phi i128 [ %R124, %subblock_4020b4_1 ]
  %R180 = phi i128 [ %R123, %subblock_4020b4_1 ]
  %R179 = phi i128 [ %R122, %subblock_4020b4_1 ]
  %R178 = phi i128 [ %R121, %subblock_4020b4_1 ]
  %R177 = phi i64 [ %R120, %subblock_4020b4_1 ]
  %R176 = phi i64 [ %R119, %subblock_4020b4_1 ]
  %R175 = phi i64 [ %R118, %subblock_4020b4_1 ]
  %R174 = phi i64 [ %R117, %subblock_4020b4_1 ]
  %R173 = phi i64 [ %R116, %subblock_4020b4_1 ]
  %R172 = phi i64 [ %R115, %subblock_4020b4_1 ]
  %R171 = phi i64 [ %R114, %subblock_4020b4_1 ]
  ; # 4020e0: mov    rdx,rbp
  ; # 4020e3: mov    rsi,r13
  ; # 4020e6: mov    rdi,rbx
  ; # 4020e9: call   QWORD PTR [rbx+0x48]
  ; r186 := (bv_add r172 0x48 :: [64])
  %R186 = add i64 %R172, 72
  ; r187 := *r186
  %r220 = inttoptr i64 %R186 to i64*
  %R187 = load i64* %r220
  %r222 = inttoptr i64 %R187 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r223 = bitcast i128 %R178 to <2 x double>
  %r224 = bitcast i128 %R179 to <2 x double>
  %r225 = bitcast i128 %R180 to <2 x double>
  %r226 = bitcast i128 %R181 to <2 x double>
  %r227 = bitcast i128 %R182 to <2 x double>
  %r228 = bitcast i128 %R183 to <2 x double>
  %r229 = bitcast i128 %R184 to <2 x double>
  %r230 = bitcast i128 %R185 to <2 x double>
  %r231 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r222(i64 %R172, i64 %R177, i64 %R173, i64 %R171, i64 %R174, i64 %R175, <2 x double> %r223, <2 x double> %r224, <2 x double> %r225, <2 x double> %r226, <2 x double> %r227, <2 x double> %r228, <2 x double> %r229, <2 x double> %r230)
  %R188 = extractvalue { i64, i64, <2 x double> } %r231, 0
  %R189 = extractvalue { i64, i64, <2 x double> } %r231, 1
  %r234 = extractvalue { i64, i64, <2 x double> } %r231, 2
  %R190 = bitcast <2 x double> %r234 to i128
  br label %block_4020ec
block_4020ec:
  %R196 = phi i128 [ %R190, %block_4020e0 ]
  %R195 = phi i64 [ %R177, %block_4020e0 ]
  %R194 = phi i64 [ %R176, %block_4020e0 ]
  %R193 = phi i64 [ %R173, %block_4020e0 ]
  %R192 = phi i64 [ %R172, %block_4020e0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R191 = phi i64 [ undef, %block_4020e0 ]
  ; # 4020ec: cmp    rbp,rax
  ; # 4020ef: ja     4020f9
  ; r197 := (bv_ult r191 r193)
  %R197 = icmp ult i64 %R191, %R193
  br i1 %R197, label %subblock_4020ec_1, label %subblock_4020ec_2
subblock_4020ec_1:
  br label %block_4020f9
subblock_4020ec_2:
  br label %block_4020f1
block_4020f1:
  %R202 = phi i128 [ %R196, %subblock_4020ec_2 ]
  %R201 = phi i64 [ %R195, %subblock_4020ec_2 ]
  %R200 = phi i64 [ %R194, %subblock_4020ec_2 ]
  %R199 = phi i64 [ %R193, %subblock_4020ec_2 ]
  %R198 = phi i64 [ %R192, %subblock_4020ec_2 ]
  ; # 4020f1: add    r13,rbp
  ; r203 := (bv_add r201 r199)
  %R203 = add i64 %R201, %R199
  ; # 4020f4: sub    r12,rbp
  ; r204 := (bv_sub r200 r199)
  %R204 = sub i64 %R200, %R199
  ; # 4020f7: jmp    4020c7
  br label %block_4020c7
block_4020f9:
  %R206 = phi i128 [ %R196, %subblock_4020ec_1 ], [ %R166, %block_4020d6 ], [ %R80, %block_40209f ]
  %R205 = phi i64 [ %R191, %subblock_4020ec_1 ], [ %R170, %block_4020d6 ], [ 0, %block_40209f ]
  ; # 4020f9: pop    rdx
  ; # 4020fa: pop    rbx
  ; # 4020fb: pop    rbp
  ; # 4020fc: pop    r12
  ; # 4020fe: pop    r13
  ; # 402100: ret
  %r252 = bitcast i128 %R206 to <2 x double>
  %r253 = insertvalue { i64, i64, <2 x double> } undef, i64 %R205, 0
  %r254 = insertvalue { i64, i64, <2 x double> } %r253, i64 undef, 1
  %r255 = insertvalue { i64, i64, <2 x double> } %r254, <2 x double> %r252, 2
  ret { i64, i64, <2 x double> } %r255
failure:
  br label %failure
}