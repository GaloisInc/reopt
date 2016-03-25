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
declare { i64, i64, <2 x double> } @F401d55(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F40217e(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402190(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402a2c(i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402a80(i64, i64, i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F401dbf(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_401dbf
block_401dbf:
  ; r0 := (alloca 0x30 :: [64])
  %r8 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 401dbf: push   r12
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401dc1: push   rbp
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 401dc2: push   rbx
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 401dc3: sub    rsp,0x10
  ; r5 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R5 = add i64 %R1, 18446744073709551576
  ; # 401dc7: test   rdi,rdi
  ; r6 := (bv_eq arg0 0x0 :: [64])
  %R6 = icmp eq i64 %a0, 0
  ; # 401dca: je     401e04
  br i1 %R6, label %subblock_401dbf_1, label %subblock_401dbf_2
subblock_401dbf_1:
  br label %block_401e04
subblock_401dbf_2:
  br label %block_401dcc
block_401dcc:
  %R21 = phi i128 [ %r7, %subblock_401dbf_2 ]
  %R20 = phi i128 [ %r6, %subblock_401dbf_2 ]
  %R19 = phi i128 [ %r5, %subblock_401dbf_2 ]
  %R18 = phi i128 [ %r4, %subblock_401dbf_2 ]
  %R17 = phi i128 [ %r3, %subblock_401dbf_2 ]
  %R16 = phi i128 [ %r2, %subblock_401dbf_2 ]
  %R15 = phi i128 [ %r1, %subblock_401dbf_2 ]
  %R14 = phi i128 [ %r0, %subblock_401dbf_2 ]
  %R13 = phi i64 [ %a5, %subblock_401dbf_2 ]
  %R12 = phi i64 [ %a4, %subblock_401dbf_2 ]
  %R11 = phi i64 [ %a0, %subblock_401dbf_2 ]
  %R10 = phi i64 [ %a1, %subblock_401dbf_2 ]
  %R9 = phi i64 [ %R5, %subblock_401dbf_2 ]
  %R8 = phi i64 [ %a2, %subblock_401dbf_2 ]
  %R7 = phi i64 [ %a3, %subblock_401dbf_2 ]
  ; # 401dcc: mov    eax,DWORD PTR [rdi+0x8c]
  ; r22 := (bv_add r11 0x8c :: [64])
  %R22 = add i64 %R11, 140
  ; r23 := *r22
  %r32 = inttoptr i64 %R22 to i32*
  %R23 = load i32* %r32
  ; # 401dd2: xor    ebp,ebp
  ; # 401dd4: mov    rbx,rdi
  ; # 401dd7: test   eax,eax
  ; r24 := (bv_slt r23 0x0 :: [32])
  %R24 = icmp slt i32 %R23, 0
  ; # 401dd9: js     401de2
  br i1 %R24, label %subblock_401dcc_1, label %subblock_401dcc_2
subblock_401dcc_1:
  br label %block_401de2
subblock_401dcc_2:
  br label %block_401ddb
block_401ddb:
  %R28 = phi i128 [ %R14, %subblock_401dcc_2 ]
  %R27 = phi i64 [ %R11, %subblock_401dcc_2 ]
  %R26 = phi i64 [ %R9, %subblock_401dcc_2 ]
  %R25 = phi i64 [ %R11, %subblock_401dcc_2 ]
  ; # 401ddb: call   402a2c
  ; r29 := (bv_add r26 0xfffffffffffffff8 :: [64])
  %R29 = add i64 %R26, 18446744073709551608
  ; r33 := (bv_add r29 0x8 :: [64])
  %R33 = add i64 %R29, 8
  %r41 = bitcast i128 %R28 to <2 x double>
  %r42 = call { i64, i64, <2 x double> } @F402a2c(i64 %R27, <2 x double> %r41)
  %R30 = extractvalue { i64, i64, <2 x double> } %r42, 0
  %R31 = extractvalue { i64, i64, <2 x double> } %r42, 1
  %r45 = extractvalue { i64, i64, <2 x double> } %r42, 2
  %R32 = bitcast <2 x double> %r45 to i128
  br label %block_401de0
block_401de0:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R49 = phi i128 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R48 = phi i128 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R47 = phi i128 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R46 = phi i128 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R45 = phi i128 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R44 = phi i128 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R43 = phi i128 [ undef, %block_401ddb ]
  %R42 = phi i128 [ %R32, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R41 = phi i64 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R40 = phi i64 [ undef, %block_401ddb ]
  %R39 = phi i64 [ %R31, %block_401ddb ]
  %R38 = phi i64 [ %R33, %block_401ddb ]
  %R37 = phi i64 [ %R25, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R36 = phi i64 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R35 = phi i64 [ undef, %block_401ddb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R34 = phi i64 [ undef, %block_401ddb ]
  ; # 401de0: mov    ebp,eax
  ; r50 := (trunc r34 32)
  %R50 = trunc i64 %R34 to i32
  ; r51 := (uext r50 64)
  %R51 = zext i32 %R50 to i64
  br label %block_401de2
block_401de2:
  %R67 = phi i128 [ %R49, %block_401de0 ], [ %R21, %subblock_401dcc_1 ]
  %R66 = phi i128 [ %R48, %block_401de0 ], [ %R20, %subblock_401dcc_1 ]
  %R65 = phi i128 [ %R47, %block_401de0 ], [ %R19, %subblock_401dcc_1 ]
  %R64 = phi i128 [ %R46, %block_401de0 ], [ %R18, %subblock_401dcc_1 ]
  %R63 = phi i128 [ %R45, %block_401de0 ], [ %R17, %subblock_401dcc_1 ]
  %R62 = phi i128 [ %R44, %block_401de0 ], [ %R16, %subblock_401dcc_1 ]
  %R61 = phi i128 [ %R43, %block_401de0 ], [ %R15, %subblock_401dcc_1 ]
  %R60 = phi i128 [ %R42, %block_401de0 ], [ %R14, %subblock_401dcc_1 ]
  %R59 = phi i64 [ %R41, %block_401de0 ], [ %R13, %subblock_401dcc_1 ]
  %R58 = phi i64 [ %R40, %block_401de0 ], [ %R12, %subblock_401dcc_1 ]
  %R57 = phi i64 [ %R39, %block_401de0 ], [ %R10, %subblock_401dcc_1 ]
  %R56 = phi i64 [ %R51, %block_401de0 ], [ 0, %subblock_401dcc_1 ]
  %R55 = phi i64 [ %R38, %block_401de0 ], [ %R9, %subblock_401dcc_1 ]
  %R54 = phi i64 [ %R37, %block_401de0 ], [ %R11, %subblock_401dcc_1 ]
  %R53 = phi i64 [ %R36, %block_401de0 ], [ %R8, %subblock_401dcc_1 ]
  %R52 = phi i64 [ %R35, %block_401de0 ], [ %R7, %subblock_401dcc_1 ]
  ; # 401de2: mov    rdi,rbx
  ; # 401de5: call   401d55
  ; r68 := (bv_add r55 0xfffffffffffffff8 :: [64])
  %R68 = add i64 %R55, 18446744073709551608
  ; r72 := (bv_add r68 0x8 :: [64])
  %R72 = add i64 %R68, 8
  %r83 = bitcast i128 %R60 to <2 x double>
  %r84 = bitcast i128 %R61 to <2 x double>
  %r85 = bitcast i128 %R62 to <2 x double>
  %r86 = bitcast i128 %R63 to <2 x double>
  %r87 = bitcast i128 %R64 to <2 x double>
  %r88 = bitcast i128 %R65 to <2 x double>
  %r89 = bitcast i128 %R66 to <2 x double>
  %r90 = bitcast i128 %R67 to <2 x double>
  %r91 = call { i64, i64, <2 x double> } @F401d55(i64 %R54, i64 %R57, i64 %R53, i64 %R52, i64 %R58, i64 %R59, <2 x double> %r83, <2 x double> %r84, <2 x double> %r85, <2 x double> %r86, <2 x double> %r87, <2 x double> %r88, <2 x double> %r89, <2 x double> %r90)
  %R69 = extractvalue { i64, i64, <2 x double> } %r91, 0
  %R70 = extractvalue { i64, i64, <2 x double> } %r91, 1
  %r94 = extractvalue { i64, i64, <2 x double> } %r91, 2
  %R71 = bitcast <2 x double> %r94 to i128
  br label %block_401dea
block_401dea:
  %R82 = phi i128 [ %R71, %block_401de2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R81 = phi i64 [ undef, %block_401de2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R80 = phi i64 [ undef, %block_401de2 ]
  %R79 = phi i64 [ %R70, %block_401de2 ]
  %R78 = phi i64 [ %R56, %block_401de2 ]
  %R77 = phi i64 [ %R72, %block_401de2 ]
  %R76 = phi i64 [ %R54, %block_401de2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R75 = phi i64 [ undef, %block_401de2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R74 = phi i64 [ undef, %block_401de2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R73 = phi i64 [ undef, %block_401de2 ]
  ; # 401dea: test   ebp,ebp
  ; r83 := (trunc r78 32)
  %R83 = trunc i64 %R78 to i32
  ; r84 := (bv_eq r83 0x0 :: [32])
  %R84 = icmp eq i32 %R83, 0
  ; # 401dec: je     401e73
  br i1 %R84, label %subblock_401dea_1, label %subblock_401dea_2
subblock_401dea_1:
  br label %block_401e73
subblock_401dea_2:
  br label %block_401df2
block_401df2:
  %R93 = phi i128 [ %R82, %subblock_401dea_2 ]
  %R92 = phi i64 [ %R81, %subblock_401dea_2 ]
  %R91 = phi i64 [ %R80, %subblock_401dea_2 ]
  %R90 = phi i64 [ %R79, %subblock_401dea_2 ]
  %R89 = phi i64 [ %R77, %subblock_401dea_2 ]
  %R88 = phi i64 [ %R76, %subblock_401dea_2 ]
  %R87 = phi i64 [ %R75, %subblock_401dea_2 ]
  %R86 = phi i64 [ %R74, %subblock_401dea_2 ]
  %R85 = phi i64 [ %R73, %subblock_401dea_2 ]
  ; # 401df2: mov    rdi,rbx
  ; # 401df5: mov    DWORD PTR [rsp+0xc],eax
  ; r94 := (trunc r85 32)
  %R94 = trunc i64 %R85 to i32
  ; r95 := (bv_add r89 0xc :: [64])
  %R95 = add i64 %R89, 12
  ; *(r95) = r94
  %r119 = inttoptr i64 %R95 to i32*
  store i32 %R94, i32* %r119
  ; # 401df9: call   402a80
  ; r96 := (bv_add r89 0xfffffffffffffff8 :: [64])
  %R96 = add i64 %R89, 18446744073709551608
  ; r100 := (bv_add r96 0x8 :: [64])
  %R100 = add i64 %R96, 8
  %r122 = bitcast i128 %R93 to <2 x double>
  %r123 = call { i64, i64, <2 x double> } @F402a80(i64 %R88, i64 %R90, i64 %R87, i64 %R86, i64 %R91, i64 %R92, <2 x double> %r122)
  %R97 = extractvalue { i64, i64, <2 x double> } %r123, 0
  %R98 = extractvalue { i64, i64, <2 x double> } %r123, 1
  %r126 = extractvalue { i64, i64, <2 x double> } %r123, 2
  %R99 = bitcast <2 x double> %r126 to i128
  br label %block_401dfe
block_401dfe:
  %R103 = phi i128 [ %R99, %block_401df2 ]
  %R102 = phi i64 [ %R100, %block_401df2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R101 = phi i64 [ undef, %block_401df2 ]
  ; # 401dfe: mov    eax,DWORD PTR [rsp+0xc]
  ; r104 := (bv_add r102 0xc :: [64])
  %R104 = add i64 %R102, 12
  ; r105 := *r104
  %r132 = inttoptr i64 %R104 to i32*
  %R105 = load i32* %r132
  ; r106 := (uext r105 64)
  %R106 = zext i32 %R105 to i64
  ; # 401e02: jmp    401e73
  br label %block_401e73
block_401e04:
  %R120 = phi i128 [ %r7, %subblock_401dbf_1 ]
  %R119 = phi i128 [ %r6, %subblock_401dbf_1 ]
  %R118 = phi i128 [ %r5, %subblock_401dbf_1 ]
  %R117 = phi i128 [ %r4, %subblock_401dbf_1 ]
  %R116 = phi i128 [ %r3, %subblock_401dbf_1 ]
  %R115 = phi i128 [ %r2, %subblock_401dbf_1 ]
  %R114 = phi i128 [ %r1, %subblock_401dbf_1 ]
  %R113 = phi i128 [ %r0, %subblock_401dbf_1 ]
  %R112 = phi i64 [ %a5, %subblock_401dbf_1 ]
  %R111 = phi i64 [ %a4, %subblock_401dbf_1 ]
  %R110 = phi i64 [ %a0, %subblock_401dbf_1 ]
  %R109 = phi i64 [ %a1, %subblock_401dbf_1 ]
  %R108 = phi i64 [ %a2, %subblock_401dbf_1 ]
  %R107 = phi i64 [ %a3, %subblock_401dbf_1 ]
  ; # 401e04: mov    rax,QWORD PTR [rip+0x2028e5]
  ; r121 := *0x6046f0 :: [64]
  %r149 = inttoptr i64 6309616 to i64*
  %R121 = load i64* %r149
  ; # 401e0b: xor    ebp,ebp
  ; # 401e0d: test   rax,rax
  ; r122 := (bv_eq r121 0x0 :: [64])
  %R122 = icmp eq i64 %R121, 0
  ; # 401e10: je     401e20
  br i1 %R122, label %subblock_401e04_1, label %subblock_401e04_2
subblock_401e04_1:
  br label %block_401e20
subblock_401e04_2:
  br label %block_401e12
block_401e12:
  %R135 = phi i128 [ %R120, %subblock_401e04_2 ]
  %R134 = phi i128 [ %R119, %subblock_401e04_2 ]
  %R133 = phi i128 [ %R118, %subblock_401e04_2 ]
  %R132 = phi i128 [ %R117, %subblock_401e04_2 ]
  %R131 = phi i128 [ %R116, %subblock_401e04_2 ]
  %R130 = phi i128 [ %R115, %subblock_401e04_2 ]
  %R129 = phi i128 [ %R114, %subblock_401e04_2 ]
  %R128 = phi i128 [ %R113, %subblock_401e04_2 ]
  %R127 = phi i64 [ %R112, %subblock_401e04_2 ]
  %R126 = phi i64 [ %R111, %subblock_401e04_2 ]
  %R125 = phi i64 [ %R109, %subblock_401e04_2 ]
  %R124 = phi i64 [ %R108, %subblock_401e04_2 ]
  %R123 = phi i64 [ %R107, %subblock_401e04_2 ]
  ; # 401e12: mov    rdi,QWORD PTR [rip+0x2028d7]
  ; r136 := *0x6046f0 :: [64]
  %r165 = inttoptr i64 6309616 to i64*
  %R136 = load i64* %r165
  ; # 401e19: call   401dbf
  %r167 = bitcast i128 %R128 to <2 x double>
  %r168 = bitcast i128 %R129 to <2 x double>
  %r169 = bitcast i128 %R130 to <2 x double>
  %r170 = bitcast i128 %R131 to <2 x double>
  %r171 = bitcast i128 %R132 to <2 x double>
  %r172 = bitcast i128 %R133 to <2 x double>
  %r173 = bitcast i128 %R134 to <2 x double>
  %r174 = bitcast i128 %R135 to <2 x double>
  %r175 = call { i64, i64, <2 x double> } @F401dbf(i64 %R136, i64 %R125, i64 %R124, i64 %R123, i64 %R126, i64 %R127, <2 x double> %r167, <2 x double> %r168, <2 x double> %r169, <2 x double> %r170, <2 x double> %r171, <2 x double> %r172, <2 x double> %r173, <2 x double> %r174)
  %R137 = extractvalue { i64, i64, <2 x double> } %r175, 0
  %R138 = extractvalue { i64, i64, <2 x double> } %r175, 1
  %r178 = extractvalue { i64, i64, <2 x double> } %r175, 2
  %R139 = bitcast <2 x double> %r178 to i128
  br label %block_401e1e
block_401e1e:
  %R144 = phi i128 [ %R139, %block_401e12 ]
  %R143 = phi i64 [ %R137, %block_401e12 ]
  %R142 = phi i64 [ %R138, %block_401e12 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R141 = phi i64 [ undef, %block_401e12 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R140 = phi i64 [ undef, %block_401e12 ]
  ; # 401e1e: mov    ebp,eax
  ; r145 := (trunc r140 32)
  %R145 = trunc i64 %R140 to i32
  ; r146 := (uext r145 64)
  %R146 = zext i32 %R145 to i64
  br label %block_401e20
block_401e20:
  %R151 = phi i128 [ %R144, %block_401e1e ], [ %R113, %subblock_401e04_1 ]
  %R150 = phi i64 [ %R143, %block_401e1e ], [ %R110, %subblock_401e04_1 ]
  %R149 = phi i64 [ %R142, %block_401e1e ], [ %R109, %subblock_401e04_1 ]
  %R148 = phi i64 [ %R146, %block_401e1e ], [ 0, %subblock_401e04_1 ]
  %R147 = phi i64 [ %R141, %block_401e1e ], [ %R108, %subblock_401e04_1 ]
  ; # 401e20: call   40217e
  %r192 = bitcast i128 %R151 to <2 x double>
  %r193 = call { i64, i64, <2 x double> } @F40217e(i64 %R150, i64 %R149, i64 %R147, <2 x double> %r192)
  %R152 = extractvalue { i64, i64, <2 x double> } %r193, 0
  %R153 = extractvalue { i64, i64, <2 x double> } %r193, 1
  %r196 = extractvalue { i64, i64, <2 x double> } %r193, 2
  %R154 = bitcast <2 x double> %r196 to i128
  br label %block_401e25
block_401e25:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R170 = phi i128 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R169 = phi i128 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R168 = phi i128 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R167 = phi i128 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R166 = phi i128 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R165 = phi i128 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R164 = phi i128 [ undef, %block_401e20 ]
  %R163 = phi i128 [ %R154, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R162 = phi i64 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R161 = phi i64 [ undef, %block_401e20 ]
  %R160 = phi i64 [ %R152, %block_401e20 ]
  %R159 = phi i64 [ %R153, %block_401e20 ]
  %R158 = phi i64 [ %R148, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R157 = phi i64 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R156 = phi i64 [ undef, %block_401e20 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R155 = phi i64 [ undef, %block_401e20 ]
  ; # 401e25: mov    rbx,QWORD PTR [rax]
  ; r171 := *r155
  %r214 = inttoptr i64 %R155 to i64*
  %R171 = load i64* %r214
  br label %block_401e28
block_401e28:
  %R187 = phi i128 [ %R341, %block_401e66 ], [ %R170, %block_401e25 ]
  %R186 = phi i128 [ %R340, %block_401e66 ], [ %R169, %block_401e25 ]
  %R185 = phi i128 [ %R339, %block_401e66 ], [ %R168, %block_401e25 ]
  %R184 = phi i128 [ %R338, %block_401e66 ], [ %R167, %block_401e25 ]
  %R183 = phi i128 [ %R337, %block_401e66 ], [ %R166, %block_401e25 ]
  %R182 = phi i128 [ %R336, %block_401e66 ], [ %R165, %block_401e25 ]
  %R181 = phi i128 [ %R335, %block_401e66 ], [ %R164, %block_401e25 ]
  %R180 = phi i128 [ %R334, %block_401e66 ], [ %R163, %block_401e25 ]
  %R179 = phi i64 [ %R333, %block_401e66 ], [ %R162, %block_401e25 ]
  %R178 = phi i64 [ %R332, %block_401e66 ], [ %R161, %block_401e25 ]
  %R177 = phi i64 [ %R331, %block_401e66 ], [ %R160, %block_401e25 ]
  %R176 = phi i64 [ %R330, %block_401e66 ], [ %R159, %block_401e25 ]
  %R175 = phi i64 [ %R329, %block_401e66 ], [ %R158, %block_401e25 ]
  %R174 = phi i64 [ %R343, %block_401e66 ], [ %R171, %block_401e25 ]
  %R173 = phi i64 [ %R327, %block_401e66 ], [ %R157, %block_401e25 ]
  %R172 = phi i64 [ %R326, %block_401e66 ], [ %R156, %block_401e25 ]
  ; # 401e28: test   rbx,rbx
  ; r188 := (bv_eq r174 0x0 :: [64])
  %R188 = icmp eq i64 %R174, 0
  ; # 401e2b: je     401e6c
  br i1 %R188, label %subblock_401e28_1, label %subblock_401e28_2
subblock_401e28_1:
  br label %block_401e6c
subblock_401e28_2:
  br label %block_401e2d
block_401e2d:
  %R204 = phi i128 [ %R187, %subblock_401e28_2 ]
  %R203 = phi i128 [ %R186, %subblock_401e28_2 ]
  %R202 = phi i128 [ %R185, %subblock_401e28_2 ]
  %R201 = phi i128 [ %R184, %subblock_401e28_2 ]
  %R200 = phi i128 [ %R183, %subblock_401e28_2 ]
  %R199 = phi i128 [ %R182, %subblock_401e28_2 ]
  %R198 = phi i128 [ %R181, %subblock_401e28_2 ]
  %R197 = phi i128 [ %R180, %subblock_401e28_2 ]
  %R196 = phi i64 [ %R179, %subblock_401e28_2 ]
  %R195 = phi i64 [ %R178, %subblock_401e28_2 ]
  %R194 = phi i64 [ %R177, %subblock_401e28_2 ]
  %R193 = phi i64 [ %R176, %subblock_401e28_2 ]
  %R192 = phi i64 [ %R175, %subblock_401e28_2 ]
  %R191 = phi i64 [ %R174, %subblock_401e28_2 ]
  %R190 = phi i64 [ %R173, %subblock_401e28_2 ]
  %R189 = phi i64 [ %R172, %subblock_401e28_2 ]
  ; # 401e2d: mov    eax,DWORD PTR [rbx+0x8c]
  ; r205 := (bv_add r191 0x8c :: [64])
  %R205 = add i64 %R191, 140
  ; r206 := *r205
  %r250 = inttoptr i64 %R205 to i32*
  %R206 = load i32* %r250
  ; # 401e33: xor    r12d,r12d
  ; # 401e36: test   eax,eax
  ; r207 := (bv_slt r206 0x0 :: [32])
  %R207 = icmp slt i32 %R206, 0
  ; # 401e38: js     401e45
  br i1 %R207, label %subblock_401e2d_1, label %subblock_401e2d_2
subblock_401e2d_1:
  br label %block_401e45
subblock_401e2d_2:
  br label %block_401e3a
block_401e3a:
  %R210 = phi i128 [ %R197, %subblock_401e2d_2 ]
  %R209 = phi i64 [ %R192, %subblock_401e2d_2 ]
  %R208 = phi i64 [ %R191, %subblock_401e2d_2 ]
  ; # 401e3a: mov    rdi,rbx
  ; # 401e3d: call   402a2c
  %r256 = bitcast i128 %R210 to <2 x double>
  %r257 = call { i64, i64, <2 x double> } @F402a2c(i64 %R208, <2 x double> %r256)
  %R211 = extractvalue { i64, i64, <2 x double> } %r257, 0
  %R212 = extractvalue { i64, i64, <2 x double> } %r257, 1
  %r260 = extractvalue { i64, i64, <2 x double> } %r257, 2
  %R213 = bitcast <2 x double> %r260 to i128
  br label %block_401e42
block_401e42:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R230 = phi i128 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R229 = phi i128 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R228 = phi i128 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R227 = phi i128 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R226 = phi i128 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R225 = phi i128 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R224 = phi i128 [ undef, %block_401e3a ]
  %R223 = phi i128 [ %R213, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R222 = phi i64 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R221 = phi i64 [ undef, %block_401e3a ]
  %R220 = phi i64 [ %R211, %block_401e3a ]
  %R219 = phi i64 [ %R212, %block_401e3a ]
  %R218 = phi i64 [ %R209, %block_401e3a ]
  %R217 = phi i64 [ %R208, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R216 = phi i64 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R215 = phi i64 [ undef, %block_401e3a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R214 = phi i64 [ undef, %block_401e3a ]
  ; # 401e42: mov    r12d,eax
  ; r231 := (trunc r214 32)
  %R231 = trunc i64 %R214 to i32
  ; r232 := (uext r231 64)
  %R232 = zext i32 %R231 to i64
  br label %block_401e45
block_401e45:
  %R249 = phi i128 [ %R230, %block_401e42 ], [ %R204, %subblock_401e2d_1 ]
  %R248 = phi i128 [ %R229, %block_401e42 ], [ %R203, %subblock_401e2d_1 ]
  %R247 = phi i128 [ %R228, %block_401e42 ], [ %R202, %subblock_401e2d_1 ]
  %R246 = phi i128 [ %R227, %block_401e42 ], [ %R201, %subblock_401e2d_1 ]
  %R245 = phi i128 [ %R226, %block_401e42 ], [ %R200, %subblock_401e2d_1 ]
  %R244 = phi i128 [ %R225, %block_401e42 ], [ %R199, %subblock_401e2d_1 ]
  %R243 = phi i128 [ %R224, %block_401e42 ], [ %R198, %subblock_401e2d_1 ]
  %R242 = phi i128 [ %R223, %block_401e42 ], [ %R197, %subblock_401e2d_1 ]
  %R241 = phi i64 [ %R232, %block_401e42 ], [ 0, %subblock_401e2d_1 ]
  %R240 = phi i64 [ %R222, %block_401e42 ], [ %R196, %subblock_401e2d_1 ]
  %R239 = phi i64 [ %R221, %block_401e42 ], [ %R195, %subblock_401e2d_1 ]
  %R238 = phi i64 [ %R220, %block_401e42 ], [ %R194, %subblock_401e2d_1 ]
  %R237 = phi i64 [ %R219, %block_401e42 ], [ %R193, %subblock_401e2d_1 ]
  %R236 = phi i64 [ %R218, %block_401e42 ], [ %R192, %subblock_401e2d_1 ]
  %R235 = phi i64 [ %R217, %block_401e42 ], [ %R191, %subblock_401e2d_1 ]
  %R234 = phi i64 [ %R216, %block_401e42 ], [ %R190, %subblock_401e2d_1 ]
  %R233 = phi i64 [ %R215, %block_401e42 ], [ %R189, %subblock_401e2d_1 ]
  ; # 401e45: mov    rax,QWORD PTR [rbx+0x38]
  ; r250 := (bv_add r235 0x38 :: [64])
  %R250 = add i64 %R235, 56
  ; r251 := *r250
  %r299 = inttoptr i64 %R250 to i64*
  %R251 = load i64* %r299
  ; # 401e49: cmp    QWORD PTR [rbx+0x28],rax
  ; r252 := (bv_add r235 0x28 :: [64])
  %R252 = add i64 %R235, 40
  ; r253 := *r252
  %r302 = inttoptr i64 %R252 to i64*
  %R253 = load i64* %r302
  ; # 401e4d: jbe    401e59
  ; r254 := (bv_ule r253 r251)
  %R254 = icmp ule i64 %R253, %R251
  br i1 %R254, label %subblock_401e45_1, label %subblock_401e45_2
subblock_401e45_1:
  br label %block_401e59
subblock_401e45_2:
  br label %block_401e4f
block_401e4f:
  %R270 = phi i128 [ %R249, %subblock_401e45_2 ]
  %R269 = phi i128 [ %R248, %subblock_401e45_2 ]
  %R268 = phi i128 [ %R247, %subblock_401e45_2 ]
  %R267 = phi i128 [ %R246, %subblock_401e45_2 ]
  %R266 = phi i128 [ %R245, %subblock_401e45_2 ]
  %R265 = phi i128 [ %R244, %subblock_401e45_2 ]
  %R264 = phi i128 [ %R243, %subblock_401e45_2 ]
  %R263 = phi i128 [ %R242, %subblock_401e45_2 ]
  %R262 = phi i64 [ %R241, %subblock_401e45_2 ]
  %R261 = phi i64 [ %R240, %subblock_401e45_2 ]
  %R260 = phi i64 [ %R239, %subblock_401e45_2 ]
  %R259 = phi i64 [ %R237, %subblock_401e45_2 ]
  %R258 = phi i64 [ %R236, %subblock_401e45_2 ]
  %R257 = phi i64 [ %R235, %subblock_401e45_2 ]
  %R256 = phi i64 [ %R234, %subblock_401e45_2 ]
  %R255 = phi i64 [ %R233, %subblock_401e45_2 ]
  ; # 401e4f: mov    rdi,rbx
  ; # 401e52: call   401d55
  %r321 = bitcast i128 %R263 to <2 x double>
  %r322 = bitcast i128 %R264 to <2 x double>
  %r323 = bitcast i128 %R265 to <2 x double>
  %r324 = bitcast i128 %R266 to <2 x double>
  %r325 = bitcast i128 %R267 to <2 x double>
  %r326 = bitcast i128 %R268 to <2 x double>
  %r327 = bitcast i128 %R269 to <2 x double>
  %r328 = bitcast i128 %R270 to <2 x double>
  %r329 = call { i64, i64, <2 x double> } @F401d55(i64 %R257, i64 %R259, i64 %R256, i64 %R255, i64 %R260, i64 %R261, <2 x double> %r321, <2 x double> %r322, <2 x double> %r323, <2 x double> %r324, <2 x double> %r325, <2 x double> %r326, <2 x double> %r327, <2 x double> %r328)
  %R271 = extractvalue { i64, i64, <2 x double> } %r329, 0
  %R272 = extractvalue { i64, i64, <2 x double> } %r329, 1
  %r332 = extractvalue { i64, i64, <2 x double> } %r329, 2
  %R273 = bitcast <2 x double> %r332 to i128
  br label %block_401e57
block_401e57:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R291 = phi i128 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R290 = phi i128 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R289 = phi i128 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R288 = phi i128 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R287 = phi i128 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R286 = phi i128 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R285 = phi i128 [ undef, %block_401e4f ]
  %R284 = phi i128 [ %R273, %block_401e4f ]
  %R283 = phi i64 [ %R262, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R282 = phi i64 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R281 = phi i64 [ undef, %block_401e4f ]
  %R280 = phi i64 [ %R271, %block_401e4f ]
  %R279 = phi i64 [ %R272, %block_401e4f ]
  %R278 = phi i64 [ %R258, %block_401e4f ]
  %R277 = phi i64 [ %R257, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R276 = phi i64 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R275 = phi i64 [ undef, %block_401e4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R274 = phi i64 [ undef, %block_401e4f ]
  ; # 401e57: or     ebp,eax
  ; r292 := (trunc r278 32)
  %R292 = trunc i64 %R278 to i32
  ; r293 := (trunc r274 32)
  %R293 = trunc i64 %R274 to i32
  ; r294 := (bv_or r292 r293)
  %R294 = or i32 %R292, %R293
  ; r295 := (uext r294 64)
  %R295 = zext i32 %R294 to i64
  br label %block_401e59
block_401e59:
  %R312 = phi i128 [ %R291, %block_401e57 ], [ %R249, %subblock_401e45_1 ]
  %R311 = phi i128 [ %R290, %block_401e57 ], [ %R248, %subblock_401e45_1 ]
  %R310 = phi i128 [ %R289, %block_401e57 ], [ %R247, %subblock_401e45_1 ]
  %R309 = phi i128 [ %R288, %block_401e57 ], [ %R246, %subblock_401e45_1 ]
  %R308 = phi i128 [ %R287, %block_401e57 ], [ %R245, %subblock_401e45_1 ]
  %R307 = phi i128 [ %R286, %block_401e57 ], [ %R244, %subblock_401e45_1 ]
  %R306 = phi i128 [ %R285, %block_401e57 ], [ %R243, %subblock_401e45_1 ]
  %R305 = phi i128 [ %R284, %block_401e57 ], [ %R242, %subblock_401e45_1 ]
  %R304 = phi i64 [ %R283, %block_401e57 ], [ %R241, %subblock_401e45_1 ]
  %R303 = phi i64 [ %R282, %block_401e57 ], [ %R240, %subblock_401e45_1 ]
  %R302 = phi i64 [ %R281, %block_401e57 ], [ %R239, %subblock_401e45_1 ]
  %R301 = phi i64 [ %R280, %block_401e57 ], [ %R238, %subblock_401e45_1 ]
  %R300 = phi i64 [ %R279, %block_401e57 ], [ %R237, %subblock_401e45_1 ]
  %R299 = phi i64 [ %R295, %block_401e57 ], [ %R236, %subblock_401e45_1 ]
  %R298 = phi i64 [ %R277, %block_401e57 ], [ %R235, %subblock_401e45_1 ]
  %R297 = phi i64 [ %R276, %block_401e57 ], [ %R234, %subblock_401e45_1 ]
  %R296 = phi i64 [ %R275, %block_401e57 ], [ %R233, %subblock_401e45_1 ]
  ; # 401e59: test   r12d,r12d
  ; r313 := (trunc r304 32)
  %R313 = trunc i64 %R304 to i32
  ; r314 := (bv_eq r313 0x0 :: [32])
  %R314 = icmp eq i32 %R313, 0
  ; # 401e5c: je     401e66
  br i1 %R314, label %subblock_401e59_1, label %subblock_401e59_2
subblock_401e59_1:
  br label %block_401e66
subblock_401e59_2:
  br label %block_401e5e
block_401e5e:
  %R322 = phi i128 [ %R305, %subblock_401e59_2 ]
  %R321 = phi i64 [ %R303, %subblock_401e59_2 ]
  %R320 = phi i64 [ %R302, %subblock_401e59_2 ]
  %R319 = phi i64 [ %R300, %subblock_401e59_2 ]
  %R318 = phi i64 [ %R299, %subblock_401e59_2 ]
  %R317 = phi i64 [ %R298, %subblock_401e59_2 ]
  %R316 = phi i64 [ %R297, %subblock_401e59_2 ]
  %R315 = phi i64 [ %R296, %subblock_401e59_2 ]
  ; # 401e5e: mov    rdi,rbx
  ; # 401e61: call   402a80
  %r383 = bitcast i128 %R322 to <2 x double>
  %r384 = call { i64, i64, <2 x double> } @F402a80(i64 %R317, i64 %R319, i64 %R316, i64 %R315, i64 %R320, i64 %R321, <2 x double> %r383)
  %R323 = extractvalue { i64, i64, <2 x double> } %r384, 0
  %R324 = extractvalue { i64, i64, <2 x double> } %r384, 1
  %r387 = extractvalue { i64, i64, <2 x double> } %r384, 2
  %R325 = bitcast <2 x double> %r387 to i128
  br label %block_401e66
block_401e66:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R341 = phi i128 [ undef, %block_401e5e ], [ %R312, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R340 = phi i128 [ undef, %block_401e5e ], [ %R311, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R339 = phi i128 [ undef, %block_401e5e ], [ %R310, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R338 = phi i128 [ undef, %block_401e5e ], [ %R309, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R337 = phi i128 [ undef, %block_401e5e ], [ %R308, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R336 = phi i128 [ undef, %block_401e5e ], [ %R307, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R335 = phi i128 [ undef, %block_401e5e ], [ %R306, %subblock_401e59_1 ]
  %R334 = phi i128 [ %R325, %block_401e5e ], [ %R305, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R333 = phi i64 [ undef, %block_401e5e ], [ %R303, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R332 = phi i64 [ undef, %block_401e5e ], [ %R302, %subblock_401e59_1 ]
  %R331 = phi i64 [ %R323, %block_401e5e ], [ %R301, %subblock_401e59_1 ]
  %R330 = phi i64 [ %R324, %block_401e5e ], [ %R300, %subblock_401e59_1 ]
  %R329 = phi i64 [ %R318, %block_401e5e ], [ %R299, %subblock_401e59_1 ]
  %R328 = phi i64 [ %R317, %block_401e5e ], [ %R298, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R327 = phi i64 [ undef, %block_401e5e ], [ %R297, %subblock_401e59_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R326 = phi i64 [ undef, %block_401e5e ], [ %R296, %subblock_401e59_1 ]
  ; # 401e66: mov    rbx,QWORD PTR [rbx+0x70]
  ; r342 := (bv_add r328 0x70 :: [64])
  %R342 = add i64 %R328, 112
  ; r343 := *r342
  %r406 = inttoptr i64 %R342 to i64*
  %R343 = load i64* %r406
  ; # 401e6a: jmp    401e28
  br label %block_401e28
block_401e6c:
  %R351 = phi i128 [ %R180, %subblock_401e28_1 ]
  %R350 = phi i64 [ %R179, %subblock_401e28_1 ]
  %R349 = phi i64 [ %R178, %subblock_401e28_1 ]
  %R348 = phi i64 [ %R177, %subblock_401e28_1 ]
  %R347 = phi i64 [ %R176, %subblock_401e28_1 ]
  %R346 = phi i64 [ %R175, %subblock_401e28_1 ]
  %R345 = phi i64 [ %R173, %subblock_401e28_1 ]
  %R344 = phi i64 [ %R172, %subblock_401e28_1 ]
  ; # 401e6c: call   402190
  %r416 = bitcast i128 %R351 to <2 x double>
  %r417 = call { i64, i64, <2 x double> } @F402190(i64 %R348, i64 %R347, i64 %R345, i64 %R344, i64 %R349, i64 %R350, <2 x double> %r416)
  %R352 = extractvalue { i64, i64, <2 x double> } %r417, 0
  %R353 = extractvalue { i64, i64, <2 x double> } %r417, 1
  %r420 = extractvalue { i64, i64, <2 x double> } %r417, 2
  %R354 = bitcast <2 x double> %r420 to i128
  br label %block_401e71
block_401e71:
  %R357 = phi i128 [ %R354, %block_401e6c ]
  %R356 = phi i64 [ %R346, %block_401e6c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R355 = phi i64 [ undef, %block_401e6c ]
  ; # 401e71: mov    eax,ebp
  ; r358 := (trunc r356 32)
  %R358 = trunc i64 %R356 to i32
  ; r359 := (uext r358 64)
  %R359 = zext i32 %R358 to i64
  br label %block_401e73
block_401e73:
  %R362 = phi i128 [ %R103, %block_401dfe ], [ %R82, %subblock_401dea_1 ], [ %R357, %block_401e71 ]
  %R361 = phi i64 [ %R101, %block_401dfe ], [ %R75, %subblock_401dea_1 ], [ %R355, %block_401e71 ]
  %R360 = phi i64 [ %R106, %block_401dfe ], [ %R73, %subblock_401dea_1 ], [ %R359, %block_401e71 ]
  ; # 401e73: add    rsp,0x10
  ; # 401e77: pop    rbx
  ; # 401e78: pop    rbp
  ; # 401e79: pop    r12
  ; # 401e7b: ret
  %r430 = bitcast i128 %R362 to <2 x double>
  %r431 = insertvalue { i64, i64, <2 x double> } undef, i64 %R360, 0
  %r432 = insertvalue { i64, i64, <2 x double> } %r431, i64 %R361, 1
  %r433 = insertvalue { i64, i64, <2 x double> } %r432, <2 x double> %r430, 2
  ret { i64, i64, <2 x double> } %r433
failure:
  br label %failure
}