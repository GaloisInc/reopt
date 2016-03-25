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
declare { i64, i64, <2 x double> } @F402a80(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402d34(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
define { i64, i64, <2 x double> } @F401e7c(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_401e7c
block_401e7c:
  ; r0 := (alloca 0x20 :: [64])
  %r8 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 401e7c: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401e7d: push   rbx
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 401e7e: mov    rbx,rdi
  ; # 401e81: push   rsi
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; *(r4) = arg1
  %r14 = inttoptr i64 %R4 to i64*
  store i64 %a1, i64* %r14
  ; # 401e82: mov    eax,DWORD PTR [rdi+0x8c]
  ; r5 := (bv_add arg0 0x8c :: [64])
  %R5 = add i64 %a0, 140
  ; r6 := *r5
  %r16 = inttoptr i64 %R5 to i32*
  %R6 = load i32* %r16
  ; # 401e88: test   eax,eax
  ; # 401e8a: jns    401ea3
  ; r7 := (bv_sle 0x0 :: [32] r6)
  %R7 = icmp sle i32 0, %R6
  br i1 %R7, label %subblock_401e7c_1, label %subblock_401e7c_2
subblock_401e7c_1:
  br label %block_401ea3
subblock_401e7c_2:
  br label %block_401e8c
block_401e8c:
  %R21 = phi i128 [ %R59, %subblock_401ea8_1 ], [ %r7, %subblock_401e7c_2 ]
  %R20 = phi i128 [ %R58, %subblock_401ea8_1 ], [ %r6, %subblock_401e7c_2 ]
  %R19 = phi i128 [ %R57, %subblock_401ea8_1 ], [ %r5, %subblock_401e7c_2 ]
  %R18 = phi i128 [ %R56, %subblock_401ea8_1 ], [ %r4, %subblock_401e7c_2 ]
  %R17 = phi i128 [ %R55, %subblock_401ea8_1 ], [ %r3, %subblock_401e7c_2 ]
  %R16 = phi i128 [ %R54, %subblock_401ea8_1 ], [ %r2, %subblock_401e7c_2 ]
  %R15 = phi i128 [ %R53, %subblock_401ea8_1 ], [ %r1, %subblock_401e7c_2 ]
  %R14 = phi i128 [ %R52, %subblock_401ea8_1 ], [ %r0, %subblock_401e7c_2 ]
  %R13 = phi i64 [ %R51, %subblock_401ea8_1 ], [ %a5, %subblock_401e7c_2 ]
  %R12 = phi i64 [ %R50, %subblock_401ea8_1 ], [ %a4, %subblock_401e7c_2 ]
  %R11 = phi i64 [ %R49, %subblock_401ea8_1 ], [ %a1, %subblock_401e7c_2 ]
  %R10 = phi i64 [ %R48, %subblock_401ea8_1 ], [ %R4, %subblock_401e7c_2 ]
  %R9 = phi i64 [ %R47, %subblock_401ea8_1 ], [ %a0, %subblock_401e7c_2 ]
  %R8 = phi i64 [ %R46, %subblock_401ea8_1 ], [ %a2, %subblock_401e7c_2 ]
  ; # 401e8c: mov    rax,QWORD PTR [rbx+0x8]
  ; r22 := (bv_add r9 0x8 :: [64])
  %R22 = add i64 %R9, 8
  ; r23 := *r22
  %r34 = inttoptr i64 %R22 to i64*
  %R23 = load i64* %r34
  ; # 401e90: cmp    rax,QWORD PTR [rbx+0x10]
  ; r24 := (bv_add r9 0x10 :: [64])
  %R24 = add i64 %R9, 16
  ; r25 := *r24
  %r37 = inttoptr i64 %R24 to i64*
  %R25 = load i64* %r37
  ; # 401e94: jae    401eae
  ; r26 := (bv_ule r25 r23)
  %R26 = icmp ule i64 %R25, %R23
  br i1 %R26, label %subblock_401e8c_1, label %subblock_401e8c_2
subblock_401e8c_1:
  br label %block_401eae
subblock_401e8c_2:
  br label %block_401e96
block_401e96:
  %R30 = phi i128 [ %R14, %subblock_401e8c_2 ]
  %R29 = phi i64 [ %R10, %subblock_401e8c_2 ]
  %R28 = phi i64 [ %R9, %subblock_401e8c_2 ]
  %R27 = phi i64 [ %R23, %subblock_401e8c_2 ]
  ; # 401e96: lea    rdx,[rax+0x1]
  ; r31 := (bv_add r27 0x1 :: [64])
  %R31 = add i64 %R27, 1
  ; # 401e9a: mov    QWORD PTR [rbx+0x8],rdx
  ; r32 := (bv_add r28 0x8 :: [64])
  %R32 = add i64 %R28, 8
  ; *(r32) = r31
  %r46 = inttoptr i64 %R32 to i64*
  store i64 %R31, i64* %r46
  ; # 401e9e: movzx  eax,BYTE PTR [rax]
  ; r33 := *r27
  %r47 = inttoptr i64 %R27 to i8*
  %R33 = load i8* %r47
  ; r34 := (uext r33 64)
  %R34 = zext i8 %R33 to i64
  ; # 401ea1: jmp    401ee4
  br label %block_401ee4
block_401ea3:
  %R38 = phi i128 [ %r0, %subblock_401e7c_1 ]
  %R37 = phi i64 [ %a0, %subblock_401e7c_1 ]
  %R36 = phi i64 [ %R4, %subblock_401e7c_1 ]
  %R35 = phi i64 [ %a0, %subblock_401e7c_1 ]
  ; # 401ea3: call   402a2c
  ; r39 := (bv_add r36 0xfffffffffffffff8 :: [64])
  %R39 = add i64 %R36, 18446744073709551608
  ; r43 := (bv_add r39 0x8 :: [64])
  %R43 = add i64 %R39, 8
  %r56 = bitcast i128 %R38 to <2 x double>
  %r57 = call { i64, i64, <2 x double> } @F402a2c(i64 %R37, <2 x double> %r56)
  %R40 = extractvalue { i64, i64, <2 x double> } %r57, 0
  %R41 = extractvalue { i64, i64, <2 x double> } %r57, 1
  %r60 = extractvalue { i64, i64, <2 x double> } %r57, 2
  %R42 = bitcast <2 x double> %r60 to i128
  br label %block_401ea8
block_401ea8:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R59 = phi i128 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R58 = phi i128 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R57 = phi i128 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R56 = phi i128 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R55 = phi i128 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R54 = phi i128 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R53 = phi i128 [ undef, %block_401ea3 ]
  %R52 = phi i128 [ %R42, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R51 = phi i64 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R50 = phi i64 [ undef, %block_401ea3 ]
  %R49 = phi i64 [ %R41, %block_401ea3 ]
  %R48 = phi i64 [ %R43, %block_401ea3 ]
  %R47 = phi i64 [ %R35, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R46 = phi i64 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R45 = phi i64 [ undef, %block_401ea3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R44 = phi i64 [ undef, %block_401ea3 ]
  ; # 401ea8: test   eax,eax
  ; r60 := (trunc r44 32)
  %R60 = trunc i64 %R44 to i32
  ; r61 := (bv_eq r60 0x0 :: [32])
  %R61 = icmp eq i32 %R60, 0
  ; # 401eaa: je     401e8c
  br i1 %R61, label %subblock_401ea8_1, label %subblock_401ea8_2
subblock_401ea8_1:
  br label %block_401e8c
subblock_401ea8_2:
  br label %block_401eac
block_401eac:
  %R76 = phi i128 [ %R59, %subblock_401ea8_2 ]
  %R75 = phi i128 [ %R58, %subblock_401ea8_2 ]
  %R74 = phi i128 [ %R57, %subblock_401ea8_2 ]
  %R73 = phi i128 [ %R56, %subblock_401ea8_2 ]
  %R72 = phi i128 [ %R55, %subblock_401ea8_2 ]
  %R71 = phi i128 [ %R54, %subblock_401ea8_2 ]
  %R70 = phi i128 [ %R53, %subblock_401ea8_2 ]
  %R69 = phi i128 [ %R52, %subblock_401ea8_2 ]
  %R68 = phi i64 [ %R51, %subblock_401ea8_2 ]
  %R67 = phi i64 [ %R50, %subblock_401ea8_2 ]
  %R66 = phi i64 [ %R49, %subblock_401ea8_2 ]
  %R65 = phi i64 [ %R48, %subblock_401ea8_2 ]
  %R64 = phi i64 [ %R47, %subblock_401ea8_2 ]
  %R63 = phi i64 [ %R46, %subblock_401ea8_2 ]
  %R62 = phi i64 [ %R45, %subblock_401ea8_2 ]
  ; # 401eac: jmp    401eb9
  br label %block_401eb9
block_401eae:
  %R90 = phi i128 [ %R21, %subblock_401e8c_1 ]
  %R89 = phi i128 [ %R20, %subblock_401e8c_1 ]
  %R88 = phi i128 [ %R19, %subblock_401e8c_1 ]
  %R87 = phi i128 [ %R18, %subblock_401e8c_1 ]
  %R86 = phi i128 [ %R17, %subblock_401e8c_1 ]
  %R85 = phi i128 [ %R16, %subblock_401e8c_1 ]
  %R84 = phi i128 [ %R15, %subblock_401e8c_1 ]
  %R83 = phi i128 [ %R14, %subblock_401e8c_1 ]
  %R82 = phi i64 [ %R13, %subblock_401e8c_1 ]
  %R81 = phi i64 [ %R12, %subblock_401e8c_1 ]
  %R80 = phi i64 [ %R11, %subblock_401e8c_1 ]
  %R79 = phi i64 [ %R10, %subblock_401e8c_1 ]
  %R78 = phi i64 [ %R9, %subblock_401e8c_1 ]
  %R77 = phi i64 [ %R8, %subblock_401e8c_1 ]
  ; # 401eae: pop    rcx
  ; r91 := *r79
  %r109 = inttoptr i64 %R79 to i64*
  %R91 = load i64* %r109
  ; # 401eaf: mov    rdi,rbx
  ; # 401eb2: pop    rbx
  ; # 401eb3: pop    rbp
  ; # 401eb4: jmp    402d34
  %r111 = bitcast i128 %R83 to <2 x double>
  %r112 = bitcast i128 %R84 to <2 x double>
  %r113 = bitcast i128 %R85 to <2 x double>
  %r114 = bitcast i128 %R86 to <2 x double>
  %r115 = bitcast i128 %R87 to <2 x double>
  %r116 = bitcast i128 %R88 to <2 x double>
  %r117 = bitcast i128 %R89 to <2 x double>
  %r118 = bitcast i128 %R90 to <2 x double>
  %r119 = call { i64, i64, <2 x double> } @F402d34(i64 %R78, i64 %R80, i64 %R77, i64 %R91, i64 %R81, i64 %R82, <2 x double> %r111, <2 x double> %r112, <2 x double> %r113, <2 x double> %r114, <2 x double> %r115, <2 x double> %r116, <2 x double> %r117, <2 x double> %r118)
  ret { i64, i64, <2 x double> } %r119
block_401eb9:
  %R109 = phi i128 [ %R76, %block_401eac ]
  %R108 = phi i128 [ %R75, %block_401eac ]
  %R107 = phi i128 [ %R74, %block_401eac ]
  %R106 = phi i128 [ %R73, %block_401eac ]
  %R105 = phi i128 [ %R72, %block_401eac ]
  %R104 = phi i128 [ %R71, %block_401eac ]
  %R103 = phi i128 [ %R70, %block_401eac ]
  %R102 = phi i128 [ %R69, %block_401eac ]
  %R101 = phi i64 [ %R68, %block_401eac ]
  %R100 = phi i64 [ %R67, %block_401eac ]
  %R99 = phi i64 [ %R66, %block_401eac ]
  %R98 = phi i64 [ %R65, %block_401eac ]
  %R97 = phi i64 [ %R64, %block_401eac ]
  %R96 = phi i64 [ %R63, %block_401eac ]
  %R95 = phi i64 [ %R62, %block_401eac ]
  ; # 401eb9: mov    rax,QWORD PTR [rbx+0x8]
  ; r110 := (bv_add r97 0x8 :: [64])
  %R110 = add i64 %R97, 8
  ; r111 := *r110
  %r136 = inttoptr i64 %R110 to i64*
  %R111 = load i64* %r136
  ; # 401ebd: cmp    rax,QWORD PTR [rbx+0x10]
  ; r112 := (bv_add r97 0x10 :: [64])
  %R112 = add i64 %R97, 16
  ; r113 := *r112
  %r139 = inttoptr i64 %R112 to i64*
  %R113 = load i64* %r139
  ; # 401ec1: jae    401ed0
  ; r114 := (bv_ule r113 r111)
  %R114 = icmp ule i64 %R113, %R111
  br i1 %R114, label %subblock_401eb9_1, label %subblock_401eb9_2
subblock_401eb9_1:
  br label %block_401ed0
subblock_401eb9_2:
  br label %block_401ec3
block_401ec3:
  %R122 = phi i128 [ %R102, %subblock_401eb9_2 ]
  %R121 = phi i64 [ %R101, %subblock_401eb9_2 ]
  %R120 = phi i64 [ %R100, %subblock_401eb9_2 ]
  %R119 = phi i64 [ %R99, %subblock_401eb9_2 ]
  %R118 = phi i64 [ %R98, %subblock_401eb9_2 ]
  %R117 = phi i64 [ %R97, %subblock_401eb9_2 ]
  %R116 = phi i64 [ %R95, %subblock_401eb9_2 ]
  %R115 = phi i64 [ %R111, %subblock_401eb9_2 ]
  ; # 401ec3: lea    rdx,[rax+0x1]
  ; r123 := (bv_add r115 0x1 :: [64])
  %R123 = add i64 %R115, 1
  ; # 401ec7: mov    QWORD PTR [rbx+0x8],rdx
  ; r124 := (bv_add r117 0x8 :: [64])
  %R124 = add i64 %R117, 8
  ; *(r124) = r123
  %r152 = inttoptr i64 %R124 to i64*
  store i64 %R123, i64* %r152
  ; # 401ecb: movzx  ebp,BYTE PTR [rax]
  ; r125 := *r115
  %r153 = inttoptr i64 %R115 to i8*
  %R125 = load i8* %r153
  ; r126 := (uext r125 64)
  %R126 = zext i8 %R125 to i64
  ; # 401ece: jmp    401eda
  br label %block_401eda
block_401ed0:
  %R141 = phi i128 [ %R109, %subblock_401eb9_1 ]
  %R140 = phi i128 [ %R108, %subblock_401eb9_1 ]
  %R139 = phi i128 [ %R107, %subblock_401eb9_1 ]
  %R138 = phi i128 [ %R106, %subblock_401eb9_1 ]
  %R137 = phi i128 [ %R105, %subblock_401eb9_1 ]
  %R136 = phi i128 [ %R104, %subblock_401eb9_1 ]
  %R135 = phi i128 [ %R103, %subblock_401eb9_1 ]
  %R134 = phi i128 [ %R102, %subblock_401eb9_1 ]
  %R133 = phi i64 [ %R101, %subblock_401eb9_1 ]
  %R132 = phi i64 [ %R100, %subblock_401eb9_1 ]
  %R131 = phi i64 [ %R99, %subblock_401eb9_1 ]
  %R130 = phi i64 [ %R98, %subblock_401eb9_1 ]
  %R129 = phi i64 [ %R97, %subblock_401eb9_1 ]
  %R128 = phi i64 [ %R96, %subblock_401eb9_1 ]
  %R127 = phi i64 [ %R95, %subblock_401eb9_1 ]
  ; # 401ed0: mov    rdi,rbx
  ; # 401ed3: call   402d34
  ; r142 := (bv_add r130 0xfffffffffffffff8 :: [64])
  %R142 = add i64 %R130, 18446744073709551608
  ; r146 := (bv_add r142 0x8 :: [64])
  %R146 = add i64 %R142, 8
  %r173 = bitcast i128 %R134 to <2 x double>
  %r174 = bitcast i128 %R135 to <2 x double>
  %r175 = bitcast i128 %R136 to <2 x double>
  %r176 = bitcast i128 %R137 to <2 x double>
  %r177 = bitcast i128 %R138 to <2 x double>
  %r178 = bitcast i128 %R139 to <2 x double>
  %r179 = bitcast i128 %R140 to <2 x double>
  %r180 = bitcast i128 %R141 to <2 x double>
  %r181 = call { i64, i64, <2 x double> } @F402d34(i64 %R129, i64 %R131, i64 %R128, i64 %R127, i64 %R132, i64 %R133, <2 x double> %r173, <2 x double> %r174, <2 x double> %r175, <2 x double> %r176, <2 x double> %r177, <2 x double> %r178, <2 x double> %r179, <2 x double> %r180)
  %R143 = extractvalue { i64, i64, <2 x double> } %r181, 0
  %R144 = extractvalue { i64, i64, <2 x double> } %r181, 1
  %r184 = extractvalue { i64, i64, <2 x double> } %r181, 2
  %R145 = bitcast <2 x double> %r184 to i128
  br label %block_401ed8
block_401ed8:
  %R155 = phi i128 [ %R145, %block_401ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R154 = phi i64 [ undef, %block_401ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R153 = phi i64 [ undef, %block_401ed0 ]
  %R152 = phi i64 [ %R144, %block_401ed0 ]
  %R151 = phi i64 [ %R146, %block_401ed0 ]
  %R150 = phi i64 [ %R129, %block_401ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R149 = phi i64 [ undef, %block_401ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R148 = phi i64 [ undef, %block_401ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R147 = phi i64 [ undef, %block_401ed0 ]
  ; # 401ed8: mov    ebp,eax
  ; r156 := (trunc r147 32)
  %R156 = trunc i64 %R147 to i32
  ; r157 := (uext r156 64)
  %R157 = zext i32 %R156 to i64
  br label %block_401eda
block_401eda:
  %R166 = phi i128 [ %R122, %block_401ec3 ], [ %R155, %block_401ed8 ]
  %R165 = phi i64 [ %R121, %block_401ec3 ], [ %R154, %block_401ed8 ]
  %R164 = phi i64 [ %R120, %block_401ec3 ], [ %R153, %block_401ed8 ]
  %R163 = phi i64 [ %R119, %block_401ec3 ], [ %R152, %block_401ed8 ]
  %R162 = phi i64 [ %R126, %block_401ec3 ], [ %R157, %block_401ed8 ]
  %R161 = phi i64 [ %R118, %block_401ec3 ], [ %R151, %block_401ed8 ]
  %R160 = phi i64 [ %R117, %block_401ec3 ], [ %R150, %block_401ed8 ]
  %R159 = phi i64 [ %R123, %block_401ec3 ], [ %R149, %block_401ed8 ]
  %R158 = phi i64 [ %R116, %block_401ec3 ], [ %R148, %block_401ed8 ]
  ; # 401eda: mov    rdi,rbx
  ; # 401edd: call   402a80
  ; r167 := (bv_add r161 0xfffffffffffffff8 :: [64])
  %R167 = add i64 %R161, 18446744073709551608
  ; r171 := (bv_add r167 0x8 :: [64])
  %R171 = add i64 %R167, 8
  %r208 = bitcast i128 %R166 to <2 x double>
  %r209 = call { i64, i64, <2 x double> } @F402a80(i64 %R160, i64 %R163, i64 %R159, i64 %R158, i64 %R164, i64 %R165, <2 x double> %r208)
  %R168 = extractvalue { i64, i64, <2 x double> } %r209, 0
  %R169 = extractvalue { i64, i64, <2 x double> } %r209, 1
  %r212 = extractvalue { i64, i64, <2 x double> } %r209, 2
  %R170 = bitcast <2 x double> %r212 to i128
  br label %block_401ee2
block_401ee2:
  %R174 = phi i128 [ %R170, %block_401eda ]
  %R173 = phi i64 [ %R162, %block_401eda ]
  %R172 = phi i64 [ %R171, %block_401eda ]
  ; # 401ee2: mov    eax,ebp
  ; r175 := (trunc r173 32)
  %R175 = trunc i64 %R173 to i32
  ; r176 := (uext r175 64)
  %R176 = zext i32 %R175 to i64
  br label %block_401ee4
block_401ee4:
  %R179 = phi i128 [ %R30, %block_401e96 ], [ %R174, %block_401ee2 ]
  %R178 = phi i64 [ %R29, %block_401e96 ], [ %R172, %block_401ee2 ]
  %R177 = phi i64 [ %R34, %block_401e96 ], [ %R176, %block_401ee2 ]
  ; # 401ee4: pop    rdx
  ; r180 := *r178
  %r222 = inttoptr i64 %R178 to i64*
  %R180 = load i64* %r222
  ; # 401ee5: pop    rbx
  ; # 401ee6: pop    rbp
  ; # 401ee7: ret
  %r224 = bitcast i128 %R179 to <2 x double>
  %r225 = insertvalue { i64, i64, <2 x double> } undef, i64 %R177, 0
  %r226 = insertvalue { i64, i64, <2 x double> } %r225, i64 %R180, 1
  %r227 = insertvalue { i64, i64, <2 x double> } %r226, <2 x double> %r224, 2
  ret { i64, i64, <2 x double> } %r227
failure:
  br label %failure
}