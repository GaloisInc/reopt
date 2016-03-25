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
declare { i64, i64, <2 x double> } @F4023a7(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4023e2(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4026e0(i64, <2 x double>)
define { i64, i64, <2 x double> } @F400860(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_400860
block_400860:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 400860: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 400861: push   rbx
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 400862: sub    rsp,0x18
  ; r4 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R4 = add i64 %R1, 18446744073709551576
  ; # 400866: test   rdi,rdi
  ; r5 := (bv_eq arg0 0x0 :: [64])
  %R5 = icmp eq i64 %a0, 0
  ; # 400869: je     400900
  br i1 %R5, label %subblock_400860_1, label %subblock_400860_2
subblock_400860_1:
  br label %block_400900
subblock_400860_2:
  br label %block_40086f
block_40086f:
  %R10 = phi i128 [ %r0, %subblock_400860_2 ]
  %R9 = phi i64 [ %a0, %subblock_400860_2 ]
  %R8 = phi i64 [ %a1, %subblock_400860_2 ]
  %R7 = phi i64 [ %R4, %subblock_400860_2 ]
  %R6 = phi i64 [ %a2, %subblock_400860_2 ]
  ; # 40086f: cmp    rdi,0x1
  ; # 400873: mov    rbp,rdi
  ; # 400876: jbe    400964
  ; r11 := (bv_ule r9 0x1 :: [64])
  %R11 = icmp ule i64 %R9, 1
  br i1 %R11, label %subblock_40086f_1, label %subblock_40086f_2
subblock_40086f_1:
  br label %block_400964
subblock_40086f_2:
  br label %block_40087c
block_40087c:
  %R16 = phi i128 [ %R10, %subblock_40086f_2 ]
  %R15 = phi i64 [ %R8, %subblock_40086f_2 ]
  %R14 = phi i64 [ %R9, %subblock_40086f_2 ]
  %R13 = phi i64 [ %R7, %subblock_40086f_2 ]
  %R12 = phi i64 [ %R6, %subblock_40086f_2 ]
  ; # 40087c: mov    ebx,0x1
  ; # 400881: jmp    40088e
  br label %block_40088e
block_400888:
  %R22 = phi i128 [ %R29, %subblock_40088e_1 ]
  %R21 = phi i64 [ %R28, %subblock_40088e_1 ]
  %R20 = phi i64 [ %R27, %subblock_40088e_1 ]
  %R19 = phi i64 [ %R26, %subblock_40088e_1 ]
  %R18 = phi i64 [ %R30, %subblock_40088e_1 ]
  %R17 = phi i64 [ %R24, %subblock_40088e_1 ]
  ; # 400888: cmp    rbx,0xf
  ; # 40088c: ja     400896
  ; r23 := (bv_ult 0xf :: [64] r18)
  %R23 = icmp ult i64 15, %R18
  br i1 %R23, label %subblock_400888_1, label %subblock_400888_2
subblock_400888_1:
  br label %block_400896
subblock_400888_2:
  br label %block_40088e
block_40088e:
  %R29 = phi i128 [ %R22, %subblock_400888_2 ], [ %R16, %block_40087c ]
  %R28 = phi i64 [ %R21, %subblock_400888_2 ], [ %R15, %block_40087c ]
  %R27 = phi i64 [ %R20, %subblock_400888_2 ], [ %R14, %block_40087c ]
  %R26 = phi i64 [ %R19, %subblock_400888_2 ], [ %R13, %block_40087c ]
  %R25 = phi i64 [ %R18, %subblock_400888_2 ], [ 1, %block_40087c ]
  %R24 = phi i64 [ %R17, %subblock_400888_2 ], [ %R12, %block_40087c ]
  ; # 40088e: add    rbx,rbx
  ; r30 := (bv_add r25 r25)
  %R30 = add i64 %R25, %R25
  ; # 400891: cmp    rbp,rbx
  ; # 400894: ja     400888
  ; r31 := (bv_ult r30 r27)
  %R31 = icmp ult i64 %R30, %R27
  br i1 %R31, label %subblock_40088e_1, label %subblock_40088e_2
subblock_40088e_1:
  br label %block_400888
subblock_40088e_2:
  br label %block_400896
block_400896:
  %R37 = phi i128 [ %R22, %subblock_400888_1 ], [ %R29, %subblock_40088e_2 ]
  %R36 = phi i64 [ %R21, %subblock_400888_1 ], [ %R28, %subblock_40088e_2 ]
  %R35 = phi i64 [ %R20, %subblock_400888_1 ], [ %R27, %subblock_40088e_2 ]
  %R34 = phi i64 [ %R19, %subblock_400888_1 ], [ %R26, %subblock_40088e_2 ]
  %R33 = phi i64 [ %R18, %subblock_400888_1 ], [ %R30, %subblock_40088e_2 ]
  %R32 = phi i64 [ %R17, %subblock_400888_1 ], [ %R24, %subblock_40088e_2 ]
  ; # 400896: mov    edi,0x6040c8
  ; # 40089b: sub    rbx,0x1
  ; r38 := (bv_add r33 0xffffffffffffffff :: [64])
  %R38 = add i64 %R33, 18446744073709551615
  ; # 40089f: call   4023a7
  ; r39 := (bv_add r34 0xfffffffffffffff8 :: [64])
  %R39 = add i64 %R34, 18446744073709551608
  ; r41 := (bv_add r39 0x8 :: [64])
  %R41 = add i64 %R39, 8
  %r43 = bitcast i128 %R37 to <2 x double>
  %r44 = call { i64, i64, <2 x double> } @F4023a7(i64 6308040, i64 %R36, i64 %R32, <2 x double> %r43)
  %r45 = extractvalue { i64, i64, <2 x double> } %r44, 2
  %R40 = bitcast <2 x double> %r45 to i128
  br label %block_4008a4
block_4008a4:
  %R49 = phi i128 [ %R40, %block_400896 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R48 = phi i64 [ undef, %block_400896 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R47 = phi i64 [ undef, %block_400896 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R46 = phi i64 [ undef, %block_400896 ]
  %R45 = phi i64 [ %R35, %block_400896 ]
  %R44 = phi i64 [ %R41, %block_400896 ]
  %R43 = phi i64 [ %R38, %block_400896 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R42 = phi i64 [ undef, %block_400896 ]
  ; # 4008a4: mov    rdx,QWORD PTR [rip+0x203815]
  ; r50 := *0x6040c0 :: [64]
  %r55 = inttoptr i64 6308032 to i64*
  %R50 = load i64* %r55
  ; # 4008ab: mov    rax,rdx
  ; # 4008ae: neg    rax
  ; r51 := (bv_sub 0x0 :: [64] r50)
  %R51 = sub i64 0, %R50
  ; # 4008b1: and    rbx,rax
  ; r52 := (bv_and r43 r51)
  %R52 = and i64 %R43, %R51
  ; # 4008b4: mov    rax,0x800000000000000f
  ; # 4008be: cmp    rbp,rax
  ; # 4008c1: jbe    400977
  ; r53 := (bv_ule r45 0x800000000000000f :: [64])
  %R53 = icmp ule i64 %R45, 9223372036854775823
  br i1 %R53, label %subblock_4008a4_1, label %subblock_4008a4_2
subblock_4008a4_1:
  br label %block_400977
subblock_4008a4_2:
  br label %block_4008c7
block_4008c7:
  %R62 = phi i128 [ %R49, %subblock_4008a4_2 ]
  %R61 = phi i64 [ %R48, %subblock_4008a4_2 ]
  %R60 = phi i64 [ %R47, %subblock_4008a4_2 ]
  %R59 = phi i64 [ %R46, %subblock_4008a4_2 ]
  %R58 = phi i64 [ %R45, %subblock_4008a4_2 ]
  %R57 = phi i64 [ %R44, %subblock_4008a4_2 ]
  %R56 = phi i64 [ %R52, %subblock_4008a4_2 ]
  %R55 = phi i64 [ %R50, %subblock_4008a4_2 ]
  %R54 = phi i64 [ %R42, %subblock_4008a4_2 ]
  ; # 4008c7: mov    rax,QWORD PTR [rip+0x2037ea]
  ; r63 := *0x6040b8 :: [64]
  %r69 = inttoptr i64 6308024 to i64*
  %R63 = load i64* %r69
  ; # 4008ce: sub    rax,rdx
  ; r64 := (bv_sub r63 r55)
  %R64 = sub i64 %R63, %R55
  ; # 4008d1: cmp    rbp,rax
  ; r65 := (bv_add r58 r55)
  %R65 = add i64 %R58, %R55
  ; r66 := (bv_eq r63 r65)
  %R66 = icmp eq i64 %R63, %R65
  ; # 4008d4: ja     400927
  ; r67 := (bv_ule r64 r58)
  %R67 = icmp ule i64 %R64, %R58
  ; r68 := (bv_complement r66)
  %R68 = xor i1 %R66, -1
  ; r69 := (bv_and r67 r68)
  %R69 = and i1 %R67, %R68
  br i1 %R69, label %subblock_4008c7_1, label %subblock_4008c7_2
subblock_4008c7_1:
  br label %block_400927
subblock_4008c7_2:
  br label %block_4008d6
block_4008d6:
  %R76 = phi i128 [ %R62, %subblock_4008c7_2 ], [ %R163, %block_400953 ], [ %R105, %subblock_400918_1 ]
  %R75 = phi i64 [ %R61, %subblock_4008c7_2 ], [ %R162, %block_400953 ], [ %R104, %subblock_400918_1 ]
  %R74 = phi i64 [ %R60, %subblock_4008c7_2 ], [ %R161, %block_400953 ], [ %R103, %subblock_400918_1 ]
  %R73 = phi i64 [ %R59, %subblock_4008c7_2 ], [ %R160, %block_400953 ], [ %R102, %subblock_400918_1 ]
  %R72 = phi i64 [ %R58, %subblock_4008c7_2 ], [ %R159, %block_400953 ], [ %R101, %subblock_400918_1 ]
  %R71 = phi i64 [ %R55, %subblock_4008c7_2 ], [ %R157, %block_400953 ], [ %R98, %subblock_400918_1 ]
  %R70 = phi i64 [ %R54, %subblock_4008c7_2 ], [ %R156, %block_400953 ], [ %R97, %subblock_400918_1 ]
  ; # 4008d6: add    rbx,rdx
  ; # 4008d9: add    rbp,rdx
  ; r77 := (bv_add r72 r71)
  %R77 = add i64 %R72, %R71
  ; # 4008dc: mov    edi,0x6040c8
  ; # 4008e1: mov    QWORD PTR [rip+0x2037d8],rbp
  ; *(0x6040c0 :: [64]) = r77
  %r85 = inttoptr i64 6308032 to i64*
  store i64 %R77, i64* %r85
  ; # 4008e8: call   4023e2
  %r86 = bitcast i128 %R76 to <2 x double>
  %r87 = call { i64, i64, <2 x double> } @F4023e2(i64 6308040, i64 %R73, i64 %R71, i64 %R70, i64 %R74, i64 %R75, <2 x double> %r86)
  %R78 = extractvalue { i64, i64, <2 x double> } %r87, 0
  %R79 = extractvalue { i64, i64, <2 x double> } %r87, 1
  %r90 = extractvalue { i64, i64, <2 x double> } %r87, 2
  %R80 = bitcast <2 x double> %r90 to i128
  br label %block_4008ed
block_4008ed:
  ; # 4008ed: mov    rax,rbx
  br label %block_4008f0
block_4008f0:
  ; # 4008f0: add    rsp,0x18
  ; # 4008f4: pop    rbx
  ; # 4008f5: pop    rbp
  ; # 4008f6: ret
  %r92 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r93 = insertvalue { i64, i64, <2 x double> } %r92, i64 undef, 1
  %r94 = insertvalue { i64, i64, <2 x double> } %r93, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r94
block_400900:
  %R84 = phi i128 [ %r0, %subblock_400860_1 ]
  %R83 = phi i64 [ %a1, %subblock_400860_1 ]
  %R82 = phi i64 [ %R4, %subblock_400860_1 ]
  %R81 = phi i64 [ %a2, %subblock_400860_1 ]
  ; # 400900: mov    edi,0x6040c8
  ; # 400905: mov    ebp,0x1
  ; # 40090a: xor    ebx,ebx
  ; # 40090c: call   4023a7
  ; r85 := (bv_add r82 0xfffffffffffffff8 :: [64])
  %R85 = add i64 %R82, 18446744073709551608
  ; r87 := (bv_add r85 0x8 :: [64])
  %R87 = add i64 %R85, 8
  %r101 = bitcast i128 %R84 to <2 x double>
  %r102 = call { i64, i64, <2 x double> } @F4023a7(i64 6308040, i64 %R83, i64 %R81, <2 x double> %r101)
  %r103 = extractvalue { i64, i64, <2 x double> } %r102, 2
  %R86 = bitcast <2 x double> %r103 to i128
  br label %block_400911
block_400911:
  %R95 = phi i128 [ %R86, %block_400900 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R94 = phi i64 [ undef, %block_400900 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R93 = phi i64 [ undef, %block_400900 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R92 = phi i64 [ undef, %block_400900 ]
  %R91 = phi i64 [ 1, %block_400900 ]
  %R90 = phi i64 [ %R87, %block_400900 ]
  %R89 = phi i64 [ 0, %block_400900 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R88 = phi i64 [ undef, %block_400900 ]
  ; # 400911: mov    rdx,QWORD PTR [rip+0x2037a8]
  ; r96 := *0x6040c0 :: [64]
  %r113 = inttoptr i64 6308032 to i64*
  %R96 = load i64* %r113
  br label %block_400918
block_400918:
  %R105 = phi i128 [ %R192, %block_400977 ], [ %R95, %block_400911 ]
  %R104 = phi i64 [ %R191, %block_400977 ], [ %R94, %block_400911 ]
  %R103 = phi i64 [ %R190, %block_400977 ], [ %R93, %block_400911 ]
  %R102 = phi i64 [ %R189, %block_400977 ], [ %R92, %block_400911 ]
  %R101 = phi i64 [ %R193, %block_400977 ], [ %R91, %block_400911 ]
  %R100 = phi i64 [ %R187, %block_400977 ], [ %R90, %block_400911 ]
  %R99 = phi i64 [ %R186, %block_400977 ], [ %R89, %block_400911 ]
  %R98 = phi i64 [ %R185, %block_400977 ], [ %R96, %block_400911 ]
  %R97 = phi i64 [ %R184, %block_400977 ], [ %R88, %block_400911 ]
  ; # 400918: mov    rax,QWORD PTR [rip+0x203799]
  ; r106 := *0x6040b8 :: [64]
  %r124 = inttoptr i64 6308024 to i64*
  %R106 = load i64* %r124
  ; # 40091f: sub    rax,rdx
  ; r107 := (bv_sub r106 r98)
  %R107 = sub i64 %R106, %R98
  ; # 400922: cmp    rbp,rax
  ; r108 := (bv_ult r101 r107)
  %R108 = icmp ult i64 %R101, %R107
  ; r109 := (bv_add r101 r98)
  %R109 = add i64 %R101, %R98
  ; r110 := (bv_eq r106 r109)
  %R110 = icmp eq i64 %R106, %R109
  ; # 400925: jbe    4008d6
  ; r111 := (bv_or r108 r110)
  %R111 = or i1 %R108, %R110
  br i1 %R111, label %subblock_400918_1, label %subblock_400918_2
subblock_400918_1:
  br label %block_4008d6
subblock_400918_2:
  br label %block_400927
block_400927:
  %R115 = phi i128 [ %R62, %subblock_4008c7_1 ], [ %R105, %subblock_400918_2 ]
  %R114 = phi i64 [ %R58, %subblock_4008c7_1 ], [ %R101, %subblock_400918_2 ]
  %R113 = phi i64 [ %R57, %subblock_4008c7_1 ], [ %R100, %subblock_400918_2 ]
  %R112 = phi i64 [ %R56, %subblock_4008c7_1 ], [ %R99, %subblock_400918_2 ]
  ; # 400927: lea    rdi,[rsp+0x8]
  ; r116 := (bv_add r113 0x8 :: [64])
  %R116 = add i64 %R113, 8
  ; # 40092c: mov    QWORD PTR [rsp+0x8],rbp
  ; *(r116) = r114
  %r136 = inttoptr i64 %R116 to i64*
  store i64 %R114, i64* %r136
  ; # 400931: call   4026e0
  ; r117 := (bv_add r113 0xfffffffffffffff8 :: [64])
  %R117 = add i64 %R113, 18446744073709551608
  ; r121 := (bv_add r117 0x8 :: [64])
  %R121 = add i64 %R117, 8
  %r139 = bitcast i128 %R115 to <2 x double>
  %r140 = call { i64, i64, <2 x double> } @F4026e0(i64 %R116, <2 x double> %r139)
  %R118 = extractvalue { i64, i64, <2 x double> } %r140, 0
  %R119 = extractvalue { i64, i64, <2 x double> } %r140, 1
  %r143 = extractvalue { i64, i64, <2 x double> } %r140, 2
  %R120 = bitcast <2 x double> %r143 to i128
  br label %block_400936
block_400936:
  %R131 = phi i128 [ %R120, %block_400927 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R130 = phi i64 [ undef, %block_400927 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R129 = phi i64 [ undef, %block_400927 ]
  %R128 = phi i64 [ %R119, %block_400927 ]
  %R127 = phi i64 [ %R114, %block_400927 ]
  %R126 = phi i64 [ %R121, %block_400927 ]
  %R125 = phi i64 [ %R112, %block_400927 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R124 = phi i64 [ undef, %block_400927 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R123 = phi i64 [ undef, %block_400927 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R122 = phi i64 [ undef, %block_400927 ]
  ; # 400936: test   rax,rax
  ; r132 := (bv_eq r122 0x0 :: [64])
  %R132 = icmp eq i64 %R122, 0
  ; # 400939: je     400980
  br i1 %R132, label %subblock_400936_1, label %subblock_400936_2
subblock_400936_1:
  br label %block_400980
subblock_400936_2:
  br label %block_40093b
block_40093b:
  %R141 = phi i128 [ %R131, %subblock_400936_2 ]
  %R140 = phi i64 [ %R130, %subblock_400936_2 ]
  %R139 = phi i64 [ %R129, %subblock_400936_2 ]
  %R138 = phi i64 [ %R128, %subblock_400936_2 ]
  %R137 = phi i64 [ %R127, %subblock_400936_2 ]
  %R136 = phi i64 [ %R126, %subblock_400936_2 ]
  %R135 = phi i64 [ %R125, %subblock_400936_2 ]
  %R134 = phi i64 [ %R123, %subblock_400936_2 ]
  %R133 = phi i64 [ %R122, %subblock_400936_2 ]
  ; # 40093b: cmp    rax,QWORD PTR [rip+0x203776]
  ; r142 := *0x6040b8 :: [64]
  %r165 = inttoptr i64 6308024 to i64*
  %R142 = load i64* %r165
  ; r143 := (bv_eq r133 r142)
  %R143 = icmp eq i64 %R133, %R142
  ; # 400942: mov    rdx,QWORD PTR [rip+0x203777]
  ; r144 := *0x6040c0 :: [64]
  %r168 = inttoptr i64 6308032 to i64*
  %R144 = load i64* %r168
  ; # 400949: je     400953
  br i1 %R143, label %subblock_40093b_1, label %subblock_40093b_2
subblock_40093b_1:
  br label %block_400953
subblock_40093b_2:
  br label %block_40094b
block_40094b:
  %R153 = phi i128 [ %R141, %subblock_40093b_2 ]
  %R152 = phi i64 [ %R140, %subblock_40093b_2 ]
  %R151 = phi i64 [ %R139, %subblock_40093b_2 ]
  %R150 = phi i64 [ %R138, %subblock_40093b_2 ]
  %R149 = phi i64 [ %R137, %subblock_40093b_2 ]
  %R148 = phi i64 [ %R136, %subblock_40093b_2 ]
  %R147 = phi i64 [ %R135, %subblock_40093b_2 ]
  %R146 = phi i64 [ %R134, %subblock_40093b_2 ]
  %R145 = phi i64 [ %R133, %subblock_40093b_2 ]
  ; # 40094b: sub    rbp,rbx
  ; r154 := (bv_sub r149 r147)
  %R154 = sub i64 %R149, %R147
  ; # 40094e: mov    rdx,rax
  ; # 400951: xor    ebx,ebx
  br label %block_400953
block_400953:
  %R163 = phi i128 [ %R153, %block_40094b ], [ %R141, %subblock_40093b_1 ]
  %R162 = phi i64 [ %R152, %block_40094b ], [ %R140, %subblock_40093b_1 ]
  %R161 = phi i64 [ %R151, %block_40094b ], [ %R139, %subblock_40093b_1 ]
  %R160 = phi i64 [ %R150, %block_40094b ], [ %R138, %subblock_40093b_1 ]
  %R159 = phi i64 [ %R154, %block_40094b ], [ %R137, %subblock_40093b_1 ]
  %R158 = phi i64 [ %R148, %block_40094b ], [ %R136, %subblock_40093b_1 ]
  %R157 = phi i64 [ %R145, %block_40094b ], [ %R144, %subblock_40093b_1 ]
  %R156 = phi i64 [ %R146, %block_40094b ], [ %R134, %subblock_40093b_1 ]
  %R155 = phi i64 [ %R145, %block_40094b ], [ %R133, %subblock_40093b_1 ]
  ; # 400953: add    rax,QWORD PTR [rsp+0x8]
  ; r164 := (bv_add r158 0x8 :: [64])
  %R164 = add i64 %R158, 8
  ; r165 := *r164
  %r190 = inttoptr i64 %R164 to i64*
  %R165 = load i64* %r190
  ; r166 := (bv_add r155 r165)
  %R166 = add i64 %R155, %R165
  ; # 400958: mov    QWORD PTR [rip+0x203759],rax
  ; *(0x6040b8 :: [64]) = r166
  %r193 = inttoptr i64 6308024 to i64*
  store i64 %R166, i64* %r193
  ; # 40095f: jmp    4008d6
  br label %block_4008d6
block_400964:
  %R171 = phi i128 [ %R10, %subblock_40086f_1 ]
  %R170 = phi i64 [ %R8, %subblock_40086f_1 ]
  %R169 = phi i64 [ %R9, %subblock_40086f_1 ]
  %R168 = phi i64 [ %R7, %subblock_40086f_1 ]
  %R167 = phi i64 [ %R6, %subblock_40086f_1 ]
  ; # 400964: mov    edi,0x6040c8
  ; # 400969: xor    ebx,ebx
  ; # 40096b: call   4023a7
  ; r172 := (bv_add r168 0xfffffffffffffff8 :: [64])
  %R172 = add i64 %R168, 18446744073709551608
  ; r174 := (bv_add r172 0x8 :: [64])
  %R174 = add i64 %R172, 8
  %r201 = bitcast i128 %R171 to <2 x double>
  %r202 = call { i64, i64, <2 x double> } @F4023a7(i64 6308040, i64 %R170, i64 %R167, <2 x double> %r201)
  %r203 = extractvalue { i64, i64, <2 x double> } %r202, 2
  %R173 = bitcast <2 x double> %r203 to i128
  br label %block_400970
block_400970:
  %R182 = phi i128 [ %R173, %block_400964 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R181 = phi i64 [ undef, %block_400964 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R180 = phi i64 [ undef, %block_400964 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R179 = phi i64 [ undef, %block_400964 ]
  %R178 = phi i64 [ %R169, %block_400964 ]
  %R177 = phi i64 [ %R174, %block_400964 ]
  %R176 = phi i64 [ 0, %block_400964 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R175 = phi i64 [ undef, %block_400964 ]
  ; # 400970: mov    rdx,QWORD PTR [rip+0x203749]
  ; r183 := *0x6040c0 :: [64]
  %r213 = inttoptr i64 6308032 to i64*
  %R183 = load i64* %r213
  br label %block_400977
block_400977:
  %R192 = phi i128 [ %R49, %subblock_4008a4_1 ], [ %R182, %block_400970 ]
  %R191 = phi i64 [ %R48, %subblock_4008a4_1 ], [ %R181, %block_400970 ]
  %R190 = phi i64 [ %R47, %subblock_4008a4_1 ], [ %R180, %block_400970 ]
  %R189 = phi i64 [ %R46, %subblock_4008a4_1 ], [ %R179, %block_400970 ]
  %R188 = phi i64 [ %R45, %subblock_4008a4_1 ], [ %R178, %block_400970 ]
  %R187 = phi i64 [ %R44, %subblock_4008a4_1 ], [ %R177, %block_400970 ]
  %R186 = phi i64 [ %R52, %subblock_4008a4_1 ], [ %R176, %block_400970 ]
  %R185 = phi i64 [ %R50, %subblock_4008a4_1 ], [ %R183, %block_400970 ]
  %R184 = phi i64 [ %R42, %subblock_4008a4_1 ], [ %R175, %block_400970 ]
  ; # 400977: add    rbp,rbx
  ; r193 := (bv_add r188 r186)
  %R193 = add i64 %R188, %R186
  ; # 40097a: jmp    400918
  br label %block_400918
block_400980:
  %R199 = phi i128 [ %R131, %subblock_400936_1 ]
  %R198 = phi i64 [ %R130, %subblock_400936_1 ]
  %R197 = phi i64 [ %R129, %subblock_400936_1 ]
  %R196 = phi i64 [ %R128, %subblock_400936_1 ]
  %R195 = phi i64 [ %R124, %subblock_400936_1 ]
  %R194 = phi i64 [ %R123, %subblock_400936_1 ]
  ; # 400980: mov    edi,0x6040c8
  ; # 400985: call   4023e2
  %r231 = bitcast i128 %R199 to <2 x double>
  %r232 = call { i64, i64, <2 x double> } @F4023e2(i64 6308040, i64 %R196, i64 %R195, i64 %R194, i64 %R197, i64 %R198, <2 x double> %r231)
  %R200 = extractvalue { i64, i64, <2 x double> } %r232, 0
  %R201 = extractvalue { i64, i64, <2 x double> } %r232, 1
  %r235 = extractvalue { i64, i64, <2 x double> } %r232, 2
  %R202 = bitcast <2 x double> %r235 to i128
  br label %block_40098a
block_40098a:
  ; # 40098a: xor    eax,eax
  ; # 40098c: jmp    4008f0
  br label %block_4008f0
failure:
  br label %failure
}