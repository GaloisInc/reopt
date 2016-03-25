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
declare { i64, i64, <2 x double> } @F4009a0(i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400bf0(i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401b0a(i64, i64, i64)
declare { i64, i64, <2 x double> } @F401c4a(i64, i64)
declare { i64, i64, <2 x double> } @F40241c(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400e40(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_400e40
block_400e40:
  ; r0 := (alloca 0x60 :: [64])
  %r1 = alloca i8, i64 96
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x60 :: [64])
  %R1 = add i64 %R0, 96
  ; # 400e40: test   rdi,rdi
  ; r2 := (bv_eq arg0 0x0 :: [64])
  %R2 = icmp eq i64 %a0, 0
  ; # 400e43: je     4011fe
  br i1 %R2, label %subblock_400e40_1, label %subblock_400e40_2
subblock_400e40_1:
  br label %block_4011fe
subblock_400e40_2:
  br label %block_400e49
block_400e49:
  %R16 = phi i128 [ %r0, %subblock_400e40_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r15
  %R15 = phi i64 [ undef, %subblock_400e40_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r14
  %R14 = phi i64 [ undef, %subblock_400e40_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r13
  %R13 = phi i64 [ undef, %subblock_400e40_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r12
  %R12 = phi i64 [ undef, %subblock_400e40_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r10
  %R11 = phi i64 [ undef, %subblock_400e40_2 ]
  %R10 = phi i64 [ %a5, %subblock_400e40_2 ]
  %R9 = phi i64 [ %a4, %subblock_400e40_2 ]
  %R8 = phi i64 [ %a0, %subblock_400e40_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbp
  %R7 = phi i64 [ undef, %subblock_400e40_2 ]
  %R6 = phi i64 [ %R1, %subblock_400e40_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbx
  %R5 = phi i64 [ undef, %subblock_400e40_2 ]
  %R4 = phi i64 [ %a2, %subblock_400e40_2 ]
  %R3 = phi i64 [ %a3, %subblock_400e40_2 ]
  ; # 400e49: push   r15
  ; r17 := (bv_add r6 0xfffffffffffffff8 :: [64])
  %R17 = add i64 %R6, 18446744073709551608
  ; *(r17) = r15
  %r20 = inttoptr i64 %R17 to i64*
  store i64 %R15, i64* %r20
  ; # 400e4b: push   r14
  ; r18 := (bv_add r6 0xfffffffffffffff0 :: [64])
  %R18 = add i64 %R6, 18446744073709551600
  ; *(r18) = r14
  %r22 = inttoptr i64 %R18 to i64*
  store i64 %R14, i64* %r22
  ; # 400e4d: push   r13
  ; r19 := (bv_add r6 0xffffffffffffffe8 :: [64])
  %R19 = add i64 %R6, 18446744073709551592
  ; *(r19) = r13
  %r24 = inttoptr i64 %R19 to i64*
  store i64 %R13, i64* %r24
  ; # 400e4f: push   r12
  ; r20 := (bv_add r6 0xffffffffffffffe0 :: [64])
  %R20 = add i64 %R6, 18446744073709551584
  ; *(r20) = r12
  %r26 = inttoptr i64 %R20 to i64*
  store i64 %R12, i64* %r26
  ; # 400e51: push   rbp
  ; r21 := (bv_add r6 0xffffffffffffffd8 :: [64])
  %R21 = add i64 %R6, 18446744073709551576
  ; *(r21) = r7
  %r28 = inttoptr i64 %R21 to i64*
  store i64 %R7, i64* %r28
  ; # 400e52: push   rbx
  ; r22 := (bv_add r6 0xffffffffffffffd0 :: [64])
  %R22 = add i64 %R6, 18446744073709551568
  ; *(r22) = r5
  %r30 = inttoptr i64 %R22 to i64*
  store i64 %R5, i64* %r30
  ; # 400e53: lea    rbx,[rdi-0x10]
  ; r23 := (bv_add r8 0xfffffffffffffff0 :: [64])
  %R23 = add i64 %R8, 18446744073709551600
  ; # 400e57: sub    rsp,0x28
  ; r24 := (bv_add r6 0xffffffffffffffa8 :: [64])
  %R24 = add i64 %R6, 18446744073709551528
  ; # 400e5b: mov    rsi,QWORD PTR [rdi-0x8]
  ; r25 := (bv_add r8 0xfffffffffffffff8 :: [64])
  %R25 = add i64 %R8, 18446744073709551608
  ; r26 := *r25
  %r34 = inttoptr i64 %R25 to i64*
  %R26 = load i64* %r34
  ; # 400e5f: test   sil,0x1
  ; r27 := (trunc r26 8)
  %R27 = trunc i64 %R26 to i8
  ; r28 := (bv_and r27 0x1 :: [8])
  %R28 = and i8 %R27, 1
  ; r29 := (bv_eq r28 0x0 :: [8])
  %R29 = icmp eq i8 %R28, 0
  ; # 400e63: je     401080
  br i1 %R29, label %subblock_400e49_1, label %subblock_400e49_2
subblock_400e49_1:
  br label %block_401080
subblock_400e49_2:
  br label %block_400e69
block_400e69:
  %R38 = phi i128 [ %R16, %subblock_400e49_2 ]
  %R37 = phi i64 [ %R11, %subblock_400e49_2 ]
  %R36 = phi i64 [ %R10, %subblock_400e49_2 ]
  %R35 = phi i64 [ %R9, %subblock_400e49_2 ]
  %R34 = phi i64 [ %R26, %subblock_400e49_2 ]
  %R33 = phi i64 [ %R24, %subblock_400e49_2 ]
  %R32 = phi i64 [ %R23, %subblock_400e49_2 ]
  %R31 = phi i64 [ %R4, %subblock_400e49_2 ]
  %R30 = phi i64 [ %R3, %subblock_400e49_2 ]
  ; # 400e69: mov    rax,rsi
  ; # 400e6c: and    rax,0xfffffffffffffffe
  ; r39 := (bv_and r34 0xfffffffffffffffe :: [64])
  %R39 = and i64 %R34, 18446744073709551614
  ; # 400e70: lea    r13,[rbx+rax*1]
  ; r40 := (bv_add r32 r39)
  %R40 = add i64 %R32, %R39
  ; # 400e74: mov    QWORD PTR [rsp+0x18],rax
  ; r41 := (bv_add r33 0x18 :: [64])
  %R41 = add i64 %R33, 24
  ; *(r41) = r39
  %r51 = inttoptr i64 %R41 to i64*
  store i64 %R39, i64* %r51
  ; # 400e79: cmp    rsi,QWORD PTR [r13]
  ; r42 := *r40
  %r52 = inttoptr i64 %R40 to i64*
  %R42 = load i64* %r52
  ; r43 := (bv_eq r34 r42)
  %R43 = icmp eq i64 %R34, %R42
  ; # 400e7d: je     400e85
  br i1 %R43, label %subblock_400e69_1, label %subblock_400e69_2
subblock_400e69_1:
  br label %block_400e85
subblock_400e69_2:
  br label %block_400e7f
block_400e7f:
  %R53 = phi i128 [ %R38, %subblock_400e69_2 ]
  %R52 = phi i64 [ %R40, %subblock_400e69_2 ]
  %R51 = phi i64 [ %R37, %subblock_400e69_2 ]
  %R50 = phi i64 [ %R36, %subblock_400e69_2 ]
  %R49 = phi i64 [ %R35, %subblock_400e69_2 ]
  %R48 = phi i64 [ %R34, %subblock_400e69_2 ]
  %R47 = phi i64 [ %R33, %subblock_400e69_2 ]
  %R46 = phi i64 [ %R32, %subblock_400e69_2 ]
  %R45 = phi i64 [ %R31, %subblock_400e69_2 ]
  %R44 = phi i64 [ %R30, %subblock_400e69_2 ]
  ; # 400e7f: hlt
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception GeneralProtectionException 0 ()
  ; # 400e80: mov    rax,QWORD PTR [rsp+0x18]
  ; r54 := (bv_add r47 0x18 :: [64])
  %R54 = add i64 %R47, 24
  ; r55 := *r54
  %r66 = inttoptr i64 %R54 to i64*
  %R55 = load i64* %r66
  br label %block_400e85
block_400e85:
  %R66 = phi i128 [ %R53, %block_400e7f ], [ %R38, %subblock_400e69_1 ]
  %R65 = phi i64 [ %R52, %block_400e7f ], [ %R40, %subblock_400e69_1 ]
  %R64 = phi i64 [ %R51, %block_400e7f ], [ %R37, %subblock_400e69_1 ]
  %R63 = phi i64 [ %R50, %block_400e7f ], [ %R36, %subblock_400e69_1 ]
  %R62 = phi i64 [ %R49, %block_400e7f ], [ %R35, %subblock_400e69_1 ]
  %R61 = phi i64 [ %R48, %block_400e7f ], [ %R34, %subblock_400e69_1 ]
  %R60 = phi i64 [ %R47, %block_400e7f ], [ %R33, %subblock_400e69_1 ]
  %R59 = phi i64 [ %R46, %block_400e7f ], [ %R32, %subblock_400e69_1 ]
  %R58 = phi i64 [ %R45, %block_400e7f ], [ %R31, %subblock_400e69_1 ]
  %R57 = phi i64 [ %R44, %block_400e7f ], [ %R30, %subblock_400e69_1 ]
  %R56 = phi i64 [ %R55, %block_400e7f ], [ %R39, %subblock_400e69_1 ]
  ; # 400e85: mov    QWORD PTR [rsp+0x8],rax
  ; r67 := (bv_add r60 0x8 :: [64])
  %R67 = add i64 %R60, 8
  ; *(r67) = r56
  %r80 = inttoptr i64 %R67 to i64*
  store i64 %R56, i64* %r80
  ; # 400e8a: mov    DWORD PTR [rsp+0x14],0x0
  ; r68 := (bv_add r60 0x14 :: [64])
  %R68 = add i64 %R60, 20
  ; *(r68) = 0x0 :: [32]
  %r82 = inttoptr i64 %R68 to i32*
  store i32 0, i32* %r82
  ; # 400e92: mov    r12d,0x1
  ; # 400e98: nop    [rax+rax*1]
  br label %block_400ea0
block_400ea0:
  %R79 = phi i128 [ %R469, %subblock_40102f_1 ], [ %R66, %block_400e85 ]
  %R78 = phi i64 [ %R468, %subblock_40102f_1 ], [ %R65, %block_400e85 ]
  %R77 = phi i64 [ %R467, %subblock_40102f_1 ], [ 1, %block_400e85 ]
  %R76 = phi i64 [ %R466, %subblock_40102f_1 ], [ %R64, %block_400e85 ]
  %R75 = phi i64 [ %R465, %subblock_40102f_1 ], [ %R63, %block_400e85 ]
  %R74 = phi i64 [ %R464, %subblock_40102f_1 ], [ %R62, %block_400e85 ]
  %R73 = phi i64 [ %R463, %subblock_40102f_1 ], [ %R61, %block_400e85 ]
  %R72 = phi i64 [ %R462, %subblock_40102f_1 ], [ %R60, %block_400e85 ]
  %R71 = phi i64 [ %R461, %subblock_40102f_1 ], [ %R59, %block_400e85 ]
  %R70 = phi i64 [ %R460, %subblock_40102f_1 ], [ %R58, %block_400e85 ]
  %R69 = phi i64 [ %R459, %subblock_40102f_1 ], [ %R57, %block_400e85 ]
  ; # 400ea0: mov    rax,QWORD PTR [rbx]
  ; r80 := *r71
  %r94 = inttoptr i64 %R71 to i64*
  %R80 = load i64* %r94
  ; # 400ea3: and    rax,QWORD PTR [r13+0x8]
  ; r81 := (bv_add r78 0x8 :: [64])
  %R81 = add i64 %R78, 8
  ; r82 := *r81
  %r97 = inttoptr i64 %R81 to i64*
  %R82 = load i64* %r97
  ; r83 := (trunc r80 8)
  %R83 = trunc i64 %R80 to i8
  ; r84 := (trunc r82 8)
  %R84 = trunc i64 %R82 to i8
  ; r85 := (bv_and r83 r84)
  %R85 = and i8 %R83, %R84
  ; # 400ea7: test   al,0x1
  ; r86 := (bv_and r85 0x1 :: [8])
  %R86 = and i8 %R85, 1
  ; r87 := (bv_eq r86 0x0 :: [8])
  %R87 = icmp eq i8 %R86, 0
  ; # 400ea9: je     400fe0
  br i1 %R87, label %subblock_400ea0_1, label %subblock_400ea0_2
subblock_400ea0_1:
  br label %block_400fe0
subblock_400ea0_2:
  br label %block_400eaf
block_400eaf:
  %R97 = phi i128 [ %R520, %subblock_401068_1 ], [ %R79, %subblock_400ea0_2 ]
  %R96 = phi i64 [ %R521, %subblock_401068_1 ], [ %R78, %subblock_400ea0_2 ]
  %R95 = phi i64 [ %R518, %subblock_401068_1 ], [ %R77, %subblock_400ea0_2 ]
  %R94 = phi i64 [ %R517, %subblock_401068_1 ], [ %R76, %subblock_400ea0_2 ]
  %R93 = phi i64 [ %R516, %subblock_401068_1 ], [ %R75, %subblock_400ea0_2 ]
  %R92 = phi i64 [ %R515, %subblock_401068_1 ], [ %R74, %subblock_400ea0_2 ]
  %R91 = phi i64 [ %R514, %subblock_401068_1 ], [ %R73, %subblock_400ea0_2 ]
  %R90 = phi i64 [ %R513, %subblock_401068_1 ], [ %R72, %subblock_400ea0_2 ]
  %R89 = phi i64 [ %R512, %subblock_401068_1 ], [ %R71, %subblock_400ea0_2 ]
  %R88 = phi i64 [ %R511, %subblock_401068_1 ], [ %R70, %subblock_400ea0_2 ]
  ; # 400eaf: mov    rcx,QWORD PTR [rsp+0x8]
  ; r98 := (bv_add r90 0x8 :: [64])
  %R98 = add i64 %R90, 8
  ; r99 := *r98
  %r115 = inttoptr i64 %R98 to i64*
  %R99 = load i64* %r115
  ; # 400eb4: mov    rax,rcx
  ; # 400eb7: or     rax,0x1
  ; r100 := (bv_or r99 0x1 :: [64])
  %R100 = or i64 %R99, 1
  ; # 400ebb: mov    QWORD PTR [rbx+0x8],rax
  ; r101 := (bv_add r89 0x8 :: [64])
  %R101 = add i64 %R89, 8
  ; *(r101) = r100
  %r119 = inttoptr i64 %R101 to i64*
  store i64 %R100, i64* %r119
  ; # 400ebf: mov    QWORD PTR [r13],rax
  ; *(r96) = r100
  %r120 = inttoptr i64 %R96 to i64*
  store i64 %R100, i64* %r120
  ; # 400ec3: mov    rax,rcx
  ; # 400ec6: shr    rax,0x5
  ; r102 := (bv_shr r99 0x5 :: [64])
  %R102 = lshr i64 %R99, 5
  ; # 400eca: sub    rax,0x1
  ; r103 := (bv_add r102 0xffffffffffffffff :: [64])
  %R103 = add i64 %R102, 18446744073709551615
  ; # 400ece: cmp    rax,0x20
  ; r104 := (bv_ult r103 0x20 :: [64])
  %R104 = icmp ult i64 %R103, 32
  ; r105 := (bv_eq r102 0x21 :: [64])
  %R105 = icmp eq i64 %R102, 33
  ; # 400ed2: jbe    400ef8
  ; r106 := (bv_or r104 r105)
  %R106 = or i1 %R104, %R105
  br i1 %R106, label %subblock_400eaf_1, label %subblock_400eaf_2
subblock_400eaf_1:
  br label %block_400ef8
subblock_400eaf_2:
  br label %block_400ed4
block_400ed4:
  %R118 = phi i128 [ %R97, %subblock_400eaf_2 ]
  %R117 = phi i64 [ %R96, %subblock_400eaf_2 ]
  %R116 = phi i64 [ %R95, %subblock_400eaf_2 ]
  %R115 = phi i64 [ %R94, %subblock_400eaf_2 ]
  %R114 = phi i64 [ %R93, %subblock_400eaf_2 ]
  %R113 = phi i64 [ %R92, %subblock_400eaf_2 ]
  %R112 = phi i64 [ %R91, %subblock_400eaf_2 ]
  %R111 = phi i64 [ %R90, %subblock_400eaf_2 ]
  %R110 = phi i64 [ %R89, %subblock_400eaf_2 ]
  %R109 = phi i64 [ %R88, %subblock_400eaf_2 ]
  %R108 = phi i64 [ %R99, %subblock_400eaf_2 ]
  %R107 = phi i64 [ %R103, %subblock_400eaf_2 ]
  ; # 400ed4: cmp    rax,0x1c00
  ; # 400eda: mov    DWORD PTR [rsp+0x10],0x3f
  ; r119 := (bv_add r111 0x10 :: [64])
  %R119 = add i64 %R111, 16
  ; *(r119) = 0x3f :: [32]
  %r139 = inttoptr i64 %R119 to i32*
  store i32 63, i32* %r139
  ; # 400ee2: ja     400efc
  ; r120 := (bv_ult 0x1c00 :: [64] r107)
  %R120 = icmp ult i64 7168, %R107
  br i1 %R120, label %subblock_400ed4_1, label %subblock_400ed4_2
subblock_400ed4_1:
  br label %block_400efc
subblock_400ed4_2:
  br label %block_400ee4
block_400ee4:
  %R131 = phi i64 [ %R117, %subblock_400ed4_2 ]
  %R130 = phi i64 [ %R116, %subblock_400ed4_2 ]
  %R129 = phi i64 [ %R115, %subblock_400ed4_2 ]
  %R128 = phi i64 [ %R114, %subblock_400ed4_2 ]
  %R127 = phi i64 [ %R113, %subblock_400ed4_2 ]
  %R126 = phi i64 [ %R112, %subblock_400ed4_2 ]
  %R125 = phi i64 [ %R111, %subblock_400ed4_2 ]
  %R124 = phi i64 [ %R110, %subblock_400ed4_2 ]
  %R123 = phi i64 [ %R109, %subblock_400ed4_2 ]
  %R122 = phi i64 [ %R108, %subblock_400ed4_2 ]
  %R121 = phi i64 [ %R107, %subblock_400ed4_2 ]
  ; # 400ee4: pxor   xmm0,xmm0
  ; # 400ee8: cvtsi2ss xmm0,eax
  ; r132 := (trunc r121 32)
  %R132 = trunc i64 %R121 to i32
  ; r133 := (fpFromBV r132 single)
  %R133 = sitofp i32 %R132 to float
  ; r134 := (uext r133 128)
  %R134 = zext i32 %R133 to i128
  ; # 400eec: movq   eax,xmm0
  ; # 400ef0: shr    eax,0x15
  ; r135 := (bv_shr r133 0x15 :: [32])
  %R135 = lshr i32 %R133, 21
  ; # 400ef3: sub    eax,0x1f0
  ; r136 := (bv_add r135 0xfffffe10 :: [32])
  %R136 = add i32 %R135, 4294966800
  ; r137 := (uext r136 64)
  %R137 = zext i32 %R136 to i64
  br label %block_400ef8
block_400ef8:
  %R149 = phi i128 [ %R134, %block_400ee4 ], [ %R97, %subblock_400eaf_1 ]
  %R148 = phi i64 [ %R131, %block_400ee4 ], [ %R96, %subblock_400eaf_1 ]
  %R147 = phi i64 [ %R130, %block_400ee4 ], [ %R95, %subblock_400eaf_1 ]
  %R146 = phi i64 [ %R129, %block_400ee4 ], [ %R94, %subblock_400eaf_1 ]
  %R145 = phi i64 [ %R128, %block_400ee4 ], [ %R93, %subblock_400eaf_1 ]
  %R144 = phi i64 [ %R127, %block_400ee4 ], [ %R92, %subblock_400eaf_1 ]
  %R143 = phi i64 [ %R126, %block_400ee4 ], [ %R91, %subblock_400eaf_1 ]
  %R142 = phi i64 [ %R125, %block_400ee4 ], [ %R90, %subblock_400eaf_1 ]
  %R141 = phi i64 [ %R124, %block_400ee4 ], [ %R89, %subblock_400eaf_1 ]
  %R140 = phi i64 [ %R123, %block_400ee4 ], [ %R88, %subblock_400eaf_1 ]
  %R139 = phi i64 [ %R122, %block_400ee4 ], [ %R99, %subblock_400eaf_1 ]
  %R138 = phi i64 [ %R137, %block_400ee4 ], [ %R103, %subblock_400eaf_1 ]
  ; # 400ef8: mov    DWORD PTR [rsp+0x10],eax
  ; r150 := (trunc r138 32)
  %R150 = trunc i64 %R138 to i32
  ; r151 := (bv_add r142 0x10 :: [64])
  %R151 = add i64 %R142, 16
  ; *(r151) = r150
  %r172 = inttoptr i64 %R151 to i32*
  store i32 %R150, i32* %r172
  br label %block_400efc
block_400efc:
  %R162 = phi i128 [ %R118, %subblock_400ed4_1 ], [ %R149, %block_400ef8 ]
  %R161 = phi i64 [ %R117, %subblock_400ed4_1 ], [ %R148, %block_400ef8 ]
  %R160 = phi i64 [ %R116, %subblock_400ed4_1 ], [ %R147, %block_400ef8 ]
  %R159 = phi i64 [ %R115, %subblock_400ed4_1 ], [ %R146, %block_400ef8 ]
  %R158 = phi i64 [ %R114, %subblock_400ed4_1 ], [ %R145, %block_400ef8 ]
  %R157 = phi i64 [ %R113, %subblock_400ed4_1 ], [ %R144, %block_400ef8 ]
  %R156 = phi i64 [ %R112, %subblock_400ed4_1 ], [ %R143, %block_400ef8 ]
  %R155 = phi i64 [ %R111, %subblock_400ed4_1 ], [ %R142, %block_400ef8 ]
  %R154 = phi i64 [ %R110, %subblock_400ed4_1 ], [ %R141, %block_400ef8 ]
  %R153 = phi i64 [ %R109, %subblock_400ed4_1 ], [ %R140, %block_400ef8 ]
  %R152 = phi i64 [ %R108, %subblock_400ed4_1 ], [ %R139, %block_400ef8 ]
  ; # 400efc: movsxd r14,DWORD PTR [rsp+0x10]
  ; r163 := (bv_add r155 0x10 :: [64])
  %R163 = add i64 %R155, 16
  ; r164 := *r163
  %r185 = inttoptr i64 %R163 to i32*
  %R164 = load i32* %r185
  ; r165 := (sext r164 64)
  %R165 = sext i32 %R164 to i64
  ; # 400f01: lea    rax,[r14+r14*2]
  ; r166 := (bv_mul 0x2 :: [64] r165)
  %R166 = mul i64 2, %R165
  ; r167 := (bv_add r165 r166)
  %R167 = add i64 %R165, %R166
  ; # 400f05: lea    r15,[rax*8+0x6040e0]
  ; r168 := (bv_mul 0x8 :: [64] r167)
  %R168 = mul i64 8, %R167
  ; r169 := (bv_add r168 0x6040e0 :: [64])
  %R169 = add i64 %R168, 6308064
  ; # 400f0d: mov    eax,DWORD PTR [rip+0x203a79]
  ; r170 := *0x60498c :: [64]
  %r192 = inttoptr i64 6310284 to i32*
  %R170 = load i32* %r192
  ; # 400f13: lea    rbp,[r15+0x8]
  ; r171 := (bv_add r168 0x6040e8 :: [64])
  %R171 = add i64 %R168, 6308072
  ; # 400f17: test   eax,eax
  ; r172 := (bv_eq r170 0x0 :: [32])
  %R172 = icmp eq i32 %R170, 0
  ; # 400f19: jne    401200
  ; r173 := (bv_complement r172)
  %R173 = xor i1 %R172, -1
  br i1 %R173, label %subblock_400efc_1, label %subblock_400efc_2
subblock_400efc_1:
  br label %block_401200
subblock_400efc_2:
  br label %block_400f1f
block_400f1f:
  %R187 = phi i128 [ %R882, %block_401240 ], [ %R825, %subblock_401200_1 ], [ %R162, %subblock_400efc_2 ]
  %R186 = phi i64 [ %R881, %block_401240 ], [ %R824, %subblock_401200_1 ], [ %R169, %subblock_400efc_2 ]
  %R185 = phi i64 [ %R880, %block_401240 ], [ %R823, %subblock_401200_1 ], [ %R165, %subblock_400efc_2 ]
  %R184 = phi i64 [ %R879, %block_401240 ], [ %R822, %subblock_401200_1 ], [ %R161, %subblock_400efc_2 ]
  %R183 = phi i64 [ %R878, %block_401240 ], [ %R821, %subblock_401200_1 ], [ %R160, %subblock_400efc_2 ]
  %R182 = phi i64 [ %R877, %block_401240 ], [ %R820, %subblock_401200_1 ], [ %R159, %subblock_400efc_2 ]
  %R181 = phi i64 [ %R876, %block_401240 ], [ %R819, %subblock_401200_1 ], [ %R158, %subblock_400efc_2 ]
  %R180 = phi i64 [ %R875, %block_401240 ], [ %R818, %subblock_401200_1 ], [ %R157, %subblock_400efc_2 ]
  %R179 = phi i64 [ %R874, %block_401240 ], [ %R817, %subblock_401200_1 ], [ %R156, %subblock_400efc_2 ]
  %R178 = phi i64 [ %R873, %block_401240 ], [ %R816, %subblock_401200_1 ], [ %R171, %subblock_400efc_2 ]
  %R177 = phi i64 [ %R872, %block_401240 ], [ %R815, %subblock_401200_1 ], [ %R155, %subblock_400efc_2 ]
  %R176 = phi i64 [ %R871, %block_401240 ], [ %R814, %subblock_401200_1 ], [ %R154, %subblock_400efc_2 ]
  %R175 = phi i64 [ %R870, %block_401240 ], [ %R813, %subblock_401200_1 ], [ %R153, %subblock_400efc_2 ]
  %R174 = phi i64 [ %R869, %block_401240 ], [ %R812, %subblock_401200_1 ], [ %R152, %subblock_400efc_2 ]
  ; # 400f1f: lea    rax,[r14+r14*2]
  ; r188 := (bv_mul 0x2 :: [64] r185)
  %R188 = mul i64 2, %R185
  ; r189 := (bv_add r185 r188)
  %R189 = add i64 %R185, %R188
  ; # 400f23: lea    rax,[rax*8+0x6040e0]
  ; r190 := (bv_mul 0x8 :: [64] r189)
  %R190 = mul i64 8, %R189
  ; r191 := (bv_add r190 0x6040e0 :: [64])
  %R191 = add i64 %R190, 6308064
  ; # 400f2b: cmp    QWORD PTR [rax+0x10],0x0
  ; r192 := (bv_add r190 0x6040f0 :: [64])
  %R192 = add i64 %R190, 6308080
  ; r193 := *r192
  %r216 = inttoptr i64 %R192 to i64*
  %R193 = load i64* %r216
  ; r194 := (bv_eq r193 0x0 :: [64])
  %R194 = icmp eq i64 %R193, 0
  ; # 400f30: je     401248
  br i1 %R194, label %subblock_400f1f_1, label %subblock_400f1f_2
subblock_400f1f_1:
  br label %block_401248
subblock_400f1f_2:
  br label %block_400f36
block_400f36:
  %R208 = phi i128 [ %R897, %block_401248 ], [ %R187, %subblock_400f1f_2 ]
  %R207 = phi i64 [ %R896, %block_401248 ], [ %R186, %subblock_400f1f_2 ]
  %R206 = phi i64 [ %R895, %block_401248 ], [ %R185, %subblock_400f1f_2 ]
  %R205 = phi i64 [ %R894, %block_401248 ], [ %R184, %subblock_400f1f_2 ]
  %R204 = phi i64 [ %R893, %block_401248 ], [ %R183, %subblock_400f1f_2 ]
  %R203 = phi i64 [ %R892, %block_401248 ], [ %R182, %subblock_400f1f_2 ]
  %R202 = phi i64 [ %R891, %block_401248 ], [ %R181, %subblock_400f1f_2 ]
  %R201 = phi i64 [ %R890, %block_401248 ], [ %R180, %subblock_400f1f_2 ]
  %R200 = phi i64 [ %R889, %block_401248 ], [ %R179, %subblock_400f1f_2 ]
  %R199 = phi i64 [ %R888, %block_401248 ], [ %R178, %subblock_400f1f_2 ]
  %R198 = phi i64 [ %R887, %block_401248 ], [ %R177, %subblock_400f1f_2 ]
  %R197 = phi i64 [ %R886, %block_401248 ], [ %R176, %subblock_400f1f_2 ]
  %R196 = phi i64 [ %R885, %block_401248 ], [ %R175, %subblock_400f1f_2 ]
  %R195 = phi i64 [ %R884, %block_401248 ], [ %R174, %subblock_400f1f_2 ]
  ; # 400f36: mov    eax,DWORD PTR [rip+0x203a50]
  ; r209 := *0x60498c :: [64]
  %r233 = inttoptr i64 6310284 to i32*
  %R209 = load i32* %r233
  ; # 400f3c: test   eax,eax
  ; r210 := (bv_eq r209 0x0 :: [32])
  %R210 = icmp eq i32 %R209, 0
  ; # 400f3e: jne    4010b0
  ; r211 := (bv_complement r210)
  %R211 = xor i1 %R210, -1
  br i1 %R211, label %subblock_400f36_1, label %subblock_400f36_2
subblock_400f36_1:
  br label %block_4010b0
subblock_400f36_2:
  br label %block_400f44
block_400f44:
  %R225 = phi i128 [ %R571, %subblock_4010b0_1 ], [ %R208, %subblock_400f36_2 ]
  %R224 = phi i64 [ %R570, %subblock_4010b0_1 ], [ %R207, %subblock_400f36_2 ]
  %R223 = phi i64 [ %R569, %subblock_4010b0_1 ], [ %R206, %subblock_400f36_2 ]
  %R222 = phi i64 [ %R568, %subblock_4010b0_1 ], [ %R205, %subblock_400f36_2 ]
  %R221 = phi i64 [ %R567, %subblock_4010b0_1 ], [ %R204, %subblock_400f36_2 ]
  %R220 = phi i64 [ %R566, %subblock_4010b0_1 ], [ %R203, %subblock_400f36_2 ]
  %R219 = phi i64 [ %R565, %subblock_4010b0_1 ], [ %R202, %subblock_400f36_2 ]
  %R218 = phi i64 [ %R564, %subblock_4010b0_1 ], [ %R201, %subblock_400f36_2 ]
  %R217 = phi i64 [ %R563, %subblock_4010b0_1 ], [ %R200, %subblock_400f36_2 ]
  %R216 = phi i64 [ %R562, %subblock_4010b0_1 ], [ %R199, %subblock_400f36_2 ]
  %R215 = phi i64 [ %R561, %subblock_4010b0_1 ], [ %R198, %subblock_400f36_2 ]
  %R214 = phi i64 [ %R560, %subblock_4010b0_1 ], [ %R197, %subblock_400f36_2 ]
  %R213 = phi i64 [ %R559, %subblock_4010b0_1 ], [ %R196, %subblock_400f36_2 ]
  %R212 = phi i64 [ %R558, %subblock_4010b0_1 ], [ %R195, %subblock_400f36_2 ]
  ; # 400f44: mov    rax,QWORD PTR [rbx]
  ; r226 := *r214
  %r251 = inttoptr i64 %R214 to i64*
  %R226 = load i64* %r251
  ; # 400f47: and    rax,QWORD PTR [r13+0x8]
  ; r227 := (bv_add r222 0x8 :: [64])
  %R227 = add i64 %R222, 8
  ; r228 := *r227
  %r254 = inttoptr i64 %R227 to i64*
  %R228 = load i64* %r254
  ; r229 := (trunc r226 8)
  %R229 = trunc i64 %R226 to i8
  ; r230 := (trunc r228 8)
  %R230 = trunc i64 %R228 to i8
  ; r231 := (bv_and r229 r230)
  %R231 = and i8 %R229, %R230
  ; # 400f4b: test   al,0x1
  ; r232 := (bv_and r231 0x1 :: [8])
  %R232 = and i8 %R231, 1
  ; r233 := (bv_eq r232 0x0 :: [8])
  %R233 = icmp eq i8 %R232, 0
  ; # 400f4d: jne    4010fd
  ; r234 := (bv_complement r233)
  %R234 = xor i1 %R233, -1
  br i1 %R234, label %subblock_400f44_1, label %subblock_400f44_2
subblock_400f44_1:
  br label %block_4010fd
subblock_400f44_2:
  br label %block_400f53
block_400f53:
  %R247 = phi i128 [ %R225, %subblock_400f44_2 ], [ %R625, %subblock_4010ee_1 ]
  %R246 = phi i64 [ %R224, %subblock_400f44_2 ], [ %R624, %subblock_4010ee_1 ]
  %R245 = phi i64 [ %R222, %subblock_400f44_2 ], [ %R622, %subblock_4010ee_1 ]
  %R244 = phi i64 [ %R221, %subblock_400f44_2 ], [ %R621, %subblock_4010ee_1 ]
  %R243 = phi i64 [ %R220, %subblock_400f44_2 ], [ %R620, %subblock_4010ee_1 ]
  %R242 = phi i64 [ %R219, %subblock_400f44_2 ], [ %R619, %subblock_4010ee_1 ]
  %R241 = phi i64 [ %R218, %subblock_400f44_2 ], [ %R618, %subblock_4010ee_1 ]
  %R240 = phi i64 [ %R217, %subblock_400f44_2 ], [ %R617, %subblock_4010ee_1 ]
  %R239 = phi i64 [ %R216, %subblock_400f44_2 ], [ %R616, %subblock_4010ee_1 ]
  %R238 = phi i64 [ %R215, %subblock_400f44_2 ], [ %R615, %subblock_4010ee_1 ]
  %R237 = phi i64 [ %R214, %subblock_400f44_2 ], [ %R614, %subblock_4010ee_1 ]
  %R236 = phi i64 [ %R213, %subblock_400f44_2 ], [ %R613, %subblock_4010ee_1 ]
  %R235 = phi i64 [ %R212, %subblock_400f44_2 ], [ %R612, %subblock_4010ee_1 ]
  ; # 400f53: mov    eax,DWORD PTR [rip+0x20378f]
  ; r248 := *0x6046e8 :: [64]
  %r275 = inttoptr i64 6309608 to i32*
  %R248 = load i32* %r275
  ; # 400f59: test   eax,eax
  ; r249 := (bv_eq r248 0x0 :: [32])
  %R249 = icmp eq i32 %R248, 0
  ; # 400f5b: je     400fa0
  br i1 %R249, label %subblock_400f53_1, label %subblock_400f53_2
subblock_400f53_1:
  br label %block_400fa0
subblock_400f53_2:
  br label %block_400f5d
block_400f5d:
  %R262 = phi i128 [ %R247, %subblock_400f53_2 ]
  %R261 = phi i64 [ %R246, %subblock_400f53_2 ]
  %R260 = phi i64 [ %R245, %subblock_400f53_2 ]
  %R259 = phi i64 [ %R244, %subblock_400f53_2 ]
  %R258 = phi i64 [ %R243, %subblock_400f53_2 ]
  %R257 = phi i64 [ %R242, %subblock_400f53_2 ]
  %R256 = phi i64 [ %R241, %subblock_400f53_2 ]
  %R255 = phi i64 [ %R240, %subblock_400f53_2 ]
  %R254 = phi i64 [ %R239, %subblock_400f53_2 ]
  %R253 = phi i64 [ %R238, %subblock_400f53_2 ]
  %R252 = phi i64 [ %R237, %subblock_400f53_2 ]
  %R251 = phi i64 [ %R236, %subblock_400f53_2 ]
  %R250 = phi i64 [ %R235, %subblock_400f53_2 ]
  ; # 400f5d: xor    eax,eax
  ; # 400f5f: mov    DWORD PTR [rip+0x203783],eax
  ; *(0x6046e8 :: [64]) = 0x0 :: [32]
  %r291 = inttoptr i64 6309608 to i32*
  store i32 0, i32* %r291
  ; # 400f65: lock or DWORD PTR [rsp],0x0
  ; r263 := *r253
  %r292 = inttoptr i64 %R253 to i32*
  %R263 = load i32* %r292
  ; *(r253) = r263
  %r294 = inttoptr i64 %R253 to i32*
  store i32 %R263, i32* %r294
  ; # 400f6a: mov    eax,DWORD PTR [rip+0x20377c]
  ; r264 := *0x6046ec :: [64]
  %r295 = inttoptr i64 6309612 to i32*
  %R264 = load i32* %r295
  ; # 400f70: test   eax,eax
  ; r265 := (bv_eq r264 0x0 :: [32])
  %R265 = icmp eq i32 %R264, 0
  ; # 400f72: je     400fa0
  br i1 %R265, label %subblock_400f5d_1, label %subblock_400f5d_2
subblock_400f5d_1:
  br label %block_400fa0
subblock_400f5d_2:
  br label %block_400f74
block_400f74:
  %R273 = phi i64 [ %R261, %subblock_400f5d_2 ]
  %R272 = phi i64 [ %R260, %subblock_400f5d_2 ]
  %R271 = phi i64 [ %R259, %subblock_400f5d_2 ]
  %R270 = phi i64 [ %R258, %subblock_400f5d_2 ]
  %R269 = phi i64 [ %R257, %subblock_400f5d_2 ]
  %R268 = phi i64 [ %R254, %subblock_400f5d_2 ]
  %R267 = phi i64 [ %R253, %subblock_400f5d_2 ]
  %R266 = phi i64 [ %R252, %subblock_400f5d_2 ]
  ; # 400f74: mov    r8d,0xca
  ; # 400f7a: mov    edi,0x6046e8
  ; # 400f7f: mov    edx,0x1
  ; # 400f84: mov    esi,0x81
  ; # 400f89: mov    rax,r8
  ; # 400f8c: syscall
  ; sys_futex
  %r306 = call { i64, i1 } @reopt.SystemCall.Linux(i64 6309608, i64 129, i64 1, i64 %R270, i64 202, i64 %R269, i64 202)
  %R274 = extractvalue { i64, i1 } %r306, 0
  br label %block_400f8e
block_400f8e:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R289 = phi i128 [ undef, %block_400f74 ]
  %R288 = phi i64 [ %R273, %block_400f74 ]
  %R287 = phi i64 [ %R272, %block_400f74 ]
  %R286 = phi i64 [ %R271, %block_400f74 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R285 = phi i64 [ undef, %block_400f74 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R284 = phi i64 [ undef, %block_400f74 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R283 = phi i64 [ undef, %block_400f74 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R282 = phi i64 [ undef, %block_400f74 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R281 = phi i64 [ undef, %block_400f74 ]
  %R280 = phi i64 [ %R268, %block_400f74 ]
  %R279 = phi i64 [ %R267, %block_400f74 ]
  %R278 = phi i64 [ %R266, %block_400f74 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R277 = phi i64 [ undef, %block_400f74 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rcx
  %R276 = phi i64 [ undef, %block_400f74 ]
  %R275 = phi i64 [ %R274, %block_400f74 ]
  ; # 400f8e: cmp    rax,0xffffffffffffffda
  ; r290 := (bv_eq r275 0xffffffffffffffda :: [64])
  %R290 = icmp eq i64 %R275, 18446744073709551578
  ; # 400f92: jne    400fa0
  ; r291 := (bv_complement r290)
  %R291 = xor i1 %R290, -1
  br i1 %R291, label %subblock_400f8e_1, label %subblock_400f8e_2
subblock_400f8e_1:
  br label %block_400fa0
subblock_400f8e_2:
  br label %block_400f94
block_400f94:
  %R302 = phi i64 [ %R288, %subblock_400f8e_2 ]
  %R301 = phi i64 [ %R287, %subblock_400f8e_2 ]
  %R300 = phi i64 [ %R286, %subblock_400f8e_2 ]
  %R299 = phi i64 [ %R285, %subblock_400f8e_2 ]
  %R298 = phi i64 [ %R284, %subblock_400f8e_2 ]
  %R297 = phi i64 [ %R283, %subblock_400f8e_2 ]
  %R296 = phi i64 [ %R282, %subblock_400f8e_2 ]
  %R295 = phi i64 [ %R280, %subblock_400f8e_2 ]
  %R294 = phi i64 [ %R279, %subblock_400f8e_2 ]
  %R293 = phi i64 [ %R278, %subblock_400f8e_2 ]
  %R292 = phi i64 [ %R277, %subblock_400f8e_2 ]
  ; # 400f94: mov    rax,r8
  ; # 400f97: mov    rsi,rdx
  ; # 400f9a: syscall
  ; sys_futex
  %r336 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R296, i64 %R292, i64 %R292, i64 %R299, i64 %R297, i64 %R298, i64 202)
  %R303 = extractvalue { i64, i1 } %r336, 0
  br label %block_400f9c
block_400f9c:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R316 = phi i128 [ undef, %block_400f94 ]
  %R315 = phi i64 [ %R302, %block_400f94 ]
  %R314 = phi i64 [ %R301, %block_400f94 ]
  %R313 = phi i64 [ %R300, %block_400f94 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R312 = phi i64 [ undef, %block_400f94 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R311 = phi i64 [ undef, %block_400f94 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R310 = phi i64 [ undef, %block_400f94 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R309 = phi i64 [ undef, %block_400f94 ]
  %R308 = phi i64 [ %R295, %block_400f94 ]
  %R307 = phi i64 [ %R294, %block_400f94 ]
  %R306 = phi i64 [ %R293, %block_400f94 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R305 = phi i64 [ undef, %block_400f94 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rcx
  %R304 = phi i64 [ undef, %block_400f94 ]
  ; # 400f9c: nop    [rax]
  br label %block_400fa0
block_400fa0:
  %R329 = phi i128 [ %R316, %block_400f9c ], [ %R289, %subblock_400f8e_1 ], [ %R262, %subblock_400f5d_1 ], [ %R247, %subblock_400f53_1 ]
  %R328 = phi i64 [ %R315, %block_400f9c ], [ %R288, %subblock_400f8e_1 ], [ %R261, %subblock_400f5d_1 ], [ %R246, %subblock_400f53_1 ]
  %R327 = phi i64 [ %R314, %block_400f9c ], [ %R287, %subblock_400f8e_1 ], [ %R260, %subblock_400f5d_1 ], [ %R245, %subblock_400f53_1 ]
  %R326 = phi i64 [ %R313, %block_400f9c ], [ %R286, %subblock_400f8e_1 ], [ %R259, %subblock_400f5d_1 ], [ %R244, %subblock_400f53_1 ]
  %R325 = phi i64 [ %R312, %block_400f9c ], [ %R285, %subblock_400f8e_1 ], [ %R258, %subblock_400f5d_1 ], [ %R243, %subblock_400f53_1 ]
  %R324 = phi i64 [ %R311, %block_400f9c ], [ %R284, %subblock_400f8e_1 ], [ %R257, %subblock_400f5d_1 ], [ %R242, %subblock_400f53_1 ]
  %R323 = phi i64 [ %R310, %block_400f9c ], [ %R283, %subblock_400f8e_1 ], [ %R256, %subblock_400f5d_1 ], [ %R241, %subblock_400f53_1 ]
  %R322 = phi i64 [ %R309, %block_400f9c ], [ %R281, %subblock_400f8e_1 ], [ %R255, %subblock_400f5d_1 ], [ %R240, %subblock_400f53_1 ]
  %R321 = phi i64 [ %R308, %block_400f9c ], [ %R280, %subblock_400f8e_1 ], [ %R254, %subblock_400f5d_1 ], [ %R239, %subblock_400f53_1 ]
  %R320 = phi i64 [ %R307, %block_400f9c ], [ %R279, %subblock_400f8e_1 ], [ %R253, %subblock_400f5d_1 ], [ %R238, %subblock_400f53_1 ]
  %R319 = phi i64 [ %R306, %block_400f9c ], [ %R278, %subblock_400f8e_1 ], [ %R252, %subblock_400f5d_1 ], [ %R237, %subblock_400f53_1 ]
  %R318 = phi i64 [ %R305, %block_400f9c ], [ %R277, %subblock_400f8e_1 ], [ %R251, %subblock_400f5d_1 ], [ %R236, %subblock_400f53_1 ]
  %R317 = phi i64 [ %R304, %block_400f9c ], [ %R276, %subblock_400f8e_1 ], [ %R250, %subblock_400f5d_1 ], [ %R235, %subblock_400f53_1 ]
  ; # 400fa0: mov    eax,DWORD PTR [r15+0x8]
  ; r330 := (bv_add r328 0x8 :: [64])
  %R330 = add i64 %R328, 8
  ; r331 := *r330
  %r365 = inttoptr i64 %R330 to i32*
  %R331 = load i32* %r365
  ; # 400fa4: test   eax,eax
  ; r332 := (bv_eq r331 0x0 :: [32])
  %R332 = icmp eq i32 %R331, 0
  ; # 400fa6: je     400fe0
  br i1 %R332, label %subblock_400fa0_1, label %subblock_400fa0_2
subblock_400fa0_1:
  br label %block_400fe0
subblock_400fa0_2:
  br label %block_400fa8
block_400fa8:
  %R345 = phi i128 [ %R329, %subblock_400fa0_2 ]
  %R344 = phi i64 [ %R328, %subblock_400fa0_2 ]
  %R343 = phi i64 [ %R327, %subblock_400fa0_2 ]
  %R342 = phi i64 [ %R326, %subblock_400fa0_2 ]
  %R341 = phi i64 [ %R325, %subblock_400fa0_2 ]
  %R340 = phi i64 [ %R324, %subblock_400fa0_2 ]
  %R339 = phi i64 [ %R323, %subblock_400fa0_2 ]
  %R338 = phi i64 [ %R322, %subblock_400fa0_2 ]
  %R337 = phi i64 [ %R321, %subblock_400fa0_2 ]
  %R336 = phi i64 [ %R320, %subblock_400fa0_2 ]
  %R335 = phi i64 [ %R319, %subblock_400fa0_2 ]
  %R334 = phi i64 [ %R318, %subblock_400fa0_2 ]
  %R333 = phi i64 [ %R317, %subblock_400fa0_2 ]
  ; # 400fa8: xor    eax,eax
  ; # 400faa: mov    DWORD PTR [r15+0x8],eax
  ; r346 := (bv_add r344 0x8 :: [64])
  %R346 = add i64 %R344, 8
  ; *(r346) = 0x0 :: [32]
  %r382 = inttoptr i64 %R346 to i32*
  store i32 0, i32* %r382
  ; # 400fae: lock or DWORD PTR [rsp],0x0
  ; r347 := *r336
  %r383 = inttoptr i64 %R336 to i32*
  %R347 = load i32* %r383
  ; *(r336) = r347
  %r385 = inttoptr i64 %R336 to i32*
  store i32 %R347, i32* %r385
  ; # 400fb3: mov    eax,DWORD PTR [rbp+0x4]
  ; r348 := (bv_add r337 0x4 :: [64])
  %R348 = add i64 %R337, 4
  ; r349 := *r348
  %r387 = inttoptr i64 %R348 to i32*
  %R349 = load i32* %r387
  ; # 400fb6: test   eax,eax
  ; r350 := (bv_eq r349 0x0 :: [32])
  %R350 = icmp eq i32 %R349, 0
  ; # 400fb8: je     400fe0
  br i1 %R350, label %subblock_400fa8_1, label %subblock_400fa8_2
subblock_400fa8_1:
  br label %block_400fe0
subblock_400fa8_2:
  br label %block_400fba
block_400fba:
  %R357 = phi i64 [ %R343, %subblock_400fa8_2 ]
  %R356 = phi i64 [ %R342, %subblock_400fa8_2 ]
  %R355 = phi i64 [ %R341, %subblock_400fa8_2 ]
  %R354 = phi i64 [ %R340, %subblock_400fa8_2 ]
  %R353 = phi i64 [ %R337, %subblock_400fa8_2 ]
  %R352 = phi i64 [ %R336, %subblock_400fa8_2 ]
  %R351 = phi i64 [ %R335, %subblock_400fa8_2 ]
  ; # 400fba: mov    r8d,0xca
  ; # 400fc0: mov    edx,0x1
  ; # 400fc5: mov    esi,0x81
  ; # 400fca: mov    rax,r8
  ; # 400fcd: mov    rdi,rbp
  ; # 400fd0: syscall
  ; sys_futex
  %r397 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R353, i64 129, i64 1, i64 %R355, i64 202, i64 %R354, i64 202)
  %R358 = extractvalue { i64, i1 } %r397, 0
  br label %block_400fd2
block_400fd2:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R371 = phi i128 [ undef, %block_400fba ]
  %R370 = phi i64 [ %R357, %block_400fba ]
  %R369 = phi i64 [ %R356, %block_400fba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R368 = phi i64 [ undef, %block_400fba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R367 = phi i64 [ undef, %block_400fba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R366 = phi i64 [ undef, %block_400fba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R365 = phi i64 [ undef, %block_400fba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R364 = phi i64 [ undef, %block_400fba ]
  %R363 = phi i64 [ %R352, %block_400fba ]
  %R362 = phi i64 [ %R351, %block_400fba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R361 = phi i64 [ undef, %block_400fba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rcx
  %R360 = phi i64 [ undef, %block_400fba ]
  %R359 = phi i64 [ %R358, %block_400fba ]
  ; # 400fd2: cmp    rax,0xffffffffffffffda
  ; r372 := (bv_eq r359 0xffffffffffffffda :: [64])
  %R372 = icmp eq i64 %R359, 18446744073709551578
  ; # 400fd6: jne    400fe0
  ; r373 := (bv_complement r372)
  %R373 = xor i1 %R372, -1
  br i1 %R373, label %subblock_400fd2_1, label %subblock_400fd2_2
subblock_400fd2_1:
  br label %block_400fe0
subblock_400fd2_2:
  br label %block_400fd8
block_400fd8:
  %R382 = phi i64 [ %R370, %subblock_400fd2_2 ]
  %R381 = phi i64 [ %R369, %subblock_400fd2_2 ]
  %R380 = phi i64 [ %R368, %subblock_400fd2_2 ]
  %R379 = phi i64 [ %R367, %subblock_400fd2_2 ]
  %R378 = phi i64 [ %R366, %subblock_400fd2_2 ]
  %R377 = phi i64 [ %R365, %subblock_400fd2_2 ]
  %R376 = phi i64 [ %R363, %subblock_400fd2_2 ]
  %R375 = phi i64 [ %R362, %subblock_400fd2_2 ]
  %R374 = phi i64 [ %R361, %subblock_400fd2_2 ]
  ; # 400fd8: mov    rax,r8
  ; # 400fdb: mov    rsi,rdx
  ; # 400fde: syscall
  ; sys_futex
  %r423 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R377, i64 %R374, i64 %R374, i64 %R380, i64 %R378, i64 %R379, i64 202)
  %R383 = extractvalue { i64, i1 } %r423, 0
  br label %block_400fe0
block_400fe0:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R392 = phi i128 [ undef, %block_400fd8 ], [ %R371, %subblock_400fd2_1 ], [ %R345, %subblock_400fa8_1 ], [ %R329, %subblock_400fa0_1 ], [ %R539, %block_40107a ], [ %R79, %subblock_400ea0_1 ]
  %R391 = phi i64 [ %R382, %block_400fd8 ], [ %R370, %subblock_400fd2_1 ], [ %R343, %subblock_400fa8_1 ], [ %R327, %subblock_400fa0_1 ], [ %R538, %block_40107a ], [ %R78, %subblock_400ea0_1 ]
  %R390 = phi i64 [ %R381, %block_400fd8 ], [ %R369, %subblock_400fd2_1 ], [ %R342, %subblock_400fa8_1 ], [ %R326, %subblock_400fa0_1 ], [ %R537, %block_40107a ], [ %R77, %subblock_400ea0_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R389 = phi i64 [ undef, %block_400fd8 ], [ %R366, %subblock_400fd2_1 ], [ %R339, %subblock_400fa8_1 ], [ %R323, %subblock_400fa0_1 ], [ %R536, %block_40107a ], [ %R74, %subblock_400ea0_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R388 = phi i64 [ undef, %block_400fd8 ], [ %R364, %subblock_400fd2_1 ], [ %R338, %subblock_400fa8_1 ], [ %R322, %subblock_400fa0_1 ], [ %R535, %block_40107a ], [ %R73, %subblock_400ea0_1 ]
  %R387 = phi i64 [ %R376, %block_400fd8 ], [ %R363, %subblock_400fd2_1 ], [ %R336, %subblock_400fa8_1 ], [ %R320, %subblock_400fa0_1 ], [ %R534, %block_40107a ], [ %R72, %subblock_400ea0_1 ]
  %R386 = phi i64 [ %R375, %block_400fd8 ], [ %R362, %subblock_400fd2_1 ], [ %R335, %subblock_400fa8_1 ], [ %R319, %subblock_400fa0_1 ], [ %R533, %block_40107a ], [ %R71, %subblock_400ea0_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R385 = phi i64 [ undef, %block_400fd8 ], [ %R361, %subblock_400fd2_1 ], [ %R334, %subblock_400fa8_1 ], [ %R318, %subblock_400fa0_1 ], [ %R532, %block_40107a ], [ %R70, %subblock_400ea0_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rcx
  %R384 = phi i64 [ undef, %block_400fd8 ], [ %R360, %subblock_400fd2_1 ], [ %R333, %subblock_400fa8_1 ], [ %R317, %subblock_400fa0_1 ], [ %R531, %block_40107a ], [ %R69, %subblock_400ea0_1 ]
  ; # 400fe0: mov    rdi,rbx
  ; # 400fe3: call   400bf0
  ; r393 := (bv_add r387 0xfffffffffffffff8 :: [64])
  %R393 = add i64 %R387, 18446744073709551608
  ; r396 := (bv_add r393 0x8 :: [64])
  %R396 = add i64 %R393, 8
  %r436 = bitcast i128 %R392 to <2 x double>
  %r437 = call { i64, i64, <2 x double> } @F400bf0(i64 %R386, i64 %R388, i64 %R385, i64 %R384, i64 %R389, <2 x double> %r436)
  %R394 = extractvalue { i64, i64, <2 x double> } %r437, 0
  %r439 = extractvalue { i64, i64, <2 x double> } %r437, 2
  %R395 = bitcast <2 x double> %r439 to i128
  br label %block_400fe8
block_400fe8:
  %R406 = phi i128 [ %R395, %block_400fe0 ]
  %R405 = phi i64 [ %R391, %block_400fe0 ]
  %R404 = phi i64 [ %R390, %block_400fe0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R403 = phi i64 [ undef, %block_400fe0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R402 = phi i64 [ undef, %block_400fe0 ]
  %R401 = phi i64 [ %R396, %block_400fe0 ]
  %R400 = phi i64 [ %R386, %block_400fe0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R399 = phi i64 [ undef, %block_400fe0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R398 = phi i64 [ undef, %block_400fe0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R397 = phi i64 [ undef, %block_400fe0 ]
  ; # 400fe8: test   eax,eax
  ; r407 := (trunc r397 32)
  %R407 = trunc i64 %R397 to i32
  ; r408 := (bv_eq r407 0x0 :: [32])
  %R408 = icmp eq i32 %R407, 0
  ; # 400fea: je     401027
  br i1 %R408, label %subblock_400fe8_1, label %subblock_400fe8_2
subblock_400fe8_1:
  br label %block_401027
subblock_400fe8_2:
  br label %block_400fec
block_400fec:
  %R415 = phi i128 [ %R406, %subblock_400fe8_2 ]
  %R414 = phi i64 [ %R405, %subblock_400fe8_2 ]
  %R413 = phi i64 [ %R404, %subblock_400fe8_2 ]
  %R412 = phi i64 [ %R403, %subblock_400fe8_2 ]
  %R411 = phi i64 [ %R402, %subblock_400fe8_2 ]
  %R410 = phi i64 [ %R401, %subblock_400fe8_2 ]
  %R409 = phi i64 [ %R400, %subblock_400fe8_2 ]
  ; # 400fec: mov    rax,QWORD PTR [rbx]
  ; r416 := *r409
  %r460 = inttoptr i64 %R409 to i64*
  %R416 = load i64* %r460
  ; # 400fef: mov    rcx,QWORD PTR [rsp+0x18]
  ; r417 := (bv_add r410 0x18 :: [64])
  %R417 = add i64 %R410, 24
  ; r418 := *r417
  %r463 = inttoptr i64 %R417 to i64*
  %R418 = load i64* %r463
  ; # 400ff4: and    rax,0xfffffffffffffffe
  ; r419 := (bv_and r416 0xfffffffffffffffe :: [64])
  %R419 = and i64 %R416, 18446744073709551614
  ; # 400ff8: sub    rbx,rax
  ; r420 := (bv_sub r409 r419)
  %R420 = sub i64 %R409, %R419
  ; # 400ffb: mov    rax,QWORD PTR [rbx+0x8]
  ; r421 := (bv_add r420 0x8 :: [64])
  %R421 = add i64 %R420, 8
  ; r422 := *r421
  %r468 = inttoptr i64 %R421 to i64*
  %R422 = load i64* %r468
  ; # 400fff: and    rax,0xfffffffffffffffe
  ; r423 := (bv_and r422 0xfffffffffffffffe :: [64])
  %R423 = and i64 %R422, 18446744073709551614
  ; # 401003: add    QWORD PTR [rsp+0x8],rax
  ; r424 := (bv_add r410 0x8 :: [64])
  %R424 = add i64 %R410, 8
  ; r425 := *r424
  %r472 = inttoptr i64 %R424 to i64*
  %R425 = load i64* %r472
  ; r426 := (bv_add r425 r423)
  %R426 = add i64 %R425, %R423
  ; *(r424) = r426
  %r475 = inttoptr i64 %R424 to i64*
  store i64 %R426, i64* %r475
  ; # 401008: lea    rdx,[rcx+rax*1]
  ; r427 := (bv_add r418 r423)
  %R427 = add i64 %R418, %R423
  ; # 40100c: cmp    rdx,0x28000
  ; # 401013: jbe    401027
  ; r428 := (bv_ule r427 0x28000 :: [64])
  %R428 = icmp ule i64 %R427, 163840
  br i1 %R428, label %subblock_400fec_1, label %subblock_400fec_2
subblock_400fec_1:
  br label %block_401027
subblock_400fec_2:
  br label %block_401015
block_401015:
  %R438 = phi i128 [ %R415, %subblock_400fec_2 ]
  %R437 = phi i64 [ %R414, %subblock_400fec_2 ]
  %R436 = phi i64 [ %R413, %subblock_400fec_2 ]
  %R435 = phi i64 [ %R412, %subblock_400fec_2 ]
  %R434 = phi i64 [ %R411, %subblock_400fec_2 ]
  %R433 = phi i64 [ %R410, %subblock_400fec_2 ]
  %R432 = phi i64 [ %R420, %subblock_400fec_2 ]
  %R431 = phi i64 [ %R427, %subblock_400fec_2 ]
  %R430 = phi i64 [ %R418, %subblock_400fec_2 ]
  %R429 = phi i64 [ %R423, %subblock_400fec_2 ]
  ; # 401015: xor    rdx,rax
  ; r439 := (bv_xor r431 r429)
  %R439 = xor i64 %R431, %R429
  ; # 401018: cmp    rax,rdx
  ; r440 := (bv_ult r429 r439)
  %R440 = icmp ult i64 %R429, %R439
  ; # 40101b: mov    eax,DWORD PTR [rsp+0x14]
  ; r441 := (bv_add r433 0x14 :: [64])
  %R441 = add i64 %R433, 20
  ; r442 := *r441
  %r491 = inttoptr i64 %R441 to i32*
  %R442 = load i32* %r491
  ; # 40101f: cmovb  eax,r12d
  ; r443 := (trunc r436 32)
  %R443 = trunc i64 %R436 to i32
  ; r444 := (mux r440 r443 r442)
  %R444 = select i1 %R440, i32 %R443, i32 %R442
  ; # 401023: mov    DWORD PTR [rsp+0x14],eax
  ; *(r441) = r444
  %r495 = inttoptr i64 %R441 to i32*
  store i32 %R444, i32* %r495
  br label %block_401027
block_401027:
  %R453 = phi i128 [ %R438, %block_401015 ], [ %R415, %subblock_400fec_1 ], [ %R406, %subblock_400fe8_1 ]
  %R452 = phi i64 [ %R437, %block_401015 ], [ %R414, %subblock_400fec_1 ], [ %R405, %subblock_400fe8_1 ]
  %R451 = phi i64 [ %R436, %block_401015 ], [ %R413, %subblock_400fec_1 ], [ %R404, %subblock_400fe8_1 ]
  %R450 = phi i64 [ %R435, %block_401015 ], [ %R412, %subblock_400fec_1 ], [ %R403, %subblock_400fe8_1 ]
  %R449 = phi i64 [ %R434, %block_401015 ], [ %R411, %subblock_400fec_1 ], [ %R402, %subblock_400fe8_1 ]
  %R448 = phi i64 [ %R433, %block_401015 ], [ %R410, %subblock_400fec_1 ], [ %R401, %subblock_400fe8_1 ]
  %R447 = phi i64 [ %R432, %block_401015 ], [ %R420, %subblock_400fec_1 ], [ %R400, %subblock_400fe8_1 ]
  %R446 = phi i64 [ %R439, %block_401015 ], [ %R427, %subblock_400fec_1 ], [ %R399, %subblock_400fe8_1 ]
  %R445 = phi i64 [ %R430, %block_401015 ], [ %R418, %subblock_400fec_1 ], [ %R398, %subblock_400fe8_1 ]
  ; # 401027: mov    rdi,r13
  ; # 40102a: call   4009a0
  ; r454 := (bv_add r448 0xfffffffffffffff8 :: [64])
  %R454 = add i64 %R448, 18446744073709551608
  ; r457 := (bv_add r454 0x8 :: [64])
  %R457 = add i64 %R454, 8
  %r507 = bitcast i128 %R453 to <2 x double>
  %r508 = call { i64, i64, <2 x double> } @F4009a0(i64 %R452, i64 %R449, i64 %R446, i64 %R445, i64 %R450, <2 x double> %r507)
  %R455 = extractvalue { i64, i64, <2 x double> } %r508, 0
  %r510 = extractvalue { i64, i64, <2 x double> } %r508, 2
  %R456 = bitcast <2 x double> %r510 to i128
  br label %block_40102f
block_40102f:
  %R469 = phi i128 [ %R456, %block_401027 ]
  %R468 = phi i64 [ %R452, %block_401027 ]
  %R467 = phi i64 [ %R451, %block_401027 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R466 = phi i64 [ undef, %block_401027 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R465 = phi i64 [ undef, %block_401027 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R464 = phi i64 [ undef, %block_401027 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R463 = phi i64 [ undef, %block_401027 ]
  %R462 = phi i64 [ %R457, %block_401027 ]
  %R461 = phi i64 [ %R447, %block_401027 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R460 = phi i64 [ undef, %block_401027 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R459 = phi i64 [ undef, %block_401027 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R458 = phi i64 [ undef, %block_401027 ]
  ; # 40102f: test   eax,eax
  ; r470 := (trunc r458 32)
  %R470 = trunc i64 %R458 to i32
  ; r471 := (bv_eq r470 0x0 :: [32])
  %R471 = icmp eq i32 %R470, 0
  ; # 401031: je     400ea0
  br i1 %R471, label %subblock_40102f_1, label %subblock_40102f_2
subblock_40102f_1:
  br label %block_400ea0
subblock_40102f_2:
  br label %block_401037
block_401037:
  %R480 = phi i128 [ %R469, %subblock_40102f_2 ]
  %R479 = phi i64 [ %R468, %subblock_40102f_2 ]
  %R478 = phi i64 [ %R467, %subblock_40102f_2 ]
  %R477 = phi i64 [ %R466, %subblock_40102f_2 ]
  %R476 = phi i64 [ %R465, %subblock_40102f_2 ]
  %R475 = phi i64 [ %R464, %subblock_40102f_2 ]
  %R474 = phi i64 [ %R462, %subblock_40102f_2 ]
  %R473 = phi i64 [ %R461, %subblock_40102f_2 ]
  %R472 = phi i64 [ %R459, %subblock_40102f_2 ]
  ; # 401037: mov    rax,QWORD PTR [r13+0x8]
  ; r481 := (bv_add r479 0x8 :: [64])
  %R481 = add i64 %R479, 8
  ; r482 := *r481
  %r536 = inttoptr i64 %R481 to i64*
  %R482 = load i64* %r536
  ; # 40103b: mov    rsi,QWORD PTR [rsp+0x18]
  ; r483 := (bv_add r474 0x18 :: [64])
  %R483 = add i64 %R474, 24
  ; r484 := *r483
  %r539 = inttoptr i64 %R483 to i64*
  %R484 = load i64* %r539
  ; # 401040: and    rax,0xfffffffffffffffe
  ; r485 := (bv_and r482 0xfffffffffffffffe :: [64])
  %R485 = and i64 %R482, 18446744073709551614
  ; # 401044: add    QWORD PTR [rsp+0x8],rax
  ; r486 := (bv_add r474 0x8 :: [64])
  %R486 = add i64 %R474, 8
  ; r487 := *r486
  %r543 = inttoptr i64 %R486 to i64*
  %R487 = load i64* %r543
  ; r488 := (bv_add r487 r485)
  %R488 = add i64 %R487, %R485
  ; *(r486) = r488
  %r546 = inttoptr i64 %R486 to i64*
  store i64 %R488, i64* %r546
  ; # 401049: lea    rdx,[rsi+rax*1]
  ; r489 := (bv_add r484 r485)
  %R489 = add i64 %R484, %R485
  ; # 40104d: cmp    rdx,0x28000
  ; # 401054: jbe    401068
  ; r490 := (bv_ule r489 0x28000 :: [64])
  %R490 = icmp ule i64 %R489, 163840
  br i1 %R490, label %subblock_401037_1, label %subblock_401037_2
subblock_401037_1:
  br label %block_401068
subblock_401037_2:
  br label %block_401056
block_401056:
  %R502 = phi i128 [ %R480, %subblock_401037_2 ]
  %R501 = phi i64 [ %R479, %subblock_401037_2 ]
  %R500 = phi i64 [ %R478, %subblock_401037_2 ]
  %R499 = phi i64 [ %R477, %subblock_401037_2 ]
  %R498 = phi i64 [ %R476, %subblock_401037_2 ]
  %R497 = phi i64 [ %R475, %subblock_401037_2 ]
  %R496 = phi i64 [ %R484, %subblock_401037_2 ]
  %R495 = phi i64 [ %R474, %subblock_401037_2 ]
  %R494 = phi i64 [ %R473, %subblock_401037_2 ]
  %R493 = phi i64 [ %R489, %subblock_401037_2 ]
  %R492 = phi i64 [ %R472, %subblock_401037_2 ]
  %R491 = phi i64 [ %R485, %subblock_401037_2 ]
  ; # 401056: mov    edi,DWORD PTR [rsp+0x14]
  ; r503 := (bv_add r495 0x14 :: [64])
  %R503 = add i64 %R495, 20
  ; r504 := *r503
  %r562 = inttoptr i64 %R503 to i32*
  %R504 = load i32* %r562
  ; # 40105a: xor    rdx,rax
  ; r505 := (bv_xor r493 r491)
  %R505 = xor i64 %R493, %R491
  ; # 40105d: cmp    rax,rdx
  ; r506 := (bv_ult r491 r505)
  %R506 = icmp ult i64 %R491, %R505
  ; # 401060: cmovb  edi,r12d
  ; r507 := (trunc r500 32)
  %R507 = trunc i64 %R500 to i32
  ; r508 := (mux r506 r507 r504)
  %R508 = select i1 %R506, i32 %R507, i32 %R504
  ; # 401064: mov    DWORD PTR [rsp+0x14],edi
  ; *(r503) = r508
  %r568 = inttoptr i64 %R503 to i32*
  store i32 %R508, i32* %r568
  br label %block_401068
block_401068:
  %R520 = phi i128 [ %R502, %block_401056 ], [ %R480, %subblock_401037_1 ]
  %R519 = phi i64 [ %R501, %block_401056 ], [ %R479, %subblock_401037_1 ]
  %R518 = phi i64 [ %R500, %block_401056 ], [ %R478, %subblock_401037_1 ]
  %R517 = phi i64 [ %R499, %block_401056 ], [ %R477, %subblock_401037_1 ]
  %R516 = phi i64 [ %R498, %block_401056 ], [ %R476, %subblock_401037_1 ]
  %R515 = phi i64 [ %R497, %block_401056 ], [ %R475, %subblock_401037_1 ]
  %R514 = phi i64 [ %R496, %block_401056 ], [ %R484, %subblock_401037_1 ]
  %R513 = phi i64 [ %R495, %block_401056 ], [ %R474, %subblock_401037_1 ]
  %R512 = phi i64 [ %R494, %block_401056 ], [ %R473, %subblock_401037_1 ]
  %R511 = phi i64 [ %R505, %block_401056 ], [ %R489, %subblock_401037_1 ]
  %R510 = phi i64 [ %R492, %block_401056 ], [ %R472, %subblock_401037_1 ]
  %R509 = phi i64 [ %R491, %block_401056 ], [ %R485, %subblock_401037_1 ]
  ; # 401068: add    r13,rax
  ; r521 := (bv_add r519 r509)
  %R521 = add i64 %R519, %R509
  ; # 40106b: mov    rax,QWORD PTR [rbx]
  ; r522 := *r512
  %r582 = inttoptr i64 %R512 to i64*
  %R522 = load i64* %r582
  ; # 40106e: and    rax,QWORD PTR [r13+0x8]
  ; r523 := (bv_add r521 0x8 :: [64])
  %R523 = add i64 %R521, 8
  ; r524 := *r523
  %r585 = inttoptr i64 %R523 to i64*
  %R524 = load i64* %r585
  ; r525 := (trunc r522 8)
  %R525 = trunc i64 %R522 to i8
  ; r526 := (trunc r524 8)
  %R526 = trunc i64 %R524 to i8
  ; r527 := (bv_and r525 r526)
  %R527 = and i8 %R525, %R526
  ; # 401072: test   al,0x1
  ; r528 := (bv_and r527 0x1 :: [8])
  %R528 = and i8 %R527, 1
  ; r529 := (bv_eq r528 0x0 :: [8])
  %R529 = icmp eq i8 %R528, 0
  ; # 401074: jne    400eaf
  ; r530 := (bv_complement r529)
  %R530 = xor i1 %R529, -1
  br i1 %R530, label %subblock_401068_1, label %subblock_401068_2
subblock_401068_1:
  br label %block_400eaf
subblock_401068_2:
  br label %block_40107a
block_40107a:
  %R539 = phi i128 [ %R520, %subblock_401068_2 ]
  %R538 = phi i64 [ %R521, %subblock_401068_2 ]
  %R537 = phi i64 [ %R518, %subblock_401068_2 ]
  %R536 = phi i64 [ %R515, %subblock_401068_2 ]
  %R535 = phi i64 [ %R514, %subblock_401068_2 ]
  %R534 = phi i64 [ %R513, %subblock_401068_2 ]
  %R533 = phi i64 [ %R512, %subblock_401068_2 ]
  %R532 = phi i64 [ %R511, %subblock_401068_2 ]
  %R531 = phi i64 [ %R510, %subblock_401068_2 ]
  ; # 40107a: jmp    400fe0
  br label %block_400fe0
block_401080:
  %R542 = phi i64 [ %R8, %subblock_400e49_1 ]
  %R541 = phi i64 [ %R26, %subblock_400e49_1 ]
  %R540 = phi i64 [ %R23, %subblock_400e49_1 ]
  ; # 401080: mov    rax,QWORD PTR [rdi-0x10]
  ; r543 := (bv_add r542 0xfffffffffffffff0 :: [64])
  %R543 = add i64 %R542, 18446744073709551600
  ; r544 := *r543
  %r606 = inttoptr i64 %R543 to i64*
  %R544 = load i64* %r606
  ; # 401084: and    rsi,0xfffffffffffffffe
  ; r545 := (bv_and r541 0xfffffffffffffffe :: [64])
  %R545 = and i64 %R541, 18446744073709551614
  ; # 401088: sub    rbx,rax
  ; r546 := (bv_sub r540 r544)
  %R546 = sub i64 %R540, %R544
  ; # 40108b: add    rsi,rax
  ; r547 := (bv_add r545 r544)
  %R547 = add i64 %R545, %R544
  ; # 40108e: test   al,0x1
  ; r548 := (trunc r544 8)
  %R548 = trunc i64 %R544 to i8
  ; r549 := (bv_and r548 0x1 :: [8])
  %R549 = and i8 %R548, 1
  ; r550 := (bv_eq r549 0x0 :: [8])
  %R550 = icmp eq i8 %R549, 0
  ; # 401090: mov    rdi,rbx
  ; # 401093: je     401096
  br i1 %R550, label %subblock_401080_1, label %subblock_401080_2
subblock_401080_1:
  br label %block_401096
subblock_401080_2:
  br label %block_401095
block_401095:
  %R552 = phi i64 [ %R546, %subblock_401080_2 ]
  %R551 = phi i64 [ %R547, %subblock_401080_2 ]
  ; # 401095: hlt
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception GeneralProtectionException 0 ()
  br label %block_401096
block_401096:
  %R554 = phi i64 [ %R552, %block_401095 ], [ %R546, %subblock_401080_1 ]
  %R553 = phi i64 [ %R551, %block_401095 ], [ %R547, %subblock_401080_1 ]
  ; # 401096: add    rsp,0x28
  ; # 40109a: pop    rbx
  ; # 40109b: pop    rbp
  ; # 40109c: pop    r12
  ; # 40109e: pop    r13
  ; # 4010a0: pop    r14
  ; # 4010a2: pop    r15
  ; # 4010a4: jmp    401c4a
  %r618 = call { i64, i64, <2 x double> } @F401c4a(i64 %R554, i64 %R553)
  ret { i64, i64, <2 x double> } %r618
block_4010b0:
  %R571 = phi i128 [ %R208, %subblock_400f36_1 ]
  %R570 = phi i64 [ %R207, %subblock_400f36_1 ]
  %R569 = phi i64 [ %R206, %subblock_400f36_1 ]
  %R568 = phi i64 [ %R205, %subblock_400f36_1 ]
  %R567 = phi i64 [ %R204, %subblock_400f36_1 ]
  %R566 = phi i64 [ %R203, %subblock_400f36_1 ]
  %R565 = phi i64 [ %R202, %subblock_400f36_1 ]
  %R564 = phi i64 [ %R201, %subblock_400f36_1 ]
  %R563 = phi i64 [ %R200, %subblock_400f36_1 ]
  %R562 = phi i64 [ %R199, %subblock_400f36_1 ]
  %R561 = phi i64 [ %R198, %subblock_400f36_1 ]
  %R560 = phi i64 [ %R197, %subblock_400f36_1 ]
  %R559 = phi i64 [ %R196, %subblock_400f36_1 ]
  %R558 = phi i64 [ %R195, %subblock_400f36_1 ]
  ; # 4010b0: mov    eax,r12d
  ; r572 := (trunc r567 32)
  %R572 = trunc i64 %R567 to i32
  ; # 4010b3: xchg   DWORD PTR [rip+0x20362f],eax
  ; r573 := *0x6046e8 :: [64]
  %r634 = inttoptr i64 6309608 to i32*
  %R573 = load i32* %r634
  ; *(0x6046e8 :: [64]) = r572
  %r636 = inttoptr i64 6309608 to i32*
  store i32 %R572, i32* %r636
  ; # 4010b9: test   eax,eax
  ; r574 := (bv_eq r573 0x0 :: [32])
  %R574 = icmp eq i32 %R573, 0
  ; # 4010bb: je     400f44
  br i1 %R574, label %subblock_4010b0_1, label %subblock_4010b0_2
subblock_4010b0_1:
  br label %block_400f44
subblock_4010b0_2:
  br label %block_4010c1
block_4010c1:
  %R582 = phi i128 [ %R571, %subblock_4010b0_2 ]
  %R581 = phi i64 [ %R570, %subblock_4010b0_2 ]
  %R580 = phi i64 [ %R569, %subblock_4010b0_2 ]
  %R579 = phi i64 [ %R568, %subblock_4010b0_2 ]
  %R578 = phi i64 [ %R567, %subblock_4010b0_2 ]
  %R577 = phi i64 [ %R562, %subblock_4010b0_2 ]
  %R576 = phi i64 [ %R561, %subblock_4010b0_2 ]
  %R575 = phi i64 [ %R560, %subblock_4010b0_2 ]
  ; # 4010c1: nop    [rax]
  br label %block_4010c8
block_4010c8:
  %R590 = phi i128 [ %R607, %subblock_4010e1_1 ], [ %R582, %block_4010c1 ]
  %R589 = phi i64 [ %R606, %subblock_4010e1_1 ], [ %R581, %block_4010c1 ]
  %R588 = phi i64 [ %R605, %subblock_4010e1_1 ], [ %R580, %block_4010c1 ]
  %R587 = phi i64 [ %R604, %subblock_4010e1_1 ], [ %R579, %block_4010c1 ]
  %R586 = phi i64 [ %R603, %subblock_4010e1_1 ], [ %R578, %block_4010c1 ]
  %R585 = phi i64 [ %R598, %subblock_4010e1_1 ], [ %R577, %block_4010c1 ]
  %R584 = phi i64 [ %R597, %subblock_4010e1_1 ], [ %R576, %block_4010c1 ]
  %R583 = phi i64 [ %R596, %subblock_4010e1_1 ], [ %R575, %block_4010c1 ]
  ; # 4010c8: mov    ecx,0x1
  ; # 4010cd: mov    edx,0x1
  ; # 4010d2: mov    esi,0x6046ec
  ; # 4010d7: mov    edi,0x6046e8
  ; # 4010dc: call   40241c
  ; r591 := (bv_add r584 0xfffffffffffffff8 :: [64])
  %R591 = add i64 %R584, 18446744073709551608
  ; r593 := (bv_add r591 0x8 :: [64])
  %R593 = add i64 %R591, 8
  %r656 = bitcast i128 %R590 to <2 x double>
  %r657 = call { i64, i64, <2 x double> } @F40241c(i64 6309608, i64 6309612, i64 1, i64 1, <2 x double> %r656)
  %r658 = extractvalue { i64, i64, <2 x double> } %r657, 2
  %R592 = bitcast <2 x double> %r658 to i128
  br label %block_4010e1
block_4010e1:
  %R607 = phi i128 [ %R592, %block_4010c8 ]
  %R606 = phi i64 [ %R589, %block_4010c8 ]
  %R605 = phi i64 [ %R588, %block_4010c8 ]
  %R604 = phi i64 [ %R587, %block_4010c8 ]
  %R603 = phi i64 [ %R586, %block_4010c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R602 = phi i64 [ undef, %block_4010c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R601 = phi i64 [ undef, %block_4010c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R600 = phi i64 [ undef, %block_4010c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R599 = phi i64 [ undef, %block_4010c8 ]
  %R598 = phi i64 [ %R585, %block_4010c8 ]
  %R597 = phi i64 [ %R593, %block_4010c8 ]
  %R596 = phi i64 [ %R583, %block_4010c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R595 = phi i64 [ undef, %block_4010c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R594 = phi i64 [ undef, %block_4010c8 ]
  ; # 4010e1: mov    eax,r12d
  ; r608 := (trunc r603 32)
  %R608 = trunc i64 %R603 to i32
  ; # 4010e4: xchg   DWORD PTR [rip+0x2035fe],eax
  ; r609 := *0x6046e8 :: [64]
  %r675 = inttoptr i64 6309608 to i32*
  %R609 = load i32* %r675
  ; *(0x6046e8 :: [64]) = r608
  %r677 = inttoptr i64 6309608 to i32*
  store i32 %R608, i32* %r677
  ; # 4010ea: test   eax,eax
  ; r610 := (bv_eq r609 0x0 :: [32])
  %R610 = icmp eq i32 %R609, 0
  ; # 4010ec: jne    4010c8
  ; r611 := (bv_complement r610)
  %R611 = xor i1 %R610, -1
  br i1 %R611, label %subblock_4010e1_1, label %subblock_4010e1_2
subblock_4010e1_1:
  br label %block_4010c8
subblock_4010e1_2:
  br label %block_4010ee
block_4010ee:
  %R625 = phi i128 [ %R607, %subblock_4010e1_2 ]
  %R624 = phi i64 [ %R606, %subblock_4010e1_2 ]
  %R623 = phi i64 [ %R605, %subblock_4010e1_2 ]
  %R622 = phi i64 [ %R604, %subblock_4010e1_2 ]
  %R621 = phi i64 [ %R603, %subblock_4010e1_2 ]
  %R620 = phi i64 [ %R602, %subblock_4010e1_2 ]
  %R619 = phi i64 [ %R601, %subblock_4010e1_2 ]
  %R618 = phi i64 [ %R600, %subblock_4010e1_2 ]
  %R617 = phi i64 [ %R599, %subblock_4010e1_2 ]
  %R616 = phi i64 [ %R598, %subblock_4010e1_2 ]
  %R615 = phi i64 [ %R597, %subblock_4010e1_2 ]
  %R614 = phi i64 [ %R596, %subblock_4010e1_2 ]
  %R613 = phi i64 [ %R595, %subblock_4010e1_2 ]
  %R612 = phi i64 [ %R594, %subblock_4010e1_2 ]
  ; # 4010ee: mov    rax,QWORD PTR [rbx]
  ; r626 := *r614
  %r694 = inttoptr i64 %R614 to i64*
  %R626 = load i64* %r694
  ; # 4010f1: and    rax,QWORD PTR [r13+0x8]
  ; r627 := (bv_add r622 0x8 :: [64])
  %R627 = add i64 %R622, 8
  ; r628 := *r627
  %r697 = inttoptr i64 %R627 to i64*
  %R628 = load i64* %r697
  ; r629 := (trunc r626 8)
  %R629 = trunc i64 %R626 to i8
  ; r630 := (trunc r628 8)
  %R630 = trunc i64 %R628 to i8
  ; r631 := (bv_and r629 r630)
  %R631 = and i8 %R629, %R630
  ; # 4010f5: test   al,0x1
  ; r632 := (bv_and r631 0x1 :: [8])
  %R632 = and i8 %R631, 1
  ; r633 := (bv_eq r632 0x0 :: [8])
  %R633 = icmp eq i8 %R632, 0
  ; # 4010f7: je     400f53
  br i1 %R633, label %subblock_4010ee_1, label %subblock_4010ee_2
subblock_4010ee_1:
  br label %block_400f53
subblock_4010ee_2:
  br label %block_4010fd
block_4010fd:
  %R641 = phi i128 [ %R225, %subblock_400f44_1 ], [ %R625, %subblock_4010ee_2 ]
  %R640 = phi i64 [ %R223, %subblock_400f44_1 ], [ %R623, %subblock_4010ee_2 ]
  %R639 = phi i64 [ %R222, %subblock_400f44_1 ], [ %R622, %subblock_4010ee_2 ]
  %R638 = phi i64 [ %R220, %subblock_400f44_1 ], [ %R620, %subblock_4010ee_2 ]
  %R637 = phi i64 [ %R219, %subblock_400f44_1 ], [ %R619, %subblock_4010ee_2 ]
  %R636 = phi i64 [ %R216, %subblock_400f44_1 ], [ %R616, %subblock_4010ee_2 ]
  %R635 = phi i64 [ %R215, %subblock_400f44_1 ], [ %R615, %subblock_4010ee_2 ]
  %R634 = phi i64 [ %R214, %subblock_400f44_1 ], [ %R614, %subblock_4010ee_2 ]
  ; # 4010fd: mov    ecx,DWORD PTR [rsp+0x10]
  ; r642 := (bv_add r635 0x10 :: [64])
  %R642 = add i64 %R635, 16
  ; r643 := *r642
  %r713 = inttoptr i64 %R642 to i32*
  %R643 = load i32* %r713
  ; r644 := (uext r643 64)
  %R644 = zext i32 %R643 to i64
  ; # 401101: mov    rax,QWORD PTR [rip+0x202fd8]
  ; r645 := *0x6040e0 :: [64]
  %r716 = inttoptr i64 6308064 to i64*
  %R645 = load i64* %r716
  ; # 401108: mov    rsi,rcx
  ; # 40110b: bt     rax,rsi
  ; r646 := (bv_and r644 0x3f :: [64])
  %R646 = and i64 %R644, 63
  ; r647 := (bv_testbit r645 r646)
  %r719 = shl i64 1, %R646
  %r720 = and i64 %R645, %r719
  %R647 = icmp ne i64 %r720, 0
  ; # 40110f: jb     401121
  br i1 %R647, label %subblock_4010fd_1, label %subblock_4010fd_2
subblock_4010fd_1:
  br label %block_401121
subblock_4010fd_2:
  br label %block_401111
block_401111:
  %R656 = phi i128 [ %R641, %subblock_4010fd_2 ]
  %R655 = phi i64 [ %R640, %subblock_4010fd_2 ]
  %R654 = phi i64 [ %R639, %subblock_4010fd_2 ]
  %R653 = phi i64 [ %R638, %subblock_4010fd_2 ]
  %R652 = phi i64 [ %R637, %subblock_4010fd_2 ]
  %R651 = phi i64 [ %R636, %subblock_4010fd_2 ]
  %R650 = phi i64 [ %R635, %subblock_4010fd_2 ]
  %R649 = phi i64 [ %R634, %subblock_4010fd_2 ]
  %R648 = phi i64 [ %R644, %subblock_4010fd_2 ]
  ; # 401111: mov    eax,0x1
  ; # 401116: shl    rax,cl
  ; r657 := (trunc r648 8)
  %R657 = trunc i64 %R648 to i8
  ; r658 := (bv_and r657 0x3f :: [8])
  %R658 = and i8 %R657, 63
  ; r659 := (bv_eq r658 0x0 :: [8])
  %R659 = icmp eq i8 %R658, 0
  br i1 %R659, label %subblock_401111_1, label %subblock_401111_2
subblock_401111_1:
  br label %block_401119
subblock_401111_2:
  ; r660 := (trunc r648 8)
  %R660 = trunc i64 %R648 to i8
  ; r661 := (bv_and r660 0x3f :: [8])
  %R661 = and i8 %R660, 63
  ; r662 := (uext r661 64)
  %R662 = zext i8 %R661 to i64
  ; r663 := (bv_shl 0x1 :: [64] r662)
  %R663 = shl i64 1, %R662
  br label %block_401119
block_401119:
  %R672 = phi i128 [ %R656, %subblock_401111_2 ], [ %R656, %subblock_401111_1 ]
  %R671 = phi i64 [ %R655, %subblock_401111_2 ], [ %R655, %subblock_401111_1 ]
  %R670 = phi i64 [ %R654, %subblock_401111_2 ], [ %R654, %subblock_401111_1 ]
  %R669 = phi i64 [ %R653, %subblock_401111_2 ], [ %R653, %subblock_401111_1 ]
  %R668 = phi i64 [ %R652, %subblock_401111_2 ], [ %R652, %subblock_401111_1 ]
  %R667 = phi i64 [ %R651, %subblock_401111_2 ], [ %R651, %subblock_401111_1 ]
  %R666 = phi i64 [ %R650, %subblock_401111_2 ], [ %R650, %subblock_401111_1 ]
  %R665 = phi i64 [ %R649, %subblock_401111_2 ], [ %R649, %subblock_401111_1 ]
  %R664 = phi i64 [ %R663, %subblock_401111_2 ], [ 1, %subblock_401111_1 ]
  ; # 401119: lock or QWORD PTR [rip+0x202fbf],rax
  ; r673 := *0x6040e0 :: [64]
  %r747 = inttoptr i64 6308064 to i64*
  %R673 = load i64* %r747
  ; r674 := (bv_or r673 r664)
  %R674 = or i64 %R673, %R664
  ; *(0x6040e0 :: [64]) = r674
  %r750 = inttoptr i64 6308064 to i64*
  store i64 %R674, i64* %r750
  br label %block_401121
block_401121:
  %R682 = phi i128 [ %R672, %block_401119 ], [ %R641, %subblock_4010fd_1 ]
  %R681 = phi i64 [ %R671, %block_401119 ], [ %R640, %subblock_4010fd_1 ]
  %R680 = phi i64 [ %R670, %block_401119 ], [ %R639, %subblock_4010fd_1 ]
  %R679 = phi i64 [ %R669, %block_401119 ], [ %R638, %subblock_4010fd_1 ]
  %R678 = phi i64 [ %R668, %block_401119 ], [ %R637, %subblock_4010fd_1 ]
  %R677 = phi i64 [ %R667, %block_401119 ], [ %R636, %subblock_4010fd_1 ]
  %R676 = phi i64 [ %R666, %block_401119 ], [ %R635, %subblock_4010fd_1 ]
  %R675 = phi i64 [ %R665, %block_401119 ], [ %R634, %subblock_4010fd_1 ]
  ; # 401121: mov    rax,QWORD PTR [rsp+0x8]
  ; r683 := (bv_add r676 0x8 :: [64])
  %R683 = add i64 %R676, 8
  ; r684 := *r683
  %r760 = inttoptr i64 %R683 to i64*
  %R684 = load i64* %r760
  ; # 401126: mov    QWORD PTR [rbx+0x8],rax
  ; r685 := (bv_add r675 0x8 :: [64])
  %R685 = add i64 %R675, 8
  ; *(r685) = r684
  %r763 = inttoptr i64 %R685 to i64*
  store i64 %R684, i64* %r763
  ; # 40112a: mov    QWORD PTR [r13],rax
  ; *(r680) = r684
  %r764 = inttoptr i64 %R680 to i64*
  store i64 %R684, i64* %r764
  ; # 40112e: mov    eax,DWORD PTR [rip+0x2035b4]
  ; r686 := *0x6046e8 :: [64]
  %r765 = inttoptr i64 6309608 to i32*
  %R686 = load i32* %r765
  ; # 401134: test   eax,eax
  ; r687 := (bv_eq r686 0x0 :: [32])
  %R687 = icmp eq i32 %R686, 0
  ; # 401136: je     401180
  br i1 %R687, label %subblock_401121_1, label %subblock_401121_2
subblock_401121_1:
  br label %block_401180
subblock_401121_2:
  br label %block_401138
block_401138:
  %R695 = phi i128 [ %R682, %subblock_401121_2 ]
  %R694 = phi i64 [ %R681, %subblock_401121_2 ]
  %R693 = phi i64 [ %R680, %subblock_401121_2 ]
  %R692 = phi i64 [ %R679, %subblock_401121_2 ]
  %R691 = phi i64 [ %R678, %subblock_401121_2 ]
  %R690 = phi i64 [ %R677, %subblock_401121_2 ]
  %R689 = phi i64 [ %R676, %subblock_401121_2 ]
  %R688 = phi i64 [ %R675, %subblock_401121_2 ]
  ; # 401138: xor    eax,eax
  ; # 40113a: mov    DWORD PTR [rip+0x2035a8],eax
  ; *(0x6046e8 :: [64]) = 0x0 :: [32]
  %r776 = inttoptr i64 6309608 to i32*
  store i32 0, i32* %r776
  ; # 401140: lock or DWORD PTR [rsp],0x0
  ; r696 := *r689
  %r777 = inttoptr i64 %R689 to i32*
  %R696 = load i32* %r777
  ; *(r689) = r696
  %r779 = inttoptr i64 %R689 to i32*
  store i32 %R696, i32* %r779
  ; # 401145: mov    eax,DWORD PTR [rip+0x2035a1]
  ; r697 := *0x6046ec :: [64]
  %r780 = inttoptr i64 6309612 to i32*
  %R697 = load i32* %r780
  ; # 40114b: test   eax,eax
  ; r698 := (bv_eq r697 0x0 :: [32])
  %R698 = icmp eq i32 %R697, 0
  ; # 40114d: je     401180
  br i1 %R698, label %subblock_401138_1, label %subblock_401138_2
subblock_401138_1:
  br label %block_401180
subblock_401138_2:
  br label %block_40114f
block_40114f:
  %R705 = phi i64 [ %R694, %subblock_401138_2 ]
  %R704 = phi i64 [ %R693, %subblock_401138_2 ]
  %R703 = phi i64 [ %R692, %subblock_401138_2 ]
  %R702 = phi i64 [ %R691, %subblock_401138_2 ]
  %R701 = phi i64 [ %R690, %subblock_401138_2 ]
  %R700 = phi i64 [ %R689, %subblock_401138_2 ]
  %R699 = phi i64 [ %R688, %subblock_401138_2 ]
  ; # 40114f: mov    r8d,0xca
  ; # 401155: mov    edi,0x6046e8
  ; # 40115a: mov    edx,0x1
  ; # 40115f: mov    esi,0x81
  ; # 401164: mov    rax,r8
  ; # 401167: syscall
  ; sys_futex
  %r790 = call { i64, i1 } @reopt.SystemCall.Linux(i64 6309608, i64 129, i64 1, i64 %R703, i64 202, i64 %R702, i64 202)
  %R706 = extractvalue { i64, i1 } %r790, 0
  br label %block_401169
block_401169:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R718 = phi i128 [ undef, %block_40114f ]
  %R717 = phi i64 [ %R705, %block_40114f ]
  %R716 = phi i64 [ %R704, %block_40114f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R715 = phi i64 [ undef, %block_40114f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R714 = phi i64 [ undef, %block_40114f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R713 = phi i64 [ undef, %block_40114f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R712 = phi i64 [ undef, %block_40114f ]
  %R711 = phi i64 [ %R701, %block_40114f ]
  %R710 = phi i64 [ %R700, %block_40114f ]
  %R709 = phi i64 [ %R699, %block_40114f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R708 = phi i64 [ undef, %block_40114f ]
  %R707 = phi i64 [ %R706, %block_40114f ]
  ; # 401169: cmp    rax,0xffffffffffffffda
  ; r719 := (bv_eq r707 0xffffffffffffffda :: [64])
  %R719 = icmp eq i64 %R707, 18446744073709551578
  ; # 40116d: jne    401180
  ; r720 := (bv_complement r719)
  %R720 = xor i1 %R719, -1
  br i1 %R720, label %subblock_401169_1, label %subblock_401169_2
subblock_401169_1:
  br label %block_401180
subblock_401169_2:
  br label %block_40116f
block_40116f:
  %R730 = phi i64 [ %R717, %subblock_401169_2 ]
  %R729 = phi i64 [ %R716, %subblock_401169_2 ]
  %R728 = phi i64 [ %R715, %subblock_401169_2 ]
  %R727 = phi i64 [ %R714, %subblock_401169_2 ]
  %R726 = phi i64 [ %R713, %subblock_401169_2 ]
  %R725 = phi i64 [ %R712, %subblock_401169_2 ]
  %R724 = phi i64 [ %R711, %subblock_401169_2 ]
  %R723 = phi i64 [ %R710, %subblock_401169_2 ]
  %R722 = phi i64 [ %R709, %subblock_401169_2 ]
  %R721 = phi i64 [ %R708, %subblock_401169_2 ]
  ; # 40116f: mov    rax,r8
  ; # 401172: mov    rsi,rdx
  ; # 401175: syscall
  ; sys_futex
  %r816 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R725, i64 %R721, i64 %R721, i64 %R728, i64 %R726, i64 %R727, i64 202)
  %R731 = extractvalue { i64, i1 } %r816, 0
  br label %block_401177
block_401177:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R739 = phi i128 [ undef, %block_40116f ]
  %R738 = phi i64 [ %R730, %block_40116f ]
  %R737 = phi i64 [ %R729, %block_40116f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R736 = phi i64 [ undef, %block_40116f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R735 = phi i64 [ undef, %block_40116f ]
  %R734 = phi i64 [ %R724, %block_40116f ]
  %R733 = phi i64 [ %R723, %block_40116f ]
  %R732 = phi i64 [ %R722, %block_40116f ]
  ; # 401177: nop    [rax+rax*1]
  br label %block_401180
block_401180:
  %R747 = phi i128 [ %R739, %block_401177 ], [ %R718, %subblock_401169_1 ], [ %R695, %subblock_401138_1 ], [ %R682, %subblock_401121_1 ]
  %R746 = phi i64 [ %R738, %block_401177 ], [ %R717, %subblock_401169_1 ], [ %R694, %subblock_401138_1 ], [ %R681, %subblock_401121_1 ]
  %R745 = phi i64 [ %R737, %block_401177 ], [ %R716, %subblock_401169_1 ], [ %R693, %subblock_401138_1 ], [ %R680, %subblock_401121_1 ]
  %R744 = phi i64 [ %R736, %block_401177 ], [ %R715, %subblock_401169_1 ], [ %R692, %subblock_401138_1 ], [ %R679, %subblock_401121_1 ]
  %R743 = phi i64 [ %R735, %block_401177 ], [ %R714, %subblock_401169_1 ], [ %R691, %subblock_401138_1 ], [ %R678, %subblock_401121_1 ]
  %R742 = phi i64 [ %R734, %block_401177 ], [ %R711, %subblock_401169_1 ], [ %R690, %subblock_401138_1 ], [ %R677, %subblock_401121_1 ]
  %R741 = phi i64 [ %R733, %block_401177 ], [ %R710, %subblock_401169_1 ], [ %R689, %subblock_401138_1 ], [ %R676, %subblock_401121_1 ]
  %R740 = phi i64 [ %R732, %block_401177 ], [ %R709, %subblock_401169_1 ], [ %R688, %subblock_401138_1 ], [ %R675, %subblock_401121_1 ]
  ; # 401180: lea    rax,[r14+r14*2]
  ; r748 := (bv_mul 0x2 :: [64] r746)
  %R748 = mul i64 2, %R746
  ; r749 := (bv_add r746 r748)
  %R749 = add i64 %R746, %R748
  ; # 401184: lea    rax,[rax*8+0x6040e0]
  ; r750 := (bv_mul 0x8 :: [64] r749)
  %R750 = mul i64 8, %R749
  ; r751 := (bv_add r750 0x6040e0 :: [64])
  %R751 = add i64 %R750, 6308064
  ; # 40118c: mov    rdx,QWORD PTR [rax+0x18]
  ; r752 := (bv_add r750 0x6040f8 :: [64])
  %R752 = add i64 %R750, 6308088
  ; r753 := *r752
  %r839 = inttoptr i64 %R752 to i64*
  %R753 = load i64* %r839
  ; # 401190: mov    QWORD PTR [rbx+0x10],rax
  ; r754 := (bv_add r740 0x10 :: [64])
  %R754 = add i64 %R740, 16
  ; *(r754) = r751
  %r842 = inttoptr i64 %R754 to i64*
  store i64 %R751, i64* %r842
  ; # 401194: mov    QWORD PTR [rbx+0x18],rdx
  ; r755 := (bv_add r740 0x18 :: [64])
  %R755 = add i64 %R740, 24
  ; *(r755) = r753
  %r844 = inttoptr i64 %R755 to i64*
  store i64 %R753, i64* %r844
  ; # 401198: mov    QWORD PTR [rax+0x18],rbx
  ; *(r752) = r740
  %r845 = inttoptr i64 %R752 to i64*
  store i64 %R740, i64* %r845
  ; # 40119c: mov    rax,QWORD PTR [rbx+0x18]
  ; r756 := *r755
  %r846 = inttoptr i64 %R755 to i64*
  %R756 = load i64* %r846
  ; # 4011a0: mov    QWORD PTR [rax+0x10],rbx
  ; r757 := (bv_add r756 0x10 :: [64])
  %R757 = add i64 %R756, 16
  ; *(r757) = r740
  %r849 = inttoptr i64 %R757 to i64*
  store i64 %R740, i64* %r849
  ; # 4011a4: mov    eax,DWORD PTR [rsp+0x14]
  ; r758 := (bv_add r741 0x14 :: [64])
  %R758 = add i64 %R741, 20
  ; r759 := *r758
  %r851 = inttoptr i64 %R758 to i32*
  %R759 = load i32* %r851
  ; # 4011a8: test   eax,eax
  ; r760 := (bv_eq r759 0x0 :: [32])
  %R760 = icmp eq i32 %R759, 0
  ; # 4011aa: jne    401255
  ; r761 := (bv_complement r760)
  %R761 = xor i1 %R760, -1
  br i1 %R761, label %subblock_401180_1, label %subblock_401180_2
subblock_401180_1:
  br label %block_401255
subblock_401180_2:
  br label %block_4011b0
block_4011b0:
  %R767 = phi i128 [ %R919, %block_40127b ], [ %R747, %subblock_401180_2 ]
  %R766 = phi i64 [ %R918, %block_40127b ], [ %R744, %subblock_401180_2 ]
  %R765 = phi i64 [ %R917, %block_40127b ], [ %R743, %subblock_401180_2 ]
  %R764 = phi i64 [ %R916, %block_40127b ], [ %R742, %subblock_401180_2 ]
  %R763 = phi i64 [ %R915, %block_40127b ], [ %R741, %subblock_401180_2 ]
  %R762 = phi i64 [ %R914, %block_40127b ], [ %R753, %subblock_401180_2 ]
  ; # 4011b0: mov    eax,DWORD PTR [rbp]
  ; r768 := *r764
  %r861 = inttoptr i64 %R764 to i32*
  %R768 = load i32* %r861
  ; r769 := (uext r768 64)
  %R769 = zext i32 %R768 to i64
  ; # 4011b3: test   eax,eax
  ; r770 := (bv_eq r768 0x0 :: [32])
  %R770 = icmp eq i32 %R768, 0
  ; # 4011b5: je     4011f0
  br i1 %R770, label %subblock_4011b0_1, label %subblock_4011b0_2
subblock_4011b0_1:
  br label %block_4011f0
subblock_4011b0_2:
  br label %block_4011b7
block_4011b7:
  %R776 = phi i128 [ %R767, %subblock_4011b0_2 ]
  %R775 = phi i64 [ %R766, %subblock_4011b0_2 ]
  %R774 = phi i64 [ %R765, %subblock_4011b0_2 ]
  %R773 = phi i64 [ %R764, %subblock_4011b0_2 ]
  %R772 = phi i64 [ %R763, %subblock_4011b0_2 ]
  %R771 = phi i64 [ %R762, %subblock_4011b0_2 ]
  ; # 4011b7: xor    eax,eax
  ; # 4011b9: mov    DWORD PTR [rbp],eax
  ; *(r773) = 0x0 :: [32]
  %r871 = inttoptr i64 %R773 to i32*
  store i32 0, i32* %r871
  ; # 4011bc: lock or DWORD PTR [rsp],0x0
  ; r777 := *r772
  %r872 = inttoptr i64 %R772 to i32*
  %R777 = load i32* %r872
  ; *(r772) = r777
  %r874 = inttoptr i64 %R772 to i32*
  store i32 %R777, i32* %r874
  ; # 4011c1: mov    eax,DWORD PTR [rbp+0x4]
  ; r778 := (bv_add r773 0x4 :: [64])
  %R778 = add i64 %R773, 4
  ; r779 := *r778
  %r876 = inttoptr i64 %R778 to i32*
  %R779 = load i32* %r876
  ; r780 := (uext r779 64)
  %R780 = zext i32 %R779 to i64
  ; # 4011c4: test   eax,eax
  ; r781 := (bv_eq r779 0x0 :: [32])
  %R781 = icmp eq i32 %R779, 0
  ; # 4011c6: je     4011f0
  br i1 %R781, label %subblock_4011b7_1, label %subblock_4011b7_2
subblock_4011b7_1:
  br label %block_4011f0
subblock_4011b7_2:
  br label %block_4011c8
block_4011c8:
  %R784 = phi i64 [ %R775, %subblock_4011b7_2 ]
  %R783 = phi i64 [ %R774, %subblock_4011b7_2 ]
  %R782 = phi i64 [ %R773, %subblock_4011b7_2 ]
  ; # 4011c8: mov    r8d,0xca
  ; # 4011ce: mov    edx,0x1
  ; # 4011d3: mov    esi,0x81
  ; # 4011d8: mov    rax,r8
  ; # 4011db: mov    rdi,rbp
  ; # 4011de: syscall
  ; sys_futex
  %r883 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R782, i64 129, i64 1, i64 %R784, i64 202, i64 %R783, i64 202)
  %R785 = extractvalue { i64, i1 } %r883, 0
  br label %block_4011e0
block_4011e0:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R792 = phi i128 [ undef, %block_4011c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R791 = phi i64 [ undef, %block_4011c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R790 = phi i64 [ undef, %block_4011c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R789 = phi i64 [ undef, %block_4011c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R788 = phi i64 [ undef, %block_4011c8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R787 = phi i64 [ undef, %block_4011c8 ]
  %R786 = phi i64 [ %R785, %block_4011c8 ]
  ; # 4011e0: cmp    rax,0xffffffffffffffda
  ; r793 := (bv_eq r786 0xffffffffffffffda :: [64])
  %R793 = icmp eq i64 %R786, 18446744073709551578
  ; # 4011e4: jne    4011f0
  ; r794 := (bv_complement r793)
  %R794 = xor i1 %R793, -1
  br i1 %R794, label %subblock_4011e0_1, label %subblock_4011e0_2
subblock_4011e0_1:
  br label %block_4011f0
subblock_4011e0_2:
  br label %block_4011e6
block_4011e6:
  %R799 = phi i64 [ %R791, %subblock_4011e0_2 ]
  %R798 = phi i64 [ %R790, %subblock_4011e0_2 ]
  %R797 = phi i64 [ %R789, %subblock_4011e0_2 ]
  %R796 = phi i64 [ %R788, %subblock_4011e0_2 ]
  %R795 = phi i64 [ %R787, %subblock_4011e0_2 ]
  ; # 4011e6: mov    rax,r8
  ; # 4011e9: mov    rsi,rdx
  ; # 4011ec: syscall
  ; sys_futex
  %r899 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R796, i64 %R795, i64 %R795, i64 %R799, i64 %R797, i64 %R798, i64 202)
  %R800 = extractvalue { i64, i1 } %r899, 0
  br label %block_4011ee
block_4011ee:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R803 = phi i128 [ undef, %block_4011e6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R802 = phi i64 [ undef, %block_4011e6 ]
  %R801 = phi i64 [ %R800, %block_4011e6 ]
  ; # 4011ee: nop
  ; r804 := (trunc r801 32)
  %R804 = trunc i64 %R801 to i32
  ; r805 := (uext r804 64)
  %R805 = zext i32 %R804 to i64
  br label %block_4011f0
block_4011f0:
  %R808 = phi i128 [ %R803, %block_4011ee ], [ %R792, %subblock_4011e0_1 ], [ %R776, %subblock_4011b7_1 ], [ %R767, %subblock_4011b0_1 ]
  %R807 = phi i64 [ %R802, %block_4011ee ], [ %R787, %subblock_4011e0_1 ], [ %R771, %subblock_4011b7_1 ], [ %R762, %subblock_4011b0_1 ]
  %R806 = phi i64 [ %R805, %block_4011ee ], [ %R786, %subblock_4011e0_1 ], [ %R780, %subblock_4011b7_1 ], [ %R769, %subblock_4011b0_1 ]
  ; # 4011f0: add    rsp,0x28
  ; # 4011f4: pop    rbx
  ; # 4011f5: pop    rbp
  ; # 4011f6: pop    r12
  ; # 4011f8: pop    r13
  ; # 4011fa: pop    r14
  ; # 4011fc: pop    r15
  br label %block_4011fe
block_4011fe:
  %R811 = phi i128 [ %R808, %block_4011f0 ], [ %r0, %subblock_400e40_1 ]
  %R810 = phi i64 [ %R807, %block_4011f0 ], [ %a2, %subblock_400e40_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %R809 = phi i64 [ %R806, %block_4011f0 ], [ undef, %subblock_400e40_1 ]
  ; # 4011fe: repz ret
  %r912 = bitcast i128 %R811 to <2 x double>
  %r913 = insertvalue { i64, i64, <2 x double> } undef, i64 %R809, 0
  %r914 = insertvalue { i64, i64, <2 x double> } %r913, i64 %R810, 1
  %r915 = insertvalue { i64, i64, <2 x double> } %r914, <2 x double> %r912, 2
  ret { i64, i64, <2 x double> } %r915
block_401200:
  %R825 = phi i128 [ %R162, %subblock_400efc_1 ]
  %R824 = phi i64 [ %R169, %subblock_400efc_1 ]
  %R823 = phi i64 [ %R165, %subblock_400efc_1 ]
  %R822 = phi i64 [ %R161, %subblock_400efc_1 ]
  %R821 = phi i64 [ %R160, %subblock_400efc_1 ]
  %R820 = phi i64 [ %R159, %subblock_400efc_1 ]
  %R819 = phi i64 [ %R158, %subblock_400efc_1 ]
  %R818 = phi i64 [ %R157, %subblock_400efc_1 ]
  %R817 = phi i64 [ %R156, %subblock_400efc_1 ]
  %R816 = phi i64 [ %R171, %subblock_400efc_1 ]
  %R815 = phi i64 [ %R155, %subblock_400efc_1 ]
  %R814 = phi i64 [ %R154, %subblock_400efc_1 ]
  %R813 = phi i64 [ %R153, %subblock_400efc_1 ]
  %R812 = phi i64 [ %R152, %subblock_400efc_1 ]
  ; # 401200: mov    eax,r12d
  ; r826 := (trunc r821 32)
  %R826 = trunc i64 %R821 to i32
  ; # 401203: xchg   DWORD PTR [r15+0x8],eax
  ; r827 := (bv_add r824 0x8 :: [64])
  %R827 = add i64 %R824, 8
  ; r828 := *r827
  %r932 = inttoptr i64 %R827 to i32*
  %R828 = load i32* %r932
  ; *(r827) = r826
  %r934 = inttoptr i64 %R827 to i32*
  store i32 %R826, i32* %r934
  ; # 401207: test   eax,eax
  ; r829 := (bv_eq r828 0x0 :: [32])
  %R829 = icmp eq i32 %R828, 0
  ; # 401209: je     400f1f
  br i1 %R829, label %subblock_401200_1, label %subblock_401200_2
subblock_401200_1:
  br label %block_400f1f
subblock_401200_2:
  br label %block_40120f
block_40120f:
  %R837 = phi i128 [ %R825, %subblock_401200_2 ]
  %R836 = phi i64 [ %R824, %subblock_401200_2 ]
  %R835 = phi i64 [ %R823, %subblock_401200_2 ]
  %R834 = phi i64 [ %R822, %subblock_401200_2 ]
  %R833 = phi i64 [ %R821, %subblock_401200_2 ]
  %R832 = phi i64 [ %R816, %subblock_401200_2 ]
  %R831 = phi i64 [ %R815, %subblock_401200_2 ]
  %R830 = phi i64 [ %R814, %subblock_401200_2 ]
  ; # 40120f: lea    rax,[r15+0xc]
  ; r838 := (bv_add r836 0xc :: [64])
  %R838 = add i64 %R836, 12
  ; # 401213: mov    QWORD PTR [rsp],rax
  ; *(r831) = r838
  %r945 = inttoptr i64 %R831 to i64*
  store i64 %R838, i64* %r945
  ; # 401217: nop    [rax+rax*1]
  br label %block_401220
block_401220:
  %R846 = phi i128 [ %R864, %subblock_401236_1 ], [ %R837, %block_40120f ]
  %R845 = phi i64 [ %R863, %subblock_401236_1 ], [ %R836, %block_40120f ]
  %R844 = phi i64 [ %R862, %subblock_401236_1 ], [ %R835, %block_40120f ]
  %R843 = phi i64 [ %R861, %subblock_401236_1 ], [ %R834, %block_40120f ]
  %R842 = phi i64 [ %R860, %subblock_401236_1 ], [ %R833, %block_40120f ]
  %R841 = phi i64 [ %R855, %subblock_401236_1 ], [ %R832, %block_40120f ]
  %R840 = phi i64 [ %R854, %subblock_401236_1 ], [ %R831, %block_40120f ]
  %R839 = phi i64 [ %R853, %subblock_401236_1 ], [ %R830, %block_40120f ]
  ; # 401220: mov    rsi,QWORD PTR [rsp]
  ; r847 := *r840
  %r954 = inttoptr i64 %R840 to i64*
  %R847 = load i64* %r954
  ; # 401224: mov    ecx,0x1
  ; # 401229: mov    edx,0x1
  ; # 40122e: mov    rdi,rbp
  ; # 401231: call   40241c
  ; r848 := (bv_add r840 0xfffffffffffffff8 :: [64])
  %R848 = add i64 %R840, 18446744073709551608
  ; r850 := (bv_add r848 0x8 :: [64])
  %R850 = add i64 %R848, 8
  %r958 = bitcast i128 %R846 to <2 x double>
  %r959 = call { i64, i64, <2 x double> } @F40241c(i64 %R841, i64 %R847, i64 1, i64 1, <2 x double> %r958)
  %r960 = extractvalue { i64, i64, <2 x double> } %r959, 2
  %R849 = bitcast <2 x double> %r960 to i128
  br label %block_401236
block_401236:
  %R864 = phi i128 [ %R849, %block_401220 ]
  %R863 = phi i64 [ %R845, %block_401220 ]
  %R862 = phi i64 [ %R844, %block_401220 ]
  %R861 = phi i64 [ %R843, %block_401220 ]
  %R860 = phi i64 [ %R842, %block_401220 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R859 = phi i64 [ undef, %block_401220 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R858 = phi i64 [ undef, %block_401220 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R857 = phi i64 [ undef, %block_401220 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R856 = phi i64 [ undef, %block_401220 ]
  %R855 = phi i64 [ %R841, %block_401220 ]
  %R854 = phi i64 [ %R850, %block_401220 ]
  %R853 = phi i64 [ %R839, %block_401220 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R852 = phi i64 [ undef, %block_401220 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R851 = phi i64 [ undef, %block_401220 ]
  ; # 401236: mov    eax,r12d
  ; r865 := (trunc r860 32)
  %R865 = trunc i64 %R860 to i32
  ; # 401239: xchg   DWORD PTR [rbp],eax
  ; r866 := *r855
  %r977 = inttoptr i64 %R855 to i32*
  %R866 = load i32* %r977
  ; *(r855) = r865
  %r979 = inttoptr i64 %R855 to i32*
  store i32 %R865, i32* %r979
  ; # 40123c: test   eax,eax
  ; r867 := (bv_eq r866 0x0 :: [32])
  %R867 = icmp eq i32 %R866, 0
  ; # 40123e: jne    401220
  ; r868 := (bv_complement r867)
  %R868 = xor i1 %R867, -1
  br i1 %R868, label %subblock_401236_1, label %subblock_401236_2
subblock_401236_1:
  br label %block_401220
subblock_401236_2:
  br label %block_401240
block_401240:
  %R882 = phi i128 [ %R864, %subblock_401236_2 ]
  %R881 = phi i64 [ %R863, %subblock_401236_2 ]
  %R880 = phi i64 [ %R862, %subblock_401236_2 ]
  %R879 = phi i64 [ %R861, %subblock_401236_2 ]
  %R878 = phi i64 [ %R860, %subblock_401236_2 ]
  %R877 = phi i64 [ %R859, %subblock_401236_2 ]
  %R876 = phi i64 [ %R858, %subblock_401236_2 ]
  %R875 = phi i64 [ %R857, %subblock_401236_2 ]
  %R874 = phi i64 [ %R856, %subblock_401236_2 ]
  %R873 = phi i64 [ %R855, %subblock_401236_2 ]
  %R872 = phi i64 [ %R854, %subblock_401236_2 ]
  %R871 = phi i64 [ %R853, %subblock_401236_2 ]
  %R870 = phi i64 [ %R852, %subblock_401236_2 ]
  %R869 = phi i64 [ %R851, %subblock_401236_2 ]
  ; # 401240: jmp    400f1f
  br label %block_400f1f
block_401248:
  %R897 = phi i128 [ %R187, %subblock_400f1f_1 ]
  %R896 = phi i64 [ %R186, %subblock_400f1f_1 ]
  %R895 = phi i64 [ %R185, %subblock_400f1f_1 ]
  %R894 = phi i64 [ %R184, %subblock_400f1f_1 ]
  %R893 = phi i64 [ %R183, %subblock_400f1f_1 ]
  %R892 = phi i64 [ %R182, %subblock_400f1f_1 ]
  %R891 = phi i64 [ %R181, %subblock_400f1f_1 ]
  %R890 = phi i64 [ %R180, %subblock_400f1f_1 ]
  %R889 = phi i64 [ %R179, %subblock_400f1f_1 ]
  %R888 = phi i64 [ %R178, %subblock_400f1f_1 ]
  %R887 = phi i64 [ %R177, %subblock_400f1f_1 ]
  %R886 = phi i64 [ %R176, %subblock_400f1f_1 ]
  %R885 = phi i64 [ %R175, %subblock_400f1f_1 ]
  %R884 = phi i64 [ %R174, %subblock_400f1f_1 ]
  %R883 = phi i64 [ %R191, %subblock_400f1f_1 ]
  ; # 401248: mov    QWORD PTR [rax+0x18],rax
  ; r898 := (bv_add r883 0x18 :: [64])
  %R898 = add i64 %R883, 24
  ; *(r898) = r883
  %r1012 = inttoptr i64 %R898 to i64*
  store i64 %R883, i64* %r1012
  ; # 40124c: mov    QWORD PTR [rax+0x10],rax
  ; r899 := (bv_add r883 0x10 :: [64])
  %R899 = add i64 %R883, 16
  ; *(r899) = r883
  %r1014 = inttoptr i64 %R899 to i64*
  store i64 %R883, i64* %r1014
  ; # 401250: jmp    400f36
  br label %block_400f36
block_401255:
  %R903 = phi i64 [ %R745, %subblock_401180_1 ]
  %R902 = phi i64 [ %R742, %subblock_401180_1 ]
  %R901 = phi i64 [ %R741, %subblock_401180_1 ]
  %R900 = phi i64 [ %R740, %subblock_401180_1 ]
  ; # 401255: lea    rdi,[rbx+0x101f]
  ; r904 := (bv_add r900 0x101f :: [64])
  %R904 = add i64 %R900, 4127
  ; # 40125c: lea    rsi,[r13-0x20]
  ; r905 := (bv_add r903 0xffffffffffffffe0 :: [64])
  %R905 = add i64 %R903, 18446744073709551584
  ; # 401260: mov    edx,0x4
  ; # 401265: and    rdi,0xfffffffffffff000
  ; r906 := (bv_and r904 0xfffffffffffff000 :: [64])
  %R906 = and i64 %R904, 18446744073709547520
  ; # 40126c: and    rsi,0xfffffffffffff000
  ; r907 := (bv_and r905 0xfffffffffffff000 :: [64])
  %R907 = and i64 %R905, 18446744073709547520
  ; # 401273: sub    rsi,rdi
  ; r908 := (bv_sub r907 r906)
  %R908 = sub i64 %R907, %R906
  ; # 401276: call   401b0a
  ; r909 := (bv_add r901 0xfffffffffffffff8 :: [64])
  %R909 = add i64 %R901, 18446744073709551608
  ; r913 := (bv_add r909 0x8 :: [64])
  %R913 = add i64 %R909, 8
  %r1026 = call { i64, i64, <2 x double> } @F401b0a(i64 %R906, i64 %R908, i64 4)
  %R910 = extractvalue { i64, i64, <2 x double> } %r1026, 0
  %R911 = extractvalue { i64, i64, <2 x double> } %r1026, 1
  %r1029 = extractvalue { i64, i64, <2 x double> } %r1026, 2
  %R912 = bitcast <2 x double> %r1029 to i128
  br label %block_40127b
block_40127b:
  %R919 = phi i128 [ %R912, %block_401255 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R918 = phi i64 [ undef, %block_401255 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R917 = phi i64 [ undef, %block_401255 ]
  %R916 = phi i64 [ %R902, %block_401255 ]
  %R915 = phi i64 [ %R913, %block_401255 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R914 = phi i64 [ undef, %block_401255 ]
  ; # 40127b: jmp    4011b0
  br label %block_4011b0
failure:
  br label %failure
}