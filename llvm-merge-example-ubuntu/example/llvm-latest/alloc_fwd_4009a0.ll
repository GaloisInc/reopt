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
declare { i64, i64, <2 x double> } @F40241c(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4009a0(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, <2 x double> %a5) {
entry:
  %r0 = bitcast <2 x double> %a5 to i128
  br label %block_4009a0
block_4009a0:
  ; r0 := (alloca 0x50 :: [64])
  %r1 = alloca i8, i64 80
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x50 :: [64])
  %R1 = add i64 %R0, 80
  ; # 4009a0: push   r15
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4009a2: push   r14
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 4009a4: push   r13
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 4009a6: push   r12
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 4009a8: mov    r13d,0x1
  ; # 4009ae: push   rbp
  ; r6 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R6 = add i64 %R1, 18446744073709551576
  ; # 4009af: push   rbx
  ; r7 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R7 = add i64 %R1, 18446744073709551568
  ; # 4009b0: sub    rsp,0x18
  ; r8 := (bv_add r1 0xffffffffffffffb8 :: [64])
  %R8 = add i64 %R1, 18446744073709551544
  ; # 4009b4: mov    QWORD PTR [rsp+0x8],rdi
  ; r9 := (bv_add r1 0xffffffffffffffc0 :: [64])
  %R9 = add i64 %R1, 18446744073709551552
  ; *(r9) = arg0
  %r12 = inttoptr i64 %R9 to i64*
  store i64 %a0, i64* %r12
  ; # 4009b9: nop    [rax]
  br label %block_4009c0
block_4009c0:
  %R14 = phi i128 [ %R179, %block_400aa0 ], [ %R164, %subblock_400a8e_1 ], [ %R145, %subblock_400a60_1 ], [ %r0, %block_4009a0 ]
  %R13 = phi i64 [ %R178, %block_400aa0 ], [ %R163, %subblock_400a8e_1 ], [ %R143, %subblock_400a60_1 ], [ 1, %block_4009a0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r10
  %R12 = phi i64 [ %R177, %block_400aa0 ], [ %R162, %subblock_400a8e_1 ], [ %R142, %subblock_400a60_1 ], [ undef, %block_4009a0 ]
  %R11 = phi i64 [ %R176, %block_400aa0 ], [ %R160, %subblock_400a8e_1 ], [ %R141, %subblock_400a60_1 ], [ %a4, %block_4009a0 ]
  %R10 = phi i64 [ %R175, %block_400aa0 ], [ %R158, %subblock_400a8e_1 ], [ %R140, %subblock_400a60_1 ], [ %R8, %block_4009a0 ]
  ; # 4009c0: mov    rax,QWORD PTR [rsp+0x8]
  ; r15 := (bv_add r10 0x8 :: [64])
  %R15 = add i64 %R10, 8
  ; r16 := *r15
  %r19 = inttoptr i64 %R15 to i64*
  %R16 = load i64* %r19
  ; # 4009c5: mov    rbx,QWORD PTR [rax+0x8]
  ; r17 := (bv_add r16 0x8 :: [64])
  %R17 = add i64 %R16, 8
  ; r18 := *r17
  %r22 = inttoptr i64 %R17 to i64*
  %R18 = load i64* %r22
  ; # 4009c9: test   bl,0x1
  ; r19 := (trunc r18 8)
  %R19 = trunc i64 %R18 to i8
  ; r20 := (bv_and r19 0x1 :: [8])
  %R20 = and i8 %R19, 1
  ; r21 := (bv_eq r20 0x0 :: [8])
  %R21 = icmp eq i8 %R20, 0
  ; # 4009cc: jne    400ab4
  ; r22 := (bv_complement r21)
  %R22 = xor i1 %R21, -1
  br i1 %R22, label %subblock_4009c0_1, label %subblock_4009c0_2
subblock_4009c0_1:
  br label %block_400ab4
subblock_4009c0_2:
  br label %block_4009d2
block_4009d2:
  %R28 = phi i128 [ %R185, %subblock_400aa8_1 ], [ %R14, %subblock_4009c0_2 ]
  %R27 = phi i64 [ %R184, %subblock_400aa8_1 ], [ %R13, %subblock_4009c0_2 ]
  %R26 = phi i64 [ %R183, %subblock_400aa8_1 ], [ %R12, %subblock_4009c0_2 ]
  %R25 = phi i64 [ %R182, %subblock_400aa8_1 ], [ %R11, %subblock_4009c0_2 ]
  %R24 = phi i64 [ %R181, %subblock_400aa8_1 ], [ %R10, %subblock_4009c0_2 ]
  %R23 = phi i64 [ %R180, %subblock_400aa8_1 ], [ %R18, %subblock_4009c0_2 ]
  ; # 4009d2: mov    rax,rbx
  ; # 4009d5: shr    rax,0x5
  ; r29 := (bv_shr r23 0x5 :: [64])
  %R29 = lshr i64 %R23, 5
  ; # 4009d9: sub    rax,0x1
  ; r30 := (bv_add r29 0xffffffffffffffff :: [64])
  %R30 = add i64 %R29, 18446744073709551615
  ; # 4009dd: cmp    rax,0x20
  ; r31 := (bv_ult r30 0x20 :: [64])
  %R31 = icmp ult i64 %R30, 32
  ; r32 := (bv_eq r29 0x21 :: [64])
  %R32 = icmp eq i64 %R29, 33
  ; # 4009e1: jbe    400a07
  ; r33 := (bv_or r31 r32)
  %R33 = or i1 %R31, %R32
  br i1 %R33, label %subblock_4009d2_1, label %subblock_4009d2_2
subblock_4009d2_1:
  br label %block_400a07
subblock_4009d2_2:
  br label %block_4009e3
block_4009e3:
  %R40 = phi i128 [ %R28, %subblock_4009d2_2 ]
  %R39 = phi i64 [ %R27, %subblock_4009d2_2 ]
  %R38 = phi i64 [ %R26, %subblock_4009d2_2 ]
  %R37 = phi i64 [ %R25, %subblock_4009d2_2 ]
  %R36 = phi i64 [ %R24, %subblock_4009d2_2 ]
  %R35 = phi i64 [ %R23, %subblock_4009d2_2 ]
  %R34 = phi i64 [ %R30, %subblock_4009d2_2 ]
  ; # 4009e3: cmp    rax,0x1c00
  ; # 4009e9: mov    DWORD PTR [rsp+0x4],0x3f
  ; r41 := (bv_add r36 0x4 :: [64])
  %R41 = add i64 %R36, 4
  ; *(r41) = 0x3f :: [32]
  %r47 = inttoptr i64 %R41 to i32*
  store i32 63, i32* %r47
  ; # 4009f1: ja     400a0b
  ; r42 := (bv_ult 0x1c00 :: [64] r34)
  %R42 = icmp ult i64 7168, %R34
  br i1 %R42, label %subblock_4009e3_1, label %subblock_4009e3_2
subblock_4009e3_1:
  br label %block_400a0b
subblock_4009e3_2:
  br label %block_4009f3
block_4009f3:
  %R48 = phi i64 [ %R39, %subblock_4009e3_2 ]
  %R47 = phi i64 [ %R38, %subblock_4009e3_2 ]
  %R46 = phi i64 [ %R37, %subblock_4009e3_2 ]
  %R45 = phi i64 [ %R36, %subblock_4009e3_2 ]
  %R44 = phi i64 [ %R35, %subblock_4009e3_2 ]
  %R43 = phi i64 [ %R34, %subblock_4009e3_2 ]
  ; # 4009f3: pxor   xmm0,xmm0
  ; # 4009f7: cvtsi2ss xmm0,eax
  ; r49 := (trunc r43 32)
  %R49 = trunc i64 %R43 to i32
  ; r50 := (fpFromBV r49 single)
  %R50 = sitofp i32 %R49 to float
  ; r51 := (uext r50 128)
  %R51 = zext i32 %R50 to i128
  ; # 4009fb: movq   eax,xmm0
  ; # 4009ff: shr    eax,0x15
  ; r52 := (bv_shr r50 0x15 :: [32])
  %R52 = lshr i32 %R50, 21
  ; # 400a02: sub    eax,0x1f0
  ; r53 := (bv_add r52 0xfffffe10 :: [32])
  %R53 = add i32 %R52, 4294966800
  ; r54 := (uext r53 64)
  %R54 = zext i32 %R53 to i64
  br label %block_400a07
block_400a07:
  %R61 = phi i128 [ %R51, %block_4009f3 ], [ %R28, %subblock_4009d2_1 ]
  %R60 = phi i64 [ %R48, %block_4009f3 ], [ %R27, %subblock_4009d2_1 ]
  %R59 = phi i64 [ %R47, %block_4009f3 ], [ %R26, %subblock_4009d2_1 ]
  %R58 = phi i64 [ %R46, %block_4009f3 ], [ %R25, %subblock_4009d2_1 ]
  %R57 = phi i64 [ %R45, %block_4009f3 ], [ %R24, %subblock_4009d2_1 ]
  %R56 = phi i64 [ %R44, %block_4009f3 ], [ %R23, %subblock_4009d2_1 ]
  %R55 = phi i64 [ %R54, %block_4009f3 ], [ %R30, %subblock_4009d2_1 ]
  ; # 400a07: mov    DWORD PTR [rsp+0x4],eax
  ; r62 := (trunc r55 32)
  %R62 = trunc i64 %R55 to i32
  ; r63 := (bv_add r57 0x4 :: [64])
  %R63 = add i64 %R57, 4
  ; *(r63) = r62
  %r70 = inttoptr i64 %R63 to i32*
  store i32 %R62, i32* %r70
  br label %block_400a0b
block_400a0b:
  %R69 = phi i128 [ %R40, %subblock_4009e3_1 ], [ %R61, %block_400a07 ]
  %R68 = phi i64 [ %R39, %subblock_4009e3_1 ], [ %R60, %block_400a07 ]
  %R67 = phi i64 [ %R38, %subblock_4009e3_1 ], [ %R59, %block_400a07 ]
  %R66 = phi i64 [ %R37, %subblock_4009e3_1 ], [ %R58, %block_400a07 ]
  %R65 = phi i64 [ %R36, %subblock_4009e3_1 ], [ %R57, %block_400a07 ]
  %R64 = phi i64 [ %R35, %subblock_4009e3_1 ], [ %R56, %block_400a07 ]
  ; # 400a0b: movsxd r12,DWORD PTR [rsp+0x4]
  ; r70 := (bv_add r65 0x4 :: [64])
  %R70 = add i64 %R65, 4
  ; r71 := *r70
  %r78 = inttoptr i64 %R70 to i32*
  %R71 = load i32* %r78
  ; r72 := (sext r71 64)
  %R72 = sext i32 %R71 to i64
  ; # 400a10: lea    rax,[r12+r12*2]
  ; r73 := (bv_mul 0x2 :: [64] r72)
  %R73 = mul i64 2, %R72
  ; r74 := (bv_add r72 r73)
  %R74 = add i64 %R72, %R73
  ; # 400a14: lea    rbp,[rax*8+0x6040e0]
  ; r75 := (bv_mul 0x8 :: [64] r74)
  %R75 = mul i64 8, %R74
  ; r76 := (bv_add r75 0x6040e0 :: [64])
  %R76 = add i64 %R75, 6308064
  ; # 400a1c: mov    eax,DWORD PTR [rip+0x203f6a]
  ; r77 := *0x60498c :: [64]
  %r85 = inttoptr i64 6310284 to i32*
  %R77 = load i32* %r85
  ; # 400a22: lea    r15,[rbp+0x8]
  ; r78 := (bv_add r75 0x6040e8 :: [64])
  %R78 = add i64 %R75, 6308072
  ; # 400a26: test   eax,eax
  ; r79 := (bv_eq r77 0x0 :: [32])
  %R79 = icmp eq i32 %R77, 0
  ; # 400a28: jne    400ac8
  ; r80 := (bv_complement r79)
  %R80 = xor i1 %R79, -1
  br i1 %R80, label %subblock_400a0b_1, label %subblock_400a0b_2
subblock_400a0b_1:
  br label %block_400ac8
subblock_400a0b_2:
  br label %block_400a2e
block_400a2e:
  %R89 = phi i128 [ %R69, %subblock_400a0b_2 ]
  %R88 = phi i64 [ %R78, %subblock_400a0b_2 ]
  %R87 = phi i64 [ %R68, %subblock_400a0b_2 ]
  %R86 = phi i64 [ %R72, %subblock_400a0b_2 ]
  %R85 = phi i64 [ %R67, %subblock_400a0b_2 ]
  %R84 = phi i64 [ %R66, %subblock_400a0b_2 ]
  %R83 = phi i64 [ %R76, %subblock_400a0b_2 ]
  %R82 = phi i64 [ %R65, %subblock_400a0b_2 ]
  %R81 = phi i64 [ %R64, %subblock_400a0b_2 ]
  ; # 400a2e: cmp    QWORD PTR [rbp+0x10],0x0
  ; r90 := (bv_add r83 0x10 :: [64])
  %R90 = add i64 %R83, 16
  ; r91 := *r90
  %r100 = inttoptr i64 %R90 to i64*
  %R91 = load i64* %r100
  ; r92 := (bv_eq r91 0x0 :: [64])
  %R92 = icmp eq i64 %R91, 0
  ; # 400a33: jne    400b20
  ; r93 := (bv_complement r92)
  %R93 = xor i1 %R92, -1
  br i1 %R93, label %subblock_400a2e_1, label %subblock_400a2e_2
subblock_400a2e_1:
  br label %block_400b20
subblock_400a2e_2:
  br label %block_400a39
block_400a39:
  %R102 = phi i128 [ %R89, %subblock_400a2e_2 ]
  %R101 = phi i64 [ %R88, %subblock_400a2e_2 ]
  %R100 = phi i64 [ %R87, %subblock_400a2e_2 ]
  %R99 = phi i64 [ %R86, %subblock_400a2e_2 ]
  %R98 = phi i64 [ %R85, %subblock_400a2e_2 ]
  %R97 = phi i64 [ %R84, %subblock_400a2e_2 ]
  %R96 = phi i64 [ %R83, %subblock_400a2e_2 ]
  %R95 = phi i64 [ %R82, %subblock_400a2e_2 ]
  %R94 = phi i64 [ %R81, %subblock_400a2e_2 ]
  ; # 400a39: mov    rax,rbx
  br label %block_400a3c
block_400a3c:
  %R112 = phi i128 [ %R102, %block_400a39 ], [ %R370, %block_400bd7 ]
  %R111 = phi i64 [ %R101, %block_400a39 ], [ %R369, %block_400bd7 ]
  %R110 = phi i64 [ %R100, %block_400a39 ], [ %R368, %block_400bd7 ]
  %R109 = phi i64 [ %R99, %block_400a39 ], [ %R367, %block_400bd7 ]
  %R108 = phi i64 [ %R98, %block_400a39 ], [ %R366, %block_400bd7 ]
  %R107 = phi i64 [ %R97, %block_400a39 ], [ %R365, %block_400bd7 ]
  %R106 = phi i64 [ %R96, %block_400a39 ], [ %R364, %block_400bd7 ]
  %R105 = phi i64 [ %R95, %block_400a39 ], [ %R363, %block_400bd7 ]
  %R104 = phi i64 [ %R94, %block_400a39 ], [ %R362, %block_400bd7 ]
  %R103 = phi i64 [ %R94, %block_400a39 ], [ %R374, %block_400bd7 ]
  ; # 400a3c: lea    rdx,[r12+r12*2]
  ; r113 := (bv_mul 0x2 :: [64] r109)
  %R113 = mul i64 2, %R109
  ; r114 := (bv_add r109 r113)
  %R114 = add i64 %R109, %R113
  ; # 400a40: lea    rdx,[rdx*8+0x6040e0]
  ; r115 := (bv_mul 0x8 :: [64] r114)
  %R115 = mul i64 8, %R114
  ; r116 := (bv_add r115 0x6040e0 :: [64])
  %R116 = add i64 %R115, 6308064
  ; # 400a48: mov    QWORD PTR [rdx+0x18],rdx
  ; r117 := (bv_add r115 0x6040f8 :: [64])
  %R117 = add i64 %R115, 6308088
  ; *(r117) = r116
  %r128 = inttoptr i64 %R117 to i64*
  store i64 %R116, i64* %r128
  ; # 400a4c: mov    QWORD PTR [rdx+0x10],rdx
  ; r118 := (bv_add r115 0x6040f0 :: [64])
  %R118 = add i64 %R115, 6308080
  ; *(r118) = r116
  %r130 = inttoptr i64 %R118 to i64*
  store i64 %R116, i64* %r130
  br label %block_400a50
block_400a50:
  %R127 = phi i128 [ %R112, %block_400a3c ], [ %R259, %block_400b12 ]
  %R126 = phi i64 [ %R111, %block_400a3c ], [ %R258, %block_400b12 ]
  %R125 = phi i64 [ %R110, %block_400a3c ], [ %R257, %block_400b12 ]
  %R124 = phi i64 [ %R108, %block_400a3c ], [ %R256, %block_400b12 ]
  %R123 = phi i64 [ %R107, %block_400a3c ], [ %R255, %block_400b12 ]
  %R122 = phi i64 [ %R106, %block_400a3c ], [ %R254, %block_400b12 ]
  %R121 = phi i64 [ %R105, %block_400a3c ], [ %R253, %block_400b12 ]
  %R120 = phi i64 [ %R104, %block_400a3c ], [ %R252, %block_400b12 ]
  %R119 = phi i64 [ %R103, %block_400a3c ], [ %R263, %block_400b12 ]
  ; # 400a50: cmp    rbx,rax
  ; r128 := (bv_eq r120 r119)
  %R128 = icmp eq i64 %R120, %R119
  ; # 400a53: je     400b20
  br i1 %R128, label %subblock_400a50_1, label %subblock_400a50_2
subblock_400a50_1:
  br label %block_400b20
subblock_400a50_2:
  br label %block_400a59
block_400a59:
  %R136 = phi i128 [ %R127, %subblock_400a50_2 ]
  %R135 = phi i64 [ %R126, %subblock_400a50_2 ]
  %R134 = phi i64 [ %R125, %subblock_400a50_2 ]
  %R133 = phi i64 [ %R124, %subblock_400a50_2 ]
  %R132 = phi i64 [ %R123, %subblock_400a50_2 ]
  %R131 = phi i64 [ %R122, %subblock_400a50_2 ]
  %R130 = phi i64 [ %R121, %subblock_400a50_2 ]
  %R129 = phi i64 [ %R119, %subblock_400a50_2 ]
  ; # 400a59: mov    edx,DWORD PTR [rbp+0x8]
  ; r137 := (bv_add r131 0x8 :: [64])
  %R137 = add i64 %R131, 8
  ; r138 := *r137
  %r150 = inttoptr i64 %R137 to i32*
  %R138 = load i32* %r150
  ; # 400a5c: test   edx,edx
  ; r139 := (bv_eq r138 0x0 :: [32])
  %R139 = icmp eq i32 %R138, 0
  ; # 400a5e: je     400aa8
  br i1 %R139, label %subblock_400a59_1, label %subblock_400a59_2
subblock_400a59_1:
  br label %block_400aa8
subblock_400a59_2:
  br label %block_400a60
block_400a60:
  %R145 = phi i128 [ %R136, %subblock_400a59_2 ]
  %R144 = phi i64 [ %R135, %subblock_400a59_2 ]
  %R143 = phi i64 [ %R134, %subblock_400a59_2 ]
  %R142 = phi i64 [ %R133, %subblock_400a59_2 ]
  %R141 = phi i64 [ %R132, %subblock_400a59_2 ]
  %R140 = phi i64 [ %R130, %subblock_400a59_2 ]
  ; # 400a60: xor    eax,eax
  ; # 400a62: mov    DWORD PTR [r15],eax
  ; *(r144) = 0x0 :: [32]
  %r159 = inttoptr i64 %R144 to i32*
  store i32 0, i32* %r159
  ; # 400a65: lock or DWORD PTR [rsp],0x0
  ; r146 := *r140
  %r160 = inttoptr i64 %R140 to i32*
  %R146 = load i32* %r160
  ; *(r140) = r146
  %r162 = inttoptr i64 %R140 to i32*
  store i32 %R146, i32* %r162
  ; # 400a6a: mov    eax,DWORD PTR [r15+0x4]
  ; r147 := (bv_add r144 0x4 :: [64])
  %R147 = add i64 %R144, 4
  ; r148 := *r147
  %r164 = inttoptr i64 %R147 to i32*
  %R148 = load i32* %r164
  ; # 400a6e: test   eax,eax
  ; r149 := (bv_eq r148 0x0 :: [32])
  %R149 = icmp eq i32 %R148, 0
  ; # 400a70: je     4009c0
  br i1 %R149, label %subblock_400a60_1, label %subblock_400a60_2
subblock_400a60_1:
  br label %block_4009c0
subblock_400a60_2:
  br label %block_400a76
block_400a76:
  %R154 = phi i64 [ %R144, %subblock_400a60_2 ]
  %R153 = phi i64 [ %R143, %subblock_400a60_2 ]
  %R152 = phi i64 [ %R142, %subblock_400a60_2 ]
  %R151 = phi i64 [ %R141, %subblock_400a60_2 ]
  %R150 = phi i64 [ %R140, %subblock_400a60_2 ]
  ; # 400a76: mov    r9d,0xca
  ; # 400a7c: mov    edx,0x1
  ; # 400a81: mov    esi,0x81
  ; # 400a86: mov    rax,r9
  ; # 400a89: mov    rdi,r15
  ; # 400a8c: syscall
  ; sys_futex
  %r172 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R154, i64 129, i64 1, i64 %R152, i64 %R151, i64 202, i64 202)
  %R155 = extractvalue { i64, i1 } %r172, 0
  br label %block_400a8e
block_400a8e:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R164 = phi i128 [ undef, %block_400a76 ]
  %R163 = phi i64 [ %R153, %block_400a76 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R162 = phi i64 [ undef, %block_400a76 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R161 = phi i64 [ undef, %block_400a76 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R160 = phi i64 [ undef, %block_400a76 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R159 = phi i64 [ undef, %block_400a76 ]
  %R158 = phi i64 [ %R150, %block_400a76 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R157 = phi i64 [ undef, %block_400a76 ]
  %R156 = phi i64 [ %R155, %block_400a76 ]
  ; # 400a8e: cmp    rax,0xffffffffffffffda
  ; r165 := (bv_eq r156 0xffffffffffffffda :: [64])
  %R165 = icmp eq i64 %R156, 18446744073709551578
  ; # 400a92: jne    4009c0
  ; r166 := (bv_complement r165)
  %R166 = xor i1 %R165, -1
  br i1 %R166, label %subblock_400a8e_1, label %subblock_400a8e_2
subblock_400a8e_1:
  br label %block_4009c0
subblock_400a8e_2:
  br label %block_400a98
block_400a98:
  %R173 = phi i64 [ %R163, %subblock_400a8e_2 ]
  %R172 = phi i64 [ %R162, %subblock_400a8e_2 ]
  %R171 = phi i64 [ %R161, %subblock_400a8e_2 ]
  %R170 = phi i64 [ %R160, %subblock_400a8e_2 ]
  %R169 = phi i64 [ %R159, %subblock_400a8e_2 ]
  %R168 = phi i64 [ %R158, %subblock_400a8e_2 ]
  %R167 = phi i64 [ %R157, %subblock_400a8e_2 ]
  ; # 400a98: mov    rax,r9
  ; # 400a9b: mov    rsi,rdx
  ; # 400a9e: syscall
  ; sys_futex
  %r192 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R169, i64 %R167, i64 %R167, i64 %R172, i64 %R170, i64 %R171, i64 202)
  %R174 = extractvalue { i64, i1 } %r192, 0
  br label %block_400aa0
block_400aa0:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R179 = phi i128 [ undef, %block_400a98 ]
  %R178 = phi i64 [ %R173, %block_400a98 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R177 = phi i64 [ undef, %block_400a98 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R176 = phi i64 [ undef, %block_400a98 ]
  %R175 = phi i64 [ %R168, %block_400a98 ]
  ; # 400aa0: jmp    4009c0
  br label %block_4009c0
block_400aa8:
  %R185 = phi i128 [ %R136, %subblock_400a59_1 ]
  %R184 = phi i64 [ %R134, %subblock_400a59_1 ]
  %R183 = phi i64 [ %R133, %subblock_400a59_1 ]
  %R182 = phi i64 [ %R132, %subblock_400a59_1 ]
  %R181 = phi i64 [ %R130, %subblock_400a59_1 ]
  %R180 = phi i64 [ %R129, %subblock_400a59_1 ]
  ; # 400aa8: mov    rbx,rax
  ; # 400aab: test   bl,0x1
  ; r186 := (trunc r180 8)
  %R186 = trunc i64 %R180 to i8
  ; r187 := (bv_and r186 0x1 :: [8])
  %R187 = and i8 %R186, 1
  ; r188 := (bv_eq r187 0x0 :: [8])
  %R188 = icmp eq i8 %R187, 0
  ; # 400aae: je     4009d2
  br i1 %R188, label %subblock_400aa8_1, label %subblock_400aa8_2
subblock_400aa8_1:
  br label %block_4009d2
subblock_400aa8_2:
  br label %block_400ab4
block_400ab4:
  %R189 = phi i128 [ %R185, %subblock_400aa8_2 ], [ %R14, %subblock_4009c0_1 ]
  ; # 400ab4: add    rsp,0x18
  ; # 400ab8: xor    eax,eax
  ; # 400aba: pop    rbx
  ; # 400abb: pop    rbp
  ; # 400abc: pop    r12
  ; # 400abe: pop    r13
  ; # 400ac0: pop    r14
  ; # 400ac2: pop    r15
  ; # 400ac4: ret
  %r209 = bitcast i128 %R189 to <2 x double>
  %r210 = insertvalue { i64, i64, <2 x double> } undef, i64 0, 0
  %r211 = insertvalue { i64, i64, <2 x double> } %r210, i64 undef, 1
  %r212 = insertvalue { i64, i64, <2 x double> } %r211, <2 x double> %r209, 2
  ret { i64, i64, <2 x double> } %r212
block_400ac8:
  %R198 = phi i128 [ %R69, %subblock_400a0b_1 ]
  %R197 = phi i64 [ %R78, %subblock_400a0b_1 ]
  %R196 = phi i64 [ %R68, %subblock_400a0b_1 ]
  %R195 = phi i64 [ %R72, %subblock_400a0b_1 ]
  %R194 = phi i64 [ %R67, %subblock_400a0b_1 ]
  %R193 = phi i64 [ %R66, %subblock_400a0b_1 ]
  %R192 = phi i64 [ %R76, %subblock_400a0b_1 ]
  %R191 = phi i64 [ %R65, %subblock_400a0b_1 ]
  %R190 = phi i64 [ %R64, %subblock_400a0b_1 ]
  ; # 400ac8: mov    eax,r13d
  ; r199 := (trunc r196 32)
  %R199 = trunc i64 %R196 to i32
  ; # 400acb: xchg   DWORD PTR [rbp+0x8],eax
  ; r200 := (bv_add r192 0x8 :: [64])
  %R200 = add i64 %R192, 8
  ; r201 := *r200
  %r224 = inttoptr i64 %R200 to i32*
  %R201 = load i32* %r224
  ; *(r200) = r199
  %r226 = inttoptr i64 %R200 to i32*
  store i32 %R199, i32* %r226
  ; # 400ace: test   eax,eax
  ; r202 := (bv_eq r201 0x0 :: [32])
  %R202 = icmp eq i32 %R201, 0
  ; # 400ad0: lea    r14,[rbp+0xc]
  ; r203 := (bv_add r192 0xc :: [64])
  %R203 = add i64 %R192, 12
  ; # 400ad4: je     400aff
  br i1 %R202, label %subblock_400ac8_1, label %subblock_400ac8_2
subblock_400ac8_1:
  br label %block_400aff
subblock_400ac8_2:
  br label %block_400ad6
block_400ad6:
  %R211 = phi i128 [ %R198, %subblock_400ac8_2 ]
  %R210 = phi i64 [ %R197, %subblock_400ac8_2 ]
  %R209 = phi i64 [ %R203, %subblock_400ac8_2 ]
  %R208 = phi i64 [ %R196, %subblock_400ac8_2 ]
  %R207 = phi i64 [ %R195, %subblock_400ac8_2 ]
  %R206 = phi i64 [ %R192, %subblock_400ac8_2 ]
  %R205 = phi i64 [ %R191, %subblock_400ac8_2 ]
  %R204 = phi i64 [ %R190, %subblock_400ac8_2 ]
  ; # 400ad6: nop    [rax+rax*1]
  br label %block_400ae0
block_400ae0:
  %R219 = phi i128 [ %R232, %subblock_400af5_1 ], [ %R211, %block_400ad6 ]
  %R218 = phi i64 [ %R231, %subblock_400af5_1 ], [ %R210, %block_400ad6 ]
  %R217 = phi i64 [ %R230, %subblock_400af5_1 ], [ %R209, %block_400ad6 ]
  %R216 = phi i64 [ %R229, %subblock_400af5_1 ], [ %R208, %block_400ad6 ]
  %R215 = phi i64 [ %R228, %subblock_400af5_1 ], [ %R207, %block_400ad6 ]
  %R214 = phi i64 [ %R225, %subblock_400af5_1 ], [ %R206, %block_400ad6 ]
  %R213 = phi i64 [ %R224, %subblock_400af5_1 ], [ %R205, %block_400ad6 ]
  %R212 = phi i64 [ %R223, %subblock_400af5_1 ], [ %R204, %block_400ad6 ]
  ; # 400ae0: mov    ecx,0x1
  ; # 400ae5: mov    edx,0x1
  ; # 400aea: mov    rsi,r14
  ; # 400aed: mov    rdi,r15
  ; # 400af0: call   40241c
  ; r220 := (bv_add r213 0xfffffffffffffff8 :: [64])
  %R220 = add i64 %R213, 18446744073709551608
  ; r222 := (bv_add r220 0x8 :: [64])
  %R222 = add i64 %R220, 8
  %r247 = bitcast i128 %R219 to <2 x double>
  %r248 = call { i64, i64, <2 x double> } @F40241c(i64 %R218, i64 %R217, i64 1, i64 1, <2 x double> %r247)
  %r249 = extractvalue { i64, i64, <2 x double> } %r248, 2
  %R221 = bitcast <2 x double> %r249 to i128
  br label %block_400af5
block_400af5:
  %R232 = phi i128 [ %R221, %block_400ae0 ]
  %R231 = phi i64 [ %R218, %block_400ae0 ]
  %R230 = phi i64 [ %R217, %block_400ae0 ]
  %R229 = phi i64 [ %R216, %block_400ae0 ]
  %R228 = phi i64 [ %R215, %block_400ae0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R227 = phi i64 [ undef, %block_400ae0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R226 = phi i64 [ undef, %block_400ae0 ]
  %R225 = phi i64 [ %R214, %block_400ae0 ]
  %R224 = phi i64 [ %R222, %block_400ae0 ]
  %R223 = phi i64 [ %R212, %block_400ae0 ]
  ; # 400af5: mov    ecx,r13d
  ; r233 := (trunc r229 32)
  %R233 = trunc i64 %R229 to i32
  ; # 400af8: xchg   DWORD PTR [r15],ecx
  ; r234 := *r231
  %r262 = inttoptr i64 %R231 to i32*
  %R234 = load i32* %r262
  ; *(r231) = r233
  %r264 = inttoptr i64 %R231 to i32*
  store i32 %R233, i32* %r264
  ; # 400afb: test   ecx,ecx
  ; r235 := (bv_eq r234 0x0 :: [32])
  %R235 = icmp eq i32 %R234, 0
  ; # 400afd: jne    400ae0
  ; r236 := (bv_complement r235)
  %R236 = xor i1 %R235, -1
  br i1 %R236, label %subblock_400af5_1, label %subblock_400af5_2
subblock_400af5_1:
  br label %block_400ae0
subblock_400af5_2:
  br label %block_400aff
block_400aff:
  %R245 = phi i128 [ %R232, %subblock_400af5_2 ], [ %R198, %subblock_400ac8_1 ]
  %R244 = phi i64 [ %R231, %subblock_400af5_2 ], [ %R197, %subblock_400ac8_1 ]
  %R243 = phi i64 [ %R229, %subblock_400af5_2 ], [ %R196, %subblock_400ac8_1 ]
  %R242 = phi i64 [ %R228, %subblock_400af5_2 ], [ %R195, %subblock_400ac8_1 ]
  %R241 = phi i64 [ %R227, %subblock_400af5_2 ], [ %R194, %subblock_400ac8_1 ]
  %R240 = phi i64 [ %R226, %subblock_400af5_2 ], [ %R193, %subblock_400ac8_1 ]
  %R239 = phi i64 [ %R225, %subblock_400af5_2 ], [ %R192, %subblock_400ac8_1 ]
  %R238 = phi i64 [ %R224, %subblock_400af5_2 ], [ %R191, %subblock_400ac8_1 ]
  %R237 = phi i64 [ %R223, %subblock_400af5_2 ], [ %R190, %subblock_400ac8_1 ]
  ; # 400aff: lea    rax,[r12+r12*2]
  ; r246 := (bv_mul 0x2 :: [64] r242)
  %R246 = mul i64 2, %R242
  ; r247 := (bv_add r242 r246)
  %R247 = add i64 %R242, %R246
  ; # 400b03: cmp    QWORD PTR [rax*8+0x6040f0],0x0
  ; r248 := (bv_mul 0x8 :: [64] r247)
  %R248 = mul i64 8, %R247
  ; r249 := (bv_add r248 0x6040f0 :: [64])
  %R249 = add i64 %R248, 6308080
  ; r250 := *r249
  %r280 = inttoptr i64 %R249 to i64*
  %R250 = load i64* %r280
  ; r251 := (bv_eq r250 0x0 :: [64])
  %R251 = icmp eq i64 %R250, 0
  ; # 400b0c: je     400bd7
  br i1 %R251, label %subblock_400aff_1, label %subblock_400aff_2
subblock_400aff_1:
  br label %block_400bd7
subblock_400aff_2:
  br label %block_400b12
block_400b12:
  %R259 = phi i128 [ %R245, %subblock_400aff_2 ]
  %R258 = phi i64 [ %R244, %subblock_400aff_2 ]
  %R257 = phi i64 [ %R243, %subblock_400aff_2 ]
  %R256 = phi i64 [ %R241, %subblock_400aff_2 ]
  %R255 = phi i64 [ %R240, %subblock_400aff_2 ]
  %R254 = phi i64 [ %R239, %subblock_400aff_2 ]
  %R253 = phi i64 [ %R238, %subblock_400aff_2 ]
  %R252 = phi i64 [ %R237, %subblock_400aff_2 ]
  ; # 400b12: mov    rax,QWORD PTR [rsp+0x8]
  ; r260 := (bv_add r253 0x8 :: [64])
  %R260 = add i64 %R253, 8
  ; r261 := *r260
  %r292 = inttoptr i64 %R260 to i64*
  %R261 = load i64* %r292
  ; # 400b17: mov    rax,QWORD PTR [rax+0x8]
  ; r262 := (bv_add r261 0x8 :: [64])
  %R262 = add i64 %R261, 8
  ; r263 := *r262
  %r295 = inttoptr i64 %R262 to i64*
  %R263 = load i64* %r295
  ; # 400b1b: jmp    400a50
  br label %block_400a50
block_400b20:
  %R269 = phi i128 [ %R89, %subblock_400a2e_1 ], [ %R127, %subblock_400a50_1 ]
  %R268 = phi i64 [ %R88, %subblock_400a2e_1 ], [ %R126, %subblock_400a50_1 ]
  %R267 = phi i64 [ %R85, %subblock_400a2e_1 ], [ %R124, %subblock_400a50_1 ]
  %R266 = phi i64 [ %R84, %subblock_400a2e_1 ], [ %R123, %subblock_400a50_1 ]
  %R265 = phi i64 [ %R82, %subblock_400a2e_1 ], [ %R121, %subblock_400a50_1 ]
  %R264 = phi i64 [ %R81, %subblock_400a2e_1 ], [ %R120, %subblock_400a50_1 ]
  ; # 400b20: mov    rdi,QWORD PTR [rsp+0x8]
  ; r270 := (bv_add r265 0x8 :: [64])
  %R270 = add i64 %R265, 8
  ; r271 := *r270
  %r304 = inttoptr i64 %R270 to i64*
  %R271 = load i64* %r304
  ; # 400b25: mov    rax,QWORD PTR [rdi+0x18]
  ; r272 := (bv_add r271 0x18 :: [64])
  %R272 = add i64 %R271, 24
  ; r273 := *r272
  %r307 = inttoptr i64 %R272 to i64*
  %R273 = load i64* %r307
  ; # 400b29: mov    rdx,QWORD PTR [rdi+0x10]
  ; r274 := (bv_add r271 0x10 :: [64])
  %R274 = add i64 %R271, 16
  ; r275 := *r274
  %r310 = inttoptr i64 %R274 to i64*
  %R275 = load i64* %r310
  ; # 400b2d: cmp    rax,rdx
  ; r276 := (bv_eq r273 r275)
  %R276 = icmp eq i64 %R273, %R275
  ; # 400b30: je     400baa
  br i1 %R276, label %subblock_400b20_1, label %subblock_400b20_2
subblock_400b20_1:
  br label %block_400baa
subblock_400b20_2:
  br label %block_400b32
block_400b32:
  %R284 = phi i128 [ %R351, %block_400bb9 ], [ %R269, %subblock_400b20_2 ]
  %R283 = phi i64 [ %R350, %block_400bb9 ], [ %R268, %subblock_400b20_2 ]
  %R282 = phi i64 [ %R349, %block_400bb9 ], [ %R267, %subblock_400b20_2 ]
  %R281 = phi i64 [ %R348, %block_400bb9 ], [ %R266, %subblock_400b20_2 ]
  %R280 = phi i64 [ %R347, %block_400bb9 ], [ %R265, %subblock_400b20_2 ]
  %R279 = phi i64 [ %R361, %block_400bb9 ], [ %R264, %subblock_400b20_2 ]
  %R278 = phi i64 [ %R359, %block_400bb9 ], [ %R275, %subblock_400b20_2 ]
  %R277 = phi i64 [ %R357, %block_400bb9 ], [ %R273, %subblock_400b20_2 ]
  ; # 400b32: mov    rsi,QWORD PTR [rsp+0x8]
  ; r285 := (bv_add r280 0x8 :: [64])
  %R285 = add i64 %R280, 8
  ; r286 := *r285
  %r322 = inttoptr i64 %R285 to i64*
  %R286 = load i64* %r322
  ; # 400b37: mov    QWORD PTR [rax+0x10],rdx
  ; r287 := (bv_add r277 0x10 :: [64])
  %R287 = add i64 %R277, 16
  ; *(r287) = r278
  %r325 = inttoptr i64 %R287 to i64*
  store i64 %R278, i64* %r325
  ; # 400b3b: mov    rdx,QWORD PTR [rsi+0x10]
  ; r288 := (bv_add r286 0x10 :: [64])
  %R288 = add i64 %R286, 16
  ; r289 := *r288
  %r327 = inttoptr i64 %R288 to i64*
  %R289 = load i64* %r327
  ; # 400b3f: mov    QWORD PTR [rdx+0x18],rax
  ; r290 := (bv_add r289 0x18 :: [64])
  %R290 = add i64 %R289, 24
  ; *(r290) = r277
  %r330 = inttoptr i64 %R290 to i64*
  store i64 %R277, i64* %r330
  ; # 400b43: mov    rax,rbx
  ; # 400b46: and    rbx,0xfffffffffffffffe
  ; r291 := (bv_and r279 0xfffffffffffffffe :: [64])
  %R291 = and i64 %R279, 18446744073709551614
  ; # 400b4a: or     rax,0x1
  ; r292 := (bv_or r279 0x1 :: [64])
  %R292 = or i64 %R279, 1
  ; # 400b4e: mov    QWORD PTR [rsi+0x8],rax
  ; r293 := (bv_add r286 0x8 :: [64])
  %R293 = add i64 %R286, 8
  ; *(r293) = r292
  %r334 = inttoptr i64 %R293 to i64*
  store i64 %R292, i64* %r334
  ; # 400b52: mov    eax,DWORD PTR [r15]
  ; r294 := *r283
  %r335 = inttoptr i64 %R283 to i32*
  %R294 = load i32* %r335
  ; # 400b55: or     QWORD PTR [rsi+rbx*1],0x1
  ; r295 := (bv_add r286 r291)
  %R295 = add i64 %R286, %R291
  ; r296 := *r295
  %r338 = inttoptr i64 %R295 to i64*
  %R296 = load i64* %r338
  ; r297 := (bv_or r296 0x1 :: [64])
  %R297 = or i64 %R296, 1
  ; *(r295) = r297
  %r341 = inttoptr i64 %R295 to i64*
  store i64 %R297, i64* %r341
  ; # 400b5a: test   eax,eax
  ; r298 := (bv_eq r294 0x0 :: [32])
  %R298 = icmp eq i32 %R294, 0
  ; # 400b5c: je     400b96
  br i1 %R298, label %subblock_400b32_1, label %subblock_400b32_2
subblock_400b32_1:
  br label %block_400b96
subblock_400b32_2:
  br label %block_400b5e
block_400b5e:
  %R303 = phi i128 [ %R284, %subblock_400b32_2 ]
  %R302 = phi i64 [ %R283, %subblock_400b32_2 ]
  %R301 = phi i64 [ %R282, %subblock_400b32_2 ]
  %R300 = phi i64 [ %R281, %subblock_400b32_2 ]
  %R299 = phi i64 [ %R280, %subblock_400b32_2 ]
  ; # 400b5e: xor    eax,eax
  ; # 400b60: mov    DWORD PTR [r15],eax
  ; *(r302) = 0x0 :: [32]
  %r348 = inttoptr i64 %R302 to i32*
  store i32 0, i32* %r348
  ; # 400b63: lock or DWORD PTR [rsp],0x0
  ; r304 := *r299
  %r349 = inttoptr i64 %R299 to i32*
  %R304 = load i32* %r349
  ; *(r299) = r304
  %r351 = inttoptr i64 %R299 to i32*
  store i32 %R304, i32* %r351
  ; # 400b68: mov    eax,DWORD PTR [r15+0x4]
  ; r305 := (bv_add r302 0x4 :: [64])
  %R305 = add i64 %R302, 4
  ; r306 := *r305
  %r353 = inttoptr i64 %R305 to i32*
  %R306 = load i32* %r353
  ; # 400b6c: test   eax,eax
  ; r307 := (bv_eq r306 0x0 :: [32])
  %R307 = icmp eq i32 %R306, 0
  ; # 400b6e: je     400b96
  br i1 %R307, label %subblock_400b5e_1, label %subblock_400b5e_2
subblock_400b5e_1:
  br label %block_400b96
subblock_400b5e_2:
  br label %block_400b70
block_400b70:
  %R310 = phi i64 [ %R302, %subblock_400b5e_2 ]
  %R309 = phi i64 [ %R301, %subblock_400b5e_2 ]
  %R308 = phi i64 [ %R300, %subblock_400b5e_2 ]
  ; # 400b70: mov    r9d,0xca
  ; # 400b76: mov    edx,0x1
  ; # 400b7b: mov    esi,0x81
  ; # 400b80: mov    rax,r9
  ; # 400b83: mov    rdi,r15
  ; # 400b86: syscall
  ; sys_futex
  %r359 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R310, i64 129, i64 1, i64 %R309, i64 %R308, i64 202, i64 202)
  %R311 = extractvalue { i64, i1 } %r359, 0
  br label %block_400b88
block_400b88:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R318 = phi i128 [ undef, %block_400b70 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R317 = phi i64 [ undef, %block_400b70 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R316 = phi i64 [ undef, %block_400b70 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R315 = phi i64 [ undef, %block_400b70 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R314 = phi i64 [ undef, %block_400b70 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R313 = phi i64 [ undef, %block_400b70 ]
  %R312 = phi i64 [ %R311, %block_400b70 ]
  ; # 400b88: cmp    rax,0xffffffffffffffda
  ; r319 := (bv_eq r312 0xffffffffffffffda :: [64])
  %R319 = icmp eq i64 %R312, 18446744073709551578
  ; # 400b8c: jne    400b96
  ; r320 := (bv_complement r319)
  %R320 = xor i1 %R319, -1
  br i1 %R320, label %subblock_400b88_1, label %subblock_400b88_2
subblock_400b88_1:
  br label %block_400b96
subblock_400b88_2:
  br label %block_400b8e
block_400b8e:
  %R325 = phi i64 [ %R317, %subblock_400b88_2 ]
  %R324 = phi i64 [ %R316, %subblock_400b88_2 ]
  %R323 = phi i64 [ %R315, %subblock_400b88_2 ]
  %R322 = phi i64 [ %R314, %subblock_400b88_2 ]
  %R321 = phi i64 [ %R313, %subblock_400b88_2 ]
  ; # 400b8e: mov    rax,r9
  ; # 400b91: mov    rsi,rdx
  ; # 400b94: syscall
  ; sys_futex
  %r375 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R322, i64 %R321, i64 %R321, i64 %R325, i64 %R323, i64 %R324, i64 202)
  %R326 = extractvalue { i64, i1 } %r375, 0
  br label %block_400b96
block_400b96:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R327 = phi i128 [ undef, %block_400b8e ], [ %R318, %subblock_400b88_1 ], [ %R303, %subblock_400b5e_1 ], [ %R284, %subblock_400b32_1 ]
  ; # 400b96: add    rsp,0x18
  ; # 400b9a: mov    eax,0x1
  ; # 400b9f: pop    rbx
  ; # 400ba0: pop    rbp
  ; # 400ba1: pop    r12
  ; # 400ba3: pop    r13
  ; # 400ba5: pop    r14
  ; # 400ba7: pop    r15
  ; # 400ba9: ret
  %r378 = bitcast i128 %R327 to <2 x double>
  %r379 = insertvalue { i64, i64, <2 x double> } undef, i64 1, 0
  %r380 = insertvalue { i64, i64, <2 x double> } %r379, i64 undef, 1
  %r381 = insertvalue { i64, i64, <2 x double> } %r380, <2 x double> %r378, 2
  ret { i64, i64, <2 x double> } %r381
block_400baa:
  %R332 = phi i128 [ %R269, %subblock_400b20_1 ]
  %R331 = phi i64 [ %R268, %subblock_400b20_1 ]
  %R330 = phi i64 [ %R267, %subblock_400b20_1 ]
  %R329 = phi i64 [ %R266, %subblock_400b20_1 ]
  %R328 = phi i64 [ %R265, %subblock_400b20_1 ]
  ; # 400baa: movzx  ecx,BYTE PTR [rsp+0x4]
  ; r333 := (bv_add r328 0x4 :: [64])
  %R333 = add i64 %R328, 4
  ; r334 := *r333
  %r388 = inttoptr i64 %R333 to i8*
  %R334 = load i8* %r388
  ; r335 := (uext r334 64)
  %R335 = zext i8 %R334 to i64
  ; # 400baf: mov    rax,0xfffffffffffffffe
  ; # 400bb6: rol    rax,cl
  ; r336 := (bv_and r335 0x3f :: [64])
  %R336 = and i64 %R335, 63
  ; r337 := (bv_shl 0xfffffffffffffffe :: [64] r336)
  %R337 = shl i64 18446744073709551614, %R336
  ; r338 := (bv_sub 0x40 :: [64] r336)
  %R338 = sub i64 64, %R336
  ; r339 := (bv_shr 0xfffffffffffffffe :: [64] r338)
  %R339 = lshr i64 18446744073709551614, %R338
  ; r340 := (bv_or r337 r339)
  %R340 = or i64 %R337, %R339
  ; r341 := (bv_eq r336 0x0 :: [64])
  %R341 = icmp eq i64 %R336, 0
  ; r342 := (bv_complement r341)
  %R342 = xor i1 %R341, -1
  br i1 %R342, label %subblock_400baa_1, label %subblock_400baa_4
subblock_400baa_1:
  ; r343 := (uext r334 64)
  %R343 = zext i8 %R334 to i64
  ; r344 := (bv_and r343 0x3f :: [64])
  %R344 = and i64 %R343, 63
  ; r345 := (bv_eq r344 0x1 :: [64])
  %R345 = icmp eq i64 %R344, 1
  br i1 %R345, label %subblock_400baa_2, label %subblock_400baa_3
subblock_400baa_2:
  br label %block_400bb9
subblock_400baa_3:
  br label %block_400bb9
subblock_400baa_4:
  br label %block_400bb9
block_400bb9:
  %R351 = phi i128 [ %R332, %subblock_400baa_4 ], [ %R332, %subblock_400baa_3 ], [ %R332, %subblock_400baa_2 ]
  %R350 = phi i64 [ %R331, %subblock_400baa_4 ], [ %R331, %subblock_400baa_3 ], [ %R331, %subblock_400baa_2 ]
  %R349 = phi i64 [ %R330, %subblock_400baa_4 ], [ %R330, %subblock_400baa_3 ], [ %R330, %subblock_400baa_2 ]
  %R348 = phi i64 [ %R329, %subblock_400baa_4 ], [ %R329, %subblock_400baa_3 ], [ %R329, %subblock_400baa_2 ]
  %R347 = phi i64 [ %R328, %subblock_400baa_4 ], [ %R328, %subblock_400baa_3 ], [ %R328, %subblock_400baa_2 ]
  %R346 = phi i64 [ %R340, %subblock_400baa_4 ], [ %R340, %subblock_400baa_3 ], [ %R340, %subblock_400baa_2 ]
  ; # 400bb9: lock and QWORD PTR [rip+0x20351f],rax
  ; r352 := *0x6040e0 :: [64]
  %r407 = inttoptr i64 6308064 to i64*
  %R352 = load i64* %r407
  ; r353 := (bv_and r352 r346)
  %R353 = and i64 %R352, %R346
  ; *(0x6040e0 :: [64]) = r353
  %r410 = inttoptr i64 6308064 to i64*
  store i64 %R353, i64* %r410
  ; # 400bc1: mov    rdi,QWORD PTR [rsp+0x8]
  ; r354 := (bv_add r347 0x8 :: [64])
  %R354 = add i64 %R347, 8
  ; r355 := *r354
  %r412 = inttoptr i64 %R354 to i64*
  %R355 = load i64* %r412
  ; # 400bc6: mov    rax,QWORD PTR [rdi+0x18]
  ; r356 := (bv_add r355 0x18 :: [64])
  %R356 = add i64 %R355, 24
  ; r357 := *r356
  %r415 = inttoptr i64 %R356 to i64*
  %R357 = load i64* %r415
  ; # 400bca: mov    rdx,QWORD PTR [rdi+0x10]
  ; r358 := (bv_add r355 0x10 :: [64])
  %R358 = add i64 %R355, 16
  ; r359 := *r358
  %r418 = inttoptr i64 %R358 to i64*
  %R359 = load i64* %r418
  ; # 400bce: mov    rbx,QWORD PTR [rdi+0x8]
  ; r360 := (bv_add r355 0x8 :: [64])
  %R360 = add i64 %R355, 8
  ; r361 := *r360
  %r421 = inttoptr i64 %R360 to i64*
  %R361 = load i64* %r421
  ; # 400bd2: jmp    400b32
  br label %block_400b32
block_400bd7:
  %R370 = phi i128 [ %R245, %subblock_400aff_1 ]
  %R369 = phi i64 [ %R244, %subblock_400aff_1 ]
  %R368 = phi i64 [ %R243, %subblock_400aff_1 ]
  %R367 = phi i64 [ %R242, %subblock_400aff_1 ]
  %R366 = phi i64 [ %R241, %subblock_400aff_1 ]
  %R365 = phi i64 [ %R240, %subblock_400aff_1 ]
  %R364 = phi i64 [ %R239, %subblock_400aff_1 ]
  %R363 = phi i64 [ %R238, %subblock_400aff_1 ]
  %R362 = phi i64 [ %R237, %subblock_400aff_1 ]
  ; # 400bd7: mov    rax,QWORD PTR [rsp+0x8]
  ; r371 := (bv_add r363 0x8 :: [64])
  %R371 = add i64 %R363, 8
  ; r372 := *r371
  %r433 = inttoptr i64 %R371 to i64*
  %R372 = load i64* %r433
  ; # 400bdc: mov    rax,QWORD PTR [rax+0x8]
  ; r373 := (bv_add r372 0x8 :: [64])
  %R373 = add i64 %R372, 8
  ; r374 := *r373
  %r436 = inttoptr i64 %R373 to i64*
  %R374 = load i64* %r436
  ; # 400be0: jmp    400a3c
  br label %block_400a3c
failure:
  br label %failure
}