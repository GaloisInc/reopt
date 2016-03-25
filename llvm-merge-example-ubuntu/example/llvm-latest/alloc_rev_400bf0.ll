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
define { i64, i64, <2 x double> } @F400bf0(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, <2 x double> %a5) {
entry:
  %r0 = bitcast <2 x double> %a5 to i128
  br label %block_400bf0
block_400bf0:
  ; r0 := (alloca 0x50 :: [64])
  %r1 = alloca i8, i64 80
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x50 :: [64])
  %R1 = add i64 %R0, 80
  ; # 400bf0: push   r15
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 400bf2: push   r14
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 400bf4: push   r13
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 400bf6: push   r12
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 400bf8: mov    r13d,0x1
  ; # 400bfe: push   rbp
  ; r6 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R6 = add i64 %R1, 18446744073709551576
  ; # 400bff: push   rbx
  ; r7 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R7 = add i64 %R1, 18446744073709551568
  ; # 400c00: sub    rsp,0x18
  ; r8 := (bv_add r1 0xffffffffffffffb8 :: [64])
  %R8 = add i64 %R1, 18446744073709551544
  ; # 400c04: mov    QWORD PTR [rsp+0x8],rdi
  ; r9 := (bv_add r1 0xffffffffffffffc0 :: [64])
  %R9 = add i64 %R1, 18446744073709551552
  ; *(r9) = arg0
  %r12 = inttoptr i64 %R9 to i64*
  store i64 %a0, i64* %r12
  ; # 400c09: nop    [rax]
  br label %block_400c10
block_400c10:
  %R14 = phi i128 [ %R178, %block_400cef ], [ %R163, %subblock_400cdd_1 ], [ %R144, %subblock_400caf_1 ], [ %r0, %block_400bf0 ]
  %R13 = phi i64 [ %R177, %block_400cef ], [ %R162, %subblock_400cdd_1 ], [ %R142, %subblock_400caf_1 ], [ 1, %block_400bf0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r10
  %R12 = phi i64 [ %R176, %block_400cef ], [ %R161, %subblock_400cdd_1 ], [ %R141, %subblock_400caf_1 ], [ undef, %block_400bf0 ]
  %R11 = phi i64 [ %R175, %block_400cef ], [ %R159, %subblock_400cdd_1 ], [ %R140, %subblock_400caf_1 ], [ %a4, %block_400bf0 ]
  %R10 = phi i64 [ %R174, %block_400cef ], [ %R157, %subblock_400cdd_1 ], [ %R139, %subblock_400caf_1 ], [ %R8, %block_400bf0 ]
  ; # 400c10: mov    rax,QWORD PTR [rsp+0x8]
  ; r15 := (bv_add r10 0x8 :: [64])
  %R15 = add i64 %R10, 8
  ; r16 := *r15
  %r19 = inttoptr i64 %R15 to i64*
  %R16 = load i64* %r19
  ; # 400c15: mov    rbx,QWORD PTR [rax]
  ; r17 := *r16
  %r21 = inttoptr i64 %R16 to i64*
  %R17 = load i64* %r21
  ; # 400c18: test   bl,0x1
  ; r18 := (trunc r17 8)
  %R18 = trunc i64 %R17 to i8
  ; r19 := (bv_and r18 0x1 :: [8])
  %R19 = and i8 %R18, 1
  ; r20 := (bv_eq r19 0x0 :: [8])
  %R20 = icmp eq i8 %R19, 0
  ; # 400c1b: jne    400d04
  ; r21 := (bv_complement r20)
  %R21 = xor i1 %R20, -1
  br i1 %R21, label %subblock_400c10_1, label %subblock_400c10_2
subblock_400c10_1:
  br label %block_400d04
subblock_400c10_2:
  br label %block_400c21
block_400c21:
  %R27 = phi i128 [ %R184, %subblock_400cf8_1 ], [ %R14, %subblock_400c10_2 ]
  %R26 = phi i64 [ %R183, %subblock_400cf8_1 ], [ %R13, %subblock_400c10_2 ]
  %R25 = phi i64 [ %R182, %subblock_400cf8_1 ], [ %R12, %subblock_400c10_2 ]
  %R24 = phi i64 [ %R181, %subblock_400cf8_1 ], [ %R11, %subblock_400c10_2 ]
  %R23 = phi i64 [ %R180, %subblock_400cf8_1 ], [ %R10, %subblock_400c10_2 ]
  %R22 = phi i64 [ %R179, %subblock_400cf8_1 ], [ %R17, %subblock_400c10_2 ]
  ; # 400c21: mov    rax,rbx
  ; # 400c24: shr    rax,0x5
  ; r28 := (bv_shr r22 0x5 :: [64])
  %R28 = lshr i64 %R22, 5
  ; # 400c28: sub    rax,0x1
  ; r29 := (bv_add r28 0xffffffffffffffff :: [64])
  %R29 = add i64 %R28, 18446744073709551615
  ; # 400c2c: cmp    rax,0x20
  ; r30 := (bv_ult r29 0x20 :: [64])
  %R30 = icmp ult i64 %R29, 32
  ; r31 := (bv_eq r28 0x21 :: [64])
  %R31 = icmp eq i64 %R28, 33
  ; # 400c30: jbe    400c56
  ; r32 := (bv_or r30 r31)
  %R32 = or i1 %R30, %R31
  br i1 %R32, label %subblock_400c21_1, label %subblock_400c21_2
subblock_400c21_1:
  br label %block_400c56
subblock_400c21_2:
  br label %block_400c32
block_400c32:
  %R39 = phi i128 [ %R27, %subblock_400c21_2 ]
  %R38 = phi i64 [ %R26, %subblock_400c21_2 ]
  %R37 = phi i64 [ %R25, %subblock_400c21_2 ]
  %R36 = phi i64 [ %R24, %subblock_400c21_2 ]
  %R35 = phi i64 [ %R23, %subblock_400c21_2 ]
  %R34 = phi i64 [ %R22, %subblock_400c21_2 ]
  %R33 = phi i64 [ %R29, %subblock_400c21_2 ]
  ; # 400c32: cmp    rax,0x1c00
  ; # 400c38: mov    DWORD PTR [rsp+0x4],0x3f
  ; r40 := (bv_add r35 0x4 :: [64])
  %R40 = add i64 %R35, 4
  ; *(r40) = 0x3f :: [32]
  %r46 = inttoptr i64 %R40 to i32*
  store i32 63, i32* %r46
  ; # 400c40: ja     400c5a
  ; r41 := (bv_ult 0x1c00 :: [64] r33)
  %R41 = icmp ult i64 7168, %R33
  br i1 %R41, label %subblock_400c32_1, label %subblock_400c32_2
subblock_400c32_1:
  br label %block_400c5a
subblock_400c32_2:
  br label %block_400c42
block_400c42:
  %R47 = phi i64 [ %R38, %subblock_400c32_2 ]
  %R46 = phi i64 [ %R37, %subblock_400c32_2 ]
  %R45 = phi i64 [ %R36, %subblock_400c32_2 ]
  %R44 = phi i64 [ %R35, %subblock_400c32_2 ]
  %R43 = phi i64 [ %R34, %subblock_400c32_2 ]
  %R42 = phi i64 [ %R33, %subblock_400c32_2 ]
  ; # 400c42: pxor   xmm0,xmm0
  ; # 400c46: cvtsi2ss xmm0,eax
  ; r48 := (trunc r42 32)
  %R48 = trunc i64 %R42 to i32
  ; r49 := (fpFromBV r48 single)
  %R49 = sitofp i32 %R48 to float
  ; r50 := (uext r49 128)
  %R50 = zext i32 %R49 to i128
  ; # 400c4a: movq   eax,xmm0
  ; # 400c4e: shr    eax,0x15
  ; r51 := (bv_shr r49 0x15 :: [32])
  %R51 = lshr i32 %R49, 21
  ; # 400c51: sub    eax,0x1f0
  ; r52 := (bv_add r51 0xfffffe10 :: [32])
  %R52 = add i32 %R51, 4294966800
  ; r53 := (uext r52 64)
  %R53 = zext i32 %R52 to i64
  br label %block_400c56
block_400c56:
  %R60 = phi i128 [ %R50, %block_400c42 ], [ %R27, %subblock_400c21_1 ]
  %R59 = phi i64 [ %R47, %block_400c42 ], [ %R26, %subblock_400c21_1 ]
  %R58 = phi i64 [ %R46, %block_400c42 ], [ %R25, %subblock_400c21_1 ]
  %R57 = phi i64 [ %R45, %block_400c42 ], [ %R24, %subblock_400c21_1 ]
  %R56 = phi i64 [ %R44, %block_400c42 ], [ %R23, %subblock_400c21_1 ]
  %R55 = phi i64 [ %R43, %block_400c42 ], [ %R22, %subblock_400c21_1 ]
  %R54 = phi i64 [ %R53, %block_400c42 ], [ %R29, %subblock_400c21_1 ]
  ; # 400c56: mov    DWORD PTR [rsp+0x4],eax
  ; r61 := (trunc r54 32)
  %R61 = trunc i64 %R54 to i32
  ; r62 := (bv_add r56 0x4 :: [64])
  %R62 = add i64 %R56, 4
  ; *(r62) = r61
  %r69 = inttoptr i64 %R62 to i32*
  store i32 %R61, i32* %r69
  br label %block_400c5a
block_400c5a:
  %R68 = phi i128 [ %R39, %subblock_400c32_1 ], [ %R60, %block_400c56 ]
  %R67 = phi i64 [ %R38, %subblock_400c32_1 ], [ %R59, %block_400c56 ]
  %R66 = phi i64 [ %R37, %subblock_400c32_1 ], [ %R58, %block_400c56 ]
  %R65 = phi i64 [ %R36, %subblock_400c32_1 ], [ %R57, %block_400c56 ]
  %R64 = phi i64 [ %R35, %subblock_400c32_1 ], [ %R56, %block_400c56 ]
  %R63 = phi i64 [ %R34, %subblock_400c32_1 ], [ %R55, %block_400c56 ]
  ; # 400c5a: movsxd r12,DWORD PTR [rsp+0x4]
  ; r69 := (bv_add r64 0x4 :: [64])
  %R69 = add i64 %R64, 4
  ; r70 := *r69
  %r77 = inttoptr i64 %R69 to i32*
  %R70 = load i32* %r77
  ; r71 := (sext r70 64)
  %R71 = sext i32 %R70 to i64
  ; # 400c5f: lea    rax,[r12+r12*2]
  ; r72 := (bv_mul 0x2 :: [64] r71)
  %R72 = mul i64 2, %R71
  ; r73 := (bv_add r71 r72)
  %R73 = add i64 %R71, %R72
  ; # 400c63: lea    rbp,[rax*8+0x6040e0]
  ; r74 := (bv_mul 0x8 :: [64] r73)
  %R74 = mul i64 8, %R73
  ; r75 := (bv_add r74 0x6040e0 :: [64])
  %R75 = add i64 %R74, 6308064
  ; # 400c6b: mov    eax,DWORD PTR [rip+0x203d1b]
  ; r76 := *0x60498c :: [64]
  %r84 = inttoptr i64 6310284 to i32*
  %R76 = load i32* %r84
  ; # 400c71: lea    r15,[rbp+0x8]
  ; r77 := (bv_add r74 0x6040e8 :: [64])
  %R77 = add i64 %R74, 6308072
  ; # 400c75: test   eax,eax
  ; r78 := (bv_eq r76 0x0 :: [32])
  %R78 = icmp eq i32 %R76, 0
  ; # 400c77: jne    400d18
  ; r79 := (bv_complement r78)
  %R79 = xor i1 %R78, -1
  br i1 %R79, label %subblock_400c5a_1, label %subblock_400c5a_2
subblock_400c5a_1:
  br label %block_400d18
subblock_400c5a_2:
  br label %block_400c7d
block_400c7d:
  %R88 = phi i128 [ %R68, %subblock_400c5a_2 ]
  %R87 = phi i64 [ %R77, %subblock_400c5a_2 ]
  %R86 = phi i64 [ %R67, %subblock_400c5a_2 ]
  %R85 = phi i64 [ %R71, %subblock_400c5a_2 ]
  %R84 = phi i64 [ %R66, %subblock_400c5a_2 ]
  %R83 = phi i64 [ %R65, %subblock_400c5a_2 ]
  %R82 = phi i64 [ %R75, %subblock_400c5a_2 ]
  %R81 = phi i64 [ %R64, %subblock_400c5a_2 ]
  %R80 = phi i64 [ %R63, %subblock_400c5a_2 ]
  ; # 400c7d: cmp    QWORD PTR [rbp+0x10],0x0
  ; r89 := (bv_add r82 0x10 :: [64])
  %R89 = add i64 %R82, 16
  ; r90 := *r89
  %r99 = inttoptr i64 %R89 to i64*
  %R90 = load i64* %r99
  ; r91 := (bv_eq r90 0x0 :: [64])
  %R91 = icmp eq i64 %R90, 0
  ; # 400c82: jne    400d70
  ; r92 := (bv_complement r91)
  %R92 = xor i1 %R91, -1
  br i1 %R92, label %subblock_400c7d_1, label %subblock_400c7d_2
subblock_400c7d_1:
  br label %block_400d70
subblock_400c7d_2:
  br label %block_400c88
block_400c88:
  %R101 = phi i128 [ %R88, %subblock_400c7d_2 ]
  %R100 = phi i64 [ %R87, %subblock_400c7d_2 ]
  %R99 = phi i64 [ %R86, %subblock_400c7d_2 ]
  %R98 = phi i64 [ %R85, %subblock_400c7d_2 ]
  %R97 = phi i64 [ %R84, %subblock_400c7d_2 ]
  %R96 = phi i64 [ %R83, %subblock_400c7d_2 ]
  %R95 = phi i64 [ %R82, %subblock_400c7d_2 ]
  %R94 = phi i64 [ %R81, %subblock_400c7d_2 ]
  %R93 = phi i64 [ %R80, %subblock_400c7d_2 ]
  ; # 400c88: mov    rax,rbx
  br label %block_400c8b
block_400c8b:
  %R111 = phi i128 [ %R101, %block_400c88 ], [ %R367, %block_400e24 ]
  %R110 = phi i64 [ %R100, %block_400c88 ], [ %R366, %block_400e24 ]
  %R109 = phi i64 [ %R99, %block_400c88 ], [ %R365, %block_400e24 ]
  %R108 = phi i64 [ %R98, %block_400c88 ], [ %R364, %block_400e24 ]
  %R107 = phi i64 [ %R97, %block_400c88 ], [ %R363, %block_400e24 ]
  %R106 = phi i64 [ %R96, %block_400c88 ], [ %R362, %block_400e24 ]
  %R105 = phi i64 [ %R95, %block_400c88 ], [ %R361, %block_400e24 ]
  %R104 = phi i64 [ %R94, %block_400c88 ], [ %R360, %block_400e24 ]
  %R103 = phi i64 [ %R93, %block_400c88 ], [ %R359, %block_400e24 ]
  %R102 = phi i64 [ %R93, %block_400c88 ], [ %R370, %block_400e24 ]
  ; # 400c8b: lea    rdx,[r12+r12*2]
  ; r112 := (bv_mul 0x2 :: [64] r108)
  %R112 = mul i64 2, %R108
  ; r113 := (bv_add r108 r112)
  %R113 = add i64 %R108, %R112
  ; # 400c8f: lea    rdx,[rdx*8+0x6040e0]
  ; r114 := (bv_mul 0x8 :: [64] r113)
  %R114 = mul i64 8, %R113
  ; r115 := (bv_add r114 0x6040e0 :: [64])
  %R115 = add i64 %R114, 6308064
  ; # 400c97: mov    QWORD PTR [rdx+0x18],rdx
  ; r116 := (bv_add r114 0x6040f8 :: [64])
  %R116 = add i64 %R114, 6308088
  ; *(r116) = r115
  %r127 = inttoptr i64 %R116 to i64*
  store i64 %R115, i64* %r127
  ; # 400c9b: mov    QWORD PTR [rdx+0x10],rdx
  ; r117 := (bv_add r114 0x6040f0 :: [64])
  %R117 = add i64 %R114, 6308080
  ; *(r117) = r115
  %r129 = inttoptr i64 %R117 to i64*
  store i64 %R115, i64* %r129
  br label %block_400c9f
block_400c9f:
  %R126 = phi i128 [ %R111, %block_400c8b ], [ %R258, %block_400d62 ]
  %R125 = phi i64 [ %R110, %block_400c8b ], [ %R257, %block_400d62 ]
  %R124 = phi i64 [ %R109, %block_400c8b ], [ %R256, %block_400d62 ]
  %R123 = phi i64 [ %R107, %block_400c8b ], [ %R255, %block_400d62 ]
  %R122 = phi i64 [ %R106, %block_400c8b ], [ %R254, %block_400d62 ]
  %R121 = phi i64 [ %R105, %block_400c8b ], [ %R253, %block_400d62 ]
  %R120 = phi i64 [ %R104, %block_400c8b ], [ %R252, %block_400d62 ]
  %R119 = phi i64 [ %R103, %block_400c8b ], [ %R251, %block_400d62 ]
  %R118 = phi i64 [ %R102, %block_400c8b ], [ %R261, %block_400d62 ]
  ; # 400c9f: cmp    rbx,rax
  ; r127 := (bv_eq r119 r118)
  %R127 = icmp eq i64 %R119, %R118
  ; # 400ca2: je     400d70
  br i1 %R127, label %subblock_400c9f_1, label %subblock_400c9f_2
subblock_400c9f_1:
  br label %block_400d70
subblock_400c9f_2:
  br label %block_400ca8
block_400ca8:
  %R135 = phi i128 [ %R126, %subblock_400c9f_2 ]
  %R134 = phi i64 [ %R125, %subblock_400c9f_2 ]
  %R133 = phi i64 [ %R124, %subblock_400c9f_2 ]
  %R132 = phi i64 [ %R123, %subblock_400c9f_2 ]
  %R131 = phi i64 [ %R122, %subblock_400c9f_2 ]
  %R130 = phi i64 [ %R121, %subblock_400c9f_2 ]
  %R129 = phi i64 [ %R120, %subblock_400c9f_2 ]
  %R128 = phi i64 [ %R118, %subblock_400c9f_2 ]
  ; # 400ca8: mov    edx,DWORD PTR [rbp+0x8]
  ; r136 := (bv_add r130 0x8 :: [64])
  %R136 = add i64 %R130, 8
  ; r137 := *r136
  %r149 = inttoptr i64 %R136 to i32*
  %R137 = load i32* %r149
  ; # 400cab: test   edx,edx
  ; r138 := (bv_eq r137 0x0 :: [32])
  %R138 = icmp eq i32 %R137, 0
  ; # 400cad: je     400cf8
  br i1 %R138, label %subblock_400ca8_1, label %subblock_400ca8_2
subblock_400ca8_1:
  br label %block_400cf8
subblock_400ca8_2:
  br label %block_400caf
block_400caf:
  %R144 = phi i128 [ %R135, %subblock_400ca8_2 ]
  %R143 = phi i64 [ %R134, %subblock_400ca8_2 ]
  %R142 = phi i64 [ %R133, %subblock_400ca8_2 ]
  %R141 = phi i64 [ %R132, %subblock_400ca8_2 ]
  %R140 = phi i64 [ %R131, %subblock_400ca8_2 ]
  %R139 = phi i64 [ %R129, %subblock_400ca8_2 ]
  ; # 400caf: xor    eax,eax
  ; # 400cb1: mov    DWORD PTR [r15],eax
  ; *(r143) = 0x0 :: [32]
  %r158 = inttoptr i64 %R143 to i32*
  store i32 0, i32* %r158
  ; # 400cb4: lock or DWORD PTR [rsp],0x0
  ; r145 := *r139
  %r159 = inttoptr i64 %R139 to i32*
  %R145 = load i32* %r159
  ; *(r139) = r145
  %r161 = inttoptr i64 %R139 to i32*
  store i32 %R145, i32* %r161
  ; # 400cb9: mov    eax,DWORD PTR [r15+0x4]
  ; r146 := (bv_add r143 0x4 :: [64])
  %R146 = add i64 %R143, 4
  ; r147 := *r146
  %r163 = inttoptr i64 %R146 to i32*
  %R147 = load i32* %r163
  ; # 400cbd: test   eax,eax
  ; r148 := (bv_eq r147 0x0 :: [32])
  %R148 = icmp eq i32 %R147, 0
  ; # 400cbf: je     400c10
  br i1 %R148, label %subblock_400caf_1, label %subblock_400caf_2
subblock_400caf_1:
  br label %block_400c10
subblock_400caf_2:
  br label %block_400cc5
block_400cc5:
  %R153 = phi i64 [ %R143, %subblock_400caf_2 ]
  %R152 = phi i64 [ %R142, %subblock_400caf_2 ]
  %R151 = phi i64 [ %R141, %subblock_400caf_2 ]
  %R150 = phi i64 [ %R140, %subblock_400caf_2 ]
  %R149 = phi i64 [ %R139, %subblock_400caf_2 ]
  ; # 400cc5: mov    r9d,0xca
  ; # 400ccb: mov    edx,0x1
  ; # 400cd0: mov    esi,0x81
  ; # 400cd5: mov    rax,r9
  ; # 400cd8: mov    rdi,r15
  ; # 400cdb: syscall
  ; sys_futex
  %r171 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R153, i64 129, i64 1, i64 %R151, i64 %R150, i64 202, i64 202)
  %R154 = extractvalue { i64, i1 } %r171, 0
  br label %block_400cdd
block_400cdd:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R163 = phi i128 [ undef, %block_400cc5 ]
  %R162 = phi i64 [ %R152, %block_400cc5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R161 = phi i64 [ undef, %block_400cc5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R160 = phi i64 [ undef, %block_400cc5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R159 = phi i64 [ undef, %block_400cc5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R158 = phi i64 [ undef, %block_400cc5 ]
  %R157 = phi i64 [ %R149, %block_400cc5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R156 = phi i64 [ undef, %block_400cc5 ]
  %R155 = phi i64 [ %R154, %block_400cc5 ]
  ; # 400cdd: cmp    rax,0xffffffffffffffda
  ; r164 := (bv_eq r155 0xffffffffffffffda :: [64])
  %R164 = icmp eq i64 %R155, 18446744073709551578
  ; # 400ce1: jne    400c10
  ; r165 := (bv_complement r164)
  %R165 = xor i1 %R164, -1
  br i1 %R165, label %subblock_400cdd_1, label %subblock_400cdd_2
subblock_400cdd_1:
  br label %block_400c10
subblock_400cdd_2:
  br label %block_400ce7
block_400ce7:
  %R172 = phi i64 [ %R162, %subblock_400cdd_2 ]
  %R171 = phi i64 [ %R161, %subblock_400cdd_2 ]
  %R170 = phi i64 [ %R160, %subblock_400cdd_2 ]
  %R169 = phi i64 [ %R159, %subblock_400cdd_2 ]
  %R168 = phi i64 [ %R158, %subblock_400cdd_2 ]
  %R167 = phi i64 [ %R157, %subblock_400cdd_2 ]
  %R166 = phi i64 [ %R156, %subblock_400cdd_2 ]
  ; # 400ce7: mov    rax,r9
  ; # 400cea: mov    rsi,rdx
  ; # 400ced: syscall
  ; sys_futex
  %r191 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R168, i64 %R166, i64 %R166, i64 %R171, i64 %R169, i64 %R170, i64 202)
  %R173 = extractvalue { i64, i1 } %r191, 0
  br label %block_400cef
block_400cef:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R178 = phi i128 [ undef, %block_400ce7 ]
  %R177 = phi i64 [ %R172, %block_400ce7 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R176 = phi i64 [ undef, %block_400ce7 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R175 = phi i64 [ undef, %block_400ce7 ]
  %R174 = phi i64 [ %R167, %block_400ce7 ]
  ; # 400cef: jmp    400c10
  br label %block_400c10
block_400cf8:
  %R184 = phi i128 [ %R135, %subblock_400ca8_1 ]
  %R183 = phi i64 [ %R133, %subblock_400ca8_1 ]
  %R182 = phi i64 [ %R132, %subblock_400ca8_1 ]
  %R181 = phi i64 [ %R131, %subblock_400ca8_1 ]
  %R180 = phi i64 [ %R129, %subblock_400ca8_1 ]
  %R179 = phi i64 [ %R128, %subblock_400ca8_1 ]
  ; # 400cf8: mov    rbx,rax
  ; # 400cfb: test   bl,0x1
  ; r185 := (trunc r179 8)
  %R185 = trunc i64 %R179 to i8
  ; r186 := (bv_and r185 0x1 :: [8])
  %R186 = and i8 %R185, 1
  ; r187 := (bv_eq r186 0x0 :: [8])
  %R187 = icmp eq i8 %R186, 0
  ; # 400cfe: je     400c21
  br i1 %R187, label %subblock_400cf8_1, label %subblock_400cf8_2
subblock_400cf8_1:
  br label %block_400c21
subblock_400cf8_2:
  br label %block_400d04
block_400d04:
  %R188 = phi i128 [ %R184, %subblock_400cf8_2 ], [ %R14, %subblock_400c10_1 ]
  ; # 400d04: add    rsp,0x18
  ; # 400d08: xor    eax,eax
  ; # 400d0a: pop    rbx
  ; # 400d0b: pop    rbp
  ; # 400d0c: pop    r12
  ; # 400d0e: pop    r13
  ; # 400d10: pop    r14
  ; # 400d12: pop    r15
  ; # 400d14: ret
  %r208 = bitcast i128 %R188 to <2 x double>
  %r209 = insertvalue { i64, i64, <2 x double> } undef, i64 0, 0
  %r210 = insertvalue { i64, i64, <2 x double> } %r209, i64 undef, 1
  %r211 = insertvalue { i64, i64, <2 x double> } %r210, <2 x double> %r208, 2
  ret { i64, i64, <2 x double> } %r211
block_400d18:
  %R197 = phi i128 [ %R68, %subblock_400c5a_1 ]
  %R196 = phi i64 [ %R77, %subblock_400c5a_1 ]
  %R195 = phi i64 [ %R67, %subblock_400c5a_1 ]
  %R194 = phi i64 [ %R71, %subblock_400c5a_1 ]
  %R193 = phi i64 [ %R66, %subblock_400c5a_1 ]
  %R192 = phi i64 [ %R65, %subblock_400c5a_1 ]
  %R191 = phi i64 [ %R75, %subblock_400c5a_1 ]
  %R190 = phi i64 [ %R64, %subblock_400c5a_1 ]
  %R189 = phi i64 [ %R63, %subblock_400c5a_1 ]
  ; # 400d18: mov    eax,r13d
  ; r198 := (trunc r195 32)
  %R198 = trunc i64 %R195 to i32
  ; # 400d1b: xchg   DWORD PTR [rbp+0x8],eax
  ; r199 := (bv_add r191 0x8 :: [64])
  %R199 = add i64 %R191, 8
  ; r200 := *r199
  %r223 = inttoptr i64 %R199 to i32*
  %R200 = load i32* %r223
  ; *(r199) = r198
  %r225 = inttoptr i64 %R199 to i32*
  store i32 %R198, i32* %r225
  ; # 400d1e: test   eax,eax
  ; r201 := (bv_eq r200 0x0 :: [32])
  %R201 = icmp eq i32 %R200, 0
  ; # 400d20: lea    r14,[rbp+0xc]
  ; r202 := (bv_add r191 0xc :: [64])
  %R202 = add i64 %R191, 12
  ; # 400d24: je     400d4f
  br i1 %R201, label %subblock_400d18_1, label %subblock_400d18_2
subblock_400d18_1:
  br label %block_400d4f
subblock_400d18_2:
  br label %block_400d26
block_400d26:
  %R210 = phi i128 [ %R197, %subblock_400d18_2 ]
  %R209 = phi i64 [ %R196, %subblock_400d18_2 ]
  %R208 = phi i64 [ %R202, %subblock_400d18_2 ]
  %R207 = phi i64 [ %R195, %subblock_400d18_2 ]
  %R206 = phi i64 [ %R194, %subblock_400d18_2 ]
  %R205 = phi i64 [ %R191, %subblock_400d18_2 ]
  %R204 = phi i64 [ %R190, %subblock_400d18_2 ]
  %R203 = phi i64 [ %R189, %subblock_400d18_2 ]
  ; # 400d26: nop    [rax+rax*1]
  br label %block_400d30
block_400d30:
  %R218 = phi i128 [ %R231, %subblock_400d45_1 ], [ %R210, %block_400d26 ]
  %R217 = phi i64 [ %R230, %subblock_400d45_1 ], [ %R209, %block_400d26 ]
  %R216 = phi i64 [ %R229, %subblock_400d45_1 ], [ %R208, %block_400d26 ]
  %R215 = phi i64 [ %R228, %subblock_400d45_1 ], [ %R207, %block_400d26 ]
  %R214 = phi i64 [ %R227, %subblock_400d45_1 ], [ %R206, %block_400d26 ]
  %R213 = phi i64 [ %R224, %subblock_400d45_1 ], [ %R205, %block_400d26 ]
  %R212 = phi i64 [ %R223, %subblock_400d45_1 ], [ %R204, %block_400d26 ]
  %R211 = phi i64 [ %R222, %subblock_400d45_1 ], [ %R203, %block_400d26 ]
  ; # 400d30: mov    ecx,0x1
  ; # 400d35: mov    edx,0x1
  ; # 400d3a: mov    rsi,r14
  ; # 400d3d: mov    rdi,r15
  ; # 400d40: call   40241c
  ; r219 := (bv_add r212 0xfffffffffffffff8 :: [64])
  %R219 = add i64 %R212, 18446744073709551608
  ; r221 := (bv_add r219 0x8 :: [64])
  %R221 = add i64 %R219, 8
  %r246 = bitcast i128 %R218 to <2 x double>
  %r247 = call { i64, i64, <2 x double> } @F40241c(i64 %R217, i64 %R216, i64 1, i64 1, <2 x double> %r246)
  %r248 = extractvalue { i64, i64, <2 x double> } %r247, 2
  %R220 = bitcast <2 x double> %r248 to i128
  br label %block_400d45
block_400d45:
  %R231 = phi i128 [ %R220, %block_400d30 ]
  %R230 = phi i64 [ %R217, %block_400d30 ]
  %R229 = phi i64 [ %R216, %block_400d30 ]
  %R228 = phi i64 [ %R215, %block_400d30 ]
  %R227 = phi i64 [ %R214, %block_400d30 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R226 = phi i64 [ undef, %block_400d30 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R225 = phi i64 [ undef, %block_400d30 ]
  %R224 = phi i64 [ %R213, %block_400d30 ]
  %R223 = phi i64 [ %R221, %block_400d30 ]
  %R222 = phi i64 [ %R211, %block_400d30 ]
  ; # 400d45: mov    ecx,r13d
  ; r232 := (trunc r228 32)
  %R232 = trunc i64 %R228 to i32
  ; # 400d48: xchg   DWORD PTR [r15],ecx
  ; r233 := *r230
  %r261 = inttoptr i64 %R230 to i32*
  %R233 = load i32* %r261
  ; *(r230) = r232
  %r263 = inttoptr i64 %R230 to i32*
  store i32 %R232, i32* %r263
  ; # 400d4b: test   ecx,ecx
  ; r234 := (bv_eq r233 0x0 :: [32])
  %R234 = icmp eq i32 %R233, 0
  ; # 400d4d: jne    400d30
  ; r235 := (bv_complement r234)
  %R235 = xor i1 %R234, -1
  br i1 %R235, label %subblock_400d45_1, label %subblock_400d45_2
subblock_400d45_1:
  br label %block_400d30
subblock_400d45_2:
  br label %block_400d4f
block_400d4f:
  %R244 = phi i128 [ %R231, %subblock_400d45_2 ], [ %R197, %subblock_400d18_1 ]
  %R243 = phi i64 [ %R230, %subblock_400d45_2 ], [ %R196, %subblock_400d18_1 ]
  %R242 = phi i64 [ %R228, %subblock_400d45_2 ], [ %R195, %subblock_400d18_1 ]
  %R241 = phi i64 [ %R227, %subblock_400d45_2 ], [ %R194, %subblock_400d18_1 ]
  %R240 = phi i64 [ %R226, %subblock_400d45_2 ], [ %R193, %subblock_400d18_1 ]
  %R239 = phi i64 [ %R225, %subblock_400d45_2 ], [ %R192, %subblock_400d18_1 ]
  %R238 = phi i64 [ %R224, %subblock_400d45_2 ], [ %R191, %subblock_400d18_1 ]
  %R237 = phi i64 [ %R223, %subblock_400d45_2 ], [ %R190, %subblock_400d18_1 ]
  %R236 = phi i64 [ %R222, %subblock_400d45_2 ], [ %R189, %subblock_400d18_1 ]
  ; # 400d4f: lea    rax,[r12+r12*2]
  ; r245 := (bv_mul 0x2 :: [64] r241)
  %R245 = mul i64 2, %R241
  ; r246 := (bv_add r241 r245)
  %R246 = add i64 %R241, %R245
  ; # 400d53: cmp    QWORD PTR [rax*8+0x6040f0],0x0
  ; r247 := (bv_mul 0x8 :: [64] r246)
  %R247 = mul i64 8, %R246
  ; r248 := (bv_add r247 0x6040f0 :: [64])
  %R248 = add i64 %R247, 6308080
  ; r249 := *r248
  %r279 = inttoptr i64 %R248 to i64*
  %R249 = load i64* %r279
  ; r250 := (bv_eq r249 0x0 :: [64])
  %R250 = icmp eq i64 %R249, 0
  ; # 400d5c: je     400e24
  br i1 %R250, label %subblock_400d4f_1, label %subblock_400d4f_2
subblock_400d4f_1:
  br label %block_400e24
subblock_400d4f_2:
  br label %block_400d62
block_400d62:
  %R258 = phi i128 [ %R244, %subblock_400d4f_2 ]
  %R257 = phi i64 [ %R243, %subblock_400d4f_2 ]
  %R256 = phi i64 [ %R242, %subblock_400d4f_2 ]
  %R255 = phi i64 [ %R240, %subblock_400d4f_2 ]
  %R254 = phi i64 [ %R239, %subblock_400d4f_2 ]
  %R253 = phi i64 [ %R238, %subblock_400d4f_2 ]
  %R252 = phi i64 [ %R237, %subblock_400d4f_2 ]
  %R251 = phi i64 [ %R236, %subblock_400d4f_2 ]
  ; # 400d62: mov    rax,QWORD PTR [rsp+0x8]
  ; r259 := (bv_add r252 0x8 :: [64])
  %R259 = add i64 %R252, 8
  ; r260 := *r259
  %r291 = inttoptr i64 %R259 to i64*
  %R260 = load i64* %r291
  ; # 400d67: mov    rax,QWORD PTR [rax]
  ; r261 := *r260
  %r293 = inttoptr i64 %R260 to i64*
  %R261 = load i64* %r293
  ; # 400d6a: jmp    400c9f
  br label %block_400c9f
block_400d70:
  %R267 = phi i128 [ %R88, %subblock_400c7d_1 ], [ %R126, %subblock_400c9f_1 ]
  %R266 = phi i64 [ %R87, %subblock_400c7d_1 ], [ %R125, %subblock_400c9f_1 ]
  %R265 = phi i64 [ %R84, %subblock_400c7d_1 ], [ %R123, %subblock_400c9f_1 ]
  %R264 = phi i64 [ %R83, %subblock_400c7d_1 ], [ %R122, %subblock_400c9f_1 ]
  %R263 = phi i64 [ %R81, %subblock_400c7d_1 ], [ %R120, %subblock_400c9f_1 ]
  %R262 = phi i64 [ %R80, %subblock_400c7d_1 ], [ %R119, %subblock_400c9f_1 ]
  ; # 400d70: mov    rax,QWORD PTR [rsp+0x8]
  ; r268 := (bv_add r263 0x8 :: [64])
  %R268 = add i64 %R263, 8
  ; r269 := *r268
  %r302 = inttoptr i64 %R268 to i64*
  %R269 = load i64* %r302
  ; # 400d75: and    rbx,0xfffffffffffffffe
  ; r270 := (bv_and r262 0xfffffffffffffffe :: [64])
  %R270 = and i64 %R262, 18446744073709551614
  ; # 400d79: sub    rax,rbx
  ; r271 := (bv_sub r269 r270)
  %R271 = sub i64 %R269, %R270
  ; # 400d7c: mov    rdx,QWORD PTR [rax+0x18]
  ; r272 := (bv_add r271 0x18 :: [64])
  %R272 = add i64 %R271, 24
  ; r273 := *r272
  %r307 = inttoptr i64 %R272 to i64*
  %R273 = load i64* %r307
  ; # 400d80: mov    rcx,QWORD PTR [rax+0x10]
  ; r274 := (bv_add r271 0x10 :: [64])
  %R274 = add i64 %R271, 16
  ; r275 := *r274
  %r310 = inttoptr i64 %R274 to i64*
  %R275 = load i64* %r310
  ; # 400d84: cmp    rdx,rcx
  ; r276 := (bv_eq r273 r275)
  %R276 = icmp eq i64 %R273, %R275
  ; # 400d87: je     400e00
  br i1 %R276, label %subblock_400d70_1, label %subblock_400d70_2
subblock_400d70_1:
  br label %block_400e00
subblock_400d70_2:
  br label %block_400d89
block_400d89:
  %R284 = phi i128 [ %R352, %block_400e0f ], [ %R267, %subblock_400d70_2 ]
  %R283 = phi i64 [ %R351, %block_400e0f ], [ %R266, %subblock_400d70_2 ]
  %R282 = phi i64 [ %R350, %block_400e0f ], [ %R265, %subblock_400d70_2 ]
  %R281 = phi i64 [ %R349, %block_400e0f ], [ %R264, %subblock_400d70_2 ]
  %R280 = phi i64 [ %R348, %block_400e0f ], [ %R263, %subblock_400d70_2 ]
  %R279 = phi i64 [ %R356, %block_400e0f ], [ %R273, %subblock_400d70_2 ]
  %R278 = phi i64 [ %R358, %block_400e0f ], [ %R275, %subblock_400d70_2 ]
  %R277 = phi i64 [ %R346, %block_400e0f ], [ %R271, %subblock_400d70_2 ]
  ; # 400d89: mov    QWORD PTR [rdx+0x10],rcx
  ; r285 := (bv_add r279 0x10 :: [64])
  %R285 = add i64 %R279, 16
  ; *(r285) = r278
  %r322 = inttoptr i64 %R285 to i64*
  store i64 %R278, i64* %r322
  ; # 400d8d: mov    rcx,QWORD PTR [rax+0x10]
  ; r286 := (bv_add r277 0x10 :: [64])
  %R286 = add i64 %R277, 16
  ; r287 := *r286
  %r324 = inttoptr i64 %R286 to i64*
  %R287 = load i64* %r324
  ; # 400d91: mov    QWORD PTR [rcx+0x18],rdx
  ; r288 := (bv_add r287 0x18 :: [64])
  %R288 = add i64 %R287, 24
  ; *(r288) = r279
  %r327 = inttoptr i64 %R288 to i64*
  store i64 %R279, i64* %r327
  ; # 400d95: mov    rdx,QWORD PTR [rax+0x8]
  ; r289 := (bv_add r277 0x8 :: [64])
  %R289 = add i64 %R277, 8
  ; r290 := *r289
  %r329 = inttoptr i64 %R289 to i64*
  %R290 = load i64* %r329
  ; # 400d99: mov    rcx,rdx
  ; # 400d9c: and    rdx,0xfffffffffffffffe
  ; r291 := (bv_and r290 0xfffffffffffffffe :: [64])
  %R291 = and i64 %R290, 18446744073709551614
  ; # 400da0: or     rcx,0x1
  ; r292 := (bv_or r290 0x1 :: [64])
  %R292 = or i64 %R290, 1
  ; # 400da4: mov    QWORD PTR [rax+0x8],rcx
  ; *(r289) = r292
  %r333 = inttoptr i64 %R289 to i64*
  store i64 %R292, i64* %r333
  ; # 400da8: or     QWORD PTR [rax+rdx*1],0x1
  ; r293 := (bv_add r277 r291)
  %R293 = add i64 %R277, %R291
  ; r294 := *r293
  %r335 = inttoptr i64 %R293 to i64*
  %R294 = load i64* %r335
  ; r295 := (bv_or r294 0x1 :: [64])
  %R295 = or i64 %R294, 1
  ; *(r293) = r295
  %r338 = inttoptr i64 %R293 to i64*
  store i64 %R295, i64* %r338
  ; # 400dad: mov    eax,DWORD PTR [r15]
  ; r296 := *r283
  %r339 = inttoptr i64 %R283 to i32*
  %R296 = load i32* %r339
  ; # 400db0: test   eax,eax
  ; r297 := (bv_eq r296 0x0 :: [32])
  %R297 = icmp eq i32 %R296, 0
  ; # 400db2: je     400dec
  br i1 %R297, label %subblock_400d89_1, label %subblock_400d89_2
subblock_400d89_1:
  br label %block_400dec
subblock_400d89_2:
  br label %block_400db4
block_400db4:
  %R302 = phi i128 [ %R284, %subblock_400d89_2 ]
  %R301 = phi i64 [ %R283, %subblock_400d89_2 ]
  %R300 = phi i64 [ %R282, %subblock_400d89_2 ]
  %R299 = phi i64 [ %R281, %subblock_400d89_2 ]
  %R298 = phi i64 [ %R280, %subblock_400d89_2 ]
  ; # 400db4: xor    eax,eax
  ; # 400db6: mov    DWORD PTR [r15],eax
  ; *(r301) = 0x0 :: [32]
  %r347 = inttoptr i64 %R301 to i32*
  store i32 0, i32* %r347
  ; # 400db9: lock or DWORD PTR [rsp],0x0
  ; r303 := *r298
  %r348 = inttoptr i64 %R298 to i32*
  %R303 = load i32* %r348
  ; *(r298) = r303
  %r350 = inttoptr i64 %R298 to i32*
  store i32 %R303, i32* %r350
  ; # 400dbe: mov    eax,DWORD PTR [r15+0x4]
  ; r304 := (bv_add r301 0x4 :: [64])
  %R304 = add i64 %R301, 4
  ; r305 := *r304
  %r352 = inttoptr i64 %R304 to i32*
  %R305 = load i32* %r352
  ; # 400dc2: test   eax,eax
  ; r306 := (bv_eq r305 0x0 :: [32])
  %R306 = icmp eq i32 %R305, 0
  ; # 400dc4: je     400dec
  br i1 %R306, label %subblock_400db4_1, label %subblock_400db4_2
subblock_400db4_1:
  br label %block_400dec
subblock_400db4_2:
  br label %block_400dc6
block_400dc6:
  %R309 = phi i64 [ %R301, %subblock_400db4_2 ]
  %R308 = phi i64 [ %R300, %subblock_400db4_2 ]
  %R307 = phi i64 [ %R299, %subblock_400db4_2 ]
  ; # 400dc6: mov    r9d,0xca
  ; # 400dcc: mov    edx,0x1
  ; # 400dd1: mov    esi,0x81
  ; # 400dd6: mov    rax,r9
  ; # 400dd9: mov    rdi,r15
  ; # 400ddc: syscall
  ; sys_futex
  %r358 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R309, i64 129, i64 1, i64 %R308, i64 %R307, i64 202, i64 202)
  %R310 = extractvalue { i64, i1 } %r358, 0
  br label %block_400dde
block_400dde:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R317 = phi i128 [ undef, %block_400dc6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R316 = phi i64 [ undef, %block_400dc6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R315 = phi i64 [ undef, %block_400dc6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R314 = phi i64 [ undef, %block_400dc6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R313 = phi i64 [ undef, %block_400dc6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R312 = phi i64 [ undef, %block_400dc6 ]
  %R311 = phi i64 [ %R310, %block_400dc6 ]
  ; # 400dde: cmp    rax,0xffffffffffffffda
  ; r318 := (bv_eq r311 0xffffffffffffffda :: [64])
  %R318 = icmp eq i64 %R311, 18446744073709551578
  ; # 400de2: jne    400dec
  ; r319 := (bv_complement r318)
  %R319 = xor i1 %R318, -1
  br i1 %R319, label %subblock_400dde_1, label %subblock_400dde_2
subblock_400dde_1:
  br label %block_400dec
subblock_400dde_2:
  br label %block_400de4
block_400de4:
  %R324 = phi i64 [ %R316, %subblock_400dde_2 ]
  %R323 = phi i64 [ %R315, %subblock_400dde_2 ]
  %R322 = phi i64 [ %R314, %subblock_400dde_2 ]
  %R321 = phi i64 [ %R313, %subblock_400dde_2 ]
  %R320 = phi i64 [ %R312, %subblock_400dde_2 ]
  ; # 400de4: mov    rax,r9
  ; # 400de7: mov    rsi,rdx
  ; # 400dea: syscall
  ; sys_futex
  %r374 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R321, i64 %R320, i64 %R320, i64 %R324, i64 %R322, i64 %R323, i64 202)
  %R325 = extractvalue { i64, i1 } %r374, 0
  br label %block_400dec
block_400dec:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R326 = phi i128 [ undef, %block_400de4 ], [ %R317, %subblock_400dde_1 ], [ %R302, %subblock_400db4_1 ], [ %R284, %subblock_400d89_1 ]
  ; # 400dec: add    rsp,0x18
  ; # 400df0: mov    eax,0x1
  ; # 400df5: pop    rbx
  ; # 400df6: pop    rbp
  ; # 400df7: pop    r12
  ; # 400df9: pop    r13
  ; # 400dfb: pop    r14
  ; # 400dfd: pop    r15
  ; # 400dff: ret
  %r377 = bitcast i128 %R326 to <2 x double>
  %r378 = insertvalue { i64, i64, <2 x double> } undef, i64 1, 0
  %r379 = insertvalue { i64, i64, <2 x double> } %r378, i64 undef, 1
  %r380 = insertvalue { i64, i64, <2 x double> } %r379, <2 x double> %r377, 2
  ret { i64, i64, <2 x double> } %r380
block_400e00:
  %R332 = phi i128 [ %R267, %subblock_400d70_1 ]
  %R331 = phi i64 [ %R266, %subblock_400d70_1 ]
  %R330 = phi i64 [ %R265, %subblock_400d70_1 ]
  %R329 = phi i64 [ %R264, %subblock_400d70_1 ]
  %R328 = phi i64 [ %R263, %subblock_400d70_1 ]
  %R327 = phi i64 [ %R271, %subblock_400d70_1 ]
  ; # 400e00: movzx  ecx,BYTE PTR [rsp+0x4]
  ; r333 := (bv_add r328 0x4 :: [64])
  %R333 = add i64 %R328, 4
  ; r334 := *r333
  %r388 = inttoptr i64 %R333 to i8*
  %R334 = load i8* %r388
  ; r335 := (uext r334 64)
  %R335 = zext i8 %R334 to i64
  ; # 400e05: mov    rdx,0xfffffffffffffffe
  ; # 400e0c: rol    rdx,cl
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
  br i1 %R342, label %subblock_400e00_1, label %subblock_400e00_4
subblock_400e00_1:
  ; r343 := (uext r334 64)
  %R343 = zext i8 %R334 to i64
  ; r344 := (bv_and r343 0x3f :: [64])
  %R344 = and i64 %R343, 63
  ; r345 := (bv_eq r344 0x1 :: [64])
  %R345 = icmp eq i64 %R344, 1
  br i1 %R345, label %subblock_400e00_2, label %subblock_400e00_3
subblock_400e00_2:
  br label %block_400e0f
subblock_400e00_3:
  br label %block_400e0f
subblock_400e00_4:
  br label %block_400e0f
block_400e0f:
  %R352 = phi i128 [ %R332, %subblock_400e00_4 ], [ %R332, %subblock_400e00_3 ], [ %R332, %subblock_400e00_2 ]
  %R351 = phi i64 [ %R331, %subblock_400e00_4 ], [ %R331, %subblock_400e00_3 ], [ %R331, %subblock_400e00_2 ]
  %R350 = phi i64 [ %R330, %subblock_400e00_4 ], [ %R330, %subblock_400e00_3 ], [ %R330, %subblock_400e00_2 ]
  %R349 = phi i64 [ %R329, %subblock_400e00_4 ], [ %R329, %subblock_400e00_3 ], [ %R329, %subblock_400e00_2 ]
  %R348 = phi i64 [ %R328, %subblock_400e00_4 ], [ %R328, %subblock_400e00_3 ], [ %R328, %subblock_400e00_2 ]
  %R347 = phi i64 [ %R340, %subblock_400e00_4 ], [ %R340, %subblock_400e00_3 ], [ %R340, %subblock_400e00_2 ]
  %R346 = phi i64 [ %R327, %subblock_400e00_4 ], [ %R327, %subblock_400e00_3 ], [ %R327, %subblock_400e00_2 ]
  ; # 400e0f: lock and QWORD PTR [rip+0x2032c9],rdx
  ; r353 := *0x6040e0 :: [64]
  %r408 = inttoptr i64 6308064 to i64*
  %R353 = load i64* %r408
  ; r354 := (bv_and r353 r347)
  %R354 = and i64 %R353, %R347
  ; *(0x6040e0 :: [64]) = r354
  %r411 = inttoptr i64 6308064 to i64*
  store i64 %R354, i64* %r411
  ; # 400e17: mov    rdx,QWORD PTR [rax+0x18]
  ; r355 := (bv_add r346 0x18 :: [64])
  %R355 = add i64 %R346, 24
  ; r356 := *r355
  %r413 = inttoptr i64 %R355 to i64*
  %R356 = load i64* %r413
  ; # 400e1b: mov    rcx,QWORD PTR [rax+0x10]
  ; r357 := (bv_add r346 0x10 :: [64])
  %R357 = add i64 %R346, 16
  ; r358 := *r357
  %r416 = inttoptr i64 %R357 to i64*
  %R358 = load i64* %r416
  ; # 400e1f: jmp    400d89
  br label %block_400d89
block_400e24:
  %R367 = phi i128 [ %R244, %subblock_400d4f_1 ]
  %R366 = phi i64 [ %R243, %subblock_400d4f_1 ]
  %R365 = phi i64 [ %R242, %subblock_400d4f_1 ]
  %R364 = phi i64 [ %R241, %subblock_400d4f_1 ]
  %R363 = phi i64 [ %R240, %subblock_400d4f_1 ]
  %R362 = phi i64 [ %R239, %subblock_400d4f_1 ]
  %R361 = phi i64 [ %R238, %subblock_400d4f_1 ]
  %R360 = phi i64 [ %R237, %subblock_400d4f_1 ]
  %R359 = phi i64 [ %R236, %subblock_400d4f_1 ]
  ; # 400e24: mov    rax,QWORD PTR [rsp+0x8]
  ; r368 := (bv_add r360 0x8 :: [64])
  %R368 = add i64 %R360, 8
  ; r369 := *r368
  %r428 = inttoptr i64 %R368 to i64*
  %R369 = load i64* %r428
  ; # 400e29: mov    rax,QWORD PTR [rax]
  ; r370 := *r369
  %r430 = inttoptr i64 %R369 to i64*
  %R370 = load i64* %r430
  ; # 400e2c: jmp    400c8b
  br label %block_400c8b
failure:
  br label %failure
}