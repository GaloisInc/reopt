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
declare { i64, i64, <2 x double> } @F402ce8(i64, <2 x double>)
define { i64, i64, <2 x double> } @F402ac5(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_402ac5
block_402ac5:
  ; r0 := (alloca 0x20 :: [64])
  %r8 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 402ac5: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402ac6: mov    rbx,rdi
  ; # 402ac9: sub    rsp,0x10
  ; r3 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R3 = add i64 %R1, 18446744073709551592
  ; # 402acd: cmp    QWORD PTR [rdi+0x20],0x0
  ; r4 := (bv_add arg0 0x20 :: [64])
  %R4 = add i64 %a0, 32
  ; r5 := *r4
  %r14 = inttoptr i64 %R4 to i64*
  %R5 = load i64* %r14
  ; r6 := (bv_eq r5 0x0 :: [64])
  %R6 = icmp eq i64 %R5, 0
  ; # 402ad2: mov    BYTE PTR [rsp+0xf],sil
  ; r7 := (trunc arg1 8)
  %R7 = trunc i64 %a1 to i8
  ; r8 := (bv_add r1 0xfffffffffffffff7 :: [64])
  %R8 = add i64 %R1, 18446744073709551607
  ; *(r8) = r7
  %r19 = inttoptr i64 %R8 to i8*
  store i8 %R7, i8* %r19
  ; # 402ad7: je     402ae5
  br i1 %R6, label %subblock_402ac5_1, label %subblock_402ac5_2
subblock_402ac5_1:
  br label %block_402ae5
subblock_402ac5_2:
  br label %block_402ad9
block_402ad9:
  %R21 = phi i128 [ %R63, %subblock_402aea_1 ], [ %r7, %subblock_402ac5_2 ]
  %R20 = phi i128 [ %R62, %subblock_402aea_1 ], [ %r6, %subblock_402ac5_2 ]
  %R19 = phi i128 [ %R61, %subblock_402aea_1 ], [ %r5, %subblock_402ac5_2 ]
  %R18 = phi i128 [ %R60, %subblock_402aea_1 ], [ %r4, %subblock_402ac5_2 ]
  %R17 = phi i128 [ %R59, %subblock_402aea_1 ], [ %r3, %subblock_402ac5_2 ]
  %R16 = phi i128 [ %R58, %subblock_402aea_1 ], [ %r2, %subblock_402ac5_2 ]
  %R15 = phi i128 [ %R57, %subblock_402aea_1 ], [ %r1, %subblock_402ac5_2 ]
  %R14 = phi i128 [ %R56, %subblock_402aea_1 ], [ %r0, %subblock_402ac5_2 ]
  %R13 = phi i64 [ %R55, %subblock_402aea_1 ], [ %a5, %subblock_402ac5_2 ]
  %R12 = phi i64 [ %R54, %subblock_402aea_1 ], [ %a4, %subblock_402ac5_2 ]
  %R11 = phi i64 [ %R53, %subblock_402aea_1 ], [ %R3, %subblock_402ac5_2 ]
  %R10 = phi i64 [ %R52, %subblock_402aea_1 ], [ %a0, %subblock_402ac5_2 ]
  %R9 = phi i64 [ %R50, %subblock_402aea_1 ], [ %a3, %subblock_402ac5_2 ]
  ; # 402ad9: mov    rdx,QWORD PTR [rbx+0x28]
  ; r22 := (bv_add r10 0x28 :: [64])
  %R22 = add i64 %R10, 40
  ; r23 := *r22
  %r34 = inttoptr i64 %R22 to i64*
  %R23 = load i64* %r34
  ; # 402add: cmp    rdx,QWORD PTR [rbx+0x20]
  ; r24 := (bv_add r10 0x20 :: [64])
  %R24 = add i64 %R10, 32
  ; r25 := *r24
  %r37 = inttoptr i64 %R24 to i64*
  %R25 = load i64* %r37
  ; r26 := (bv_ult r23 r25)
  %R26 = icmp ult i64 %R23, %R25
  ; # 402ae1: jb     402af3
  br i1 %R26, label %subblock_402ad9_1, label %subblock_402ad9_2
subblock_402ad9_1:
  br label %block_402af3
subblock_402ad9_2:
  br label %block_402ae3
block_402ae3:
  %R39 = phi i128 [ %R21, %subblock_402ad9_2 ]
  %R38 = phi i128 [ %R20, %subblock_402ad9_2 ]
  %R37 = phi i128 [ %R19, %subblock_402ad9_2 ]
  %R36 = phi i128 [ %R18, %subblock_402ad9_2 ]
  %R35 = phi i128 [ %R17, %subblock_402ad9_2 ]
  %R34 = phi i128 [ %R16, %subblock_402ad9_2 ]
  %R33 = phi i128 [ %R15, %subblock_402ad9_2 ]
  %R32 = phi i128 [ %R14, %subblock_402ad9_2 ]
  %R31 = phi i64 [ %R13, %subblock_402ad9_2 ]
  %R30 = phi i64 [ %R12, %subblock_402ad9_2 ]
  %R29 = phi i64 [ %R11, %subblock_402ad9_2 ]
  %R28 = phi i64 [ %R10, %subblock_402ad9_2 ]
  %R27 = phi i64 [ %R9, %subblock_402ad9_2 ]
  ; # 402ae3: jmp    402b0f
  br label %block_402b0f
block_402ae5:
  %R43 = phi i128 [ %r0, %subblock_402ac5_1 ]
  %R42 = phi i64 [ %a0, %subblock_402ac5_1 ]
  %R41 = phi i64 [ %R3, %subblock_402ac5_1 ]
  %R40 = phi i64 [ %a0, %subblock_402ac5_1 ]
  ; # 402ae5: call   402ce8
  ; r44 := (bv_add r41 0xfffffffffffffff8 :: [64])
  %R44 = add i64 %R41, 18446744073709551608
  ; r48 := (bv_add r44 0x8 :: [64])
  %R48 = add i64 %R44, 8
  %r59 = bitcast i128 %R43 to <2 x double>
  %r60 = call { i64, i64, <2 x double> } @F402ce8(i64 %R42, <2 x double> %r59)
  %R45 = extractvalue { i64, i64, <2 x double> } %r60, 0
  %R46 = extractvalue { i64, i64, <2 x double> } %r60, 1
  %r63 = extractvalue { i64, i64, <2 x double> } %r60, 2
  %R47 = bitcast <2 x double> %r63 to i128
  br label %block_402aea
block_402aea:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R63 = phi i128 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R62 = phi i128 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R61 = phi i128 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R60 = phi i128 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R59 = phi i128 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R58 = phi i128 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R57 = phi i128 [ undef, %block_402ae5 ]
  %R56 = phi i128 [ %R47, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R55 = phi i64 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R54 = phi i64 [ undef, %block_402ae5 ]
  %R53 = phi i64 [ %R48, %block_402ae5 ]
  %R52 = phi i64 [ %R40, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R51 = phi i64 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R50 = phi i64 [ undef, %block_402ae5 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R49 = phi i64 [ undef, %block_402ae5 ]
  ; # 402aea: test   eax,eax
  ; r64 := (trunc r49 32)
  %R64 = trunc i64 %R49 to i32
  ; r65 := (bv_eq r64 0x0 :: [32])
  %R65 = icmp eq i32 %R64, 0
  ; # 402aec: je     402ad9
  br i1 %R65, label %subblock_402aea_1, label %subblock_402aea_2
subblock_402aea_1:
  br label %block_402ad9
subblock_402aea_2:
  br label %block_402aee
block_402aee:
  %R67 = phi i128 [ %R121, %subblock_402b1f_1 ], [ %R56, %subblock_402aea_2 ]
  %R66 = phi i64 [ %R119, %subblock_402b1f_1 ], [ %R51, %subblock_402aea_2 ]
  ; # 402aee: or     eax,0xffffffff
  ; # 402af1: jmp    402b29
  br label %block_402b29
block_402af3:
  %R81 = phi i128 [ %R21, %subblock_402ad9_1 ]
  %R80 = phi i128 [ %R20, %subblock_402ad9_1 ]
  %R79 = phi i128 [ %R19, %subblock_402ad9_1 ]
  %R78 = phi i128 [ %R18, %subblock_402ad9_1 ]
  %R77 = phi i128 [ %R17, %subblock_402ad9_1 ]
  %R76 = phi i128 [ %R16, %subblock_402ad9_1 ]
  %R75 = phi i128 [ %R15, %subblock_402ad9_1 ]
  %R74 = phi i128 [ %R14, %subblock_402ad9_1 ]
  %R73 = phi i64 [ %R13, %subblock_402ad9_1 ]
  %R72 = phi i64 [ %R12, %subblock_402ad9_1 ]
  %R71 = phi i64 [ %R11, %subblock_402ad9_1 ]
  %R70 = phi i64 [ %R10, %subblock_402ad9_1 ]
  %R69 = phi i64 [ %R23, %subblock_402ad9_1 ]
  %R68 = phi i64 [ %R9, %subblock_402ad9_1 ]
  ; # 402af3: movzx  eax,BYTE PTR [rsp+0xf]
  ; r82 := (bv_add r71 0xf :: [64])
  %R82 = add i64 %R71, 15
  ; r83 := *r82
  %r99 = inttoptr i64 %R82 to i8*
  %R83 = load i8* %r99
  ; r84 := (uext r83 32)
  %R84 = zext i8 %R83 to i32
  ; r85 := (uext r83 64)
  %R85 = zext i8 %R83 to i64
  ; # 402af8: movsx  esi,BYTE PTR [rbx+0x8b]
  ; r86 := (bv_add r70 0x8b :: [64])
  %R86 = add i64 %R70, 139
  ; r87 := *r86
  %r104 = inttoptr i64 %R86 to i8*
  %R87 = load i8* %r104
  ; r88 := (sext r87 32)
  %R88 = sext i8 %R87 to i32
  ; # 402aff: cmp    eax,esi
  ; r89 := (bv_eq r84 r88)
  %R89 = icmp eq i32 %R84, %R88
  ; # 402b01: je     402b0f
  br i1 %R89, label %subblock_402af3_1, label %subblock_402af3_2
subblock_402af3_1:
  br label %block_402b0f
subblock_402af3_2:
  br label %block_402b03
block_402b03:
  %R93 = phi i128 [ %R74, %subblock_402af3_2 ]
  %R92 = phi i64 [ %R70, %subblock_402af3_2 ]
  %R91 = phi i64 [ %R69, %subblock_402af3_2 ]
  %R90 = phi i64 [ %R85, %subblock_402af3_2 ]
  ; # 402b03: lea    rsi,[rdx+0x1]
  ; r94 := (bv_add r91 0x1 :: [64])
  %R94 = add i64 %R91, 1
  ; # 402b07: mov    QWORD PTR [rbx+0x28],rsi
  ; r95 := (bv_add r92 0x28 :: [64])
  %R95 = add i64 %R92, 40
  ; *(r95) = r94
  %r114 = inttoptr i64 %R95 to i64*
  store i64 %R94, i64* %r114
  ; # 402b0b: mov    BYTE PTR [rdx],al
  ; r96 := (trunc r90 8)
  %R96 = trunc i64 %R90 to i8
  ; *(r91) = r96
  %r116 = inttoptr i64 %R91 to i8*
  store i8 %R96, i8* %r116
  ; # 402b0d: jmp    402b29
  br label %block_402b29
block_402b0f:
  %R109 = phi i128 [ %R39, %block_402ae3 ], [ %R81, %subblock_402af3_1 ]
  %R108 = phi i128 [ %R38, %block_402ae3 ], [ %R80, %subblock_402af3_1 ]
  %R107 = phi i128 [ %R37, %block_402ae3 ], [ %R79, %subblock_402af3_1 ]
  %R106 = phi i128 [ %R36, %block_402ae3 ], [ %R78, %subblock_402af3_1 ]
  %R105 = phi i128 [ %R35, %block_402ae3 ], [ %R77, %subblock_402af3_1 ]
  %R104 = phi i128 [ %R34, %block_402ae3 ], [ %R76, %subblock_402af3_1 ]
  %R103 = phi i128 [ %R33, %block_402ae3 ], [ %R75, %subblock_402af3_1 ]
  %R102 = phi i128 [ %R32, %block_402ae3 ], [ %R74, %subblock_402af3_1 ]
  %R101 = phi i64 [ %R31, %block_402ae3 ], [ %R73, %subblock_402af3_1 ]
  %R100 = phi i64 [ %R30, %block_402ae3 ], [ %R72, %subblock_402af3_1 ]
  %R99 = phi i64 [ %R29, %block_402ae3 ], [ %R71, %subblock_402af3_1 ]
  %R98 = phi i64 [ %R28, %block_402ae3 ], [ %R70, %subblock_402af3_1 ]
  %R97 = phi i64 [ %R27, %block_402ae3 ], [ %R68, %subblock_402af3_1 ]
  ; # 402b0f: mov    edx,0x1
  ; # 402b14: lea    rsi,[rsp+0xf]
  ; r110 := (bv_add r99 0xf :: [64])
  %R110 = add i64 %R99, 15
  ; # 402b19: mov    rdi,rbx
  ; # 402b1c: call   QWORD PTR [rbx+0x48]
  ; r111 := (bv_add r98 0x48 :: [64])
  %R111 = add i64 %R98, 72
  ; r112 := *r111
  %r132 = inttoptr i64 %R111 to i64*
  %R112 = load i64* %r132
  ; r113 := (bv_add r99 0xfffffffffffffff8 :: [64])
  %R113 = add i64 %R99, 18446744073709551608
  ; r117 := (bv_add r113 0x8 :: [64])
  %R117 = add i64 %R113, 8
  %r136 = inttoptr i64 %R112 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r137 = bitcast i128 %R102 to <2 x double>
  %r138 = bitcast i128 %R103 to <2 x double>
  %r139 = bitcast i128 %R104 to <2 x double>
  %r140 = bitcast i128 %R105 to <2 x double>
  %r141 = bitcast i128 %R106 to <2 x double>
  %r142 = bitcast i128 %R107 to <2 x double>
  %r143 = bitcast i128 %R108 to <2 x double>
  %r144 = bitcast i128 %R109 to <2 x double>
  %r145 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r136(i64 %R98, i64 %R110, i64 1, i64 %R97, i64 %R100, i64 %R101, <2 x double> %r137, <2 x double> %r138, <2 x double> %r139, <2 x double> %r140, <2 x double> %r141, <2 x double> %r142, <2 x double> %r143, <2 x double> %r144)
  %R114 = extractvalue { i64, i64, <2 x double> } %r145, 0
  %R115 = extractvalue { i64, i64, <2 x double> } %r145, 1
  %r148 = extractvalue { i64, i64, <2 x double> } %r145, 2
  %R116 = bitcast <2 x double> %r148 to i128
  br label %block_402b1f
block_402b1f:
  %R121 = phi i128 [ %R116, %block_402b0f ]
  %R120 = phi i64 [ %R117, %block_402b0f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R119 = phi i64 [ undef, %block_402b0f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R118 = phi i64 [ undef, %block_402b0f ]
  ; # 402b1f: dec    rax
  ; r122 := (bv_eq r118 0x1 :: [64])
  %R122 = icmp eq i64 %R118, 1
  ; # 402b22: jne    402aee
  ; r123 := (bv_complement r122)
  %R123 = xor i1 %R122, -1
  br i1 %R123, label %subblock_402b1f_1, label %subblock_402b1f_2
subblock_402b1f_1:
  br label %block_402aee
subblock_402b1f_2:
  br label %block_402b24
block_402b24:
  %R126 = phi i128 [ %R121, %subblock_402b1f_2 ]
  %R125 = phi i64 [ %R120, %subblock_402b1f_2 ]
  %R124 = phi i64 [ %R119, %subblock_402b1f_2 ]
  ; # 402b24: movzx  eax,BYTE PTR [rsp+0xf]
  ; r127 := (bv_add r125 0xf :: [64])
  %R127 = add i64 %R125, 15
  ; r128 := *r127
  %r160 = inttoptr i64 %R127 to i8*
  %R128 = load i8* %r160
  ; r129 := (uext r128 64)
  %R129 = zext i8 %R128 to i64
  br label %block_402b29
block_402b29:
  %R132 = phi i128 [ %R93, %block_402b03 ], [ %R126, %block_402b24 ], [ %R67, %block_402aee ]
  %R131 = phi i64 [ %R91, %block_402b03 ], [ %R124, %block_402b24 ], [ %R66, %block_402aee ]
  %R130 = phi i64 [ %R90, %block_402b03 ], [ %R129, %block_402b24 ], [ 4294967295, %block_402aee ]
  ; # 402b29: add    rsp,0x10
  ; # 402b2d: pop    rbx
  ; # 402b2e: ret
  %r166 = bitcast i128 %R132 to <2 x double>
  %r167 = insertvalue { i64, i64, <2 x double> } undef, i64 %R130, 0
  %r168 = insertvalue { i64, i64, <2 x double> } %r167, i64 %R131, 1
  %r169 = insertvalue { i64, i64, <2 x double> } %r168, <2 x double> %r166, 2
  ret { i64, i64, <2 x double> } %r169
failure:
  br label %failure
}