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
define { i64, i64, <2 x double> } @F402d93(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_402d93
block_402d93:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 402d93: movzx  rax,sil
  ; r2 := (trunc arg1 8)
  %R2 = trunc i64 %a1 to i8
  ; r3 := (uext r2 64)
  %R3 = zext i8 %R2 to i64
  ; # 402d97: mov    r8,0x101010101010101
  ; # 402da1: imul   rax,r8
  ; r4 := (sext 0x101010101010101 :: [64] 128)
  %R4 = sext i64 72340172838076673 to i128
  ; r5 := (sext r3 128)
  %R5 = sext i64 %R3 to i128
  ; r6 := (bv_mul r4 r5)
  %R6 = mul i128 %R4, %R5
  ; r7 := (trunc r6 64)
  %R7 = trunc i128 %R6 to i64
  ; # 402da5: cmp    rdx,0x7e
  ; # 402da9: ja     402e23
  ; r8 := (bv_ult 0x7e :: [64] arg2)
  %R8 = icmp ult i64 126, %a2
  br i1 %R8, label %subblock_402d93_1, label %subblock_402d93_2
subblock_402d93_1:
  br label %block_402e23
subblock_402d93_2:
  br label %block_402dab
block_402dab:
  %R13 = phi i128 [ %r0, %subblock_402d93_2 ]
  %R12 = phi i64 [ %a0, %subblock_402d93_2 ]
  %R11 = phi i64 [ %a1, %subblock_402d93_2 ]
  %R10 = phi i64 [ %a2, %subblock_402d93_2 ]
  %R9 = phi i64 [ %R7, %subblock_402d93_2 ]
  ; # 402dab: test   edx,edx
  ; r14 := (trunc r10 32)
  %R14 = trunc i64 %R10 to i32
  ; r15 := (bv_eq r14 0x0 :: [32])
  %R15 = icmp eq i32 %R14, 0
  ; # 402dad: je     402e1f
  br i1 %R15, label %subblock_402dab_1, label %subblock_402dab_2
subblock_402dab_1:
  br label %block_402e1f
subblock_402dab_2:
  br label %block_402daf
block_402daf:
  %R20 = phi i128 [ %R13, %subblock_402dab_2 ]
  %R19 = phi i64 [ %R12, %subblock_402dab_2 ]
  %R18 = phi i64 [ %R11, %subblock_402dab_2 ]
  %R17 = phi i64 [ %R10, %subblock_402dab_2 ]
  %R16 = phi i64 [ %R9, %subblock_402dab_2 ]
  ; # 402daf: mov    BYTE PTR [rdi],sil
  ; r21 := (trunc r18 8)
  %R21 = trunc i64 %R18 to i8
  ; *(r19) = r21
  %r24 = inttoptr i64 %R19 to i8*
  store i8 %R21, i8* %r24
  ; # 402db2: mov    BYTE PTR [rdi+rdx*1-0x1],sil
  ; r22 := (bv_add r19 r17)
  %R22 = add i64 %R19, %R17
  ; r23 := (bv_add r22 0xffffffffffffffff :: [64])
  %R23 = add i64 %R22, 18446744073709551615
  ; *(r23) = r21
  %r27 = inttoptr i64 %R23 to i8*
  store i8 %R21, i8* %r27
  ; # 402db7: cmp    edx,0x2
  ; r24 := (trunc r17 32)
  %R24 = trunc i64 %R17 to i32
  ; # 402dba: jbe    402e1f
  ; r25 := (bv_ule r24 0x2 :: [32])
  %R25 = icmp ule i32 %R24, 2
  br i1 %R25, label %subblock_402daf_1, label %subblock_402daf_2
subblock_402daf_1:
  br label %block_402e1f
subblock_402daf_2:
  br label %block_402dbc
block_402dbc:
  %R29 = phi i128 [ %R20, %subblock_402daf_2 ]
  %R28 = phi i64 [ %R19, %subblock_402daf_2 ]
  %R27 = phi i64 [ %R17, %subblock_402daf_2 ]
  %R26 = phi i64 [ %R16, %subblock_402daf_2 ]
  ; # 402dbc: mov    WORD PTR [rdi+0x1],ax
  ; r30 := (trunc r26 16)
  %R30 = trunc i64 %R26 to i16
  ; r31 := (bv_add r28 0x1 :: [64])
  %R31 = add i64 %R28, 1
  ; *(r31) = r30
  %r36 = inttoptr i64 %R31 to i16*
  store i16 %R30, i16* %r36
  ; # 402dc0: mov    WORD PTR [rdi+rdx*1-0x3],ax
  ; r32 := (bv_add r28 r27)
  %R32 = add i64 %R28, %R27
  ; r33 := (bv_add r32 0xfffffffffffffffd :: [64])
  %R33 = add i64 %R32, 18446744073709551613
  ; *(r33) = r30
  %r39 = inttoptr i64 %R33 to i16*
  store i16 %R30, i16* %r39
  ; # 402dc5: cmp    edx,0x6
  ; r34 := (trunc r27 32)
  %R34 = trunc i64 %R27 to i32
  ; # 402dc8: jbe    402e1f
  ; r35 := (bv_ule r34 0x6 :: [32])
  %R35 = icmp ule i32 %R34, 6
  br i1 %R35, label %subblock_402dbc_1, label %subblock_402dbc_2
subblock_402dbc_1:
  br label %block_402e1f
subblock_402dbc_2:
  br label %block_402dca
block_402dca:
  %R39 = phi i128 [ %R29, %subblock_402dbc_2 ]
  %R38 = phi i64 [ %R28, %subblock_402dbc_2 ]
  %R37 = phi i64 [ %R27, %subblock_402dbc_2 ]
  %R36 = phi i64 [ %R26, %subblock_402dbc_2 ]
  ; # 402dca: mov    DWORD PTR [rdi+0x3],eax
  ; r40 := (trunc r36 32)
  %R40 = trunc i64 %R36 to i32
  ; r41 := (bv_add r38 0x3 :: [64])
  %R41 = add i64 %R38, 3
  ; *(r41) = r40
  %r48 = inttoptr i64 %R41 to i32*
  store i32 %R40, i32* %r48
  ; # 402dcd: mov    DWORD PTR [rdi+rdx*1-0x7],eax
  ; r42 := (bv_add r38 r37)
  %R42 = add i64 %R38, %R37
  ; r43 := (bv_add r42 0xfffffffffffffff9 :: [64])
  %R43 = add i64 %R42, 18446744073709551609
  ; *(r43) = r40
  %r51 = inttoptr i64 %R43 to i32*
  store i32 %R40, i32* %r51
  ; # 402dd1: cmp    edx,0xe
  ; r44 := (trunc r37 32)
  %R44 = trunc i64 %R37 to i32
  ; # 402dd4: jbe    402e1f
  ; r45 := (bv_ule r44 0xe :: [32])
  %R45 = icmp ule i32 %R44, 14
  br i1 %R45, label %subblock_402dca_1, label %subblock_402dca_2
subblock_402dca_1:
  br label %block_402e1f
subblock_402dca_2:
  br label %block_402dd6
block_402dd6:
  %R49 = phi i128 [ %R39, %subblock_402dca_2 ]
  %R48 = phi i64 [ %R38, %subblock_402dca_2 ]
  %R47 = phi i64 [ %R37, %subblock_402dca_2 ]
  %R46 = phi i64 [ %R36, %subblock_402dca_2 ]
  ; # 402dd6: mov    QWORD PTR [rdi+0x7],rax
  ; r50 := (bv_add r48 0x7 :: [64])
  %R50 = add i64 %R48, 7
  ; *(r50) = r46
  %r59 = inttoptr i64 %R50 to i64*
  store i64 %R46, i64* %r59
  ; # 402dda: mov    QWORD PTR [rdi+rdx*1-0xf],rax
  ; r51 := (bv_add r48 r47)
  %R51 = add i64 %R48, %R47
  ; r52 := (bv_add r51 0xfffffffffffffff1 :: [64])
  %R52 = add i64 %R51, 18446744073709551601
  ; *(r52) = r46
  %r62 = inttoptr i64 %R52 to i64*
  store i64 %R46, i64* %r62
  ; # 402ddf: cmp    edx,0x1e
  ; r53 := (trunc r47 32)
  %R53 = trunc i64 %R47 to i32
  ; # 402de2: jbe    402e1f
  ; r54 := (bv_ule r53 0x1e :: [32])
  %R54 = icmp ule i32 %R53, 30
  br i1 %R54, label %subblock_402dd6_1, label %subblock_402dd6_2
subblock_402dd6_1:
  br label %block_402e1f
subblock_402dd6_2:
  br label %block_402de4
block_402de4:
  %R58 = phi i128 [ %R49, %subblock_402dd6_2 ]
  %R57 = phi i64 [ %R48, %subblock_402dd6_2 ]
  %R56 = phi i64 [ %R47, %subblock_402dd6_2 ]
  %R55 = phi i64 [ %R46, %subblock_402dd6_2 ]
  ; # 402de4: mov    QWORD PTR [rdi+0xf],rax
  ; r59 := (bv_add r57 0xf :: [64])
  %R59 = add i64 %R57, 15
  ; *(r59) = r55
  %r70 = inttoptr i64 %R59 to i64*
  store i64 %R55, i64* %r70
  ; # 402de8: mov    QWORD PTR [rdi+0x17],rax
  ; r60 := (bv_add r57 0x17 :: [64])
  %R60 = add i64 %R57, 23
  ; *(r60) = r55
  %r72 = inttoptr i64 %R60 to i64*
  store i64 %R55, i64* %r72
  ; # 402dec: mov    QWORD PTR [rdi+rdx*1-0x1f],rax
  ; r61 := (bv_add r57 r56)
  %R61 = add i64 %R57, %R56
  ; r62 := (bv_add r61 0xffffffffffffffe1 :: [64])
  %R62 = add i64 %R61, 18446744073709551585
  ; *(r62) = r55
  %r75 = inttoptr i64 %R62 to i64*
  store i64 %R55, i64* %r75
  ; # 402df1: mov    QWORD PTR [rdi+rdx*1-0x17],rax
  ; r63 := (bv_add r61 0xffffffffffffffe9 :: [64])
  %R63 = add i64 %R61, 18446744073709551593
  ; *(r63) = r55
  %r77 = inttoptr i64 %R63 to i64*
  store i64 %R55, i64* %r77
  ; # 402df6: cmp    edx,0x3e
  ; r64 := (trunc r56 32)
  %R64 = trunc i64 %R56 to i32
  ; # 402df9: jbe    402e1f
  ; r65 := (bv_ule r64 0x3e :: [32])
  %R65 = icmp ule i32 %R64, 62
  br i1 %R65, label %subblock_402de4_1, label %subblock_402de4_2
subblock_402de4_1:
  br label %block_402e1f
subblock_402de4_2:
  br label %block_402dfb
block_402dfb:
  %R69 = phi i128 [ %R58, %subblock_402de4_2 ]
  %R68 = phi i64 [ %R57, %subblock_402de4_2 ]
  %R67 = phi i64 [ %R56, %subblock_402de4_2 ]
  %R66 = phi i64 [ %R55, %subblock_402de4_2 ]
  ; # 402dfb: mov    QWORD PTR [rdi+0x1f],rax
  ; r70 := (bv_add r68 0x1f :: [64])
  %R70 = add i64 %R68, 31
  ; *(r70) = r66
  %r85 = inttoptr i64 %R70 to i64*
  store i64 %R66, i64* %r85
  ; # 402dff: mov    QWORD PTR [rdi+0x27],rax
  ; r71 := (bv_add r68 0x27 :: [64])
  %R71 = add i64 %R68, 39
  ; *(r71) = r66
  %r87 = inttoptr i64 %R71 to i64*
  store i64 %R66, i64* %r87
  ; # 402e03: mov    QWORD PTR [rdi+0x2f],rax
  ; r72 := (bv_add r68 0x2f :: [64])
  %R72 = add i64 %R68, 47
  ; *(r72) = r66
  %r89 = inttoptr i64 %R72 to i64*
  store i64 %R66, i64* %r89
  ; # 402e07: mov    QWORD PTR [rdi+0x37],rax
  ; r73 := (bv_add r68 0x37 :: [64])
  %R73 = add i64 %R68, 55
  ; *(r73) = r66
  %r91 = inttoptr i64 %R73 to i64*
  store i64 %R66, i64* %r91
  ; # 402e0b: mov    QWORD PTR [rdi+rdx*1-0x3f],rax
  ; r74 := (bv_add r68 r67)
  %R74 = add i64 %R68, %R67
  ; r75 := (bv_add r74 0xffffffffffffffc1 :: [64])
  %R75 = add i64 %R74, 18446744073709551553
  ; *(r75) = r66
  %r94 = inttoptr i64 %R75 to i64*
  store i64 %R66, i64* %r94
  ; # 402e10: mov    QWORD PTR [rdi+rdx*1-0x37],rax
  ; r76 := (bv_add r74 0xffffffffffffffc9 :: [64])
  %R76 = add i64 %R74, 18446744073709551561
  ; *(r76) = r66
  %r96 = inttoptr i64 %R76 to i64*
  store i64 %R66, i64* %r96
  ; # 402e15: mov    QWORD PTR [rdi+rdx*1-0x2f],rax
  ; r77 := (bv_add r74 0xffffffffffffffd1 :: [64])
  %R77 = add i64 %R74, 18446744073709551569
  ; *(r77) = r66
  %r98 = inttoptr i64 %R77 to i64*
  store i64 %R66, i64* %r98
  ; # 402e1a: mov    QWORD PTR [rdi+rdx*1-0x27],rax
  ; r78 := (bv_add r74 0xffffffffffffffd9 :: [64])
  %R78 = add i64 %R74, 18446744073709551577
  ; *(r78) = r66
  %r100 = inttoptr i64 %R78 to i64*
  store i64 %R66, i64* %r100
  br label %block_402e1f
block_402e1f:
  %R79 = phi i128 [ %R69, %block_402dfb ], [ %R58, %subblock_402de4_1 ], [ %R49, %subblock_402dd6_1 ], [ %R39, %subblock_402dca_1 ], [ %R29, %subblock_402dbc_1 ], [ %R20, %subblock_402daf_1 ], [ %R13, %subblock_402dab_1 ]
  ; # 402e1f: mov    rax,rdi
  ; # 402e22: ret
  %r102 = bitcast i128 %R79 to <2 x double>
  %r103 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r104 = insertvalue { i64, i64, <2 x double> } %r103, i64 undef, 1
  %r105 = insertvalue { i64, i64, <2 x double> } %r104, <2 x double> %r102, 2
  ret { i64, i64, <2 x double> } %r105
block_402e23:
  %R84 = phi i128 [ %r0, %subblock_402d93_1 ]
  %R83 = phi i1 [ 0, %subblock_402d93_1 ]
  %R82 = phi i64 [ %a0, %subblock_402d93_1 ]
  %R81 = phi i64 [ %a2, %subblock_402d93_1 ]
  %R80 = phi i64 [ %R7, %subblock_402d93_1 ]
  ; # 402e23: test   edi,0xf
  ; r85 := (trunc r82 32)
  %R85 = trunc i64 %R82 to i32
  ; r86 := (bv_and r85 0xf :: [32])
  %R86 = and i32 %R85, 15
  ; r87 := (bv_eq r86 0x0 :: [32])
  %R87 = icmp eq i32 %R86, 0
  ; # 402e29: mov    r8,rdi
  ; # 402e2c: mov    QWORD PTR [rdi+rdx*1-0x8],rax
  ; r88 := (bv_add r82 r81)
  %R88 = add i64 %R82, %R81
  ; r89 := (bv_add r88 0xfffffffffffffff8 :: [64])
  %R89 = add i64 %R88, 18446744073709551608
  ; *(r89) = r80
  %r116 = inttoptr i64 %R89 to i64*
  store i64 %R80, i64* %r116
  ; # 402e31: mov    rcx,rdx
  ; # 402e34: jne    402e41
  ; r90 := (bv_complement r87)
  %R90 = xor i1 %R87, -1
  br i1 %R90, label %subblock_402e23_1, label %subblock_402e23_2
subblock_402e23_1:
  br label %block_402e41
subblock_402e23_2:
  br label %block_402e36
block_402e36:
  %R95 = phi i128 [ %R101, %block_402e41 ], [ %R84, %subblock_402e23_2 ]
  %R94 = phi i1 [ %R100, %block_402e41 ], [ %R83, %subblock_402e23_2 ]
  %R93 = phi i64 [ %R108, %block_402e41 ], [ %R82, %subblock_402e23_2 ]
  %R92 = phi i64 [ %R107, %block_402e41 ], [ %R81, %subblock_402e23_2 ]
  %R91 = phi i64 [ %R97, %block_402e41 ], [ %R80, %subblock_402e23_2 ]
  ; # 402e36: shr    rcx,0x3
  ; r96 := (bv_shr r92 0x3 :: [64])
  %R96 = lshr i64 %R92, 3
  ; # 402e3a: rep stos QWORD PTR [rdi],rax
  ; memset (r96,r91,r93,r94)
  %r124 = inttoptr i64 %R93 to i64*
  call void @reopt.MemSet.i64(i64* %r124, i64 %R91, i64 %R96, i1 %R94)
  ; # 402e3d: mov    rax,r8
  ; # 402e40: ret
  %r125 = bitcast i128 %R95 to <2 x double>
  %r126 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r127 = insertvalue { i64, i64, <2 x double> } %r126, i64 undef, 1
  %r128 = insertvalue { i64, i64, <2 x double> } %r127, <2 x double> %r125, 2
  ret { i64, i64, <2 x double> } %r128
block_402e41:
  %R101 = phi i128 [ %R84, %subblock_402e23_1 ]
  %R100 = phi i1 [ %R83, %subblock_402e23_1 ]
  %R99 = phi i64 [ %R82, %subblock_402e23_1 ]
  %R98 = phi i64 [ %R81, %subblock_402e23_1 ]
  %R97 = phi i64 [ %R80, %subblock_402e23_1 ]
  ; # 402e41: xor    edx,edx
  ; # 402e43: sub    edx,edi
  ; r102 := (trunc r99 32)
  %R102 = trunc i64 %R99 to i32
  ; r103 := (bv_sub 0x0 :: [32] r102)
  %R103 = sub i32 0, %R102
  ; # 402e45: and    edx,0xf
  ; r104 := (bv_and r103 0xf :: [32])
  %R104 = and i32 %R103, 15
  ; r105 := (uext r104 64)
  %R105 = zext i32 %R104 to i64
  ; # 402e48: mov    QWORD PTR [rdi],rax
  ; *(r99) = r97
  %r138 = inttoptr i64 %R99 to i64*
  store i64 %R97, i64* %r138
  ; # 402e4b: mov    QWORD PTR [rdi+0x8],rax
  ; r106 := (bv_add r99 0x8 :: [64])
  %R106 = add i64 %R99, 8
  ; *(r106) = r97
  %r140 = inttoptr i64 %R106 to i64*
  store i64 %R97, i64* %r140
  ; # 402e4f: sub    rcx,rdx
  ; r107 := (bv_sub r98 r105)
  %R107 = sub i64 %R98, %R105
  ; # 402e52: add    rdi,rdx
  ; r108 := (bv_add r99 r105)
  %R108 = add i64 %R99, %R105
  ; # 402e55: jmp    402e36
  br label %block_402e36
failure:
  br label %failure
}