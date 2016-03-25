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
define { i64, i64, <2 x double> } @F402375(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_402375
block_402375:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 402375: mov    rax,rdi
  ; # 402378: cmp    rdx,0x8
  ; r2 := (bv_ult arg2 0x8 :: [64])
  %R2 = icmp ult i64 %a2, 8
  ; # 40237c: jb     402392
  br i1 %R2, label %subblock_402375_1, label %subblock_402375_2
subblock_402375_1:
  br label %block_402392
subblock_402375_2:
  br label %block_40237e
block_40237e:
  %R8 = phi i128 [ %r0, %subblock_402375_2 ]
  %R7 = phi i1 [ 0, %subblock_402375_2 ]
  %R6 = phi i64 [ %a0, %subblock_402375_2 ]
  %R5 = phi i64 [ %a1, %subblock_402375_2 ]
  %R4 = phi i64 [ %a2, %subblock_402375_2 ]
  %R3 = phi i64 [ %a0, %subblock_402375_2 ]
  ; # 40237e: test   edi,0x7
  ; r9 := (trunc r6 32)
  %R9 = trunc i64 %R6 to i32
  ; r10 := (bv_and r9 0x7 :: [32])
  %R10 = and i32 %R9, 7
  ; r11 := (bv_eq r10 0x0 :: [32])
  %R11 = icmp eq i32 %R10, 0
  ; # 402384: je     402392
  br i1 %R11, label %subblock_40237e_1, label %subblock_40237e_2
subblock_40237e_1:
  br label %block_402392
subblock_40237e_2:
  br label %block_402386
block_402386:
  %R17 = phi i128 [ %R17, %subblock_402386_1 ], [ %R8, %subblock_40237e_2 ]
  %R16 = phi i1 [ %R16, %subblock_402386_1 ], [ %R7, %subblock_40237e_2 ]
  %R15 = phi i64 [ %R24, %subblock_402386_1 ], [ %R6, %subblock_40237e_2 ]
  %R14 = phi i64 [ %R21, %subblock_402386_1 ], [ %R5, %subblock_40237e_2 ]
  %R13 = phi i64 [ %R25, %subblock_402386_1 ], [ %R4, %subblock_40237e_2 ]
  %R12 = phi i64 [ %R12, %subblock_402386_1 ], [ %R3, %subblock_40237e_2 ]
  ; # 402386: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; r18 := *r14
  %r20 = inttoptr i64 %R14 to i8*
  %R18 = load i8* %r20
  ; *(r15) = r18
  %r22 = inttoptr i64 %R15 to i8*
  store i8 %R18, i8* %r22
  ; r19 := (bv_add r14 0xffffffffffffffff :: [64])
  %R19 = add i64 %R14, 18446744073709551615
  ; r20 := (bv_add r14 0x1 :: [64])
  %R20 = add i64 %R14, 1
  ; r21 := (mux r16 r19 r20)
  %R21 = select i1 %R16, i64 %R19, i64 %R20
  ; r22 := (bv_add r15 0xffffffffffffffff :: [64])
  %R22 = add i64 %R15, 18446744073709551615
  ; r23 := (bv_add r15 0x1 :: [64])
  %R23 = add i64 %R15, 1
  ; r24 := (mux r16 r22 r23)
  %R24 = select i1 %R16, i64 %R22, i64 %R23
  ; # 402387: dec    rdx
  ; r25 := (bv_add r13 0xffffffffffffffff :: [64])
  %R25 = add i64 %R13, 18446744073709551615
  ; # 40238a: test   edi,0x7
  ; r26 := (trunc r24 32)
  %R26 = trunc i64 %R24 to i32
  ; r27 := (bv_and r26 0x7 :: [32])
  %R27 = and i32 %R26, 7
  ; r28 := (bv_eq r27 0x0 :: [32])
  %R28 = icmp eq i32 %R27, 0
  ; # 402390: jne    402386
  ; r29 := (bv_complement r28)
  %R29 = xor i1 %R28, -1
  br i1 %R29, label %subblock_402386_1, label %subblock_402386_2
subblock_402386_1:
  br label %block_402386
subblock_402386_2:
  br label %block_402392
block_402392:
  %R35 = phi i128 [ %R17, %subblock_402386_2 ], [ %R8, %subblock_40237e_1 ], [ %r0, %subblock_402375_1 ]
  %R34 = phi i1 [ %R16, %subblock_402386_2 ], [ %R7, %subblock_40237e_1 ], [ 0, %subblock_402375_1 ]
  %R33 = phi i64 [ %R24, %subblock_402386_2 ], [ %R6, %subblock_40237e_1 ], [ %a0, %subblock_402375_1 ]
  %R32 = phi i64 [ %R21, %subblock_402386_2 ], [ %R5, %subblock_40237e_1 ], [ %a1, %subblock_402375_1 ]
  %R31 = phi i64 [ %R25, %subblock_402386_2 ], [ %R4, %subblock_40237e_1 ], [ %a2, %subblock_402375_1 ]
  %R30 = phi i64 [ %R12, %subblock_402386_2 ], [ %R3, %subblock_40237e_1 ], [ %a0, %subblock_402375_1 ]
  ; # 402392: mov    rcx,rdx
  ; # 402395: shr    rcx,0x3
  ; r36 := (bv_shr r31 0x3 :: [64])
  %R36 = lshr i64 %R31, 3
  ; # 402399: rep movs QWORD PTR [rdi],QWORD PTR [rsi]
  ; memcopy (8,r36,r32,r33,r34)
  call void @reopt.MemCopy.i64(i64 %R33, i64 %R32, i64 %R36, i1 %R34)
  ; r37 := (bv_mul r36 0x8 :: [64])
  %R37 = mul i64 %R36, 8
  ; r38 := (bv_sub r32 r37)
  %R38 = sub i64 %R32, %R37
  ; r39 := (bv_add r32 r37)
  %R39 = add i64 %R32, %R37
  ; r40 := (mux r34 r38 r39)
  %R40 = select i1 %R34, i64 %R38, i64 %R39
  ; r41 := (bv_sub r33 r37)
  %R41 = sub i64 %R33, %R37
  ; r42 := (bv_add r33 r37)
  %R42 = add i64 %R33, %R37
  ; r43 := (mux r34 r41 r42)
  %R43 = select i1 %R34, i64 %R41, i64 %R42
  ; # 40239c: and    edx,0x7
  ; r44 := (trunc r31 32)
  %R44 = trunc i64 %R31 to i32
  ; r45 := (bv_and r44 0x7 :: [32])
  %R45 = and i32 %R44, 7
  ; r46 := (bv_eq r45 0x0 :: [32])
  %R46 = icmp eq i32 %R45, 0
  ; r47 := (uext r45 64)
  %R47 = zext i32 %R45 to i64
  ; # 40239f: je     4023a6
  br i1 %R46, label %subblock_402392_1, label %subblock_402392_2
subblock_402392_1:
  br label %block_4023a6
subblock_402392_2:
  br label %block_4023a1
block_4023a1:
  %R53 = phi i128 [ %R53, %subblock_4023a1_1 ], [ %R35, %subblock_402392_2 ]
  %R52 = phi i1 [ %R52, %subblock_4023a1_1 ], [ %R34, %subblock_402392_2 ]
  %R51 = phi i64 [ %R60, %subblock_4023a1_1 ], [ %R43, %subblock_402392_2 ]
  %R50 = phi i64 [ %R57, %subblock_4023a1_1 ], [ %R40, %subblock_402392_2 ]
  %R49 = phi i64 [ %R64, %subblock_4023a1_1 ], [ %R47, %subblock_402392_2 ]
  %R48 = phi i64 [ %R48, %subblock_4023a1_1 ], [ %R30, %subblock_402392_2 ]
  ; # 4023a1: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; r54 := *r50
  %r58 = inttoptr i64 %R50 to i8*
  %R54 = load i8* %r58
  ; *(r51) = r54
  %r60 = inttoptr i64 %R51 to i8*
  store i8 %R54, i8* %r60
  ; r55 := (bv_add r50 0xffffffffffffffff :: [64])
  %R55 = add i64 %R50, 18446744073709551615
  ; r56 := (bv_add r50 0x1 :: [64])
  %R56 = add i64 %R50, 1
  ; r57 := (mux r52 r55 r56)
  %R57 = select i1 %R52, i64 %R55, i64 %R56
  ; r58 := (bv_add r51 0xffffffffffffffff :: [64])
  %R58 = add i64 %R51, 18446744073709551615
  ; r59 := (bv_add r51 0x1 :: [64])
  %R59 = add i64 %R51, 1
  ; r60 := (mux r52 r58 r59)
  %R60 = select i1 %R52, i64 %R58, i64 %R59
  ; # 4023a2: dec    edx
  ; r61 := (trunc r49 32)
  %R61 = trunc i64 %R49 to i32
  ; r62 := (bv_add r61 0xffffffff :: [32])
  %R62 = add i32 %R61, 4294967295
  ; r63 := (bv_eq r61 0x1 :: [32])
  %R63 = icmp eq i32 %R61, 1
  ; r64 := (uext r62 64)
  %R64 = zext i32 %R62 to i64
  ; # 4023a4: jne    4023a1
  ; r65 := (bv_complement r63)
  %R65 = xor i1 %R63, -1
  br i1 %R65, label %subblock_4023a1_1, label %subblock_4023a1_2
subblock_4023a1_1:
  br label %block_4023a1
subblock_4023a1_2:
  br label %block_4023a6
block_4023a6:
  %R68 = phi i128 [ %R53, %subblock_4023a1_2 ], [ %R35, %subblock_402392_1 ]
  %R67 = phi i64 [ %R64, %subblock_4023a1_2 ], [ %R47, %subblock_402392_1 ]
  %R66 = phi i64 [ %R48, %subblock_4023a1_2 ], [ %R30, %subblock_402392_1 ]
  ; # 4023a6: ret
  %r75 = bitcast i128 %R68 to <2 x double>
  %r76 = insertvalue { i64, i64, <2 x double> } undef, i64 %R66, 0
  %r77 = insertvalue { i64, i64, <2 x double> } %r76, i64 %R67, 1
  %r78 = insertvalue { i64, i64, <2 x double> } %r77, <2 x double> %r75, 2
  ret { i64, i64, <2 x double> } %r78
failure:
  br label %failure
}