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
define { i64, i64, <2 x double> } @F400645(i64 %a0, i64 %a1, i64 %a2) {
entry:
  br label %block_400645
block_400645:
  ; r0 := (alloca 0x0 :: [64])
  %r0 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 400645: mov    rax,rdi
  ; # 400648: cmp    rdx,0x8
  ; r2 := (bv_ult arg2 0x8 :: [64])
  %R2 = icmp ult i64 %a2, 8
  ; # 40064c: jb     400662
  br i1 %R2, label %subblock_400645_1, label %subblock_400645_2
subblock_400645_1:
  br label %block_400662
subblock_400645_2:
  br label %block_40064e
block_40064e:
  %R7 = phi i1 [ 0, %subblock_400645_2 ]
  %R6 = phi i64 [ %a0, %subblock_400645_2 ]
  %R5 = phi i64 [ %a1, %subblock_400645_2 ]
  %R4 = phi i64 [ %a2, %subblock_400645_2 ]
  %R3 = phi i64 [ %a0, %subblock_400645_2 ]
  ; # 40064e: test   edi,0x7
  ; r8 := (trunc r6 32)
  %R8 = trunc i64 %R6 to i32
  ; r9 := (bv_and r8 0x7 :: [32])
  %R9 = and i32 %R8, 7
  ; r10 := (bv_eq r9 0x0 :: [32])
  %R10 = icmp eq i32 %R9, 0
  ; # 400654: je     400662
  br i1 %R10, label %subblock_40064e_1, label %subblock_40064e_2
subblock_40064e_1:
  br label %block_400662
subblock_40064e_2:
  br label %block_400656
block_400656:
  %R15 = phi i1 [ %R15, %subblock_400656_1 ], [ %R7, %subblock_40064e_2 ]
  %R14 = phi i64 [ %R22, %subblock_400656_1 ], [ %R6, %subblock_40064e_2 ]
  %R13 = phi i64 [ %R19, %subblock_400656_1 ], [ %R5, %subblock_40064e_2 ]
  %R12 = phi i64 [ %R23, %subblock_400656_1 ], [ %R4, %subblock_40064e_2 ]
  %R11 = phi i64 [ %R11, %subblock_400656_1 ], [ %R3, %subblock_40064e_2 ]
  ; # 400656: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; r16 := *r13
  %r17 = inttoptr i64 %R13 to i8*
  %R16 = load i8* %r17
  ; *(r14) = r16
  %r19 = inttoptr i64 %R14 to i8*
  store i8 %R16, i8* %r19
  ; r17 := (bv_add r13 0xffffffffffffffff :: [64])
  %R17 = add i64 %R13, 18446744073709551615
  ; r18 := (bv_add r13 0x1 :: [64])
  %R18 = add i64 %R13, 1
  ; r19 := (mux r15 r17 r18)
  %R19 = select i1 %R15, i64 %R17, i64 %R18
  ; r20 := (bv_add r14 0xffffffffffffffff :: [64])
  %R20 = add i64 %R14, 18446744073709551615
  ; r21 := (bv_add r14 0x1 :: [64])
  %R21 = add i64 %R14, 1
  ; r22 := (mux r15 r20 r21)
  %R22 = select i1 %R15, i64 %R20, i64 %R21
  ; # 400657: dec    rdx
  ; r23 := (bv_add r12 0xffffffffffffffff :: [64])
  %R23 = add i64 %R12, 18446744073709551615
  ; # 40065a: test   edi,0x7
  ; r24 := (trunc r22 32)
  %R24 = trunc i64 %R22 to i32
  ; r25 := (bv_and r24 0x7 :: [32])
  %R25 = and i32 %R24, 7
  ; r26 := (bv_eq r25 0x0 :: [32])
  %R26 = icmp eq i32 %R25, 0
  ; # 400660: jne    400656
  ; r27 := (bv_complement r26)
  %R27 = xor i1 %R26, -1
  br i1 %R27, label %subblock_400656_1, label %subblock_400656_2
subblock_400656_1:
  br label %block_400656
subblock_400656_2:
  br label %block_400662
block_400662:
  %R32 = phi i1 [ %R15, %subblock_400656_2 ], [ %R7, %subblock_40064e_1 ], [ 0, %subblock_400645_1 ]
  %R31 = phi i64 [ %R22, %subblock_400656_2 ], [ %R6, %subblock_40064e_1 ], [ %a0, %subblock_400645_1 ]
  %R30 = phi i64 [ %R19, %subblock_400656_2 ], [ %R5, %subblock_40064e_1 ], [ %a1, %subblock_400645_1 ]
  %R29 = phi i64 [ %R23, %subblock_400656_2 ], [ %R4, %subblock_40064e_1 ], [ %a2, %subblock_400645_1 ]
  %R28 = phi i64 [ %R11, %subblock_400656_2 ], [ %R3, %subblock_40064e_1 ], [ %a0, %subblock_400645_1 ]
  ; # 400662: mov    rcx,rdx
  ; # 400665: shr    rcx,0x3
  ; r33 := (bv_shr r29 0x3 :: [64])
  %R33 = lshr i64 %R29, 3
  ; # 400669: rep movs QWORD PTR [rdi],QWORD PTR [rsi]
  ; memcopy (8,r33,r30,r31,r32)
  call void @reopt.MemCopy.i64(i64 %R31, i64 %R30, i64 %R33, i1 %R32)
  ; r34 := (bv_mul r33 0x8 :: [64])
  %R34 = mul i64 %R33, 8
  ; r35 := (bv_sub r30 r34)
  %R35 = sub i64 %R30, %R34
  ; r36 := (bv_add r30 r34)
  %R36 = add i64 %R30, %R34
  ; r37 := (mux r32 r35 r36)
  %R37 = select i1 %R32, i64 %R35, i64 %R36
  ; r38 := (bv_sub r31 r34)
  %R38 = sub i64 %R31, %R34
  ; r39 := (bv_add r31 r34)
  %R39 = add i64 %R31, %R34
  ; r40 := (mux r32 r38 r39)
  %R40 = select i1 %R32, i64 %R38, i64 %R39
  ; # 40066c: and    edx,0x7
  ; r41 := (trunc r29 32)
  %R41 = trunc i64 %R29 to i32
  ; r42 := (bv_and r41 0x7 :: [32])
  %R42 = and i32 %R41, 7
  ; r43 := (bv_eq r42 0x0 :: [32])
  %R43 = icmp eq i32 %R42, 0
  ; r44 := (uext r42 64)
  %R44 = zext i32 %R42 to i64
  ; # 40066f: je     400676
  br i1 %R43, label %subblock_400662_1, label %subblock_400662_2
subblock_400662_1:
  br label %block_400676
subblock_400662_2:
  br label %block_400671
block_400671:
  %R49 = phi i1 [ %R49, %subblock_400671_1 ], [ %R32, %subblock_400662_2 ]
  %R48 = phi i64 [ %R56, %subblock_400671_1 ], [ %R40, %subblock_400662_2 ]
  %R47 = phi i64 [ %R53, %subblock_400671_1 ], [ %R37, %subblock_400662_2 ]
  %R46 = phi i64 [ %R60, %subblock_400671_1 ], [ %R44, %subblock_400662_2 ]
  %R45 = phi i64 [ %R45, %subblock_400671_1 ], [ %R28, %subblock_400662_2 ]
  ; # 400671: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; r50 := *r47
  %r53 = inttoptr i64 %R47 to i8*
  %R50 = load i8* %r53
  ; *(r48) = r50
  %r55 = inttoptr i64 %R48 to i8*
  store i8 %R50, i8* %r55
  ; r51 := (bv_add r47 0xffffffffffffffff :: [64])
  %R51 = add i64 %R47, 18446744073709551615
  ; r52 := (bv_add r47 0x1 :: [64])
  %R52 = add i64 %R47, 1
  ; r53 := (mux r49 r51 r52)
  %R53 = select i1 %R49, i64 %R51, i64 %R52
  ; r54 := (bv_add r48 0xffffffffffffffff :: [64])
  %R54 = add i64 %R48, 18446744073709551615
  ; r55 := (bv_add r48 0x1 :: [64])
  %R55 = add i64 %R48, 1
  ; r56 := (mux r49 r54 r55)
  %R56 = select i1 %R49, i64 %R54, i64 %R55
  ; # 400672: dec    edx
  ; r57 := (trunc r46 32)
  %R57 = trunc i64 %R46 to i32
  ; r58 := (bv_add r57 0xffffffff :: [32])
  %R58 = add i32 %R57, 4294967295
  ; r59 := (bv_eq r57 0x1 :: [32])
  %R59 = icmp eq i32 %R57, 1
  ; r60 := (uext r58 64)
  %R60 = zext i32 %R58 to i64
  ; # 400674: jne    400671
  ; r61 := (bv_complement r59)
  %R61 = xor i1 %R59, -1
  br i1 %R61, label %subblock_400671_1, label %subblock_400671_2
subblock_400671_1:
  br label %block_400671
subblock_400671_2:
  br label %block_400676
block_400676:
  %R62 = phi i64 [ %R45, %subblock_400671_2 ], [ %R28, %subblock_400662_1 ]
  ; # 400676: ret
  %r68 = insertvalue { i64, i64, <2 x double> } undef, i64 %R62, 0
  %r69 = insertvalue { i64, i64, <2 x double> } %r68, i64 undef, 1
  %r70 = insertvalue { i64, i64, <2 x double> } %r69, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r70
failure:
  br label %failure
}