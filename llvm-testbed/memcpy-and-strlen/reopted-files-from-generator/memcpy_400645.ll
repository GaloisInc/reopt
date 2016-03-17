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
  %R14 = phi i64 [ %R21, %subblock_400656_1 ], [ %R6, %subblock_40064e_2 ]
  %R13 = phi i64 [ %R18, %subblock_400656_1 ], [ %R5, %subblock_40064e_2 ]
  %R12 = phi i64 [ %R22, %subblock_400656_1 ], [ %R4, %subblock_40064e_2 ]
  %R11 = phi i64 [ %R11, %subblock_400656_1 ], [ %R3, %subblock_40064e_2 ]
  ; # 400656: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; r16 := (bv_add r13 0xffffffffffffffff :: [64])
  %R16 = add i64 %R13, 18446744073709551615
  ; r17 := (bv_add r13 0x1 :: [64])
  %R17 = add i64 %R13, 1
  ; r18 := (mux r15 r16 r17)
  %R18 = select i1 %R15, i64 %R16, i64 %R17
  ; r19 := (bv_add r14 0xffffffffffffffff :: [64])
  %R19 = add i64 %R14, 18446744073709551615
  ; r20 := (bv_add r14 0x1 :: [64])
  %R20 = add i64 %R14, 1
  ; r21 := (mux r15 r19 r20)
  %R21 = select i1 %R15, i64 %R19, i64 %R20
  ; # 400657: dec    rdx
  ; r22 := (bv_add r12 0xffffffffffffffff :: [64])
  %R22 = add i64 %R12, 18446744073709551615
  ; # 40065a: test   edi,0x7
  ; r23 := (trunc r21 32)
  %R23 = trunc i64 %R21 to i32
  ; r24 := (bv_and r23 0x7 :: [32])
  %R24 = and i32 %R23, 7
  ; r25 := (bv_eq r24 0x0 :: [32])
  %R25 = icmp eq i32 %R24, 0
  ; # 400660: jne    400656
  ; r26 := (bv_complement r25)
  %R26 = xor i1 %R25, -1
  br i1 %R26, label %subblock_400656_1, label %subblock_400656_2
subblock_400656_1:
  br label %block_400656
subblock_400656_2:
  br label %block_400662
block_400662:
  %R31 = phi i1 [ %R15, %subblock_400656_2 ], [ %R7, %subblock_40064e_1 ], [ 0, %subblock_400645_1 ]
  %R30 = phi i64 [ %R21, %subblock_400656_2 ], [ %R6, %subblock_40064e_1 ], [ %a0, %subblock_400645_1 ]
  %R29 = phi i64 [ %R18, %subblock_400656_2 ], [ %R5, %subblock_40064e_1 ], [ %a1, %subblock_400645_1 ]
  %R28 = phi i64 [ %R22, %subblock_400656_2 ], [ %R4, %subblock_40064e_1 ], [ %a2, %subblock_400645_1 ]
  %R27 = phi i64 [ %R11, %subblock_400656_2 ], [ %R3, %subblock_40064e_1 ], [ %a0, %subblock_400645_1 ]
  ; # 400662: mov    rcx,rdx
  ; # 400665: shr    rcx,0x3
  ; r32 := (bv_shr r28 0x3 :: [64])
  %R32 = lshr i64 %R28, 3
  ; # 400669: rep movs QWORD PTR [rdi],QWORD PTR [rsi]
  ; memcopy (8,r32,r29,r30,r31)
  call void @reopt.MemCopy.i64(i64 %R30, i64 %R29, i64 %R32, i1 %R31)
  ; # 40066c: and    edx,0x7
  ; r33 := (trunc r28 32)
  %R33 = trunc i64 %R28 to i32
  ; r34 := (bv_and r33 0x7 :: [32])
  %R34 = and i32 %R33, 7
  ; r35 := (bv_eq r34 0x0 :: [32])
  %R35 = icmp eq i32 %R34, 0
  ; r36 := (uext r34 64)
  %R36 = zext i32 %R34 to i64
  ; # 40066f: je     400676
  br i1 %R35, label %subblock_400662_1, label %subblock_400662_2
subblock_400662_1:
  br label %block_400676
subblock_400662_2:
  br label %block_400671
block_400671:
  %R38 = phi i64 [ %R42, %subblock_400671_1 ], [ %R36, %subblock_400662_2 ]
  %R37 = phi i64 [ %R37, %subblock_400671_1 ], [ %R27, %subblock_400662_2 ]
  ; # 400671: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; # 400672: dec    edx
  ; r39 := (trunc r38 32)
  %R39 = trunc i64 %R38 to i32
  ; r40 := (bv_add r39 0xffffffff :: [32])
  %R40 = add i32 %R39, 4294967295
  ; r41 := (bv_eq r39 0x1 :: [32])
  %R41 = icmp eq i32 %R39, 1
  ; r42 := (uext r40 64)
  %R42 = zext i32 %R40 to i64
  ; # 400674: jne    400671
  ; r43 := (bv_complement r41)
  %R43 = xor i1 %R41, -1
  br i1 %R43, label %subblock_400671_1, label %subblock_400671_2
subblock_400671_1:
  br label %block_400671
subblock_400671_2:
  br label %block_400676
block_400676:
  %R44 = phi i64 [ %R37, %subblock_400671_2 ], [ %R27, %subblock_400662_1 ]
  ; # 400676: ret
  %r46 = insertvalue { i64, i64, <2 x double> } undef, i64 %R44, 0
  %r47 = insertvalue { i64, i64, <2 x double> } %r46, i64 undef, 1
  %r48 = insertvalue { i64, i64, <2 x double> } %r47, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r48
failure:
  br label %failure
}