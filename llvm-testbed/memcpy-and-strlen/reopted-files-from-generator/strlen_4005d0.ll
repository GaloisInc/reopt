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
define { i64, i64, <2 x double> } @F4005d0(i64 %a0, i64 %a1, i64 %a2) {
entry:
  br label %block_4005d0
block_4005d0:
  ; r0 := (alloca 0x0 :: [64])
  %r0 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 4005d0: test   dil,0x7
  ; r2 := (trunc arg0 8)
  %R2 = trunc i64 %a0 to i8
  ; r3 := (bv_and r2 0x7 :: [8])
  %R3 = and i8 %R2, 7
  ; r4 := (bv_eq r3 0x0 :: [8])
  %R4 = icmp eq i8 %R3, 0
  ; # 4005d4: je     40063d
  br i1 %R4, label %subblock_4005d0_1, label %subblock_4005d0_2
subblock_4005d0_1:
  br label %block_40063d
subblock_4005d0_2:
  br label %block_4005d6
block_4005d6:
  %R6 = phi i64 [ %a0, %subblock_4005d0_2 ]
  %R5 = phi i64 [ %a2, %subblock_4005d0_2 ]
  ; # 4005d6: cmp    BYTE PTR [rdi],0x0
  ; r7 := *r6
  %r8 = inttoptr i64 %R6 to i8*
  %R7 = load i8* %r8
  ; r8 := (bv_eq r7 0x0 :: [8])
  %R8 = icmp eq i8 %R7, 0
  ; # 4005d9: je     400642
  br i1 %R8, label %subblock_4005d6_1, label %subblock_4005d6_2
subblock_4005d6_1:
  br label %block_400642
subblock_4005d6_2:
  br label %block_4005db
block_4005db:
  %R10 = phi i64 [ %R6, %subblock_4005d6_2 ]
  %R9 = phi i64 [ %R5, %subblock_4005d6_2 ]
  ; # 4005db: mov    rax,rdi
  ; # 4005de: jmp    4005e5
  br label %block_4005e5
block_4005e0:
  %R13 = phi i64 [ %R18, %subblock_4005e5_1 ]
  %R12 = phi i64 [ %R17, %subblock_4005e5_1 ]
  %R11 = phi i64 [ %R19, %subblock_4005e5_1 ]
  ; # 4005e0: cmp    BYTE PTR [rax],0x0
  ; r14 := *r11
  %r16 = inttoptr i64 %R11 to i8*
  %R14 = load i8* %r16
  ; r15 := (bv_eq r14 0x0 :: [8])
  %R15 = icmp eq i8 %R14, 0
  ; # 4005e3: je     400639
  br i1 %R15, label %subblock_4005e0_1, label %subblock_4005e0_2
subblock_4005e0_1:
  br label %block_400639
subblock_4005e0_2:
  br label %block_4005e5
block_4005e5:
  %R18 = phi i64 [ %R13, %subblock_4005e0_2 ], [ %R10, %block_4005db ]
  %R17 = phi i64 [ %R12, %subblock_4005e0_2 ], [ %R9, %block_4005db ]
  %R16 = phi i64 [ %R11, %subblock_4005e0_2 ], [ %R10, %block_4005db ]
  ; # 4005e5: add    rax,0x1
  ; r19 := (bv_add r16 0x1 :: [64])
  %R19 = add i64 %R16, 1
  ; r20 := (trunc r19 8)
  %R20 = trunc i64 %R19 to i8
  ; # 4005e9: test   al,0x7
  ; r21 := (bv_and r20 0x7 :: [8])
  %R21 = and i8 %R20, 7
  ; r22 := (bv_eq r21 0x0 :: [8])
  %R22 = icmp eq i8 %R21, 0
  ; # 4005eb: jne    4005e0
  ; r23 := (bv_complement r22)
  %R23 = xor i1 %R22, -1
  br i1 %R23, label %subblock_4005e5_1, label %subblock_4005e5_2
subblock_4005e5_1:
  br label %block_4005e0
subblock_4005e5_2:
  br label %block_4005ed
block_4005ed:
  %R25 = phi i64 [ %R18, %subblock_4005e5_2 ], [ %R65, %block_40063d ]
  %R24 = phi i64 [ %R19, %subblock_4005e5_2 ], [ %R65, %block_40063d ]
  ; # 4005ed: mov    rdx,QWORD PTR [rax]
  ; r26 := *r24
  %r29 = inttoptr i64 %R24 to i64*
  %R26 = load i64* %r29
  ; # 4005f0: mov    r8,0xfefefefefefefeff
  ; # 4005fa: mov    rsi,0x8080808080808080
  ; # 400604: lea    rcx,[rdx+r8*1]
  ; r27 := (bv_add r26 0xfefefefefefefeff :: [64])
  %R27 = add i64 %R26, 18374403900871474943
  ; # 400608: not    rdx
  ; r28 := (bv_complement r26)
  %R28 = xor i64 %R26, -1
  ; # 40060b: and    rdx,rcx
  ; r29 := (bv_and r28 r27)
  %R29 = and i64 %R28, %R27
  ; # 40060e: test   rdx,rsi
  ; r30 := (bv_and r29 0x8080808080808080 :: [64])
  %R30 = and i64 %R29, 9259542123273814144
  ; r31 := (bv_eq r30 0x0 :: [64])
  %R31 = icmp eq i64 %R30, 0
  ; # 400611: jne    400634
  ; r32 := (bv_complement r31)
  %R32 = xor i1 %R31, -1
  br i1 %R32, label %subblock_4005ed_1, label %subblock_4005ed_2
subblock_4005ed_1:
  br label %block_400634
subblock_4005ed_2:
  br label %block_400613
block_400613:
  %R36 = phi i64 [ 18374403900871474943, %subblock_4005ed_2 ]
  %R35 = phi i64 [ %R25, %subblock_4005ed_2 ]
  %R34 = phi i64 [ 9259542123273814144, %subblock_4005ed_2 ]
  %R33 = phi i64 [ %R24, %subblock_4005ed_2 ]
  ; # 400613: nop    [rax+rax*1]
  br label %block_400618
block_400618:
  %R40 = phi i64 [ %R40, %subblock_400618_1 ], [ %R36, %block_400613 ]
  %R39 = phi i64 [ %R39, %subblock_400618_1 ], [ %R35, %block_400613 ]
  %R38 = phi i64 [ %R38, %subblock_400618_1 ], [ %R34, %block_400613 ]
  %R37 = phi i64 [ %R41, %subblock_400618_1 ], [ %R33, %block_400613 ]
  ; # 400618: add    rax,0x8
  ; r41 := (bv_add r37 0x8 :: [64])
  %R41 = add i64 %R37, 8
  ; # 40061c: mov    rdx,QWORD PTR [rax]
  ; r42 := *r41
  %r46 = inttoptr i64 %R41 to i64*
  %R42 = load i64* %r46
  ; # 40061f: lea    rcx,[rdx+r8*1]
  ; r43 := (bv_add r42 r40)
  %R43 = add i64 %R42, %R40
  ; # 400623: not    rdx
  ; r44 := (bv_complement r42)
  %R44 = xor i64 %R42, -1
  ; # 400626: and    rdx,rcx
  ; r45 := (bv_and r44 r43)
  %R45 = and i64 %R44, %R43
  ; # 400629: test   rdx,rsi
  ; r46 := (bv_and r45 r38)
  %R46 = and i64 %R45, %R38
  ; r47 := (bv_eq r46 0x0 :: [64])
  %R47 = icmp eq i64 %R46, 0
  ; # 40062c: je     400618
  br i1 %R47, label %subblock_400618_1, label %subblock_400618_2
subblock_400618_1:
  br label %block_400618
subblock_400618_2:
  br label %block_40062e
block_40062e:
  %R50 = phi i64 [ %R39, %subblock_400618_2 ]
  %R49 = phi i64 [ %R45, %subblock_400618_2 ]
  %R48 = phi i64 [ %R41, %subblock_400618_2 ]
  ; # 40062e: jmp    400634
  br label %block_400634
block_400630:
  %R53 = phi i64 [ %R57, %subblock_400634_1 ]
  %R52 = phi i64 [ %R56, %subblock_400634_1 ]
  %R51 = phi i64 [ %R55, %subblock_400634_1 ]
  ; # 400630: add    rax,0x1
  ; r54 := (bv_add r51 0x1 :: [64])
  %R54 = add i64 %R51, 1
  br label %block_400634
block_400634:
  %R57 = phi i64 [ %R50, %block_40062e ], [ %R53, %block_400630 ], [ %R25, %subblock_4005ed_1 ]
  %R56 = phi i64 [ %R49, %block_40062e ], [ %R52, %block_400630 ], [ %R29, %subblock_4005ed_1 ]
  %R55 = phi i64 [ %R48, %block_40062e ], [ %R54, %block_400630 ], [ %R24, %subblock_4005ed_1 ]
  ; # 400634: cmp    BYTE PTR [rax],0x0
  ; r58 := *r55
  %r63 = inttoptr i64 %R55 to i8*
  %R58 = load i8* %r63
  ; r59 := (bv_eq r58 0x0 :: [8])
  %R59 = icmp eq i8 %R58, 0
  ; # 400637: jne    400630
  ; r60 := (bv_complement r59)
  %R60 = xor i1 %R59, -1
  br i1 %R60, label %subblock_400634_1, label %subblock_400634_2
subblock_400634_1:
  br label %block_400630
subblock_400634_2:
  br label %block_400639
block_400639:
  %R63 = phi i64 [ %R13, %subblock_4005e0_1 ], [ %R57, %subblock_400634_2 ]
  %R62 = phi i64 [ %R12, %subblock_4005e0_1 ], [ %R56, %subblock_400634_2 ]
  %R61 = phi i64 [ %R11, %subblock_4005e0_1 ], [ %R55, %subblock_400634_2 ]
  ; # 400639: sub    rax,rdi
  ; r64 := (bv_sub r61 r63)
  %R64 = sub i64 %R61, %R63
  ; # 40063c: ret
  %r71 = insertvalue { i64, i64, <2 x double> } undef, i64 %R64, 0
  %r72 = insertvalue { i64, i64, <2 x double> } %r71, i64 %R62, 1
  %r73 = insertvalue { i64, i64, <2 x double> } %r72, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r73
block_40063d:
  %R65 = phi i64 [ %a0, %subblock_4005d0_1 ]
  ; # 40063d: mov    rax,rdi
  ; # 400640: jmp    4005ed
  br label %block_4005ed
block_400642:
  %R66 = phi i64 [ %R5, %subblock_4005d6_1 ]
  ; # 400642: xor    eax,eax
  ; # 400644: ret
  %r76 = insertvalue { i64, i64, <2 x double> } undef, i64 0, 0
  %r77 = insertvalue { i64, i64, <2 x double> } %r76, i64 %R66, 1
  %r78 = insertvalue { i64, i64, <2 x double> } %r77, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r78
failure:
  br label %failure
}