declare i1 @reopt.EvenParity(i8)
declare i2 @reopt.Read_X87_RC()
declare void @reopt.Write_X87_RC(i2)
declare i2 @reopt.Read_X87_PC()
declare void @reopt.Write_X87_PC(i2)
declare i16 @reopt.Read_FS()
declare void @reopt.Write_FS(i16)
declare i16 @reopt.Read_GS()
declare void @reopt.Write_GS(i16)
declare void @reopt.MemCopy(i64, i64, i64, i64, i1)
declare i64 @reopt.MemCmp(i64, i64, i64, i64, i1)
declare { i64, i1 } @reopt.SystemCall.Linux(i64, i64, i64, i64, i64, i64, i64)
declare { i64, i1 } @reopt.SystemCall.FreeBSD(i64, i64, i64, i64, i64, i64, i64)
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
define { i64, i64, <2 x double> } @F4009b0(i64 %a0, <2 x double> %a1) {
entry:
  %r0 = bitcast <2 x double> %a1 to i128
  br label %block_4009b0
block_4009b0:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 4009b0: test   dil,0x7
  ; r2 := (trunc arg0 8)
  %R2 = trunc i64 %a0 to i8
  ; r3 := (bv_and r2 0x7 :: [8])
  %R3 = and i8 %R2, 7
  ; r4 := (bv_eq r3 0x0 :: [8])
  %R4 = icmp eq i8 %R3, 0
  ; # 4009b4: je     400a1d
  br i1 %R4, label %subblock_4009b0_1, label %subblock_4009b0_2
subblock_4009b0_1:
  br label %block_400a1d
subblock_4009b0_2:
  br label %block_4009b6
block_4009b6:
  %R6 = phi i128 [ %r0, %subblock_4009b0_2 ]
  %R5 = phi i64 [ %a0, %subblock_4009b0_2 ]
  ; # 4009b6: cmp    BYTE PTR [rdi],0x0
  ; r7 := *r5
  %r9 = inttoptr i64 %R5 to i8*
  %R7 = load i8* %r9
  ; r8 := (bv_eq r7 0x0 :: [8])
  %R8 = icmp eq i8 %R7, 0
  ; # 4009b9: mov    rax,rdi
  ; # 4009bc: jne    4009c5
  ; r9 := (bv_complement r8)
  %R9 = xor i1 %R8, -1
  br i1 %R9, label %subblock_4009b6_1, label %subblock_4009b6_2
subblock_4009b6_1:
  br label %block_4009c5
subblock_4009b6_2:
  br label %block_4009be
block_4009be:
  %R10 = phi i128 [ %R6, %subblock_4009b6_2 ]
  ; # 4009be: jmp    400a22
  br label %block_400a22
block_4009c0:
  %R13 = phi i128 [ %R18, %subblock_4009c5_1 ]
  %R12 = phi i64 [ %R17, %subblock_4009c5_1 ]
  %R11 = phi i64 [ %R19, %subblock_4009c5_1 ]
  ; # 4009c0: cmp    BYTE PTR [rax],0x0
  ; r14 := *r11
  %r17 = inttoptr i64 %R11 to i8*
  %R14 = load i8* %r17
  ; r15 := (bv_eq r14 0x0 :: [8])
  %R15 = icmp eq i8 %R14, 0
  ; # 4009c3: je     400a19
  br i1 %R15, label %subblock_4009c0_1, label %subblock_4009c0_2
subblock_4009c0_1:
  br label %block_400a19
subblock_4009c0_2:
  br label %block_4009c5
block_4009c5:
  %R18 = phi i128 [ %R13, %subblock_4009c0_2 ], [ %R6, %subblock_4009b6_1 ]
  %R17 = phi i64 [ %R12, %subblock_4009c0_2 ], [ %R5, %subblock_4009b6_1 ]
  %R16 = phi i64 [ %R11, %subblock_4009c0_2 ], [ %R5, %subblock_4009b6_1 ]
  ; # 4009c5: add    rax,0x1
  ; r19 := (bv_add r16 0x1 :: [64])
  %R19 = add i64 %R16, 1
  ; r20 := (trunc r19 8)
  %R20 = trunc i64 %R19 to i8
  ; # 4009c9: test   al,0x7
  ; r21 := (bv_and r20 0x7 :: [8])
  %R21 = and i8 %R20, 7
  ; r22 := (bv_eq r21 0x0 :: [8])
  %R22 = icmp eq i8 %R21, 0
  ; # 4009cb: jne    4009c0
  ; r23 := (bv_complement r22)
  %R23 = xor i1 %R22, -1
  br i1 %R23, label %subblock_4009c5_1, label %subblock_4009c5_2
subblock_4009c5_1:
  br label %block_4009c0
subblock_4009c5_2:
  br label %block_4009cd
block_4009cd:
  %R26 = phi i128 [ %R18, %subblock_4009c5_2 ], [ %R69, %block_400a1d ]
  %R25 = phi i64 [ %R17, %subblock_4009c5_2 ], [ %R68, %block_400a1d ]
  %R24 = phi i64 [ %R19, %subblock_4009c5_2 ], [ %R68, %block_400a1d ]
  ; # 4009cd: mov    rcx,QWORD PTR [rax]
  ; r27 := *r24
  %r31 = inttoptr i64 %R24 to i64*
  %R27 = load i64* %r31
  ; # 4009d0: mov    r8,0xfefefefefefefeff
  ; # 4009da: mov    rsi,0x8080808080808080
  ; # 4009e4: lea    rdx,[rcx+r8*1]
  ; r28 := (bv_add r27 0xfefefefefefefeff :: [64])
  %R28 = add i64 %R27, 18374403900871474943
  ; # 4009e8: not    rcx
  ; r29 := (bv_complement r27)
  %R29 = xor i64 %R27, -1
  ; # 4009eb: and    rdx,rcx
  ; r30 := (bv_and r28 r29)
  %R30 = and i64 %R28, %R29
  ; # 4009ee: test   rdx,rsi
  ; r31 := (bv_and r30 0x8080808080808080 :: [64])
  %R31 = and i64 %R30, 9259542123273814144
  ; r32 := (bv_eq r31 0x0 :: [64])
  %R32 = icmp eq i64 %R31, 0
  ; # 4009f1: jne    400a14
  ; r33 := (bv_complement r32)
  %R33 = xor i1 %R32, -1
  br i1 %R33, label %subblock_4009cd_1, label %subblock_4009cd_2
subblock_4009cd_1:
  br label %block_400a14
subblock_4009cd_2:
  br label %block_4009f3
block_4009f3:
  %R38 = phi i128 [ %R26, %subblock_4009cd_2 ]
  %R37 = phi i64 [ 18374403900871474943, %subblock_4009cd_2 ]
  %R36 = phi i64 [ %R25, %subblock_4009cd_2 ]
  %R35 = phi i64 [ 9259542123273814144, %subblock_4009cd_2 ]
  %R34 = phi i64 [ %R24, %subblock_4009cd_2 ]
  ; # 4009f3: nop    [rax+rax*1]
  br label %block_4009f8
block_4009f8:
  %R43 = phi i128 [ %R43, %subblock_4009f8_1 ], [ %R38, %block_4009f3 ]
  %R42 = phi i64 [ %R42, %subblock_4009f8_1 ], [ %R37, %block_4009f3 ]
  %R41 = phi i64 [ %R41, %subblock_4009f8_1 ], [ %R36, %block_4009f3 ]
  %R40 = phi i64 [ %R40, %subblock_4009f8_1 ], [ %R35, %block_4009f3 ]
  %R39 = phi i64 [ %R44, %subblock_4009f8_1 ], [ %R34, %block_4009f3 ]
  ; # 4009f8: add    rax,0x8
  ; r44 := (bv_add r39 0x8 :: [64])
  %R44 = add i64 %R39, 8
  ; # 4009fc: mov    rcx,QWORD PTR [rax]
  ; r45 := *r44
  %r50 = inttoptr i64 %R44 to i64*
  %R45 = load i64* %r50
  ; # 4009ff: lea    rdx,[rcx+r8*1]
  ; r46 := (bv_add r45 r42)
  %R46 = add i64 %R45, %R42
  ; # 400a03: not    rcx
  ; r47 := (bv_complement r45)
  %R47 = xor i64 %R45, -1
  ; # 400a06: and    rdx,rcx
  ; r48 := (bv_and r46 r47)
  %R48 = and i64 %R46, %R47
  ; # 400a09: test   rdx,rsi
  ; r49 := (bv_and r48 r40)
  %R49 = and i64 %R48, %R40
  ; r50 := (bv_eq r49 0x0 :: [64])
  %R50 = icmp eq i64 %R49, 0
  ; # 400a0c: je     4009f8
  br i1 %R50, label %subblock_4009f8_1, label %subblock_4009f8_2
subblock_4009f8_1:
  br label %block_4009f8
subblock_4009f8_2:
  br label %block_400a0e
block_400a0e:
  %R53 = phi i128 [ %R43, %subblock_4009f8_2 ]
  %R52 = phi i64 [ %R41, %subblock_4009f8_2 ]
  %R51 = phi i64 [ %R44, %subblock_4009f8_2 ]
  ; # 400a0e: jmp    400a14
  br label %block_400a14
block_400a10:
  %R56 = phi i128 [ %R60, %subblock_400a14_1 ]
  %R55 = phi i64 [ %R59, %subblock_400a14_1 ]
  %R54 = phi i64 [ %R58, %subblock_400a14_1 ]
  ; # 400a10: add    rax,0x1
  ; r57 := (bv_add r54 0x1 :: [64])
  %R57 = add i64 %R54, 1
  br label %block_400a14
block_400a14:
  %R60 = phi i128 [ %R53, %block_400a0e ], [ %R56, %block_400a10 ], [ %R26, %subblock_4009cd_1 ]
  %R59 = phi i64 [ %R52, %block_400a0e ], [ %R55, %block_400a10 ], [ %R25, %subblock_4009cd_1 ]
  %R58 = phi i64 [ %R51, %block_400a0e ], [ %R57, %block_400a10 ], [ %R24, %subblock_4009cd_1 ]
  ; # 400a14: cmp    BYTE PTR [rax],0x0
  ; r61 := *r58
  %r67 = inttoptr i64 %R58 to i8*
  %R61 = load i8* %r67
  ; r62 := (bv_eq r61 0x0 :: [8])
  %R62 = icmp eq i8 %R61, 0
  ; # 400a17: jne    400a10
  ; r63 := (bv_complement r62)
  %R63 = xor i1 %R62, -1
  br i1 %R63, label %subblock_400a14_1, label %subblock_400a14_2
subblock_400a14_1:
  br label %block_400a10
subblock_400a14_2:
  br label %block_400a19
block_400a19:
  %R66 = phi i128 [ %R13, %subblock_4009c0_1 ], [ %R60, %subblock_400a14_2 ]
  %R65 = phi i64 [ %R12, %subblock_4009c0_1 ], [ %R59, %subblock_400a14_2 ]
  %R64 = phi i64 [ %R11, %subblock_4009c0_1 ], [ %R58, %subblock_400a14_2 ]
  ; # 400a19: sub    rax,rdi
  ; r67 := (bv_sub r64 r65)
  %R67 = sub i64 %R64, %R65
  ; # 400a1c: ret
  %r75 = bitcast i128 %R66 to <2 x double>
  %r76 = insertvalue { i64, i64, <2 x double> } undef, i64 %R67, 0
  %r77 = insertvalue { i64, i64, <2 x double> } %r76, i64 undef, 1
  %r78 = insertvalue { i64, i64, <2 x double> } %r77, <2 x double> %r75, 2
  ret { i64, i64, <2 x double> } %r78
block_400a1d:
  %R69 = phi i128 [ %r0, %subblock_4009b0_1 ]
  %R68 = phi i64 [ %a0, %subblock_4009b0_1 ]
  ; # 400a1d: mov    rax,rdi
  ; # 400a20: jmp    4009cd
  br label %block_4009cd
block_400a22:
  %R70 = phi i128 [ %R10, %block_4009be ]
  ; # 400a22: xor    eax,eax
  ; # 400a24: ret
  %r82 = bitcast i128 %R70 to <2 x double>
  %r83 = insertvalue { i64, i64, <2 x double> } undef, i64 0, 0
  %r84 = insertvalue { i64, i64, <2 x double> } %r83, i64 undef, 1
  %r85 = insertvalue { i64, i64, <2 x double> } %r84, <2 x double> %r82, 2
  ret { i64, i64, <2 x double> } %r85
failure:
  br label %failure
}