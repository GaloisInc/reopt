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
declare { i64, i64, <2 x double> } @F4021a0(i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4029ad(i64 %a0, i64 %a1, i64 %a2, i64 %a3, <2 x double> %a4) {
entry:
  %r0 = bitcast <2 x double> %a4 to i128
  br label %block_4029ad
block_4029ad:
  ; r0 := (alloca 0x20 :: [64])
  %r1 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 4029ad: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4029ae: push   rbx
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 4029af: mov    esi,0x2b
  ; # 4029b4: push   rcx
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; *(r4) = arg3
  %r7 = inttoptr i64 %R4 to i64*
  store i64 %a3, i64* %r7
  ; # 4029b5: mov    rbp,rdi
  ; # 4029b8: mov    ebx,0x2
  ; # 4029bd: call   4021a0
  %r8 = bitcast i128 %r0 to <2 x double>
  %r9 = call { i64, i64, <2 x double> } @F4021a0(i64 %a0, i64 43, <2 x double> %r8)
  %R5 = extractvalue { i64, i64, <2 x double> } %r9, 0
  %R6 = extractvalue { i64, i64, <2 x double> } %r9, 1
  %r12 = extractvalue { i64, i64, <2 x double> } %r9, 2
  %R7 = bitcast <2 x double> %r12 to i128
  br label %block_4029c2
block_4029c2:
  %R11 = phi i128 [ %R7, %block_4029ad ]
  %R10 = phi i64 [ %a0, %block_4029ad ]
  %R9 = phi i64 [ 2, %block_4029ad ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R8 = phi i64 [ undef, %block_4029ad ]
  ; # 4029c2: test   rax,rax
  ; r12 := (bv_eq r8 0x0 :: [64])
  %R12 = icmp eq i64 %R8, 0
  ; # 4029c5: jne    4029d0
  ; r13 := (bv_complement r12)
  %R13 = xor i1 %R12, -1
  br i1 %R13, label %subblock_4029c2_1, label %subblock_4029c2_2
subblock_4029c2_1:
  br label %block_4029d0
subblock_4029c2_2:
  br label %block_4029c7
block_4029c7:
  %R15 = phi i128 [ %R11, %subblock_4029c2_2 ]
  %R14 = phi i64 [ %R10, %subblock_4029c2_2 ]
  ; # 4029c7: xor    ebx,ebx
  ; # 4029c9: cmp    BYTE PTR [rbp],0x72
  ; r16 := *r14
  %r22 = inttoptr i64 %R14 to i8*
  %R16 = load i8* %r22
  ; r17 := (bv_eq r16 0x72 :: [8])
  %R17 = icmp eq i8 %R16, 114
  ; # 4029cd: setne  bl
  ; r18 := (mux r17 0x0 :: [8] 0x1 :: [8])
  %R18 = select i1 %R17, i8 0, i8 1
  ; r19 := (uext r18 64)
  %R19 = zext i8 %R18 to i64
  br label %block_4029d0
block_4029d0:
  %R22 = phi i128 [ %R15, %block_4029c7 ], [ %R11, %subblock_4029c2_1 ]
  %R21 = phi i64 [ %R14, %block_4029c7 ], [ %R10, %subblock_4029c2_1 ]
  %R20 = phi i64 [ %R19, %block_4029c7 ], [ %R9, %subblock_4029c2_1 ]
  ; # 4029d0: mov    esi,0x78
  ; # 4029d5: mov    rdi,rbp
  ; # 4029d8: call   4021a0
  %r30 = bitcast i128 %R22 to <2 x double>
  %r31 = call { i64, i64, <2 x double> } @F4021a0(i64 %R21, i64 120, <2 x double> %r30)
  %R23 = extractvalue { i64, i64, <2 x double> } %r31, 0
  %R24 = extractvalue { i64, i64, <2 x double> } %r31, 1
  %r34 = extractvalue { i64, i64, <2 x double> } %r31, 2
  %R25 = bitcast <2 x double> %r34 to i128
  br label %block_4029dd
block_4029dd:
  %R29 = phi i128 [ %R25, %block_4029d0 ]
  %R28 = phi i64 [ %R21, %block_4029d0 ]
  %R27 = phi i64 [ %R20, %block_4029d0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R26 = phi i64 [ undef, %block_4029d0 ]
  ; # 4029dd: mov    edx,ebx
  ; r30 := (trunc r27 32)
  %R30 = trunc i64 %R27 to i32
  ; r31 := (uext r30 64)
  %R31 = zext i32 %R30 to i64
  ; # 4029df: mov    esi,0x65
  ; # 4029e4: mov    rdi,rbp
  ; # 4029e7: or     dl,0x80
  ; r32 := (trunc r27 8)
  %R32 = trunc i64 %R27 to i8
  ; r33 := (bv_or r32 0x80 :: [8])
  %R33 = or i8 %R32, 128
  ; r34 := (bv_and r31 0xffffffffffffff00 :: [64])
  %R34 = and i64 %R31, 18446744073709551360
  ; r35 := (uext r33 64)
  %R35 = zext i8 %R33 to i64
  ; r36 := (bv_or r34 r35)
  %R36 = or i64 %R34, %R35
  ; # 4029ea: test   rax,rax
  ; r37 := (bv_eq r26 0x0 :: [64])
  %R37 = icmp eq i64 %R26, 0
  ; # 4029ed: cmovne ebx,edx
  ; r38 := (trunc r36 32)
  %R38 = trunc i64 %R36 to i32
  ; r39 := (mux r37 r30 r38)
  %R39 = select i1 %R37, i32 %R30, i32 %R38
  ; r40 := (uext r39 64)
  %R40 = zext i32 %R39 to i64
  ; # 4029f0: call   4021a0
  %r51 = bitcast i128 %R29 to <2 x double>
  %r52 = call { i64, i64, <2 x double> } @F4021a0(i64 %R28, i64 101, <2 x double> %r51)
  %R41 = extractvalue { i64, i64, <2 x double> } %r52, 0
  %R42 = extractvalue { i64, i64, <2 x double> } %r52, 1
  %r55 = extractvalue { i64, i64, <2 x double> } %r52, 2
  %R43 = bitcast <2 x double> %r55 to i128
  br label %block_4029f5
block_4029f5:
  %R46 = phi i64 [ %R28, %block_4029dd ]
  %R45 = phi i64 [ %R40, %block_4029dd ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R44 = phi i64 [ undef, %block_4029dd ]
  ; # 4029f5: mov    edx,ebx
  ; r47 := (trunc r45 32)
  %R47 = trunc i64 %R45 to i32
  ; # 4029f7: or     edx,0x80000
  ; r48 := (bv_or r47 0x80000 :: [32])
  %R48 = or i32 %R47, 524288
  ; # 4029fd: test   rax,rax
  ; r49 := (bv_eq r44 0x0 :: [64])
  %R49 = icmp eq i64 %R44, 0
  ; # 402a00: mov    al,BYTE PTR [rbp]
  ; r50 := *r46
  %r63 = inttoptr i64 %R46 to i8*
  %R50 = load i8* %r63
  ; r51 := (bv_and r44 0xffffffffffffff00 :: [64])
  %R51 = and i64 %R44, 18446744073709551360
  ; r52 := (uext r50 64)
  %R52 = zext i8 %R50 to i64
  ; r53 := (bv_or r51 r52)
  %R53 = or i64 %R51, %R52
  ; # 402a03: cmovne ebx,edx
  ; r54 := (mux r49 r47 r48)
  %R54 = select i1 %R49, i32 %R47, i32 %R48
  ; r55 := (uext r54 64)
  %R55 = zext i32 %R54 to i64
  ; # 402a06: cmp    al,0x72
  ; r56 := (trunc r53 8)
  %R56 = trunc i64 %R53 to i8
  ; r57 := (bv_eq r56 0x72 :: [8])
  %R57 = icmp eq i8 %R56, 114
  ; # 402a08: je     402a26
  br i1 %R57, label %subblock_4029f5_1, label %subblock_4029f5_2
subblock_4029f5_1:
  br label %block_402a26
subblock_4029f5_2:
  br label %block_402a0a
block_402a0a:
  %R59 = phi i64 [ %R55, %subblock_4029f5_2 ]
  %R58 = phi i64 [ %R53, %subblock_4029f5_2 ]
  ; # 402a0a: cmp    al,0x77
  ; r60 := (trunc r58 8)
  %R60 = trunc i64 %R58 to i8
  ; r61 := (bv_eq r60 0x77 :: [8])
  %R61 = icmp eq i8 %R60, 119
  ; # 402a0c: jne    402a16
  ; r62 := (bv_complement r61)
  %R62 = xor i1 %R61, -1
  br i1 %R62, label %subblock_402a0a_1, label %subblock_402a0a_2
subblock_402a0a_1:
  br label %block_402a16
subblock_402a0a_2:
  br label %block_402a0e
block_402a0e:
  %R63 = phi i64 [ %R59, %subblock_402a0a_2 ]
  ; # 402a0e: or     ebx,0x240
  ; r64 := (trunc r63 32)
  %R64 = trunc i64 %R63 to i32
  ; r65 := (bv_or r64 0x240 :: [32])
  %R65 = or i32 %R64, 576
  ; r66 := (uext r65 64)
  %R66 = zext i32 %R65 to i64
  ; # 402a14: jmp    402a26
  br label %block_402a26
block_402a16:
  %R68 = phi i64 [ %R59, %subblock_402a0a_1 ]
  %R67 = phi i64 [ %R58, %subblock_402a0a_1 ]
  ; # 402a16: mov    edx,ebx
  ; r69 := (trunc r68 32)
  %R69 = trunc i64 %R68 to i32
  ; # 402a18: or     ebx,0x440
  ; r70 := (bv_or r69 0x440 :: [32])
  %R70 = or i32 %R69, 1088
  ; # 402a1e: or     edx,0x40
  ; r71 := (bv_or r69 0x40 :: [32])
  %R71 = or i32 %R69, 64
  ; # 402a21: cmp    al,0x61
  ; r72 := (trunc r67 8)
  %R72 = trunc i64 %R67 to i8
  ; r73 := (bv_eq r72 0x61 :: [8])
  %R73 = icmp eq i8 %R72, 97
  ; # 402a23: cmovne ebx,edx
  ; r74 := (mux r73 r70 r71)
  %R74 = select i1 %R73, i32 %R70, i32 %R71
  ; r75 := (uext r74 64)
  %R75 = zext i32 %R74 to i64
  br label %block_402a26
block_402a26:
  %R76 = phi i64 [ %R66, %block_402a0e ], [ %R75, %block_402a16 ], [ %R55, %subblock_4029f5_1 ]
  ; # 402a26: mov    eax,ebx
  ; r77 := (trunc r76 32)
  %R77 = trunc i64 %R76 to i32
  ; r78 := (uext r77 64)
  %R78 = zext i32 %R77 to i64
  ; # 402a28: pop    rdx
  ; # 402a29: pop    rbx
  ; # 402a2a: pop    rbp
  ; # 402a2b: ret
  %r93 = insertvalue { i64, i64, <2 x double> } undef, i64 %R78, 0
  %r94 = insertvalue { i64, i64, <2 x double> } %r93, i64 undef, 1
  %r95 = insertvalue { i64, i64, <2 x double> } %r94, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r95
failure:
  br label %failure
}