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
declare { i64, i64, <2 x double> } @F4022c0(i64, i64)
define { i64, i64, <2 x double> } @F4002a0(i64 %a0, i64 %a1) {
entry:
  br label %block_4002a0
block_4002a0:
  ; r0 := (alloca 0x20 :: [64])
  %r0 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 4002a0: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4002a1: mov    rbp,rsp
  ; # 4002a4: sub    rsp,0x10
  ; r3 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R3 = add i64 %R1, 18446744073709551592
  ; # 4002a8: mov    QWORD PTR [rbp-0x8],rdi
  ; r4 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R4 = add i64 %R1, 18446744073709551600
  ; *(r4) = arg0
  %r6 = inttoptr i64 %R4 to i64*
  store i64 %a0, i64* %r6
  ; # 4002ac: mov    QWORD PTR [rbp-0x10],rsi
  ; *(r3) = arg1
  %r7 = inttoptr i64 %R3 to i64*
  store i64 %a1, i64* %r7
  ; # 4002b0: cmp    QWORD PTR [rbp-0x8],0x0
  ; r5 := *r4
  %r8 = inttoptr i64 %R4 to i64*
  %R5 = load i64* %r8
  ; r6 := (bv_eq r5 0x0 :: [64])
  %R6 = icmp eq i64 %R5, 0
  ; # 4002b5: jne    4002c4
  ; r7 := (bv_complement r6)
  %R7 = xor i1 %R6, -1
  br i1 %R7, label %subblock_4002a0_1, label %subblock_4002a0_2
subblock_4002a0_1:
  br label %block_4002c4
subblock_4002a0_2:
  br label %block_4002b7
block_4002b7:
  %R9 = phi i64 [ %R2, %subblock_4002a0_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %R8 = phi i64 [ undef, %subblock_4002a0_2 ]
  ; # 4002b7: cmp    QWORD PTR [rbp-0x10],0x0
  ; r10 := (bv_add r9 0xfffffffffffffff0 :: [64])
  %R10 = add i64 %R9, 18446744073709551600
  ; r11 := *r10
  %r15 = inttoptr i64 %R10 to i64*
  %R11 = load i64* %r15
  ; r12 := (bv_eq r11 0x0 :: [64])
  %R12 = icmp eq i64 %R11, 0
  ; # 4002bc: sete   al
  ; r13 := (mux r12 0x1 :: [8] 0x0 :: [8])
  %R13 = select i1 %R12, i8 1, i8 0
  ; r14 := (bv_and r8 0xffffffffffffff00 :: [64])
  %R14 = and i64 %R8, 18446744073709551360
  ; r15 := (uext r13 64)
  %R15 = zext i8 %R13 to i64
  ; r16 := (bv_or r14 r15)
  %R16 = or i64 %R14, %R15
  ; # 4002bf: movzx  eax,al
  ; r17 := (trunc r16 8)
  %R17 = trunc i64 %R16 to i8
  ; r18 := (uext r17 64)
  %R18 = zext i8 %R17 to i64
  ; # 4002c2: jmp    400333
  br label %block_400333
block_4002c4:
  %R19 = phi i64 [ %R2, %subblock_4002a0_1 ]
  ; # 4002c4: cmp    QWORD PTR [rbp-0x10],0x0
  ; r20 := (bv_add r19 0xfffffffffffffff0 :: [64])
  %R20 = add i64 %R19, 18446744073709551600
  ; r21 := *r20
  %r26 = inttoptr i64 %R20 to i64*
  %R21 = load i64* %r26
  ; r22 := (bv_eq r21 0x0 :: [64])
  %R22 = icmp eq i64 %R21, 0
  ; # 4002c9: je     40032d
  br i1 %R22, label %subblock_4002c4_1, label %subblock_4002c4_2
subblock_4002c4_1:
  br label %block_40032d
subblock_4002c4_2:
  br label %block_4002cb
block_4002cb:
  %R23 = phi i64 [ %R19, %subblock_4002c4_2 ]
  ; # 4002cb: mov    rax,QWORD PTR [rbp-0x10]
  ; r24 := (bv_add r23 0xfffffffffffffff0 :: [64])
  %R24 = add i64 %R23, 18446744073709551600
  ; r25 := *r24
  %r31 = inttoptr i64 %R24 to i64*
  %R25 = load i64* %r31
  ; # 4002cf: mov    rdx,QWORD PTR [rax+0x10]
  ; r26 := (bv_add r25 0x10 :: [64])
  %R26 = add i64 %R25, 16
  ; r27 := *r26
  %r34 = inttoptr i64 %R26 to i64*
  %R27 = load i64* %r34
  ; # 4002d3: mov    rax,QWORD PTR [rbp-0x8]
  ; r28 := (bv_add r23 0xfffffffffffffff8 :: [64])
  %R28 = add i64 %R23, 18446744073709551608
  ; r29 := *r28
  %r37 = inttoptr i64 %R28 to i64*
  %R29 = load i64* %r37
  ; # 4002d7: mov    rax,QWORD PTR [rax+0x10]
  ; r30 := (bv_add r29 0x10 :: [64])
  %R30 = add i64 %R29, 16
  ; r31 := *r30
  %r40 = inttoptr i64 %R30 to i64*
  %R31 = load i64* %r40
  ; # 4002db: mov    rsi,rdx
  ; # 4002de: mov    rdi,rax
  ; # 4002e1: call   4022c0
  %r42 = call { i64, i64, <2 x double> } @F4022c0(i64 %R31, i64 %R27)
  %R32 = extractvalue { i64, i64, <2 x double> } %r42, 0
  br label %block_4002e6
block_4002e6:
  %R34 = phi i64 [ %R23, %block_4002cb ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R33 = phi i64 [ undef, %block_4002cb ]
  ; # 4002e6: test   eax,eax
  ; r35 := (trunc r33 32)
  %R35 = trunc i64 %R33 to i32
  ; r36 := (bv_eq r35 0x0 :: [32])
  %R36 = icmp eq i32 %R35, 0
  ; # 4002e8: jne    40032d
  ; r37 := (bv_complement r36)
  %R37 = xor i1 %R36, -1
  br i1 %R37, label %subblock_4002e6_1, label %subblock_4002e6_2
subblock_4002e6_1:
  br label %block_40032d
subblock_4002e6_2:
  br label %block_4002ea
block_4002ea:
  %R38 = phi i64 [ %R34, %subblock_4002e6_2 ]
  ; # 4002ea: mov    rax,QWORD PTR [rbp-0x10]
  ; r39 := (bv_add r38 0xfffffffffffffff0 :: [64])
  %R39 = add i64 %R38, 18446744073709551600
  ; r40 := *r39
  %r51 = inttoptr i64 %R39 to i64*
  %R40 = load i64* %r51
  ; # 4002ee: mov    rdx,QWORD PTR [rax]
  ; r41 := *r40
  %r53 = inttoptr i64 %R40 to i64*
  %R41 = load i64* %r53
  ; # 4002f1: mov    rax,QWORD PTR [rbp-0x8]
  ; r42 := (bv_add r38 0xfffffffffffffff8 :: [64])
  %R42 = add i64 %R38, 18446744073709551608
  ; r43 := *r42
  %r56 = inttoptr i64 %R42 to i64*
  %R43 = load i64* %r56
  ; # 4002f5: mov    rax,QWORD PTR [rax]
  ; r44 := *r43
  %r58 = inttoptr i64 %R43 to i64*
  %R44 = load i64* %r58
  ; # 4002f8: mov    rsi,rdx
  ; # 4002fb: mov    rdi,rax
  ; # 4002fe: call   4002a0
  %r60 = call { i64, i64, <2 x double> } @F4002a0(i64 %R44, i64 %R41)
  %R45 = extractvalue { i64, i64, <2 x double> } %r60, 0
  br label %block_400303
block_400303:
  %R47 = phi i64 [ %R38, %block_4002ea ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R46 = phi i64 [ undef, %block_4002ea ]
  ; # 400303: test   eax,eax
  ; r48 := (trunc r46 32)
  %R48 = trunc i64 %R46 to i32
  ; r49 := (bv_eq r48 0x0 :: [32])
  %R49 = icmp eq i32 %R48, 0
  ; # 400305: je     40032d
  br i1 %R49, label %subblock_400303_1, label %subblock_400303_2
subblock_400303_1:
  br label %block_40032d
subblock_400303_2:
  br label %block_400307
block_400307:
  %R50 = phi i64 [ %R47, %subblock_400303_2 ]
  ; # 400307: mov    rax,QWORD PTR [rbp-0x10]
  ; r51 := (bv_add r50 0xfffffffffffffff0 :: [64])
  %R51 = add i64 %R50, 18446744073709551600
  ; r52 := *r51
  %r68 = inttoptr i64 %R51 to i64*
  %R52 = load i64* %r68
  ; # 40030b: mov    rdx,QWORD PTR [rax+0x8]
  ; r53 := (bv_add r52 0x8 :: [64])
  %R53 = add i64 %R52, 8
  ; r54 := *r53
  %r71 = inttoptr i64 %R53 to i64*
  %R54 = load i64* %r71
  ; # 40030f: mov    rax,QWORD PTR [rbp-0x8]
  ; r55 := (bv_add r50 0xfffffffffffffff8 :: [64])
  %R55 = add i64 %R50, 18446744073709551608
  ; r56 := *r55
  %r74 = inttoptr i64 %R55 to i64*
  %R56 = load i64* %r74
  ; # 400313: mov    rax,QWORD PTR [rax+0x8]
  ; r57 := (bv_add r56 0x8 :: [64])
  %R57 = add i64 %R56, 8
  ; r58 := *r57
  %r77 = inttoptr i64 %R57 to i64*
  %R58 = load i64* %r77
  ; # 400317: mov    rsi,rdx
  ; # 40031a: mov    rdi,rax
  ; # 40031d: call   4002a0
  %r79 = call { i64, i64, <2 x double> } @F4002a0(i64 %R58, i64 %R54)
  %R59 = extractvalue { i64, i64, <2 x double> } %r79, 0
  br label %block_400322
block_400322:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R60 = phi i64 [ undef, %block_400307 ]
  ; # 400322: test   eax,eax
  ; r61 := (trunc r60 32)
  %R61 = trunc i64 %R60 to i32
  ; r62 := (bv_eq r61 0x0 :: [32])
  %R62 = icmp eq i32 %R61, 0
  ; # 400324: je     40032d
  br i1 %R62, label %subblock_400322_1, label %subblock_400322_2
subblock_400322_1:
  br label %block_40032d
subblock_400322_2:
  br label %block_400326
block_400326:
  ; # 400326: mov    eax,0x1
  ; # 40032b: jmp    400332
  br label %block_400332
block_40032d:
  ; # 40032d: mov    eax,0x0
  br label %block_400332
block_400332:
  %R63 = phi i64 [ 1, %block_400326 ], [ 0, %block_40032d ]
  ; # 400332: nop
  br label %block_400333
block_400333:
  %R64 = phi i64 [ %R18, %block_4002b7 ], [ %R63, %block_400332 ]
  ; # 400333: leave
  ; # 400334: ret
  %r86 = insertvalue { i64, i64, <2 x double> } undef, i64 %R64, 0
  %r87 = insertvalue { i64, i64, <2 x double> } %r86, i64 undef, 1
  %r88 = insertvalue { i64, i64, <2 x double> } %r87, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r88
failure:
  br label %failure
}