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
declare { i64, i64, <2 x double> } @F400130(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400e40(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401280(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4018b0(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401d23(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401e7c(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
define { i64, i64, <2 x double> } @F40041e(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_40041e
block_40041e:
  ; r0 := (alloca 0x40 :: [64])
  %r1 = alloca i8, i64 64
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x40 :: [64])
  %R1 = add i64 %R0, 64
  ; # 40041e: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 40041f: mov    rbp,rsp
  ; # 400422: sub    rsp,0x30
  ; # 400426: mov    QWORD PTR [rbp-0x28],rdi
  ; r3 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R3 = add i64 %R1, 18446744073709551568
  ; *(r3) = arg0
  %r6 = inttoptr i64 %R3 to i64*
  store i64 %a0, i64* %r6
  ; # 40042a: mov    QWORD PTR [rbp-0x10],0x8
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; *(r4) = 0x8 :: [64]
  %r8 = inttoptr i64 %R4 to i64*
  store i64 8, i64* %r8
  ; # 400432: mov    DWORD PTR [rbp-0x18],0x0
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; *(r5) = 0x0 :: [32]
  %r10 = inttoptr i64 %R5 to i32*
  store i32 0, i32* %r10
  ; # 400439: mov    rax,QWORD PTR [rbp-0x10]
  ; r6 := *r4
  %r11 = inttoptr i64 %R4 to i64*
  %R6 = load i64* %r11
  ; # 40043d: mov    rdi,rax
  ; # 400440: call   401280
  %r13 = bitcast i128 %r0 to <2 x double>
  %r14 = call { i64, i64, <2 x double> } @F401280(i64 %R6, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %r13)
  %R7 = extractvalue { i64, i64, <2 x double> } %r14, 0
  %R8 = extractvalue { i64, i64, <2 x double> } %r14, 1
  %r17 = extractvalue { i64, i64, <2 x double> } %r14, 2
  %R9 = bitcast <2 x double> %r17 to i128
  br label %block_400445
block_400445:
  %R17 = phi i128 [ %R9, %block_40041e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R16 = phi i64 [ undef, %block_40041e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R15 = phi i64 [ undef, %block_40041e ]
  %R14 = phi i64 [ %R8, %block_40041e ]
  %R13 = phi i64 [ %R2, %block_40041e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R12 = phi i64 [ undef, %block_40041e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R11 = phi i64 [ undef, %block_40041e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R10 = phi i64 [ undef, %block_40041e ]
  ; # 400445: mov    QWORD PTR [rbp-0x8],rax
  ; r18 := (bv_add r13 0xfffffffffffffff8 :: [64])
  %R18 = add i64 %R13, 18446744073709551608
  ; *(r18) = r10
  %r28 = inttoptr i64 %R18 to i64*
  store i64 %R10, i64* %r28
  ; # 400449: cmp    QWORD PTR [rbp-0x8],0x0
  ; r19 := *r18
  %r29 = inttoptr i64 %R18 to i64*
  %R19 = load i64* %r29
  ; r20 := (bv_eq r19 0x0 :: [64])
  %R20 = icmp eq i64 %R19, 0
  ; # 40044e: jne    40045a
  ; r21 := (bv_complement r20)
  %R21 = xor i1 %R20, -1
  br i1 %R21, label %subblock_400445_1, label %subblock_400445_2
subblock_400445_1:
  br label %block_40045a
subblock_400445_2:
  br label %block_400450
block_400450:
  %R25 = phi i128 [ %R17, %subblock_400445_2 ]
  %R24 = phi i64 [ %R14, %subblock_400445_2 ]
  %R23 = phi i64 [ %R13, %subblock_400445_2 ]
  %R22 = phi i64 [ %R12, %subblock_400445_2 ]
  ; # 400450: mov    edi,0x59
  ; # 400455: call   400130
  %r37 = bitcast i128 %R25 to <2 x double>
  %r38 = call { i64, i64, <2 x double> } @F400130(i64 89, i64 %R24, i64 %R22, <2 x double> %r37)
  %R26 = extractvalue { i64, i64, <2 x double> } %r38, 0
  %R27 = extractvalue { i64, i64, <2 x double> } %r38, 1
  %r41 = extractvalue { i64, i64, <2 x double> } %r38, 2
  %R28 = bitcast <2 x double> %r41 to i128
  br label %block_40045a
block_40045a:
  %R35 = phi i128 [ %R28, %block_400450 ], [ %R17, %subblock_400445_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R34 = phi i64 [ undef, %block_400450 ], [ %R16, %subblock_400445_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R33 = phi i64 [ undef, %block_400450 ], [ %R15, %subblock_400445_1 ]
  %R32 = phi i64 [ %R27, %block_400450 ], [ %R14, %subblock_400445_1 ]
  %R31 = phi i64 [ %R23, %block_400450 ], [ %R13, %subblock_400445_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R30 = phi i64 [ undef, %block_400450 ], [ %R12, %subblock_400445_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R29 = phi i64 [ undef, %block_400450 ], [ %R11, %subblock_400445_1 ]
  ; # 40045a: mov    DWORD PTR [rbp-0x18],0x0
  ; r36 := (bv_add r31 0xffffffffffffffe8 :: [64])
  %R36 = add i64 %R31, 18446744073709551592
  ; *(r36) = 0x0 :: [32]
  %r51 = inttoptr i64 %R36 to i32*
  store i32 0, i32* %r51
  ; # 400461: jmp    4004dc
  br label %block_4004dc
block_400463:
  %R50 = phi i128 [ %R192, %subblock_4004e8_1 ]
  %R49 = phi i128 [ %R191, %subblock_4004e8_1 ]
  %R48 = phi i128 [ %R190, %subblock_4004e8_1 ]
  %R47 = phi i128 [ %R189, %subblock_4004e8_1 ]
  %R46 = phi i128 [ %R188, %subblock_4004e8_1 ]
  %R45 = phi i128 [ %R187, %subblock_4004e8_1 ]
  %R44 = phi i128 [ %R186, %subblock_4004e8_1 ]
  %R43 = phi i128 [ %R185, %subblock_4004e8_1 ]
  %R42 = phi i64 [ %R184, %subblock_4004e8_1 ]
  %R41 = phi i64 [ %R183, %subblock_4004e8_1 ]
  %R40 = phi i64 [ %R182, %subblock_4004e8_1 ]
  %R39 = phi i64 [ %R181, %subblock_4004e8_1 ]
  %R38 = phi i64 [ %R180, %subblock_4004e8_1 ]
  %R37 = phi i64 [ %R179, %subblock_4004e8_1 ]
  ; # 400463: mov    eax,DWORD PTR [rbp-0x18]
  ; r51 := (bv_add r39 0xffffffffffffffe8 :: [64])
  %R51 = add i64 %R39, 18446744073709551592
  ; r52 := *r51
  %r67 = inttoptr i64 %R51 to i32*
  %R52 = load i32* %r67
  ; # 400466: cdqe
  ; r53 := (sext r52 64)
  %R53 = sext i32 %R52 to i64
  ; # 400468: cmp    rax,QWORD PTR [rbp-0x10]
  ; r54 := (bv_add r39 0xfffffffffffffff0 :: [64])
  %R54 = add i64 %R39, 18446744073709551600
  ; r55 := *r54
  %r71 = inttoptr i64 %R54 to i64*
  %R55 = load i64* %r71
  ; r56 := (bv_eq r53 r55)
  %R56 = icmp eq i64 %R53, %R55
  ; # 40046c: jne    40049a
  ; r57 := (bv_complement r56)
  %R57 = xor i1 %R56, -1
  br i1 %R57, label %subblock_400463_1, label %subblock_400463_2
subblock_400463_1:
  br label %block_40049a
subblock_400463_2:
  br label %block_40046e
block_40046e:
  %R62 = phi i128 [ %R43, %subblock_400463_2 ]
  %R61 = phi i64 [ %R42, %subblock_400463_2 ]
  %R60 = phi i64 [ %R41, %subblock_400463_2 ]
  %R59 = phi i64 [ %R39, %subblock_400463_2 ]
  %R58 = phi i64 [ %R37, %subblock_400463_2 ]
  ; # 40046e: shl    QWORD PTR [rbp-0x10],1
  ; r63 := (bv_add r59 0xfffffffffffffff0 :: [64])
  %R63 = add i64 %R59, 18446744073709551600
  ; r64 := *r63
  %r81 = inttoptr i64 %R63 to i64*
  %R64 = load i64* %r81
  ; r65 := (bv_shl r64 0x1 :: [64])
  %R65 = shl i64 %R64, 1
  ; *(r63) = r65
  %r84 = inttoptr i64 %R63 to i64*
  store i64 %R65, i64* %r84
  ; # 400472: mov    rdx,QWORD PTR [rbp-0x10]
  ; r66 := *r63
  %r85 = inttoptr i64 %R63 to i64*
  %R66 = load i64* %r85
  ; # 400476: mov    rax,QWORD PTR [rbp-0x8]
  ; r67 := (bv_add r59 0xfffffffffffffff8 :: [64])
  %R67 = add i64 %R59, 18446744073709551608
  ; r68 := *r67
  %r88 = inttoptr i64 %R67 to i64*
  %R68 = load i64* %r88
  ; # 40047a: mov    rsi,rdx
  ; # 40047d: mov    rdi,rax
  ; # 400480: call   4018b0
  %r90 = bitcast i128 %R62 to <2 x double>
  %r91 = call { i64, i64, <2 x double> } @F4018b0(i64 %R68, i64 %R66, i64 %R66, i64 %R58, i64 %R60, i64 %R61, <2 x double> %r90)
  %R69 = extractvalue { i64, i64, <2 x double> } %r91, 0
  %R70 = extractvalue { i64, i64, <2 x double> } %r91, 1
  %r94 = extractvalue { i64, i64, <2 x double> } %r91, 2
  %R71 = bitcast <2 x double> %r94 to i128
  br label %block_400485
block_400485:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R86 = phi i128 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R85 = phi i128 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R84 = phi i128 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R83 = phi i128 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R82 = phi i128 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R81 = phi i128 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R80 = phi i128 [ undef, %block_40046e ]
  %R79 = phi i128 [ %R71, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R78 = phi i64 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R77 = phi i64 [ undef, %block_40046e ]
  %R76 = phi i64 [ %R70, %block_40046e ]
  %R75 = phi i64 [ %R59, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R74 = phi i64 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R73 = phi i64 [ undef, %block_40046e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R72 = phi i64 [ undef, %block_40046e ]
  ; # 400485: mov    QWORD PTR [rbp-0x8],rax
  ; r87 := (bv_add r75 0xfffffffffffffff8 :: [64])
  %R87 = add i64 %R75, 18446744073709551608
  ; *(r87) = r72
  %r112 = inttoptr i64 %R87 to i64*
  store i64 %R72, i64* %r112
  ; # 400489: cmp    QWORD PTR [rbp-0x8],0x0
  ; r88 := *r87
  %r113 = inttoptr i64 %R87 to i64*
  %R88 = load i64* %r113
  ; r89 := (bv_eq r88 0x0 :: [64])
  %R89 = icmp eq i64 %R88, 0
  ; # 40048e: jne    40049a
  ; r90 := (bv_complement r89)
  %R90 = xor i1 %R89, -1
  br i1 %R90, label %subblock_400485_1, label %subblock_400485_2
subblock_400485_1:
  br label %block_40049a
subblock_400485_2:
  br label %block_400490
block_400490:
  %R94 = phi i128 [ %R79, %subblock_400485_2 ]
  %R93 = phi i64 [ %R76, %subblock_400485_2 ]
  %R92 = phi i64 [ %R75, %subblock_400485_2 ]
  %R91 = phi i64 [ %R74, %subblock_400485_2 ]
  ; # 400490: mov    edi,0x62
  ; # 400495: call   400130
  %r121 = bitcast i128 %R94 to <2 x double>
  %r122 = call { i64, i64, <2 x double> } @F400130(i64 98, i64 %R93, i64 %R91, <2 x double> %r121)
  %R95 = extractvalue { i64, i64, <2 x double> } %r122, 0
  %R96 = extractvalue { i64, i64, <2 x double> } %r122, 1
  %r125 = extractvalue { i64, i64, <2 x double> } %r122, 2
  %R97 = bitcast <2 x double> %r125 to i128
  br label %block_40049a
block_40049a:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R111 = phi i128 [ undef, %block_400490 ], [ %R86, %subblock_400485_1 ], [ %R50, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R110 = phi i128 [ undef, %block_400490 ], [ %R85, %subblock_400485_1 ], [ %R49, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R109 = phi i128 [ undef, %block_400490 ], [ %R84, %subblock_400485_1 ], [ %R48, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R108 = phi i128 [ undef, %block_400490 ], [ %R83, %subblock_400485_1 ], [ %R47, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R107 = phi i128 [ undef, %block_400490 ], [ %R82, %subblock_400485_1 ], [ %R46, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R106 = phi i128 [ undef, %block_400490 ], [ %R81, %subblock_400485_1 ], [ %R45, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R105 = phi i128 [ undef, %block_400490 ], [ %R80, %subblock_400485_1 ], [ %R44, %subblock_400463_1 ]
  %R104 = phi i128 [ %R97, %block_400490 ], [ %R79, %subblock_400485_1 ], [ %R43, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R103 = phi i64 [ undef, %block_400490 ], [ %R78, %subblock_400485_1 ], [ %R42, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R102 = phi i64 [ undef, %block_400490 ], [ %R77, %subblock_400485_1 ], [ %R41, %subblock_400463_1 ]
  %R101 = phi i64 [ %R96, %block_400490 ], [ %R76, %subblock_400485_1 ], [ %R40, %subblock_400463_1 ]
  %R100 = phi i64 [ %R92, %block_400490 ], [ %R75, %subblock_400485_1 ], [ %R39, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R99 = phi i64 [ undef, %block_400490 ], [ %R74, %subblock_400485_1 ], [ %R38, %subblock_400463_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R98 = phi i64 [ undef, %block_400490 ], [ %R73, %subblock_400485_1 ], [ %R37, %subblock_400463_1 ]
  ; # 40049a: mov    rax,QWORD PTR [rbp-0x28]
  ; r112 := (bv_add r100 0xffffffffffffffd8 :: [64])
  %R112 = add i64 %R100, 18446744073709551576
  ; r113 := *r112
  %r142 = inttoptr i64 %R112 to i64*
  %R113 = load i64* %r142
  ; # 40049e: mov    rdi,rax
  ; # 4004a1: call   401e7c
  %r144 = bitcast i128 %R104 to <2 x double>
  %r145 = bitcast i128 %R105 to <2 x double>
  %r146 = bitcast i128 %R106 to <2 x double>
  %r147 = bitcast i128 %R107 to <2 x double>
  %r148 = bitcast i128 %R108 to <2 x double>
  %r149 = bitcast i128 %R109 to <2 x double>
  %r150 = bitcast i128 %R110 to <2 x double>
  %r151 = bitcast i128 %R111 to <2 x double>
  %r152 = call { i64, i64, <2 x double> } @F401e7c(i64 %R113, i64 %R101, i64 %R99, i64 %R98, i64 %R102, i64 %R103, <2 x double> %r144, <2 x double> %r145, <2 x double> %r146, <2 x double> %r147, <2 x double> %r148, <2 x double> %r149, <2 x double> %r150, <2 x double> %r151)
  %R114 = extractvalue { i64, i64, <2 x double> } %r152, 0
  %R115 = extractvalue { i64, i64, <2 x double> } %r152, 1
  %r155 = extractvalue { i64, i64, <2 x double> } %r152, 2
  %R116 = bitcast <2 x double> %r155 to i128
  br label %block_4004a6
block_4004a6:
  %R124 = phi i128 [ %R116, %block_40049a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R123 = phi i64 [ undef, %block_40049a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R122 = phi i64 [ undef, %block_40049a ]
  %R121 = phi i64 [ %R115, %block_40049a ]
  %R120 = phi i64 [ %R100, %block_40049a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R119 = phi i64 [ undef, %block_40049a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R118 = phi i64 [ undef, %block_40049a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R117 = phi i64 [ undef, %block_40049a ]
  ; # 4004a6: mov    DWORD PTR [rbp-0x14],eax
  ; r125 := (trunc r117 32)
  %R125 = trunc i64 %R117 to i32
  ; r126 := (bv_add r120 0xffffffffffffffec :: [64])
  %R126 = add i64 %R120, 18446744073709551596
  ; *(r126) = r125
  %r167 = inttoptr i64 %R126 to i32*
  store i32 %R125, i32* %r167
  ; # 4004a9: cmp    DWORD PTR [rbp-0x14],0xffffffff
  ; r127 := *r126
  %r168 = inttoptr i64 %R126 to i32*
  %R127 = load i32* %r168
  ; r128 := (bv_eq r127 0xffffffff :: [32])
  %R128 = icmp eq i32 %R127, 4294967295
  ; # 4004ad: jne    4004b9
  ; r129 := (bv_complement r128)
  %R129 = xor i1 %R128, -1
  br i1 %R129, label %subblock_4004a6_1, label %subblock_4004a6_2
subblock_4004a6_1:
  br label %block_4004b9
subblock_4004a6_2:
  br label %block_4004af
block_4004af:
  %R133 = phi i128 [ %R124, %subblock_4004a6_2 ]
  %R132 = phi i64 [ %R121, %subblock_4004a6_2 ]
  %R131 = phi i64 [ %R120, %subblock_4004a6_2 ]
  %R130 = phi i64 [ %R119, %subblock_4004a6_2 ]
  ; # 4004af: mov    edi,0x69
  ; # 4004b4: call   400130
  %r176 = bitcast i128 %R133 to <2 x double>
  %r177 = call { i64, i64, <2 x double> } @F400130(i64 105, i64 %R132, i64 %R130, <2 x double> %r176)
  %R134 = extractvalue { i64, i64, <2 x double> } %r177, 0
  %R135 = extractvalue { i64, i64, <2 x double> } %r177, 1
  %r180 = extractvalue { i64, i64, <2 x double> } %r177, 2
  %R136 = bitcast <2 x double> %r180 to i128
  br label %block_4004b9
block_4004b9:
  %R143 = phi i128 [ %R136, %block_4004af ], [ %R124, %subblock_4004a6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R142 = phi i64 [ undef, %block_4004af ], [ %R123, %subblock_4004a6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R141 = phi i64 [ undef, %block_4004af ], [ %R122, %subblock_4004a6_1 ]
  %R140 = phi i64 [ %R135, %block_4004af ], [ %R121, %subblock_4004a6_1 ]
  %R139 = phi i64 [ %R131, %block_4004af ], [ %R120, %subblock_4004a6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R138 = phi i64 [ undef, %block_4004af ], [ %R119, %subblock_4004a6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R137 = phi i64 [ undef, %block_4004af ], [ %R118, %subblock_4004a6_1 ]
  ; # 4004b9: mov    eax,0x2c
  ; # 4004be: movsx  eax,al
  ; r144 := (sext 0x2c :: [8] 32)
  %R144 = sext i8 44 to i32
  ; # 4004c1: cmp    eax,DWORD PTR [rbp-0x14]
  ; r145 := (bv_add r139 0xffffffffffffffec :: [64])
  %R145 = add i64 %R139, 18446744073709551596
  ; r146 := *r145
  %r191 = inttoptr i64 %R145 to i32*
  %R146 = load i32* %r191
  ; r147 := (bv_eq r144 r146)
  %R147 = icmp eq i32 %R144, %R146
  ; # 4004c4: je     4004f2
  br i1 %R147, label %subblock_4004b9_1, label %subblock_4004b9_2
subblock_4004b9_1:
  br label %block_4004f2
subblock_4004b9_2:
  br label %block_4004c6
block_4004c6:
  %R153 = phi i128 [ %R143, %subblock_4004b9_2 ]
  %R152 = phi i64 [ %R142, %subblock_4004b9_2 ]
  %R151 = phi i64 [ %R141, %subblock_4004b9_2 ]
  %R150 = phi i64 [ %R140, %subblock_4004b9_2 ]
  %R149 = phi i64 [ %R139, %subblock_4004b9_2 ]
  %R148 = phi i64 [ %R137, %subblock_4004b9_2 ]
  ; # 4004c6: mov    eax,DWORD PTR [rbp-0x18]
  ; r154 := (bv_add r149 0xffffffffffffffe8 :: [64])
  %R154 = add i64 %R149, 18446744073709551592
  ; r155 := *r154
  %r201 = inttoptr i64 %R154 to i32*
  %R155 = load i32* %r201
  ; # 4004c9: movsxd rdx,eax
  ; r156 := (sext r155 64)
  %R156 = sext i32 %R155 to i64
  ; # 4004cc: mov    rax,QWORD PTR [rbp-0x8]
  ; r157 := (bv_add r149 0xfffffffffffffff8 :: [64])
  %R157 = add i64 %R149, 18446744073709551608
  ; r158 := *r157
  %r205 = inttoptr i64 %R157 to i64*
  %R158 = load i64* %r205
  ; # 4004d0: add    rax,rdx
  ; r159 := (bv_add r158 r156)
  %R159 = add i64 %R158, %R156
  ; # 4004d3: mov    edx,DWORD PTR [rbp-0x14]
  ; r160 := (bv_add r149 0xffffffffffffffec :: [64])
  %R160 = add i64 %R149, 18446744073709551596
  ; r161 := *r160
  %r209 = inttoptr i64 %R160 to i32*
  %R161 = load i32* %r209
  ; r162 := (uext r161 64)
  %R162 = zext i32 %R161 to i64
  ; # 4004d6: mov    BYTE PTR [rax],dl
  ; r163 := (trunc r161 8)
  %R163 = trunc i32 %R161 to i8
  ; *(r159) = r163
  %r213 = inttoptr i64 %R159 to i8*
  store i8 %R163, i8* %r213
  ; # 4004d8: add    DWORD PTR [rbp-0x18],0x1
  ; r164 := *r154
  %r214 = inttoptr i64 %R154 to i32*
  %R164 = load i32* %r214
  ; r165 := (bv_add r164 0x1 :: [32])
  %R165 = add i32 %R164, 1
  ; *(r154) = r165
  %r217 = inttoptr i64 %R154 to i32*
  store i32 %R165, i32* %r217
  br label %block_4004dc
block_4004dc:
  %R172 = phi i128 [ %R153, %block_4004c6 ], [ %R35, %block_40045a ]
  %R171 = phi i64 [ %R152, %block_4004c6 ], [ %R34, %block_40045a ]
  %R170 = phi i64 [ %R151, %block_4004c6 ], [ %R33, %block_40045a ]
  %R169 = phi i64 [ %R150, %block_4004c6 ], [ %R32, %block_40045a ]
  %R168 = phi i64 [ %R149, %block_4004c6 ], [ %R31, %block_40045a ]
  %R167 = phi i64 [ %R162, %block_4004c6 ], [ %R30, %block_40045a ]
  %R166 = phi i64 [ %R148, %block_4004c6 ], [ %R29, %block_40045a ]
  ; # 4004dc: mov    rax,QWORD PTR [rbp-0x28]
  ; r173 := (bv_add r168 0xffffffffffffffd8 :: [64])
  %R173 = add i64 %R168, 18446744073709551576
  ; r174 := *r173
  %r226 = inttoptr i64 %R173 to i64*
  %R174 = load i64* %r226
  ; # 4004e0: mov    rdi,rax
  ; # 4004e3: call   401d23
  %r228 = bitcast i128 %R172 to <2 x double>
  %r229 = call { i64, i64, <2 x double> } @F401d23(i64 %R174, i64 %R169, i64 %R167, i64 %R166, i64 %R170, i64 %R171, <2 x double> %r228)
  %R175 = extractvalue { i64, i64, <2 x double> } %r229, 0
  %R176 = extractvalue { i64, i64, <2 x double> } %r229, 1
  %r232 = extractvalue { i64, i64, <2 x double> } %r229, 2
  %R177 = bitcast <2 x double> %r232 to i128
  br label %block_4004e8
block_4004e8:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R192 = phi i128 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R191 = phi i128 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R190 = phi i128 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R189 = phi i128 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R188 = phi i128 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R187 = phi i128 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R186 = phi i128 [ undef, %block_4004dc ]
  %R185 = phi i128 [ %R177, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R184 = phi i64 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R183 = phi i64 [ undef, %block_4004dc ]
  %R182 = phi i64 [ %R176, %block_4004dc ]
  %R181 = phi i64 [ %R168, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R180 = phi i64 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R179 = phi i64 [ undef, %block_4004dc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R178 = phi i64 [ undef, %block_4004dc ]
  ; # 4004e8: test   eax,eax
  ; r193 := (trunc r178 32)
  %R193 = trunc i64 %R178 to i32
  ; r194 := (bv_eq r193 0x0 :: [32])
  %R194 = icmp eq i32 %R193, 0
  ; # 4004ea: je     400463
  br i1 %R194, label %subblock_4004e8_1, label %subblock_4004e8_2
subblock_4004e8_1:
  br label %block_400463
subblock_4004e8_2:
  br label %block_4004f0
block_4004f0:
  %R201 = phi i128 [ %R185, %subblock_4004e8_2 ]
  %R200 = phi i64 [ %R184, %subblock_4004e8_2 ]
  %R199 = phi i64 [ %R183, %subblock_4004e8_2 ]
  %R198 = phi i64 [ %R182, %subblock_4004e8_2 ]
  %R197 = phi i64 [ %R181, %subblock_4004e8_2 ]
  %R196 = phi i64 [ %R180, %subblock_4004e8_2 ]
  %R195 = phi i64 [ %R179, %subblock_4004e8_2 ]
  ; # 4004f0: jmp    4004f3
  br label %block_4004f3
block_4004f2:
  %R208 = phi i128 [ %R143, %subblock_4004b9_1 ]
  %R207 = phi i64 [ %R142, %subblock_4004b9_1 ]
  %R206 = phi i64 [ %R141, %subblock_4004b9_1 ]
  %R205 = phi i64 [ %R140, %subblock_4004b9_1 ]
  %R204 = phi i64 [ %R139, %subblock_4004b9_1 ]
  %R203 = phi i64 [ %R138, %subblock_4004b9_1 ]
  %R202 = phi i64 [ %R137, %subblock_4004b9_1 ]
  ; # 4004f2: nop
  br label %block_4004f3
block_4004f3:
  %R215 = phi i128 [ %R208, %block_4004f2 ], [ %R201, %block_4004f0 ]
  %R214 = phi i64 [ %R207, %block_4004f2 ], [ %R200, %block_4004f0 ]
  %R213 = phi i64 [ %R206, %block_4004f2 ], [ %R199, %block_4004f0 ]
  %R212 = phi i64 [ %R205, %block_4004f2 ], [ %R198, %block_4004f0 ]
  %R211 = phi i64 [ %R204, %block_4004f2 ], [ %R197, %block_4004f0 ]
  %R210 = phi i64 [ %R203, %block_4004f2 ], [ %R196, %block_4004f0 ]
  %R209 = phi i64 [ %R202, %block_4004f2 ], [ %R195, %block_4004f0 ]
  ; # 4004f3: cmp    DWORD PTR [rbp-0x18],0x0
  ; r216 := (bv_add r211 0xffffffffffffffe8 :: [64])
  %R216 = add i64 %R211, 18446744073709551592
  ; r217 := *r216
  %r273 = inttoptr i64 %R216 to i32*
  %R217 = load i32* %r273
  ; r218 := (bv_eq r217 0x0 :: [32])
  %R218 = icmp eq i32 %R217, 0
  ; # 4004f7: jne    40050c
  ; r219 := (bv_complement r218)
  %R219 = xor i1 %R218, -1
  br i1 %R219, label %subblock_4004f3_1, label %subblock_4004f3_2
subblock_4004f3_1:
  br label %block_40050c
subblock_4004f3_2:
  br label %block_4004f9
block_4004f9:
  %R226 = phi i128 [ %R215, %subblock_4004f3_2 ]
  %R225 = phi i64 [ %R214, %subblock_4004f3_2 ]
  %R224 = phi i64 [ %R213, %subblock_4004f3_2 ]
  %R223 = phi i64 [ %R212, %subblock_4004f3_2 ]
  %R222 = phi i64 [ %R211, %subblock_4004f3_2 ]
  %R221 = phi i64 [ %R210, %subblock_4004f3_2 ]
  %R220 = phi i64 [ %R209, %subblock_4004f3_2 ]
  ; # 4004f9: mov    rax,QWORD PTR [rbp-0x8]
  ; r227 := (bv_add r222 0xfffffffffffffff8 :: [64])
  %R227 = add i64 %R222, 18446744073709551608
  ; r228 := *r227
  %r285 = inttoptr i64 %R227 to i64*
  %R228 = load i64* %r285
  ; # 4004fd: mov    rdi,rax
  ; # 400500: call   400e40
  %r287 = bitcast i128 %R226 to <2 x double>
  %r288 = call { i64, i64, <2 x double> } @F400e40(i64 %R228, i64 %R223, i64 %R221, i64 %R220, i64 %R224, i64 %R225, <2 x double> %r287)
  %R229 = extractvalue { i64, i64, <2 x double> } %r288, 0
  %R230 = extractvalue { i64, i64, <2 x double> } %r288, 1
  %r291 = extractvalue { i64, i64, <2 x double> } %r288, 2
  %R231 = bitcast <2 x double> %r291 to i128
  br label %block_400505
block_400505:
  %R233 = phi i128 [ %R231, %block_4004f9 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R232 = phi i64 [ undef, %block_4004f9 ]
  ; # 400505: mov    eax,0x0
  ; # 40050a: jmp    400520
  br label %block_400520
block_40050c:
  %R235 = phi i128 [ %R215, %subblock_4004f3_1 ]
  %R234 = phi i64 [ %R211, %subblock_4004f3_1 ]
  ; # 40050c: mov    eax,DWORD PTR [rbp-0x18]
  ; r236 := (bv_add r234 0xffffffffffffffe8 :: [64])
  %R236 = add i64 %R234, 18446744073709551592
  ; r237 := *r236
  %r298 = inttoptr i64 %R236 to i32*
  %R237 = load i32* %r298
  ; # 40050f: movsxd rdx,eax
  ; r238 := (sext r237 64)
  %R238 = sext i32 %R237 to i64
  ; # 400512: mov    rax,QWORD PTR [rbp-0x8]
  ; r239 := (bv_add r234 0xfffffffffffffff8 :: [64])
  %R239 = add i64 %R234, 18446744073709551608
  ; r240 := *r239
  %r302 = inttoptr i64 %R239 to i64*
  %R240 = load i64* %r302
  ; # 400516: add    rax,rdx
  ; r241 := (bv_add r240 r238)
  %R241 = add i64 %R240, %R238
  ; # 400519: mov    BYTE PTR [rax],0x0
  ; *(r241) = 0x0 :: [8]
  %r305 = inttoptr i64 %R241 to i8*
  store i8 0, i8* %r305
  ; # 40051c: mov    rax,QWORD PTR [rbp-0x8]
  ; r242 := *r239
  %r306 = inttoptr i64 %R239 to i64*
  %R242 = load i64* %r306
  br label %block_400520
block_400520:
  %R245 = phi i128 [ %R233, %block_400505 ], [ %R235, %block_40050c ]
  %R244 = phi i64 [ %R232, %block_400505 ], [ %R238, %block_40050c ]
  %R243 = phi i64 [ 0, %block_400505 ], [ %R242, %block_40050c ]
  ; # 400520: leave
  ; # 400521: ret
  %r311 = bitcast i128 %R245 to <2 x double>
  %r312 = insertvalue { i64, i64, <2 x double> } undef, i64 %R243, 0
  %r313 = insertvalue { i64, i64, <2 x double> } %r312, i64 %R244, 1
  %r314 = insertvalue { i64, i64, <2 x double> } %r313, <2 x double> %r311, 2
  ret { i64, i64, <2 x double> } %r314
failure:
  br label %failure
}