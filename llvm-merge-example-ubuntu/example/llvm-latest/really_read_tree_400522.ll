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
declare { i64, i64, <2 x double> } @F40041e(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401280(i64, i64, i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400522(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_400522
block_400522:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 400522: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 400523: mov    rbp,rsp
  ; # 400526: sub    rsp,0x20
  ; # 40052a: mov    QWORD PTR [rbp-0x18],rdi
  ; r3 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R3 = add i64 %R1, 18446744073709551584
  ; *(r3) = arg0
  %r6 = inttoptr i64 %R3 to i64*
  store i64 %a0, i64* %r6
  ; # 40052e: mov    rax,QWORD PTR [rbp-0x18]
  ; r4 := *r3
  %r7 = inttoptr i64 %R3 to i64*
  %R4 = load i64* %r7
  ; # 400532: mov    rdi,rax
  ; # 400535: call   40041e
  %r9 = bitcast i128 %r0 to <2 x double>
  %r10 = call { i64, i64, <2 x double> } @F40041e(i64 %R4, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %r9)
  %R5 = extractvalue { i64, i64, <2 x double> } %r10, 0
  %R6 = extractvalue { i64, i64, <2 x double> } %r10, 1
  %r13 = extractvalue { i64, i64, <2 x double> } %r10, 2
  %R7 = bitcast <2 x double> %r13 to i128
  br label %block_40053a
block_40053a:
  %R15 = phi i128 [ %R7, %block_400522 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R14 = phi i64 [ undef, %block_400522 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R13 = phi i64 [ undef, %block_400522 ]
  %R12 = phi i64 [ %R6, %block_400522 ]
  %R11 = phi i64 [ %R2, %block_400522 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R10 = phi i64 [ undef, %block_400522 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R9 = phi i64 [ undef, %block_400522 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R8 = phi i64 [ undef, %block_400522 ]
  ; # 40053a: mov    QWORD PTR [rbp-0x10],rax
  ; r16 := (bv_add r11 0xfffffffffffffff0 :: [64])
  %R16 = add i64 %R11, 18446744073709551600
  ; *(r16) = r8
  %r24 = inttoptr i64 %R16 to i64*
  store i64 %R8, i64* %r24
  ; # 40053e: cmp    QWORD PTR [rbp-0x10],0x0
  ; r17 := *r16
  %r25 = inttoptr i64 %R16 to i64*
  %R17 = load i64* %r25
  ; r18 := (bv_eq r17 0x0 :: [64])
  %R18 = icmp eq i64 %R17, 0
  ; # 400543: jne    40054c
  ; r19 := (bv_complement r18)
  %R19 = xor i1 %R18, -1
  br i1 %R19, label %subblock_40053a_1, label %subblock_40053a_2
subblock_40053a_1:
  br label %block_40054c
subblock_40053a_2:
  br label %block_400545
block_400545:
  %R21 = phi i128 [ %R15, %subblock_40053a_2 ]
  %R20 = phi i64 [ %R10, %subblock_40053a_2 ]
  ; # 400545: mov    eax,0x0
  ; # 40054a: jmp    4005a8
  br label %block_4005a8
block_40054c:
  %R28 = phi i128 [ %R15, %subblock_40053a_1 ]
  %R27 = phi i64 [ %R14, %subblock_40053a_1 ]
  %R26 = phi i64 [ %R13, %subblock_40053a_1 ]
  %R25 = phi i64 [ %R12, %subblock_40053a_1 ]
  %R24 = phi i64 [ %R11, %subblock_40053a_1 ]
  %R23 = phi i64 [ %R10, %subblock_40053a_1 ]
  %R22 = phi i64 [ %R9, %subblock_40053a_1 ]
  ; # 40054c: mov    edi,0x18
  ; # 400551: call   401280
  %r38 = bitcast i128 %R28 to <2 x double>
  %r39 = call { i64, i64, <2 x double> } @F401280(i64 24, i64 %R25, i64 %R23, i64 %R22, i64 %R26, i64 %R27, <2 x double> %r38)
  %R29 = extractvalue { i64, i64, <2 x double> } %r39, 0
  %R30 = extractvalue { i64, i64, <2 x double> } %r39, 1
  %r42 = extractvalue { i64, i64, <2 x double> } %r39, 2
  %R31 = bitcast <2 x double> %r42 to i128
  br label %block_400556
block_400556:
  %R39 = phi i128 [ %R31, %block_40054c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R38 = phi i64 [ undef, %block_40054c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R37 = phi i64 [ undef, %block_40054c ]
  %R36 = phi i64 [ %R30, %block_40054c ]
  %R35 = phi i64 [ %R24, %block_40054c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R34 = phi i64 [ undef, %block_40054c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R33 = phi i64 [ undef, %block_40054c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R32 = phi i64 [ undef, %block_40054c ]
  ; # 400556: mov    QWORD PTR [rbp-0x8],rax
  ; r40 := (bv_add r35 0xfffffffffffffff8 :: [64])
  %R40 = add i64 %R35, 18446744073709551608
  ; *(r40) = r32
  %r53 = inttoptr i64 %R40 to i64*
  store i64 %R32, i64* %r53
  ; # 40055a: cmp    QWORD PTR [rbp-0x8],0x0
  ; r41 := *r40
  %r54 = inttoptr i64 %R40 to i64*
  %R41 = load i64* %r54
  ; r42 := (bv_eq r41 0x0 :: [64])
  %R42 = icmp eq i64 %R41, 0
  ; # 40055f: jne    40056b
  ; r43 := (bv_complement r42)
  %R43 = xor i1 %R42, -1
  br i1 %R43, label %subblock_400556_1, label %subblock_400556_2
subblock_400556_1:
  br label %block_40056b
subblock_400556_2:
  br label %block_400561
block_400561:
  %R47 = phi i128 [ %R39, %subblock_400556_2 ]
  %R46 = phi i64 [ %R36, %subblock_400556_2 ]
  %R45 = phi i64 [ %R35, %subblock_400556_2 ]
  %R44 = phi i64 [ %R34, %subblock_400556_2 ]
  ; # 400561: mov    edi,0x84
  ; # 400566: call   400130
  %r62 = bitcast i128 %R47 to <2 x double>
  %r63 = call { i64, i64, <2 x double> } @F400130(i64 132, i64 %R46, i64 %R44, <2 x double> %r62)
  %R48 = extractvalue { i64, i64, <2 x double> } %r63, 0
  %R49 = extractvalue { i64, i64, <2 x double> } %r63, 1
  %r66 = extractvalue { i64, i64, <2 x double> } %r63, 2
  %R50 = bitcast <2 x double> %r66 to i128
  br label %block_40056b
block_40056b:
  %R56 = phi i128 [ %R50, %block_400561 ], [ %R39, %subblock_400556_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R55 = phi i64 [ undef, %block_400561 ], [ %R38, %subblock_400556_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R54 = phi i64 [ undef, %block_400561 ], [ %R37, %subblock_400556_1 ]
  %R53 = phi i64 [ %R49, %block_400561 ], [ %R36, %subblock_400556_1 ]
  %R52 = phi i64 [ %R45, %block_400561 ], [ %R35, %subblock_400556_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R51 = phi i64 [ undef, %block_400561 ], [ %R33, %subblock_400556_1 ]
  ; # 40056b: mov    rax,QWORD PTR [rbp-0x8]
  ; r57 := (bv_add r52 0xfffffffffffffff8 :: [64])
  %R57 = add i64 %R52, 18446744073709551608
  ; r58 := *r57
  %r75 = inttoptr i64 %R57 to i64*
  %R58 = load i64* %r75
  ; # 40056f: mov    rdx,QWORD PTR [rbp-0x10]
  ; r59 := (bv_add r52 0xfffffffffffffff0 :: [64])
  %R59 = add i64 %R52, 18446744073709551600
  ; r60 := *r59
  %r78 = inttoptr i64 %R59 to i64*
  %R60 = load i64* %r78
  ; # 400573: mov    QWORD PTR [rax+0x10],rdx
  ; r61 := (bv_add r58 0x10 :: [64])
  %R61 = add i64 %R58, 16
  ; *(r61) = r60
  %r81 = inttoptr i64 %R61 to i64*
  store i64 %R60, i64* %r81
  ; # 400577: mov    rax,QWORD PTR [rbp-0x18]
  ; r62 := (bv_add r52 0xffffffffffffffe8 :: [64])
  %R62 = add i64 %R52, 18446744073709551592
  ; r63 := *r62
  %r83 = inttoptr i64 %R62 to i64*
  %R63 = load i64* %r83
  ; # 40057b: mov    rdi,rax
  ; # 40057e: call   400522
  %r85 = bitcast i128 %R56 to <2 x double>
  %r86 = call { i64, i64, <2 x double> } @F400522(i64 %R63, i64 %R53, i64 %R60, i64 %R51, i64 %R54, i64 %R55, <2 x double> %r85)
  %R64 = extractvalue { i64, i64, <2 x double> } %r86, 0
  %R65 = extractvalue { i64, i64, <2 x double> } %r86, 1
  %r89 = extractvalue { i64, i64, <2 x double> } %r86, 2
  %R66 = bitcast <2 x double> %r89 to i128
  br label %block_400583
block_400583:
  %R73 = phi i128 [ %R66, %block_40056b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R72 = phi i64 [ undef, %block_40056b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R71 = phi i64 [ undef, %block_40056b ]
  %R70 = phi i64 [ %R65, %block_40056b ]
  %R69 = phi i64 [ %R52, %block_40056b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R68 = phi i64 [ undef, %block_40056b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R67 = phi i64 [ undef, %block_40056b ]
  ; # 400583: mov    rdx,rax
  ; # 400586: mov    rax,QWORD PTR [rbp-0x8]
  ; r74 := (bv_add r69 0xfffffffffffffff8 :: [64])
  %R74 = add i64 %R69, 18446744073709551608
  ; r75 := *r74
  %r99 = inttoptr i64 %R74 to i64*
  %R75 = load i64* %r99
  ; # 40058a: mov    QWORD PTR [rax],rdx
  ; *(r75) = r67
  %r101 = inttoptr i64 %R75 to i64*
  store i64 %R67, i64* %r101
  ; # 40058d: mov    rax,QWORD PTR [rbp-0x18]
  ; r76 := (bv_add r69 0xffffffffffffffe8 :: [64])
  %R76 = add i64 %R69, 18446744073709551592
  ; r77 := *r76
  %r103 = inttoptr i64 %R76 to i64*
  %R77 = load i64* %r103
  ; # 400591: mov    rdi,rax
  ; # 400594: call   400522
  %r105 = bitcast i128 %R73 to <2 x double>
  %r106 = call { i64, i64, <2 x double> } @F400522(i64 %R77, i64 %R70, i64 %R67, i64 %R68, i64 %R71, i64 %R72, <2 x double> %r105)
  %R78 = extractvalue { i64, i64, <2 x double> } %r106, 0
  %R79 = extractvalue { i64, i64, <2 x double> } %r106, 1
  %r109 = extractvalue { i64, i64, <2 x double> } %r106, 2
  %R80 = bitcast <2 x double> %r109 to i128
  br label %block_400599
block_400599:
  %R83 = phi i128 [ %R80, %block_400583 ]
  %R82 = phi i64 [ %R69, %block_400583 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R81 = phi i64 [ undef, %block_400583 ]
  ; # 400599: mov    rdx,rax
  ; # 40059c: mov    rax,QWORD PTR [rbp-0x8]
  ; r84 := (bv_add r82 0xfffffffffffffff8 :: [64])
  %R84 = add i64 %R82, 18446744073709551608
  ; r85 := *r84
  %r115 = inttoptr i64 %R84 to i64*
  %R85 = load i64* %r115
  ; # 4005a0: mov    QWORD PTR [rax+0x8],rdx
  ; r86 := (bv_add r85 0x8 :: [64])
  %R86 = add i64 %R85, 8
  ; *(r86) = r81
  %r118 = inttoptr i64 %R86 to i64*
  store i64 %R81, i64* %r118
  ; # 4005a4: mov    rax,QWORD PTR [rbp-0x8]
  ; r87 := *r84
  %r119 = inttoptr i64 %R84 to i64*
  %R87 = load i64* %r119
  br label %block_4005a8
block_4005a8:
  %R90 = phi i128 [ %R21, %block_400545 ], [ %R83, %block_400599 ]
  %R89 = phi i64 [ %R20, %block_400545 ], [ %R81, %block_400599 ]
  %R88 = phi i64 [ 0, %block_400545 ], [ %R87, %block_400599 ]
  ; # 4005a8: leave
  ; # 4005a9: ret
  %r124 = bitcast i128 %R90 to <2 x double>
  %r125 = insertvalue { i64, i64, <2 x double> } undef, i64 %R88, 0
  %r126 = insertvalue { i64, i64, <2 x double> } %r125, i64 %R89, 1
  %r127 = insertvalue { i64, i64, <2 x double> } %r126, <2 x double> %r124, 2
  ret { i64, i64, <2 x double> } %r127
failure:
  br label %failure
}