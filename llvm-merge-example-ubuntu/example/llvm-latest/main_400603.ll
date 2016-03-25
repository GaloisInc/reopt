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
declare { i64, i64, <2 x double> } @F400256(i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4002a0(i64, i64)
declare { i64, i64, <2 x double> } @F4003c1(i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4005aa(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400603(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_400603
block_400603:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 400603: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 400604: mov    rbp,rsp
  ; # 400607: sub    rsp,0x20
  ; r3 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R3 = add i64 %R1, 18446744073709551576
  ; # 40060b: mov    DWORD PTR [rbp-0x14],edi
  ; r4 := (trunc arg0 32)
  %R4 = trunc i64 %a0 to i32
  ; r5 := (bv_add r1 0xffffffffffffffe4 :: [64])
  %R5 = add i64 %R1, 18446744073709551588
  ; *(r5) = r4
  %r8 = inttoptr i64 %R5 to i32*
  store i32 %R4, i32* %r8
  ; # 40060e: mov    QWORD PTR [rbp-0x20],rsi
  ; *(r3) = arg1
  %r9 = inttoptr i64 %R3 to i64*
  store i64 %a1, i64* %r9
  ; # 400612: cmp    DWORD PTR [rbp-0x14],0x1
  ; r6 := *r5
  %r10 = inttoptr i64 %R5 to i32*
  %R6 = load i32* %r10
  ; r7 := (ssbb_overflows r6 0x1 :: [32] 0x0 :: [1])
  %r12 = zext i1 0 to i32
  %r13 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %R6, i32 1)
  %r14 = extractvalue { i32, i1 } %r13, 0
  %r15 = extractvalue { i32, i1 } %r13, 1
  %r16 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %r14, i32 %r12)
  %r17 = extractvalue { i32, i1 } %r16, 1
  %R7 = or i1 %r15, %r17
  ; r8 := (bv_add r6 0xffffffff :: [32])
  %R8 = add i32 %R6, 4294967295
  ; r9 := (bv_slt r8 0x0 :: [32])
  %R9 = icmp slt i32 %R8, 0
  ; r10 := (bv_eq r6 0x1 :: [32])
  %R10 = icmp eq i32 %R6, 1
  ; # 400616: jg     400622
  ; r11 := (bv_complement r10)
  %R11 = xor i1 %R10, -1
  ; r12 := (bv_eq r9 r7)
  %R12 = icmp eq i1 %R9, %R7
  ; r13 := (bv_and r11 r12)
  %R13 = and i1 %R11, %R12
  br i1 %R13, label %subblock_400603_1, label %subblock_400603_2
subblock_400603_1:
  br label %block_400622
subblock_400603_2:
  br label %block_400618
block_400618:
  %R17 = phi i128 [ %r0, %subblock_400603_2 ]
  %R16 = phi i64 [ %a1, %subblock_400603_2 ]
  %R15 = phi i64 [ %R2, %subblock_400603_2 ]
  %R14 = phi i64 [ %a2, %subblock_400603_2 ]
  ; # 400618: mov    edi,0x9e
  ; # 40061d: call   400130
  %r29 = bitcast i128 %R17 to <2 x double>
  %r30 = call { i64, i64, <2 x double> } @F400130(i64 158, i64 %R16, i64 %R14, <2 x double> %r29)
  %R18 = extractvalue { i64, i64, <2 x double> } %r30, 0
  %R19 = extractvalue { i64, i64, <2 x double> } %r30, 1
  %r33 = extractvalue { i64, i64, <2 x double> } %r30, 2
  %R20 = bitcast <2 x double> %r33 to i128
  br label %block_400622
block_400622:
  %R22 = phi i128 [ %R20, %block_400618 ], [ %r0, %subblock_400603_1 ]
  %R21 = phi i64 [ %R15, %block_400618 ], [ %R2, %subblock_400603_1 ]
  ; # 400622: mov    rax,QWORD PTR [rbp-0x20]
  ; r23 := (bv_add r21 0xffffffffffffffe0 :: [64])
  %R23 = add i64 %R21, 18446744073709551584
  ; r24 := *r23
  %r38 = inttoptr i64 %R23 to i64*
  %R24 = load i64* %r38
  ; # 400626: mov    rax,QWORD PTR [rax+0x8]
  ; r25 := (bv_add r24 0x8 :: [64])
  %R25 = add i64 %R24, 8
  ; r26 := *r25
  %r41 = inttoptr i64 %R25 to i64*
  %R26 = load i64* %r41
  ; # 40062a: mov    QWORD PTR [rbp-0x10],rax
  ; r27 := (bv_add r21 0xfffffffffffffff0 :: [64])
  %R27 = add i64 %R21, 18446744073709551600
  ; *(r27) = r26
  %r44 = inttoptr i64 %R27 to i64*
  store i64 %R26, i64* %r44
  ; # 40062e: mov    esi,0x604080
  ; # 400633: mov    edi,0x0
  ; # 400638: call   400256
  %r45 = bitcast i128 %R22 to <2 x double>
  %r46 = call { i64, i64, <2 x double> } @F400256(i64 0, i64 6307968, <2 x double> %r45)
  %r47 = extractvalue { i64, i64, <2 x double> } %r46, 2
  %R28 = bitcast <2 x double> %r47 to i128
  br label %block_40063d
block_40063d:
  %R32 = phi i128 [ %R28, %block_400622 ]
  %R31 = phi i64 [ %R21, %block_400622 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R30 = phi i64 [ undef, %block_400622 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R29 = phi i64 [ undef, %block_400622 ]
  ; # 40063d: mov    rax,QWORD PTR [rbp-0x10]
  ; r33 := (bv_add r31 0xfffffffffffffff0 :: [64])
  %R33 = add i64 %R31, 18446744073709551600
  ; r34 := *r33
  %r54 = inttoptr i64 %R33 to i64*
  %R34 = load i64* %r54
  ; # 400641: mov    esi,0x604080
  ; # 400646: mov    rdi,rax
  ; # 400649: call   4003c1
  %r56 = bitcast i128 %R32 to <2 x double>
  %r57 = call { i64, i64, <2 x double> } @F4003c1(i64 %R34, i64 6307968, i64 %R30, i64 %R29, <2 x double> %r56)
  %r58 = extractvalue { i64, i64, <2 x double> } %r57, 2
  %R35 = bitcast <2 x double> %r58 to i128
  br label %block_40064e
block_40064e:
  %R40 = phi i128 [ %R35, %block_40063d ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R39 = phi i64 [ undef, %block_40063d ]
  %R38 = phi i64 [ %R31, %block_40063d ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R37 = phi i64 [ undef, %block_40063d ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R36 = phi i64 [ undef, %block_40063d ]
  ; # 40064e: mov    rax,QWORD PTR [rbp-0x10]
  ; r41 := (bv_add r38 0xfffffffffffffff0 :: [64])
  %R41 = add i64 %R38, 18446744073709551600
  ; r42 := *r41
  %r66 = inttoptr i64 %R41 to i64*
  %R42 = load i64* %r66
  ; # 400652: mov    rdi,rax
  ; # 400655: call   4005aa
  %r68 = bitcast i128 %R40 to <2 x double>
  %r69 = call { i64, i64, <2 x double> } @F4005aa(i64 %R42, i64 %R39, i64 %R37, i64 %R36, <2 x double> %r68)
  %R43 = extractvalue { i64, i64, <2 x double> } %r69, 0
  %r71 = extractvalue { i64, i64, <2 x double> } %r69, 2
  %R44 = bitcast <2 x double> %r71 to i128
  br label %block_40065a
block_40065a:
  %R47 = phi i128 [ %R44, %block_40064e ]
  %R46 = phi i64 [ %R38, %block_40064e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R45 = phi i64 [ undef, %block_40064e ]
  ; # 40065a: mov    QWORD PTR [rbp-0x8],rax
  ; r48 := (bv_add r46 0xfffffffffffffff8 :: [64])
  %R48 = add i64 %R46, 18446744073709551608
  ; *(r48) = r45
  %r77 = inttoptr i64 %R48 to i64*
  store i64 %R45, i64* %r77
  ; # 40065e: mov    rax,QWORD PTR [rbp-0x8]
  ; r49 := *r48
  %r78 = inttoptr i64 %R48 to i64*
  %R49 = load i64* %r78
  ; # 400662: mov    rsi,rax
  ; # 400665: mov    edi,0x0
  ; # 40066a: call   400256
  %r80 = bitcast i128 %R47 to <2 x double>
  %r81 = call { i64, i64, <2 x double> } @F400256(i64 0, i64 %R49, <2 x double> %r80)
  %r82 = extractvalue { i64, i64, <2 x double> } %r81, 2
  %R50 = bitcast <2 x double> %r82 to i128
  br label %block_40066f
block_40066f:
  %R51 = phi i64 [ %R46, %block_40065a ]
  ; # 40066f: mov    rax,QWORD PTR [rbp-0x8]
  ; r52 := (bv_add r51 0xfffffffffffffff8 :: [64])
  %R52 = add i64 %R51, 18446744073709551608
  ; r53 := *r52
  %r86 = inttoptr i64 %R52 to i64*
  %R53 = load i64* %r86
  ; # 400673: mov    rsi,rax
  ; # 400676: mov    edi,0x604080
  ; # 40067b: call   4002a0
  %r88 = call { i64, i64, <2 x double> } @F4002a0(i64 6307968, i64 %R53)
  %R54 = extractvalue { i64, i64, <2 x double> } %r88, 0
  br label %block_400680
block_400680:
  ; # 400680: test   eax,eax
  ; # 400682: sete   al
  ; # 400685: movzx  eax,al
  ; # 400688: leave
  ; # 400689: ret
  %r90 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r91 = insertvalue { i64, i64, <2 x double> } %r90, i64 undef, 1
  %r92 = insertvalue { i64, i64, <2 x double> } %r91, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r92
failure:
  br label %failure
}