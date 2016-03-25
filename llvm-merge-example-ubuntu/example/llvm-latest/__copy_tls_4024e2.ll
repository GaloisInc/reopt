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
declare { i64, i64, <2 x double> } @F402375(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4024e2(i64 %a0, i64 %a1, i64 %a2, i64 %a3, <2 x double> %a4) {
entry:
  %r0 = bitcast <2 x double> %a4 to i128
  br label %block_4024e2
block_4024e2:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 4024e2: push   r13
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4024e4: push   r12
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 4024e6: lea    r13,[rdi+0x8]
  ; r4 := (bv_add arg0 0x8 :: [64])
  %R4 = add i64 %a0, 8
  ; # 4024ea: push   rbp
  ; r5 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R5 = add i64 %R1, 18446744073709551592
  ; # 4024eb: push   rbx
  ; r6 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R6 = add i64 %R1, 18446744073709551584
  ; # 4024ec: mov    r12,rdi
  ; # 4024ef: push   rcx
  ; r7 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R7 = add i64 %R1, 18446744073709551576
  ; *(r7) = arg3
  %r10 = inttoptr i64 %R7 to i64*
  store i64 %a3, i64* %r10
  ; # 4024f0: mov    rax,QWORD PTR [rip+0x2024a9]
  ; r8 := *0x6049a0 :: [64]
  %r11 = inttoptr i64 6310304 to i64*
  %R8 = load i64* %r11
  ; # 4024f7: mov    rbx,QWORD PTR [rip+0x2024aa]
  ; r9 := *0x6049a8 :: [64]
  %r13 = inttoptr i64 6310312 to i64*
  %R9 = load i64* %r13
  ; # 4024fe: mov    rbp,QWORD PTR [rip+0x202493]
  ; r10 := *0x604998 :: [64]
  %r15 = inttoptr i64 6310296 to i64*
  %R10 = load i64* %r15
  ; # 402505: lea    rax,[rdi+rax*1-0x150]
  ; r11 := (bv_add arg0 r8)
  %R11 = add i64 %a0, %R8
  ; r12 := (bv_add r11 0xfffffffffffffeb0 :: [64])
  %R12 = add i64 %R11, 18446744073709551280
  ; # 40250d: neg    rbx
  ; r13 := (bv_sub 0x0 :: [64] r9)
  %R13 = sub i64 0, %R9
  ; # 402510: and    rbx,rax
  ; r14 := (bv_and r13 r12)
  %R14 = and i64 %R13, %R12
  br label %block_402513
block_402513:
  %R19 = phi i128 [ %R41, %block_402534 ], [ %r0, %block_4024e2 ]
  %R18 = phi i64 [ %R40, %block_402534 ], [ %R4, %block_4024e2 ]
  %R17 = phi i64 [ %R39, %block_402534 ], [ %a0, %block_4024e2 ]
  %R16 = phi i64 [ %R42, %block_402534 ], [ %R10, %block_4024e2 ]
  %R15 = phi i64 [ %R37, %block_402534 ], [ %R14, %block_4024e2 ]
  ; # 402513: test   rbp,rbp
  ; r20 := (bv_eq r16 0x0 :: [64])
  %R20 = icmp eq i64 %R16, 0
  ; # 402516: je     40253a
  br i1 %R20, label %subblock_402513_1, label %subblock_402513_2
subblock_402513_1:
  br label %block_40253a
subblock_402513_2:
  br label %block_402518
block_402518:
  %R25 = phi i128 [ %R19, %subblock_402513_2 ]
  %R24 = phi i64 [ %R18, %subblock_402513_2 ]
  %R23 = phi i64 [ %R17, %subblock_402513_2 ]
  %R22 = phi i64 [ %R16, %subblock_402513_2 ]
  %R21 = phi i64 [ %R15, %subblock_402513_2 ]
  ; # 402518: mov    rdi,rbx
  ; # 40251b: sub    rdi,QWORD PTR [rbp+0x28]
  ; r26 := (bv_add r22 0x28 :: [64])
  %R26 = add i64 %R22, 40
  ; r27 := *r26
  %r33 = inttoptr i64 %R26 to i64*
  %R27 = load i64* %r33
  ; r28 := (bv_sub r21 r27)
  %R28 = sub i64 %R21, %R27
  ; # 40251f: mov    rdx,QWORD PTR [rbp+0x10]
  ; r29 := (bv_add r22 0x10 :: [64])
  %R29 = add i64 %R22, 16
  ; r30 := *r29
  %r37 = inttoptr i64 %R29 to i64*
  %R30 = load i64* %r37
  ; # 402523: add    r13,0x8
  ; r31 := (bv_add r24 0x8 :: [64])
  %R31 = add i64 %R24, 8
  ; # 402527: mov    QWORD PTR [r13-0x8],rdi
  ; *(r24) = r28
  %r40 = inttoptr i64 %R24 to i64*
  store i64 %R28, i64* %r40
  ; # 40252b: mov    rsi,QWORD PTR [rbp+0x8]
  ; r32 := (bv_add r22 0x8 :: [64])
  %R32 = add i64 %R22, 8
  ; r33 := *r32
  %r42 = inttoptr i64 %R32 to i64*
  %R33 = load i64* %r42
  ; # 40252f: call   402375
  %r44 = bitcast i128 %R25 to <2 x double>
  %r45 = call { i64, i64, <2 x double> } @F402375(i64 %R28, i64 %R33, i64 %R30, <2 x double> %r44)
  %R34 = extractvalue { i64, i64, <2 x double> } %r45, 0
  %R35 = extractvalue { i64, i64, <2 x double> } %r45, 1
  %r48 = extractvalue { i64, i64, <2 x double> } %r45, 2
  %R36 = bitcast <2 x double> %r48 to i128
  br label %block_402534
block_402534:
  %R41 = phi i128 [ %R36, %block_402518 ]
  %R40 = phi i64 [ %R31, %block_402518 ]
  %R39 = phi i64 [ %R23, %block_402518 ]
  %R38 = phi i64 [ %R22, %block_402518 ]
  %R37 = phi i64 [ %R21, %block_402518 ]
  ; # 402534: mov    rbp,QWORD PTR [rbp]
  ; r42 := *r38
  %r55 = inttoptr i64 %R38 to i64*
  %R42 = load i64* %r55
  ; # 402538: jmp    402513
  br label %block_402513
block_40253a:
  %R44 = phi i64 [ %R17, %subblock_402513_1 ]
  %R43 = phi i64 [ %R15, %subblock_402513_1 ]
  ; # 40253a: mov    rax,QWORD PTR [rip+0x20246f]
  ; r45 := *0x6049b0 :: [64]
  %r59 = inttoptr i64 6310320 to i64*
  %R45 = load i64* %r59
  ; # 402541: mov    QWORD PTR [r12],rax
  ; *(r44) = r45
  %r61 = inttoptr i64 %R44 to i64*
  store i64 %R45, i64* %r61
  ; # 402545: mov    QWORD PTR [rbx+0x148],r12
  ; r46 := (bv_add r43 0x148 :: [64])
  %R46 = add i64 %R43, 328
  ; *(r46) = r44
  %r63 = inttoptr i64 %R46 to i64*
  store i64 %R44, i64* %r63
  ; # 40254c: mov    rax,rbx
  ; # 40254f: mov    QWORD PTR [rbx+0x8],r12
  ; r47 := (bv_add r43 0x8 :: [64])
  %R47 = add i64 %R43, 8
  ; *(r47) = r44
  %r65 = inttoptr i64 %R47 to i64*
  store i64 %R44, i64* %r65
  ; # 402553: pop    rdx
  ; # 402554: pop    rbx
  ; # 402555: pop    rbp
  ; # 402556: pop    r12
  ; # 402558: pop    r13
  ; # 40255a: ret
  %r66 = insertvalue { i64, i64, <2 x double> } undef, i64 %R43, 0
  %r67 = insertvalue { i64, i64, <2 x double> } %r66, i64 undef, 1
  %r68 = insertvalue { i64, i64, <2 x double> } %r67, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r68
failure:
  br label %failure
}