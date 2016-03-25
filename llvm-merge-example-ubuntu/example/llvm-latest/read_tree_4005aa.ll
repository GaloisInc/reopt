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
declare { i64, i64, <2 x double> } @F400522(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401c7a(i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401ee8(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4005aa(i64 %a0, i64 %a1, i64 %a2, i64 %a3, <2 x double> %a4) {
entry:
  %r0 = bitcast <2 x double> %a4 to i128
  br label %block_4005aa
block_4005aa:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 4005aa: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4005ab: mov    rbp,rsp
  ; # 4005ae: sub    rsp,0x20
  ; # 4005b2: mov    QWORD PTR [rbp-0x18],rdi
  ; r3 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R3 = add i64 %R1, 18446744073709551584
  ; *(r3) = arg0
  %r6 = inttoptr i64 %R3 to i64*
  store i64 %a0, i64* %r6
  ; # 4005b6: mov    rdx,QWORD PTR [rip+0x203a53]
  ; r4 := *data@(604010)
  %r7 = inttoptr i64 6307856 to i64*
  %R4 = load i64* %r7
  ; # 4005bd: mov    rax,QWORD PTR [rbp-0x18]
  ; r5 := *r3
  %r9 = inttoptr i64 %R3 to i64*
  %R5 = load i64* %r9
  ; # 4005c1: mov    rsi,rdx
  ; # 4005c4: mov    rdi,rax
  ; # 4005c7: call   401ee8
  %r11 = bitcast i128 %r0 to <2 x double>
  %r12 = call { i64, i64, <2 x double> } @F401ee8(i64 %R5, i64 %R4, i64 %R4, i64 %a3, <2 x double> %r11)
  %R6 = extractvalue { i64, i64, <2 x double> } %r12, 0
  %R7 = extractvalue { i64, i64, <2 x double> } %r12, 1
  %r15 = extractvalue { i64, i64, <2 x double> } %r12, 2
  %R8 = bitcast <2 x double> %r15 to i128
  br label %block_4005cc
block_4005cc:
  %R16 = phi i128 [ %R8, %block_4005aa ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R15 = phi i64 [ undef, %block_4005aa ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R14 = phi i64 [ undef, %block_4005aa ]
  %R13 = phi i64 [ %R7, %block_4005aa ]
  %R12 = phi i64 [ %R2, %block_4005aa ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R11 = phi i64 [ undef, %block_4005aa ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R10 = phi i64 [ undef, %block_4005aa ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R9 = phi i64 [ undef, %block_4005aa ]
  ; # 4005cc: mov    QWORD PTR [rbp-0x10],rax
  ; r17 := (bv_add r12 0xfffffffffffffff0 :: [64])
  %R17 = add i64 %R12, 18446744073709551600
  ; *(r17) = r9
  %r26 = inttoptr i64 %R17 to i64*
  store i64 %R9, i64* %r26
  ; # 4005d0: cmp    QWORD PTR [rbp-0x10],0x0
  ; r18 := *r17
  %r27 = inttoptr i64 %R17 to i64*
  %R18 = load i64* %r27
  ; r19 := (bv_eq r18 0x0 :: [64])
  %R19 = icmp eq i64 %R18, 0
  ; # 4005d5: jne    4005e1
  ; r20 := (bv_complement r19)
  %R20 = xor i1 %R19, -1
  br i1 %R20, label %subblock_4005cc_1, label %subblock_4005cc_2
subblock_4005cc_1:
  br label %block_4005e1
subblock_4005cc_2:
  br label %block_4005d7
block_4005d7:
  %R24 = phi i128 [ %R16, %subblock_4005cc_2 ]
  %R23 = phi i64 [ %R13, %subblock_4005cc_2 ]
  %R22 = phi i64 [ %R12, %subblock_4005cc_2 ]
  %R21 = phi i64 [ %R11, %subblock_4005cc_2 ]
  ; # 4005d7: mov    edi,0x93
  ; # 4005dc: call   400130
  %r35 = bitcast i128 %R24 to <2 x double>
  %r36 = call { i64, i64, <2 x double> } @F400130(i64 147, i64 %R23, i64 %R21, <2 x double> %r35)
  %R25 = extractvalue { i64, i64, <2 x double> } %r36, 0
  %R26 = extractvalue { i64, i64, <2 x double> } %r36, 1
  %r39 = extractvalue { i64, i64, <2 x double> } %r36, 2
  %R27 = bitcast <2 x double> %r39 to i128
  br label %block_4005e1
block_4005e1:
  %R34 = phi i128 [ %R27, %block_4005d7 ], [ %R16, %subblock_4005cc_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R33 = phi i64 [ undef, %block_4005d7 ], [ %R15, %subblock_4005cc_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R32 = phi i64 [ undef, %block_4005d7 ], [ %R14, %subblock_4005cc_1 ]
  %R31 = phi i64 [ %R26, %block_4005d7 ], [ %R13, %subblock_4005cc_1 ]
  %R30 = phi i64 [ %R22, %block_4005d7 ], [ %R12, %subblock_4005cc_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R29 = phi i64 [ undef, %block_4005d7 ], [ %R11, %subblock_4005cc_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R28 = phi i64 [ undef, %block_4005d7 ], [ %R10, %subblock_4005cc_1 ]
  ; # 4005e1: mov    rax,QWORD PTR [rbp-0x10]
  ; r35 := (bv_add r30 0xfffffffffffffff0 :: [64])
  %R35 = add i64 %R30, 18446744073709551600
  ; r36 := *r35
  %r49 = inttoptr i64 %R35 to i64*
  %R36 = load i64* %r49
  ; # 4005e5: mov    rdi,rax
  ; # 4005e8: call   400522
  %r51 = bitcast i128 %R34 to <2 x double>
  %r52 = call { i64, i64, <2 x double> } @F400522(i64 %R36, i64 %R31, i64 %R29, i64 %R28, i64 %R32, i64 %R33, <2 x double> %r51)
  %R37 = extractvalue { i64, i64, <2 x double> } %r52, 0
  %R38 = extractvalue { i64, i64, <2 x double> } %r52, 1
  %r55 = extractvalue { i64, i64, <2 x double> } %r52, 2
  %R39 = bitcast <2 x double> %r55 to i128
  br label %block_4005ed
block_4005ed:
  %R45 = phi i128 [ %R39, %block_4005e1 ]
  %R44 = phi i64 [ %R38, %block_4005e1 ]
  %R43 = phi i64 [ %R30, %block_4005e1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R42 = phi i64 [ undef, %block_4005e1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R41 = phi i64 [ undef, %block_4005e1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R40 = phi i64 [ undef, %block_4005e1 ]
  ; # 4005ed: mov    QWORD PTR [rbp-0x8],rax
  ; r46 := (bv_add r43 0xfffffffffffffff8 :: [64])
  %R46 = add i64 %R43, 18446744073709551608
  ; *(r46) = r40
  %r64 = inttoptr i64 %R46 to i64*
  store i64 %R40, i64* %r64
  ; # 4005f1: mov    rax,QWORD PTR [rbp-0x10]
  ; r47 := (bv_add r43 0xfffffffffffffff0 :: [64])
  %R47 = add i64 %R43, 18446744073709551600
  ; r48 := *r47
  %r66 = inttoptr i64 %R47 to i64*
  %R48 = load i64* %r66
  ; # 4005f5: mov    rdi,rax
  ; # 4005f8: call   401c7a
  %r68 = bitcast i128 %R45 to <2 x double>
  %r69 = call { i64, i64, <2 x double> } @F401c7a(i64 %R48, i64 %R44, i64 %R42, i64 %R41, <2 x double> %r68)
  %r70 = extractvalue { i64, i64, <2 x double> } %r69, 2
  %R49 = bitcast <2 x double> %r70 to i128
  br label %block_4005fd
block_4005fd:
  %R51 = phi i128 [ %R49, %block_4005ed ]
  %R50 = phi i64 [ %R43, %block_4005ed ]
  ; # 4005fd: mov    rax,QWORD PTR [rbp-0x8]
  ; r52 := (bv_add r50 0xfffffffffffffff8 :: [64])
  %R52 = add i64 %R50, 18446744073709551608
  ; r53 := *r52
  %r75 = inttoptr i64 %R52 to i64*
  %R53 = load i64* %r75
  ; # 400601: leave
  ; # 400602: ret
  %r77 = bitcast i128 %R51 to <2 x double>
  %r78 = insertvalue { i64, i64, <2 x double> } undef, i64 %R53, 0
  %r79 = insertvalue { i64, i64, <2 x double> } %r78, i64 undef, 1
  %r80 = insertvalue { i64, i64, <2 x double> } %r79, <2 x double> %r77, 2
  ret { i64, i64, <2 x double> } %r80
failure:
  br label %failure
}