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
declare { i64, i64, <2 x double> } @F400335(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F401c7a(i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401ee8(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4003c1(i64 %a0, i64 %a1, i64 %a2, i64 %a3, <2 x double> %a4) {
entry:
  %r0 = bitcast <2 x double> %a4 to i128
  br label %block_4003c1
block_4003c1:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 4003c1: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4003c2: mov    rbp,rsp
  ; # 4003c5: sub    rsp,0x20
  ; r3 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R3 = add i64 %R1, 18446744073709551576
  ; # 4003c9: mov    QWORD PTR [rbp-0x18],rdi
  ; r4 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R4 = add i64 %R1, 18446744073709551584
  ; *(r4) = arg0
  %r7 = inttoptr i64 %R4 to i64*
  store i64 %a0, i64* %r7
  ; # 4003cd: mov    QWORD PTR [rbp-0x20],rsi
  ; *(r3) = arg1
  %r8 = inttoptr i64 %R3 to i64*
  store i64 %a1, i64* %r8
  ; # 4003d1: mov    rdx,QWORD PTR [rip+0x203c40]
  ; r5 := *data@(604018)
  %r9 = inttoptr i64 6307864 to i64*
  %R5 = load i64* %r9
  ; # 4003d8: mov    rax,QWORD PTR [rbp-0x18]
  ; r6 := *r4
  %r11 = inttoptr i64 %R4 to i64*
  %R6 = load i64* %r11
  ; # 4003dc: mov    rsi,rdx
  ; # 4003df: mov    rdi,rax
  ; # 4003e2: call   401ee8
  %r13 = bitcast i128 %r0 to <2 x double>
  %r14 = call { i64, i64, <2 x double> } @F401ee8(i64 %R6, i64 %R5, i64 %R5, i64 %a3, <2 x double> %r13)
  %R7 = extractvalue { i64, i64, <2 x double> } %r14, 0
  %R8 = extractvalue { i64, i64, <2 x double> } %r14, 1
  %r17 = extractvalue { i64, i64, <2 x double> } %r14, 2
  %R9 = bitcast <2 x double> %r17 to i128
  br label %block_4003e7
block_4003e7:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R24 = phi i128 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R23 = phi i128 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R22 = phi i128 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R21 = phi i128 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R20 = phi i128 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R19 = phi i128 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R18 = phi i128 [ undef, %block_4003c1 ]
  %R17 = phi i128 [ %R9, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R16 = phi i64 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R15 = phi i64 [ undef, %block_4003c1 ]
  %R14 = phi i64 [ %R8, %block_4003c1 ]
  %R13 = phi i64 [ %R2, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R12 = phi i64 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R11 = phi i64 [ undef, %block_4003c1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R10 = phi i64 [ undef, %block_4003c1 ]
  ; # 4003e7: mov    QWORD PTR [rbp-0x8],rax
  ; r25 := (bv_add r13 0xfffffffffffffff8 :: [64])
  %R25 = add i64 %R13, 18446744073709551608
  ; *(r25) = r10
  %r35 = inttoptr i64 %R25 to i64*
  store i64 %R10, i64* %r35
  ; # 4003eb: cmp    QWORD PTR [rbp-0x8],0x0
  ; r26 := *r25
  %r36 = inttoptr i64 %R25 to i64*
  %R26 = load i64* %r36
  ; r27 := (bv_eq r26 0x0 :: [64])
  %R27 = icmp eq i64 %R26, 0
  ; # 4003f0: jne    4003fc
  ; r28 := (bv_complement r27)
  %R28 = xor i1 %R27, -1
  br i1 %R28, label %subblock_4003e7_1, label %subblock_4003e7_2
subblock_4003e7_1:
  br label %block_4003fc
subblock_4003e7_2:
  br label %block_4003f2
block_4003f2:
  %R32 = phi i128 [ %R17, %subblock_4003e7_2 ]
  %R31 = phi i64 [ %R14, %subblock_4003e7_2 ]
  %R30 = phi i64 [ %R13, %subblock_4003e7_2 ]
  %R29 = phi i64 [ %R12, %subblock_4003e7_2 ]
  ; # 4003f2: mov    edi,0x4b
  ; # 4003f7: call   400130
  %r44 = bitcast i128 %R32 to <2 x double>
  %r45 = call { i64, i64, <2 x double> } @F400130(i64 75, i64 %R31, i64 %R29, <2 x double> %r44)
  %R33 = extractvalue { i64, i64, <2 x double> } %r45, 0
  %R34 = extractvalue { i64, i64, <2 x double> } %r45, 1
  %r48 = extractvalue { i64, i64, <2 x double> } %r45, 2
  %R35 = bitcast <2 x double> %r48 to i128
  br label %block_4003fc
block_4003fc:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R47 = phi i128 [ undef, %block_4003f2 ], [ %R24, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R46 = phi i128 [ undef, %block_4003f2 ], [ %R23, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R45 = phi i128 [ undef, %block_4003f2 ], [ %R22, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R44 = phi i128 [ undef, %block_4003f2 ], [ %R21, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R43 = phi i128 [ undef, %block_4003f2 ], [ %R20, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R42 = phi i128 [ undef, %block_4003f2 ], [ %R19, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R41 = phi i128 [ undef, %block_4003f2 ], [ %R18, %subblock_4003e7_1 ]
  %R40 = phi i128 [ %R35, %block_4003f2 ], [ %R17, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R39 = phi i64 [ undef, %block_4003f2 ], [ %R16, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R38 = phi i64 [ undef, %block_4003f2 ], [ %R15, %subblock_4003e7_1 ]
  %R37 = phi i64 [ %R30, %block_4003f2 ], [ %R13, %subblock_4003e7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R36 = phi i64 [ undef, %block_4003f2 ], [ %R11, %subblock_4003e7_1 ]
  ; # 4003fc: mov    rdx,QWORD PTR [rbp-0x20]
  ; r48 := (bv_add r37 0xffffffffffffffe0 :: [64])
  %R48 = add i64 %R37, 18446744073709551584
  ; r49 := *r48
  %r63 = inttoptr i64 %R48 to i64*
  %R49 = load i64* %r63
  ; # 400400: mov    rax,QWORD PTR [rbp-0x8]
  ; r50 := (bv_add r37 0xfffffffffffffff8 :: [64])
  %R50 = add i64 %R37, 18446744073709551608
  ; r51 := *r50
  %r66 = inttoptr i64 %R50 to i64*
  %R51 = load i64* %r66
  ; # 400404: mov    rsi,rdx
  ; # 400407: mov    rdi,rax
  ; # 40040a: call   400335
  %r68 = bitcast i128 %R40 to <2 x double>
  %r69 = bitcast i128 %R41 to <2 x double>
  %r70 = bitcast i128 %R42 to <2 x double>
  %r71 = bitcast i128 %R43 to <2 x double>
  %r72 = bitcast i128 %R44 to <2 x double>
  %r73 = bitcast i128 %R45 to <2 x double>
  %r74 = bitcast i128 %R46 to <2 x double>
  %r75 = bitcast i128 %R47 to <2 x double>
  %r76 = call { i64, i64, <2 x double> } @F400335(i64 %R51, i64 %R49, i64 %R49, i64 %R36, i64 %R38, i64 %R39, <2 x double> %r68, <2 x double> %r69, <2 x double> %r70, <2 x double> %r71, <2 x double> %r72, <2 x double> %r73, <2 x double> %r74, <2 x double> %r75)
  %R52 = extractvalue { i64, i64, <2 x double> } %r76, 0
  %R53 = extractvalue { i64, i64, <2 x double> } %r76, 1
  %r79 = extractvalue { i64, i64, <2 x double> } %r76, 2
  %R54 = bitcast <2 x double> %r79 to i128
  br label %block_40040f
block_40040f:
  %R59 = phi i128 [ %R54, %block_4003fc ]
  %R58 = phi i64 [ %R53, %block_4003fc ]
  %R57 = phi i64 [ %R37, %block_4003fc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R56 = phi i64 [ undef, %block_4003fc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R55 = phi i64 [ undef, %block_4003fc ]
  ; # 40040f: mov    rax,QWORD PTR [rbp-0x8]
  ; r60 := (bv_add r57 0xfffffffffffffff8 :: [64])
  %R60 = add i64 %R57, 18446744073709551608
  ; r61 := *r60
  %r87 = inttoptr i64 %R60 to i64*
  %R61 = load i64* %r87
  ; # 400413: mov    rdi,rax
  ; # 400416: call   401c7a
  %r89 = bitcast i128 %R59 to <2 x double>
  %r90 = call { i64, i64, <2 x double> } @F401c7a(i64 %R61, i64 %R58, i64 %R56, i64 %R55, <2 x double> %r89)
  %r91 = extractvalue { i64, i64, <2 x double> } %r90, 2
  %R62 = bitcast <2 x double> %r91 to i128
  br label %block_40041b
block_40041b:
  %R63 = phi i128 [ %R62, %block_40040f ]
  ; # 40041b: nop
  ; # 40041c: leave
  ; # 40041d: ret
  %r94 = bitcast i128 %R63 to <2 x double>
  %r95 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r96 = insertvalue { i64, i64, <2 x double> } %r95, i64 undef, 1
  %r97 = insertvalue { i64, i64, <2 x double> } %r96, <2 x double> %r94, 2
  ret { i64, i64, <2 x double> } %r97
failure:
  br label %failure
}