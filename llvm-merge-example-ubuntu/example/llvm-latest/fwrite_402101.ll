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
declare { i64, i64, <2 x double> } @F402058(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F402a2c(i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402a80(i64, i64, i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F402101(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_402101
block_402101:
  ; r0 := (alloca 0x40 :: [64])
  %r8 = alloca i8, i64 64
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x40 :: [64])
  %R1 = add i64 %R0, 64
  ; # 402101: push   r15
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402103: push   r14
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 402105: mov    eax,0x0
  ; # 40210a: push   r13
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 40210c: mov    r13,rsi
  ; # 40210f: push   r12
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 402111: imul   r13,rdx
  ; r6 := (sext arg2 128)
  %R6 = sext i64 %a2 to i128
  ; r7 := (sext arg1 128)
  %R7 = sext i64 %a1 to i128
  ; r8 := (bv_mul r6 r7)
  %R8 = mul i128 %R6, %R7
  ; r9 := (trunc r8 64)
  %R9 = trunc i128 %R8 to i64
  ; # 402115: push   rbp
  ; r10 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R10 = add i64 %R1, 18446744073709551576
  ; # 402116: test   rsi,rsi
  ; # 402119: push   rbx
  ; r11 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R11 = add i64 %R1, 18446744073709551568
  ; # 40211a: mov    rbx,rdx
  ; # 40211d: push   r8
  ; r12 := (bv_add r1 0xffffffffffffffc8 :: [64])
  %R12 = add i64 %R1, 18446744073709551560
  ; *(r12) = arg4
  %r22 = inttoptr i64 %R12 to i64*
  store i64 %a4, i64* %r22
  ; # 40211f: cmove  rbx,rax
  ; # 402123: mov    eax,DWORD PTR [rcx+0x8c]
  ; r13 := (bv_add arg3 0x8c :: [64])
  %R13 = add i64 %a3, 140
  ; r14 := *r13
  %r24 = inttoptr i64 %R13 to i32*
  %R14 = load i32* %r24
  ; # 402129: xor    r14d,r14d
  ; # 40212c: mov    r15,rdi
  ; # 40212f: mov    rbp,rsi
  ; # 402132: mov    r12,rcx
  ; # 402135: test   eax,eax
  ; r15 := (bv_slt r14 0x0 :: [32])
  %R15 = icmp slt i32 %R14, 0
  ; # 402137: js     402144
  br i1 %R15, label %subblock_402101_1, label %subblock_402101_2
subblock_402101_1:
  br label %block_402144
subblock_402101_2:
  br label %block_402139
block_402139:
  %R21 = phi i128 [ %r0, %subblock_402101_2 ]
  %R20 = phi i64 [ %a0, %subblock_402101_2 ]
  %R19 = phi i64 [ %R9, %subblock_402101_2 ]
  %R18 = phi i64 [ %a3, %subblock_402101_2 ]
  %R17 = phi i64 [ %a1, %subblock_402101_2 ]
  %R16 = phi i64 [ %a3, %subblock_402101_2 ]
  ; # 402139: mov    rdi,rcx
  ; # 40213c: call   402a2c
  %r33 = bitcast i128 %R21 to <2 x double>
  %r34 = call { i64, i64, <2 x double> } @F402a2c(i64 %R16, <2 x double> %r33)
  %R22 = extractvalue { i64, i64, <2 x double> } %r34, 0
  %R23 = extractvalue { i64, i64, <2 x double> } %r34, 1
  %r37 = extractvalue { i64, i64, <2 x double> } %r34, 2
  %R24 = bitcast <2 x double> %r37 to i128
  br label %block_402141
block_402141:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R40 = phi i128 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R39 = phi i128 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R38 = phi i128 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R37 = phi i128 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R36 = phi i128 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R35 = phi i128 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R34 = phi i128 [ undef, %block_402139 ]
  %R33 = phi i128 [ %R24, %block_402139 ]
  %R32 = phi i64 [ %R20, %block_402139 ]
  %R31 = phi i64 [ %R19, %block_402139 ]
  %R30 = phi i64 [ %R18, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R29 = phi i64 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R28 = phi i64 [ undef, %block_402139 ]
  %R27 = phi i64 [ %R17, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R26 = phi i64 [ undef, %block_402139 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R25 = phi i64 [ undef, %block_402139 ]
  ; # 402141: mov    r14d,eax
  ; r41 := (trunc r25 32)
  %R41 = trunc i64 %R25 to i32
  ; r42 := (uext r41 64)
  %R42 = zext i32 %R41 to i64
  br label %block_402144
block_402144:
  %R58 = phi i128 [ %R40, %block_402141 ], [ %r7, %subblock_402101_1 ]
  %R57 = phi i128 [ %R39, %block_402141 ], [ %r6, %subblock_402101_1 ]
  %R56 = phi i128 [ %R38, %block_402141 ], [ %r5, %subblock_402101_1 ]
  %R55 = phi i128 [ %R37, %block_402141 ], [ %r4, %subblock_402101_1 ]
  %R54 = phi i128 [ %R36, %block_402141 ], [ %r3, %subblock_402101_1 ]
  %R53 = phi i128 [ %R35, %block_402141 ], [ %r2, %subblock_402101_1 ]
  %R52 = phi i128 [ %R34, %block_402141 ], [ %r1, %subblock_402101_1 ]
  %R51 = phi i128 [ %R33, %block_402141 ], [ %r0, %subblock_402101_1 ]
  %R50 = phi i64 [ %R32, %block_402141 ], [ %a0, %subblock_402101_1 ]
  %R49 = phi i64 [ %R42, %block_402141 ], [ 0, %subblock_402101_1 ]
  %R48 = phi i64 [ %R31, %block_402141 ], [ %R9, %subblock_402101_1 ]
  %R47 = phi i64 [ %R30, %block_402141 ], [ %a3, %subblock_402101_1 ]
  %R46 = phi i64 [ %R29, %block_402141 ], [ %a5, %subblock_402101_1 ]
  %R45 = phi i64 [ %R28, %block_402141 ], [ %a4, %subblock_402101_1 ]
  %R44 = phi i64 [ %R27, %block_402141 ], [ %a1, %subblock_402101_1 ]
  %R43 = phi i64 [ %R26, %block_402141 ], [ %a3, %subblock_402101_1 ]
  ; # 402144: mov    rdi,r15
  ; # 402147: mov    rdx,r12
  ; # 40214a: mov    rsi,r13
  ; # 40214d: call   402058
  %r73 = bitcast i128 %R51 to <2 x double>
  %r74 = bitcast i128 %R52 to <2 x double>
  %r75 = bitcast i128 %R53 to <2 x double>
  %r76 = bitcast i128 %R54 to <2 x double>
  %r77 = bitcast i128 %R55 to <2 x double>
  %r78 = bitcast i128 %R56 to <2 x double>
  %r79 = bitcast i128 %R57 to <2 x double>
  %r80 = bitcast i128 %R58 to <2 x double>
  %r81 = call { i64, i64, <2 x double> } @F402058(i64 %R50, i64 %R48, i64 %R47, i64 %R43, i64 %R45, i64 %R46, <2 x double> %r73, <2 x double> %r74, <2 x double> %r75, <2 x double> %r76, <2 x double> %r77, <2 x double> %r78, <2 x double> %r79, <2 x double> %r80)
  %R59 = extractvalue { i64, i64, <2 x double> } %r81, 0
  %r83 = extractvalue { i64, i64, <2 x double> } %r81, 2
  %R60 = bitcast <2 x double> %r83 to i128
  br label %block_402152
block_402152:
  %R71 = phi i128 [ %R60, %block_402144 ]
  %R70 = phi i64 [ %R49, %block_402144 ]
  %R69 = phi i64 [ %R48, %block_402144 ]
  %R68 = phi i64 [ %R47, %block_402144 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R67 = phi i64 [ undef, %block_402144 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R66 = phi i64 [ undef, %block_402144 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R65 = phi i64 [ undef, %block_402144 ]
  %R64 = phi i64 [ %R44, %block_402144 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R63 = phi i64 [ undef, %block_402144 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R62 = phi i64 [ undef, %block_402144 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R61 = phi i64 [ undef, %block_402144 ]
  ; # 402152: test   r14d,r14d
  ; r72 := (trunc r70 32)
  %R72 = trunc i64 %R70 to i32
  ; r73 := (bv_eq r72 0x0 :: [32])
  %R73 = icmp eq i32 %R72, 0
  ; # 402155: mov    r15,rax
  ; # 402158: je     402162
  br i1 %R73, label %subblock_402152_1, label %subblock_402152_2
subblock_402152_1:
  br label %block_402162
subblock_402152_2:
  br label %block_40215a
block_40215a:
  %R83 = phi i128 [ %R71, %subblock_402152_2 ]
  %R82 = phi i64 [ %R61, %subblock_402152_2 ]
  %R81 = phi i64 [ %R69, %subblock_402152_2 ]
  %R80 = phi i64 [ %R68, %subblock_402152_2 ]
  %R79 = phi i64 [ %R67, %subblock_402152_2 ]
  %R78 = phi i64 [ %R66, %subblock_402152_2 ]
  %R77 = phi i64 [ %R65, %subblock_402152_2 ]
  %R76 = phi i64 [ %R64, %subblock_402152_2 ]
  %R75 = phi i64 [ %R63, %subblock_402152_2 ]
  %R74 = phi i64 [ %R62, %subblock_402152_2 ]
  ; # 40215a: mov    rdi,r12
  ; # 40215d: call   402a80
  %r108 = bitcast i128 %R83 to <2 x double>
  %r109 = call { i64, i64, <2 x double> } @F402a80(i64 %R80, i64 %R77, i64 %R75, i64 %R74, i64 %R78, i64 %R79, <2 x double> %r108)
  %R84 = extractvalue { i64, i64, <2 x double> } %r109, 0
  %R85 = extractvalue { i64, i64, <2 x double> } %r109, 1
  %r112 = extractvalue { i64, i64, <2 x double> } %r109, 2
  %R86 = bitcast <2 x double> %r112 to i128
  br label %block_402162
block_402162:
  %R90 = phi i128 [ %R86, %block_40215a ], [ %R71, %subblock_402152_1 ]
  %R89 = phi i64 [ %R82, %block_40215a ], [ %R61, %subblock_402152_1 ]
  %R88 = phi i64 [ %R81, %block_40215a ], [ %R69, %subblock_402152_1 ]
  %R87 = phi i64 [ %R76, %block_40215a ], [ %R64, %subblock_402152_1 ]
  ; # 402162: cmp    r13,r15
  ; r91 := (bv_eq r88 r89)
  %R91 = icmp eq i64 %R88, %R89
  ; # 402165: mov    rax,rbx
  ; # 402168: je     402172
  br i1 %R91, label %subblock_402162_1, label %subblock_402162_2
subblock_402162_1:
  br label %block_402172
subblock_402162_2:
  br label %block_40216a
block_40216a:
  %R94 = phi i128 [ %R90, %subblock_402162_2 ]
  %R93 = phi i64 [ %R89, %subblock_402162_2 ]
  %R92 = phi i64 [ %R87, %subblock_402162_2 ]
  ; # 40216a: mov    rax,r15
  ; # 40216d: xor    edx,edx
  ; # 40216f: div    rbp
  ; r95 := (bv_eq r92 0x0 :: [64])
  %R95 = icmp eq i64 %R92, 0
  br i1 %R95, label %subblock_40216a_1, label %subblock_40216a_4
subblock_40216a_1:
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception DivideError ()
  ; r96 := (uext r93 128)
  %R96 = zext i64 %R93 to i128
  ; r97 := (uext r92 128)
  %R97 = zext i64 %R92 to i128
  ; r98 := (bv_uquot r96 r97)
  %R98 = udiv i128 %R96, %R97
  ; r99 := (trunc r98 64)
  %R99 = trunc i128 %R98 to i64
  ; r100 := (uext r99 128)
  %R100 = zext i64 %R99 to i128
  ; r101 := (bv_eq r98 r100)
  %R101 = icmp eq i128 %R98, %R100
  ; r102 := (bv_complement r101)
  %R102 = xor i1 %R101, -1
  br i1 %R102, label %subblock_40216a_2, label %subblock_40216a_3
subblock_40216a_2:
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception DivideError ()
  br label %block_402172
subblock_40216a_3:
  br label %block_402172
subblock_40216a_4:
  ; r103 := (uext r93 128)
  %R103 = zext i64 %R93 to i128
  ; r104 := (uext r92 128)
  %R104 = zext i64 %R92 to i128
  ; r105 := (bv_uquot r103 r104)
  %R105 = udiv i128 %R103, %R104
  ; r106 := (trunc r105 64)
  %R106 = trunc i128 %R105 to i64
  ; r107 := (uext r106 128)
  %R107 = zext i64 %R106 to i128
  ; r108 := (bv_eq r105 r107)
  %R108 = icmp eq i128 %R105, %R107
  ; r109 := (bv_complement r108)
  %R109 = xor i1 %R108, -1
  br i1 %R109, label %subblock_40216a_5, label %subblock_40216a_6
subblock_40216a_5:
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception DivideError ()
  br label %block_402172
subblock_40216a_6:
  br label %block_402172
block_402172:
  %R110 = phi i128 [ %R94, %subblock_40216a_6 ], [ %R94, %subblock_40216a_5 ], [ %R94, %subblock_40216a_3 ], [ %R94, %subblock_40216a_2 ], [ %R90, %subblock_402162_1 ]
  ; # 402172: pop    rdx
  ; # 402173: pop    rbx
  ; # 402174: pop    rbp
  ; # 402175: pop    r12
  ; # 402177: pop    r13
  ; # 402179: pop    r14
  ; # 40217b: pop    r15
  ; # 40217d: ret
  %r138 = bitcast i128 %R110 to <2 x double>
  %r139 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r140 = insertvalue { i64, i64, <2 x double> } %r139, i64 undef, 1
  %r141 = insertvalue { i64, i64, <2 x double> } %r140, <2 x double> %r138, 2
  ret { i64, i64, <2 x double> } %r141
failure:
  br label %failure
}