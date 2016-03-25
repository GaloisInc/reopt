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
declare { i64, i64, <2 x double> } @F401280(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4021a0(i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402681(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402b32(i64)
declare { i64, i64, <2 x double> } @F402b4f(i64, i64, i64)
declare { i64, i64, <2 x double> } @F402bf3(i64, i64, i64)
declare { i64, i64, <2 x double> } @F402c09(i64, i64, i64)
declare { i64, i64, <2 x double> } @F402d6a(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402d93(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F40284f(i64 %a0, i64 %a1, <2 x double> %a2) {
entry:
  %r0 = bitcast <2 x double> %a2 to i128
  br label %block_40284f
block_40284f:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 40284f: push   r12
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402851: push   rbp
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 402852: mov    r12,rsi
  ; # 402855: push   rbx
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 402856: mov    ebp,edi
  ; r5 := (trunc arg0 32)
  %R5 = trunc i64 %a0 to i32
  ; r6 := (uext r5 64)
  %R6 = zext i32 %R5 to i64
  ; # 402858: mov    edi,0x402f82
  ; # 40285d: sub    rsp,0x10
  ; # 402861: movsx  esi,BYTE PTR [rsi]
  ; r7 := *arg1
  %r9 = inttoptr i64 %a1 to i8*
  %R7 = load i8* %r9
  ; r8 := (sext r7 32)
  %R8 = sext i8 %R7 to i32
  ; r9 := (uext r8 64)
  %R9 = zext i32 %R8 to i64
  ; # 402864: call   4021a0
  ; r10 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R10 = add i64 %R1, 18446744073709551568
  ; r14 := (bv_add r10 0x8 :: [64])
  %R14 = add i64 %R10, 8
  %r15 = bitcast i128 %r0 to <2 x double>
  %r16 = call { i64, i64, <2 x double> } @F4021a0(i64 4206466, i64 %R9, <2 x double> %r15)
  %R11 = extractvalue { i64, i64, <2 x double> } %r16, 0
  %R12 = extractvalue { i64, i64, <2 x double> } %r16, 1
  %r19 = extractvalue { i64, i64, <2 x double> } %r16, 2
  %R13 = bitcast <2 x double> %r19 to i128
  br label %block_402869
block_402869:
  %R25 = phi i128 [ %R13, %block_40284f ]
  %R24 = phi i64 [ %a1, %block_40284f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R23 = phi i64 [ undef, %block_40284f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R22 = phi i64 [ undef, %block_40284f ]
  %R21 = phi i64 [ %R11, %block_40284f ]
  %R20 = phi i64 [ %R12, %block_40284f ]
  %R19 = phi i64 [ %R6, %block_40284f ]
  %R18 = phi i64 [ %R14, %block_40284f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R17 = phi i64 [ undef, %block_40284f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R16 = phi i64 [ undef, %block_40284f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R15 = phi i64 [ undef, %block_40284f ]
  ; # 402869: test   rax,rax
  ; r26 := (bv_eq r15 0x0 :: [64])
  %R26 = icmp eq i64 %R15, 0
  ; # 40286c: jne    402880
  ; r27 := (bv_complement r26)
  %R27 = xor i1 %R26, -1
  br i1 %R27, label %subblock_402869_1, label %subblock_402869_2
subblock_402869_1:
  br label %block_402880
subblock_402869_2:
  br label %block_40286e
block_40286e:
  %R31 = phi i128 [ %R25, %subblock_402869_2 ]
  %R30 = phi i64 [ %R21, %subblock_402869_2 ]
  %R29 = phi i64 [ %R20, %subblock_402869_2 ]
  %R28 = phi i64 [ %R17, %subblock_402869_2 ]
  ; # 40286e: call   402681
  %r38 = bitcast i128 %R31 to <2 x double>
  %r39 = call { i64, i64, <2 x double> } @F402681(i64 %R30, i64 %R29, i64 %R28, <2 x double> %r38)
  %R32 = extractvalue { i64, i64, <2 x double> } %r39, 0
  %R33 = extractvalue { i64, i64, <2 x double> } %r39, 1
  %r42 = extractvalue { i64, i64, <2 x double> } %r39, 2
  %R34 = bitcast <2 x double> %r42 to i128
  br label %block_402873
block_402873:
  %R36 = phi i128 [ %R34, %block_40286e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R35 = phi i64 [ undef, %block_40286e ]
  ; # 402873: mov    DWORD PTR [rax],0x16
  ; *(r35) = 0x16 :: [32]
  %r46 = inttoptr i64 %R35 to i32*
  store i32 22, i32* %r46
  ; # 402879: xor    eax,eax
  ; # 40287b: jmp    4029a4
  br label %block_4029a4
block_402880:
  %R45 = phi i128 [ %R25, %subblock_402869_1 ]
  %R44 = phi i64 [ %R24, %subblock_402869_1 ]
  %R43 = phi i64 [ %R23, %subblock_402869_1 ]
  %R42 = phi i64 [ %R22, %subblock_402869_1 ]
  %R41 = phi i64 [ %R20, %subblock_402869_1 ]
  %R40 = phi i64 [ %R19, %subblock_402869_1 ]
  %R39 = phi i64 [ %R18, %subblock_402869_1 ]
  %R38 = phi i64 [ %R17, %subblock_402869_1 ]
  %R37 = phi i64 [ %R16, %subblock_402869_1 ]
  ; # 402880: mov    edi,0x4f0
  ; # 402885: call   401280
  ; r46 := (bv_add r39 0xfffffffffffffff8 :: [64])
  %R46 = add i64 %R39, 18446744073709551608
  ; r50 := (bv_add r46 0x8 :: [64])
  %R50 = add i64 %R46, 8
  %r58 = bitcast i128 %R45 to <2 x double>
  %r59 = call { i64, i64, <2 x double> } @F401280(i64 1264, i64 %R41, i64 %R38, i64 %R37, i64 %R42, i64 %R43, <2 x double> %r58)
  %R47 = extractvalue { i64, i64, <2 x double> } %r59, 0
  %R48 = extractvalue { i64, i64, <2 x double> } %r59, 1
  %r62 = extractvalue { i64, i64, <2 x double> } %r59, 2
  %R49 = bitcast <2 x double> %r62 to i128
  br label %block_40288a
block_40288a:
  %R55 = phi i128 [ %R49, %block_402880 ]
  %R54 = phi i64 [ %R44, %block_402880 ]
  %R53 = phi i64 [ %R40, %block_402880 ]
  %R52 = phi i64 [ %R50, %block_402880 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R51 = phi i64 [ undef, %block_402880 ]
  ; # 40288a: mov    rbx,rax
  ; # 40288d: xor    eax,eax
  ; # 40288f: test   rbx,rbx
  ; r56 := (bv_eq r51 0x0 :: [64])
  %R56 = icmp eq i64 %R51, 0
  ; # 402892: je     4029a4
  br i1 %R56, label %subblock_40288a_1, label %subblock_40288a_2
subblock_40288a_1:
  br label %block_4029a4
subblock_40288a_2:
  br label %block_402898
block_402898:
  %R61 = phi i128 [ %R55, %subblock_40288a_2 ]
  %R60 = phi i64 [ %R54, %subblock_40288a_2 ]
  %R59 = phi i64 [ %R53, %subblock_40288a_2 ]
  %R58 = phi i64 [ %R52, %subblock_40288a_2 ]
  %R57 = phi i64 [ %R51, %subblock_40288a_2 ]
  ; # 402898: xor    esi,esi
  ; # 40289a: mov    edx,0xe8
  ; # 40289f: mov    rdi,rbx
  ; # 4028a2: call   402d93
  ; r62 := (bv_add r58 0xfffffffffffffff8 :: [64])
  %R62 = add i64 %R58, 18446744073709551608
  ; r64 := (bv_add r62 0x8 :: [64])
  %R64 = add i64 %R62, 8
  %r77 = bitcast i128 %R61 to <2 x double>
  %r78 = call { i64, i64, <2 x double> } @F402d93(i64 %R57, i64 0, i64 232, <2 x double> %r77)
  %r79 = extractvalue { i64, i64, <2 x double> } %r78, 2
  %R63 = bitcast <2 x double> %r79 to i128
  br label %block_4028a7
block_4028a7:
  %R69 = phi i128 [ %R63, %block_402898 ]
  %R68 = phi i64 [ %R60, %block_402898 ]
  %R67 = phi i64 [ %R59, %block_402898 ]
  %R66 = phi i64 [ %R64, %block_402898 ]
  %R65 = phi i64 [ %R57, %block_402898 ]
  ; # 4028a7: mov    esi,0x2b
  ; # 4028ac: mov    rdi,r12
  ; # 4028af: call   4021a0
  ; r70 := (bv_add r66 0xfffffffffffffff8 :: [64])
  %R70 = add i64 %R66, 18446744073709551608
  ; r74 := (bv_add r70 0x8 :: [64])
  %R74 = add i64 %R70, 8
  %r88 = bitcast i128 %R69 to <2 x double>
  %r89 = call { i64, i64, <2 x double> } @F4021a0(i64 %R68, i64 43, <2 x double> %r88)
  %R71 = extractvalue { i64, i64, <2 x double> } %r89, 0
  %R72 = extractvalue { i64, i64, <2 x double> } %r89, 1
  %r92 = extractvalue { i64, i64, <2 x double> } %r89, 2
  %R73 = bitcast <2 x double> %r92 to i128
  br label %block_4028b4
block_4028b4:
  %R80 = phi i128 [ %R73, %block_4028a7 ]
  %R79 = phi i64 [ %R68, %block_4028a7 ]
  %R78 = phi i64 [ %R67, %block_4028a7 ]
  %R77 = phi i64 [ %R74, %block_4028a7 ]
  %R76 = phi i64 [ %R65, %block_4028a7 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R75 = phi i64 [ undef, %block_4028a7 ]
  ; # 4028b4: test   rax,rax
  ; r81 := (bv_eq r75 0x0 :: [64])
  %R81 = icmp eq i64 %R75, 0
  ; # 4028b7: jne    4028cc
  ; r82 := (bv_complement r81)
  %R82 = xor i1 %R81, -1
  br i1 %R82, label %subblock_4028b4_1, label %subblock_4028b4_2
subblock_4028b4_1:
  br label %block_4028cc
subblock_4028b4_2:
  br label %block_4028b9
block_4028b9:
  %R87 = phi i128 [ %R80, %subblock_4028b4_2 ]
  %R86 = phi i64 [ %R79, %subblock_4028b4_2 ]
  %R85 = phi i64 [ %R78, %subblock_4028b4_2 ]
  %R84 = phi i64 [ %R77, %subblock_4028b4_2 ]
  %R83 = phi i64 [ %R76, %subblock_4028b4_2 ]
  ; # 4028b9: xor    eax,eax
  ; # 4028bb: cmp    BYTE PTR [r12],0x72
  ; r88 := *r86
  %r107 = inttoptr i64 %R86 to i8*
  %R88 = load i8* %r107
  ; r89 := (bv_eq r88 0x72 :: [8])
  %R89 = icmp eq i8 %R88, 114
  ; # 4028c0: sete   al
  ; r90 := (mux r89 0x1 :: [8] 0x0 :: [8])
  %R90 = select i1 %R89, i8 1, i8 0
  ; r91 := (uext r90 64)
  %R91 = zext i8 %R90 to i64
  ; # 4028c3: lea    eax,[rax*4+0x4]
  ; r92 := (bv_mul 0x4 :: [64] r91)
  %R92 = mul i64 4, %R91
  ; r93 := (bv_add r92 0x4 :: [64])
  %R93 = add i64 %R92, 4
  ; r94 := (trunc r93 32)
  %R94 = trunc i64 %R93 to i32
  ; # 4028ca: mov    DWORD PTR [rbx],eax
  ; *(r83) = r94
  %r115 = inttoptr i64 %R83 to i32*
  store i32 %R94, i32* %r115
  br label %block_4028cc
block_4028cc:
  %R99 = phi i128 [ %R87, %block_4028b9 ], [ %R80, %subblock_4028b4_1 ]
  %R98 = phi i64 [ %R86, %block_4028b9 ], [ %R79, %subblock_4028b4_1 ]
  %R97 = phi i64 [ %R85, %block_4028b9 ], [ %R78, %subblock_4028b4_1 ]
  %R96 = phi i64 [ %R84, %block_4028b9 ], [ %R77, %subblock_4028b4_1 ]
  %R95 = phi i64 [ %R83, %block_4028b9 ], [ %R76, %subblock_4028b4_1 ]
  ; # 4028cc: mov    esi,0x65
  ; # 4028d1: mov    rdi,r12
  ; # 4028d4: call   4021a0
  ; r100 := (bv_add r96 0xfffffffffffffff8 :: [64])
  %R100 = add i64 %R96, 18446744073709551608
  ; r104 := (bv_add r100 0x8 :: [64])
  %R104 = add i64 %R100, 8
  %r123 = bitcast i128 %R99 to <2 x double>
  %r124 = call { i64, i64, <2 x double> } @F4021a0(i64 %R98, i64 101, <2 x double> %r123)
  %R101 = extractvalue { i64, i64, <2 x double> } %r124, 0
  %R102 = extractvalue { i64, i64, <2 x double> } %r124, 1
  %r127 = extractvalue { i64, i64, <2 x double> } %r124, 2
  %R103 = bitcast <2 x double> %r127 to i128
  br label %block_4028d9
block_4028d9:
  %R112 = phi i128 [ %R103, %block_4028cc ]
  %R111 = phi i64 [ %R98, %block_4028cc ]
  %R110 = phi i64 [ %R102, %block_4028cc ]
  %R109 = phi i64 [ %R97, %block_4028cc ]
  %R108 = phi i64 [ %R104, %block_4028cc ]
  %R107 = phi i64 [ %R95, %block_4028cc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R106 = phi i64 [ undef, %block_4028cc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R105 = phi i64 [ undef, %block_4028cc ]
  ; # 4028d9: test   rax,rax
  ; r113 := (bv_eq r105 0x0 :: [64])
  %R113 = icmp eq i64 %R105, 0
  ; # 4028dc: je     4028f2
  br i1 %R113, label %subblock_4028d9_1, label %subblock_4028d9_2
subblock_4028d9_1:
  br label %block_4028f2
subblock_4028d9_2:
  br label %block_4028de
block_4028de:
  %R117 = phi i64 [ %R111, %subblock_4028d9_2 ]
  %R116 = phi i64 [ %R109, %subblock_4028d9_2 ]
  %R115 = phi i64 [ %R108, %subblock_4028d9_2 ]
  %R114 = phi i64 [ %R107, %subblock_4028d9_2 ]
  ; # 4028de: movsxd rdi,ebp
  ; r118 := (trunc r116 32)
  %R118 = trunc i64 %R116 to i32
  ; r119 := (sext r118 64)
  %R119 = sext i32 %R118 to i64
  ; # 4028e1: mov    edx,0x1
  ; # 4028e6: mov    esi,0x2
  ; # 4028eb: mov    eax,0x48
  ; # 4028f0: syscall
  ; sys_fcntl
  %r144 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R119, i64 2, i64 1, i64 undef, i64 undef, i64 undef, i64 72)
  %R120 = extractvalue { i64, i1 } %r144, 0
  br label %block_4028f2
block_4028f2:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R127 = phi i128 [ undef, %block_4028de ], [ %R112, %subblock_4028d9_1 ]
  %R126 = phi i64 [ %R117, %block_4028de ], [ %R111, %subblock_4028d9_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R125 = phi i64 [ undef, %block_4028de ], [ %R110, %subblock_4028d9_1 ]
  %R124 = phi i64 [ %R116, %block_4028de ], [ %R109, %subblock_4028d9_1 ]
  %R123 = phi i64 [ %R115, %block_4028de ], [ %R108, %subblock_4028d9_1 ]
  %R122 = phi i64 [ %R114, %block_4028de ], [ %R107, %subblock_4028d9_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R121 = phi i64 [ undef, %block_4028de ], [ %R106, %subblock_4028d9_1 ]
  ; # 4028f2: cmp    BYTE PTR [r12],0x61
  ; r128 := *r126
  %r153 = inttoptr i64 %R126 to i8*
  %R128 = load i8* %r153
  ; r129 := (bv_eq r128 0x61 :: [8])
  %R129 = icmp eq i8 %R128, 97
  ; # 4028f7: jne    402927
  ; r130 := (bv_complement r129)
  %R130 = xor i1 %R129, -1
  br i1 %R130, label %subblock_4028f2_1, label %subblock_4028f2_2
subblock_4028f2_1:
  br label %block_402927
subblock_4028f2_2:
  br label %block_4028f9
block_4028f9:
  %R134 = phi i64 [ %R124, %subblock_4028f2_2 ]
  %R133 = phi i64 [ %R123, %subblock_4028f2_2 ]
  %R132 = phi i64 [ %R122, %subblock_4028f2_2 ]
  %R131 = phi i64 [ %R121, %subblock_4028f2_2 ]
  ; # 4028f9: mov    r8d,0x48
  ; # 4028ff: movsxd rdi,ebp
  ; r135 := (trunc r134 32)
  %R135 = trunc i64 %R134 to i32
  ; r136 := (sext r135 64)
  %R136 = sext i32 %R135 to i64
  ; # 402902: mov    esi,0x3
  ; # 402907: mov    rax,r8
  ; # 40290a: syscall
  ; sys_fcntl
  %r163 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R136, i64 3, i64 %R131, i64 undef, i64 undef, i64 undef, i64 72)
  %R137 = extractvalue { i64, i1 } %r163, 0
  br label %block_40290c
block_40290c:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R145 = phi i128 [ undef, %block_4028f9 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R144 = phi i64 [ undef, %block_4028f9 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R143 = phi i64 [ undef, %block_4028f9 ]
  %R142 = phi i64 [ %R134, %block_4028f9 ]
  %R141 = phi i64 [ %R133, %block_4028f9 ]
  %R140 = phi i64 [ %R132, %block_4028f9 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R139 = phi i64 [ undef, %block_4028f9 ]
  %R138 = phi i64 [ %R137, %block_4028f9 ]
  ; # 40290c: test   ah,0x4
  ; r146 := (bv_shr r138 0x8 :: [64])
  %R146 = lshr i64 %R138, 8
  ; r147 := (trunc r146 8)
  %R147 = trunc i64 %R146 to i8
  ; r148 := (bv_and r147 0x4 :: [8])
  %R148 = and i8 %R147, 4
  ; r149 := (bv_eq r148 0x0 :: [8])
  %R149 = icmp eq i8 %R148, 0
  ; # 40290f: jne    402921
  ; r150 := (bv_complement r149)
  %R150 = xor i1 %R149, -1
  br i1 %R150, label %subblock_40290c_1, label %subblock_40290c_2
subblock_40290c_1:
  br label %block_402921
subblock_40290c_2:
  br label %block_402911
block_402911:
  %R155 = phi i64 [ %R144, %subblock_40290c_2 ]
  %R154 = phi i64 [ %R142, %subblock_40290c_2 ]
  %R153 = phi i64 [ %R141, %subblock_40290c_2 ]
  %R152 = phi i64 [ %R140, %subblock_40290c_2 ]
  %R151 = phi i64 [ %R138, %subblock_40290c_2 ]
  ; # 402911: or     ah,0x4
  ; r156 := (bv_shr r151 0x8 :: [64])
  %R156 = lshr i64 %R151, 8
  ; r157 := (trunc r156 8)
  %R157 = trunc i64 %R156 to i8
  ; r158 := (bv_or r157 0x4 :: [8])
  %R158 = or i8 %R157, 4
  ; r159 := (bv_and r151 0xffffffffffff0000 :: [64])
  %R159 = and i64 %R151, 18446744073709486080
  ; r160 := (uext r158 64)
  %R160 = zext i8 %R158 to i64
  ; r161 := (bv_shl r160 0x8 :: [64])
  %R161 = shl i64 %R160, 8
  ; r162 := (bv_or r159 r161)
  %R162 = or i64 %R159, %R161
  ; r163 := (bv_shl r151 0x38 :: [64])
  %R163 = shl i64 %R151, 56
  ; r164 := (bv_shr r163 0x38 :: [64])
  %R164 = lshr i64 %R163, 56
  ; r165 := (bv_or r162 r164)
  %R165 = or i64 %R162, %R164
  ; # 402914: mov    esi,0x4
  ; # 402919: movsxd rdx,eax
  ; r166 := (trunc r165 32)
  %R166 = trunc i64 %R165 to i32
  ; r167 := (sext r166 64)
  %R167 = sext i32 %R166 to i64
  ; # 40291c: mov    rax,r8
  ; # 40291f: syscall
  ; sys_fcntl
  %r195 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R155, i64 4, i64 %R167, i64 undef, i64 undef, i64 undef, i64 72)
  %R168 = extractvalue { i64, i1 } %r195, 0
  br label %block_402921
block_402921:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R174 = phi i128 [ undef, %block_402911 ], [ %R145, %subblock_40290c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R173 = phi i64 [ undef, %block_402911 ], [ %R143, %subblock_40290c_1 ]
  %R172 = phi i64 [ %R154, %block_402911 ], [ %R142, %subblock_40290c_1 ]
  %R171 = phi i64 [ %R153, %block_402911 ], [ %R141, %subblock_40290c_1 ]
  %R170 = phi i64 [ %R152, %block_402911 ], [ %R140, %subblock_40290c_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R169 = phi i64 [ undef, %block_402911 ], [ %R139, %subblock_40290c_1 ]
  ; # 402921: or     DWORD PTR [rbx],0x80
  ; r175 := *r170
  %r203 = inttoptr i64 %R170 to i32*
  %R175 = load i32* %r203
  ; r176 := (bv_or r175 0x80 :: [32])
  %R176 = or i32 %R175, 128
  ; *(r170) = r176
  %r206 = inttoptr i64 %R170 to i32*
  store i32 %R176, i32* %r206
  br label %block_402927
block_402927:
  %R182 = phi i128 [ %R174, %block_402921 ], [ %R127, %subblock_4028f2_1 ]
  %R181 = phi i64 [ %R173, %block_402921 ], [ %R125, %subblock_4028f2_1 ]
  %R180 = phi i64 [ %R172, %block_402921 ], [ %R124, %subblock_4028f2_1 ]
  %R179 = phi i64 [ %R171, %block_402921 ], [ %R123, %subblock_4028f2_1 ]
  %R178 = phi i64 [ %R170, %block_402921 ], [ %R122, %subblock_4028f2_1 ]
  %R177 = phi i64 [ %R169, %block_402921 ], [ %R121, %subblock_4028f2_1 ]
  ; # 402927: test   BYTE PTR [rbx],0x8
  ; r183 := *r178
  %r213 = inttoptr i64 %R178 to i8*
  %R183 = load i8* %r213
  ; r184 := (bv_and r183 0x8 :: [8])
  %R184 = and i8 %R183, 8
  ; r185 := (bv_eq r184 0x0 :: [8])
  %R185 = icmp eq i8 %R184, 0
  ; # 40292a: lea    rax,[rbx+0xf0]
  ; r186 := (bv_add r178 0xf0 :: [64])
  %R186 = add i64 %R178, 240
  ; # 402931: mov    DWORD PTR [rbx+0x78],ebp
  ; r187 := (trunc r180 32)
  %R187 = trunc i64 %R180 to i32
  ; r188 := (bv_add r178 0x78 :: [64])
  %R188 = add i64 %R178, 120
  ; *(r188) = r187
  %r220 = inttoptr i64 %R188 to i32*
  store i32 %R187, i32* %r220
  ; # 402934: mov    QWORD PTR [rbx+0x60],0x400
  ; r189 := (bv_add r178 0x60 :: [64])
  %R189 = add i64 %R178, 96
  ; *(r189) = 0x400 :: [64]
  %r222 = inttoptr i64 %R189 to i64*
  store i64 1024, i64* %r222
  ; # 40293c: mov    BYTE PTR [rbx+0x8b],0xff
  ; r190 := (bv_add r178 0x8b :: [64])
  %R190 = add i64 %R178, 139
  ; *(r190) = 0xff :: [8]
  %r224 = inttoptr i64 %R190 to i8*
  store i8 255, i8* %r224
  ; # 402943: mov    QWORD PTR [rbx+0x58],rax
  ; r191 := (bv_add r178 0x58 :: [64])
  %R191 = add i64 %R178, 88
  ; *(r191) = r186
  %r226 = inttoptr i64 %R191 to i64*
  store i64 %R186, i64* %r226
  ; # 402947: jne    402969
  ; r192 := (bv_complement r185)
  %R192 = xor i1 %R185, -1
  br i1 %R192, label %subblock_402927_1, label %subblock_402927_2
subblock_402927_1:
  br label %block_402969
subblock_402927_2:
  br label %block_402949
block_402949:
  %R195 = phi i64 [ %R180, %subblock_402927_2 ]
  %R194 = phi i64 [ %R179, %subblock_402927_2 ]
  %R193 = phi i64 [ %R178, %subblock_402927_2 ]
  ; # 402949: movsxd rdi,ebp
  ; r196 := (trunc r195 32)
  %R196 = trunc i64 %R195 to i32
  ; r197 := (sext r196 64)
  %R197 = sext i32 %R196 to i64
  ; # 40294c: lea    rdx,[rsp+0x8]
  ; r198 := (bv_add r194 0x8 :: [64])
  %R198 = add i64 %R194, 8
  ; # 402951: mov    esi,0x5413
  ; # 402956: mov    eax,0x10
  ; # 40295b: syscall
  ; sys_ioctl
  %r234 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R197, i64 21523, i64 %R198, i64 undef, i64 undef, i64 undef, i64 16)
  %R199 = extractvalue { i64, i1 } %r234, 0
  br label %block_40295d
block_40295d:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R204 = phi i128 [ undef, %block_402949 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R203 = phi i64 [ undef, %block_402949 ]
  %R202 = phi i64 [ %R193, %block_402949 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R201 = phi i64 [ undef, %block_402949 ]
  %R200 = phi i64 [ %R199, %block_402949 ]
  ; # 40295d: test   rax,rax
  ; r205 := (bv_eq r200 0x0 :: [64])
  %R205 = icmp eq i64 %R200, 0
  ; # 402960: jne    402969
  ; r206 := (bv_complement r205)
  %R206 = xor i1 %R205, -1
  br i1 %R206, label %subblock_40295d_1, label %subblock_40295d_2
subblock_40295d_1:
  br label %block_402969
subblock_40295d_2:
  br label %block_402962
block_402962:
  %R210 = phi i128 [ %R204, %subblock_40295d_2 ]
  %R209 = phi i64 [ %R203, %subblock_40295d_2 ]
  %R208 = phi i64 [ %R202, %subblock_40295d_2 ]
  %R207 = phi i64 [ %R201, %subblock_40295d_2 ]
  ; # 402962: mov    BYTE PTR [rbx+0x8b],0xa
  ; r211 := (bv_add r208 0x8b :: [64])
  %R211 = add i64 %R208, 139
  ; *(r211) = 0xa :: [8]
  %r248 = inttoptr i64 %R211 to i8*
  store i8 10, i8* %r248
  br label %block_402969
block_402969:
  %R215 = phi i128 [ %R210, %block_402962 ], [ %R204, %subblock_40295d_1 ], [ %R182, %subblock_402927_1 ]
  %R214 = phi i64 [ %R209, %block_402962 ], [ %R203, %subblock_40295d_1 ], [ %R181, %subblock_402927_1 ]
  %R213 = phi i64 [ %R208, %block_402962 ], [ %R202, %subblock_40295d_1 ], [ %R178, %subblock_402927_1 ]
  %R212 = phi i64 [ %R207, %block_402962 ], [ %R201, %subblock_40295d_1 ], [ %R177, %subblock_402927_1 ]
  ; # 402969: cmp    DWORD PTR [rip+0x202014],0x0
  ; r216 := *0x604984 :: [64]
  %r253 = inttoptr i64 6310276 to i32*
  %R216 = load i32* %r253
  ; r217 := (bv_eq r216 0x0 :: [32])
  %R217 = icmp eq i32 %R216, 0
  ; # 402970: mov    QWORD PTR [rbx+0x40],0x402b4f
  ; r218 := (bv_add r213 0x40 :: [64])
  %R218 = add i64 %R213, 64
  ; *(r218) = FunctionEntry(402b4f)
  %r257 = inttoptr i64 %R218 to i64*
  store i64 ptrtoint ({ i64, i64, <2 x double> }(i64, i64, i64)* @F402b4f to i64), i64* %r257
  ; # 402978: mov    QWORD PTR [rbx+0x48],0x402c09
  ; r219 := (bv_add r213 0x48 :: [64])
  %R219 = add i64 %R213, 72
  ; *(r219) = FunctionEntry(402c09)
  %r259 = inttoptr i64 %R219 to i64*
  store i64 ptrtoint ({ i64, i64, <2 x double> }(i64, i64, i64)* @F402c09 to i64), i64* %r259
  ; # 402980: mov    QWORD PTR [rbx+0x50],0x402bf3
  ; r220 := (bv_add r213 0x50 :: [64])
  %R220 = add i64 %R213, 80
  ; *(r220) = FunctionEntry(402bf3)
  %r261 = inttoptr i64 %R220 to i64*
  store i64 ptrtoint ({ i64, i64, <2 x double> }(i64, i64, i64)* @F402bf3 to i64), i64* %r261
  ; # 402988: mov    QWORD PTR [rbx+0x18],0x402b32
  ; r221 := (bv_add r213 0x18 :: [64])
  %R221 = add i64 %R213, 24
  ; *(r221) = FunctionEntry(402b32)
  %r263 = inttoptr i64 %R221 to i64*
  store i64 ptrtoint ({ i64, i64, <2 x double> }(i64)* @F402b32 to i64), i64* %r263
  ; # 402990: jne    40299c
  ; r222 := (bv_complement r217)
  %R222 = xor i1 %R217, -1
  br i1 %R222, label %subblock_402969_1, label %subblock_402969_2
subblock_402969_1:
  br label %block_40299c
subblock_402969_2:
  br label %block_402992
block_402992:
  %R226 = phi i128 [ %R215, %subblock_402969_2 ]
  %R225 = phi i64 [ %R214, %subblock_402969_2 ]
  %R224 = phi i64 [ %R213, %subblock_402969_2 ]
  %R223 = phi i64 [ %R212, %subblock_402969_2 ]
  ; # 402992: mov    DWORD PTR [rbx+0x8c],0xffffffff
  ; r227 := (bv_add r224 0x8c :: [64])
  %R227 = add i64 %R224, 140
  ; *(r227) = 0xffffffff :: [32]
  %r270 = inttoptr i64 %R227 to i32*
  store i32 4294967295, i32* %r270
  br label %block_40299c
block_40299c:
  %R231 = phi i128 [ %R226, %block_402992 ], [ %R215, %subblock_402969_1 ]
  %R230 = phi i64 [ %R225, %block_402992 ], [ %R214, %subblock_402969_1 ]
  %R229 = phi i64 [ %R224, %block_402992 ], [ %R213, %subblock_402969_1 ]
  %R228 = phi i64 [ %R223, %block_402992 ], [ %R212, %subblock_402969_1 ]
  ; # 40299c: mov    rdi,rbx
  ; # 40299f: call   402d6a
  %r275 = bitcast i128 %R231 to <2 x double>
  %r276 = call { i64, i64, <2 x double> } @F402d6a(i64 %R229, i64 %R230, i64 %R228, <2 x double> %r275)
  %R232 = extractvalue { i64, i64, <2 x double> } %r276, 0
  %r278 = extractvalue { i64, i64, <2 x double> } %r276, 2
  %R233 = bitcast <2 x double> %r278 to i128
  br label %block_4029a4
block_4029a4:
  %R235 = phi i128 [ %R36, %block_402873 ], [ %R233, %block_40299c ], [ %R55, %subblock_40288a_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R234 = phi i64 [ 0, %block_402873 ], [ undef, %block_40299c ], [ 0, %subblock_40288a_1 ]
  ; # 4029a4: add    rsp,0x10
  ; # 4029a8: pop    rbx
  ; # 4029a9: pop    rbp
  ; # 4029aa: pop    r12
  ; # 4029ac: ret
  %r282 = bitcast i128 %R235 to <2 x double>
  %r283 = insertvalue { i64, i64, <2 x double> } undef, i64 %R234, 0
  %r284 = insertvalue { i64, i64, <2 x double> } %r283, i64 undef, 1
  %r285 = insertvalue { i64, i64, <2 x double> } %r284, <2 x double> %r282, 2
  ret { i64, i64, <2 x double> } %r285
failure:
  br label %failure
}