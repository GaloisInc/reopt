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
define { i64, i64, <2 x double> } @F40241c(i64 %a0, i64 %a1, i64 %a2, i64 %a3, <2 x double> %a4) {
entry:
  %r0 = bitcast <2 x double> %a4 to i128
  br label %block_40241c
block_40241c:
  ; r0 := (alloca 0x18 :: [64])
  %r1 = alloca i8, i64 24
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x18 :: [64])
  %R1 = add i64 %R0, 24
  ; # 40241c: mov    eax,0x80
  ; # 402421: test   ecx,ecx
  ; r2 := (trunc arg3 32)
  %R2 = trunc i64 %a3 to i32
  ; r3 := (bv_eq r2 0x0 :: [32])
  %R3 = icmp eq i32 %R2, 0
  ; # 402423: push   r12
  ; r4 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R4 = add i64 %R1, 18446744073709551608
  ; # 402425: cmovne ecx,eax
  ; r5 := (mux r3 r2 0x80 :: [32])
  %R5 = select i1 %R3, i32 %R2, i32 128
  ; r6 := (uext r5 64)
  %R6 = zext i32 %R5 to i64
  ; # 402428: push   rbp
  ; r7 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R7 = add i64 %R1, 18446744073709551600
  ; # 402429: mov    r8,rsi
  ; # 40242c: push   rbx
  ; r8 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R8 = add i64 %R1, 18446744073709551592
  ; # 40242d: mov    eax,0x65
  ; # 402432: mov    ebx,edx
  ; r9 := (trunc arg2 32)
  %R9 = trunc i64 %a2 to i32
  ; r10 := (uext r9 64)
  %R10 = zext i32 %R9 to i64
  br label %block_402434
block_402434:
  %R16 = phi i128 [ %R44, %block_402443 ], [ %r0, %block_40241c ]
  %R15 = phi i64 [ %R43, %block_402443 ], [ %a1, %block_40241c ]
  %R14 = phi i64 [ %R42, %block_402443 ], [ %a0, %block_40241c ]
  %R13 = phi i64 [ %R41, %block_402443 ], [ %R10, %block_40241c ]
  %R12 = phi i64 [ %R40, %block_402443 ], [ %R6, %block_40241c ]
  %R11 = phi i64 [ %R39, %block_402443 ], [ 101, %block_40241c ]
  ; # 402434: dec    eax
  ; r17 := (trunc r11 32)
  %R17 = trunc i64 %R11 to i32
  ; r18 := (bv_add r17 0xffffffff :: [32])
  %R18 = add i32 %R17, 4294967295
  ; r19 := (bv_eq r17 0x1 :: [32])
  %R19 = icmp eq i32 %R17, 1
  ; r20 := (uext r18 64)
  %R20 = zext i32 %R18 to i64
  ; # 402436: je     40244e
  br i1 %R19, label %subblock_402434_1, label %subblock_402434_2
subblock_402434_1:
  br label %block_40244e
subblock_402434_2:
  br label %block_402438
block_402438:
  %R26 = phi i128 [ %R16, %subblock_402434_2 ]
  %R25 = phi i64 [ %R15, %subblock_402434_2 ]
  %R24 = phi i64 [ %R14, %subblock_402434_2 ]
  %R23 = phi i64 [ %R13, %subblock_402434_2 ]
  %R22 = phi i64 [ %R12, %subblock_402434_2 ]
  %R21 = phi i64 [ %R20, %subblock_402434_2 ]
  ; # 402438: test   r8,r8
  ; r27 := (bv_eq r25 0x0 :: [64])
  %R27 = icmp eq i64 %R25, 0
  ; # 40243b: jne    402445
  ; r28 := (bv_complement r27)
  %R28 = xor i1 %R27, -1
  br i1 %R28, label %subblock_402438_1, label %subblock_402438_2
subblock_402438_1:
  br label %block_402445
subblock_402438_2:
  br label %block_40243d
block_40243d:
  %R34 = phi i128 [ %R50, %subblock_402445_1 ], [ %R26, %subblock_402438_2 ]
  %R33 = phi i64 [ %R49, %subblock_402445_1 ], [ %R25, %subblock_402438_2 ]
  %R32 = phi i64 [ %R48, %subblock_402445_1 ], [ %R24, %subblock_402438_2 ]
  %R31 = phi i64 [ %R47, %subblock_402445_1 ], [ %R23, %subblock_402438_2 ]
  %R30 = phi i64 [ %R46, %subblock_402445_1 ], [ %R22, %subblock_402438_2 ]
  %R29 = phi i64 [ %R45, %subblock_402445_1 ], [ %R21, %subblock_402438_2 ]
  ; # 40243d: mov    edx,DWORD PTR [rdi]
  ; r35 := *r32
  %r37 = inttoptr i64 %R32 to i32*
  %R35 = load i32* %r37
  ; # 40243f: cmp    edx,ebx
  ; r36 := (trunc r31 32)
  %R36 = trunc i64 %R31 to i32
  ; r37 := (bv_eq r35 r36)
  %R37 = icmp eq i32 %R35, %R36
  ; # 402441: jne    402491
  ; r38 := (bv_complement r37)
  %R38 = xor i1 %R37, -1
  br i1 %R38, label %subblock_40243d_1, label %subblock_40243d_2
subblock_40243d_1:
  br label %block_402491
subblock_40243d_2:
  br label %block_402443
block_402443:
  %R44 = phi i128 [ %R34, %subblock_40243d_2 ]
  %R43 = phi i64 [ %R33, %subblock_40243d_2 ]
  %R42 = phi i64 [ %R32, %subblock_40243d_2 ]
  %R41 = phi i64 [ %R31, %subblock_40243d_2 ]
  %R40 = phi i64 [ %R30, %subblock_40243d_2 ]
  %R39 = phi i64 [ %R29, %subblock_40243d_2 ]
  ; # 402443: jmp    402434
  br label %block_402434
block_402445:
  %R50 = phi i128 [ %R26, %subblock_402438_1 ]
  %R49 = phi i64 [ %R25, %subblock_402438_1 ]
  %R48 = phi i64 [ %R24, %subblock_402438_1 ]
  %R47 = phi i64 [ %R23, %subblock_402438_1 ]
  %R46 = phi i64 [ %R22, %subblock_402438_1 ]
  %R45 = phi i64 [ %R21, %subblock_402438_1 ]
  ; # 402445: mov    edx,DWORD PTR [r8]
  ; r51 := *r49
  %r54 = inttoptr i64 %R49 to i32*
  %R51 = load i32* %r54
  ; # 402448: test   edx,edx
  ; r52 := (bv_eq r51 0x0 :: [32])
  %R52 = icmp eq i32 %R51, 0
  ; # 40244a: je     40243d
  br i1 %R52, label %subblock_402445_1, label %subblock_402445_2
subblock_402445_1:
  br label %block_40243d
subblock_402445_2:
  br label %block_40244c
block_40244c:
  %R57 = phi i128 [ %R50, %subblock_402445_2 ]
  %R56 = phi i64 [ %R49, %subblock_402445_2 ]
  %R55 = phi i64 [ %R48, %subblock_402445_2 ]
  %R54 = phi i64 [ %R47, %subblock_402445_2 ]
  %R53 = phi i64 [ %R46, %subblock_402445_2 ]
  ; # 40244c: jmp    402453
  br label %block_402453
block_40244e:
  %R62 = phi i128 [ %R16, %subblock_402434_1 ]
  %R61 = phi i64 [ %R15, %subblock_402434_1 ]
  %R60 = phi i64 [ %R14, %subblock_402434_1 ]
  %R59 = phi i64 [ %R13, %subblock_402434_1 ]
  %R58 = phi i64 [ %R12, %subblock_402434_1 ]
  ; # 40244e: test   r8,r8
  ; r63 := (bv_eq r61 0x0 :: [64])
  %R63 = icmp eq i64 %R61, 0
  ; # 402451: je     402457
  br i1 %R63, label %subblock_40244e_1, label %subblock_40244e_2
subblock_40244e_1:
  br label %block_402457
subblock_40244e_2:
  br label %block_402453
block_402453:
  %R68 = phi i128 [ %R57, %block_40244c ], [ %R62, %subblock_40244e_2 ]
  %R67 = phi i64 [ %R56, %block_40244c ], [ %R61, %subblock_40244e_2 ]
  %R66 = phi i64 [ %R55, %block_40244c ], [ %R60, %subblock_40244e_2 ]
  %R65 = phi i64 [ %R54, %block_40244c ], [ %R59, %subblock_40244e_2 ]
  %R64 = phi i64 [ %R53, %block_40244c ], [ %R58, %subblock_40244e_2 ]
  ; # 402453: lock inc DWORD PTR [r8]
  ; r69 := *r67
  %r73 = inttoptr i64 %R67 to i32*
  %R69 = load i32* %r73
  ; r70 := (bv_add r69 0x1 :: [32])
  %R70 = add i32 %R69, 1
  ; *(r67) = r70
  %r76 = inttoptr i64 %R67 to i32*
  store i32 %R70, i32* %r76
  br label %block_402457
block_402457:
  %R75 = phi i128 [ %R68, %block_402453 ], [ %R62, %subblock_40244e_1 ]
  %R74 = phi i64 [ %R67, %block_402453 ], [ %R61, %subblock_40244e_1 ]
  %R73 = phi i64 [ %R66, %block_402453 ], [ %R60, %subblock_40244e_1 ]
  %R72 = phi i64 [ %R65, %block_402453 ], [ %R59, %subblock_40244e_1 ]
  %R71 = phi i64 [ %R64, %block_402453 ], [ %R58, %subblock_40244e_1 ]
  ; # 402457: movsxd rdx,ebx
  ; r76 := (trunc r72 32)
  %R76 = trunc i64 %R72 to i32
  ; r77 := (sext r76 64)
  %R77 = sext i32 %R76 to i64
  ; # 40245a: mov    ebp,0xca
  ; # 40245f: movsxd r9,ecx
  ; r78 := (trunc r71 32)
  %R78 = trunc i64 %R71 to i32
  ; r79 := (sext r78 64)
  %R79 = sext i32 %R78 to i64
  ; # 402462: mov    r12d,0xca
  br label %block_402468
block_402468:
  %R85 = phi i128 [ %R118, %block_402486 ], [ %R103, %subblock_402479_1 ], [ %R75, %block_402457 ]
  %R84 = phi i64 [ %R117, %block_402486 ], [ %R101, %subblock_402479_1 ], [ %R79, %block_402457 ]
  %R83 = phi i64 [ %R116, %block_402486 ], [ %R100, %subblock_402479_1 ], [ %R74, %block_402457 ]
  %R82 = phi i64 [ %R115, %block_402486 ], [ %R99, %subblock_402479_1 ], [ %R73, %block_402457 ]
  %R81 = phi i64 [ %R114, %block_402486 ], [ %R98, %subblock_402479_1 ], [ %R72, %block_402457 ]
  %R80 = phi i64 [ %R113, %block_402486 ], [ %R97, %subblock_402479_1 ], [ %R77, %block_402457 ]
  ; # 402468: mov    eax,DWORD PTR [rdi]
  ; r86 := *r82
  %r92 = inttoptr i64 %R82 to i32*
  %R86 = load i32* %r92
  ; # 40246a: cmp    ebx,eax
  ; r87 := (trunc r81 32)
  %R87 = trunc i64 %R81 to i32
  ; r88 := (bv_eq r87 r86)
  %R88 = icmp eq i32 %R87, %R86
  ; # 40246c: jne    402488
  ; r89 := (bv_complement r88)
  %R89 = xor i1 %R88, -1
  br i1 %R89, label %subblock_402468_1, label %subblock_402468_2
subblock_402468_1:
  br label %block_402488
subblock_402468_2:
  br label %block_40246e
block_40246e:
  %R94 = phi i64 [ %R84, %subblock_402468_2 ]
  %R93 = phi i64 [ %R83, %subblock_402468_2 ]
  %R92 = phi i64 [ %R82, %subblock_402468_2 ]
  %R91 = phi i64 [ %R81, %subblock_402468_2 ]
  %R90 = phi i64 [ %R80, %subblock_402468_2 ]
  ; # 40246e: xor    r10d,r10d
  ; # 402471: mov    rax,rbp
  ; # 402474: mov    rsi,r9
  ; # 402477: syscall
  ; sys_futex
  %r102 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R92, i64 %R94, i64 %R90, i64 0, i64 %R93, i64 %R94, i64 202)
  %R95 = extractvalue { i64, i1 } %r102, 0
  br label %block_402479
block_402479:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R103 = phi i128 [ undef, %block_40246e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R102 = phi i64 [ undef, %block_40246e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R101 = phi i64 [ undef, %block_40246e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R100 = phi i64 [ undef, %block_40246e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R99 = phi i64 [ undef, %block_40246e ]
  %R98 = phi i64 [ %R91, %block_40246e ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R97 = phi i64 [ undef, %block_40246e ]
  %R96 = phi i64 [ %R95, %block_40246e ]
  ; # 402479: cmp    rax,0xffffffffffffffda
  ; r104 := (bv_eq r96 0xffffffffffffffda :: [64])
  %R104 = icmp eq i64 %R96, 18446744073709551578
  ; # 40247d: jne    402468
  ; r105 := (bv_complement r104)
  %R105 = xor i1 %R104, -1
  br i1 %R105, label %subblock_402479_1, label %subblock_402479_2
subblock_402479_1:
  br label %block_402468
subblock_402479_2:
  br label %block_40247f
block_40247f:
  %R111 = phi i64 [ %R102, %subblock_402479_2 ]
  %R110 = phi i64 [ %R101, %subblock_402479_2 ]
  %R109 = phi i64 [ %R100, %subblock_402479_2 ]
  %R108 = phi i64 [ %R99, %subblock_402479_2 ]
  %R107 = phi i64 [ %R98, %subblock_402479_2 ]
  %R106 = phi i64 [ %R97, %subblock_402479_2 ]
  ; # 40247f: xor    esi,esi
  ; # 402481: mov    rax,r12
  ; # 402484: syscall
  ; sys_futex
  %r120 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R108, i64 0, i64 %R106, i64 %R111, i64 %R109, i64 %R110, i64 202)
  %R112 = extractvalue { i64, i1 } %r120, 0
  br label %block_402486
block_402486:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R118 = phi i128 [ undef, %block_40247f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R117 = phi i64 [ undef, %block_40247f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R116 = phi i64 [ undef, %block_40247f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R115 = phi i64 [ undef, %block_40247f ]
  %R114 = phi i64 [ %R107, %block_40247f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R113 = phi i64 [ undef, %block_40247f ]
  ; # 402486: jmp    402468
  br label %block_402468
block_402488:
  %R120 = phi i128 [ %R85, %subblock_402468_1 ]
  %R119 = phi i64 [ %R83, %subblock_402468_1 ]
  ; # 402488: test   r8,r8
  ; r121 := (bv_eq r119 0x0 :: [64])
  %R121 = icmp eq i64 %R119, 0
  ; # 40248b: je     402491
  br i1 %R121, label %subblock_402488_1, label %subblock_402488_2
subblock_402488_1:
  br label %block_402491
subblock_402488_2:
  br label %block_40248d
block_40248d:
  %R123 = phi i128 [ %R120, %subblock_402488_2 ]
  %R122 = phi i64 [ %R119, %subblock_402488_2 ]
  ; # 40248d: lock dec DWORD PTR [r8]
  ; r124 := *r122
  %r133 = inttoptr i64 %R122 to i32*
  %R124 = load i32* %r133
  ; r125 := (bv_add r124 0xffffffff :: [32])
  %R125 = add i32 %R124, 4294967295
  ; *(r122) = r125
  %r136 = inttoptr i64 %R122 to i32*
  store i32 %R125, i32* %r136
  br label %block_402491
block_402491:
  %R126 = phi i128 [ %R34, %subblock_40243d_1 ], [ %R123, %block_40248d ], [ %R120, %subblock_402488_1 ]
  ; # 402491: pop    rbx
  ; # 402492: pop    rbp
  ; # 402493: pop    r12
  ; # 402495: ret
  %r138 = bitcast i128 %R126 to <2 x double>
  %r139 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r140 = insertvalue { i64, i64, <2 x double> } %r139, i64 undef, 1
  %r141 = insertvalue { i64, i64, <2 x double> } %r140, <2 x double> %r138, 2
  ret { i64, i64, <2 x double> } %r141
failure:
  br label %failure
}