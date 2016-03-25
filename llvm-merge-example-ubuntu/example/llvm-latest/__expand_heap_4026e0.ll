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
declare { i64, i64, <2 x double> } @F401b20(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402681(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4026e0(i64 %a0, <2 x double> %a1) {
entry:
  %r0 = bitcast <2 x double> %a1 to i128
  br label %block_4026e0
block_4026e0:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 4026e0: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4026e1: push   rbx
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 4026e2: mov    rdx,0x7fffffffffffefff
  ; # 4026ec: sub    rsp,0x18
  ; r4 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R4 = add i64 %R1, 18446744073709551576
  ; # 4026f0: mov    rax,QWORD PTR [rdi]
  ; r5 := *arg0
  %r7 = inttoptr i64 %a0 to i64*
  %R5 = load i64* %r7
  ; # 4026f3: cmp    rax,rdx
  ; # 4026f6: ja     402830
  ; r6 := (bv_ult 0x7fffffffffffefff :: [64] r5)
  %R6 = icmp ult i64 9223372036854771711, %R5
  br i1 %R6, label %subblock_4026e0_1, label %subblock_4026e0_2
subblock_4026e0_1:
  br label %block_402830
subblock_4026e0_2:
  br label %block_4026fc
block_4026fc:
  %R10 = phi i128 [ %r0, %subblock_4026e0_2 ]
  %R9 = phi i64 [ %a0, %subblock_4026e0_2 ]
  %R8 = phi i64 [ %R4, %subblock_4026e0_2 ]
  %R7 = phi i64 [ %R5, %subblock_4026e0_2 ]
  ; # 4026fc: mov    rdx,rax
  ; # 4026ff: mov    rsi,QWORD PTR [rip+0x20223a]
  ; r11 := *0x604940 :: [64]
  %r14 = inttoptr i64 6310208 to i64*
  %R11 = load i64* %r14
  ; # 402706: mov    rbp,rdi
  ; # 402709: neg    rdx
  ; r12 := (bv_sub 0x0 :: [64] r7)
  %R12 = sub i64 0, %R7
  ; # 40270c: and    edx,0xfff
  ; r13 := (trunc r12 32)
  %R13 = trunc i64 %R12 to i32
  ; r14 := (bv_and r13 0xfff :: [32])
  %R14 = and i32 %R13, 4095
  ; r15 := (uext r14 64)
  %R15 = zext i32 %R14 to i64
  ; # 402712: add    rdx,rax
  ; r16 := (bv_add r15 r7)
  %R16 = add i64 %R15, %R7
  ; # 402715: test   rsi,rsi
  ; r17 := (bv_eq r11 0x0 :: [64])
  %R17 = icmp eq i64 %R11, 0
  ; # 402718: jne    40273a
  ; r18 := (bv_complement r17)
  %R18 = xor i1 %R17, -1
  br i1 %R18, label %subblock_4026fc_1, label %subblock_4026fc_2
subblock_4026fc_1:
  br label %block_40273a
subblock_4026fc_2:
  br label %block_40271a
block_40271a:
  %R21 = phi i64 [ %R11, %subblock_4026fc_2 ]
  %R20 = phi i64 [ %R9, %subblock_4026fc_2 ]
  %R19 = phi i64 [ %R8, %subblock_4026fc_2 ]
  ; # 40271a: mov    eax,0xc
  ; # 40271f: mov    rdi,rsi
  ; # 402722: syscall
  ; sys_brk
  %r26 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R21, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 12)
  %R22 = extractvalue { i64, i1 } %r26, 0
  br label %block_402724
block_402724:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R27 = phi i128 [ undef, %block_40271a ]
  %R26 = phi i64 [ %R20, %block_40271a ]
  %R25 = phi i64 [ %R19, %block_40271a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R24 = phi i64 [ undef, %block_40271a ]
  %R23 = phi i64 [ %R22, %block_40271a ]
  ; # 402724: mov    rsi,rax
  ; # 402727: neg    rsi
  ; r28 := (bv_sub 0x0 :: [64] r23)
  %R28 = sub i64 0, %R23
  ; # 40272a: and    esi,0xfff
  ; r29 := (trunc r28 32)
  %R29 = trunc i64 %R28 to i32
  ; r30 := (bv_and r29 0xfff :: [32])
  %R30 = and i32 %R29, 4095
  ; r31 := (uext r30 64)
  %R31 = zext i32 %R30 to i64
  ; # 402730: add    rsi,rax
  ; r32 := (bv_add r31 r23)
  %R32 = add i64 %R31, %R23
  ; # 402733: mov    QWORD PTR [rip+0x202206],rsi
  ; *(0x604940 :: [64]) = r32
  %r38 = inttoptr i64 6310208 to i64*
  store i64 %R32, i64* %r38
  br label %block_40273a
block_40273a:
  %R37 = phi i128 [ %R27, %block_402724 ], [ %R10, %subblock_4026fc_1 ]
  %R36 = phi i64 [ %R32, %block_402724 ], [ %R11, %subblock_4026fc_1 ]
  %R35 = phi i64 [ %R26, %block_402724 ], [ %R9, %subblock_4026fc_1 ]
  %R34 = phi i64 [ %R25, %block_402724 ], [ %R8, %subblock_4026fc_1 ]
  %R33 = phi i64 [ %R24, %block_402724 ], [ %R16, %subblock_4026fc_1 ]
  ; # 40273a: mov    rax,rsi
  ; # 40273d: not    rax
  ; r38 := (bv_complement r36)
  %R38 = xor i64 %R36, -1
  ; # 402740: cmp    rdx,rax
  ; r39 := (bv_ult r33 r38)
  %R39 = icmp ult i64 %R33, %R38
  ; # 402743: jb     4027a0
  br i1 %R39, label %subblock_40273a_1, label %subblock_40273a_2
subblock_40273a_1:
  br label %block_4027a0
subblock_40273a_2:
  br label %block_402745
block_402745:
  %R42 = phi i128 [ %R95, %subblock_4027c7_1 ], [ %R112, %subblock_4027f3_1 ], [ %R120, %subblock_402803_1 ], [ %R37, %subblock_40273a_2 ]
  %R41 = phi i64 [ %R91, %subblock_4027c7_1 ], [ %R110, %subblock_4027f3_1 ], [ %R119, %subblock_402803_1 ], [ %R35, %subblock_40273a_2 ]
  %R40 = phi i64 [ %R89, %subblock_4027c7_1 ], [ %R109, %subblock_4027f3_1 ], [ %R118, %subblock_402803_1 ], [ %R33, %subblock_40273a_2 ]
  ; # 402745: mov    ecx,DWORD PTR [rip+0x2021ed]
  ; r43 := *0x604938 :: [64]
  %r49 = inttoptr i64 6310200 to i32*
  %R43 = load i32* %r49
  ; # 40274b: mov    esi,0x1000
  ; # 402750: mov    r8d,0xffffffff
  ; # 402756: shr    ecx,1
  ; r44 := (bv_shr r43 0x1 :: [32])
  %R44 = lshr i32 %R43, 1
  ; r45 := (trunc r44 8)
  %R45 = trunc i32 %R44 to i8
  ; # 402758: shl    rsi,cl
  ; r46 := (bv_and r45 0x3f :: [8])
  %R46 = and i8 %R45, 63
  ; r47 := (bv_eq r46 0x0 :: [8])
  %R47 = icmp eq i8 %R46, 0
  br i1 %R47, label %subblock_402745_1, label %subblock_402745_2
subblock_402745_1:
  br label %block_40275b
subblock_402745_2:
  ; r48 := (trunc r44 8)
  %R48 = trunc i32 %R44 to i8
  ; r49 := (bv_and r48 0x3f :: [8])
  %R49 = and i8 %R48, 63
  ; r50 := (uext r49 64)
  %R50 = zext i8 %R49 to i64
  ; r51 := (bv_shl 0x1000 :: [64] r50)
  %R51 = shl i64 4096, %R50
  br label %block_40275b
block_40275b:
  %R56 = phi i128 [ %R42, %subblock_402745_2 ], [ %R42, %subblock_402745_1 ]
  %R55 = phi i64 [ 4294967295, %subblock_402745_2 ], [ 4294967295, %subblock_402745_1 ]
  %R54 = phi i64 [ %R51, %subblock_402745_2 ], [ 4096, %subblock_402745_1 ]
  %R53 = phi i64 [ %R41, %subblock_402745_2 ], [ %R41, %subblock_402745_1 ]
  %R52 = phi i64 [ %R40, %subblock_402745_2 ], [ %R40, %subblock_402745_1 ]
  ; # 40275b: mov    ecx,0x22
  ; # 402760: cmp    rdx,rsi
  ; # 402763: mov    rbx,rsi
  ; # 402766: cmovae rbx,rdx
  ; r57 := (bv_ule r54 r52)
  %R57 = icmp ule i64 %R54, %R52
  ; r58 := (mux r57 r52 r54)
  %R58 = select i1 %R57, i64 %R52, i64 %R54
  ; # 40276a: xor    r9d,r9d
  ; # 40276d: xor    edi,edi
  ; # 40276f: mov    edx,0x3
  ; # 402774: mov    rsi,rbx
  ; # 402777: call   401b20
  %r66 = bitcast i128 %R56 to <2 x double>
  %r67 = call { i64, i64, <2 x double> } @F401b20(i64 0, i64 %R58, i64 3, i64 34, i64 %R55, i64 0, <2 x double> %r66)
  %R59 = extractvalue { i64, i64, <2 x double> } %r67, 0
  %R60 = extractvalue { i64, i64, <2 x double> } %r67, 1
  %r70 = extractvalue { i64, i64, <2 x double> } %r67, 2
  %R61 = bitcast <2 x double> %r70 to i128
  br label %block_40277c
block_40277c:
  %R66 = phi i128 [ %R61, %block_40275b ]
  %R65 = phi i64 [ %R53, %block_40275b ]
  %R64 = phi i64 [ %R58, %block_40275b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R63 = phi i64 [ undef, %block_40275b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R62 = phi i64 [ undef, %block_40275b ]
  ; # 40277c: cmp    rax,0xffffffffffffffff
  ; r67 := (bv_eq r62 0xffffffffffffffff :: [64])
  %R67 = icmp eq i64 %R62, 18446744073709551615
  ; # 402780: je     402848
  br i1 %R67, label %subblock_40277c_1, label %subblock_40277c_2
subblock_40277c_1:
  br label %block_402848
subblock_40277c_2:
  br label %block_402786
block_402786:
  %R72 = phi i128 [ %R66, %subblock_40277c_2 ]
  %R71 = phi i64 [ %R65, %subblock_40277c_2 ]
  %R70 = phi i64 [ %R64, %subblock_40277c_2 ]
  %R69 = phi i64 [ %R63, %subblock_40277c_2 ]
  %R68 = phi i64 [ %R62, %subblock_40277c_2 ]
  ; # 402786: add    DWORD PTR [rip+0x2021ab],0x1
  ; r73 := *0x604938 :: [64]
  %r83 = inttoptr i64 6310200 to i32*
  %R73 = load i32* %r83
  ; r74 := (bv_add r73 0x1 :: [32])
  %R74 = add i32 %R73, 1
  ; *(0x604938 :: [64]) = r74
  %r86 = inttoptr i64 6310200 to i32*
  store i32 %R74, i32* %r86
  ; # 40278d: mov    QWORD PTR [rbp],rbx
  ; *(r71) = r70
  %r87 = inttoptr i64 %R71 to i64*
  store i64 %R70, i64* %r87
  br label %block_402791
block_402791:
  %R77 = phi i128 [ %R72, %block_402786 ], [ %R141, %block_402848 ], [ %R129, %block_402817 ], [ %R139, %block_402835 ]
  %R76 = phi i64 [ %R69, %block_402786 ], [ %R140, %block_402848 ], [ %R126, %block_402817 ], [ %R138, %block_402835 ]
  %R75 = phi i64 [ %R68, %block_402786 ], [ 0, %block_402848 ], [ %R125, %block_402817 ], [ 0, %block_402835 ]
  ; # 402791: add    rsp,0x18
  ; # 402795: pop    rbx
  ; # 402796: pop    rbp
  ; # 402797: ret
  %r91 = bitcast i128 %R77 to <2 x double>
  %r92 = insertvalue { i64, i64, <2 x double> } undef, i64 %R75, 0
  %r93 = insertvalue { i64, i64, <2 x double> } %r92, i64 %R76, 1
  %r94 = insertvalue { i64, i64, <2 x double> } %r93, <2 x double> %r91, 2
  ret { i64, i64, <2 x double> } %r94
block_4027a0:
  %R82 = phi i128 [ %R37, %subblock_40273a_1 ]
  %R81 = phi i64 [ %R36, %subblock_40273a_1 ]
  %R80 = phi i64 [ %R35, %subblock_40273a_1 ]
  %R79 = phi i64 [ %R34, %subblock_40273a_1 ]
  %R78 = phi i64 [ %R33, %subblock_40273a_1 ]
  ; # 4027a0: mov    r8,QWORD PTR [rip+0x2021e9]
  ; r83 := *0x604990 :: [64]
  %r100 = inttoptr i64 6310288 to i64*
  %R83 = load i64* %r100
  ; # 4027a7: lea    rdi,[rdx+rsi*1]
  ; r84 := (bv_add r78 r81)
  %R84 = add i64 %R78, %R81
  ; # 4027ab: mov    eax,0x0
  ; # 4027b0: lea    rcx,[r8-0x800000]
  ; r85 := (bv_add r83 0xffffffffff800000 :: [64])
  %R85 = add i64 %R83, 18446744073701163008
  ; # 4027b7: cmp    r8,0x800001
  ; r86 := (bv_ult r83 0x800001 :: [64])
  %R86 = icmp ult i64 %R83, 8388609
  ; # 4027be: cmovb  rcx,rax
  ; r87 := (mux r86 0x0 :: [64] r85)
  %R87 = select i1 %R86, i64 0, i64 %R85
  ; # 4027c2: cmp    rdi,rcx
  ; # 4027c5: jbe    4027d0
  ; r88 := (bv_ule r84 r87)
  %R88 = icmp ule i64 %R84, %R87
  br i1 %R88, label %subblock_4027a0_1, label %subblock_4027a0_2
subblock_4027a0_1:
  br label %block_4027d0
subblock_4027a0_2:
  br label %block_4027c7
block_4027c7:
  %R95 = phi i128 [ %R82, %subblock_4027a0_2 ]
  %R94 = phi i64 [ %R83, %subblock_4027a0_2 ]
  %R93 = phi i64 [ %R84, %subblock_4027a0_2 ]
  %R92 = phi i64 [ %R81, %subblock_4027a0_2 ]
  %R91 = phi i64 [ %R80, %subblock_4027a0_2 ]
  %R90 = phi i64 [ %R79, %subblock_4027a0_2 ]
  %R89 = phi i64 [ %R78, %subblock_4027a0_2 ]
  ; # 4027c7: cmp    r8,rsi
  ; # 4027ca: ja     402745
  ; r96 := (bv_ult r92 r94)
  %R96 = icmp ult i64 %R92, %R94
  br i1 %R96, label %subblock_4027c7_1, label %subblock_4027c7_2
subblock_4027c7_1:
  br label %block_402745
subblock_4027c7_2:
  br label %block_4027d0
block_4027d0:
  %R102 = phi i128 [ %R95, %subblock_4027c7_2 ], [ %R82, %subblock_4027a0_1 ]
  %R101 = phi i64 [ %R93, %subblock_4027c7_2 ], [ %R84, %subblock_4027a0_1 ]
  %R100 = phi i64 [ %R92, %subblock_4027c7_2 ], [ %R81, %subblock_4027a0_1 ]
  %R99 = phi i64 [ %R91, %subblock_4027c7_2 ], [ %R80, %subblock_4027a0_1 ]
  %R98 = phi i64 [ %R90, %subblock_4027c7_2 ], [ %R79, %subblock_4027a0_1 ]
  %R97 = phi i64 [ %R89, %subblock_4027c7_2 ], [ %R78, %subblock_4027a0_1 ]
  ; # 4027d0: lea    rcx,[rsp+0x8]
  ; r103 := (bv_add r98 0x8 :: [64])
  %R103 = add i64 %R98, 8
  ; # 4027d5: lea    rax,[rsp-0x7ffff8]
  ; r104 := (bv_add r98 0xffffffffff800008 :: [64])
  %R104 = add i64 %R98, 18446744073701163016
  ; # 4027dd: mov    r8d,0x0
  ; # 4027e3: cmp    rcx,0x800001
  ; r105 := (bv_ult r103 0x800001 :: [64])
  %R105 = icmp ult i64 %R103, 8388609
  ; # 4027ea: cmovb  rax,r8
  ; r106 := (mux r105 0x0 :: [64] r104)
  %R106 = select i1 %R105, i64 0, i64 %R104
  ; # 4027ee: cmp    rcx,rsi
  ; # 4027f1: jbe    4027fc
  ; r107 := (bv_ule r103 r100)
  %R107 = icmp ule i64 %R103, %R100
  br i1 %R107, label %subblock_4027d0_1, label %subblock_4027d0_2
subblock_4027d0_1:
  br label %block_4027fc
subblock_4027d0_2:
  br label %block_4027f3
block_4027f3:
  %R112 = phi i128 [ %R102, %subblock_4027d0_2 ]
  %R111 = phi i64 [ %R101, %subblock_4027d0_2 ]
  %R110 = phi i64 [ %R99, %subblock_4027d0_2 ]
  %R109 = phi i64 [ %R97, %subblock_4027d0_2 ]
  %R108 = phi i64 [ %R106, %subblock_4027d0_2 ]
  ; # 4027f3: cmp    rdi,rax
  ; # 4027f6: ja     402745
  ; r113 := (bv_ult r108 r111)
  %R113 = icmp ult i64 %R108, %R111
  br i1 %R113, label %subblock_4027f3_1, label %subblock_4027f3_2
subblock_4027f3_1:
  br label %block_402745
subblock_4027f3_2:
  br label %block_4027fc
block_4027fc:
  %R115 = phi i64 [ %R111, %subblock_4027f3_2 ], [ %R101, %subblock_4027d0_1 ]
  %R114 = phi i64 [ %R110, %subblock_4027f3_2 ], [ %R99, %subblock_4027d0_1 ]
  ; # 4027fc: mov    eax,0xc
  ; # 402801: syscall
  ; sys_brk
  %r134 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R115, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 12)
  %R116 = extractvalue { i64, i1 } %r134, 0
  br label %block_402803
block_402803:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R120 = phi i128 [ undef, %block_4027fc ]
  %R119 = phi i64 [ %R114, %block_4027fc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R118 = phi i64 [ undef, %block_4027fc ]
  %R117 = phi i64 [ %R116, %block_4027fc ]
  ; # 402803: mov    rcx,QWORD PTR [rip+0x202136]
  ; r121 := *0x604940 :: [64]
  %r140 = inttoptr i64 6310208 to i64*
  %R121 = load i64* %r140
  ; # 40280a: lea    rsi,[rdx+rcx*1]
  ; r122 := (bv_add r118 r121)
  %R122 = add i64 %R118, %R121
  ; # 40280e: cmp    rsi,rax
  ; r123 := (bv_eq r122 r117)
  %R123 = icmp eq i64 %R122, %R117
  ; # 402811: jne    402745
  ; r124 := (bv_complement r123)
  %R124 = xor i1 %R123, -1
  br i1 %R124, label %subblock_402803_1, label %subblock_402803_2
subblock_402803_1:
  br label %block_402745
subblock_402803_2:
  br label %block_402817
block_402817:
  %R129 = phi i128 [ %R120, %subblock_402803_2 ]
  %R128 = phi i64 [ %R122, %subblock_402803_2 ]
  %R127 = phi i64 [ %R119, %subblock_402803_2 ]
  %R126 = phi i64 [ %R118, %subblock_402803_2 ]
  %R125 = phi i64 [ %R121, %subblock_402803_2 ]
  ; # 402817: mov    QWORD PTR [rbp],rdx
  ; *(r127) = r126
  %r150 = inttoptr i64 %R127 to i64*
  store i64 %R126, i64* %r150
  ; # 40281b: mov    QWORD PTR [rip+0x20211e],rsi
  ; *(0x604940 :: [64]) = r128
  %r151 = inttoptr i64 6310208 to i64*
  store i64 %R128, i64* %r151
  ; # 402822: mov    rax,rcx
  ; # 402825: jmp    402791
  br label %block_402791
block_402830:
  %R133 = phi i128 [ %r0, %subblock_4026e0_1 ]
  %R132 = phi i64 [ %a0, %subblock_4026e0_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rsi
  %R131 = phi i64 [ undef, %subblock_4026e0_1 ]
  %R130 = phi i64 [ 9223372036854771711, %subblock_4026e0_1 ]
  ; # 402830: call   402681
  %r156 = bitcast i128 %R133 to <2 x double>
  %r157 = call { i64, i64, <2 x double> } @F402681(i64 %R132, i64 %R131, i64 %R130, <2 x double> %r156)
  %R134 = extractvalue { i64, i64, <2 x double> } %r157, 0
  %R135 = extractvalue { i64, i64, <2 x double> } %r157, 1
  %r160 = extractvalue { i64, i64, <2 x double> } %r157, 2
  %R136 = bitcast <2 x double> %r160 to i128
  br label %block_402835
block_402835:
  %R139 = phi i128 [ %R136, %block_402830 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R138 = phi i64 [ undef, %block_402830 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R137 = phi i64 [ undef, %block_402830 ]
  ; # 402835: mov    DWORD PTR [rax],0xc
  ; *(r137) = 0xc :: [32]
  %r165 = inttoptr i64 %R137 to i32*
  store i32 12, i32* %r165
  ; # 40283b: xor    eax,eax
  ; # 40283d: jmp    402791
  br label %block_402791
block_402848:
  %R141 = phi i128 [ %R66, %subblock_40277c_1 ]
  %R140 = phi i64 [ %R63, %subblock_40277c_1 ]
  ; # 402848: xor    eax,eax
  ; # 40284a: jmp    402791
  br label %block_402791
failure:
  br label %failure
}