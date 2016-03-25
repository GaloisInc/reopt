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
declare { i64, i64, <2 x double> } @F4009a0(i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400e40(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401280(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401bb6(i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402375(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402681(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4018b0(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_4018b0
block_4018b0:
  ; r0 := (alloca 0x50 :: [64])
  %r1 = alloca i8, i64 80
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x50 :: [64])
  %R1 = add i64 %R0, 80
  ; # 4018b0: test   rdi,rdi
  ; r2 := (bv_eq arg0 0x0 :: [64])
  %R2 = icmp eq i64 %a0, 0
  ; # 4018b3: je     401ac0
  br i1 %R2, label %subblock_4018b0_1, label %subblock_4018b0_2
subblock_4018b0_1:
  br label %block_401ac0
subblock_4018b0_2:
  br label %block_4018b9
block_4018b9:
  %R15 = phi i128 [ %r0, %subblock_4018b0_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r15
  %R14 = phi i64 [ undef, %subblock_4018b0_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r14
  %R13 = phi i64 [ undef, %subblock_4018b0_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r13
  %R12 = phi i64 [ undef, %subblock_4018b0_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register r12
  %R11 = phi i64 [ undef, %subblock_4018b0_2 ]
  %R10 = phi i64 [ %a5, %subblock_4018b0_2 ]
  %R9 = phi i64 [ %a4, %subblock_4018b0_2 ]
  %R8 = phi i64 [ %a0, %subblock_4018b0_2 ]
  %R7 = phi i64 [ %a1, %subblock_4018b0_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbp
  %R6 = phi i64 [ undef, %subblock_4018b0_2 ]
  %R5 = phi i64 [ %R1, %subblock_4018b0_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbx
  %R4 = phi i64 [ undef, %subblock_4018b0_2 ]
  %R3 = phi i64 [ %a3, %subblock_4018b0_2 ]
  ; # 4018b9: push   r15
  ; r16 := (bv_add r5 0xfffffffffffffff8 :: [64])
  %R16 = add i64 %R5, 18446744073709551608
  ; *(r16) = r14
  %r19 = inttoptr i64 %R16 to i64*
  store i64 %R14, i64* %r19
  ; # 4018bb: lea    rdx,[rsi-0x1]
  ; r17 := (bv_add r7 0xffffffffffffffff :: [64])
  %R17 = add i64 %R7, 18446744073709551615
  ; # 4018bf: push   r14
  ; r18 := (bv_add r5 0xfffffffffffffff0 :: [64])
  %R18 = add i64 %R5, 18446744073709551600
  ; *(r18) = r13
  %r22 = inttoptr i64 %R18 to i64*
  store i64 %R13, i64* %r22
  ; # 4018c1: push   r13
  ; r19 := (bv_add r5 0xffffffffffffffe8 :: [64])
  %R19 = add i64 %R5, 18446744073709551592
  ; *(r19) = r12
  %r24 = inttoptr i64 %R19 to i64*
  store i64 %R12, i64* %r24
  ; # 4018c3: push   r12
  ; r20 := (bv_add r5 0xffffffffffffffe0 :: [64])
  %R20 = add i64 %R5, 18446744073709551584
  ; *(r20) = r11
  %r26 = inttoptr i64 %R20 to i64*
  store i64 %R11, i64* %r26
  ; # 4018c5: mov    rax,0x7fffffffffffefdf
  ; # 4018cf: push   rbp
  ; r21 := (bv_add r5 0xffffffffffffffd8 :: [64])
  %R21 = add i64 %R5, 18446744073709551576
  ; *(r21) = r6
  %r28 = inttoptr i64 %R21 to i64*
  store i64 %R6, i64* %r28
  ; # 4018d0: push   rbx
  ; r22 := (bv_add r5 0xffffffffffffffd0 :: [64])
  %R22 = add i64 %R5, 18446744073709551568
  ; *(r22) = r4
  %r30 = inttoptr i64 %R22 to i64*
  store i64 %R4, i64* %r30
  ; # 4018d1: mov    rbx,rdi
  ; # 4018d4: sub    rsp,0x18
  ; r23 := (bv_add r5 0xffffffffffffffb8 :: [64])
  %R23 = add i64 %R5, 18446744073709551544
  ; # 4018d8: cmp    rdx,rax
  ; r24 := (bv_eq r7 0x7fffffffffffefe0 :: [64])
  %R24 = icmp eq i64 %R7, 9223372036854771680
  ; # 4018db: ja     401980
  ; r25 := (bv_ule 0x7fffffffffffefdf :: [64] r17)
  %R25 = icmp ule i64 9223372036854771679, %R17
  ; r26 := (bv_complement r24)
  %R26 = xor i1 %R24, -1
  ; r27 := (bv_and r25 r26)
  %R27 = and i1 %R25, %R26
  br i1 %R27, label %subblock_4018b9_1, label %subblock_4018b9_2
subblock_4018b9_1:
  br label %block_401980
subblock_4018b9_2:
  br label %block_4018e1
block_4018e1:
  %R35 = phi i128 [ %R15, %subblock_4018b9_2 ]
  %R34 = phi i64 [ %R10, %subblock_4018b9_2 ]
  %R33 = phi i64 [ %R9, %subblock_4018b9_2 ]
  %R32 = phi i64 [ %R7, %subblock_4018b9_2 ]
  %R31 = phi i64 [ %R23, %subblock_4018b9_2 ]
  %R30 = phi i64 [ %R8, %subblock_4018b9_2 ]
  %R29 = phi i64 [ %R17, %subblock_4018b9_2 ]
  %R28 = phi i64 [ %R3, %subblock_4018b9_2 ]
  ; # 4018e1: lea    rbp,[rsi+0x2f]
  ; r36 := (bv_add r32 0x2f :: [64])
  %R36 = add i64 %R32, 47
  ; # 4018e5: and    rbp,0xffffffffffffffe0
  ; r37 := (bv_and r36 0xffffffffffffffe0 :: [64])
  %R37 = and i64 %R36, 18446744073709551584
  br label %block_4018e9
block_4018e9:
  %R46 = phi i128 [ %R35, %block_4018e1 ], [ %R157, %block_401989 ]
  %R45 = phi i64 [ %R34, %block_4018e1 ], [ %R156, %block_401989 ]
  %R44 = phi i64 [ %R33, %block_4018e1 ], [ %R155, %block_401989 ]
  %R43 = phi i64 [ %R32, %block_4018e1 ], [ %R154, %block_401989 ]
  %R42 = phi i64 [ %R37, %block_4018e1 ], [ 32, %block_401989 ]
  %R41 = phi i64 [ %R31, %block_4018e1 ], [ %R153, %block_401989 ]
  %R40 = phi i64 [ %R30, %block_4018e1 ], [ %R152, %block_401989 ]
  %R39 = phi i64 [ %R29, %block_4018e1 ], [ %R151, %block_401989 ]
  %R38 = phi i64 [ %R28, %block_4018e1 ], [ %R150, %block_401989 ]
  ; # 4018e9: mov    rax,QWORD PTR [rbx-0x8]
  ; r47 := (bv_add r40 0xfffffffffffffff8 :: [64])
  %R47 = add i64 %R40, 18446744073709551608
  ; r48 := *r47
  %r56 = inttoptr i64 %R47 to i64*
  %R48 = load i64* %r56
  ; # 4018ed: lea    r15,[rbx-0x10]
  ; r49 := (bv_add r40 0xfffffffffffffff0 :: [64])
  %R49 = add i64 %R40, 18446744073709551600
  ; # 4018f1: mov    r12,rax
  ; # 4018f4: and    r12,0xfffffffffffffffe
  ; r50 := (bv_and r48 0xfffffffffffffffe :: [64])
  %R50 = and i64 %R48, 18446744073709551614
  ; r51 := (trunc r48 8)
  %R51 = trunc i64 %R48 to i8
  ; # 4018f8: test   al,0x1
  ; r52 := (bv_and r51 0x1 :: [8])
  %R52 = and i8 %R51, 1
  ; r53 := (bv_eq r52 0x0 :: [8])
  %R53 = icmp eq i8 %R52, 0
  ; # 4018fa: jne    401998
  ; r54 := (bv_complement r53)
  %R54 = xor i1 %R53, -1
  br i1 %R54, label %subblock_4018e9_1, label %subblock_4018e9_2
subblock_4018e9_1:
  br label %block_401998
subblock_4018e9_2:
  br label %block_401900
block_401900:
  %R65 = phi i128 [ %R46, %subblock_4018e9_2 ]
  %R64 = phi i64 [ %R49, %subblock_4018e9_2 ]
  %R63 = phi i64 [ %R50, %subblock_4018e9_2 ]
  %R62 = phi i64 [ %R45, %subblock_4018e9_2 ]
  %R61 = phi i64 [ %R44, %subblock_4018e9_2 ]
  %R60 = phi i64 [ %R43, %subblock_4018e9_2 ]
  %R59 = phi i64 [ %R42, %subblock_4018e9_2 ]
  %R58 = phi i64 [ %R41, %subblock_4018e9_2 ]
  %R57 = phi i64 [ %R40, %subblock_4018e9_2 ]
  %R56 = phi i64 [ %R39, %subblock_4018e9_2 ]
  %R55 = phi i64 [ %R38, %subblock_4018e9_2 ]
  ; # 401900: mov    r14,QWORD PTR [rbx-0x10]
  ; r66 := (bv_add r57 0xfffffffffffffff0 :: [64])
  %R66 = add i64 %R57, 18446744073709551600
  ; r67 := *r66
  %r76 = inttoptr i64 %R66 to i64*
  %R67 = load i64* %r76
  ; # 401904: test   r14b,0x1
  ; r68 := (trunc r67 8)
  %R68 = trunc i64 %R67 to i8
  ; r69 := (bv_and r68 0x1 :: [8])
  %R69 = and i8 %R68, 1
  ; r70 := (bv_eq r69 0x0 :: [8])
  %R70 = icmp eq i8 %R69, 0
  ; # 401908: lea    r13,[r12+r14*1]
  ; r71 := (bv_add r63 r67)
  %R71 = add i64 %R63, %R67
  ; # 40190c: lea    r12,[r14+rbp*1]
  ; r72 := (bv_add r67 r59)
  %R72 = add i64 %R67, %R59
  ; # 401910: je     401913
  br i1 %R70, label %subblock_401900_1, label %subblock_401900_2
subblock_401900_1:
  br label %block_401913
subblock_401900_2:
  br label %block_401912
block_401912:
  %R85 = phi i128 [ %R65, %subblock_401900_2 ]
  %R84 = phi i64 [ %R64, %subblock_401900_2 ]
  %R83 = phi i64 [ %R67, %subblock_401900_2 ]
  %R82 = phi i64 [ %R71, %subblock_401900_2 ]
  %R81 = phi i64 [ %R72, %subblock_401900_2 ]
  %R80 = phi i64 [ %R62, %subblock_401900_2 ]
  %R79 = phi i64 [ %R61, %subblock_401900_2 ]
  %R78 = phi i64 [ %R60, %subblock_401900_2 ]
  %R77 = phi i64 [ %R59, %subblock_401900_2 ]
  %R76 = phi i64 [ %R58, %subblock_401900_2 ]
  %R75 = phi i64 [ %R57, %subblock_401900_2 ]
  %R74 = phi i64 [ %R56, %subblock_401900_2 ]
  %R73 = phi i64 [ %R55, %subblock_401900_2 ]
  ; # 401912: hlt
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception GeneralProtectionException 0 ()
  br label %block_401913
block_401913:
  %R98 = phi i128 [ %R85, %block_401912 ], [ %R65, %subblock_401900_1 ]
  %R97 = phi i64 [ %R84, %block_401912 ], [ %R64, %subblock_401900_1 ]
  %R96 = phi i64 [ %R83, %block_401912 ], [ %R67, %subblock_401900_1 ]
  %R95 = phi i64 [ %R82, %block_401912 ], [ %R71, %subblock_401900_1 ]
  %R94 = phi i64 [ %R81, %block_401912 ], [ %R72, %subblock_401900_1 ]
  %R93 = phi i64 [ %R80, %block_401912 ], [ %R62, %subblock_401900_1 ]
  %R92 = phi i64 [ %R79, %block_401912 ], [ %R61, %subblock_401900_1 ]
  %R91 = phi i64 [ %R78, %block_401912 ], [ %R60, %subblock_401900_1 ]
  %R90 = phi i64 [ %R77, %block_401912 ], [ %R59, %subblock_401900_1 ]
  %R89 = phi i64 [ %R76, %block_401912 ], [ %R58, %subblock_401900_1 ]
  %R88 = phi i64 [ %R75, %block_401912 ], [ %R57, %subblock_401900_1 ]
  %R87 = phi i64 [ %R74, %block_401912 ], [ %R56, %subblock_401900_1 ]
  %R86 = phi i64 [ %R73, %block_401912 ], [ %R55, %subblock_401900_1 ]
  ; # 401913: cmp    r12,0xfff
  ; # 40191a: jbe    401a10
  ; r99 := (bv_ule r94 0xfff :: [64])
  %R99 = icmp ule i64 %R94, 4095
  br i1 %R99, label %subblock_401913_1, label %subblock_401913_2
subblock_401913_1:
  br label %block_401a10
subblock_401913_2:
  br label %block_401920
block_401920:
  %R107 = phi i128 [ %R274, %subblock_401a18_1 ], [ %R98, %subblock_401913_2 ]
  %R106 = phi i64 [ %R273, %subblock_401a18_1 ], [ %R97, %subblock_401913_2 ]
  %R105 = phi i64 [ %R272, %subblock_401a18_1 ], [ %R96, %subblock_401913_2 ]
  %R104 = phi i64 [ %R271, %subblock_401a18_1 ], [ %R95, %subblock_401913_2 ]
  %R103 = phi i64 [ %R270, %subblock_401a18_1 ], [ %R94, %subblock_401913_2 ]
  %R102 = phi i64 [ %R269, %subblock_401a18_1 ], [ %R92, %subblock_401913_2 ]
  %R101 = phi i64 [ %R266, %subblock_401a18_1 ], [ %R88, %subblock_401913_2 ]
  %R100 = phi i64 [ %R265, %subblock_401a18_1 ], [ %R87, %subblock_401913_2 ]
  ; # 401920: add    r12,0xfff
  ; r108 := (bv_add r103 0xfff :: [64])
  %R108 = add i64 %R103, 4095
  ; # 401927: and    r12,0xfffffffffffff000
  ; r109 := (bv_and r108 0xfffffffffffff000 :: [64])
  %R109 = and i64 %R108, 18446744073709547520
  ; # 40192e: cmp    r13,r12
  ; r110 := (bv_eq r104 r109)
  %R110 = icmp eq i64 %R104, %R109
  ; # 401931: je     4019c8
  br i1 %R110, label %subblock_401920_1, label %subblock_401920_2
subblock_401920_1:
  br label %block_4019c8
subblock_401920_2:
  br label %block_401937
block_401937:
  %R117 = phi i128 [ %R107, %subblock_401920_2 ]
  %R116 = phi i64 [ %R106, %subblock_401920_2 ]
  %R115 = phi i64 [ %R105, %subblock_401920_2 ]
  %R114 = phi i64 [ %R104, %subblock_401920_2 ]
  %R113 = phi i64 [ %R109, %subblock_401920_2 ]
  %R112 = phi i64 [ %R102, %subblock_401920_2 ]
  %R111 = phi i64 [ %R101, %subblock_401920_2 ]
  ; # 401937: mov    rdi,r15
  ; # 40193a: xor    eax,eax
  ; # 40193c: mov    ecx,0x1
  ; # 401941: sub    rdi,r14
  ; r118 := (bv_sub r116 r115)
  %R118 = sub i64 %R116, %R115
  ; # 401944: mov    rdx,r12
  ; # 401947: mov    rsi,r13
  ; # 40194a: call   401bb6
  %r129 = bitcast i128 %R117 to <2 x double>
  %r130 = call { i64, i64, <2 x double> } @F401bb6(i64 %R118, i64 %R114, i64 %R113, i64 1, i64 %R112, <2 x double> %r129)
  %R119 = extractvalue { i64, i64, <2 x double> } %r130, 0
  %R120 = extractvalue { i64, i64, <2 x double> } %r130, 1
  %r133 = extractvalue { i64, i64, <2 x double> } %r130, 2
  %R121 = bitcast <2 x double> %r133 to i128
  br label %block_40194f
block_40194f:
  %R128 = phi i128 [ %R121, %block_401937 ]
  %R127 = phi i64 [ %R115, %block_401937 ]
  %R126 = phi i64 [ %R114, %block_401937 ]
  %R125 = phi i64 [ %R113, %block_401937 ]
  %R124 = phi i64 [ %R111, %block_401937 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R123 = phi i64 [ undef, %block_401937 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R122 = phi i64 [ undef, %block_401937 ]
  ; # 40194f: cmp    rax,0xffffffffffffffff
  ; r129 := (bv_eq r122 0xffffffffffffffff :: [64])
  %R129 = icmp eq i64 %R122, 18446744073709551615
  ; # 401953: je     401ae8
  br i1 %R129, label %subblock_40194f_1, label %subblock_40194f_2
subblock_40194f_1:
  br label %block_401ae8
subblock_40194f_2:
  br label %block_401959
block_401959:
  %R134 = phi i128 [ %R128, %subblock_40194f_2 ]
  %R133 = phi i64 [ %R127, %subblock_40194f_2 ]
  %R132 = phi i64 [ %R125, %subblock_40194f_2 ]
  %R131 = phi i64 [ %R123, %subblock_40194f_2 ]
  %R130 = phi i64 [ %R122, %subblock_40194f_2 ]
  ; # 401959: add    rax,r14
  ; r135 := (bv_add r130 r133)
  %R135 = add i64 %R130, %R133
  ; # 40195c: sub    r12,r14
  ; r136 := (bv_sub r132 r133)
  %R136 = sub i64 %R132, %R133
  ; # 40195f: mov    QWORD PTR [rax+0x8],r12
  ; r137 := (bv_add r135 0x8 :: [64])
  %R137 = add i64 %R135, 8
  ; *(r137) = r136
  %r151 = inttoptr i64 %R137 to i64*
  store i64 %R136, i64* %r151
  ; # 401963: add    rsp,0x18
  ; # 401967: add    rax,0x10
  ; r138 := (bv_add r135 0x10 :: [64])
  %R138 = add i64 %R135, 16
  ; # 40196b: pop    rbx
  ; # 40196c: pop    rbp
  ; # 40196d: pop    r12
  ; # 40196f: pop    r13
  ; # 401971: pop    r14
  ; # 401973: pop    r15
  ; # 401975: ret
  %r153 = bitcast i128 %R134 to <2 x double>
  %r154 = insertvalue { i64, i64, <2 x double> } undef, i64 %R138, 0
  %r155 = insertvalue { i64, i64, <2 x double> } %r154, i64 %R131, 1
  %r156 = insertvalue { i64, i64, <2 x double> } %r155, <2 x double> %r153, 2
  ret { i64, i64, <2 x double> } %r156
block_401980:
  %R147 = phi i128 [ %R15, %subblock_4018b9_1 ]
  %R146 = phi i64 [ %R10, %subblock_4018b9_1 ]
  %R145 = phi i64 [ %R9, %subblock_4018b9_1 ]
  %R144 = phi i64 [ %R8, %subblock_4018b9_1 ]
  %R143 = phi i64 [ %R7, %subblock_4018b9_1 ]
  %R142 = phi i64 [ %R23, %subblock_4018b9_1 ]
  %R141 = phi i64 [ %R8, %subblock_4018b9_1 ]
  %R140 = phi i64 [ %R17, %subblock_4018b9_1 ]
  %R139 = phi i64 [ %R3, %subblock_4018b9_1 ]
  ; # 401980: test   rsi,rsi
  ; r148 := (bv_eq r143 0x0 :: [64])
  %R148 = icmp eq i64 %R143, 0
  ; # 401983: jne    401af8
  ; r149 := (bv_complement r148)
  %R149 = xor i1 %R148, -1
  br i1 %R149, label %subblock_401980_1, label %subblock_401980_2
subblock_401980_1:
  br label %block_401af8
subblock_401980_2:
  br label %block_401989
block_401989:
  %R157 = phi i128 [ %R147, %subblock_401980_2 ]
  %R156 = phi i64 [ %R146, %subblock_401980_2 ]
  %R155 = phi i64 [ %R145, %subblock_401980_2 ]
  %R154 = phi i64 [ %R143, %subblock_401980_2 ]
  %R153 = phi i64 [ %R142, %subblock_401980_2 ]
  %R152 = phi i64 [ %R141, %subblock_401980_2 ]
  %R151 = phi i64 [ %R140, %subblock_401980_2 ]
  %R150 = phi i64 [ %R139, %subblock_401980_2 ]
  ; # 401989: mov    ebp,0x20
  ; # 40198e: jmp    4018e9
  br label %block_4018e9
block_401998:
  %R168 = phi i128 [ %R46, %subblock_4018e9_1 ]
  %R167 = phi i64 [ %R49, %subblock_4018e9_1 ]
  %R166 = phi i64 [ %R50, %subblock_4018e9_1 ]
  %R165 = phi i64 [ %R45, %subblock_4018e9_1 ]
  %R164 = phi i64 [ %R44, %subblock_4018e9_1 ]
  %R163 = phi i64 [ %R43, %subblock_4018e9_1 ]
  %R162 = phi i64 [ %R42, %subblock_4018e9_1 ]
  %R161 = phi i64 [ %R40, %subblock_4018e9_1 ]
  %R160 = phi i64 [ %R39, %subblock_4018e9_1 ]
  %R159 = phi i64 [ %R38, %subblock_4018e9_1 ]
  %R158 = phi i64 [ %R48, %subblock_4018e9_1 ]
  ; # 401998: lea    r13,[r15+r12*1]
  ; r169 := (bv_add r167 r166)
  %R169 = add i64 %R167, %R166
  ; # 40199c: cmp    rax,QWORD PTR [r13]
  ; r170 := *r169
  %r188 = inttoptr i64 %R169 to i64*
  %R170 = load i64* %r188
  ; r171 := (bv_eq r158 r170)
  %R171 = icmp eq i64 %R158, %R170
  ; # 4019a0: je     4019a3
  br i1 %R171, label %subblock_401998_1, label %subblock_401998_2
subblock_401998_1:
  br label %block_4019a3
subblock_401998_2:
  br label %block_4019a2
block_4019a2:
  %R182 = phi i128 [ %R168, %subblock_401998_2 ]
  %R181 = phi i64 [ %R167, %subblock_401998_2 ]
  %R180 = phi i64 [ %R169, %subblock_401998_2 ]
  %R179 = phi i64 [ %R166, %subblock_401998_2 ]
  %R178 = phi i64 [ %R165, %subblock_401998_2 ]
  %R177 = phi i64 [ %R164, %subblock_401998_2 ]
  %R176 = phi i64 [ %R163, %subblock_401998_2 ]
  %R175 = phi i64 [ %R162, %subblock_401998_2 ]
  %R174 = phi i64 [ %R161, %subblock_401998_2 ]
  %R173 = phi i64 [ %R160, %subblock_401998_2 ]
  %R172 = phi i64 [ %R159, %subblock_401998_2 ]
  ; # 4019a2: hlt
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception GeneralProtectionException 0 ()
  br label %block_4019a3
block_4019a3:
  %R193 = phi i128 [ %R182, %block_4019a2 ], [ %R168, %subblock_401998_1 ]
  %R192 = phi i64 [ %R181, %block_4019a2 ], [ %R167, %subblock_401998_1 ]
  %R191 = phi i64 [ %R180, %block_4019a2 ], [ %R169, %subblock_401998_1 ]
  %R190 = phi i64 [ %R179, %block_4019a2 ], [ %R166, %subblock_401998_1 ]
  %R189 = phi i64 [ %R178, %block_4019a2 ], [ %R165, %subblock_401998_1 ]
  %R188 = phi i64 [ %R177, %block_4019a2 ], [ %R164, %subblock_401998_1 ]
  %R187 = phi i64 [ %R176, %block_4019a2 ], [ %R163, %subblock_401998_1 ]
  %R186 = phi i64 [ %R175, %block_4019a2 ], [ %R162, %subblock_401998_1 ]
  %R185 = phi i64 [ %R174, %block_4019a2 ], [ %R161, %subblock_401998_1 ]
  %R184 = phi i64 [ %R173, %block_4019a2 ], [ %R160, %subblock_401998_1 ]
  %R183 = phi i64 [ %R172, %block_4019a2 ], [ %R159, %subblock_401998_1 ]
  ; # 4019a3: cmp    r12,rbp
  ; r194 := (bv_ult r190 r186)
  %R194 = icmp ult i64 %R190, %R186
  ; # 4019a6: jb     401a50
  br i1 %R194, label %subblock_4019a3_1, label %subblock_4019a3_2
subblock_4019a3_1:
  br label %block_401a50
subblock_4019a3_2:
  br label %block_4019ac
block_4019ac:
  %R203 = phi i128 [ %R193, %subblock_4019a3_2 ]
  %R202 = phi i64 [ %R192, %subblock_4019a3_2 ]
  %R201 = phi i64 [ %R191, %subblock_4019a3_2 ]
  %R200 = phi i64 [ %R190, %subblock_4019a3_2 ]
  %R199 = phi i64 [ %R189, %subblock_4019a3_2 ]
  %R198 = phi i64 [ %R188, %subblock_4019a3_2 ]
  %R197 = phi i64 [ %R187, %subblock_4019a3_2 ]
  %R196 = phi i64 [ %R186, %subblock_4019a3_2 ]
  %R195 = phi i64 [ %R185, %subblock_4019a3_2 ]
  ; # 4019ac: mov    rax,r12
  ; # 4019af: or     rax,0x1
  ; r204 := (bv_or r200 0x1 :: [64])
  %R204 = or i64 %R200, 1
  ; # 4019b3: mov    QWORD PTR [rbx-0x8],rax
  ; r205 := (bv_add r195 0xfffffffffffffff8 :: [64])
  %R205 = add i64 %R195, 18446744073709551608
  ; *(r205) = r204
  %r225 = inttoptr i64 %R205 to i64*
  store i64 %R204, i64* %r225
  ; # 4019b7: mov    QWORD PTR [r13],rax
  ; *(r201) = r204
  %r226 = inttoptr i64 %R201 to i64*
  store i64 %R204, i64* %r226
  br label %block_4019bb
block_4019bb:
  %R213 = phi i128 [ %R203, %block_4019ac ], [ %R340, %subblock_401a5c_1 ]
  %R212 = phi i64 [ %R202, %block_4019ac ], [ %R339, %subblock_401a5c_1 ]
  %R211 = phi i64 [ %R199, %block_4019ac ], [ %R336, %subblock_401a5c_1 ]
  %R210 = phi i64 [ %R198, %block_4019ac ], [ %R335, %subblock_401a5c_1 ]
  %R209 = phi i64 [ %R197, %block_4019ac ], [ %R334, %subblock_401a5c_1 ]
  %R208 = phi i64 [ %R196, %block_4019ac ], [ %R333, %subblock_401a5c_1 ]
  %R207 = phi i64 [ %R195, %block_4019ac ], [ %R332, %subblock_401a5c_1 ]
  %R206 = phi i64 [ %R204, %block_4019ac ], [ %R345, %subblock_401a5c_1 ]
  ; # 4019bb: and    rax,0xfffffffffffffffe
  ; r214 := (bv_and r206 0xfffffffffffffffe :: [64])
  %R214 = and i64 %R206, 18446744073709551614
  ; # 4019bf: lea    rdx,[rax-0x10]
  ; r215 := (bv_add r214 0xfffffffffffffff0 :: [64])
  %R215 = add i64 %R214, 18446744073709551600
  ; # 4019c3: cmp    rdx,rbp
  ; # 4019c6: ja     4019e0
  ; r216 := (bv_ult r208 r215)
  %R216 = icmp ult i64 %R208, %R215
  br i1 %R216, label %subblock_4019bb_1, label %subblock_4019bb_2
subblock_4019bb_1:
  br label %block_4019e0
subblock_4019bb_2:
  br label %block_4019c8
block_4019c8:
  %R219 = phi i128 [ %R416, %subblock_401ae8_1 ], [ %R107, %subblock_401920_1 ], [ %R245, %block_401a0d ], [ %R213, %subblock_4019bb_2 ]
  %R218 = phi i64 [ %R413, %subblock_401ae8_1 ], [ %R101, %subblock_401920_1 ], [ %R244, %block_401a0d ], [ %R207, %subblock_4019bb_2 ]
  %R217 = phi i64 [ %R412, %subblock_401ae8_1 ], [ %R100, %subblock_401920_1 ], [ %R243, %block_401a0d ], [ %R215, %subblock_4019bb_2 ]
  ; # 4019c8: mov    rax,rbx
  br label %block_4019cb
block_4019cb:
  %R222 = phi i128 [ %R303, %block_401a3d ], [ %R219, %block_4019c8 ], [ %R419, %block_401af1 ], [ %R429, %block_401afd ]
  %R221 = phi i64 [ %R301, %block_401a3d ], [ %R217, %block_4019c8 ], [ %R418, %block_401af1 ], [ %R428, %block_401afd ]
  %R220 = phi i64 [ %R305, %block_401a3d ], [ %R218, %block_4019c8 ], [ 0, %block_401af1 ], [ 0, %block_401afd ]
  ; # 4019cb: add    rsp,0x18
  ; # 4019cf: pop    rbx
  ; # 4019d0: pop    rbp
  ; # 4019d1: pop    r12
  ; # 4019d3: pop    r13
  ; # 4019d5: pop    r14
  ; # 4019d7: pop    r15
  ; # 4019d9: ret
  %r244 = bitcast i128 %R222 to <2 x double>
  %r245 = insertvalue { i64, i64, <2 x double> } undef, i64 %R220, 0
  %r246 = insertvalue { i64, i64, <2 x double> } %r245, i64 %R221, 1
  %r247 = insertvalue { i64, i64, <2 x double> } %r246, <2 x double> %r244, 2
  ret { i64, i64, <2 x double> } %r247
block_4019e0:
  %R230 = phi i128 [ %R213, %subblock_4019bb_1 ]
  %R229 = phi i64 [ %R212, %subblock_4019bb_1 ]
  %R228 = phi i64 [ %R211, %subblock_4019bb_1 ]
  %R227 = phi i64 [ %R210, %subblock_4019bb_1 ]
  %R226 = phi i64 [ %R209, %subblock_4019bb_1 ]
  %R225 = phi i64 [ %R208, %subblock_4019bb_1 ]
  %R224 = phi i64 [ %R207, %subblock_4019bb_1 ]
  %R223 = phi i64 [ %R214, %subblock_4019bb_1 ]
  ; # 4019e0: mov    rdx,rax
  ; # 4019e3: add    r15,rbp
  ; r231 := (bv_add r229 r225)
  %R231 = add i64 %R229, %R225
  ; # 4019e6: mov    rcx,rbp
  ; # 4019e9: sub    rdx,rbp
  ; r232 := (bv_sub r223 r225)
  %R232 = sub i64 %R223, %R225
  ; # 4019ec: or     rcx,0x1
  ; r233 := (bv_or r225 0x1 :: [64])
  %R233 = or i64 %R225, 1
  ; # 4019f0: lea    rdi,[r15+0x10]
  ; r234 := (bv_add r231 0x10 :: [64])
  %R234 = add i64 %R231, 16
  ; # 4019f4: or     rdx,0x1
  ; r235 := (bv_or r232 0x1 :: [64])
  %R235 = or i64 %R232, 1
  ; # 4019f8: mov    QWORD PTR [r15],rcx
  ; *(r231) = r233
  %r261 = inttoptr i64 %R231 to i64*
  store i64 %R233, i64* %r261
  ; # 4019fb: mov    QWORD PTR [r15+0x8],rdx
  ; r236 := (bv_add r231 0x8 :: [64])
  %R236 = add i64 %R231, 8
  ; *(r236) = r235
  %r263 = inttoptr i64 %R236 to i64*
  store i64 %R235, i64* %r263
  ; # 4019ff: mov    QWORD PTR [rbx+rax*1-0x10],rdx
  ; r237 := (bv_add r224 r223)
  %R237 = add i64 %R224, %R223
  ; r238 := (bv_add r237 0xfffffffffffffff0 :: [64])
  %R238 = add i64 %R237, 18446744073709551600
  ; *(r238) = r235
  %r266 = inttoptr i64 %R238 to i64*
  store i64 %R235, i64* %r266
  ; # 401a04: mov    QWORD PTR [rbx-0x8],rcx
  ; r239 := (bv_add r224 0xfffffffffffffff8 :: [64])
  %R239 = add i64 %R224, 18446744073709551608
  ; *(r239) = r233
  %r268 = inttoptr i64 %R239 to i64*
  store i64 %R233, i64* %r268
  ; # 401a08: call   400e40
  %r269 = bitcast i128 %R230 to <2 x double>
  %r270 = call { i64, i64, <2 x double> } @F400e40(i64 %R234, i64 %R226, i64 %R235, i64 %R233, i64 %R227, i64 %R228, <2 x double> %r269)
  %R240 = extractvalue { i64, i64, <2 x double> } %r270, 0
  %R241 = extractvalue { i64, i64, <2 x double> } %r270, 1
  %r273 = extractvalue { i64, i64, <2 x double> } %r270, 2
  %R242 = bitcast <2 x double> %r273 to i128
  br label %block_401a0d
block_401a0d:
  %R245 = phi i128 [ %R242, %block_4019e0 ]
  %R244 = phi i64 [ %R224, %block_4019e0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R243 = phi i64 [ undef, %block_4019e0 ]
  ; # 401a0d: jmp    4019c8
  br label %block_4019c8
block_401a10:
  %R258 = phi i128 [ %R98, %subblock_401913_1 ]
  %R257 = phi i64 [ %R97, %subblock_401913_1 ]
  %R256 = phi i64 [ %R96, %subblock_401913_1 ]
  %R255 = phi i64 [ %R95, %subblock_401913_1 ]
  %R254 = phi i64 [ %R94, %subblock_401913_1 ]
  %R253 = phi i64 [ %R93, %subblock_401913_1 ]
  %R252 = phi i64 [ %R92, %subblock_401913_1 ]
  %R251 = phi i64 [ %R91, %subblock_401913_1 ]
  %R250 = phi i64 [ %R90, %subblock_401913_1 ]
  %R249 = phi i64 [ %R89, %subblock_401913_1 ]
  %R248 = phi i64 [ %R88, %subblock_401913_1 ]
  %R247 = phi i64 [ %R87, %subblock_401913_1 ]
  %R246 = phi i64 [ %R86, %subblock_401913_1 ]
  ; # 401a10: mov    rdi,rbp
  ; # 401a13: call   401280
  ; r259 := (bv_add r249 0xfffffffffffffff8 :: [64])
  %R259 = add i64 %R249, 18446744073709551608
  ; r263 := (bv_add r259 0x8 :: [64])
  %R263 = add i64 %R259, 8
  %r293 = bitcast i128 %R258 to <2 x double>
  %r294 = call { i64, i64, <2 x double> } @F401280(i64 %R250, i64 %R251, i64 %R247, i64 %R246, i64 %R252, i64 %R253, <2 x double> %r293)
  %R260 = extractvalue { i64, i64, <2 x double> } %r294, 0
  %R261 = extractvalue { i64, i64, <2 x double> } %r294, 1
  %r297 = extractvalue { i64, i64, <2 x double> } %r294, 2
  %R262 = bitcast <2 x double> %r297 to i128
  br label %block_401a18
block_401a18:
  %R274 = phi i128 [ %R262, %block_401a10 ]
  %R273 = phi i64 [ %R257, %block_401a10 ]
  %R272 = phi i64 [ %R256, %block_401a10 ]
  %R271 = phi i64 [ %R255, %block_401a10 ]
  %R270 = phi i64 [ %R254, %block_401a10 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R269 = phi i64 [ undef, %block_401a10 ]
  %R268 = phi i64 [ %R250, %block_401a10 ]
  %R267 = phi i64 [ %R263, %block_401a10 ]
  %R266 = phi i64 [ %R248, %block_401a10 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R265 = phi i64 [ undef, %block_401a10 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R264 = phi i64 [ undef, %block_401a10 ]
  ; # 401a18: test   rax,rax
  ; r275 := (bv_eq r264 0x0 :: [64])
  %R275 = icmp eq i64 %R264, 0
  ; # 401a1b: je     401920
  br i1 %R275, label %subblock_401a18_1, label %subblock_401a18_2
subblock_401a18_1:
  br label %block_401920
subblock_401a18_2:
  br label %block_401a21
block_401a21:
  %R280 = phi i128 [ %R274, %subblock_401a18_2 ]
  %R279 = phi i64 [ %R268, %subblock_401a18_2 ]
  %R278 = phi i64 [ %R267, %subblock_401a18_2 ]
  %R277 = phi i64 [ %R266, %subblock_401a18_2 ]
  %R276 = phi i64 [ %R264, %subblock_401a18_2 ]
  ; # 401a21: lea    rdx,[rbp-0x10]
  ; r281 := (bv_add r279 0xfffffffffffffff0 :: [64])
  %R281 = add i64 %R279, 18446744073709551600
  ; # 401a25: mov    rsi,rbx
  ; # 401a28: mov    rdi,rax
  ; # 401a2b: mov    QWORD PTR [rsp+0x8],rax
  ; r282 := (bv_add r278 0x8 :: [64])
  %R282 = add i64 %R278, 8
  ; *(r282) = r276
  %r318 = inttoptr i64 %R282 to i64*
  store i64 %R276, i64* %r318
  ; # 401a30: call   402375
  ; r283 := (bv_add r278 0xfffffffffffffff8 :: [64])
  %R283 = add i64 %R278, 18446744073709551608
  ; r287 := (bv_add r283 0x8 :: [64])
  %R287 = add i64 %R283, 8
  %r321 = bitcast i128 %R280 to <2 x double>
  %r322 = call { i64, i64, <2 x double> } @F402375(i64 %R276, i64 %R277, i64 %R281, <2 x double> %r321)
  %R284 = extractvalue { i64, i64, <2 x double> } %r322, 0
  %R285 = extractvalue { i64, i64, <2 x double> } %r322, 1
  %r325 = extractvalue { i64, i64, <2 x double> } %r322, 2
  %R286 = bitcast <2 x double> %r325 to i128
  br label %block_401a35
block_401a35:
  %R295 = phi i128 [ %R286, %block_401a21 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R294 = phi i64 [ undef, %block_401a21 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R293 = phi i64 [ undef, %block_401a21 ]
  %R292 = phi i64 [ %R285, %block_401a21 ]
  %R291 = phi i64 [ %R287, %block_401a21 ]
  %R290 = phi i64 [ %R277, %block_401a21 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R289 = phi i64 [ undef, %block_401a21 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R288 = phi i64 [ undef, %block_401a21 ]
  ; # 401a35: mov    rdi,rbx
  ; # 401a38: call   400e40
  ; r296 := (bv_add r291 0xfffffffffffffff8 :: [64])
  %R296 = add i64 %R291, 18446744073709551608
  ; r300 := (bv_add r296 0x8 :: [64])
  %R300 = add i64 %R296, 8
  %r337 = bitcast i128 %R295 to <2 x double>
  %r338 = call { i64, i64, <2 x double> } @F400e40(i64 %R290, i64 %R292, i64 %R289, i64 %R288, i64 %R293, i64 %R294, <2 x double> %r337)
  %R297 = extractvalue { i64, i64, <2 x double> } %r338, 0
  %R298 = extractvalue { i64, i64, <2 x double> } %r338, 1
  %r341 = extractvalue { i64, i64, <2 x double> } %r338, 2
  %R299 = bitcast <2 x double> %r341 to i128
  br label %block_401a3d
block_401a3d:
  %R303 = phi i128 [ %R299, %block_401a35 ]
  %R302 = phi i64 [ %R300, %block_401a35 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R301 = phi i64 [ undef, %block_401a35 ]
  ; # 401a3d: mov    rcx,QWORD PTR [rsp+0x8]
  ; r304 := (bv_add r302 0x8 :: [64])
  %R304 = add i64 %R302, 8
  ; r305 := *r304
  %r347 = inttoptr i64 %R304 to i64*
  %R305 = load i64* %r347
  ; # 401a42: mov    rax,rcx
  ; # 401a45: jmp    4019cb
  br label %block_4019cb
block_401a50:
  %R315 = phi i128 [ %R193, %subblock_4019a3_1 ]
  %R314 = phi i64 [ %R192, %subblock_4019a3_1 ]
  %R313 = phi i64 [ %R191, %subblock_4019a3_1 ]
  %R312 = phi i64 [ %R190, %subblock_4019a3_1 ]
  %R311 = phi i64 [ %R188, %subblock_4019a3_1 ]
  %R310 = phi i64 [ %R187, %subblock_4019a3_1 ]
  %R309 = phi i64 [ %R186, %subblock_4019a3_1 ]
  %R308 = phi i64 [ %R185, %subblock_4019a3_1 ]
  %R307 = phi i64 [ %R184, %subblock_4019a3_1 ]
  %R306 = phi i64 [ %R183, %subblock_4019a3_1 ]
  ; # 401a50: mov    rdi,r13
  ; # 401a53: call   4009a0
  %r359 = bitcast i128 %R315 to <2 x double>
  %r360 = call { i64, i64, <2 x double> } @F4009a0(i64 %R313, i64 %R310, i64 %R307, i64 %R306, i64 %R311, <2 x double> %r359)
  %R316 = extractvalue { i64, i64, <2 x double> } %r360, 0
  %r362 = extractvalue { i64, i64, <2 x double> } %r360, 2
  %R317 = bitcast <2 x double> %r362 to i128
  br label %block_401a58
block_401a58:
  %R329 = phi i128 [ %R317, %block_401a50 ]
  %R328 = phi i64 [ %R314, %block_401a50 ]
  %R327 = phi i64 [ %R313, %block_401a50 ]
  %R326 = phi i64 [ %R312, %block_401a50 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R325 = phi i64 [ undef, %block_401a50 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R324 = phi i64 [ undef, %block_401a50 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R323 = phi i64 [ undef, %block_401a50 ]
  %R322 = phi i64 [ %R309, %block_401a50 ]
  %R321 = phi i64 [ %R308, %block_401a50 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R320 = phi i64 [ undef, %block_401a50 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R319 = phi i64 [ undef, %block_401a50 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R318 = phi i64 [ undef, %block_401a50 ]
  ; # 401a58: test   eax,eax
  ; r330 := (trunc r318 32)
  %R330 = trunc i64 %R318 to i32
  ; r331 := (bv_eq r330 0x0 :: [32])
  %R331 = icmp eq i32 %R330, 0
  ; # 401a5a: je     401ad0
  br i1 %R331, label %subblock_401a58_1, label %subblock_401a58_2
subblock_401a58_1:
  br label %block_401ad0
subblock_401a58_2:
  br label %block_401a5c
block_401a5c:
  %R340 = phi i128 [ %R329, %subblock_401a58_2 ]
  %R339 = phi i64 [ %R328, %subblock_401a58_2 ]
  %R338 = phi i64 [ %R327, %subblock_401a58_2 ]
  %R337 = phi i64 [ %R326, %subblock_401a58_2 ]
  %R336 = phi i64 [ %R325, %subblock_401a58_2 ]
  %R335 = phi i64 [ %R324, %subblock_401a58_2 ]
  %R334 = phi i64 [ %R323, %subblock_401a58_2 ]
  %R333 = phi i64 [ %R322, %subblock_401a58_2 ]
  %R332 = phi i64 [ %R321, %subblock_401a58_2 ]
  ; # 401a5c: mov    rdx,QWORD PTR [r13+0x8]
  ; r341 := (bv_add r338 0x8 :: [64])
  %R341 = add i64 %R338, 8
  ; r342 := *r341
  %r388 = inttoptr i64 %R341 to i64*
  %R342 = load i64* %r388
  ; # 401a60: and    rdx,0xfffffffffffffffe
  ; r343 := (bv_and r342 0xfffffffffffffffe :: [64])
  %R343 = and i64 %R342, 18446744073709551614
  ; # 401a64: lea    rcx,[r12+rdx*1]
  ; r344 := (bv_add r337 r343)
  %R344 = add i64 %R337, %R343
  ; # 401a68: mov    rax,rcx
  ; # 401a6b: or     rax,0x1
  ; r345 := (bv_or r344 0x1 :: [64])
  %R345 = or i64 %R344, 1
  ; # 401a6f: cmp    rcx,rbp
  ; # 401a72: mov    QWORD PTR [rbx-0x8],rax
  ; r346 := (bv_add r332 0xfffffffffffffff8 :: [64])
  %R346 = add i64 %R332, 18446744073709551608
  ; *(r346) = r345
  %r394 = inttoptr i64 %R346 to i64*
  store i64 %R345, i64* %r394
  ; # 401a76: mov    QWORD PTR [r13+rdx*1],rax
  ; r347 := (bv_add r338 r343)
  %R347 = add i64 %R338, %R343
  ; *(r347) = r345
  %r396 = inttoptr i64 %R347 to i64*
  store i64 %R345, i64* %r396
  ; # 401a7b: jae    4019bb
  ; r348 := (bv_ule r333 r344)
  %R348 = icmp ule i64 %R333, %R344
  br i1 %R348, label %subblock_401a5c_1, label %subblock_401a5c_2
subblock_401a5c_1:
  br label %block_4019bb
subblock_401a5c_2:
  br label %block_401a81
block_401a81:
  %R357 = phi i128 [ %R340, %subblock_401a5c_2 ], [ %R409, %block_401ad0 ]
  %R356 = phi i64 [ %R337, %subblock_401a5c_2 ], [ %R407, %block_401ad0 ]
  %R355 = phi i64 [ %R336, %subblock_401a5c_2 ], [ %R406, %block_401ad0 ]
  %R354 = phi i64 [ %R335, %subblock_401a5c_2 ], [ %R405, %block_401ad0 ]
  %R353 = phi i64 [ %R334, %subblock_401a5c_2 ], [ %R404, %block_401ad0 ]
  %R352 = phi i64 [ %R333, %subblock_401a5c_2 ], [ %R403, %block_401ad0 ]
  %R351 = phi i64 [ %R332, %subblock_401a5c_2 ], [ %R402, %block_401ad0 ]
  %R350 = phi i64 [ %R343, %subblock_401a5c_2 ], [ %R401, %block_401ad0 ]
  %R349 = phi i64 [ %R344, %subblock_401a5c_2 ], [ %R400, %block_401ad0 ]
  ; # 401a81: lea    rdi,[rbp-0x10]
  ; r358 := (bv_add r352 0xfffffffffffffff0 :: [64])
  %R358 = add i64 %R352, 18446744073709551600
  ; # 401a85: call   401280
  %r408 = bitcast i128 %R357 to <2 x double>
  %r409 = call { i64, i64, <2 x double> } @F401280(i64 %R358, i64 %R353, i64 %R350, i64 %R349, i64 %R354, i64 %R355, <2 x double> %r408)
  %R359 = extractvalue { i64, i64, <2 x double> } %r409, 0
  %R360 = extractvalue { i64, i64, <2 x double> } %r409, 1
  %r412 = extractvalue { i64, i64, <2 x double> } %r409, 2
  %R361 = bitcast <2 x double> %r412 to i128
  br label %block_401a8a
block_401a8a:
  %R366 = phi i128 [ %R361, %block_401a81 ]
  %R365 = phi i64 [ %R356, %block_401a81 ]
  %R364 = phi i64 [ %R351, %block_401a81 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R363 = phi i64 [ undef, %block_401a81 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R362 = phi i64 [ undef, %block_401a81 ]
  ; # 401a8a: test   rax,rax
  ; r367 := (bv_eq r362 0x0 :: [64])
  %R367 = icmp eq i64 %R362, 0
  ; # 401a8d: mov    rbp,rax
  ; # 401a90: je     401af1
  br i1 %R367, label %subblock_401a8a_1, label %subblock_401a8a_2
subblock_401a8a_1:
  br label %block_401af1
subblock_401a8a_2:
  br label %block_401a92
block_401a92:
  %R372 = phi i128 [ %R366, %subblock_401a8a_2 ]
  %R371 = phi i64 [ %R365, %subblock_401a8a_2 ]
  %R370 = phi i64 [ %R362, %subblock_401a8a_2 ]
  %R369 = phi i64 [ %R364, %subblock_401a8a_2 ]
  %R368 = phi i64 [ %R362, %subblock_401a8a_2 ]
  ; # 401a92: lea    rdx,[r12-0x10]
  ; r373 := (bv_add r371 0xfffffffffffffff0 :: [64])
  %R373 = add i64 %R371, 18446744073709551600
  ; # 401a97: mov    rsi,rbx
  ; # 401a9a: mov    rdi,rax
  ; # 401a9d: call   402375
  %r426 = bitcast i128 %R372 to <2 x double>
  %r427 = call { i64, i64, <2 x double> } @F402375(i64 %R368, i64 %R369, i64 %R373, <2 x double> %r426)
  %R374 = extractvalue { i64, i64, <2 x double> } %r427, 0
  %R375 = extractvalue { i64, i64, <2 x double> } %r427, 1
  %r430 = extractvalue { i64, i64, <2 x double> } %r427, 2
  %R376 = bitcast <2 x double> %r430 to i128
  br label %block_401aa2
block_401aa2:
  %R384 = phi i128 [ %R376, %block_401a92 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R383 = phi i64 [ undef, %block_401a92 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R382 = phi i64 [ undef, %block_401a92 ]
  %R381 = phi i64 [ %R375, %block_401a92 ]
  %R380 = phi i64 [ %R370, %block_401a92 ]
  %R379 = phi i64 [ %R369, %block_401a92 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R378 = phi i64 [ undef, %block_401a92 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R377 = phi i64 [ undef, %block_401a92 ]
  ; # 401aa2: mov    rdi,rbx
  ; # 401aa5: call   400e40
  %r440 = bitcast i128 %R384 to <2 x double>
  %r441 = call { i64, i64, <2 x double> } @F400e40(i64 %R379, i64 %R381, i64 %R378, i64 %R377, i64 %R382, i64 %R383, <2 x double> %r440)
  %R385 = extractvalue { i64, i64, <2 x double> } %r441, 0
  %R386 = extractvalue { i64, i64, <2 x double> } %r441, 1
  %r444 = extractvalue { i64, i64, <2 x double> } %r441, 2
  %R387 = bitcast <2 x double> %r444 to i128
  br label %block_401aaa
block_401aaa:
  %R390 = phi i128 [ %R387, %block_401aa2 ]
  %R389 = phi i64 [ %R380, %block_401aa2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R388 = phi i64 [ undef, %block_401aa2 ]
  ; # 401aaa: add    rsp,0x18
  ; # 401aae: mov    rax,rbp
  ; # 401ab1: pop    rbx
  ; # 401ab2: pop    rbp
  ; # 401ab3: pop    r12
  ; # 401ab5: pop    r13
  ; # 401ab7: pop    r14
  ; # 401ab9: pop    r15
  ; # 401abb: ret
  %r449 = bitcast i128 %R390 to <2 x double>
  %r450 = insertvalue { i64, i64, <2 x double> } undef, i64 %R389, 0
  %r451 = insertvalue { i64, i64, <2 x double> } %r450, i64 %R388, 1
  %r452 = insertvalue { i64, i64, <2 x double> } %r451, <2 x double> %r449, 2
  ret { i64, i64, <2 x double> } %r452
block_401ac0:
  %R396 = phi i128 [ %r0, %subblock_4018b0_1 ]
  %R395 = phi i64 [ %a5, %subblock_4018b0_1 ]
  %R394 = phi i64 [ %a4, %subblock_4018b0_1 ]
  %R393 = phi i64 [ %a1, %subblock_4018b0_1 ]
  %R392 = phi i64 [ %a2, %subblock_4018b0_1 ]
  %R391 = phi i64 [ %a3, %subblock_4018b0_1 ]
  ; # 401ac0: mov    rdi,rsi
  ; # 401ac3: jmp    401280
  %r459 = bitcast i128 %R396 to <2 x double>
  %r460 = call { i64, i64, <2 x double> } @F401280(i64 %R393, i64 %R393, i64 %R392, i64 %R391, i64 %R394, i64 %R395, <2 x double> %r459)
  ret { i64, i64, <2 x double> } %r460
block_401ad0:
  %R409 = phi i128 [ %R329, %subblock_401a58_1 ]
  %R408 = phi i64 [ %R327, %subblock_401a58_1 ]
  %R407 = phi i64 [ %R326, %subblock_401a58_1 ]
  %R406 = phi i64 [ %R325, %subblock_401a58_1 ]
  %R405 = phi i64 [ %R324, %subblock_401a58_1 ]
  %R404 = phi i64 [ %R323, %subblock_401a58_1 ]
  %R403 = phi i64 [ %R322, %subblock_401a58_1 ]
  %R402 = phi i64 [ %R321, %subblock_401a58_1 ]
  %R401 = phi i64 [ %R320, %subblock_401a58_1 ]
  %R400 = phi i64 [ %R319, %subblock_401a58_1 ]
  ; # 401ad0: mov    rax,r12
  ; # 401ad3: or     rax,0x1
  ; r410 := (bv_or r407 0x1 :: [64])
  %R410 = or i64 %R407, 1
  ; # 401ad7: mov    QWORD PTR [rbx-0x8],rax
  ; r411 := (bv_add r402 0xfffffffffffffff8 :: [64])
  %R411 = add i64 %R402, 18446744073709551608
  ; *(r411) = r410
  %r473 = inttoptr i64 %R411 to i64*
  store i64 %R410, i64* %r473
  ; # 401adb: mov    QWORD PTR [r13],rax
  ; *(r408) = r410
  %r474 = inttoptr i64 %R408 to i64*
  store i64 %R410, i64* %r474
  ; # 401adf: jmp    401a81
  br label %block_401a81
block_401ae8:
  %R416 = phi i128 [ %R128, %subblock_40194f_1 ]
  %R415 = phi i64 [ %R126, %subblock_40194f_1 ]
  %R414 = phi i64 [ %R125, %subblock_40194f_1 ]
  %R413 = phi i64 [ %R124, %subblock_40194f_1 ]
  %R412 = phi i64 [ %R123, %subblock_40194f_1 ]
  ; # 401ae8: cmp    r13,r12
  ; # 401aeb: ja     4019c8
  ; r417 := (bv_ult r414 r415)
  %R417 = icmp ult i64 %R414, %R415
  br i1 %R417, label %subblock_401ae8_1, label %subblock_401ae8_2
subblock_401ae8_1:
  br label %block_4019c8
subblock_401ae8_2:
  br label %block_401af1
block_401af1:
  %R419 = phi i128 [ %R416, %subblock_401ae8_2 ], [ %R366, %subblock_401a8a_1 ]
  %R418 = phi i64 [ %R412, %subblock_401ae8_2 ], [ %R363, %subblock_401a8a_1 ]
  ; # 401af1: xor    eax,eax
  ; # 401af3: jmp    4019cb
  br label %block_4019cb
block_401af8:
  %R423 = phi i128 [ %R147, %subblock_401980_1 ]
  %R422 = phi i64 [ %R144, %subblock_401980_1 ]
  %R421 = phi i64 [ %R143, %subblock_401980_1 ]
  %R420 = phi i64 [ %R140, %subblock_401980_1 ]
  ; # 401af8: call   402681
  %r487 = bitcast i128 %R423 to <2 x double>
  %r488 = call { i64, i64, <2 x double> } @F402681(i64 %R422, i64 %R421, i64 %R420, <2 x double> %r487)
  %R424 = extractvalue { i64, i64, <2 x double> } %r488, 0
  %R425 = extractvalue { i64, i64, <2 x double> } %r488, 1
  %r491 = extractvalue { i64, i64, <2 x double> } %r488, 2
  %R426 = bitcast <2 x double> %r491 to i128
  br label %block_401afd
block_401afd:
  %R429 = phi i128 [ %R426, %block_401af8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R428 = phi i64 [ undef, %block_401af8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R427 = phi i64 [ undef, %block_401af8 ]
  ; # 401afd: mov    DWORD PTR [rax],0xc
  ; *(r427) = 0xc :: [32]
  %r496 = inttoptr i64 %R427 to i32*
  store i32 12, i32* %r496
  ; # 401b03: xor    eax,eax
  ; # 401b05: jmp    4019cb
  br label %block_4019cb
failure:
  br label %failure
}