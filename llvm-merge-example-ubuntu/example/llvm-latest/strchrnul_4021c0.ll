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
declare { i64, i64, <2 x double> } @F402300(i64, <2 x double>)
define { i64, i64, <2 x double> } @F4021c0(i64 %a0, i64 %a1, <2 x double> %a2) {
entry:
  %r0 = bitcast <2 x double> %a2 to i128
  br label %block_4021c0
block_4021c0:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 4021c0: movzx  esi,sil
  ; r2 := (trunc arg1 8)
  %R2 = trunc i64 %a1 to i8
  ; r3 := (uext r2 32)
  %R3 = zext i8 %R2 to i32
  ; r4 := (uext r2 64)
  %R4 = zext i8 %R2 to i64
  ; # 4021c4: push   rbx
  ; r5 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R5 = add i64 %R1, 18446744073709551608
  ; # 4021c5: mov    rbx,rdi
  ; # 4021c8: test   esi,esi
  ; r6 := (bv_eq r3 0x0 :: [32])
  %R6 = icmp eq i32 %R3, 0
  ; # 4021ca: jne    4021ef
  ; r7 := (bv_complement r6)
  %R7 = xor i1 %R6, -1
  br i1 %R7, label %subblock_4021c0_1, label %subblock_4021c0_2
subblock_4021c0_1:
  br label %block_4021ef
subblock_4021c0_2:
  br label %block_4021cc
block_4021cc:
  %R10 = phi i128 [ %r0, %subblock_4021c0_2 ]
  %R9 = phi i64 [ %a0, %subblock_4021c0_2 ]
  %R8 = phi i64 [ %a0, %subblock_4021c0_2 ]
  ; # 4021cc: jmp    4022a8
  br label %block_4022a8
block_4021d8:
  %R13 = phi i128 [ %R30, %subblock_4021ef_1 ]
  %R12 = phi i64 [ %R29, %subblock_4021ef_1 ]
  %R11 = phi i64 [ %R28, %subblock_4021ef_1 ]
  ; # 4021d8: movzx  eax,BYTE PTR [rbx]
  ; r14 := *r11
  %r16 = inttoptr i64 %R11 to i8*
  %R14 = load i8* %r16
  ; r15 := (uext r14 64)
  %R15 = zext i8 %R14 to i64
  ; # 4021db: test   al,al
  ; r16 := (bv_eq r14 0x0 :: [8])
  %R16 = icmp eq i8 %R14, 0
  ; # 4021dd: je     40229f
  br i1 %R16, label %subblock_4021d8_1, label %subblock_4021d8_2
subblock_4021d8_1:
  br label %block_40229f
subblock_4021d8_2:
  br label %block_4021e3
block_4021e3:
  %R20 = phi i128 [ %R13, %subblock_4021d8_2 ]
  %R19 = phi i64 [ %R12, %subblock_4021d8_2 ]
  %R18 = phi i64 [ %R11, %subblock_4021d8_2 ]
  %R17 = phi i64 [ %R15, %subblock_4021d8_2 ]
  ; # 4021e3: cmp    esi,eax
  ; r21 := (trunc r19 32)
  %R21 = trunc i64 %R19 to i32
  ; r22 := (trunc r17 32)
  %R22 = trunc i64 %R17 to i32
  ; r23 := (bv_eq r21 r22)
  %R23 = icmp eq i32 %R21, %R22
  ; # 4021e5: je     40229f
  br i1 %R23, label %subblock_4021e3_1, label %subblock_4021e3_2
subblock_4021e3_1:
  br label %block_40229f
subblock_4021e3_2:
  br label %block_4021eb
block_4021eb:
  %R26 = phi i128 [ %R20, %subblock_4021e3_2 ]
  %R25 = phi i64 [ %R19, %subblock_4021e3_2 ]
  %R24 = phi i64 [ %R18, %subblock_4021e3_2 ]
  ; # 4021eb: add    rbx,0x1
  ; r27 := (bv_add r24 0x1 :: [64])
  %R27 = add i64 %R24, 1
  br label %block_4021ef
block_4021ef:
  %R30 = phi i128 [ %R26, %block_4021eb ], [ %r0, %subblock_4021c0_1 ]
  %R29 = phi i64 [ %R25, %block_4021eb ], [ %R4, %subblock_4021c0_1 ]
  %R28 = phi i64 [ %R27, %block_4021eb ], [ %a0, %subblock_4021c0_1 ]
  ; # 4021ef: test   bl,0x7
  ; r31 := (trunc r28 8)
  %R31 = trunc i64 %R28 to i8
  ; r32 := (bv_and r31 0x7 :: [8])
  %R32 = and i8 %R31, 7
  ; r33 := (bv_eq r32 0x0 :: [8])
  %R33 = icmp eq i8 %R32, 0
  ; # 4021f2: jne    4021d8
  ; r34 := (bv_complement r33)
  %R34 = xor i1 %R33, -1
  br i1 %R34, label %subblock_4021ef_1, label %subblock_4021ef_2
subblock_4021ef_1:
  br label %block_4021d8
subblock_4021ef_2:
  br label %block_4021f4
block_4021f4:
  %R37 = phi i128 [ %R30, %subblock_4021ef_2 ]
  %R36 = phi i64 [ %R29, %subblock_4021ef_2 ]
  %R35 = phi i64 [ %R28, %subblock_4021ef_2 ]
  ; # 4021f4: mov    rdi,0x101010101010101
  ; # 4021fe: movsxd r9,esi
  ; r38 := (trunc r36 32)
  %R38 = trunc i64 %R36 to i32
  ; # 402201: mov    rdx,QWORD PTR [rbx]
  ; r39 := *r35
  %r42 = inttoptr i64 %R35 to i64*
  %R39 = load i64* %r42
  ; # 402204: imul   r9,rdi
  ; r40 := (sext 0x101010101010101 :: [64] 128)
  %R40 = sext i64 72340172838076673 to i128
  ; r41 := (sext r38 128)
  %R41 = sext i32 %R38 to i128
  ; r42 := (bv_mul r40 r41)
  %R42 = mul i128 %R40, %R41
  ; r43 := (trunc r42 64)
  %R43 = trunc i128 %R42 to i64
  ; # 402208: mov    r10,0x8080808080808080
  ; # 402212: mov    rcx,rdx
  ; # 402215: sub    rcx,rdi
  ; r44 := (bv_add r39 0xfefefefefefefeff :: [64])
  %R44 = add i64 %R39, 18374403900871474943
  ; # 402218: mov    rax,r9
  ; # 40221b: xor    rax,rdx
  ; r45 := (bv_xor r43 r39)
  %R45 = xor i64 %R43, %R39
  ; # 40221e: not    rdx
  ; r46 := (bv_complement r39)
  %R46 = xor i64 %R39, -1
  ; # 402221: and    rcx,rdx
  ; r47 := (bv_and r44 r46)
  %R47 = and i64 %R44, %R46
  ; # 402224: mov    rdx,rax
  ; # 402227: not    rax
  ; r48 := (bv_complement r45)
  %R48 = xor i64 %R45, -1
  ; # 40222a: sub    rdx,rdi
  ; r49 := (bv_add r45 0xfefefefefefefeff :: [64])
  %R49 = add i64 %R45, 18374403900871474943
  ; # 40222d: mov    rdi,rdx
  ; # 402230: mov    rdx,rax
  ; # 402233: mov    rax,rdi
  ; # 402236: and    rax,rdx
  ; r50 := (bv_and r49 r48)
  %R50 = and i64 %R49, %R48
  ; # 402239: or     rax,rcx
  ; r51 := (bv_or r50 r47)
  %R51 = or i64 %R50, %R47
  ; # 40223c: test   rax,r10
  ; r52 := (bv_and r51 0x8080808080808080 :: [64])
  %R52 = and i64 %R51, 9259542123273814144
  ; r53 := (bv_eq r52 0x0 :: [64])
  %R53 = icmp eq i64 %R52, 0
  ; # 40223f: jne    40227f
  ; r54 := (bv_complement r53)
  %R54 = xor i1 %R53, -1
  br i1 %R54, label %subblock_4021f4_1, label %subblock_4021f4_2
subblock_4021f4_1:
  br label %block_40227f
subblock_4021f4_2:
  br label %block_402241
block_402241:
  %R59 = phi i128 [ %R37, %subblock_4021f4_2 ]
  %R58 = phi i64 [ 9259542123273814144, %subblock_4021f4_2 ]
  %R57 = phi i64 [ %R43, %subblock_4021f4_2 ]
  %R56 = phi i64 [ %R36, %subblock_4021f4_2 ]
  %R55 = phi i64 [ %R35, %subblock_4021f4_2 ]
  ; # 402241: mov    rdi,0xfefefefefefefeff
  ; # 40224b: nop    [rax+rax*1]
  br label %block_402250
block_402250:
  %R65 = phi i128 [ %R65, %subblock_402250_1 ], [ %R59, %block_402241 ]
  %R64 = phi i64 [ %R64, %subblock_402250_1 ], [ %R58, %block_402241 ]
  %R63 = phi i64 [ %R63, %subblock_402250_1 ], [ %R57, %block_402241 ]
  %R62 = phi i64 [ %R62, %subblock_402250_1 ], [ 18374403900871474943, %block_402241 ]
  %R61 = phi i64 [ %R61, %subblock_402250_1 ], [ %R56, %block_402241 ]
  %R60 = phi i64 [ %R66, %subblock_402250_1 ], [ %R55, %block_402241 ]
  ; # 402250: add    rbx,0x8
  ; r66 := (bv_add r60 0x8 :: [64])
  %R66 = add i64 %R60, 8
  ; # 402254: mov    rdx,QWORD PTR [rbx]
  ; r67 := *r66
  %r71 = inttoptr i64 %R66 to i64*
  %R67 = load i64* %r71
  ; # 402257: mov    rax,r9
  ; # 40225a: xor    rax,rdx
  ; r68 := (bv_xor r63 r67)
  %R68 = xor i64 %R63, %R67
  ; # 40225d: lea    rcx,[rdx+rdi*1]
  ; r69 := (bv_add r67 r62)
  %R69 = add i64 %R67, %R62
  ; # 402261: not    rdx
  ; r70 := (bv_complement r67)
  %R70 = xor i64 %R67, -1
  ; # 402264: lea    r8,[rax+rdi*1]
  ; r71 := (bv_add r68 r62)
  %R71 = add i64 %R68, %R62
  ; # 402268: not    rax
  ; r72 := (bv_complement r68)
  %R72 = xor i64 %R68, -1
  ; # 40226b: and    rcx,rdx
  ; r73 := (bv_and r69 r70)
  %R73 = and i64 %R69, %R70
  ; # 40226e: mov    rdx,rax
  ; # 402271: mov    rax,r8
  ; # 402274: and    rax,rdx
  ; r74 := (bv_and r71 r72)
  %R74 = and i64 %R71, %R72
  ; # 402277: or     rax,rcx
  ; r75 := (bv_or r74 r73)
  %R75 = or i64 %R74, %R73
  ; # 40227a: test   rax,r10
  ; r76 := (bv_and r75 r64)
  %R76 = and i64 %R75, %R64
  ; r77 := (bv_eq r76 0x0 :: [64])
  %R77 = icmp eq i64 %R76, 0
  ; # 40227d: je     402250
  br i1 %R77, label %subblock_402250_1, label %subblock_402250_2
subblock_402250_1:
  br label %block_402250
subblock_402250_2:
  br label %block_40227f
block_40227f:
  %R80 = phi i128 [ %R65, %subblock_402250_2 ], [ %R37, %subblock_4021f4_1 ]
  %R79 = phi i64 [ %R61, %subblock_402250_2 ], [ %R36, %subblock_4021f4_1 ]
  %R78 = phi i64 [ %R66, %subblock_402250_2 ], [ %R35, %subblock_4021f4_1 ]
  ; # 40227f: movzx  eax,BYTE PTR [rbx]
  ; r81 := *r78
  %r86 = inttoptr i64 %R78 to i8*
  %R81 = load i8* %r86
  ; r82 := (uext r81 64)
  %R82 = zext i8 %R81 to i64
  ; # 402282: test   al,al
  ; r83 := (bv_eq r81 0x0 :: [8])
  %R83 = icmp eq i8 %R81, 0
  ; # 402284: jne    40229b
  ; r84 := (bv_complement r83)
  %R84 = xor i1 %R83, -1
  br i1 %R84, label %subblock_40227f_1, label %subblock_40227f_2
subblock_40227f_1:
  br label %block_40229b
subblock_40227f_2:
  br label %block_402286
block_402286:
  %R86 = phi i128 [ %R80, %subblock_40227f_2 ]
  %R85 = phi i64 [ %R78, %subblock_40227f_2 ]
  ; # 402286: jmp    40229f
  br label %block_40229f
block_402290:
  %R89 = phi i128 [ %R97, %subblock_40229b_1 ]
  %R88 = phi i64 [ %R96, %subblock_40229b_1 ]
  %R87 = phi i64 [ %R95, %subblock_40229b_1 ]
  ; # 402290: add    rbx,0x1
  ; r90 := (bv_add r87 0x1 :: [64])
  %R90 = add i64 %R87, 1
  ; # 402294: movzx  eax,BYTE PTR [rbx]
  ; r91 := *r90
  %r97 = inttoptr i64 %R90 to i8*
  %R91 = load i8* %r97
  ; r92 := (uext r91 64)
  %R92 = zext i8 %R91 to i64
  ; # 402297: test   al,al
  ; r93 := (bv_eq r91 0x0 :: [8])
  %R93 = icmp eq i8 %R91, 0
  ; # 402299: je     40229f
  br i1 %R93, label %subblock_402290_1, label %subblock_402290_2
subblock_402290_1:
  br label %block_40229f
subblock_402290_2:
  br label %block_40229b
block_40229b:
  %R97 = phi i128 [ %R89, %subblock_402290_2 ], [ %R80, %subblock_40227f_1 ]
  %R96 = phi i64 [ %R88, %subblock_402290_2 ], [ %R79, %subblock_40227f_1 ]
  %R95 = phi i64 [ %R90, %subblock_402290_2 ], [ %R78, %subblock_40227f_1 ]
  %R94 = phi i64 [ %R92, %subblock_402290_2 ], [ %R82, %subblock_40227f_1 ]
  ; # 40229b: cmp    esi,eax
  ; r98 := (trunc r96 32)
  %R98 = trunc i64 %R96 to i32
  ; r99 := (trunc r94 32)
  %R99 = trunc i64 %R94 to i32
  ; r100 := (bv_eq r98 r99)
  %R100 = icmp eq i32 %R98, %R99
  ; # 40229d: jne    402290
  ; r101 := (bv_complement r100)
  %R101 = xor i1 %R100, -1
  br i1 %R101, label %subblock_40229b_1, label %subblock_40229b_2
subblock_40229b_1:
  br label %block_402290
subblock_40229b_2:
  br label %block_40229f
block_40229f:
  %R103 = phi i128 [ %R20, %subblock_4021e3_1 ], [ %R13, %subblock_4021d8_1 ], [ %R86, %block_402286 ], [ %R89, %subblock_402290_1 ], [ %R97, %subblock_40229b_2 ]
  %R102 = phi i64 [ %R18, %subblock_4021e3_1 ], [ %R11, %subblock_4021d8_1 ], [ %R85, %block_402286 ], [ %R90, %subblock_402290_1 ], [ %R95, %subblock_40229b_2 ]
  ; # 40229f: mov    rax,rbx
  ; # 4022a2: pop    rbx
  ; # 4022a3: ret
  %r111 = bitcast i128 %R103 to <2 x double>
  %r112 = insertvalue { i64, i64, <2 x double> } undef, i64 %R102, 0
  %r113 = insertvalue { i64, i64, <2 x double> } %r112, i64 undef, 1
  %r114 = insertvalue { i64, i64, <2 x double> } %r113, <2 x double> %r111, 2
  ret { i64, i64, <2 x double> } %r114
block_4022a8:
  %R106 = phi i128 [ %R10, %block_4021cc ]
  %R105 = phi i64 [ %R9, %block_4021cc ]
  %R104 = phi i64 [ %R8, %block_4021cc ]
  ; # 4022a8: call   402300
  %r118 = bitcast i128 %R106 to <2 x double>
  %r119 = call { i64, i64, <2 x double> } @F402300(i64 %R105, <2 x double> %r118)
  %R107 = extractvalue { i64, i64, <2 x double> } %r119, 0
  %r121 = extractvalue { i64, i64, <2 x double> } %r119, 2
  %R108 = bitcast <2 x double> %r121 to i128
  br label %block_4022ad
block_4022ad:
  %R111 = phi i128 [ %R108, %block_4022a8 ]
  %R110 = phi i64 [ %R104, %block_4022a8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R109 = phi i64 [ undef, %block_4022a8 ]
  ; # 4022ad: add    rax,rbx
  ; r112 := (bv_add r109 r110)
  %R112 = add i64 %R109, %R110
  ; # 4022b0: pop    rbx
  ; # 4022b1: ret
  %r127 = bitcast i128 %R111 to <2 x double>
  %r128 = insertvalue { i64, i64, <2 x double> } undef, i64 %R112, 0
  %r129 = insertvalue { i64, i64, <2 x double> } %r128, i64 undef, 1
  %r130 = insertvalue { i64, i64, <2 x double> } %r129, <2 x double> %r127, 2
  ret { i64, i64, <2 x double> } %r130
failure:
  br label %failure
}