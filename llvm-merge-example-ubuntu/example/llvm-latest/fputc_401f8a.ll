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
declare { i64, i64, <2 x double> } @F402a2c(i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402a80(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402ac5(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
define { i64, i64, <2 x double> } @F401f8a(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_401f8a
block_401f8a:
  ; r0 := (alloca 0x20 :: [64])
  %r8 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 401f8a: push   r12
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401f8c: push   rbp
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 401f8d: mov    r12d,edi
  ; r4 := (trunc arg0 32)
  %R4 = trunc i64 %a0 to i32
  ; r5 := (uext r4 64)
  %R5 = zext i32 %R4 to i64
  ; # 401f90: push   rbx
  ; r6 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R6 = add i64 %R1, 18446744073709551592
  ; # 401f91: mov    eax,DWORD PTR [rsi+0x8c]
  ; r7 := (bv_add arg1 0x8c :: [64])
  %R7 = add i64 %a1, 140
  ; r8 := *r7
  %r17 = inttoptr i64 %R7 to i32*
  %R8 = load i32* %r17
  ; # 401f97: mov    rbx,rsi
  ; # 401f9a: movzx  ebp,dil
  ; r9 := (trunc arg0 8)
  %R9 = trunc i64 %a0 to i8
  ; r10 := (uext r9 64)
  %R10 = zext i8 %R9 to i64
  ; # 401f9e: test   eax,eax
  ; # 401fa0: jns    401fb1
  ; r11 := (bv_sle 0x0 :: [32] r8)
  %R11 = icmp sle i32 0, %R8
  br i1 %R11, label %subblock_401f8a_1, label %subblock_401f8a_2
subblock_401f8a_1:
  br label %block_401fb1
subblock_401f8a_2:
  br label %block_401fa2
block_401fa2:
  %R25 = phi i128 [ %R71, %subblock_401fb9_1 ], [ %r7, %subblock_401f8a_2 ]
  %R24 = phi i128 [ %R70, %subblock_401fb9_1 ], [ %r6, %subblock_401f8a_2 ]
  %R23 = phi i128 [ %R69, %subblock_401fb9_1 ], [ %r5, %subblock_401f8a_2 ]
  %R22 = phi i128 [ %R68, %subblock_401fb9_1 ], [ %r4, %subblock_401f8a_2 ]
  %R21 = phi i128 [ %R67, %subblock_401fb9_1 ], [ %r3, %subblock_401f8a_2 ]
  %R20 = phi i128 [ %R66, %subblock_401fb9_1 ], [ %r2, %subblock_401f8a_2 ]
  %R19 = phi i128 [ %R65, %subblock_401fb9_1 ], [ %r1, %subblock_401f8a_2 ]
  %R18 = phi i128 [ %R64, %subblock_401fb9_1 ], [ %r0, %subblock_401f8a_2 ]
  %R17 = phi i64 [ %R63, %subblock_401fb9_1 ], [ %R5, %subblock_401f8a_2 ]
  %R16 = phi i64 [ %R62, %subblock_401fb9_1 ], [ %a5, %subblock_401f8a_2 ]
  %R15 = phi i64 [ %R61, %subblock_401fb9_1 ], [ %a4, %subblock_401f8a_2 ]
  %R14 = phi i64 [ %R59, %subblock_401fb9_1 ], [ %R10, %subblock_401f8a_2 ]
  %R13 = phi i64 [ %R58, %subblock_401fb9_1 ], [ %a1, %subblock_401f8a_2 ]
  %R12 = phi i64 [ %R57, %subblock_401fb9_1 ], [ %a3, %subblock_401f8a_2 ]
  ; # 401fa2: movsx  edx,BYTE PTR [rbx+0x8b]
  ; r26 := (bv_add r13 0x8b :: [64])
  %R26 = add i64 %R13, 139
  ; r27 := *r26
  %r37 = inttoptr i64 %R26 to i8*
  %R27 = load i8* %r37
  ; r28 := (sext r27 32)
  %R28 = sext i8 %R27 to i32
  ; r29 := (uext r28 64)
  %R29 = zext i32 %R28 to i64
  ; # 401fa9: mov    eax,ebp
  ; r30 := (trunc r14 32)
  %R30 = trunc i64 %R14 to i32
  ; r31 := (uext r30 64)
  %R31 = zext i32 %R30 to i64
  ; # 401fab: cmp    ebp,edx
  ; r32 := (bv_eq r30 r28)
  %R32 = icmp eq i32 %R30, %R28
  ; # 401fad: jne    401fbf
  ; r33 := (bv_complement r32)
  %R33 = xor i1 %R32, -1
  br i1 %R33, label %subblock_401fa2_1, label %subblock_401fa2_2
subblock_401fa2_1:
  br label %block_401fbf
subblock_401fa2_2:
  br label %block_401faf
block_401faf:
  %R47 = phi i128 [ %R25, %subblock_401fa2_2 ]
  %R46 = phi i128 [ %R24, %subblock_401fa2_2 ]
  %R45 = phi i128 [ %R23, %subblock_401fa2_2 ]
  %R44 = phi i128 [ %R22, %subblock_401fa2_2 ]
  %R43 = phi i128 [ %R21, %subblock_401fa2_2 ]
  %R42 = phi i128 [ %R20, %subblock_401fa2_2 ]
  %R41 = phi i128 [ %R19, %subblock_401fa2_2 ]
  %R40 = phi i128 [ %R18, %subblock_401fa2_2 ]
  %R39 = phi i64 [ %R17, %subblock_401fa2_2 ]
  %R38 = phi i64 [ %R16, %subblock_401fa2_2 ]
  %R37 = phi i64 [ %R15, %subblock_401fa2_2 ]
  %R36 = phi i64 [ %R13, %subblock_401fa2_2 ]
  %R35 = phi i64 [ %R29, %subblock_401fa2_2 ]
  %R34 = phi i64 [ %R12, %subblock_401fa2_2 ]
  ; # 401faf: jmp    401fd6
  br label %block_401fd6
block_401fb1:
  %R52 = phi i128 [ %r0, %subblock_401f8a_1 ]
  %R51 = phi i64 [ %R5, %subblock_401f8a_1 ]
  %R50 = phi i64 [ %a1, %subblock_401f8a_1 ]
  %R49 = phi i64 [ %R10, %subblock_401f8a_1 ]
  %R48 = phi i64 [ %a1, %subblock_401f8a_1 ]
  ; # 401fb1: mov    rdi,rsi
  ; # 401fb4: call   402a2c
  %r64 = bitcast i128 %R52 to <2 x double>
  %r65 = call { i64, i64, <2 x double> } @F402a2c(i64 %R50, <2 x double> %r64)
  %R53 = extractvalue { i64, i64, <2 x double> } %r65, 0
  %R54 = extractvalue { i64, i64, <2 x double> } %r65, 1
  %r68 = extractvalue { i64, i64, <2 x double> } %r65, 2
  %R55 = bitcast <2 x double> %r68 to i128
  br label %block_401fb9
block_401fb9:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R71 = phi i128 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R70 = phi i128 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R69 = phi i128 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R68 = phi i128 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R67 = phi i128 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R66 = phi i128 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R65 = phi i128 [ undef, %block_401fb1 ]
  %R64 = phi i128 [ %R55, %block_401fb1 ]
  %R63 = phi i64 [ %R51, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R62 = phi i64 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R61 = phi i64 [ undef, %block_401fb1 ]
  %R60 = phi i64 [ %R54, %block_401fb1 ]
  %R59 = phi i64 [ %R49, %block_401fb1 ]
  %R58 = phi i64 [ %R48, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R57 = phi i64 [ undef, %block_401fb1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R56 = phi i64 [ undef, %block_401fb1 ]
  ; # 401fb9: test   eax,eax
  ; r72 := (trunc r56 32)
  %R72 = trunc i64 %R56 to i32
  ; r73 := (bv_eq r72 0x0 :: [32])
  %R73 = icmp eq i32 %R72, 0
  ; # 401fbb: je     401fa2
  br i1 %R73, label %subblock_401fb9_1, label %subblock_401fb9_2
subblock_401fb9_1:
  br label %block_401fa2
subblock_401fb9_2:
  br label %block_401fbd
block_401fbd:
  %R88 = phi i128 [ %R71, %subblock_401fb9_2 ]
  %R87 = phi i128 [ %R70, %subblock_401fb9_2 ]
  %R86 = phi i128 [ %R69, %subblock_401fb9_2 ]
  %R85 = phi i128 [ %R68, %subblock_401fb9_2 ]
  %R84 = phi i128 [ %R67, %subblock_401fb9_2 ]
  %R83 = phi i128 [ %R66, %subblock_401fb9_2 ]
  %R82 = phi i128 [ %R65, %subblock_401fb9_2 ]
  %R81 = phi i128 [ %R64, %subblock_401fb9_2 ]
  %R80 = phi i64 [ %R63, %subblock_401fb9_2 ]
  %R79 = phi i64 [ %R62, %subblock_401fb9_2 ]
  %R78 = phi i64 [ %R61, %subblock_401fb9_2 ]
  %R77 = phi i64 [ %R60, %subblock_401fb9_2 ]
  %R76 = phi i64 [ %R59, %subblock_401fb9_2 ]
  %R75 = phi i64 [ %R58, %subblock_401fb9_2 ]
  %R74 = phi i64 [ %R57, %subblock_401fb9_2 ]
  ; # 401fbd: jmp    401fe5
  br label %block_401fe5
block_401fbf:
  %R102 = phi i128 [ %R25, %subblock_401fa2_1 ]
  %R101 = phi i128 [ %R24, %subblock_401fa2_1 ]
  %R100 = phi i128 [ %R23, %subblock_401fa2_1 ]
  %R99 = phi i128 [ %R22, %subblock_401fa2_1 ]
  %R98 = phi i128 [ %R21, %subblock_401fa2_1 ]
  %R97 = phi i128 [ %R20, %subblock_401fa2_1 ]
  %R96 = phi i128 [ %R19, %subblock_401fa2_1 ]
  %R95 = phi i128 [ %R18, %subblock_401fa2_1 ]
  %R94 = phi i64 [ %R17, %subblock_401fa2_1 ]
  %R93 = phi i64 [ %R16, %subblock_401fa2_1 ]
  %R92 = phi i64 [ %R15, %subblock_401fa2_1 ]
  %R91 = phi i64 [ %R13, %subblock_401fa2_1 ]
  %R90 = phi i64 [ %R12, %subblock_401fa2_1 ]
  %R89 = phi i64 [ %R31, %subblock_401fa2_1 ]
  ; # 401fbf: mov    rdx,QWORD PTR [rbx+0x28]
  ; r103 := (bv_add r91 0x28 :: [64])
  %R103 = add i64 %R91, 40
  ; r104 := *r103
  %r118 = inttoptr i64 %R103 to i64*
  %R104 = load i64* %r118
  ; # 401fc3: cmp    rdx,QWORD PTR [rbx+0x20]
  ; r105 := (bv_add r91 0x20 :: [64])
  %R105 = add i64 %R91, 32
  ; r106 := *r105
  %r121 = inttoptr i64 %R105 to i64*
  %R106 = load i64* %r121
  ; # 401fc7: jae    401fd6
  ; r107 := (bv_ule r106 r104)
  %R107 = icmp ule i64 %R106, %R104
  br i1 %R107, label %subblock_401fbf_1, label %subblock_401fbf_2
subblock_401fbf_1:
  br label %block_401fd6
subblock_401fbf_2:
  br label %block_401fc9
block_401fc9:
  %R112 = phi i128 [ %R95, %subblock_401fbf_2 ]
  %R111 = phi i64 [ %R94, %subblock_401fbf_2 ]
  %R110 = phi i64 [ %R91, %subblock_401fbf_2 ]
  %R109 = phi i64 [ %R104, %subblock_401fbf_2 ]
  %R108 = phi i64 [ %R89, %subblock_401fbf_2 ]
  ; # 401fc9: lea    rcx,[rdx+0x1]
  ; r113 := (bv_add r109 0x1 :: [64])
  %R113 = add i64 %R109, 1
  ; # 401fcd: mov    QWORD PTR [rbx+0x28],rcx
  ; r114 := (bv_add r110 0x28 :: [64])
  %R114 = add i64 %R110, 40
  ; *(r114) = r113
  %r131 = inttoptr i64 %R114 to i64*
  store i64 %R113, i64* %r131
  ; # 401fd1: mov    BYTE PTR [rdx],r12b
  ; r115 := (trunc r111 8)
  %R115 = trunc i64 %R111 to i8
  ; *(r109) = r115
  %r133 = inttoptr i64 %R109 to i8*
  store i8 %R115, i8* %r133
  ; # 401fd4: jmp    40201e
  br label %block_40201e
block_401fd6:
  %R129 = phi i128 [ %R47, %block_401faf ], [ %R102, %subblock_401fbf_1 ]
  %R128 = phi i128 [ %R46, %block_401faf ], [ %R101, %subblock_401fbf_1 ]
  %R127 = phi i128 [ %R45, %block_401faf ], [ %R100, %subblock_401fbf_1 ]
  %R126 = phi i128 [ %R44, %block_401faf ], [ %R99, %subblock_401fbf_1 ]
  %R125 = phi i128 [ %R43, %block_401faf ], [ %R98, %subblock_401fbf_1 ]
  %R124 = phi i128 [ %R42, %block_401faf ], [ %R97, %subblock_401fbf_1 ]
  %R123 = phi i128 [ %R41, %block_401faf ], [ %R96, %subblock_401fbf_1 ]
  %R122 = phi i128 [ %R40, %block_401faf ], [ %R95, %subblock_401fbf_1 ]
  %R121 = phi i64 [ %R39, %block_401faf ], [ %R94, %subblock_401fbf_1 ]
  %R120 = phi i64 [ %R38, %block_401faf ], [ %R93, %subblock_401fbf_1 ]
  %R119 = phi i64 [ %R37, %block_401faf ], [ %R92, %subblock_401fbf_1 ]
  %R118 = phi i64 [ %R36, %block_401faf ], [ %R91, %subblock_401fbf_1 ]
  %R117 = phi i64 [ %R35, %block_401faf ], [ %R104, %subblock_401fbf_1 ]
  %R116 = phi i64 [ %R34, %block_401faf ], [ %R90, %subblock_401fbf_1 ]
  ; # 401fd6: mov    esi,r12d
  ; r130 := (trunc r121 32)
  %R130 = trunc i64 %R121 to i32
  ; r131 := (uext r130 64)
  %R131 = zext i32 %R130 to i64
  ; # 401fd9: mov    rdi,rbx
  ; # 401fdc: pop    rbx
  ; # 401fdd: pop    rbp
  ; # 401fde: pop    r12
  ; # 401fe0: jmp    402ac5
  %r150 = bitcast i128 %R122 to <2 x double>
  %r151 = bitcast i128 %R123 to <2 x double>
  %r152 = bitcast i128 %R124 to <2 x double>
  %r153 = bitcast i128 %R125 to <2 x double>
  %r154 = bitcast i128 %R126 to <2 x double>
  %r155 = bitcast i128 %R127 to <2 x double>
  %r156 = bitcast i128 %R128 to <2 x double>
  %r157 = bitcast i128 %R129 to <2 x double>
  %r158 = call { i64, i64, <2 x double> } @F402ac5(i64 %R118, i64 %R131, i64 %R117, i64 %R116, i64 %R119, i64 %R120, <2 x double> %r150, <2 x double> %r151, <2 x double> %r152, <2 x double> %r153, <2 x double> %r154, <2 x double> %r155, <2 x double> %r156, <2 x double> %r157)
  ret { i64, i64, <2 x double> } %r158
block_401fe5:
  %R149 = phi i128 [ %R88, %block_401fbd ]
  %R148 = phi i128 [ %R87, %block_401fbd ]
  %R147 = phi i128 [ %R86, %block_401fbd ]
  %R146 = phi i128 [ %R85, %block_401fbd ]
  %R145 = phi i128 [ %R84, %block_401fbd ]
  %R144 = phi i128 [ %R83, %block_401fbd ]
  %R143 = phi i128 [ %R82, %block_401fbd ]
  %R142 = phi i128 [ %R81, %block_401fbd ]
  %R141 = phi i64 [ %R80, %block_401fbd ]
  %R140 = phi i64 [ %R79, %block_401fbd ]
  %R139 = phi i64 [ %R78, %block_401fbd ]
  %R138 = phi i64 [ %R77, %block_401fbd ]
  %R137 = phi i64 [ %R76, %block_401fbd ]
  %R136 = phi i64 [ %R75, %block_401fbd ]
  %R135 = phi i64 [ %R74, %block_401fbd ]
  ; # 401fe5: movsx  edx,BYTE PTR [rbx+0x8b]
  ; r150 := (bv_add r136 0x8b :: [64])
  %R150 = add i64 %R136, 139
  ; r151 := *r150
  %r175 = inttoptr i64 %R150 to i8*
  %R151 = load i8* %r175
  ; r152 := (sext r151 32)
  %R152 = sext i8 %R151 to i32
  ; r153 := (uext r152 64)
  %R153 = zext i32 %R152 to i64
  ; # 401fec: cmp    ebp,edx
  ; r154 := (trunc r137 32)
  %R154 = trunc i64 %R137 to i32
  ; r155 := (bv_eq r154 r152)
  %R155 = icmp eq i32 %R154, %R152
  ; # 401fee: je     402007
  br i1 %R155, label %subblock_401fe5_1, label %subblock_401fe5_2
subblock_401fe5_1:
  br label %block_402007
subblock_401fe5_2:
  br label %block_401ff0
block_401ff0:
  %R170 = phi i128 [ %R149, %subblock_401fe5_2 ]
  %R169 = phi i128 [ %R148, %subblock_401fe5_2 ]
  %R168 = phi i128 [ %R147, %subblock_401fe5_2 ]
  %R167 = phi i128 [ %R146, %subblock_401fe5_2 ]
  %R166 = phi i128 [ %R145, %subblock_401fe5_2 ]
  %R165 = phi i128 [ %R144, %subblock_401fe5_2 ]
  %R164 = phi i128 [ %R143, %subblock_401fe5_2 ]
  %R163 = phi i128 [ %R142, %subblock_401fe5_2 ]
  %R162 = phi i64 [ %R141, %subblock_401fe5_2 ]
  %R161 = phi i64 [ %R140, %subblock_401fe5_2 ]
  %R160 = phi i64 [ %R139, %subblock_401fe5_2 ]
  %R159 = phi i64 [ %R138, %subblock_401fe5_2 ]
  %R158 = phi i64 [ %R137, %subblock_401fe5_2 ]
  %R157 = phi i64 [ %R136, %subblock_401fe5_2 ]
  %R156 = phi i64 [ %R135, %subblock_401fe5_2 ]
  ; # 401ff0: mov    rdx,QWORD PTR [rbx+0x28]
  ; r171 := (bv_add r157 0x28 :: [64])
  %R171 = add i64 %R157, 40
  ; r172 := *r171
  %r197 = inttoptr i64 %R171 to i64*
  %R172 = load i64* %r197
  ; # 401ff4: cmp    rdx,QWORD PTR [rbx+0x20]
  ; r173 := (bv_add r157 0x20 :: [64])
  %R173 = add i64 %R157, 32
  ; r174 := *r173
  %r200 = inttoptr i64 %R173 to i64*
  %R174 = load i64* %r200
  ; # 401ff8: jae    402007
  ; r175 := (bv_ule r174 r172)
  %R175 = icmp ule i64 %R174, %R172
  br i1 %R175, label %subblock_401ff0_1, label %subblock_401ff0_2
subblock_401ff0_1:
  br label %block_402007
subblock_401ff0_2:
  br label %block_401ffa
block_401ffa:
  %R183 = phi i128 [ %R163, %subblock_401ff0_2 ]
  %R182 = phi i64 [ %R162, %subblock_401ff0_2 ]
  %R181 = phi i64 [ %R161, %subblock_401ff0_2 ]
  %R180 = phi i64 [ %R160, %subblock_401ff0_2 ]
  %R179 = phi i64 [ %R159, %subblock_401ff0_2 ]
  %R178 = phi i64 [ %R158, %subblock_401ff0_2 ]
  %R177 = phi i64 [ %R157, %subblock_401ff0_2 ]
  %R176 = phi i64 [ %R172, %subblock_401ff0_2 ]
  ; # 401ffa: lea    rcx,[rdx+0x1]
  ; r184 := (bv_add r176 0x1 :: [64])
  %R184 = add i64 %R176, 1
  ; # 401ffe: mov    QWORD PTR [rbx+0x28],rcx
  ; r185 := (bv_add r177 0x28 :: [64])
  %R185 = add i64 %R177, 40
  ; *(r185) = r184
  %r213 = inttoptr i64 %R185 to i64*
  store i64 %R184, i64* %r213
  ; # 402002: mov    BYTE PTR [rdx],r12b
  ; r186 := (trunc r182 8)
  %R186 = trunc i64 %R182 to i8
  ; *(r176) = r186
  %r215 = inttoptr i64 %R176 to i8*
  store i8 %R186, i8* %r215
  ; # 402005: jmp    402014
  br label %block_402014
block_402007:
  %R200 = phi i128 [ %R170, %subblock_401ff0_1 ], [ %R149, %subblock_401fe5_1 ]
  %R199 = phi i128 [ %R169, %subblock_401ff0_1 ], [ %R148, %subblock_401fe5_1 ]
  %R198 = phi i128 [ %R168, %subblock_401ff0_1 ], [ %R147, %subblock_401fe5_1 ]
  %R197 = phi i128 [ %R167, %subblock_401ff0_1 ], [ %R146, %subblock_401fe5_1 ]
  %R196 = phi i128 [ %R166, %subblock_401ff0_1 ], [ %R145, %subblock_401fe5_1 ]
  %R195 = phi i128 [ %R165, %subblock_401ff0_1 ], [ %R144, %subblock_401fe5_1 ]
  %R194 = phi i128 [ %R164, %subblock_401ff0_1 ], [ %R143, %subblock_401fe5_1 ]
  %R193 = phi i128 [ %R163, %subblock_401ff0_1 ], [ %R142, %subblock_401fe5_1 ]
  %R192 = phi i64 [ %R162, %subblock_401ff0_1 ], [ %R141, %subblock_401fe5_1 ]
  %R191 = phi i64 [ %R161, %subblock_401ff0_1 ], [ %R140, %subblock_401fe5_1 ]
  %R190 = phi i64 [ %R160, %subblock_401ff0_1 ], [ %R139, %subblock_401fe5_1 ]
  %R189 = phi i64 [ %R157, %subblock_401ff0_1 ], [ %R136, %subblock_401fe5_1 ]
  %R188 = phi i64 [ %R172, %subblock_401ff0_1 ], [ %R153, %subblock_401fe5_1 ]
  %R187 = phi i64 [ %R156, %subblock_401ff0_1 ], [ %R135, %subblock_401fe5_1 ]
  ; # 402007: mov    esi,r12d
  ; r201 := (trunc r192 32)
  %R201 = trunc i64 %R192 to i32
  ; r202 := (uext r201 64)
  %R202 = zext i32 %R201 to i64
  ; # 40200a: mov    rdi,rbx
  ; # 40200d: call   402ac5
  %r232 = bitcast i128 %R193 to <2 x double>
  %r233 = bitcast i128 %R194 to <2 x double>
  %r234 = bitcast i128 %R195 to <2 x double>
  %r235 = bitcast i128 %R196 to <2 x double>
  %r236 = bitcast i128 %R197 to <2 x double>
  %r237 = bitcast i128 %R198 to <2 x double>
  %r238 = bitcast i128 %R199 to <2 x double>
  %r239 = bitcast i128 %R200 to <2 x double>
  %r240 = call { i64, i64, <2 x double> } @F402ac5(i64 %R189, i64 %R202, i64 %R188, i64 %R187, i64 %R190, i64 %R191, <2 x double> %r232, <2 x double> %r233, <2 x double> %r234, <2 x double> %r235, <2 x double> %r236, <2 x double> %r237, <2 x double> %r238, <2 x double> %r239)
  %R203 = extractvalue { i64, i64, <2 x double> } %r240, 0
  %R204 = extractvalue { i64, i64, <2 x double> } %r240, 1
  %r243 = extractvalue { i64, i64, <2 x double> } %r240, 2
  %R205 = bitcast <2 x double> %r243 to i128
  br label %block_402012
block_402012:
  %R213 = phi i128 [ %R205, %block_402007 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R212 = phi i64 [ undef, %block_402007 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R211 = phi i64 [ undef, %block_402007 ]
  %R210 = phi i64 [ %R204, %block_402007 ]
  %R209 = phi i64 [ %R189, %block_402007 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R208 = phi i64 [ undef, %block_402007 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R207 = phi i64 [ undef, %block_402007 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R206 = phi i64 [ undef, %block_402007 ]
  ; # 402012: mov    ebp,eax
  ; r214 := (trunc r206 32)
  %R214 = trunc i64 %R206 to i32
  ; r215 := (uext r214 64)
  %R215 = zext i32 %R214 to i64
  br label %block_402014
block_402014:
  %R223 = phi i128 [ %R183, %block_401ffa ], [ %R213, %block_402012 ]
  %R222 = phi i64 [ %R181, %block_401ffa ], [ %R212, %block_402012 ]
  %R221 = phi i64 [ %R180, %block_401ffa ], [ %R211, %block_402012 ]
  %R220 = phi i64 [ %R179, %block_401ffa ], [ %R210, %block_402012 ]
  %R219 = phi i64 [ %R178, %block_401ffa ], [ %R215, %block_402012 ]
  %R218 = phi i64 [ %R177, %block_401ffa ], [ %R209, %block_402012 ]
  %R217 = phi i64 [ %R176, %block_401ffa ], [ %R208, %block_402012 ]
  %R216 = phi i64 [ %R184, %block_401ffa ], [ %R207, %block_402012 ]
  ; # 402014: mov    rdi,rbx
  ; # 402017: call   402a80
  %r263 = bitcast i128 %R223 to <2 x double>
  %r264 = call { i64, i64, <2 x double> } @F402a80(i64 %R218, i64 %R220, i64 %R217, i64 %R216, i64 %R221, i64 %R222, <2 x double> %r263)
  %R224 = extractvalue { i64, i64, <2 x double> } %r264, 0
  %R225 = extractvalue { i64, i64, <2 x double> } %r264, 1
  %r267 = extractvalue { i64, i64, <2 x double> } %r264, 2
  %R226 = bitcast <2 x double> %r267 to i128
  br label %block_40201c
block_40201c:
  %R229 = phi i128 [ %R226, %block_402014 ]
  %R228 = phi i64 [ %R219, %block_402014 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R227 = phi i64 [ undef, %block_402014 ]
  ; # 40201c: mov    eax,ebp
  ; r230 := (trunc r228 32)
  %R230 = trunc i64 %R228 to i32
  ; r231 := (uext r230 64)
  %R231 = zext i32 %R230 to i64
  br label %block_40201e
block_40201e:
  %R234 = phi i128 [ %R112, %block_401fc9 ], [ %R229, %block_40201c ]
  %R233 = phi i64 [ %R109, %block_401fc9 ], [ %R227, %block_40201c ]
  %R232 = phi i64 [ %R108, %block_401fc9 ], [ %R231, %block_40201c ]
  ; # 40201e: pop    rbx
  ; # 40201f: pop    rbp
  ; # 402020: pop    r12
  ; # 402022: ret
  %r277 = bitcast i128 %R234 to <2 x double>
  %r278 = insertvalue { i64, i64, <2 x double> } undef, i64 %R232, 0
  %r279 = insertvalue { i64, i64, <2 x double> } %r278, i64 %R233, 1
  %r280 = insertvalue { i64, i64, <2 x double> } %r279, <2 x double> %r277, 2
  ret { i64, i64, <2 x double> } %r280
failure:
  br label %failure
}