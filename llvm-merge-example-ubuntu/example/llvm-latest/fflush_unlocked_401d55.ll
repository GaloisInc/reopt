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
define { i64, i64, <2 x double> } @F401d55(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_401d55
block_401d55:
  ; r0 := (alloca 0x10 :: [64])
  %r8 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 401d55: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401d56: mov    rax,QWORD PTR [rdi+0x38]
  ; r3 := (bv_add arg0 0x38 :: [64])
  %R3 = add i64 %a0, 56
  ; r4 := *r3
  %r13 = inttoptr i64 %R3 to i64*
  %R4 = load i64* %r13
  ; # 401d5a: mov    rbx,rdi
  ; # 401d5d: cmp    QWORD PTR [rdi+0x28],rax
  ; r5 := (bv_add arg0 0x28 :: [64])
  %R5 = add i64 %a0, 40
  ; r6 := *r5
  %r16 = inttoptr i64 %R5 to i64*
  %R6 = load i64* %r16
  ; # 401d61: ja     401d80
  ; r7 := (bv_ult r4 r6)
  %R7 = icmp ult i64 %R4, %R6
  br i1 %R7, label %subblock_401d55_1, label %subblock_401d55_2
subblock_401d55_1:
  br label %block_401d80
subblock_401d55_2:
  br label %block_401d63
block_401d63:
  %R20 = phi i128 [ %R79, %subblock_401d87_1 ], [ %r7, %subblock_401d55_2 ]
  %R19 = phi i128 [ %R78, %subblock_401d87_1 ], [ %r6, %subblock_401d55_2 ]
  %R18 = phi i128 [ %R77, %subblock_401d87_1 ], [ %r5, %subblock_401d55_2 ]
  %R17 = phi i128 [ %R76, %subblock_401d87_1 ], [ %r4, %subblock_401d55_2 ]
  %R16 = phi i128 [ %R75, %subblock_401d87_1 ], [ %r3, %subblock_401d55_2 ]
  %R15 = phi i128 [ %R74, %subblock_401d87_1 ], [ %r2, %subblock_401d55_2 ]
  %R14 = phi i128 [ %R73, %subblock_401d87_1 ], [ %r1, %subblock_401d55_2 ]
  %R13 = phi i128 [ %R72, %subblock_401d87_1 ], [ %r0, %subblock_401d55_2 ]
  %R12 = phi i64 [ %R71, %subblock_401d87_1 ], [ %a5, %subblock_401d55_2 ]
  %R11 = phi i64 [ %R70, %subblock_401d87_1 ], [ %a4, %subblock_401d55_2 ]
  %R10 = phi i64 [ %R69, %subblock_401d87_1 ], [ %a0, %subblock_401d55_2 ]
  %R9 = phi i64 [ %R68, %subblock_401d87_1 ], [ %a2, %subblock_401d55_2 ]
  %R8 = phi i64 [ %R67, %subblock_401d87_1 ], [ %a3, %subblock_401d55_2 ]
  ; # 401d63: mov    rsi,QWORD PTR [rbx+0x8]
  ; r21 := (bv_add r10 0x8 :: [64])
  %R21 = add i64 %R10, 8
  ; r22 := *r21
  %r33 = inttoptr i64 %R21 to i64*
  %R22 = load i64* %r33
  ; # 401d67: mov    rax,QWORD PTR [rbx+0x10]
  ; r23 := (bv_add r10 0x10 :: [64])
  %R23 = add i64 %R10, 16
  ; r24 := *r23
  %r36 = inttoptr i64 %R23 to i64*
  %R24 = load i64* %r36
  ; # 401d6b: cmp    rsi,rax
  ; # 401d6e: jae    401d93
  ; r25 := (bv_ule r24 r22)
  %R25 = icmp ule i64 %R24, %R22
  br i1 %R25, label %subblock_401d63_1, label %subblock_401d63_2
subblock_401d63_1:
  br label %block_401d93
subblock_401d63_2:
  br label %block_401d70
block_401d70:
  %R39 = phi i128 [ %R20, %subblock_401d63_2 ]
  %R38 = phi i128 [ %R19, %subblock_401d63_2 ]
  %R37 = phi i128 [ %R18, %subblock_401d63_2 ]
  %R36 = phi i128 [ %R17, %subblock_401d63_2 ]
  %R35 = phi i128 [ %R16, %subblock_401d63_2 ]
  %R34 = phi i128 [ %R15, %subblock_401d63_2 ]
  %R33 = phi i128 [ %R14, %subblock_401d63_2 ]
  %R32 = phi i128 [ %R13, %subblock_401d63_2 ]
  %R31 = phi i64 [ %R12, %subblock_401d63_2 ]
  %R30 = phi i64 [ %R11, %subblock_401d63_2 ]
  %R29 = phi i64 [ %R22, %subblock_401d63_2 ]
  %R28 = phi i64 [ %R10, %subblock_401d63_2 ]
  %R27 = phi i64 [ %R8, %subblock_401d63_2 ]
  %R26 = phi i64 [ %R24, %subblock_401d63_2 ]
  ; # 401d70: sub    rsi,rax
  ; r40 := (bv_sub r29 r26)
  %R40 = sub i64 %R29, %R26
  ; # 401d73: mov    edx,0x1
  ; # 401d78: mov    rdi,rbx
  ; # 401d7b: call   QWORD PTR [rbx+0x50]
  ; r41 := (bv_add r28 0x50 :: [64])
  %R41 = add i64 %R28, 80
  ; r42 := *r41
  %r55 = inttoptr i64 %R41 to i64*
  %R42 = load i64* %r55
  %r57 = inttoptr i64 %R42 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r58 = bitcast i128 %R32 to <2 x double>
  %r59 = bitcast i128 %R33 to <2 x double>
  %r60 = bitcast i128 %R34 to <2 x double>
  %r61 = bitcast i128 %R35 to <2 x double>
  %r62 = bitcast i128 %R36 to <2 x double>
  %r63 = bitcast i128 %R37 to <2 x double>
  %r64 = bitcast i128 %R38 to <2 x double>
  %r65 = bitcast i128 %R39 to <2 x double>
  %r66 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r57(i64 %R28, i64 %R40, i64 1, i64 %R27, i64 %R30, i64 %R31, <2 x double> %r58, <2 x double> %r59, <2 x double> %r60, <2 x double> %r61, <2 x double> %r62, <2 x double> %r63, <2 x double> %r64, <2 x double> %r65)
  %R43 = extractvalue { i64, i64, <2 x double> } %r66, 0
  %R44 = extractvalue { i64, i64, <2 x double> } %r66, 1
  %r69 = extractvalue { i64, i64, <2 x double> } %r66, 2
  %R45 = bitcast <2 x double> %r69 to i128
  br label %block_401d7e
block_401d7e:
  %R48 = phi i128 [ %R45, %block_401d70 ]
  %R47 = phi i64 [ %R28, %block_401d70 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R46 = phi i64 [ undef, %block_401d70 ]
  ; # 401d7e: jmp    401d93
  br label %block_401d93
block_401d80:
  %R61 = phi i128 [ %r7, %subblock_401d55_1 ]
  %R60 = phi i128 [ %r6, %subblock_401d55_1 ]
  %R59 = phi i128 [ %r5, %subblock_401d55_1 ]
  %R58 = phi i128 [ %r4, %subblock_401d55_1 ]
  %R57 = phi i128 [ %r3, %subblock_401d55_1 ]
  %R56 = phi i128 [ %r2, %subblock_401d55_1 ]
  %R55 = phi i128 [ %r1, %subblock_401d55_1 ]
  %R54 = phi i128 [ %r0, %subblock_401d55_1 ]
  %R53 = phi i64 [ %a5, %subblock_401d55_1 ]
  %R52 = phi i64 [ %a4, %subblock_401d55_1 ]
  %R51 = phi i64 [ %a0, %subblock_401d55_1 ]
  %R50 = phi i64 [ %a0, %subblock_401d55_1 ]
  %R49 = phi i64 [ %a3, %subblock_401d55_1 ]
  ; # 401d80: xor    edx,edx
  ; # 401d82: xor    esi,esi
  ; # 401d84: call   QWORD PTR [rdi+0x48]
  ; r62 := (bv_add r51 0x48 :: [64])
  %R62 = add i64 %R51, 72
  ; r63 := *r62
  %r88 = inttoptr i64 %R62 to i64*
  %R63 = load i64* %r88
  %r90 = inttoptr i64 %R63 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r91 = bitcast i128 %R54 to <2 x double>
  %r92 = bitcast i128 %R55 to <2 x double>
  %r93 = bitcast i128 %R56 to <2 x double>
  %r94 = bitcast i128 %R57 to <2 x double>
  %r95 = bitcast i128 %R58 to <2 x double>
  %r96 = bitcast i128 %R59 to <2 x double>
  %r97 = bitcast i128 %R60 to <2 x double>
  %r98 = bitcast i128 %R61 to <2 x double>
  %r99 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r90(i64 %R51, i64 0, i64 0, i64 %R49, i64 %R52, i64 %R53, <2 x double> %r91, <2 x double> %r92, <2 x double> %r93, <2 x double> %r94, <2 x double> %r95, <2 x double> %r96, <2 x double> %r97, <2 x double> %r98)
  %R64 = extractvalue { i64, i64, <2 x double> } %r99, 0
  %R65 = extractvalue { i64, i64, <2 x double> } %r99, 1
  %r102 = extractvalue { i64, i64, <2 x double> } %r99, 2
  %R66 = bitcast <2 x double> %r102 to i128
  br label %block_401d87
block_401d87:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R79 = phi i128 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R78 = phi i128 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R77 = phi i128 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R76 = phi i128 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R75 = phi i128 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R74 = phi i128 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R73 = phi i128 [ undef, %block_401d80 ]
  %R72 = phi i128 [ %R66, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R71 = phi i64 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R70 = phi i64 [ undef, %block_401d80 ]
  %R69 = phi i64 [ %R50, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R68 = phi i64 [ undef, %block_401d80 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R67 = phi i64 [ undef, %block_401d80 ]
  ; # 401d87: or     eax,0xffffffff
  ; # 401d8a: cmp    QWORD PTR [rbx+0x28],0x0
  ; r80 := (bv_add r69 0x28 :: [64])
  %R80 = add i64 %R69, 40
  ; r81 := *r80
  %r118 = inttoptr i64 %R80 to i64*
  %R81 = load i64* %r118
  ; r82 := (bv_eq r81 0x0 :: [64])
  %R82 = icmp eq i64 %R81, 0
  ; # 401d8f: jne    401d63
  ; r83 := (bv_complement r82)
  %R83 = xor i1 %R82, -1
  br i1 %R83, label %subblock_401d87_1, label %subblock_401d87_2
subblock_401d87_1:
  br label %block_401d63
subblock_401d87_2:
  br label %block_401d91
block_401d91:
  %R86 = phi i128 [ %R72, %subblock_401d87_2 ]
  %R85 = phi i64 [ %R68, %subblock_401d87_2 ]
  %R84 = phi i64 [ 4294967295, %subblock_401d87_2 ]
  ; # 401d91: jmp    401dbd
  br label %block_401dbd
block_401d93:
  %R89 = phi i128 [ %R48, %block_401d7e ], [ %R13, %subblock_401d63_1 ]
  %R88 = phi i64 [ %R47, %block_401d7e ], [ %R10, %subblock_401d63_1 ]
  %R87 = phi i64 [ %R46, %block_401d7e ], [ %R9, %subblock_401d63_1 ]
  ; # 401d93: mov    QWORD PTR [rbx+0x20],0x0
  ; r90 := (bv_add r88 0x20 :: [64])
  %R90 = add i64 %R88, 32
  ; *(r90) = 0x0 :: [64]
  %r129 = inttoptr i64 %R90 to i64*
  store i64 0, i64* %r129
  ; # 401d9b: mov    QWORD PTR [rbx+0x38],0x0
  ; r91 := (bv_add r88 0x38 :: [64])
  %R91 = add i64 %R88, 56
  ; *(r91) = 0x0 :: [64]
  %r131 = inttoptr i64 %R91 to i64*
  store i64 0, i64* %r131
  ; # 401da3: xor    eax,eax
  ; # 401da5: mov    QWORD PTR [rbx+0x28],0x0
  ; r92 := (bv_add r88 0x28 :: [64])
  %R92 = add i64 %R88, 40
  ; *(r92) = 0x0 :: [64]
  %r133 = inttoptr i64 %R92 to i64*
  store i64 0, i64* %r133
  ; # 401dad: mov    QWORD PTR [rbx+0x10],0x0
  ; r93 := (bv_add r88 0x10 :: [64])
  %R93 = add i64 %R88, 16
  ; *(r93) = 0x0 :: [64]
  %r135 = inttoptr i64 %R93 to i64*
  store i64 0, i64* %r135
  ; # 401db5: mov    QWORD PTR [rbx+0x8],0x0
  ; r94 := (bv_add r88 0x8 :: [64])
  %R94 = add i64 %R88, 8
  ; *(r94) = 0x0 :: [64]
  %r137 = inttoptr i64 %R94 to i64*
  store i64 0, i64* %r137
  br label %block_401dbd
block_401dbd:
  %R97 = phi i128 [ %R89, %block_401d93 ], [ %R86, %block_401d91 ]
  %R96 = phi i64 [ %R87, %block_401d93 ], [ %R85, %block_401d91 ]
  %R95 = phi i64 [ 0, %block_401d93 ], [ %R84, %block_401d91 ]
  ; # 401dbd: pop    rbx
  ; # 401dbe: ret
  %r141 = bitcast i128 %R97 to <2 x double>
  %r142 = insertvalue { i64, i64, <2 x double> } undef, i64 %R95, 0
  %r143 = insertvalue { i64, i64, <2 x double> } %r142, i64 %R96, 1
  %r144 = insertvalue { i64, i64, <2 x double> } %r143, <2 x double> %r141, 2
  ret { i64, i64, <2 x double> } %r144
failure:
  br label %failure
}