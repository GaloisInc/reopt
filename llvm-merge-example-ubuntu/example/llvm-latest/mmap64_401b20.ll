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
declare { i64, i64, <2 x double> } @F401b1f(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F402681(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4026b0(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F401b20(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_401b20
block_401b20:
  ; r0 := (alloca 0x40 :: [64])
  %r1 = alloca i8, i64 64
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x40 :: [64])
  %R1 = add i64 %R0, 64
  ; # 401b20: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401b21: push   rbx
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 401b22: sub    rsp,0x28
  ; r4 := (bv_add r1 0xffffffffffffffc8 :: [64])
  %R4 = add i64 %R1, 18446744073709551560
  ; # 401b26: test   r9d,0xfff
  ; r5 := (trunc arg5 32)
  %R5 = trunc i64 %a5 to i32
  ; r6 := (bv_and r5 0xfff :: [32])
  %R6 = and i32 %R5, 4095
  ; r7 := (bv_eq r6 0x0 :: [32])
  %R7 = icmp eq i32 %R6, 0
  ; # 401b2d: je     401b3c
  br i1 %R7, label %subblock_401b20_1, label %subblock_401b20_2
subblock_401b20_1:
  br label %block_401b3c
subblock_401b20_2:
  br label %block_401b2f
block_401b2f:
  %R11 = phi i128 [ %r0, %subblock_401b20_2 ]
  %R10 = phi i64 [ %a0, %subblock_401b20_2 ]
  %R9 = phi i64 [ %a1, %subblock_401b20_2 ]
  %R8 = phi i64 [ %a2, %subblock_401b20_2 ]
  ; # 401b2f: call   402681
  %r14 = bitcast i128 %R11 to <2 x double>
  %r15 = call { i64, i64, <2 x double> } @F402681(i64 %R10, i64 %R9, i64 %R8, <2 x double> %r14)
  %R12 = extractvalue { i64, i64, <2 x double> } %r15, 0
  %R13 = extractvalue { i64, i64, <2 x double> } %r15, 1
  %r18 = extractvalue { i64, i64, <2 x double> } %r15, 2
  %R14 = bitcast <2 x double> %r18 to i128
  br label %block_401b34
block_401b34:
  %R17 = phi i128 [ %R14, %block_401b2f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R16 = phi i64 [ undef, %block_401b2f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R15 = phi i64 [ undef, %block_401b2f ]
  ; # 401b34: mov    DWORD PTR [rax],0x16
  ; *(r15) = 0x16 :: [32]
  %r23 = inttoptr i64 %R15 to i32*
  store i32 22, i32* %r23
  ; # 401b3a: jmp    401baa
  br label %block_401baa
block_401b3c:
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm7
  %R32 = phi i128 [ undef, %subblock_401b20_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm6
  %R31 = phi i128 [ undef, %subblock_401b20_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm5
  %R30 = phi i128 [ undef, %subblock_401b20_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm4
  %R29 = phi i128 [ undef, %subblock_401b20_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm3
  %R28 = phi i128 [ undef, %subblock_401b20_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm2
  %R27 = phi i128 [ undef, %subblock_401b20_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm1
  %R26 = phi i128 [ undef, %subblock_401b20_1 ]
  %R25 = phi i128 [ %r0, %subblock_401b20_1 ]
  %R24 = phi i64 [ %a5, %subblock_401b20_1 ]
  %R23 = phi i64 [ %a4, %subblock_401b20_1 ]
  %R22 = phi i64 [ %a0, %subblock_401b20_1 ]
  %R21 = phi i64 [ %a1, %subblock_401b20_1 ]
  %R20 = phi i64 [ %R4, %subblock_401b20_1 ]
  %R19 = phi i64 [ %a2, %subblock_401b20_1 ]
  %R18 = phi i64 [ %a3, %subblock_401b20_1 ]
  ; # 401b3c: mov    rax,0x7ffffffffffffffe
  ; # 401b46: cmp    rsi,rax
  ; # 401b49: jbe    401b58
  ; r33 := (bv_ule r21 0x7ffffffffffffffe :: [64])
  %R33 = icmp ule i64 %R21, 9223372036854775806
  br i1 %R33, label %subblock_401b3c_1, label %subblock_401b3c_2
subblock_401b3c_1:
  br label %block_401b58
subblock_401b3c_2:
  br label %block_401b4b
block_401b4b:
  %R37 = phi i128 [ %R25, %subblock_401b3c_2 ]
  %R36 = phi i64 [ %R22, %subblock_401b3c_2 ]
  %R35 = phi i64 [ %R21, %subblock_401b3c_2 ]
  %R34 = phi i64 [ %R19, %subblock_401b3c_2 ]
  ; # 401b4b: call   402681
  %r44 = bitcast i128 %R37 to <2 x double>
  %r45 = call { i64, i64, <2 x double> } @F402681(i64 %R36, i64 %R35, i64 %R34, <2 x double> %r44)
  %R38 = extractvalue { i64, i64, <2 x double> } %r45, 0
  %R39 = extractvalue { i64, i64, <2 x double> } %r45, 1
  %r48 = extractvalue { i64, i64, <2 x double> } %r45, 2
  %R40 = bitcast <2 x double> %r48 to i128
  br label %block_401b50
block_401b50:
  %R43 = phi i128 [ %R40, %block_401b4b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R42 = phi i64 [ undef, %block_401b4b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R41 = phi i64 [ undef, %block_401b4b ]
  ; # 401b50: mov    DWORD PTR [rax],0xc
  ; *(r41) = 0xc :: [32]
  %r53 = inttoptr i64 %R41 to i32*
  store i32 12, i32* %r53
  ; # 401b56: jmp    401baa
  br label %block_401baa
block_401b58:
  %R58 = phi i128 [ %R32, %subblock_401b3c_1 ]
  %R57 = phi i128 [ %R31, %subblock_401b3c_1 ]
  %R56 = phi i128 [ %R30, %subblock_401b3c_1 ]
  %R55 = phi i128 [ %R29, %subblock_401b3c_1 ]
  %R54 = phi i128 [ %R28, %subblock_401b3c_1 ]
  %R53 = phi i128 [ %R27, %subblock_401b3c_1 ]
  %R52 = phi i128 [ %R26, %subblock_401b3c_1 ]
  %R51 = phi i128 [ %R25, %subblock_401b3c_1 ]
  %R50 = phi i64 [ %R24, %subblock_401b3c_1 ]
  %R49 = phi i64 [ %R23, %subblock_401b3c_1 ]
  %R48 = phi i64 [ %R22, %subblock_401b3c_1 ]
  %R47 = phi i64 [ %R21, %subblock_401b3c_1 ]
  %R46 = phi i64 [ %R20, %subblock_401b3c_1 ]
  %R45 = phi i64 [ %R19, %subblock_401b3c_1 ]
  %R44 = phi i64 [ %R18, %subblock_401b3c_1 ]
  ; # 401b58: test   cl,0x10
  ; r59 := (trunc r44 8)
  %R59 = trunc i64 %R44 to i8
  ; r60 := (bv_and r59 0x10 :: [8])
  %R60 = and i8 %R59, 16
  ; r61 := (bv_eq r60 0x0 :: [8])
  %R61 = icmp eq i8 %R60, 0
  ; # 401b5b: mov    ebx,edx
  ; r62 := (trunc r45 32)
  %R62 = trunc i64 %R45 to i32
  ; r63 := (uext r62 64)
  %R63 = zext i32 %R62 to i64
  ; # 401b5d: mov    ebp,ecx
  ; r64 := (trunc r44 32)
  %R64 = trunc i64 %R44 to i32
  ; r65 := (uext r64 64)
  %R65 = zext i32 %R64 to i64
  ; # 401b5f: je     401b8c
  br i1 %R61, label %subblock_401b58_1, label %subblock_401b58_2
subblock_401b58_1:
  br label %block_401b8c
subblock_401b58_2:
  br label %block_401b61
block_401b61:
  %R82 = phi i128 [ %R58, %subblock_401b58_2 ]
  %R81 = phi i128 [ %R57, %subblock_401b58_2 ]
  %R80 = phi i128 [ %R56, %subblock_401b58_2 ]
  %R79 = phi i128 [ %R55, %subblock_401b58_2 ]
  %R78 = phi i128 [ %R54, %subblock_401b58_2 ]
  %R77 = phi i128 [ %R53, %subblock_401b58_2 ]
  %R76 = phi i128 [ %R52, %subblock_401b58_2 ]
  %R75 = phi i128 [ %R51, %subblock_401b58_2 ]
  %R74 = phi i64 [ %R50, %subblock_401b58_2 ]
  %R73 = phi i64 [ %R49, %subblock_401b58_2 ]
  %R72 = phi i64 [ %R48, %subblock_401b58_2 ]
  %R71 = phi i64 [ %R47, %subblock_401b58_2 ]
  %R70 = phi i64 [ %R65, %subblock_401b58_2 ]
  %R69 = phi i64 [ %R46, %subblock_401b58_2 ]
  %R68 = phi i64 [ %R63, %subblock_401b58_2 ]
  %R67 = phi i64 [ %R45, %subblock_401b58_2 ]
  %R66 = phi i64 [ %R44, %subblock_401b58_2 ]
  ; # 401b61: mov    QWORD PTR [rsp+0x18],r9
  ; r83 := (bv_add r69 0x18 :: [64])
  %R83 = add i64 %R69, 24
  ; *(r83) = r74
  %r94 = inttoptr i64 %R83 to i64*
  store i64 %R74, i64* %r94
  ; # 401b66: mov    DWORD PTR [rsp+0x14],r8d
  ; r84 := (trunc r73 32)
  %R84 = trunc i64 %R73 to i32
  ; r85 := (bv_add r69 0x14 :: [64])
  %R85 = add i64 %R69, 20
  ; *(r85) = r84
  %r97 = inttoptr i64 %R85 to i32*
  store i32 %R84, i32* %r97
  ; # 401b6b: mov    QWORD PTR [rsp+0x8],rsi
  ; r86 := (bv_add r69 0x8 :: [64])
  %R86 = add i64 %R69, 8
  ; *(r86) = r71
  %r99 = inttoptr i64 %R86 to i64*
  store i64 %R71, i64* %r99
  ; # 401b70: mov    QWORD PTR [rsp],rdi
  ; *(r69) = r72
  %r100 = inttoptr i64 %R69 to i64*
  store i64 %R72, i64* %r100
  ; # 401b74: call   401b1f
  ; r87 := (bv_add r69 0xfffffffffffffff8 :: [64])
  %R87 = add i64 %R69, 18446744073709551608
  ; r91 := (bv_add r87 0x8 :: [64])
  %R91 = add i64 %R87, 8
  %r103 = bitcast i128 %R75 to <2 x double>
  %r104 = bitcast i128 %R76 to <2 x double>
  %r105 = bitcast i128 %R77 to <2 x double>
  %r106 = bitcast i128 %R78 to <2 x double>
  %r107 = bitcast i128 %R79 to <2 x double>
  %r108 = bitcast i128 %R80 to <2 x double>
  %r109 = bitcast i128 %R81 to <2 x double>
  %r110 = bitcast i128 %R82 to <2 x double>
  %r111 = call { i64, i64, <2 x double> } @F401b1f(i64 %R72, i64 %R71, i64 %R67, i64 %R66, i64 %R73, i64 %R74, <2 x double> %r103, <2 x double> %r104, <2 x double> %r105, <2 x double> %r106, <2 x double> %r107, <2 x double> %r108, <2 x double> %r109, <2 x double> %r110)
  %R88 = extractvalue { i64, i64, <2 x double> } %r111, 0
  %R89 = extractvalue { i64, i64, <2 x double> } %r111, 1
  %r114 = extractvalue { i64, i64, <2 x double> } %r111, 2
  %R90 = bitcast <2 x double> %r114 to i128
  br label %block_401b79
block_401b79:
  %R94 = phi i64 [ %R70, %block_401b61 ]
  %R93 = phi i64 [ %R91, %block_401b61 ]
  %R92 = phi i64 [ %R68, %block_401b61 ]
  ; # 401b79: mov    r9,QWORD PTR [rsp+0x18]
  ; r95 := (bv_add r93 0x18 :: [64])
  %R95 = add i64 %R93, 24
  ; r96 := *r95
  %r120 = inttoptr i64 %R95 to i64*
  %R96 = load i64* %r120
  ; # 401b7e: mov    r8d,DWORD PTR [rsp+0x14]
  ; r97 := (bv_add r93 0x14 :: [64])
  %R97 = add i64 %R93, 20
  ; r98 := *r97
  %r123 = inttoptr i64 %R97 to i32*
  %R98 = load i32* %r123
  ; r99 := (uext r98 64)
  %R99 = zext i32 %R98 to i64
  ; # 401b83: mov    rsi,QWORD PTR [rsp+0x8]
  ; r100 := (bv_add r93 0x8 :: [64])
  %R100 = add i64 %R93, 8
  ; r101 := *r100
  %r127 = inttoptr i64 %R100 to i64*
  %R101 = load i64* %r127
  ; # 401b88: mov    rdi,QWORD PTR [rsp]
  ; r102 := *r93
  %r129 = inttoptr i64 %R93 to i64*
  %R102 = load i64* %r129
  br label %block_401b8c
block_401b8c:
  %R108 = phi i64 [ %R96, %block_401b79 ], [ %R50, %subblock_401b58_1 ]
  %R107 = phi i64 [ %R99, %block_401b79 ], [ %R49, %subblock_401b58_1 ]
  %R106 = phi i64 [ %R102, %block_401b79 ], [ %R48, %subblock_401b58_1 ]
  %R105 = phi i64 [ %R101, %block_401b79 ], [ %R47, %subblock_401b58_1 ]
  %R104 = phi i64 [ %R94, %block_401b79 ], [ %R65, %subblock_401b58_1 ]
  %R103 = phi i64 [ %R92, %block_401b79 ], [ %R63, %subblock_401b58_1 ]
  ; # 401b8c: movsxd r10,ebp
  ; r109 := (trunc r104 32)
  %R109 = trunc i64 %R104 to i32
  ; r110 := (sext r109 64)
  %R110 = sext i32 %R109 to i64
  ; # 401b8f: movsxd r8,r8d
  ; r111 := (trunc r107 32)
  %R111 = trunc i64 %R107 to i32
  ; r112 := (sext r111 64)
  %R112 = sext i32 %R111 to i64
  ; # 401b92: movsxd rdx,ebx
  ; r113 := (trunc r103 32)
  %R113 = trunc i64 %R103 to i32
  ; r114 := (sext r113 64)
  %R114 = sext i32 %R113 to i64
  ; # 401b95: mov    eax,0x9
  ; # 401b9a: syscall
  ; sys_mmap
  %r143 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R106, i64 %R105, i64 %R114, i64 %R110, i64 %R112, i64 %R108, i64 9)
  %R115 = extractvalue { i64, i1 } %r143, 0
  br label %block_401b9c
block_401b9c:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R119 = phi i128 [ undef, %block_401b8c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R118 = phi i64 [ undef, %block_401b8c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R117 = phi i64 [ undef, %block_401b8c ]
  %R116 = phi i64 [ %R115, %block_401b8c ]
  ; # 401b9c: add    rsp,0x28
  ; # 401ba0: mov    rdi,rax
  ; # 401ba3: pop    rbx
  ; # 401ba4: pop    rbp
  ; # 401ba5: jmp    4026b0
  %r149 = bitcast i128 %R119 to <2 x double>
  %r150 = call { i64, i64, <2 x double> } @F4026b0(i64 %R116, i64 %R118, i64 %R117, <2 x double> %r149)
  ret { i64, i64, <2 x double> } %r150
block_401baa:
  %R124 = phi i128 [ %R17, %block_401b34 ], [ %R43, %block_401b50 ]
  %R123 = phi i64 [ %R16, %block_401b34 ], [ %R42, %block_401b50 ]
  ; # 401baa: add    rsp,0x28
  ; # 401bae: or     rax,0xffffffffffffffff
  ; # 401bb2: pop    rbx
  ; # 401bb3: pop    rbp
  ; # 401bb4: ret
  %r153 = bitcast i128 %R124 to <2 x double>
  %r154 = insertvalue { i64, i64, <2 x double> } undef, i64 18446744073709551615, 0
  %r155 = insertvalue { i64, i64, <2 x double> } %r154, i64 %R123, 1
  %r156 = insertvalue { i64, i64, <2 x double> } %r155, <2 x double> %r153, 2
  ret { i64, i64, <2 x double> } %r156
failure:
  br label %failure
}