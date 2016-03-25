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
declare { i64, i64, <2 x double> } @F4021a0(i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402681(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4026b0(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F40284f(i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4029ad(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F401ee8(i64 %a0, i64 %a1, i64 %a2, i64 %a3, <2 x double> %a4) {
entry:
  %r0 = bitcast <2 x double> %a4 to i128
  br label %block_401ee8
block_401ee8:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 401ee8: push   r13
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401eea: push   r12
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 401eec: push   rbp
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 401eed: mov    rbp,rsi
  ; # 401ef0: push   rbx
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 401ef1: push   rcx
  ; r6 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R6 = add i64 %R1, 18446744073709551576
  ; *(r6) = arg3
  %r9 = inttoptr i64 %R6 to i64*
  store i64 %a3, i64* %r9
  ; # 401ef2: movsx  esi,BYTE PTR [rsi]
  ; r7 := *arg1
  %r10 = inttoptr i64 %a1 to i8*
  %R7 = load i8* %r10
  ; r8 := (sext r7 32)
  %R8 = sext i8 %R7 to i32
  ; r9 := (uext r8 64)
  %R9 = zext i32 %R8 to i64
  ; # 401ef5: mov    rbx,rdi
  ; # 401ef8: mov    edi,0x402f82
  ; # 401efd: call   4021a0
  ; r10 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R10 = add i64 %R1, 18446744073709551568
  ; r14 := (bv_add r10 0x8 :: [64])
  %R14 = add i64 %R10, 8
  %r16 = bitcast i128 %r0 to <2 x double>
  %r17 = call { i64, i64, <2 x double> } @F4021a0(i64 4206466, i64 %R9, <2 x double> %r16)
  %R11 = extractvalue { i64, i64, <2 x double> } %r17, 0
  %R12 = extractvalue { i64, i64, <2 x double> } %r17, 1
  %r20 = extractvalue { i64, i64, <2 x double> } %r17, 2
  %R13 = bitcast <2 x double> %r20 to i128
  br label %block_401f02
block_401f02:
  %R23 = phi i128 [ %R13, %block_401ee8 ]
  %R22 = phi i64 [ %R11, %block_401ee8 ]
  %R21 = phi i64 [ %R12, %block_401ee8 ]
  %R20 = phi i64 [ %a1, %block_401ee8 ]
  %R19 = phi i64 [ %R14, %block_401ee8 ]
  %R18 = phi i64 [ %a0, %block_401ee8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R17 = phi i64 [ undef, %block_401ee8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R16 = phi i64 [ undef, %block_401ee8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R15 = phi i64 [ undef, %block_401ee8 ]
  ; # 401f02: test   rax,rax
  ; r24 := (bv_eq r15 0x0 :: [64])
  %R24 = icmp eq i64 %R15, 0
  ; # 401f05: jne    401f16
  ; r25 := (bv_complement r24)
  %R25 = xor i1 %R24, -1
  br i1 %R25, label %subblock_401f02_1, label %subblock_401f02_2
subblock_401f02_1:
  br label %block_401f16
subblock_401f02_2:
  br label %block_401f07
block_401f07:
  %R30 = phi i128 [ %R23, %subblock_401f02_2 ]
  %R29 = phi i64 [ %R22, %subblock_401f02_2 ]
  %R28 = phi i64 [ %R21, %subblock_401f02_2 ]
  %R27 = phi i64 [ %R19, %subblock_401f02_2 ]
  %R26 = phi i64 [ %R17, %subblock_401f02_2 ]
  ; # 401f07: call   402681
  ; r31 := (bv_add r27 0xfffffffffffffff8 :: [64])
  %R31 = add i64 %R27, 18446744073709551608
  ; r35 := (bv_add r31 0x8 :: [64])
  %R35 = add i64 %R31, 8
  %r40 = bitcast i128 %R30 to <2 x double>
  %r41 = call { i64, i64, <2 x double> } @F402681(i64 %R29, i64 %R28, i64 %R26, <2 x double> %r40)
  %R32 = extractvalue { i64, i64, <2 x double> } %r41, 0
  %R33 = extractvalue { i64, i64, <2 x double> } %r41, 1
  %r44 = extractvalue { i64, i64, <2 x double> } %r41, 2
  %R34 = bitcast <2 x double> %r44 to i128
  br label %block_401f0c
block_401f0c:
  %R38 = phi i128 [ %R34, %block_401f07 ]
  %R37 = phi i64 [ %R35, %block_401f07 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R36 = phi i64 [ undef, %block_401f07 ]
  ; # 401f0c: xor    esi,esi
  ; # 401f0e: mov    DWORD PTR [rax],0x16
  ; *(r36) = 0x16 :: [32]
  %r49 = inttoptr i64 %R36 to i32*
  store i32 22, i32* %r49
  ; # 401f14: jmp    401f7f
  br label %block_401f7f
block_401f16:
  %R45 = phi i128 [ %R23, %subblock_401f02_1 ]
  %R44 = phi i64 [ %R21, %subblock_401f02_1 ]
  %R43 = phi i64 [ %R20, %subblock_401f02_1 ]
  %R42 = phi i64 [ %R19, %subblock_401f02_1 ]
  %R41 = phi i64 [ %R18, %subblock_401f02_1 ]
  %R40 = phi i64 [ %R17, %subblock_401f02_1 ]
  %R39 = phi i64 [ %R16, %subblock_401f02_1 ]
  ; # 401f16: mov    rdi,rbp
  ; # 401f19: mov    r12d,0x2
  ; # 401f1f: call   4029ad
  ; r46 := (bv_add r42 0xfffffffffffffff8 :: [64])
  %R46 = add i64 %R42, 18446744073709551608
  ; r48 := (bv_add r46 0x8 :: [64])
  %R48 = add i64 %R46, 8
  %r59 = bitcast i128 %R45 to <2 x double>
  %r60 = call { i64, i64, <2 x double> } @F4029ad(i64 %R43, i64 %R44, i64 %R40, i64 %R39, <2 x double> %r59)
  %R47 = extractvalue { i64, i64, <2 x double> } %r60, 0
  br label %block_401f24
block_401f24:
  %R53 = phi i64 [ 2, %block_401f16 ]
  %R52 = phi i64 [ %R43, %block_401f16 ]
  %R51 = phi i64 [ %R48, %block_401f16 ]
  %R50 = phi i64 [ %R41, %block_401f16 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R49 = phi i64 [ undef, %block_401f16 ]
  ; # 401f24: movsxd rsi,eax
  ; r54 := (trunc r49 32)
  %R54 = trunc i64 %R49 to i32
  ; r55 := (sext r54 64)
  %R55 = sext i32 %R54 to i64
  ; # 401f27: mov    edx,0x1b6
  ; # 401f2c: mov    rax,r12
  ; # 401f2f: mov    r13,rsi
  ; # 401f32: mov    rdi,rbx
  ; # 401f35: syscall
  ; sys_open
  %r69 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R50, i64 %R55, i64 438, i64 undef, i64 undef, i64 undef, i64 2)
  %R56 = extractvalue { i64, i1 } %r69, 0
  br label %block_401f37
block_401f37:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R64 = phi i128 [ undef, %block_401f24 ]
  %R63 = phi i64 [ %R55, %block_401f24 ]
  %R62 = phi i64 [ %R53, %block_401f24 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R61 = phi i64 [ undef, %block_401f24 ]
  %R60 = phi i64 [ %R52, %block_401f24 ]
  %R59 = phi i64 [ %R51, %block_401f24 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R58 = phi i64 [ undef, %block_401f24 ]
  %R57 = phi i64 [ %R56, %block_401f24 ]
  ; # 401f37: mov    rdi,rax
  ; # 401f3a: call   4026b0
  ; r65 := (bv_add r59 0xfffffffffffffff8 :: [64])
  %R65 = add i64 %R59, 18446744073709551608
  ; r69 := (bv_add r65 0x8 :: [64])
  %R69 = add i64 %R65, 8
  %r81 = bitcast i128 %R64 to <2 x double>
  %r82 = call { i64, i64, <2 x double> } @F4026b0(i64 %R57, i64 %R61, i64 %R58, <2 x double> %r81)
  %R66 = extractvalue { i64, i64, <2 x double> } %r82, 0
  %R67 = extractvalue { i64, i64, <2 x double> } %r82, 1
  %r85 = extractvalue { i64, i64, <2 x double> } %r82, 2
  %R68 = bitcast <2 x double> %r85 to i128
  br label %block_401f3f
block_401f3f:
  %R75 = phi i128 [ %R68, %block_401f37 ]
  %R74 = phi i64 [ %R63, %block_401f37 ]
  %R73 = phi i64 [ %R62, %block_401f37 ]
  %R72 = phi i64 [ %R60, %block_401f37 ]
  %R71 = phi i64 [ %R69, %block_401f37 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R70 = phi i64 [ undef, %block_401f37 ]
  ; # 401f3f: xor    esi,esi
  ; # 401f41: test   eax,eax
  ; r76 := (trunc r70 32)
  %R76 = trunc i64 %R70 to i32
  ; r77 := (bv_slt r76 0x0 :: [32])
  %R77 = icmp slt i32 %R76, 0
  ; # 401f43: mov    rbx,rax
  ; # 401f46: js     401f7f
  br i1 %R77, label %subblock_401f3f_1, label %subblock_401f3f_2
subblock_401f3f_1:
  br label %block_401f7f
subblock_401f3f_2:
  br label %block_401f48
block_401f48:
  %R84 = phi i128 [ %R75, %subblock_401f3f_2 ]
  %R83 = phi i64 [ %R74, %subblock_401f3f_2 ]
  %R82 = phi i64 [ %R73, %subblock_401f3f_2 ]
  %R81 = phi i64 [ %R72, %subblock_401f3f_2 ]
  %R80 = phi i64 [ %R71, %subblock_401f3f_2 ]
  %R79 = phi i64 [ %R70, %subblock_401f3f_2 ]
  %R78 = phi i64 [ %R70, %subblock_401f3f_2 ]
  ; # 401f48: and    r13d,0x80000
  ; r85 := (trunc r83 32)
  %R85 = trunc i64 %R83 to i32
  ; r86 := (bv_and r85 0x80000 :: [32])
  %R86 = and i32 %R85, 524288
  ; r87 := (bv_eq r86 0x0 :: [32])
  %R87 = icmp eq i32 %R86, 0
  ; # 401f4f: je     401f63
  br i1 %R87, label %subblock_401f48_1, label %subblock_401f48_2
subblock_401f48_1:
  br label %block_401f63
subblock_401f48_2:
  br label %block_401f51
block_401f51:
  %R92 = phi i64 [ %R82, %subblock_401f48_2 ]
  %R91 = phi i64 [ %R81, %subblock_401f48_2 ]
  %R90 = phi i64 [ %R80, %subblock_401f48_2 ]
  %R89 = phi i64 [ %R79, %subblock_401f48_2 ]
  %R88 = phi i64 [ %R78, %subblock_401f48_2 ]
  ; # 401f51: movsxd rdi,eax
  ; r93 := (trunc r88 32)
  %R93 = trunc i64 %R88 to i32
  ; r94 := (sext r93 64)
  %R94 = sext i32 %R93 to i64
  ; # 401f54: mov    edx,0x1
  ; # 401f59: mov    eax,0x48
  ; # 401f5e: mov    rsi,r12
  ; # 401f61: syscall
  ; sys_fcntl
  %r112 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R94, i64 %R92, i64 1, i64 undef, i64 undef, i64 undef, i64 72)
  %R95 = extractvalue { i64, i1 } %r112, 0
  br label %block_401f63
block_401f63:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R99 = phi i128 [ undef, %block_401f51 ], [ %R84, %subblock_401f48_1 ]
  %R98 = phi i64 [ %R91, %block_401f51 ], [ %R81, %subblock_401f48_1 ]
  %R97 = phi i64 [ %R90, %block_401f51 ], [ %R80, %subblock_401f48_1 ]
  %R96 = phi i64 [ %R89, %block_401f51 ], [ %R79, %subblock_401f48_1 ]
  ; # 401f63: mov    rsi,rbp
  ; # 401f66: mov    edi,ebx
  ; r100 := (trunc r96 32)
  %R100 = trunc i64 %R96 to i32
  ; r101 := (uext r100 64)
  %R101 = zext i32 %R100 to i64
  ; # 401f68: call   40284f
  ; r102 := (bv_add r97 0xfffffffffffffff8 :: [64])
  %R102 = add i64 %R97, 18446744073709551608
  ; r105 := (bv_add r102 0x8 :: [64])
  %R105 = add i64 %R102, 8
  %r122 = bitcast i128 %R99 to <2 x double>
  %r123 = call { i64, i64, <2 x double> } @F40284f(i64 %R101, i64 %R98, <2 x double> %r122)
  %R103 = extractvalue { i64, i64, <2 x double> } %r123, 0
  %r125 = extractvalue { i64, i64, <2 x double> } %r123, 2
  %R104 = bitcast <2 x double> %r125 to i128
  br label %block_401f6d
block_401f6d:
  %R109 = phi i128 [ %R104, %block_401f63 ]
  %R108 = phi i64 [ %R105, %block_401f63 ]
  %R107 = phi i64 [ %R96, %block_401f63 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R106 = phi i64 [ undef, %block_401f63 ]
  ; # 401f6d: test   rax,rax
  ; r110 := (bv_eq r106 0x0 :: [64])
  %R110 = icmp eq i64 %R106, 0
  ; # 401f70: mov    rsi,rax
  ; # 401f73: jne    401f7f
  ; r111 := (bv_complement r110)
  %R111 = xor i1 %R110, -1
  br i1 %R111, label %subblock_401f6d_1, label %subblock_401f6d_2
subblock_401f6d_1:
  br label %block_401f7f
subblock_401f6d_2:
  br label %block_401f75
block_401f75:
  %R113 = phi i64 [ %R108, %subblock_401f6d_2 ]
  %R112 = phi i64 [ %R107, %subblock_401f6d_2 ]
  ; # 401f75: movsxd rdi,ebx
  ; r114 := (trunc r112 32)
  %R114 = trunc i64 %R112 to i32
  ; r115 := (sext r114 64)
  %R115 = sext i32 %R114 to i64
  ; # 401f78: mov    eax,0x3
  ; # 401f7d: syscall
  ; sys_close
  %r137 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R115, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 3)
  %R116 = extractvalue { i64, i1 } %r137, 0
  br label %block_401f7f
block_401f7f:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R119 = phi i128 [ %R38, %block_401f0c ], [ undef, %block_401f75 ], [ %R109, %subblock_401f6d_1 ], [ %R75, %subblock_401f3f_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R118 = phi i64 [ 0, %block_401f0c ], [ undef, %block_401f75 ], [ %R106, %subblock_401f6d_1 ], [ 0, %subblock_401f3f_1 ]
  %R117 = phi i64 [ %R37, %block_401f0c ], [ %R113, %block_401f75 ], [ %R108, %subblock_401f6d_1 ], [ %R71, %subblock_401f3f_1 ]
  ; # 401f7f: pop    rdx
  ; r120 := *r117
  %r142 = inttoptr i64 %R117 to i64*
  %R120 = load i64* %r142
  ; # 401f80: mov    rax,rsi
  ; # 401f83: pop    rbx
  ; # 401f84: pop    rbp
  ; # 401f85: pop    r12
  ; # 401f87: pop    r13
  ; # 401f89: ret
  %r144 = bitcast i128 %R119 to <2 x double>
  %r145 = insertvalue { i64, i64, <2 x double> } undef, i64 %R118, 0
  %r146 = insertvalue { i64, i64, <2 x double> } %r145, i64 %R120, 1
  %r147 = insertvalue { i64, i64, <2 x double> } %r146, <2 x double> %r144, 2
  ret { i64, i64, <2 x double> } %r147
failure:
  br label %failure
}