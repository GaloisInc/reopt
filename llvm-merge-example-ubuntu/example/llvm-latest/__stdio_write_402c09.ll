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
declare { i64, i64, <2 x double> } @F4026b0(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F402c09(i64 %a0, i64 %a1, i64 %a2) {
entry:
  br label %block_402c09
block_402c09:
  ; r0 := (alloca 0x60 :: [64])
  %r0 = alloca i8, i64 96
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x60 :: [64])
  %R1 = add i64 %R0, 96
  ; # 402c09: push   r15
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402c0b: push   r14
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 402c0d: mov    r15,rdx
  ; # 402c10: push   r13
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 402c12: push   r12
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 402c14: mov    r13d,0x2
  ; # 402c1a: push   rbp
  ; r6 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R6 = add i64 %R1, 18446744073709551576
  ; # 402c1b: push   rbx
  ; r7 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R7 = add i64 %R1, 18446744073709551568
  ; # 402c1c: mov    r14d,0x14
  ; # 402c22: mov    rbx,rdi
  ; # 402c25: sub    rsp,0x28
  ; r8 := (bv_add r1 0xffffffffffffffa8 :: [64])
  %R8 = add i64 %R1, 18446744073709551528
  ; # 402c29: mov    rax,QWORD PTR [rdi+0x38]
  ; r9 := (bv_add arg0 0x38 :: [64])
  %R9 = add i64 %a0, 56
  ; r10 := *r9
  %r11 = inttoptr i64 %R9 to i64*
  %R10 = load i64* %r11
  ; # 402c2d: mov    r12,QWORD PTR [rdi+0x28]
  ; r11 := (bv_add arg0 0x28 :: [64])
  %R11 = add i64 %a0, 40
  ; r12 := *r11
  %r14 = inttoptr i64 %R11 to i64*
  %R12 = load i64* %r14
  ; # 402c31: mov    QWORD PTR [rsp+0x10],rsi
  ; r13 := (bv_add r1 0xffffffffffffffb8 :: [64])
  %R13 = add i64 %R1, 18446744073709551544
  ; *(r13) = arg1
  %r17 = inttoptr i64 %R13 to i64*
  store i64 %a1, i64* %r17
  ; # 402c36: mov    QWORD PTR [rsp+0x18],rdx
  ; r14 := (bv_add r1 0xffffffffffffffc0 :: [64])
  %R14 = add i64 %R1, 18446744073709551552
  ; *(r14) = arg2
  %r19 = inttoptr i64 %R14 to i64*
  store i64 %a2, i64* %r19
  ; # 402c3b: mov    rbp,rsp
  ; # 402c3e: sub    r12,rax
  ; r15 := (bv_sub r12 r10)
  %R15 = sub i64 %R12, %R10
  ; # 402c41: mov    QWORD PTR [rsp],rax
  ; *(r8) = r10
  %r21 = inttoptr i64 %R8 to i64*
  store i64 %R10, i64* %r21
  ; # 402c45: mov    QWORD PTR [rsp+0x8],r12
  ; r16 := (bv_add r1 0xffffffffffffffb0 :: [64])
  %R16 = add i64 %R1, 18446744073709551536
  ; *(r16) = r15
  %r23 = inttoptr i64 %R16 to i64*
  store i64 %R15, i64* %r23
  ; # 402c4a: add    r12,rdx
  ; r17 := (bv_add r15 arg2)
  %R17 = add i64 %R15, %a2
  br label %block_402c4d
block_402c4d:
  %R21 = phi i64 [ %R94, %block_402ccc ], [ 2, %block_402c09 ]
  %R20 = phi i64 [ %R93, %block_402ccc ], [ %R17, %block_402c09 ]
  %R19 = phi i64 [ %R92, %block_402ccc ], [ %R8, %block_402c09 ]
  %R18 = phi i64 [ %R91, %block_402ccc ], [ %a0, %block_402c09 ]
  ; # 402c4d: movsxd rdi,DWORD PTR [rbx+0x78]
  ; r22 := (bv_add r18 0x78 :: [64])
  %R22 = add i64 %R18, 120
  ; r23 := *r22
  %r30 = inttoptr i64 %R22 to i32*
  %R23 = load i32* %r30
  ; r24 := (sext r23 64)
  %R24 = sext i32 %R23 to i64
  ; # 402c51: movsxd rdx,r13d
  ; r25 := (trunc r21 32)
  %R25 = trunc i64 %R21 to i32
  ; r26 := (sext r25 64)
  %R26 = sext i32 %R25 to i64
  ; # 402c54: mov    rax,r14
  ; # 402c57: mov    rsi,rbp
  ; # 402c5a: syscall
  ; sys_writev
  %r35 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R24, i64 %R19, i64 %R26, i64 undef, i64 undef, i64 undef, i64 20)
  %R27 = extractvalue { i64, i1 } %r35, 0
  br label %block_402c5c
block_402c5c:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R35 = phi i128 [ undef, %block_402c4d ]
  %R34 = phi i64 [ %R21, %block_402c4d ]
  %R33 = phi i64 [ %R20, %block_402c4d ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R32 = phi i64 [ undef, %block_402c4d ]
  %R31 = phi i64 [ %R19, %block_402c4d ]
  %R30 = phi i64 [ %R18, %block_402c4d ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R29 = phi i64 [ undef, %block_402c4d ]
  %R28 = phi i64 [ %R27, %block_402c4d ]
  ; # 402c5c: mov    rdi,rax
  ; # 402c5f: call   4026b0
  %r45 = bitcast i128 %R35 to <2 x double>
  %r46 = call { i64, i64, <2 x double> } @F4026b0(i64 %R28, i64 %R32, i64 %R29, <2 x double> %r45)
  %R36 = extractvalue { i64, i64, <2 x double> } %r46, 0
  %R37 = extractvalue { i64, i64, <2 x double> } %r46, 1
  %r49 = extractvalue { i64, i64, <2 x double> } %r46, 2
  %R38 = bitcast <2 x double> %r49 to i128
  br label %block_402c64
block_402c64:
  %R43 = phi i64 [ %R34, %block_402c5c ]
  %R42 = phi i64 [ %R33, %block_402c5c ]
  %R41 = phi i64 [ %R31, %block_402c5c ]
  %R40 = phi i64 [ %R30, %block_402c5c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R39 = phi i64 [ undef, %block_402c5c ]
  ; # 402c64: cmp    r12,rax
  ; r44 := (bv_eq r42 r39)
  %R44 = icmp eq i64 %R42, %R39
  ; # 402c67: jne    402c85
  ; r45 := (bv_complement r44)
  %R45 = xor i1 %R44, -1
  br i1 %R45, label %subblock_402c64_1, label %subblock_402c64_2
subblock_402c64_1:
  br label %block_402c85
subblock_402c64_2:
  br label %block_402c69
block_402c69:
  %R46 = phi i64 [ %R40, %subblock_402c64_2 ]
  ; # 402c69: mov    rax,QWORD PTR [rbx+0x58]
  ; r47 := (bv_add r46 0x58 :: [64])
  %R47 = add i64 %R46, 88
  ; r48 := *r47
  %r60 = inttoptr i64 %R47 to i64*
  %R48 = load i64* %r60
  ; # 402c6d: mov    rdx,rax
  ; # 402c70: add    rdx,QWORD PTR [rbx+0x60]
  ; r49 := (bv_add r46 0x60 :: [64])
  %R49 = add i64 %R46, 96
  ; r50 := *r49
  %r63 = inttoptr i64 %R49 to i64*
  %R50 = load i64* %r63
  ; r51 := (bv_add r48 r50)
  %R51 = add i64 %R48, %R50
  ; # 402c74: mov    QWORD PTR [rbx+0x38],rax
  ; r52 := (bv_add r46 0x38 :: [64])
  %R52 = add i64 %R46, 56
  ; *(r52) = r48
  %r67 = inttoptr i64 %R52 to i64*
  store i64 %R48, i64* %r67
  ; # 402c78: mov    QWORD PTR [rbx+0x28],rax
  ; r53 := (bv_add r46 0x28 :: [64])
  %R53 = add i64 %R46, 40
  ; *(r53) = r48
  %r69 = inttoptr i64 %R53 to i64*
  store i64 %R48, i64* %r69
  ; # 402c7c: mov    rax,r15
  ; # 402c7f: mov    QWORD PTR [rbx+0x20],rdx
  ; r54 := (bv_add r46 0x20 :: [64])
  %R54 = add i64 %R46, 32
  ; *(r54) = r51
  %r71 = inttoptr i64 %R54 to i64*
  store i64 %R51, i64* %r71
  ; # 402c83: jmp    402cd9
  br label %block_402cd9
block_402c85:
  %R59 = phi i64 [ %R43, %subblock_402c64_1 ]
  %R58 = phi i64 [ %R42, %subblock_402c64_1 ]
  %R57 = phi i64 [ %R41, %subblock_402c64_1 ]
  %R56 = phi i64 [ %R40, %subblock_402c64_1 ]
  %R55 = phi i64 [ %R39, %subblock_402c64_1 ]
  ; # 402c85: test   rax,rax
  ; # 402c88: jns    402cb6
  ; r60 := (bv_sle 0x0 :: [64] r55)
  %R60 = icmp sle i64 0, %R55
  br i1 %R60, label %subblock_402c85_1, label %subblock_402c85_2
subblock_402c85_1:
  br label %block_402cb6
subblock_402c85_2:
  br label %block_402c8a
block_402c8a:
  %R62 = phi i64 [ %R59, %subblock_402c85_2 ]
  %R61 = phi i64 [ %R56, %subblock_402c85_2 ]
  ; # 402c8a: or     DWORD PTR [rbx],0x20
  ; r63 := *r61
  %r80 = inttoptr i64 %R61 to i32*
  %R63 = load i32* %r80
  ; r64 := (bv_or r63 0x20 :: [32])
  %R64 = or i32 %R63, 32
  ; *(r61) = r64
  %r83 = inttoptr i64 %R61 to i32*
  store i32 %R64, i32* %r83
  ; # 402c8d: xor    eax,eax
  ; # 402c8f: cmp    r13d,0x2
  ; r65 := (trunc r62 32)
  %R65 = trunc i64 %R62 to i32
  ; r66 := (bv_eq r65 0x2 :: [32])
  %R66 = icmp eq i32 %R65, 2
  ; # 402c93: mov    QWORD PTR [rbx+0x20],0x0
  ; r67 := (bv_add r61 0x20 :: [64])
  %R67 = add i64 %R61, 32
  ; *(r67) = 0x0 :: [64]
  %r87 = inttoptr i64 %R67 to i64*
  store i64 0, i64* %r87
  ; # 402c9b: mov    QWORD PTR [rbx+0x38],0x0
  ; r68 := (bv_add r61 0x38 :: [64])
  %R68 = add i64 %R61, 56
  ; *(r68) = 0x0 :: [64]
  %r89 = inttoptr i64 %R68 to i64*
  store i64 0, i64* %r89
  ; # 402ca3: mov    QWORD PTR [rbx+0x28],0x0
  ; r69 := (bv_add r61 0x28 :: [64])
  %R69 = add i64 %R61, 40
  ; *(r69) = 0x0 :: [64]
  %r91 = inttoptr i64 %R69 to i64*
  store i64 0, i64* %r91
  ; # 402cab: je     402cd9
  br i1 %R66, label %subblock_402c8a_1, label %subblock_402c8a_2
subblock_402c8a_1:
  br label %block_402cd9
subblock_402c8a_2:
  br label %block_402cad
block_402cad:
  ; # 402cad: mov    rax,r15
  ; # 402cb0: sub    rax,QWORD PTR [rbp+0x8]
  ; # 402cb4: jmp    402cd9
  br label %block_402cd9
block_402cb6:
  %R74 = phi i64 [ %R59, %subblock_402c85_1 ]
  %R73 = phi i64 [ %R58, %subblock_402c85_1 ]
  %R72 = phi i64 [ %R57, %subblock_402c85_1 ]
  %R71 = phi i64 [ %R56, %subblock_402c85_1 ]
  %R70 = phi i64 [ %R55, %subblock_402c85_1 ]
  ; # 402cb6: mov    rdx,QWORD PTR [rbp+0x8]
  ; r75 := (bv_add r72 0x8 :: [64])
  %R75 = add i64 %R72, 8
  ; r76 := *r75
  %r98 = inttoptr i64 %R75 to i64*
  %R76 = load i64* %r98
  ; # 402cba: sub    r12,rax
  ; r77 := (bv_sub r73 r70)
  %R77 = sub i64 %R73, %R70
  ; # 402cbd: cmp    rax,rdx
  ; # 402cc0: jbe    402ccc
  ; r78 := (bv_ule r70 r76)
  %R78 = icmp ule i64 %R70, %R76
  br i1 %R78, label %subblock_402cb6_1, label %subblock_402cb6_2
subblock_402cb6_1:
  br label %block_402ccc
subblock_402cb6_2:
  br label %block_402cc2
block_402cc2:
  %R84 = phi i64 [ %R74, %subblock_402cb6_2 ]
  %R83 = phi i64 [ %R77, %subblock_402cb6_2 ]
  %R82 = phi i64 [ %R72, %subblock_402cb6_2 ]
  %R81 = phi i64 [ %R71, %subblock_402cb6_2 ]
  %R80 = phi i64 [ %R76, %subblock_402cb6_2 ]
  %R79 = phi i64 [ %R70, %subblock_402cb6_2 ]
  ; # 402cc2: sub    rax,rdx
  ; r85 := (bv_sub r79 r80)
  %R85 = sub i64 %R79, %R80
  ; # 402cc5: add    rbp,0x10
  ; r86 := (bv_add r82 0x10 :: [64])
  %R86 = add i64 %R82, 16
  ; # 402cc9: dec    r13d
  ; r87 := (trunc r84 32)
  %R87 = trunc i64 %R84 to i32
  ; r88 := (bv_add r87 0xffffffff :: [32])
  %R88 = add i32 %R87, 4294967295
  ; r89 := (uext r88 64)
  %R89 = zext i32 %R88 to i64
  br label %block_402ccc
block_402ccc:
  %R94 = phi i64 [ %R89, %block_402cc2 ], [ %R74, %subblock_402cb6_1 ]
  %R93 = phi i64 [ %R83, %block_402cc2 ], [ %R77, %subblock_402cb6_1 ]
  %R92 = phi i64 [ %R86, %block_402cc2 ], [ %R72, %subblock_402cb6_1 ]
  %R91 = phi i64 [ %R81, %block_402cc2 ], [ %R71, %subblock_402cb6_1 ]
  %R90 = phi i64 [ %R85, %block_402cc2 ], [ %R70, %subblock_402cb6_1 ]
  ; # 402ccc: add    QWORD PTR [rbp],rax
  ; r95 := *r92
  %r118 = inttoptr i64 %R92 to i64*
  %R95 = load i64* %r118
  ; r96 := (bv_add r95 r90)
  %R96 = add i64 %R95, %R90
  ; *(r92) = r96
  %r121 = inttoptr i64 %R92 to i64*
  store i64 %R96, i64* %r121
  ; # 402cd0: sub    QWORD PTR [rbp+0x8],rax
  ; r97 := (bv_add r92 0x8 :: [64])
  %R97 = add i64 %R92, 8
  ; r98 := *r97
  %r123 = inttoptr i64 %R97 to i64*
  %R98 = load i64* %r123
  ; r99 := (bv_sub r98 r90)
  %R99 = sub i64 %R98, %R90
  ; *(r97) = r99
  %r126 = inttoptr i64 %R97 to i64*
  store i64 %R99, i64* %r126
  ; # 402cd4: jmp    402c4d
  br label %block_402c4d
block_402cd9:
  ; # 402cd9: add    rsp,0x28
  ; # 402cdd: pop    rbx
  ; # 402cde: pop    rbp
  ; # 402cdf: pop    r12
  ; # 402ce1: pop    r13
  ; # 402ce3: pop    r14
  ; # 402ce5: pop    r15
  ; # 402ce7: ret
  %r127 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r128 = insertvalue { i64, i64, <2 x double> } %r127, i64 undef, 1
  %r129 = insertvalue { i64, i64, <2 x double> } %r128, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r129
failure:
  br label %failure
}