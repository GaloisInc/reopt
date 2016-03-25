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
define { i64, i64, <2 x double> } @F402b4f(i64 %a0, i64 %a1, i64 %a2) {
entry:
  br label %block_402b4f
block_402b4f:
  ; r0 := (alloca 0x40 :: [64])
  %r0 = alloca i8, i64 64
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x40 :: [64])
  %R1 = add i64 %R0, 64
  ; # 402b4f: push   r12
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402b51: push   rbp
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 402b52: mov    r12,rdx
  ; # 402b55: push   rbx
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 402b56: xor    edx,edx
  ; # 402b58: mov    rbp,rsi
  ; # 402b5b: mov    rbx,rdi
  ; # 402b5e: sub    rsp,0x20
  ; r5 := (bv_add r1 0xffffffffffffffc8 :: [64])
  %R5 = add i64 %R1, 18446744073709551560
  ; # 402b62: mov    rax,QWORD PTR [rdi+0x60]
  ; r6 := (bv_add arg0 0x60 :: [64])
  %R6 = add i64 %a0, 96
  ; r7 := *r6
  %r8 = inttoptr i64 %R6 to i64*
  %R7 = load i64* %r8
  ; # 402b66: mov    QWORD PTR [rsp],rsi
  ; *(r5) = arg1
  %r10 = inttoptr i64 %R5 to i64*
  store i64 %a1, i64* %r10
  ; # 402b6a: mov    rsi,r12
  ; # 402b6d: test   rax,rax
  ; r8 := (bv_eq r7 0x0 :: [64])
  %R8 = icmp eq i64 %R7, 0
  ; # 402b70: mov    QWORD PTR [rsp+0x18],rax
  ; r9 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R9 = add i64 %R1, 18446744073709551584
  ; *(r9) = r7
  %r13 = inttoptr i64 %R9 to i64*
  store i64 %R7, i64* %r13
  ; # 402b75: mov    eax,0x13
  ; # 402b7a: setne  dl
  ; r10 := (mux r8 0x0 :: [8] 0x1 :: [8])
  %R10 = select i1 %R8, i8 0, i8 1
  ; r11 := (uext r10 64)
  %R11 = zext i8 %R10 to i64
  ; # 402b7d: sub    rsi,rdx
  ; r12 := (bv_sub arg2 r11)
  %R12 = sub i64 %a2, %R11
  ; # 402b80: mov    rdx,QWORD PTR [rdi+0x58]
  ; r13 := (bv_add arg0 0x58 :: [64])
  %R13 = add i64 %a0, 88
  ; r14 := *r13
  %r18 = inttoptr i64 %R13 to i64*
  %R14 = load i64* %r18
  ; # 402b84: movsxd rdi,DWORD PTR [rdi+0x78]
  ; r15 := (bv_add arg0 0x78 :: [64])
  %R15 = add i64 %a0, 120
  ; r16 := *r15
  %r21 = inttoptr i64 %R15 to i32*
  %R16 = load i32* %r21
  ; r17 := (sext r16 64)
  %R17 = sext i32 %R16 to i64
  ; # 402b88: mov    QWORD PTR [rsp+0x8],rsi
  ; r18 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R18 = add i64 %R1, 18446744073709551568
  ; *(r18) = r12
  %r25 = inttoptr i64 %R18 to i64*
  store i64 %R12, i64* %r25
  ; # 402b8d: mov    rsi,rsp
  ; # 402b90: mov    QWORD PTR [rsp+0x10],rdx
  ; r19 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R19 = add i64 %R1, 18446744073709551576
  ; *(r19) = r14
  %r27 = inttoptr i64 %R19 to i64*
  store i64 %R14, i64* %r27
  ; # 402b95: mov    edx,0x2
  ; # 402b9a: syscall
  ; sys_readv
  %r28 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R17, i64 %R5, i64 2, i64 undef, i64 undef, i64 undef, i64 19)
  %R20 = extractvalue { i64, i1 } %r28, 0
  br label %block_402b9c
block_402b9c:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R28 = phi i128 [ undef, %block_402b4f ]
  %R27 = phi i64 [ %a2, %block_402b4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R26 = phi i64 [ undef, %block_402b4f ]
  %R25 = phi i64 [ %a1, %block_402b4f ]
  %R24 = phi i64 [ %R5, %block_402b4f ]
  %R23 = phi i64 [ %a0, %block_402b4f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R22 = phi i64 [ undef, %block_402b4f ]
  %R21 = phi i64 [ %R20, %block_402b4f ]
  ; # 402b9c: mov    rdi,rax
  ; # 402b9f: call   4026b0
  ; r29 := (bv_add r24 0xfffffffffffffff8 :: [64])
  %R29 = add i64 %R24, 18446744073709551608
  ; r33 := (bv_add r29 0x8 :: [64])
  %R33 = add i64 %R29, 8
  %r40 = bitcast i128 %R28 to <2 x double>
  %r41 = call { i64, i64, <2 x double> } @F4026b0(i64 %R21, i64 %R26, i64 %R22, <2 x double> %r40)
  %R30 = extractvalue { i64, i64, <2 x double> } %r41, 0
  %R31 = extractvalue { i64, i64, <2 x double> } %r41, 1
  %r44 = extractvalue { i64, i64, <2 x double> } %r41, 2
  %R32 = bitcast <2 x double> %r44 to i128
  br label %block_402ba4
block_402ba4:
  %R38 = phi i64 [ %R27, %block_402b9c ]
  %R37 = phi i64 [ %R25, %block_402b9c ]
  %R36 = phi i64 [ %R33, %block_402b9c ]
  %R35 = phi i64 [ %R23, %block_402b9c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R34 = phi i64 [ undef, %block_402b9c ]
  ; # 402ba4: test   rax,rax
  ; r39 := (bv_slt r34 0x0 :: [64])
  %R39 = icmp slt i64 %R34, 0
  ; r40 := (bv_eq r34 0x0 :: [64])
  %R40 = icmp eq i64 %R34, 0
  ; # 402ba7: jg     402bb5
  ; r41 := (bv_complement r40)
  %R41 = xor i1 %R40, -1
  ; r42 := (bv_eq r39 0x0 :: [1])
  %R42 = icmp eq i1 %R39, 0
  ; r43 := (bv_and r41 r42)
  %R43 = and i1 %R41, %R42
  br i1 %R43, label %subblock_402ba4_1, label %subblock_402ba4_2
subblock_402ba4_1:
  br label %block_402bb5
subblock_402ba4_2:
  br label %block_402ba9
block_402ba9:
  %R45 = phi i64 [ %R35, %subblock_402ba4_2 ]
  %R44 = phi i64 [ %R34, %subblock_402ba4_2 ]
  ; # 402ba9: mov    edx,eax
  ; r46 := (trunc r44 32)
  %R46 = trunc i64 %R44 to i32
  ; # 402bab: and    edx,0x30
  ; r47 := (bv_and r46 0x30 :: [32])
  %R47 = and i32 %R46, 48
  ; # 402bae: xor    edx,0x10
  ; r48 := (bv_xor r47 0x10 :: [32])
  %R48 = xor i32 %R47, 16
  ; # 402bb1: or     DWORD PTR [rbx],edx
  ; r49 := *r45
  %r61 = inttoptr i64 %R45 to i32*
  %R49 = load i32* %r61
  ; r50 := (bv_or r49 r48)
  %R50 = or i32 %R49, %R48
  ; *(r45) = r50
  %r64 = inttoptr i64 %R45 to i32*
  store i32 %R50, i32* %r64
  ; # 402bb3: jmp    402bea
  br label %block_402bea
block_402bb5:
  %R55 = phi i64 [ %R38, %subblock_402ba4_1 ]
  %R54 = phi i64 [ %R37, %subblock_402ba4_1 ]
  %R53 = phi i64 [ %R36, %subblock_402ba4_1 ]
  %R52 = phi i64 [ %R35, %subblock_402ba4_1 ]
  %R51 = phi i64 [ %R34, %subblock_402ba4_1 ]
  ; # 402bb5: mov    rcx,QWORD PTR [rsp+0x8]
  ; r56 := (bv_add r53 0x8 :: [64])
  %R56 = add i64 %R53, 8
  ; r57 := *r56
  %r71 = inttoptr i64 %R56 to i64*
  %R57 = load i64* %r71
  ; # 402bba: cmp    rcx,rax
  ; # 402bbd: jae    402bea
  ; r58 := (bv_ule r51 r57)
  %R58 = icmp ule i64 %R51, %R57
  br i1 %R58, label %subblock_402bb5_1, label %subblock_402bb5_2
subblock_402bb5_1:
  br label %block_402bea
subblock_402bb5_2:
  br label %block_402bbf
block_402bbf:
  %R63 = phi i64 [ %R55, %subblock_402bb5_2 ]
  %R62 = phi i64 [ %R54, %subblock_402bb5_2 ]
  %R61 = phi i64 [ %R52, %subblock_402bb5_2 ]
  %R60 = phi i64 [ %R57, %subblock_402bb5_2 ]
  %R59 = phi i64 [ %R51, %subblock_402bb5_2 ]
  ; # 402bbf: mov    rdx,QWORD PTR [rbx+0x58]
  ; r64 := (bv_add r61 0x58 :: [64])
  %R64 = add i64 %R61, 88
  ; r65 := *r64
  %r80 = inttoptr i64 %R64 to i64*
  %R65 = load i64* %r80
  ; # 402bc3: sub    rax,rcx
  ; r66 := (bv_sub r59 r60)
  %R66 = sub i64 %R59, %R60
  ; # 402bc6: add    rax,rdx
  ; r67 := (bv_add r66 r65)
  %R67 = add i64 %R66, %R65
  ; # 402bc9: cmp    QWORD PTR [rbx+0x60],0x0
  ; r68 := (bv_add r61 0x60 :: [64])
  %R68 = add i64 %R61, 96
  ; r69 := *r68
  %r85 = inttoptr i64 %R68 to i64*
  %R69 = load i64* %r85
  ; r70 := (bv_eq r69 0x0 :: [64])
  %R70 = icmp eq i64 %R69, 0
  ; # 402bce: mov    QWORD PTR [rbx+0x8],rdx
  ; r71 := (bv_add r61 0x8 :: [64])
  %R71 = add i64 %R61, 8
  ; *(r71) = r65
  %r89 = inttoptr i64 %R71 to i64*
  store i64 %R65, i64* %r89
  ; # 402bd2: mov    QWORD PTR [rbx+0x10],rax
  ; r72 := (bv_add r61 0x10 :: [64])
  %R72 = add i64 %R61, 16
  ; *(r72) = r67
  %r91 = inttoptr i64 %R72 to i64*
  store i64 %R67, i64* %r91
  ; # 402bd6: mov    rax,r12
  ; # 402bd9: je     402bea
  br i1 %R70, label %subblock_402bbf_1, label %subblock_402bbf_2
subblock_402bbf_1:
  br label %block_402bea
subblock_402bbf_2:
  br label %block_402bdb
block_402bdb:
  %R76 = phi i64 [ %R63, %subblock_402bbf_2 ]
  %R75 = phi i64 [ %R62, %subblock_402bbf_2 ]
  %R74 = phi i64 [ %R61, %subblock_402bbf_2 ]
  %R73 = phi i64 [ %R65, %subblock_402bbf_2 ]
  ; # 402bdb: lea    rcx,[rdx+0x1]
  ; r77 := (bv_add r73 0x1 :: [64])
  %R77 = add i64 %R73, 1
  ; # 402bdf: mov    QWORD PTR [rbx+0x8],rcx
  ; r78 := (bv_add r74 0x8 :: [64])
  %R78 = add i64 %R74, 8
  ; *(r78) = r77
  %r98 = inttoptr i64 %R78 to i64*
  store i64 %R77, i64* %r98
  ; # 402be3: mov    dl,BYTE PTR [rdx]
  ; r79 := *r73
  %r99 = inttoptr i64 %R73 to i8*
  %R79 = load i8* %r99
  ; r80 := (bv_and r73 0xffffffffffffff00 :: [64])
  %R80 = and i64 %R73, 18446744073709551360
  ; r81 := (uext r79 64)
  %R81 = zext i8 %R79 to i64
  ; r82 := (bv_or r80 r81)
  %R82 = or i64 %R80, %R81
  ; # 402be5: mov    BYTE PTR [rbp+r12*1-0x1],dl
  ; r83 := (trunc r82 8)
  %R83 = trunc i64 %R82 to i8
  ; r84 := (bv_add r75 r76)
  %R84 = add i64 %R75, %R76
  ; r85 := (bv_add r84 0xffffffffffffffff :: [64])
  %R85 = add i64 %R84, 18446744073709551615
  ; *(r85) = r83
  %r107 = inttoptr i64 %R85 to i8*
  store i8 %R83, i8* %r107
  br label %block_402bea
block_402bea:
  ; # 402bea: add    rsp,0x20
  ; # 402bee: pop    rbx
  ; # 402bef: pop    rbp
  ; # 402bf0: pop    r12
  ; # 402bf2: ret
  %r108 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r109 = insertvalue { i64, i64, <2 x double> } %r108, i64 undef, 1
  %r110 = insertvalue { i64, i64, <2 x double> } %r109, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r110
failure:
  br label %failure
}