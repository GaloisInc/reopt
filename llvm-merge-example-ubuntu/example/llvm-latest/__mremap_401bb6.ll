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
define { i64, i64, <2 x double> } @F401bb6(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, <2 x double> %a5) {
entry:
  %r0 = bitcast <2 x double> %a5 to i128
  br label %block_401bb6
block_401bb6:
  ; r0 := (alloca 0x80 :: [64])
  %r1 = alloca i8, i64 128
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x80 :: [64])
  %R1 = add i64 %R0, 128
  ; # 401bb6: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401bb7: mov    rax,0x7ffffffffffffffe
  ; # 401bc1: sub    rsp,0x70
  ; r3 := (bv_add r1 0xffffffffffffff88 :: [64])
  %R3 = add i64 %R1, 18446744073709551496
  ; # 401bc5: cmp    rdx,rax
  ; # 401bc8: mov    QWORD PTR [rsp+0x60],r8
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; *(r4) = arg4
  %r7 = inttoptr i64 %R4 to i64*
  store i64 %a4, i64* %r7
  ; # 401bcd: jbe    401be0
  ; r5 := (bv_ule arg2 0x7ffffffffffffffe :: [64])
  %R5 = icmp ule i64 %a2, 9223372036854775806
  br i1 %R5, label %subblock_401bb6_1, label %subblock_401bb6_2
subblock_401bb6_1:
  br label %block_401be0
subblock_401bb6_2:
  br label %block_401bcf
block_401bcf:
  %R9 = phi i128 [ %r0, %subblock_401bb6_2 ]
  %R8 = phi i64 [ %a0, %subblock_401bb6_2 ]
  %R7 = phi i64 [ %a1, %subblock_401bb6_2 ]
  %R6 = phi i64 [ %a2, %subblock_401bb6_2 ]
  ; # 401bcf: call   402681
  %r13 = bitcast i128 %R9 to <2 x double>
  %r14 = call { i64, i64, <2 x double> } @F402681(i64 %R8, i64 %R7, i64 %R6, <2 x double> %r13)
  %R10 = extractvalue { i64, i64, <2 x double> } %r14, 0
  %R11 = extractvalue { i64, i64, <2 x double> } %r14, 1
  %r17 = extractvalue { i64, i64, <2 x double> } %r14, 2
  %R12 = bitcast <2 x double> %r17 to i128
  br label %block_401bd4
block_401bd4:
  %R15 = phi i128 [ %R12, %block_401bcf ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R14 = phi i64 [ undef, %block_401bcf ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R13 = phi i64 [ undef, %block_401bcf ]
  ; # 401bd4: mov    DWORD PTR [rax],0xc
  ; *(r13) = 0xc :: [32]
  %r22 = inttoptr i64 %R13 to i32*
  store i32 12, i32* %r22
  ; # 401bda: or     rax,0xffffffffffffffff
  ; # 401bde: jmp    401c43
  br label %block_401c43
block_401be0:
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm7
  %R29 = phi i128 [ undef, %subblock_401bb6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm6
  %R28 = phi i128 [ undef, %subblock_401bb6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm5
  %R27 = phi i128 [ undef, %subblock_401bb6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm4
  %R26 = phi i128 [ undef, %subblock_401bb6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm3
  %R25 = phi i128 [ undef, %subblock_401bb6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm2
  %R24 = phi i128 [ undef, %subblock_401bb6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register xmm1
  %R23 = phi i128 [ undef, %subblock_401bb6_1 ]
  %R22 = phi i128 [ %r0, %subblock_401bb6_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r9
  %R21 = phi i64 [ undef, %subblock_401bb6_1 ]
  %R20 = phi i64 [ %a0, %subblock_401bb6_1 ]
  %R19 = phi i64 [ %a1, %subblock_401bb6_1 ]
  %R18 = phi i64 [ %R3, %subblock_401bb6_1 ]
  %R17 = phi i64 [ %a2, %subblock_401bb6_1 ]
  %R16 = phi i64 [ %a3, %subblock_401bb6_1 ]
  ; # 401be0: xor    r8d,r8d
  ; # 401be3: test   cl,0x2
  ; r30 := (trunc r16 8)
  %R30 = trunc i64 %R16 to i8
  ; r31 := (bv_and r30 0x2 :: [8])
  %R31 = and i8 %R30, 2
  ; r32 := (bv_eq r31 0x0 :: [8])
  %R32 = icmp eq i8 %R31, 0
  ; # 401be6: mov    ebx,ecx
  ; r33 := (trunc r16 32)
  %R33 = trunc i64 %R16 to i32
  ; r34 := (uext r33 64)
  %R34 = zext i32 %R33 to i64
  ; # 401be8: je     401c31
  br i1 %R32, label %subblock_401be0_1, label %subblock_401be0_2
subblock_401be0_1:
  br label %block_401c31
subblock_401be0_2:
  br label %block_401bea
block_401bea:
  %R50 = phi i128 [ %R29, %subblock_401be0_2 ]
  %R49 = phi i128 [ %R28, %subblock_401be0_2 ]
  %R48 = phi i128 [ %R27, %subblock_401be0_2 ]
  %R47 = phi i128 [ %R26, %subblock_401be0_2 ]
  %R46 = phi i128 [ %R25, %subblock_401be0_2 ]
  %R45 = phi i128 [ %R24, %subblock_401be0_2 ]
  %R44 = phi i128 [ %R23, %subblock_401be0_2 ]
  %R43 = phi i128 [ %R22, %subblock_401be0_2 ]
  %R42 = phi i64 [ %R21, %subblock_401be0_2 ]
  %R41 = phi i64 [ 0, %subblock_401be0_2 ]
  %R40 = phi i64 [ %R20, %subblock_401be0_2 ]
  %R39 = phi i64 [ %R19, %subblock_401be0_2 ]
  %R38 = phi i64 [ %R18, %subblock_401be0_2 ]
  %R37 = phi i64 [ %R34, %subblock_401be0_2 ]
  %R36 = phi i64 [ %R17, %subblock_401be0_2 ]
  %R35 = phi i64 [ %R16, %subblock_401be0_2 ]
  ; # 401bea: mov    QWORD PTR [rsp+0x18],rdx
  ; r51 := (bv_add r38 0x18 :: [64])
  %R51 = add i64 %R38, 24
  ; *(r51) = r36
  %r59 = inttoptr i64 %R51 to i64*
  store i64 %R36, i64* %r59
  ; # 401bef: mov    QWORD PTR [rsp+0x10],rsi
  ; r52 := (bv_add r38 0x10 :: [64])
  %R52 = add i64 %R38, 16
  ; *(r52) = r39
  %r61 = inttoptr i64 %R52 to i64*
  store i64 %R39, i64* %r61
  ; # 401bf4: mov    QWORD PTR [rsp+0x8],rdi
  ; r53 := (bv_add r38 0x8 :: [64])
  %R53 = add i64 %R38, 8
  ; *(r53) = r40
  %r63 = inttoptr i64 %R53 to i64*
  store i64 %R40, i64* %r63
  ; # 401bf9: call   401b1f
  ; r54 := (bv_add r38 0xfffffffffffffff8 :: [64])
  %R54 = add i64 %R38, 18446744073709551608
  ; r58 := (bv_add r54 0x8 :: [64])
  %R58 = add i64 %R54, 8
  %r66 = bitcast i128 %R43 to <2 x double>
  %r67 = bitcast i128 %R44 to <2 x double>
  %r68 = bitcast i128 %R45 to <2 x double>
  %r69 = bitcast i128 %R46 to <2 x double>
  %r70 = bitcast i128 %R47 to <2 x double>
  %r71 = bitcast i128 %R48 to <2 x double>
  %r72 = bitcast i128 %R49 to <2 x double>
  %r73 = bitcast i128 %R50 to <2 x double>
  %r74 = call { i64, i64, <2 x double> } @F401b1f(i64 %R40, i64 %R39, i64 %R36, i64 %R35, i64 %R41, i64 %R42, <2 x double> %r66, <2 x double> %r67, <2 x double> %r68, <2 x double> %r69, <2 x double> %r70, <2 x double> %r71, <2 x double> %r72, <2 x double> %r73)
  %R55 = extractvalue { i64, i64, <2 x double> } %r74, 0
  %R56 = extractvalue { i64, i64, <2 x double> } %r74, 1
  %r77 = extractvalue { i64, i64, <2 x double> } %r74, 2
  %R57 = bitcast <2 x double> %r77 to i128
  br label %block_401bfe
block_401bfe:
  %R60 = phi i64 [ %R58, %block_401bea ]
  %R59 = phi i64 [ %R37, %block_401bea ]
  ; # 401bfe: lea    rax,[rsp+0x80]
  ; r61 := (bv_add r60 0x80 :: [64])
  %R61 = add i64 %R60, 128
  ; # 401c06: mov    r8,QWORD PTR [rsp+0x60]
  ; r62 := (bv_add r60 0x60 :: [64])
  %R62 = add i64 %R60, 96
  ; r63 := *r62
  %r83 = inttoptr i64 %R62 to i64*
  %R63 = load i64* %r83
  ; # 401c0b: mov    rdx,QWORD PTR [rsp+0x18]
  ; r64 := (bv_add r60 0x18 :: [64])
  %R64 = add i64 %R60, 24
  ; r65 := *r64
  %r86 = inttoptr i64 %R64 to i64*
  %R65 = load i64* %r86
  ; # 401c10: mov    rsi,QWORD PTR [rsp+0x10]
  ; r66 := (bv_add r60 0x10 :: [64])
  %R66 = add i64 %R60, 16
  ; r67 := *r66
  %r89 = inttoptr i64 %R66 to i64*
  %R67 = load i64* %r89
  ; # 401c15: mov    rdi,QWORD PTR [rsp+0x8]
  ; r68 := (bv_add r60 0x8 :: [64])
  %R68 = add i64 %R60, 8
  ; r69 := *r68
  %r92 = inttoptr i64 %R68 to i64*
  %R69 = load i64* %r92
  ; # 401c1a: mov    QWORD PTR [rsp+0x30],rax
  ; r70 := (bv_add r60 0x30 :: [64])
  %R70 = add i64 %R60, 48
  ; *(r70) = r61
  %r95 = inttoptr i64 %R70 to i64*
  store i64 %R61, i64* %r95
  ; # 401c1f: lea    rax,[rsp+0x40]
  ; r71 := (bv_add r60 0x40 :: [64])
  %R71 = add i64 %R60, 64
  ; # 401c24: mov    DWORD PTR [rsp+0x28],0x20
  ; r72 := (bv_add r60 0x28 :: [64])
  %R72 = add i64 %R60, 40
  ; *(r72) = 0x20 :: [32]
  %r98 = inttoptr i64 %R72 to i32*
  store i32 32, i32* %r98
  ; # 401c2c: mov    QWORD PTR [rsp+0x38],rax
  ; r73 := (bv_add r60 0x38 :: [64])
  %R73 = add i64 %R60, 56
  ; *(r73) = r71
  %r100 = inttoptr i64 %R73 to i64*
  store i64 %R71, i64* %r100
  br label %block_401c31
block_401c31:
  %R78 = phi i64 [ %R63, %block_401bfe ], [ 0, %subblock_401be0_1 ]
  %R77 = phi i64 [ %R69, %block_401bfe ], [ %R20, %subblock_401be0_1 ]
  %R76 = phi i64 [ %R67, %block_401bfe ], [ %R19, %subblock_401be0_1 ]
  %R75 = phi i64 [ %R59, %block_401bfe ], [ %R34, %subblock_401be0_1 ]
  %R74 = phi i64 [ %R65, %block_401bfe ], [ %R17, %subblock_401be0_1 ]
  ; # 401c31: movsxd r10,ebx
  ; r79 := (trunc r75 32)
  %R79 = trunc i64 %R75 to i32
  ; r80 := (sext r79 64)
  %R80 = sext i32 %R79 to i64
  ; # 401c34: mov    eax,0x19
  ; # 401c39: syscall
  ; sys_mremap
  %r108 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R77, i64 %R76, i64 %R74, i64 %R80, i64 %R78, i64 undef, i64 25)
  %R81 = extractvalue { i64, i1 } %r108, 0
  br label %block_401c3b
block_401c3b:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R85 = phi i128 [ undef, %block_401c31 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R84 = phi i64 [ undef, %block_401c31 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R83 = phi i64 [ undef, %block_401c31 ]
  %R82 = phi i64 [ %R81, %block_401c31 ]
  ; # 401c3b: mov    rdi,rax
  ; # 401c3e: call   4026b0
  %r114 = bitcast i128 %R85 to <2 x double>
  %r115 = call { i64, i64, <2 x double> } @F4026b0(i64 %R82, i64 %R84, i64 %R83, <2 x double> %r114)
  %R86 = extractvalue { i64, i64, <2 x double> } %r115, 0
  %R87 = extractvalue { i64, i64, <2 x double> } %r115, 1
  %r118 = extractvalue { i64, i64, <2 x double> } %r115, 2
  %R88 = bitcast <2 x double> %r118 to i128
  br label %block_401c43
block_401c43:
  %R91 = phi i128 [ %R15, %block_401bd4 ], [ %R88, %block_401c3b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R90 = phi i64 [ %R14, %block_401bd4 ], [ undef, %block_401c3b ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R89 = phi i64 [ 18446744073709551615, %block_401bd4 ], [ undef, %block_401c3b ]
  ; # 401c43: add    rsp,0x70
  ; # 401c47: pop    rbx
  ; # 401c48: ret
  %r123 = bitcast i128 %R91 to <2 x double>
  %r124 = insertvalue { i64, i64, <2 x double> } undef, i64 %R89, 0
  %r125 = insertvalue { i64, i64, <2 x double> } %r124, i64 %R90, 1
  %r126 = insertvalue { i64, i64, <2 x double> } %r125, <2 x double> %r123, 2
  ret { i64, i64, <2 x double> } %r126
failure:
  br label %failure
}