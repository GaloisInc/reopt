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
define { i64, i64, <2 x double> } @F402ee9(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_402ee9
block_402ee9:
  ; r0 := (alloca 0x10 :: [64])
  %r8 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 402ee9: mov    al,BYTE PTR [rdi+0x8a]
  ; r2 := (bv_add arg0 0x8a :: [64])
  %R2 = add i64 %a0, 138
  ; r3 := *r2
  %r12 = inttoptr i64 %R2 to i8*
  %R3 = load i8* %r12
  ; r4 := (bv_and unsupported (Initial register rax) 0xffffffffffffff00 :: [64])
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %R4 = and i64 undef, 18446744073709551360
  ; r5 := (uext r3 64)
  %R5 = zext i8 %R3 to i64
  ; r6 := (bv_or r4 r5)
  %R6 = or i64 %R4, %R5
  ; # 402eef: push   rbx
  ; r7 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R7 = add i64 %R1, 18446744073709551608
  ; # 402ef0: mov    rbx,rdi
  ; # 402ef3: lea    edx,[rax-0x1]
  ; r8 := (bv_add r6 0xffffffffffffffff :: [64])
  %R8 = add i64 %R6, 18446744073709551615
  ; r9 := (trunc r8 32)
  %R9 = trunc i64 %R8 to i32
  ; r10 := (uext r9 64)
  %R10 = zext i32 %R9 to i64
  ; # 402ef6: or     eax,edx
  ; r11 := (trunc r6 32)
  %R11 = trunc i64 %R6 to i32
  ; r12 := (bv_or r11 r9)
  %R12 = or i32 %R11, %R9
  ; r13 := (trunc r12 8)
  %R13 = trunc i32 %R12 to i8
  ; # 402ef8: mov    BYTE PTR [rdi+0x8a],al
  ; *(r2) = r13
  %r24 = inttoptr i64 %R2 to i8*
  store i8 %R13, i8* %r24
  ; # 402efe: mov    rax,QWORD PTR [rdi+0x58]
  ; r14 := (bv_add arg0 0x58 :: [64])
  %R14 = add i64 %a0, 88
  ; r15 := *r14
  %r26 = inttoptr i64 %R14 to i64*
  %R15 = load i64* %r26
  ; # 402f02: cmp    QWORD PTR [rdi+0x28],rax
  ; r16 := (bv_add arg0 0x28 :: [64])
  %R16 = add i64 %a0, 40
  ; r17 := *r16
  %r29 = inttoptr i64 %R16 to i64*
  %R17 = load i64* %r29
  ; # 402f06: jbe    402f0f
  ; r18 := (bv_ule r17 r15)
  %R18 = icmp ule i64 %R17, %R15
  br i1 %R18, label %subblock_402ee9_1, label %subblock_402ee9_2
subblock_402ee9_1:
  br label %block_402f0f
subblock_402ee9_2:
  br label %block_402f08
block_402f08:
  %R31 = phi i128 [ %r7, %subblock_402ee9_2 ]
  %R30 = phi i128 [ %r6, %subblock_402ee9_2 ]
  %R29 = phi i128 [ %r5, %subblock_402ee9_2 ]
  %R28 = phi i128 [ %r4, %subblock_402ee9_2 ]
  %R27 = phi i128 [ %r3, %subblock_402ee9_2 ]
  %R26 = phi i128 [ %r2, %subblock_402ee9_2 ]
  %R25 = phi i128 [ %r1, %subblock_402ee9_2 ]
  %R24 = phi i128 [ %r0, %subblock_402ee9_2 ]
  %R23 = phi i64 [ %a5, %subblock_402ee9_2 ]
  %R22 = phi i64 [ %a4, %subblock_402ee9_2 ]
  %R21 = phi i64 [ %a0, %subblock_402ee9_2 ]
  %R20 = phi i64 [ %a0, %subblock_402ee9_2 ]
  %R19 = phi i64 [ %a3, %subblock_402ee9_2 ]
  ; # 402f08: xor    edx,edx
  ; # 402f0a: xor    esi,esi
  ; # 402f0c: call   QWORD PTR [rdi+0x48]
  ; r32 := (bv_add r21 0x48 :: [64])
  %R32 = add i64 %R21, 72
  ; r33 := *r32
  %r46 = inttoptr i64 %R32 to i64*
  %R33 = load i64* %r46
  %r48 = inttoptr i64 %R33 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r49 = bitcast i128 %R24 to <2 x double>
  %r50 = bitcast i128 %R25 to <2 x double>
  %r51 = bitcast i128 %R26 to <2 x double>
  %r52 = bitcast i128 %R27 to <2 x double>
  %r53 = bitcast i128 %R28 to <2 x double>
  %r54 = bitcast i128 %R29 to <2 x double>
  %r55 = bitcast i128 %R30 to <2 x double>
  %r56 = bitcast i128 %R31 to <2 x double>
  %r57 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r48(i64 %R21, i64 0, i64 0, i64 %R19, i64 %R22, i64 %R23, <2 x double> %r49, <2 x double> %r50, <2 x double> %r51, <2 x double> %r52, <2 x double> %r53, <2 x double> %r54, <2 x double> %r55, <2 x double> %r56)
  %R34 = extractvalue { i64, i64, <2 x double> } %r57, 0
  %R35 = extractvalue { i64, i64, <2 x double> } %r57, 1
  %r60 = extractvalue { i64, i64, <2 x double> } %r57, 2
  %R36 = bitcast <2 x double> %r60 to i128
  br label %block_402f0f
block_402f0f:
  %R39 = phi i128 [ %R36, %block_402f08 ], [ %r0, %subblock_402ee9_1 ]
  %R38 = phi i64 [ %R20, %block_402f08 ], [ %a0, %subblock_402ee9_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R37 = phi i64 [ undef, %block_402f08 ], [ %R10, %subblock_402ee9_1 ]
  ; # 402f0f: mov    eax,DWORD PTR [rbx]
  ; r40 := *r38
  %r65 = inttoptr i64 %R38 to i32*
  %R40 = load i32* %r65
  ; r41 := (uext r40 64)
  %R41 = zext i32 %R40 to i64
  ; # 402f11: mov    QWORD PTR [rbx+0x20],0x0
  ; r42 := (bv_add r38 0x20 :: [64])
  %R42 = add i64 %R38, 32
  ; *(r42) = 0x0 :: [64]
  %r69 = inttoptr i64 %R42 to i64*
  store i64 0, i64* %r69
  ; # 402f19: mov    QWORD PTR [rbx+0x38],0x0
  ; r43 := (bv_add r38 0x38 :: [64])
  %R43 = add i64 %R38, 56
  ; *(r43) = 0x0 :: [64]
  %r71 = inttoptr i64 %R43 to i64*
  store i64 0, i64* %r71
  ; # 402f21: mov    QWORD PTR [rbx+0x28],0x0
  ; r44 := (bv_add r38 0x28 :: [64])
  %R44 = add i64 %R38, 40
  ; *(r44) = 0x0 :: [64]
  %r73 = inttoptr i64 %R44 to i64*
  store i64 0, i64* %r73
  ; # 402f29: test   al,0x4
  ; r45 := (trunc r40 8)
  %R45 = trunc i32 %R40 to i8
  ; r46 := (bv_and r45 0x4 :: [8])
  %R46 = and i8 %R45, 4
  ; r47 := (bv_eq r46 0x0 :: [8])
  %R47 = icmp eq i8 %R46, 0
  ; # 402f2b: je     402f37
  br i1 %R47, label %subblock_402f0f_1, label %subblock_402f0f_2
subblock_402f0f_1:
  br label %block_402f37
subblock_402f0f_2:
  br label %block_402f2d
block_402f2d:
  %R51 = phi i128 [ %R39, %subblock_402f0f_2 ]
  %R50 = phi i64 [ %R38, %subblock_402f0f_2 ]
  %R49 = phi i64 [ %R37, %subblock_402f0f_2 ]
  %R48 = phi i64 [ %R41, %subblock_402f0f_2 ]
  ; # 402f2d: or     eax,0x20
  ; r52 := (trunc r48 32)
  %R52 = trunc i64 %R48 to i32
  ; r53 := (bv_or r52 0x20 :: [32])
  %R53 = or i32 %R52, 32
  ; # 402f30: mov    DWORD PTR [rbx],eax
  ; *(r50) = r53
  %r83 = inttoptr i64 %R50 to i32*
  store i32 %R53, i32* %r83
  ; # 402f32: or     eax,0xffffffff
  ; # 402f35: jmp    402f4d
  br label %block_402f4d
block_402f37:
  %R56 = phi i128 [ %R39, %subblock_402f0f_1 ]
  %R55 = phi i64 [ %R38, %subblock_402f0f_1 ]
  %R54 = phi i64 [ %R41, %subblock_402f0f_1 ]
  ; # 402f37: mov    rdx,QWORD PTR [rbx+0x60]
  ; r57 := (bv_add r55 0x60 :: [64])
  %R57 = add i64 %R55, 96
  ; r58 := *r57
  %r88 = inttoptr i64 %R57 to i64*
  %R58 = load i64* %r88
  ; # 402f3b: add    rdx,QWORD PTR [rbx+0x58]
  ; r59 := (bv_add r55 0x58 :: [64])
  %R59 = add i64 %R55, 88
  ; r60 := *r59
  %r91 = inttoptr i64 %R59 to i64*
  %R60 = load i64* %r91
  ; r61 := (bv_add r58 r60)
  %R61 = add i64 %R58, %R60
  ; # 402f3f: shl    eax,0x1b
  ; r62 := (trunc r54 32)
  %R62 = trunc i64 %R54 to i32
  ; r63 := (bv_shl r62 0x1b :: [32])
  %R63 = shl i32 %R62, 27
  ; # 402f42: sar    eax,0x1f
  ; r64 := (bv_sar r63 0x1f :: [32])
  %R64 = ashr i32 %R63, 31
  ; r65 := (uext r64 64)
  %R65 = zext i32 %R64 to i64
  ; # 402f45: mov    QWORD PTR [rbx+0x10],rdx
  ; r66 := (bv_add r55 0x10 :: [64])
  %R66 = add i64 %R55, 16
  ; *(r66) = r61
  %r99 = inttoptr i64 %R66 to i64*
  store i64 %R61, i64* %r99
  ; # 402f49: mov    QWORD PTR [rbx+0x8],rdx
  ; r67 := (bv_add r55 0x8 :: [64])
  %R67 = add i64 %R55, 8
  ; *(r67) = r61
  %r101 = inttoptr i64 %R67 to i64*
  store i64 %R61, i64* %r101
  br label %block_402f4d
block_402f4d:
  %R70 = phi i128 [ %R51, %block_402f2d ], [ %R56, %block_402f37 ]
  %R69 = phi i64 [ %R49, %block_402f2d ], [ %R61, %block_402f37 ]
  %R68 = phi i64 [ 4294967295, %block_402f2d ], [ %R65, %block_402f37 ]
  ; # 402f4d: pop    rbx
  ; # 402f4e: ret
  %r105 = bitcast i128 %R70 to <2 x double>
  %r106 = insertvalue { i64, i64, <2 x double> } undef, i64 %R68, 0
  %r107 = insertvalue { i64, i64, <2 x double> } %r106, i64 %R69, 1
  %r108 = insertvalue { i64, i64, <2 x double> } %r107, <2 x double> %r105, 2
  ret { i64, i64, <2 x double> } %r108
failure:
  br label %failure
}