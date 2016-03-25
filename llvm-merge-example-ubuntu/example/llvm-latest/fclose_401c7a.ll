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
declare { i64, i64, <2 x double> } @F400e40(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401c79(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401dbf(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F40217e(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402190(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402a2c(i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402a80(i64, i64, i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F401c7a(i64 %a0, i64 %a1, i64 %a2, i64 %a3, <2 x double> %a4) {
entry:
  %r0 = bitcast <2 x double> %a4 to i128
  br label %block_401c7a
block_401c7a:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 401c7a: push   r13
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401c7c: push   r12
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 401c7e: xor    r13d,r13d
  ; # 401c81: push   rbp
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 401c82: push   rbx
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 401c83: mov    rbx,rdi
  ; # 401c86: push   rcx
  ; r6 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R6 = add i64 %R1, 18446744073709551576
  ; *(r6) = arg3
  %r9 = inttoptr i64 %R6 to i64*
  store i64 %a3, i64* %r9
  ; # 401c87: mov    eax,DWORD PTR [rdi+0x8c]
  ; r7 := (bv_add arg0 0x8c :: [64])
  %R7 = add i64 %a0, 140
  ; r8 := *r7
  %r11 = inttoptr i64 %R7 to i32*
  %R8 = load i32* %r11
  ; # 401c8d: test   eax,eax
  ; r9 := (bv_slt r8 0x0 :: [32])
  %R9 = icmp slt i32 %R8, 0
  ; # 401c8f: js     401c99
  br i1 %R9, label %subblock_401c7a_1, label %subblock_401c7a_2
subblock_401c7a_1:
  br label %block_401c99
subblock_401c7a_2:
  br label %block_401c91
block_401c91:
  %R12 = phi i128 [ %r0, %subblock_401c7a_2 ]
  %R11 = phi i64 [ %a0, %subblock_401c7a_2 ]
  %R10 = phi i64 [ %a0, %subblock_401c7a_2 ]
  ; # 401c91: call   402a2c
  %r17 = bitcast i128 %R12 to <2 x double>
  %r18 = call { i64, i64, <2 x double> } @F402a2c(i64 %R11, <2 x double> %r17)
  %R13 = extractvalue { i64, i64, <2 x double> } %r18, 0
  %R14 = extractvalue { i64, i64, <2 x double> } %r18, 1
  %r21 = extractvalue { i64, i64, <2 x double> } %r18, 2
  %R15 = bitcast <2 x double> %r21 to i128
  br label %block_401c96
block_401c96:
  %R20 = phi i128 [ %R15, %block_401c91 ]
  %R19 = phi i64 [ %R14, %block_401c91 ]
  %R18 = phi i64 [ %R10, %block_401c91 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R17 = phi i64 [ undef, %block_401c91 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R16 = phi i64 [ undef, %block_401c91 ]
  ; # 401c96: mov    r13d,eax
  ; r21 := (trunc r16 32)
  %R21 = trunc i64 %R16 to i32
  ; r22 := (uext r21 64)
  %R22 = zext i32 %R21 to i64
  br label %block_401c99
block_401c99:
  %R27 = phi i128 [ %R20, %block_401c96 ], [ %r0, %subblock_401c7a_1 ]
  %R26 = phi i64 [ %R22, %block_401c96 ], [ 0, %subblock_401c7a_1 ]
  %R25 = phi i64 [ %R19, %block_401c96 ], [ %a1, %subblock_401c7a_1 ]
  %R24 = phi i64 [ %R18, %block_401c96 ], [ %a0, %subblock_401c7a_1 ]
  %R23 = phi i64 [ %R17, %block_401c96 ], [ %a2, %subblock_401c7a_1 ]
  ; # 401c99: mov    rdi,rbx
  ; # 401c9c: call   401c79
  %r35 = bitcast i128 %R27 to <2 x double>
  %r36 = call { i64, i64, <2 x double> } @F401c79(i64 %R24, i64 %R25, i64 %R23, <2 x double> %r35)
  %R28 = extractvalue { i64, i64, <2 x double> } %r36, 0
  %R29 = extractvalue { i64, i64, <2 x double> } %r36, 1
  %r39 = extractvalue { i64, i64, <2 x double> } %r36, 2
  %R30 = bitcast <2 x double> %r39 to i128
  br label %block_401ca1
block_401ca1:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R46 = phi i128 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R45 = phi i128 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R44 = phi i128 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R43 = phi i128 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R42 = phi i128 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R41 = phi i128 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R40 = phi i128 [ undef, %block_401c99 ]
  %R39 = phi i128 [ %R30, %block_401c99 ]
  %R38 = phi i64 [ %R26, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R37 = phi i64 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R36 = phi i64 [ undef, %block_401c99 ]
  %R35 = phi i64 [ %R28, %block_401c99 ]
  %R34 = phi i64 [ %R29, %block_401c99 ]
  %R33 = phi i64 [ %R24, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R32 = phi i64 [ undef, %block_401c99 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R31 = phi i64 [ undef, %block_401c99 ]
  ; # 401ca1: mov    ebp,DWORD PTR [rbx]
  ; r47 := *r33
  %r57 = inttoptr i64 %R33 to i32*
  %R47 = load i32* %r57
  ; # 401ca3: and    ebp,0x1
  ; r48 := (bv_and r47 0x1 :: [32])
  %R48 = and i32 %R47, 1
  ; r49 := (bv_eq r48 0x0 :: [32])
  %R49 = icmp eq i32 %R48, 0
  ; r50 := (uext r48 64)
  %R50 = zext i32 %R48 to i64
  ; # 401ca6: jne    401cd8
  ; r51 := (bv_complement r49)
  %R51 = xor i1 %R49, -1
  br i1 %R51, label %subblock_401ca1_1, label %subblock_401ca1_2
subblock_401ca1_1:
  br label %block_401cd8
subblock_401ca1_2:
  br label %block_401ca8
block_401ca8:
  %R58 = phi i128 [ %R39, %subblock_401ca1_2 ]
  %R57 = phi i64 [ %R38, %subblock_401ca1_2 ]
  %R56 = phi i64 [ %R35, %subblock_401ca1_2 ]
  %R55 = phi i64 [ %R34, %subblock_401ca1_2 ]
  %R54 = phi i64 [ %R50, %subblock_401ca1_2 ]
  %R53 = phi i64 [ %R33, %subblock_401ca1_2 ]
  %R52 = phi i64 [ %R32, %subblock_401ca1_2 ]
  ; # 401ca8: call   40217e
  %r70 = bitcast i128 %R58 to <2 x double>
  %r71 = call { i64, i64, <2 x double> } @F40217e(i64 %R56, i64 %R55, i64 %R52, <2 x double> %r70)
  %R59 = extractvalue { i64, i64, <2 x double> } %r71, 0
  %R60 = extractvalue { i64, i64, <2 x double> } %r71, 1
  %r74 = extractvalue { i64, i64, <2 x double> } %r71, 2
  %R61 = bitcast <2 x double> %r74 to i128
  br label %block_401cad
block_401cad:
  %R70 = phi i128 [ %R61, %block_401ca8 ]
  %R69 = phi i64 [ %R57, %block_401ca8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R68 = phi i64 [ undef, %block_401ca8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R67 = phi i64 [ undef, %block_401ca8 ]
  %R66 = phi i64 [ %R59, %block_401ca8 ]
  %R65 = phi i64 [ %R60, %block_401ca8 ]
  %R64 = phi i64 [ %R54, %block_401ca8 ]
  %R63 = phi i64 [ %R53, %block_401ca8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R62 = phi i64 [ undef, %block_401ca8 ]
  ; # 401cad: mov    rcx,QWORD PTR [rbx+0x68]
  ; r71 := (bv_add r63 0x68 :: [64])
  %R71 = add i64 %R63, 104
  ; r72 := *r71
  %r86 = inttoptr i64 %R71 to i64*
  %R72 = load i64* %r86
  ; # 401cb1: test   rcx,rcx
  ; r73 := (bv_eq r72 0x0 :: [64])
  %R73 = icmp eq i64 %R72, 0
  ; # 401cb4: je     401cbe
  br i1 %R73, label %subblock_401cad_1, label %subblock_401cad_2
subblock_401cad_1:
  br label %block_401cbe
subblock_401cad_2:
  br label %block_401cb6
block_401cb6:
  %R83 = phi i128 [ %R70, %subblock_401cad_2 ]
  %R82 = phi i64 [ %R69, %subblock_401cad_2 ]
  %R81 = phi i64 [ %R68, %subblock_401cad_2 ]
  %R80 = phi i64 [ %R67, %subblock_401cad_2 ]
  %R79 = phi i64 [ %R66, %subblock_401cad_2 ]
  %R78 = phi i64 [ %R65, %subblock_401cad_2 ]
  %R77 = phi i64 [ %R64, %subblock_401cad_2 ]
  %R76 = phi i64 [ %R63, %subblock_401cad_2 ]
  %R75 = phi i64 [ %R72, %subblock_401cad_2 ]
  %R74 = phi i64 [ %R62, %subblock_401cad_2 ]
  ; # 401cb6: mov    rdx,QWORD PTR [rbx+0x70]
  ; r84 := (bv_add r76 0x70 :: [64])
  %R84 = add i64 %R76, 112
  ; r85 := *r84
  %r100 = inttoptr i64 %R84 to i64*
  %R85 = load i64* %r100
  ; # 401cba: mov    QWORD PTR [rcx+0x70],rdx
  ; r86 := (bv_add r75 0x70 :: [64])
  %R86 = add i64 %R75, 112
  ; *(r86) = r85
  %r103 = inttoptr i64 %R86 to i64*
  store i64 %R85, i64* %r103
  br label %block_401cbe
block_401cbe:
  %R96 = phi i128 [ %R83, %block_401cb6 ], [ %R70, %subblock_401cad_1 ]
  %R95 = phi i64 [ %R82, %block_401cb6 ], [ %R69, %subblock_401cad_1 ]
  %R94 = phi i64 [ %R81, %block_401cb6 ], [ %R68, %subblock_401cad_1 ]
  %R93 = phi i64 [ %R80, %block_401cb6 ], [ %R67, %subblock_401cad_1 ]
  %R92 = phi i64 [ %R79, %block_401cb6 ], [ %R66, %subblock_401cad_1 ]
  %R91 = phi i64 [ %R78, %block_401cb6 ], [ %R65, %subblock_401cad_1 ]
  %R90 = phi i64 [ %R77, %block_401cb6 ], [ %R64, %subblock_401cad_1 ]
  %R89 = phi i64 [ %R76, %block_401cb6 ], [ %R63, %subblock_401cad_1 ]
  %R88 = phi i64 [ %R75, %block_401cb6 ], [ %R72, %subblock_401cad_1 ]
  %R87 = phi i64 [ %R74, %block_401cb6 ], [ %R62, %subblock_401cad_1 ]
  ; # 401cbe: mov    rdx,QWORD PTR [rbx+0x70]
  ; r97 := (bv_add r89 0x70 :: [64])
  %R97 = add i64 %R89, 112
  ; r98 := *r97
  %r115 = inttoptr i64 %R97 to i64*
  %R98 = load i64* %r115
  ; # 401cc2: test   rdx,rdx
  ; r99 := (bv_eq r98 0x0 :: [64])
  %R99 = icmp eq i64 %R98, 0
  ; # 401cc5: je     401ccb
  br i1 %R99, label %subblock_401cbe_1, label %subblock_401cbe_2
subblock_401cbe_1:
  br label %block_401ccb
subblock_401cbe_2:
  br label %block_401cc7
block_401cc7:
  %R110 = phi i128 [ %R96, %subblock_401cbe_2 ]
  %R109 = phi i64 [ %R95, %subblock_401cbe_2 ]
  %R108 = phi i64 [ %R94, %subblock_401cbe_2 ]
  %R107 = phi i64 [ %R93, %subblock_401cbe_2 ]
  %R106 = phi i64 [ %R92, %subblock_401cbe_2 ]
  %R105 = phi i64 [ %R91, %subblock_401cbe_2 ]
  %R104 = phi i64 [ %R90, %subblock_401cbe_2 ]
  %R103 = phi i64 [ %R89, %subblock_401cbe_2 ]
  %R102 = phi i64 [ %R98, %subblock_401cbe_2 ]
  %R101 = phi i64 [ %R88, %subblock_401cbe_2 ]
  %R100 = phi i64 [ %R87, %subblock_401cbe_2 ]
  ; # 401cc7: mov    QWORD PTR [rdx+0x68],rcx
  ; r111 := (bv_add r102 0x68 :: [64])
  %R111 = add i64 %R102, 104
  ; *(r111) = r101
  %r130 = inttoptr i64 %R111 to i64*
  store i64 %R101, i64* %r130
  br label %block_401ccb
block_401ccb:
  %R122 = phi i128 [ %R110, %block_401cc7 ], [ %R96, %subblock_401cbe_1 ]
  %R121 = phi i64 [ %R109, %block_401cc7 ], [ %R95, %subblock_401cbe_1 ]
  %R120 = phi i64 [ %R108, %block_401cc7 ], [ %R94, %subblock_401cbe_1 ]
  %R119 = phi i64 [ %R107, %block_401cc7 ], [ %R93, %subblock_401cbe_1 ]
  %R118 = phi i64 [ %R106, %block_401cc7 ], [ %R92, %subblock_401cbe_1 ]
  %R117 = phi i64 [ %R105, %block_401cc7 ], [ %R91, %subblock_401cbe_1 ]
  %R116 = phi i64 [ %R104, %block_401cc7 ], [ %R90, %subblock_401cbe_1 ]
  %R115 = phi i64 [ %R103, %block_401cc7 ], [ %R89, %subblock_401cbe_1 ]
  %R114 = phi i64 [ %R102, %block_401cc7 ], [ %R98, %subblock_401cbe_1 ]
  %R113 = phi i64 [ %R101, %block_401cc7 ], [ %R88, %subblock_401cbe_1 ]
  %R112 = phi i64 [ %R100, %block_401cc7 ], [ %R87, %subblock_401cbe_1 ]
  ; # 401ccb: cmp    rbx,QWORD PTR [rax]
  ; r123 := *r112
  %r142 = inttoptr i64 %R112 to i64*
  %R123 = load i64* %r142
  ; r124 := (bv_eq r115 r123)
  %R124 = icmp eq i64 %R115, %R123
  ; # 401cce: jne    401cd3
  ; r125 := (bv_complement r124)
  %R125 = xor i1 %R124, -1
  br i1 %R125, label %subblock_401ccb_1, label %subblock_401ccb_2
subblock_401ccb_1:
  br label %block_401cd3
subblock_401ccb_2:
  br label %block_401cd0
block_401cd0:
  %R136 = phi i128 [ %R122, %subblock_401ccb_2 ]
  %R135 = phi i64 [ %R121, %subblock_401ccb_2 ]
  %R134 = phi i64 [ %R120, %subblock_401ccb_2 ]
  %R133 = phi i64 [ %R119, %subblock_401ccb_2 ]
  %R132 = phi i64 [ %R118, %subblock_401ccb_2 ]
  %R131 = phi i64 [ %R117, %subblock_401ccb_2 ]
  %R130 = phi i64 [ %R116, %subblock_401ccb_2 ]
  %R129 = phi i64 [ %R115, %subblock_401ccb_2 ]
  %R128 = phi i64 [ %R114, %subblock_401ccb_2 ]
  %R127 = phi i64 [ %R113, %subblock_401ccb_2 ]
  %R126 = phi i64 [ %R112, %subblock_401ccb_2 ]
  ; # 401cd0: mov    QWORD PTR [rax],rdx
  ; *(r126) = r128
  %r157 = inttoptr i64 %R126 to i64*
  store i64 %R128, i64* %r157
  br label %block_401cd3
block_401cd3:
  %R146 = phi i128 [ %R136, %block_401cd0 ], [ %R122, %subblock_401ccb_1 ]
  %R145 = phi i64 [ %R135, %block_401cd0 ], [ %R121, %subblock_401ccb_1 ]
  %R144 = phi i64 [ %R134, %block_401cd0 ], [ %R120, %subblock_401ccb_1 ]
  %R143 = phi i64 [ %R133, %block_401cd0 ], [ %R119, %subblock_401ccb_1 ]
  %R142 = phi i64 [ %R132, %block_401cd0 ], [ %R118, %subblock_401ccb_1 ]
  %R141 = phi i64 [ %R131, %block_401cd0 ], [ %R117, %subblock_401ccb_1 ]
  %R140 = phi i64 [ %R130, %block_401cd0 ], [ %R116, %subblock_401ccb_1 ]
  %R139 = phi i64 [ %R129, %block_401cd0 ], [ %R115, %subblock_401ccb_1 ]
  %R138 = phi i64 [ %R128, %block_401cd0 ], [ %R114, %subblock_401ccb_1 ]
  %R137 = phi i64 [ %R127, %block_401cd0 ], [ %R113, %subblock_401ccb_1 ]
  ; # 401cd3: call   402190
  %r168 = bitcast i128 %R146 to <2 x double>
  %r169 = call { i64, i64, <2 x double> } @F402190(i64 %R142, i64 %R141, i64 %R138, i64 %R137, i64 %R143, i64 %R144, <2 x double> %r168)
  %R147 = extractvalue { i64, i64, <2 x double> } %r169, 0
  %R148 = extractvalue { i64, i64, <2 x double> } %r169, 1
  %r172 = extractvalue { i64, i64, <2 x double> } %r169, 2
  %R149 = bitcast <2 x double> %r172 to i128
  br label %block_401cd8
block_401cd8:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R165 = phi i128 [ undef, %block_401cd3 ], [ %R46, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R164 = phi i128 [ undef, %block_401cd3 ], [ %R45, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R163 = phi i128 [ undef, %block_401cd3 ], [ %R44, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R162 = phi i128 [ undef, %block_401cd3 ], [ %R43, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R161 = phi i128 [ undef, %block_401cd3 ], [ %R42, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R160 = phi i128 [ undef, %block_401cd3 ], [ %R41, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R159 = phi i128 [ undef, %block_401cd3 ], [ %R40, %subblock_401ca1_1 ]
  %R158 = phi i128 [ %R149, %block_401cd3 ], [ %R39, %subblock_401ca1_1 ]
  %R157 = phi i64 [ %R145, %block_401cd3 ], [ %R38, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R156 = phi i64 [ undef, %block_401cd3 ], [ %R37, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R155 = phi i64 [ undef, %block_401cd3 ], [ %R36, %subblock_401ca1_1 ]
  %R154 = phi i64 [ %R148, %block_401cd3 ], [ %R34, %subblock_401ca1_1 ]
  %R153 = phi i64 [ %R140, %block_401cd3 ], [ %R50, %subblock_401ca1_1 ]
  %R152 = phi i64 [ %R139, %block_401cd3 ], [ %R33, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R151 = phi i64 [ undef, %block_401cd3 ], [ %R32, %subblock_401ca1_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R150 = phi i64 [ undef, %block_401cd3 ], [ %R31, %subblock_401ca1_1 ]
  ; # 401cd8: mov    rdi,rbx
  ; # 401cdb: call   401dbf
  %r190 = bitcast i128 %R158 to <2 x double>
  %r191 = bitcast i128 %R159 to <2 x double>
  %r192 = bitcast i128 %R160 to <2 x double>
  %r193 = bitcast i128 %R161 to <2 x double>
  %r194 = bitcast i128 %R162 to <2 x double>
  %r195 = bitcast i128 %R163 to <2 x double>
  %r196 = bitcast i128 %R164 to <2 x double>
  %r197 = bitcast i128 %R165 to <2 x double>
  %r198 = call { i64, i64, <2 x double> } @F401dbf(i64 %R152, i64 %R154, i64 %R151, i64 %R150, i64 %R155, i64 %R156, <2 x double> %r190, <2 x double> %r191, <2 x double> %r192, <2 x double> %r193, <2 x double> %r194, <2 x double> %r195, <2 x double> %r196, <2 x double> %r197)
  %R166 = extractvalue { i64, i64, <2 x double> } %r198, 0
  %R167 = extractvalue { i64, i64, <2 x double> } %r198, 1
  %r201 = extractvalue { i64, i64, <2 x double> } %r198, 2
  %R168 = bitcast <2 x double> %r201 to i128
  br label %block_401ce0
block_401ce0:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R184 = phi i128 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R183 = phi i128 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R182 = phi i128 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R181 = phi i128 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R180 = phi i128 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R179 = phi i128 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R178 = phi i128 [ undef, %block_401cd8 ]
  %R177 = phi i128 [ %R168, %block_401cd8 ]
  %R176 = phi i64 [ %R157, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R175 = phi i64 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R174 = phi i64 [ undef, %block_401cd8 ]
  %R173 = phi i64 [ %R167, %block_401cd8 ]
  %R172 = phi i64 [ %R153, %block_401cd8 ]
  %R171 = phi i64 [ %R152, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R170 = phi i64 [ undef, %block_401cd8 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R169 = phi i64 [ undef, %block_401cd8 ]
  ; # 401ce0: mov    rdi,rbx
  ; # 401ce3: mov    r12d,eax
  ; # 401ce6: call   QWORD PTR [rbx+0x18]
  ; r185 := (bv_add r171 0x18 :: [64])
  %R185 = add i64 %R171, 24
  ; r186 := *r185
  %r220 = inttoptr i64 %R185 to i64*
  %R186 = load i64* %r220
  %r222 = inttoptr i64 %R186 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r223 = bitcast i128 %R177 to <2 x double>
  %r224 = bitcast i128 %R178 to <2 x double>
  %r225 = bitcast i128 %R179 to <2 x double>
  %r226 = bitcast i128 %R180 to <2 x double>
  %r227 = bitcast i128 %R181 to <2 x double>
  %r228 = bitcast i128 %R182 to <2 x double>
  %r229 = bitcast i128 %R183 to <2 x double>
  %r230 = bitcast i128 %R184 to <2 x double>
  %r231 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r222(i64 %R171, i64 %R173, i64 %R170, i64 %R169, i64 %R174, i64 %R175, <2 x double> %r223, <2 x double> %r224, <2 x double> %r225, <2 x double> %r226, <2 x double> %r227, <2 x double> %r228, <2 x double> %r229, <2 x double> %r230)
  %R187 = extractvalue { i64, i64, <2 x double> } %r231, 0
  %R188 = extractvalue { i64, i64, <2 x double> } %r231, 1
  %r234 = extractvalue { i64, i64, <2 x double> } %r231, 2
  %R189 = bitcast <2 x double> %r234 to i128
  br label %block_401ce9
block_401ce9:
  %R198 = phi i128 [ %R189, %block_401ce0 ]
  %R197 = phi i64 [ %R176, %block_401ce0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R196 = phi i64 [ undef, %block_401ce0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R195 = phi i64 [ undef, %block_401ce0 ]
  %R194 = phi i64 [ %R188, %block_401ce0 ]
  %R193 = phi i64 [ %R172, %block_401ce0 ]
  %R192 = phi i64 [ %R171, %block_401ce0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R191 = phi i64 [ undef, %block_401ce0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R190 = phi i64 [ undef, %block_401ce0 ]
  ; # 401ce9: mov    rdi,QWORD PTR [rbx+0xa8]
  ; r199 := (bv_add r192 0xa8 :: [64])
  %R199 = add i64 %R192, 168
  ; r200 := *r199
  %r246 = inttoptr i64 %R199 to i64*
  %R200 = load i64* %r246
  ; # 401cf0: or     r12d,eax
  ; # 401cf3: test   rdi,rdi
  ; r201 := (bv_eq r200 0x0 :: [64])
  %R201 = icmp eq i64 %R200, 0
  ; # 401cf6: je     401cfd
  br i1 %R201, label %subblock_401ce9_1, label %subblock_401ce9_2
subblock_401ce9_1:
  br label %block_401cfd
subblock_401ce9_2:
  br label %block_401cf8
block_401cf8:
  %R211 = phi i128 [ %R198, %subblock_401ce9_2 ]
  %R210 = phi i64 [ %R197, %subblock_401ce9_2 ]
  %R209 = phi i64 [ %R196, %subblock_401ce9_2 ]
  %R208 = phi i64 [ %R195, %subblock_401ce9_2 ]
  %R207 = phi i64 [ %R200, %subblock_401ce9_2 ]
  %R206 = phi i64 [ %R194, %subblock_401ce9_2 ]
  %R205 = phi i64 [ %R193, %subblock_401ce9_2 ]
  %R204 = phi i64 [ %R192, %subblock_401ce9_2 ]
  %R203 = phi i64 [ %R191, %subblock_401ce9_2 ]
  %R202 = phi i64 [ %R190, %subblock_401ce9_2 ]
  ; # 401cf8: call   400e40
  %r259 = bitcast i128 %R211 to <2 x double>
  %r260 = call { i64, i64, <2 x double> } @F400e40(i64 %R207, i64 %R206, i64 %R203, i64 %R202, i64 %R208, i64 %R209, <2 x double> %r259)
  %R212 = extractvalue { i64, i64, <2 x double> } %r260, 0
  %R213 = extractvalue { i64, i64, <2 x double> } %r260, 1
  %r263 = extractvalue { i64, i64, <2 x double> } %r260, 2
  %R214 = bitcast <2 x double> %r263 to i128
  br label %block_401cfd
block_401cfd:
  %R223 = phi i128 [ %R214, %block_401cf8 ], [ %R198, %subblock_401ce9_1 ]
  %R222 = phi i64 [ %R210, %block_401cf8 ], [ %R197, %subblock_401ce9_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R221 = phi i64 [ undef, %block_401cf8 ], [ %R196, %subblock_401ce9_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R220 = phi i64 [ undef, %block_401cf8 ], [ %R195, %subblock_401ce9_1 ]
  %R219 = phi i64 [ %R213, %block_401cf8 ], [ %R194, %subblock_401ce9_1 ]
  %R218 = phi i64 [ %R205, %block_401cf8 ], [ %R193, %subblock_401ce9_1 ]
  %R217 = phi i64 [ %R204, %block_401cf8 ], [ %R192, %subblock_401ce9_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R216 = phi i64 [ undef, %block_401cf8 ], [ %R191, %subblock_401ce9_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R215 = phi i64 [ undef, %block_401cf8 ], [ %R190, %subblock_401ce9_1 ]
  ; # 401cfd: test   ebp,ebp
  ; r224 := (trunc r218 32)
  %R224 = trunc i64 %R218 to i32
  ; r225 := (bv_eq r224 0x0 :: [32])
  %R225 = icmp eq i32 %R224, 0
  ; # 401cff: jne    401d0b
  ; r226 := (bv_complement r225)
  %R226 = xor i1 %R225, -1
  br i1 %R226, label %subblock_401cfd_1, label %subblock_401cfd_2
subblock_401cfd_1:
  br label %block_401d0b
subblock_401cfd_2:
  br label %block_401d01
block_401d01:
  %R233 = phi i128 [ %R223, %subblock_401cfd_2 ]
  %R232 = phi i64 [ %R221, %subblock_401cfd_2 ]
  %R231 = phi i64 [ %R220, %subblock_401cfd_2 ]
  %R230 = phi i64 [ %R219, %subblock_401cfd_2 ]
  %R229 = phi i64 [ %R217, %subblock_401cfd_2 ]
  %R228 = phi i64 [ %R216, %subblock_401cfd_2 ]
  %R227 = phi i64 [ %R215, %subblock_401cfd_2 ]
  ; # 401d01: mov    rdi,rbx
  ; # 401d04: call   400e40
  %r284 = bitcast i128 %R233 to <2 x double>
  %r285 = call { i64, i64, <2 x double> } @F400e40(i64 %R229, i64 %R230, i64 %R228, i64 %R227, i64 %R231, i64 %R232, <2 x double> %r284)
  %R234 = extractvalue { i64, i64, <2 x double> } %r285, 0
  %R235 = extractvalue { i64, i64, <2 x double> } %r285, 1
  %r288 = extractvalue { i64, i64, <2 x double> } %r285, 2
  %R236 = bitcast <2 x double> %r288 to i128
  br label %block_401d09
block_401d09:
  %R237 = phi i128 [ %R236, %block_401d01 ]
  ; # 401d09: jmp    401d18
  br label %block_401d18
block_401d0b:
  %R245 = phi i128 [ %R223, %subblock_401cfd_1 ]
  %R244 = phi i64 [ %R222, %subblock_401cfd_1 ]
  %R243 = phi i64 [ %R221, %subblock_401cfd_1 ]
  %R242 = phi i64 [ %R220, %subblock_401cfd_1 ]
  %R241 = phi i64 [ %R219, %subblock_401cfd_1 ]
  %R240 = phi i64 [ %R217, %subblock_401cfd_1 ]
  %R239 = phi i64 [ %R216, %subblock_401cfd_1 ]
  %R238 = phi i64 [ %R215, %subblock_401cfd_1 ]
  ; # 401d0b: test   r13d,r13d
  ; r246 := (trunc r244 32)
  %R246 = trunc i64 %R244 to i32
  ; r247 := (bv_eq r246 0x0 :: [32])
  %R247 = icmp eq i32 %R246, 0
  ; # 401d0e: je     401d18
  br i1 %R247, label %subblock_401d0b_1, label %subblock_401d0b_2
subblock_401d0b_1:
  br label %block_401d18
subblock_401d0b_2:
  br label %block_401d10
block_401d10:
  %R254 = phi i128 [ %R245, %subblock_401d0b_2 ]
  %R253 = phi i64 [ %R243, %subblock_401d0b_2 ]
  %R252 = phi i64 [ %R242, %subblock_401d0b_2 ]
  %R251 = phi i64 [ %R241, %subblock_401d0b_2 ]
  %R250 = phi i64 [ %R240, %subblock_401d0b_2 ]
  %R249 = phi i64 [ %R239, %subblock_401d0b_2 ]
  %R248 = phi i64 [ %R238, %subblock_401d0b_2 ]
  ; # 401d10: mov    rdi,rbx
  ; # 401d13: call   402a80
  %r308 = bitcast i128 %R254 to <2 x double>
  %r309 = call { i64, i64, <2 x double> } @F402a80(i64 %R250, i64 %R251, i64 %R249, i64 %R248, i64 %R252, i64 %R253, <2 x double> %r308)
  %R255 = extractvalue { i64, i64, <2 x double> } %r309, 0
  %R256 = extractvalue { i64, i64, <2 x double> } %r309, 1
  %r312 = extractvalue { i64, i64, <2 x double> } %r309, 2
  %R257 = bitcast <2 x double> %r312 to i128
  br label %block_401d18
block_401d18:
  %R258 = phi i128 [ %R237, %block_401d09 ], [ %R257, %block_401d10 ], [ %R245, %subblock_401d0b_1 ]
  ; # 401d18: pop    rdx
  ; # 401d19: mov    eax,r12d
  ; # 401d1c: pop    rbx
  ; # 401d1d: pop    rbp
  ; # 401d1e: pop    r12
  ; # 401d20: pop    r13
  ; # 401d22: ret
  %r315 = bitcast i128 %R258 to <2 x double>
  %r316 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r317 = insertvalue { i64, i64, <2 x double> } %r316, i64 undef, 1
  %r318 = insertvalue { i64, i64, <2 x double> } %r317, <2 x double> %r315, 2
  ret { i64, i64, <2 x double> } %r318
failure:
  br label %failure
}