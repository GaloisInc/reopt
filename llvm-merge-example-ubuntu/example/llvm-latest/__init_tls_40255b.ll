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
declare { i64, i64, <2 x double> } @F402496(i64)
declare { i64, i64, <2 x double> } @F4024e2(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F40255b(i64 %a0, <2 x double> %a1) {
entry:
  %r0 = bitcast <2 x double> %a1 to i128
  br label %block_40255b
block_40255b:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 40255b: mov    r8,QWORD PTR [rdi+0x18]
  ; r2 := (bv_add arg0 0x18 :: [64])
  %R2 = add i64 %a0, 24
  ; r3 := *r2
  %r5 = inttoptr i64 %R2 to i64*
  %R3 = load i64* %r5
  ; # 40255f: mov    rsi,QWORD PTR [rdi+0x28]
  ; r4 := (bv_add arg0 0x28 :: [64])
  %R4 = add i64 %a0, 40
  ; r5 := *r4
  %r8 = inttoptr i64 %R4 to i64*
  %R5 = load i64* %r8
  ; # 402563: xor    ecx,ecx
  ; # 402565: xor    eax,eax
  ; # 402567: mov    rdx,r8
  br label %block_40256a
block_40256a:
  %R13 = phi i128 [ %R56, %block_402589 ], [ %r0, %block_40255b ]
  %R12 = phi i64 [ %R55, %block_402589 ], [ %R3, %block_40255b ]
  %R11 = phi i64 [ %R54, %block_402589 ], [ %a0, %block_40255b ]
  %R10 = phi i64 [ %R57, %block_402589 ], [ %R5, %block_40255b ]
  %R9 = phi i64 [ %R52, %block_402589 ], [ %R1, %block_40255b ]
  %R8 = phi i64 [ %R60, %block_402589 ], [ %R3, %block_40255b ]
  %R7 = phi i64 [ %R50, %block_402589 ], [ 0, %block_40255b ]
  %R6 = phi i64 [ %R49, %block_402589 ], [ 0, %block_40255b ]
  ; # 40256a: test   rsi,rsi
  ; r14 := (bv_eq r10 0x0 :: [64])
  %R14 = icmp eq i64 %R10, 0
  ; # 40256d: je     402592
  br i1 %R14, label %subblock_40256a_1, label %subblock_40256a_2
subblock_40256a_1:
  br label %block_402592
subblock_40256a_2:
  br label %block_40256f
block_40256f:
  %R22 = phi i128 [ %R13, %subblock_40256a_2 ]
  %R21 = phi i64 [ %R12, %subblock_40256a_2 ]
  %R20 = phi i64 [ %R11, %subblock_40256a_2 ]
  %R19 = phi i64 [ %R10, %subblock_40256a_2 ]
  %R18 = phi i64 [ %R9, %subblock_40256a_2 ]
  %R17 = phi i64 [ %R8, %subblock_40256a_2 ]
  %R16 = phi i64 [ %R7, %subblock_40256a_2 ]
  %R15 = phi i64 [ %R6, %subblock_40256a_2 ]
  ; # 40256f: mov    r9d,DWORD PTR [rdx]
  ; r23 := *r17
  %r27 = inttoptr i64 %R17 to i32*
  %R23 = load i32* %r27
  ; r24 := (uext r23 64)
  %R24 = zext i32 %R23 to i64
  ; # 402572: cmp    r9d,0x6
  ; r25 := (bv_eq r23 0x6 :: [32])
  %R25 = icmp eq i32 %R23, 6
  ; # 402576: jne    402581
  ; r26 := (bv_complement r25)
  %R26 = xor i1 %R25, -1
  br i1 %R26, label %subblock_40256f_1, label %subblock_40256f_2
subblock_40256f_1:
  br label %block_402581
subblock_40256f_2:
  br label %block_402578
block_402578:
  %R33 = phi i128 [ %R22, %subblock_40256f_2 ]
  %R32 = phi i64 [ %R21, %subblock_40256f_2 ]
  %R31 = phi i64 [ %R20, %subblock_40256f_2 ]
  %R30 = phi i64 [ %R19, %subblock_40256f_2 ]
  %R29 = phi i64 [ %R18, %subblock_40256f_2 ]
  %R28 = phi i64 [ %R17, %subblock_40256f_2 ]
  %R27 = phi i64 [ %R15, %subblock_40256f_2 ]
  ; # 402578: mov    rcx,r8
  ; # 40257b: sub    rcx,QWORD PTR [rdx+0x10]
  ; r34 := (bv_add r28 0x10 :: [64])
  %R34 = add i64 %R28, 16
  ; r35 := *r34
  %r40 = inttoptr i64 %R34 to i64*
  %R35 = load i64* %r40
  ; r36 := (bv_sub r32 r35)
  %R36 = sub i64 %R32, %R35
  ; # 40257f: jmp    402589
  br label %block_402589
block_402581:
  %R45 = phi i128 [ %R22, %subblock_40256f_1 ]
  %R44 = phi i64 [ %R24, %subblock_40256f_1 ]
  %R43 = phi i64 [ %R21, %subblock_40256f_1 ]
  %R42 = phi i64 [ %R20, %subblock_40256f_1 ]
  %R41 = phi i64 [ %R19, %subblock_40256f_1 ]
  %R40 = phi i64 [ %R18, %subblock_40256f_1 ]
  %R39 = phi i64 [ %R17, %subblock_40256f_1 ]
  %R38 = phi i64 [ %R16, %subblock_40256f_1 ]
  %R37 = phi i64 [ %R15, %subblock_40256f_1 ]
  ; # 402581: cmp    r9d,0x7
  ; r46 := (trunc r44 32)
  %R46 = trunc i64 %R44 to i32
  ; r47 := (bv_eq r46 0x7 :: [32])
  %R47 = icmp eq i32 %R46, 7
  ; # 402585: cmove  rax,rdx
  ; r48 := (mux r47 r39 r37)
  %R48 = select i1 %R47, i64 %R39, i64 %R37
  br label %block_402589
block_402589:
  %R56 = phi i128 [ %R33, %block_402578 ], [ %R45, %block_402581 ]
  %R55 = phi i64 [ %R32, %block_402578 ], [ %R43, %block_402581 ]
  %R54 = phi i64 [ %R31, %block_402578 ], [ %R42, %block_402581 ]
  %R53 = phi i64 [ %R30, %block_402578 ], [ %R41, %block_402581 ]
  %R52 = phi i64 [ %R29, %block_402578 ], [ %R40, %block_402581 ]
  %R51 = phi i64 [ %R28, %block_402578 ], [ %R39, %block_402581 ]
  %R50 = phi i64 [ %R36, %block_402578 ], [ %R38, %block_402581 ]
  %R49 = phi i64 [ %R27, %block_402578 ], [ %R48, %block_402581 ]
  ; # 402589: dec    rsi
  ; r57 := (bv_add r53 0xffffffffffffffff :: [64])
  %R57 = add i64 %R53, 18446744073709551615
  ; # 40258c: add    rdx,QWORD PTR [rdi+0x20]
  ; r58 := (bv_add r54 0x20 :: [64])
  %R58 = add i64 %R54, 32
  ; r59 := *r58
  %r65 = inttoptr i64 %R58 to i64*
  %R59 = load i64* %r65
  ; r60 := (bv_add r51 r59)
  %R60 = add i64 %R51, %R59
  ; # 402590: jmp    40256a
  br label %block_40256a
block_402592:
  %R65 = phi i128 [ %R13, %subblock_40256a_1 ]
  %R64 = phi i64 [ %R9, %subblock_40256a_1 ]
  %R63 = phi i64 [ %R8, %subblock_40256a_1 ]
  %R62 = phi i64 [ %R7, %subblock_40256a_1 ]
  %R61 = phi i64 [ %R6, %subblock_40256a_1 ]
  ; # 402592: test   rax,rax
  ; r66 := (bv_eq r61 0x0 :: [64])
  %R66 = icmp eq i64 %R61, 0
  ; # 402595: push   rdx
  ; r67 := (bv_add r64 0xfffffffffffffff8 :: [64])
  %R67 = add i64 %R64, 18446744073709551608
  ; *(r67) = r63
  %r75 = inttoptr i64 %R67 to i64*
  store i64 %R63, i64* %r75
  ; # 402596: je     4025da
  br i1 %R66, label %subblock_402592_1, label %subblock_402592_2
subblock_402592_1:
  br label %block_4025da
subblock_402592_2:
  br label %block_402598
block_402598:
  %R71 = phi i128 [ %R65, %subblock_402592_2 ]
  %R70 = phi i64 [ %R67, %subblock_402592_2 ]
  %R69 = phi i64 [ %R62, %subblock_402592_2 ]
  %R68 = phi i64 [ %R61, %subblock_402592_2 ]
  ; # 402598: mov    rdx,QWORD PTR [rax+0x20]
  ; r72 := (bv_add r68 0x20 :: [64])
  %R72 = add i64 %R68, 32
  ; r73 := *r72
  %r81 = inttoptr i64 %R72 to i64*
  %R73 = load i64* %r81
  ; # 40259c: add    rcx,QWORD PTR [rax+0x10]
  ; r74 := (bv_add r68 0x10 :: [64])
  %R74 = add i64 %R68, 16
  ; r75 := *r74
  %r84 = inttoptr i64 %R74 to i64*
  %R75 = load i64* %r84
  ; r76 := (bv_add r69 r75)
  %R76 = add i64 %R69, %R75
  ; # 4025a0: mov    QWORD PTR [rip+0x2023ed],0x604720
  ; *(0x604998 :: [64]) = 0x604720 :: [64]
  %r87 = inttoptr i64 6310296 to i64*
  store i64 6309664, i64* %r87
  ; # 4025ab: mov    QWORD PTR [rip+0x20217e],rdx
  ; *(0x604730 :: [64]) = r73
  %r88 = inttoptr i64 6309680 to i64*
  store i64 %R73, i64* %r88
  ; # 4025b2: mov    rdx,QWORD PTR [rax+0x28]
  ; r77 := (bv_add r68 0x28 :: [64])
  %R77 = add i64 %R68, 40
  ; r78 := *r77
  %r90 = inttoptr i64 %R77 to i64*
  %R78 = load i64* %r90
  ; # 4025b6: mov    rax,QWORD PTR [rax+0x30]
  ; r79 := (bv_add r68 0x30 :: [64])
  %R79 = add i64 %R68, 48
  ; r80 := *r79
  %r93 = inttoptr i64 %R79 to i64*
  %R80 = load i64* %r93
  ; # 4025ba: mov    QWORD PTR [rip+0x202167],rcx
  ; *(0x604728 :: [64]) = r76
  %r95 = inttoptr i64 6309672 to i64*
  store i64 %R76, i64* %r95
  ; # 4025c1: mov    QWORD PTR [rip+0x2023e4],0x1
  ; *(0x6049b0 :: [64]) = 0x1 :: [64]
  %r96 = inttoptr i64 6310320 to i64*
  store i64 1, i64* %r96
  ; # 4025cc: mov    QWORD PTR [rip+0x202165],rdx
  ; *(0x604738 :: [64]) = r78
  %r97 = inttoptr i64 6309688 to i64*
  store i64 %R78, i64* %r97
  ; # 4025d3: mov    QWORD PTR [rip+0x202166],rax
  ; *(0x604740 :: [64]) = r80
  %r98 = inttoptr i64 6309696 to i64*
  store i64 %R80, i64* %r98
  br label %block_4025da
block_4025da:
  %R82 = phi i128 [ %R71, %block_402598 ], [ %R65, %subblock_402592_1 ]
  %R81 = phi i64 [ %R70, %block_402598 ], [ %R67, %subblock_402592_1 ]
  ; # 4025da: mov    rdx,QWORD PTR [rip+0x202157]
  ; r83 := *0x604738 :: [64]
  %r101 = inttoptr i64 6309688 to i64*
  %R83 = load i64* %r101
  ; # 4025e1: mov    rsi,QWORD PTR [rip+0x202158]
  ; r84 := *0x604740 :: [64]
  %r103 = inttoptr i64 6309696 to i64*
  %R84 = load i64* %r103
  ; # 4025e8: mov    rax,rdx
  ; # 4025eb: add    rax,QWORD PTR [rip+0x202136]
  ; r85 := *0x604728 :: [64]
  %r105 = inttoptr i64 6309672 to i64*
  %R85 = load i64* %r105
  ; r86 := (bv_add r83 r85)
  %R86 = add i64 %R83, %R85
  ; # 4025f2: lea    rcx,[rsi-0x1]
  ; r87 := (bv_add r84 0xffffffffffffffff :: [64])
  %R87 = add i64 %R84, 18446744073709551615
  ; # 4025f6: neg    rax
  ; r88 := (bv_sub 0x0 :: [64] r86)
  %R88 = sub i64 0, %R86
  ; # 4025f9: and    rax,rcx
  ; r89 := (bv_and r88 r87)
  %R89 = and i64 %R88, %R87
  ; # 4025fc: add    rax,rdx
  ; r90 := (bv_add r89 r83)
  %R90 = add i64 %R89, %R83
  ; # 4025ff: cmp    rsi,0x7
  ; # 402603: mov    QWORD PTR [rip+0x20212e],rax
  ; *(0x604738 :: [64]) = r90
  %r112 = inttoptr i64 6309688 to i64*
  store i64 %R90, i64* %r112
  ; # 40260a: ja     402617
  ; r91 := (bv_ult 0x7 :: [64] r84)
  %R91 = icmp ult i64 7, %R84
  br i1 %R91, label %subblock_4025da_1, label %subblock_4025da_2
subblock_4025da_1:
  br label %block_402617
subblock_4025da_2:
  br label %block_40260c
block_40260c:
  %R95 = phi i128 [ %R82, %subblock_4025da_2 ]
  %R94 = phi i64 [ %R81, %subblock_4025da_2 ]
  %R93 = phi i64 [ %R87, %subblock_4025da_2 ]
  %R92 = phi i64 [ %R90, %subblock_4025da_2 ]
  ; # 40260c: mov    QWORD PTR [rip+0x202129],0x8
  ; *(0x604740 :: [64]) = 0x8 :: [64]
  %r118 = inttoptr i64 6309696 to i64*
  store i64 8, i64* %r118
  br label %block_402617
block_402617:
  %R99 = phi i128 [ %R95, %block_40260c ], [ %R82, %subblock_4025da_1 ]
  %R98 = phi i64 [ %R94, %block_40260c ], [ %R81, %subblock_4025da_1 ]
  %R97 = phi i64 [ %R93, %block_40260c ], [ %R87, %subblock_4025da_1 ]
  %R96 = phi i64 [ %R92, %block_40260c ], [ %R90, %subblock_4025da_1 ]
  ; # 402617: mov    rdx,QWORD PTR [rip+0x202122]
  ; r100 := *0x604740 :: [64]
  %r123 = inttoptr i64 6309696 to i64*
  %R100 = load i64* %r123
  ; # 40261e: mov    QWORD PTR [rip+0x202123],rax
  ; *(0x604748 :: [64]) = r96
  %r125 = inttoptr i64 6309704 to i64*
  store i64 %R96, i64* %r125
  ; # 402625: mov    edi,0x604760
  ; # 40262a: lea    rax,[rax+rdx*1+0x167]
  ; r101 := (bv_add r96 r100)
  %R101 = add i64 %R96, %R100
  ; r102 := (bv_add r101 0x167 :: [64])
  %R102 = add i64 %R101, 359
  ; # 402632: mov    QWORD PTR [rip+0x20236f],rdx
  ; *(0x6049a8 :: [64]) = r100
  %r128 = inttoptr i64 6310312 to i64*
  store i64 %R100, i64* %r128
  ; # 402639: and    rax,0xfffffffffffffff8
  ; r103 := (bv_and r102 0xfffffffffffffff8 :: [64])
  %R103 = and i64 %R102, 18446744073709551608
  ; # 40263d: cmp    rax,0x1d8
  ; # 402643: mov    rsi,rax
  ; # 402646: mov    QWORD PTR [rip+0x202353],rax
  ; *(0x6049a0 :: [64]) = r103
  %r130 = inttoptr i64 6310304 to i64*
  store i64 %R103, i64* %r130
  ; # 40264d: jbe    40266d
  ; r104 := (bv_ule r103 0x1d8 :: [64])
  %R104 = icmp ule i64 %R103, 472
  br i1 %R104, label %subblock_402617_1, label %subblock_402617_2
subblock_402617_1:
  br label %block_40266d
subblock_402617_2:
  br label %block_40264f
block_40264f:
  %R106 = phi i64 [ %R103, %subblock_402617_2 ]
  %R105 = phi i64 [ %R98, %subblock_402617_2 ]
  ; # 40264f: mov    r10d,0x22
  ; # 402655: or     r8,0xffffffffffffffff
  ; # 402659: xor    r9d,r9d
  ; # 40265c: mov    edx,0x3
  ; # 402661: xor    edi,edi
  ; # 402663: mov    eax,0x9
  ; # 402668: syscall
  ; sys_mmap
  %r134 = call { i64, i1 } @reopt.SystemCall.Linux(i64 0, i64 %R106, i64 3, i64 34, i64 18446744073709551615, i64 0, i64 9)
  %R107 = extractvalue { i64, i1 } %r134, 0
  br label %block_40266a
block_40266a:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R113 = phi i128 [ undef, %block_40264f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R112 = phi i64 [ undef, %block_40264f ]
  %R111 = phi i64 [ %R105, %block_40264f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R110 = phi i64 [ undef, %block_40264f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rcx
  %R109 = phi i64 [ undef, %block_40264f ]
  %R108 = phi i64 [ %R107, %block_40264f ]
  ; # 40266a: mov    rdi,rax
  br label %block_40266d
block_40266d:
  %R119 = phi i128 [ %R113, %block_40266a ], [ %R99, %subblock_402617_1 ]
  %R118 = phi i64 [ %R108, %block_40266a ], [ 6309728, %subblock_402617_1 ]
  %R117 = phi i64 [ %R112, %block_40266a ], [ %R103, %subblock_402617_1 ]
  %R116 = phi i64 [ %R111, %block_40266a ], [ %R98, %subblock_402617_1 ]
  %R115 = phi i64 [ %R110, %block_40266a ], [ %R100, %subblock_402617_1 ]
  %R114 = phi i64 [ %R109, %block_40266a ], [ %R97, %subblock_402617_1 ]
  ; # 40266d: call   4024e2
  ; r120 := (bv_add r116 0xfffffffffffffff8 :: [64])
  %R120 = add i64 %R116, 18446744073709551608
  ; r122 := (bv_add r120 0x8 :: [64])
  %R122 = add i64 %R120, 8
  %r150 = bitcast i128 %R119 to <2 x double>
  %r151 = call { i64, i64, <2 x double> } @F4024e2(i64 %R118, i64 %R117, i64 %R115, i64 %R114, <2 x double> %r150)
  %R121 = extractvalue { i64, i64, <2 x double> } %r151, 0
  br label %block_402672
block_402672:
  %R124 = phi i64 [ %R122, %block_40266d ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R123 = phi i64 [ undef, %block_40266d ]
  ; # 402672: mov    rdi,rax
  ; # 402675: call   402496
  ; r125 := (bv_add r124 0xfffffffffffffff8 :: [64])
  %R125 = add i64 %R124, 18446744073709551608
  ; r129 := (bv_add r125 0x8 :: [64])
  %R129 = add i64 %R125, 8
  %r157 = call { i64, i64, <2 x double> } @F402496(i64 %R123)
  %R126 = extractvalue { i64, i64, <2 x double> } %r157, 0
  %R127 = extractvalue { i64, i64, <2 x double> } %r157, 1
  %r160 = extractvalue { i64, i64, <2 x double> } %r157, 2
  %R128 = bitcast <2 x double> %r160 to i128
  br label %block_40267a
block_40267a:
  %R133 = phi i128 [ %R128, %block_402672 ]
  %R132 = phi i64 [ %R129, %block_402672 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R131 = phi i64 [ undef, %block_402672 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R130 = phi i64 [ undef, %block_402672 ]
  ; # 40267a: test   eax,eax
  ; r134 := (trunc r130 32)
  %R134 = trunc i64 %R130 to i32
  ; # 40267c: jns    40267f
  ; r135 := (bv_sle 0x0 :: [32] r134)
  %R135 = icmp sle i32 0, %R134
  br i1 %R135, label %subblock_40267a_1, label %subblock_40267a_2
subblock_40267a_1:
  br label %block_40267f
subblock_40267a_2:
  br label %block_40267e
block_40267e:
  %R138 = phi i128 [ %R133, %subblock_40267a_2 ]
  %R137 = phi i64 [ %R132, %subblock_40267a_2 ]
  %R136 = phi i64 [ %R131, %subblock_40267a_2 ]
  ; # 40267e: hlt
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception GeneralProtectionException 0 ()
  br label %block_40267f
block_40267f:
  %R141 = phi i128 [ %R138, %block_40267e ], [ %R133, %subblock_40267a_1 ]
  %R140 = phi i64 [ %R137, %block_40267e ], [ %R132, %subblock_40267a_1 ]
  %R139 = phi i64 [ %R136, %block_40267e ], [ %R131, %subblock_40267a_1 ]
  ; # 40267f: pop    rax
  ; r142 := *r140
  %r174 = inttoptr i64 %R140 to i64*
  %R142 = load i64* %r174
  ; # 402680: ret
  %r176 = bitcast i128 %R141 to <2 x double>
  %r177 = insertvalue { i64, i64, <2 x double> } undef, i64 %R142, 0
  %r178 = insertvalue { i64, i64, <2 x double> } %r177, i64 %R139, 1
  %r179 = insertvalue { i64, i64, <2 x double> } %r178, <2 x double> %r176, 2
  ret { i64, i64, <2 x double> } %r179
failure:
  br label %failure
}