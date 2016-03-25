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
declare { i64, i64, <2 x double> } @F400bf0(i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400e40(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F401b20(i64, i64, i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F40241c(i64, i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402681(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4026e0(i64, <2 x double>)
define { i64, i64, <2 x double> } @F401280(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_401280
block_401280:
  ; r0 := (alloca 0x70 :: [64])
  %r1 = alloca i8, i64 112
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x70 :: [64])
  %R1 = add i64 %R0, 112
  ; # 401280: push   r15
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401282: lea    rdx,[rdi-0x1]
  ; r3 := (bv_add arg0 0xffffffffffffffff :: [64])
  %R3 = add i64 %a0, 18446744073709551615
  ; # 401286: push   r14
  ; r4 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R4 = add i64 %R1, 18446744073709551600
  ; # 401288: push   r13
  ; r5 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R5 = add i64 %R1, 18446744073709551592
  ; # 40128a: push   r12
  ; r6 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R6 = add i64 %R1, 18446744073709551584
  ; # 40128c: mov    rax,0x7fffffffffffefdf
  ; # 401296: push   rbp
  ; r7 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R7 = add i64 %R1, 18446744073709551576
  ; # 401297: push   rbx
  ; r8 := (bv_add r1 0xffffffffffffffd0 :: [64])
  %R8 = add i64 %R1, 18446744073709551568
  ; # 401298: sub    rsp,0x38
  ; r9 := (bv_add r1 0xffffffffffffff98 :: [64])
  %R9 = add i64 %R1, 18446744073709551512
  ; # 40129c: cmp    rdx,rax
  ; r10 := (bv_ult r3 0x7fffffffffffefdf :: [64])
  %R10 = icmp ult i64 %R3, 9223372036854771679
  ; r11 := (bv_eq arg0 0x7fffffffffffefe0 :: [64])
  %R11 = icmp eq i64 %a0, 9223372036854771680
  ; # 40129f: jbe    4014a0
  ; r12 := (bv_or r10 r11)
  %R12 = or i1 %R10, %R11
  br i1 %R12, label %subblock_401280_1, label %subblock_401280_2
subblock_401280_1:
  br label %block_4014a0
subblock_401280_2:
  br label %block_4012a5
block_4012a5:
  %R20 = phi i128 [ %r0, %subblock_401280_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r10
  %R19 = phi i64 [ undef, %subblock_401280_2 ]
  %R18 = phi i64 [ %a5, %subblock_401280_2 ]
  %R17 = phi i64 [ %a4, %subblock_401280_2 ]
  %R16 = phi i64 [ %a0, %subblock_401280_2 ]
  %R15 = phi i64 [ %a1, %subblock_401280_2 ]
  %R14 = phi i64 [ %R9, %subblock_401280_2 ]
  %R13 = phi i64 [ %R3, %subblock_401280_2 ]
  ; # 4012a5: test   rdi,rdi
  ; r21 := (bv_eq r16 0x0 :: [64])
  %R21 = icmp eq i64 %R16, 0
  ; # 4012a8: jne    4017be
  ; r22 := (bv_complement r21)
  %R22 = xor i1 %R21, -1
  br i1 %R22, label %subblock_4012a5_1, label %subblock_4012a5_2
subblock_4012a5_1:
  br label %block_4017be
subblock_4012a5_2:
  br label %block_4012ae
block_4012ae:
  %R27 = phi i128 [ %R20, %subblock_4012a5_2 ]
  %R26 = phi i64 [ %R19, %subblock_4012a5_2 ]
  %R25 = phi i64 [ %R18, %subblock_4012a5_2 ]
  %R24 = phi i64 [ %R17, %subblock_4012a5_2 ]
  %R23 = phi i64 [ %R14, %subblock_4012a5_2 ]
  ; # 4012ae: mov    QWORD PTR [rsp+0x8],0xffffffffffffffff
  ; r28 := (bv_add r23 0x8 :: [64])
  %R28 = add i64 %R23, 8
  ; *(r28) = 0xffffffffffffffff :: [64]
  %r31 = inttoptr i64 %R28 to i64*
  store i64 18446744073709551615, i64* %r31
  ; # 4012b7: mov    DWORD PTR [rsp+0x1c],0x0
  ; r29 := (bv_add r23 0x1c :: [64])
  %R29 = add i64 %R23, 28
  ; *(r29) = 0x0 :: [32]
  %r33 = inttoptr i64 %R29 to i32*
  store i32 0, i32* %r33
  ; # 4012bf: mov    QWORD PTR [rsp+0x10],0x20
  ; r30 := (bv_add r23 0x10 :: [64])
  %R30 = add i64 %R23, 16
  ; *(r30) = 0x20 :: [64]
  %r35 = inttoptr i64 %R30 to i64*
  store i64 32, i64* %r35
  br label %block_4012c8
block_4012c8:
  %R35 = phi i128 [ %R27, %block_4012ae ], [ %R710, %block_401718 ], [ %R891, %block_401850 ]
  %R34 = phi i64 [ %R26, %block_4012ae ], [ %R708, %block_401718 ], [ %R889, %block_401850 ]
  %R33 = phi i64 [ %R25, %block_4012ae ], [ %R707, %block_401718 ], [ %R888, %block_401850 ]
  %R32 = phi i64 [ %R24, %block_4012ae ], [ %R706, %block_401718 ], [ %R887, %block_401850 ]
  %R31 = phi i64 [ %R23, %block_4012ae ], [ %R705, %block_401718 ], [ %R886, %block_401850 ]
  ; # 4012c8: mov    r12d,0x1
  ; # 4012ce: nop
  br label %block_4012d0
block_4012d0:
  %R41 = phi i128 [ %R134, %subblock_40135d_1 ], [ %R114, %subblock_401333_1 ], [ %R103, %subblock_40132c_1 ], [ %R35, %block_4012c8 ]
  %R40 = phi i64 [ %R133, %subblock_40135d_1 ], [ %R112, %subblock_401333_1 ], [ %R101, %subblock_40132c_1 ], [ 1, %block_4012c8 ]
  %R39 = phi i64 [ %R132, %subblock_40135d_1 ], [ %R111, %subblock_401333_1 ], [ %R100, %subblock_40132c_1 ], [ %R34, %block_4012c8 ]
  %R38 = phi i64 [ %R131, %subblock_40135d_1 ], [ %R110, %subblock_401333_1 ], [ %R99, %subblock_40132c_1 ], [ %R33, %block_4012c8 ]
  %R37 = phi i64 [ %R130, %subblock_40135d_1 ], [ %R109, %subblock_401333_1 ], [ %R98, %subblock_40132c_1 ], [ %R32, %block_4012c8 ]
  %R36 = phi i64 [ %R128, %subblock_40135d_1 ], [ %R107, %subblock_401333_1 ], [ %R96, %subblock_40132c_1 ], [ %R31, %block_4012c8 ]
  ; # 4012d0: mov    rbx,QWORD PTR [rip+0x202e09]
  ; r42 := *0x6040e0 :: [64]
  %r47 = inttoptr i64 6308064 to i64*
  %R42 = load i64* %r47
  ; # 4012d7: and    rbx,QWORD PTR [rsp+0x8]
  ; r43 := (bv_add r36 0x8 :: [64])
  %R43 = add i64 %R36, 8
  ; r44 := *r43
  %r50 = inttoptr i64 %R43 to i64*
  %R44 = load i64* %r50
  ; r45 := (bv_and r42 r44)
  %R45 = and i64 %R42, %R44
  ; r46 := (bv_eq r45 0x0 :: [64])
  %R46 = icmp eq i64 %R45, 0
  ; # 4012dc: je     401388
  br i1 %R46, label %subblock_4012d0_1, label %subblock_4012d0_2
subblock_4012d0_1:
  br label %block_401388
subblock_4012d0_2:
  br label %block_4012e2
block_4012e2:
  %R53 = phi i128 [ %R150, %subblock_40136f_1 ], [ %R41, %subblock_4012d0_2 ]
  %R52 = phi i64 [ %R149, %subblock_40136f_1 ], [ %R40, %subblock_4012d0_2 ]
  %R51 = phi i64 [ %R148, %subblock_40136f_1 ], [ %R39, %subblock_4012d0_2 ]
  %R50 = phi i64 [ %R147, %subblock_40136f_1 ], [ %R38, %subblock_4012d0_2 ]
  %R49 = phi i64 [ %R146, %subblock_40136f_1 ], [ %R37, %subblock_4012d0_2 ]
  %R48 = phi i64 [ %R145, %subblock_40136f_1 ], [ %R36, %subblock_4012d0_2 ]
  %R47 = phi i64 [ %R154, %subblock_40136f_1 ], [ %R45, %subblock_4012d0_2 ]
  ; # 4012e2: bsf    rbx,rbx
  ; r54 := (bsf r47)
  %R54 = call i64 @llvm.cttz.i64(i64 %R47, i1 1)
  ; # 4012e6: mov    r15d,ebx
  ; r55 := (trunc r54 32)
  %R55 = trunc i64 %R54 to i32
  ; r56 := (uext r55 64)
  %R56 = zext i32 %R55 to i64
  ; # 4012e9: movsxd rbx,ebx
  ; r57 := (sext r55 64)
  %R57 = sext i32 %R55 to i64
  ; # 4012ec: lea    rax,[rbx+rbx*2]
  ; r58 := (bv_mul 0x2 :: [64] r57)
  %R58 = mul i64 2, %R57
  ; r59 := (bv_add r57 r58)
  %R59 = add i64 %R57, %R58
  ; # 4012f0: lea    rbp,[rax*8+0x6040e0]
  ; r60 := (bv_mul 0x8 :: [64] r59)
  %R60 = mul i64 8, %R59
  ; r61 := (bv_add r60 0x6040e0 :: [64])
  %R61 = add i64 %R60, 6308064
  ; # 4012f8: mov    eax,DWORD PTR [rip+0x20368e]
  ; r62 := *0x60498c :: [64]
  %r69 = inttoptr i64 6310284 to i32*
  %R62 = load i32* %r69
  ; # 4012fe: lea    r14,[rbp+0x8]
  ; r63 := (bv_add r60 0x6040e8 :: [64])
  %R63 = add i64 %R60, 6308072
  ; # 401302: test   eax,eax
  ; r64 := (bv_eq r62 0x0 :: [32])
  %R64 = icmp eq i32 %R62, 0
  ; # 401304: jne    401518
  ; r65 := (bv_complement r64)
  %R65 = xor i1 %R64, -1
  br i1 %R65, label %subblock_4012e2_1, label %subblock_4012e2_2
subblock_4012e2_1:
  br label %block_401518
subblock_4012e2_2:
  br label %block_40130a
block_40130a:
  %R75 = phi i128 [ %R416, %block_40154f ], [ %R368, %subblock_401518_1 ], [ %R53, %subblock_4012e2_2 ]
  %R74 = phi i64 [ %R415, %block_40154f ], [ %R367, %subblock_401518_1 ], [ %R56, %subblock_4012e2_2 ]
  %R73 = phi i64 [ %R414, %block_40154f ], [ %R366, %subblock_401518_1 ], [ %R63, %subblock_4012e2_2 ]
  %R72 = phi i64 [ %R413, %block_40154f ], [ %R365, %subblock_401518_1 ], [ %R52, %subblock_4012e2_2 ]
  %R71 = phi i64 [ %R412, %block_40154f ], [ %R364, %subblock_401518_1 ], [ %R51, %subblock_4012e2_2 ]
  %R70 = phi i64 [ %R411, %block_40154f ], [ %R363, %subblock_401518_1 ], [ %R50, %subblock_4012e2_2 ]
  %R69 = phi i64 [ %R410, %block_40154f ], [ %R362, %subblock_401518_1 ], [ %R49, %subblock_4012e2_2 ]
  %R68 = phi i64 [ %R409, %block_40154f ], [ %R361, %subblock_401518_1 ], [ %R61, %subblock_4012e2_2 ]
  %R67 = phi i64 [ %R408, %block_40154f ], [ %R360, %subblock_401518_1 ], [ %R48, %subblock_4012e2_2 ]
  %R66 = phi i64 [ %R407, %block_40154f ], [ %R359, %subblock_401518_1 ], [ %R57, %subblock_4012e2_2 ]
  ; # 40130a: lea    rax,[rbx+rbx*2]
  ; r76 := (bv_mul 0x2 :: [64] r66)
  %R76 = mul i64 2, %R66
  ; r77 := (bv_add r66 r76)
  %R77 = add i64 %R66, %R76
  ; # 40130e: lea    rax,[rax*8+0x6040e0]
  ; r78 := (bv_mul 0x8 :: [64] r77)
  %R78 = mul i64 8, %R77
  ; r79 := (bv_add r78 0x6040e0 :: [64])
  %R79 = add i64 %R78, 6308064
  ; # 401316: mov    rbx,QWORD PTR [rax+0x10]
  ; r80 := (bv_add r78 0x6040f0 :: [64])
  %R80 = add i64 %R78, 6308080
  ; r81 := *r80
  %r89 = inttoptr i64 %R80 to i64*
  %R81 = load i64* %r89
  ; # 40131a: test   rbx,rbx
  ; r82 := (bv_eq r81 0x0 :: [64])
  %R82 = icmp eq i64 %R81, 0
  ; # 40131d: je     401558
  br i1 %R82, label %subblock_40130a_1, label %subblock_40130a_2
subblock_40130a_1:
  br label %block_401558
subblock_40130a_2:
  br label %block_401323
block_401323:
  %R93 = phi i128 [ %R75, %subblock_40130a_2 ]
  %R92 = phi i64 [ %R74, %subblock_40130a_2 ]
  %R91 = phi i64 [ %R73, %subblock_40130a_2 ]
  %R90 = phi i64 [ %R72, %subblock_40130a_2 ]
  %R89 = phi i64 [ %R71, %subblock_40130a_2 ]
  %R88 = phi i64 [ %R70, %subblock_40130a_2 ]
  %R87 = phi i64 [ %R69, %subblock_40130a_2 ]
  %R86 = phi i64 [ %R68, %subblock_40130a_2 ]
  %R85 = phi i64 [ %R67, %subblock_40130a_2 ]
  %R84 = phi i64 [ %R81, %subblock_40130a_2 ]
  %R83 = phi i64 [ %R79, %subblock_40130a_2 ]
  ; # 401323: cmp    rbx,rax
  ; r94 := (bv_eq r84 r83)
  %R94 = icmp eq i64 %R84, %R83
  ; # 401326: jne    401568
  ; r95 := (bv_complement r94)
  %R95 = xor i1 %R94, -1
  br i1 %R95, label %subblock_401323_1, label %subblock_401323_2
subblock_401323_1:
  br label %block_401568
subblock_401323_2:
  br label %block_40132c
block_40132c:
  %R103 = phi i128 [ %R93, %subblock_401323_2 ], [ %R425, %block_401558 ]
  %R102 = phi i64 [ %R91, %subblock_401323_2 ], [ %R424, %block_401558 ]
  %R101 = phi i64 [ %R90, %subblock_401323_2 ], [ %R423, %block_401558 ]
  %R100 = phi i64 [ %R89, %subblock_401323_2 ], [ %R422, %block_401558 ]
  %R99 = phi i64 [ %R88, %subblock_401323_2 ], [ %R421, %block_401558 ]
  %R98 = phi i64 [ %R87, %subblock_401323_2 ], [ %R420, %block_401558 ]
  %R97 = phi i64 [ %R86, %subblock_401323_2 ], [ %R419, %block_401558 ]
  %R96 = phi i64 [ %R85, %subblock_401323_2 ], [ %R418, %block_401558 ]
  ; # 40132c: mov    eax,DWORD PTR [rbp+0x8]
  ; r104 := (bv_add r97 0x8 :: [64])
  %R104 = add i64 %R97, 8
  ; r105 := *r104
  %r114 = inttoptr i64 %R104 to i32*
  %R105 = load i32* %r114
  ; # 40132f: test   eax,eax
  ; r106 := (bv_eq r105 0x0 :: [32])
  %R106 = icmp eq i32 %R105, 0
  ; # 401331: je     4012d0
  br i1 %R106, label %subblock_40132c_1, label %subblock_40132c_2
subblock_40132c_1:
  br label %block_4012d0
subblock_40132c_2:
  br label %block_401333
block_401333:
  %R114 = phi i128 [ %R103, %subblock_40132c_2 ]
  %R113 = phi i64 [ %R102, %subblock_40132c_2 ]
  %R112 = phi i64 [ %R101, %subblock_40132c_2 ]
  %R111 = phi i64 [ %R100, %subblock_40132c_2 ]
  %R110 = phi i64 [ %R99, %subblock_40132c_2 ]
  %R109 = phi i64 [ %R98, %subblock_40132c_2 ]
  %R108 = phi i64 [ %R97, %subblock_40132c_2 ]
  %R107 = phi i64 [ %R96, %subblock_40132c_2 ]
  ; # 401333: xor    eax,eax
  ; # 401335: mov    DWORD PTR [rbp+0x8],eax
  ; r115 := (bv_add r108 0x8 :: [64])
  %R115 = add i64 %R108, 8
  ; *(r115) = 0x0 :: [32]
  %r126 = inttoptr i64 %R115 to i32*
  store i32 0, i32* %r126
  ; # 401338: lock or DWORD PTR [rsp],0x0
  ; r116 := *r107
  %r127 = inttoptr i64 %R107 to i32*
  %R116 = load i32* %r127
  ; *(r107) = r116
  %r129 = inttoptr i64 %R107 to i32*
  store i32 %R116, i32* %r129
  ; # 40133d: mov    eax,DWORD PTR [r14+0x4]
  ; r117 := (bv_add r113 0x4 :: [64])
  %R117 = add i64 %R113, 4
  ; r118 := *r117
  %r131 = inttoptr i64 %R117 to i32*
  %R118 = load i32* %r131
  ; # 401341: test   eax,eax
  ; r119 := (bv_eq r118 0x0 :: [32])
  %R119 = icmp eq i32 %R118, 0
  ; # 401343: je     4012d0
  br i1 %R119, label %subblock_401333_1, label %subblock_401333_2
subblock_401333_1:
  br label %block_4012d0
subblock_401333_2:
  br label %block_401345
block_401345:
  %R124 = phi i64 [ %R113, %subblock_401333_2 ]
  %R123 = phi i64 [ %R112, %subblock_401333_2 ]
  %R122 = phi i64 [ %R111, %subblock_401333_2 ]
  %R121 = phi i64 [ %R109, %subblock_401333_2 ]
  %R120 = phi i64 [ %R107, %subblock_401333_2 ]
  ; # 401345: mov    r9d,0xca
  ; # 40134b: mov    edx,0x1
  ; # 401350: mov    esi,0x81
  ; # 401355: mov    rax,r9
  ; # 401358: mov    rdi,r14
  ; # 40135b: syscall
  ; sys_futex
  %r139 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R124, i64 129, i64 1, i64 %R122, i64 %R121, i64 202, i64 202)
  %R125 = extractvalue { i64, i1 } %r139, 0
  br label %block_40135d
block_40135d:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R134 = phi i128 [ undef, %block_401345 ]
  %R133 = phi i64 [ %R123, %block_401345 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R132 = phi i64 [ undef, %block_401345 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R131 = phi i64 [ undef, %block_401345 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R130 = phi i64 [ undef, %block_401345 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R129 = phi i64 [ undef, %block_401345 ]
  %R128 = phi i64 [ %R120, %block_401345 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R127 = phi i64 [ undef, %block_401345 ]
  %R126 = phi i64 [ %R125, %block_401345 ]
  ; # 40135d: cmp    rax,0xffffffffffffffda
  ; r135 := (bv_eq r126 0xffffffffffffffda :: [64])
  %R135 = icmp eq i64 %R126, 18446744073709551578
  ; # 401361: jne    4012d0
  ; r136 := (bv_complement r135)
  %R136 = xor i1 %R135, -1
  br i1 %R136, label %subblock_40135d_1, label %subblock_40135d_2
subblock_40135d_1:
  br label %block_4012d0
subblock_40135d_2:
  br label %block_401367
block_401367:
  %R143 = phi i64 [ %R133, %subblock_40135d_2 ]
  %R142 = phi i64 [ %R132, %subblock_40135d_2 ]
  %R141 = phi i64 [ %R131, %subblock_40135d_2 ]
  %R140 = phi i64 [ %R130, %subblock_40135d_2 ]
  %R139 = phi i64 [ %R129, %subblock_40135d_2 ]
  %R138 = phi i64 [ %R128, %subblock_40135d_2 ]
  %R137 = phi i64 [ %R127, %subblock_40135d_2 ]
  ; # 401367: mov    rax,r9
  ; # 40136a: mov    rsi,rdx
  ; # 40136d: syscall
  ; sys_futex
  %r159 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R139, i64 %R137, i64 %R137, i64 %R142, i64 %R140, i64 %R141, i64 202)
  %R144 = extractvalue { i64, i1 } %r159, 0
  br label %block_40136f
block_40136f:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R150 = phi i128 [ undef, %block_401367 ]
  %R149 = phi i64 [ %R143, %block_401367 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R148 = phi i64 [ undef, %block_401367 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R147 = phi i64 [ undef, %block_401367 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R146 = phi i64 [ undef, %block_401367 ]
  %R145 = phi i64 [ %R138, %block_401367 ]
  ; # 40136f: mov    rbx,QWORD PTR [rip+0x202d6a]
  ; r151 := *0x6040e0 :: [64]
  %r167 = inttoptr i64 6308064 to i64*
  %R151 = load i64* %r167
  ; # 401376: and    rbx,QWORD PTR [rsp+0x8]
  ; r152 := (bv_add r145 0x8 :: [64])
  %R152 = add i64 %R145, 8
  ; r153 := *r152
  %r170 = inttoptr i64 %R152 to i64*
  %R153 = load i64* %r170
  ; r154 := (bv_and r151 r153)
  %R154 = and i64 %R151, %R153
  ; r155 := (bv_eq r154 0x0 :: [64])
  %R155 = icmp eq i64 %R154, 0
  ; # 40137b: jne    4012e2
  ; r156 := (bv_complement r155)
  %R156 = xor i1 %R155, -1
  br i1 %R156, label %subblock_40136f_1, label %subblock_40136f_2
subblock_40136f_1:
  br label %block_4012e2
subblock_40136f_2:
  br label %block_401381
block_401381:
  %R158 = phi i128 [ %R150, %subblock_40136f_2 ]
  %R157 = phi i64 [ %R145, %subblock_40136f_2 ]
  ; # 401381: nop    [rax]
  br label %block_401388
block_401388:
  %R160 = phi i128 [ %R158, %block_401381 ], [ %R41, %subblock_4012d0_1 ]
  %R159 = phi i64 [ %R157, %block_401381 ], [ %R36, %subblock_4012d0_1 ]
  ; # 401388: mov    rax,QWORD PTR [rsp+0x10]
  ; r161 := (bv_add r159 0x10 :: [64])
  %R161 = add i64 %R159, 16
  ; r162 := *r161
  %r180 = inttoptr i64 %R161 to i64*
  %R162 = load i64* %r180
  ; # 40138d: add    rax,0x20
  ; r163 := (bv_add r162 0x20 :: [64])
  %R163 = add i64 %R162, 32
  ; # 401391: mov    QWORD PTR [rsp+0x28],rax
  ; r164 := (bv_add r159 0x28 :: [64])
  %R164 = add i64 %R159, 40
  ; *(r164) = r163
  %r184 = inttoptr i64 %R164 to i64*
  store i64 %R163, i64* %r184
  ; # 401396: mov    eax,DWORD PTR [rip+0x2035f0]
  ; r165 := *0x60498c :: [64]
  %r185 = inttoptr i64 6310284 to i32*
  %R165 = load i32* %r185
  ; # 40139c: test   eax,eax
  ; r166 := (bv_eq r165 0x0 :: [32])
  %R166 = icmp eq i32 %R165, 0
  ; # 40139e: jne    40172e
  ; r167 := (bv_complement r166)
  %R167 = xor i1 %R166, -1
  br i1 %R167, label %subblock_401388_1, label %subblock_401388_2
subblock_401388_1:
  br label %block_40172e
subblock_401388_2:
  br label %block_4013a4
block_4013a4:
  %R169 = phi i128 [ %R737, %block_401770 ], [ %R721, %subblock_40172e_1 ], [ %R160, %subblock_401388_2 ]
  %R168 = phi i64 [ %R736, %block_401770 ], [ %R720, %subblock_40172e_1 ], [ %R159, %subblock_401388_2 ]
  ; # 4013a4: lea    rdi,[rsp+0x28]
  ; r170 := (bv_add r168 0x28 :: [64])
  %R170 = add i64 %R168, 40
  ; # 4013a9: call   4026e0
  ; r171 := (bv_add r168 0xfffffffffffffff8 :: [64])
  %R171 = add i64 %R168, 18446744073709551608
  ; r175 := (bv_add r171 0x8 :: [64])
  %R175 = add i64 %R171, 8
  %r194 = bitcast i128 %R169 to <2 x double>
  %r195 = call { i64, i64, <2 x double> } @F4026e0(i64 %R170, <2 x double> %r194)
  %R172 = extractvalue { i64, i64, <2 x double> } %r195, 0
  %R173 = extractvalue { i64, i64, <2 x double> } %r195, 1
  %r198 = extractvalue { i64, i64, <2 x double> } %r195, 2
  %R174 = bitcast <2 x double> %r198 to i128
  br label %block_4013ae
block_4013ae:
  %R184 = phi i128 [ %R174, %block_4013a4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R183 = phi i64 [ undef, %block_4013a4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R182 = phi i64 [ undef, %block_4013a4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R181 = phi i64 [ undef, %block_4013a4 ]
  %R180 = phi i64 [ %R173, %block_4013a4 ]
  %R179 = phi i64 [ %R175, %block_4013a4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R178 = phi i64 [ undef, %block_4013a4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R177 = phi i64 [ undef, %block_4013a4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R176 = phi i64 [ undef, %block_4013a4 ]
  ; # 4013ae: test   rax,rax
  ; r185 := (bv_eq r176 0x0 :: [64])
  %R185 = icmp eq i64 %R176, 0
  ; # 4013b1: je     4017f2
  br i1 %R185, label %subblock_4013ae_1, label %subblock_4013ae_2
subblock_4013ae_1:
  br label %block_4017f2
subblock_4013ae_2:
  br label %block_4013b7
block_4013b7:
  %R193 = phi i128 [ %R184, %subblock_4013ae_2 ]
  %R192 = phi i64 [ %R183, %subblock_4013ae_2 ]
  %R191 = phi i64 [ %R182, %subblock_4013ae_2 ]
  %R190 = phi i64 [ %R181, %subblock_4013ae_2 ]
  %R189 = phi i64 [ %R180, %subblock_4013ae_2 ]
  %R188 = phi i64 [ %R179, %subblock_4013ae_2 ]
  %R187 = phi i64 [ %R177, %subblock_4013ae_2 ]
  %R186 = phi i64 [ %R176, %subblock_4013ae_2 ]
  ; # 4013b7: cmp    rax,QWORD PTR [rip+0x202d12]
  ; r194 := *0x6040d0 :: [64]
  %r218 = inttoptr i64 6308048 to i64*
  %R194 = load i64* %r218
  ; r195 := (bv_eq r186 r194)
  %R195 = icmp eq i64 %R186, %R194
  ; # 4013be: je     401775
  br i1 %R195, label %subblock_4013b7_1, label %subblock_4013b7_2
subblock_4013b7_1:
  br label %block_401775
subblock_4013b7_2:
  br label %block_4013c4
block_4013c4:
  %R202 = phi i128 [ %R193, %subblock_4013b7_2 ]
  %R201 = phi i64 [ %R192, %subblock_4013b7_2 ]
  %R200 = phi i64 [ %R191, %subblock_4013b7_2 ]
  %R199 = phi i64 [ %R190, %subblock_4013b7_2 ]
  %R198 = phi i64 [ %R188, %subblock_4013b7_2 ]
  %R197 = phi i64 [ %R187, %subblock_4013b7_2 ]
  %R196 = phi i64 [ %R186, %subblock_4013b7_2 ]
  ; # 4013c4: mov    rsi,QWORD PTR [rsp+0x28]
  ; r203 := (bv_add r198 0x28 :: [64])
  %R203 = add i64 %R198, 40
  ; r204 := *r203
  %r229 = inttoptr i64 %R203 to i64*
  %R204 = load i64* %r229
  ; # 4013c9: lea    rbp,[rax+0x20]
  ; r205 := (bv_add r196 0x20 :: [64])
  %R205 = add i64 %R196, 32
  ; # 4013cd: mov    QWORD PTR [rax+0x10],0x1
  ; r206 := (bv_add r196 0x10 :: [64])
  %R206 = add i64 %R196, 16
  ; *(r206) = 0x1 :: [64]
  %r233 = inttoptr i64 %R206 to i64*
  store i64 1, i64* %r233
  ; # 4013d5: lea    rdx,[rsi-0x20]
  ; r207 := (bv_add r204 0xffffffffffffffe0 :: [64])
  %R207 = add i64 %R204, 18446744073709551584
  ; # 4013d9: mov    QWORD PTR [rsp+0x28],rdx
  ; *(r203) = r207
  %r235 = inttoptr i64 %R203 to i64*
  store i64 %R207, i64* %r235
  br label %block_4013de
block_4013de:
  %R216 = phi i128 [ %R202, %block_4013c4 ], [ %R745, %block_401775 ]
  %R215 = phi i64 [ %R201, %block_4013c4 ], [ %R744, %block_401775 ]
  %R214 = phi i64 [ %R200, %block_4013c4 ], [ %R743, %block_401775 ]
  %R213 = phi i64 [ %R199, %block_4013c4 ], [ %R742, %block_401775 ]
  %R212 = phi i64 [ %R204, %block_4013c4 ], [ %R741, %block_401775 ]
  %R211 = phi i64 [ %R205, %block_4013c4 ], [ %R738, %block_401775 ]
  %R210 = phi i64 [ %R198, %block_4013c4 ], [ %R740, %block_401775 ]
  %R209 = phi i64 [ %R207, %block_4013c4 ], [ %R747, %block_401775 ]
  %R208 = phi i64 [ %R197, %block_4013c4 ], [ %R739, %block_401775 ]
  ; # 4013de: lea    rax,[rbp+rdx*1]
  ; r217 := (bv_add r211 r209)
  %R217 = add i64 %R211, %R209
  ; # 4013e3: or     rdx,0x1
  ; r218 := (bv_or r209 0x1 :: [64])
  %R218 = or i64 %R209, 1
  ; # 4013e7: lea    r12,[rbp-0x10]
  ; r219 := (bv_add r211 0xfffffffffffffff0 :: [64])
  %R219 = add i64 %R211, 18446744073709551600
  ; # 4013eb: mov    edi,0x6040d8
  ; # 4013f0: mov    QWORD PTR [rax-0x10],rdx
  ; r220 := (bv_add r217 0xfffffffffffffff0 :: [64])
  %R220 = add i64 %R217, 18446744073709551600
  ; *(r220) = r218
  %r249 = inttoptr i64 %R220 to i64*
  store i64 %R218, i64* %r249
  ; # 4013f4: mov    QWORD PTR [rax-0x8],0x1
  ; r221 := (bv_add r217 0xfffffffffffffff8 :: [64])
  %R221 = add i64 %R217, 18446744073709551608
  ; *(r221) = 0x1 :: [64]
  %r251 = inttoptr i64 %R221 to i64*
  store i64 1, i64* %r251
  ; # 4013fc: mov    QWORD PTR [rbp-0x8],rdx
  ; r222 := (bv_add r211 0xfffffffffffffff8 :: [64])
  %R222 = add i64 %R211, 18446744073709551608
  ; *(r222) = r218
  %r253 = inttoptr i64 %R222 to i64*
  store i64 %R218, i64* %r253
  ; # 401400: mov    edx,DWORD PTR [rip+0x202cd2]
  ; r223 := *0x6040d8 :: [64]
  %r254 = inttoptr i64 6308056 to i32*
  %R223 = load i32* %r254
  ; r224 := (uext r223 64)
  %R224 = zext i32 %R223 to i64
  ; # 401406: mov    QWORD PTR [rip+0x202cc3],rax
  ; *(0x6040d0 :: [64]) = r217
  %r257 = inttoptr i64 6308048 to i64*
  store i64 %R217, i64* %r257
  ; # 40140d: test   edx,edx
  ; r225 := (bv_eq r223 0x0 :: [32])
  %R225 = icmp eq i32 %R223, 0
  ; # 40140f: je     401450
  br i1 %R225, label %subblock_4013de_1, label %subblock_4013de_2
subblock_4013de_1:
  br label %block_401450
subblock_4013de_2:
  br label %block_401411
block_401411:
  %R236 = phi i128 [ %R216, %subblock_4013de_2 ]
  %R235 = phi i64 [ %R219, %subblock_4013de_2 ]
  %R234 = phi i64 [ %R215, %subblock_4013de_2 ]
  %R233 = phi i64 [ %R214, %subblock_4013de_2 ]
  %R232 = phi i64 [ %R213, %subblock_4013de_2 ]
  %R231 = phi i64 [ 6308056, %subblock_4013de_2 ]
  %R230 = phi i64 [ %R212, %subblock_4013de_2 ]
  %R229 = phi i64 [ %R211, %subblock_4013de_2 ]
  %R228 = phi i64 [ %R210, %subblock_4013de_2 ]
  %R227 = phi i64 [ %R224, %subblock_4013de_2 ]
  %R226 = phi i64 [ %R208, %subblock_4013de_2 ]
  ; # 401411: xor    eax,eax
  ; # 401413: mov    DWORD PTR [rip+0x202cbf],eax
  ; *(0x6040d8 :: [64]) = 0x0 :: [32]
  %r270 = inttoptr i64 6308056 to i32*
  store i32 0, i32* %r270
  ; # 401419: lock or DWORD PTR [rsp],0x0
  ; r237 := *r228
  %r271 = inttoptr i64 %R228 to i32*
  %R237 = load i32* %r271
  ; *(r228) = r237
  %r273 = inttoptr i64 %R228 to i32*
  store i32 %R237, i32* %r273
  ; # 40141e: mov    eax,DWORD PTR [rip+0x202cb8]
  ; r238 := *0x6040dc :: [64]
  %r274 = inttoptr i64 6308060 to i32*
  %R238 = load i32* %r274
  ; # 401424: test   eax,eax
  ; r239 := (bv_eq r238 0x0 :: [32])
  %R239 = icmp eq i32 %R238, 0
  ; # 401426: je     401450
  br i1 %R239, label %subblock_401411_1, label %subblock_401411_2
subblock_401411_1:
  br label %block_401450
subblock_401411_2:
  br label %block_401428
block_401428:
  %R245 = phi i64 [ %R235, %subblock_401411_2 ]
  %R244 = phi i64 [ %R234, %subblock_401411_2 ]
  %R243 = phi i64 [ %R233, %subblock_401411_2 ]
  %R242 = phi i64 [ %R231, %subblock_401411_2 ]
  %R241 = phi i64 [ %R229, %subblock_401411_2 ]
  %R240 = phi i64 [ %R228, %subblock_401411_2 ]
  ; # 401428: mov    r8d,0xca
  ; # 40142e: mov    edx,0x1
  ; # 401433: mov    esi,0x81
  ; # 401438: mov    rax,r8
  ; # 40143b: syscall
  ; sys_futex
  %r283 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R242, i64 129, i64 1, i64 %R244, i64 202, i64 %R243, i64 202)
  %R246 = extractvalue { i64, i1 } %r283, 0
  br label %block_40143d
block_40143d:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R258 = phi i128 [ undef, %block_401428 ]
  %R257 = phi i64 [ %R245, %block_401428 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R256 = phi i64 [ undef, %block_401428 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R255 = phi i64 [ undef, %block_401428 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R254 = phi i64 [ undef, %block_401428 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R253 = phi i64 [ undef, %block_401428 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R252 = phi i64 [ undef, %block_401428 ]
  %R251 = phi i64 [ %R241, %block_401428 ]
  %R250 = phi i64 [ %R240, %block_401428 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R249 = phi i64 [ undef, %block_401428 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rcx
  %R248 = phi i64 [ undef, %block_401428 ]
  %R247 = phi i64 [ %R246, %block_401428 ]
  ; # 40143d: cmp    rax,0xffffffffffffffda
  ; r259 := (bv_eq r247 0xffffffffffffffda :: [64])
  %R259 = icmp eq i64 %R247, 18446744073709551578
  ; # 401441: jne    401450
  ; r260 := (bv_complement r259)
  %R260 = xor i1 %R259, -1
  br i1 %R260, label %subblock_40143d_1, label %subblock_40143d_2
subblock_40143d_1:
  br label %block_401450
subblock_40143d_2:
  br label %block_401443
block_401443:
  %R268 = phi i64 [ %R257, %subblock_40143d_2 ]
  %R267 = phi i64 [ %R256, %subblock_40143d_2 ]
  %R266 = phi i64 [ %R255, %subblock_40143d_2 ]
  %R265 = phi i64 [ %R254, %subblock_40143d_2 ]
  %R264 = phi i64 [ %R253, %subblock_40143d_2 ]
  %R263 = phi i64 [ %R251, %subblock_40143d_2 ]
  %R262 = phi i64 [ %R250, %subblock_40143d_2 ]
  %R261 = phi i64 [ %R249, %subblock_40143d_2 ]
  ; # 401443: mov    rax,r8
  ; # 401446: mov    rsi,rdx
  ; # 401449: syscall
  ; sys_futex
  %r307 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R264, i64 %R261, i64 %R261, i64 %R267, i64 %R265, i64 %R266, i64 202)
  %R269 = extractvalue { i64, i1 } %r307, 0
  br label %block_40144b
block_40144b:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R277 = phi i128 [ undef, %block_401443 ]
  %R276 = phi i64 [ %R268, %block_401443 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R275 = phi i64 [ undef, %block_401443 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R274 = phi i64 [ undef, %block_401443 ]
  %R273 = phi i64 [ %R263, %block_401443 ]
  %R272 = phi i64 [ %R262, %block_401443 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R271 = phi i64 [ undef, %block_401443 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rcx
  %R270 = phi i64 [ undef, %block_401443 ]
  ; # 40144b: nop    [rax+rax*1]
  br label %block_401450
block_401450:
  %R285 = phi i128 [ %R277, %block_40144b ], [ %R258, %subblock_40143d_1 ], [ %R236, %subblock_401411_1 ], [ %R216, %subblock_4013de_1 ]
  %R284 = phi i64 [ %R276, %block_40144b ], [ %R257, %subblock_40143d_1 ], [ %R235, %subblock_401411_1 ], [ %R219, %subblock_4013de_1 ]
  %R283 = phi i64 [ %R275, %block_40144b ], [ %R254, %subblock_40143d_1 ], [ %R232, %subblock_401411_1 ], [ %R213, %subblock_4013de_1 ]
  %R282 = phi i64 [ %R274, %block_40144b ], [ %R252, %subblock_40143d_1 ], [ %R230, %subblock_401411_1 ], [ %R212, %subblock_4013de_1 ]
  %R281 = phi i64 [ %R273, %block_40144b ], [ %R251, %subblock_40143d_1 ], [ %R229, %subblock_401411_1 ], [ %R211, %subblock_4013de_1 ]
  %R280 = phi i64 [ %R272, %block_40144b ], [ %R250, %subblock_40143d_1 ], [ %R228, %subblock_401411_1 ], [ %R210, %subblock_4013de_1 ]
  %R279 = phi i64 [ %R271, %block_40144b ], [ %R249, %subblock_40143d_1 ], [ %R227, %subblock_401411_1 ], [ %R224, %subblock_4013de_1 ]
  %R278 = phi i64 [ %R270, %block_40144b ], [ %R248, %subblock_40143d_1 ], [ %R226, %subblock_401411_1 ], [ %R208, %subblock_4013de_1 ]
  ; # 401450: test   r12,r12
  ; r286 := (bv_eq r284 0x0 :: [64])
  %R286 = icmp eq i64 %R284, 0
  ; # 401453: je     40183b
  br i1 %R286, label %subblock_401450_1, label %subblock_401450_2
subblock_401450_1:
  br label %block_40183b
subblock_401450_2:
  br label %block_401459
block_401459:
  %R294 = phi i128 [ %R285, %subblock_401450_2 ]
  %R293 = phi i64 [ %R284, %subblock_401450_2 ]
  %R292 = phi i64 [ %R283, %subblock_401450_2 ]
  %R291 = phi i64 [ %R282, %subblock_401450_2 ]
  %R290 = phi i64 [ %R281, %subblock_401450_2 ]
  %R289 = phi i64 [ %R280, %subblock_401450_2 ]
  %R288 = phi i64 [ %R279, %subblock_401450_2 ]
  %R287 = phi i64 [ %R278, %subblock_401450_2 ]
  ; # 401459: mov    rdi,r12
  ; # 40145c: call   400bf0
  ; r295 := (bv_add r289 0xfffffffffffffff8 :: [64])
  %R295 = add i64 %R289, 18446744073709551608
  ; r298 := (bv_add r295 0x8 :: [64])
  %R298 = add i64 %R295, 8
  %r336 = bitcast i128 %R294 to <2 x double>
  %r337 = call { i64, i64, <2 x double> } @F400bf0(i64 %R293, i64 %R291, i64 %R288, i64 %R287, i64 %R292, <2 x double> %r336)
  %R296 = extractvalue { i64, i64, <2 x double> } %r337, 0
  %r339 = extractvalue { i64, i64, <2 x double> } %r337, 2
  %R297 = bitcast <2 x double> %r339 to i128
  br label %block_401461
block_401461:
  %R305 = phi i128 [ %R297, %block_401459 ]
  %R304 = phi i64 [ %R293, %block_401459 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R303 = phi i64 [ undef, %block_401459 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R302 = phi i64 [ undef, %block_401459 ]
  %R301 = phi i64 [ %R290, %block_401459 ]
  %R300 = phi i64 [ %R298, %block_401459 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R299 = phi i64 [ undef, %block_401459 ]
  ; # 401461: test   eax,eax
  ; r306 := (trunc r299 32)
  %R306 = trunc i64 %R299 to i32
  ; r307 := (bv_eq r306 0x0 :: [32])
  %R307 = icmp eq i32 %R306, 0
  ; # 401463: je     401722
  br i1 %R307, label %subblock_401461_1, label %subblock_401461_2
subblock_401461_1:
  br label %block_401722
subblock_401461_2:
  br label %block_401469
block_401469:
  %R313 = phi i128 [ %R305, %subblock_401461_2 ]
  %R312 = phi i64 [ %R304, %subblock_401461_2 ]
  %R311 = phi i64 [ %R303, %subblock_401461_2 ]
  %R310 = phi i64 [ %R302, %subblock_401461_2 ]
  %R309 = phi i64 [ %R301, %subblock_401461_2 ]
  %R308 = phi i64 [ %R300, %subblock_401461_2 ]
  ; # 401469: mov    rax,QWORD PTR [rbp-0x10]
  ; r314 := (bv_add r309 0xfffffffffffffff0 :: [64])
  %R314 = add i64 %R309, 18446744073709551600
  ; r315 := *r314
  %r357 = inttoptr i64 %R314 to i64*
  %R315 = load i64* %r357
  ; # 40146d: mov    rbx,r12
  ; # 401470: mov    rdx,QWORD PTR [rbp-0x8]
  ; r316 := (bv_add r309 0xfffffffffffffff8 :: [64])
  %R316 = add i64 %R309, 18446744073709551608
  ; r317 := *r316
  %r360 = inttoptr i64 %R316 to i64*
  %R317 = load i64* %r360
  ; # 401474: and    rax,0xfffffffffffffffe
  ; r318 := (bv_and r315 0xfffffffffffffffe :: [64])
  %R318 = and i64 %R315, 18446744073709551614
  ; # 401478: sub    rbx,rax
  ; r319 := (bv_sub r312 r318)
  %R319 = sub i64 %R312, %R318
  ; # 40147b: mov    rax,QWORD PTR [rbx+0x8]
  ; r320 := (bv_add r319 0x8 :: [64])
  %R320 = add i64 %R319, 8
  ; r321 := *r320
  %r365 = inttoptr i64 %R320 to i64*
  %R321 = load i64* %r365
  ; # 40147f: and    rax,0xfffffffffffffffe
  ; r322 := (bv_and r321 0xfffffffffffffffe :: [64])
  %R322 = and i64 %R321, 18446744073709551614
  ; # 401483: add    rax,rdx
  ; r323 := (bv_add r322 r317)
  %R323 = add i64 %R322, %R317
  ; # 401486: and    rdx,0xfffffffffffffffe
  ; r324 := (bv_and r317 0xfffffffffffffffe :: [64])
  %R324 = and i64 %R317, 18446744073709551614
  ; # 40148a: mov    QWORD PTR [rbx+0x8],rax
  ; *(r320) = r323
  %r370 = inttoptr i64 %R320 to i64*
  store i64 %R323, i64* %r370
  ; # 40148e: mov    QWORD PTR [r12+rdx*1],rax
  ; r325 := (bv_add r312 r324)
  %R325 = add i64 %R312, %R324
  ; *(r325) = r323
  %r372 = inttoptr i64 %R325 to i64*
  store i64 %R323, i64* %r372
  ; # 401492: jmp    401642
  br label %block_401642
block_4014a0:
  %R331 = phi i128 [ %r0, %subblock_401280_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register r10
  %R330 = phi i64 [ undef, %subblock_401280_1 ]
  %R329 = phi i64 [ %a5, %subblock_401280_1 ]
  %R328 = phi i64 [ %a4, %subblock_401280_1 ]
  %R327 = phi i64 [ %a0, %subblock_401280_1 ]
  %R326 = phi i64 [ %R9, %subblock_401280_1 ]
  ; # 4014a0: add    rdi,0x2f
  ; r332 := (bv_add r327 0x2f :: [64])
  %R332 = add i64 %R327, 47
  ; # 4014a4: and    rdi,0xffffffffffffffe0
  ; r333 := (bv_and r332 0xffffffffffffffe0 :: [64])
  %R333 = and i64 %R332, 18446744073709551584
  ; # 4014a8: cmp    rdi,0x38000
  ; # 4014af: mov    QWORD PTR [rsp+0x10],rdi
  ; r334 := (bv_add r326 0x10 :: [64])
  %R334 = add i64 %R326, 16
  ; *(r334) = r333
  %r382 = inttoptr i64 %R334 to i64*
  store i64 %R333, i64* %r382
  ; # 4014b4: jbe    4016da
  ; r335 := (bv_ule r333 0x38000 :: [64])
  %R335 = icmp ule i64 %R333, 229376
  br i1 %R335, label %subblock_4014a0_1, label %subblock_4014a0_2
subblock_4014a0_1:
  br label %block_4016da
subblock_4014a0_2:
  br label %block_4014ba
block_4014ba:
  %R337 = phi i128 [ %R331, %subblock_4014a0_2 ]
  %R336 = phi i64 [ %R333, %subblock_4014a0_2 ]
  ; # 4014ba: mov    rbx,rdi
  ; # 4014bd: xor    r9d,r9d
  ; # 4014c0: xor    edi,edi
  ; # 4014c2: add    rbx,0x100f
  ; r338 := (bv_add r336 0x100f :: [64])
  %R338 = add i64 %R336, 4111
  ; # 4014c9: mov    r8d,0xffffffff
  ; # 4014cf: mov    ecx,0x22
  ; # 4014d4: and    rbx,0xfffffffffffff000
  ; r339 := (bv_and r338 0xfffffffffffff000 :: [64])
  %R339 = and i64 %R338, 18446744073709547520
  ; # 4014db: mov    edx,0x3
  ; # 4014e0: mov    rsi,rbx
  ; # 4014e3: call   401b20
  %r388 = bitcast i128 %R337 to <2 x double>
  %r389 = call { i64, i64, <2 x double> } @F401b20(i64 0, i64 %R339, i64 3, i64 34, i64 4294967295, i64 0, <2 x double> %r388)
  %R340 = extractvalue { i64, i64, <2 x double> } %r389, 0
  %R341 = extractvalue { i64, i64, <2 x double> } %r389, 1
  %r392 = extractvalue { i64, i64, <2 x double> } %r389, 2
  %R342 = bitcast <2 x double> %r392 to i128
  br label %block_4014e8
block_4014e8:
  %R346 = phi i128 [ %R342, %block_4014ba ]
  %R345 = phi i64 [ %R339, %block_4014ba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R344 = phi i64 [ undef, %block_4014ba ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R343 = phi i64 [ undef, %block_4014ba ]
  ; # 4014e8: cmp    rax,0xffffffffffffffff
  ; r347 := (bv_eq r343 0xffffffffffffffff :: [64])
  %R347 = icmp eq i64 %R343, 18446744073709551615
  ; # 4014ec: je     40183b
  br i1 %R347, label %subblock_4014e8_1, label %subblock_4014e8_2
subblock_4014e8_1:
  br label %block_40183b
subblock_4014e8_2:
  br label %block_4014f2
block_4014f2:
  %R351 = phi i128 [ %R346, %subblock_4014e8_2 ]
  %R350 = phi i64 [ %R345, %subblock_4014e8_2 ]
  %R349 = phi i64 [ %R344, %subblock_4014e8_2 ]
  %R348 = phi i64 [ %R343, %subblock_4014e8_2 ]
  ; # 4014f2: sub    rbx,0x10
  ; r352 := (bv_add r350 0xfffffffffffffff0 :: [64])
  %R352 = add i64 %R350, 18446744073709551600
  ; # 4014f6: mov    QWORD PTR [rax+0x10],0x10
  ; r353 := (bv_add r348 0x10 :: [64])
  %R353 = add i64 %R348, 16
  ; *(r353) = 0x10 :: [64]
  %r405 = inttoptr i64 %R353 to i64*
  store i64 16, i64* %r405
  ; # 4014fe: add    rax,0x20
  ; r354 := (bv_add r348 0x20 :: [64])
  %R354 = add i64 %R348, 32
  ; # 401502: mov    QWORD PTR [rax-0x8],rbx
  ; r355 := (bv_add r348 0x18 :: [64])
  %R355 = add i64 %R348, 24
  ; *(r355) = r352
  %r408 = inttoptr i64 %R355 to i64*
  store i64 %R352, i64* %r408
  br label %block_401506
block_401506:
  %R358 = phi i128 [ %R798, %block_4017c3 ], [ %R351, %block_4014f2 ], [ %R870, %block_40183b ]
  %R357 = phi i64 [ %R797, %block_4017c3 ], [ %R349, %block_4014f2 ], [ %R869, %block_40183b ]
  %R356 = phi i64 [ 0, %block_4017c3 ], [ %R354, %block_4014f2 ], [ 0, %block_40183b ]
  ; # 401506: add    rsp,0x38
  ; # 40150a: pop    rbx
  ; # 40150b: pop    rbp
  ; # 40150c: pop    r12
  ; # 40150e: pop    r13
  ; # 401510: pop    r14
  ; # 401512: pop    r15
  ; # 401514: ret
  %r412 = bitcast i128 %R358 to <2 x double>
  %r413 = insertvalue { i64, i64, <2 x double> } undef, i64 %R356, 0
  %r414 = insertvalue { i64, i64, <2 x double> } %r413, i64 %R357, 1
  %r415 = insertvalue { i64, i64, <2 x double> } %r414, <2 x double> %r412, 2
  ret { i64, i64, <2 x double> } %r415
block_401518:
  %R368 = phi i128 [ %R53, %subblock_4012e2_1 ]
  %R367 = phi i64 [ %R56, %subblock_4012e2_1 ]
  %R366 = phi i64 [ %R63, %subblock_4012e2_1 ]
  %R365 = phi i64 [ %R52, %subblock_4012e2_1 ]
  %R364 = phi i64 [ %R51, %subblock_4012e2_1 ]
  %R363 = phi i64 [ %R50, %subblock_4012e2_1 ]
  %R362 = phi i64 [ %R49, %subblock_4012e2_1 ]
  %R361 = phi i64 [ %R61, %subblock_4012e2_1 ]
  %R360 = phi i64 [ %R48, %subblock_4012e2_1 ]
  %R359 = phi i64 [ %R57, %subblock_4012e2_1 ]
  ; # 401518: mov    eax,r12d
  ; r369 := (trunc r365 32)
  %R369 = trunc i64 %R365 to i32
  ; # 40151b: xchg   DWORD PTR [rbp+0x8],eax
  ; r370 := (bv_add r361 0x8 :: [64])
  %R370 = add i64 %R361, 8
  ; r371 := *r370
  %r428 = inttoptr i64 %R370 to i32*
  %R371 = load i32* %r428
  ; *(r370) = r369
  %r430 = inttoptr i64 %R370 to i32*
  store i32 %R369, i32* %r430
  ; # 40151e: test   eax,eax
  ; r372 := (bv_eq r371 0x0 :: [32])
  %R372 = icmp eq i32 %R371, 0
  ; # 401520: je     40130a
  br i1 %R372, label %subblock_401518_1, label %subblock_401518_2
subblock_401518_1:
  br label %block_40130a
subblock_401518_2:
  br label %block_401526
block_401526:
  %R379 = phi i128 [ %R368, %subblock_401518_2 ]
  %R378 = phi i64 [ %R367, %subblock_401518_2 ]
  %R377 = phi i64 [ %R366, %subblock_401518_2 ]
  %R376 = phi i64 [ %R365, %subblock_401518_2 ]
  %R375 = phi i64 [ %R361, %subblock_401518_2 ]
  %R374 = phi i64 [ %R360, %subblock_401518_2 ]
  %R373 = phi i64 [ %R359, %subblock_401518_2 ]
  ; # 401526: lea    r13,[rbp+0xc]
  ; r380 := (bv_add r375 0xc :: [64])
  %R380 = add i64 %R375, 12
  ; # 40152a: nop    [rax+rax*1]
  br label %block_401530
block_401530:
  %R388 = phi i128 [ %R402, %subblock_401545_1 ], [ %R379, %block_401526 ]
  %R387 = phi i64 [ %R401, %subblock_401545_1 ], [ %R378, %block_401526 ]
  %R386 = phi i64 [ %R400, %subblock_401545_1 ], [ %R377, %block_401526 ]
  %R385 = phi i64 [ %R399, %subblock_401545_1 ], [ %R380, %block_401526 ]
  %R384 = phi i64 [ %R398, %subblock_401545_1 ], [ %R376, %block_401526 ]
  %R383 = phi i64 [ %R394, %subblock_401545_1 ], [ %R375, %block_401526 ]
  %R382 = phi i64 [ %R393, %subblock_401545_1 ], [ %R374, %block_401526 ]
  %R381 = phi i64 [ %R392, %subblock_401545_1 ], [ %R373, %block_401526 ]
  ; # 401530: mov    ecx,0x1
  ; # 401535: mov    edx,0x1
  ; # 40153a: mov    rsi,r13
  ; # 40153d: mov    rdi,r14
  ; # 401540: call   40241c
  ; r389 := (bv_add r382 0xfffffffffffffff8 :: [64])
  %R389 = add i64 %R382, 18446744073709551608
  ; r391 := (bv_add r389 0x8 :: [64])
  %R391 = add i64 %R389, 8
  %r450 = bitcast i128 %R388 to <2 x double>
  %r451 = call { i64, i64, <2 x double> } @F40241c(i64 %R386, i64 %R385, i64 1, i64 1, <2 x double> %r450)
  %r452 = extractvalue { i64, i64, <2 x double> } %r451, 2
  %R390 = bitcast <2 x double> %r452 to i128
  br label %block_401545
block_401545:
  %R402 = phi i128 [ %R390, %block_401530 ]
  %R401 = phi i64 [ %R387, %block_401530 ]
  %R400 = phi i64 [ %R386, %block_401530 ]
  %R399 = phi i64 [ %R385, %block_401530 ]
  %R398 = phi i64 [ %R384, %block_401530 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r10
  %R397 = phi i64 [ undef, %block_401530 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R396 = phi i64 [ undef, %block_401530 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R395 = phi i64 [ undef, %block_401530 ]
  %R394 = phi i64 [ %R383, %block_401530 ]
  %R393 = phi i64 [ %R391, %block_401530 ]
  %R392 = phi i64 [ %R381, %block_401530 ]
  ; # 401545: mov    ecx,r12d
  ; r403 := (trunc r398 32)
  %R403 = trunc i64 %R398 to i32
  ; # 401548: xchg   DWORD PTR [r14],ecx
  ; r404 := *r400
  %r466 = inttoptr i64 %R400 to i32*
  %R404 = load i32* %r466
  ; *(r400) = r403
  %r468 = inttoptr i64 %R400 to i32*
  store i32 %R403, i32* %r468
  ; # 40154b: test   ecx,ecx
  ; r405 := (bv_eq r404 0x0 :: [32])
  %R405 = icmp eq i32 %R404, 0
  ; # 40154d: jne    401530
  ; r406 := (bv_complement r405)
  %R406 = xor i1 %R405, -1
  br i1 %R406, label %subblock_401545_1, label %subblock_401545_2
subblock_401545_1:
  br label %block_401530
subblock_401545_2:
  br label %block_40154f
block_40154f:
  %R416 = phi i128 [ %R402, %subblock_401545_2 ]
  %R415 = phi i64 [ %R401, %subblock_401545_2 ]
  %R414 = phi i64 [ %R400, %subblock_401545_2 ]
  %R413 = phi i64 [ %R398, %subblock_401545_2 ]
  %R412 = phi i64 [ %R397, %subblock_401545_2 ]
  %R411 = phi i64 [ %R396, %subblock_401545_2 ]
  %R410 = phi i64 [ %R395, %subblock_401545_2 ]
  %R409 = phi i64 [ %R394, %subblock_401545_2 ]
  %R408 = phi i64 [ %R393, %subblock_401545_2 ]
  %R407 = phi i64 [ %R392, %subblock_401545_2 ]
  ; # 40154f: jmp    40130a
  br label %block_40130a
block_401558:
  %R425 = phi i128 [ %R75, %subblock_40130a_1 ]
  %R424 = phi i64 [ %R73, %subblock_40130a_1 ]
  %R423 = phi i64 [ %R72, %subblock_40130a_1 ]
  %R422 = phi i64 [ %R71, %subblock_40130a_1 ]
  %R421 = phi i64 [ %R70, %subblock_40130a_1 ]
  %R420 = phi i64 [ %R69, %subblock_40130a_1 ]
  %R419 = phi i64 [ %R68, %subblock_40130a_1 ]
  %R418 = phi i64 [ %R67, %subblock_40130a_1 ]
  %R417 = phi i64 [ %R79, %subblock_40130a_1 ]
  ; # 401558: mov    QWORD PTR [rax+0x18],rax
  ; r426 := (bv_add r417 0x18 :: [64])
  %R426 = add i64 %R417, 24
  ; *(r426) = r417
  %r491 = inttoptr i64 %R426 to i64*
  store i64 %R417, i64* %r491
  ; # 40155c: mov    QWORD PTR [rax+0x10],rax
  ; r427 := (bv_add r417 0x10 :: [64])
  %R427 = add i64 %R417, 16
  ; *(r427) = r417
  %r493 = inttoptr i64 %R427 to i64*
  store i64 %R417, i64* %r493
  ; # 401560: jmp    40132c
  br label %block_40132c
block_401568:
  %R435 = phi i128 [ %R93, %subblock_401323_1 ]
  %R434 = phi i64 [ %R92, %subblock_401323_1 ]
  %R433 = phi i64 [ %R91, %subblock_401323_1 ]
  %R432 = phi i64 [ %R89, %subblock_401323_1 ]
  %R431 = phi i64 [ %R88, %subblock_401323_1 ]
  %R430 = phi i64 [ %R87, %subblock_401323_1 ]
  %R429 = phi i64 [ %R85, %subblock_401323_1 ]
  %R428 = phi i64 [ %R84, %subblock_401323_1 ]
  ; # 401568: cmp    r15d,0x27
  ; r436 := (trunc r434 32)
  %R436 = trunc i64 %R434 to i32
  ; r437 := (ssbb_overflows r436 0x27 :: [32] 0x0 :: [1])
  %r503 = zext i1 0 to i32
  %r504 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %R436, i32 39)
  %r505 = extractvalue { i32, i1 } %r504, 0
  %r506 = extractvalue { i32, i1 } %r504, 1
  %r507 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %r505, i32 %r503)
  %r508 = extractvalue { i32, i1 } %r507, 1
  %R437 = or i1 %r506, %r508
  ; r438 := (bv_add r436 0xffffffd9 :: [32])
  %R438 = add i32 %R436, 4294967257
  ; r439 := (bv_slt r438 0x0 :: [32])
  %R439 = icmp slt i32 %R438, 0
  ; r440 := (bv_eq r436 0x27 :: [32])
  %R440 = icmp eq i32 %R436, 39
  ; # 40156c: jle    4016a0
  ; r441 := (bv_complement r440)
  %R441 = xor i1 %R440, -1
  ; r442 := (bv_eq r439 r437)
  %R442 = icmp eq i1 %R439, %R437
  ; r443 := (bv_and r441 r442)
  %R443 = and i1 %R441, %R442
  ; r444 := (bv_complement r443)
  %R444 = xor i1 %R443, -1
  br i1 %R444, label %subblock_401568_1, label %subblock_401568_2
subblock_401568_1:
  br label %block_4016a0
subblock_401568_2:
  br label %block_401572
block_401572:
  %R452 = phi i128 [ %R435, %subblock_401568_2 ]
  %R451 = phi i64 [ %R434, %subblock_401568_2 ]
  %R450 = phi i64 [ %R433, %subblock_401568_2 ]
  %R449 = phi i64 [ %R432, %subblock_401568_2 ]
  %R448 = phi i64 [ %R431, %subblock_401568_2 ]
  %R447 = phi i64 [ %R430, %subblock_401568_2 ]
  %R446 = phi i64 [ %R429, %subblock_401568_2 ]
  %R445 = phi i64 [ %R428, %subblock_401568_2 ]
  ; # 401572: mov    eax,DWORD PTR [rsp+0x1c]
  ; r453 := (bv_add r446 0x1c :: [64])
  %R453 = add i64 %R446, 28
  ; r454 := *r453
  %r526 = inttoptr i64 %R453 to i32*
  %R454 = load i32* %r526
  ; # 401576: add    eax,0x2
  ; r455 := (bv_add r454 0x2 :: [32])
  %R455 = add i32 %R454, 2
  ; # 401579: cmp    r15d,eax
  ; r456 := (trunc r451 32)
  %R456 = trunc i64 %R451 to i32
  ; r457 := (ssbb_overflows r456 r455 0x0 :: [1])
  %r530 = zext i1 0 to i32
  %r531 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %R456, i32 %R455)
  %r532 = extractvalue { i32, i1 } %r531, 0
  %r533 = extractvalue { i32, i1 } %r531, 1
  %r534 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %r532, i32 %r530)
  %r535 = extractvalue { i32, i1 } %r534, 1
  %R457 = or i1 %r533, %r535
  ; r458 := (bv_sub r456 r455)
  %R458 = sub i32 %R456, %R455
  ; r459 := (bv_slt r458 0x0 :: [32])
  %R459 = icmp slt i32 %R458, 0
  ; r460 := (bv_eq r456 r455)
  %R460 = icmp eq i32 %R456, %R455
  ; # 40157c: jle    401696
  ; r461 := (bv_complement r460)
  %R461 = xor i1 %R460, -1
  ; r462 := (bv_eq r459 r457)
  %R462 = icmp eq i1 %R459, %R457
  ; r463 := (bv_and r461 r462)
  %R463 = and i1 %R461, %R462
  ; r464 := (bv_complement r463)
  %R464 = xor i1 %R463, -1
  br i1 %R464, label %subblock_401572_1, label %subblock_401572_2
subblock_401572_1:
  br label %block_401696
subblock_401572_2:
  br label %block_401582
block_401582:
  %R472 = phi i128 [ %R452, %subblock_401572_2 ]
  %R471 = phi i64 [ %R451, %subblock_401572_2 ]
  %R470 = phi i64 [ %R450, %subblock_401572_2 ]
  %R469 = phi i64 [ %R449, %subblock_401572_2 ]
  %R468 = phi i64 [ %R448, %subblock_401572_2 ]
  %R467 = phi i64 [ %R447, %subblock_401572_2 ]
  %R466 = phi i64 [ %R446, %subblock_401572_2 ]
  %R465 = phi i64 [ %R445, %subblock_401572_2 ]
  ; # 401582: mov    rdx,QWORD PTR [rbx+0x8]
  ; r473 := (bv_add r465 0x8 :: [64])
  %R473 = add i64 %R465, 8
  ; r474 := *r473
  %r553 = inttoptr i64 %R473 to i64*
  %R474 = load i64* %r553
  ; # 401586: and    rdx,0xfffffffffffffffe
  ; r475 := (bv_and r474 0xfffffffffffffffe :: [64])
  %R475 = and i64 %R474, 18446744073709551614
  ; # 40158a: mov    rsi,rdx
  ; # 40158d: sub    rsi,QWORD PTR [rsp+0x10]
  ; r476 := (bv_add r466 0x10 :: [64])
  %R476 = add i64 %R466, 16
  ; r477 := *r476
  %r557 = inttoptr i64 %R476 to i64*
  %R477 = load i64* %r557
  ; r478 := (bv_sub r475 r477)
  %R478 = sub i64 %R475, %R477
  br label %block_401592
block_401592:
  %R488 = phi i128 [ %R472, %block_401582 ], [ %R755, %subblock_401782_1 ]
  %R487 = phi i64 [ %R471, %block_401582 ], [ %R754, %subblock_401782_1 ]
  %R486 = phi i64 [ %R470, %block_401582 ], [ %R753, %subblock_401782_1 ]
  %R485 = phi i64 [ %R469, %block_401582 ], [ %R752, %subblock_401782_1 ]
  %R484 = phi i64 [ %R468, %block_401582 ], [ %R751, %subblock_401782_1 ]
  %R483 = phi i64 [ %R467, %block_401582 ], [ %R750, %subblock_401782_1 ]
  %R482 = phi i64 [ %R478, %block_401582 ], [ %R761, %subblock_401782_1 ]
  %R481 = phi i64 [ %R466, %block_401582 ], [ %R749, %subblock_401782_1 ]
  %R480 = phi i64 [ %R465, %block_401582 ], [ %R748, %subblock_401782_1 ]
  %R479 = phi i64 [ %R475, %block_401582 ], [ %R758, %subblock_401782_1 ]
  ; # 401592: mov    rax,rsi
  ; # 401595: shr    rax,0x5
  ; r489 := (bv_shr r482 0x5 :: [64])
  %R489 = lshr i64 %R482, 5
  ; # 401599: sub    rax,0x1
  ; r490 := (bv_add r489 0xffffffffffffffff :: [64])
  %R490 = add i64 %R489, 18446744073709551615
  ; # 40159d: cmp    rax,0x20
  ; r491 := (bv_ult r490 0x20 :: [64])
  %R491 = icmp ult i64 %R490, 32
  ; r492 := (bv_eq r489 0x21 :: [64])
  %R492 = icmp eq i64 %R489, 33
  ; # 4015a1: jbe    4016a0
  ; r493 := (bv_or r491 r492)
  %R493 = or i1 %R491, %R492
  br i1 %R493, label %subblock_401592_1, label %subblock_401592_2
subblock_401592_1:
  br label %block_4016a0
subblock_401592_2:
  br label %block_4015a7
block_4015a7:
  %R504 = phi i128 [ %R488, %subblock_401592_2 ]
  %R503 = phi i64 [ %R487, %subblock_401592_2 ]
  %R502 = phi i64 [ %R486, %subblock_401592_2 ]
  %R501 = phi i64 [ %R485, %subblock_401592_2 ]
  %R500 = phi i64 [ %R484, %subblock_401592_2 ]
  %R499 = phi i64 [ %R483, %subblock_401592_2 ]
  %R498 = phi i64 [ %R482, %subblock_401592_2 ]
  %R497 = phi i64 [ %R481, %subblock_401592_2 ]
  %R496 = phi i64 [ %R480, %subblock_401592_2 ]
  %R495 = phi i64 [ %R479, %subblock_401592_2 ]
  %R494 = phi i64 [ %R490, %subblock_401592_2 ]
  ; # 4015a7: cmp    rax,0x1c00
  ; # 4015ad: mov    ecx,0x3f
  ; # 4015b2: jbe    4017a4
  ; r505 := (bv_ule r494 0x1c00 :: [64])
  %R505 = icmp ule i64 %R494, 7168
  br i1 %R505, label %subblock_4015a7_1, label %subblock_4015a7_2
subblock_4015a7_1:
  br label %block_4017a4
subblock_4015a7_2:
  br label %block_4015b8
block_4015b8:
  %R516 = phi i128 [ %R781, %block_4017a4 ], [ %R504, %subblock_4015a7_2 ]
  %R515 = phi i64 [ %R780, %block_4017a4 ], [ %R503, %subblock_4015a7_2 ]
  %R514 = phi i64 [ %R779, %block_4017a4 ], [ %R502, %subblock_4015a7_2 ]
  %R513 = phi i64 [ %R778, %block_4017a4 ], [ %R501, %subblock_4015a7_2 ]
  %R512 = phi i64 [ %R777, %block_4017a4 ], [ %R500, %subblock_4015a7_2 ]
  %R511 = phi i64 [ %R776, %block_4017a4 ], [ %R499, %subblock_4015a7_2 ]
  %R510 = phi i64 [ %R775, %block_4017a4 ], [ %R498, %subblock_4015a7_2 ]
  %R509 = phi i64 [ %R774, %block_4017a4 ], [ %R497, %subblock_4015a7_2 ]
  %R508 = phi i64 [ %R773, %block_4017a4 ], [ %R496, %subblock_4015a7_2 ]
  %R507 = phi i64 [ %R772, %block_4017a4 ], [ %R495, %subblock_4015a7_2 ]
  %R506 = phi i64 [ %R788, %block_4017a4 ], [ 63, %subblock_4015a7_2 ]
  ; # 4015b8: cmp    r15d,ecx
  ; r517 := (trunc r515 32)
  %R517 = trunc i64 %R515 to i32
  ; r518 := (trunc r506 32)
  %R518 = trunc i64 %R506 to i32
  ; r519 := (bv_eq r517 r518)
  %R519 = icmp eq i32 %R517, %R518
  ; # 4015bb: jne    4016a0
  ; r520 := (bv_complement r519)
  %R520 = xor i1 %R519, -1
  br i1 %R520, label %subblock_4015b8_1, label %subblock_4015b8_2
subblock_4015b8_1:
  br label %block_4016a0
subblock_4015b8_2:
  br label %block_4015c1
block_4015c1:
  %R528 = phi i128 [ %R516, %subblock_4015b8_2 ]
  %R527 = phi i64 [ %R514, %subblock_4015b8_2 ]
  %R526 = phi i64 [ %R512, %subblock_4015b8_2 ]
  %R525 = phi i64 [ %R511, %subblock_4015b8_2 ]
  %R524 = phi i64 [ %R510, %subblock_4015b8_2 ]
  %R523 = phi i64 [ %R509, %subblock_4015b8_2 ]
  %R522 = phi i64 [ %R508, %subblock_4015b8_2 ]
  %R521 = phi i64 [ %R507, %subblock_4015b8_2 ]
  ; # 4015c1: mov    r10,QWORD PTR [rsp+0x10]
  ; r529 := (bv_add r523 0x10 :: [64])
  %R529 = add i64 %R523, 16
  ; r530 := *r529
  %r611 = inttoptr i64 %R529 to i64*
  %R530 = load i64* %r611
  ; # 4015c6: mov    rax,QWORD PTR [rbx+0x18]
  ; r531 := (bv_add r522 0x18 :: [64])
  %R531 = add i64 %R522, 24
  ; r532 := *r531
  %r614 = inttoptr i64 %R531 to i64*
  %R532 = load i64* %r614
  ; # 4015ca: mov    rdi,QWORD PTR [rbx+0x10]
  ; r533 := (bv_add r522 0x10 :: [64])
  %R533 = add i64 %R522, 16
  ; r534 := *r533
  %r617 = inttoptr i64 %R533 to i64*
  %R534 = load i64* %r617
  ; # 4015ce: lea    rcx,[rbx+r10*1]
  ; r535 := (bv_add r522 r530)
  %R535 = add i64 %R522, %R530
  ; # 4015d2: mov    QWORD PTR [rcx+0x18],rax
  ; r536 := (bv_add r535 0x18 :: [64])
  %R536 = add i64 %R535, 24
  ; *(r536) = r532
  %r621 = inttoptr i64 %R536 to i64*
  store i64 %R532, i64* %r621
  ; # 4015d6: mov    QWORD PTR [rcx+0x10],rdi
  ; r537 := (bv_add r535 0x10 :: [64])
  %R537 = add i64 %R535, 16
  ; *(r537) = r534
  %r623 = inttoptr i64 %R537 to i64*
  store i64 %R534, i64* %r623
  ; # 4015da: mov    QWORD PTR [rax+0x10],rcx
  ; r538 := (bv_add r532 0x10 :: [64])
  %R538 = add i64 %R532, 16
  ; *(r538) = r535
  %r625 = inttoptr i64 %R538 to i64*
  store i64 %R535, i64* %r625
  ; # 4015de: mov    rax,QWORD PTR [rcx+0x10]
  ; r539 := *r537
  %r626 = inttoptr i64 %R537 to i64*
  %R539 = load i64* %r626
  ; # 4015e2: mov    QWORD PTR [rax+0x18],rcx
  ; r540 := (bv_add r539 0x18 :: [64])
  %R540 = add i64 %R539, 24
  ; *(r540) = r535
  %r629 = inttoptr i64 %R540 to i64*
  store i64 %R535, i64* %r629
  ; # 4015e6: mov    rax,r10
  ; # 4015e9: mov    QWORD PTR [rcx+0x8],rsi
  ; r541 := (bv_add r535 0x8 :: [64])
  %R541 = add i64 %R535, 8
  ; *(r541) = r524
  %r631 = inttoptr i64 %R541 to i64*
  store i64 %R524, i64* %r631
  ; # 4015ed: or     rax,0x1
  ; r542 := (bv_or r530 0x1 :: [64])
  %R542 = or i64 %R530, 1
  ; # 4015f1: mov    QWORD PTR [rcx],rax
  ; *(r535) = r542
  %r633 = inttoptr i64 %R535 to i64*
  store i64 %R542, i64* %r633
  ; # 4015f4: mov    QWORD PTR [rbx+rdx*1],rsi
  ; r543 := (bv_add r522 r521)
  %R543 = add i64 %R522, %R521
  ; *(r543) = r524
  %r635 = inttoptr i64 %R543 to i64*
  store i64 %R524, i64* %r635
  ; # 4015f8: mov    QWORD PTR [rbx+0x8],rax
  ; r544 := (bv_add r522 0x8 :: [64])
  %R544 = add i64 %R522, 8
  ; *(r544) = r542
  %r637 = inttoptr i64 %R544 to i64*
  store i64 %R542, i64* %r637
  br label %block_4015fc
block_4015fc:
  %R552 = phi i128 [ %R528, %block_4015c1 ], [ %R663, %block_4016b1 ]
  %R551 = phi i64 [ %R527, %block_4015c1 ], [ %R662, %block_4016b1 ]
  %R550 = phi i64 [ %R530, %block_4015c1 ], [ %R661, %block_4016b1 ]
  %R549 = phi i64 [ %R526, %block_4015c1 ], [ %R660, %block_4016b1 ]
  %R548 = phi i64 [ %R525, %block_4015c1 ], [ %R659, %block_4016b1 ]
  %R547 = phi i64 [ %R523, %block_4015c1 ], [ %R658, %block_4016b1 ]
  %R546 = phi i64 [ %R522, %block_4015c1 ], [ %R657, %block_4016b1 ]
  %R545 = phi i64 [ %R542, %block_4015c1 ], [ %R671, %block_4016b1 ]
  ; # 4015fc: mov    edx,DWORD PTR [r14]
  ; r553 := *r551
  %r646 = inttoptr i64 %R551 to i32*
  %R553 = load i32* %r646
  ; # 4015ff: test   edx,edx
  ; r554 := (bv_eq r553 0x0 :: [32])
  %R554 = icmp eq i32 %R553, 0
  ; # 401601: je     401642
  br i1 %R554, label %subblock_4015fc_1, label %subblock_4015fc_2
subblock_4015fc_1:
  br label %block_401642
subblock_4015fc_2:
  br label %block_401603
block_401603:
  %R561 = phi i128 [ %R552, %subblock_4015fc_2 ]
  %R560 = phi i64 [ %R551, %subblock_4015fc_2 ]
  %R559 = phi i64 [ %R550, %subblock_4015fc_2 ]
  %R558 = phi i64 [ %R549, %subblock_4015fc_2 ]
  %R557 = phi i64 [ %R548, %subblock_4015fc_2 ]
  %R556 = phi i64 [ %R547, %subblock_4015fc_2 ]
  %R555 = phi i64 [ %R546, %subblock_4015fc_2 ]
  ; # 401603: xor    eax,eax
  ; # 401605: mov    DWORD PTR [r14],eax
  ; *(r560) = 0x0 :: [32]
  %r656 = inttoptr i64 %R560 to i32*
  store i32 0, i32* %r656
  ; # 401608: lock or DWORD PTR [rsp],0x0
  ; r562 := *r556
  %r657 = inttoptr i64 %R556 to i32*
  %R562 = load i32* %r657
  ; *(r556) = r562
  %r659 = inttoptr i64 %R556 to i32*
  store i32 %R562, i32* %r659
  ; # 40160d: mov    eax,DWORD PTR [r14+0x4]
  ; r563 := (bv_add r560 0x4 :: [64])
  %R563 = add i64 %R560, 4
  ; r564 := *r563
  %r661 = inttoptr i64 %R563 to i32*
  %R564 = load i32* %r661
  ; # 401611: test   eax,eax
  ; r565 := (bv_eq r564 0x0 :: [32])
  %R565 = icmp eq i32 %R564, 0
  ; # 401613: je     40163e
  br i1 %R565, label %subblock_401603_1, label %subblock_401603_2
subblock_401603_1:
  br label %block_40163e
subblock_401603_2:
  br label %block_401615
block_401615:
  %R570 = phi i64 [ %R560, %subblock_401603_2 ]
  %R569 = phi i64 [ %R559, %subblock_401603_2 ]
  %R568 = phi i64 [ %R557, %subblock_401603_2 ]
  %R567 = phi i64 [ %R556, %subblock_401603_2 ]
  %R566 = phi i64 [ %R555, %subblock_401603_2 ]
  ; # 401615: mov    r9d,0xca
  ; # 40161b: mov    edx,0x1
  ; # 401620: mov    esi,0x81
  ; # 401625: mov    rax,r9
  ; # 401628: mov    rdi,r14
  ; # 40162b: syscall
  ; sys_futex
  %r669 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R570, i64 129, i64 1, i64 %R569, i64 %R568, i64 202, i64 202)
  %R571 = extractvalue { i64, i1 } %r669, 0
  br label %block_40162d
block_40162d:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R580 = phi i128 [ undef, %block_401615 ]
  %R579 = phi i64 [ %R570, %block_401615 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R578 = phi i64 [ undef, %block_401615 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R577 = phi i64 [ undef, %block_401615 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R576 = phi i64 [ undef, %block_401615 ]
  %R575 = phi i64 [ %R567, %block_401615 ]
  %R574 = phi i64 [ %R566, %block_401615 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R573 = phi i64 [ undef, %block_401615 ]
  %R572 = phi i64 [ %R571, %block_401615 ]
  ; # 40162d: cmp    rax,0xffffffffffffffda
  ; r581 := (bv_eq r572 0xffffffffffffffda :: [64])
  %R581 = icmp eq i64 %R572, 18446744073709551578
  ; # 401631: jne    40163e
  ; r582 := (bv_complement r581)
  %R582 = xor i1 %R581, -1
  br i1 %R582, label %subblock_40162d_1, label %subblock_40162d_2
subblock_40162d_1:
  br label %block_40163e
subblock_40162d_2:
  br label %block_401633
block_401633:
  %R589 = phi i64 [ %R579, %subblock_40162d_2 ]
  %R588 = phi i64 [ %R578, %subblock_40162d_2 ]
  %R587 = phi i64 [ %R577, %subblock_40162d_2 ]
  %R586 = phi i64 [ %R576, %subblock_40162d_2 ]
  %R585 = phi i64 [ %R575, %subblock_40162d_2 ]
  %R584 = phi i64 [ %R574, %subblock_40162d_2 ]
  %R583 = phi i64 [ %R573, %subblock_40162d_2 ]
  ; # 401633: mov    rax,r9
  ; # 401636: mov    rdi,r14
  ; # 401639: mov    rsi,rdx
  ; # 40163c: syscall
  ; sys_futex
  %r689 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R589, i64 %R583, i64 %R583, i64 %R588, i64 %R586, i64 %R587, i64 202)
  %R590 = extractvalue { i64, i1 } %r689, 0
  br label %block_40163e
block_40163e:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R595 = phi i128 [ undef, %block_401633 ], [ %R580, %subblock_40162d_1 ], [ %R561, %subblock_401603_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R594 = phi i64 [ undef, %block_401633 ], [ %R577, %subblock_40162d_1 ], [ %R558, %subblock_401603_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R593 = phi i64 [ undef, %block_401633 ], [ %R576, %subblock_40162d_1 ], [ %R557, %subblock_401603_1 ]
  %R592 = phi i64 [ %R585, %block_401633 ], [ %R575, %subblock_40162d_1 ], [ %R556, %subblock_401603_1 ]
  %R591 = phi i64 [ %R584, %block_401633 ], [ %R574, %subblock_40162d_1 ], [ %R555, %subblock_401603_1 ]
  ; # 40163e: mov    rax,QWORD PTR [rbx+0x8]
  ; r596 := (bv_add r591 0x8 :: [64])
  %R596 = add i64 %R591, 8
  ; r597 := *r596
  %r697 = inttoptr i64 %R596 to i64*
  %R597 = load i64* %r697
  br label %block_401642
block_401642:
  %R603 = phi i128 [ %R595, %block_40163e ], [ %R552, %subblock_4015fc_1 ], [ %R313, %block_401469 ], [ %R717, %block_401722 ]
  %R602 = phi i64 [ %R594, %block_40163e ], [ %R549, %subblock_4015fc_1 ], [ %R311, %block_401469 ], [ %R715, %block_401722 ]
  %R601 = phi i64 [ %R593, %block_40163e ], [ %R548, %subblock_4015fc_1 ], [ %R310, %block_401469 ], [ %R714, %block_401722 ]
  %R600 = phi i64 [ %R592, %block_40163e ], [ %R547, %subblock_4015fc_1 ], [ %R308, %block_401469 ], [ %R712, %block_401722 ]
  %R599 = phi i64 [ %R591, %block_40163e ], [ %R546, %subblock_4015fc_1 ], [ %R319, %block_401469 ], [ %R716, %block_401722 ]
  %R598 = phi i64 [ %R597, %block_40163e ], [ %R545, %subblock_4015fc_1 ], [ %R323, %block_401469 ], [ %R719, %block_401722 ]
  ; # 401642: and    rax,0xfffffffffffffffe
  ; r604 := (bv_and r598 0xfffffffffffffffe :: [64])
  %R604 = and i64 %R598, 18446744073709551614
  ; # 401646: lea    rdx,[rax-0x10]
  ; r605 := (bv_add r604 0xfffffffffffffff0 :: [64])
  %R605 = add i64 %R604, 18446744073709551600
  ; # 40164a: cmp    rdx,QWORD PTR [rsp+0x10]
  ; r606 := (bv_add r600 0x10 :: [64])
  %R606 = add i64 %R600, 16
  ; r607 := *r606
  %r708 = inttoptr i64 %R606 to i64*
  %R607 = load i64* %r708
  ; # 40164f: jbe    401683
  ; r608 := (bv_ule r605 r607)
  %R608 = icmp ule i64 %R605, %R607
  br i1 %R608, label %subblock_401642_1, label %subblock_401642_2
subblock_401642_1:
  br label %block_401683
subblock_401642_2:
  br label %block_401651
block_401651:
  %R614 = phi i128 [ %R603, %subblock_401642_2 ]
  %R613 = phi i64 [ %R602, %subblock_401642_2 ]
  %R612 = phi i64 [ %R601, %subblock_401642_2 ]
  %R611 = phi i64 [ %R600, %subblock_401642_2 ]
  %R610 = phi i64 [ %R599, %subblock_401642_2 ]
  %R609 = phi i64 [ %R604, %subblock_401642_2 ]
  ; # 401651: mov    rdi,QWORD PTR [rsp+0x10]
  ; r615 := (bv_add r611 0x10 :: [64])
  %R615 = add i64 %R611, 16
  ; r616 := *r615
  %r718 = inttoptr i64 %R615 to i64*
  %R616 = load i64* %r718
  ; # 401656: mov    rcx,rax
  ; # 401659: lea    rdx,[rbx+rdi*1]
  ; r617 := (bv_add r610 r616)
  %R617 = add i64 %R610, %R616
  ; # 40165d: mov    rsi,rdi
  ; # 401660: sub    rcx,rdi
  ; r618 := (bv_sub r609 r616)
  %R618 = sub i64 %R609, %R616
  ; # 401663: or     rsi,0x1
  ; r619 := (bv_or r616 0x1 :: [64])
  %R619 = or i64 %R616, 1
  ; # 401667: or     rcx,0x1
  ; r620 := (bv_or r618 0x1 :: [64])
  %R620 = or i64 %R618, 1
  ; # 40166b: lea    rdi,[rdx+0x10]
  ; r621 := (bv_add r617 0x10 :: [64])
  %R621 = add i64 %R617, 16
  ; # 40166f: mov    QWORD PTR [rdx],rsi
  ; *(r617) = r619
  %r725 = inttoptr i64 %R617 to i64*
  store i64 %R619, i64* %r725
  ; # 401672: mov    QWORD PTR [rdx+0x8],rcx
  ; r622 := (bv_add r617 0x8 :: [64])
  %R622 = add i64 %R617, 8
  ; *(r622) = r620
  %r727 = inttoptr i64 %R622 to i64*
  store i64 %R620, i64* %r727
  ; # 401676: mov    QWORD PTR [rbx+rax*1],rcx
  ; r623 := (bv_add r610 r609)
  %R623 = add i64 %R610, %R609
  ; *(r623) = r620
  %r729 = inttoptr i64 %R623 to i64*
  store i64 %R620, i64* %r729
  ; # 40167a: mov    QWORD PTR [rbx+0x8],rsi
  ; r624 := (bv_add r610 0x8 :: [64])
  %R624 = add i64 %R610, 8
  ; *(r624) = r619
  %r731 = inttoptr i64 %R624 to i64*
  store i64 %R619, i64* %r731
  ; # 40167e: call   400e40
  %r732 = bitcast i128 %R614 to <2 x double>
  %r733 = call { i64, i64, <2 x double> } @F400e40(i64 %R621, i64 %R619, i64 %R617, i64 %R620, i64 %R612, i64 %R613, <2 x double> %r732)
  %R625 = extractvalue { i64, i64, <2 x double> } %r733, 0
  %R626 = extractvalue { i64, i64, <2 x double> } %r733, 1
  %r736 = extractvalue { i64, i64, <2 x double> } %r733, 2
  %R627 = bitcast <2 x double> %r736 to i128
  br label %block_401683
block_401683:
  %R630 = phi i128 [ %R627, %block_401651 ], [ %R603, %subblock_401642_1 ]
  %R629 = phi i64 [ %R610, %block_401651 ], [ %R599, %subblock_401642_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R628 = phi i64 [ undef, %block_401651 ], [ %R605, %subblock_401642_1 ]
  ; # 401683: add    rsp,0x38
  ; # 401687: lea    rax,[rbx+0x10]
  ; r631 := (bv_add r629 0x10 :: [64])
  %R631 = add i64 %R629, 16
  ; # 40168b: pop    rbx
  ; # 40168c: pop    rbp
  ; # 40168d: pop    r12
  ; # 40168f: pop    r13
  ; # 401691: pop    r14
  ; # 401693: pop    r15
  ; # 401695: ret
  %r742 = bitcast i128 %R630 to <2 x double>
  %r743 = insertvalue { i64, i64, <2 x double> } undef, i64 %R631, 0
  %r744 = insertvalue { i64, i64, <2 x double> } %r743, i64 %R628, 1
  %r745 = insertvalue { i64, i64, <2 x double> } %r744, <2 x double> %r742, 2
  ret { i64, i64, <2 x double> } %r745
block_401696:
  %R639 = phi i128 [ %R452, %subblock_401572_1 ]
  %R638 = phi i64 [ %R451, %subblock_401572_1 ]
  %R637 = phi i64 [ %R450, %subblock_401572_1 ]
  %R636 = phi i64 [ %R449, %subblock_401572_1 ]
  %R635 = phi i64 [ %R448, %subblock_401572_1 ]
  %R634 = phi i64 [ %R447, %subblock_401572_1 ]
  %R633 = phi i64 [ %R446, %subblock_401572_1 ]
  %R632 = phi i64 [ %R445, %subblock_401572_1 ]
  ; # 401696: cmp    r15d,0x3f
  ; r640 := (trunc r638 32)
  %R640 = trunc i64 %R638 to i32
  ; r641 := (bv_eq r640 0x3f :: [32])
  %R641 = icmp eq i32 %R640, 63
  ; # 40169a: je     401782
  br i1 %R641, label %subblock_401696_1, label %subblock_401696_2
subblock_401696_1:
  br label %block_401782
subblock_401696_2:
  br label %block_4016a0
block_4016a0:
  %R649 = phi i128 [ %R516, %subblock_4015b8_1 ], [ %R488, %subblock_401592_1 ], [ %R770, %block_40179f ], [ %R639, %subblock_401696_2 ], [ %R435, %subblock_401568_1 ]
  %R648 = phi i64 [ %R515, %subblock_4015b8_1 ], [ %R487, %subblock_401592_1 ], [ %R769, %block_40179f ], [ %R638, %subblock_401696_2 ], [ %R434, %subblock_401568_1 ]
  %R647 = phi i64 [ %R514, %subblock_4015b8_1 ], [ %R486, %subblock_401592_1 ], [ %R768, %block_40179f ], [ %R637, %subblock_401696_2 ], [ %R433, %subblock_401568_1 ]
  %R646 = phi i64 [ %R513, %subblock_4015b8_1 ], [ %R485, %subblock_401592_1 ], [ %R767, %block_40179f ], [ %R636, %subblock_401696_2 ], [ %R432, %subblock_401568_1 ]
  %R645 = phi i64 [ %R512, %subblock_4015b8_1 ], [ %R484, %subblock_401592_1 ], [ %R766, %block_40179f ], [ %R635, %subblock_401696_2 ], [ %R431, %subblock_401568_1 ]
  %R644 = phi i64 [ %R511, %subblock_4015b8_1 ], [ %R483, %subblock_401592_1 ], [ %R765, %block_40179f ], [ %R634, %subblock_401696_2 ], [ %R430, %subblock_401568_1 ]
  %R643 = phi i64 [ %R509, %subblock_4015b8_1 ], [ %R481, %subblock_401592_1 ], [ %R764, %block_40179f ], [ %R633, %subblock_401696_2 ], [ %R429, %subblock_401568_1 ]
  %R642 = phi i64 [ %R508, %subblock_4015b8_1 ], [ %R480, %subblock_401592_1 ], [ %R763, %block_40179f ], [ %R632, %subblock_401696_2 ], [ %R428, %subblock_401568_1 ]
  ; # 4016a0: mov    rax,QWORD PTR [rbx+0x18]
  ; r650 := (bv_add r642 0x18 :: [64])
  %R650 = add i64 %R642, 24
  ; r651 := *r650
  %r765 = inttoptr i64 %R650 to i64*
  %R651 = load i64* %r765
  ; # 4016a4: mov    rdx,QWORD PTR [rbx+0x10]
  ; r652 := (bv_add r642 0x10 :: [64])
  %R652 = add i64 %R642, 16
  ; r653 := *r652
  %r768 = inttoptr i64 %R652 to i64*
  %R653 = load i64* %r768
  ; # 4016a8: cmp    rax,rdx
  ; r654 := (bv_eq r651 r653)
  %R654 = icmp eq i64 %R651, %R653
  ; # 4016ab: je     4017d0
  br i1 %R654, label %subblock_4016a0_1, label %subblock_4016a0_2
subblock_4016a0_1:
  br label %block_4017d0
subblock_4016a0_2:
  br label %block_4016b1
block_4016b1:
  %R663 = phi i128 [ %R827, %block_4017dd ], [ %R649, %subblock_4016a0_2 ]
  %R662 = phi i64 [ %R826, %block_4017dd ], [ %R647, %subblock_4016a0_2 ]
  %R661 = phi i64 [ %R825, %block_4017dd ], [ %R646, %subblock_4016a0_2 ]
  %R660 = phi i64 [ %R824, %block_4017dd ], [ %R645, %subblock_4016a0_2 ]
  %R659 = phi i64 [ %R823, %block_4017dd ], [ %R644, %subblock_4016a0_2 ]
  %R658 = phi i64 [ %R822, %block_4017dd ], [ %R643, %subblock_4016a0_2 ]
  %R657 = phi i64 [ %R821, %block_4017dd ], [ %R642, %subblock_4016a0_2 ]
  %R656 = phi i64 [ %R833, %block_4017dd ], [ %R653, %subblock_4016a0_2 ]
  %R655 = phi i64 [ %R831, %block_4017dd ], [ %R651, %subblock_4016a0_2 ]
  ; # 4016b1: mov    QWORD PTR [rax+0x10],rdx
  ; r664 := (bv_add r655 0x10 :: [64])
  %R664 = add i64 %R655, 16
  ; *(r664) = r656
  %r781 = inttoptr i64 %R664 to i64*
  store i64 %R656, i64* %r781
  ; # 4016b5: mov    rdx,QWORD PTR [rbx+0x10]
  ; r665 := (bv_add r657 0x10 :: [64])
  %R665 = add i64 %R657, 16
  ; r666 := *r665
  %r783 = inttoptr i64 %R665 to i64*
  %R666 = load i64* %r783
  ; # 4016b9: mov    QWORD PTR [rdx+0x18],rax
  ; r667 := (bv_add r666 0x18 :: [64])
  %R667 = add i64 %R666, 24
  ; *(r667) = r655
  %r786 = inttoptr i64 %R667 to i64*
  store i64 %R655, i64* %r786
  ; # 4016bd: mov    rdx,QWORD PTR [rbx+0x8]
  ; r668 := (bv_add r657 0x8 :: [64])
  %R668 = add i64 %R657, 8
  ; r669 := *r668
  %r788 = inttoptr i64 %R668 to i64*
  %R669 = load i64* %r788
  ; # 4016c1: mov    rax,rdx
  ; # 4016c4: and    rdx,0xfffffffffffffffe
  ; r670 := (bv_and r669 0xfffffffffffffffe :: [64])
  %R670 = and i64 %R669, 18446744073709551614
  ; # 4016c8: or     rax,0x1
  ; r671 := (bv_or r669 0x1 :: [64])
  %R671 = or i64 %R669, 1
  ; # 4016cc: mov    QWORD PTR [rbx+0x8],rax
  ; *(r668) = r671
  %r792 = inttoptr i64 %R668 to i64*
  store i64 %R671, i64* %r792
  ; # 4016d0: or     QWORD PTR [rbx+rdx*1],0x1
  ; r672 := (bv_add r657 r670)
  %R672 = add i64 %R657, %R670
  ; r673 := *r672
  %r794 = inttoptr i64 %R672 to i64*
  %R673 = load i64* %r794
  ; r674 := (bv_or r673 0x1 :: [64])
  %R674 = or i64 %R673, 1
  ; *(r672) = r674
  %r797 = inttoptr i64 %R672 to i64*
  store i64 %R674, i64* %r797
  ; # 4016d5: jmp    4015fc
  br label %block_4015fc
block_4016da:
  %R680 = phi i128 [ %R331, %subblock_4014a0_1 ]
  %R679 = phi i64 [ %R330, %subblock_4014a0_1 ]
  %R678 = phi i64 [ %R329, %subblock_4014a0_1 ]
  %R677 = phi i64 [ %R328, %subblock_4014a0_1 ]
  %R676 = phi i64 [ %R333, %subblock_4014a0_1 ]
  %R675 = phi i64 [ %R326, %subblock_4014a0_1 ]
  ; # 4016da: mov    rcx,rdi
  ; # 4016dd: shr    rcx,0x5
  ; r681 := (bv_shr r676 0x5 :: [64])
  %R681 = lshr i64 %R676, 5
  ; # 4016e1: sub    rcx,0x1
  ; r682 := (bv_add r681 0xffffffffffffffff :: [64])
  %R682 = add i64 %R681, 18446744073709551615
  ; # 4016e5: cmp    rcx,0x20
  ; r683 := (bv_ult r682 0x20 :: [64])
  %R683 = icmp ult i64 %R682, 32
  ; r684 := (bv_eq r681 0x21 :: [64])
  %R684 = icmp eq i64 %R681, 33
  ; # 4016e9: jbe    401842
  ; r685 := (bv_or r683 r684)
  %R685 = or i1 %R683, %R684
  br i1 %R685, label %subblock_4016da_1, label %subblock_4016da_2
subblock_4016da_1:
  br label %block_401842
subblock_4016da_2:
  br label %block_4016ef
block_4016ef:
  %R690 = phi i64 [ %R679, %subblock_4016da_2 ]
  %R689 = phi i64 [ %R678, %subblock_4016da_2 ]
  %R688 = phi i64 [ %R677, %subblock_4016da_2 ]
  %R687 = phi i64 [ %R675, %subblock_4016da_2 ]
  %R686 = phi i64 [ %R682, %subblock_4016da_2 ]
  ; # 4016ef: pxor   xmm0,xmm0
  ; # 4016f3: mov    r14,0xffffffffffffffff
  ; # 4016fa: cvtsi2ss xmm0,ecx
  ; r691 := (trunc r686 32)
  %R691 = trunc i64 %R686 to i32
  ; r692 := (fpFromBV r691 single)
  %R692 = sitofp i32 %R691 to float
  ; r693 := (uext r692 128)
  %R693 = zext i32 %R692 to i128
  ; # 4016fe: movq   eax,xmm0
  ; # 401702: add    eax,0x1fffff
  ; r694 := (bv_add r692 0x1fffff :: [32])
  %R694 = add i32 %R692, 2097151
  ; # 401707: shr    eax,0x15
  ; r695 := (bv_shr r694 0x15 :: [32])
  %R695 = lshr i32 %R694, 21
  ; # 40170a: sub    eax,0x1f0
  ; r696 := (bv_add r695 0xfffffe10 :: [32])
  %R696 = add i32 %R695, 4294966800
  ; r697 := (trunc r696 8)
  %R697 = trunc i32 %R696 to i8
  ; # 40170f: mov    ecx,eax
  ; # 401711: mov    DWORD PTR [rsp+0x1c],eax
  ; r698 := (bv_add r687 0x1c :: [64])
  %R698 = add i64 %R687, 28
  ; *(r698) = r696
  %r822 = inttoptr i64 %R698 to i32*
  store i32 %R696, i32* %r822
  ; # 401715: shl    r14,cl
  ; r699 := (bv_and r697 0x3f :: [8])
  %R699 = and i8 %R697, 63
  ; r700 := (bv_eq r699 0x0 :: [8])
  %R700 = icmp eq i8 %R699, 0
  br i1 %R700, label %subblock_4016ef_1, label %subblock_4016ef_2
subblock_4016ef_1:
  br label %block_401718
subblock_4016ef_2:
  ; r701 := (trunc r696 8)
  %R701 = trunc i32 %R696 to i8
  ; r702 := (bv_and r701 0x3f :: [8])
  %R702 = and i8 %R701, 63
  ; r703 := (uext r702 64)
  %R703 = zext i8 %R702 to i64
  ; r704 := (bv_shl 0xffffffffffffffff :: [64] r703)
  %R704 = shl i64 18446744073709551615, %R703
  br label %block_401718
block_401718:
  %R710 = phi i128 [ %R693, %subblock_4016ef_2 ], [ %R693, %subblock_4016ef_1 ]
  %R709 = phi i64 [ %R704, %subblock_4016ef_2 ], [ 18446744073709551615, %subblock_4016ef_1 ]
  %R708 = phi i64 [ %R690, %subblock_4016ef_2 ], [ %R690, %subblock_4016ef_1 ]
  %R707 = phi i64 [ %R689, %subblock_4016ef_2 ], [ %R689, %subblock_4016ef_1 ]
  %R706 = phi i64 [ %R688, %subblock_4016ef_2 ], [ %R688, %subblock_4016ef_1 ]
  %R705 = phi i64 [ %R687, %subblock_4016ef_2 ], [ %R687, %subblock_4016ef_1 ]
  ; # 401718: mov    QWORD PTR [rsp+0x8],r14
  ; r711 := (bv_add r705 0x8 :: [64])
  %R711 = add i64 %R705, 8
  ; *(r711) = r709
  %r836 = inttoptr i64 %R711 to i64*
  store i64 %R709, i64* %r836
  ; # 40171d: jmp    4012c8
  br label %block_4012c8
block_401722:
  %R717 = phi i128 [ %R305, %subblock_401461_1 ]
  %R716 = phi i64 [ %R304, %subblock_401461_1 ]
  %R715 = phi i64 [ %R303, %subblock_401461_1 ]
  %R714 = phi i64 [ %R302, %subblock_401461_1 ]
  %R713 = phi i64 [ %R301, %subblock_401461_1 ]
  %R712 = phi i64 [ %R300, %subblock_401461_1 ]
  ; # 401722: mov    rax,QWORD PTR [rbp-0x8]
  ; r718 := (bv_add r713 0xfffffffffffffff8 :: [64])
  %R718 = add i64 %R713, 18446744073709551608
  ; r719 := *r718
  %r844 = inttoptr i64 %R718 to i64*
  %R719 = load i64* %r844
  ; # 401726: mov    rbx,r12
  ; # 401729: jmp    401642
  br label %block_401642
block_40172e:
  %R721 = phi i128 [ %R160, %subblock_401388_1 ]
  %R720 = phi i64 [ %R159, %subblock_401388_1 ]
  ; # 40172e: mov    eax,0x1
  ; # 401733: xchg   DWORD PTR [rip+0x20299f],eax
  ; r722 := *0x6040d8 :: [64]
  %r848 = inttoptr i64 6308056 to i32*
  %R722 = load i32* %r848
  ; *(0x6040d8 :: [64]) = 0x1 :: [32]
  %r850 = inttoptr i64 6308056 to i32*
  store i32 1, i32* %r850
  ; # 401739: test   eax,eax
  ; r723 := (bv_eq r722 0x0 :: [32])
  %R723 = icmp eq i32 %R722, 0
  ; # 40173b: je     4013a4
  br i1 %R723, label %subblock_40172e_1, label %subblock_40172e_2
subblock_40172e_1:
  br label %block_4013a4
subblock_40172e_2:
  br label %block_401741
block_401741:
  %R725 = phi i128 [ %R721, %subblock_40172e_2 ]
  %R724 = phi i64 [ %R720, %subblock_40172e_2 ]
  ; # 401741: nop    [rax]
  br label %block_401748
block_401748:
  %R727 = phi i128 [ %R732, %subblock_401761_1 ], [ %R725, %block_401741 ]
  %R726 = phi i64 [ %R731, %subblock_401761_1 ], [ %R724, %block_401741 ]
  ; # 401748: mov    ecx,0x1
  ; # 40174d: mov    edx,0x1
  ; # 401752: mov    esi,0x6040dc
  ; # 401757: mov    edi,0x6040d8
  ; # 40175c: call   40241c
  ; r728 := (bv_add r726 0xfffffffffffffff8 :: [64])
  %R728 = add i64 %R726, 18446744073709551608
  ; r730 := (bv_add r728 0x8 :: [64])
  %R730 = add i64 %R728, 8
  %r858 = bitcast i128 %R727 to <2 x double>
  %r859 = call { i64, i64, <2 x double> } @F40241c(i64 6308056, i64 6308060, i64 1, i64 1, <2 x double> %r858)
  %r860 = extractvalue { i64, i64, <2 x double> } %r859, 2
  %R729 = bitcast <2 x double> %r860 to i128
  br label %block_401761
block_401761:
  %R732 = phi i128 [ %R729, %block_401748 ]
  %R731 = phi i64 [ %R730, %block_401748 ]
  ; # 401761: mov    eax,0x1
  ; # 401766: xchg   DWORD PTR [rip+0x20296c],eax
  ; r733 := *0x6040d8 :: [64]
  %r864 = inttoptr i64 6308056 to i32*
  %R733 = load i32* %r864
  ; *(0x6040d8 :: [64]) = 0x1 :: [32]
  %r866 = inttoptr i64 6308056 to i32*
  store i32 1, i32* %r866
  ; # 40176c: test   eax,eax
  ; r734 := (bv_eq r733 0x0 :: [32])
  %R734 = icmp eq i32 %R733, 0
  ; # 40176e: jne    401748
  ; r735 := (bv_complement r734)
  %R735 = xor i1 %R734, -1
  br i1 %R735, label %subblock_401761_1, label %subblock_401761_2
subblock_401761_1:
  br label %block_401748
subblock_401761_2:
  br label %block_401770
block_401770:
  %R737 = phi i128 [ %R732, %subblock_401761_2 ]
  %R736 = phi i64 [ %R731, %subblock_401761_2 ]
  ; # 401770: jmp    4013a4
  br label %block_4013a4
block_401775:
  %R745 = phi i128 [ %R193, %subblock_4013b7_1 ]
  %R744 = phi i64 [ %R192, %subblock_4013b7_1 ]
  %R743 = phi i64 [ %R191, %subblock_4013b7_1 ]
  %R742 = phi i64 [ %R190, %subblock_4013b7_1 ]
  %R741 = phi i64 [ %R189, %subblock_4013b7_1 ]
  %R740 = phi i64 [ %R188, %subblock_4013b7_1 ]
  %R739 = phi i64 [ %R187, %subblock_4013b7_1 ]
  %R738 = phi i64 [ %R186, %subblock_4013b7_1 ]
  ; # 401775: mov    rdx,QWORD PTR [rsp+0x28]
  ; r746 := (bv_add r740 0x28 :: [64])
  %R746 = add i64 %R740, 40
  ; r747 := *r746
  %r880 = inttoptr i64 %R746 to i64*
  %R747 = load i64* %r880
  ; # 40177a: mov    rbp,rax
  ; # 40177d: jmp    4013de
  br label %block_4013de
block_401782:
  %R755 = phi i128 [ %R639, %subblock_401696_1 ]
  %R754 = phi i64 [ %R638, %subblock_401696_1 ]
  %R753 = phi i64 [ %R637, %subblock_401696_1 ]
  %R752 = phi i64 [ %R636, %subblock_401696_1 ]
  %R751 = phi i64 [ %R635, %subblock_401696_1 ]
  %R750 = phi i64 [ %R634, %subblock_401696_1 ]
  %R749 = phi i64 [ %R633, %subblock_401696_1 ]
  %R748 = phi i64 [ %R632, %subblock_401696_1 ]
  ; # 401782: mov    rdx,QWORD PTR [rbx+0x8]
  ; r756 := (bv_add r748 0x8 :: [64])
  %R756 = add i64 %R748, 8
  ; r757 := *r756
  %r891 = inttoptr i64 %R756 to i64*
  %R757 = load i64* %r891
  ; # 401786: and    rdx,0xfffffffffffffffe
  ; r758 := (bv_and r757 0xfffffffffffffffe :: [64])
  %R758 = and i64 %R757, 18446744073709551614
  ; # 40178a: mov    rsi,rdx
  ; # 40178d: sub    rsi,QWORD PTR [rsp+0x10]
  ; r759 := (bv_add r749 0x10 :: [64])
  %R759 = add i64 %R749, 16
  ; r760 := *r759
  %r895 = inttoptr i64 %R759 to i64*
  %R760 = load i64* %r895
  ; r761 := (bv_sub r758 r760)
  %R761 = sub i64 %R758, %R760
  ; # 401792: cmp    rsi,0x38000
  ; # 401799: ja     401592
  ; r762 := (bv_ult 0x38000 :: [64] r761)
  %R762 = icmp ult i64 229376, %R761
  br i1 %R762, label %subblock_401782_1, label %subblock_401782_2
subblock_401782_1:
  br label %block_401592
subblock_401782_2:
  br label %block_40179f
block_40179f:
  %R770 = phi i128 [ %R755, %subblock_401782_2 ]
  %R769 = phi i64 [ %R754, %subblock_401782_2 ]
  %R768 = phi i64 [ %R753, %subblock_401782_2 ]
  %R767 = phi i64 [ %R752, %subblock_401782_2 ]
  %R766 = phi i64 [ %R751, %subblock_401782_2 ]
  %R765 = phi i64 [ %R750, %subblock_401782_2 ]
  %R764 = phi i64 [ %R749, %subblock_401782_2 ]
  %R763 = phi i64 [ %R748, %subblock_401782_2 ]
  ; # 40179f: jmp    4016a0
  br label %block_4016a0
block_4017a4:
  %R781 = phi i128 [ %R504, %subblock_4015a7_1 ]
  %R780 = phi i64 [ %R503, %subblock_4015a7_1 ]
  %R779 = phi i64 [ %R502, %subblock_4015a7_1 ]
  %R778 = phi i64 [ %R501, %subblock_4015a7_1 ]
  %R777 = phi i64 [ %R500, %subblock_4015a7_1 ]
  %R776 = phi i64 [ %R499, %subblock_4015a7_1 ]
  %R775 = phi i64 [ %R498, %subblock_4015a7_1 ]
  %R774 = phi i64 [ %R497, %subblock_4015a7_1 ]
  %R773 = phi i64 [ %R496, %subblock_4015a7_1 ]
  %R772 = phi i64 [ %R495, %subblock_4015a7_1 ]
  %R771 = phi i64 [ %R494, %subblock_4015a7_1 ]
  ; # 4017a4: pxor   xmm1,xmm1
  ; # 4017a8: cvtsi2ss xmm1,eax
  ; r782 := (trunc r771 32)
  %R782 = trunc i64 %R771 to i32
  ; r783 := (fpFromBV r782 single)
  %R783 = sitofp i32 %R782 to float
  ; # 4017ac: movq   eax,xmm1
  ; # 4017b0: shr    eax,0x15
  ; r784 := (bv_shr r783 0x15 :: [32])
  %R784 = lshr i32 %R783, 21
  ; r785 := (uext r784 64)
  %R785 = zext i32 %R784 to i64
  ; # 4017b3: lea    ecx,[rax-0x1f0]
  ; r786 := (bv_add r785 0xfffffffffffffe10 :: [64])
  %R786 = add i64 %R785, 18446744073709551120
  ; r787 := (trunc r786 32)
  %R787 = trunc i64 %R786 to i32
  ; r788 := (uext r787 64)
  %R788 = zext i32 %R787 to i64
  ; # 4017b9: jmp    4015b8
  br label %block_4015b8
block_4017be:
  %R792 = phi i128 [ %R20, %subblock_4012a5_1 ]
  %R791 = phi i64 [ %R16, %subblock_4012a5_1 ]
  %R790 = phi i64 [ %R15, %subblock_4012a5_1 ]
  %R789 = phi i64 [ %R13, %subblock_4012a5_1 ]
  ; # 4017be: call   402681
  %r929 = bitcast i128 %R792 to <2 x double>
  %r930 = call { i64, i64, <2 x double> } @F402681(i64 %R791, i64 %R790, i64 %R789, <2 x double> %r929)
  %R793 = extractvalue { i64, i64, <2 x double> } %r930, 0
  %R794 = extractvalue { i64, i64, <2 x double> } %r930, 1
  %r933 = extractvalue { i64, i64, <2 x double> } %r930, 2
  %R795 = bitcast <2 x double> %r933 to i128
  br label %block_4017c3
block_4017c3:
  %R798 = phi i128 [ %R795, %block_4017be ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R797 = phi i64 [ undef, %block_4017be ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R796 = phi i64 [ undef, %block_4017be ]
  ; # 4017c3: mov    DWORD PTR [rax],0xc
  ; *(r796) = 0xc :: [32]
  %r938 = inttoptr i64 %R796 to i32*
  store i32 12, i32* %r938
  ; # 4017c9: xor    eax,eax
  ; # 4017cb: jmp    401506
  br label %block_401506
block_4017d0:
  %R806 = phi i128 [ %R649, %subblock_4016a0_1 ]
  %R805 = phi i64 [ %R648, %subblock_4016a0_1 ]
  %R804 = phi i64 [ %R647, %subblock_4016a0_1 ]
  %R803 = phi i64 [ %R646, %subblock_4016a0_1 ]
  %R802 = phi i64 [ %R645, %subblock_4016a0_1 ]
  %R801 = phi i64 [ %R644, %subblock_4016a0_1 ]
  %R800 = phi i64 [ %R643, %subblock_4016a0_1 ]
  %R799 = phi i64 [ %R642, %subblock_4016a0_1 ]
  ; # 4017d0: mov    ecx,r15d
  ; # 4017d3: mov    rax,0xfffffffffffffffe
  ; # 4017da: rol    rax,cl
  ; r807 := (trunc r805 8)
  %R807 = trunc i64 %R805 to i8
  ; r808 := (uext r807 64)
  %R808 = zext i8 %R807 to i64
  ; r809 := (bv_and r808 0x3f :: [64])
  %R809 = and i64 %R808, 63
  ; r810 := (bv_shl 0xfffffffffffffffe :: [64] r809)
  %R810 = shl i64 18446744073709551614, %R809
  ; r811 := (bv_sub 0x40 :: [64] r809)
  %R811 = sub i64 64, %R809
  ; r812 := (bv_shr 0xfffffffffffffffe :: [64] r811)
  %R812 = lshr i64 18446744073709551614, %R811
  ; r813 := (bv_or r810 r812)
  %R813 = or i64 %R810, %R812
  ; r814 := (bv_eq r809 0x0 :: [64])
  %R814 = icmp eq i64 %R809, 0
  ; r815 := (bv_complement r814)
  %R815 = xor i1 %R814, -1
  br i1 %R815, label %subblock_4017d0_1, label %subblock_4017d0_4
subblock_4017d0_1:
  ; r816 := (trunc r805 8)
  %R816 = trunc i64 %R805 to i8
  ; r817 := (uext r816 64)
  %R817 = zext i8 %R816 to i64
  ; r818 := (bv_and r817 0x3f :: [64])
  %R818 = and i64 %R817, 63
  ; r819 := (bv_eq r818 0x1 :: [64])
  %R819 = icmp eq i64 %R818, 1
  br i1 %R819, label %subblock_4017d0_2, label %subblock_4017d0_3
subblock_4017d0_2:
  br label %block_4017dd
subblock_4017d0_3:
  br label %block_4017dd
subblock_4017d0_4:
  br label %block_4017dd
block_4017dd:
  %R827 = phi i128 [ %R806, %subblock_4017d0_4 ], [ %R806, %subblock_4017d0_3 ], [ %R806, %subblock_4017d0_2 ]
  %R826 = phi i64 [ %R804, %subblock_4017d0_4 ], [ %R804, %subblock_4017d0_3 ], [ %R804, %subblock_4017d0_2 ]
  %R825 = phi i64 [ %R803, %subblock_4017d0_4 ], [ %R803, %subblock_4017d0_3 ], [ %R803, %subblock_4017d0_2 ]
  %R824 = phi i64 [ %R802, %subblock_4017d0_4 ], [ %R802, %subblock_4017d0_3 ], [ %R802, %subblock_4017d0_2 ]
  %R823 = phi i64 [ %R801, %subblock_4017d0_4 ], [ %R801, %subblock_4017d0_3 ], [ %R801, %subblock_4017d0_2 ]
  %R822 = phi i64 [ %R800, %subblock_4017d0_4 ], [ %R800, %subblock_4017d0_3 ], [ %R800, %subblock_4017d0_2 ]
  %R821 = phi i64 [ %R799, %subblock_4017d0_4 ], [ %R799, %subblock_4017d0_3 ], [ %R799, %subblock_4017d0_2 ]
  %R820 = phi i64 [ %R813, %subblock_4017d0_4 ], [ %R813, %subblock_4017d0_3 ], [ %R813, %subblock_4017d0_2 ]
  ; # 4017dd: lock and QWORD PTR [rip+0x2028fb],rax
  ; r828 := *0x6040e0 :: [64]
  %r968 = inttoptr i64 6308064 to i64*
  %R828 = load i64* %r968
  ; r829 := (bv_and r828 r820)
  %R829 = and i64 %R828, %R820
  ; *(0x6040e0 :: [64]) = r829
  %r971 = inttoptr i64 6308064 to i64*
  store i64 %R829, i64* %r971
  ; # 4017e5: mov    rax,QWORD PTR [rbx+0x18]
  ; r830 := (bv_add r821 0x18 :: [64])
  %R830 = add i64 %R821, 24
  ; r831 := *r830
  %r973 = inttoptr i64 %R830 to i64*
  %R831 = load i64* %r973
  ; # 4017e9: mov    rdx,QWORD PTR [rbx+0x10]
  ; r832 := (bv_add r821 0x10 :: [64])
  %R832 = add i64 %R821, 16
  ; r833 := *r832
  %r976 = inttoptr i64 %R832 to i64*
  %R833 = load i64* %r976
  ; # 4017ed: jmp    4016b1
  br label %block_4016b1
block_4017f2:
  %R838 = phi i128 [ %R184, %subblock_4013ae_1 ]
  %R837 = phi i64 [ %R183, %subblock_4013ae_1 ]
  %R836 = phi i64 [ %R182, %subblock_4013ae_1 ]
  %R835 = phi i64 [ %R179, %subblock_4013ae_1 ]
  %R834 = phi i64 [ %R178, %subblock_4013ae_1 ]
  ; # 4017f2: mov    eax,DWORD PTR [rip+0x2028e0]
  ; r839 := *0x6040d8 :: [64]
  %r983 = inttoptr i64 6308056 to i32*
  %R839 = load i32* %r983
  ; # 4017f8: mov    edi,0x6040d8
  ; # 4017fd: test   eax,eax
  ; r840 := (bv_eq r839 0x0 :: [32])
  %R840 = icmp eq i32 %R839, 0
  ; # 4017ff: je     40183b
  br i1 %R840, label %subblock_4017f2_1, label %subblock_4017f2_2
subblock_4017f2_1:
  br label %block_40183b
subblock_4017f2_2:
  br label %block_401801
block_401801:
  %R846 = phi i128 [ %R838, %subblock_4017f2_2 ]
  %R845 = phi i64 [ %R837, %subblock_4017f2_2 ]
  %R844 = phi i64 [ %R836, %subblock_4017f2_2 ]
  %R843 = phi i64 [ 6308056, %subblock_4017f2_2 ]
  %R842 = phi i64 [ %R835, %subblock_4017f2_2 ]
  %R841 = phi i64 [ %R834, %subblock_4017f2_2 ]
  ; # 401801: xor    eax,eax
  ; # 401803: mov    DWORD PTR [rip+0x2028cf],eax
  ; *(0x6040d8 :: [64]) = 0x0 :: [32]
  %r992 = inttoptr i64 6308056 to i32*
  store i32 0, i32* %r992
  ; # 401809: lock or DWORD PTR [rsp],0x0
  ; r847 := *r842
  %r993 = inttoptr i64 %R842 to i32*
  %R847 = load i32* %r993
  ; *(r842) = r847
  %r995 = inttoptr i64 %R842 to i32*
  store i32 %R847, i32* %r995
  ; # 40180e: mov    eax,DWORD PTR [rip+0x2028c8]
  ; r848 := *0x6040dc :: [64]
  %r996 = inttoptr i64 6308060 to i32*
  %R848 = load i32* %r996
  ; # 401814: test   eax,eax
  ; r849 := (bv_eq r848 0x0 :: [32])
  %R849 = icmp eq i32 %R848, 0
  ; # 401816: je     40183b
  br i1 %R849, label %subblock_401801_1, label %subblock_401801_2
subblock_401801_1:
  br label %block_40183b
subblock_401801_2:
  br label %block_401818
block_401818:
  %R852 = phi i64 [ %R845, %subblock_401801_2 ]
  %R851 = phi i64 [ %R844, %subblock_401801_2 ]
  %R850 = phi i64 [ %R843, %subblock_401801_2 ]
  ; # 401818: mov    r8d,0xca
  ; # 40181e: mov    edx,0x1
  ; # 401823: mov    esi,0x81
  ; # 401828: mov    rax,r8
  ; # 40182b: syscall
  ; sys_futex
  %r1002 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R850, i64 129, i64 1, i64 %R852, i64 202, i64 %R851, i64 202)
  %R853 = extractvalue { i64, i1 } %r1002, 0
  br label %block_40182d
block_40182d:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R860 = phi i128 [ undef, %block_401818 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r10
  %R859 = phi i64 [ undef, %block_401818 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r9
  %R858 = phi i64 [ undef, %block_401818 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R857 = phi i64 [ undef, %block_401818 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R856 = phi i64 [ undef, %block_401818 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R855 = phi i64 [ undef, %block_401818 ]
  %R854 = phi i64 [ %R853, %block_401818 ]
  ; # 40182d: cmp    rax,0xffffffffffffffda
  ; r861 := (bv_eq r854 0xffffffffffffffda :: [64])
  %R861 = icmp eq i64 %R854, 18446744073709551578
  ; # 401831: jne    40183b
  ; r862 := (bv_complement r861)
  %R862 = xor i1 %R861, -1
  br i1 %R862, label %subblock_40182d_1, label %subblock_40182d_2
subblock_40182d_1:
  br label %block_40183b
subblock_40182d_2:
  br label %block_401833
block_401833:
  %R867 = phi i64 [ %R859, %subblock_40182d_2 ]
  %R866 = phi i64 [ %R858, %subblock_40182d_2 ]
  %R865 = phi i64 [ %R857, %subblock_40182d_2 ]
  %R864 = phi i64 [ %R856, %subblock_40182d_2 ]
  %R863 = phi i64 [ %R855, %subblock_40182d_2 ]
  ; # 401833: mov    rax,r8
  ; # 401836: mov    rsi,rdx
  ; # 401839: syscall
  ; sys_futex
  %r1018 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R864, i64 %R863, i64 %R863, i64 %R867, i64 %R865, i64 %R866, i64 202)
  %R868 = extractvalue { i64, i1 } %r1018, 0
  br label %block_40183b
block_40183b:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R870 = phi i128 [ %R285, %subblock_401450_1 ], [ undef, %block_401833 ], [ %R860, %subblock_40182d_1 ], [ %R846, %subblock_401801_1 ], [ %R838, %subblock_4017f2_1 ], [ %R346, %subblock_4014e8_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R869 = phi i64 [ %R279, %subblock_401450_1 ], [ undef, %block_401833 ], [ %R855, %subblock_40182d_1 ], [ %R841, %subblock_401801_1 ], [ %R834, %subblock_4017f2_1 ], [ %R344, %subblock_4014e8_1 ]
  ; # 40183b: xor    eax,eax
  ; # 40183d: jmp    401506
  br label %block_401506
block_401842:
  %R876 = phi i128 [ %R680, %subblock_4016da_1 ]
  %R875 = phi i64 [ %R679, %subblock_4016da_1 ]
  %R874 = phi i64 [ %R678, %subblock_4016da_1 ]
  %R873 = phi i64 [ %R677, %subblock_4016da_1 ]
  %R872 = phi i64 [ %R675, %subblock_4016da_1 ]
  %R871 = phi i64 [ %R682, %subblock_4016da_1 ]
  ; # 401842: mov    r14,0xffffffffffffffff
  ; # 401849: mov    DWORD PTR [rsp+0x1c],ecx
  ; r877 := (trunc r871 32)
  %R877 = trunc i64 %R871 to i32
  ; r878 := (bv_add r872 0x1c :: [64])
  %R878 = add i64 %R872, 28
  ; *(r878) = r877
  %r1030 = inttoptr i64 %R878 to i32*
  store i32 %R877, i32* %r1030
  ; # 40184d: shl    r14,cl
  ; r879 := (trunc r871 8)
  %R879 = trunc i64 %R871 to i8
  ; r880 := (bv_and r879 0x3f :: [8])
  %R880 = and i8 %R879, 63
  ; r881 := (bv_eq r880 0x0 :: [8])
  %R881 = icmp eq i8 %R880, 0
  br i1 %R881, label %subblock_401842_1, label %subblock_401842_2
subblock_401842_1:
  br label %block_401850
subblock_401842_2:
  ; r882 := (trunc r871 8)
  %R882 = trunc i64 %R871 to i8
  ; r883 := (bv_and r882 0x3f :: [8])
  %R883 = and i8 %R882, 63
  ; r884 := (uext r883 64)
  %R884 = zext i8 %R883 to i64
  ; r885 := (bv_shl 0xffffffffffffffff :: [64] r884)
  %R885 = shl i64 18446744073709551615, %R884
  br label %block_401850
block_401850:
  %R891 = phi i128 [ %R876, %subblock_401842_2 ], [ %R876, %subblock_401842_1 ]
  %R890 = phi i64 [ %R885, %subblock_401842_2 ], [ 18446744073709551615, %subblock_401842_1 ]
  %R889 = phi i64 [ %R875, %subblock_401842_2 ], [ %R875, %subblock_401842_1 ]
  %R888 = phi i64 [ %R874, %subblock_401842_2 ], [ %R874, %subblock_401842_1 ]
  %R887 = phi i64 [ %R873, %subblock_401842_2 ], [ %R873, %subblock_401842_1 ]
  %R886 = phi i64 [ %R872, %subblock_401842_2 ], [ %R872, %subblock_401842_1 ]
  ; # 401850: mov    QWORD PTR [rsp+0x8],r14
  ; r892 := (bv_add r886 0x8 :: [64])
  %R892 = add i64 %R886, 8
  ; *(r892) = r890
  %r1045 = inttoptr i64 %R892 to i64*
  store i64 %R890, i64* %r1045
  ; # 401855: jmp    4012c8
  br label %block_4012c8
failure:
  br label %failure
}