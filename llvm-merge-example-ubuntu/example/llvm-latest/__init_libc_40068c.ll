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
declare { i64, i64, <2 x double> } @F40068b(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F40255b(i64, <2 x double>)
define { i64, i64, <2 x double> } @F40068c(i64 %a0, i64 %a1, <2 x double> %a2) {
entry:
  %r0 = bitcast <2 x double> %a2 to i128
  br label %block_40068c
block_40068c:
  ; r0 := (alloca 0x160 :: [64])
  %r1 = alloca i8, i64 352
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x160 :: [64])
  %R1 = add i64 %R0, 352
  ; # 40068c: sub    rsp,0x158
  ; r2 := (bv_add r1 0xfffffffffffffea8 :: [64])
  %R2 = add i64 %R1, 18446744073709551272
  ; # 400693: mov    rdx,rdi
  ; # 400696: xor    eax,eax
  ; # 400698: lea    rdi,[rsp+0x20]
  ; r3 := (bv_add r1 0xfffffffffffffec8 :: [64])
  %R3 = add i64 %R1, 18446744073709551304
  ; # 40069d: mov    ecx,0x4c
  ; # 4006a2: mov    QWORD PTR [rip+0x20405f],rdx
  ; *(0x604708 :: [64]) = arg0
  %r6 = inttoptr i64 6309640 to i64*
  store i64 %a0, i64* %r6
  ; # 4006a9: rep stos DWORD PTR [rdi],eax
  ; memset (0x4c :: [64],0x0 :: [32],r3,0x0 :: [1])
  %r7 = inttoptr i64 %R3 to i32*
  call void @reopt.MemSet.i32(i32* %r7, i32 0, i64 76, i1 0)
  ; # 4006ab: xor    eax,eax
  br label %block_4006ad
block_4006ad:
  %R8 = phi i128 [ %R8, %subblock_4006ad_1 ], [ %r0, %block_40068c ]
  %R7 = phi i64 [ %R7, %subblock_4006ad_1 ], [ %a1, %block_40068c ]
  %R6 = phi i64 [ %R6, %subblock_4006ad_1 ], [ %R2, %block_40068c ]
  %R5 = phi i64 [ %R5, %subblock_4006ad_1 ], [ %a0, %block_40068c ]
  %R4 = phi i64 [ %R13, %subblock_4006ad_1 ], [ 0, %block_40068c ]
  ; # 4006ad: cmp    QWORD PTR [rdx+rax*8],0x0
  ; r9 := (bv_mul 0x8 :: [64] r4)
  %R9 = mul i64 8, %R4
  ; r10 := (bv_add r5 r9)
  %R10 = add i64 %R5, %R9
  ; r11 := *r10
  %r15 = inttoptr i64 %R10 to i64*
  %R11 = load i64* %r15
  ; r12 := (bv_eq r11 0x0 :: [64])
  %R12 = icmp eq i64 %R11, 0
  ; # 4006b2: lea    rax,[rax+0x1]
  ; r13 := (bv_add r4 0x1 :: [64])
  %R13 = add i64 %R4, 1
  ; # 4006b6: jne    4006ad
  ; r14 := (bv_complement r12)
  %R14 = xor i1 %R12, -1
  br i1 %R14, label %subblock_4006ad_1, label %subblock_4006ad_2
subblock_4006ad_1:
  br label %block_4006ad
subblock_4006ad_2:
  br label %block_4006b8
block_4006b8:
  %R19 = phi i128 [ %R8, %subblock_4006ad_2 ]
  %R18 = phi i64 [ %R7, %subblock_4006ad_2 ]
  %R17 = phi i64 [ %R6, %subblock_4006ad_2 ]
  %R16 = phi i64 [ %R5, %subblock_4006ad_2 ]
  %R15 = phi i64 [ %R13, %subblock_4006ad_2 ]
  ; # 4006b8: lea    rax,[rdx+rax*8]
  ; r20 := (bv_mul 0x8 :: [64] r15)
  %R20 = mul i64 8, %R15
  ; r21 := (bv_add r16 r20)
  %R21 = add i64 %R16, %R20
  ; # 4006bc: mov    QWORD PTR [rip+0x2042cd],rax
  ; *(0x604990 :: [64]) = r21
  %r27 = inttoptr i64 6310288 to i64*
  store i64 %R21, i64* %r27
  br label %block_4006c3
block_4006c3:
  %R25 = phi i128 [ %R47, %block_4006da ], [ %R19, %block_4006b8 ]
  %R24 = phi i64 [ %R46, %block_4006da ], [ %R18, %block_4006b8 ]
  %R23 = phi i64 [ %R45, %block_4006da ], [ %R17, %block_4006b8 ]
  %R22 = phi i64 [ %R48, %block_4006da ], [ %R21, %block_4006b8 ]
  ; # 4006c3: mov    rdx,QWORD PTR [rax]
  ; r26 := *r22
  %r32 = inttoptr i64 %R22 to i64*
  %R26 = load i64* %r32
  ; # 4006c6: test   rdx,rdx
  ; r27 := (bv_eq r26 0x0 :: [64])
  %R27 = icmp eq i64 %R26, 0
  ; # 4006c9: je     4006e0
  br i1 %R27, label %subblock_4006c3_1, label %subblock_4006c3_2
subblock_4006c3_1:
  br label %block_4006e0
subblock_4006c3_2:
  br label %block_4006cb
block_4006cb:
  %R32 = phi i128 [ %R25, %subblock_4006c3_2 ]
  %R31 = phi i64 [ %R24, %subblock_4006c3_2 ]
  %R30 = phi i64 [ %R23, %subblock_4006c3_2 ]
  %R29 = phi i64 [ %R26, %subblock_4006c3_2 ]
  %R28 = phi i64 [ %R22, %subblock_4006c3_2 ]
  ; # 4006cb: cmp    rdx,0x25
  ; # 4006cf: ja     4006da
  ; r33 := (bv_ult 0x25 :: [64] r29)
  %R33 = icmp ult i64 37, %R29
  br i1 %R33, label %subblock_4006cb_1, label %subblock_4006cb_2
subblock_4006cb_1:
  br label %block_4006da
subblock_4006cb_2:
  br label %block_4006d1
block_4006d1:
  %R38 = phi i128 [ %R32, %subblock_4006cb_2 ]
  %R37 = phi i64 [ %R31, %subblock_4006cb_2 ]
  %R36 = phi i64 [ %R30, %subblock_4006cb_2 ]
  %R35 = phi i64 [ %R29, %subblock_4006cb_2 ]
  %R34 = phi i64 [ %R28, %subblock_4006cb_2 ]
  ; # 4006d1: mov    rcx,QWORD PTR [rax+0x8]
  ; r39 := (bv_add r34 0x8 :: [64])
  %R39 = add i64 %R34, 8
  ; r40 := *r39
  %r47 = inttoptr i64 %R39 to i64*
  %R40 = load i64* %r47
  ; # 4006d5: mov    QWORD PTR [rsp+rdx*8+0x20],rcx
  ; r41 := (bv_mul 0x8 :: [64] r35)
  %R41 = mul i64 8, %R35
  ; r42 := (bv_add r36 r41)
  %R42 = add i64 %R36, %R41
  ; r43 := (bv_add r42 0x20 :: [64])
  %R43 = add i64 %R42, 32
  ; *(r43) = r40
  %r52 = inttoptr i64 %R43 to i64*
  store i64 %R40, i64* %r52
  br label %block_4006da
block_4006da:
  %R47 = phi i128 [ %R38, %block_4006d1 ], [ %R32, %subblock_4006cb_1 ]
  %R46 = phi i64 [ %R37, %block_4006d1 ], [ %R31, %subblock_4006cb_1 ]
  %R45 = phi i64 [ %R36, %block_4006d1 ], [ %R30, %subblock_4006cb_1 ]
  %R44 = phi i64 [ %R34, %block_4006d1 ], [ %R28, %subblock_4006cb_1 ]
  ; # 4006da: add    rax,0x10
  ; r48 := (bv_add r44 0x10 :: [64])
  %R48 = add i64 %R44, 16
  ; # 4006de: jmp    4006c3
  br label %block_4006c3
block_4006e0:
  %R51 = phi i128 [ %R25, %subblock_4006c3_1 ]
  %R50 = phi i64 [ %R24, %subblock_4006c3_1 ]
  %R49 = phi i64 [ %R23, %subblock_4006c3_1 ]
  ; # 4006e0: mov    rax,QWORD PTR [rsp+0xa0]
  ; r52 := (bv_add r49 0xa0 :: [64])
  %R52 = add i64 %R49, 160
  ; r53 := *r52
  %r62 = inttoptr i64 %R52 to i64*
  %R53 = load i64* %r62
  ; # 4006e8: test   rsi,rsi
  ; r54 := (bv_eq r50 0x0 :: [64])
  %R54 = icmp eq i64 %R50, 0
  ; # 4006eb: mov    QWORD PTR [rip+0x20426e],rax
  ; *(0x604960 :: [64]) = r53
  %r65 = inttoptr i64 6310240 to i64*
  store i64 %R53, i64* %r65
  ; # 4006f2: mov    rax,QWORD PTR [rsp+0x120]
  ; r55 := (bv_add r49 0x120 :: [64])
  %R55 = add i64 %R49, 288
  ; r56 := *r55
  %r67 = inttoptr i64 %R55 to i64*
  %R56 = load i64* %r67
  ; # 4006fa: mov    QWORD PTR [rip+0x2042ef],rax
  ; *(0x6049f0 :: [64]) = r56
  %r69 = inttoptr i64 6310384 to i64*
  store i64 %R56, i64* %r69
  ; # 400701: mov    rax,QWORD PTR [rsp+0x50]
  ; r57 := (bv_add r49 0x50 :: [64])
  %R57 = add i64 %R49, 80
  ; r58 := *r57
  %r71 = inttoptr i64 %R57 to i64*
  %R58 = load i64* %r71
  ; # 400706: mov    QWORD PTR [rip+0x2042ab],rax
  ; *(0x6049b8 :: [64]) = r58
  %r73 = inttoptr i64 6310328 to i64*
  store i64 %R58, i64* %r73
  ; # 40070d: jne    400737
  ; r59 := (bv_complement r54)
  %R59 = xor i1 %R54, -1
  br i1 %R59, label %subblock_4006e0_1, label %subblock_4006e0_2
subblock_4006e0_1:
  br label %block_400737
subblock_4006e0_2:
  br label %block_40070f
block_40070f:
  %R61 = phi i128 [ %R104, %subblock_400745_1 ], [ %R51, %subblock_4006e0_2 ]
  %R60 = phi i64 [ %R102, %subblock_400745_1 ], [ %R49, %subblock_4006e0_2 ]
  ; # 40070f: lea    rdi,[rsp+0x20]
  ; r62 := (bv_add r60 0x20 :: [64])
  %R62 = add i64 %R60, 32
  ; # 400714: call   40255b
  ; r63 := (bv_add r60 0xfffffffffffffff8 :: [64])
  %R63 = add i64 %R60, 18446744073709551608
  ; r67 := (bv_add r63 0x8 :: [64])
  %R67 = add i64 %R63, 8
  %r80 = bitcast i128 %R61 to <2 x double>
  %r81 = call { i64, i64, <2 x double> } @F40255b(i64 %R62, <2 x double> %r80)
  %R64 = extractvalue { i64, i64, <2 x double> } %r81, 0
  %R65 = extractvalue { i64, i64, <2 x double> } %r81, 1
  %r84 = extractvalue { i64, i64, <2 x double> } %r81, 2
  %R66 = bitcast <2 x double> %r84 to i128
  br label %block_400719
block_400719:
  %R71 = phi i128 [ %R66, %block_40070f ]
  %R70 = phi i64 [ %R65, %block_40070f ]
  %R69 = phi i64 [ %R67, %block_40070f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R68 = phi i64 [ undef, %block_40070f ]
  ; # 400719: mov    rdi,QWORD PTR [rsp+0xe8]
  ; r72 := (bv_add r69 0xe8 :: [64])
  %R72 = add i64 %R69, 232
  ; r73 := *r72
  %r91 = inttoptr i64 %R72 to i64*
  %R73 = load i64* %r91
  ; # 400721: call   40068b
  ; r74 := (bv_add r69 0xfffffffffffffff8 :: [64])
  %R74 = add i64 %R69, 18446744073709551608
  ; r78 := (bv_add r74 0x8 :: [64])
  %R78 = add i64 %R74, 8
  %r95 = bitcast i128 %R71 to <2 x double>
  %r96 = call { i64, i64, <2 x double> } @F40068b(i64 %R73, i64 %R70, i64 %R68, <2 x double> %r95)
  %R75 = extractvalue { i64, i64, <2 x double> } %r96, 0
  %R76 = extractvalue { i64, i64, <2 x double> } %r96, 1
  %r99 = extractvalue { i64, i64, <2 x double> } %r96, 2
  %R77 = bitcast <2 x double> %r99 to i128
  br label %block_400726
block_400726:
  %R82 = phi i128 [ %R77, %block_400719 ]
  %R81 = phi i1 [ 0, %block_400719 ]
  %R80 = phi i64 [ %R78, %block_400719 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R79 = phi i64 [ undef, %block_400719 ]
  ; # 400726: mov    rax,QWORD PTR [rsp+0x80]
  ; r83 := (bv_add r80 0x80 :: [64])
  %R83 = add i64 %R80, 128
  ; r84 := *r83
  %r106 = inttoptr i64 %R83 to i64*
  %R84 = load i64* %r106
  ; # 40072e: cmp    QWORD PTR [rsp+0x78],rax
  ; r85 := (bv_add r80 0x78 :: [64])
  %R85 = add i64 %R80, 120
  ; r86 := *r85
  %r109 = inttoptr i64 %R85 to i64*
  %R86 = load i64* %r109
  ; r87 := (bv_eq r86 r84)
  %R87 = icmp eq i64 %R86, %R84
  ; # 400733: jne    400772
  ; r88 := (bv_complement r87)
  %R88 = xor i1 %R87, -1
  br i1 %R88, label %subblock_400726_1, label %subblock_400726_2
subblock_400726_1:
  br label %block_400772
subblock_400726_2:
  br label %block_400735
block_400735:
  %R92 = phi i128 [ %R82, %subblock_400726_2 ]
  %R91 = phi i1 [ %R81, %subblock_400726_2 ]
  %R90 = phi i64 [ %R80, %subblock_400726_2 ]
  %R89 = phi i64 [ %R79, %subblock_400726_2 ]
  ; # 400735: jmp    400755
  br label %block_400755
block_400737:
  %R96 = phi i128 [ %R51, %subblock_4006e0_1 ]
  %R95 = phi i64 [ %R50, %subblock_4006e0_1 ]
  %R94 = phi i64 [ %R49, %subblock_4006e0_1 ]
  %R93 = phi i64 [ %R58, %subblock_4006e0_1 ]
  ; # 400737: mov    QWORD PTR [rip+0x20396a],rsi
  ; *(0x6040a8 :: [64]) = r95
  %r121 = inttoptr i64 6308008 to i64*
  store i64 %R95, i64* %r121
  br label %block_40073e
block_40073e:
  %R100 = phi i128 [ %R122, %block_400753 ], [ %R96, %block_400737 ]
  %R99 = phi i64 [ %R121, %block_400753 ], [ %R95, %block_400737 ]
  %R98 = phi i64 [ %R120, %block_400753 ], [ %R94, %block_400737 ]
  %R97 = phi i64 [ %R119, %block_400753 ], [ %R93, %block_400737 ]
  ; # 40073e: mov    QWORD PTR [rip+0x20396b],rsi
  ; *(0x6040b0 :: [64]) = r99
  %r126 = inttoptr i64 6308016 to i64*
  store i64 %R99, i64* %r126
  br label %block_400745
block_400745:
  %R104 = phi i128 [ %R115, %subblock_40074f_1 ], [ %R100, %block_40073e ]
  %R103 = phi i64 [ %R114, %subblock_40074f_1 ], [ %R99, %block_40073e ]
  %R102 = phi i64 [ %R113, %subblock_40074f_1 ], [ %R98, %block_40073e ]
  %R101 = phi i64 [ %R112, %subblock_40074f_1 ], [ %R97, %block_40073e ]
  ; # 400745: inc    rsi
  ; r105 := (bv_add r103 0x1 :: [64])
  %R105 = add i64 %R103, 1
  ; # 400748: mov    al,BYTE PTR [rsi-0x1]
  ; r106 := *r103
  %r132 = inttoptr i64 %R103 to i8*
  %R106 = load i8* %r132
  ; r107 := (bv_and r101 0xffffffffffffff00 :: [64])
  %R107 = and i64 %R101, 18446744073709551360
  ; r108 := (uext r106 64)
  %R108 = zext i8 %R106 to i64
  ; r109 := (bv_or r107 r108)
  %R109 = or i64 %R107, %R108
  ; # 40074b: test   al,al
  ; r110 := (trunc r109 8)
  %R110 = trunc i64 %R109 to i8
  ; r111 := (bv_eq r110 0x0 :: [8])
  %R111 = icmp eq i8 %R110, 0
  ; # 40074d: je     40070f
  br i1 %R111, label %subblock_400745_1, label %subblock_400745_2
subblock_400745_1:
  br label %block_40070f
subblock_400745_2:
  br label %block_40074f
block_40074f:
  %R115 = phi i128 [ %R104, %subblock_400745_2 ]
  %R114 = phi i64 [ %R105, %subblock_400745_2 ]
  %R113 = phi i64 [ %R102, %subblock_400745_2 ]
  %R112 = phi i64 [ %R109, %subblock_400745_2 ]
  ; # 40074f: cmp    al,0x2f
  ; r116 := (trunc r112 8)
  %R116 = trunc i64 %R112 to i8
  ; r117 := (bv_eq r116 0x2f :: [8])
  %R117 = icmp eq i8 %R116, 47
  ; # 400751: jne    400745
  ; r118 := (bv_complement r117)
  %R118 = xor i1 %R117, -1
  br i1 %R118, label %subblock_40074f_1, label %subblock_40074f_2
subblock_40074f_1:
  br label %block_400745
subblock_40074f_2:
  br label %block_400753
block_400753:
  %R122 = phi i128 [ %R115, %subblock_40074f_2 ]
  %R121 = phi i64 [ %R114, %subblock_40074f_2 ]
  %R120 = phi i64 [ %R113, %subblock_40074f_2 ]
  %R119 = phi i64 [ %R112, %subblock_40074f_2 ]
  ; # 400753: jmp    40073e
  br label %block_40073e
block_400755:
  %R126 = phi i128 [ %R92, %block_400735 ]
  %R125 = phi i1 [ %R91, %block_400735 ]
  %R124 = phi i64 [ %R90, %block_400735 ]
  %R123 = phi i64 [ %R89, %block_400735 ]
  ; # 400755: mov    rax,QWORD PTR [rsp+0x90]
  ; r127 := (bv_add r124 0x90 :: [64])
  %R127 = add i64 %R124, 144
  ; r128 := *r127
  %r155 = inttoptr i64 %R127 to i64*
  %R128 = load i64* %r155
  ; # 40075d: cmp    QWORD PTR [rsp+0x88],rax
  ; r129 := (bv_add r124 0x88 :: [64])
  %R129 = add i64 %R124, 136
  ; r130 := *r129
  %r158 = inttoptr i64 %R129 to i64*
  %R130 = load i64* %r158
  ; r131 := (bv_eq r130 r128)
  %R131 = icmp eq i64 %R130, %R128
  ; # 400765: jne    400772
  ; r132 := (bv_complement r131)
  %R132 = xor i1 %R131, -1
  br i1 %R132, label %subblock_400755_1, label %subblock_400755_2
subblock_400755_1:
  br label %block_400772
subblock_400755_2:
  br label %block_400767
block_400767:
  %R137 = phi i128 [ %R126, %subblock_400755_2 ]
  %R136 = phi i1 [ %R125, %subblock_400755_2 ]
  %R135 = phi i64 [ %R124, %subblock_400755_2 ]
  %R134 = phi i64 [ %R123, %subblock_400755_2 ]
  %R133 = phi i64 [ %R128, %subblock_400755_2 ]
  ; # 400767: cmp    QWORD PTR [rsp+0xd8],0x0
  ; r138 := (bv_add r135 0xd8 :: [64])
  %R138 = add i64 %R135, 216
  ; r139 := *r138
  %r168 = inttoptr i64 %R138 to i64*
  %R139 = load i64* %r168
  ; r140 := (bv_eq r139 0x0 :: [64])
  %R140 = icmp eq i64 %R139, 0
  ; # 400770: je     4007da
  br i1 %R140, label %subblock_400767_1, label %subblock_400767_2
subblock_400767_1:
  br label %block_4007da
subblock_400767_2:
  br label %block_400772
block_400772:
  %R142 = phi i1 [ %R136, %subblock_400767_2 ], [ %R125, %subblock_400755_1 ], [ %R81, %subblock_400726_1 ]
  %R141 = phi i64 [ %R135, %subblock_400767_2 ], [ %R124, %subblock_400755_1 ], [ %R80, %subblock_400726_1 ]
  ; # 400772: lea    rdi,[rsp+0x8]
  ; r143 := (bv_add r141 0x8 :: [64])
  %R143 = add i64 %R141, 8
  ; # 400777: mov    edx,0x6
  ; # 40077c: xor    eax,eax
  ; # 40077e: mov    rcx,rdx
  ; # 400781: lea    r8,[rsp+0x8]
  ; # 400786: mov    esi,0x3
  ; # 40078b: rep stos DWORD PTR [rdi],eax
  ; memset (0x6 :: [64],0x0 :: [32],r143,r142)
  %r174 = inttoptr i64 %R143 to i32*
  call void @reopt.MemSet.i32(i32* %r174, i32 0, i64 6, i1 %R142)
  ; # 40078d: mov    DWORD PTR [rsp+0x10],0x1
  ; r144 := (bv_add r141 0x10 :: [64])
  %R144 = add i64 %R141, 16
  ; *(r144) = 0x1 :: [32]
  %r176 = inttoptr i64 %R144 to i32*
  store i32 1, i32* %r176
  ; # 400795: mov    DWORD PTR [rsp+0x18],0x2
  ; r145 := (bv_add r141 0x18 :: [64])
  %R145 = add i64 %R141, 24
  ; *(r145) = 0x2 :: [32]
  %r178 = inttoptr i64 %R145 to i32*
  store i32 2, i32* %r178
  ; # 40079d: mov    eax,0x7
  ; # 4007a2: mov    rdi,r8
  ; # 4007a5: mov    rdx,rcx
  ; # 4007a8: syscall
  ; sys_poll
  %r179 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R143, i64 3, i64 0, i64 undef, i64 undef, i64 undef, i64 7)
  %R146 = extractvalue { i64, i1 } %r179, 0
  br label %block_4007aa
block_4007aa:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R150 = phi i128 [ undef, %block_400772 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R149 = phi i64 [ undef, %block_400772 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R148 = phi i64 [ undef, %block_400772 ]
  %R147 = phi i64 [ %R146, %block_400772 ]
  ; # 4007aa: mov    esi,0x2
  ; # 4007af: mov    edi,0x402f78
  br label %block_4007b4
block_4007b4:
  %R156 = phi i128 [ %R185, %subblock_4007c7_1 ], [ %R150, %block_4007aa ]
  %R155 = phi i64 [ %R184, %subblock_4007c7_1 ], [ %R149, %block_4007aa ]
  %R154 = phi i64 [ %R183, %subblock_4007c7_1 ], [ 4206456, %block_4007aa ]
  %R153 = phi i64 [ %R182, %subblock_4007c7_1 ], [ 2, %block_4007aa ]
  %R152 = phi i64 [ %R186, %subblock_4007c7_1 ], [ %R148, %block_4007aa ]
  %R151 = phi i64 [ %R180, %subblock_4007c7_1 ], [ %R147, %block_4007aa ]
  ; # 4007b4: test   BYTE PTR [r8+rdx*8+0x6],0x20
  ; r157 := (bv_mul 0x8 :: [64] r152)
  %R157 = mul i64 8, %R152
  ; r158 := (bv_add r155 r157)
  %R158 = add i64 %R155, %R157
  ; r159 := (bv_add r158 0x6 :: [64])
  %R159 = add i64 %R158, 6
  ; r160 := *r159
  %r194 = inttoptr i64 %R159 to i8*
  %R160 = load i8* %r194
  ; r161 := (bv_and r160 0x20 :: [8])
  %R161 = and i8 %R160, 32
  ; r162 := (bv_eq r161 0x0 :: [8])
  %R162 = icmp eq i8 %R161, 0
  ; # 4007ba: je     4007c7
  br i1 %R162, label %subblock_4007b4_1, label %subblock_4007b4_2
subblock_4007b4_1:
  br label %block_4007c7
subblock_4007b4_2:
  br label %block_4007bc
block_4007bc:
  %R165 = phi i64 [ %R154, %subblock_4007b4_2 ]
  %R164 = phi i64 [ %R153, %subblock_4007b4_2 ]
  %R163 = phi i64 [ %R152, %subblock_4007b4_2 ]
  ; # 4007bc: mov    rax,rsi
  ; # 4007bf: syscall
  ; sys_open
  %r201 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R165, i64 %R164, i64 %R163, i64 undef, i64 undef, i64 undef, i64 2)
  %R166 = extractvalue { i64, i1 } %r201, 0
  br label %block_4007c1
block_4007c1:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R172 = phi i128 [ undef, %block_4007bc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register r8
  %R171 = phi i64 [ undef, %block_4007bc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R170 = phi i64 [ undef, %block_4007bc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R169 = phi i64 [ undef, %block_4007bc ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R168 = phi i64 [ undef, %block_4007bc ]
  %R167 = phi i64 [ %R166, %block_4007bc ]
  ; # 4007c1: test   rax,rax
  ; # 4007c4: jns    4007c7
  ; r173 := (bv_sle 0x0 :: [64] r167)
  %R173 = icmp sle i64 0, %R167
  br i1 %R173, label %subblock_4007c1_1, label %subblock_4007c1_2
subblock_4007c1_1:
  br label %block_4007c7
subblock_4007c1_2:
  br label %block_4007c6
block_4007c6:
  %R179 = phi i128 [ %R172, %subblock_4007c1_2 ]
  %R178 = phi i64 [ %R171, %subblock_4007c1_2 ]
  %R177 = phi i64 [ %R170, %subblock_4007c1_2 ]
  %R176 = phi i64 [ %R169, %subblock_4007c1_2 ]
  %R175 = phi i64 [ %R168, %subblock_4007c1_2 ]
  %R174 = phi i64 [ %R167, %subblock_4007c1_2 ]
  ; # 4007c6: hlt
  ; # UNIMPLEMENTED: PLACEHOLDER: Exception GeneralProtectionException 0 ()
  br label %block_4007c7
block_4007c7:
  %R185 = phi i128 [ %R179, %block_4007c6 ], [ %R172, %subblock_4007c1_1 ], [ %R156, %subblock_4007b4_1 ]
  %R184 = phi i64 [ %R178, %block_4007c6 ], [ %R171, %subblock_4007c1_1 ], [ %R155, %subblock_4007b4_1 ]
  %R183 = phi i64 [ %R177, %block_4007c6 ], [ %R170, %subblock_4007c1_1 ], [ %R154, %subblock_4007b4_1 ]
  %R182 = phi i64 [ %R176, %block_4007c6 ], [ %R169, %subblock_4007c1_1 ], [ %R153, %subblock_4007b4_1 ]
  %R181 = phi i64 [ %R175, %block_4007c6 ], [ %R168, %subblock_4007c1_1 ], [ %R152, %subblock_4007b4_1 ]
  %R180 = phi i64 [ %R174, %block_4007c6 ], [ %R167, %subblock_4007c1_1 ], [ %R151, %subblock_4007b4_1 ]
  ; # 4007c7: inc    rdx
  ; r186 := (bv_add r181 0x1 :: [64])
  %R186 = add i64 %R181, 1
  ; # 4007ca: cmp    rdx,0x3
  ; r187 := (bv_eq r181 0x2 :: [64])
  %R187 = icmp eq i64 %R181, 2
  ; # 4007ce: jne    4007b4
  ; r188 := (bv_complement r187)
  %R188 = xor i1 %R187, -1
  br i1 %R188, label %subblock_4007c7_1, label %subblock_4007c7_2
subblock_4007c7_1:
  br label %block_4007b4
subblock_4007c7_2:
  br label %block_4007d0
block_4007d0:
  %R191 = phi i128 [ %R185, %subblock_4007c7_2 ]
  %R190 = phi i64 [ %R186, %subblock_4007c7_2 ]
  %R189 = phi i64 [ %R180, %subblock_4007c7_2 ]
  ; # 4007d0: mov    DWORD PTR [rip+0x2041ae],0x1
  ; *(0x604988 :: [64]) = 0x1 :: [32]
  %r228 = inttoptr i64 6310280 to i32*
  store i32 1, i32* %r228
  br label %block_4007da
block_4007da:
  %R194 = phi i128 [ %R137, %subblock_400767_1 ], [ %R191, %block_4007d0 ]
  %R193 = phi i64 [ %R134, %subblock_400767_1 ], [ %R190, %block_4007d0 ]
  %R192 = phi i64 [ %R133, %subblock_400767_1 ], [ %R189, %block_4007d0 ]
  ; # 4007da: add    rsp,0x158
  ; # 4007e1: ret
  %r232 = bitcast i128 %R194 to <2 x double>
  %r233 = insertvalue { i64, i64, <2 x double> } undef, i64 %R192, 0
  %r234 = insertvalue { i64, i64, <2 x double> } %r233, i64 %R193, 1
  %r235 = insertvalue { i64, i64, <2 x double> } %r234, <2 x double> %r232, 2
  ret { i64, i64, <2 x double> } %r235
failure:
  br label %failure
}