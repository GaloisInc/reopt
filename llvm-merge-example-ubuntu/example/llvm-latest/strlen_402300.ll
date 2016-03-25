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
define { i64, i64, <2 x double> } @F402300(i64 %a0, <2 x double> %a1) {
entry:
  %r0 = bitcast <2 x double> %a1 to i128
  br label %block_402300
block_402300:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 402300: test   dil,0x7
  ; r2 := (trunc arg0 8)
  %R2 = trunc i64 %a0 to i8
  ; r3 := (bv_and r2 0x7 :: [8])
  %R3 = and i8 %R2, 7
  ; r4 := (bv_eq r3 0x0 :: [8])
  %R4 = icmp eq i8 %R3, 0
  ; # 402304: je     40236d
  br i1 %R4, label %subblock_402300_1, label %subblock_402300_2
subblock_402300_1:
  br label %block_40236d
subblock_402300_2:
  br label %block_402306
block_402306:
  %R6 = phi i128 [ %r0, %subblock_402300_2 ]
  %R5 = phi i64 [ %a0, %subblock_402300_2 ]
  ; # 402306: cmp    BYTE PTR [rdi],0x0
  ; r7 := *r5
  %r9 = inttoptr i64 %R5 to i8*
  %R7 = load i8* %r9
  ; r8 := (bv_eq r7 0x0 :: [8])
  %R8 = icmp eq i8 %R7, 0
  ; # 402309: je     402372
  br i1 %R8, label %subblock_402306_1, label %subblock_402306_2
subblock_402306_1:
  br label %block_402372
subblock_402306_2:
  br label %block_40230b
block_40230b:
  %R10 = phi i128 [ %R6, %subblock_402306_2 ]
  %R9 = phi i64 [ %R5, %subblock_402306_2 ]
  ; # 40230b: mov    rax,rdi
  ; # 40230e: jmp    402315
  br label %block_402315
block_402310:
  %R13 = phi i128 [ %R18, %subblock_402315_1 ]
  %R12 = phi i64 [ %R17, %subblock_402315_1 ]
  %R11 = phi i64 [ %R19, %subblock_402315_1 ]
  ; # 402310: cmp    BYTE PTR [rax],0x0
  ; r14 := *r11
  %r17 = inttoptr i64 %R11 to i8*
  %R14 = load i8* %r17
  ; r15 := (bv_eq r14 0x0 :: [8])
  %R15 = icmp eq i8 %R14, 0
  ; # 402313: je     402369
  br i1 %R15, label %subblock_402310_1, label %subblock_402310_2
subblock_402310_1:
  br label %block_402369
subblock_402310_2:
  br label %block_402315
block_402315:
  %R18 = phi i128 [ %R13, %subblock_402310_2 ], [ %R10, %block_40230b ]
  %R17 = phi i64 [ %R12, %subblock_402310_2 ], [ %R9, %block_40230b ]
  %R16 = phi i64 [ %R11, %subblock_402310_2 ], [ %R9, %block_40230b ]
  ; # 402315: add    rax,0x1
  ; r19 := (bv_add r16 0x1 :: [64])
  %R19 = add i64 %R16, 1
  ; r20 := (trunc r19 8)
  %R20 = trunc i64 %R19 to i8
  ; # 402319: test   al,0x7
  ; r21 := (bv_and r20 0x7 :: [8])
  %R21 = and i8 %R20, 7
  ; r22 := (bv_eq r21 0x0 :: [8])
  %R22 = icmp eq i8 %R21, 0
  ; # 40231b: jne    402310
  ; r23 := (bv_complement r22)
  %R23 = xor i1 %R22, -1
  br i1 %R23, label %subblock_402315_1, label %subblock_402315_2
subblock_402315_1:
  br label %block_402310
subblock_402315_2:
  br label %block_40231d
block_40231d:
  %R26 = phi i128 [ %R18, %subblock_402315_2 ], [ %R69, %block_40236d ]
  %R25 = phi i64 [ %R17, %subblock_402315_2 ], [ %R68, %block_40236d ]
  %R24 = phi i64 [ %R19, %subblock_402315_2 ], [ %R68, %block_40236d ]
  ; # 40231d: mov    rdx,QWORD PTR [rax]
  ; r27 := *r24
  %r31 = inttoptr i64 %R24 to i64*
  %R27 = load i64* %r31
  ; # 402320: mov    r8,0xfefefefefefefeff
  ; # 40232a: mov    rsi,0x8080808080808080
  ; # 402334: lea    rcx,[rdx+r8*1]
  ; r28 := (bv_add r27 0xfefefefefefefeff :: [64])
  %R28 = add i64 %R27, 18374403900871474943
  ; # 402338: not    rdx
  ; r29 := (bv_complement r27)
  %R29 = xor i64 %R27, -1
  ; # 40233b: and    rdx,rcx
  ; r30 := (bv_and r29 r28)
  %R30 = and i64 %R29, %R28
  ; # 40233e: test   rdx,rsi
  ; r31 := (bv_and r30 0x8080808080808080 :: [64])
  %R31 = and i64 %R30, 9259542123273814144
  ; r32 := (bv_eq r31 0x0 :: [64])
  %R32 = icmp eq i64 %R31, 0
  ; # 402341: jne    402364
  ; r33 := (bv_complement r32)
  %R33 = xor i1 %R32, -1
  br i1 %R33, label %subblock_40231d_1, label %subblock_40231d_2
subblock_40231d_1:
  br label %block_402364
subblock_40231d_2:
  br label %block_402343
block_402343:
  %R38 = phi i128 [ %R26, %subblock_40231d_2 ]
  %R37 = phi i64 [ 18374403900871474943, %subblock_40231d_2 ]
  %R36 = phi i64 [ %R25, %subblock_40231d_2 ]
  %R35 = phi i64 [ 9259542123273814144, %subblock_40231d_2 ]
  %R34 = phi i64 [ %R24, %subblock_40231d_2 ]
  ; # 402343: nop    [rax+rax*1]
  br label %block_402348
block_402348:
  %R43 = phi i128 [ %R43, %subblock_402348_1 ], [ %R38, %block_402343 ]
  %R42 = phi i64 [ %R42, %subblock_402348_1 ], [ %R37, %block_402343 ]
  %R41 = phi i64 [ %R41, %subblock_402348_1 ], [ %R36, %block_402343 ]
  %R40 = phi i64 [ %R40, %subblock_402348_1 ], [ %R35, %block_402343 ]
  %R39 = phi i64 [ %R44, %subblock_402348_1 ], [ %R34, %block_402343 ]
  ; # 402348: add    rax,0x8
  ; r44 := (bv_add r39 0x8 :: [64])
  %R44 = add i64 %R39, 8
  ; # 40234c: mov    rdx,QWORD PTR [rax]
  ; r45 := *r44
  %r50 = inttoptr i64 %R44 to i64*
  %R45 = load i64* %r50
  ; # 40234f: lea    rcx,[rdx+r8*1]
  ; r46 := (bv_add r45 r42)
  %R46 = add i64 %R45, %R42
  ; # 402353: not    rdx
  ; r47 := (bv_complement r45)
  %R47 = xor i64 %R45, -1
  ; # 402356: and    rdx,rcx
  ; r48 := (bv_and r47 r46)
  %R48 = and i64 %R47, %R46
  ; # 402359: test   rdx,rsi
  ; r49 := (bv_and r48 r40)
  %R49 = and i64 %R48, %R40
  ; r50 := (bv_eq r49 0x0 :: [64])
  %R50 = icmp eq i64 %R49, 0
  ; # 40235c: je     402348
  br i1 %R50, label %subblock_402348_1, label %subblock_402348_2
subblock_402348_1:
  br label %block_402348
subblock_402348_2:
  br label %block_40235e
block_40235e:
  %R53 = phi i128 [ %R43, %subblock_402348_2 ]
  %R52 = phi i64 [ %R41, %subblock_402348_2 ]
  %R51 = phi i64 [ %R44, %subblock_402348_2 ]
  ; # 40235e: jmp    402364
  br label %block_402364
block_402360:
  %R56 = phi i128 [ %R60, %subblock_402364_1 ]
  %R55 = phi i64 [ %R59, %subblock_402364_1 ]
  %R54 = phi i64 [ %R58, %subblock_402364_1 ]
  ; # 402360: add    rax,0x1
  ; r57 := (bv_add r54 0x1 :: [64])
  %R57 = add i64 %R54, 1
  br label %block_402364
block_402364:
  %R60 = phi i128 [ %R53, %block_40235e ], [ %R56, %block_402360 ], [ %R26, %subblock_40231d_1 ]
  %R59 = phi i64 [ %R52, %block_40235e ], [ %R55, %block_402360 ], [ %R25, %subblock_40231d_1 ]
  %R58 = phi i64 [ %R51, %block_40235e ], [ %R57, %block_402360 ], [ %R24, %subblock_40231d_1 ]
  ; # 402364: cmp    BYTE PTR [rax],0x0
  ; r61 := *r58
  %r67 = inttoptr i64 %R58 to i8*
  %R61 = load i8* %r67
  ; r62 := (bv_eq r61 0x0 :: [8])
  %R62 = icmp eq i8 %R61, 0
  ; # 402367: jne    402360
  ; r63 := (bv_complement r62)
  %R63 = xor i1 %R62, -1
  br i1 %R63, label %subblock_402364_1, label %subblock_402364_2
subblock_402364_1:
  br label %block_402360
subblock_402364_2:
  br label %block_402369
block_402369:
  %R66 = phi i128 [ %R13, %subblock_402310_1 ], [ %R60, %subblock_402364_2 ]
  %R65 = phi i64 [ %R12, %subblock_402310_1 ], [ %R59, %subblock_402364_2 ]
  %R64 = phi i64 [ %R11, %subblock_402310_1 ], [ %R58, %subblock_402364_2 ]
  ; # 402369: sub    rax,rdi
  ; r67 := (bv_sub r64 r65)
  %R67 = sub i64 %R64, %R65
  ; # 40236c: ret
  %r75 = bitcast i128 %R66 to <2 x double>
  %r76 = insertvalue { i64, i64, <2 x double> } undef, i64 %R67, 0
  %r77 = insertvalue { i64, i64, <2 x double> } %r76, i64 undef, 1
  %r78 = insertvalue { i64, i64, <2 x double> } %r77, <2 x double> %r75, 2
  ret { i64, i64, <2 x double> } %r78
block_40236d:
  %R69 = phi i128 [ %r0, %subblock_402300_1 ]
  %R68 = phi i64 [ %a0, %subblock_402300_1 ]
  ; # 40236d: mov    rax,rdi
  ; # 402370: jmp    40231d
  br label %block_40231d
block_402372:
  %R70 = phi i128 [ %R6, %subblock_402306_1 ]
  ; # 402372: xor    eax,eax
  ; # 402374: ret
  %r82 = bitcast i128 %R70 to <2 x double>
  %r83 = insertvalue { i64, i64, <2 x double> } undef, i64 0, 0
  %r84 = insertvalue { i64, i64, <2 x double> } %r83, i64 undef, 1
  %r85 = insertvalue { i64, i64, <2 x double> } %r84, <2 x double> %r82, 2
  ret { i64, i64, <2 x double> } %r85
failure:
  br label %failure
}