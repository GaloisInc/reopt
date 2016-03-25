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
declare { i64, i64, <2 x double> } @F401280(i64, i64, i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F401860(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_401860
block_401860:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 401860: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401861: mov    rbx,rdi
  ; # 401864: call   401280
  %r5 = bitcast i128 %r0 to <2 x double>
  %r6 = call { i64, i64, <2 x double> } @F401280(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %r5)
  %R3 = extractvalue { i64, i64, <2 x double> } %r6, 0
  %R4 = extractvalue { i64, i64, <2 x double> } %r6, 1
  %r9 = extractvalue { i64, i64, <2 x double> } %r6, 2
  %R5 = bitcast <2 x double> %r9 to i128
  br label %block_401869
block_401869:
  %R7 = phi i64 [ %a0, %block_401860 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R6 = phi i64 [ undef, %block_401860 ]
  ; # 401869: test   rax,rax
  ; r8 := (bv_eq r6 0x0 :: [64])
  %R8 = icmp eq i64 %R6, 0
  ; # 40186c: je     40189f
  br i1 %R8, label %subblock_401869_1, label %subblock_401869_2
subblock_401869_1:
  br label %block_40189f
subblock_401869_2:
  br label %block_40186e
block_40186e:
  %R10 = phi i64 [ %R7, %subblock_401869_2 ]
  %R9 = phi i64 [ %R6, %subblock_401869_2 ]
  ; # 40186e: test   BYTE PTR [rax-0x8],0x1
  ; r11 := (bv_add r9 0xfffffffffffffff8 :: [64])
  %R11 = add i64 %R9, 18446744073709551608
  ; r12 := *r11
  %r17 = inttoptr i64 %R11 to i8*
  %R12 = load i8* %r17
  ; r13 := (bv_and r12 0x1 :: [8])
  %R13 = and i8 %R12, 1
  ; r14 := (bv_eq r13 0x0 :: [8])
  %R14 = icmp eq i8 %R13, 0
  ; # 401872: je     40189f
  br i1 %R14, label %subblock_40186e_1, label %subblock_40186e_2
subblock_40186e_1:
  br label %block_40189f
subblock_40186e_2:
  br label %block_401874
block_401874:
  %R16 = phi i64 [ %R10, %subblock_40186e_2 ]
  %R15 = phi i64 [ %R9, %subblock_40186e_2 ]
  ; # 401874: lea    rdi,[rbx+0x7]
  ; r17 := (bv_add r16 0x7 :: [64])
  %R17 = add i64 %R16, 7
  ; # 401878: shr    rdi,0x3
  ; r18 := (bv_shr r17 0x3 :: [64])
  %R18 = lshr i64 %R17, 3
  ; r19 := (bv_eq r18 0x0 :: [64])
  %R19 = icmp eq i64 %R18, 0
  ; # 40187c: test   rdi,rdi
  ; # 40187f: je     40189f
  br i1 %R19, label %subblock_401874_1, label %subblock_401874_2
subblock_401874_1:
  br label %block_40189f
subblock_401874_2:
  br label %block_401881
block_401881:
  %R21 = phi i64 [ %R18, %subblock_401874_2 ]
  %R20 = phi i64 [ %R15, %subblock_401874_2 ]
  ; # 401881: mov    rdx,rax
  ; # 401884: nop    [rax]
  br label %block_401888
block_401888:
  %R23 = phi i64 [ %R31, %subblock_401895_1 ], [ %R21, %block_401881 ]
  %R22 = phi i64 [ %R30, %subblock_401895_1 ], [ %R20, %block_401881 ]
  ; # 401888: cmp    QWORD PTR [rdx],0x0
  ; r24 := *r22
  %r30 = inttoptr i64 %R22 to i64*
  %R24 = load i64* %r30
  ; r25 := (bv_eq r24 0x0 :: [64])
  %R25 = icmp eq i64 %R24, 0
  ; # 40188c: je     401895
  br i1 %R25, label %subblock_401888_1, label %subblock_401888_2
subblock_401888_1:
  br label %block_401895
subblock_401888_2:
  br label %block_40188e
block_40188e:
  %R27 = phi i64 [ %R23, %subblock_401888_2 ]
  %R26 = phi i64 [ %R22, %subblock_401888_2 ]
  ; # 40188e: mov    QWORD PTR [rdx],0x0
  ; *(r26) = 0x0 :: [64]
  %r35 = inttoptr i64 %R26 to i64*
  store i64 0, i64* %r35
  br label %block_401895
block_401895:
  %R29 = phi i64 [ %R27, %block_40188e ], [ %R23, %subblock_401888_1 ]
  %R28 = phi i64 [ %R26, %block_40188e ], [ %R22, %subblock_401888_1 ]
  ; # 401895: add    rdx,0x8
  ; r30 := (bv_add r28 0x8 :: [64])
  %R30 = add i64 %R28, 8
  ; # 401899: sub    rdi,0x1
  ; r31 := (bv_add r29 0xffffffffffffffff :: [64])
  %R31 = add i64 %R29, 18446744073709551615
  ; r32 := (bv_eq r29 0x1 :: [64])
  %R32 = icmp eq i64 %R29, 1
  ; # 40189d: jne    401888
  ; r33 := (bv_complement r32)
  %R33 = xor i1 %R32, -1
  br i1 %R33, label %subblock_401895_1, label %subblock_401895_2
subblock_401895_1:
  br label %block_401888
subblock_401895_2:
  br label %block_40189f
block_40189f:
  ; # 40189f: pop    rbx
  ; # 4018a0: ret
  %r42 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r43 = insertvalue { i64, i64, <2 x double> } %r42, i64 undef, 1
  %r44 = insertvalue { i64, i64, <2 x double> } %r43, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r44
failure:
  br label %failure
}