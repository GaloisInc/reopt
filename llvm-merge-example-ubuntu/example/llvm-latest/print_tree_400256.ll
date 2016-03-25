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
define { i64, i64, <2 x double> } @F400256(i64 %a0, i64 %a1, <2 x double> %a2) {
entry:
  %r0 = bitcast <2 x double> %a2 to i128
  br label %block_400256
block_400256:
  ; r0 := (alloca 0x20 :: [64])
  %r1 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 400256: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 400257: mov    rbp,rsp
  ; # 40025a: sub    rsp,0x10
  ; r3 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R3 = add i64 %R1, 18446744073709551592
  ; # 40025e: mov    DWORD PTR [rbp-0x4],edi
  ; r4 := (trunc arg0 32)
  %R4 = trunc i64 %a0 to i32
  ; r5 := (bv_add r1 0xfffffffffffffff4 :: [64])
  %R5 = add i64 %R1, 18446744073709551604
  ; *(r5) = r4
  %r8 = inttoptr i64 %R5 to i32*
  store i32 %R4, i32* %r8
  ; # 400261: mov    QWORD PTR [rbp-0x10],rsi
  ; *(r3) = arg1
  %r9 = inttoptr i64 %R3 to i64*
  store i64 %a1, i64* %r9
  ; # 400265: cmp    QWORD PTR [rbp-0x10],0x0
  ; r6 := *r3
  %r10 = inttoptr i64 %R3 to i64*
  %R6 = load i64* %r10
  ; r7 := (bv_eq r6 0x0 :: [64])
  %R7 = icmp eq i64 %R6, 0
  ; # 40026a: je     40029d
  br i1 %R7, label %subblock_400256_1, label %subblock_400256_2
subblock_400256_1:
  br label %block_40029d
subblock_400256_2:
  br label %block_40026c
block_40026c:
  %R9 = phi i128 [ %r0, %subblock_400256_2 ]
  %R8 = phi i64 [ %R2, %subblock_400256_2 ]
  ; # 40026c: mov    rax,QWORD PTR [rbp-0x10]
  ; r10 := (bv_add r8 0xfffffffffffffff0 :: [64])
  %R10 = add i64 %R8, 18446744073709551600
  ; r11 := *r10
  %r16 = inttoptr i64 %R10 to i64*
  %R11 = load i64* %r16
  ; # 400270: mov    rax,QWORD PTR [rax]
  ; r12 := *r11
  %r18 = inttoptr i64 %R11 to i64*
  %R12 = load i64* %r18
  ; # 400273: mov    edx,DWORD PTR [rbp-0x4]
  ; r13 := (bv_add r8 0xfffffffffffffffc :: [64])
  %R13 = add i64 %R8, 18446744073709551612
  ; r14 := *r13
  %r21 = inttoptr i64 %R13 to i32*
  %R14 = load i32* %r21
  ; # 400276: add    edx,0x1
  ; r15 := (bv_add r14 0x1 :: [32])
  %R15 = add i32 %R14, 1
  ; r16 := (uext r15 64)
  %R16 = zext i32 %R15 to i64
  ; # 400279: mov    rsi,rax
  ; # 40027c: mov    edi,edx
  ; # 40027e: call   400256
  %r25 = bitcast i128 %R9 to <2 x double>
  %r26 = call { i64, i64, <2 x double> } @F400256(i64 %R16, i64 %R12, <2 x double> %r25)
  %r27 = extractvalue { i64, i64, <2 x double> } %r26, 2
  %R17 = bitcast <2 x double> %r27 to i128
  br label %block_400283
block_400283:
  %R19 = phi i128 [ %R17, %block_40026c ]
  %R18 = phi i64 [ %R8, %block_40026c ]
  ; # 400283: mov    rax,QWORD PTR [rbp-0x10]
  ; r20 := (bv_add r18 0xfffffffffffffff0 :: [64])
  %R20 = add i64 %R18, 18446744073709551600
  ; r21 := *r20
  %r32 = inttoptr i64 %R20 to i64*
  %R21 = load i64* %r32
  ; # 400287: mov    rax,QWORD PTR [rax+0x8]
  ; r22 := (bv_add r21 0x8 :: [64])
  %R22 = add i64 %R21, 8
  ; r23 := *r22
  %r35 = inttoptr i64 %R22 to i64*
  %R23 = load i64* %r35
  ; # 40028b: mov    edx,DWORD PTR [rbp-0x4]
  ; r24 := (bv_add r18 0xfffffffffffffffc :: [64])
  %R24 = add i64 %R18, 18446744073709551612
  ; r25 := *r24
  %r38 = inttoptr i64 %R24 to i32*
  %R25 = load i32* %r38
  ; # 40028e: add    edx,0x1
  ; r26 := (bv_add r25 0x1 :: [32])
  %R26 = add i32 %R25, 1
  ; r27 := (uext r26 64)
  %R27 = zext i32 %R26 to i64
  ; # 400291: mov    rsi,rax
  ; # 400294: mov    edi,edx
  ; # 400296: call   400256
  %r42 = bitcast i128 %R19 to <2 x double>
  %r43 = call { i64, i64, <2 x double> } @F400256(i64 %R27, i64 %R23, <2 x double> %r42)
  %r44 = extractvalue { i64, i64, <2 x double> } %r43, 2
  %R28 = bitcast <2 x double> %r44 to i128
  br label %block_40029b
block_40029b:
  %R29 = phi i128 [ %R28, %block_400283 ]
  ; # 40029b: jmp    40029e
  br label %block_40029e
block_40029d:
  %R30 = phi i128 [ %r0, %subblock_400256_1 ]
  ; # 40029d: nop
  br label %block_40029e
block_40029e:
  %R31 = phi i128 [ %R29, %block_40029b ], [ %R30, %block_40029d ]
  ; # 40029e: leave
  ; # 40029f: ret
  %r49 = bitcast i128 %R31 to <2 x double>
  %r50 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r51 = insertvalue { i64, i64, <2 x double> } %r50, i64 undef, 1
  %r52 = insertvalue { i64, i64, <2 x double> } %r51, <2 x double> %r49, 2
  ret { i64, i64, <2 x double> } %r52
failure:
  br label %failure
}