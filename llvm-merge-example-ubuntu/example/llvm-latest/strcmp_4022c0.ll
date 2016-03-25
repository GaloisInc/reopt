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
define { i64, i64, <2 x double> } @F4022c0(i64 %a0, i64 %a1) {
entry:
  br label %block_4022c0
block_4022c0:
  ; r0 := (alloca 0x0 :: [64])
  %r0 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 4022c0: movzx  eax,BYTE PTR [rdi]
  ; r2 := *arg0
  %r3 = inttoptr i64 %a0 to i8*
  %R2 = load i8* %r3
  ; r3 := (uext r2 64)
  %R3 = zext i8 %R2 to i64
  ; # 4022c3: movzx  edx,BYTE PTR [rsi]
  ; r4 := *arg1
  %r6 = inttoptr i64 %a1 to i8*
  %R4 = load i8* %r6
  ; r5 := (uext r4 64)
  %R5 = zext i8 %R4 to i64
  ; # 4022c6: cmp    dl,al
  ; r6 := (bv_eq r4 r2)
  %R6 = icmp eq i8 %R4, %R2
  ; # 4022c8: je     4022e2
  br i1 %R6, label %subblock_4022c0_1, label %subblock_4022c0_2
subblock_4022c0_1:
  br label %block_4022e2
subblock_4022c0_2:
  br label %block_4022ca
block_4022ca:
  %R8 = phi i64 [ %R5, %subblock_4022c0_2 ]
  %R7 = phi i64 [ %R3, %subblock_4022c0_2 ]
  ; # 4022ca: jmp    4022e8
  br label %block_4022e8
block_4022d0:
  %R10 = phi i64 [ %R21, %subblock_4022e2_1 ]
  %R9 = phi i64 [ %R20, %subblock_4022e2_1 ]
  ; # 4022d0: add    rdi,0x1
  ; r11 := (bv_add r10 0x1 :: [64])
  %R11 = add i64 %R10, 1
  ; # 4022d4: add    rsi,0x1
  ; r12 := (bv_add r9 0x1 :: [64])
  %R12 = add i64 %R9, 1
  ; # 4022d8: movzx  edx,BYTE PTR [rdi]
  ; r13 := *r11
  %r16 = inttoptr i64 %R11 to i8*
  %R13 = load i8* %r16
  ; r14 := (uext r13 64)
  %R14 = zext i8 %R13 to i64
  ; # 4022db: movzx  ecx,BYTE PTR [rsi]
  ; r15 := *r12
  %r19 = inttoptr i64 %R12 to i8*
  %R15 = load i8* %r19
  ; r16 := (uext r15 64)
  %R16 = zext i8 %R15 to i64
  ; # 4022de: cmp    dl,cl
  ; r17 := (bv_eq r13 r15)
  %R17 = icmp eq i8 %R13, %R15
  ; # 4022e0: jne    4022f0
  ; r18 := (bv_complement r17)
  %R18 = xor i1 %R17, -1
  br i1 %R18, label %subblock_4022d0_1, label %subblock_4022d0_2
subblock_4022d0_1:
  br label %block_4022f0
subblock_4022d0_2:
  br label %block_4022e2
block_4022e2:
  %R21 = phi i64 [ %R11, %subblock_4022d0_2 ], [ %a0, %subblock_4022c0_1 ]
  %R20 = phi i64 [ %R12, %subblock_4022d0_2 ], [ %a1, %subblock_4022c0_1 ]
  %R19 = phi i64 [ %R14, %subblock_4022d0_2 ], [ %R5, %subblock_4022c0_1 ]
  ; # 4022e2: test   dl,dl
  ; r22 := (trunc r19 8)
  %R22 = trunc i64 %R19 to i8
  ; r23 := (bv_eq r22 0x0 :: [8])
  %R23 = icmp eq i8 %R22, 0
  ; # 4022e4: jne    4022d0
  ; r24 := (bv_complement r23)
  %R24 = xor i1 %R23, -1
  br i1 %R24, label %subblock_4022e2_1, label %subblock_4022e2_2
subblock_4022e2_1:
  br label %block_4022d0
subblock_4022e2_2:
  br label %block_4022e6
block_4022e6:
  %R25 = phi i64 [ %R19, %subblock_4022e2_2 ]
  ; # 4022e6: xor    eax,eax
  br label %block_4022e8
block_4022e8:
  %R27 = phi i64 [ %R8, %block_4022ca ], [ %R25, %block_4022e6 ]
  %R26 = phi i64 [ %R7, %block_4022ca ], [ 0, %block_4022e6 ]
  ; # 4022e8: sub    eax,edx
  ; r28 := (trunc r26 32)
  %R28 = trunc i64 %R26 to i32
  ; r29 := (trunc r27 32)
  %R29 = trunc i64 %R27 to i32
  ; r30 := (bv_sub r28 r29)
  %R30 = sub i32 %R28, %R29
  ; r31 := (uext r30 64)
  %R31 = zext i32 %R30 to i64
  ; # 4022ea: ret
  %r37 = insertvalue { i64, i64, <2 x double> } undef, i64 %R31, 0
  %r38 = insertvalue { i64, i64, <2 x double> } %r37, i64 undef, 1
  %r39 = insertvalue { i64, i64, <2 x double> } %r38, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r39
block_4022f0:
  %R33 = phi i64 [ %R14, %subblock_4022d0_1 ]
  %R32 = phi i64 [ %R16, %subblock_4022d0_1 ]
  ; # 4022f0: movzx  eax,dl
  ; r34 := (trunc r33 8)
  %R34 = trunc i64 %R33 to i8
  ; r35 := (uext r34 32)
  %R35 = zext i8 %R34 to i32
  ; # 4022f3: movzx  edx,cl
  ; r36 := (trunc r32 8)
  %R36 = trunc i64 %R32 to i8
  ; r37 := (uext r36 32)
  %R37 = zext i8 %R36 to i32
  ; # 4022f6: sub    eax,edx
  ; r38 := (bv_sub r35 r37)
  %R38 = sub i32 %R35, %R37
  ; r39 := (uext r38 64)
  %R39 = zext i32 %R38 to i64
  ; # 4022f8: ret
  %r48 = insertvalue { i64, i64, <2 x double> } undef, i64 %R39, 0
  %r49 = insertvalue { i64, i64, <2 x double> } %r48, i64 undef, 1
  %r50 = insertvalue { i64, i64, <2 x double> } %r49, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r50
failure:
  br label %failure
}