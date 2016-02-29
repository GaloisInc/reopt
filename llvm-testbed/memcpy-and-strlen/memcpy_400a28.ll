declare i1 @reopt.EvenParity(i8)
declare i2 @reopt.Read_X87_RC()
declare void @reopt.Write_X87_RC(i2)
declare i2 @reopt.Read_X87_PC()
declare void @reopt.Write_X87_PC(i2)
declare i16 @reopt.Read_FS()
declare void @reopt.Write_FS(i16)
declare i16 @reopt.Read_GS()
declare void @reopt.Write_GS(i16)
declare void @reopt.MemCopy(i64, i64, i64, i64, i1)
declare i64 @reopt.MemCmp(i64, i64, i64, i64, i1)
declare { i64, i1 } @reopt.SystemCall.Linux(i64, i64, i64, i64, i64, i64, i64)
declare { i64, i1 } @reopt.SystemCall.FreeBSD(i64, i64, i64, i64, i64, i64, i64)
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
define { i64, i64, <2 x double> } @F400a28(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_400a28
block_400a28:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 400a28: mov    rax,rdi
  ; # 400a2b: cmp    rdx,0x8
  ; r2 := (bv_ult arg2 0x8 :: [64])
  %R2 = icmp ult i64 %a2, 8
  ; # 400a2f: jb     400a45
  br i1 %R2, label %subblock_400a28_1, label %subblock_400a28_2
subblock_400a28_1:
  br label %block_400a45
subblock_400a28_2:
  br label %block_400a31
block_400a31:
  %R6 = phi i128 [ %r0, %subblock_400a28_2 ]
  %R5 = phi i1 [ 0, %subblock_400a28_2 ]
  %R4 = phi i64 [ %a0, %subblock_400a28_2 ]
  %R3 = phi i64 [ %a2, %subblock_400a28_2 ]
  ; # 400a31: test   edi,0x7
  ; r7 := (trunc r4 32)
  %R7 = trunc i64 %R4 to i32
  ; r8 := (bv_and r7 0x7 :: [32])
  %R8 = and i32 %R7, 7
  ; r9 := (bv_eq r8 0x0 :: [32])
  %R9 = icmp eq i32 %R8, 0
  ; # 400a37: je     400a45
  br i1 %R9, label %subblock_400a31_1, label %subblock_400a31_2
subblock_400a31_1:
  br label %block_400a45
subblock_400a31_2:
  br label %block_400a39
block_400a39:
  %R13 = phi i128 [ %R13, %subblock_400a39_1 ], [ %R6, %subblock_400a31_2 ]
  %R12 = phi i1 [ %R12, %subblock_400a39_1 ], [ %R5, %subblock_400a31_2 ]
  %R11 = phi i64 [ %R16, %subblock_400a39_1 ], [ %R4, %subblock_400a31_2 ]
  %R10 = phi i64 [ %R17, %subblock_400a39_1 ], [ %R3, %subblock_400a31_2 ]
  ; # 400a39: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; r14 := (bv_add r11 0xffffffffffffffff :: [64])
  %R14 = add i64 %R11, 18446744073709551615
  ; r15 := (bv_add r11 0x1 :: [64])
  %R15 = add i64 %R11, 1
  ; r16 := (mux r12 r14 r15)
  %R16 = select i1 %R12, i64 %R14, i64 %R15
  ; # 400a3a: dec    rdx
  ; r17 := (bv_add r10 0xffffffffffffffff :: [64])
  %R17 = add i64 %R10, 18446744073709551615
  ; # 400a3d: test   edi,0x7
  ; r18 := (trunc r16 32)
  %R18 = trunc i64 %R16 to i32
  ; r19 := (bv_and r18 0x7 :: [32])
  %R19 = and i32 %R18, 7
  ; r20 := (bv_eq r19 0x0 :: [32])
  %R20 = icmp eq i32 %R19, 0
  ; # 400a43: jne    400a39
  ; r21 := (bv_complement r20)
  %R21 = xor i1 %R20, -1
  br i1 %R21, label %subblock_400a39_1, label %subblock_400a39_2
subblock_400a39_1:
  br label %block_400a39
subblock_400a39_2:
  br label %block_400a45
block_400a45:
  %R23 = phi i128 [ %R13, %subblock_400a39_2 ], [ %R6, %subblock_400a31_1 ], [ %r0, %subblock_400a28_1 ]
  %R22 = phi i64 [ %R17, %subblock_400a39_2 ], [ %R3, %subblock_400a31_1 ], [ %a2, %subblock_400a28_1 ]
  ; # 400a45: mov    rcx,rdx
  ; # 400a48: shr    rcx,0x3
  ; # 400a4c: rep movs QWORD PTR [rdi],QWORD PTR [rsi]
  ; # 400a4f: and    edx,0x7
  ; r24 := (trunc r22 32)
  %R24 = trunc i64 %R22 to i32
  ; r25 := (bv_and r24 0x7 :: [32])
  %R25 = and i32 %R24, 7
  ; r26 := (bv_eq r25 0x0 :: [32])
  %R26 = icmp eq i32 %R25, 0
  ; r27 := (uext r25 64)
  %R27 = zext i32 %R25 to i64
  ; # 400a52: je     400a59
  br i1 %R26, label %subblock_400a45_1, label %subblock_400a45_2
subblock_400a45_1:
  br label %block_400a59
subblock_400a45_2:
  br label %block_400a54
block_400a54:
  %R29 = phi i128 [ %R29, %subblock_400a54_1 ], [ %R23, %subblock_400a45_2 ]
  %R28 = phi i64 [ %R33, %subblock_400a54_1 ], [ %R27, %subblock_400a45_2 ]
  ; # 400a54: movs   BYTE PTR [rdi],BYTE PTR [rsi]
  ; # 400a55: dec    edx
  ; r30 := (trunc r28 32)
  %R30 = trunc i64 %R28 to i32
  ; r31 := (bv_add r30 0xffffffff :: [32])
  %R31 = add i32 %R30, 4294967295
  ; r32 := (bv_eq r30 0x1 :: [32])
  %R32 = icmp eq i32 %R30, 1
  ; r33 := (uext r31 64)
  %R33 = zext i32 %R31 to i64
  ; # 400a57: jne    400a54
  ; r34 := (bv_complement r32)
  %R34 = xor i1 %R32, -1
  br i1 %R34, label %subblock_400a54_1, label %subblock_400a54_2
subblock_400a54_1:
  br label %block_400a54
subblock_400a54_2:
  br label %block_400a59
block_400a59:
  %R35 = phi i128 [ %R29, %subblock_400a54_2 ], [ %R23, %subblock_400a45_1 ]
  ; # 400a59: ret
  %r38 = bitcast i128 %R35 to <2 x double>
  %r39 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r40 = insertvalue { i64, i64, <2 x double> } %r39, i64 undef, 1
  %r41 = insertvalue { i64, i64, <2 x double> } %r40, <2 x double> %r38, 2
  ret { i64, i64, <2 x double> } %r41
failure:
  br label %failure
}