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
declare { i64, i64, <2 x double> } @F402e57(i64)
define { i64, i64, <2 x double> } @F402496(i64 %a0) {
entry:
  br label %block_402496
block_402496:
  ; r0 := (alloca 0x10 :: [64])
  %r0 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 402496: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402497: mov    QWORD PTR [rdi],rdi
  ; *(arg0) = arg0
  %r4 = inttoptr i64 %a0 to i64*
  store i64 %a0, i64* %r4
  ; # 40249a: mov    rbx,rdi
  ; # 40249d: call   402e57
  %r5 = call { i64, i64, <2 x double> } @F402e57(i64 %a0)
  %R3 = extractvalue { i64, i64, <2 x double> } %r5, 0
  %r7 = extractvalue { i64, i64, <2 x double> } %r5, 2
  %R4 = bitcast <2 x double> %r7 to i128
  br label %block_4024a2
block_4024a2:
  %R7 = phi i128 [ %R4, %block_402496 ]
  %R6 = phi i64 [ %a0, %block_402496 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R5 = phi i64 [ undef, %block_402496 ]
  ; # 4024a2: or     edx,0xffffffff
  ; # 4024a5: test   eax,eax
  ; r8 := (trunc r5 32)
  %R8 = trunc i64 %R5 to i32
  ; r9 := (bv_slt r8 0x0 :: [32])
  %R9 = icmp slt i32 %R8, 0
  ; r10 := (bv_eq r8 0x0 :: [32])
  %R10 = icmp eq i32 %R8, 0
  ; # 4024a7: js     4024de
  br i1 %R9, label %subblock_4024a2_1, label %subblock_4024a2_2
subblock_4024a2_1:
  br label %block_4024de
subblock_4024a2_2:
  br label %block_4024a9
block_4024a9:
  %R12 = phi i1 [ %R10, %subblock_4024a2_2 ]
  %R11 = phi i64 [ %R6, %subblock_4024a2_2 ]
  ; # 4024a9: jne    4024b5
  ; r13 := (bv_complement r12)
  %R13 = xor i1 %R12, -1
  br i1 %R13, label %subblock_4024a9_1, label %subblock_4024a9_2
subblock_4024a9_1:
  br label %block_4024b5
subblock_4024a9_2:
  br label %block_4024ab
block_4024ab:
  %R14 = phi i64 [ %R11, %subblock_4024a9_2 ]
  ; # 4024ab: mov    DWORD PTR [rip+0x2024cb],0x1
  ; *(0x604980 :: [64]) = 0x1 :: [32]
  %r19 = inttoptr i64 6310272 to i32*
  store i32 1, i32* %r19
  br label %block_4024b5
block_4024b5:
  %R15 = phi i64 [ %R14, %block_4024ab ], [ %R11, %subblock_4024a9_1 ]
  ; # 4024b5: lea    rdi,[rbx+0x38]
  ; r16 := (bv_add r15 0x38 :: [64])
  %R16 = add i64 %R15, 56
  ; # 4024b9: mov    eax,0xda
  ; # 4024be: syscall
  ; sys_set_tid_address
  %r22 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R16, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 218)
  %R17 = extractvalue { i64, i1 } %r22, 0
  br label %block_4024c0
block_4024c0:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R20 = phi i128 [ undef, %block_4024b5 ]
  %R19 = phi i64 [ %R15, %block_4024b5 ]
  %R18 = phi i64 [ %R17, %block_4024b5 ]
  ; # 4024c0: mov    DWORD PTR [rbx+0x38],eax
  ; r21 := (trunc r18 32)
  %R21 = trunc i64 %R18 to i32
  ; r22 := (bv_add r19 0x38 :: [64])
  %R22 = add i64 %R19, 56
  ; *(r22) = r21
  %r29 = inttoptr i64 %R22 to i32*
  store i32 %R21, i32* %r29
  ; # 4024c3: lea    rax,[rbx+0xe0]
  ; r23 := (bv_add r19 0xe0 :: [64])
  %R23 = add i64 %R19, 224
  ; # 4024ca: mov    QWORD PTR [rbx+0x100],0x6049c0
  ; r24 := (bv_add r19 0x100 :: [64])
  %R24 = add i64 %R19, 256
  ; *(r24) = 0x6049c0 :: [64]
  %r32 = inttoptr i64 %R24 to i64*
  store i64 6310336, i64* %r32
  ; # 4024d5: xor    edx,edx
  ; # 4024d7: mov    QWORD PTR [rbx+0xe0],rax
  ; *(r23) = r23
  %r33 = inttoptr i64 %R23 to i64*
  store i64 %R23, i64* %r33
  br label %block_4024de
block_4024de:
  %R26 = phi i128 [ %R20, %block_4024c0 ], [ %R7, %subblock_4024a2_1 ]
  %R25 = phi i64 [ 0, %block_4024c0 ], [ 4294967295, %subblock_4024a2_1 ]
  ; # 4024de: mov    eax,edx
  ; r27 := (trunc r25 32)
  %R27 = trunc i64 %R25 to i32
  ; r28 := (uext r27 64)
  %R28 = zext i32 %R27 to i64
  ; # 4024e0: pop    rbx
  ; # 4024e1: ret
  %r38 = bitcast i128 %R26 to <2 x double>
  %r39 = insertvalue { i64, i64, <2 x double> } undef, i64 %R28, 0
  %r40 = insertvalue { i64, i64, <2 x double> } %r39, i64 %R25, 1
  %r41 = insertvalue { i64, i64, <2 x double> } %r40, <2 x double> %r38, 2
  ret { i64, i64, <2 x double> } %r41
failure:
  br label %failure
}