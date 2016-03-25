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
define { i64, i64, <2 x double> } @F402ce8(i64 %a0, <2 x double> %a1) {
entry:
  %r0 = bitcast <2 x double> %a1 to i128
  br label %block_402ce8
block_402ce8:
  ; r0 := (alloca 0x0 :: [64])
  %r1 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 402ce8: mov    al,BYTE PTR [rdi+0x8a]
  ; r2 := (bv_add arg0 0x8a :: [64])
  %R2 = add i64 %a0, 138
  ; r3 := *r2
  %r5 = inttoptr i64 %R2 to i8*
  %R3 = load i8* %r5
  ; r4 := (bv_and unsupported (Initial register rax) 0xffffffffffffff00 :: [64])
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %R4 = and i64 undef, 18446744073709551360
  ; r5 := (uext r3 64)
  %R5 = zext i8 %R3 to i64
  ; r6 := (bv_or r4 r5)
  %R6 = or i64 %R4, %R5
  ; # 402cee: lea    edx,[rax-0x1]
  ; r7 := (bv_add r6 0xffffffffffffffff :: [64])
  %R7 = add i64 %R6, 18446744073709551615
  ; r8 := (trunc r7 32)
  %R8 = trunc i64 %R7 to i32
  ; r9 := (uext r8 64)
  %R9 = zext i32 %R8 to i64
  ; # 402cf1: or     eax,edx
  ; r10 := (trunc r6 32)
  %R10 = trunc i64 %R6 to i32
  ; r11 := (bv_or r10 r8)
  %R11 = or i32 %R10, %R8
  ; r12 := (trunc r11 8)
  %R12 = trunc i32 %R11 to i8
  ; # 402cf3: mov    BYTE PTR [rdi+0x8a],al
  ; *(r2) = r12
  %r16 = inttoptr i64 %R2 to i8*
  store i8 %R12, i8* %r16
  ; # 402cf9: mov    eax,DWORD PTR [rdi]
  ; r13 := *arg0
  %r17 = inttoptr i64 %a0 to i32*
  %R13 = load i32* %r17
  ; r14 := (uext r13 64)
  %R14 = zext i32 %R13 to i64
  ; # 402cfb: test   al,0x8
  ; r15 := (trunc r13 8)
  %R15 = trunc i32 %R13 to i8
  ; r16 := (bv_and r15 0x8 :: [8])
  %R16 = and i8 %R15, 8
  ; r17 := (bv_eq r16 0x0 :: [8])
  %R17 = icmp eq i8 %R16, 0
  ; # 402cfd: je     402d08
  br i1 %R17, label %subblock_402ce8_1, label %subblock_402ce8_2
subblock_402ce8_1:
  br label %block_402d08
subblock_402ce8_2:
  br label %block_402cff
block_402cff:
  %R21 = phi i128 [ %r0, %subblock_402ce8_2 ]
  %R20 = phi i64 [ %a0, %subblock_402ce8_2 ]
  %R19 = phi i64 [ %R9, %subblock_402ce8_2 ]
  %R18 = phi i64 [ %R14, %subblock_402ce8_2 ]
  ; # 402cff: or     eax,0x20
  ; r22 := (trunc r18 32)
  %R22 = trunc i64 %R18 to i32
  ; r23 := (bv_or r22 0x20 :: [32])
  %R23 = or i32 %R22, 32
  ; # 402d02: mov    DWORD PTR [rdi],eax
  ; *(r20) = r23
  %r29 = inttoptr i64 %R20 to i32*
  store i32 %R23, i32* %r29
  ; # 402d04: or     eax,0xffffffff
  ; # 402d07: ret
  %r30 = bitcast i128 %R21 to <2 x double>
  %r31 = insertvalue { i64, i64, <2 x double> } undef, i64 4294967295, 0
  %r32 = insertvalue { i64, i64, <2 x double> } %r31, i64 %R19, 1
  %r33 = insertvalue { i64, i64, <2 x double> } %r32, <2 x double> %r30, 2
  ret { i64, i64, <2 x double> } %r33
block_402d08:
  %R26 = phi i128 [ %r0, %subblock_402ce8_1 ]
  %R25 = phi i64 [ %a0, %subblock_402ce8_1 ]
  %R24 = phi i64 [ %R9, %subblock_402ce8_1 ]
  ; # 402d08: mov    rax,QWORD PTR [rdi+0x58]
  ; r27 := (bv_add r25 0x58 :: [64])
  %R27 = add i64 %R25, 88
  ; r28 := *r27
  %r38 = inttoptr i64 %R27 to i64*
  %R28 = load i64* %r38
  ; # 402d0c: mov    QWORD PTR [rdi+0x10],0x0
  ; r29 := (bv_add r25 0x10 :: [64])
  %R29 = add i64 %R25, 16
  ; *(r29) = 0x0 :: [64]
  %r41 = inttoptr i64 %R29 to i64*
  store i64 0, i64* %r41
  ; # 402d14: mov    QWORD PTR [rdi+0x8],0x0
  ; r30 := (bv_add r25 0x8 :: [64])
  %R30 = add i64 %R25, 8
  ; *(r30) = 0x0 :: [64]
  %r43 = inttoptr i64 %R30 to i64*
  store i64 0, i64* %r43
  ; # 402d1c: mov    QWORD PTR [rdi+0x38],rax
  ; r31 := (bv_add r25 0x38 :: [64])
  %R31 = add i64 %R25, 56
  ; *(r31) = r28
  %r45 = inttoptr i64 %R31 to i64*
  store i64 %R28, i64* %r45
  ; # 402d20: mov    QWORD PTR [rdi+0x28],rax
  ; r32 := (bv_add r25 0x28 :: [64])
  %R32 = add i64 %R25, 40
  ; *(r32) = r28
  %r47 = inttoptr i64 %R32 to i64*
  store i64 %R28, i64* %r47
  ; # 402d24: add    rax,QWORD PTR [rdi+0x60]
  ; r33 := (bv_add r25 0x60 :: [64])
  %R33 = add i64 %R25, 96
  ; r34 := *r33
  %r49 = inttoptr i64 %R33 to i64*
  %R34 = load i64* %r49
  ; r35 := (bv_add r28 r34)
  %R35 = add i64 %R28, %R34
  ; # 402d28: mov    QWORD PTR [rdi+0x20],rax
  ; r36 := (bv_add r25 0x20 :: [64])
  %R36 = add i64 %R25, 32
  ; *(r36) = r35
  %r53 = inttoptr i64 %R36 to i64*
  store i64 %R35, i64* %r53
  ; # 402d2c: xor    eax,eax
  ; # 402d2e: ret
  %r54 = bitcast i128 %R26 to <2 x double>
  %r55 = insertvalue { i64, i64, <2 x double> } undef, i64 0, 0
  %r56 = insertvalue { i64, i64, <2 x double> } %r55, i64 %R24, 1
  %r57 = insertvalue { i64, i64, <2 x double> } %r56, <2 x double> %r54, 2
  ret { i64, i64, <2 x double> } %r57
failure:
  br label %failure
}