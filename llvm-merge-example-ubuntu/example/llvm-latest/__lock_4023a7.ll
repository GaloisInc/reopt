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
declare { i64, i64, <2 x double> } @F40241c(i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4023a7(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_4023a7
block_4023a7:
  ; r0 := (alloca 0x20 :: [64])
  %r1 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 4023a7: mov    eax,DWORD PTR [rip+0x2025df]
  ; r2 := *0x60498c :: [64]
  %r4 = inttoptr i64 6310284 to i32*
  %R2 = load i32* %r4
  ; # 4023ad: test   eax,eax
  ; r3 := (bv_eq r2 0x0 :: [32])
  %R3 = icmp eq i32 %R2, 0
  ; # 4023af: jne    4023b2
  ; r4 := (bv_complement r3)
  %R4 = xor i1 %R3, -1
  br i1 %R4, label %subblock_4023a7_1, label %subblock_4023a7_2
subblock_4023a7_1:
  br label %block_4023b2
subblock_4023a7_2:
  br label %block_4023b1
block_4023b1:
  %R5 = phi i128 [ %r0, %subblock_4023a7_2 ]
  ; # 4023b1: ret
  %r9 = bitcast i128 %R5 to <2 x double>
  %r10 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r11 = insertvalue { i64, i64, <2 x double> } %r10, i64 undef, 1
  %r12 = insertvalue { i64, i64, <2 x double> } %r11, <2 x double> %r9, 2
  ret { i64, i64, <2 x double> } %r12
block_4023b2:
  %R11 = phi i128 [ %r0, %subblock_4023a7_1 ]
  %R10 = phi i64 [ %a0, %subblock_4023a7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbp
  %R9 = phi i64 [ undef, %subblock_4023a7_1 ]
  %R8 = phi i64 [ %R1, %subblock_4023a7_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbx
  %R7 = phi i64 [ undef, %subblock_4023a7_1 ]
  %R6 = phi i64 [ %a2, %subblock_4023a7_1 ]
  ; # 4023b2: push   rbp
  ; r12 := (bv_add r8 0xfffffffffffffff8 :: [64])
  %R12 = add i64 %R8, 18446744073709551608
  ; *(r12) = r9
  %r20 = inttoptr i64 %R12 to i64*
  store i64 %R9, i64* %r20
  ; # 4023b3: push   rbx
  ; r13 := (bv_add r8 0xfffffffffffffff0 :: [64])
  %R13 = add i64 %R8, 18446744073709551600
  ; *(r13) = r7
  %r22 = inttoptr i64 %R13 to i64*
  store i64 %R7, i64* %r22
  ; # 4023b4: lea    rbp,[rdi+0x4]
  ; r14 := (bv_add r10 0x4 :: [64])
  %R14 = add i64 %R10, 4
  ; # 4023b8: push   rdx
  ; r15 := (bv_add r8 0xffffffffffffffe8 :: [64])
  %R15 = add i64 %R8, 18446744073709551592
  ; *(r15) = r6
  %r25 = inttoptr i64 %R15 to i64*
  store i64 %R6, i64* %r25
  ; # 4023b9: mov    rbx,rdi
  br label %block_4023bc
block_4023bc:
  %R18 = phi i128 [ %R27, %block_4023dc ], [ %R11, %block_4023b2 ]
  %R17 = phi i64 [ %R26, %block_4023dc ], [ %R14, %block_4023b2 ]
  %R16 = phi i64 [ %R25, %block_4023dc ], [ %R10, %block_4023b2 ]
  ; # 4023bc: mov    eax,0x1
  ; # 4023c1: xchg   DWORD PTR [rbx],eax
  ; r19 := *r16
  %r29 = inttoptr i64 %R16 to i32*
  %R19 = load i32* %r29
  ; *(r16) = 0x1 :: [32]
  %r31 = inttoptr i64 %R16 to i32*
  store i32 1, i32* %r31
  ; # 4023c3: test   eax,eax
  ; r20 := (bv_eq r19 0x0 :: [32])
  %R20 = icmp eq i32 %R19, 0
  ; # 4023c5: je     4023de
  br i1 %R20, label %subblock_4023bc_1, label %subblock_4023bc_2
subblock_4023bc_1:
  br label %block_4023de
subblock_4023bc_2:
  br label %block_4023c7
block_4023c7:
  %R23 = phi i128 [ %R18, %subblock_4023bc_2 ]
  %R22 = phi i64 [ %R17, %subblock_4023bc_2 ]
  %R21 = phi i64 [ %R16, %subblock_4023bc_2 ]
  ; # 4023c7: mov    ecx,0x1
  ; # 4023cc: mov    edx,0x1
  ; # 4023d1: mov    rsi,rbp
  ; # 4023d4: mov    rdi,rbx
  ; # 4023d7: call   40241c
  %r36 = bitcast i128 %R23 to <2 x double>
  %r37 = call { i64, i64, <2 x double> } @F40241c(i64 %R21, i64 %R22, i64 1, i64 1, <2 x double> %r36)
  %r38 = extractvalue { i64, i64, <2 x double> } %r37, 2
  %R24 = bitcast <2 x double> %r38 to i128
  br label %block_4023dc
block_4023dc:
  %R27 = phi i128 [ %R24, %block_4023c7 ]
  %R26 = phi i64 [ %R22, %block_4023c7 ]
  %R25 = phi i64 [ %R21, %block_4023c7 ]
  ; # 4023dc: jmp    4023bc
  br label %block_4023bc
block_4023de:
  %R28 = phi i128 [ %R18, %subblock_4023bc_1 ]
  ; # 4023de: pop    rax
  ; # 4023df: pop    rbx
  ; # 4023e0: pop    rbp
  ; # 4023e1: ret
  %r44 = bitcast i128 %R28 to <2 x double>
  %r45 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r46 = insertvalue { i64, i64, <2 x double> } %r45, i64 undef, 1
  %r47 = insertvalue { i64, i64, <2 x double> } %r46, <2 x double> %r44, 2
  ret { i64, i64, <2 x double> } %r47
failure:
  br label %failure
}