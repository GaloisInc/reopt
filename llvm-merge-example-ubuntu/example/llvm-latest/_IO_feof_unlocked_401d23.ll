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
declare { i64, i64, <2 x double> } @F402a2c(i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402a80(i64, i64, i64, i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F401d23(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  br label %block_401d23
block_401d23:
  ; r0 := (alloca 0x20 :: [64])
  %r1 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 401d23: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 401d24: push   rbx
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 401d25: xor    eax,eax
  ; # 401d27: push   rcx
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; *(r4) = arg3
  %r7 = inttoptr i64 %R4 to i64*
  store i64 %a3, i64* %r7
  ; # 401d28: mov    edx,DWORD PTR [rdi+0x8c]
  ; r5 := (bv_add arg0 0x8c :: [64])
  %R5 = add i64 %a0, 140
  ; r6 := *r5
  %r9 = inttoptr i64 %R5 to i32*
  %R6 = load i32* %r9
  ; r7 := (uext r6 64)
  %R7 = zext i32 %R6 to i64
  ; # 401d2e: mov    rbp,rdi
  ; # 401d31: test   edx,edx
  ; r8 := (bv_slt r6 0x0 :: [32])
  %R8 = icmp slt i32 %R6, 0
  ; # 401d33: js     401d3a
  br i1 %R8, label %subblock_401d23_1, label %subblock_401d23_2
subblock_401d23_1:
  br label %block_401d3a
subblock_401d23_2:
  br label %block_401d35
block_401d35:
  %R12 = phi i128 [ %r0, %subblock_401d23_2 ]
  %R11 = phi i64 [ %a0, %subblock_401d23_2 ]
  %R10 = phi i64 [ %a0, %subblock_401d23_2 ]
  %R9 = phi i64 [ %R4, %subblock_401d23_2 ]
  ; # 401d35: call   402a2c
  ; r13 := (bv_add r9 0xfffffffffffffff8 :: [64])
  %R13 = add i64 %R9, 18446744073709551608
  ; r17 := (bv_add r13 0x8 :: [64])
  %R17 = add i64 %R13, 8
  %r19 = bitcast i128 %R12 to <2 x double>
  %r20 = call { i64, i64, <2 x double> } @F402a2c(i64 %R11, <2 x double> %r19)
  %R14 = extractvalue { i64, i64, <2 x double> } %r20, 0
  %R15 = extractvalue { i64, i64, <2 x double> } %r20, 1
  %r23 = extractvalue { i64, i64, <2 x double> } %r20, 2
  %R16 = bitcast <2 x double> %r23 to i128
  br label %block_401d3a
block_401d3a:
  %R26 = phi i128 [ %R16, %block_401d35 ], [ %r0, %subblock_401d23_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R25 = phi i64 [ undef, %block_401d35 ], [ %a5, %subblock_401d23_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R24 = phi i64 [ undef, %block_401d35 ], [ %a4, %subblock_401d23_1 ]
  %R23 = phi i64 [ %R15, %block_401d35 ], [ %a1, %subblock_401d23_1 ]
  %R22 = phi i64 [ %R10, %block_401d35 ], [ %a0, %subblock_401d23_1 ]
  %R21 = phi i64 [ %R17, %block_401d35 ], [ %R4, %subblock_401d23_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R20 = phi i64 [ undef, %block_401d35 ], [ %R7, %subblock_401d23_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R19 = phi i64 [ undef, %block_401d35 ], [ %a3, %subblock_401d23_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R18 = phi i64 [ undef, %block_401d35 ], [ 0, %subblock_401d23_1 ]
  ; # 401d3a: mov    ebx,DWORD PTR [rbp]
  ; r27 := *r22
  %r34 = inttoptr i64 %R22 to i32*
  %R27 = load i32* %r34
  ; # 401d3d: shr    ebx,0x4
  ; r28 := (bv_shr r27 0x4 :: [32])
  %R28 = lshr i32 %R27, 4
  ; # 401d40: and    ebx,0x1
  ; r29 := (bv_and r28 0x1 :: [32])
  %R29 = and i32 %R28, 1
  ; r30 := (uext r29 64)
  %R30 = zext i32 %R29 to i64
  ; # 401d43: test   eax,eax
  ; r31 := (trunc r18 32)
  %R31 = trunc i64 %R18 to i32
  ; r32 := (bv_eq r31 0x0 :: [32])
  %R32 = icmp eq i32 %R31, 0
  ; # 401d45: je     401d4f
  br i1 %R32, label %subblock_401d3a_1, label %subblock_401d3a_2
subblock_401d3a_1:
  br label %block_401d4f
subblock_401d3a_2:
  br label %block_401d47
block_401d47:
  %R41 = phi i128 [ %R26, %subblock_401d3a_2 ]
  %R40 = phi i64 [ %R25, %subblock_401d3a_2 ]
  %R39 = phi i64 [ %R24, %subblock_401d3a_2 ]
  %R38 = phi i64 [ %R23, %subblock_401d3a_2 ]
  %R37 = phi i64 [ %R22, %subblock_401d3a_2 ]
  %R36 = phi i64 [ %R21, %subblock_401d3a_2 ]
  %R35 = phi i64 [ %R30, %subblock_401d3a_2 ]
  %R34 = phi i64 [ %R20, %subblock_401d3a_2 ]
  %R33 = phi i64 [ %R19, %subblock_401d3a_2 ]
  ; # 401d47: mov    rdi,rbp
  ; # 401d4a: call   402a80
  ; r42 := (bv_add r36 0xfffffffffffffff8 :: [64])
  %R42 = add i64 %R36, 18446744073709551608
  ; r46 := (bv_add r42 0x8 :: [64])
  %R46 = add i64 %R42, 8
  %r52 = bitcast i128 %R41 to <2 x double>
  %r53 = call { i64, i64, <2 x double> } @F402a80(i64 %R37, i64 %R38, i64 %R34, i64 %R33, i64 %R39, i64 %R40, <2 x double> %r52)
  %R43 = extractvalue { i64, i64, <2 x double> } %r53, 0
  %R44 = extractvalue { i64, i64, <2 x double> } %r53, 1
  %r56 = extractvalue { i64, i64, <2 x double> } %r53, 2
  %R45 = bitcast <2 x double> %r56 to i128
  br label %block_401d4f
block_401d4f:
  %R49 = phi i128 [ %R45, %block_401d47 ], [ %R26, %subblock_401d3a_1 ]
  %R48 = phi i64 [ %R46, %block_401d47 ], [ %R21, %subblock_401d3a_1 ]
  %R47 = phi i64 [ %R35, %block_401d47 ], [ %R30, %subblock_401d3a_1 ]
  ; # 401d4f: mov    eax,ebx
  ; r50 := (trunc r47 32)
  %R50 = trunc i64 %R47 to i32
  ; r51 := (uext r50 64)
  %R51 = zext i32 %R50 to i64
  ; # 401d51: pop    rdx
  ; r52 := *r48
  %r63 = inttoptr i64 %R48 to i64*
  %R52 = load i64* %r63
  ; # 401d52: pop    rbx
  ; # 401d53: pop    rbp
  ; # 401d54: ret
  %r65 = bitcast i128 %R49 to <2 x double>
  %r66 = insertvalue { i64, i64, <2 x double> } undef, i64 %R51, 0
  %r67 = insertvalue { i64, i64, <2 x double> } %r66, i64 %R52, 1
  %r68 = insertvalue { i64, i64, <2 x double> } %r67, <2 x double> %r65, 2
  ret { i64, i64, <2 x double> } %r68
failure:
  br label %failure
}