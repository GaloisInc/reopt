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
define { i64, i64, <2 x double> } @F402a2c(i64 %a0, <2 x double> %a1) {
entry:
  %r0 = bitcast <2 x double> %a1 to i128
  br label %block_402a2c
block_402a2c:
  ; r0 := (alloca 0x20 :: [64])
  %r1 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 402a2c: push   r12
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402a2e: push   rbp
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 402a2f: push   rbx
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 402a30: mov    rax,QWORD PTR fs:0x0
  ; r5 := *unsupported (assignment r230 := fs.base)
  ; UNIMPLEMENTED: FnValueUnsupported: assignment r230 := fs.base
  %r7 = inttoptr i64 undef to i64*
  %R5 = load i64* %r7
  ; # 402a39: mov    ebp,DWORD PTR [rax+0x38]
  ; r6 := (bv_add r5 0x38 :: [64])
  %R6 = add i64 %R5, 56
  ; r7 := *r6
  %r10 = inttoptr i64 %R6 to i32*
  %R7 = load i32* %r10
  ; r8 := (uext r7 64)
  %R8 = zext i32 %R7 to i64
  ; # 402a3c: mov    edx,DWORD PTR [rdi+0x8c]
  ; r9 := (bv_add arg0 0x8c :: [64])
  %R9 = add i64 %a0, 140
  ; r10 := *r9
  %r14 = inttoptr i64 %R9 to i32*
  %R10 = load i32* %r14
  ; r11 := (uext r10 64)
  %R11 = zext i32 %R10 to i64
  ; # 402a42: xor    eax,eax
  ; # 402a44: cmp    ebp,edx
  ; r12 := (bv_eq r7 r10)
  %R12 = icmp eq i32 %R7, %R10
  ; # 402a46: je     402a7b
  br i1 %R12, label %subblock_402a2c_1, label %subblock_402a2c_2
subblock_402a2c_1:
  br label %block_402a7b
subblock_402a2c_2:
  br label %block_402a48
block_402a48:
  %R15 = phi i128 [ %r0, %subblock_402a2c_2 ]
  %R14 = phi i64 [ %a0, %subblock_402a2c_2 ]
  %R13 = phi i64 [ %R8, %subblock_402a2c_2 ]
  ; # 402a48: lea    r12,[rdi+0x8c]
  ; r16 := (bv_add r14 0x8c :: [64])
  %R16 = add i64 %R14, 140
  ; # 402a4f: mov    rbx,rdi
  br label %block_402a52
block_402a52:
  %R20 = phi i128 [ %R43, %block_402a74 ], [ %R15, %block_402a48 ]
  %R19 = phi i64 [ %R42, %block_402a74 ], [ %R16, %block_402a48 ]
  %R18 = phi i64 [ %R41, %block_402a74 ], [ %R13, %block_402a48 ]
  %R17 = phi i64 [ %R40, %block_402a74 ], [ %R14, %block_402a48 ]
  ; # 402a52: xor    eax,eax
  ; # 402a54: lock cmpxchg DWORD PTR [r12],ebp
  ; r21 := *r19
  %r26 = inttoptr i64 %R19 to i32*
  %R21 = load i32* %r26
  ; r22 := (bv_eq r21 0x0 :: [32])
  %R22 = icmp eq i32 %R21, 0
  br i1 %R22, label %subblock_402a52_1, label %subblock_402a52_2
subblock_402a52_1:
  ; r23 := (trunc r18 32)
  %R23 = trunc i64 %R18 to i32
  ; *(r19) = r23
  %r30 = inttoptr i64 %R19 to i32*
  store i32 %R23, i32* %r30
  br label %block_402a5a
subblock_402a52_2:
  ; r24 := (uext r21 64)
  %R24 = zext i32 %R21 to i64
  ; *(r19) = r21
  %r32 = inttoptr i64 %R19 to i32*
  store i32 %R21, i32* %r32
  br label %block_402a5a
block_402a5a:
  %R29 = phi i128 [ %R20, %subblock_402a52_2 ], [ %R20, %subblock_402a52_1 ]
  %R28 = phi i64 [ %R19, %subblock_402a52_2 ], [ %R19, %subblock_402a52_1 ]
  %R27 = phi i64 [ %R18, %subblock_402a52_2 ], [ %R18, %subblock_402a52_1 ]
  %R26 = phi i64 [ %R17, %subblock_402a52_2 ], [ %R17, %subblock_402a52_1 ]
  %R25 = phi i64 [ %R24, %subblock_402a52_2 ], [ 0, %subblock_402a52_1 ]
  ; # 402a5a: test   eax,eax
  ; r30 := (trunc r25 32)
  %R30 = trunc i64 %R25 to i32
  ; r31 := (bv_eq r30 0x0 :: [32])
  %R31 = icmp eq i32 %R30, 0
  ; # 402a5c: mov    edx,eax
  ; r32 := (uext r30 64)
  %R32 = zext i32 %R30 to i64
  ; # 402a5e: je     402a76
  br i1 %R31, label %subblock_402a5a_1, label %subblock_402a5a_2
subblock_402a5a_1:
  br label %block_402a76
subblock_402a5a_2:
  br label %block_402a60
block_402a60:
  %R37 = phi i128 [ %R29, %subblock_402a5a_2 ]
  %R36 = phi i64 [ %R28, %subblock_402a5a_2 ]
  %R35 = phi i64 [ %R27, %subblock_402a5a_2 ]
  %R34 = phi i64 [ %R26, %subblock_402a5a_2 ]
  %R33 = phi i64 [ %R32, %subblock_402a5a_2 ]
  ; # 402a60: lea    rsi,[rbx+0x90]
  ; r38 := (bv_add r34 0x90 :: [64])
  %R38 = add i64 %R34, 144
  ; # 402a67: mov    ecx,0x1
  ; # 402a6c: mov    rdi,r12
  ; # 402a6f: call   40241c
  %r47 = bitcast i128 %R37 to <2 x double>
  %r48 = call { i64, i64, <2 x double> } @F40241c(i64 %R36, i64 %R38, i64 %R33, i64 1, <2 x double> %r47)
  %r49 = extractvalue { i64, i64, <2 x double> } %r48, 2
  %R39 = bitcast <2 x double> %r49 to i128
  br label %block_402a74
block_402a74:
  %R43 = phi i128 [ %R39, %block_402a60 ]
  %R42 = phi i64 [ %R36, %block_402a60 ]
  %R41 = phi i64 [ %R35, %block_402a60 ]
  %R40 = phi i64 [ %R34, %block_402a60 ]
  ; # 402a74: jmp    402a52
  br label %block_402a52
block_402a76:
  %R45 = phi i128 [ %R29, %subblock_402a5a_1 ]
  %R44 = phi i64 [ %R32, %subblock_402a5a_1 ]
  ; # 402a76: mov    eax,0x1
  br label %block_402a7b
block_402a7b:
  %R48 = phi i128 [ %R45, %block_402a76 ], [ %r0, %subblock_402a2c_1 ]
  %R47 = phi i64 [ %R44, %block_402a76 ], [ %R11, %subblock_402a2c_1 ]
  %R46 = phi i64 [ 1, %block_402a76 ], [ 0, %subblock_402a2c_1 ]
  ; # 402a7b: pop    rbx
  ; # 402a7c: pop    rbp
  ; # 402a7d: pop    r12
  ; # 402a7f: ret
  %r60 = bitcast i128 %R48 to <2 x double>
  %r61 = insertvalue { i64, i64, <2 x double> } undef, i64 %R46, 0
  %r62 = insertvalue { i64, i64, <2 x double> } %r61, i64 %R47, 1
  %r63 = insertvalue { i64, i64, <2 x double> } %r62, <2 x double> %r60, 2
  ret { i64, i64, <2 x double> } %r63
failure:
  br label %failure
}