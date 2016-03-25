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
declare { i64, i64, <2 x double> } @F400130(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F40068c(i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F4007e2(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400802(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_400802
block_400802:
  ; r0 := (alloca 0x30 :: [64])
  %r1 = alloca i8, i64 48
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x30 :: [64])
  %R1 = add i64 %R0, 48
  ; # 400802: push   r13
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 400804: push   r12
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 400806: mov    r13,rdi
  ; # 400809: push   rbp
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 40080a: push   rbx
  ; r5 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R5 = add i64 %R1, 18446744073709551584
  ; # 40080b: mov    rbx,rdx
  ; # 40080e: push   rax
  ; r6 := (bv_add r1 0xffffffffffffffd8 :: [64])
  %R6 = add i64 %R1, 18446744073709551576
  ; *(r6) = unsupported (Initial register rax)
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %r9 = inttoptr i64 %R6 to i64*
  store i64 undef, i64* %r9
  ; # 40080f: movsxd rax,esi
  ; r7 := (trunc arg1 32)
  %R7 = trunc i64 %a1 to i32
  ; r8 := (sext r7 64)
  %R8 = sext i32 %R7 to i64
  ; # 400812: mov    rsi,QWORD PTR [rdx]
  ; r9 := *arg2
  %r12 = inttoptr i64 %a2 to i64*
  %R9 = load i64* %r12
  ; # 400815: lea    r12,[rdx+rax*8+0x8]
  ; r10 := (bv_mul 0x8 :: [64] r8)
  %R10 = mul i64 8, %R8
  ; r11 := (bv_add arg2 r10)
  %R11 = add i64 %a2, %R10
  ; r12 := (bv_add r11 0x8 :: [64])
  %R12 = add i64 %R11, 8
  ; # 40081a: mov    rbp,rax
  ; # 40081d: mov    rdi,r12
  ; # 400820: call   40068c
  %r17 = bitcast i128 %r0 to <2 x double>
  %r18 = call { i64, i64, <2 x double> } @F40068c(i64 %R12, i64 %R9, <2 x double> %r17)
  %R13 = extractvalue { i64, i64, <2 x double> } %r18, 0
  %R14 = extractvalue { i64, i64, <2 x double> } %r18, 1
  %r21 = extractvalue { i64, i64, <2 x double> } %r18, 2
  %R15 = bitcast <2 x double> %r21 to i128
  br label %block_400825
block_400825:
  %R23 = phi i128 [ %R15, %block_400802 ]
  %R22 = phi i64 [ %a0, %block_400802 ]
  %R21 = phi i64 [ %R12, %block_400802 ]
  %R20 = phi i64 [ %R13, %block_400802 ]
  %R19 = phi i64 [ %R14, %block_400802 ]
  %R18 = phi i64 [ %R8, %block_400802 ]
  %R17 = phi i64 [ %a2, %block_400802 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R16 = phi i64 [ undef, %block_400802 ]
  ; # 400825: call   4007e2
  %r31 = bitcast i128 %R23 to <2 x double>
  %r32 = call { i64, i64, <2 x double> } @F4007e2(i64 %R20, i64 %R19, i64 %R16, <2 x double> %r31)
  %r33 = extractvalue { i64, i64, <2 x double> } %r32, 2
  %R24 = bitcast <2 x double> %r33 to i128
  br label %block_40082a
block_40082a:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R39 = phi i128 [ undef, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R38 = phi i128 [ undef, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R37 = phi i128 [ undef, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R36 = phi i128 [ undef, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R35 = phi i128 [ undef, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R34 = phi i128 [ undef, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R33 = phi i128 [ undef, %block_400825 ]
  %R32 = phi i128 [ %R24, %block_400825 ]
  %R31 = phi i64 [ %R22, %block_400825 ]
  %R30 = phi i64 [ %R21, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R29 = phi i64 [ undef, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R28 = phi i64 [ undef, %block_400825 ]
  %R27 = phi i64 [ %R18, %block_400825 ]
  %R26 = phi i64 [ %R17, %block_400825 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R25 = phi i64 [ undef, %block_400825 ]
  ; # 40082a: mov    edi,ebp
  ; r40 := (trunc r27 32)
  %R40 = trunc i64 %R27 to i32
  ; r41 := (uext r40 64)
  %R41 = zext i32 %R40 to i64
  ; # 40082c: mov    rdx,r12
  ; # 40082f: mov    rsi,rbx
  ; # 400832: call   r13
  %r52 = inttoptr i64 %R31 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r53 = bitcast i128 %R32 to <2 x double>
  %r54 = bitcast i128 %R33 to <2 x double>
  %r55 = bitcast i128 %R34 to <2 x double>
  %r56 = bitcast i128 %R35 to <2 x double>
  %r57 = bitcast i128 %R36 to <2 x double>
  %r58 = bitcast i128 %R37 to <2 x double>
  %r59 = bitcast i128 %R38 to <2 x double>
  %r60 = bitcast i128 %R39 to <2 x double>
  %r61 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r52(i64 %R41, i64 %R26, i64 %R30, i64 %R25, i64 %R28, i64 %R29, <2 x double> %r53, <2 x double> %r54, <2 x double> %r55, <2 x double> %r56, <2 x double> %r57, <2 x double> %r58, <2 x double> %r59, <2 x double> %r60)
  %R42 = extractvalue { i64, i64, <2 x double> } %r61, 0
  %R43 = extractvalue { i64, i64, <2 x double> } %r61, 1
  %r64 = extractvalue { i64, i64, <2 x double> } %r61, 2
  %R44 = bitcast <2 x double> %r64 to i128
  br label %block_400835
block_400835:
  %R48 = phi i128 [ %R44, %block_40082a ]
  %R47 = phi i64 [ %R43, %block_40082a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R46 = phi i64 [ undef, %block_40082a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R45 = phi i64 [ undef, %block_40082a ]
  ; # 400835: mov    edi,eax
  ; r49 := (trunc r45 32)
  %R49 = trunc i64 %R45 to i32
  ; r50 := (uext r49 64)
  %R50 = zext i32 %R49 to i64
  ; # 400837: call   400130
  %r72 = bitcast i128 %R48 to <2 x double>
  %r73 = call { i64, i64, <2 x double> } @F400130(i64 %R50, i64 %R47, i64 %R46, <2 x double> %r72)
  %R51 = extractvalue { i64, i64, <2 x double> } %r73, 0
  %R52 = extractvalue { i64, i64, <2 x double> } %r73, 1
  %r76 = extractvalue { i64, i64, <2 x double> } %r73, 2
  %R53 = bitcast <2 x double> %r76 to i128
  br label %block_40083c
block_40083c:
  %R56 = phi i128 [ %R53, %block_400835 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R55 = phi i64 [ undef, %block_400835 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R54 = phi i64 [ undef, %block_400835 ]
  ; # 40083c: ret
  %r81 = bitcast i128 %R56 to <2 x double>
  %r82 = insertvalue { i64, i64, <2 x double> } undef, i64 %R54, 0
  %r83 = insertvalue { i64, i64, <2 x double> } %r82, i64 %R55, 1
  %r84 = insertvalue { i64, i64, <2 x double> } %r83, <2 x double> %r81, 2
  ret { i64, i64, <2 x double> } %r84
failure:
  br label %failure
}