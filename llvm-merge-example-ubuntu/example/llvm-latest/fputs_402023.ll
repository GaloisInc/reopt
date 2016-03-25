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
declare { i64, i64, <2 x double> } @F402101(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F402300(i64, <2 x double>)
define { i64, i64, <2 x double> } @F402023(i64 %a0, i64 %a1, <2 x double> %a2) {
entry:
  %r0 = bitcast <2 x double> %a2 to i128
  br label %block_402023
block_402023:
  ; r0 := (alloca 0x20 :: [64])
  %r1 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 402023: push   r12
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402025: push   rbp
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; # 402026: mov    r12,rsi
  ; # 402029: push   rbx
  ; r4 := (bv_add r1 0xffffffffffffffe8 :: [64])
  %R4 = add i64 %R1, 18446744073709551592
  ; # 40202a: mov    rbp,rdi
  ; # 40202d: call   402300
  %r7 = bitcast i128 %r0 to <2 x double>
  %r8 = call { i64, i64, <2 x double> } @F402300(i64 %a0, <2 x double> %r7)
  %R5 = extractvalue { i64, i64, <2 x double> } %r8, 0
  %r10 = extractvalue { i64, i64, <2 x double> } %r8, 2
  %R6 = bitcast <2 x double> %r10 to i128
  br label %block_402032
block_402032:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R19 = phi i128 [ undef, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R18 = phi i128 [ undef, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R17 = phi i128 [ undef, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R16 = phi i128 [ undef, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R15 = phi i128 [ undef, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R14 = phi i128 [ undef, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R13 = phi i128 [ undef, %block_402023 ]
  %R12 = phi i128 [ %R6, %block_402023 ]
  %R11 = phi i64 [ %a1, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R10 = phi i64 [ undef, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R9 = phi i64 [ undef, %block_402023 ]
  %R8 = phi i64 [ %a0, %block_402023 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R7 = phi i64 [ undef, %block_402023 ]
  ; # 402032: mov    rcx,r12
  ; # 402035: mov    rbx,rax
  ; # 402038: mov    rdi,rbp
  ; # 40203b: mov    rdx,rax
  ; # 40203e: mov    esi,0x1
  ; # 402043: call   402101
  %r25 = bitcast i128 %R12 to <2 x double>
  %r26 = bitcast i128 %R13 to <2 x double>
  %r27 = bitcast i128 %R14 to <2 x double>
  %r28 = bitcast i128 %R15 to <2 x double>
  %r29 = bitcast i128 %R16 to <2 x double>
  %r30 = bitcast i128 %R17 to <2 x double>
  %r31 = bitcast i128 %R18 to <2 x double>
  %r32 = bitcast i128 %R19 to <2 x double>
  %r33 = call { i64, i64, <2 x double> } @F402101(i64 %R8, i64 1, i64 %R7, i64 %R11, i64 %R9, i64 %R10, <2 x double> %r25, <2 x double> %r26, <2 x double> %r27, <2 x double> %r28, <2 x double> %r29, <2 x double> %r30, <2 x double> %r31, <2 x double> %r32)
  %r34 = extractvalue { i64, i64, <2 x double> } %r33, 2
  %R20 = bitcast <2 x double> %r34 to i128
  br label %block_402048
block_402048:
  %R21 = phi i128 [ %R20, %block_402032 ]
  ; # 402048: cmp    rbx,rax
  ; # 40204b: setne  al
  ; # 40204e: movzx  eax,al
  ; # 402051: pop    rbx
  ; # 402052: neg    eax
  ; # 402054: pop    rbp
  ; # 402055: pop    r12
  ; # 402057: ret
  %r37 = bitcast i128 %R21 to <2 x double>
  %r38 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r39 = insertvalue { i64, i64, <2 x double> } %r38, i64 undef, 1
  %r40 = insertvalue { i64, i64, <2 x double> } %r39, <2 x double> %r37, 2
  ret { i64, i64, <2 x double> } %r40
failure:
  br label %failure
}