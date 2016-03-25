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
declare { i64, i64, <2 x double> } @F400190(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400210(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_400210
block_400210:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 400210: cmp    BYTE PTR [rip+0x203e89],0x0
  ; r2 := *0x6040a0 :: [64]
  %r4 = inttoptr i64 6308000 to i8*
  %R2 = load i8* %r4
  ; r3 := (bv_eq r2 0x0 :: [8])
  %R3 = icmp eq i8 %R2, 0
  ; # 400217: jne    40022a
  ; r4 := (bv_complement r3)
  %R4 = xor i1 %R3, -1
  br i1 %R4, label %subblock_400210_1, label %subblock_400210_2
subblock_400210_1:
  br label %block_40022a
subblock_400210_2:
  br label %block_400219
block_400219:
  %R10 = phi i128 [ %r0, %subblock_400210_2 ]
  %R9 = phi i64 [ %a0, %subblock_400210_2 ]
  %R8 = phi i64 [ %a1, %subblock_400210_2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbp
  %R7 = phi i64 [ undef, %subblock_400210_2 ]
  %R6 = phi i64 [ %R1, %subblock_400210_2 ]
  %R5 = phi i64 [ %a2, %subblock_400210_2 ]
  ; # 400219: push   rbp
  ; r11 := (bv_add r6 0xfffffffffffffff8 :: [64])
  %R11 = add i64 %R6, 18446744073709551608
  ; *(r11) = r7
  %r15 = inttoptr i64 %R11 to i64*
  store i64 %R7, i64* %r15
  ; # 40021a: mov    rbp,rsp
  ; # 40021d: call   400190
  %r16 = bitcast i128 %R10 to <2 x double>
  %r17 = call { i64, i64, <2 x double> } @F400190(i64 %R9, i64 %R8, i64 %R5, <2 x double> %r16)
  %R12 = extractvalue { i64, i64, <2 x double> } %r17, 0
  %R13 = extractvalue { i64, i64, <2 x double> } %r17, 1
  %r20 = extractvalue { i64, i64, <2 x double> } %r17, 2
  %R14 = bitcast <2 x double> %r20 to i128
  br label %block_400222
block_400222:
  ; # 400222: pop    rbp
  ; # 400223: mov    BYTE PTR [rip+0x203e76],0x1
  ; *(0x6040a0 :: [64]) = 0x1 :: [8]
  %r22 = inttoptr i64 6308000 to i8*
  store i8 1, i8* %r22
  br label %block_40022a
block_40022a:
  ; # 40022a: repz ret
  %r23 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r24 = insertvalue { i64, i64, <2 x double> } %r23, i64 undef, 1
  %r25 = insertvalue { i64, i64, <2 x double> } %r24, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r25
failure:
  br label %failure
}