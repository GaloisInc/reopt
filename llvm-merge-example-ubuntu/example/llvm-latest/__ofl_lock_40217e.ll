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
declare { i64, i64, <2 x double> } @F4023a7(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F40217e(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_40217e
block_40217e:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 40217e: push   rax
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; *(r2) = unsupported (Initial register rax)
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %r5 = inttoptr i64 %R2 to i64*
  store i64 undef, i64* %r5
  ; # 40217f: mov    edi,0x6046f8
  ; # 402184: call   4023a7
  ; r3 := (bv_add r1 0xfffffffffffffff0 :: [64])
  %R3 = add i64 %R1, 18446744073709551600
  ; r5 := (bv_add r3 0x8 :: [64])
  %R5 = add i64 %R3, 8
  %r8 = bitcast i128 %r0 to <2 x double>
  %r9 = call { i64, i64, <2 x double> } @F4023a7(i64 6309624, i64 %a1, i64 %a2, <2 x double> %r8)
  %r10 = extractvalue { i64, i64, <2 x double> } %r9, 2
  %R4 = bitcast <2 x double> %r10 to i128
  br label %block_402189
block_402189:
  %R7 = phi i128 [ %R4, %block_40217e ]
  %R6 = phi i64 [ %R5, %block_40217e ]
  ; # 402189: mov    eax,0x604700
  ; # 40218e: pop    rdx
  ; r8 := *r6
  %r14 = inttoptr i64 %R6 to i64*
  %R8 = load i64* %r14
  ; # 40218f: ret
  %r16 = bitcast i128 %R7 to <2 x double>
  %r17 = insertvalue { i64, i64, <2 x double> } undef, i64 6309632, 0
  %r18 = insertvalue { i64, i64, <2 x double> } %r17, i64 %R8, 1
  %r19 = insertvalue { i64, i64, <2 x double> } %r18, <2 x double> %r16, 2
  ret { i64, i64, <2 x double> } %r19
failure:
  br label %failure
}