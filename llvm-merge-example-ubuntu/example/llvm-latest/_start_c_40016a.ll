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
declare { i64, i64, <2 x double> } @F400603(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400802(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F40016a(i64 %a0, <2 x double> %a1) {
entry:
  %r0 = bitcast <2 x double> %a1 to i128
  br label %block_40016a
block_40016a:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 40016a: push   rax
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; *(r2) = unsupported (Initial register rax)
  ; UNIMPLEMENTED: FnValueUnsupported: Initial register rax
  %r5 = inttoptr i64 %R2 to i64*
  store i64 undef, i64* %r5
  ; # 40016b: mov    rsi,QWORD PTR [rdi]
  ; r3 := *arg0
  %r6 = inttoptr i64 %a0 to i64*
  %R3 = load i64* %r6
  ; # 40016e: lea    rdx,[rdi+0x8]
  ; r4 := (bv_add arg0 0x8 :: [64])
  %R4 = add i64 %a0, 8
  ; # 400172: xor    r9d,r9d
  ; # 400175: mov    r8d,0x402f54
  ; # 40017b: mov    ecx,0x400120
  ; # 400180: mov    edi,0x400603
  ; # 400185: call   400802
  %r9 = bitcast i128 %r0 to <2 x double>
  %r10 = call { i64, i64, <2 x double> } @F400802(i64 ptrtoint ({ i64, i64, <2 x double> }(i64, i64, i64, <2 x double>)* @F400603 to i64), i64 %R3, i64 %R4, <2 x double> %r9)
  %R5 = extractvalue { i64, i64, <2 x double> } %r10, 0
  %R6 = extractvalue { i64, i64, <2 x double> } %r10, 1
  %r13 = extractvalue { i64, i64, <2 x double> } %r10, 2
  %R7 = bitcast <2 x double> %r13 to i128
  br label %block_40018a
block_40018a:
  %R11 = phi i128 [ %R7, %block_40016a ]
  %R10 = phi i64 [ %R5, %block_40016a ]
  %R9 = phi i64 [ %R6, %block_40016a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R8 = phi i64 [ undef, %block_40016a ]
  ; # 40018a: nop    [rax+rax*1]
  %r19 = bitcast i128 %R11 to <2 x double>
  %r20 = call { i64, i64, <2 x double> } @F400190(i64 %R10, i64 %R9, i64 %R8, <2 x double> %r19)
  ret { i64, i64, <2 x double> } %r20
failure:
  br label %failure
}