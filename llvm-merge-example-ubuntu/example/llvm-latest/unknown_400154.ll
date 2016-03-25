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
declare { i64, i64, <2 x double> } @F40016a(i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400190(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400603(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F400802(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400154(<2 x double> %a0) {
entry:
  %r0 = bitcast <2 x double> %a0 to i128
  br label %block_400154
block_400154:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 400154: xor    rbp,rbp
  ; # 400157: mov    rdi,rsp
  ; # 40015a: lea    rsi,[rip+0xffffffffffbffe9f]
  ; # 400161: and    rsp,0xfffffffffffffff0
  ; r2 := (bv_and r1 0xfffffffffffffff0 :: [64])
  %R2 = and i64 %R1, 18446744073709551600
  ; # 400165: call   40016a
  ; r3 := (bv_add r2 0xfffffffffffffff8 :: [64])
  %R3 = add i64 %R2, 18446744073709551608
  ; r6 := (bv_add r3 0x8 :: [64])
  %R6 = add i64 %R3, 8
  %r7 = bitcast i128 %r0 to <2 x double>
  %r8 = call { i64, i64, <2 x double> } @F40016a(i64 %R1, <2 x double> %r7)
  %R4 = extractvalue { i64, i64, <2 x double> } %r8, 0
  %r10 = extractvalue { i64, i64, <2 x double> } %r8, 2
  %R5 = bitcast <2 x double> %r10 to i128
  br label %block_40016a
block_40016a:
  %R10 = phi i128 [ %R5, %block_400154 ]
  %R9 = phi i64 [ %R4, %block_400154 ]
  %R8 = phi i64 [ %R6, %block_400154 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R7 = phi i64 [ undef, %block_400154 ]
  ; # 40016a: push   rax
  ; r11 := (bv_add r8 0xfffffffffffffff8 :: [64])
  %R11 = add i64 %R8, 18446744073709551608
  ; *(r11) = r7
  %r17 = inttoptr i64 %R11 to i64*
  store i64 %R7, i64* %r17
  ; # 40016b: mov    rsi,QWORD PTR [rdi]
  ; r12 := *r9
  %r18 = inttoptr i64 %R9 to i64*
  %R12 = load i64* %r18
  ; # 40016e: lea    rdx,[rdi+0x8]
  ; r13 := (bv_add r9 0x8 :: [64])
  %R13 = add i64 %R9, 8
  ; # 400172: xor    r9d,r9d
  ; # 400175: mov    r8d,0x402f54
  ; # 40017b: mov    ecx,0x400120
  ; # 400180: mov    edi,0x400603
  ; # 400185: call   400802
  %r21 = bitcast i128 %R10 to <2 x double>
  %r22 = call { i64, i64, <2 x double> } @F400802(i64 ptrtoint ({ i64, i64, <2 x double> }(i64, i64, i64, <2 x double>)* @F400603 to i64), i64 %R12, i64 %R13, <2 x double> %r21)
  %R14 = extractvalue { i64, i64, <2 x double> } %r22, 0
  %R15 = extractvalue { i64, i64, <2 x double> } %r22, 1
  %r25 = extractvalue { i64, i64, <2 x double> } %r22, 2
  %R16 = bitcast <2 x double> %r25 to i128
  br label %block_40018a
block_40018a:
  %R20 = phi i128 [ %R16, %block_40016a ]
  %R19 = phi i64 [ %R14, %block_40016a ]
  %R18 = phi i64 [ %R15, %block_40016a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R17 = phi i64 [ undef, %block_40016a ]
  ; # 40018a: nop    [rax+rax*1]
  %r31 = bitcast i128 %R20 to <2 x double>
  %r32 = call { i64, i64, <2 x double> } @F400190(i64 %R19, i64 %R18, i64 %R17, <2 x double> %r31)
  ret { i64, i64, <2 x double> } %r32
failure:
  br label %failure
}