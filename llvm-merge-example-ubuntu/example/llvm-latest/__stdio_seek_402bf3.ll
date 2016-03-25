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
declare { i64, i64, <2 x double> } @F4026b0(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F402bf3(i64 %a0, i64 %a1, i64 %a2) {
entry:
  br label %block_402bf3
block_402bf3:
  ; r0 := (alloca 0x0 :: [64])
  %r0 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 402bf3: movsxd rdi,DWORD PTR [rdi+0x78]
  ; r2 := (bv_add arg0 0x78 :: [64])
  %R2 = add i64 %a0, 120
  ; r3 := *r2
  %r4 = inttoptr i64 %R2 to i32*
  %R3 = load i32* %r4
  ; r4 := (sext r3 64)
  %R4 = sext i32 %R3 to i64
  ; # 402bf7: movsxd rdx,edx
  ; r5 := (trunc arg2 32)
  %R5 = trunc i64 %a2 to i32
  ; r6 := (sext r5 64)
  %R6 = sext i32 %R5 to i64
  ; # 402bfa: mov    eax,0x8
  ; # 402bff: syscall
  ; sys_lseek
  %r9 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R4, i64 %a1, i64 %R6, i64 undef, i64 undef, i64 undef, i64 8)
  %R7 = extractvalue { i64, i1 } %r9, 0
  br label %block_402c01
block_402c01:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register xmm0
  %R11 = phi i128 [ undef, %block_402bf3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rsi
  %R10 = phi i64 [ undef, %block_402bf3 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdx
  %R9 = phi i64 [ undef, %block_402bf3 ]
  %R8 = phi i64 [ %R7, %block_402bf3 ]
  ; # 402c01: mov    rdi,rax
  ; # 402c04: jmp    4026b0
  %r15 = bitcast i128 %R11 to <2 x double>
  %r16 = call { i64, i64, <2 x double> } @F4026b0(i64 %R8, i64 %R10, i64 %R9, <2 x double> %r15)
  ret { i64, i64, <2 x double> } %r16
failure:
  br label %failure
}