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
define { i64, i64, <2 x double> } @F40268f(i64 %a0) {
entry:
  br label %block_40268f
block_40268f:
  ; r0 := (alloca 0x0 :: [64])
  %r0 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 40268f: movsxd rdi,edi
  ; r2 := (trunc arg0 32)
  %R2 = trunc i64 %a0 to i32
  ; r3 := (sext r2 64)
  %R3 = sext i32 %R2 to i64
  ; # 402692: mov    eax,0xe7
  ; # 402697: syscall
  ; sys_exit_group
  %r5 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R3, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 231)
  %R4 = extractvalue { i64, i1 } %r5, 0
  br label %block_402699
block_402699:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R5 = phi i64 [ undef, %block_40268f ]
  ; # 402699: mov    edx,0x3c
  br label %block_40269e
block_40269e:
  %R6 = phi i64 [ %R8, %block_4026a3 ], [ %R5, %block_402699 ]
  ; # 40269e: mov    rax,rdx
  ; # 4026a1: syscall
  ; sys_exit
  %r9 = call { i64, i1 } @reopt.SystemCall.Linux(i64 %R6, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 60)
  %R7 = extractvalue { i64, i1 } %r9, 0
  br label %block_4026a3
block_4026a3:
  ; UNIMPLEMENTED: FnValueUnsupported: post-syscall register rdi
  %R8 = phi i64 [ undef, %block_40269e ]
  ; # 4026a3: jmp    40269e
  br label %block_40269e
failure:
  br label %failure
}