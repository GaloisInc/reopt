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
declare { i64, i64, <2 x double> } @F4021c0(i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4021a0(i64 %a0, i64 %a1, <2 x double> %a2) {
entry:
  %r0 = bitcast <2 x double> %a2 to i128
  br label %block_4021a0
block_4021a0:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 4021a0: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4021a1: mov    ebx,esi
  ; r3 := (trunc arg1 32)
  %R3 = trunc i64 %a1 to i32
  ; r4 := (uext r3 64)
  %R4 = zext i32 %R3 to i64
  ; # 4021a3: call   4021c0
  %r7 = bitcast i128 %r0 to <2 x double>
  %r8 = call { i64, i64, <2 x double> } @F4021c0(i64 %a0, i64 %a1, <2 x double> %r7)
  %R5 = extractvalue { i64, i64, <2 x double> } %r8, 0
  %r10 = extractvalue { i64, i64, <2 x double> } %r8, 2
  %R6 = bitcast <2 x double> %r10 to i128
  br label %block_4021a8
block_4021a8:
  %R9 = phi i128 [ %R6, %block_4021a0 ]
  %R8 = phi i64 [ %R4, %block_4021a0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R7 = phi i64 [ undef, %block_4021a0 ]
  ; # 4021a8: cmp    BYTE PTR [rax],bl
  ; r10 := *r7
  %r15 = inttoptr i64 %R7 to i8*
  %R10 = load i8* %r15
  ; r11 := (trunc r8 8)
  %R11 = trunc i64 %R8 to i8
  ; r12 := (bv_eq r10 r11)
  %R12 = icmp eq i8 %R10, %R11
  ; # 4021aa: mov    edx,0x0
  ; # 4021af: pop    rbx
  ; # 4021b0: cmovne rax,rdx
  ; r13 := (mux r12 r7 0x0 :: [64])
  %R13 = select i1 %R12, i64 %R7, i64 0
  ; # 4021b4: ret
  %r20 = bitcast i128 %R9 to <2 x double>
  %r21 = insertvalue { i64, i64, <2 x double> } undef, i64 %R13, 0
  %r22 = insertvalue { i64, i64, <2 x double> } %r21, i64 0, 1
  %r23 = insertvalue { i64, i64, <2 x double> } %r22, <2 x double> %r20, 2
  ret { i64, i64, <2 x double> } %r23
failure:
  br label %failure
}