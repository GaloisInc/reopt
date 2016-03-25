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
declare { i64, i64, <2 x double> } @F402681(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4026b0(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_4026b0
block_4026b0:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 4026b0: cmp    rdi,0xfffffffffffff000
  ; # 4026b7: ja     4026c0
  ; r2 := (bv_ult 0xfffffffffffff000 :: [64] arg0)
  %R2 = icmp ult i64 18446744073709547520, %a0
  br i1 %R2, label %subblock_4026b0_1, label %subblock_4026b0_2
subblock_4026b0_1:
  br label %block_4026c0
subblock_4026b0_2:
  br label %block_4026b9
block_4026b9:
  %R5 = phi i128 [ %r0, %subblock_4026b0_2 ]
  %R4 = phi i64 [ %a0, %subblock_4026b0_2 ]
  %R3 = phi i64 [ %a2, %subblock_4026b0_2 ]
  ; # 4026b9: mov    rax,rdi
  ; # 4026bc: ret
  %r8 = bitcast i128 %R5 to <2 x double>
  %r9 = insertvalue { i64, i64, <2 x double> } undef, i64 %R4, 0
  %r10 = insertvalue { i64, i64, <2 x double> } %r9, i64 %R3, 1
  %r11 = insertvalue { i64, i64, <2 x double> } %r10, <2 x double> %r8, 2
  ret { i64, i64, <2 x double> } %r11
block_4026c0:
  %R11 = phi i128 [ %r0, %subblock_4026b0_1 ]
  %R10 = phi i64 [ %a0, %subblock_4026b0_1 ]
  %R9 = phi i64 [ %a1, %subblock_4026b0_1 ]
  %R8 = phi i64 [ %R1, %subblock_4026b0_1 ]
  ; UNIMPLEMENTED: FnValueUnsupported: Initial (callee) register rbx
  %R7 = phi i64 [ undef, %subblock_4026b0_1 ]
  %R6 = phi i64 [ %a2, %subblock_4026b0_1 ]
  ; # 4026c0: push   rbx
  ; r12 := (bv_add r8 0xfffffffffffffff8 :: [64])
  %R12 = add i64 %R8, 18446744073709551608
  ; *(r12) = r7
  %r19 = inttoptr i64 %R12 to i64*
  store i64 %R7, i64* %r19
  ; # 4026c1: mov    rbx,rdi
  ; # 4026c4: call   402681
  %r20 = bitcast i128 %R11 to <2 x double>
  %r21 = call { i64, i64, <2 x double> } @F402681(i64 %R10, i64 %R9, i64 %R6, <2 x double> %r20)
  %R13 = extractvalue { i64, i64, <2 x double> } %r21, 0
  %R14 = extractvalue { i64, i64, <2 x double> } %r21, 1
  %r24 = extractvalue { i64, i64, <2 x double> } %r21, 2
  %R15 = bitcast <2 x double> %r24 to i128
  br label %block_4026c9
block_4026c9:
  %R19 = phi i128 [ %R15, %block_4026c0 ]
  %R18 = phi i64 [ %R10, %block_4026c0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R17 = phi i64 [ undef, %block_4026c0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R16 = phi i64 [ undef, %block_4026c0 ]
  ; # 4026c9: mov    edi,ebx
  ; r20 := (trunc r18 32)
  %R20 = trunc i64 %R18 to i32
  ; # 4026cb: neg    edi
  ; r21 := (bv_sub 0x0 :: [32] r20)
  %R21 = sub i32 0, %R20
  ; # 4026cd: mov    DWORD PTR [rax],edi
  ; *(r16) = r21
  %r32 = inttoptr i64 %R16 to i32*
  store i32 %R21, i32* %r32
  ; # 4026cf: mov    rax,0xffffffffffffffff
  ; # 4026d6: pop    rbx
  ; # 4026d7: ret
  %r33 = bitcast i128 %R19 to <2 x double>
  %r34 = insertvalue { i64, i64, <2 x double> } undef, i64 18446744073709551615, 0
  %r35 = insertvalue { i64, i64, <2 x double> } %r34, i64 %R17, 1
  %r36 = insertvalue { i64, i64, <2 x double> } %r35, <2 x double> %r33, 2
  ret { i64, i64, <2 x double> } %r36
failure:
  br label %failure
}