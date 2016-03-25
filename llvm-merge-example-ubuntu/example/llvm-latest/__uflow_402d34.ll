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
declare { i64, i64, <2 x double> } @F402ee9(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
define { i64, i64, <2 x double> } @F402d34(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_402d34
block_402d34:
  ; r0 := (alloca 0x20 :: [64])
  %r8 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 402d34: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402d35: mov    rbx,rdi
  ; # 402d38: sub    rsp,0x10
  ; # 402d3c: call   402ee9
  ; r3 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R3 = add i64 %R1, 18446744073709551584
  ; r7 := (bv_add r3 0x8 :: [64])
  %R7 = add i64 %R3, 8
  %r14 = bitcast i128 %r0 to <2 x double>
  %r15 = bitcast i128 %r1 to <2 x double>
  %r16 = bitcast i128 %r2 to <2 x double>
  %r17 = bitcast i128 %r3 to <2 x double>
  %r18 = bitcast i128 %r4 to <2 x double>
  %r19 = bitcast i128 %r5 to <2 x double>
  %r20 = bitcast i128 %r6 to <2 x double>
  %r21 = bitcast i128 %r7 to <2 x double>
  %r22 = call { i64, i64, <2 x double> } @F402ee9(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %r14, <2 x double> %r15, <2 x double> %r16, <2 x double> %r17, <2 x double> %r18, <2 x double> %r19, <2 x double> %r20, <2 x double> %r21)
  %R4 = extractvalue { i64, i64, <2 x double> } %r22, 0
  %R5 = extractvalue { i64, i64, <2 x double> } %r22, 1
  %r25 = extractvalue { i64, i64, <2 x double> } %r22, 2
  %R6 = bitcast <2 x double> %r25 to i128
  br label %block_402d41
block_402d41:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R22 = phi i128 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R21 = phi i128 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R20 = phi i128 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R19 = phi i128 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R18 = phi i128 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R17 = phi i128 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R16 = phi i128 [ undef, %block_402d34 ]
  %R15 = phi i128 [ %R6, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R14 = phi i64 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R13 = phi i64 [ undef, %block_402d34 ]
  %R12 = phi i64 [ %R7, %block_402d34 ]
  %R11 = phi i64 [ %a0, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R10 = phi i64 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R9 = phi i64 [ undef, %block_402d34 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R8 = phi i64 [ undef, %block_402d34 ]
  ; # 402d41: test   eax,eax
  ; r23 := (trunc r8 32)
  %R23 = trunc i64 %R8 to i32
  ; r24 := (bv_eq r23 0x0 :: [32])
  %R24 = icmp eq i32 %R23, 0
  ; # 402d43: je     402d4a
  br i1 %R24, label %subblock_402d41_1, label %subblock_402d41_2
subblock_402d41_1:
  br label %block_402d4a
subblock_402d41_2:
  br label %block_402d45
block_402d45:
  %R26 = phi i128 [ %R51, %subblock_402d5a_1 ], [ %R15, %subblock_402d41_2 ]
  %R25 = phi i64 [ %R49, %subblock_402d5a_1 ], [ %R10, %subblock_402d41_2 ]
  ; # 402d45: or     eax,0xffffffff
  ; # 402d48: jmp    402d64
  br label %block_402d64
block_402d4a:
  %R39 = phi i128 [ %R22, %subblock_402d41_1 ]
  %R38 = phi i128 [ %R21, %subblock_402d41_1 ]
  %R37 = phi i128 [ %R20, %subblock_402d41_1 ]
  %R36 = phi i128 [ %R19, %subblock_402d41_1 ]
  %R35 = phi i128 [ %R18, %subblock_402d41_1 ]
  %R34 = phi i128 [ %R17, %subblock_402d41_1 ]
  %R33 = phi i128 [ %R16, %subblock_402d41_1 ]
  %R32 = phi i128 [ %R15, %subblock_402d41_1 ]
  %R31 = phi i64 [ %R14, %subblock_402d41_1 ]
  %R30 = phi i64 [ %R13, %subblock_402d41_1 ]
  %R29 = phi i64 [ %R12, %subblock_402d41_1 ]
  %R28 = phi i64 [ %R11, %subblock_402d41_1 ]
  %R27 = phi i64 [ %R9, %subblock_402d41_1 ]
  ; # 402d4a: mov    edx,0x1
  ; # 402d4f: lea    rsi,[rsp+0xf]
  ; r40 := (bv_add r29 0xf :: [64])
  %R40 = add i64 %R29, 15
  ; # 402d54: mov    rdi,rbx
  ; # 402d57: call   QWORD PTR [rbx+0x40]
  ; r41 := (bv_add r28 0x40 :: [64])
  %R41 = add i64 %R28, 64
  ; r42 := *r41
  %r61 = inttoptr i64 %R41 to i64*
  %R42 = load i64* %r61
  ; r43 := (bv_add r29 0xfffffffffffffff8 :: [64])
  %R43 = add i64 %R29, 18446744073709551608
  ; r47 := (bv_add r43 0x8 :: [64])
  %R47 = add i64 %R43, 8
  %r65 = inttoptr i64 %R42 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r66 = bitcast i128 %R32 to <2 x double>
  %r67 = bitcast i128 %R33 to <2 x double>
  %r68 = bitcast i128 %R34 to <2 x double>
  %r69 = bitcast i128 %R35 to <2 x double>
  %r70 = bitcast i128 %R36 to <2 x double>
  %r71 = bitcast i128 %R37 to <2 x double>
  %r72 = bitcast i128 %R38 to <2 x double>
  %r73 = bitcast i128 %R39 to <2 x double>
  %r74 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r65(i64 %R28, i64 %R40, i64 1, i64 %R27, i64 %R30, i64 %R31, <2 x double> %r66, <2 x double> %r67, <2 x double> %r68, <2 x double> %r69, <2 x double> %r70, <2 x double> %r71, <2 x double> %r72, <2 x double> %r73)
  %R44 = extractvalue { i64, i64, <2 x double> } %r74, 0
  %R45 = extractvalue { i64, i64, <2 x double> } %r74, 1
  %r77 = extractvalue { i64, i64, <2 x double> } %r74, 2
  %R46 = bitcast <2 x double> %r77 to i128
  br label %block_402d5a
block_402d5a:
  %R51 = phi i128 [ %R46, %block_402d4a ]
  %R50 = phi i64 [ %R47, %block_402d4a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R49 = phi i64 [ undef, %block_402d4a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R48 = phi i64 [ undef, %block_402d4a ]
  ; # 402d5a: dec    rax
  ; r52 := (bv_eq r48 0x1 :: [64])
  %R52 = icmp eq i64 %R48, 1
  ; # 402d5d: jne    402d45
  ; r53 := (bv_complement r52)
  %R53 = xor i1 %R52, -1
  br i1 %R53, label %subblock_402d5a_1, label %subblock_402d5a_2
subblock_402d5a_1:
  br label %block_402d45
subblock_402d5a_2:
  br label %block_402d5f
block_402d5f:
  %R56 = phi i128 [ %R51, %subblock_402d5a_2 ]
  %R55 = phi i64 [ %R50, %subblock_402d5a_2 ]
  %R54 = phi i64 [ %R49, %subblock_402d5a_2 ]
  ; # 402d5f: movzx  eax,BYTE PTR [rsp+0xf]
  ; r57 := (bv_add r55 0xf :: [64])
  %R57 = add i64 %R55, 15
  ; r58 := *r57
  %r89 = inttoptr i64 %R57 to i8*
  %R58 = load i8* %r89
  ; r59 := (uext r58 64)
  %R59 = zext i8 %R58 to i64
  br label %block_402d64
block_402d64:
  %R62 = phi i128 [ %R26, %block_402d45 ], [ %R56, %block_402d5f ]
  %R61 = phi i64 [ %R25, %block_402d45 ], [ %R54, %block_402d5f ]
  %R60 = phi i64 [ 4294967295, %block_402d45 ], [ %R59, %block_402d5f ]
  ; # 402d64: add    rsp,0x10
  ; # 402d68: pop    rbx
  ; # 402d69: ret
  %r95 = bitcast i128 %R62 to <2 x double>
  %r96 = insertvalue { i64, i64, <2 x double> } undef, i64 %R60, 0
  %r97 = insertvalue { i64, i64, <2 x double> } %r96, i64 %R61, 1
  %r98 = insertvalue { i64, i64, <2 x double> } %r97, <2 x double> %r95, 2
  ret { i64, i64, <2 x double> } %r98
failure:
  br label %failure
}