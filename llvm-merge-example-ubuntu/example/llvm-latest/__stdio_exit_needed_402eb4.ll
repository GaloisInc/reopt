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
declare { i64, i64, <2 x double> } @F40217e(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F402e67(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
define { i64, i64, <2 x double> } @F402eb4(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_402eb4
block_402eb4:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 402eb4: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 402eb5: call   40217e
  %r5 = bitcast i128 %r0 to <2 x double>
  %r6 = call { i64, i64, <2 x double> } @F40217e(i64 %a0, i64 %a1, i64 %a2, <2 x double> %r5)
  %R3 = extractvalue { i64, i64, <2 x double> } %r6, 0
  %R4 = extractvalue { i64, i64, <2 x double> } %r6, 1
  %r9 = extractvalue { i64, i64, <2 x double> } %r6, 2
  %R5 = bitcast <2 x double> %r9 to i128
  br label %block_402eba
block_402eba:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R19 = phi i128 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R18 = phi i128 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R17 = phi i128 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R16 = phi i128 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R15 = phi i128 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R14 = phi i128 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R13 = phi i128 [ undef, %block_402eb4 ]
  %R12 = phi i128 [ %R5, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R11 = phi i64 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R10 = phi i64 [ undef, %block_402eb4 ]
  %R9 = phi i64 [ %R4, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R8 = phi i64 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R7 = phi i64 [ undef, %block_402eb4 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R6 = phi i64 [ undef, %block_402eb4 ]
  ; # 402eba: mov    rbx,QWORD PTR [rax]
  ; r20 := *r6
  %r25 = inttoptr i64 %R6 to i64*
  %R20 = load i64* %r25
  br label %block_402ebd
block_402ebd:
  %R34 = phi i128 [ %R64, %block_402eca ], [ %R19, %block_402eba ]
  %R33 = phi i128 [ %R63, %block_402eca ], [ %R18, %block_402eba ]
  %R32 = phi i128 [ %R62, %block_402eca ], [ %R17, %block_402eba ]
  %R31 = phi i128 [ %R61, %block_402eca ], [ %R16, %block_402eba ]
  %R30 = phi i128 [ %R60, %block_402eca ], [ %R15, %block_402eba ]
  %R29 = phi i128 [ %R59, %block_402eca ], [ %R14, %block_402eba ]
  %R28 = phi i128 [ %R58, %block_402eca ], [ %R13, %block_402eba ]
  %R27 = phi i128 [ %R57, %block_402eca ], [ %R12, %block_402eba ]
  %R26 = phi i64 [ %R56, %block_402eca ], [ %R11, %block_402eba ]
  %R25 = phi i64 [ %R55, %block_402eca ], [ %R10, %block_402eba ]
  %R24 = phi i64 [ %R54, %block_402eca ], [ %R9, %block_402eba ]
  %R23 = phi i64 [ %R66, %block_402eca ], [ %R20, %block_402eba ]
  %R22 = phi i64 [ %R52, %block_402eca ], [ %R8, %block_402eba ]
  %R21 = phi i64 [ %R51, %block_402eca ], [ %R7, %block_402eba ]
  ; # 402ebd: test   rbx,rbx
  ; r35 := (bv_eq r23 0x0 :: [64])
  %R35 = icmp eq i64 %R23, 0
  ; # 402ec0: je     402ed0
  br i1 %R35, label %subblock_402ebd_1, label %subblock_402ebd_2
subblock_402ebd_1:
  br label %block_402ed0
subblock_402ebd_2:
  br label %block_402ec2
block_402ec2:
  %R49 = phi i128 [ %R34, %subblock_402ebd_2 ]
  %R48 = phi i128 [ %R33, %subblock_402ebd_2 ]
  %R47 = phi i128 [ %R32, %subblock_402ebd_2 ]
  %R46 = phi i128 [ %R31, %subblock_402ebd_2 ]
  %R45 = phi i128 [ %R30, %subblock_402ebd_2 ]
  %R44 = phi i128 [ %R29, %subblock_402ebd_2 ]
  %R43 = phi i128 [ %R28, %subblock_402ebd_2 ]
  %R42 = phi i128 [ %R27, %subblock_402ebd_2 ]
  %R41 = phi i64 [ %R26, %subblock_402ebd_2 ]
  %R40 = phi i64 [ %R25, %subblock_402ebd_2 ]
  %R39 = phi i64 [ %R24, %subblock_402ebd_2 ]
  %R38 = phi i64 [ %R23, %subblock_402ebd_2 ]
  %R37 = phi i64 [ %R22, %subblock_402ebd_2 ]
  %R36 = phi i64 [ %R21, %subblock_402ebd_2 ]
  ; # 402ec2: mov    rdi,rbx
  ; # 402ec5: call   402e67
  %r56 = bitcast i128 %R42 to <2 x double>
  %r57 = bitcast i128 %R43 to <2 x double>
  %r58 = bitcast i128 %R44 to <2 x double>
  %r59 = bitcast i128 %R45 to <2 x double>
  %r60 = bitcast i128 %R46 to <2 x double>
  %r61 = bitcast i128 %R47 to <2 x double>
  %r62 = bitcast i128 %R48 to <2 x double>
  %r63 = bitcast i128 %R49 to <2 x double>
  %r64 = call { i64, i64, <2 x double> } @F402e67(i64 %R38, i64 %R39, i64 %R37, i64 %R36, i64 %R40, i64 %R41, <2 x double> %r56, <2 x double> %r57, <2 x double> %r58, <2 x double> %r59, <2 x double> %r60, <2 x double> %r61, <2 x double> %r62, <2 x double> %r63)
  %r65 = extractvalue { i64, i64, <2 x double> } %r64, 2
  %R50 = bitcast <2 x double> %r65 to i128
  br label %block_402eca
block_402eca:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R64 = phi i128 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R63 = phi i128 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R62 = phi i128 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R61 = phi i128 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R60 = phi i128 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R59 = phi i128 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R58 = phi i128 [ undef, %block_402ec2 ]
  %R57 = phi i128 [ %R50, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R56 = phi i64 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R55 = phi i64 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R54 = phi i64 [ undef, %block_402ec2 ]
  %R53 = phi i64 [ %R38, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R52 = phi i64 [ undef, %block_402ec2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R51 = phi i64 [ undef, %block_402ec2 ]
  ; # 402eca: mov    rbx,QWORD PTR [rbx+0x70]
  ; r65 := (bv_add r53 0x70 :: [64])
  %R65 = add i64 %R53, 112
  ; r66 := *r65
  %r82 = inttoptr i64 %R65 to i64*
  %R66 = load i64* %r82
  ; # 402ece: jmp    402ebd
  br label %block_402ebd
block_402ed0:
  %R79 = phi i128 [ %R34, %subblock_402ebd_1 ]
  %R78 = phi i128 [ %R33, %subblock_402ebd_1 ]
  %R77 = phi i128 [ %R32, %subblock_402ebd_1 ]
  %R76 = phi i128 [ %R31, %subblock_402ebd_1 ]
  %R75 = phi i128 [ %R30, %subblock_402ebd_1 ]
  %R74 = phi i128 [ %R29, %subblock_402ebd_1 ]
  %R73 = phi i128 [ %R28, %subblock_402ebd_1 ]
  %R72 = phi i128 [ %R27, %subblock_402ebd_1 ]
  %R71 = phi i64 [ %R26, %subblock_402ebd_1 ]
  %R70 = phi i64 [ %R25, %subblock_402ebd_1 ]
  %R69 = phi i64 [ %R24, %subblock_402ebd_1 ]
  %R68 = phi i64 [ %R22, %subblock_402ebd_1 ]
  %R67 = phi i64 [ %R21, %subblock_402ebd_1 ]
  ; # 402ed0: mov    rdi,QWORD PTR [rip+0x201a71]
  ; r80 := *0x604948 :: [64]
  %r97 = inttoptr i64 6310216 to i64*
  %R80 = load i64* %r97
  ; # 402ed7: call   402e67
  %r99 = bitcast i128 %R72 to <2 x double>
  %r100 = bitcast i128 %R73 to <2 x double>
  %r101 = bitcast i128 %R74 to <2 x double>
  %r102 = bitcast i128 %R75 to <2 x double>
  %r103 = bitcast i128 %R76 to <2 x double>
  %r104 = bitcast i128 %R77 to <2 x double>
  %r105 = bitcast i128 %R78 to <2 x double>
  %r106 = bitcast i128 %R79 to <2 x double>
  %r107 = call { i64, i64, <2 x double> } @F402e67(i64 %R80, i64 %R69, i64 %R68, i64 %R67, i64 %R70, i64 %R71, <2 x double> %r99, <2 x double> %r100, <2 x double> %r101, <2 x double> %r102, <2 x double> %r103, <2 x double> %r104, <2 x double> %r105, <2 x double> %r106)
  %r108 = extractvalue { i64, i64, <2 x double> } %r107, 2
  %R81 = bitcast <2 x double> %r108 to i128
  br label %block_402edc
block_402edc:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R94 = phi i128 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R93 = phi i128 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R92 = phi i128 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R91 = phi i128 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R90 = phi i128 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R89 = phi i128 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R88 = phi i128 [ undef, %block_402ed0 ]
  %R87 = phi i128 [ %R81, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R86 = phi i64 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R85 = phi i64 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rsi
  %R84 = phi i64 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R83 = phi i64 [ undef, %block_402ed0 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R82 = phi i64 [ undef, %block_402ed0 ]
  ; # 402edc: pop    rbx
  ; # 402edd: mov    rdi,QWORD PTR [rip+0x20180c]
  ; r95 := *0x6046f0 :: [64]
  %r123 = inttoptr i64 6309616 to i64*
  %R95 = load i64* %r123
  ; # 402ee4: jmp    402e67
  %r125 = bitcast i128 %R87 to <2 x double>
  %r126 = bitcast i128 %R88 to <2 x double>
  %r127 = bitcast i128 %R89 to <2 x double>
  %r128 = bitcast i128 %R90 to <2 x double>
  %r129 = bitcast i128 %R91 to <2 x double>
  %r130 = bitcast i128 %R92 to <2 x double>
  %r131 = bitcast i128 %R93 to <2 x double>
  %r132 = bitcast i128 %R94 to <2 x double>
  %r133 = call { i64, i64, <2 x double> } @F402e67(i64 %R95, i64 %R84, i64 %R83, i64 %R82, i64 %R85, i64 %R86, <2 x double> %r125, <2 x double> %r126, <2 x double> %r127, <2 x double> %r128, <2 x double> %r129, <2 x double> %r130, <2 x double> %r131, <2 x double> %r132)
  ret { i64, i64, <2 x double> } %r133
failure:
  br label %failure
}