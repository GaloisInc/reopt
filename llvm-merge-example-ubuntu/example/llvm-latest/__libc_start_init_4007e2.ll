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
declare { i64, i64, <2 x double> } @F400120(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F4007e2(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_4007e2
block_4007e2:
  ; r0 := (alloca 0x10 :: [64])
  %r1 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 4007e2: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4007e3: mov    ebx,0x603fe8
  ; # 4007e8: call   400120
  %r5 = bitcast i128 %r0 to <2 x double>
  %r6 = call { i64, i64, <2 x double> } @F400120(i64 %a0, i64 %a1, i64 %a2, <2 x double> %r5)
  %R3 = extractvalue { i64, i64, <2 x double> } %r6, 0
  %R4 = extractvalue { i64, i64, <2 x double> } %r6, 1
  %r9 = extractvalue { i64, i64, <2 x double> } %r6, 2
  %R5 = bitcast <2 x double> %r9 to i128
  br label %block_4007ed
block_4007ed:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R20 = phi i128 [ %R55, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R19 = phi i128 [ %R54, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R18 = phi i128 [ %R53, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R17 = phi i128 [ %R52, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R16 = phi i128 [ %R51, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R15 = phi i128 [ %R50, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R14 = phi i128 [ %R49, %block_4007fa ], [ undef, %block_4007e2 ]
  %R13 = phi i128 [ %R48, %block_4007fa ], [ %R5, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R12 = phi i64 [ %R47, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R11 = phi i64 [ %R46, %block_4007fa ], [ undef, %block_4007e2 ]
  %R10 = phi i64 [ %R45, %block_4007fa ], [ %R3, %block_4007e2 ]
  %R9 = phi i64 [ %R44, %block_4007fa ], [ %R4, %block_4007e2 ]
  %R8 = phi i64 [ %R56, %block_4007fa ], [ 6307816, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R7 = phi i64 [ %R42, %block_4007fa ], [ undef, %block_4007e2 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R6 = phi i64 [ %R41, %block_4007fa ], [ undef, %block_4007e2 ]
  ; # 4007ed: cmp    rbx,0x603ff0
  ; # 4007f4: jae    400800
  ; r21 := (bv_ule data@(603ff0) r8)
  %R21 = icmp ule i64 6307824, %R8
  br i1 %R21, label %subblock_4007ed_1, label %subblock_4007ed_2
subblock_4007ed_1:
  br label %block_400800
subblock_4007ed_2:
  br label %block_4007f6
block_4007f6:
  %R36 = phi i128 [ %R20, %subblock_4007ed_2 ]
  %R35 = phi i128 [ %R19, %subblock_4007ed_2 ]
  %R34 = phi i128 [ %R18, %subblock_4007ed_2 ]
  %R33 = phi i128 [ %R17, %subblock_4007ed_2 ]
  %R32 = phi i128 [ %R16, %subblock_4007ed_2 ]
  %R31 = phi i128 [ %R15, %subblock_4007ed_2 ]
  %R30 = phi i128 [ %R14, %subblock_4007ed_2 ]
  %R29 = phi i128 [ %R13, %subblock_4007ed_2 ]
  %R28 = phi i64 [ %R12, %subblock_4007ed_2 ]
  %R27 = phi i64 [ %R11, %subblock_4007ed_2 ]
  %R26 = phi i64 [ %R10, %subblock_4007ed_2 ]
  %R25 = phi i64 [ %R9, %subblock_4007ed_2 ]
  %R24 = phi i64 [ %R8, %subblock_4007ed_2 ]
  %R23 = phi i64 [ %R7, %subblock_4007ed_2 ]
  %R22 = phi i64 [ %R6, %subblock_4007ed_2 ]
  ; # 4007f6: xor    eax,eax
  ; # 4007f8: call   QWORD PTR [rbx]
  ; r37 := *r24
  %r42 = inttoptr i64 %R24 to i64*
  %R37 = load i64* %r42
  %r44 = inttoptr i64 %R37 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r45 = bitcast i128 %R29 to <2 x double>
  %r46 = bitcast i128 %R30 to <2 x double>
  %r47 = bitcast i128 %R31 to <2 x double>
  %r48 = bitcast i128 %R32 to <2 x double>
  %r49 = bitcast i128 %R33 to <2 x double>
  %r50 = bitcast i128 %R34 to <2 x double>
  %r51 = bitcast i128 %R35 to <2 x double>
  %r52 = bitcast i128 %R36 to <2 x double>
  %r53 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r44(i64 %R26, i64 %R25, i64 %R23, i64 %R22, i64 %R27, i64 %R28, <2 x double> %r45, <2 x double> %r46, <2 x double> %r47, <2 x double> %r48, <2 x double> %r49, <2 x double> %r50, <2 x double> %r51, <2 x double> %r52)
  %R38 = extractvalue { i64, i64, <2 x double> } %r53, 0
  %R39 = extractvalue { i64, i64, <2 x double> } %r53, 1
  %r56 = extractvalue { i64, i64, <2 x double> } %r53, 2
  %R40 = bitcast <2 x double> %r56 to i128
  br label %block_4007fa
block_4007fa:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R55 = phi i128 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R54 = phi i128 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R53 = phi i128 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R52 = phi i128 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R51 = phi i128 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R50 = phi i128 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R49 = phi i128 [ undef, %block_4007f6 ]
  %R48 = phi i128 [ %R40, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R47 = phi i64 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R46 = phi i64 [ undef, %block_4007f6 ]
  %R45 = phi i64 [ %R38, %block_4007f6 ]
  %R44 = phi i64 [ %R39, %block_4007f6 ]
  %R43 = phi i64 [ %R24, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R42 = phi i64 [ undef, %block_4007f6 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R41 = phi i64 [ undef, %block_4007f6 ]
  ; # 4007fa: add    rbx,0x8
  ; r56 := (bv_add r43 0x8 :: [64])
  %R56 = add i64 %R43, 8
  ; # 4007fe: jmp    4007ed
  br label %block_4007ed
block_400800:
  %R57 = phi i128 [ %R13, %subblock_4007ed_1 ]
  ; # 400800: pop    rbx
  ; # 400801: ret
  %r75 = bitcast i128 %R57 to <2 x double>
  %r76 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r77 = insertvalue { i64, i64, <2 x double> } %r76, i64 undef, 1
  %r78 = insertvalue { i64, i64, <2 x double> } %r77, <2 x double> %r75, 2
  ret { i64, i64, <2 x double> } %r78
failure:
  br label %failure
}