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
declare { i64, i64, <2 x double> } @F402f54(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F40083d(i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, <2 x double> %a6, <2 x double> %a7, <2 x double> %a8, <2 x double> %a9, <2 x double> %a10, <2 x double> %a11, <2 x double> %a12, <2 x double> %a13) {
entry:
  %r0 = bitcast <2 x double> %a6 to i128
  %r1 = bitcast <2 x double> %a7 to i128
  %r2 = bitcast <2 x double> %a8 to i128
  %r3 = bitcast <2 x double> %a9 to i128
  %r4 = bitcast <2 x double> %a10 to i128
  %r5 = bitcast <2 x double> %a11 to i128
  %r6 = bitcast <2 x double> %a12 to i128
  %r7 = bitcast <2 x double> %a13 to i128
  br label %block_40083d
block_40083d:
  ; r0 := (alloca 0x10 :: [64])
  %r8 = alloca i8, i64 16
  %R0 = ptrtoint i8* %r8 to i64
  ; r1 := (bv_add r0 0x10 :: [64])
  %R1 = add i64 %R0, 16
  ; # 40083d: push   rbx
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 40083e: mov    ebx,0x603ff8
  br label %block_400843
block_400843:
  %R17 = phi i128 [ %R53, %block_400854 ], [ %r7, %block_40083d ]
  %R16 = phi i128 [ %R52, %block_400854 ], [ %r6, %block_40083d ]
  %R15 = phi i128 [ %R51, %block_400854 ], [ %r5, %block_40083d ]
  %R14 = phi i128 [ %R50, %block_400854 ], [ %r4, %block_40083d ]
  %R13 = phi i128 [ %R49, %block_400854 ], [ %r3, %block_40083d ]
  %R12 = phi i128 [ %R48, %block_400854 ], [ %r2, %block_40083d ]
  %R11 = phi i128 [ %R47, %block_400854 ], [ %r1, %block_40083d ]
  %R10 = phi i128 [ %R46, %block_400854 ], [ %r0, %block_40083d ]
  %R9 = phi i64 [ %R45, %block_400854 ], [ %a5, %block_40083d ]
  %R8 = phi i64 [ %R44, %block_400854 ], [ %a4, %block_40083d ]
  %R7 = phi i64 [ %R43, %block_400854 ], [ %a0, %block_40083d ]
  %R6 = phi i64 [ %R42, %block_400854 ], [ %a1, %block_40083d ]
  %R5 = phi i64 [ %R41, %block_400854 ], [ 6307832, %block_40083d ]
  %R4 = phi i64 [ %R40, %block_400854 ], [ %a2, %block_40083d ]
  %R3 = phi i64 [ %R39, %block_400854 ], [ %a3, %block_40083d ]
  ; # 400843: cmp    rbx,0x603ff0
  ; # 40084a: jbe    400856
  ; r18 := (bv_ule r5 data@(603ff0))
  %R18 = icmp ule i64 %R5, 6307824
  br i1 %R18, label %subblock_400843_1, label %subblock_400843_2
subblock_400843_1:
  br label %block_400856
subblock_400843_2:
  br label %block_40084c
block_40084c:
  %R33 = phi i128 [ %R17, %subblock_400843_2 ]
  %R32 = phi i128 [ %R16, %subblock_400843_2 ]
  %R31 = phi i128 [ %R15, %subblock_400843_2 ]
  %R30 = phi i128 [ %R14, %subblock_400843_2 ]
  %R29 = phi i128 [ %R13, %subblock_400843_2 ]
  %R28 = phi i128 [ %R12, %subblock_400843_2 ]
  %R27 = phi i128 [ %R11, %subblock_400843_2 ]
  %R26 = phi i128 [ %R10, %subblock_400843_2 ]
  %R25 = phi i64 [ %R9, %subblock_400843_2 ]
  %R24 = phi i64 [ %R8, %subblock_400843_2 ]
  %R23 = phi i64 [ %R7, %subblock_400843_2 ]
  %R22 = phi i64 [ %R6, %subblock_400843_2 ]
  %R21 = phi i64 [ %R5, %subblock_400843_2 ]
  %R20 = phi i64 [ %R4, %subblock_400843_2 ]
  %R19 = phi i64 [ %R3, %subblock_400843_2 ]
  ; # 40084c: sub    rbx,0x8
  ; r34 := (bv_add r21 0xfffffffffffffff8 :: [64])
  %R34 = add i64 %R21, 18446744073709551608
  ; # 400850: xor    eax,eax
  ; # 400852: call   QWORD PTR [rbx]
  ; r35 := *r34
  %r44 = inttoptr i64 %R34 to i64*
  %R35 = load i64* %r44
  %r46 = inttoptr i64 %R35 to { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)*
  %r47 = bitcast i128 %R26 to <2 x double>
  %r48 = bitcast i128 %R27 to <2 x double>
  %r49 = bitcast i128 %R28 to <2 x double>
  %r50 = bitcast i128 %R29 to <2 x double>
  %r51 = bitcast i128 %R30 to <2 x double>
  %r52 = bitcast i128 %R31 to <2 x double>
  %r53 = bitcast i128 %R32 to <2 x double>
  %r54 = bitcast i128 %R33 to <2 x double>
  %r55 = call { i64, i64, <2 x double> }(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)* %r46(i64 %R23, i64 %R22, i64 %R20, i64 %R19, i64 %R24, i64 %R25, <2 x double> %r47, <2 x double> %r48, <2 x double> %r49, <2 x double> %r50, <2 x double> %r51, <2 x double> %r52, <2 x double> %r53, <2 x double> %r54)
  %R36 = extractvalue { i64, i64, <2 x double> } %r55, 0
  %R37 = extractvalue { i64, i64, <2 x double> } %r55, 1
  %r58 = extractvalue { i64, i64, <2 x double> } %r55, 2
  %R38 = bitcast <2 x double> %r58 to i128
  br label %block_400854
block_400854:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R53 = phi i128 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R52 = phi i128 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R51 = phi i128 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R50 = phi i128 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R49 = phi i128 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R48 = phi i128 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R47 = phi i128 [ undef, %block_40084c ]
  %R46 = phi i128 [ %R38, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R45 = phi i64 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R44 = phi i64 [ undef, %block_40084c ]
  %R43 = phi i64 [ %R36, %block_40084c ]
  %R42 = phi i64 [ %R37, %block_40084c ]
  %R41 = phi i64 [ %R34, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R40 = phi i64 [ undef, %block_40084c ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R39 = phi i64 [ undef, %block_40084c ]
  ; # 400854: jmp    400843
  br label %block_400843
block_400856:
  %R57 = phi i128 [ %R10, %subblock_400843_1 ]
  %R56 = phi i64 [ %R7, %subblock_400843_1 ]
  %R55 = phi i64 [ %R6, %subblock_400843_1 ]
  %R54 = phi i64 [ %R4, %subblock_400843_1 ]
  ; # 400856: pop    rbx
  ; # 400857: xor    eax,eax
  ; # 400859: jmp    402f54
  %r79 = bitcast i128 %R57 to <2 x double>
  %r80 = call { i64, i64, <2 x double> } @F402f54(i64 %R56, i64 %R55, i64 %R54, <2 x double> %r79)
  ret { i64, i64, <2 x double> } %r80
failure:
  br label %failure
}