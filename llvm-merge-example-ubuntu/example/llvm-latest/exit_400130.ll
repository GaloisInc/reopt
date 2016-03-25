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
declare { i64, i64, <2 x double> } @F40083c(i64, i64, i64, <2 x double>)
declare { i64, i64, <2 x double> } @F40083d(i64, i64, i64, i64, i64, i64, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>, <2 x double>)
declare { i64, i64, <2 x double> } @F40268f(i64)
declare { i64, i64, <2 x double> } @F402eb4(i64, i64, i64, <2 x double>)
define { i64, i64, <2 x double> } @F400130(i64 %a0, i64 %a1, i64 %a2, <2 x double> %a3) {
entry:
  %r0 = bitcast <2 x double> %a3 to i128
  br label %block_400130
block_400130:
  ; r0 := (alloca 0x20 :: [64])
  %r1 = alloca i8, i64 32
  %R0 = ptrtoint i8* %r1 to i64
  ; r1 := (bv_add r0 0x20 :: [64])
  %R1 = add i64 %R0, 32
  ; # 400130: sub    rsp,0x18
  ; # 400134: xor    eax,eax
  ; # 400136: mov    DWORD PTR [rsp+0xc],edi
  ; r2 := (trunc arg0 32)
  %R2 = trunc i64 %a0 to i32
  ; r3 := (bv_add r1 0xfffffffffffffff4 :: [64])
  %R3 = add i64 %R1, 18446744073709551604
  ; *(r3) = r2
  %r6 = inttoptr i64 %R3 to i32*
  store i32 %R2, i32* %r6
  ; # 40013a: call   40083c
  ; r4 := (bv_add r1 0xffffffffffffffe0 :: [64])
  %R4 = add i64 %R1, 18446744073709551584
  ; r8 := (bv_add r4 0x8 :: [64])
  %R8 = add i64 %R4, 8
  %r9 = bitcast i128 %r0 to <2 x double>
  %r10 = call { i64, i64, <2 x double> } @F40083c(i64 %a0, i64 %a1, i64 %a2, <2 x double> %r9)
  %R5 = extractvalue { i64, i64, <2 x double> } %r10, 0
  %R6 = extractvalue { i64, i64, <2 x double> } %r10, 1
  %r13 = extractvalue { i64, i64, <2 x double> } %r10, 2
  %R7 = bitcast <2 x double> %r13 to i128
  br label %block_40013f
block_40013f:
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm7
  %R23 = phi i128 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm6
  %R22 = phi i128 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm5
  %R21 = phi i128 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm4
  %R20 = phi i128 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm3
  %R19 = phi i128 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm2
  %R18 = phi i128 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register xmm1
  %R17 = phi i128 [ undef, %block_400130 ]
  %R16 = phi i128 [ %R7, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r9
  %R15 = phi i64 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register r8
  %R14 = phi i64 [ undef, %block_400130 ]
  %R13 = phi i64 [ %R5, %block_400130 ]
  %R12 = phi i64 [ %R6, %block_400130 ]
  %R11 = phi i64 [ %R8, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R10 = phi i64 [ undef, %block_400130 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rcx
  %R9 = phi i64 [ undef, %block_400130 ]
  ; # 40013f: call   40083d
  ; r24 := (bv_add r11 0xfffffffffffffff8 :: [64])
  %R24 = add i64 %R11, 18446744073709551608
  ; r28 := (bv_add r24 0x8 :: [64])
  %R28 = add i64 %R24, 8
  %r32 = bitcast i128 %R16 to <2 x double>
  %r33 = bitcast i128 %R17 to <2 x double>
  %r34 = bitcast i128 %R18 to <2 x double>
  %r35 = bitcast i128 %R19 to <2 x double>
  %r36 = bitcast i128 %R20 to <2 x double>
  %r37 = bitcast i128 %R21 to <2 x double>
  %r38 = bitcast i128 %R22 to <2 x double>
  %r39 = bitcast i128 %R23 to <2 x double>
  %r40 = call { i64, i64, <2 x double> } @F40083d(i64 %R13, i64 %R12, i64 %R10, i64 %R9, i64 %R14, i64 %R15, <2 x double> %r32, <2 x double> %r33, <2 x double> %r34, <2 x double> %r35, <2 x double> %r36, <2 x double> %r37, <2 x double> %r38, <2 x double> %r39)
  %R25 = extractvalue { i64, i64, <2 x double> } %r40, 0
  %R26 = extractvalue { i64, i64, <2 x double> } %r40, 1
  %r43 = extractvalue { i64, i64, <2 x double> } %r40, 2
  %R27 = bitcast <2 x double> %r43 to i128
  br label %block_400144
block_400144:
  %R33 = phi i128 [ %R27, %block_40013f ]
  %R32 = phi i64 [ %R25, %block_40013f ]
  %R31 = phi i64 [ %R26, %block_40013f ]
  %R30 = phi i64 [ %R28, %block_40013f ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R29 = phi i64 [ undef, %block_40013f ]
  ; # 400144: xor    eax,eax
  ; # 400146: call   402eb4
  ; r34 := (bv_add r30 0xfffffffffffffff8 :: [64])
  %R34 = add i64 %R30, 18446744073709551608
  ; r35 := (bv_add r34 0x8 :: [64])
  %R35 = add i64 %R34, 8
  %r52 = bitcast i128 %R33 to <2 x double>
  %r53 = call { i64, i64, <2 x double> } @F402eb4(i64 %R32, i64 %R31, i64 %R29, <2 x double> %r52)
  br label %block_40014b
block_40014b:
  %R36 = phi i64 [ %R35, %block_400144 ]
  ; # 40014b: mov    edi,DWORD PTR [rsp+0xc]
  ; r37 := (bv_add r36 0xc :: [64])
  %R37 = add i64 %R36, 12
  ; r38 := *r37
  %r56 = inttoptr i64 %R37 to i32*
  %R38 = load i32* %r56
  ; r39 := (uext r38 64)
  %R39 = zext i32 %R38 to i64
  ; # 40014f: call   40268f
  ; r40 := (bv_add r36 0xfffffffffffffff8 :: [64])
  %R40 = add i64 %R36, 18446744073709551608
  ; r43 := (bv_add r40 0x8 :: [64])
  %R43 = add i64 %R40, 8
  %r61 = call { i64, i64, <2 x double> } @F40268f(i64 %R39)
  %R41 = extractvalue { i64, i64, <2 x double> } %r61, 0
  %r63 = extractvalue { i64, i64, <2 x double> } %r61, 2
  %R42 = bitcast <2 x double> %r63 to i128
  br label %block_400154
block_400154:
  %R45 = phi i128 [ %R42, %block_40014b ]
  %R44 = phi i64 [ %R43, %block_40014b ]
  ; # 400154: xor    rbp,rbp
  ; # 400157: mov    rdi,rsp
  ; # 40015a: lea    rsi,[rip+0xffffffffffbffe9f]
  ; # 400161: and    rsp,0xfffffffffffffff0
  ; r46 := (bv_and r44 0xfffffffffffffff0 :: [64])
  %R46 = and i64 %R44, 18446744073709551600
  ; # 400165: call   40016a
  ; r47 := (bv_add r46 0xfffffffffffffff8 :: [64])
  %R47 = add i64 %R46, 18446744073709551608
  ; r50 := (bv_add r47 0x8 :: [64])
  %R50 = add i64 %R47, 8
  %r70 = bitcast i128 %R45 to <2 x double>
  %r71 = call { i64, i64, <2 x double> } @F40016a(i64 %R44, <2 x double> %r70)
  %R48 = extractvalue { i64, i64, <2 x double> } %r71, 0
  %r73 = extractvalue { i64, i64, <2 x double> } %r71, 2
  %R49 = bitcast <2 x double> %r73 to i128
  br label %block_40016a
block_40016a:
  %R54 = phi i128 [ %R49, %block_400154 ]
  %R53 = phi i64 [ %R48, %block_400154 ]
  %R52 = phi i64 [ %R50, %block_400154 ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rax
  %R51 = phi i64 [ undef, %block_400154 ]
  ; # 40016a: push   rax
  ; r55 := (bv_add r52 0xfffffffffffffff8 :: [64])
  %R55 = add i64 %R52, 18446744073709551608
  ; *(r55) = r51
  %r80 = inttoptr i64 %R55 to i64*
  store i64 %R51, i64* %r80
  ; # 40016b: mov    rsi,QWORD PTR [rdi]
  ; r56 := *r53
  %r81 = inttoptr i64 %R53 to i64*
  %R56 = load i64* %r81
  ; # 40016e: lea    rdx,[rdi+0x8]
  ; r57 := (bv_add r53 0x8 :: [64])
  %R57 = add i64 %R53, 8
  ; # 400172: xor    r9d,r9d
  ; # 400175: mov    r8d,0x402f54
  ; # 40017b: mov    ecx,0x400120
  ; # 400180: mov    edi,0x400603
  ; # 400185: call   400802
  %r84 = bitcast i128 %R54 to <2 x double>
  %r85 = call { i64, i64, <2 x double> } @F400802(i64 ptrtoint ({ i64, i64, <2 x double> }(i64, i64, i64, <2 x double>)* @F400603 to i64), i64 %R56, i64 %R57, <2 x double> %r84)
  %R58 = extractvalue { i64, i64, <2 x double> } %r85, 0
  %R59 = extractvalue { i64, i64, <2 x double> } %r85, 1
  %r88 = extractvalue { i64, i64, <2 x double> } %r85, 2
  %R60 = bitcast <2 x double> %r88 to i128
  br label %block_40018a
block_40018a:
  %R64 = phi i128 [ %R60, %block_40016a ]
  %R63 = phi i64 [ %R58, %block_40016a ]
  %R62 = phi i64 [ %R59, %block_40016a ]
  ; UNIMPLEMENTED: FnValueUnsupported: post-call register rdx
  %R61 = phi i64 [ undef, %block_40016a ]
  ; # 40018a: nop    [rax+rax*1]
  %r94 = bitcast i128 %R64 to <2 x double>
  %r95 = call { i64, i64, <2 x double> } @F400190(i64 %R63, i64 %R62, i64 %R61, <2 x double> %r94)
  ret { i64, i64, <2 x double> } %r95
failure:
  br label %failure
}