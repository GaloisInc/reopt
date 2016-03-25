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
define { i64, i64, <2 x double> } @F4001d0() {
entry:
  br label %block_4001d0
block_4001d0:
  ; r0 := (alloca 0x8 :: [64])
  %r0 = alloca i8, i64 8
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x8 :: [64])
  %R1 = add i64 %R0, 8
  ; # 4001d0: mov    esi,0x604098
  ; # 4001d5: push   rbp
  ; r2 := (bv_add r1 0xfffffffffffffff8 :: [64])
  %R2 = add i64 %R1, 18446744073709551608
  ; # 4001d6: sub    rsi,0x604098
  ; # 4001dd: sar    rsi,0x3
  ; r3 := (bv_sar 0x0 :: [64] 0x3 :: [64])
  %R3 = ashr i64 0, 3
  ; # 4001e1: mov    rbp,rsp
  ; # 4001e4: mov    rax,rsi
  ; # 4001e7: shr    rax,0x3f
  ; r4 := (bv_shr r3 0x3f :: [64])
  %R4 = lshr i64 %R3, 63
  ; # 4001eb: add    rsi,rax
  ; r5 := (bv_add r3 r4)
  %R5 = add i64 %R3, %R4
  ; # 4001ee: sar    rsi,1
  ; r6 := (bv_sar r5 0x1 :: [64])
  %R6 = ashr i64 %R5, 1
  ; r7 := (bv_eq r6 0x0 :: [64])
  %R7 = icmp eq i64 %R6, 0
  ; # 4001f1: je     400208
  br i1 %R7, label %subblock_4001d0_1, label %subblock_4001d0_2
subblock_4001d0_1:
  br label %block_400208
subblock_4001d0_2:
  br label %block_4001f3
block_4001f3:
  ; # 4001f3: mov    eax,0x0
  ; # 4001f8: test   rax,rax
  ; # 4001fb: je     400208
  br label %block_400208
block_400208:
  ; # 400208: pop    rbp
  ; # 400209: ret
  %r9 = insertvalue { i64, i64, <2 x double> } undef, i64 undef, 0
  %r10 = insertvalue { i64, i64, <2 x double> } %r9, i64 undef, 1
  %r11 = insertvalue { i64, i64, <2 x double> } %r10, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r11
failure:
  br label %failure
}