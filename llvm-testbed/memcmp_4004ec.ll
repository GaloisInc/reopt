declare i1 @reopt.EvenParity(i8)
declare i2 @reopt.Read_X87_RC()
declare void @reopt.Write_X87_RC(i2)
declare i2 @reopt.Read_X87_PC()
declare void @reopt.Write_X87_PC(i2)
declare i16 @reopt.Read_FS()
declare void @reopt.Write_FS(i16)
declare i16 @reopt.Read_GS()
declare void @reopt.Write_GS(i16)
declare void @reopt.MemCopy(i64, i64, i64, i64, i1)
declare i64 @reopt.MemCmp(i64, i64, i64, i64, i1)
declare { i64, i1 } @reopt.SystemCall(i64, i64, i64, i64, i64, i64, i64)
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
define { i64, i64, <2 x double> } @F4004ec(i64 %a0, i64 %a1, i64 %a2) {
entry:
  br label %block_4004ec
block_4004ec:
  ; r0 := (alloca 0x0 :: [64])
  %r0 = alloca i8, i64 0
  %R0 = ptrtoint i8* %r0 to i64
  ; r1 := (bv_add r0 0x0 :: [64])
  %R1 = add i64 %R0, 0
  ; # 4004ec: xor    ecx,ecx
  br label %block_4004ee
block_4004ee:
  %R5 = phi i64 [ %R10, %subblock_4004f3_1 ], [ %a0, %block_4004ec ]
  %R4 = phi i64 [ %R9, %subblock_4004f3_1 ], [ %a1, %block_4004ec ]
  %R3 = phi i64 [ %R8, %subblock_4004f3_1 ], [ %a2, %block_4004ec ]
  %R2 = phi i64 [ %R17, %subblock_4004f3_1 ], [ 0, %block_4004ec ]
  ; # 4004ee: cmp    rcx,rdx
  ; r6 := (bv_eq r2 r3)
  %R6 = icmp eq i64 %R2, %R3
  ; # 4004f1: je     400506
  br i1 %R6, label %subblock_4004ee_1, label %subblock_4004ee_2
subblock_4004ee_1:
  br label %block_400506
subblock_4004ee_2:
  br label %block_4004f3
block_4004f3:
  %R10 = phi i64 [ %R5, %subblock_4004ee_2 ]
  %R9 = phi i64 [ %R4, %subblock_4004ee_2 ]
  %R8 = phi i64 [ %R3, %subblock_4004ee_2 ]
  %R7 = phi i64 [ %R2, %subblock_4004ee_2 ]
  ; # 4004f3: movzx  eax,BYTE PTR [rdi+rcx*1]
  ; r11 := (bv_add r10 r7)
  %R11 = add i64 %R10, %R7
  ; r12 := *r11
  %r13 = inttoptr i64 %R11 to i8*
  %R12 = load i8* %r13
  ; r13 := (uext r12 32)
  %R13 = zext i8 %R12 to i32
  ; # 4004f7: movzx  r8d,BYTE PTR [rsi+rcx*1]
  ; r14 := (bv_add r9 r7)
  %R14 = add i64 %R9, %R7
  ; r15 := *r14
  %r17 = inttoptr i64 %R14 to i8*
  %R15 = load i8* %r17
  ; r16 := (uext r15 32)
  %R16 = zext i8 %R15 to i32
  ; # 4004fc: inc    rcx
  ; r17 := (bv_add r7 0x1 :: [64])
  %R17 = add i64 %R7, 1
  ; # 4004ff: sub    eax,r8d
  ; r18 := (bv_sub r13 r16)
  %R18 = sub i32 %R13, %R16
  ; r19 := (bv_eq r13 r16)
  %R19 = icmp eq i32 %R13, %R16
  ; r20 := (uext r18 64)
  %R20 = zext i32 %R18 to i64
  ; # 400502: je     4004ee
  br i1 %R19, label %subblock_4004f3_1, label %subblock_4004f3_2
subblock_4004f3_1:
  br label %block_4004ee
subblock_4004f3_2:
  br label %block_400504
block_400504:
  %R21 = phi i64 [ %R20, %subblock_4004f3_2 ]
  ; # 400504: jmp    400508
  br label %block_400508
block_400506:
  ; # 400506: xor    eax,eax
  br label %block_400508
block_400508:
  %R22 = phi i64 [ %R21, %block_400504 ], [ 0, %block_400506 ]
  ; # 400508: ret
  %r26 = insertvalue { i64, i64, <2 x double> } undef, i64 %R22, 0
  %r27 = insertvalue { i64, i64, <2 x double> } %r26, i64 undef, 1
  %r28 = insertvalue { i64, i64, <2 x double> } %r27, <2 x double> undef, 2
  ret { i64, i64, <2 x double> } %r28
failure:
  br label %failure
}