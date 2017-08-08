
;; Replace SZ by {8,16,32,64} as appropriate

;; We try to be faithful to the original semantics, and
;; do not use memmove/memcpy.

define void @reopt.MemCopy.iSZ(i64 %dest, i64 %src, i64 %count, i1 %df) nounwind
{
entry:
    br i1 %df, label %backward, label %forward
backward:
    tail call void @reopt.MemCopy.iSZ.backward(i64 %dest, i64 %src, i64 %count)
    ret void

forward:
    tail call void @reopt.MemCopy.iSZ.forward(i64 %dest, i64 %src, i64 %count)
    ret void
}

define void @reopt.MemCopy.iSZ.backward(i64 %dest, i64 %src, i64 %count) nounwind
{
entry:
    br label %backward

backward:
    %c = phi i64 [ %count, %entry], [%cNext,  %backDoIt]
    %di = phi i64 [ %dest, %entry], [%diNext, %backDoIt]
    %si = phi i64 [ %src, %entry],  [%siNext, %backDoIt]

    %isZero = icmp eq i64 %c, 0
    br i1 %isZero, label %backDone, label %backDoIt
backDoIt:
    %diPtr = inttoptr i64 %di to iSZ *
    %siPtr = inttoptr i64 %si to iSZ *

    %siV = load iSZ, iSZ * %siPtr
    store iSZ %siV, iSZ *%diPtr

    %diNext = sub i64 %di, udiv(i64 SZ, i64 8)
    %siNext = sub i64 %si, udiv(i64 SZ, i64 8)
    %cNext  = sub i64 %c, 1
    br label %backward
backDone:
    ret void
}

define void @reopt.MemCopy.iSZ.forward(i64 %dest, i64 %src, i64 %count) nounwind
{
entry:
    br label %forward
forward:
    %c = phi i64  [ %count, %entry], [%cNext,  %forwDoIt]
    %di = phi i64 [ %dest, %entry],  [%diNext, %forwDoIt]
    %si = phi i64 [ %src, %entry],   [%siNext, %forwDoIt]

    %isZero = icmp eq i64 %c, 0
    br i1 %isZero, label %forwDone, label %forwDoIt
forwDoIt:
    %diPtr = inttoptr i64 %di to iSZ *
    %siPtr = inttoptr i64 %si to iSZ *

    %siV = load iSZ, iSZ * %siPtr
    store iSZ %siV, iSZ *%diPtr

    %diNext = add i64 %di, udiv(i64 SZ, i64 8)
    %siNext = add i64 %si, udiv(i64 SZ, i64 8)
    %cNext  = sub i64 %c, 1
    br label %forward
forwDone:
    ret void
}



    ; %nbytes =   mul i64 udiv(i64 SZ, i64 8), %count

    ; %srcBaseLess1  =  sub i64 %src, %nbytes
    ; %srcBase = add i64 %srcBaseLess1, 1
    ; %destBaseLess1 = sub i64 %dest, %nbytes
    ; %destBase = add i64 %destBaseLess1, 1

    ; %realSrc  = select i1 %df, i64 %srcBase, i64 %src
    ; %realDest = select i1 %df, i64 %destBase, i64 %dest

    ; %srcP = inttoptr i64 %realSrc   to iSZ *
    ; %destP = inttoptr i64 %realDest to iSZ *
    ; tail call void @llvm.memmove.p0iSZ.p0iSZ.i64(iSZ * %destP, iSZ * %srcP, i64 %count, i32 0, i1 0)
    ; ret void
