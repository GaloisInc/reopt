
;; Replace SZ by {8,16,32,64} as appropriate

;; We try to be as faithful to the original semantics as possibly,
;; hence we don't use memmove/memcpy

define void @reopt.MemSet.iSZ(i64 %dest, iSZ %val, i64 %count, i1 %df) nounwind
{
entry:
    br i1 %df, label %backward, label %forward
backward:
    tail call void @reopt.MemSet.iSZ.backward(i64 %dest, iSZ %val, i64 %count)
    ret void
    
forward:
    tail call void @reopt.MemSet.iSZ.forward(i64 %dest, iSZ %val, i64 %count)
    ret void        
}

define void @reopt.MemSet.iSZ.backward(i64 %dest, iSZ %val, i64 %count) nounwind
{
entry:
    br label %backward
    
backward:
    %c = phi i64 [ %count, %entry], [%cNext,  %backDoIt]
    %di = phi i64 [ %dest, %entry], [%diNext, %backDoIt]
    
    %isZero = icmp eq i64 %c, 0
    br i1 %isZero, label %backDone, label %backDoIt
backDoIt:
    %diPtr = inttoptr i64 %di to iSZ *

    store iSZ %val, iSZ *%diPtr

    %diNext = sub i64 %di, udiv(i64 SZ, i64 8)
    %cNext  = sub i64 %c, 1
    br label %backward
backDone:
    ret void
}

define void @reopt.MemSet.iSZ.forward(i64 %dest, iSZ %val, i64 %count) nounwind
{
entry:
    br label %forward
forward:
    %c = phi i64  [ %count, %entry], [%cNext,  %forwDoIt]
    %di = phi i64 [ %dest, %entry],  [%diNext, %forwDoIt]
    
    %isZero = icmp eq i64 %c, 0
    br i1 %isZero, label %forwDone, label %forwDoIt
forwDoIt:
    %diPtr = inttoptr i64 %di to iSZ *

    store iSZ %val, iSZ *%diPtr

    %diNext = add i64 %di, udiv(i64 SZ, i64 8)
    %cNext  = sub i64 %c, 1
    br label %forward
forwDone:
    ret void
}
