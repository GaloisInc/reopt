
;; Replace SZ by {8,16,32,64} as appropriate

;; We use memmove in this file because we are not guaranteed that the
;; regions do not overlap.

declare void @llvm.memmove.p0iSZ.p0iSZ.i64(iSZ *, iSZ *, i64, i32, i1)

define void @reopt.MemCopy.iSZ(i64 %dest, i64 %src, i64 %count, i1 %df) nounwind
{
    %nbytes =   mul i64 udiv(i64 SZ, i64 8), %count
    
    %srcBaseLess1  =  sub i64 %src, %nbytes
    %srcBase = add i64 %srcBaseLess1, 1
    %destBaseLess1 = sub i64 %dest, %nbytes
    %destBase = add i64 %destBaseLess1, 1

    %realSrc  = select i1 %df, i64 %srcBase, i64 %src
    %realDest = select i1 %df, i64 %destBase, i64 %dest
    
    %srcP = inttoptr i64 %realSrc   to iSZ *
    %destP = inttoptr i64 %realDest to iSZ *
    tail call void @llvm.memmove.p0iSZ.p0iSZ.i64(iSZ * %destP, iSZ * %srcP, i64 %count, i32 0, i1 0)
    ret void
}
