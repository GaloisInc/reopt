; ModuleID = 'tree.bc'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "amd64-portbld-freebsd10.0"

%struct.tree = type { %struct.tree*, %struct.tree*, i8* }
%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, i8*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64, %struct.pthread_mutex*, %struct.pthread*, i32, i32, %union.__mbstate_t }
%struct.__sbuf = type { i8*, i32 }
%struct.pthread_mutex = type opaque
%struct.pthread = type opaque
%union.__mbstate_t = type { i64, [120 x i8] }

@sep = constant i8 44, align 1
@.str = private unnamed_addr constant [2 x i8] c"w\00", align 1
@w_str = global i8* getelementptr inbounds ([2 x i8]* @.str, i64 0, i64 0), align 8
@.str1 = private unnamed_addr constant [2 x i8] c"r\00", align 1
@r_str = global i8* getelementptr inbounds ([2 x i8]* @.str1, i64 0, i64 0), align 8
@.str2 = private unnamed_addr constant [5 x i8] c"left\00", align 1
@g_left = global %struct.tree { %struct.tree* null, %struct.tree* null, i8* getelementptr inbounds ([5 x i8]* @.str2, i32 0, i32 0) }, align 8
@.str3 = private unnamed_addr constant [12 x i8] c"right_right\00", align 1
@g_right_right = global %struct.tree { %struct.tree* null, %struct.tree* null, i8* getelementptr inbounds ([12 x i8]* @.str3, i32 0, i32 0) }, align 8
@.str4 = private unnamed_addr constant [6 x i8] c"right\00", align 1
@g_right = global %struct.tree { %struct.tree* null, %struct.tree* @g_right_right, i8* getelementptr inbounds ([6 x i8]* @.str4, i32 0, i32 0) }, align 8
@.str5 = private unnamed_addr constant [5 x i8] c"root\00", align 1
@root = global %struct.tree { %struct.tree* @g_left, %struct.tree* @g_right, i8* getelementptr inbounds ([5 x i8]* @.str5, i32 0, i32 0) }, align 8

; Function Attrs: nounwind uwtable
define i32 @tree_equal(%struct.tree* readonly %t1, %struct.tree* readonly %t2) #0 {
  %1 = icmp eq %struct.tree* %t1, null
  %2 = icmp eq %struct.tree* %t2, null
  %brmerge = or i1 %1, %2
  %.mux = and i1 %1, %2
  br i1 %brmerge, label %24, label %3

; <label>:3                                       ; preds = %0
  %4 = getelementptr inbounds %struct.tree* %t1, i64 0, i32 2
  %5 = load i8** %4, align 8, !tbaa !1
  %6 = getelementptr inbounds %struct.tree* %t2, i64 0, i32 2
  %7 = load i8** %6, align 8, !tbaa !1
  %8 = tail call i32 inttoptr (i64 4333568 to i32 (i8*, i8*)*)(i8* %5, i8* %7) #2
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %24

; <label>:10                                      ; preds = %3
  %11 = getelementptr inbounds %struct.tree* %t1, i64 0, i32 0
  %12 = load %struct.tree** %11, align 8, !tbaa !6
  %13 = getelementptr inbounds %struct.tree* %t2, i64 0, i32 0
  %14 = load %struct.tree** %13, align 8, !tbaa !6
  %15 = tail call i32 @tree_equal(%struct.tree* %12, %struct.tree* %14)
  %16 = icmp eq i32 %15, 0
  br i1 %16, label %24, label %17

; <label>:17                                      ; preds = %10
  %18 = getelementptr inbounds %struct.tree* %t1, i64 0, i32 1
  %19 = load %struct.tree** %18, align 8, !tbaa !7
  %20 = getelementptr inbounds %struct.tree* %t2, i64 0, i32 1
  %21 = load %struct.tree** %20, align 8, !tbaa !7
  %22 = tail call i32 @tree_equal(%struct.tree* %19, %struct.tree* %21)
  %23 = icmp ne i32 %22, 0
  br label %24

; <label>:24                                      ; preds = %17, %10, %3, %0
  %.sink = phi i1 [ false, %10 ], [ false, %3 ], [ %23, %17 ], [ %.mux, %0 ]
  %25 = zext i1 %.sink to i32
  ret i32 %25
}

; Function Attrs: nounwind uwtable
define void @write_tree(i8* %filename, %struct.tree* readonly %t) #0 {
  %1 = load i8** inttoptr (i64 4583520 to i8**), align 32, !tbaa !8
  %2 = tail call %struct.__sFILE* inttoptr (i64 4291312 to %struct.__sFILE* (i8*, i8*)*)(i8* %filename, i8* %1) #2
  %3 = icmp eq %struct.__sFILE* %2, null
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

; <label>:5                                       ; preds = %0
  tail call fastcc void @really_write_tree(%struct.__sFILE* %2, %struct.tree* %t)
  %6 = tail call i32 inttoptr (i64 4295440 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %2) #2
  ret void
}

; Function Attrs: nounwind uwtable
define internal fastcc void @really_write_tree(%struct.__sFILE* %hdl, %struct.tree* readonly %t) #0 {
  %1 = icmp eq %struct.tree* %t, null
  br i1 %1, label %tailrecurse._crit_edge, label %tailrecurse.preheader

tailrecurse.preheader:                            ; preds = %0
  br label %tailrecurse

tailrecurse:                                      ; preds = %tailrecurse, %tailrecurse.preheader
  %t.tr1 = phi %struct.tree* [ %9, %tailrecurse ], [ %t, %tailrecurse.preheader ]
  %2 = getelementptr inbounds %struct.tree* %t.tr1, i64 0, i32 2
  %3 = load i8** %2, align 8, !tbaa !1
  %4 = tail call i32 inttoptr (i64 4272016 to i32 (i8*, %struct.__sFILE*)*)(i8* %3, %struct.__sFILE* %hdl) #2
  %5 = tail call i32 inttoptr (i64 4272144 to i32 (i32, %struct.__sFILE*)*)(i32 44, %struct.__sFILE* %hdl) #2
  %6 = getelementptr inbounds %struct.tree* %t.tr1, i64 0, i32 0
  %7 = load %struct.tree** %6, align 8, !tbaa !6
  tail call fastcc void @really_write_tree(%struct.__sFILE* %hdl, %struct.tree* %7)
  %8 = getelementptr inbounds %struct.tree* %t.tr1, i64 0, i32 1
  %9 = load %struct.tree** %8, align 8, !tbaa !7
  %10 = icmp eq %struct.tree* %9, null
  br i1 %10, label %tailrecurse._crit_edge.loopexit, label %tailrecurse

tailrecurse._crit_edge.loopexit:                  ; preds = %tailrecurse
  br label %tailrecurse._crit_edge

tailrecurse._crit_edge:                           ; preds = %tailrecurse._crit_edge.loopexit, %0
  %11 = tail call i32 inttoptr (i64 4272144 to i32 (i32, %struct.__sFILE*)*)(i32 44, %struct.__sFILE* %hdl) #2
  ret void
}

; Function Attrs: nounwind uwtable
define i8* @read_tree_payload(%struct.__sFILE* %hdl) #0 {
  %1 = tail call i8* inttoptr (i64 4197456 to i8* (i64)*)(i64 8) #2
  %2 = icmp eq i8* %1, null
  br i1 %2, label %5, label %.preheader

.preheader:                                       ; preds = %0
  %3 = tail call i32 inttoptr (i64 4196768 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %hdl) #2
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %.lr.ph.preheader, label %.thread

.lr.ph.preheader:                                 ; preds = %.preheader
  br label %.lr.ph

; <label>:5                                       ; preds = %0
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

.lr.ph:                                           ; preds = %15, %.lr.ph.preheader
  %indvars.iv = phi i64 [ %indvars.iv.next, %15 ], [ 0, %.lr.ph.preheader ]
  %buf.05 = phi i8* [ %buf.1, %15 ], [ %1, %.lr.ph.preheader ]
  %sz.03 = phi i64 [ %sz.1, %15 ], [ 8, %.lr.ph.preheader ]
  %6 = icmp eq i64 %indvars.iv, %sz.03
  br i1 %6, label %7, label %12

; <label>:7                                       ; preds = %.lr.ph
  %8 = shl i64 %indvars.iv, 1
  %9 = tail call i8* inttoptr (i64 4211424 to i8* (i8*, i64)*)(i8* %buf.05, i64 %8) #2
  %10 = icmp eq i8* %9, null
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

; <label>:12                                      ; preds = %7, %.lr.ph
  %sz.1 = phi i64 [ %8, %7 ], [ %sz.03, %.lr.ph ]
  %buf.1 = phi i8* [ %9, %7 ], [ %buf.05, %.lr.ph ]
  %13 = tail call i32 inttoptr (i64 4196096 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %hdl) #2
  switch i32 %13, label %15 [
    i32 -1, label %14
    i32 44, label %._crit_edge7
  ]

; <label>:14                                      ; preds = %12
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

; <label>:15                                      ; preds = %12
  %16 = trunc i32 %13 to i8
  %17 = getelementptr inbounds i8* %buf.1, i64 %indvars.iv
  store i8 %16, i8* %17, align 1, !tbaa !9
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %18 = tail call i32 inttoptr (i64 4196768 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %hdl) #2
  %19 = icmp eq i32 %18, 0
  br i1 %19, label %.lr.ph, label %._crit_edge7

._crit_edge7:                                     ; preds = %15, %12
  %buf.1.lcssa17 = phi i8* [ %buf.1, %12 ], [ %buf.1, %15 ]
  %indvars.iv.sink = phi i64 [ %indvars.iv.next, %15 ], [ %indvars.iv, %12 ]
  %20 = trunc i64 %indvars.iv.sink to i32
  %21 = icmp eq i32 %20, 0
  br i1 %21, label %.thread, label %22

.thread:                                          ; preds = %._crit_edge7, %.preheader
  %buf.215 = phi i8* [ %buf.1.lcssa17, %._crit_edge7 ], [ %1, %.preheader ]
  tail call void inttoptr (i64 4217184 to void (i8*)*)(i8* %buf.215) #2
  br label %25

; <label>:22                                      ; preds = %._crit_edge7
  %sext = shl i64 %indvars.iv.sink, 32
  %23 = ashr exact i64 %sext, 32
  %24 = getelementptr inbounds i8* %buf.1.lcssa17, i64 %23
  store i8 0, i8* %24, align 1, !tbaa !9
  br label %25

; <label>:25                                      ; preds = %22, %.thread
  %.0 = phi i8* [ null, %.thread ], [ %buf.1.lcssa17, %22 ]
  ret i8* %.0
}

; Function Attrs: nounwind uwtable
define %struct.tree* @read_tree(i8* %filename) #0 {
  %1 = load i8** @r_str, align 8, !tbaa !8
  %2 = tail call %struct.__sFILE* inttoptr (i64 4291312 to %struct.__sFILE* (i8*, i8*)*)(i8* %filename, i8* %1) #2
  %3 = icmp eq %struct.__sFILE* %2, null
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

; <label>:5                                       ; preds = %0
  %6 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %2)
  %7 = tail call i32 inttoptr (i64 4295440 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %2) #2
  ret %struct.tree* %6
}

; Function Attrs: nounwind uwtable
define internal fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %hdl) #0 {
  %1 = tail call i8* inttoptr (i64 4197456 to i8* (i64)*)(i64 8) #2
  %2 = icmp eq i8* %1, null
  br i1 %2, label %5, label %.preheader.i

.preheader.i:                                     ; preds = %0
  %3 = tail call i32 inttoptr (i64 4196768 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %hdl) #2
  %4 = icmp eq i32 %3, 0
  br i1 %4, label %.lr.ph.i.preheader, label %read_tree_payload.exit.thread

.lr.ph.i.preheader:                               ; preds = %.preheader.i
  br label %.lr.ph.i

; <label>:5                                       ; preds = %0
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

.lr.ph.i:                                         ; preds = %15, %.lr.ph.i.preheader
  %indvars.iv.i = phi i64 [ %indvars.iv.next.i, %15 ], [ 0, %.lr.ph.i.preheader ]
  %buf.05.i = phi i8* [ %buf.1.i, %15 ], [ %1, %.lr.ph.i.preheader ]
  %sz.03.i = phi i64 [ %sz.1.i, %15 ], [ 8, %.lr.ph.i.preheader ]
  %6 = icmp eq i64 %indvars.iv.i, %sz.03.i
  br i1 %6, label %7, label %12

; <label>:7                                       ; preds = %.lr.ph.i
  %8 = shl i64 %indvars.iv.i, 1
  %9 = tail call i8* inttoptr (i64 4211424 to i8* (i8*, i64)*)(i8* %buf.05.i, i64 %8) #2
  %10 = icmp eq i8* %9, null
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

; <label>:12                                      ; preds = %7, %.lr.ph.i
  %sz.1.i = phi i64 [ %8, %7 ], [ %sz.03.i, %.lr.ph.i ]
  %buf.1.i = phi i8* [ %9, %7 ], [ %buf.05.i, %.lr.ph.i ]
  %13 = tail call i32 inttoptr (i64 4196096 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %hdl) #2
  switch i32 %13, label %15 [
    i32 -1, label %14
    i32 44, label %._crit_edge7.i
  ]

; <label>:14                                      ; preds = %12
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

; <label>:15                                      ; preds = %12
  %16 = trunc i32 %13 to i8
  %17 = getelementptr inbounds i8* %buf.1.i, i64 %indvars.iv.i
  store i8 %16, i8* %17, align 1, !tbaa !9
  %indvars.iv.next.i = add nuw nsw i64 %indvars.iv.i, 1
  %18 = tail call i32 inttoptr (i64 4196768 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %hdl) #2
  %19 = icmp eq i32 %18, 0
  br i1 %19, label %.lr.ph.i, label %._crit_edge7.i

._crit_edge7.i:                                   ; preds = %15, %12
  %buf.1.i.lcssa4 = phi i8* [ %buf.1.i, %15 ], [ %buf.1.i, %12 ]
  %indvars.iv.sink.i = phi i64 [ %indvars.iv.next.i, %15 ], [ %indvars.iv.i, %12 ]
  %20 = trunc i64 %indvars.iv.sink.i to i32
  %21 = icmp eq i32 %20, 0
  br i1 %21, label %read_tree_payload.exit.thread, label %read_tree_payload.exit

read_tree_payload.exit.thread:                    ; preds = %._crit_edge7.i, %.preheader.i
  %buf.215.i = phi i8* [ %buf.1.i.lcssa4, %._crit_edge7.i ], [ %1, %.preheader.i ]
  tail call void inttoptr (i64 4217184 to void (i8*)*)(i8* %buf.215.i) #2
  br label %38

read_tree_payload.exit:                           ; preds = %._crit_edge7.i
  %sext.i = shl i64 %indvars.iv.sink.i, 32
  %22 = ashr exact i64 %sext.i, 32
  %23 = getelementptr inbounds i8* %buf.1.i.lcssa4, i64 %22
  store i8 0, i8* %23, align 1, !tbaa !9
  %24 = icmp eq i8* %buf.1.i.lcssa4, null
  br i1 %24, label %38, label %25

; <label>:25                                      ; preds = %read_tree_payload.exit
  %26 = tail call i8* inttoptr (i64 4197456 to i8* (i64)*)(i64 24) #2
  %27 = icmp eq i8* %26, null
  br i1 %27, label %28, label %29

; <label>:28                                      ; preds = %25
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

; <label>:29                                      ; preds = %25
  %30 = bitcast i8* %26 to %struct.tree*
  %31 = getelementptr inbounds i8* %26, i64 16
  %32 = bitcast i8* %31 to i8**
  store i8* %buf.1.i.lcssa4, i8** %32, align 8, !tbaa !1
  %33 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %hdl)
  %34 = bitcast i8* %26 to %struct.tree**
  store %struct.tree* %33, %struct.tree** %34, align 8, !tbaa !6
  %35 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %hdl)
  %36 = getelementptr inbounds i8* %26, i64 8
  %37 = bitcast i8* %36 to %struct.tree**
  store %struct.tree* %35, %struct.tree** %37, align 8, !tbaa !7
  ret %struct.tree* %30

; <label>:38                                      ; preds = %read_tree_payload.exit, %read_tree_payload.exit.thread
  ret %struct.tree* null
}

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** nocapture readonly %argv) #0 {
  %1 = icmp slt i32 %argc, 2
  br i1 %1, label %2, label %3

; <label>:2                                       ; preds = %0
  tail call void @exit(i32 1) #3
  unreachable

; <label>:3                                       ; preds = %0
  %4 = getelementptr inbounds i8** %argv, i64 1
  %5 = load i8** %4, align 8, !tbaa !8
  %6 = load i8** inttoptr (i64 4583520 to i8**), align 32, !tbaa !8
  %7 = tail call %struct.__sFILE* inttoptr (i64 4291312 to %struct.__sFILE* (i8*, i8*)*)(i8* %5, i8* %6) #2
  %8 = icmp eq %struct.__sFILE* %7, null
  br i1 %8, label %9, label %write_tree.exit

; <label>:9                                       ; preds = %3
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

write_tree.exit:                                  ; preds = %3
  tail call fastcc void @really_write_tree(%struct.__sFILE* %7, %struct.tree* inttoptr (i64 4583608 to %struct.tree*)) #2
  %10 = tail call i32 inttoptr (i64 4295440 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %7) #2
  %11 = load i8** @r_str, align 8, !tbaa !8
  %12 = tail call %struct.__sFILE* inttoptr (i64 4291312 to %struct.__sFILE* (i8*, i8*)*)(i8* %5, i8* %11) #2
  %13 = icmp eq %struct.__sFILE* %12, null
  br i1 %13, label %14, label %read_tree.exit

; <label>:14                                      ; preds = %write_tree.exit
  tail call void inttoptr (i64 4252544 to void (i32)*)(i32 1) #3
  unreachable

read_tree.exit:                                   ; preds = %write_tree.exit
  %15 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %12) #2
  %16 = tail call i32 inttoptr (i64 4295440 to i32 (%struct.__sFILE*)*)(%struct.__sFILE* %12) #2
  %17 = tail call i32 @tree_equal(%struct.tree* inttoptr (i64 4583608 to %struct.tree*), %struct.tree* %15)
  %18 = icmp eq i32 %17, 0
  %19 = zext i1 %18 to i32
  ret i32 %19
}

; Function Attrs: noreturn
declare void @exit(i32) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { noreturn "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
attributes #3 = { noreturn nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (tags/RELEASE_350/final)"}
!1 = metadata !{metadata !2, metadata !3, i64 16}
!2 = metadata !{metadata !"tree", metadata !3, i64 0, metadata !3, i64 8, metadata !3, i64 16}
!3 = metadata !{metadata !"any pointer", metadata !4, i64 0}
!4 = metadata !{metadata !"omnipotent char", metadata !5, i64 0}
!5 = metadata !{metadata !"Simple C/C++ TBAA"}
!6 = metadata !{metadata !2, metadata !3, i64 0}
!7 = metadata !{metadata !2, metadata !3, i64 8}
!8 = metadata !{metadata !3, metadata !3, i64 0}
!9 = metadata !{metadata !4, metadata !4, i64 0}
