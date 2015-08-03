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
@__isthreaded = external global i32

; Function Attrs: nounwind readonly uwtable
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
  %8 = tail call i32 @strcmp(i8* %5, i8* %7) #5
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

; Function Attrs: nounwind readonly
declare i32 @strcmp(i8* nocapture, i8* nocapture) #1

; Function Attrs: nounwind uwtable
define void @write_tree(i8* nocapture readonly %filename, %struct.tree* readonly %t) #2 {
  %1 = load i8** @w_str, align 8, !tbaa !8
  %2 = tail call %struct.__sFILE* @fopen(i8* %filename, i8* %1) #6
  %3 = icmp eq %struct.__sFILE* %2, null
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  tail call void @exit(i32 1) #7
  unreachable

; <label>:5                                       ; preds = %0
  tail call fastcc void @really_write_tree(%struct.__sFILE* %2, %struct.tree* %t)
  %6 = tail call i32 @fclose(%struct.__sFILE* %2) #6
  ret void
}

; Function Attrs: nounwind
declare noalias %struct.__sFILE* @fopen(i8* nocapture readonly, i8* nocapture readonly) #3

; Function Attrs: noreturn
declare void @exit(i32) #4

; Function Attrs: nounwind uwtable
define internal fastcc void @really_write_tree(%struct.__sFILE* %hdl, %struct.tree* readonly %t) #2 {
  %1 = icmp eq %struct.tree* %t, null
  br i1 %1, label %tailrecurse._crit_edge, label %tailrecurse.preheader

tailrecurse.preheader:                            ; preds = %0
  br label %tailrecurse

tailrecurse:                                      ; preds = %tailrecurse, %tailrecurse.preheader
  %t.tr1 = phi %struct.tree* [ %9, %tailrecurse ], [ %t, %tailrecurse.preheader ]
  %2 = getelementptr inbounds %struct.tree* %t.tr1, i64 0, i32 2
  %3 = load i8** %2, align 8, !tbaa !1
  %4 = tail call i32 @fputs(i8* %3, %struct.__sFILE* %hdl) #6
  %5 = tail call i32 @fputc(i32 44, %struct.__sFILE* %hdl) #6
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
  %11 = tail call i32 @fputc(i32 44, %struct.__sFILE* %hdl) #6
  ret void
}

; Function Attrs: nounwind
declare i32 @fclose(%struct.__sFILE* nocapture) #3

; Function Attrs: nounwind uwtable
define noalias i8* @read_tree_payload(%struct.__sFILE* nocapture %hdl) #2 {
  %1 = tail call noalias i8* @malloc(i64 8) #6
  %2 = icmp eq i8* %1, null
  br i1 %2, label %4, label %.preheader

.preheader:                                       ; preds = %0
  %3 = getelementptr inbounds %struct.__sFILE* %hdl, i64 0, i32 3
  br label %5

; <label>:4                                       ; preds = %0
  tail call void @exit(i32 1) #7
  unreachable

; <label>:5                                       ; preds = %28, %.preheader
  %indvars.iv = phi i64 [ 0, %.preheader ], [ %indvars.iv.next, %28 ]
  %sz.0 = phi i64 [ 8, %.preheader ], [ %sz.1, %28 ]
  %buf.0 = phi i8* [ %1, %.preheader ], [ %buf.1, %28 ]
  %6 = load i32* @__isthreaded, align 4, !tbaa !9
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %8, label %12

; <label>:8                                       ; preds = %5
  %9 = load i16* %3, align 2, !tbaa !11
  %10 = lshr i16 %9, 5
  %.lobit = and i16 %10, 1
  %11 = zext i16 %.lobit to i32
  br label %14

; <label>:12                                      ; preds = %5
  %13 = tail call i32 @feof(%struct.__sFILE* %hdl) #6
  br label %14

; <label>:14                                      ; preds = %12, %8
  %15 = phi i32 [ %11, %8 ], [ %13, %12 ]
  %16 = icmp eq i32 %15, 0
  %17 = trunc i64 %indvars.iv to i32
  br i1 %16, label %18, label %31

; <label>:18                                      ; preds = %14
  %19 = icmp eq i64 %indvars.iv, %sz.0
  br i1 %19, label %20, label %25

; <label>:20                                      ; preds = %18
  %21 = shl i64 %indvars.iv, 1
  %22 = tail call i8* @realloc(i8* %buf.0, i64 %21) #6
  %23 = icmp eq i8* %22, null
  br i1 %23, label %24, label %25

; <label>:24                                      ; preds = %20
  tail call void @exit(i32 1) #7
  unreachable

; <label>:25                                      ; preds = %20, %18
  %sz.1 = phi i64 [ %21, %20 ], [ %sz.0, %18 ]
  %buf.1 = phi i8* [ %22, %20 ], [ %buf.0, %18 ]
  %26 = tail call i32 @fgetc(%struct.__sFILE* %hdl) #6
  switch i32 %26, label %28 [
    i32 -1, label %27
    i32 44, label %31
  ]

; <label>:27                                      ; preds = %25
  tail call void @exit(i32 1) #7
  unreachable

; <label>:28                                      ; preds = %25
  %29 = trunc i32 %26 to i8
  %30 = getelementptr inbounds i8* %buf.1, i64 %indvars.iv
  store i8 %29, i8* %30, align 1, !tbaa !16
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %5

; <label>:31                                      ; preds = %25, %14
  %.lcssa = phi i32 [ %17, %25 ], [ %17, %14 ]
  %indvars.iv.lcssa = phi i64 [ %indvars.iv, %25 ], [ %indvars.iv, %14 ]
  %buf.2 = phi i8* [ %buf.0, %14 ], [ %buf.1, %25 ]
  %32 = icmp eq i32 %.lcssa, 0
  br i1 %32, label %33, label %34

; <label>:33                                      ; preds = %31
  tail call void @free(i8* %buf.2)
  br label %37

; <label>:34                                      ; preds = %31
  %sext = shl i64 %indvars.iv.lcssa, 32
  %35 = ashr exact i64 %sext, 32
  %36 = getelementptr inbounds i8* %buf.2, i64 %35
  store i8 0, i8* %36, align 1, !tbaa !16
  br label %37

; <label>:37                                      ; preds = %34, %33
  %.0 = phi i8* [ null, %33 ], [ %buf.2, %34 ]
  ret i8* %.0
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #3

; Function Attrs: nounwind
declare i32 @feof(%struct.__sFILE* nocapture) #3

; Function Attrs: nounwind
declare noalias i8* @realloc(i8* nocapture, i64) #3

; Function Attrs: nounwind
declare i32 @fgetc(%struct.__sFILE* nocapture) #3

; Function Attrs: nounwind
declare void @free(i8* nocapture) #3

; Function Attrs: nounwind uwtable
define noalias %struct.tree* @read_tree(i8* nocapture readonly %filename) #2 {
  %1 = load i8** @r_str, align 8, !tbaa !8
  %2 = tail call %struct.__sFILE* @fopen(i8* %filename, i8* %1) #6
  %3 = icmp eq %struct.__sFILE* %2, null
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  tail call void @exit(i32 1) #7
  unreachable

; <label>:5                                       ; preds = %0
  %6 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %2)
  %7 = tail call i32 @fclose(%struct.__sFILE* %2) #6
  ret %struct.tree* %6
}

; Function Attrs: nounwind uwtable
define internal fastcc noalias %struct.tree* @really_read_tree(%struct.__sFILE* %hdl) #2 {
  %1 = tail call i8* @read_tree_payload(%struct.__sFILE* %hdl)
  %2 = icmp eq i8* %1, null
  br i1 %2, label %16, label %3

; <label>:3                                       ; preds = %0
  %4 = tail call noalias i8* @malloc(i64 24) #6
  %5 = icmp eq i8* %4, null
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %3
  tail call void @exit(i32 1) #7
  unreachable

; <label>:7                                       ; preds = %3
  %8 = bitcast i8* %4 to %struct.tree*
  %9 = getelementptr inbounds i8* %4, i64 16
  %10 = bitcast i8* %9 to i8**
  store i8* %1, i8** %10, align 8, !tbaa !1
  %11 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %hdl)
  %12 = bitcast i8* %4 to %struct.tree**
  store %struct.tree* %11, %struct.tree** %12, align 8, !tbaa !6
  %13 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %hdl)
  %14 = getelementptr inbounds i8* %4, i64 8
  %15 = bitcast i8* %14 to %struct.tree**
  store %struct.tree* %13, %struct.tree** %15, align 8, !tbaa !7
  ret %struct.tree* %8

; <label>:16                                      ; preds = %0
  ret %struct.tree* null
}

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** nocapture readonly %argv) #2 {
  %1 = icmp slt i32 %argc, 2
  br i1 %1, label %2, label %3

; <label>:2                                       ; preds = %0
  tail call void @exit(i32 1) #7
  unreachable

; <label>:3                                       ; preds = %0
  %4 = getelementptr inbounds i8** %argv, i64 1
  %5 = load i8** %4, align 8, !tbaa !8
  %6 = load i8** @w_str, align 8, !tbaa !8
  %7 = tail call %struct.__sFILE* @fopen(i8* %5, i8* %6) #6
  %8 = icmp eq %struct.__sFILE* %7, null
  br i1 %8, label %9, label %write_tree.exit

; <label>:9                                       ; preds = %3
  tail call void @exit(i32 1) #7
  unreachable

write_tree.exit:                                  ; preds = %3
  tail call fastcc void @really_write_tree(%struct.__sFILE* %7, %struct.tree* @root) #6
  %10 = tail call i32 @fclose(%struct.__sFILE* %7) #6
  %11 = load i8** @r_str, align 8, !tbaa !8
  %12 = tail call %struct.__sFILE* @fopen(i8* %5, i8* %11) #6
  %13 = icmp eq %struct.__sFILE* %12, null
  br i1 %13, label %14, label %read_tree.exit

; <label>:14                                      ; preds = %write_tree.exit
  tail call void @exit(i32 1) #7
  unreachable

read_tree.exit:                                   ; preds = %write_tree.exit
  %15 = tail call fastcc %struct.tree* @really_read_tree(%struct.__sFILE* %12) #6
  %16 = tail call i32 @fclose(%struct.__sFILE* %12) #6
  %17 = tail call i32 @tree_equal(%struct.tree* @root, %struct.tree* %15)
  %18 = icmp eq i32 %17, 0
  %19 = zext i1 %18 to i32
  ret i32 %19
}

; Function Attrs: nounwind
declare i32 @fputs(i8* nocapture readonly, %struct.__sFILE* nocapture) #3

; Function Attrs: nounwind
declare i32 @fputc(i32, %struct.__sFILE* nocapture) #3

attributes #0 = { nounwind readonly uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readonly "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { noreturn "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind readonly }
attributes #6 = { nounwind }
attributes #7 = { noreturn nounwind }

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
!9 = metadata !{metadata !10, metadata !10, i64 0}
!10 = metadata !{metadata !"int", metadata !4, i64 0}
!11 = metadata !{metadata !12, metadata !13, i64 16}
!12 = metadata !{metadata !"__sFILE", metadata !3, i64 0, metadata !10, i64 8, metadata !10, i64 12, metadata !13, i64 16, metadata !13, i64 18, metadata !14, i64 24, metadata !10, i64 40, metadata !3, i64 48, metadata !3, i64 56, metadata !3, i64 64, metadata !3, i64 72, metadata !3, i64 80, metadata !14, i64 88, metadata !3, i64 104, metadata !10, i64 112, metadata !4, i64 116, metadata !4, i64 119, metadata !14, i64 120, metadata !10, i64 136, metadata !15, i64 144, metadata !3, i64 152, metadata !3, i64 160, metadata !10, i64 168, metadata !10, i64 172, metadata !4, i64 176}
!13 = metadata !{metadata !"short", metadata !4, i64 0}
!14 = metadata !{metadata !"__sbuf", metadata !3, i64 0, metadata !10, i64 8}
!15 = metadata !{metadata !"long", metadata !4, i64 0}
!16 = metadata !{metadata !4, metadata !4, i64 0}
