	.text
	.file	"unknown_402f54.ll"
	.globl	F402f54
	.align	16, 0x90
	.type	F402f54,@function
F402f54:                                # @F402f54
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp0:
	.cfi_def_cfa_offset 16
.Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp2:
	.cfi_def_cfa_register %rbp
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movq	%rsp, %rsi
	addq	$-16, %rsi
	movq	%rsi, %rsp
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbp, %rsp
	popq	%rbp
	retq
.Ltmp3:
	.size	F402f54, .Ltmp3-F402f54
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
