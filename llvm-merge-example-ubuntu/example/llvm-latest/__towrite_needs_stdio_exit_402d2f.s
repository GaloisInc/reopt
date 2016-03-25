	.text
	.file	"__towrite_needs_stdio_exit_402d2f.ll"
	.globl	F402d2f
	.align	16, 0x90
	.type	F402d2f,@function
F402d2f:                                # @F402d2f
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
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402eb4
	movq	%rbp, %rsp
	popq	%rbp
	retq
.Ltmp3:
	.size	F402d2f, .Ltmp3-F402d2f
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
