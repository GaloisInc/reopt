	.text
	.file	"write_tree_4003c1.ll"
	.globl	F4003c1
	.align	16, 0x90
	.type	F4003c1,@function
F4003c1:                                # @F4003c1
	.cfi_startproc
# BB#0:                                 # %block_4003e7
	subq	$56, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 64
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	movq	6307864, %rsi
	movq	24(%rsp), %rdi
	movq	%rsi, %rdx
	callq	F401ee8
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testq	%rax, %rax
	jne	.LBB0_2
# BB#1:                                 # %block_4003f2
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$75, %edi
	movq	%rdx, %rsi
	callq	F400130
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_2:                                # %block_40041b
	movq	16(%rsp), %rsi
	movq	40(%rsp), %rdi
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rsi, %rdx
	callq	F400335
	movq	40(%rsp), %rdi
	movq	%rdx, %rsi
	callq	F401c7a
	addq	$56, %rsp
	retq
.Ltmp1:
	.size	F4003c1, .Ltmp1-F4003c1
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
