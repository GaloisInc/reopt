	.text
	.file	"read_tree_4005aa.ll"
	.globl	F4005aa
	.align	16, 0x90
	.type	F4005aa,@function
F4005aa:                                # @F4005aa
	.cfi_startproc
# BB#0:                                 # %block_4005cc
	subq	$56, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 64
	movq	%rdi, 24(%rsp)
	movq	6307856, %rsi
	movq	%rsi, %rdx
	callq	F401ee8
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testq	%rax, %rax
	jne	.LBB0_2
# BB#1:                                 # %block_4005d7
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$147, %edi
	movq	%rdx, %rsi
	callq	F400130
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_2:                                # %block_4005fd
	movq	32(%rsp), %rdi
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rsi
	callq	F400522
	movq	32(%rsp), %rdi
	movq	%rdx, %rsi
	callq	F401c7a
	movq	40(%rsp), %rax
	addq	$56, %rsp
	retq
.Ltmp1:
	.size	F4005aa, .Ltmp1-F4005aa
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
