	.text
	.file	"print_tree_400256.ll"
	.globl	F400256
	.align	16, 0x90
	.type	F400256,@function
F400256:                                # @F400256
	.cfi_startproc
# BB#0:                                 # %block_400256
	subq	$40, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 48
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	%edi, 28(%rsp)
	movq	%rsi, 16(%rsp)
	testq	%rsi, %rsi
	je	.LBB0_2
# BB#1:                                 # %block_400283
	movq	16(%rsp), %rdx
	movq	(%rdx), %rsi
	movl	28(%rsp), %edi
	incl	%edi
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F400256
	movq	16(%rsp), %rax
	movq	8(%rax), %rsi
	movl	28(%rsp), %edi
	incl	%edi
	callq	F400256
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_2:                                # %block_40029e
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$40, %rsp
	retq
.Ltmp1:
	.size	F400256, .Ltmp1-F400256
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
