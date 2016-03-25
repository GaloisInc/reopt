	.text
	.file	"really_read_tree_400522.ll"
	.globl	F400522
	.align	16, 0x90
	.type	F400522,@function
F400522:                                # @F400522
	.cfi_startproc
# BB#0:                                 # %block_40053a
	subq	$56, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 64
	movq	%rdi, 24(%rsp)
	callq	F40041e
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	xorl	%eax, %eax
	testq	%rax, %rax
	je	.LBB0_4
# BB#1:                                 # %block_400556
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$24, %edi
	movq	%rdx, %rsi
	callq	F401280
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	testq	%rax, %rax
	jne	.LBB0_3
# BB#2:                                 # %block_400561
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$132, %edi
	movq	%rdx, %rsi
	callq	F400130
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
.LBB0_3:                                # %block_400599
	movq	32(%rsp), %rax
	movq	40(%rsp), %rdi
	movq	%rax, 16(%rdi)
	movq	24(%rsp), %rdi
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	F400522
	movq	24(%rsp), %rdi
	movq	%rdx, %rsi
	callq	F400522
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movq	40(%rsp), %rax
.LBB0_4:                                # %block_4005a8
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$56, %rsp
	retq
.Ltmp1:
	.size	F400522, .Ltmp1-F400522
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
