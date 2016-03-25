	.text
	.file	"really_write_tree_400335.ll"
	.globl	F400335
	.align	16, 0x90
	.type	F400335,@function
F400335:                                # @F400335
	.cfi_startproc
# BB#0:                                 # %block_400335
	subq	$40, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 48
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	testq	%rsi, %rsi
	je	.LBB0_2
# BB#1:                                 # %block_40038f
	movq	16(%rsp), %rcx
	movq	24(%rsp), %rsi
	movq	16(%rcx), %rdi
	movd	%rax, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402023
	movq	24(%rsp), %rsi
	movl	$44, %edi
	movq	%rsi, %rdx
	callq	F401f8a
	movq	16(%rsp), %rax
	movq	24(%rsp), %rdi
	movq	(%rax), %rsi
	movq	%rsi, %rdx
	callq	F400335
	movq	16(%rsp), %rax
	movq	24(%rsp), %rdi
	movq	8(%rax), %rsi
	movq	%rsi, %rdx
	callq	F400335
	jmp	.LBB0_3
.LBB0_2:                                # %block_4003a8
	movd	%rax, %xmm0
	movd	%rdx, %xmm8
	movd	%xmm1, %rax
	pshufd	$78, %xmm1, %xmm9       # xmm9 = xmm1[2,3,0,1]
	movd	%rax, %xmm1
	movd	%xmm9, %rax
	movd	%rax, %xmm9
	movd	%xmm2, %rax
	pshufd	$78, %xmm2, %xmm10      # xmm10 = xmm2[2,3,0,1]
	movd	%rax, %xmm2
	movd	%xmm10, %rax
	movd	%rax, %xmm10
	movd	%xmm3, %rax
	pshufd	$78, %xmm3, %xmm11      # xmm11 = xmm3[2,3,0,1]
	movd	%rax, %xmm3
	movd	%xmm11, %rax
	movd	%rax, %xmm11
	movd	%xmm4, %rax
	pshufd	$78, %xmm4, %xmm12      # xmm12 = xmm4[2,3,0,1]
	movd	%rax, %xmm4
	movd	%xmm12, %rax
	movd	%rax, %xmm12
	movd	%xmm5, %rax
	pshufd	$78, %xmm5, %xmm13      # xmm13 = xmm5[2,3,0,1]
	movd	%rax, %xmm5
	movd	%xmm13, %rax
	movd	%rax, %xmm13
	movd	%xmm6, %rax
	pshufd	$78, %xmm6, %xmm14      # xmm14 = xmm6[2,3,0,1]
	movd	%rax, %xmm6
	movd	%xmm14, %rax
	movd	%rax, %xmm14
	movd	%xmm7, %rax
	pshufd	$78, %xmm7, %xmm15      # xmm15 = xmm7[2,3,0,1]
	movd	%rax, %xmm7
	movd	%xmm15, %rax
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	movd	%rax, %xmm8
	movq	24(%rsp), %rsi
	punpcklqdq	%xmm8, %xmm7    # xmm7 = xmm7[0],xmm8[0]
	movl	$44, %edi
	movq	%rsi, %rdx
	callq	F401f8a
.LBB0_3:                                # %block_4003be
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$40, %rsp
	retq
.Ltmp1:
	.size	F400335, .Ltmp1-F400335
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
