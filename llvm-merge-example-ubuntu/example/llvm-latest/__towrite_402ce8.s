	.text
	.file	"__towrite_402ce8.ll"
	.globl	F402ce8
	.align	16, 0x90
	.type	F402ce8,@function
F402ce8:                                # @F402ce8
	.cfi_startproc
# BB#0:                                 # %block_402ce8
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movzbl	138(%rdi), %esi
	leaq	-1(%rsi), %rax
	movl	%eax, %edx
	orl	%eax, %esi
	movb	%sil, 138(%rdi)
	movl	(%rdi), %esi
	testb	$8, %sil
	je	.LBB0_2
# BB#1:                                 # %block_402cff
	orl	$32, %esi
	movl	%esi, (%rdi)
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	retq
.LBB0_2:                                # %block_402d08
	movq	88(%rdi), %rax
	movq	$0, 16(%rdi)
	movq	$0, 8(%rdi)
	movq	%rax, 56(%rdi)
	movq	%rax, 40(%rdi)
	addq	96(%rdi), %rax
	movq	%rax, 32(%rdi)
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%eax, %eax
	retq
.Ltmp0:
	.size	F402ce8, .Ltmp0-F402ce8
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
