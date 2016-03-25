	.text
	.file	"__mremap_401bb6.ll"
	.globl	F401bb6
	.align	16, 0x90
	.type	F401bb6,@function
F401bb6:                                # @F401bb6
	.cfi_startproc
# BB#0:                                 # %block_401bb6
	pushq	%r14
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1:
	.cfi_def_cfa_offset 24
	subq	$136, %rsp
.Ltmp2:
	.cfi_def_cfa_offset 160
.Ltmp3:
	.cfi_offset %rbx, -24
.Ltmp4:
	.cfi_offset %r14, -16
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r10
	movq	%r8, 112(%rsp)
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	cmpq	%rax, %rdx
	jae	.LBB0_1
# BB#2:                                 # %block_401be0
	xorl	%r8d, %r8d
	testb	$2, %cl
	movl	%ecx, %r14d
	je	.LBB0_4
# BB#3:                                 # %block_401bfe
	leaq	16(%rsp), %rbx
	movq	%rdx, 24(%rbx)
	movq	%rsi, 16(%rbx)
	movq	%rdi, 8(%rbx)
	movd	%r9, %xmm0
	movd	%r10, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%r8d, %r8d
	callq	F401b1f
	movq	%rbx, %rax
	subq	$-128, %rax
	movq	96(%rbx), %r8
	movq	24(%rbx), %rdx
	movq	8(%rbx), %rdi
	movq	16(%rbx), %rsi
	movq	%rax, 48(%rbx)
	leaq	64(%rbx), %rax
	movl	$32, 40(%rbx)
	movq	%rax, 56(%rbx)
.LBB0_4:                                # %block_401c3b
	movslq	%r14d, %rcx
	movq	$25, (%rsp)
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	jmp	.LBB0_5
.LBB0_1:                                # %block_401bd4
	movd	%r9, %xmm0
	movd	%r10, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402681
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	$12, (%rax)
.LBB0_5:                                # %block_401c43
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	$-1, %rax
	addq	$136, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5:
	.size	F401bb6, .Ltmp5-F401bb6
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
