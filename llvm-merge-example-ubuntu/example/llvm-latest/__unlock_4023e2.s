	.text
	.file	"__unlock_4023e2.ll"
	.globl	F4023e2
	.align	16, 0x90
	.type	F4023e2,@function
F4023e2:                                # @F4023e2
	.cfi_startproc
# BB#0:                                 # %block_4023e2
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp3:
	.cfi_def_cfa_offset 48
.Ltmp4:
	.cfi_offset %rbx, -32
.Ltmp5:
	.cfi_offset %r14, -24
.Ltmp6:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movd	%xmm0, %r15
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbx
	movl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB0_4
# BB#1:                                 # %block_4023e8
	movl	$0, (%rdi)
	movl	4(%rdi), %eax
	testq	%rax, %rax
	je	.LBB0_4
# BB#2:                                 # %block_40240d
	movq	$202, (%rsp)
	movl	$129, %esi
	movl	$1, %edx
	movl	$202, %r8d
	callq	reopt.SystemCall.Linux
	cmpq	$-38, %rax
	jne	.LBB0_4
# BB#3:                                 # %block_402413
	movq	$202, (%rsp)
	callq	reopt.SystemCall.Linux
.LBB0_4:                                # %block_40241b
	movd	%r15, %xmm0
	movd	%rbx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r14, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7:
	.size	F4023e2, .Ltmp7-F4023e2
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
