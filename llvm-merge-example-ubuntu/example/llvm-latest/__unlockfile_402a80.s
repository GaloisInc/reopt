	.text
	.file	"__unlockfile_402a80.ll"
	.globl	F402a80
	.align	16, 0x90
	.type	F402a80,@function
F402a80:                                # @F402a80
	.cfi_startproc
# BB#0:                                 # %block_402a80
	pushq	%r14
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp2:
	.cfi_def_cfa_offset 48
.Ltmp3:
	.cfi_offset %rbx, -24
.Ltmp4:
	.cfi_offset %r14, -16
	movd	%xmm0, %r14
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbx
	movl	$0, 140(%rdi)
	movl	144(%rdi), %eax
	testq	%rax, %rax
	je	.LBB0_3
# BB#1:                                 # %block_402ab6
	addq	$140, %rdi
	movq	$202, (%rsp)
	movl	$129, %esi
	movl	$1, %edx
	movl	$202, %r8d
	callq	reopt.SystemCall.Linux
	cmpq	$-38, %rax
	jne	.LBB0_3
# BB#2:                                 # %block_402abc
	movq	$202, (%rsp)
	callq	reopt.SystemCall.Linux
.LBB0_3:                                # %block_402ac4
	movd	%r14, %xmm0
	movd	%rbx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%edx, %edx
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5:
	.size	F402a80, .Ltmp5-F402a80
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
