	.text
	.file	"__stdio_read_402b4f.ll"
	.globl	F402b4f
	.align	16, 0x90
	.type	F402b4f,@function
F402b4f:                                # @F402b4f
	.cfi_startproc
# BB#0:                                 # %block_402ba4
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp3:
	.cfi_def_cfa_offset 40
	subq	$72, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 112
.Ltmp5:
	.cfi_offset %rbx, -40
.Ltmp6:
	.cfi_offset %r12, -32
.Ltmp7:
	.cfi_offset %r14, -24
.Ltmp8:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	leaq	16(%rsp), %r12
	movq	96(%rbx), %rax
	movq	%r15, 16(%rsp)
	movq	%rax, 40(%rsp)
	cmpq	$1, %rax
	movq	%r14, %rax
	adcq	$-1, %rax
	movq	88(%rbx), %rcx
	movslq	120(%rbx), %rdi
	movq	%rax, 24(%rsp)
	movq	%rcx, 32(%rsp)
	movq	$19, (%rsp)
	movl	$2, %edx
	movq	%r12, %rsi
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	testq	%rax, %rax
	je	.LBB0_2
# BB#1:                                 # %block_402ba4
	movb	$1, %al
	testb	%al, %al
	je	.LBB0_2
# BB#3:                                 # %block_402bb5
	movq	8(%r12), %rax
	cmpq	%rax, %rax
	jbe	.LBB0_6
# BB#4:                                 # %block_402bbf
	movq	88(%rbx), %rax
	cmpq	$0, 96(%rbx)
	movq	%rax, 8(%rbx)
	je	.LBB0_6
# BB#5:                                 # %block_402bdb
	leaq	1(%rax), %rcx
	movq	%rcx, 8(%rbx)
	movb	(%rax), %al
	movb	%al, -1(%r15,%r14)
	jmp	.LBB0_6
.LBB0_2:                                # %block_402ba9
	orb	$16, (%rbx)
.LBB0_6:                                # %block_402bea
	addq	$72, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp9:
	.size	F402b4f, .Ltmp9-F402b4f
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
