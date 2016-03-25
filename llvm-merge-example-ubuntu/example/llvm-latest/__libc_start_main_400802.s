	.text
	.file	"__libc_start_main_400802.ll"
	.globl	F400802
	.align	16, 0x90
	.type	F400802,@function
F400802:                                # @F400802
	.cfi_startproc
# BB#0:                                 # %block_40083c
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
	subq	$56, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 96
.Ltmp5:
	.cfi_offset %rbx, -40
.Ltmp6:
	.cfi_offset %r12, -32
.Ltmp7:
	.cfi_offset %r14, -24
.Ltmp8:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rdi, %r14
	movslq	%esi, %r12
	movq	(%rbx), %rsi
	leaq	8(%rbx,%r12,8), %r15
	movq	%r15, %rdi
	callq	F40068c
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F4007e2
	movl	%r12d, %edi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	*%r14
	xorl	%edi, %edi
	movq	%rdx, %rsi
	callq	F400130
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp9:
	.size	F400802, .Ltmp9-F400802
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
