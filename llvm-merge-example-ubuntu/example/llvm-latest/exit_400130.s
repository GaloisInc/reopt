	.text
	.file	"exit_400130.ll"
	.globl	F400130
	.align	16, 0x90
	.type	F400130,@function
F400130:                                # @F400130
	.cfi_startproc
# BB#0:                                 # %block_40018a
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 48
.Ltmp2:
	.cfi_offset %rbx, -16
	movl	%edi, 20(%rsp)
	leaq	8(%rsp), %rbx
	callq	F40083c
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F40083d
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F402eb4
	movl	20(%rsp), %edi
	callq	F40268f
	movq	%rbx, %rdi
	callq	F40016a
	movq	(%rax), %rsi
	leaq	8(%rax), %rdx
	movl	$F400603, %edi
	callq	F400802
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F400190
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F400130, .Ltmp3-F400130
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
