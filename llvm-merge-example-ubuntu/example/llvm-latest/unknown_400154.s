	.text
	.file	"unknown_400154.ll"
	.globl	F400154
	.align	16, 0x90
	.type	F400154,@function
F400154:                                # @F400154
	.cfi_startproc
# BB#0:                                 # %block_40018a
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	leaq	24(%rsp), %rdi
	callq	F40016a
	movq	(%rax), %rsi
	leaq	8(%rax), %rdx
	movl	$F400603, %edi
	callq	F400802
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F400190
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F400154, .Ltmp1-F400154
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
