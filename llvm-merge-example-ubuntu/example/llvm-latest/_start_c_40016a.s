	.text
	.file	"_start_c_40016a.ll"
	.globl	F40016a
	.align	16, 0x90
	.type	F40016a,@function
F40016a:                                # @F40016a
	.cfi_startproc
# BB#0:                                 # %block_40018a
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movq	(%rdi), %rsi
	leaq	8(%rdi), %rdx
	movl	$F400603, %edi
	callq	F400802
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F400190
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F40016a, .Ltmp1-F40016a
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
