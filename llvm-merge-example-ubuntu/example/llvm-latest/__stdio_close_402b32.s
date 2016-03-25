	.text
	.file	"__stdio_close_402b32.ll"
	.globl	F402b32
	.align	16, 0x90
	.type	F402b32,@function
F402b32:                                # @F402b32
	.cfi_startproc
# BB#0:                                 # %block_402b4d
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movl	120(%rdi), %edi
	callq	F402b2f
	movq	$3, (%rsp)
	xorl	%edi, %edi
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F402b32, .Ltmp1-F402b32
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
