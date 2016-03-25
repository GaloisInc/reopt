	.text
	.file	"__munmap_401c4a.ll"
	.globl	F401c4a
	.align	16, 0x90
	.type	F401c4a,@function
F401c4a:                                # @F401c4a
	.cfi_startproc
# BB#0:                                 # %block_401c74
	subq	$40, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 48
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	callq	F401b1f
	movq	16(%rsp), %rsi
	movq	24(%rsp), %rdi
	movq	$11, (%rsp)
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	addq	$40, %rsp
	retq
.Ltmp1:
	.size	F401c4a, .Ltmp1-F401c4a
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
