	.text
	.file	"__madvise_401b0a.ll"
	.globl	F401b0a
	.align	16, 0x90
	.type	F401b0a,@function
F401b0a:                                # @F401b0a
	.cfi_startproc
# BB#0:                                 # %block_401b1d
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movslq	%edx, %rdx
	movq	$28, (%rsp)
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	movq	16(%rsp), %rdx
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F401b0a, .Ltmp1-F401b0a
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
