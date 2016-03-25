	.text
	.file	"__stdio_seek_402bf3.ll"
	.globl	F402bf3
	.align	16, 0x90
	.type	F402bf3,@function
F402bf3:                                # @F402bf3
	.cfi_startproc
# BB#0:                                 # %block_402c01
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movslq	120(%rdi), %rdi
	movslq	%edx, %rdx
	movq	$8, (%rsp)
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F402bf3, .Ltmp1-F402bf3
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
