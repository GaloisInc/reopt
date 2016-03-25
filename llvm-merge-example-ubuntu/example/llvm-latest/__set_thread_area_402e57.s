	.text
	.file	"__set_thread_area_402e57.ll"
	.globl	F402e57
	.align	16, 0x90
	.type	F402e57,@function
F402e57:                                # @F402e57
	.cfi_startproc
# BB#0:                                 # %block_402e66
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movq	%rdi, %rax
	movq	$158, (%rsp)
	movl	$4098, %edi             # imm = 0x1002
	movq	%rax, %rsi
	callq	reopt.SystemCall.Linux
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F402e57, .Ltmp1-F402e57
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
