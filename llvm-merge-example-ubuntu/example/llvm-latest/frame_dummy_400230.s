	.text
	.file	"frame_dummy_400230.ll"
	.globl	F400230
	.align	16, 0x90
	.type	F400230,@function
F400230:                                # @F400230
	.cfi_startproc
# BB#0:                                 # %block_40023b
	pushq	%rax
.Ltmp0:
	.cfi_def_cfa_offset 16
	callq	F4001d0
	popq	%rcx
	retq
.Ltmp1:
	.size	F400230, .Ltmp1-F400230
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
