	.text
	.file	"__ofl_lock_40217e.ll"
	.globl	F40217e
	.align	16, 0x90
	.type	F40217e,@function
F40217e:                                # @F40217e
	.cfi_startproc
# BB#0:                                 # %block_402189
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movl	$6309624, %edi          # imm = 0x6046F8
	callq	F4023a7
	movq	16(%rsp), %rdx
	movl	$6309632, %eax          # imm = 0x604700
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F40217e, .Ltmp1-F40217e
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
