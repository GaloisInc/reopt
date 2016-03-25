	.text
	.file	"fputs_402023.ll"
	.globl	F402023
	.align	16, 0x90
	.type	F402023,@function
F402023:                                # @F402023
	.cfi_startproc
# BB#0:                                 # %block_402048
	pushq	%r14
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1:
	.cfi_def_cfa_offset 24
	subq	$40, %rsp
.Ltmp2:
	.cfi_def_cfa_offset 64
.Ltmp3:
	.cfi_offset %rbx, -24
.Ltmp4:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	F402300
	movl	$1, %esi
	movq	%rbx, %rdi
	movq	%r14, %rcx
	callq	F402101
	addq	$40, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5:
	.size	F402023, .Ltmp5-F402023
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
