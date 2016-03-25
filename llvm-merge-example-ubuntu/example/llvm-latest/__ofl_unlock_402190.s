	.text
	.file	"__ofl_unlock_402190.ll"
	.globl	F402190
	.align	16, 0x90
	.type	F402190,@function
F402190:                                # @F402190
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp0:
	.cfi_def_cfa_offset 16
.Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp2:
	.cfi_def_cfa_register %rbp
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movd	%rax, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$6309624, %edi          # imm = 0x6046F8
	callq	F4023e2
	movq	%rbp, %rsp
	popq	%rbp
	retq
.Ltmp3:
	.size	F402190, .Ltmp3-F402190
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
