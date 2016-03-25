	.text
	.file	"__do_global_dtors_aux_400210.ll"
	.globl	F400210
	.align	16, 0x90
	.type	F400210,@function
F400210:                                # @F400210
	.cfi_startproc
# BB#0:                                 # %block_400210
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	cmpb	$0, 6308000
	jne	.LBB0_2
# BB#1:                                 # %block_400222
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F400190
	movb	$1, 6308000
.LBB0_2:                                # %block_40022a
	addq	$24, %rsp
	retq
.Ltmp1:
	.size	F400210, .Ltmp1-F400210
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
