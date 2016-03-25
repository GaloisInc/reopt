	.text
	.file	"__init_ssp_40068b.ll"
	.globl	F40068b
	.align	16, 0x90
	.type	F40068b,@function
F40068b:                                # @F40068b
	.cfi_startproc
# BB#0:                                 # %entry
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	retq
.Ltmp0:
	.size	F40068b, .Ltmp0-F40068b
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
