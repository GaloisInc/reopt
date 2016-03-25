	.text
	.file	"__funcs_on_exit_40083c.ll"
	.globl	F40083c
	.align	16, 0x90
	.type	F40083c,@function
F40083c:                                # @F40083c
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
	.size	F40083c, .Ltmp0-F40083c
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
