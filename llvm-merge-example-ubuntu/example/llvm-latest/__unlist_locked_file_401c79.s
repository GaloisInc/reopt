	.text
	.file	"__unlist_locked_file_401c79.ll"
	.globl	F401c79
	.align	16, 0x90
	.type	F401c79,@function
F401c79:                                # @F401c79
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
	.size	F401c79, .Ltmp0-F401c79
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
