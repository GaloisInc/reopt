	.text
	.file	"__errno_location_402681.ll"
	.globl	F402681
	.align	16, 0x90
	.type	F402681,@function
F402681:                                # @F402681
	.cfi_startproc
# BB#0:                                 # %entry
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movq	(%rax), %rax
	addq	$68, %rax
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	retq
.Ltmp0:
	.size	F402681, .Ltmp0-F402681
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
