	.text
	.file	"strchr_4021a0.ll"
	.globl	F4021a0
	.align	16, 0x90
	.type	F4021a0,@function
F4021a0:                                # @F4021a0
	.cfi_startproc
# BB#0:                                 # %block_4021a8
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
.Ltmp2:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
                                        # kill: RSI<def> RBX<kill>
	callq	F4021c0
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movzbl	(%rax), %edx
	movzbl	%bl, %esi
	cmpl	%esi, %edx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%eax, %eax
	xorl	%edx, %edx
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F4021a0, .Ltmp3-F4021a0
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
