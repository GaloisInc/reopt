	.text
	.file	"__lock_4023a7.ll"
	.globl	F4023a7
	.align	16, 0x90
	.type	F4023a7,@function
F4023a7:                                # @F4023a7
	.cfi_startproc
# BB#0:                                 # %block_4023a7
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
	movq	%rdi, %rbx
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	cmpl	$0, 6310284
	je	.LBB0_4
# BB#1:                                 # %block_4023b2
	leaq	4(%rbx), %r14
	movq	%rdx, 16(%rsp)
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_3:                                # %block_4023c7
                                        #   in Loop: Header=BB0_2 Depth=1
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$1, %edx
	movl	$1, %ecx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	F40241c
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_2:                                # %block_4023bc
                                        # =>This Inner Loop Header: Depth=1
	cmpl	$0, (%rbx)
	movl	$1, (%rbx)
	jne	.LBB0_3
.LBB0_4:                                # %block_4023b1
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$40, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5:
	.size	F4023a7, .Ltmp5-F4023a7
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
