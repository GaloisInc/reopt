	.text
	.file	"__copy_tls_4024e2.ll"
	.globl	F4024e2
	.align	16, 0x90
	.type	F4024e2,@function
F4024e2:                                # @F4024e2
	.cfi_startproc
# BB#0:                                 # %block_4024e2
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp3:
	.cfi_def_cfa_offset 40
	subq	$56, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 96
.Ltmp5:
	.cfi_offset %rbx, -40
.Ltmp6:
	.cfi_offset %r12, -32
.Ltmp7:
	.cfi_offset %r14, -24
.Ltmp8:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rax
	movq	%rcx, 16(%rsp)
	movq	6310296, %rbx
	movq	6310304, %rcx
	leaq	-336(%r14,%rcx), %rcx
	xorl	%r15d, %r15d
	subq	6310312, %r15
	andq	%rcx, %r15
	leaq	8(%r14), %r12
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                # %block_402534
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r15, %rdi
	subq	40(%rbx), %rdi
	movq	16(%rbx), %rdx
	movq	%rdi, (%r12)
	movq	8(%rbx), %rsi
	movd	%r8, %xmm0
	movd	%rax, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402375
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rax
	movq	(%rbx), %rbx
	addq	$8, %r12
.LBB0_1:                                # %block_402513
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB0_2
# BB#3:                                 # %block_40253a
	movq	6310320, %rax
	movq	%rax, (%r14)
	movq	%r14, 328(%r15)
	movq	%r14, 8(%r15)
	movq	%r15, %rax
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp9:
	.size	F4024e2, .Ltmp9-F4024e2
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
