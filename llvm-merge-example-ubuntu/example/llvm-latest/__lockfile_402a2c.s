	.text
	.file	"__lockfile_402a2c.ll"
	.globl	F402a2c
	.align	16, 0x90
	.type	F402a2c,@function
F402a2c:                                # @F402a2c
	.cfi_startproc
# BB#0:                                 # %block_402a2c
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
	subq	$40, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 80
.Ltmp5:
	.cfi_offset %rbx, -40
.Ltmp6:
	.cfi_offset %r12, -32
.Ltmp7:
	.cfi_offset %r14, -24
.Ltmp8:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movq	(%rax), %rdx
	movl	56(%rdx), %r12d
	movl	140(%r15), %edx
	xorl	%r14d, %r14d
	cmpl	%edx, %r12d
	je	.LBB0_7
# BB#1:                                 # %block_402a48
	leaq	140(%r15), %rbx
	movl	$1, %r14d
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_6:                                # %block_402a60
                                        #   in Loop: Header=BB0_2 Depth=1
	leaq	144(%r15), %rsi
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$1, %ecx
	movq	%rbx, %rdi
	callq	F40241c
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_2:                                # %block_402a52
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %edx
	testq	%rdx, %rdx
	je	.LBB0_3
# BB#4:                                 # %subblock_402a52_2
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	%edx, (%rbx)
	jmp	.LBB0_5
	.align	16, 0x90
.LBB0_3:                                # %subblock_402a52_1
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	%r12d, (%rbx)
	xorl	%edx, %edx
.LBB0_5:                                # %block_402a5a
                                        #   in Loop: Header=BB0_2 Depth=1
	testl	%edx, %edx
	jne	.LBB0_6
.LBB0_7:                                # %block_402a7b
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r14, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp9:
	.size	F402a2c, .Ltmp9-F402a2c
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
