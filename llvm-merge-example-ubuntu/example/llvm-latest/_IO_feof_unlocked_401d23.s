	.text
	.file	"_IO_feof_unlocked_401d23.ll"
	.globl	F401d23
	.align	16, 0x90
	.type	F401d23,@function
F401d23:                                # @F401d23
	.cfi_startproc
# BB#0:                                 # %block_401d23
	pushq	%rbp
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r15
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r14
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%r13
.Ltmp3:
	.cfi_def_cfa_offset 40
	pushq	%r12
.Ltmp4:
	.cfi_def_cfa_offset 48
	pushq	%rbx
.Ltmp5:
	.cfi_def_cfa_offset 56
	subq	$40, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 96
.Ltmp7:
	.cfi_offset %rbx, -56
.Ltmp8:
	.cfi_offset %r12, -48
.Ltmp9:
	.cfi_offset %r13, -40
.Ltmp10:
	.cfi_offset %r14, -32
.Ltmp11:
	.cfi_offset %r15, -24
.Ltmp12:
	.cfi_offset %rbp, -16
	movq	%r9, %r14
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdi, %r13
	movd	%xmm0, %rdi
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	leaq	16(%rsp), %rbp
	movq	%r12, 16(%rsp)
	movl	140(%r13), %edx
	testl	%edx, %edx
	js	.LBB0_2
# BB#1:                                 # %block_401d35
	movd	%rdi, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r13, %rdi
	callq	F402a2c
	movq	%rdx, %rsi
	movd	%xmm0, %rdi
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
                                        # implicit-def: RDX
.LBB0_2:                                # %block_401d3a
	movl	(%r13), %ebx
	shrl	$4, %ebx
	andl	$1, %ebx
	movb	$1, %al
	testb	%al, %al
	jne	.LBB0_4
# BB#3:                                 # %block_401d47
	movd	%rdi, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r13, %rdi
	movq	%r12, %rcx
	movq	%r15, %r8
	movq	%r14, %r9
	callq	F402a80
	movd	%xmm0, %rdi
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_4:                                # %block_401d4f
	movq	(%rbp), %rdx
	movd	%rdi, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F401d23, .Ltmp13-F401d23
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
