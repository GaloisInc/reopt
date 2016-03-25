	.text
	.file	"fclose_401c7a.ll"
	.globl	F401c7a
	.align	16, 0x90
	.type	F401c7a,@function
F401c7a:                                # @F401c7a
	.cfi_startproc
# BB#0:                                 # %block_401c7a
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp3:
	.cfi_def_cfa_offset 80
.Ltmp4:
	.cfi_offset %rbx, -32
.Ltmp5:
	.cfi_offset %r14, -24
.Ltmp6:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rdi, %r15
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
	movq	%rcx, 8(%rsp)
	cmpl	$0, 140(%r15)
	js	.LBB0_2
# BB#1:                                 # %block_401c96
	movd	%rax, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	callq	F402a2c
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
.LBB0_2:                                # %block_401ca1
	movd	%rax, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	F401c79
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movl	(%r15), %r14d
	testb	$1, %r14b
	jne	.LBB0_10
# BB#3:                                 # %block_401cad
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F40217e
	movq	104(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB0_5
# BB#4:                                 # %block_401cb6
	movq	112(%r15), %rsi
	movq	%rsi, 112(%rcx)
.LBB0_5:                                # %block_401cbe
	pshufd	$78, %xmm0, %xmm1       # xmm1 = xmm0[2,3,0,1]
	movq	112(%r15), %r8
	testq	%r8, %r8
	je	.LBB0_7
# BB#6:                                 # %block_401cc7
	movq	%rcx, 104(%r8)
.LBB0_7:                                # %block_401ccb
	movd	%xmm0, %rsi
	movd	%xmm1, %rdi
	cmpq	(%rax), %r15
	jne	.LBB0_9
# BB#8:                                 # %block_401cd0
	movq	%r8, (%rax)
.LBB0_9:                                # %block_401cd3
	movd	%rsi, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%r8, %rdx
	callq	F402190
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
.LBB0_10:                               # %block_401ce9
	andq	$1, %r14
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	F401dbf
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	*24(%r15)
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movq	168(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB0_12
# BB#11:                                # %block_401cf8
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rsi
	callq	F400e40
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_12:                               # %block_401cfd
	testl	%r14d, %r14d
	je	.LBB0_13
# BB#14:                                # %block_401d0b
	movb	$1, %bl
	testb	%bl, %bl
	jne	.LBB0_17
# BB#15:                                # %block_401d10
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	F402a80
	jmp	.LBB0_16
.LBB0_13:                               # %block_401d01
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	F400e40
.LBB0_16:                               # %block_401d18
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_17:                               # %block_401d18
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7:
	.size	F401c7a, .Ltmp7-F401c7a
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
