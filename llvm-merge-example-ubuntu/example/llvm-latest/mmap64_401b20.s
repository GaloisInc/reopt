	.text
	.file	"mmap64_401b20.ll"
	.globl	F401b20
	.align	16, 0x90
	.type	F401b20,@function
F401b20:                                # @F401b20
	.cfi_startproc
# BB#0:                                 # %block_401b20
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp3:
	.cfi_def_cfa_offset 112
.Ltmp4:
	.cfi_offset %rbx, -32
.Ltmp5:
	.cfi_offset %r14, -24
.Ltmp6:
	.cfi_offset %r15, -16
	movd	%xmm0, %rbx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r10
	testw	$4095, %r9w             # imm = 0xFFF
	je	.LBB0_3
# BB#1:                                 # %block_401b34
	movd	%rbx, %xmm0
	movd	%r10, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402681
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	$22, (%rax)
	jmp	.LBB0_2
.LBB0_3:                                # %block_401b3c
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	cmpq	%rax, %rsi
	jae	.LBB0_4
# BB#5:                                 # %block_401b58
	testb	$16, %cl
	movl	%edx, %r14d
	movl	%ecx, %r15d
	je	.LBB0_7
# BB#6:                                 # %block_401b79
	movq	%r9, 48(%rsp)
	movl	%r8d, 44(%rsp)
	movq	%rsi, 32(%rsp)
	movq	%rdi, 24(%rsp)
	movd	%rbx, %xmm0
	movd	%r10, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F401b1f
	movq	48(%rsp), %r9
	movl	44(%rsp), %r8d
	movq	24(%rsp), %rdi
	movq	32(%rsp), %rsi
.LBB0_7:                                # %block_401b9c
	movslq	%r15d, %rcx
	movslq	%r8d, %r8
	movslq	%r14d, %rdx
	movq	$9, (%rsp)
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	jmp	.LBB0_8
.LBB0_4:                                # %block_401b50
	movd	%rbx, %xmm0
	movd	%r10, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402681
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	$12, (%rax)
.LBB0_2:                                # %block_401baa
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	$-1, %rax
.LBB0_8:                                # %block_401baa
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7:
	.size	F401b20, .Ltmp7-F401b20
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
