	.text
	.file	"fopen64_401ee8.ll"
	.globl	F401ee8
	.align	16, 0x90
	.type	F401ee8,@function
F401ee8:                                # @F401ee8
	.cfi_startproc
# BB#0:                                 # %block_401f02
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 32
	subq	$64, %rsp
.Ltmp3:
	.cfi_def_cfa_offset 96
.Ltmp4:
	.cfi_offset %rbx, -32
.Ltmp5:
	.cfi_offset %r14, -24
.Ltmp6:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	leaq	24(%rsp), %r15
	movq	%rcx, 24(%rsp)
	movsbl	(%rbx), %esi
	movl	$4206466, %edi          # imm = 0x402F82
	callq	F4021a0
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	testq	%rax, %rax
	je	.LBB0_1
# BB#2:                                 # %block_401f3f
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	callq	F4029ad
	movq	$2, (%rsp)
	xorl	%esi, %esi
	movl	$438, %edx              # imm = 0x1B6
	movq	%r14, %rdi
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testl	%eax, %eax
	js	.LBB0_8
# BB#3:                                 # %block_401f48
	movb	$1, %dl
	testb	%dl, %dl
	jne	.LBB0_5
# BB#4:                                 # %block_401f51
	movq	$72, (%rsp)
	xorl	%edi, %edi
	movl	$2, %esi
	movl	$1, %edx
	callq	reopt.SystemCall.Linux
                                        # implicit-def: RAX
                                        # implicit-def: RCX
.LBB0_5:                                # %block_401f6d
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%edi, %edi
	movq	%rbx, %rsi
	callq	F40284f
	testq	%rax, %rax
	je	.LBB0_7
# BB#6:
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	jmp	.LBB0_8
.LBB0_1:                                # %block_401f0c
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F402681
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	$22, (%rax)
	jmp	.LBB0_8
.LBB0_7:                                # %block_401f75
	movq	$3, (%rsp)
	xorl	%edi, %edi
	callq	reopt.SystemCall.Linux
                                        # implicit-def: RAX
                                        # implicit-def: RCX
.LBB0_8:                                # %block_401f7f
	movq	(%r15), %rdx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%eax, %eax
	addq	$64, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7:
	.size	F401ee8, .Ltmp7-F401ee8
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
