	.text
	.file	"fputc_401f8a.ll"
	.globl	F401f8a
	.align	16, 0x90
	.type	F401f8a,@function
F401f8a:                                # @F401f8a
	.cfi_startproc
# BB#0:                                 # %block_401f8a
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
	subq	$152, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 208
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
	movq	%rsi, %rbx
	movd	%xmm0, %r10
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbp
	movl	%edi, %r14d
	movzbl	%dil, %r15d
	cmpl	$0, 140(%rbx)
	js	.LBB0_1
# BB#4:                                 # %block_401fb9
	movaps	%xmm1, (%rsp)           # 16-byte Spill
	movdqa	%xmm2, 16(%rsp)         # 16-byte Spill
	movdqa	%xmm3, 32(%rsp)         # 16-byte Spill
	movdqa	%xmm4, 48(%rsp)         # 16-byte Spill
	movdqa	%xmm5, 64(%rsp)         # 16-byte Spill
	movdqa	%xmm6, 80(%rsp)         # 16-byte Spill
	movdqa	%xmm7, 96(%rsp)         # 16-byte Spill
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	movq	%r8, %r12
	movq	%r9, %r13
	movd	%r10, %xmm0
	movd	%rbp, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	callq	F402a2c
	movd	%xmm0, %r10
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbp
	testl	%eax, %eax
	je	.LBB0_5
# BB#7:                                 # %block_401fe5
	movsbl	139(%rbx), %eax
	cmpl	%eax, %r15d
	je	.LBB0_10
# BB#8:                                 # %block_401ff0
	movq	40(%rbx), %rax
	cmpq	%rax, 32(%rbx)
	ja	.LBB0_9
.LBB0_10:                               # %block_402012
	movd	%r10, %xmm0
	movd	%rbp, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	F402ac5
	movd	%xmm0, %r10
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbp
	xorl	%r15d, %r15d
                                        # implicit-def: RAX
                                        # implicit-def: RCX
	jmp	.LBB0_11
.LBB0_5:
	movq	%r13, %r9
	movq	%r12, %r8
	movq	112(%rsp), %rcx         # 8-byte Reload
	movdqa	96(%rsp), %xmm7         # 16-byte Reload
	movdqa	80(%rsp), %xmm6         # 16-byte Reload
	movdqa	64(%rsp), %xmm5         # 16-byte Reload
	movdqa	48(%rsp), %xmm4         # 16-byte Reload
	movdqa	32(%rsp), %xmm3         # 16-byte Reload
	movdqa	16(%rsp), %xmm2         # 16-byte Reload
	movdqa	(%rsp), %xmm1           # 16-byte Reload
.LBB0_1:                                # %block_401fa2
	movsbl	139(%rbx), %edx
	cmpl	%edx, %r15d
	je	.LBB0_6
# BB#2:                                 # %block_401fbf
	movq	40(%rbx), %rdx
	cmpq	%rdx, 32(%rbx)
	ja	.LBB0_3
.LBB0_6:                                # %block_401fd6
	movd	%r10, %xmm0
	movd	%rbp, %xmm8
	movd	%xmm1, %rax
	pshufd	$78, %xmm1, %xmm9       # xmm9 = xmm1[2,3,0,1]
	movd	%rax, %xmm1
	movd	%xmm9, %rax
	movd	%rax, %xmm9
	movd	%xmm2, %rax
	pshufd	$78, %xmm2, %xmm10      # xmm10 = xmm2[2,3,0,1]
	movd	%rax, %xmm2
	movd	%xmm10, %rax
	movd	%rax, %xmm10
	movd	%xmm3, %rax
	pshufd	$78, %xmm3, %xmm11      # xmm11 = xmm3[2,3,0,1]
	movd	%rax, %xmm3
	movd	%xmm11, %rax
	movd	%rax, %xmm11
	movd	%xmm4, %rax
	pshufd	$78, %xmm4, %xmm12      # xmm12 = xmm4[2,3,0,1]
	movd	%rax, %xmm4
	movd	%xmm12, %rax
	movd	%rax, %xmm12
	movd	%xmm5, %rax
	pshufd	$78, %xmm5, %xmm13      # xmm13 = xmm5[2,3,0,1]
	movd	%rax, %xmm5
	movd	%xmm13, %rax
	movd	%rax, %xmm13
	movd	%xmm6, %rax
	pshufd	$78, %xmm6, %xmm14      # xmm14 = xmm6[2,3,0,1]
	movd	%rax, %xmm6
	movd	%xmm14, %rax
	movd	%rax, %xmm14
	movd	%xmm7, %rax
	pshufd	$78, %xmm7, %xmm15      # xmm15 = xmm7[2,3,0,1]
	movd	%rax, %xmm7
	movd	%xmm15, %rax
	movd	%rax, %xmm15
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	F402ac5
	jmp	.LBB0_13
.LBB0_3:                                # %block_401fc9
	leaq	1(%rdx), %rax
	movq	%rax, 40(%rbx)
	movb	%r14b, (%rdx)
	jmp	.LBB0_12
.LBB0_9:                                # %block_401ffa
	leaq	1(%rax), %rcx
	movq	%rcx, 40(%rbx)
	movb	%r14b, (%rax)
.LBB0_11:                               # %block_40201c
	movd	%r10, %xmm0
	movd	%rbp, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	F402a80
	movd	%xmm0, %r10
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbp
                                        # implicit-def: RDX
.LBB0_12:                               # %block_40201e
	movd	%r10, %xmm0
	movd	%rbp, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rax
.LBB0_13:                               # %block_40201e
	addq	$152, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F401f8a, .Ltmp13-F401f8a
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
