	.text
	.file	"fgetc_401e7c.ll"
	.globl	F401e7c
	.align	16, 0x90
	.type	F401e7c,@function
F401e7c:                                # @F401e7c
	.cfi_startproc
# BB#0:                                 # %block_401e7c
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 48
	subq	$144, %rsp
.Ltmp5:
	.cfi_def_cfa_offset 192
.Ltmp6:
	.cfi_offset %rbx, -48
.Ltmp7:
	.cfi_offset %r12, -40
.Ltmp8:
	.cfi_offset %r13, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movq	%r9, %r14
	movq	%r8, %r15
	movq	%rdx, %r12
	movq	%rdi, %rbx
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	leaq	120(%rsp), %r13
	movq	%rsi, 120(%rsp)
	cmpl	$0, 140(%rbx)
	js	.LBB0_1
# BB#3:                                 # %block_401ea8
	movaps	%xmm1, (%rsp)           # 16-byte Spill
	movdqa	%xmm2, 16(%rsp)         # 16-byte Spill
	movdqa	%xmm3, 32(%rsp)         # 16-byte Spill
	movdqa	%xmm4, 48(%rsp)         # 16-byte Spill
	movdqa	%xmm5, 64(%rsp)         # 16-byte Spill
	movdqa	%xmm6, 80(%rsp)         # 16-byte Spill
	movdqa	%xmm7, 96(%rsp)         # 16-byte Spill
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	callq	F402a2c
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testl	%eax, %eax
	je	.LBB0_4
# BB#6:                                 # %block_401eb9
	movq	8(%rbx), %rdi
	cmpq	%rdi, 16(%rbx)
	ja	.LBB0_7
# BB#8:                                 # %block_401ed8
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	callq	F402d34
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	xorl	%r14d, %r14d
                                        # implicit-def: RDX
	jmp	.LBB0_9
.LBB0_4:
	movdqa	96(%rsp), %xmm7         # 16-byte Reload
	movdqa	80(%rsp), %xmm6         # 16-byte Reload
	movdqa	64(%rsp), %xmm5         # 16-byte Reload
	movdqa	48(%rsp), %xmm4         # 16-byte Reload
	movdqa	32(%rsp), %xmm3         # 16-byte Reload
	movdqa	16(%rsp), %xmm2         # 16-byte Reload
	movdqa	(%rsp), %xmm1           # 16-byte Reload
.LBB0_1:                                # %block_401e8c
	movq	8(%rbx), %rdx
	cmpq	%rdx, 16(%rbx)
	ja	.LBB0_2
# BB#5:                                 # %block_401eae
	movd	%rax, %xmm0
	movd	%rcx, %xmm8
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
	movq	(%r13), %rcx
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movq	%rbx, %rdi
	movq	%r12, %rdx
	movq	%r15, %r8
	movq	%r14, %r9
	callq	F402d34
	jmp	.LBB0_11
.LBB0_2:                                # %block_401e96
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rbx)
	movzbl	(%rdx), %r14d
	jmp	.LBB0_10
.LBB0_7:                                # %block_401ec3
	leaq	1(%rdi), %rdx
	movq	%rdx, 8(%rbx)
	movzbl	(%rdi), %r14d
.LBB0_9:                                # %block_401ee2
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	callq	F402a80
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_10:                               # %block_401ee4
	movq	(%r13), %rdx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r14, %rax
.LBB0_11:                               # %block_401ee4
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp11:
	.size	F401e7c, .Ltmp11-F401e7c
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
