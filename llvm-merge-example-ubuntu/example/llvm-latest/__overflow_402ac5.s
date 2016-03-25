	.text
	.file	"__overflow_402ac5.ll"
	.globl	F402ac5
	.align	16, 0x90
	.type	F402ac5,@function
F402ac5:                                # @F402ac5
	.cfi_startproc
# BB#0:                                 # %block_402ac5
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
	movdqa	%xmm7, 96(%rsp)         # 16-byte Spill
	movdqa	%xmm6, 80(%rsp)         # 16-byte Spill
	movdqa	%xmm5, 64(%rsp)         # 16-byte Spill
	movdqa	%xmm4, 48(%rsp)         # 16-byte Spill
	movdqa	%xmm3, 32(%rsp)         # 16-byte Spill
	movdqa	%xmm2, 16(%rsp)         # 16-byte Spill
	movdqa	%xmm1, (%rsp)           # 16-byte Spill
	movq	%r9, %r14
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rdi, %rbx
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	leaq	120(%rsp), %r13
	cmpq	$0, 32(%rbx)
	movb	%sil, 135(%rsp)
	jne	.LBB0_1
# BB#7:                                 # %block_402aea
	movd	%rcx, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	callq	F402ce8
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	testl	%eax, %eax
                                        # implicit-def: RDX
	jne	.LBB0_6
.LBB0_1:                                # %block_402ad9
	movq	40(%rbx), %rdx
	cmpq	32(%rbx), %rdx
	jae	.LBB0_4
# BB#2:                                 # %block_402af3
	movzbl	15(%r13), %eax
	movsbl	139(%rbx), %esi
	cmpl	%esi, %eax
	jne	.LBB0_3
.LBB0_4:                                # %block_402b1f
	movd	%rcx, %xmm0
	movd	%rdi, %xmm8
	movdqa	(%rsp), %xmm1           # 16-byte Reload
	movd	%xmm1, %rax
	pshufd	$78, %xmm1, %xmm2       # xmm2 = xmm1[2,3,0,1]
	movd	%rax, %xmm1
	movd	%xmm2, %rax
	movd	%rax, %xmm9
	movdqa	16(%rsp), %xmm2         # 16-byte Reload
	movd	%xmm2, %rax
	pshufd	$78, %xmm2, %xmm3       # xmm3 = xmm2[2,3,0,1]
	movd	%rax, %xmm2
	movd	%xmm3, %rax
	movd	%rax, %xmm10
	movdqa	32(%rsp), %xmm3         # 16-byte Reload
	movd	%xmm3, %rax
	pshufd	$78, %xmm3, %xmm4       # xmm4 = xmm3[2,3,0,1]
	movd	%rax, %xmm3
	movd	%xmm4, %rax
	movd	%rax, %xmm11
	movdqa	48(%rsp), %xmm4         # 16-byte Reload
	movd	%xmm4, %rax
	pshufd	$78, %xmm4, %xmm5       # xmm5 = xmm4[2,3,0,1]
	movd	%rax, %xmm4
	movd	%xmm5, %rax
	movd	%rax, %xmm12
	movdqa	64(%rsp), %xmm5         # 16-byte Reload
	movd	%xmm5, %rax
	pshufd	$78, %xmm5, %xmm6       # xmm6 = xmm5[2,3,0,1]
	movd	%rax, %xmm5
	movd	%xmm6, %rax
	movd	%rax, %xmm13
	movdqa	80(%rsp), %xmm6         # 16-byte Reload
	movd	%xmm6, %rax
	pshufd	$78, %xmm6, %xmm7       # xmm7 = xmm6[2,3,0,1]
	movd	%rax, %xmm6
	movd	%xmm7, %rax
	movd	%rax, %xmm14
	movdqa	96(%rsp), %xmm7         # 16-byte Reload
	movd	%xmm7, %rax
	pshufd	$78, %xmm7, %xmm15      # xmm15 = xmm7[2,3,0,1]
	movd	%rax, %xmm7
	movd	%xmm15, %rax
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	movd	%rax, %xmm8
	leaq	15(%r13), %rsi
	punpcklqdq	%xmm8, %xmm7    # xmm7 = xmm7[0],xmm8[0]
	movl	$1, %edx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	movq	%r15, %r8
	movq	%r14, %r9
	callq	*72(%rbx)
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	cmpq	$1, %rax
	jne	.LBB0_6
# BB#5:                                 # %block_402b24
	movzbl	15(%r13), %eax
                                        # implicit-def: RDX
	jmp	.LBB0_6
.LBB0_3:                                # %block_402b03
	leaq	1(%rdx), %rsi
	movq	%rsi, 40(%rbx)
	movb	%al, (%rdx)
.LBB0_6:                                # %block_402b29
	movd	%rcx, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp11:
	.size	F402ac5, .Ltmp11-F402ac5
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
