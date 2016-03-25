	.text
	.file	"fflush_unlocked_401d55.ll"
	.globl	F401d55
	.align	16, 0x90
	.type	F401d55,@function
F401d55:                                # @F401d55
	.cfi_startproc
# BB#0:                                 # %block_401d55
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
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	%r8, 96(%rsp)           # 8-byte Spill
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	movq	%rdi, %rbx
	movd	%xmm0, %r11
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
	movd	%xmm1, %r14
	pshufd	$78, %xmm1, %xmm0       # xmm0 = xmm1[2,3,0,1]
	movd	%xmm0, %r12
	movd	%xmm2, %rdi
	pshufd	$78, %xmm2, %xmm0       # xmm0 = xmm2[2,3,0,1]
	movq	%xmm0, 120(%rsp)        # 8-byte Folded Spill
	movd	%xmm3, %rbp
	pshufd	$78, %xmm3, %xmm0       # xmm0 = xmm3[2,3,0,1]
	movd	%xmm0, %r13
	movd	%xmm4, %rsi
	pshufd	$78, %xmm4, %xmm0       # xmm0 = xmm4[2,3,0,1]
	movq	56(%rbx), %rax
	cmpq	40(%rbx), %rax
	movd	%xmm0, %r10
	pshufd	$78, %xmm5, %xmm0       # xmm0 = xmm5[2,3,0,1]
	pshufd	$78, %xmm6, %xmm1       # xmm1 = xmm6[2,3,0,1]
	pshufd	$78, %xmm7, %xmm2       # xmm2 = xmm7[2,3,0,1]
	movd	%xmm5, %r8
	movd	%xmm0, %rcx
	movd	%xmm6, %r9
	movd	%xmm1, %r15
	movq	%xmm7, 112(%rsp)        # 8-byte Folded Spill
	movd	%xmm2, %rax
	jae	.LBB0_1
# BB#6:                                 # %block_401d87
	movd	%r11, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movd	%r14, %xmm1
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movd	%r12, %xmm2
	punpcklqdq	%xmm2, %xmm1    # xmm1 = xmm1[0],xmm2[0]
	movd	%rdi, %xmm2
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	120(%rsp), %r12         # 8-byte Reload
	movd	%r12, %xmm3
	punpcklqdq	%xmm3, %xmm2    # xmm2 = xmm2[0],xmm3[0]
	movd	%rbp, %xmm3
	movq	%rbp, 72(%rsp)          # 8-byte Spill
	movd	%r13, %xmm4
	movq	%r13, 56(%rsp)          # 8-byte Spill
	punpcklqdq	%xmm4, %xmm3    # xmm3 = xmm3[0],xmm4[0]
	movd	%rsi, %xmm4
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movd	%r10, %xmm5
	punpcklqdq	%xmm5, %xmm4    # xmm4 = xmm4[0],xmm5[0]
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movd	%r8, %xmm5
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movd	%rcx, %xmm6
	punpcklqdq	%xmm6, %xmm5    # xmm5 = xmm5[0],xmm6[0]
	movq	%r9, %r14
	movd	%r14, %xmm6
	movd	%r15, %xmm7
	punpcklqdq	%xmm7, %xmm6    # xmm6 = xmm6[0],xmm7[0]
	movq	112(%rsp), %rbp         # 8-byte Reload
	movd	%rbp, %xmm7
	movd	%rax, %xmm8
	movq	%rax, 40(%rsp)          # 8-byte Spill
	punpcklqdq	%xmm8, %xmm7    # xmm7 = xmm7[0],xmm8[0]
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbx, %rdi
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	96(%rsp), %r8           # 8-byte Reload
	movq	104(%rsp), %r9          # 8-byte Reload
	callq	*72(%rbx)
	movd	%xmm0, %r11
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	cmpq	$0, 40(%rbx)
	movq	%r12, %r13
	movq	32(%rsp), %r12          # 8-byte Reload
	movq	%rbp, %r8
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	16(%rsp), %rbp          # 8-byte Reload
	movq	24(%rsp), %r10          # 8-byte Reload
	jne	.LBB0_2
	jmp	.LBB0_5
.LBB0_1:
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%r13, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	%rbp, 72(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	120(%rsp), %r13         # 8-byte Reload
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%r9, %r14
	movq	%r8, %rbp
	movq	%rax, %r8
.LBB0_2:                                # %block_401d63
	movq	8(%rbx), %rsi
	movq	16(%rbx), %rax
	cmpq	%rsi, %rax
	jbe	.LBB0_4
# BB#3:                                 # %block_401d70
	subq	%rax, %rsi
	movd	%r11, %xmm0
	movd	%rdx, %xmm8
	movq	48(%rsp), %xmm1         # 8-byte Folded Reload
	movd	%r12, %xmm9
	movq	64(%rsp), %xmm2         # 8-byte Folded Reload
	movd	%r13, %xmm10
	movq	72(%rsp), %xmm3         # 8-byte Folded Reload
	movq	56(%rsp), %xmm11        # 8-byte Folded Reload
	movq	80(%rsp), %xmm4         # 8-byte Folded Reload
	movd	%r10, %xmm12
	movd	%rbp, %xmm5
	movd	%rcx, %xmm13
	movd	%r14, %xmm6
	movd	%r15, %xmm14
	movd	%r8, %xmm7
	movq	40(%rsp), %xmm15        # 8-byte Folded Reload
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movl	$1, %edx
	movq	%rbx, %rdi
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	96(%rsp), %r8           # 8-byte Reload
	movq	104(%rsp), %r9          # 8-byte Reload
	callq	*80(%rbx)
	movd	%xmm0, %r11
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
.LBB0_4:                                # %block_401d93
	movq	$0, 32(%rbx)
	movq	$0, 56(%rbx)
	movq	$0, 40(%rbx)
	pxor	%xmm0, %xmm0
	movdqu	%xmm0, 8(%rbx)
	xorl	%eax, %eax
.LBB0_5:                                # %block_401dbd
	movd	%r11, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	128(%rsp), %rdx         # 8-byte Reload
	addq	$152, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F401d55, .Ltmp13-F401d55
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
