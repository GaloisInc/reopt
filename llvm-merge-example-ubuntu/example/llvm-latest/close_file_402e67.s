	.text
	.file	"close_file_402e67.ll"
	.globl	F402e67
	.align	16, 0x90
	.type	F402e67,@function
F402e67:                                # @F402e67
	.cfi_startproc
# BB#0:                                 # %block_402e67
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
	subq	$264, %rsp              # imm = 0x108
.Ltmp6:
	.cfi_def_cfa_offset 320
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
	movq	%rdi, %rbx
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	testq	%rbx, %rbx
	je	.LBB0_9
# BB#1:                                 # %block_402e6c
	movq	%r8, 232(%rsp)          # 8-byte Spill
	movq	%r9, 240(%rsp)          # 8-byte Spill
	pshufd	$78, %xmm1, %xmm13      # xmm13 = xmm1[2,3,0,1]
	pshufd	$78, %xmm2, %xmm12      # xmm12 = xmm2[2,3,0,1]
	pshufd	$78, %xmm3, %xmm11      # xmm11 = xmm3[2,3,0,1]
	pshufd	$78, %xmm4, %xmm10      # xmm10 = xmm4[2,3,0,1]
	pshufd	$78, %xmm5, %xmm9       # xmm9 = xmm5[2,3,0,1]
	pshufd	$78, %xmm6, %xmm8       # xmm8 = xmm6[2,3,0,1]
	pshufd	$78, %xmm7, %xmm0       # xmm0 = xmm7[2,3,0,1]
	cmpl	$0, 140(%rbx)
	js	.LBB0_2
# BB#3:                                 # %block_402e7a
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	movdqa	%xmm0, 96(%rsp)         # 16-byte Spill
	movd	%rax, %xmm0
	movdqa	%xmm1, 112(%rsp)        # 16-byte Spill
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	movdqa	%xmm7, 208(%rsp)        # 16-byte Spill
	movdqa	%xmm6, 192(%rsp)        # 16-byte Spill
	movdqa	%xmm5, 176(%rsp)        # 16-byte Spill
	movdqa	%xmm4, 160(%rsp)        # 16-byte Spill
	movdqa	%xmm3, 144(%rsp)        # 16-byte Spill
	movdqa	%xmm2, 128(%rsp)        # 16-byte Spill
	movdqa	%xmm8, 80(%rsp)         # 16-byte Spill
	movdqa	%xmm9, 64(%rsp)         # 16-byte Spill
	movdqa	%xmm10, 48(%rsp)        # 16-byte Spill
	movdqa	%xmm11, 32(%rsp)        # 16-byte Spill
	movdqa	%xmm12, 16(%rsp)        # 16-byte Spill
	movdqa	%xmm13, (%rsp)          # 16-byte Spill
	callq	F402a2c
	movdqa	(%rsp), %xmm13          # 16-byte Reload
	movdqa	16(%rsp), %xmm12        # 16-byte Reload
	movdqa	32(%rsp), %xmm11        # 16-byte Reload
	movdqa	48(%rsp), %xmm10        # 16-byte Reload
	movdqa	64(%rsp), %xmm9         # 16-byte Reload
	movdqa	80(%rsp), %xmm8         # 16-byte Reload
	movdqa	112(%rsp), %xmm1        # 16-byte Reload
	movdqa	128(%rsp), %xmm2        # 16-byte Reload
	movdqa	144(%rsp), %xmm3        # 16-byte Reload
	movdqa	160(%rsp), %xmm4        # 16-byte Reload
	movdqa	176(%rsp), %xmm5        # 16-byte Reload
	movdqa	192(%rsp), %xmm6        # 16-byte Reload
	movdqa	208(%rsp), %xmm7        # 16-byte Reload
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movdqa	96(%rsp), %xmm0         # 16-byte Reload
	jmp	.LBB0_4
.LBB0_2:
	movq	%rcx, 224(%rsp)         # 8-byte Spill
.LBB0_4:                                # %block_402e7f
	movd	%xmm1, %r8
	movd	%xmm13, %r12
	movd	%xmm2, %r15
	movq	%xmm12, 208(%rsp)       # 8-byte Folded Spill
	movd	%xmm3, %r14
	movq	%xmm11, 192(%rsp)       # 8-byte Folded Spill
	movd	%xmm4, %rsi
	movd	%xmm10, %rcx
	movd	%xmm5, %r11
	movd	%xmm9, %rbp
	movd	%xmm6, %r10
	movd	%xmm8, %r9
	movd	%xmm7, %r13
	movq	40(%rbx), %rdx
	cmpq	56(%rbx), %rdx
	movd	%xmm0, %rdx
	ja	.LBB0_6
# BB#5:
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rbp, 96(%rsp)          # 8-byte Spill
	movq	%r11, 112(%rsp)         # 8-byte Spill
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	movq	%r9, 144(%rsp)          # 8-byte Spill
	movq	%r10, 160(%rsp)         # 8-byte Spill
	movq	%r13, 176(%rsp)         # 8-byte Spill
	movq	192(%rsp), %r13         # 8-byte Reload
	movq	208(%rsp), %r12         # 8-byte Reload
	jmp	.LBB0_7
.LBB0_6:                                # %block_402e89
	movd	%rax, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movd	%r8, %xmm1
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movd	%r12, %xmm2
	movq	%r12, (%rsp)            # 8-byte Spill
	punpcklqdq	%xmm2, %xmm1    # xmm1 = xmm1[0],xmm2[0]
	movd	%r15, %xmm2
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	208(%rsp), %r15         # 8-byte Reload
	movd	%r15, %xmm3
	punpcklqdq	%xmm3, %xmm2    # xmm2 = xmm2[0],xmm3[0]
	movd	%r14, %xmm3
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	192(%rsp), %r14         # 8-byte Reload
	movd	%r14, %xmm4
	punpcklqdq	%xmm4, %xmm3    # xmm3 = xmm3[0],xmm4[0]
	movd	%rsi, %xmm4
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movd	%rcx, %xmm5
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	punpcklqdq	%xmm5, %xmm4    # xmm4 = xmm4[0],xmm5[0]
	movd	%r11, %xmm5
	movq	%r11, 112(%rsp)         # 8-byte Spill
	movd	%rbp, %xmm6
	movq	%rbp, 96(%rsp)          # 8-byte Spill
	punpcklqdq	%xmm6, %xmm5    # xmm5 = xmm5[0],xmm6[0]
	movd	%r10, %xmm6
	movq	%r10, 160(%rsp)         # 8-byte Spill
	movd	%r9, %xmm7
	movq	%r9, 144(%rsp)          # 8-byte Spill
	punpcklqdq	%xmm7, %xmm6    # xmm6 = xmm6[0],xmm7[0]
	movd	%r13, %xmm7
	movq	%r13, 176(%rsp)         # 8-byte Spill
	movd	%rdx, %xmm8
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	punpcklqdq	%xmm8, %xmm7    # xmm7 = xmm7[0],xmm8[0]
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbx, %rdi
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	232(%rsp), %r8          # 8-byte Reload
	movq	240(%rsp), %r9          # 8-byte Reload
	callq	*72(%rbx)
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movq	%r14, %r13
	movq	%r15, %r12
.LBB0_7:                                # %block_402e93
	movq	8(%rbx), %rsi
	movq	16(%rbx), %rdx
	cmpq	%rsi, %rdx
	ja	.LBB0_8
.LBB0_9:                                # %block_402eb3
	movd	%rax, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	jmp	.LBB0_10
.LBB0_8:                                # %block_402ea0
	subq	%rdx, %rsi
	movd	%rax, %xmm0
	movd	%rdi, %xmm8
	movq	16(%rsp), %xmm1         # 8-byte Folded Reload
	movq	(%rsp), %xmm9           # 8-byte Folded Reload
	movq	32(%rsp), %xmm2         # 8-byte Folded Reload
	movd	%r12, %xmm10
	movq	48(%rsp), %xmm3         # 8-byte Folded Reload
	movd	%r13, %xmm11
	movq	80(%rsp), %xmm4         # 8-byte Folded Reload
	movq	64(%rsp), %xmm12        # 8-byte Folded Reload
	movq	112(%rsp), %xmm5        # 8-byte Folded Reload
	movq	96(%rsp), %xmm13        # 8-byte Folded Reload
	movq	160(%rsp), %xmm6        # 8-byte Folded Reload
	movq	144(%rsp), %xmm14       # 8-byte Folded Reload
	movq	176(%rsp), %xmm7        # 8-byte Folded Reload
	movq	128(%rsp), %xmm15       # 8-byte Folded Reload
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
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	232(%rsp), %r8          # 8-byte Reload
	movq	240(%rsp), %r9          # 8-byte Reload
	callq	*80(%rbx)
.LBB0_10:                               # %block_402eb3
	addq	$264, %rsp              # imm = 0x108
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F402e67, .Ltmp13-F402e67
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
