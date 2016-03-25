	.text
	.file	"__fwritex_402058.ll"
	.globl	F402058
	.align	16, 0x90
	.type	F402058,@function
F402058:                                # @F402058
	.cfi_startproc
# BB#0:                                 # %block_402058
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
	subq	$200, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 256
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
	movq	%r9, %r12
	movq	%r8, %r15
	movq	%rdi, %r14
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbx
	leaq	160(%rsp), %rdi
	movq	%r15, 160(%rsp)
	cmpq	$0, 32(%rdx)
	je	.LBB0_14
# BB#1:
	movq	%rdx, %r13
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	jmp	.LBB0_2
.LBB0_14:                               # %block_40209b
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movd	%rax, %xmm0
	movaps	%xmm1, 16(%rsp)         # 16-byte Spill
	movd	%rbx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rdi
	movq	%rdx, %r13
	movaps	%xmm7, 128(%rsp)        # 16-byte Spill
	movaps	%xmm6, 112(%rsp)        # 16-byte Spill
	movaps	%xmm5, 96(%rsp)         # 16-byte Spill
	movaps	%xmm4, 80(%rsp)         # 16-byte Spill
	movaps	%xmm3, 48(%rsp)         # 16-byte Spill
	movaps	%xmm2, 32(%rsp)         # 16-byte Spill
	callq	F402ce8
	movdqa	16(%rsp), %xmm1         # 16-byte Reload
	movdqa	32(%rsp), %xmm2         # 16-byte Reload
	movdqa	48(%rsp), %xmm3         # 16-byte Reload
	movdqa	80(%rsp), %xmm4         # 16-byte Reload
	movdqa	96(%rsp), %xmm5         # 16-byte Reload
	movdqa	112(%rsp), %xmm6        # 16-byte Reload
	movdqa	128(%rsp), %xmm7        # 16-byte Reload
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbx
	xorl	%ebp, %ebp
	testl	%eax, %eax
	jne	.LBB0_15
.LBB0_2:                                # %block_402070
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movd	%xmm1, %rbx
	pshufd	$78, %xmm1, %xmm0       # xmm0 = xmm1[2,3,0,1]
	movd	%xmm0, %rcx
	movd	%xmm2, %r15
	pshufd	$78, %xmm2, %xmm0       # xmm0 = xmm2[2,3,0,1]
	movd	%xmm0, %rax
	movd	%xmm3, %r10
	pshufd	$78, %xmm3, %xmm0       # xmm0 = xmm3[2,3,0,1]
	movd	%xmm0, %r8
	movd	%xmm4, %r11
	pshufd	$78, %xmm4, %xmm0       # xmm0 = xmm4[2,3,0,1]
	movd	%xmm0, %rsi
	movd	%xmm5, %r12
	pshufd	$78, %xmm5, %xmm0       # xmm0 = xmm5[2,3,0,1]
	movd	%xmm0, %r14
	movq	%xmm6, 16(%rsp)         # 8-byte Folded Spill
	pshufd	$78, %xmm6, %xmm0       # xmm0 = xmm6[2,3,0,1]
	movd	%xmm0, %r9
	movq	%xmm7, 128(%rsp)        # 8-byte Folded Spill
	pshufd	$78, %xmm7, %xmm0       # xmm0 = xmm7[2,3,0,1]
	movq	%r13, %rdi
	movq	32(%rdi), %rbp
	subq	40(%rdi), %rbp
	movq	144(%rsp), %rdx         # 8-byte Reload
	cmpq	%rbp, %rdx
	movd	%xmm0, %r13
	ja	.LBB0_3
# BB#4:                                 # %block_4020a3
	movq	128(%rsp), %rdx         # 8-byte Reload
	xorl	%ebp, %ebp
	cmpb	$0, 139(%rdi)
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	js	.LBB0_5
# BB#6:
	movq	%r13, (%rsp)            # 8-byte Spill
	movq	144(%rsp), %rbp         # 8-byte Reload
	movq	112(%rsp), %r13         # 8-byte Reload
	jmp	.LBB0_7
	.align	16, 0x90
.LBB0_10:                               # %block_4020c0
                                        #   in Loop: Header=BB0_7 Depth=1
	decq	%rbp
.LBB0_7:                                # %block_4020af
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbp, %rbp
	je	.LBB0_8
# BB#9:                                 # %block_4020b4
                                        #   in Loop: Header=BB0_7 Depth=1
	movzbl	-1(%rbp,%r13), %edi
	cmpl	$10, %edi
	jne	.LBB0_10
# BB#11:                                # %block_4020ec
	movq	96(%rsp), %xmm0         # 8-byte Folded Reload
	movq	80(%rsp), %xmm8         # 8-byte Folded Reload
	movd	%rbx, %xmm1
	movd	%rcx, %xmm9
	movd	%r15, %xmm2
	movd	%rax, %xmm10
	movd	%r10, %xmm3
	movd	%r8, %xmm11
	movd	%r11, %xmm4
	movd	%rsi, %xmm12
	movd	%r12, %xmm5
	movd	%r14, %xmm13
	movq	16(%rsp), %xmm6         # 8-byte Folded Reload
	movd	%r9, %xmm14
	movd	%rdx, %xmm7
	movq	(%rsp), %xmm15          # 8-byte Folded Reload
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rbp, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	*72(%r14)
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbx
	cmpq	%rbp, %rax
	jb	.LBB0_15
# BB#12:                                # %block_4020f1
	addq	%rbp, %r13
	subq	%rbp, 144(%rsp)         # 8-byte Folded Spill
	jmp	.LBB0_13
.LBB0_3:                                # %block_40207d
	movq	96(%rsp), %xmm0         # 8-byte Folded Reload
	movq	80(%rsp), %xmm8         # 8-byte Folded Reload
	movd	%rbx, %xmm1
	movd	%rcx, %xmm9
	movd	%r15, %xmm2
	movd	%rax, %xmm10
	movd	%r10, %xmm3
	movd	%r8, %xmm11
	movd	%r11, %xmm4
	movd	%rsi, %xmm12
	movd	%r12, %xmm5
	movd	%r14, %xmm13
	movq	16(%rsp), %xmm6         # 8-byte Folded Reload
	movd	%r9, %xmm14
	movq	128(%rsp), %xmm7        # 8-byte Folded Reload
	movd	%r13, %xmm15
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rcx
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	*72(%rdi)
	jmp	.LBB0_16
.LBB0_5:
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	112(%rsp), %r13         # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB0_13
.LBB0_8:
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
.LBB0_13:                               # %block_4020d6
	movq	40(%r14), %rdi
	movd	%rax, %xmm0
	movd	%rbx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r13, %rsi
	movq	144(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdx
	callq	F402375
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbx
	addq	%r15, 40(%r14)
	addq	%r15, %rbp
.LBB0_15:                               # %block_4020f9
	movd	%rax, %xmm0
	movd	%rbx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbp, %rax
.LBB0_16:                               # %block_4020f9
	addq	$200, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F402058, .Ltmp13-F402058
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
