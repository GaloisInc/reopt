	.text
	.file	"fwrite_402101.ll"
	.globl	F402101
	.align	16, 0x90
	.type	F402101,@function
F402101:                                # @F402101
	.cfi_startproc
# BB#0:                                 # %block_402101
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
	subq	$184, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 240
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
	movq	%rcx, %r12
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movd	%xmm1, %r10
	pshufd	$78, %xmm1, %xmm0       # xmm0 = xmm1[2,3,0,1]
	movd	%xmm0, %rbp
	movd	%xmm2, %r15
	pshufd	$78, %xmm2, %xmm0       # xmm0 = xmm2[2,3,0,1]
	movd	%xmm0, %r13
	movd	%xmm3, %rbx
	pshufd	$78, %xmm3, %xmm0       # xmm0 = xmm3[2,3,0,1]
	movd	%xmm0, %r11
	movd	%xmm4, %r14
	pshufd	$78, %xmm4, %xmm0       # xmm0 = xmm4[2,3,0,1]
	pshufd	$78, %xmm5, %xmm1       # xmm1 = xmm5[2,3,0,1]
	pshufd	$78, %xmm6, %xmm2       # xmm2 = xmm6[2,3,0,1]
	pshufd	$78, %xmm7, %xmm3       # xmm3 = xmm7[2,3,0,1]
	imulq	%rsi, %rdx
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movq	%r8, 128(%rsp)
	cmpl	$0, 140(%r12)
	movq	%xmm0, 72(%rsp)         # 8-byte Folded Spill
	movq	%xmm5, 80(%rsp)         # 8-byte Folded Spill
	movq	%xmm1, 88(%rsp)         # 8-byte Folded Spill
	movq	%xmm6, 96(%rsp)         # 8-byte Folded Spill
	movq	%xmm2, 104(%rsp)        # 8-byte Folded Spill
	movd	%xmm7, %rsi
	movd	%xmm3, %rdx
	js	.LBB0_2
# BB#1:                                 # %block_402141
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbp, 24(%rsp)          # 8-byte Spill
	movq	%r9, %rbp
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	%r8, %r13
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	%r14, %r12
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	%r10, 40(%rsp)          # 8-byte Spill
	movq	%r11, %r15
	movq	%rdx, %r14
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	callq	F402a2c
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %r11
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r12, %r14
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %r8
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%rbp, %r9
	movq	24(%rsp), %rbp          # 8-byte Reload
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_2:                                # %block_402152
	movd	%rax, %xmm0
	movd	%rcx, %xmm8
	movd	%r10, %xmm1
	movd	%rbp, %xmm9
	movd	%r15, %xmm2
	movd	%r13, %xmm10
	movd	%rbx, %xmm3
	movd	%r11, %xmm11
	movd	%r14, %xmm4
	movq	72(%rsp), %xmm12        # 8-byte Folded Reload
	movq	80(%rsp), %xmm5         # 8-byte Folded Reload
	movq	88(%rsp), %xmm13        # 8-byte Folded Reload
	movq	96(%rsp), %xmm6         # 8-byte Folded Reload
	movq	104(%rsp), %xmm14       # 8-byte Folded Reload
	movd	%rsi, %xmm7
	movd	%rdx, %xmm15
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%r12, %rcx
	callq	F402058
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movb	$1, %dl
	testb	%dl, %dl
	jne	.LBB0_4
# BB#3:                                 # %block_40215a
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r12, %rdi
	callq	F402a80
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_4:                                # %block_402162
	cmpq	%rax, 112(%rsp)         # 8-byte Folded Reload
	je	.LBB0_6
# BB#5:                                 # %block_40216a
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
.LBB0_6:                                # %block_402172
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$184, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F402101, .Ltmp13-F402101
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
