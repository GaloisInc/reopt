	.text
	.file	"__libc_exit_fini_40083d.ll"
	.globl	F40083d
	.align	16, 0x90
	.type	F40083d,@function
F40083d:                                # @F40083d
	.cfi_startproc
# BB#0:                                 # %block_40083d
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
	subq	$136, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 192
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
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movd	%xmm1, %rdx
	pshufd	$78, %xmm1, %xmm0       # xmm0 = xmm1[2,3,0,1]
	movq	%xmm0, 80(%rsp)         # 8-byte Folded Spill
	movq	%xmm2, 72(%rsp)         # 8-byte Folded Spill
	pshufd	$78, %xmm2, %xmm0       # xmm0 = xmm2[2,3,0,1]
	movq	%xmm0, 64(%rsp)         # 8-byte Folded Spill
	movq	%xmm3, 56(%rsp)         # 8-byte Folded Spill
	pshufd	$78, %xmm3, %xmm0       # xmm0 = xmm3[2,3,0,1]
	movq	%xmm0, 48(%rsp)         # 8-byte Folded Spill
	movq	%xmm4, 40(%rsp)         # 8-byte Folded Spill
	pshufd	$78, %xmm4, %xmm0       # xmm0 = xmm4[2,3,0,1]
	movd	%rdx, %xmm1
	movaps	%xmm1, 16(%rsp)         # 16-byte Spill
	movq	%xmm0, 8(%rsp)          # 8-byte Folded Spill
	pshufd	$78, %xmm5, %xmm0       # xmm0 = xmm5[2,3,0,1]
	pshufd	$78, %xmm6, %xmm1       # xmm1 = xmm6[2,3,0,1]
	pshufd	$78, %xmm7, %xmm2       # xmm2 = xmm7[2,3,0,1]
	movq	%xmm5, (%rsp)           # 8-byte Folded Spill
	movd	%xmm0, %r14
	movd	%xmm6, %r15
	movd	%xmm1, %r12
	movd	%xmm7, %r13
	movd	%xmm2, %rbx
	movl	$6307824, %ebp          # imm = 0x603FF0
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                # %block_400854
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	80(%rsp), %xmm2         # 8-byte Folded Reload
	movdqa	16(%rsp), %xmm1         # 16-byte Reload
	punpcklqdq	%xmm2, %xmm1    # xmm1 = xmm1[0],xmm2[0]
	movq	72(%rsp), %xmm2         # 8-byte Folded Reload
	movq	64(%rsp), %xmm3         # 8-byte Folded Reload
	punpcklqdq	%xmm3, %xmm2    # xmm2 = xmm2[0],xmm3[0]
	movq	56(%rsp), %xmm3         # 8-byte Folded Reload
	movq	48(%rsp), %xmm4         # 8-byte Folded Reload
	punpcklqdq	%xmm4, %xmm3    # xmm3 = xmm3[0],xmm4[0]
	movq	40(%rsp), %xmm4         # 8-byte Folded Reload
	movq	8(%rsp), %xmm5          # 8-byte Folded Reload
	punpcklqdq	%xmm5, %xmm4    # xmm4 = xmm4[0],xmm5[0]
	movq	(%rsp), %xmm5           # 8-byte Folded Reload
	movd	%r14, %xmm6
	punpcklqdq	%xmm6, %xmm5    # xmm5 = xmm5[0],xmm6[0]
	movd	%r15, %xmm6
	movd	%r12, %xmm7
	punpcklqdq	%xmm7, %xmm6    # xmm6 = xmm6[0],xmm7[0]
	movd	%r13, %xmm7
	movd	%rbx, %xmm8
	punpcklqdq	%xmm8, %xmm7    # xmm7 = xmm7[0],xmm8[0]
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	96(%rsp), %r8           # 8-byte Reload
	movq	104(%rsp), %r9          # 8-byte Reload
	callq	*(%rbp)
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	addq	$-8, %rbp
.LBB0_1:                                # %block_400843
                                        # =>This Inner Loop Header: Depth=1
	leaq	8(%rbp), %rdx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	cmpq	$6307825, %rdx          # imm = 0x603FF1
	jae	.LBB0_2
# BB#3:                                 # %block_400856
	movq	112(%rsp), %rdx         # 8-byte Reload
	callq	F402f54
	addq	$136, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F40083d, .Ltmp13-F40083d
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
