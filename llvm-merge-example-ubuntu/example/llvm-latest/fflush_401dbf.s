	.text
	.file	"fflush_401dbf.ll"
	.globl	F401dbf
	.align	16, 0x90
	.type	F401dbf,@function
F401dbf:                                # @F401dbf
	.cfi_startproc
# BB#0:                                 # %block_401dbf
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
	subq	$168, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 224
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
	movq	%rdx, %r10
	movq	%rdi, %r12
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movq	%xmm1, 72(%rsp)         # 8-byte Folded Spill
	pshufd	$78, %xmm1, %xmm0       # xmm0 = xmm1[2,3,0,1]
	movq	%xmm0, 64(%rsp)         # 8-byte Folded Spill
	movq	%xmm2, 96(%rsp)         # 8-byte Folded Spill
	pshufd	$78, %xmm2, %xmm0       # xmm0 = xmm2[2,3,0,1]
	movq	%xmm0, 88(%rsp)         # 8-byte Folded Spill
	movd	%xmm3, %r13
	pshufd	$78, %xmm3, %xmm0       # xmm0 = xmm3[2,3,0,1]
	movq	%xmm0, 112(%rsp)        # 8-byte Folded Spill
	movd	%xmm4, %r14
	pshufd	$78, %xmm4, %xmm0       # xmm0 = xmm4[2,3,0,1]
	pshufd	$78, %xmm5, %xmm1       # xmm1 = xmm5[2,3,0,1]
	pshufd	$78, %xmm6, %xmm2       # xmm2 = xmm6[2,3,0,1]
	pshufd	$78, %xmm7, %xmm3       # xmm3 = xmm7[2,3,0,1]
	testq	%r12, %r12
	movq	%xmm0, 80(%rsp)         # 8-byte Folded Spill
	movq	%xmm5, 104(%rsp)        # 8-byte Folded Spill
	movd	%xmm1, %rbp
	movd	%xmm6, %rdx
	movd	%xmm2, %rbx
	movd	%xmm7, %r15
	movd	%xmm3, %r11
	je	.LBB0_5
# BB#1:                                 # %block_401dcc
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	%rbp, 24(%rsp)          # 8-byte Spill
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r15, %rbx
	movq	%r11, 56(%rsp)          # 8-byte Spill
	movq	%rcx, %r14
	movq	%r8, %r15
	movq	%r9, %r13
	movq	%r10, %rbp
	leaq	128(%rsp), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpl	$0, 140(%r12)
	js	.LBB0_3
# BB#2:                                 # %block_401de0
	movd	%rax, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r12, %rdi
	callq	F402a2c
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
.LBB0_3:                                # %block_401dea
	movq	%rbp, %rdx
	movq	%rbx, %r10
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	40(%rsp), %rbp          # 8-byte Reload
	movd	%rax, %xmm0
	movd	%rdi, %xmm8
	movq	72(%rsp), %xmm1         # 8-byte Folded Reload
	movq	64(%rsp), %xmm9         # 8-byte Folded Reload
	movq	96(%rsp), %xmm2         # 8-byte Folded Reload
	movq	88(%rsp), %xmm10        # 8-byte Folded Reload
	movq	16(%rsp), %xmm3         # 8-byte Folded Reload
	movq	112(%rsp), %xmm11       # 8-byte Folded Reload
	movq	32(%rsp), %xmm4         # 8-byte Folded Reload
	movq	80(%rsp), %xmm12        # 8-byte Folded Reload
	movq	104(%rsp), %xmm5        # 8-byte Folded Reload
	movq	24(%rsp), %xmm13        # 8-byte Folded Reload
	movd	%rbp, %xmm6
	movd	%rbx, %xmm14
	movd	%r10, %xmm7
	movq	56(%rsp), %xmm15        # 8-byte Folded Reload
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movq	%r12, %rdi
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%r13, %r9
	callq	F401d55
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movb	$1, %al
	testb	%al, %al
	jne	.LBB0_19
# BB#4:                                 # %block_401dfe
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r12, %rdi
	movq	%rdx, %rsi
	callq	F402a80
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	12(%rax), %eax
	jmp	.LBB0_19
.LBB0_5:                                # %block_401e04
	cmpq	$0, 6309616
	je	.LBB0_7
# BB#6:                                 # %block_401e1e
	movd	%rax, %xmm0
	movd	%rdi, %xmm8
	movq	72(%rsp), %xmm1         # 8-byte Folded Reload
	movq	64(%rsp), %xmm9         # 8-byte Folded Reload
	movq	96(%rsp), %xmm2         # 8-byte Folded Reload
	movq	88(%rsp), %xmm10        # 8-byte Folded Reload
	movd	%r13, %xmm3
	movq	112(%rsp), %xmm11       # 8-byte Folded Reload
	movd	%r14, %xmm4
	movq	80(%rsp), %xmm12        # 8-byte Folded Reload
	movq	104(%rsp), %xmm5        # 8-byte Folded Reload
	movd	%rbp, %xmm13
	movd	%rdx, %xmm6
	movd	%rbx, %xmm14
	movd	%r15, %xmm7
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	movd	%r11, %xmm8
	movq	6309616, %rdi
	punpcklqdq	%xmm8, %xmm7    # xmm7 = xmm7[0],xmm8[0]
	movq	%r10, %rdx
	movq	%r10, %rbp
	callq	F401dbf
	movq	%rbp, %r10
	movq	%rax, %r12
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
.LBB0_7:                                # %block_401e25
	xorl	%edx, %edx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movd	%rax, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r12, %rdi
	movq	%r10, %rdx
	callq	F40217e
	movd	%xmm0, %rbp
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movq	(%rax), %r15
                                        # implicit-def: RBX
                                        # implicit-def: RSI
                                        # implicit-def: RCX
	movq	%rcx, 112(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 104(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 96(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 88(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 80(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 72(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 64(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 56(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 48(%rsp)          # 8-byte Spill
                                        # implicit-def: R11
                                        # implicit-def: R14
                                        # implicit-def: R12
                                        # implicit-def: R9
                                        # implicit-def: R8
                                        # implicit-def: R10
                                        # implicit-def: RCX
	jmp	.LBB0_8
	.align	16, 0x90
.LBB0_16:                               # %block_401e5e
                                        #   in Loop: Header=BB0_8 Depth=1
	movd	%rbp, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	movq	%rdx, %rsi
	movq	%r10, %rdx
	callq	F402a80
	movd	%xmm0, %rbp
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
                                        # implicit-def: RBX
                                        # implicit-def: RSI
                                        # implicit-def: RCX
	movq	%rcx, 112(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 104(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 96(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 88(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 80(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 72(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 64(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 56(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 48(%rsp)          # 8-byte Spill
                                        # implicit-def: R11
                                        # implicit-def: R14
                                        # implicit-def: R12
                                        # implicit-def: R9
                                        # implicit-def: R8
                                        # implicit-def: R10
                                        # implicit-def: RCX
	movq	112(%r15), %r15
.LBB0_8:                                # %block_401e28
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB0_18
# BB#9:                                 # %block_401e2d
                                        #   in Loop: Header=BB0_8 Depth=1
	cmpl	$0, 140(%r15)
	js	.LBB0_11
# BB#10:                                # %block_401e42
                                        #   in Loop: Header=BB0_8 Depth=1
	movd	%rbp, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	callq	F402a2c
	movd	%xmm0, %rbp
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
                                        # implicit-def: RBX
                                        # implicit-def: RSI
                                        # implicit-def: RCX
	movq	%rcx, 112(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 104(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 96(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 88(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 80(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 72(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 64(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 56(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 48(%rsp)          # 8-byte Spill
                                        # implicit-def: R11
                                        # implicit-def: R14
                                        # implicit-def: R12
                                        # implicit-def: R9
                                        # implicit-def: R8
                                        # implicit-def: R10
                                        # implicit-def: RCX
.LBB0_11:                               # %block_401e45
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	40(%r15), %r13
	cmpq	56(%r15), %r13
	ja	.LBB0_13
# BB#12:                                #   in Loop: Header=BB0_8 Depth=1
	movq	%rbx, %r13
	jmp	.LBB0_14
	.align	16, 0x90
.LBB0_13:                               # %block_401e57
                                        #   in Loop: Header=BB0_8 Depth=1
	movd	%rbp, %xmm0
	movd	%rdi, %xmm8
	movd	%r14, %xmm1
	movd	%r12, %xmm9
	movq	48(%rsp), %xmm2         # 8-byte Folded Reload
	movd	%r11, %xmm10
	movq	64(%rsp), %xmm3         # 8-byte Folded Reload
	movq	56(%rsp), %xmm11        # 8-byte Folded Reload
	movq	80(%rsp), %xmm4         # 8-byte Folded Reload
	movq	72(%rsp), %xmm12        # 8-byte Folded Reload
	movq	96(%rsp), %xmm5         # 8-byte Folded Reload
	movq	88(%rsp), %xmm13        # 8-byte Folded Reload
	movq	112(%rsp), %xmm6        # 8-byte Folded Reload
	movq	104(%rsp), %xmm14       # 8-byte Folded Reload
	movd	%rbx, %xmm7
	movd	%rsi, %xmm15
	punpcklqdq	%xmm8, %xmm0    # xmm0 = xmm0[0],xmm8[0]
	punpcklqdq	%xmm9, %xmm1    # xmm1 = xmm1[0],xmm9[0]
	punpcklqdq	%xmm10, %xmm2   # xmm2 = xmm2[0],xmm10[0]
	punpcklqdq	%xmm11, %xmm3   # xmm3 = xmm3[0],xmm11[0]
	punpcklqdq	%xmm12, %xmm4   # xmm4 = xmm4[0],xmm12[0]
	punpcklqdq	%xmm13, %xmm5   # xmm5 = xmm5[0],xmm13[0]
	punpcklqdq	%xmm14, %xmm6   # xmm6 = xmm6[0],xmm14[0]
	punpcklqdq	%xmm15, %xmm7   # xmm7 = xmm7[0],xmm15[0]
	movq	%r15, %rdi
	movq	%rdx, %rsi
	movq	%r10, %rdx
	callq	F401d55
	movd	%xmm0, %rbp
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
                                        # implicit-def: R13
                                        # implicit-def: RSI
                                        # implicit-def: RCX
	movq	%rcx, 112(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 104(%rsp)         # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 96(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 88(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 80(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 72(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 64(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 56(%rsp)          # 8-byte Spill
                                        # implicit-def: RCX
	movq	%rcx, 48(%rsp)          # 8-byte Spill
                                        # implicit-def: R11
                                        # implicit-def: R14
                                        # implicit-def: R12
                                        # implicit-def: R9
                                        # implicit-def: R8
	movl	$4294967295, %ecx       # imm = 0xFFFFFFFF
	movq	%rcx, 40(%rsp)          # 8-byte Spill
                                        # implicit-def: R10
                                        # implicit-def: RCX
.LBB0_14:                               # %block_401e59
                                        #   in Loop: Header=BB0_8 Depth=1
	movb	$1, %bl
	testb	%bl, %bl
	je	.LBB0_16
# BB#15:                                #   in Loop: Header=BB0_8 Depth=1
	movq	%r13, %rbx
	movq	112(%r15), %r15
	jmp	.LBB0_8
.LBB0_18:                               # %block_401e71
	movd	%rbp, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%r10, %rdx
	callq	F402190
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	%eax, %eax
.LBB0_19:                               # %block_401e73
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$168, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F401dbf, .Ltmp13-F401dbf
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
