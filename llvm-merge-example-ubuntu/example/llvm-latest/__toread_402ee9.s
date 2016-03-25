	.text
	.file	"__toread_402ee9.ll"
	.globl	F402ee9
	.align	16, 0x90
	.type	F402ee9,@function
F402ee9:                                # @F402ee9
	.cfi_startproc
# BB#0:                                 # %block_402ee9
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
.Ltmp2:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movd	%xmm0, %r10
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movzbl	138(%rbx), %eax
	leaq	-1(%rax), %rsi
	movl	%esi, %edx
	orl	%esi, %eax
	movb	%al, 138(%rbx)
	movq	40(%rbx), %rax
	cmpq	88(%rbx), %rax
	jbe	.LBB0_2
# BB#1:                                 # %block_402f08
	movd	%r10, %xmm0
	movd	%rdi, %xmm8
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
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbx, %rdi
	callq	*72(%rbx)
	movd	%xmm0, %r10
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
                                        # implicit-def: RDX
.LBB0_2:                                # %block_402f0f
	movl	(%rbx), %eax
	pxor	%xmm0, %xmm0
	movdqu	%xmm0, 32(%rbx)
	movq	$0, 56(%rbx)
	testb	$4, %al
	je	.LBB0_4
# BB#3:                                 # %block_402f2d
	orl	$32, %eax
	movl	%eax, (%rbx)
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	jmp	.LBB0_5
.LBB0_4:                                # %block_402f37
	movq	96(%rbx), %rdx
	addq	88(%rbx), %rdx
	shll	$27, %eax
	sarl	$31, %eax
	movq	%rdx, 16(%rbx)
	movq	%rdx, 8(%rbx)
.LBB0_5:                                # %block_402f4d
	movd	%r10, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F402ee9, .Ltmp3-F402ee9
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
