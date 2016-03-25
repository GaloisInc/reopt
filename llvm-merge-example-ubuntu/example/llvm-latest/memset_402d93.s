	.text
	.file	"memset_402d93.ll"
	.globl	F402d93
	.align	16, 0x90
	.type	F402d93,@function
F402d93:                                # @F402d93
	.cfi_startproc
# BB#0:                                 # %block_402d93
	pushq	%r14
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp2:
	.cfi_def_cfa_offset 32
.Ltmp3:
	.cfi_offset %rbx, -24
.Ltmp4:
	.cfi_offset %r14, -16
	movq	%rsi, %rax
	movd	%xmm0, %rbx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r14
	movzbl	%al, %ecx
	movabsq	$72340172838076673, %rsi # imm = 0x101010101010101
	imulq	%rcx, %rsi
	cmpq	$126, %rdx
	jbe	.LBB0_1
# BB#9:                                 # %block_402e23
	movq	%rsi, -8(%rdi,%rdx)
	testb	$15, %dil
	je	.LBB0_10
# BB#11:                                # %block_402e41
	movl	%edi, %eax
	negl	%eax
	andl	$15, %eax
	movq	%rsi, (%rdi)
	movq	%rsi, 8(%rdi)
	subq	%rax, %rdx
	addq	%rax, %rdi
.LBB0_10:                               # %block_402e36
	shrq	$3, %rdx
	xorl	%ecx, %ecx
	callq	reopt.MemSet.i64
	jmp	.LBB0_8
.LBB0_1:                                # %block_402dab
	testl	%edx, %edx
	je	.LBB0_8
# BB#2:                                 # %block_402daf
	movb	%al, (%rdi)
	movb	%al, -1(%rdi,%rdx)
	cmpl	$3, %edx
	jb	.LBB0_8
# BB#3:                                 # %block_402dbc
	movw	%si, 1(%rdi)
	movw	%si, -3(%rdi,%rdx)
	cmpl	$7, %edx
	jb	.LBB0_8
# BB#4:                                 # %block_402dca
	movl	%esi, 3(%rdi)
	movl	%esi, -7(%rdi,%rdx)
	cmpl	$15, %edx
	jb	.LBB0_8
# BB#5:                                 # %block_402dd6
	movq	%rsi, 7(%rdi)
	movq	%rsi, -15(%rdi,%rdx)
	cmpl	$31, %edx
	jb	.LBB0_8
# BB#6:                                 # %block_402de4
	movq	%rsi, 15(%rdi)
	movq	%rsi, 23(%rdi)
	movq	%rsi, -31(%rdi,%rdx)
	movq	%rsi, -23(%rdi,%rdx)
	cmpl	$63, %edx
	jb	.LBB0_8
# BB#7:                                 # %block_402dfb
	movq	%rsi, 31(%rdi)
	movq	%rsi, 39(%rdi)
	movq	%rsi, 47(%rdi)
	movq	%rsi, 55(%rdi)
	movq	%rsi, -63(%rdi,%rdx)
	movq	%rsi, -55(%rdi,%rdx)
	movq	%rsi, -47(%rdi,%rdx)
	movq	%rsi, -39(%rdi,%rdx)
.LBB0_8:                                # %block_402e1f
	movd	%rbx, %xmm0
	movd	%r14, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5:
	.size	F402d93, .Ltmp5-F402d93
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
