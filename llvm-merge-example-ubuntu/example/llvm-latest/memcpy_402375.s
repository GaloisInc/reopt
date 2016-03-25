	.text
	.file	"memcpy_402375.ll"
	.globl	F402375
	.align	16, 0x90
	.type	F402375,@function
F402375:                                # @F402375
	.cfi_startproc
# BB#0:                                 # %block_402375
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
	subq	$24, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 80
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
	movq	%rdx, %rbx
	movq	%rsi, %r15
	pshufd	$78, %xmm0, %xmm1       # xmm1 = xmm0[2,3,0,1]
	cmpq	$8, %rbx
	jb	.LBB0_1
# BB#2:                                 # %block_40237e
	testb	$7, %dil
	je	.LBB0_1
# BB#3:                                 # %subblock_40237e_2
	decq	%rbx
	movq	%rdi, %r12
	jmp	.LBB0_4
	.align	16, 0x90
.LBB0_10:                               # %subblock_402386_1
                                        #   in Loop: Header=BB0_4 Depth=1
	decq	%rbx
.LBB0_4:                                # %block_402386
                                        # =>This Inner Loop Header: Depth=1
	movb	(%r15), %al
	movb	%al, (%r12)
	incq	%r15
	incq	%r12
	testb	$7, %r12b
	jne	.LBB0_10
	jmp	.LBB0_5
.LBB0_1:
	movq	%rdi, %r12
.LBB0_5:
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movd	%xmm0, %r14
	movd	%xmm1, %rbp
	movq	%rbx, %r13
	shrq	$3, %r13
	xorl	%ecx, %ecx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	reopt.MemCopy.i64
	andl	$7, %ebx
	je	.LBB0_6
# BB#7:
	leaq	(%r15,%r13,8), %rsi
	leaq	(%r12,%r13,8), %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	.align	16, 0x90
.LBB0_8:                                # %block_4023a1
                                        # =>This Inner Loop Header: Depth=1
	movb	(%rsi), %dl
	movb	%dl, (%rcx)
	incq	%rsi
	incq	%rcx
	leal	-1(%rbx), %edx
	cmpl	$1, %ebx
	movq	%rdx, %rbx
	jne	.LBB0_8
	jmp	.LBB0_9
.LBB0_6:
	movq	%rbx, %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB0_9:                                # %block_4023a6
	movd	%r14, %xmm0
	movd	%rbp, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F402375, .Ltmp13-F402375
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
