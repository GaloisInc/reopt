	.text
	.file	"__simple_malloc_400860.ll"
	.globl	F400860
	.align	16, 0x90
	.type	F400860,@function
F400860:                                # @F400860
	.cfi_startproc
# BB#0:                                 # %block_400860
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp3:
	.cfi_def_cfa_offset 40
	subq	$56, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 96
.Ltmp5:
	.cfi_offset %rbx, -40
.Ltmp6:
	.cfi_offset %r12, -32
.Ltmp7:
	.cfi_offset %r14, -24
.Ltmp8:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rax
	leaq	16(%rsp), %r14
	testq	%r12, %r12
	je	.LBB0_12
# BB#1:                                 # %block_40086f
	movl	$1, %ebx
	cmpq	$2, %r12
	jae	.LBB0_5
# BB#2:                                 # %block_400970
	movd	%rcx, %xmm0
	movd	%rax, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$6308040, %edi          # imm = 0x6040C8
	callq	F4023a7
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	xorl	%r15d, %r15d
	movq	6308032, %rdx
	jmp	.LBB0_3
	.align	16, 0x90
.LBB0_5:                                # %block_40088e
                                        # =>This Inner Loop Header: Depth=1
	addq	%rbx, %rbx
	cmpq	%r12, %rbx
	jae	.LBB0_6
# BB#4:                                 # %block_400888
                                        #   in Loop: Header=BB0_5 Depth=1
	cmpq	$15, %rbx
	jbe	.LBB0_5
.LBB0_6:                                # %block_4008a4
	decq	%rbx
	movd	%rcx, %xmm0
	movd	%rax, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$6308040, %edi          # imm = 0x6040C8
	callq	F4023a7
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movq	6308032, %rdx
	movq	%rdx, %r15
	negq	%r15
	andq	%rbx, %r15
	movabsq	$-9223372036854775792, %rsi # imm = 0x8000000000000010
	cmpq	%rsi, %r12
	jae	.LBB0_7
.LBB0_3:                                # %block_400977
	addq	%r15, %r12
	jmp	.LBB0_13
.LBB0_12:                               # %block_400911
	movd	%rcx, %xmm0
	movd	%rax, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$6308040, %edi          # imm = 0x6040C8
	callq	F4023a7
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	xorl	%r15d, %r15d
	movl	$1, %r12d
	movq	6308032, %rdx
.LBB0_13:                               # %block_400918
	movq	6308024, %rsi
	movq	%rsi, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %r12
	jb	.LBB0_10
# BB#14:                                # %block_400918
	leaq	(%r12,%rdx), %rdi
	cmpq	%rdi, %rsi
	jmp	.LBB0_9
.LBB0_7:                                # %block_4008c7
	movq	6308024, %rsi
	movq	%rsi, %rdi
	subq	%rdx, %rdi
	leaq	(%r12,%rdx), %rbx
	cmpq	%rbx, %rsi
	setne	%bl
	cmpq	%r12, %rdi
	ja	.LBB0_10
# BB#8:                                 # %block_4008c7
	testb	%bl, %bl
.LBB0_9:                                # %block_4008c7
                                        # implicit-def: RSI
	je	.LBB0_10
# BB#15:                                # %block_400936
	movq	%r12, 8(%r14)
	addq	$8, %r14
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r14, %rdi
	callq	F4026e0
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testq	%rax, %rax
	je	.LBB0_11
# BB#16:                                # %block_40093b
	cmpq	6308024, %rax
	jne	.LBB0_18
# BB#17:
	movq	6308032, %rdx
	jmp	.LBB0_10
.LBB0_18:                               # %block_40094b
	subq	%r15, %r12
                                        # implicit-def: RDX
.LBB0_10:                               # %block_4008d6
	addq	%rdx, %r12
	movq	%r12, 6308032
.LBB0_11:                               # %block_4008f0
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$6308040, %edi          # imm = 0x6040C8
	callq	F4023e2
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp9:
	.size	F400860, .Ltmp9-F400860
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
