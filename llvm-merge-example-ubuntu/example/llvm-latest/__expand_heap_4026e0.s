	.text
	.file	"__expand_heap_4026e0.ll"
	.globl	F4026e0
	.align	16, 0x90
	.type	F4026e0,@function
F4026e0:                                # @F4026e0
	.cfi_startproc
# BB#0:                                 # %block_4026e0
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
	movq	%rdi, %r14
	movd	%xmm0, %r15
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r12
	movabsq	$9223372036854771711, %rdx # imm = 0x7FFFFFFFFFFFEFFF
	movq	(%r14), %rax
	cmpq	%rdx, %rax
	jbe	.LBB0_1
# BB#16:                                # %block_402835
	movd	%r15, %xmm0
	movd	%r12, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r14, %rdi
	callq	F402681
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	$12, (%rax)
	xorl	%r15d, %r15d
	jmp	.LBB0_9
.LBB0_1:                                # %block_4026fc
	movq	6310208, %rdi
	movl	%eax, %edx
	negl	%edx
	andl	$4095, %edx             # imm = 0xFFF
	testq	%rdi, %rdi
	je	.LBB0_3
# BB#2:
	addq	%rax, %rdx
	jmp	.LBB0_4
.LBB0_3:                                # %block_402724
	movq	$12, (%rsp)
	callq	reopt.SystemCall.Linux
	movl	%eax, %edi
	negl	%edi
	andl	$4095, %edi             # imm = 0xFFF
	addq	%rax, %rdi
	movq	%rdi, 6310208
                                        # implicit-def: RDX
.LBB0_4:                                # %block_40273a
	movq	%rdi, %rax
	notq	%rax
	cmpq	%rax, %rdx
	jae	.LBB0_5
# BB#10:                                # %block_4027a0
	movq	6310288, %rcx
	leaq	(%rdx,%rdi), %rax
	leaq	-8388608(%rcx), %rsi
	xorl	%ebx, %ebx
	cmpq	$8388609, %rcx          # imm = 0x800001
	cmovaeq	%rsi, %rbx
	cmpq	%rbx, %rax
	jbe	.LBB0_12
# BB#11:                                # %block_4027c7
	cmpq	%rcx, %rdi
	jb	.LBB0_5
.LBB0_12:                               # %block_4027d0
	leaq	16(%rsp), %rsi
	leaq	8(%rsi), %rbx
	addq	$-8388600, %rsi         # imm = 0xFFFFFFFFFF800008
	xorl	%ecx, %ecx
	cmpq	$8388609, %rbx          # imm = 0x800001
	cmovaeq	%rsi, %rcx
	cmpq	%rdi, %rbx
	jbe	.LBB0_14
# BB#13:                                # %block_4027f3
	cmpq	%rax, %rcx
	jb	.LBB0_5
.LBB0_14:                               # %block_402803
	movq	$12, (%rsp)
	movq	%rax, %rdi
	callq	reopt.SystemCall.Linux
	cmpq	%rax, %rax
                                        # implicit-def: RDX
	je	.LBB0_15
.LBB0_5:                                # %block_402745
	movl	6310200, %ecx
	shrl	%ecx
	movl	$4096, %ebx             # imm = 0x1000
	testb	$63, %cl
	je	.LBB0_7
# BB#6:                                 # %subblock_402745_2
	movl	$4096, %ebx             # imm = 0x1000
                                        # kill: CL<def> CL<kill> ECX<kill>
	shlq	%cl, %rbx
.LBB0_7:                                # %block_40277c
	cmpq	%rdx, %rbx
	cmovbeq	%rdx, %rbx
	movd	%r15, %xmm0
	movd	%r12, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%r15d, %r15d
	xorl	%edi, %edi
	movl	$3, %edx
	movl	$34, %ecx
	movl	$4294967295, %r8d       # imm = 0xFFFFFFFF
	xorl	%r9d, %r9d
	movq	%rbx, %rsi
	callq	F401b20
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	cmpq	$-1, %rax
	je	.LBB0_9
# BB#8:                                 # %block_402786
	incl	6310200
	movq	%rbx, (%r14)
                                        # implicit-def: R15
	jmp	.LBB0_9
.LBB0_15:                               # %block_402817
	movq	6310208, %r15
	movq	%rax, 6310208
                                        # implicit-def: RAX
                                        # implicit-def: RCX
.LBB0_9:                                # %block_402791
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rax
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp9:
	.size	F4026e0, .Ltmp9-F4026e0
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
