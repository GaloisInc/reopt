	.text
	.file	"main_400603.ll"
	.globl	F400603
	.align	16, 0x90
	.type	F400603,@function
F400603:                                # @F400603
	.cfi_startproc
# BB#0:                                 # %block_400603
	subq	$56, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 64
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r9
	movl	%edi, 28(%rsp)
	movq	%rsi, 16(%rsp)
	movl	28(%rsp), %ecx
	movl	%ecx, %edi
	decl	%edi
	seto	%al
	cmpl	$0, %edi
	seto	%r10b
	cmpl	$1, %ecx
	je	.LBB0_2
# BB#1:                                 # %block_400603
	orb	%r10b, %al
	shrl	$31, %edi
	xorb	%al, %dil
	movzbl	%dil, %eax
	cmpl	$1, %eax
	jne	.LBB0_3
.LBB0_2:                                # %block_400618
	movd	%r8, %xmm0
	movd	%r9, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$158, %edi
	callq	F400130
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r9
.LBB0_3:                                # %block_400680
	movq	16(%rsp), %rax
	movq	8(%rax), %rax
	movq	%rax, 32(%rsp)
	movd	%r8, %xmm0
	movd	%r9, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%edi, %edi
	movl	$6307968, %esi          # imm = 0x604080
	callq	F400256
	movq	32(%rsp), %rdi
	movl	$6307968, %esi          # imm = 0x604080
	callq	F4003c1
	movq	32(%rsp), %rdi
	callq	F4005aa
	xorl	%edi, %edi
	callq	F400256
	movq	40(%rsp), %rsi
	movl	$6307968, %edi          # imm = 0x604080
	callq	F4002a0
	addq	$56, %rsp
	retq
.Ltmp1:
	.size	F400603, .Ltmp1-F400603
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
