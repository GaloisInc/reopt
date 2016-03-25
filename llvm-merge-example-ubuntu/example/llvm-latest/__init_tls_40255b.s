	.text
	.file	"__init_tls_40255b.ll"
	.globl	F40255b
	.align	16, 0x90
	.type	F40255b,@function
F40255b:                                # @F40255b
	.cfi_startproc
# BB#0:                                 # %block_40255b
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp3:
	.cfi_def_cfa_offset 64
.Ltmp4:
	.cfi_offset %rbx, -32
.Ltmp5:
	.cfi_offset %r14, -24
.Ltmp6:
	.cfi_offset %r15, -16
	movd	%xmm0, %r14
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r15
	movq	24(%rdi), %r8
	movq	40(%rdi), %rsi
	xorl	%eax, %eax
	movq	%r8, %rdx
	xorl	%ecx, %ecx
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_5:                                # %block_402589
                                        #   in Loop: Header=BB0_1 Depth=1
	decq	%rsi
	addq	32(%rdi), %rdx
.LBB0_1:                                # %block_40256a
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB0_6
# BB#2:                                 # %block_40256f
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	(%rdx), %ebx
	cmpq	$6, %rbx
	je	.LBB0_3
# BB#4:                                 # %block_402581
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpl	$7, %ebx
	cmoveq	%rdx, %rcx
	jmp	.LBB0_5
	.align	16, 0x90
.LBB0_3:                                # %block_402578
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r8, %rax
	subq	16(%rdx), %rax
	jmp	.LBB0_5
.LBB0_6:                                # %block_402592
	testq	%rcx, %rcx
	movq	%rdx, 24(%rsp)
	je	.LBB0_8
# BB#7:                                 # %block_402598
	movq	32(%rcx), %rdx
	addq	16(%rcx), %rax
	movq	$6309664, 6310296       # imm = 0x604720
	movq	%rdx, 6309680
	movq	40(%rcx), %rdx
	movq	48(%rcx), %rcx
	movq	%rax, 6309672
	movq	$1, 6310320
	movq	%rdx, 6309688
	movq	%rcx, 6309696
.LBB0_8:                                # %block_4025da
	movq	6309688, %rdx
	movq	6309696, %rsi
	movq	6309672, %rax
	addq	%rdx, %rax
	leaq	-1(%rsi), %rcx
	negq	%rax
	andq	%rcx, %rax
	addq	%rdx, %rax
	movq	%rax, 6309688
	cmpq	$7, %rsi
	ja	.LBB0_10
# BB#9:                                 # %block_40260c
	movq	$8, 6309696
.LBB0_10:                               # %block_402617
	movq	6309696, %rdx
	movq	%rax, 6309704
	leaq	359(%rax,%rdx), %rsi
	movq	%rdx, 6310312
	andq	$-8, %rsi
	movq	%rsi, 6310304
	movl	$6309728, %edi          # imm = 0x604760
	cmpq	$473, %rsi              # imm = 0x1D9
	jb	.LBB0_12
# BB#11:                                # %block_40264f
	movq	$9, (%rsp)
	xorl	%edi, %edi
	movl	$3, %edx
	movl	$34, %ecx
	movq	$-1, %r8
	xorl	%r9d, %r9d
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
                                        # implicit-def: RSI
                                        # implicit-def: RDX
                                        # implicit-def: RCX
.LBB0_12:                               # %block_40267f
	movd	%r14, %xmm0
	movd	%r15, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F4024e2
	callq	F402496
	movq	24(%rsp), %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7:
	.size	F40255b, .Ltmp7-F40255b
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
