	.text
	.file	"__wait_40241c.ll"
	.globl	F40241c
	.align	16, 0x90
	.type	F40241c,@function
F40241c:                                # @F40241c
	.cfi_startproc
# BB#0:                                 # %block_40241c
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp5:
	.cfi_def_cfa_offset 80
.Ltmp6:
	.cfi_offset %rbx, -48
.Ltmp7:
	.cfi_offset %r12, -40
.Ltmp8:
	.cfi_offset %r13, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movd	%xmm0, %r15
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r12
	testl	%ecx, %ecx
	movl	$128, %eax
	cmovel	%ecx, %eax
	movl	%eax, %eax
	movl	%edx, %r13d
	movl	$101, %ecx
	.align	16, 0x90
.LBB0_1:                                # %block_402434
                                        # =>This Inner Loop Header: Depth=1
                                        # kill: ECX<def> ECX<kill> RCX<kill> RCX<def>
	cmpl	$1, %ecx
	leal	-1(%rcx), %ecx
	je	.LBB0_5
# BB#2:                                 # %block_402438
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	%r14, %r14
	je	.LBB0_3
# BB#4:                                 # %block_402445
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpl	$0, (%r14)
	jne	.LBB0_6
.LBB0_3:                                # %block_40243d
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpl	%r13d, (%rbx)
	je	.LBB0_1
	jmp	.LBB0_13
.LBB0_5:                                # %block_40244e
	testq	%r14, %r14
	je	.LBB0_7
.LBB0_6:                                # %block_402453
	incl	(%r14)
.LBB0_7:                                # %block_402457
	movslq	%r13d, %rdx
	movslq	%eax, %rsi
	jmp	.LBB0_8
	.align	16, 0x90
.LBB0_10:                               # %block_40247f
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	$202, (%rsp)
	xorl	%esi, %esi
	callq	reopt.SystemCall.Linux
                                        # implicit-def: RSI
                                        # implicit-def: RDX
.LBB0_8:                                # %block_402468
                                        # =>This Inner Loop Header: Depth=1
	cmpl	(%rbx), %r13d
	jne	.LBB0_11
# BB#9:                                 # %block_402479
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	$202, (%rsp)
	xorl	%ecx, %ecx
	movq	%rbx, %rdi
	movq	%r14, %r8
	movq	%rsi, %r9
	callq	reopt.SystemCall.Linux
	cmpq	$-38, %rax
                                        # implicit-def: RSI
                                        # implicit-def: RDX
	jne	.LBB0_8
	jmp	.LBB0_10
.LBB0_11:                               # %block_402488
	testq	%r14, %r14
	je	.LBB0_13
# BB#12:                                # %block_40248d
	decl	(%r14)
.LBB0_13:                               # %block_402491
	movd	%r15, %xmm0
	movd	%r12, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp11:
	.size	F40241c, .Ltmp11-F40241c
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
