	.text
	.file	"strlen_402300.ll"
	.globl	F402300
	.align	16, 0x90
	.type	F402300,@function
F402300:                                # @F402300
	.cfi_startproc
# BB#0:                                 # %block_402300
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r9
	testb	$7, %dil
	movq	%rdi, %rax
	je	.LBB0_6
# BB#1:                                 # %block_402306
	cmpb	$0, (%rdi)
	je	.LBB0_12
# BB#2:                                 # %block_40230b
	leaq	1(%rdi), %rax
	jmp	.LBB0_5
	.align	16, 0x90
.LBB0_4:                                # %subblock_402310_2
                                        #   in Loop: Header=BB0_5 Depth=1
	incq	%rax
.LBB0_5:                                # %block_402315
                                        # =>This Inner Loop Header: Depth=1
	testb	$7, %al
	je	.LBB0_6
# BB#3:                                 # %block_402310
                                        #   in Loop: Header=BB0_5 Depth=1
	cmpb	$0, (%rax)
	jne	.LBB0_4
	jmp	.LBB0_11
.LBB0_6:                                # %block_40231d
	movabsq	$-9187201950435737472, %r11 # imm = 0x8080808080808080
	movabsq	$-72340172838076673, %rcx # imm = 0xFEFEFEFEFEFEFEFF
	movq	(%rax), %rdx
	leaq	(%rdx,%rcx), %r10
	notq	%rdx
	andq	%r10, %rdx
	jmp	.LBB0_8
	.align	16, 0x90
.LBB0_7:                                # %block_402343
                                        #   in Loop: Header=BB0_8 Depth=1
	addq	$8, %rax
	movq	(%rax), %rdx
	leaq	(%rdx,%rcx), %rsi
	notq	%rdx
	andq	%rsi, %rdx
.LBB0_8:                                # %block_402343
                                        # =>This Inner Loop Header: Depth=1
	testq	%r11, %rdx
	je	.LBB0_7
	jmp	.LBB0_10
	.align	16, 0x90
.LBB0_9:                                # %block_402360
                                        #   in Loop: Header=BB0_10 Depth=1
	incq	%rax
.LBB0_10:                               # %block_402364
                                        # =>This Inner Loop Header: Depth=1
	cmpb	$0, (%rax)
	jne	.LBB0_9
.LBB0_11:                               # %block_402369
	subq	%rdi, %rax
	movd	%r8, %xmm0
	movd	%r9, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	retq
.LBB0_12:                               # %block_402372
	movd	%r8, %xmm0
	movd	%r9, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%eax, %eax
	retq
.Ltmp0:
	.size	F402300, .Ltmp0-F402300
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
