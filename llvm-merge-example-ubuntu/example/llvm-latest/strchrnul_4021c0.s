	.text
	.file	"strchrnul_4021c0.ll"
	.globl	F4021c0
	.align	16, 0x90
	.type	F4021c0,@function
F4021c0:                                # @F4021c0
	.cfi_startproc
# BB#0:                                 # %block_4021c0
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp3:
	.cfi_def_cfa_offset 48
.Ltmp4:
	.cfi_offset %rbx, -32
.Ltmp5:
	.cfi_offset %r14, -24
.Ltmp6:
	.cfi_offset %r15, -16
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r9
	movzbl	%sil, %edx
	testb	%dl, %dl
	jne	.LBB0_5
# BB#1:                                 # %block_4022ad
	movd	%r8, %xmm0
	movd	%r9, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402300
	jmp	.LBB0_13
	.align	16, 0x90
.LBB0_4:                                # %block_4021eb
                                        #   in Loop: Header=BB0_5 Depth=1
	incq	%rdi
.LBB0_5:                                # %block_4021ef
                                        # =>This Inner Loop Header: Depth=1
	testb	$7, %dil
	je	.LBB0_6
# BB#2:                                 # %block_4021d8
                                        #   in Loop: Header=BB0_5 Depth=1
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB0_12
# BB#3:                                 # %block_4021e3
                                        #   in Loop: Header=BB0_5 Depth=1
	cmpl	%eax, %edx
	jne	.LBB0_4
	jmp	.LBB0_12
.LBB0_6:                                # %block_4021f4
	movabsq	$-9187201950435737472, %r10 # imm = 0x8080808080808080
	movabsq	$-72340172838076673, %r14 # imm = 0xFEFEFEFEFEFEFEFF
	movq	(%rdi), %r11
	movabsq	$72340172838076673, %r15 # imm = 0x101010101010101
	imulq	%rdx, %r15
	movq	%r15, %rsi
	xorq	%r11, %rsi
	leaq	(%r11,%r14), %rcx
	notq	%r11
	andq	%rcx, %r11
	movq	%rsi, %rcx
	notq	%rcx
	addq	%r14, %rsi
	andq	%rcx, %rsi
	orq	%r11, %rsi
	testq	%r10, %rsi
	jne	.LBB0_8
	.align	16, 0x90
.LBB0_7:                                # %block_402241
                                        # =>This Inner Loop Header: Depth=1
	addq	$8, %rdi
	movq	(%rdi), %rsi
	movq	%r15, %rcx
	xorq	%rsi, %rcx
	leaq	(%rsi,%r14), %rax
	notq	%rsi
	leaq	(%rcx,%r14), %rbx
	notq	%rcx
	andq	%rax, %rsi
	andq	%rbx, %rcx
	orq	%rsi, %rcx
	testq	%r10, %rcx
	je	.LBB0_7
.LBB0_8:                                # %block_40227f
	movzbl	(%rdi), %eax
	jmp	.LBB0_10
	.align	16, 0x90
.LBB0_9:                                # %block_402290
                                        #   in Loop: Header=BB0_10 Depth=1
	movzbl	1(%rdi), %eax
	incq	%rdi
.LBB0_10:                               # %block_402290
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB0_12
# BB#11:                                # %block_40229b
                                        #   in Loop: Header=BB0_10 Depth=1
	cmpl	%eax, %edx
	jne	.LBB0_9
.LBB0_12:                               # %block_40229f
	movd	%r8, %xmm0
	movd	%r9, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdi, %rax
.LBB0_13:                               # %block_40229f
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp7:
	.size	F4021c0, .Ltmp7-F4021c0
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
