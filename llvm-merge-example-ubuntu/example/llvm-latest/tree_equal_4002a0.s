	.text
	.file	"tree_equal_4002a0.ll"
	.globl	F4002a0
	.align	16, 0x90
	.type	F4002a0,@function
F4002a0:                                # @F4002a0
	.cfi_startproc
# BB#0:                                 # %block_4002a0
	subq	$40, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 48
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	cmpq	$0, 24(%rsp)
	je	.LBB0_1
# BB#4:                                 # %block_4002c4
	cmpq	$0, 16(%rsp)
	je	.LBB0_8
# BB#5:                                 # %block_4002e6
	movq	16(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	16(%rax), %rsi
	movq	16(%rcx), %rdi
	callq	F4022c0
	testl	%eax, %eax
	jne	.LBB0_8
# BB#6:                                 # %block_400303
	movq	16(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	(%rax), %rsi
	movq	(%rcx), %rdi
	callq	F4002a0
	testl	%eax, %eax
	je	.LBB0_8
# BB#7:                                 # %block_400322
	movq	16(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	8(%rax), %rsi
	movq	8(%rcx), %rdi
	callq	F4002a0
	movl	$1, %eax
	testl	%eax, %eax
	jne	.LBB0_9
.LBB0_8:                                # %block_40032d
	xorl	%eax, %eax
.LBB0_9:                                # %block_400333
	addq	$40, %rsp
	retq
.LBB0_1:                                # %block_4002b7
	movb	$1, %al
	cmpq	$0, 16(%rsp)
	je	.LBB0_3
# BB#2:                                 # %select.mid
	xorl	%eax, %eax
.LBB0_3:                                # %select.end
	movzbl	%al, %eax
	addq	$40, %rsp
	retq
.Ltmp1:
	.size	F4002a0, .Ltmp1-F4002a0
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
