	.text
	.file	"strcmp_4022c0.ll"
	.globl	F4022c0
	.align	16, 0x90
	.type	F4022c0,@function
F4022c0:                                # @F4022c0
	.cfi_startproc
# BB#0:                                 # %block_4022c0
	movzbl	(%rdi), %ecx
	movzbl	(%rsi), %eax
	cmpl	%ecx, %eax
	jne	.LBB0_5
# BB#1:                                 # %subblock_4022c0_1
	incq	%rdi
	incq	%rsi
	xorl	%ecx, %ecx
	jmp	.LBB0_4
	.align	16, 0x90
.LBB0_3:                                # %subblock_4022d0_2
                                        #   in Loop: Header=BB0_4 Depth=1
	incq	%rdi
	incq	%rsi
.LBB0_4:                                # %block_4022e2
                                        # =>This Inner Loop Header: Depth=1
	testb	%al, %al
	je	.LBB0_5
# BB#2:                                 # %block_4022d0
                                        #   in Loop: Header=BB0_4 Depth=1
	movzbl	(%rdi), %eax
	movzbl	(%rsi), %edx
	cmpl	%edx, %eax
	je	.LBB0_3
# BB#6:                                 # %block_4022f0
	subl	%edx, %eax
	retq
.LBB0_5:                                # %block_4022e8
	subl	%eax, %ecx
	movq	%rcx, %rax
	retq
.Ltmp0:
	.size	F4022c0, .Ltmp0-F4022c0
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
