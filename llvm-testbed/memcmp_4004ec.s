	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 14, 5
	.globl	_F4004ec
	.align	4, 0x90
_F4004ec:                               ## @F4004ec
	.cfi_startproc
## BB#0:                                ## %block_4004ec
	xorl	%r8d, %r8d
	jmp	LBB0_1
	.align	4, 0x90
LBB0_3:                                 ## %subblock_4004f3_1
                                        ##   in Loop: Header=BB0_1 Depth=1
	incq	%r8
LBB0_1:                                 ## %block_4004ee
                                        ## =>This Inner Loop Header: Depth=1
	cmpq	%rdx, %r8
	movl	$0, %eax
	je	LBB0_4
## BB#2:                                ## %block_4004f3
                                        ##   in Loop: Header=BB0_1 Depth=1
	movzbl	(%r8,%rdi), %eax
	movzbl	(%r8,%rsi), %ecx
	subl	%ecx, %eax
	je	LBB0_3
LBB0_4:                                 ## %block_400508
	retq
	.cfi_endproc


.subsections_via_symbols
