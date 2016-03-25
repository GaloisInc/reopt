	.text
	.file	"__fmodeflags_4029ad.ll"
	.globl	F4029ad
	.align	16, 0x90
	.type	F4029ad,@function
F4029ad:                                # @F4029ad
	.cfi_startproc
# BB#0:                                 # %block_4029c2
	pushq	%rbp
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1:
	.cfi_def_cfa_offset 24
	subq	$40, %rsp
.Ltmp2:
	.cfi_def_cfa_offset 64
.Ltmp3:
	.cfi_offset %rbx, -24
.Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rdi, %rbx
	movq	%rcx, 16(%rsp)
	movl	$43, %esi
                                        # kill: RDI<def> RBX<kill>
	callq	F4021a0
	movd	%xmm0, %rsi
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	$2, %edx
	testq	%rax, %rax
	jne	.LBB0_5
# BB#1:                                 # %block_4029c7
	movzbl	(%rbx), %edx
	cmpl	$114, %edx
	jne	.LBB0_3
# BB#2:
	xorl	%edx, %edx
	jmp	.LBB0_4
.LBB0_3:                                # %select.mid
	movb	$1, %dl
.LBB0_4:                                # %select.end
	movzbl	%dl, %edx
.LBB0_5:                                # %block_4029f5
	movb	%dl, %al
	orb	$-128, %al
	movl	%edx, %edi
	andl	$-256, %edi
	movzbl	%al, %eax
	orl	%edi, %eax
	testq	%rax, %rax
	cmovel	%edx, %eax
	movl	%eax, %ebp
	orl	$524288, %ebp           # imm = 0x80000
	testq	%rax, %rax
	movd	%rsi, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	cmovel	%eax, %ebp
	movl	$120, %esi
	movq	%rbx, %rdi
	callq	F4021a0
	movl	$101, %esi
	movq	%rbx, %rdi
	callq	F4021a0
	movzbl	(%rbx), %ecx
	cmpq	$114, %rcx
	movl	%ebp, %eax
	je	.LBB0_9
# BB#6:                                 # %block_402a0a
	cmpl	$119, %ecx
	je	.LBB0_7
# BB#8:                                 # %block_402a16
	movl	%eax, %edx
	orl	$1088, %edx             # imm = 0x440
	orl	$64, %eax
	cmpl	$97, %ecx
	cmovel	%edx, %eax
	movl	%eax, %eax
	jmp	.LBB0_9
.LBB0_7:                                # %block_402a0e
	orl	$576, %eax              # imm = 0x240
.LBB0_9:                                # %block_402a26
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	retq
.Ltmp5:
	.size	F4029ad, .Ltmp5-F4029ad
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
