	.text
	.file	"__stdio_write_402c09.ll"
	.globl	F402c09
	.align	16, 0x90
	.type	F402c09,@function
F402c09:                                # @F402c09
	.cfi_startproc
# BB#0:                                 # %block_402c09
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
	subq	$104, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 144
.Ltmp5:
	.cfi_offset %rbx, -40
.Ltmp6:
	.cfi_offset %r12, -32
.Ltmp7:
	.cfi_offset %r14, -24
.Ltmp8:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	leaq	16(%rsp), %rbx
	movq	40(%r14), %r12
	movq	56(%r14), %rax
	movq	%rsi, 32(%rsp)
	movq	%rdx, 40(%rsp)
	subq	%rax, %r12
	movq	%rax, 16(%rsp)
	movq	%r12, 24(%rsp)
	addq	%rdx, %r12
	movl	$2, %r15d
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_8:                                # %block_402ccc
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	%rax, (%rbx)
	subq	%rax, 8(%rbx)
                                        # implicit-def: R12
.LBB0_1:                                # %block_402c64
                                        # =>This Inner Loop Header: Depth=1
	movslq	120(%r14), %rdi
	movslq	%r15d, %rdx
	movq	$20, (%rsp)
	movq	%rbx, %rsi
	callq	reopt.SystemCall.Linux
	movq	%rax, %rdi
	callq	F4026b0
	cmpq	%rax, %r12
	je	.LBB0_2
# BB#4:                                 # %block_402c85
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	%rax, %rax
	js	.LBB0_5
# BB#6:                                 # %block_402cb6
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	8(%rbx), %rax
	jbe	.LBB0_8
# BB#7:                                 # %block_402cc2
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$16, %rbx
	decl	%r15d
	jmp	.LBB0_8
.LBB0_2:                                # %block_402c69
	movq	88(%r14), %rax
	movq	96(%r14), %rcx
	addq	%rax, %rcx
	movq	%rax, 56(%r14)
	movq	%rax, 40(%r14)
	movq	%rcx, 32(%r14)
	jmp	.LBB0_3
.LBB0_5:                                # %block_402c8a
	orb	$32, (%r14)
	xorps	%xmm0, %xmm0
	movups	%xmm0, 32(%r14)
	movq	$0, 56(%r14)
.LBB0_3:                                # %block_402cd9
	addq	$104, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp9:
	.size	F402c09, .Ltmp9-F402c09
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
