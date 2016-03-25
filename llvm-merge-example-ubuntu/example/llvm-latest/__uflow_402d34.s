	.text
	.file	"__uflow_402d34.ll"
	.globl	F402d34
	.align	16, 0x90
	.type	F402d34,@function
F402d34:                                # @F402d34
	.cfi_startproc
# BB#0:                                 # %block_402d41
	pushq	%r14
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
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	F402ee9
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	testl	%eax, %eax
	jne	.LBB0_3
# BB#1:                                 # %block_402d5a
	leaq	16(%rsp), %r14
	leaq	15(%r14), %rsi
	movd	%rcx, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$1, %edx
	movq	%rbx, %rdi
	callq	*64(%rbx)
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	cmpq	$1, %rax
	jne	.LBB0_3
# BB#2:                                 # %block_402d5f
	movzbl	15(%r14), %eax
.LBB0_3:                                # %block_402d64
	movd	%rcx, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$40, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5:
	.size	F402d34, .Ltmp5-F402d34
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
