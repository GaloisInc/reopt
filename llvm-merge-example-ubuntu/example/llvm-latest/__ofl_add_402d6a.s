	.text
	.file	"__ofl_add_402d6a.ll"
	.globl	F402d6a
	.align	16, 0x90
	.type	F402d6a,@function
F402d6a:                                # @F402d6a
	.cfi_startproc
# BB#0:                                 # %block_402d73
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
.Ltmp2:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	F40217e
	movd	%xmm0, %rsi
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdi
	movq	(%rax), %rcx
	movq	%rcx, 112(%rbx)
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB0_2
# BB#1:                                 # %block_402d82
	movq	%rbx, 104(%rcx)
.LBB0_2:                                # %block_402d8e
	movq	%rbx, (%rax)
	movd	%rsi, %xmm0
	movd	%rdi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	F402190
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F402d6a, .Ltmp3-F402d6a
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
