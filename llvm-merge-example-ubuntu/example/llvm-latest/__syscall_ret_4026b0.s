	.text
	.file	"__syscall_ret_4026b0.ll"
	.globl	F4026b0
	.align	16, 0x90
	.type	F4026b0,@function
F4026b0:                                # @F4026b0
	.cfi_startproc
# BB#0:                                 # %block_4026b0
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
.Ltmp2:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	cmpq	$-4096, %rbx            # imm = 0xFFFFFFFFFFFFF000
	jbe	.LBB0_1
# BB#3:                                 # %block_4026c9
	movq	%rbx, %rdi
	callq	F402681
	negl	%ebx
	movl	%ebx, (%rax)
	movq	$-1, %rax
	jmp	.LBB0_2
.LBB0_1:                                # %block_4026b9
	movq	%rbx, %rax
.LBB0_2:                                # %block_4026b9
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F4026b0, .Ltmp3-F4026b0
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
