	.text
	.file	"__init_tp_402496.ll"
	.globl	F402496
	.align	16, 0x90
	.type	F402496,@function
F402496:                                # @F402496
	.cfi_startproc
# BB#0:                                 # %block_4024a2
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 48
.Ltmp2:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rbx, (%rbx)
                                        # kill: RDI<def> RBX<kill>
	callq	F402e57
	movl	$4294967295, %eax       # imm = 0xFFFFFFFF
	testl	%eax, %eax
	js	.LBB0_1
# BB#2:                                 # %block_4024a9
	testl	%eax, %eax
	jne	.LBB0_4
# BB#3:                                 # %block_4024ab
	movl	$1, 6310272
.LBB0_4:                                # %block_4024c0
	leaq	56(%rbx), %rdi
	movq	$218, (%rsp)
	callq	reopt.SystemCall.Linux
	movl	%eax, 56(%rbx)
	leaq	224(%rbx), %rax
	movq	$6310336, 256(%rbx)     # imm = 0x6049C0
	movq	%rax, 224(%rbx)
	xorl	%eax, %eax
                                        # implicit-def: RCX
                                        # implicit-def: RDX
	jmp	.LBB0_5
.LBB0_1:
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rdx
.LBB0_5:                                # %block_4024de
	movd	%rcx, %xmm0
	movd	%rdx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rax, %rdx
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F402496, .Ltmp3-F402496
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
