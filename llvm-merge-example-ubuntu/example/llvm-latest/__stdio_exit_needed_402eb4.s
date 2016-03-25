	.text
	.file	"__stdio_exit_needed_402eb4.ll"
	.globl	F402eb4
	.align	16, 0x90
	.type	F402eb4,@function
F402eb4:                                # @F402eb4
	.cfi_startproc
# BB#0:                                 # %block_402eba
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
.Ltmp2:
	.cfi_offset %rbx, -16
	callq	F40217e
	movq	%rdx, %rsi
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movq	(%rax), %rbx
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                # %block_402eca
                                        #   in Loop: Header=BB0_1 Depth=1
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	callq	F402e67
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movq	112(%rbx), %rbx
                                        # implicit-def: RSI
.LBB0_1:                                # %block_402ebd
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB0_2
# BB#3:                                 # %block_402edc
	movq	6310216, %rdi
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F402e67
	movq	6309616, %rdi
	callq	F402e67
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F402eb4, .Ltmp3-F402eb4
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
