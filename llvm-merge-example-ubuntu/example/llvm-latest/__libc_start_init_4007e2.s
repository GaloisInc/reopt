	.text
	.file	"__libc_start_init_4007e2.ll"
	.globl	F4007e2
	.align	16, 0x90
	.type	F4007e2,@function
F4007e2:                                # @F4007e2
	.cfi_startproc
# BB#0:                                 # %block_4007e2
	pushq	%rbx
.Ltmp0:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
.Ltmp2:
	.cfi_offset %rbx, -16
	callq	F400120
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movl	$6307816, %ebx          # imm = 0x603FE8
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                # %block_4007fa
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	*(%rbx)
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	addq	$8, %rbx
.LBB0_1:                                # %block_4007ed
                                        # =>This Inner Loop Header: Depth=1
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	cmpq	$6307823, %rbx          # imm = 0x603FEF
	jbe	.LBB0_2
# BB#3:                                 # %block_400800
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F4007e2, .Ltmp3-F4007e2
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
