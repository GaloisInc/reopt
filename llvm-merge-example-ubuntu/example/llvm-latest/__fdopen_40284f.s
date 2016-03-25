	.text
	.file	"__fdopen_40284f.ll"
	.globl	F40284f
	.align	16, 0x90
	.type	F40284f,@function
F40284f:                                # @F40284f
	.cfi_startproc
# BB#0:                                 # %block_402869
	pushq	%r14
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp1:
	.cfi_def_cfa_offset 24
	subq	$56, %rsp
.Ltmp2:
	.cfi_def_cfa_offset 80
.Ltmp3:
	.cfi_offset %rbx, -24
.Ltmp4:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movl	%edi, %r14d
	movsbl	(%rbx), %esi
	movl	$4206466, %edi          # imm = 0x402F82
	callq	F4021a0
	movd	%xmm0, %rcx
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movd	%rcx, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	testq	%rax, %rax
	je	.LBB0_1
# BB#2:                                 # %block_40288a
	movl	$1264, %edi             # imm = 0x4F0
	movq	%rdx, %rsi
	callq	F401280
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testq	%rax, %rax
	je	.LBB0_20
# BB#3:                                 # %block_4028b4
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%esi, %esi
	movl	$232, %edx
	callq	F402d93
	movl	$43, %esi
	movq	%rbx, %rdi
	callq	F4021a0
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testq	%rax, %rax
	jne	.LBB0_7
# BB#4:                                 # %block_4028b9
	movzbl	(%rbx), %esi
	movb	$1, %dl
	cmpl	$114, %esi
	je	.LBB0_6
# BB#5:                                 # %select.mid
	xorl	%edx, %edx
.LBB0_6:                                # %select.end
	movzbl	%dl, %edx
	leal	4(,%rdx,4), %edx
	movl	%edx, (%rax)
.LBB0_7:                                # %block_4028d9
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$101, %esi
	movq	%rbx, %rdi
	callq	F4021a0
	testq	%rax, %rax
	je	.LBB0_8
# BB#9:                                 # %block_4028de
	movslq	%r14d, %rdi
	movq	$72, (%rsp)
	movl	$2, %esi
	movl	$1, %edx
	callq	reopt.SystemCall.Linux
                                        # implicit-def: RAX
                                        # implicit-def: RCX
                                        # implicit-def: RDX
	jmp	.LBB0_10
.LBB0_1:                                # %block_402873
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	F402681
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	movl	$22, (%rax)
	jmp	.LBB0_20
.LBB0_8:
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_10:                               # %block_4028f2
	movzbl	(%rbx), %esi
	cmpl	$97, %esi
	jne	.LBB0_14
# BB#11:                                # %block_40290c
	movslq	%r14d, %rdi
	movq	$72, (%rsp)
	movl	$3, %esi
	callq	reopt.SystemCall.Linux
	movq	%rax, %rcx
	shrq	$8, %rcx
	testb	$4, %cl
	jne	.LBB0_13
# BB#12:                                # %block_402911
	orb	$4, %cl
	movl	%eax, %edx
	andl	$-65536, %edx           # imm = 0xFFFFFFFFFFFF0000
	movzbl	%cl, %ecx
	shlq	$8, %rcx
	orl	%edx, %ecx
	movzbl	%al, %eax
	orl	%ecx, %eax
	movslq	%eax, %rdx
	movq	$72, (%rsp)
	movl	$4, %esi
	callq	reopt.SystemCall.Linux
.LBB0_13:                               # %block_402921
	orb	$-128, (%rax)
                                        # implicit-def: RAX
                                        # implicit-def: RCX
                                        # implicit-def: RDX
.LBB0_14:                               # %block_402927
	testb	$8, (%rax)
	movl	%r14d, (%rax)
	movq	$1024, (%rax)           # imm = 0x400
	movb	$-1, (%rax)
	jne	.LBB0_17
# BB#15:                                # %block_40295d
	leaq	16(%rsp), %rdx
	movslq	%r14d, %rdi
	addq	$8, %rdx
	movq	$16, (%rsp)
	movl	$21523, %esi            # imm = 0x5413
	callq	reopt.SystemCall.Linux
	testq	%rax, %rax
	jne	.LBB0_17
# BB#16:                                # %block_402962
	movb	$10, (%rax)
                                        # implicit-def: RAX
                                        # implicit-def: RCX
                                        # implicit-def: RDX
.LBB0_17:                               # %block_402969
	cmpl	$0, 6310276
	movq	$F402b4f, (%rax)
	movq	$F402c09, (%rax)
	movq	$F402bf3, (%rax)
	movq	$F402b32, (%rax)
	jne	.LBB0_19
# BB#18:                                # %block_402992
	movl	$-1, (%rax)
.LBB0_19:                               # %block_40299c
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rsi
	callq	F402d6a
	movd	%xmm0, %rax
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_20:                               # %block_4029a4
	movd	%rax, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	xorl	%eax, %eax
	addq	$56, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp5:
	.size	F40284f, .Ltmp5-F40284f
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
