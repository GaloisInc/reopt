	.text
	.file	"read_tree_payload_40041e.ll"
	.globl	F40041e
	.align	16, 0x90
	.type	F40041e,@function
F40041e:                                # @F40041e
	.cfi_startproc
# BB#0:                                 # %block_400445
	subq	$72, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 80
	movq	%rdi, 24(%rsp)
	movq	$8, 48(%rsp)
	movl	$0, 40(%rsp)
	movq	48(%rsp), %rdi
	callq	F401280
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testq	%rax, %rax
	jne	.LBB0_2
# BB#1:                                 # %block_400450
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$89, %edi
	movq	%rdx, %rsi
	callq	F400130
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_2:                                # %block_40045a
	movl	$0, 40(%rsp)
                                        # implicit-def: RAX
	jmp	.LBB0_10
	.align	16, 0x90
.LBB0_9:                                # %block_4004c6
                                        #   in Loop: Header=BB0_10 Depth=1
	movslq	40(%rsp), %rsi
	movq	56(%rsp), %rdi
	movl	44(%rsp), %eax
	movb	%al, (%rdi,%rsi)
	incl	40(%rsp)
.LBB0_10:                               # %block_4004e8
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rsp), %rdi
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	F401d23
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testl	%eax, %eax
	jne	.LBB0_11
# BB#3:                                 # %block_400463
                                        #   in Loop: Header=BB0_10 Depth=1
	movslq	40(%rsp), %rax
	cmpq	48(%rsp), %rax
	jne	.LBB0_6
# BB#4:                                 # %block_400485
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	48(%rsp), %rsi
	addq	%rsi, %rsi
	movq	%rsi, 48(%rsp)
	movq	56(%rsp), %rdi
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rsi, %rdx
	callq	F4018b0
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	testq	%rax, %rax
	jne	.LBB0_6
# BB#5:                                 # %block_400490
                                        #   in Loop: Header=BB0_10 Depth=1
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$98, %edi
	movq	%rdx, %rsi
	callq	F400130
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_6:                                # %block_4004a6
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	24(%rsp), %rdi
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rsi
	callq	F401e7c
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	cmpl	$-1, %eax
	jne	.LBB0_8
# BB#7:                                 # %block_4004af
                                        #   in Loop: Header=BB0_10 Depth=1
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$105, %edi
	movq	%rdx, %rsi
	callq	F400130
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
.LBB0_8:                                # %block_4004b9
                                        #   in Loop: Header=BB0_10 Depth=1
	cmpl	$44, 44(%rsp)
	jne	.LBB0_9
.LBB0_11:                               # %block_4004f3
	cmpl	$0, 40(%rsp)
	je	.LBB0_12
# BB#13:                                # %block_40050c
	movslq	40(%rsp), %rdx
	movq	56(%rsp), %rax
	movb	$0, (%rax,%rdx)
	movq	56(%rsp), %rax
	jmp	.LBB0_14
.LBB0_12:                               # %block_4004f9
	movq	56(%rsp), %rdi
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rdx, %rsi
	callq	F400e40
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rcx
	xorl	%eax, %eax
                                        # implicit-def: RDX
.LBB0_14:                               # %block_400520
	movd	%r8, %xmm0
	movd	%rcx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$72, %rsp
	retq
.Ltmp1:
	.size	F40041e, .Ltmp1-F40041e
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
