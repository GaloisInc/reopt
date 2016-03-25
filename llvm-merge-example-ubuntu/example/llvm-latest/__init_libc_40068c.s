	.text
	.file	"__init_libc_40068c.ll"
	.globl	F40068c
	.align	16, 0x90
	.type	F40068c,@function
F40068c:                                # @F40068c
	.cfi_startproc
# BB#0:                                 # %block_40068c
	pushq	%r15
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp3:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 48
	subq	$368, %rsp              # imm = 0x170
.Ltmp5:
	.cfi_def_cfa_offset 416
.Ltmp6:
	.cfi_offset %rbx, -48
.Ltmp7:
	.cfi_offset %r12, -40
.Ltmp8:
	.cfi_offset %r13, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movd	%xmm0, %r12
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %r13
	leaq	24(%rsp), %r15
	leaq	56(%rsp), %rdi
	movq	%r14, 6309640
	xorl	%esi, %esi
	movl	$76, %edx
	xorl	%ecx, %ecx
	callq	reopt.MemSet.i32
	movl	$1, %eax
	movq	%r14, %rcx
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                # %subblock_4006ad_1
                                        #   in Loop: Header=BB0_1 Depth=1
	incq	%rax
	addq	$8, %rcx
.LBB0_1:                                # %block_4006ad
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%rcx)
	jne	.LBB0_2
# BB#3:                                 # %block_4006b8
	leaq	(%r14,%rax,8), %rax
	movq	%rax, 6310288
	jmp	.LBB0_4
	.align	16, 0x90
.LBB0_7:                                # %block_4006da
                                        #   in Loop: Header=BB0_4 Depth=1
	addq	$16, %rax
.LBB0_4:                                # %block_4006c3
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB0_8
# BB#5:                                 # %block_4006cb
                                        #   in Loop: Header=BB0_4 Depth=1
	cmpq	$37, %rcx
	ja	.LBB0_7
# BB#6:                                 # %block_4006d1
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	8(%rax), %rdx
	movq	%rdx, 32(%r15,%rcx,8)
	jmp	.LBB0_7
.LBB0_8:                                # %block_4006e0
	movq	160(%r15), %rax
	movq	%rax, 6310240
	movq	288(%r15), %rax
	movq	%rax, 6310384
	movq	80(%r15), %rax
	movq	%rax, 6310328
	testq	%rbx, %rbx
	je	.LBB0_9
# BB#13:                                # %block_400737
	movq	%rbx, 6308008
.LBB0_14:                               # %block_40073e
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_15 Depth 2
	movq	%rbx, 6308016
	.align	16, 0x90
.LBB0_15:                               # %block_400745
                                        #   Parent Loop BB0_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_9
# BB#16:                                # %block_40074f
                                        #   in Loop: Header=BB0_15 Depth=2
	andq	$-256, %rax
	orq	%rcx, %rax
	movzbl	%al, %ecx
	incq	%rbx
	cmpl	$47, %ecx
	jne	.LBB0_15
	jmp	.LBB0_14
.LBB0_9:                                # %block_400726
	leaq	32(%r15), %rdi
	movd	%r12, %xmm0
	movd	%r13, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	callq	F40255b
	movq	232(%r15), %rdi
	movq	%rdx, %rsi
	callq	F40068b
	movq	120(%r15), %rax
	cmpq	128(%r15), %rax
	jne	.LBB0_17
# BB#10:                                # %block_400755
	movq	144(%r15), %rax
	cmpq	%rax, 136(%r15)
	jne	.LBB0_17
# BB#11:                                # %block_400767
	cmpq	$0, 216(%r15)
	je	.LBB0_12
.LBB0_17:                               # %block_400772
	movq	%r15, %rbx
	addq	$8, %rbx
	xorl	%esi, %esi
	movl	$6, %edx
	xorl	%ecx, %ecx
	movq	%rbx, %rdi
	callq	reopt.MemSet.i32
	movl	$1, 16(%r15)
	movl	$2, 24(%r15)
	movq	$7, (%rsp)
	movl	$3, %esi
	xorl	%edx, %edx
	movq	%rbx, %rdi
	callq	reopt.SystemCall.Linux
	movl	$2, %esi
	movl	$4206456, %edi          # imm = 0x402F78
                                        # implicit-def: R8
                                        # implicit-def: RBX
                                        # implicit-def: RCX
                                        # implicit-def: RDX
	.align	16, 0x90
.LBB0_18:                               # %block_4007b4
                                        # =>This Inner Loop Header: Depth=1
	testb	$32, 6(%rcx,%rdx,8)
	je	.LBB0_20
# BB#19:                                # %block_4007c1
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$2, (%rsp)
	callq	reopt.SystemCall.Linux
                                        # implicit-def: R8
                                        # implicit-def: RBX
                                        # implicit-def: RCX
                                        # implicit-def: RDI
                                        # implicit-def: RSI
                                        # implicit-def: RDX
.LBB0_20:                               # %block_4007c7
                                        #   in Loop: Header=BB0_18 Depth=1
	cmpq	$2, %rdx
	leaq	1(%rdx), %rdx
	jne	.LBB0_18
# BB#21:                                # %block_4007d0
	movl	$1, 6310280
.LBB0_22:                               # %block_4007da
	movd	%r8, %xmm0
	movd	%rbx, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	addq	$368, %rsp              # imm = 0x170
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_12:
	movd	%xmm0, %r8
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rbx
                                        # implicit-def: RDX
	jmp	.LBB0_22
.Ltmp11:
	.size	F40068c, .Ltmp11-F40068c
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
