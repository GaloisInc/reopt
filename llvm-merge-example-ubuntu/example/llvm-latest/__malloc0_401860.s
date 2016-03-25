	.text
	.file	"__malloc0_401860.ll"
	.globl	F401860
	.align	16, 0x90
	.type	F401860,@function
F401860:                                # @F401860
	.cfi_startproc
# BB#0:                                 # %block_401869
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
	callq	F401280
	testq	%rax, %rax
	je	.LBB0_6
# BB#1:                                 # %block_40186e
	testb	$1, (%rax)
	je	.LBB0_6
# BB#2:                                 # %block_401874
	addq	$7, %rbx
	shrq	$3, %rbx
	je	.LBB0_6
	.align	16, 0x90
.LBB0_3:                                # %block_401888
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%rax)
	je	.LBB0_5
# BB#4:                                 # %block_40188e
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$0, (%rax)
.LBB0_5:                                # %block_401895
                                        #   in Loop: Header=BB0_3 Depth=1
	addq	$8, %rax
	cmpq	$1, %rbx
	leaq	-1(%rbx), %rbx
	jne	.LBB0_3
.LBB0_6:                                # %block_40189f
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp3:
	.size	F401860, .Ltmp3-F401860
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
