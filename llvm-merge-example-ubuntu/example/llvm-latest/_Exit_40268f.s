	.text
	.file	"_Exit_40268f.ll"
	.globl	F40268f
	.align	16, 0x90
	.type	F40268f,@function
F40268f:                                # @F40268f
	.cfi_startproc
# BB#0:                                 # %block_40268f
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	movslq	%edi, %rdi
	movq	$231, (%rsp)
	callq	reopt.SystemCall.Linux
	.align	16, 0x90
.LBB0_1:                                # %block_40269e
                                        # =>This Inner Loop Header: Depth=1
	movq	$60, (%rsp)
	callq	reopt.SystemCall.Linux
	jmp	.LBB0_1
.Ltmp1:
	.size	F40268f, .Ltmp1-F40268f
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
