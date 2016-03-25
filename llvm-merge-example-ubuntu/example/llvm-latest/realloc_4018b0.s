	.text
	.file	"realloc_4018b0.ll"
	.globl	F4018b0
	.align	16, 0x90
	.type	F4018b0,@function
F4018b0:                                # @F4018b0
	.cfi_startproc
# BB#0:                                 # %block_4018b0
	pushq	%rbp
.Ltmp0:
	.cfi_def_cfa_offset 16
	pushq	%r15
.Ltmp1:
	.cfi_def_cfa_offset 24
	pushq	%r14
.Ltmp2:
	.cfi_def_cfa_offset 32
	pushq	%r13
.Ltmp3:
	.cfi_def_cfa_offset 40
	pushq	%r12
.Ltmp4:
	.cfi_def_cfa_offset 48
	pushq	%rbx
.Ltmp5:
	.cfi_def_cfa_offset 56
	subq	$104, %rsp
.Ltmp6:
	.cfi_def_cfa_offset 160
.Ltmp7:
	.cfi_offset %rbx, -56
.Ltmp8:
	.cfi_offset %r12, -48
.Ltmp9:
	.cfi_offset %r13, -40
.Ltmp10:
	.cfi_offset %r14, -32
.Ltmp11:
	.cfi_offset %r15, -24
.Ltmp12:
	.cfi_offset %rbp, -16
	movq	%r9, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	testq	%r15, %r15
	je	.LBB0_24
# BB#1:                                 # %block_4018b9
	leaq	-1(%rbx), %rdx
	movabsq	$9223372036854771679, %rdi # imm = 0x7FFFFFFFFFFFEFDF
	leaq	1(%rdi), %rax
	cmpq	%rax, %rbx
	setne	%al
	cmpq	%rdi, %rdx
	jb	.LBB0_3
# BB#2:                                 # %block_4018b9
	testb	%al, %al
	je	.LBB0_3
# BB#27:                                # %block_401980
	movl	$32, %ebp
	testq	%rbx, %rbx
	je	.LBB0_4
# BB#28:                                # %block_401afd
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	F402681
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movl	$12, (%rax)
.LBB0_26:                               # %block_401af1
	xorl	%r15d, %r15d
                                        # implicit-def: RDX
	jmp	.LBB0_12
.LBB0_24:                               # %block_401ac0
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	movq	%r12, %r9
	callq	F401280
	jmp	.LBB0_13
.LBB0_3:                                # %block_4018e1
	leaq	47(%rbx), %rbp
	andq	$-32, %rbp
.LBB0_4:                                # %block_4018e9
	movq	-8(%r15), %rax
	leaq	-16(%r15), %rdi
	movq	%rax, %r13
	andq	$-2, %r13
	testb	$1, %al
	je	.LBB0_5
# BB#9:                                 # %block_4019a3
	cmpq	%rbp, %r13
	jae	.LBB0_10
# BB#18:                                # %block_401a58
	leaq	(%rdi,%r13), %r14
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r8, 8(%rsp)            # 8-byte Spill
	callq	F4009a0
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	testl	%eax, %eax
	je	.LBB0_21
# BB#19:                                # %block_401a5c
	movq	-8(%r13,%r15), %rdx
	andq	$-2, %rdx
	leaq	(%r13,%rdx), %rcx
	movq	%rcx, %rax
	orq	$1, %rax
	movq	%rax, -8(%r15)
	movq	%rax, (%r14,%rdx)
	cmpq	%rcx, %rbp
	ja	.LBB0_22
# BB#20:
	movq	8(%rsp), %r8            # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB0_11
.LBB0_5:                                # %block_401913
	movq	-16(%r15), %rax
	leaq	(%rax,%rbp), %r14
	cmpq	$4096, %r14             # imm = 0x1000
	jae	.LBB0_6
# BB#15:                                # %block_401a18
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbp, %rdi
	movq	%rbx, %rsi
	movq	%r8, %rbx
	movq	%r12, %r9
	callq	F401280
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	testq	%rax, %rax
	je	.LBB0_16
# BB#17:                                # %block_401a3d
	addq	$-16, %rbp
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rsi
	movq	%rbp, %rdx
	callq	F402375
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	F400e40
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	movq	40(%rsp), %r15
                                        # implicit-def: RDX
	jmp	.LBB0_12
.LBB0_10:                               # %block_4019ac
	movq	%r13, %rax
	orq	$1, %rax
	movq	%rax, -8(%r15)
	movq	%rax, -16(%r13,%r15)
.LBB0_11:                               # %block_4019bb
	andq	$-2, %rax
	leaq	-16(%rax), %rdx
	cmpq	%rdx, %rbp
	jae	.LBB0_12
# BB#14:                                # %block_4019e0
	movq	%rax, %rdx
	subq	%rbp, %rdx
	movq	%rbp, %rcx
	orq	$1, %rcx
	leaq	16(%rdi,%rbp), %rdi
	orq	$1, %rdx
	movq	%rcx, -16(%rbp,%r15)
	movq	%rdx, -8(%rbp,%r15)
	movq	%rdx, -16(%r15,%rax)
	movq	%rcx, -8(%r15)
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbx, %rsi
	movq	%r12, %r9
	callq	F400e40
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
                                        # implicit-def: RDX
	jmp	.LBB0_12
.LBB0_21:                               # %block_401ad0
	movq	%r13, %rax
	orq	$1, %rax
	movq	%rax, -8(%r15)
	movq	%rax, -16(%r13,%r15)
                                        # implicit-def: RDX
                                        # implicit-def: RCX
.LBB0_22:                               # %block_401a8a
	addq	$-16, %rbp
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%rbp, %rdi
	callq	F401280
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	testq	%rax, %rax
	je	.LBB0_26
# BB#23:                                # %block_401aaa
	addq	$-16, %r13
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	F402375
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	F400e40
	jmp	.LBB0_13
.LBB0_16:
                                        # implicit-def: RDX
	movq	%rbx, %r8
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB0_6:                                # %block_401920
	addq	%rax, %r13
	addq	$4095, %r14             # imm = 0xFFF
	andq	$-4096, %r14            # imm = 0xFFFFFFFFFFFFF000
	cmpq	%r14, %r13
	je	.LBB0_12
# BB#7:                                 # %block_40194f
	subq	%rax, %rdi
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movl	$1, %ecx
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%rax, %rbx
	callq	F401bb6
	movd	%xmm0, %r9
	pshufd	$78, %xmm0, %xmm0       # xmm0 = xmm0[2,3,0,1]
	movd	%xmm0, %rsi
	cmpq	$-1, %rax
	je	.LBB0_25
# BB#8:                                 # %block_401959
	subq	%rbx, %r14
	movq	%r14, (%rax)
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	jmp	.LBB0_13
.LBB0_25:                               # %block_401ae8
	cmpq	%r13, %r14
                                        # implicit-def: RDX
	jae	.LBB0_26
.LBB0_12:                               # %block_4019cb
	movd	%r9, %xmm0
	movd	%rsi, %xmm1
	punpcklqdq	%xmm1, %xmm0    # xmm0 = xmm0[0],xmm1[0]
	movq	%r15, %rax
.LBB0_13:                               # %block_4019cb
	addq	$104, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp13:
	.size	F4018b0, .Ltmp13-F4018b0
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
