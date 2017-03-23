	.file	"test-tail-call.c"
	.text
	.p2align 4,,15
	.globl	callee2
	.type	callee2, @function
callee2:
.LFB0:
	.cfi_startproc
	addl	%edi, g(%rip)
	ret
	.cfi_endproc
.LFE0:
	.size	callee2, .-callee2
	.p2align 4,,15
	.globl	callee1
	.type	callee1, @function
callee1:
.LFB1:
	.cfi_startproc
	leal	(%rdi,%rdi), %eax
	addl	%eax, g(%rip)
	jmp	callee2
	.cfi_endproc
.LFE1:
	.size	callee1, .-callee1
	.p2align 4,,15
	.globl	_start
	.type	_start, @function
_start:
.LFB2:
	.cfi_startproc
	movl	g(%rip), %edi
	call	callee1
#APP
# 18 "test-tail-call.c" 1
	movq $60,%rax
movq $0,%rdi
syscall
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE2:
	.size	_start, .-_start
	.globl	g
	.data
	.align 4
	.type	g, @object
	.size	g, 4
g:
	.long	-11
	.ident	"GCC: (Ubuntu 6.2.0-5ubuntu12) 6.2.0 20161005"
	.section	.note.GNU-stack,"",@progbits
