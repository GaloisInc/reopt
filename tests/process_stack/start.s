# A start method that pushs
	.globl _start
	.type _start, @function
_start:
	mov %rsp, %rdi
	call run
	mov $60, %rax
	syscall
	hlt
