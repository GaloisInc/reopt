	.globl mywrite
	.type mywrite, @function
mywrite:
	mov $1, %rax
	syscall
	ret
