#define EXIT() \
  asm("movq $60,%rax\n"              \
      "movq $0,%rdi\n"               \
      "syscall")
