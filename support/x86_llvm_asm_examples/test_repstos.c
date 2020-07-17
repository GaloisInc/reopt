#include<stdint.h>

void foo(uint64_t cnt, void* rdi, uint64_t val) {
  asm ( "rep stosq"
        :
        : "c" (cnt), "D" (rdi), "a" (val)
        : "memory");
}
