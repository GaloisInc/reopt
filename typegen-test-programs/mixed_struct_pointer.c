// mixed_struct_pointer.c --- passing a pointer into a function containing a bitvec and a pointer


#include "test.h"

int global_v = 0;

struct global_struct {
  int *ptr;
  int val;
} global_struct_v = { &global_v, 42 };

void NOINLINE
add(struct global_struct *p) {
  *(p->ptr) += p->val;
}

void NOINLINE
doit(void) {
  add(&global_struct_v);
}

int
main(void) {
  doit();
  return 0;    
}

  
