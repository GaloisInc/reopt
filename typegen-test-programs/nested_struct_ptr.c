// mixed_struct_pointer.c --- passing a pointer into a function containing a bitvec and a pointer


#include "test.h"

int global_v = 0;

struct global_struct {
  int *ptr;
  int val;
} global_struct_v = { &global_v, 42 };

struct super_struct {
  int some_val;
  struct global_struct glob;
} global_super_struct;


void NOINLINE
init_super(struct super_struct *p) {
  p->some_val = 42;
  p->glob.ptr = &global_v;
  p->glob.val = 43;
}

void NOINLINE
add(struct global_struct *p) {
  *(p->ptr) += p->val;
}

void NOINLINE
add_super(struct super_struct *p) {
  add(&(p->glob));
}

void NOINLINE
doit(void) {
  init_super(&global_super_struct);
  add_super(&global_super_struct);
  add(&global_struct_v);
}

int
main(void) {
  doit();
  return 0;    
}

  
