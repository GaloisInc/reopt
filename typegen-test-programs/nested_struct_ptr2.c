// mixed_struct_ptr2.c --- passing a pointer into a function containing a bitvec and a pointer


#include "test.h"

int global_v = 0;

struct global_struct {
  int *ptr;
  int val;
} global_struct_v = { &global_v, 42 };

struct super_struct {
  int some_val;
  struct global_struct glob;
  int *some_pointer;
};

// models global heap
struct global_data {
  // padding doesn't seem to matter, nor does the order.
  //  unsigned long padding1[4];
  struct global_struct s2;
  // unsigned long padding2[4];
  struct super_struct s1;
  // unsigned long padding3[4];
} global_data_v;

void NOINLINE
init_super(struct super_struct *p) {
  p->some_val = 42;
  p->glob.ptr = &global_v;
  p->glob.val = 43;
  p->some_pointer = &p->some_val;
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
doit(struct global_data *p) {
  init_super(&p->s1);
  add_super(&p->s1);
  add(&p->s2);
}

int
main(void) {
  doit(&global_data_v);
  return 0;    
}

  
