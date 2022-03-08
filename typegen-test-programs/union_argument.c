// union_argument.c --- passing a type dep. on another type

#include "test.h"

int global_v = 0;

union u {
  int *ptr;
  int val;
} global_u = { .ptr = &global_v };

int NOINLINE
plusplus(int tag, union u *p) {
  if (tag) 
    return ++(*(p->ptr));
  else
    return ++(p->val);
}

void NOINLINE
doit(void) {
  plusplus(global_v, &global_u);
}

int
main(void) {
  doit();
  return 0;    
}

  
