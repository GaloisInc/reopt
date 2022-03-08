// union_in_struct.c --- passing a dep. type

#include "test.h"

int global_v = 0;

union u {
  int *ptr;
  int val;
} global_u = { .ptr = &global_v };

struct s {
  int tag;
  union u u;
} global_s = { 0, { .ptr = &global_v }};

int NOINLINE
plusplus(struct s *p) {
  if (p->tag) 
    return ++(*(p->u.ptr));
  else
    return ++(p->u.val);
}

void NOINLINE
doit(void) {
  plusplus(&global_s);
}

int
main(void) {
  doit();
  return 0;    
}

  
