// simple_pointer.c --- passing a pointer into a function to be incremented


#include "test.h"

int global_v = 0;

void NOINLINE
inc(int *p) {
  *p += 1;
}

void NOINLINE
doit(void) {
  inc(&global_v);
}

int
main(void) {
  doit();
  return 0;
}
