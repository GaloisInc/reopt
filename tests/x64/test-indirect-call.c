#include "util.h"

int g = -11;

int (*fptr)(int);

int callee(int x) {
  return x * 2;
}

void _start() {
  fptr = callee;
  g = fptr(g);

  EXIT();
}
