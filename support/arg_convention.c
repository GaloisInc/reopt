#include<stdio.h>

void floatFn(float x) {}
void doubleFn(double x) {}

void boolFn(_Bool x) {}

void complexFloatFn(_Complex float x) {}
void complexDoubleFn(_Complex double x) {}

enum foo { x };

int global;

void enumFn(enum foo x) {}

int main(int argc, char** argv) {
  printf("sizeof(bool)     = %lu\n", sizeof(_Bool));
  printf("sizeof(short)     = %lu\n", sizeof(short));
  printf("sizeof(int)       = %lu\n", sizeof(int));
  printf("sizeof(long)      = %lu\n", sizeof(long));
  printf("sizeof(long long) = %lu\n", sizeof(long long));
  _Bool b = 3;
  printf("b = %lu\n", (long unsigned) b);

  return 0;
}
