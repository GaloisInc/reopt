// This program prints out the sizes of different datatypes
// in bytes so that we can check for reopt compatibility.
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <wchar.h>

int main(int argc, char** argv) {
  printf("sizeof(short)     = %lu\n", sizeof(short));
  printf("sizeof(int)       = %lu\n", sizeof(int));
  printf("sizeof(long)      = %lu\n", sizeof(long));
  printf("sizeof(long long) = %lu\n", sizeof(long long));
  printf("sizeof(intmax_t)  = %lu\n", sizeof(intmax_t));
  printf("sizeof(ssize_t)   = %lu\n", sizeof(ssize_t));
  printf("sizeof(ptrdiff_t) = %lu\n", sizeof(ptrdiff_t));
  printf("sizeof(wint_t)    = %lu\n", sizeof(wint_t));
  printf("sizeof(double)      = %lu\n", sizeof(double));
  printf("sizeof(long double) = %lu\n", sizeof(long double));
  
  
  return 0;
}
