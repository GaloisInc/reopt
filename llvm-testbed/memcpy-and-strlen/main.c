// Compare original 'memcpy' and 'strlen' to reopted versions
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// memcpy.
//
// In addition to doing the copy, 'memcpy' returns a pointer to the
// result (should be the same as in the input 'dest', no?)
void *F400a28(void *dest, const void *src, size_t n);
void *(*reopted_memcpy)(void *dest, const void *src, size_t n) = F400a28;

// strlen
size_t F4009b0(const char *s);
size_t (*reopted_strlen)(const char *s) = F4009b0;

int main(void)
{
  printf("Equalities should hold:\n");
  
  char b1[] = "Time flies like the wind; fruit flies like bananas.";
  
  // Destinations for (r)eopt and (o)riginal 'memcpy'.
  char *c1_r = malloc(strlen(b1) + 1);
  char *c1_o = malloc(strlen(b1) + 1);
  
  char *c1_r_ret = reopted_memcpy(c1_r, b1, strlen(b1) + 1);
  char *c1_o_ret = memcpy(c1_o, b1, strlen(b1) + 1);
  
  printf("%ld = %ld = %ld = %ld = %ld = %ld\n",
         strlen(b1), strlen(c1_o), strlen(c1_r), 
         reopted_strlen(b1), reopted_strlen(c1_o), reopted_strlen(c1_r));

  printf("%p = %p\n",
         (void *) c1_o, (void *) c1_o_ret);

  printf("%p = %p\n",
         (void *) c1_r, (void *) c1_r_ret);
  
  printf("%s =\n%s =\n%s =\n%s =\n%s\n", 
         b1, c1_o, c1_r, c1_o_ret, c1_r_ret);
  
  return 0;
}
