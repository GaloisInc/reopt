// Use 'memcpy' and 'strlen' so we can reopt them.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main() {
/*
int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: %s STRING\n", argv[0]);
    exit(2);
  }

  char *orig = argv[1];
  */
  char *orig = "Time flies like the wind; fruit flies like bananas";
  char *copy = malloc(strlen(orig) + 1);
  char *copy_ret = memcpy(copy, orig, strlen(orig) + 1);
  /*
  printf("%p = %p\n", (void *) copy, (void *) copy_ret);
  printf("%s\n", copy);
  */

  // Eliminate warming about unused 'copy_ret'.
  return ! (copy_ret == copy_ret);
}
