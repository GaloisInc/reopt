// nested_mutual_rec_types.c --- Nested, mutually recursive types.

#include <stdlib.h>
#include "test.h"

struct forest;

struct tree {
  int val;
  struct forest *children;
};

struct forest {
  struct tree *child;
  struct forest *next;
};

struct tree global_tree;

int NOINLINE tree_sum(struct tree *p);

int NOINLINE
forest_sum(struct forest *p) {
  int sum = 0;
  for (struct forest *ptr = p; p; p = p->next) {
    sum += tree_sum(p->child);
  }
  return sum;
}

int NOINLINE
tree_sum(struct tree *p) {
  if (p == NULL) {
    return 0;
  } else {
    int children_sum = forest_sum(p->children);
    return (children_sum + p->val);
  }
}

void NOINLINE
doit(void) {
  tree_sum(&global_tree);
}

int
main(void) {
  doit();
  return 0;    
}

  
