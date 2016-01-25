
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "data.h"

struct tree g_left = { .left = NULL, .right = NULL, .payload = "left" };
struct tree g_right_right = { .left = NULL, .right = NULL, .payload = "right_right" };
struct tree g_right = { .left = NULL, .right = &g_right_right, .payload = "right" };
struct tree root = { .left = &g_left, .right = &g_right, .payload = "root" };

/*
void
print_tree(int depth, struct tree *t) {
    if (t == NULL)
        return;

    printf("%*sNODE %s\n", depth + 5, " -> ", t->payload);
    print_tree(depth + 1, t->left);
    print_tree(depth + 1, t->right);
}
*/

int tree_equal(const struct tree *t1, const struct tree *t2) {
    if (t1 == NULL)
        return t2 == NULL;

    return (t2 != NULL
            && !STRCMP(t1->payload, t2->payload)
            && tree_equal(t1->left, t2->left)
            && tree_equal(t1->right, t2->right));
}


static void
really_write_tree(FILE *hdl, const struct tree *t) {
    if (t) {
        FPUTS(t->payload, hdl);
        FPUTC(sep, hdl);
        really_write_tree(hdl, t->left);
        really_write_tree(hdl, t->right);
    } else {
        FPUTC(sep, hdl);
    }
}

void
write_tree(const char *filename, const struct tree *t) {
    FILE *hdl = FOPEN(filename, W_STR);
    if (hdl == NULL) {
        // perror("write_tree");
        EXIT(1);
    }

    really_write_tree(hdl, t);
    FCLOSE(hdl);
}

char *
read_tree_payload(FILE *hdl) {
    size_t sz = 8;
    int i = 0;
    char *buf = MALLOC(sz);
    if (buf == NULL) {
        // perror("read_tree_payload (malloc)");
        EXIT(1);
    }

    for (i = 0; !FEOF(hdl); i++) {
        if (i == sz) {
            sz = sz * 2;
            buf = REALLOC(buf, sz);
            if (buf == NULL) {
                // perror("read_tree_payload (realloc)");
                EXIT(1);
            }
        }

        int r = FGETC(hdl);
        if (r == EOF) {
            // perror("read_tree_payload (fgetc)");
            EXIT(1);
        }

        if (r == sep)
            break;

        buf[i] = (char) r;
    }

    if (i == 0) {
        FREE(buf);
        return NULL;
    } else {
        buf[i] = 0;
        return buf;
    }
}

static struct tree*
really_read_tree(FILE *hdl) {
    char *payload = read_tree_payload(hdl);
    if (payload == NULL) {
        return NULL;
    } else {
        struct tree *t = MALLOC(sizeof(struct tree));
        if (t == NULL) {
            //perror("really_read_tree");
            EXIT(1);
        }

        t->payload = payload;
        t->left  = really_read_tree(hdl);
        t->right = really_read_tree(hdl);
        return t;
    }
}

struct tree*
read_tree(char *filename) {
    FILE *hdl = FOPEN(filename, r_str);
    if (hdl == NULL) {
        // perror("read_tree");
        EXIT(1);
    }
    struct tree *t = really_read_tree(hdl);
    FCLOSE(hdl);
    return t;
}

int
main(int argc, char *argv[]) {
    if (argc < 2) {
        // printf("Usage: tree tmpfile\n");
        exit(1);
    }
    char *filename = argv[1];

    // print_tree(0, &root);
    write_tree(filename, & ROOT);
    struct tree *read_in = read_tree(filename);
    // print_tree(0, read_in);
    // printf("Trees are %sequal\n", tree_equal(&root, read_in) ? "" : "NOT ");
    return !(tree_equal(& ROOT, read_in));
}
