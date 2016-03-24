
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "data.h"

struct tree g_left = { .left = NULL, .right = NULL, .payload = "left" };
struct tree g_right_right = { .left = NULL, .right = NULL, .payload = "right_right" };
struct tree g_right = { .left = NULL, .right = &g_right_right, .payload = "right" };
struct tree root = { .left = &g_left, .right = &g_right, .payload = "root" };

// Var-arg no-op.
#define NOP(...) ;
// This file is full of undefined UPPER_CASE names. The names without
// leading underscores are meant to defined with '-DNAME=value' when
// calling 'gcc', but defining the names with leading underscores is
// optional.
//
// Define '_DEBUG' to turn on debug messages. These use 'printf',
// which may cause problems for 'reopt', so only define '_DEBUG' when
// building the the frankenstein binary.
#ifdef _DEBUG
# define _PRINTF printf
# define _PERROR perror
#else
# define _PRINTF(...) NOP(__VA_ARGS__)
# define _PERROR(...) NOP(__VA_ARGS__)
#endif

void
print_tree(int depth, struct tree *t) {
    if (t == NULL)
        return;

    _PRINTF("%*sNODE %s\n", depth + 5, " -> ", t->payload);
    PRINT_TREE(depth + 1, t->left);
    PRINT_TREE(depth + 1, t->right);
}

int tree_equal(const struct tree *t1, const struct tree *t2) {
    if (t1 == NULL)
        return t2 == NULL;

    return (t2 != NULL
            && !STRCMP(t1->payload, t2->payload)
            && TREE_EQUAL(t1->left, t2->left)
            && TREE_EQUAL(t1->right, t2->right));
}


static void
really_write_tree(FILE *hdl, const struct tree *t) {
    if (t) {
        FPUTS(t->payload, hdl);
        FPUTC(sep, hdl);
        REALLY_WRITE_TREE(hdl, t->left);
        REALLY_WRITE_TREE(hdl, t->right);
    } else {
        FPUTC(sep, hdl);
    }
}

void
write_tree(const char *filename, const struct tree *t) {
    FILE *hdl = FOPEN(filename, W_STR);
    if (hdl == NULL) {
        _PERROR("write_tree");
        EXIT(__LINE__);
    }

    REALLY_WRITE_TREE(hdl, t);
    FCLOSE(hdl);
}

char *
read_tree_payload(FILE *hdl) {
    size_t sz = 8;
    int i = 0;
    char *buf = MALLOC(sz);
    if (buf == NULL) {
        _PERROR("read_tree_payload (malloc)");
        EXIT(__LINE__);
    }

    for (i = 0; !FEOF(hdl); i++) {
        if (i == sz) {
            sz = sz * 2;
            buf = REALLOC(buf, sz);
            if (buf == NULL) {
                _PERROR("read_tree_payload (realloc)");
                EXIT(__LINE__);
            }
        }

        int r = FGETC(hdl);
        if (r == EOF) {
            _PERROR("read_tree_payload (fgetc)");
            EXIT(__LINE__);
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
    char *payload = READ_TREE_PAYLOAD(hdl);
    if (payload == NULL) {
        return NULL;
    } else {
        struct tree *t = MALLOC(sizeof(struct tree));
        if (t == NULL) {
            _PERROR("really_read_tree");
            EXIT(__LINE__);
        }

        t->payload = payload;
        t->left  = REALLY_READ_TREE(hdl);
        t->right = REALLY_READ_TREE(hdl);
        return t;
    }
}

struct tree*
read_tree(char *filename) {
    FILE *hdl = FOPEN(filename, r_str);
    if (hdl == NULL) {
        _PERROR("read_tree");
        EXIT(__LINE__);
    }
    struct tree *t = REALLY_READ_TREE(hdl);
    FCLOSE(hdl);
    return t;
}

int
main(int argc, char *argv[]) {
    if (argc < 2) {
        _PRINTF("Usage: tree tmpfile\n");
        exit(__LINE__);
    }
    char *filename = argv[1];

    PRINT_TREE(0, &root);
    WRITE_TREE(filename, & ROOT);
    struct tree *read_in = READ_TREE(filename);
    PRINT_TREE(0, read_in);
    _PRINTF("Trees are %sequal\n", TREE_EQUAL(&root, read_in) ? "" : "NOT ");
    return !(TREE_EQUAL(& ROOT, read_in));
}
