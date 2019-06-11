// This file contains C using inline assembly for the movsd instruction.
//
// It is designex to both let us test the semantics of movsd, and
// provide examples that use C inline assembly that we can compile to
// LLVM to view LLVM's inline assembly syntax.

#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

////////////////////////////////////////////////////////////////////////
// Common code

static
void ffail(const char* nm, const char* prop_name) {
    fprintf(stderr, "%s: %s failed\n", nm, prop_name);
    exit(1);
}

#define fail() ffail(__FUNCTION__, "bad")

#define assert(nm, b) if (!(b)) { ffail(__FUNCTION__, nm); }

#define arraylen(a) (sizeof(a) / sizeof(a[0]))

////////////////////////////////////////////////////////////////////////
// Primitives

// These functions directly implement a rep_movsb program.

// rep movsb with df=0
__attribute__ ((noinline))
void rep_movsb_df0(uint8_t** dest, uint8_t** src, uint64_t* count) {
  asm ("cld\nrep movsb"
       : "+c" (*count), "+S" (*src), "+D" (*dest));
}

__attribute__ ((noinline))
void rep_movsw_df0(uint16_t** dest, uint16_t** src, uint64_t* count) {
  asm ("cld\nrep movsw"
       : "+c" (*count), "+S" (*src), "+D" (*dest));
}

__attribute__ ((noinline))
void rep_movsd_df0(uint32_t** dest, uint32_t** src, uint64_t* count) {
  asm ("cld\nrep movsd"
       : "+c" (*count), "+S" (*src), "+D" (*dest));
}

__attribute__ ((noinline))
void rep_movsq_df0(uint64_t** dest, uint64_t** src, uint64_t* count) {
  asm ("cld\nrep movsq"
       : "+c" (*count), "+S" (*src), "+D" (*dest));
}

////////////////////////////////////////////////////////////////////////
// Tests

// This tests the rep movsb implementation
static
void test_rep_movsb_df0() {
    uint8_t src[] = {1,2};
    uint8_t dest_init[] = {5,6,7,8};

    uint8_t dest[arraylen(dest_init)];
    memcpy(dest, dest_init, sizeof(dest_init));

    uint8_t* dest_ptr = dest;
    uint8_t* src_ptr  = src;
    uint64_t cnt=arraylen(src);

    rep_movsb_df0(&dest_ptr, &src_ptr, &cnt);

    // Number of bytes not copied.
    uint64_t left = sizeof(dest) - sizeof(src);

    assert("check_dest", dest_ptr == dest + arraylen(src));
    assert("check_src",  src_ptr  == src  + arraylen(src));
    assert("check_cnt",  cnt == 0);
    assert("mem_pre",  memcmp(src, dest, sizeof(src)) == 0);
    assert("mem_post", memcmp(dest+arraylen(src), dest_init+arraylen(src), left) == 0);
}

// This tests the rep movsw implementation
void test_rep_movsw_df0() {
    uint16_t src[] = {1,2};
    uint16_t dest_init[] = {5,6,7,8};

    uint16_t dest[arraylen(dest_init)];
    memcpy(dest, dest_init, sizeof(dest_init));

    uint16_t* dest_ptr = dest;
    uint16_t* src_ptr  = src;
    uint64_t cnt=arraylen(src);

    rep_movsw_df0(&dest_ptr, &src_ptr, &cnt);

    // Number of bytes not copied.
    uint64_t left = sizeof(dest) - sizeof(src);

    assert("check_dest", dest_ptr == dest + arraylen(src));
    assert("check_src",  src_ptr  == src  + arraylen(src));
    assert("check_cnt",  cnt == 0);
    assert("mem_pre",  memcmp(src, dest, sizeof(src)) == 0);
    assert("mem_post", memcmp(dest+arraylen(src), dest_init+arraylen(src), left) == 0);
}

// This tests the rep movsd implementation
void test_rep_movsd_df0() {
    uint32_t src[] = {1,2};
    uint32_t dest_init[] = {5,6,7,8};

    uint32_t dest[arraylen(dest_init)];
    memcpy(dest, dest_init, sizeof(dest_init));

    uint32_t* dest_ptr = dest;
    uint32_t* src_ptr  = src;
    uint64_t cnt=arraylen(src);

    rep_movsd_df0(&dest_ptr, &src_ptr, &cnt);

    // Number of bytes not copied.
    uint64_t left = sizeof(dest) - sizeof(src);

    assert("check_dest", dest_ptr == dest + arraylen(src));
    assert("check_src",  src_ptr  == src  + arraylen(src));
    assert("check_cnt",  cnt == 0);
    assert("mem_pre",  memcmp(src, dest, sizeof(src)) == 0);
    assert("mem_post", memcmp(dest+arraylen(src), dest_init+arraylen(src), left) == 0);
}

// This tests the rep movsq implementation
void test_rep_movsq_df0() {
    uint64_t src[] = {1,2};
    uint64_t dest_init[] = {5,6,7,8};

    uint64_t dest[arraylen(dest_init)];
    memcpy(dest, dest_init, sizeof(dest_init));

    uint64_t* dest_ptr = dest;
    uint64_t* src_ptr  = src;
    uint64_t cnt=arraylen(src);

    rep_movsq_df0(&dest_ptr, &src_ptr, &cnt);

    // Number of bytes not copied.
    uint64_t left = sizeof(dest) - sizeof(src);

    assert("check_dest", dest_ptr == dest + arraylen(src));
    assert("check_src",  src_ptr  == src  + arraylen(src));
    assert("check_cnt",  cnt == 0);
    assert("mem_pre",  memcmp(src, dest, sizeof(src)) == 0);
    assert("mem_post", memcmp(dest+arraylen(src), dest_init+arraylen(src), left) == 0);
}

////////////////////////////////////////////////////////////////////////
// main

int main(int argc, char** argv) {
    test_rep_movsb_df0();
    test_rep_movsw_df0();
    test_rep_movsd_df0();
    test_rep_movsw_df0();
    return 0;
}
