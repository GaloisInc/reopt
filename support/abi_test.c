#include <emmintrin.h>
#include <immintrin.h>

struct double_pair {
    double x;
    double y;
};

// s.x is passed in xmm0, and s.y is passed in xmm1.
// In the return value r.x is passed in xmm0 and r.y is passed in xmm1.
struct double_pair add_doublepair(struct double_pair s) {
    struct double_pair r = { .x = s.x * s.x, .y = s.x * s.y };
    return r;
}

// This will get returned via xmm0
__m128i set_128_i32() {
    return _mm_set_epi32(3,2,1,0);
}

struct mm128_pair {
    __m128 d0;
    __m128 d1;
};

// The return value for this is a buffer passed in by rdi.
struct mm128_pair set_mm128_pair() {
    struct mm128_pair s =
	{ .d0 = _mm_set_ps(3.0, 2.0, 1.0, 0.0),
	  .d1 = _mm_set_ps(3.0, 2.0, 1.0, 0.0)
	};
    return s;
}

// Arguments passed in xmm0, xmm1 and return value stored in xmm0.
__m128 add_ps(__m128 a, __m128 b) {
    b = _mm_add_ps(a,b);
    b = _mm_add_ps(b,b);
    return b;
}

// This returns in ymm0.
__m256 set_256_pd() {
    return _mm256_set_pd(3.0, 2.0, 1.0, 0.0);
}

struct mm256_pair {
    __m256 d0;
    __m256 d1;
};

// This returns in ymm0.
struct mm256_pair set_mm256_pair() {
    struct mm256_pair s =
	{ .d0 = _mm256_set_pd(3.0, 2.0, 1.0, 0.0),
	  .d1 = _mm256_set_pd(3.0, 2.0, 1.0, 0.0)
	};
    return s;
}

// This takes the two arguments in ymm0 and ymm1 and returns in ymm0.
__m256d add_ymm2(__m256d x1, __m256d x0) {
    return _mm256_add_pd(x1,x0);
}

// This takes the 8 arguments in ymm0-7, and a final argument passed on the stack.
// The return value is in ymm0
__m256d add_ymm9(__m256d x0, __m256d x1,
		 __m256d x2, __m256d x3,
		 __m256d x4, __m256d x5,
		 __m256d x6, __m256d x7,
		 __m256d x8) {
    __m256d r;
    r = x0;
    r = _mm256_add_pd(_mm256_add_pd(r,r), x1);
    r = _mm256_add_pd(_mm256_add_pd(r,r), x2);
    r = _mm256_add_pd(_mm256_add_pd(r,r), x3);
    r = _mm256_add_pd(_mm256_add_pd(r,r), x4);
    r = _mm256_add_pd(_mm256_add_pd(r,r), x5);
    r = _mm256_add_pd(_mm256_add_pd(r,r), x6);
    r = _mm256_add_pd(_mm256_add_pd(r,r), x7);
    r = _mm256_add_pd(_mm256_add_pd(r,r), x8);
    return r;
}


struct uint64_pair {
    uint64_t x;
    uint64_t y;
};

// s.x is passed in rdi, and s.y is passed in rsi.
// In the return value rax holds r.x, and rdx holds r.y
struct uint64_pair add_struct(struct uint64_pair s) {
    struct uint64_pair r = { .x = s.x * s.x, .y = s.x * s.y };
    return r;
}

// This contains a double and uint64_t
struct mixed_pair {
    double   d;
    uint64_t x;
};

// s.x is stored in rdi and s.d is stored in xmm0.
// In the return value, r.x is in rax and r.d is in xmm0.
struct mixed_pair add_mixedpair(struct mixed_pair s) {
    struct mixed_pair r =
	{ .x = s.x * s.x
	, .d = s.d * s.d
	};
    return r;
}

// This contains 4 fields, and will be passed in memory.
struct return_quad {
    double d0;
    double d1;
    uint64_t x;
    uint64_t y;
};

// rdi contains the buffer for storing the return value.
struct return_quad add_rq(struct uint64_pair s) {
    struct return_quad r =
	{ .x  = s.x * s.x
	, .y  = s.x * s.y
	, .d0 = 5
	, .d1 = 7
	};
    return r;
}

typedef double __m512d __attribute__((__vector_size__(64)));

#define DEFAULT_FN_ATTRS512 __attribute__((__always_inline__, __nodebug__, __min_vector_width__(512)))

static __inline__ __m512d DEFAULT_FN_ATTRS512
mm512_set_pd (double __A, double __B, double __C, double __D,
        double __E, double __F, double __G, double __H)
{
  return __extension__ (__m512d)
  { __H, __G, __F, __E, __D, __C, __B, __A };
}

// This returns in zmm0.
__m512d set_512_pd() {
    return mm512_set_pd (7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0);

}


__m512d copy(__m512d a) {
    return a;
}
