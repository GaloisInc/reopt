#include <emmintrin.h>

// This will get returned via xmm0
__m128i f() {
   return _mm_set_epi32(1,2,3,4);
}

// Arguments passed in xmm0, xmm1 and return value stored in xmm0.
__m128 add_ps(__m128 a, __m128 b) {
    b = _mm_add_ps(a,b);
    b = _mm_add_ps(b,b);
    return b;
}
