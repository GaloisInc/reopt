// This file contains test cases to generate AVX 512 intrinsics.
//
// It must be compiled with -mavx512f
#include<immintrin.h>
#include<stdlib.h>

__m512d add(__m512d a, __m512d b) {
    return _mm512_add_round_pd(a, b, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

// This returns in zmm0.
__m512d set_512_pd() {
    return _mm512_set_pd (7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0);

}
