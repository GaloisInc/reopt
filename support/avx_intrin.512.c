// This file contains test cases to generate AVX intrinsics.
#include<immintrin.h>
#include<stdlib.h>

__m512d add(__m512d a, __m512d b) {
    return _mm512_add_round_pd(a, b, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
