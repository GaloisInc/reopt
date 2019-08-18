; This file contains various functions in LLVM that we can compile to
; view how LLVM translates those.

; The comments below are the behavior observed with:
; `llc --mtriple=x86_64-pc-linux-elf`

; Should be stored in eax
define i32 @return_i32() {
  ret i32 31337
}

; Should be stored in rax
define i64 @return_i64() {
  ret i64 999999931337
}

; Low 64-bits in rax, high 64-bits in rdx
define i128 @return_i128() {
  ; This is 313373133731337 * 2^64 + 313373133731338
  ret i128 5780713997518331592879028411921930
}

; A buffer to write the return value is allocated and it is place in rdi.
; This uses a least-significant byte order so
; [rdi+24] denotes the least-significant 64-bits,
; [rdi+16] denotes the second least-significant 64-bits,
; [rdi+ 8] denotes the second most-significant 64-bits,
; [rdi]    denotes the most-significant 64-bits,
define i256 @return_i256() {
  ; This is 313373133731337 * 2^192
  ;       + 313373133731338 * 2^128
  ;       + 313373133731339 * 2^64
  ;       + 313373133731340
  ret i256 1967075041568537869393178730535755539094031647537784440104416919434655244
}

; The return value is stored in low 32-bits of xmm0
define float @return_float() {
  ret float 47.0
}

; The return value is stored in low 64-bits of xmm0
define double @return_double() {
  ret double 47.0
}

; Return value stored in xmm0.
define <2 x double> @return_2double() {
  ret <2 x double> <double 47.0, double 57.0 >
}

; On processors that support:
; SSE: [47.0,57.0] is stored in xmm0 and [67.0, 77.0] is stored in xmm1
; AVX256 or 512:   [47.0,57.0, 67.0, 77.0] is stored in ymm0
define <4 x double> @return_4double() {
  ret <4 x double> <double 47.0, double 57.0, double 67.0, double 77.0 >
}

; On processors that support:
; SSE only:
;    xmm0: [17.0,27.0]
;    xmm1: [37.0,47.0]
;    xmm2: [57.0,67.0]
;    xmm3: [77.0,87.0]
; AVX256:
;    ymm0: [17.0,27.0,37.0,47.0]
;    ymm1: [57.0,67.0,77.0, 87.0]
; AVX512:
;    ymm0: [17.0,27.0,37.0,47.0,57.0,67.0,77.0, 87.0]
define <8 x double> @return_8double() {
  ret <8 x double> <double 17.0, double 27.0, double 37.0, double 47.0,
                    double 57.0, double 67.0, double 77.0, double 87.0>
}

; rdi is a pointer to a 32 byte buffer containing the return value

; The first argument is stored in (rsi, rdx, rcx, r8) with rsi
; containing the least-significant 8 bytes.

; The second argument is stored in (r9, [rsp+8], [rsp+16], [rsp+24])
; with r9 containing the least significant 8 bytes.
;
; When returning rax contains the address in rdi.
define i256 @add_i256(i256 %a, i256 %b) {
  %r = add i256 %a, %b
  ret i256 %r
}

; On processors that support:
; SSE only:
;    xmm0 = xmm0 + xmm2
;    xmm1 = xmm1 + xmm3
; AVX256 and AVX512:
;    ymm0 = ymm0 + ymm1
define <4 x double> @add_4double(<4 x double> %a, <4 x double> %b) {
  %r = fadd <4 x double> %a, %b
  ret <4 x double> %r
}

; This adds 64-bit integers rather than doubles, and uses the same
; calling convention as add_4double
define <4 x i64> @add_4i64(<4 x i64> %a, <4 x i64> %b) {
  %r = add <4 x i64> %a, %b
  ret <4 x i64> %r
}

; On processors that support:
; SSE only:
;    xmm0 = xmm0 + xmm4
;    xmm1 = xmm1 + xmm5
;    xmm2 = xmm2 + xmm6
;    xmm3 = xmm3 + xmm7
; AVX256 and AVX512:
;    ymm0 = ymm0 + ymm2
;    ymm1 = ymm1 + ymm3
; AVX256 and AVX512:
;    zmm0 = zmm0 + zmm1
define <8 x double> @add_8double(<8 x double> %a, <8 x double> %b) {
  %r = fadd <8 x double> %a, %b
  ret <8 x double> %r
}