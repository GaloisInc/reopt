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

; [47.0,57.0] is stored in xmm0 and [67.0, 77.0] is stored in xmm1
define <4 x double> @return_4double() {
  ret <4 x double> <double 47.0, double 57.0, double 67.0, double 77.0 >
}
