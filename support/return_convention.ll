c; This file contains various functions to check the return convention on x86.

define i32 @return_i32() {
  ret i32 31337
}

define i64 @return_i64() {
  ret i64 999999931337
}

define i128 @return_i128() {
  ret i128 31337
}

define float @return_float() {
  ret float 47.0
}

define double @return_double() {
  ret double 47.0
}

define <2 x double> @return_2double() {
  ret <2 x double> <double 47.0, double 57.0 >
}

define <4 x double> @return_4double() {
  ret <4 x double> <double 47.0, double 57.0, double 67.0, double 77.0 >
}
