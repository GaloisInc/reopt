double fpadd(double x) {
  long y = x;
  return y + 1;
}

int main() {
  double x = fpadd(42.0);
  return 0;
}
