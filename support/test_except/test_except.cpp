#include <iostream>

int main () {
  try {
    throw 20;
  } catch (int e) {
    std::cout << "An exception occurred. Exception Nr. " << e << '\n';
  }
  return 0;
}
