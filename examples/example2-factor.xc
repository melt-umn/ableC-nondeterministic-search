#include <search.xh>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

// Factorization using Fermat’s method
search int factor(int n) {
  choose int a = range((int)ceil(sqrt(n)), n / 2);
  float b = sqrt(a * a - n);
  require b == floor(b);
  succeed a - (int)floor(b);
}

int main() {
  int result1;
  bool success1 = invoke(search_sequential, &result1, factor(5959));
  printf("factor(5959) %s: %d\n", success1? "succeeded" : "failed", result1);
  
  return !(success1 && result1 == 59);
}
