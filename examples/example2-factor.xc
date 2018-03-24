#include <search.xh>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

// Factorization using Fermat’s method
search int factor(int n) {
  if (n % 2 == 0) {
    succeed 2;
  } else {
    choose int a = range((int)ceil(sqrt(n)), n);
    float b = sqrt(a * a - n);
    require b == floor(b);
    succeed a - (int)floor(b);
  }
}

int main() {
  int result1 = -1;
  bool success1 = invoke(search_sequential, &result1, factor(5959));
  printf("factor(5959) %s: %d\n", success1? "succeeded" : "failed", result1);
  
  int result2 = -1;
  bool success2 = invoke(search_sequential, &result2, factor(30));
  printf("factor(30) %s: %d\n", success2? "succeeded" : "failed", result2);
  
  int result3 = -1;
  bool success3 = invoke(search_sequential, &result3, factor(17));
  printf("factor(17) %s: %d\n", success3? "succeeded" : "failed", result3);
  
  int result4 = -1;
  bool success4 = invoke(search_sequential, &result4, factor(105));
  printf("factor(105) %s: %d\n", success4? "succeeded" : "failed", result4);
  
  return !(success1 && result1 == 59 && success2 && result2 == 2 && success3 && result3 == 1 && success4 && result4 == 7);
}
