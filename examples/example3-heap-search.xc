#include <search.xh>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

search float heap_contains_help(float lower, float upper, float *data, size_t size, unsigned index);

search float heap_contains_help(float lower, float upper, float *data, size_t size, unsigned index) {
  require index < size;
  float val = data[index];
  require val <= upper;
  choice {
    {
      require lower <= val;
      succeed val;
    }
    choose succeed heap_contains_help(lower, upper, data, size, index * 2 + 1);
    choose succeed heap_contains_help(lower, upper, data, size, index * 2 + 2);
  }
}

search float heap_contains(float lower, float upper, float *data, size_t size) {
  choose succeed heap_contains_help(lower, upper, data, size, 0);
}

int main() {
  float data[] = {-0.11f, 0.23f, 0.21f, 0.94f, 1.23f, 0.22f, 3.14f, 5, 100, 10};
  size_t size = sizeof(data) / sizeof(float);
  
  float result1;
  bool success1 = invoke(search_sequential, &result1, heap_contains(0, 0.5, data, size));
  printf("heap_contains(0, 0.5) %s: %f\n", success1? "succeeded" : "failed", result1);
  
  float result2;
  bool success2 = invoke(search_sequential, &result2, heap_contains(-1, 1, data, size));
  printf("heap_contains(-1, 1) %s: %f\n", success2? "succeeded" : "failed", result2);
  
  float result3;
  bool success3 = invoke(search_sequential, &result3, heap_contains(7, 8, data, size));
  printf("heap_contains(7, 8) %s: %f\n", success3? "succeeded" : "failed", result3);
  
  float result4;
  bool success4 = invoke(search_sequential, &result4, heap_contains(42, 100, data, size));
  printf("heap_contains(42, 100) %s: %f\n", success4? "succeeded" : "failed", result4);
  
  return !(success1 && result1 == 0.23f && success2 && result2 == -0.11f && !success3 && success4 && result4 == 100);
}
