#include <search.xh>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

search float heap_contains_help(float lower, float upper, float *data, size_t size, unsigned index) {
  require index < size;
  float val = data[index];
  require val <= upper;
  choice {
    {
      require lower <= val;
      succeed val;
    }
    spawn choose succeed heap_contains_help(lower, upper, data, size, index * 2 + 1);
    spawn choose succeed heap_contains_help(lower, upper, data, size, index * 2 + 2);
  }
}

search float heap_contains(float lower, float upper, float *data, size_t size) {
  choose succeed heap_contains_help(lower, upper, data, size, 0);
}

int main() {
  float data[] = {-0.11f, 0.23f, 0.21f, 0.94f, 1.23f, 0.22f, 3.14f, 5, 100, 10};
  size_t size = sizeof(data) / sizeof(float);

  printf("Sequential:\n");
  
  float result1 = -1;
  bool success1 = invoke(search_sequential(2), &result1, heap_contains(0, 0.5, data, size));
  printf("heap_contains(0, 0.5) %s: %f\n", success1? "succeeded" : "failed", result1);
  
  float result2 = -1;
  bool success2 = invoke(search_sequential(2), &result2, heap_contains(-1, 1, data, size));
  printf("heap_contains(-1, 1) %s: %f\n", success2? "succeeded" : "failed", result2);
  
  float result3 = -1;
  bool success3 = invoke(search_sequential(2), &result3, heap_contains(7, 8, data, size));
  printf("heap_contains(7, 8) %s: %f\n", success3? "succeeded" : "failed", result3);
  
  float result4 = -1;
  bool success4 = invoke(search_sequential(2), &result4, heap_contains(42, 100, data, size));
  printf("heap_contains(42, 100) %s: %f\n", success4? "succeeded" : "failed", result4);
  
  printf("Parallel:\n");
  
  float result5 = -1;
  bool success5 = invoke(search_parallel_spawn(2, 1, 4), &result5, heap_contains(0, 0.5, data, size));
  printf("heap_contains(0, 0.5) %s: %f\n", success5? "succeeded" : "failed", result5);
  
  float result6 = -1;
  bool success6 = invoke(search_parallel_spawn(2, 1, 4), &result6, heap_contains(-1, 1, data, size));
  printf("heap_contains(-1, 1) %s: %f\n", success6? "succeeded" : "failed", result6);
  
  float result7 = -1;
  bool success7 = invoke(search_parallel_spawn(2, 1, 4), &result7, heap_contains(7, 8, data, size));
  printf("heap_contains(7, 8) %s: %f\n", success7? "succeeded" : "failed", result7);
  
  float result8 = -1;
  bool success8 = invoke(search_parallel_spawn(2, 1, 4), &result8, heap_contains(42, 100, data, size));
  printf("heap_contains(42, 100) %s: %f\n", success8? "succeeded" : "failed", result8);
  
  return !(success1 && result1 == 0.23f && success2 && result2 == -0.11f && !success3 && success4 && result4 == 100 && success5 && result5 >= 0 && result5 < 0.5 && success6 && result6 >= -1 && result6 < 1 && !success7 && success8 && result8 == 100);
}
