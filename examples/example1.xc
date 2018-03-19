#include <search.xh>
#include <refcount.h>
#include <stdio.h>
#include <stdbool.h>

search int foo(int x, int y) {
  refcount_tag z_rt;
  int *z_p = refcount_malloc(sizeof(int), &z_rt);
  choice {
    { add_ref(z_rt); *z_p = x; }
    { add_ref(z_rt); *z_p = y; }
  } finally {
    remove_ref(z_rt);
  }
  int z = *z_p;
  remove_ref(z_rt);
  require z % 2 == 0;
  succeed z;
}

int main() {
  int result1 = -1;
  bool success1 = invoke(search_sequential, &result1, foo(42, 33));
  printf("foo(42, 33) %s: %d\n", success1? "succeeded" : "failed", result1);
  
  int result2 = -1;
  bool success2 = invoke(search_sequential, &result2, foo(47, 36));
  printf("foo(47, 36) %s: %d\n", success2? "succeeded" : "failed", result2);
  
  int result3 = -1;
  bool success3 = invoke(search_sequential, &result3, foo(1, 5));
  printf("foo(1, 5) %s: %d\n", success3? "succeeded" : "failed", result3);
  
  return !(success1 && success2 && !success3 && result1 == 42 && result2 == 36);
}
