#include <search.xh>
#include <stdio.h>
#include <stdbool.h>

search int foo(int a, unsigned b) {
  choice {
    choice for (int i = 0; i < 4; i++) {
      choose succeed foo(a - i, b * 10 + i);
    }
    { require a == 0; succeed b; }
  }
}

int main() {
  int result1 = -1;
  bool success1 = invoke(search_parallel_spawn(9, 8), &result1, foo(20, 0));
  printf("foo(20, 0) %s: %d\n", success1? "succeeded" : "failed", result1);

  return !(success1);
}
