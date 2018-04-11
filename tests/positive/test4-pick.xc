#include <search.xh>
#include <stdio.h>
#include <stdbool.h>

search int foo(int a, int b) {
  choice {
    { printf("foo fail\n"); fail; }
    { printf("foo %d\n", a); succeed a; }
    { printf("foo %d\n", b); succeed b; }
  }
}

search int bar(int a, int b) {
  choice {
    { printf("bar fail\n"); fail; }
    { printf("bar %d\n", a); succeed a; }
    { printf("bar %d\n", b); succeed b; }
  }
}

search int baz(int a, int b, int c, int d) {
  pick int x = foo(a, b);
  printf("x = %d\n", x);
  pick int y = bar(c, d);
  printf("y = %d\n", y);
  require x == y;
  succeed x;
}

int main() {
  int result1 = -1;
  bool success1 = invoke(search_sequential, &result1, baz(1, 2, 2, 3));
  printf("baz(1, 2, 2, 3) %s: %d\n", success1? "succeeded" : "failed", result1);
  
  int result2 = -1;
  bool success2 = invoke(search_sequential, &result2, baz(1, 2, 3, 2));
  printf("baz(1, 2, 3, 2) %s: %d\n", success2? "succeeded" : "failed", result2);
  
  int result3 = -1;
  bool success3 = invoke(search_sequential, &result3, baz(2, 1, 2, 3));
  printf("baz(2, 1, 2, 3) %s: %d\n", success3? "succeeded" : "failed", result3);
  
  int result4 = -1;
  bool success4 = invoke(search_sequential, &result4, baz(2, 1, 3, 2));
  printf("baz(2, 1, 3, 2) %s: %d\n", success4? "succeeded" : "failed", result4);

  return !(!success1 && !success2 && success3 && result3 == 2 && !success4);
}
