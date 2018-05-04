#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L

#include <search.xh>
#include <stdio.h>
#include <stdbool.h>
#include <time.h>

search unsigned long long foo(int a, unsigned long long b) {
  require a >= 0;
  choice {
    choice for (int i = 1; i < 5; i++) {
      nanosleep((const struct timespec[]){{0, rand() % 10000000}}, NULL);
      choose succeed foo(a - i, b * 10 + i);
    }
    {
      fprintf(stdout, "Trying %d %llu\n", a, b);
      nanosleep((const struct timespec[]){{0, rand() % 10000000}}, NULL);
      require a == 0;
      succeed b;
    }
  }
}

int main() {
  unsigned long long result1 = -1;
  bool success1 = invoke(search_parallel_spawn(3, 6, 8), &result1, foo(20, 0));
  printf("foo(20, 0) %s: %llu\n", success1? "succeeded" : "failed", result1);
  
  unsigned long long result2 = -1;
  bool success2 = invoke(search_parallel_share(3, 6, 8), &result2, foo(20, 0));
  printf("foo(20, 0) %s: %llu\n", success2? "succeeded" : "failed", result2);

  return !(success1 && success2);
}
