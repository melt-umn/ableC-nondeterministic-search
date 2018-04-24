#include <atomic_wrappers.h>

unsigned atomic_add_fetch(unsigned *ptr, unsigned val) {
  return __atomic_add_fetch(ptr, val, __ATOMIC_RELAXED);
}

unsigned atomic_sub_fetch(unsigned *ptr, unsigned val) {
  return __atomic_sub_fetch(ptr, val, __ATOMIC_RELAXED);
}
