// Wrappers for C11 atomic builtins, needed because ableC doesn't support these yet
unsigned atomic_add_fetch(unsigned *ptr, unsigned val);
unsigned atomic_sub_fetch(unsigned *ptr, unsigned val);
