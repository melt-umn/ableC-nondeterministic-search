#include <pthread.h>
#include <refcount.h>

typedef struct pick_status_s *pick_status_t;
struct pick_status_s {
  pthread_mutex_t mutex;
  _Bool cancelled;
};

// Not static inline to avoid need for -lpthread
refcount_tag_t pick_status_init(pick_status_t *p_status);
_Bool try_pick(pick_status_t status);
