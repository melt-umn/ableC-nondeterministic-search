#include <pthread.h>
#include <refcount.h>

#ifndef _PICK_H
#define _PICK_H

typedef struct pick_status_s *pick_status_t;
struct pick_status_s {
  pthread_mutex_t mutex;
  _Bool cancelled;
};

refcount_tag_t pick_status_init(pick_status_t *p_status);
_Bool try_pick(pick_status_t status);

#endif
