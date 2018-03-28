#include <stdbool.h>

#include <pick.h>

void finalize_pick_status(void *p) {
  pthread_mutex_destroy(&((pick_status_t)p)->mutex);
}

refcount_tag_t pick_status_init(pick_status_t *p_status) {
  refcount_tag_t rt;
  pick_status_t status =
    refcount_final_malloc(sizeof(struct pick_status_s), &rt, 0, NULL, finalize_pick_status);
  pthread_mutex_init(&status->mutex, NULL);
  status->cancelled = false;
  *p_status = status;
  return rt;
}

bool try_pick(pick_status_t status) {
  pthread_mutex_lock(&status->mutex);
  bool cancelled = status->cancelled;
  status->cancelled = true;
  pthread_mutex_unlock(&status->mutex);
  return !cancelled;
}
