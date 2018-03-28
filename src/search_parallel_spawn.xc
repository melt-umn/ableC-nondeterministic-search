#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <pthread.h>

#include <search_drivers.xh>
/*
struct params {
  pthread_mutex_t buffer_mutex;
  task_buffer_t buffer;
  bool *p_success;
};

void *worker(void *args) {
  task_buffer_t global_buffer = ((struct params)args)->buffer;
  bool *p_success = ((struct params)args)->p_success;
  
  struct task_buffer buffer =
    create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
  bool failure = false;
  
  do {
    task(&buffer);
    task.remove_ref();
    if (!get_task(&buffer, &task)) {
      pthread_mutex_lock(buffer_mutex);
      failure = !get_task(global_buffer, &task);
      pthread_mutex_unlock(buffer_mutex);
    }
  } while (!*p_success && !failure);
  
  destroy_task_buffer(buffer);
}

void search_parallel_spawn(task_t task, closure<() -> void> *notify_success, size_t initial_steps, unsigned max_threads) {
  struct task_buffer buffer =
    create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
  bool success = false, *p_success = &success;
  *notify_success = lambda () -> (void) { *p_success = true; };
  (*notify_success).remove_ref();
}
*/
