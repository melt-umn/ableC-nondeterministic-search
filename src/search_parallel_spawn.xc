#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <pthread.h>

#include <search_drivers.xh>
#include <search_driver_util.xh>

struct params {
  size_t depth;
  pthread_mutex_t global_buffer_mutex;
  task_buffer_t global_buffer;
  bool *p_success;
};

void *spawn_worker(void *args) {
  // Copy data from parameter struct
  size_t depth = ((struct params *)args)->depth;
  pthread_mutex_t *p_global_buffer_mutex = &((struct params *)args)->global_buffer_mutex;
  task_buffer_t *p_global_buffer = &((struct params *)args)->global_buffer;
  bool *p_success = ((struct params *)args)->p_success;
  
  size_t buffers_size = 0, buffers_capacity = 0;
  task_buffer_t *buffers = NULL;
  bool failure;
  
  // Evaluate tasks until success or failure
  do {
    failure = true;
    for (size_t i = 0; i < buffers_size; i++) {
      failure &= search_step(buffers + i) == 0;
    }
    if (failure && !*p_success) {
      // If the local buffers are empty, get a task from the global buffer
      task_t task;
      pthread_mutex_lock(p_global_buffer_mutex);
      failure = !get_task(p_global_buffer, &task);
      pthread_mutex_unlock(p_global_buffer_mutex);
      if (!failure) {
        // Expand the task to the specified depth
        task_buffer_t buffer = expand(task, depth, p_success);
        
        // Initialize new buffers each containing one element from the expanded buffer
        buffers_size = buffer.size;
        if (buffers_size > buffers_capacity) {
          // Allocate additional buffers
          buffers = realloc(buffers, buffers_size * sizeof(task_buffer_t));
          for (size_t i = buffers_capacity; i < buffers_size; i++) {
            buffers[i] =
              create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
          }
          buffers_capacity = buffers_size;
        }
        for (size_t i = 0; i < buffers_size; i++) {
          get_task(&buffer, &task);
          put_task(buffers + i, task);
        }
        destroy_task_buffer(buffer);
      }
    }
  } while (!*p_success && !failure);
  
  // Cleanup
  for (size_t i = 0; i < buffers_capacity; i++) {
    destroy_task_buffer(buffers[i]);
  }
  free(buffers);
}

void search_parallel_spawn(task_t task, closure<() -> void> *notify_success,
                           size_t global_depth, size_t thread_depth, unsigned num_threads) {
  bool success = false, *p_success = &success;
  *notify_success = lambda () -> (void) { *p_success = true; };

  // Expand the task to the specified depth
  task_buffer_t buffer = expand(task, global_depth, p_success);
  
  // Launch worker threads to evaluate tasks until finished
  struct params params = {thread_depth, PTHREAD_MUTEX_INITIALIZER, buffer, p_success};
  pthread_t threads[num_threads];
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_create(&threads[i], NULL, spawn_worker, &params);
  }
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_join(threads[i], NULL);
  }
  
  // Cleanup
  pthread_mutex_destroy(&params.global_buffer_mutex);
  destroy_task_buffer(params.global_buffer);
  (*notify_success).remove_ref();
}
