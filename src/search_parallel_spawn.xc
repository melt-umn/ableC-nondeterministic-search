#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <pthread.h>

#include <search_drivers.xh>

struct params {
  pthread_mutex_t buffer_mutex;
  task_buffer_t *buffer;
  bool *p_success;
};

void *worker(void *args) {
  // Copy data from parameter struct
  pthread_mutex_t *global_buffer_mutex = &((struct params *)args)->buffer_mutex;
  task_buffer_t *global_buffer = ((struct params *)args)->buffer;
  bool *p_success = ((struct params *)args)->p_success;
  
  task_buffer_t buffer =
    create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
  bool failure = false;

  // Evaluate tasks until success or failure
  task_t task;
  do {
    if (!get_task(&buffer, &task)) {
      // If the local buffer is empty, get a task from the global buffer
      pthread_mutex_lock(global_buffer_mutex);
      failure = !get_task(global_buffer, &task);
      pthread_mutex_unlock(global_buffer_mutex);
    }
    if (!failure) {
      // Evaluate a task if one exists
      task(&buffer);
      task.remove_ref();
      
      open_frame(&buffer);
    }
  } while (!*p_success && !failure);

  // Delete the local buffer
  destroy_task_buffer(buffer);
}

void search_parallel_spawn(task_t task, closure<() -> void> *notify_success, size_t initial_depth, unsigned num_threads) {
  bool success = false, *p_success = &success;
  *notify_success = lambda () -> (void) { *p_success = true; };

  // Expand all tasks initial_depth number of times using 2 buffers
  task_buffer_t
    buffer1 = create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, 0),
    buffer2 = create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, 0);
  put_task(&buffer1, task);

  for (int i = 0; i < initial_depth; i++) {
    // Evaluate all tasks in buffer1, dispatching to buffer2
    task_t task;
    while (get_task(&buffer1, &task)) {
      task(&buffer2);
      task.remove_ref();

      // If we succeed while doing this, then exit immediately
      if (success) {
        goto done;
      }
    }

    // Swap the buffers
    task_buffer_t tmp = buffer1;
    buffer1 = buffer2;
    buffer2 = tmp;
  }

  {
    // Launch worker threads to evaluate tasks until finished
    struct params params = {PTHREAD_MUTEX_INITIALIZER, &buffer1, p_success};
    pthread_t threads[num_threads];
    for (int i = 0; i < num_threads; i++) {
      pthread_create(&threads[i], NULL, worker, &params);
    }
    for (int i = 0; i < num_threads; i++) {
      pthread_join(threads[i], NULL);
    }
    pthread_mutex_destroy(&params.buffer_mutex);
  }

 done:
  // Cleanup
  destroy_task_buffer(buffer1);
  destroy_task_buffer(buffer2);
  (*notify_success).remove_ref();
}

