#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <pthread.h>
#include <sched.h>
#include <assert.h>
#include <atomic_wrappers.h>

#include <search_drivers.xh>
#include <search_driver_util.xh>

struct params {
  struct shared_params {
    struct params *thread_params;
    unsigned num_threads;
    size_t depth;
    pthread_mutex_t global_buffer_mutex;
    task_buffer_t global_buffer;
    unsigned num_waiting;
    bool *p_done;
  } *shared_params;
  unsigned index;
  pthread_mutex_t mutex;
  unsigned num_stealing;
  size_t buffers_size;
  task_buffer_t *buffers;
};

void *steal_worker(void *args) {
  // Copy data from parameter struct
  struct shared_params *shared_params = ((struct params *)args)->shared_params;
  struct params *thread_params = shared_params->thread_params;
  unsigned num_threads = shared_params->num_threads;
  unsigned depth = shared_params->depth;
  pthread_mutex_t *p_global_buffer_mutex = &shared_params->global_buffer_mutex;
  task_buffer_t *p_global_buffer = &shared_params->global_buffer;
  unsigned *p_num_waiting = &shared_params->num_waiting;
  bool *p_done = shared_params->p_done;
  unsigned index = ((struct params *)args)->index;
  pthread_mutex_t *p_mutex = &((struct params *)args)->mutex;
  unsigned *p_num_stealing = &((struct params *)args)->num_stealing;
  size_t buffers_size = ((struct params *)args)->buffers_size;
  task_buffer_t *buffers = ((struct params *)args)->buffers;

  bool global_buffer_empty = false;
  size_t buffers_capacity = 0;

  pthread_mutex_lock(p_mutex);
  do {
    bool valid_task = false;
    for (size_t i = 0; i < buffers_size; i++) {
      valid_task |= search_step(buffers + i);
    }
    if (!valid_task) {
      buffers_size = 0;
      ((struct params *)args)->buffers_size = 0;
    } else {
      while (!*p_done && *p_num_stealing > 0) {
        // Allow another thread to steal a task
        pthread_mutex_unlock(p_mutex);
        //printf("Thread %d allowing steal\n", index);
        pthread_mutex_lock(p_mutex);
      }
    }
    if (!valid_task && !*p_done && !global_buffer_empty) {
      // The local buffers are empty, get a task from the global buffer
      task_t task;
      pthread_mutex_lock(p_global_buffer_mutex);
      valid_task = get_task(p_global_buffer, &task) > 0;
      pthread_mutex_unlock(p_global_buffer_mutex);
      if (valid_task) {
        // Expand the task to the specified depth
        task_buffer_t buffer = expand(task, depth);
        
        // Initialize new buffers each containing one element from the expanded buffer
        buffers_size = buffer.size;
        if (buffers_size > buffers_capacity) {
          // Allocate additional buffers
          buffers = realloc(buffers, buffers_size * sizeof(task_buffer_t));
          for (size_t i = buffers_capacity; i < buffers_size; i++) {
            buffers[i] =
              create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY,
                                 DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
          }
          buffers_capacity = buffers_size;
          ((struct params *)args)->buffers_size = buffers_size;
          ((struct params *)args)->buffers = buffers;
        }
        for (size_t i = 0; i < buffers_size; i++) {
          get_task(&buffer, &task);
          put_task(buffers + i, task);
        }
        destroy_task_buffer(buffer);
        ((struct params *)args)->buffers_size = buffers_size;
      } else {
        global_buffer_empty = true;
      }
    }
    while (!valid_task && !*p_done) {
      // Attempt to steal a task from another thread
      unsigned victim = rand() % num_threads;
      unsigned victim_buffers_size = thread_params[victim].buffers_size;
      if (victim_buffers_size > 0) {
        task_t task;
        if (atomic_add_fetch(p_num_waiting, 1) == num_threads) {
          // All other threads are also waiting for a task
          *p_done = true;
        } else {
          pthread_mutex_unlock(p_mutex);
          atomic_add_fetch(&thread_params[victim].num_stealing, 1);
          pthread_mutex_lock(&thread_params[victim].mutex);
          atomic_sub_fetch(p_num_waiting, 1);

          if (!*p_done) {
            size_t victim_buffers_size = thread_params[victim].buffers_size;
            task_buffer_t *victim_buffers = thread_params[victim].buffers;
            size_t max_victim_buffer_size = 0;
            task_buffer_t *p_victim_buffer;
            for (size_t i = 0; i < victim_buffers_size; i++) {
              size_t victim_buffer_size = victim_buffers[i].size;
              if (victim_buffer_size > max_victim_buffer_size) {
                valid_task = true;
                max_victim_buffer_size = victim_buffer_size;
                p_victim_buffer = victim_buffers + i;
                break;
              }
            }
            if (valid_task) {
              get_task(p_victim_buffer, &task);
            }
          }
          
          pthread_mutex_unlock(&thread_params[victim].mutex);
          atomic_sub_fetch(&thread_params[victim].num_stealing, 1);
          pthread_mutex_lock(p_mutex);
        }
        if (valid_task) {
          // Copy the provided task into the first buffer
          printf("Thread %d stealing task\n", index);
          if (buffers_capacity == 0) {
            // Buffers have not yet been allocated, allocate a buffer
            buffers = malloc(sizeof(task_buffer_t));
            *buffers =
              create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY,
                                 DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
            buffers_capacity = 1;
          }
          buffers_size = 1;
          ((struct params *)args)->buffers_size = 1;
          ((struct params *)args)->buffers = buffers;
          put_task(buffers, task);
        }
      }
    }
  } while (!*p_done);
  
  // Cleanup
  pthread_mutex_unlock(p_mutex);
  for (size_t i = 0; i < buffers_capacity; i++) {
    destroy_task_buffer(buffers[i]);
  }
  free(buffers);
}

void search_parallel_steal(task_t task, closure<() -> void> *notify_success,
                           size_t global_depth, size_t thread_depth, unsigned num_threads) {
  bool done = false, *p_done = &done;
  *notify_success = lambda () -> (void) { *p_done = true; };
  
  // Expand the task until there are enough tasks for all threads
  task_buffer_t buffer = expand(task, global_depth);
  
  // Initialize worker thread parameters
  struct params params[num_threads];
  struct shared_params shared_params =
    {params, num_threads, thread_depth, PTHREAD_MUTEX_INITIALIZER, buffer, 0, p_done};
  for (unsigned i = 0; i < num_threads; i++) {
    params[i] = (struct params){&shared_params, i, PTHREAD_MUTEX_INITIALIZER, 0};
  }
  
  // Launch worker threads to evaluate tasks until finished
  pthread_t threads[num_threads];
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_create(&threads[i], NULL, steal_worker, params + i);
  }
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_join(threads[i], NULL);
  }

  // Cleanup
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_mutex_destroy(&params[i].mutex);
  }
  pthread_mutex_destroy(&shared_params.global_buffer_mutex);
  destroy_task_buffer(shared_params.global_buffer);
  (*notify_success).remove_ref();
}
