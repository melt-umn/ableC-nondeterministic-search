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
  struct common_params {
    struct params *thread_params;
    unsigned num_threads;
    size_t depth;
    pthread_mutex_t global_buffer_mutex;
    task_buffer_t global_buffer;
    unsigned num_waiting;
    bool *p_done;
  } *common_params;
  unsigned index;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  bool has_task;
  task_t task;
};

void *share_worker(void *args) {
  // Copy data from parameter struct
  struct common_params *common_params = ((struct params *)args)->common_params;
  struct params *thread_params = common_params->thread_params;
  unsigned num_threads = common_params->num_threads;
  unsigned depth = common_params->depth;
  pthread_mutex_t *p_global_buffer_mutex = &common_params->global_buffer_mutex;
  task_buffer_t *p_global_buffer = &common_params->global_buffer;
  unsigned *p_num_waiting = &common_params->num_waiting;
  bool *p_done = common_params->p_done;
  unsigned index = ((struct params *)args)->index;
  pthread_mutex_t *p_mutex = &((struct params *)args)->mutex;
  pthread_cond_t *p_cond = &((struct params *)args)->cond;
  bool *p_has_task = &((struct params *)args)->has_task;
  task_t *p_task = &((struct params *)args)->task;

  bool global_buffer_empty = false;
  
  size_t buffers_size = 0, buffers_capacity = 0;
  task_buffer_t *buffers = NULL;
  
  do {
    size_t max_buffer_index = 0;
    size_t max_buffer_size = 0;
    for (size_t i = 0; i < buffers_size; i++) {
      if (search_step(buffers + i) && buffers[i].size > max_buffer_size) {
        max_buffer_index = i;
        max_buffer_size = buffers[i].size;
      }
    }
    if (!*p_done) {
      if (max_buffer_size > 0) {
        // Check if any other threads are in need of a task
        for (unsigned i = 0;
             i < num_threads && max_buffer_size > 1 && *p_num_waiting > 0;
             i++, max_buffer_size--) {
          unsigned other_index = (index + i) % num_threads;
          if (!thread_params[other_index].has_task) {
            // Give the other thread a task from the largest buffer
            pthread_mutex_lock(&thread_params[other_index].mutex);
            get_back_task(buffers + max_buffer_index, &thread_params[other_index].task);
            thread_params[other_index].has_task = true;
            pthread_cond_signal(&thread_params[other_index].cond);
            pthread_mutex_unlock(&thread_params[other_index].mutex);
          }
        }
      } else {
        bool valid_task = false;
        if (!global_buffer_empty) {
          // The local buffers are empty, get a task from the global buffer
          task_t task;
          pthread_mutex_lock(p_global_buffer_mutex);
          valid_task = get_task(p_global_buffer, &task) > 0;
          pthread_mutex_unlock(p_global_buffer_mutex);
          if (valid_task) {
            // Expand the task to the specified depth
            task_buffer_t buffer = expand(task, depth, p_done);
        
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
            }
            for (size_t i = 0; i < buffers_size; i++) {
              get_task(&buffer, &task);
              put_task(buffers + i, task);
            }
            destroy_task_buffer(buffer);
          } else {
            global_buffer_empty = true;
          }
        }
        if (!valid_task) {
          // The global buffer is empty
          // Attempt to get a task provided by another thread
          pthread_mutex_lock(p_mutex);
          
          while (!(valid_task = *p_has_task) && !*p_done) {
            if (atomic_add_fetch(p_num_waiting, 1) == num_threads) {
              // All other threads are also waiting for a task
              *p_done = true;
            } else {
              pthread_cond_wait(p_cond, p_mutex);
              atomic_sub_fetch(p_num_waiting, 1);
            }
          }
          if (valid_task) {
            // Copy the provided task into the first buffer
            //printf("Thread %d got task\n", index);
            if (buffers_capacity == 0) {
              // Buffers have not yet been allocated, allocate a buffer
              buffers = malloc(sizeof(task_buffer_t));
              *buffers =
                create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY,
                                   DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
              buffers_capacity = 1;
            }
            put_task(buffers, *p_task);
            *p_has_task = false;
            buffers_size = 1;
          }
      
          pthread_mutex_unlock(p_mutex);
        }
      }
    }
  } while (!*p_done);
  
  // Wake up next thread, so none are left waiting on exit
  pthread_cond_signal(&thread_params[(index + 1) % num_threads].cond);
  
  // Cleanup
  for (size_t i = 0; i < buffers_capacity; i++) {
    destroy_task_buffer(buffers[i]);
  }
  free(buffers);
}

void search_parallel_share(task_t task, closure<() -> void> *notify_success,
                           size_t global_depth, size_t thread_depth, unsigned num_threads) {
  bool done = false, *p_done = &done;
  *notify_success = lambda () -> (void) { *p_done = true; };
  
  // Expand the task until there are enough tasks for all threads
  task_buffer_t buffer = expand(task, global_depth, p_done);
  
  // Initialize worker thread parameters
  struct params params[num_threads];
  struct common_params common_params =
    {params, num_threads, thread_depth, PTHREAD_MUTEX_INITIALIZER, buffer, 0, p_done};
  for (unsigned i = 0; i < num_threads; i++) {
    params[i] =
      (struct params){&common_params, i, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, false};
  }
  
  // Launch worker threads to evaluate tasks until finished
  pthread_t threads[num_threads];
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_create(&threads[i], NULL, share_worker, params + i);
  }
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_join(threads[i], NULL);
  }

  // Cleanup
  for (unsigned i = 0; i < num_threads; i++) {
    pthread_mutex_destroy(&params[i].mutex);
    pthread_cond_destroy(&params[i].cond);
    if (params[i].has_task) {
      params[i].task.remove_ref();
    }
  }
  
  pthread_mutex_destroy(&common_params.global_buffer_mutex);
  destroy_task_buffer(common_params.global_buffer);
  (*notify_success).remove_ref();
}
