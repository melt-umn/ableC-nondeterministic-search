#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <pthread.h>
#include <sched.h>
#include <assert.h>
#include <atomic_wrappers.h>

#include <search_drivers.xh>

struct params {
  unsigned index;
  struct info {
    unsigned num_threads;
    struct thread_info {
      pthread_mutex_t mutex;
      pthread_cond_t cond;
      bool has_task;
      task_t task;
    } *thread_info;
    unsigned *p_num_waiting;
    bool *p_done;
    task_buffer_t buffer0;
  } info;
};

void *steal_worker(void *args) {
  // Copy data from parameter struct
  unsigned index = ((struct params *)args)->index;
  struct info info = ((struct params *)args)->info;
  unsigned num_threads = info.num_threads;
  struct thread_info *thread_info = info.thread_info;
  pthread_mutex_t *p_mutex = &thread_info[index].mutex;
  pthread_cond_t *p_cond = &thread_info[index].cond;
  bool *p_has_task = &thread_info[index].has_task;
  task_t *p_task = &thread_info[index].task;
  pthread_mutex_t *p_next_mutex = &thread_info[(index + 1) % num_threads].mutex;
  pthread_cond_t *p_next_cond = &thread_info[(index + 1) % num_threads].cond;
  bool *p_next_has_task = &thread_info[(index + 1) % num_threads].has_task;
  task_t *p_next_task = &thread_info[(index + 1) % num_threads].task;
  unsigned *p_num_waiting = info.p_num_waiting;
  bool *p_done = info.p_done;
  
  task_buffer_t buffer;
  if (index == 0) {
    // Use the provided buffer
    buffer = info.buffer0;
  } else {
    // Create the buffer
    buffer = create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
  }
  
  task_t task;
  do {
    // Attempt to get a task
    if (!get_task(&buffer, &task)) {
      // This thread's buffer is empty
      // Attempt to get a task provided by the previous thread
      pthread_mutex_lock(p_mutex);
      
      while (!*p_has_task && !*p_done) {
        if (atomic_add_fetch(p_num_waiting, 1) == num_threads) {
          // All other threads are also waiting for a task
          *p_done = true;
        }
        pthread_cond_wait(p_cond, p_mutex);
        atomic_sub_fetch(p_num_waiting, 1);
      }
      if (!*p_done) {
        task = *p_task;
        *p_has_task = false;
      }
      
      pthread_mutex_unlock(p_mutex);
    }
    if (!*p_done) {
      // Evaluate the task
      task(&buffer);
      task.remove_ref();
      
      open_frame(&buffer);
      
      if (buffer.size > 1 && !*p_next_has_task) {
        // Give the next thread a task to steal
        pthread_mutex_lock(p_next_mutex);
        get_task(&buffer, p_next_task);
        *p_next_has_task = true;
        pthread_cond_signal(p_next_cond);
        pthread_mutex_unlock(p_next_mutex);
      }
    }
  } while (!*p_done);

  // Wake up next thread if it is still waiting
  pthread_cond_signal(p_next_cond);

  // Cleanup
  destroy_task_buffer(buffer);
}

void search_parallel_steal(task_t task, closure<() -> void> *notify_success, unsigned num_threads) {
  bool done = false, *p_done = &done;
  *notify_success = lambda () -> (void) { *p_done = true; };

  // Expand all tasks until there are enough for all threads
  task_buffer_t
    buffer1 = create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY),
    buffer2 = create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
  put_task(&buffer1, task);

  while (buffer1.size < num_threads) {
    // Evaluate all tasks in buffer1, dispatching to buffer2
    task_t task;
    while (get_task(&buffer1, &task)) {
      task(&buffer2);
      task.remove_ref();

      // If we succeed while doing this, then exit immediately
      if (done) {
        goto done;
      }
    }
    
    // Swap the buffers
    task_buffer_t tmp = buffer1;
    buffer1 = buffer2;
    buffer2 = tmp;
  }
  
  {
    // Initialize worker thread parameters
    struct thread_info thread_info[num_threads];
    for (unsigned i = 0; i < num_threads; i++) {
      task_t initial_task;
      get_task(&buffer1, &initial_task);
      thread_info[i] = (struct thread_info){
        PTHREAD_MUTEX_INITIALIZER,
        PTHREAD_COND_INITIALIZER,
        true, initial_task
      };
    }
    unsigned num_waiting = 0;
    struct info info = {num_threads, thread_info, &num_waiting, p_done, buffer1};

    // Launch worker threads to evaluate tasks until finished
    struct params params[num_threads];
    pthread_t threads[num_threads];
    for (unsigned i = 0; i < num_threads; i++) {
      params[i] = (struct params){i, info};
      pthread_create(&threads[i], NULL, steal_worker, params + i);
    }
    for (unsigned i = 0; i < num_threads; i++) {
      pthread_join(threads[i], NULL);
    }

    // Cleanup
    for (unsigned i = 0; i < num_threads; i++) {
      pthread_mutex_destroy(&thread_info[i].mutex);
      pthread_cond_destroy(&thread_info[i].cond);
    }
  }

 done:
  // Cleanup
  destroy_task_buffer(buffer2);
  (*notify_success).remove_ref();
}
