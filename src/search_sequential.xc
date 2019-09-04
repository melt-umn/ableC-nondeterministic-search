#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include <search_drivers.xh>
#include <search_driver_util.xh>

void search_sequential(task_t task, closure<() -> void> *notify_success, size_t depth) {
  bool success = false, *p_success = &success;
  *notify_success = lambda () -> (void) { *p_success = true; };

  // Initially expand the task into a new buffer
  task_buffer_t buffer = expand(task, depth, p_success);
  size_t buffers_size = buffer.size;
  
  if (!success) {
    // Initialize new buffers each containing one element from the expanded buffer
    task_buffer_t buffers[buffers_size];
    for (size_t i = 0; i < buffers_size; i++) {
      buffers[i] =
        create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
      task_t task;
      get_task(&buffer, &task);
      put_task(buffers + i, task);
    }
    
    bool failure;
    do {
      // Perform a step on each buffer
      failure = true;
      for (size_t i = 0; i < buffers_size && !success; i++) {
        failure &= search_step(buffers + i) == 0;
      }
    } while (!success && !failure);

    // Cleanup
    for (size_t i = 0; i < buffers_size; i++) {
      destroy_task_buffer(buffers[i]);
    }
  }

  // Cleanup
  destroy_task_buffer(buffer);
  (*notify_success).remove_ref();
}
