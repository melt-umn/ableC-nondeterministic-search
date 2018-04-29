#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include <search_drivers.xh>
#include <search_driver_util.xh>

void search_sequential_dfs(task_t task, closure<() -> void> *notify_success) {
  bool success = false, *p_success = &success;
  *notify_success = lambda () -> (void) { *p_success = true; };

  // Initialize the buffer
  task_buffer_t buffer =
    create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, DEFAULT_TASK_BUFFER_FRAMES_CAPACITY);
  put_task(&buffer, task);
  
  // Step until either a success occurs or the buffer is empty
  while (!success && search_step(&buffer));
  
  // Cleanup
  destroy_task_buffer(buffer);
  (*notify_success).remove_ref();
}
