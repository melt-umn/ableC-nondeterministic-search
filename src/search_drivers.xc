#include <string.h>
#include <stdbool.h>

#include <search_drivers.xh>

void search_sequential(task_t task, closure<() -> void> *notify_success) {
  struct task_buffer buffer = create_task_buffer(100, false);
  bool success = false, *p_success = &success;
  *notify_success = lambda allocate(malloc) () -> (void) { *p_success = true; };

  task_t task;
  while (!success || get_task(&buffer, &task)) {
    task(&buffer);
  }

  destroy_task_buffer(buffer);
  (*notify_success).free(free);
}
