#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include <search_drivers.xh>

void search_sequential(task_t task, closure<() -> void> *notify_success) {
  struct task_buffer buffer = create_task_buffer(100, 10, false);
  bool success = false, *p_success = &success;
  *notify_success = lambda allocate(malloc) () -> (void) { *p_success = true; };

  do {
    task(&buffer);
  } while (!success && get_task(&buffer, &task));

  destroy_task_buffer(buffer);
  (*notify_success).free(free);
}
