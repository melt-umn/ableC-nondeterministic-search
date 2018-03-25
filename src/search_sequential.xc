#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include <search_drivers.xh>

void search_sequential(task_t task, closure<() -> void> *notify_success) {
  struct task_buffer buffer = create_task_buffer(100, 10);
  bool success = false, *p_success = &success;
  *notify_success = lambda () -> (void) { *p_success = true; };

  do {
    //fprintf(stderr, "Evaluating task %s\n", task._fn_name);
    task(&buffer);
    task.remove_ref();
  } while (!success && get_task(&buffer, &task));

  //fprintf(stderr, "Destroying buffer\n");
  destroy_task_buffer(buffer);
  //fprintf(stderr, "Removing notify ref\n");
  (*notify_success).remove_ref();
}
