#include <task.xh>
#include <search_driver_util.xh>
#include <stdlib.h>
#include <stdbool.h>

bool search_step(task_buffer_t *const p_buffer) {
  task_t task;
  if (get_task(p_buffer, &task)) {
    task(p_buffer);
    task.remove_ref();
    open_frame(p_buffer);
    return true;
  } else {
    return false;
  }
}

task_buffer_t expand(task_t task, size_t depth, bool *abort) {
  // Expand all tasks depth number of times using 2 buffers,
  // or until abort is set to true
  task_buffer_t
    buffer1 = create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, 0),
    buffer2 = create_task_buffer(DEFAULT_TASK_BUFFER_CAPACITY, 0);
  put_task(&buffer1, task);
  
  for (size_t i = 0; i < depth && (!abort || !*abort); i++) {
    // Evaluate all tasks in buffer1, dispatching to buffer2
    task_t task;
    while ((!abort || !*abort) && get_task(&buffer1, &task)) {
      task(&buffer2);
      task.remove_ref();
    }
    
    // Swap the buffers
    task_buffer_t tmp = buffer1;
    buffer1 = buffer2;
    buffer2 = tmp;
  }

  // Cleanup
  destroy_task_buffer(buffer2);
  return buffer1;
}
