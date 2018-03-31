#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include <task.xh>

struct task_buffer create_task_buffer(size_t capacity, size_t framesCapacity) {
  task_buffer_t result;
  result.tasks = malloc(sizeof(task_t) * capacity);
  result.size = 0;
  result.capacity = capacity;
  result.isFrameOpened = false;
  result.currentFrame = (struct frame){0, 0};
  result.frames = malloc(sizeof(struct frame) * framesCapacity);
  result.framesSize = 0;
  result.framesCapacity = framesCapacity;
  return result;
}

void destroy_task_buffer(task_buffer_t buffer) {
  task_t task;
  while (get_task(&buffer, &task)) {
    task.remove_ref();
  }
  
  free(buffer.tasks);
  free(buffer.frames);
}

void open_frame(task_buffer_t *const p_buffer) {
  // Set a flag indicating any new tasks should go in a new frame
  p_buffer->isFrameOpened = true;
}

void put_task(task_buffer_t *const p_buffer, task_t task) {
  task_buffer_t buffer = *p_buffer;

  // Open a new frame, if requested
  if (buffer.isFrameOpened) {
    buffer.isFrameOpened = false;
    if (buffer.framesSize == buffer.framesCapacity) {
      buffer.framesCapacity *= 2;
      buffer.frames = realloc(buffer.frames, sizeof(struct frame) * buffer.framesCapacity);
    }
    buffer.frames[buffer.framesSize] = buffer.currentFrame;
    buffer.framesSize++;
    buffer.currentFrame.start = buffer.currentFrame.end;
    *p_buffer = buffer;
    //fprintf(stderr, "open_frame %lu\n", buffer.framesSize);
  }

  // Put the new task in a buffer
  //fprintf(stderr, "put_task %s\n", task._fn_name);
  if (buffer.currentFrame.end == buffer.capacity) {
    buffer.capacity *= 2;
    buffer.tasks = realloc(buffer.tasks, sizeof(task_t) * buffer.capacity);
  }
  buffer.tasks[buffer.currentFrame.end] = task;
  buffer.size++;
  buffer.currentFrame.end++;
  *p_buffer = buffer;
}

size_t get_task(task_buffer_t *const p_buffer, task_t *task) {
  task_buffer_t buffer = *p_buffer;
  size_t old_size = buffer.size;
  if (buffer.size > 0) {
    // Invariant: If the buffer is non-empty, the current frame will be non-empty
    *task = buffer.tasks[buffer.currentFrame.start];
    buffer.size--;
    buffer.currentFrame.start++;
    buffer.isFrameOpened = false;
  }
  if (buffer.size > 0) {
    if (buffer.currentFrame.start == buffer.currentFrame.end) {
      //fprintf(stderr, "closing frame %lu\n", buffer.framesSize);
      // Current frame is empty but buffer is non-empty, restore the previous frame
      buffer.framesSize--;
      buffer.currentFrame = buffer.frames[buffer.framesSize];
      buffer.isFrameOpened = true;
    }
  } else {
    // Buffer is empty, restart from index 0 to save memory
    buffer.currentFrame = (struct frame){0, 0};
  }
  *p_buffer = buffer;
  // if (old_size)
  //   fprintf(stderr, "get_task %d %s\n", old_size, task->_fn_name);
  // else
  //   fprintf(stderr, "get_task empty\n");
  return old_size;
}
