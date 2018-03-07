#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include <task.xh>

struct task_buffer create_task_buffer(size_t capacity, size_t framesCapacity, bool is_shared) {
  task_buffer_t result;
  result.tasks = malloc(sizeof(task_t) * capacity);
  result.size = 0;
  result.capacity = capacity;
  result.currentFrame = (struct frame){0, 0};
  result.frames = malloc(sizeof(struct frame) * framesCapacity);
  result.framesSize = 0;
  result.framesCapacity = framesCapacity;
  result.is_shared = is_shared;
  result.mutex = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
  return result;
}

void destroy_task_buffer(task_buffer_t buffer) {
  free(buffer.tasks);
  free(buffer.frames);
}

void open_frame(task_buffer_t *const p_buffer) {
  task_buffer_t buffer = *p_buffer;
  if (buffer.is_shared) {
    pthread_mutex_lock(&(p_buffer->mutex));
  }
  if (buffer.currentFrame.start < buffer.currentFrame.end) {
    // If the topmost frame is non-empty, open a new frame
    if (buffer.framesSize == buffer.framesCapacity) {
      buffer.framesCapacity *= 2;
      buffer.frames = realloc(buffer.frames, buffer.framesCapacity);
    }
    buffer.frames[buffer.framesSize] = buffer.currentFrame;
    buffer.framesSize++;
    buffer.currentFrame.start = buffer.currentFrame.end;
    *p_buffer = buffer;
    //fprintf(stderr, "open_frame %lu\n", buffer.framesSize);
  } else 
    //fprintf(stderr, "open_frame current\n");
  if (buffer.is_shared) {
    pthread_mutex_unlock(&(p_buffer->mutex));
  }
}

void put_task(task_buffer_t *const p_buffer, task_t task) {
  task_buffer_t buffer = *p_buffer;
  if (buffer.is_shared) {
    pthread_mutex_lock(&(p_buffer->mutex));
  }
  //fprintf(stderr, "put_task %s\n", task._fn_name);
  if (buffer.size == buffer.capacity) {
    buffer.capacity *= 2;
    buffer.tasks = realloc(buffer.tasks, buffer.capacity);
  }
  buffer.tasks[buffer.currentFrame.end] = task;
  buffer.size++;
  buffer.currentFrame.end++;
  *p_buffer = buffer;
  if (buffer.is_shared) {
    pthread_mutex_unlock(&(p_buffer->mutex));
  }
}

size_t get_task(task_buffer_t *const p_buffer, task_t *task) {
  task_buffer_t buffer = *p_buffer;
  if (buffer.is_shared) {
    pthread_mutex_lock(&(p_buffer->mutex));
  }
  size_t old_size = buffer.size;
  if (buffer.size > 0) {
    // Invariant: If the buffer is non-empty, the current frame will be non-empty
    *task = buffer.tasks[buffer.currentFrame.start];
    buffer.size--;
    buffer.currentFrame.start++;
  }
  if (buffer.size > 0 && buffer.currentFrame.start == buffer.currentFrame.end) {
    //fprintf(stderr, "closing frame %lu\n", buffer.framesSize);
    // Current frame is empty, restore the previous one
    buffer.framesSize--;
    buffer.currentFrame = buffer.frames[buffer.framesSize];
  }
  *p_buffer = buffer;
  //if (old_size)
    //fprintf(stderr, "get_task %s\n", task->_fn_name);
    //else
    //fprintf(stderr, "get_task empty\n");
  if (buffer.is_shared) {
    pthread_mutex_unlock(&(p_buffer->mutex));
  }
  return old_size;
}
