#include <string.h>
#include <stdbool.h>

#include <task.xh>

struct task_buffer create_task_buffer(size_t capacity, bool is_shared) {
  // TODO: Check capacity > 0
  
  struct task_buffer result;
  result.tasks = malloc(sizeof(task_t) * capacity);
  result.size = 0;
  result.capacity = capacity;
  result.is_shared = is_shared;
  result.mutex = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
  return result;
}

void destroy_task_buffer(struct task_buffer buffer) {
  free(buffer.tasks);
}

void put_task(const task_buffer_t buffer, task_t task) {
  if (buffer->is_shared) {
    pthread_mutex_lock(&(buffer->mutex));
  }
  if (buffer->size == buffer->capacity) {
    buffer->capacity *= 2;
    buffer->tasks = realloc(buffer->tasks, buffer->capacity);
  }
  buffer->tasks[buffer->size] = task;
  buffer->size++;
  if (buffer->is_shared) {
    pthread_mutex_unlock(&(buffer->mutex));
  }
}

int get_task(const task_buffer_t buffer, task_t *task) {
  if (buffer->is_shared) {
    pthread_mutex_lock(&(buffer->mutex));
  }
  size_t size = buffer->size;
  if (size > 0) {
    *task = buffer->tasks[size - 1];
    buffer->size--;
  }
  if (buffer->is_shared) {
    pthread_mutex_unlock(&(buffer->mutex));
  }
  return size;
}
