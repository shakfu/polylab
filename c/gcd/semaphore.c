#include <dispatch/dispatch.h>
#include <stdio.h>
#include <unistd.h>

static dispatch_semaphore_t sem;

static void work(void *ctx) {
  int id = (int)(intptr_t)ctx;

  dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
  printf("Task %d starting\n", id);
  sleep(1);
  printf("Task %d done\n", id);
  dispatch_semaphore_signal(sem);
}

int main(void) {
  sem = dispatch_semaphore_create(2); // allow 2 concurrent tasks
  dispatch_queue_t q = dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0);

  for (int i = 1; i <= 5; i++) {
    dispatch_async_f(q, (void *)(intptr_t)i, work);
  }

  sleep(6); // allow tasks to finish
  return 0;
}
