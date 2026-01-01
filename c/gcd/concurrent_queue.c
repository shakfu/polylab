#include <dispatch/dispatch.h>
#include <stdio.h>

static int value = 0;

static void reader(void *ctx) {
  printf("read: %d\n", value);
}

static void writer(void *ctx) {
  value++;
  printf("write: %d\n", value);
}

int main(void) {
  dispatch_queue_t q =
      dispatch_queue_create("com.example.concurrent",
                            DISPATCH_QUEUE_CONCURRENT);

  dispatch_async_f(q, NULL, reader);
  dispatch_async_f(q, NULL, reader);
  dispatch_barrier_async_f(q, NULL, writer);
  dispatch_async_f(q, NULL, reader);

  sleep(1);
  return 0;
}
