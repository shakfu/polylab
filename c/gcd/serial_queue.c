#include <dispatch/dispatch.h>
#include <stdio.h>

static int counter = 0;

static void increment(void *ctx) {
  (void)ctx;
  counter++;
  printf("counter = %d\n", counter);
}

int main(void) {
  dispatch_queue_t serial =
      dispatch_queue_create("com.example.serial", DISPATCH_QUEUE_SERIAL);

  for (int i = 0; i < 5; i++) {
    dispatch_async_f(serial, NULL, increment);
  }

  dispatch_sync(serial, ^{}); // wait until queue drains (safe exit)
  return 0;
}
