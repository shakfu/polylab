#include <dispatch/dispatch.h>
#include <stdio.h>

static void process(void *ctx) {
  const char *name = ctx;
  printf("Processing %s\n", name);
}

int main(void) {
  dispatch_queue_t q =
      dispatch_queue_create("com.example.pipeline",
                            DISPATCH_QUEUE_CONCURRENT);

  const char *files[] = {"a.txt", "b.txt", "c.txt"};

  for (int i = 0; i < 3; i++) {
    dispatch_async_f(q, (void *)files[i], process);
  }

  dispatch_barrier_sync(q, ^{}); // wait
  return 0;
}
