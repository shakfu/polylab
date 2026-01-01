#include <dispatch/dispatch.h>
#include <stdio.h>

static dispatch_once_t once;
static int expensive_resource;

static void init_resource(void *ctx) {
  (void)ctx;
  expensive_resource = 42;
  puts("Initialized resource");
}

static void use_resource(void *ctx) {
  (void)ctx;
  dispatch_once_f(&once, NULL, init_resource);
  printf("Resource value = %d\n", expensive_resource);
}

int main(void) {
  dispatch_queue_t q = dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0);

  for (int i = 0; i < 3; i++) {
    dispatch_async_f(q, NULL, use_resource);
  }

  sleep(1);
  return 0;
}
