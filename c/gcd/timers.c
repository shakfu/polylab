#include <dispatch/dispatch.h>
#include <stdio.h>

static void tick(void *ctx) {
  int *count = ctx;
  printf("tick %d\n", (*count)++);
  if (*count > 5) exit(0);
}

int main(void) {
  static int counter = 1;

  dispatch_source_t timer =
      dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0,
                             dispatch_get_main_queue());

  dispatch_source_set_timer(timer,
                            dispatch_time(DISPATCH_TIME_NOW, 0),
                            1 * NSEC_PER_SEC,
                            0);

  dispatch_set_context(timer, &counter);
  dispatch_source_set_event_handler_f(timer, tick);
  dispatch_resume(timer);

  dispatch_main();
}

