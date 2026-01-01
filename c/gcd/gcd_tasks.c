// gcd_tasks.c - reusable GCD task system (pure C, no Blocks)
//
// Build: clang -std=c11 -Wall -Wextra -O2 gcd_tasks.c -o gcd_tasks
// Run:   ./gcd_tasks

#include <dispatch/dispatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

typedef void (*gcd_task_fn)(void *ctx, void **out_result);

// A handle you can wait on (like a tiny future).
typedef struct gcd_task_handle {
  dispatch_semaphore_t done;
  void *result;
  int status; // 0 ok, nonzero for user-defined status (here always 0)
} gcd_task_handle;

typedef struct gcd_task_system {
  dispatch_queue_t q;          // worker queue
  dispatch_group_t group;      // tracks all submitted tasks
  dispatch_semaphore_t slots;  // limits concurrency (pool size)
  bool shutting_down;
} gcd_task_system;

typedef struct task_wrapper_ctx {
  gcd_task_system *sys;
  gcd_task_fn fn;
  void *user_ctx;
  gcd_task_handle *handle; // optional; if NULL, fire-and-forget
} task_wrapper_ctx;

// ---- Internal runner (no Blocks) ----
static void task_runner(void *p) {
  task_wrapper_ctx *tw = (task_wrapper_ctx *)p;

  // run user task
  void *out = NULL;
  tw->fn(tw->user_ctx, &out);

  // store result & signal handle
  if (tw->handle) {
    tw->handle->result = out;
    tw->handle->status = 0;
    dispatch_semaphore_signal(tw->handle->done);
  }

  // release a "slot" for concurrency limiting
  dispatch_semaphore_signal(tw->sys->slots);

  free(tw);
}

// ---- Public API ----

static int gcdts_init(gcd_task_system *sys, int max_concurrency, const char *label) {
  if (!sys || max_concurrency <= 0) return -1;

  sys->q = dispatch_queue_create(label ? label : "com.example.gcdtasks",
                                 DISPATCH_QUEUE_CONCURRENT);
  sys->group = dispatch_group_create();
  sys->slots = dispatch_semaphore_create(max_concurrency);
  sys->shutting_down = false;
  return 0;
}

// Submit a task. If you pass handle_out != NULL, you get a handle you can wait on.
// If handle_out == NULL, it's fire-and-forget (still tracked by wait_all()).
static int gcdts_submit(gcd_task_system *sys,
                        gcd_task_fn fn,
                        void *user_ctx,
                        gcd_task_handle **handle_out) {
  if (!sys || !fn) return -1;
  if (sys->shutting_down) return -1;

  // Optional handle
  gcd_task_handle *h = NULL;
  if (handle_out) {
    h = (gcd_task_handle *)calloc(1, sizeof(*h));
    if (!h) return -1;
    h->done = dispatch_semaphore_create(0);
    *handle_out = h;
  }

  // Wait for an available slot (limits concurrency)
  dispatch_semaphore_wait(sys->slots, DISPATCH_TIME_FOREVER);

  task_wrapper_ctx *tw = (task_wrapper_ctx *)malloc(sizeof(*tw));
  if (!tw) {
    dispatch_semaphore_signal(sys->slots);
    if (h) free(h);
    return -1;
  }

  tw->sys = sys;
  tw->fn = fn;
  tw->user_ctx = user_ctx;
  tw->handle = h;

  // Track in group + run asynchronously (no Blocks)
  dispatch_group_async_f(sys->group, sys->q, tw, task_runner);
  return 0;
}

// Wait for one handle, then destroy it. Returns result via *out_result if desired.
static int gcdts_wait(gcd_task_handle *h, void **out_result) {
  if (!h) return -1;
  dispatch_semaphore_wait(h->done, DISPATCH_TIME_FOREVER);
  if (out_result) *out_result = h->result;

  // dispatch objects are refcounted; on modern macOS ARC manages in ObjC,
  // in C you generally just release by letting them go out of scope.
  // Free our wrapper handle struct.
  free(h);
  return 0;
}

// Wait for all tasks submitted so far.
static void gcdts_wait_all(gcd_task_system *sys) {
  if (!sys) return;
  dispatch_group_wait(sys->group, DISPATCH_TIME_FOREVER);
}

// Shutdown: prevent new submissions, wait for all running tasks, then free.
static void gcdts_shutdown(gcd_task_system *sys) {
  if (!sys) return;
  sys->shutting_down = true;
  gcdts_wait_all(sys);
  // In plain C on macOS, dispatch objects live until process exit unless using dispatch_release
  // (deprecated). It’s fine to leave them; process cleanup reclaims.
  // If you target older SDKs and have dispatch_release available, you could release here.
}

// ---- Demo tasks ----

typedef struct work_ctx {
  int id;
  int ms;
} work_ctx;

static void work_task(void *ctx, void **out_result) {
  work_ctx *w = (work_ctx *)ctx;
  usleep((useconds_t)w->ms * 1000u);
  printf("[gcd tasks] task %d finished (%dms)\n", w->id, w->ms);

  // Example: allocate a result
  int *res = (int *)malloc(sizeof(int));
  *res = w->id * 10;
  *out_result = res;

  free(w); // user ctx ownership handled by task
}

int main(void) {
  gcd_task_system sys;
  if (gcdts_init(&sys, 3, "com.example.gcdtasks.demo") != 0) {
    fprintf(stderr, "Failed to init gcd task system\n");
    return 1;
  }

  // Submit a few fire-and-forget tasks (still part of wait_all)
  for (int i = 1; i <= 5; i++) {
    work_ctx *w = (work_ctx *)malloc(sizeof(*w));
    w->id = i;
    w->ms = 150 + (i * 80);
    gcdts_submit(&sys, work_task, w, NULL);
  }

  // Submit a task and keep a handle (future-like)
  gcd_task_handle *h = NULL;
  {
    work_ctx *w = (work_ctx *)malloc(sizeof(*w));
    w->id = 99;
    w->ms = 400;
    gcdts_submit(&sys, work_task, w, &h);
  }

  // Wait for that specific task
  void *result = NULL;
  gcdts_wait(h, &result);
  printf("[gcd tasks] handle result = %d\n", *(int *)result);
  free(result);

  // Wait for everything else
  gcdts_wait_all(&sys);
  puts("[gcd tasks] all tasks finished");

  gcdts_shutdown(&sys);
  return 0;
}
