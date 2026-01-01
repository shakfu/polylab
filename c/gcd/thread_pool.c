// thread_pool.c - simple reusable pthread thread pool (pure C)
//
// Build: clang -std=c11 -Wall -Wextra -O2 -pthread thread_pool.c -o thread_pool
// Run:   ./thread_pool

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <unistd.h>

typedef void (*tp_task_fn)(void *);

// -------------------- Task queue --------------------

typedef struct tp_task {
  tp_task_fn fn;
  void *ctx;
  struct tp_task *next;
} tp_task;

typedef struct thread_pool {
  pthread_t *threads;
  size_t nthreads;

  tp_task *head;
  tp_task *tail;

  pthread_mutex_t mu;
  pthread_cond_t  cv_has_work;
  pthread_cond_t  cv_drained;

  bool stop;
  size_t pending; // queued + running
} thread_pool;

static void *tp_worker(void *arg) {
  thread_pool *tp = (thread_pool *)arg;

  for (;;) {
    pthread_mutex_lock(&tp->mu);

    while (!tp->stop && tp->head == NULL) {
      pthread_cond_wait(&tp->cv_has_work, &tp->mu);
    }

    if (tp->stop && tp->head == NULL) {
      pthread_mutex_unlock(&tp->mu);
      return NULL;
    }

    tp_task *t = tp->head;
    tp->head = t->next;
    if (!tp->head) tp->tail = NULL;

    pthread_mutex_unlock(&tp->mu);

    // Run task outside lock
    t->fn(t->ctx);
    free(t);

    pthread_mutex_lock(&tp->mu);
    tp->pending--;
    if (tp->pending == 0) {
      pthread_cond_broadcast(&tp->cv_drained);
    }
    pthread_mutex_unlock(&tp->mu);
  }
}

static int tp_init(thread_pool *tp, size_t nthreads) {
  if (!tp || nthreads == 0) return -1;

  tp->threads = calloc(nthreads, sizeof(pthread_t));
  if (!tp->threads) return -1;

  tp->nthreads = nthreads;
  tp->head = tp->tail = NULL;
  tp->stop = false;
  tp->pending = 0;

  if (pthread_mutex_init(&tp->mu, NULL) != 0) return -1;
  if (pthread_cond_init(&tp->cv_has_work, NULL) != 0) return -1;
  if (pthread_cond_init(&tp->cv_drained, NULL) != 0) return -1;

  for (size_t i = 0; i < nthreads; i++) {
    if (pthread_create(&tp->threads[i], NULL, tp_worker, tp) != 0) return -1;
  }
  return 0;
}

static int tp_submit(thread_pool *tp, tp_task_fn fn, void *ctx) {
  if (!tp || !fn) return -1;

  tp_task *t = malloc(sizeof(*t));
  if (!t) return -1;

  t->fn = fn;
  t->ctx = ctx;
  t->next = NULL;

  pthread_mutex_lock(&tp->mu);
  if (tp->stop) {
    pthread_mutex_unlock(&tp->mu);
    free(t);
    return -1;
  }

  if (tp->tail) tp->tail->next = t;
  else          tp->head = t;
  tp->tail = t;

  tp->pending++; // track outstanding work
  pthread_cond_signal(&tp->cv_has_work);
  pthread_mutex_unlock(&tp->mu);

  return 0;
}

// Wait for all currently-submitted tasks to finish.
static void tp_wait(thread_pool *tp) {
  pthread_mutex_lock(&tp->mu);
  while (tp->pending != 0) {
    pthread_cond_wait(&tp->cv_drained, &tp->mu);
  }
  pthread_mutex_unlock(&tp->mu);
}

// Stop: optionally wait/drain first (recommended), then join threads.
static void tp_destroy(thread_pool *tp) {
  if (!tp) return;

  pthread_mutex_lock(&tp->mu);
  tp->stop = true;
  pthread_cond_broadcast(&tp->cv_has_work);
  pthread_mutex_unlock(&tp->mu);

  for (size_t i = 0; i < tp->nthreads; i++) {
    pthread_join(tp->threads[i], NULL);
  }

  // Free remaining queued tasks (if any)
  tp_task *cur = tp->head;
  while (cur) {
    tp_task *n = cur->next;
    free(cur);
    cur = n;
  }

  pthread_cond_destroy(&tp->cv_drained);
  pthread_cond_destroy(&tp->cv_has_work);
  pthread_mutex_destroy(&tp->mu);
  free(tp->threads);
}

// -------------------- Demo --------------------

static void demo_work(void *ctx) {
  int id = (int)(intptr_t)ctx;
  usleep(200000 + (id * 50000));
  printf("[pthread pool] task %d done\n", id);
}

int main(void) {
  thread_pool tp;
  if (tp_init(&tp, 4) != 0) {
    fprintf(stderr, "Failed to init thread pool\n");
    return 1;
  }

  for (int i = 1; i <= 10; i++) {
    tp_submit(&tp, demo_work, (void *)(intptr_t)i);
  }

  tp_wait(&tp);
  puts("[pthread pool] all tasks finished");

  tp_destroy(&tp);
  return 0;
}
