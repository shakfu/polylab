// gcd_noblocks.c
// clang -std=c11 -Wall -Wextra -O2 gcd_noblocks.c -o gcd_noblocks

#include <dispatch/dispatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

struct task_ctx {
    int id;
};

static void do_work(void* ctx)
{
    struct task_ctx* t = (struct task_ctx*)ctx;

    unsigned usec = 200000u + (unsigned)(arc4random_uniform(600000u));
    usleep(usec);

    printf("Task %d done (work: %u us)\n", t->id, usec);
    free(t);
}

static void all_done(void* ctx)
{
    (void)ctx;
    puts("All tasks completed!");
    exit(0);
}

int main(void)
{
    dispatch_queue_t q = dispatch_get_global_queue(QOS_CLASS_USER_INITIATED,
                                                   0);
    dispatch_group_t g = dispatch_group_create();

    puts("Scheduling tasks...");

    for (int i = 1; i <= 5; i++) {
        struct task_ctx* t = malloc(sizeof *t);
        t->id = i;

        // Schedule work and associate it with the group
        dispatch_group_async_f(g, q, t, do_work);
    }

    // Notify when the group completes (no Blocks)
    dispatch_group_notify_f(g, dispatch_get_main_queue(), NULL, all_done);

    // Run the main dispatch loop
    dispatch_main();
    return 0; // unreachable
}
