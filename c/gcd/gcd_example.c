// gcc -Wall -Wextra -O2 -std=c11 gcd_example.c -o gcd_example
// ./gcd_example

#include <dispatch/dispatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> // sleep/usleep

static void do_work(int id)
{
    // Simulate variable work time
    unsigned usec = 200000u + (unsigned)(arc4random_uniform(600000u));
    usleep(usec);
    printf("Task %d done on thread (simulated work: %u us)\n", id, usec);
}

int main(void)
{
    dispatch_queue_t q = dispatch_get_global_queue(QOS_CLASS_USER_INITIATED,
                                                   0);
    dispatch_group_t g = dispatch_group_create();

    puts("Scheduling tasks...");

    for (int i = 1; i <= 5; i++) {
        // dispatch_group_async schedules work and associates it with the group
        dispatch_group_async(g, q, ^{ do_work(i); });
    }

    // When all grouped tasks finish, run this on the main queue.
    dispatch_group_notify(g, dispatch_get_main_queue(), ^{
        puts("All tasks completed! (notify on main queue)");
        // Exit the main-thread dispatch loop below.
        exit(0);
    });

    // Keep the process alive to allow async work + main-queue notify to run.
    // (You can also use dispatch_main() in real daemon-style programs.)
    dispatch_main();

    return 0; // unreachable
}
