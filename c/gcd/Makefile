CC_BLOCKS := clang -std=c11 -Wall -Wextra -O2 -fblocks
CC_NOBLOCKS := clang -std=c11 -Wall -Wextra -O2


.PHONY: all build clean

all: build

build:
	@$(CC_BLOCKS) gcd_example.c -o gcd_example
	@$(CC_NOBLOCKS) gcd_noblocks.c -o gcd_noblocks
	@$(CC_NOBLOCKS) serial_queue.c -o serial_queue
	@$(CC_NOBLOCKS) semaphore.c -o semaphore
	@$(CC_NOBLOCKS) dispatch_once.c -o dispatch_once
	@$(CC_NOBLOCKS) timers.c -o timers
	@$(CC_NOBLOCKS) concurrent_queue.c -o concurrent_queue
	@$(CC_NOBLOCKS) producer_worker.c -o producer_worker
	@$(CC_NOBLOCKS) thread_pool.c -o thread_pool
	@$(CC_NOBLOCKS) gcd_tasks.c -o gcd_tasks

clean:
	@rm -f gcd_example gcd_noblocks serial_queue semaphore  \
		   dispatch_once timers concurrent_queue producer_worker \
		   thread_pool gcd_tasks

