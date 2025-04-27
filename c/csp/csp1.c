// csp (Communicating sequential processes) example in c
// using gemme3:4b

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h> // For sleep()

// Define the counter
int counter = 0;

// Mutex and condition variable for synchronization
pthread_mutex_t mutex;
pthread_cond_t condition;

// Process 1: Increments the counter
void *increment_process(void *arg) {
    while (1) {
        pthread_mutex_lock(&mutex);
        counter++;
        printf("Incremented counter to: %d\n", counter);
        pthread_cond_signal(&condition); // Signal the reader process
        pthread_mutex_unlock(&mutex);
        sleep(1); // Simulate some work
    }
    return NULL;
}

// Process 2: Reads the counter
void *read_process(void *arg) {
    while (1) {
        pthread_mutex_lock(&mutex);
        while (counter == 0) {
            pthread_cond_wait(&condition, &mutex); // Wait until the counter is incremented
        }
        printf("Counter value: %d\n", counter);
        pthread_mutex_unlock(&mutex);
        sleep(2); // Simulate some work
    }
    return NULL;
}


int main() {
    pthread_t increment_thread;
    pthread_t read_thread;

    // Initialize the mutex and condition variable
    pthread_mutex_init(&mutex, NULL);
    pthread_cond_init(&condition, NULL);

    // Create the processes (threads)
    pthread_create(&increment_thread, NULL, increment_process, NULL);
    pthread_create(&read_thread, NULL, read_process, NULL);

    // Join the threads (so the main thread doesn't exit prematurely)
    pthread_join(increment_thread, NULL);
    pthread_join(read_thread, NULL);


    // Clean up
    pthread_mutex_destroy(&mutex);
    pthread_cond_destroy(&condition);

    printf("Program finished.\n");
    return 0;
}
