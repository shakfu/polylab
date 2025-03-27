#include "queue.h"
#include <stdio.h>
#include <stdlib.h>
#define size 3

int main()
{
    int head, tail, element;
    int queue[size];

    // initialize queue
    init(&head, &tail);

    // enqueue elements
    while (full(tail, size) != 1) {
        element = rand();
        printf("enqueue element %d\n", element);
        enqueue(queue, &tail, element);
        printf("head=%d,tail=%d\n", head, tail);
        // press enter to enqueue more
        getchar();
    }
    printf("queue is full\n");

    // dequeue elements from
    while (!empty(head, tail)) {
        element = dequeue(queue, &head);
        printf("dequeue element %d \n", element);
        // press enter to pop more
        getchar();
    }
    printf("queue is empty\n");
    getchar();
}
