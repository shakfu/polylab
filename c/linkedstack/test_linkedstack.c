#include <stdio.h>
#include <stdlib.h>

#include "linkedstack.h"


int main()
{
    struct node* head = NULL;
    int size, element, counter = 0;

    /*
         stack size is dynamic and
         specified at runtime
    */
    printf("Enter stack size:");
    scanf("%d", &size);

    printf("Push elements to stack\n");
    init(head);
    while (counter < size) {
        getchar();
        element = rand();
        printf("push element %d into stack\n", element);
        head = push(head, element);
        counter++;
    }
    printf("Pop elements from stack\n");
    while (0 == empty(head)) {
        head = pop(head, &element);
        printf("pop element %d from stack\n", element);
        getchar();
    }

    getchar();
}
