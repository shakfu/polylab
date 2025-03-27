#include <stdlib.h>

struct node {
    int data;
    struct node* next;
};

void init(struct node* s) { s = NULL; }

struct node* push(struct node* s, int data)
{
    struct node* tmp = (struct node*)malloc(sizeof(struct node));
    if (tmp == NULL) {
        // no memory available
        exit(0);
    }
    tmp->data = data;
    tmp->next = s;
    s = tmp;
    return s;
}
struct node* pop(struct node* s, int* element)
{
    struct node* tmp = s;
    *element = s->data;
    s = s->next;
    free(tmp);
    return s;
}

int empty(struct node* s) { return s == NULL ? 1 : 0; }
