#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char* strdup(const char* s);


typedef struct counter {
    int val;
    char* name;
} counter;

counter* counter_create(int start, char* name)
{
    counter* c;

    c = malloc(sizeof(*c));
    c->val = start;
    c->name = strdup(name);

    return c;
}

void counter_destroy(counter* c)
{
    if (c == NULL)
        return;
    free(c->name);
    free(c);
}

void counter_add(counter* c, int amount)
{
    if (c == NULL)
        return;
    c->val += amount;
}

void counter_subtract(counter* c, int amount)
{
    if (c == NULL)
        return;
    c->val -= amount;
}

void counter_increment(counter* c)
{
    if (c == NULL)
        return;
    c->val++;
}

void counter_decrement(counter* c)
{
    if (c == NULL)
        return;
    c->val--;
}

int counter_getval(counter* c)
{
    if (c == NULL)
        return 0;
    return c->val;
}

void counter_show(counter* c)
{
    printf("counter: %d\n", c->val);
    printf("counter.name: %s\n", c->name);
}


int main()
{
    counter* c = counter_create(0, "hello world");
    counter_add(c, 10);
    counter_show(c);
    counter_destroy(c);

    return 0;
}
