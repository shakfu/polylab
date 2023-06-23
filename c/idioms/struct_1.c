
#include <stdio.h>

typedef struct _foo
{
    int x;
    int y;
} t_foo;

void display(t_foo f[static 1])
{
    if (f)
        printf("x:%d y:%d\n", f->x, f->y);
}


int main(void)
{
    t_foo foo = {.x = 10, .y=20};
    display(&foo);
    display(NULL);

    printf("HELLO\n");
}
