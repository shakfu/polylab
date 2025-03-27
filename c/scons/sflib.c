#include "sfl.h"
#include <stdio.h>

int display(void)
{
    coprintf("hello %s", "person!");
    printf("cwd: %s", get_curdir());
    // printf("hello world\n");
    return 0;
}
