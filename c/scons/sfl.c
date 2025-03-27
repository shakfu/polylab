#include "sfl.h"
#include <stdio.h>

int main(int argc, char* argv[])
{
    coprintf("hello %s", "person!");
    printf("cwd: %s", get_curdir());
    // printf("hello world\n");
    return 0;
}
