#include <stdio.h>

int main(void)
{
    int list[5] = {0, 1, 2, 3, 4};

    for (int i=0; i < 5; ++i) {
        printf("++i: %d\n", i);
    }

    for (int i=0; i < 5; i++) {
        printf("i++: %d\n", i);
    }
}

