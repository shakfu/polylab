#include "Judy.h"
#include <stdio.h>

#define MAXLINELEN 256

int main()
{
    Pvoid_t assoc_arr = (Pvoid_t)NULL;
    PWord_t val;

    uint8_t idx[MAXLINELEN];

    // insert some values
    JSLI(val, assoc_arr, "hello");
    *val = 4;
    JSLI(val, assoc_arr, "world");
    *val = 8;
    JSLI(val, assoc_arr, "!");
    *val = 16;

    // iterate over indexes-values
    idx[0] = '\0';

    JSLF(val, assoc_arr, idx);
    while (val != NULL) {
        printf("'%s' -> %d\n", idx, *val);
        JSLN(val, assoc_arr, idx);
    }

    JudySLFreeArray(&assoc_arr, PJE0);
    return 0;
}