#include <stdio.h>
#include "Judy.h"

typedef char* string;

typedef struct dict {
    Pvoid_t map;
    PWord_t cursor;
} dict;

dict *dict_create()
{
    dict *d;
    d = malloc(sizeof(*d));
    d->map = (Pvoid_t) NULL;
    d->cursor = NULL;
    return d;
}

// type specialized insert
int dict_insert(dict *d, string key, double value)
{
    JSLI(d->cursor, d->map, key);
    *(d->cursor) = value;
    return 0;
}

// type specialized get
double dict_get(dict *d, string key)
{
    JSLG(d->cursor, d->map, key);
    if (d->cursor == NULL) {
        printf("key: %s not in dict\n", key);
        return;
    }
    return *(d->cursor);
}

double dict_destroy(dict *d)
{   
    JudySLFreeArray(&(d->map), PJE0);
    d->cursor = NULL;
    free(d);
}

 
int main()
{
    dict *d = dict_create();
    dict_insert(d, "hello", 100.0);
    printf("value: %f\n", dict_get(d, "hello"));
    dict_destroy(d);

    // Pvoid_t hashmap = (Pvoid_t) NULL;
    // PWord_t value;
    // int retval;

    // /* populating */
    // JSLI(value, hashmap, "hello");
    // *value = 100.0;
    // JSLI(value, hashmap, "world");
    // *value = 50;

    // /* retrieving existent */
    // JSLG(value, hashmap, "hello");
    // printf("value: %f\n", (double)*value);

    // /* unknown key */
    // JSLG(value, hashmap, "nonexistingkey");
    // if ( value == NULL ) { fprintf(stderr, "key 'nonexistingkey' does not exist\n"); }

    // /* deleting */
    // JSLD(retval, hashmap, "world");
    // JSLG(value, hashmap, "world");
    // if ( value == NULL ) { fprintf(stderr, "key 'world' does not exist anymore\n"); }

    // JudySLFreeArray(&hashmap, PJE0);

    return 0;
}